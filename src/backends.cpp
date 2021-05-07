#include "backends.h"
#include "compiler.h"
#include "calling_conventions.h"
#include "format.h"

#include <stdio.h>

size_t vm_backend_single_func(Array<uint8_t>& out_code, const CodeBlock* code, uint64_t labels) {
  size_t* const label_indexes = allocate_default<size_t>(labels);
  Array<size_t> instruction_offsets ={};

  size_t entry_point_label = code->label;

  {
    auto code_i = code->code.begin();
    const auto code_end = code->code.end();

    //First is guaranteed to be label
    {
      const auto p = ByteCode::PARSE::LABEL(code_i);

      label_indexes[p.u64.val] = out_code.size;

      code_i += ByteCode::SIZE_OF::LABEL;
    }

    while (code_i < code_end) {
      switch (*code_i) {
        case ByteCode::RESERVE: {
            code_i += ByteCode::SIZE_OF::RESERVE;
            break;
          }
        case ByteCode::LABEL: {
            const auto p = ByteCode::PARSE::LABEL(code_i);

            label_indexes[p.u64.val] = out_code.size;

            code_i += ByteCode::SIZE_OF::LABEL;
            break;
          }
        case ByteCode::JUMP_TO_FIXED: {
            const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED(code_i);
            code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED;

            //Can ignore anything between a fixed jump and a label - it wont every be reached
            while (code_i < code_end && code_i[0] != ByteCode::LABEL) {
              code_i += ByteCode::instruction_size(code_i[0]);
            }

            size_t next = 0;

            //Could be series of labels so needs to while loop
            while (code_i + next < code_end && code_i[next] == ByteCode::LABEL) {
              const auto p_l = ByteCode::PARSE::LABEL(code_i + next);

              if (p_l.u64.val == p_j.u64.val) {
                //About to jump to next instruction - can ignore jump
                goto SKIP_JUMP;
              }

              //Next might also be a label
              next += ByteCode::SIZE_OF::LABEL;
            }

            instruction_offsets.insert(out_code.size);
            ByteCode::EMIT::JUMP_TO_FIXED(out_code, p_j.u64);
          SKIP_JUMP:
            break;
          }
        case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO:
        case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO:
          //Could try removing these jumps maybe
          //but they shouldnt ever jump to the next instruction anyway
          //Would be checking for something that never happens
        case ByteCode::CALL:
          instruction_offsets.insert(out_code.size);
          goto NEXT_INSTRUCTION;//Remove warning about fallthrough
        default: {
          NEXT_INSTRUCTION:
            const size_t i_size = ByteCode::instruction_size(*code_i);
            out_code.insert_uninit(i_size);
            memcpy_ts(out_code.data + out_code.size - i_size, i_size, code_i, i_size);

            code_i += i_size;
            break;
          }
      }
    }
  }

  {
    auto jumps_i = instruction_offsets.begin();
    const auto jumps_end = instruction_offsets.end();

    for (; jumps_i < jumps_end; jumps_i++) {
      const size_t index = *jumps_i;

      switch (out_code.data[index]) {
        case ByteCode::CALL: {
            const auto p = ByteCode::PARSE::CALL(out_code.data + index);
            ByteCode::WRITE::CALL(out_code.data + index, label_indexes[p.u64.val]);
            break;
          }
        case ByteCode::JUMP_TO_FIXED: {
            const auto p = ByteCode::PARSE::JUMP_TO_FIXED(out_code.data + index);
            ByteCode::WRITE::JUMP_TO_FIXED(out_code.data + index, label_indexes[p.u64.val]);
            break;
          }
        case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
            const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_ZERO(out_code.data + index);
            ByteCode::WRITE::JUMP_TO_FIXED_IF_VAL_ZERO(out_code.data + index, p.val, label_indexes[p.u64.val]);
            break;
          }
        case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
            const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(out_code.data + index);
            ByteCode::WRITE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(out_code.data + index, p.val, label_indexes[p.u64.val]);
            break;
          }
      }
    }
  }

  size_t entry_point_index = -1;
  if (entry_point_label <= labels) {
    entry_point_index = label_indexes[entry_point_label];
  }

  free_no_destruct(label_indexes);
  return entry_point_index;
}

size_t vm_backend(Array<uint8_t>& out_code, const Compiler* comp) {
  size_t* const label_indexes = allocate_default<size_t>(comp->labels);
  Array<size_t> instruction_offsets ={};

  size_t entry_point_label = -1;

  {
    auto func_i = comp->functions.begin_const_iter();
    const auto func_end = comp->functions.end_const_iter();

    for (; func_i != func_end; func_i.next()) {
      const Function* const func = func_i.get();
      const CodeBlock* const code = &func->code_block;

      auto code_i = code->code.begin();
      const auto code_end = code->code.end();

      //First is guaranteed to be label
      {
        const auto p = ByteCode::PARSE::LABEL(code_i);

        label_indexes[p.u64.val] = out_code.size;

        //Find entry point
        if (func->name == comp->entry_point
            && func->parameter_types.size == 0) {
          entry_point_label = p.u64.val;
        }

        code_i += ByteCode::SIZE_OF::LABEL;
      }

      while (code_i < code_end) {
        switch (*code_i) {
          case ByteCode::RESERVE: {
              code_i += ByteCode::SIZE_OF::RESERVE;
              break;
            }
          case ByteCode::LABEL: {
              const auto p = ByteCode::PARSE::LABEL(code_i);

              label_indexes[p.u64.val] = out_code.size;

              code_i += ByteCode::SIZE_OF::LABEL;
              break;
            }
          case ByteCode::JUMP_TO_FIXED: {
              const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED(code_i);
              code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED;

              //Can ignore anything between a fixed jump and a label - it wont every be reached
              while (code_i < code_end && code_i[0] != ByteCode::LABEL) {
                code_i += ByteCode::instruction_size(code_i[0]);
              }

              size_t next = 0;

              //Could be series of labels so needs to while loop
              while (code_i + next < code_end && code_i[next] == ByteCode::LABEL) {
                const auto p_l = ByteCode::PARSE::LABEL(code_i + next);

                if (p_l.u64.val == p_j.u64.val) {
                  //About to jump to next instruction - can ignore jump
                  goto SKIP_JUMP;
                }

                //Next might also be a label
                next += ByteCode::SIZE_OF::LABEL;
              }

              instruction_offsets.insert(out_code.size);
              ByteCode::EMIT::JUMP_TO_FIXED(out_code, p_j.u64);
            SKIP_JUMP:
              break;
            }
          case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO:
          case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO:
            //Could try removing these jumps maybe
            //but they shouldnt ever jump to the next instruction anyway
            //Would be checking for something that never happens
          case ByteCode::CALL:
            instruction_offsets.insert(out_code.size);
            goto NEXT_INSTRUCTION;//Remove warning about fallthrough
          default: {
            NEXT_INSTRUCTION:
              const size_t i_size = ByteCode::instruction_size(*code_i);
              out_code.insert_uninit(i_size);
              memcpy_ts(out_code.data + out_code.size - i_size, i_size, code_i, i_size);

              code_i += i_size;
              break;
            }
        }
      }
    }
  }

  {
    auto jumps_i = instruction_offsets.begin();
    const auto jumps_end = instruction_offsets.end();

    for (; jumps_i < jumps_end; jumps_i++) {
      const size_t index = *jumps_i;

      switch (out_code.data[index]) {
        case ByteCode::CALL: {
            const auto p = ByteCode::PARSE::CALL(out_code.data + index);
            ByteCode::WRITE::CALL(out_code.data + index, label_indexes[p.u64.val]);
            break;
          }
        case ByteCode::JUMP_TO_FIXED: {
            const auto p = ByteCode::PARSE::JUMP_TO_FIXED(out_code.data + index);
            ByteCode::WRITE::JUMP_TO_FIXED(out_code.data + index, label_indexes[p.u64.val]);
            break;
          }
        case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
            const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_ZERO(out_code.data + index);
            ByteCode::WRITE::JUMP_TO_FIXED_IF_VAL_ZERO(out_code.data + index, p.val, label_indexes[p.u64.val]);
            break;
          }
        case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
            const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(out_code.data + index);
            ByteCode::WRITE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(out_code.data + index, p.val, label_indexes[p.u64.val]);
            break;
          }
      }
    }
  }

  size_t entry_point_index = -1;
  if (entry_point_label <= comp->labels) {
    entry_point_index = label_indexes[entry_point_label];
  }

  free_no_destruct(label_indexes);
  return entry_point_index;
}

void X64::mov(Array<uint8_t>& arr,
              uint8_t from,
              uint8_t to) {
  arr.insert(X64::REX_W | X64::rex_r_rm(from, to));
  arr.insert(X64::MOV_R_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from, to));
}

static void emit_sib(Array<uint8_t>& arr, const X64::SIB& sib, uint8_t mod_byte) {
  if (!sib.use_base) {
    assert(!sib.use_index);//Must use both if using index

    if (!sib.use_index) {
      arr.insert(00'000'000 | mod_byte);
      arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, RBP.REG));

      arr.reserve_extra(4);

      memcpy_ts(arr.data + arr.size, arr.capacity - arr.size,
                (const uint8_t*)&sib.disp, 4);

      arr.size += 4;
    }
    else {
      assert(sib.index != RSP.REG);//Not a valid code unfortunately
      
      arr.insert(00'000'000 | mod_byte);
      arr.insert(X64::sib(sib.scale, sib.index, RBP.REG));

      arr.reserve_extra(4);

      memcpy_ts(arr.data + arr.size, arr.capacity - arr.size,
                (const uint8_t*)&sib.disp, 4);

      arr.size += 4;
    }
  }
  else if (sib.use_base && !sib.use_index) {
    if (sib.disp == 0 && (sib.base & 0b111) != RBP.REG) {
      arr.insert(00'000'000 | mod_byte);
      arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, sib.base));
    }
    else if (-128 <= sib.disp  && sib.disp <= 127) {
      arr.insert(01'000'000 | mod_byte);
      arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, sib.base));
      arr.insert((uint8_t)sib.disp);
    }
    else {
      arr.insert(10'000'000 | mod_byte);
      arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, sib.base));

      arr.reserve_extra(4);

      memcpy_ts(arr.data + arr.size, arr.capacity - arr.size,
                (const uint8_t*)&sib.disp, 4);

      arr.size += 4;
    }
  }
  else {
    //use base and index

    if (sib.disp == 0 && (sib.base & 0b111) != RBP.REG) {
      arr.insert(00'000'000 | mod_byte);
      arr.insert(X64::sib(sib.scale, sib.index, sib.base));
    }
    else if (-128 <= sib.disp  && sib.disp <= 127) {
      arr.insert(01'000'000 | mod_byte);
      arr.insert(X64::sib(sib.scale, sib.index, sib.base));
      arr.insert((uint8_t)sib.disp);
    }
    else {
      arr.insert(10'000'000 | mod_byte);
      arr.insert(X64::sib(sib.scale, sib.index, sib.base));

      arr.reserve_extra(4);

      memcpy_ts(arr.data + arr.size, arr.capacity - arr.size,
                (const uint8_t*)&sib.disp, 4);

      arr.size += 4;
    }
  }
}

static void emit_mod_rm(Array<uint8_t>& arr, const X64::R r, const X64::RM& rm) {
  if (!rm.indirect) {
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(r.r, rm.r));
    return;
  }

  switch (rm.r) {
    case RSP.REG:
    case R12.REG: {
        //SIB byte time

        if (rm.use_sib) {
          emit_sib(arr, rm.sib, X64::modrm_r_rm(r.r, rm.r));
        }
        else if (rm.disp == 0) {
          arr.insert(X64::MODRM_MOD_INDIRECT | X64::modrm_r_rm(r.r, rm.r));
          arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, rm.r));
        }
        else if (-128 <= rm.disp  && rm.disp <= 127) {
          arr.insert(0b01000000 | X64::modrm_r_rm(r.r, rm.r));
          arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, rm.r));
          arr.insert((int8_t)rm.disp);
        }
        else {
          arr.insert(0b10000000 | X64::modrm_r_rm(r.r, rm.r));
          arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, rm.r));
          arr.reserve_extra(4);

          memcpy_ts(arr.data + arr.size, arr.capacity - arr.size,
                    (const uint8_t*)&rm.disp, 4);
          arr.size += 4;
        }
        break;
      }

    case RBP.REG:
    case R13.REG: {
        //Encode disp 0 by having a 1 byte disp of 0
        goto RM_DEFAULT;
      }

    default: {
        if (rm.disp == 0) {
          arr.insert(X64::MODRM_MOD_INDIRECT | X64::modrm_r_rm(r.r, rm.r));
          break;
        }

      RM_DEFAULT:
        if (-128 <= rm.disp  && rm.disp <= 127) {
          arr.insert(0b01000000 | X64::modrm_r_rm(r.r, rm.r));
          arr.insert((int8_t)rm.disp);
          break;
        }
        else {
          arr.insert(0b10000000 | X64::modrm_r_rm(r.r, rm.r));
          arr.reserve_extra(4);

          memcpy_ts(arr.data + arr.size, arr.capacity - arr.size,
                    (const uint8_t*)&rm.disp, 4);
          arr.size += 4;
          break;
        }
      }
  }
}

void X64::mov(Array<uint8_t>& arr,
              R r,
              const RM& rm) {

  arr.insert(X64::REX_W | X64::rex_r_rm(r.r, rm.r));
  arr.insert(X64::MOV_R_TO_RM);

  emit_mod_rm(arr, r, rm);
}

void X64::mov(Array<uint8_t>& arr,
              const RM& rm,
              R r) {
  arr.insert(X64::REX_W | X64::rex_r_rm(r.r, rm.r));
  arr.insert(X64::MOV_RM_TO_R);

  emit_mod_rm(arr, r, rm);
}

void X64::mov(Array<uint8_t>& arr,
              R r,
              uint64_t u64) {
  arr.insert(X64::REX_W | X64::rex_b(r.r));
  arr.insert(X64::MOV_64_TO_R + (r.r & 0b111));

  arr.reserve_extra(sizeof(uint64_t));
  x64_to_bytes(u64, arr.data + arr.size);
  arr.size += sizeof(uint64_t);
}

void X64::mov(Array<uint8_t>& arr,
              const RM& rm,
              uint32_t u32) {
  if ((rm.r & 0b1000) > 0) {
    arr.insert(X64::REX | X64::rex_b(rm.r));
  }

  arr.insert(X64::MOV_IMM32_RM);

  emit_mod_rm(arr, R{ '\0' }, rm);

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(u32, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

void X64::sub(Array<uint8_t>& arr,
              uint8_t r,
              uint8_t rm) {
  arr.insert(X64::REX_W | X64::rex_r_rm(r, rm));
  arr.insert(X64::SUB_R_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT
             | X64::modrm_r_rm(r, rm));
}

void X64::sub(Array<uint8_t>& arr,
              uint8_t rm,
              int32_t i32) {
  arr.insert(X64::REX_W | X64::rex_rm(rm));
  arr.insert(X64::SUB_32_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(5, rm));

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(i32, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

void X64::push(Array<uint8_t>& arr, uint8_t reg) {
  if ((reg & 0b0000'1000) > 0) {
    arr.insert(X64::REX_W | X64::REX_B);
  }

  arr.insert(X64::PUSH_R + (reg & 0b0000'0111));
}

void X64::pop(Array<uint8_t>& arr, uint8_t reg) {
  if ((reg & 0b0000'1000) > 0) {
    arr.insert(X64::REX_W | X64::REX_B);
  }

  arr.insert(X64::POP_R + (reg & 0b0000'0111));
}

void X64::ret(Array<uint8_t>& arr) {
  arr.insert(X64::RET_NEAR);
}

inline static void jump_near(Array<uint8_t>& arr, int32_t relative) {
  arr.reserve_extra(1 + sizeof(relative));

  arr.insert(X64::JMP_NEAR);
  x32_to_bytes((uint32_t)relative, arr.data + arr.size);
  arr.size += sizeof(relative);
}

inline static void call_near(Array<uint8_t>& arr, int32_t relative) {
  arr.reserve_extra(1 + sizeof(relative));

  arr.insert(X64::CALL_NEAR);
  x32_to_bytes((uint32_t)relative, arr.data + arr.size);
  arr.size += sizeof(relative);
}

inline static void jump_zero(Array<uint8_t>& arr, int32_t relative) {
  arr.insert(0x0F);
  arr.insert(X64::JZ_NEAR);

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(relative, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

inline static void jump_not_equal(Array<uint8_t>& arr, int32_t relative) {
  arr.insert(0x0F);
  arr.insert(X64::JNE_NEAR);

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(relative, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

inline static void cmp(Array<uint8_t>& arr, uint8_t r, uint8_t rm) {
  arr.insert(X64::REX_W | X64::rex_r_rm(r, rm));
  arr.insert(X64::CMP_R_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT
             | X64::modrm_r_rm(r, rm));
}

inline static void cmp(Array<uint8_t>& arr, uint8_t rm, int32_t i32) {
  arr.insert(X64::REX_W | X64::rex_rm(rm));
  arr.insert(X64::CMP_32_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(7, rm));

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(i32, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

static void check_eq(Array<size_t>& instruction_offsets,
                     Array<uint8_t>& out_code,
                     const uint8_t** code_i_ptr, const uint8_t* end) {

  const uint8_t* const code_i = *code_i_ptr;
  const uint8_t* const code_i_2 = code_i + ByteCode::SIZE_OF::EQ_R64S;

  switch (*code_i) {
    case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
        const auto p_e = ByteCode::PARSE::EQ_R64S(code_i);
        const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i_2);

        cmp(out_code, p_e.val1, p_e.val2);

        instruction_offsets.insert(out_code.size);
        jump_zero(out_code, (int32_t)p_j.u64.val);

        *code_i_ptr = code_i_2 + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
        break;
      }
    case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
        const auto p_e = ByteCode::PARSE::EQ_R64S(code_i);
        const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i_2);

        cmp(out_code, p_e.val1, p_e.val2);

        instruction_offsets.insert(out_code.size);
        jump_not_equal(out_code, (int32_t)p_j.u64.val);

        *code_i_ptr = code_i_2 + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
        break;
      }
    default: {
        const auto p = ByteCode::PARSE::EQ_R64S(code_i);

        cmp(out_code, p.val1, p.val2);

        out_code.insert(X64::REX | X64::rex_rm(p.val2));
        out_code.insert(0x0F);
        out_code.insert(X64::SETE_RM8);
        out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_rm(p.val2));

        //Clear the top of the register
        out_code.insert(X64::REX_W | X64::rex_r_rm(p.val2, p.val2));
        out_code.insert(0x0F);
        out_code.insert(X64::MOV_ZX_RM8_TO_R);
        out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(p.val2, p.val2));


        *code_i_ptr = code_i_2;
        break;
      }
  }
}

static void check_for_jumps(Array<size_t>& instruction_offsets, Array<uint8_t>& out_code, uint8_t val, const uint8_t** code_i_ptr, const uint8_t* end) {

  const uint8_t* code_i = *code_i_ptr;

  if (code_i >= end) {
    return;
  }

  switch (*code_i) {
    case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
        const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

        if (val != p.val) {
          cmp(out_code, p.val, (int32_t)0);
        }


        instruction_offsets.insert(out_code.size);
        jump_zero(out_code, (int32_t)p.u64.val);

        *code_i_ptr = code_i + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
        break;
      }
    case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
        const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

        if (val != p.val) {
          cmp(out_code, p.val, (int32_t)0);
        }

        instruction_offsets.insert(out_code.size);
        jump_not_equal(out_code, (int32_t)p.u64.val);

        *code_i_ptr = code_i + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
        break;
      }

    default:
      return;
  }
}

size_t x86_64_machine_code_backend(Array<uint8_t>& out_code, const Compiler* comp) {
  size_t* const label_indexes = allocate_default<size_t>(comp->labels);
  Array<size_t> instruction_offsets ={};

  size_t entry_point_label = -1;

  {
    auto func_i = comp->functions.begin_const_iter();
    const auto func_end = comp->functions.end_const_iter();

    for (; func_i != func_end; func_i.next()) {
      const Function* const func = func_i.get();
      const CodeBlock* const code = &func->code_block;

      auto code_i = code->code.begin();
      const auto code_end = code->code.end();

      //First is guaranteed to be label
      {
        const auto p = ByteCode::PARSE::LABEL(code_i);

        label_indexes[p.u64.val] = out_code.size;

        //Find entry point
        if (func->name == comp->entry_point
            && func->parameter_types.size == 0) {
          entry_point_label = p.u64.val;
        }

        code_i += ByteCode::SIZE_OF::LABEL;
      }

      while (code_i < code_end) {
        switch (*code_i) {
          case ByteCode::RESERVE: {
              code_i += ByteCode::SIZE_OF::RESERVE;
              break;
            }
          case ByteCode::COPY_R64_TO_R64: {
              const auto p = ByteCode::PARSE::COPY_R64_TO_R64(code_i);
              X64::mov(out_code, p.val1, p.val2);
              code_i += ByteCode::SIZE_OF::COPY_R64_TO_R64;
              break;
            }
          case ByteCode::SET_R64_TO_64: {
              const auto p = ByteCode::PARSE::SET_R64_TO_64(code_i);
              X64::mov(out_code, X64::R{ p.val }, p.u64);
              code_i += ByteCode::SIZE_OF::SET_R64_TO_64;
              break;
            }
          case ByteCode::ADD_R64S: {
              const auto p = ByteCode::PARSE::ADD_R64S(code_i);

              out_code.insert(X64::REX_W | X64::rex_r_rm(p.val1, p.val2));
              out_code.insert(X64::ADD_R_TO_RM);
              out_code.insert(X64::MODRM_MOD_DIRECT
                              | X64::modrm_r_rm(p.val1, p.val2));

              code_i += ByteCode::SIZE_OF::ADD_R64S;

              check_for_jumps(instruction_offsets, out_code, p.val2, &code_i, code_end);
              break;
            }
          case ByteCode::SUB_R64S: {
              const auto p = ByteCode::PARSE::SUB_R64S(code_i);

              X64::sub(out_code, p.val1, p.val2);

              code_i += ByteCode::SIZE_OF::SUB_R64S;

              check_for_jumps(instruction_offsets, out_code, p.val2, &code_i, code_end);
              break;
            }
          case ByteCode::MUL_R64S: {
              const auto p = ByteCode::PARSE::MUL_R64S(code_i);

              //Can use IMUL here even though it might be unsigned (only important if you need the overflow??)
              out_code.insert(X64::REX_W | X64::rex_r_rm(p.val2, p.val1));
              out_code.insert(0x0F);
              out_code.insert(X64::IMUL_RM_TO_R);
              out_code.insert(X64::MODRM_MOD_DIRECT
                              | X64::modrm_r_rm(p.val2, p.val1));

              code_i += ByteCode::SIZE_OF::MUL_R64S;

              check_for_jumps(instruction_offsets, out_code, p.val2, &code_i, code_end);
              break;
            }
          case ByteCode::DIV_RU64S: {
              const auto p = ByteCode::PARSE::DIV_RU64S(code_i);

              assert(p.val2 == 0);

              //Set RDX to 0
              X64::mov(out_code, X64::R{ RDX.REG }, 0ull);

              out_code.insert(X64::REX_W | X64::rex_rm(p.val1));
              out_code.insert(X64::DIV_RM_TO_RAX);
              out_code.insert(X64::MODRM_MOD_DIRECT
                              | X64::modrm_r_rm(6, p.val1));


              code_i += ByteCode::SIZE_OF::DIV_RU64S;

              check_for_jumps(instruction_offsets, out_code, 0, &code_i, code_end);
              break;
            }
          case ByteCode::DIV_RI64S: {
              const auto p = ByteCode::PARSE::DIV_RI64S(code_i);

              assert(p.val2 == 0);

              //Sign extend to RDX
              out_code.insert(X64::REX_W);
              out_code.insert(X64::CQO);

              out_code.insert(X64::REX_W | X64::rex_rm(p.val1));
              out_code.insert(X64::IDIV_RM_TO_RAX);
              out_code.insert(X64::MODRM_MOD_DIRECT
                              | X64::modrm_r_rm(7, p.val1));


              code_i += ByteCode::SIZE_OF::DIV_RI64S;

              check_for_jumps(instruction_offsets, out_code, 0, &code_i, code_end);
              break;
            }
          case ByteCode::OR_R64S: {
              const auto p = ByteCode::PARSE::OR_R64S(code_i);

              out_code.insert(X64::REX_W | X64::rex_r_rm(p.val1, p.val2));
              out_code.insert(X64::OR_R_TO_RM);
              out_code.insert(X64::MODRM_MOD_DIRECT
                              | X64::modrm_r_rm(p.val1, p.val2));


              code_i += ByteCode::SIZE_OF::OR_R64S;

              check_for_jumps(instruction_offsets, out_code, 0, &code_i, code_end);
              break;
            }
          case ByteCode::AND_R64S: {
              const auto p = ByteCode::PARSE::AND_R64S(code_i);

              out_code.insert(X64::REX_W | X64::rex_r_rm(p.val1, p.val2));
              out_code.insert(X64::AND_R_TO_RM);
              out_code.insert(X64::MODRM_MOD_DIRECT
                              | X64::modrm_r_rm(p.val1, p.val2));

              code_i += ByteCode::SIZE_OF::AND_R64S;

              check_for_jumps(instruction_offsets, out_code, 0, &code_i, code_end);
              break;
            }
          case ByteCode::EQ_R64S: {
              check_eq(instruction_offsets, out_code, &code_i, code_end);
              break;
            }
          case ByteCode::NEG_R64: {
              const auto p = ByteCode::PARSE::NEG_R64(code_i);

              out_code.insert(X64::REX_W | X64::rex_rm(p.val));
              out_code.insert(X64::NEG_RM);
              out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(3, p.val));

              code_i += ByteCode::SIZE_OF::NEG_R64;

              check_for_jumps(instruction_offsets, out_code, 0, &code_i, code_end);
              break;
            }
          case ByteCode::PUSH_R64: {
              const auto p = ByteCode::PARSE::PUSH_R64(code_i);

              X64::push(out_code, p.val);
              code_i += ByteCode::SIZE_OF::PUSH_R64;
              break;
            }
          case ByteCode::POP_TO_R64: {
              const auto p = ByteCode::PARSE::POP_TO_R64(code_i);

              X64::pop(out_code, p.val);
              code_i += ByteCode::SIZE_OF::POP_TO_R64;
              break;
            }
          case ByteCode::PUSH_FRAME: {
              const auto p = ByteCode::PARSE::PUSH_FRAME(code_i);

              X64::push(out_code, RBP.REG);
              X64::mov(out_code, RSP.REG, RBP.REG);

              code_i += ByteCode::SIZE_OF::PUSH_FRAME;
              break;
            }
          case ByteCode::POP_FRAME: {
              const auto p = ByteCode::PARSE::POP_FRAME(code_i);

              X64::mov(out_code, RBP.REG, RSP.REG);
              X64::pop(out_code, RBP.REG);

              code_i += ByteCode::SIZE_OF::POP_FRAME;
              break;
            }
          case ByteCode::ALLOCATE_STACK: {
              const auto p = ByteCode::PARSE::ALLOCATE_STACK(code_i);

              X64::sub(out_code, RSP.REG, (int32_t)p.u64.sig_val);

              code_i += ByteCode::SIZE_OF::ALLOCATE_STACK;
              break;
            }
          case ByteCode::COPY_R64_TO_STACK_TOP: {
              const auto i = ByteCode::PARSE::COPY_R64_TO_STACK_TOP(code_i);

              X64::RM rm = {};

              rm.r = RSP.REG;
              rm.indirect = true;
              rm.use_sib = false;
              rm.disp = (int32_t)i.u64.sig_val;

              X64::mov(out_code, X64::R{ i.val }, rm);

              code_i += ByteCode::SIZE_OF::COPY_R64_TO_STACK_TOP;
              break;
            }
          case ByteCode::COPY_64_TO_STACK_TOP: {
              const auto i = ByteCode::PARSE::COPY_64_TO_STACK_TOP(code_i);

              const uint32_t low = i.u64_1.val & 0xFFFFFFFF;
              const uint32_t high = (i.u64_1.val >> (8 * 4)) & 0xFFFFFFFF;

              X64::RM rm = {};

              rm.r = RSP.REG;
              rm.indirect = true;
              rm.use_sib = false;
              rm.disp = (int32_t)i.u64_2.sig_val + 4;

              X64::mov(out_code, rm, high);

              rm.disp = (int32_t)i.u64_2.sig_val;

              X64::mov(out_code, rm, low);

              code_i += ByteCode::SIZE_OF::COPY_64_TO_STACK_TOP;
              break;
            }
          case ByteCode::COPY_R64_TO_STACK: {
              const auto i = ByteCode::PARSE::COPY_R64_TO_STACK(code_i);

              X64::RM rm = {};

              rm.r = RBP.REG;
              rm.indirect = true;
              rm.use_sib = false;
              rm.disp = (int32_t)i.u64.sig_val;

              X64::mov(out_code, X64::R{ i.val }, rm);

              code_i += ByteCode::SIZE_OF::COPY_R64_TO_STACK;
              break;
            }
          case ByteCode::COPY_64_TO_STACK: {
              const auto i = ByteCode::PARSE::COPY_64_TO_STACK(code_i);

              const uint32_t low = i.u64_1.val & 0xFFFFFFFF;
              const uint32_t high = (i.u64_1.val >> (8 * 4)) & 0xFFFFFFFF;
              
              X64::RM rm = {};

              rm.r = RBP.REG;
              rm.indirect = true;
              rm.use_sib = false;
              rm.disp = (int32_t)i.u64_2.sig_val + 4;

              X64::mov(out_code, rm, high);

              rm.disp = (int32_t)i.u64_2.sig_val;

              X64::mov(out_code, rm, low);


              code_i += ByteCode::SIZE_OF::COPY_64_TO_STACK;
              break;
            }
          case ByteCode::COPY_R64_FROM_STACK: {
              const auto i = ByteCode::PARSE::COPY_R64_FROM_STACK(code_i);

              X64::RM rm = {};

              rm.r = RBP.REG;
              rm.indirect = true;
              rm.use_sib = false;
              rm.disp = (int32_t)i.u64.sig_val;

              X64::mov(out_code, rm, X64::R{ i.val });

              code_i += ByteCode::SIZE_OF::COPY_R64_FROM_STACK;
              break;
            }
          case ByteCode::COPY_R64_FROM_MEM: {
              const auto i = ByteCode::PARSE::COPY_R64_FROM_MEM(code_i);

              X64::RM rm = {};

              rm.r = i.val2;
              rm.indirect = true;
              rm.use_sib = false;
              rm.disp = 0;

              X64::mov(out_code, rm, X64::R{ i.val1 });

              code_i += ByteCode::SIZE_OF::COPY_R64_FROM_MEM;
              break;
            }
          case ByteCode::COPY_R64_FROM_MEM_COMPLEX: {
              const auto i = ByteCode::PARSE::COPY_R64_FROM_MEM_COMPLEX(code_i);

              X64::RM rm = {};

              rm.r = RSP.REG;
              rm.indirect = true;
              rm.use_sib = true;

              //use SIB
              rm.sib.base = i.mem.base;
              rm.sib.index = i.mem.index;
              rm.sib.scale = i.mem.scale;
              rm.sib.disp = i.mem.disp;

              X64::mov(out_code, rm, X64::R{ i.val });

              code_i += ByteCode::SIZE_OF::COPY_R64_FROM_MEM_COMPLEX;
              break;
            }
          case ByteCode::CONV_RU8_TO_R64: {
              const auto i = ByteCode::PARSE::CONV_RU8_TO_R64(code_i);

              out_code.insert(X64::REX_W | X64::rex_r_rm(i.val, i.val));
              out_code.insert(0x0F);
              out_code.insert(X64::MOV_ZX_RM8_TO_R);
              out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(i.val, i.val));

              code_i += ByteCode::SIZE_OF::CONV_RU8_TO_R64;
              break;
            }
          case ByteCode::CONV_RI8_TO_R64: {
              const auto i = ByteCode::PARSE::CONV_RI8_TO_R64(code_i);

              out_code.insert(X64::REX_W | X64::rex_r_rm(i.val, i.val));
              out_code.insert(0x0F);
              out_code.insert(X64::MOV_SX_RM8_TO_R);
              out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(i.val, i.val));

              code_i += ByteCode::SIZE_OF::COPY_R64_FROM_STACK;
              break;
            }
          case ByteCode::LABEL: {
              const auto p = ByteCode::PARSE::LABEL(code_i);

              label_indexes[p.u64.val] = out_code.size;

              code_i += ByteCode::SIZE_OF::LABEL;
              break;
            }
          case ByteCode::RETURN: {
              X64::ret(out_code);
              code_i += ByteCode::SIZE_OF::RETURN;
              break;
            }
          case ByteCode::CALL: {
              const auto p = ByteCode::PARSE::CALL(code_i);

              instruction_offsets.insert(out_code.size);
              call_near(out_code, (int32_t)p.u64.val);

              code_i += ByteCode::SIZE_OF::CALL;
              break;
            }
          case ByteCode::JUMP_TO_FIXED: {
              const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED(code_i);
              code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED;

              //Can ignore anything between a fixed jump and a label - it wont every be reached
              while (code_i < code_end && code_i[0] != ByteCode::LABEL) {
                code_i += ByteCode::instruction_size(code_i[0]);
              }

              size_t next = 0;

              //Could be series of labels so needs to while loop
              while (code_i + next < code_end && code_i[next] == ByteCode::LABEL) {
                const auto p_l = ByteCode::PARSE::LABEL(code_i + next);

                if (p_l.u64.val == p_j.u64.val) {
                  //About to jump to next instruction - can ignore jump
                  goto SKIP_JUMP;
                }

                //Next might also be a label
                next += ByteCode::SIZE_OF::LABEL;
              }

              instruction_offsets.insert(out_code.size);
              jump_near(out_code, (int32_t)p_j.u64.val);

            SKIP_JUMP:
              break;
            }
          case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
              const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

              //need to recompare
              cmp(out_code, p.val, (int32_t)0);

              instruction_offsets.insert(out_code.size);
              jump_zero(out_code, (int32_t)p.u64.val);

              code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
              break;
            }
          case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
              const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

              //need to recompare
              cmp(out_code, p.val, (int32_t)0);

              instruction_offsets.insert(out_code.size);
              jump_not_equal(out_code, (int32_t)p.u64.val);

              code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
              break;
            }
          default: {
              printf("INTERNAL ERROR: Backend found unsupported bytecode instruction\n"
                     "                Instruction: %d\n", (int)*code_i);
              return -1;
            }
        }
      }
    }
  }

  {
    auto jumps_i = instruction_offsets.begin();
    const auto jumps_end = instruction_offsets.end();

    for (; jumps_i < jumps_end; jumps_i++) {
      size_t index = *jumps_i;

      if (out_code.data[index] == 0x0F) index++;

      //jumps are all from the end of the instruction
      //all jumps are a op + imm32 (so far)

      const uint64_t j_index = x32_from_bytes(out_code.data + index + 1);
      const int32_t jump = (int32_t)label_indexes[j_index] - (int32_t)(index + 1 + sizeof(int32_t));

      x32_to_bytes(jump, out_code.data + index + 1);
    }
  }

  size_t entry_point_index = -1;
  if (entry_point_label <= comp->labels) {
    entry_point_index = label_indexes[entry_point_label];
  }

  free_no_destruct(label_indexes);
  return entry_point_index;
}

struct RegisterNames {
  OwnedPtr<char> r;
  OwnedPtr<char> rm;
};

const char* b8_no_rex_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "AL";
    case 1: return "CL";
    case 2: return "DL";
    case 3: return "BL";
    case 4: return "AH";
    case 5: return "CH";
    case 6: return "DH";
    case 7: return "BH";
  }

  return "INVALID REGISTER";
}

const char* b8_rex_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "AL";
    case 1: return "CL";
    case 2: return "DL";
    case 3: return "BL";
    case 4: return "SPL";
    case 5: return "BPL";
    case 6: return "SIL";
    case 7: return "DIL";
    case 8: return "R8L";
    case 9: return "R9L";
    case 10: return "R10L";
    case 11: return "R11L";
    case 12: return "R12L";
    case 13: return "R13L";
    case 14: return "R14L";
    case 15: return "R15L";
  }

  return "INVALID REGISTER";
}

const char* b32_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "EAX";
    case 1: return "ECX";
    case 2: return "EDX";
    case 3: return "EBX";
    case 4: return "ESP";
    case 5: return "EBP";
    case 6: return "ESI";
    case 7: return "EDI";
    case 8: return "R8D";
    case 9: return "R9D";
    case 10: return "R10D";
    case 11: return "R11D";
    case 12: return "R12D";
    case 13: return "R13D";
    case 14: return "R14D";
    case 15: return "R15D";
  }

  return "INVALID REGISTER";
}

struct x86PrintOptions {
  bool short_operand = false;
  FUNCTION_PTR<const char*, uint8_t> r_name;
  FUNCTION_PTR<const char*, uint8_t> rm_name;
};



static OwnedPtr<char> rm_reg_string(x86PrintOptions* const p_opts,
                                    uint8_t rex, uint8_t modrm, const uint8_t** rest) {
  uint8_t address_mode = (modrm & 0b11'000000) >> 6;
  uint8_t rm = modrm & X64::MODRM_RM_MASK;

  if ((modrm & 0b11'000000) == 0b11'000000) {
    rm |= ((rex & X64::REX_B) << X64::REX_B_SHIFT);

    return format("{}", p_opts->rm_name(rm));
  }

  constexpr auto SIZE = [](bool short_operand)->const char* {
    if (short_operand) {
      return "WORD PTR";
    }
    else {
      return "DWORD PTR";
    }
  };

  const char* const size_operand = SIZE(p_opts->short_operand);

  switch (rm) {
    case RSP.REG: {
        //SIB byte time
        const uint8_t sib = *(*rest)++;

        const uint8_t scale = 1 << ((sib & 0b11'000'000) >> 6);
        const uint8_t index = ((rex & X64::REX_X) << 2) | ((sib & X64::SIB_INDEX_MASK) >> 3);
        const uint8_t base  = ((rex & X64::REX_B) << 3) | ((sib & X64::SIB_BASE_MASK));

        const bool INDEX_RSP = index == RSP.REG;
        const bool BASE_RBP  = (base & 0b111) == 0b101;

        switch (address_mode) {
          case 0b00: {
              if (INDEX_RSP && BASE_RBP) {
                int32_t disp = x32_from_bytes(*rest);
                *rest += 4;

                return format("{} [{}]", size_operand, disp);
              }
              else if (INDEX_RSP) {
                return format("{} [{}]", size_operand, p_opts->rm_name(base));
              }
              else if (BASE_RBP) {
                int32_t disp = x32_from_bytes(*rest);
                *rest += 4;

                char sign = disp >= 0 ? '+' : '-';

                return format("{} [({} * {}) {} {}]", size_operand, p_opts->rm_name(index), scale, sign, absolute(disp));
              }
              else {
                return format("{} [{} + ({} * {})]",
                              size_operand,
                              p_opts->rm_name(base),
                              p_opts->rm_name(index),
                              scale);
              }
            }
          case 0b01: {
              const uint8_t disp = *(*rest)++;

              char sign = disp >= 0 ? '+' : '-';

              if (INDEX_RSP) {
                return format("{} [{} {} {}]",
                              size_operand,
                              p_opts->rm_name(base),
                              sign, absolute(disp));
              }
              else {
                return format("{} [{} + ({} * {}) {} {}]",
                              size_operand,
                              p_opts->rm_name(base),
                              p_opts->rm_name(index), scale,
                              sign, absolute(disp));
              }
            }
          case 0b10: {
              int32_t disp = x32_from_bytes(*rest);
              *rest += 4;

              char sign = disp >= 0 ? '+' : '-';

              if (INDEX_RSP) {
                return format("{} [{} {} {}]",
                              size_operand,
                              p_opts->rm_name(base),
                              sign, absolute(disp));
              }
              else {
                return format("{} [{} + ({} * {}) {} {}]",
                              size_operand,
                              p_opts->rm_name(base),
                              p_opts->rm_name(index), scale,
                              sign, absolute(disp));
              }
            }
        }

        throw std::exception("Internal error, should not be here");
      }
    case RBP.REG: {
        if (address_mode == 0b00) {
          int32_t disp = x32_from_bytes(*rest);
          *rest += 4;

          char sign = disp >= 0 ? '+' : '-';

          return format("{} [RIP {} {}]", size_operand, sign, absolute(disp));
        }

        goto NORMAL_MODRM;
      }
    default: {
      NORMAL_MODRM:
        rm |= ((rex & X64::REX_B) << X64::REX_B_SHIFT);

        switch (address_mode) {
          case 0b00: {
              return format("{} [{}]", size_operand, p_opts->rm_name(rm));
            }
          case 0b01: {
              int8_t disp = *(*rest)++;

              char sign = disp >= 0 ? '+' : '-';

              return format("{} [{} {} {}]", size_operand, p_opts->rm_name(rm), sign, absolute(disp));
            }
          case 0b10: {
              int32_t disp = x32_from_bytes(*rest);
              *rest += 4;

              char sign = disp >= 0 ? '+' : '-';

              return format("{} [{} {} {}]", size_operand, p_opts->rm_name(rm), sign, absolute(disp));
            }
        }

        throw std::exception("Internal error, should not be here");
      }
  }

  throw std::exception("Internal error, should not be here");
}

static OwnedPtr<char> r_reg_string(x86PrintOptions* p_opts,
                                   uint8_t rex, uint8_t modrm) {
  uint8_t r = ((rex & X64::REX_R) << X64::REX_R_SHIFT)
    | ((modrm & X64::MODRM_REG_MASK) >> X64::MODRM_REG_SHIFT);

  return  format("{}", p_opts->r_name(r));
}


static RegisterNames register_names(x86PrintOptions* p_opts,
                                    uint8_t rex, uint8_t modrm, const uint8_t** rest) {
  return { r_reg_string(p_opts, rex, modrm), rm_reg_string(p_opts, rex, modrm, rest) };
}

void print_x86_64(const uint8_t* machine_code, size_t size) {
  const uint8_t* bytes = machine_code;
  const uint8_t* const end = machine_code + size;

  x86PrintOptions p_opts ={};

  while (bytes < end) {
    printf("0x%-4llx: ", bytes - machine_code);

    p_opts.short_operand = bytes[0] == 0x66;
    if (p_opts.short_operand) {
      bytes++;
    }

    const uint8_t maybe_rex = *bytes++;
    if ((maybe_rex & 0b1111'1000) == X64::REX_W) {
      //REX_W instruction
      const uint8_t op = *bytes++;
      switch (op) {
        case X64::ADD_R_TO_RM: {
            uint8_t modrm = *bytes++;

            p_opts.r_name = x86_64_reg_name_from_num;
            p_opts.rm_name = x86_64_reg_name_from_num;

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("add %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::OR_R_TO_RM: {
            uint8_t modrm = *bytes++;

            p_opts.r_name = x86_64_reg_name_from_num;
            p_opts.rm_name = x86_64_reg_name_from_num;

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("or  %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::AND_R_TO_RM: {
            uint8_t modrm = *bytes++;

            p_opts.r_name = x86_64_reg_name_from_num;
            p_opts.rm_name = x86_64_reg_name_from_num;

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("and %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::SUB_R_TO_RM: {
            uint8_t modrm = *bytes++;

            p_opts.r_name = x86_64_reg_name_from_num;
            p_opts.rm_name = x86_64_reg_name_from_num;

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("sub %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::CMP_R_TO_RM: {
            uint8_t modrm = *bytes++;

            p_opts.r_name = x86_64_reg_name_from_num;
            p_opts.rm_name = x86_64_reg_name_from_num;

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("cmp %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::PUSH_R:
        case (X64::PUSH_R + 1):
        case (X64::PUSH_R + 2):
        case (X64::PUSH_R + 3):
        case (X64::PUSH_R + 4):
        case (X64::PUSH_R + 5):
        case (X64::PUSH_R + 6):
        case (X64::PUSH_R + 7): {
            const uint8_t reg = (op - X64::PUSH_R) | ((maybe_rex & 0b00000100) << 1);
            const char* r_string = x86_64_reg_name_from_num(reg);

            printf("push %s\n", r_string);
            break;
          }
        case X64::POP_R:
        case (X64::POP_R + 1):
        case (X64::POP_R + 2):
        case (X64::POP_R + 3):
        case (X64::POP_R + 4):
        case (X64::POP_R + 5):
        case (X64::POP_R + 6):
        case (X64::POP_R + 7): {
            const uint8_t reg = (op - X64::POP_R) | ((maybe_rex & 0b00000100) << 1);
            const char* r_string = x86_64_reg_name_from_num(reg);

            printf("pop %s\n", r_string);
            break;
          }
        case 0x81: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = x86_64_reg_name_from_num;

            OwnedPtr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            int32_t imm32 = x32_from_bytes(bytes);
            bytes += 4;

            uint8_t r_val = (modrm & 0b0011'1000) >> 3;

            if (r_val == 5) {
              printf("sub %s, 0x%x\n", rm_string.ptr, imm32);
            }
            else if (r_val == 7) {
              printf("cmp %s, 0x%x\n", rm_string.ptr, imm32);
            }
            else {
              printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx.2 0x%.2hhx ...\n",
                     maybe_rex, op, modrm);

              return;
            }
            break;
          }
        case X64::MOV_R_TO_RM: {
            uint8_t modrm = *bytes++;

            p_opts.r_name = x86_64_reg_name_from_num;
            p_opts.rm_name = x86_64_reg_name_from_num;

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("mov %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::MOV_RM_TO_R: {
            uint8_t modrm = *bytes++;

            p_opts.r_name = x86_64_reg_name_from_num;
            p_opts.rm_name = x86_64_reg_name_from_num;

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("mov %s, %s\n", names.r.ptr, names.rm.ptr);
            break;
          }
        case X64::CQO: {
            printf("cqo\n");
            break;
          }
        case 0x0F: {
            uint8_t op2 = *bytes++;
            switch (op2) {
              case X64::IMUL_RM_TO_R: {
                  uint8_t modrm = *bytes++;

                  p_opts.r_name = x86_64_reg_name_from_num;
                  p_opts.rm_name = x86_64_reg_name_from_num;

                  RegisterNames names =  register_names(&p_opts, maybe_rex, modrm, &bytes);

                  printf("imul %s, %s\n", names.r.ptr, names.rm.ptr);
                  break;
                }
              case X64::MOV_ZX_RM8_TO_R: {
                  uint8_t modrm = *bytes++;

                  p_opts.r_name = x86_64_reg_name_from_num;
                  p_opts.rm_name = b8_rex_reg_name;

                  RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

                  printf("movzx %s, %s\n", names.r.ptr, names.rm.ptr);
                  break;
                }
              case X64::MOV_SX_RM8_TO_R: {
                  uint8_t modrm = *bytes++;

                  p_opts.r_name = x86_64_reg_name_from_num;
                  p_opts.rm_name = x86_64_reg_name_from_num;

                  RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

                  printf("movsx %s, %s\n", names.r.ptr, names.rm.ptr);
                  break;
                }
              default: {
                  printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                         maybe_rex, op, op2);

                  return;
                }
            }
            break;
          }
        case X64::MOV_64_TO_R:
        case (X64::MOV_64_TO_R + 1):
        case (X64::MOV_64_TO_R + 2):
        case (X64::MOV_64_TO_R + 3):
        case (X64::MOV_64_TO_R + 4):
        case (X64::MOV_64_TO_R + 5):
        case (X64::MOV_64_TO_R + 6):
        case (X64::MOV_64_TO_R + 7): {
            const uint8_t reg = (op - X64::MOV_64_TO_R) | ((maybe_rex & 0b0000'0001) << 3);
            const char* r_string = x86_64_reg_name_from_num(reg);

            uint64_t imm64 = x64_from_bytes(bytes);
            bytes += 8;

            printf("mov %s, 0x%llx\n", r_string, imm64);
            break;
          }
        case 0xF7: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = x86_64_reg_name_from_num;

            const uint8_t r = (modrm & 0b0011'1000) >> 3;
            OwnedPtr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            if (r == 3) {
              printf("neg %s\n", rm_string.ptr);
            }
            else if (r == 4) {
              printf("mul %s\n", rm_string.ptr);
            }
            else if (r == 6) {
              printf("div %s\n", rm_string.ptr);
            }
            else if (r == 7) {
              printf("idiv %s\n", rm_string.ptr);
            }
            else {
              printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                     maybe_rex, op, modrm);

              return;
            }
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx\n",
                   maybe_rex, op);

            return;
          }
      }
    }
    else if ((maybe_rex & 0b1111'1000) == X64::REX) {
      uint8_t op = *bytes++;
      switch (op) {
        case X64::MOV_IMM32_RM: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = x86_64_reg_name_from_num;

            OwnedPtr<char> rm = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            uint32_t val = x32_from_bytes(bytes);
            bytes += 4;

            printf("mov %s, %u\n", rm.ptr, val);
            break;
          }
        case 0x0F: {
            uint8_t op2 = *bytes++;
            switch (op2) {
              case X64::SETE_RM8: {
                  uint8_t modrm = *bytes++;

                  p_opts.rm_name = x86_64_reg_name_from_num;

                  OwnedPtr<char> r_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);
                  printf("sete %s\n", r_string.ptr);
                  break;
                }
              default: {
                  printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                         maybe_rex, op, op2);

                  return;
                }
            }
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx\n",
                   maybe_rex, op);

            return;
          }
      }
    }
    else if (maybe_rex == 0x0F) {
      //0xFF instructions
      uint8_t op = *bytes++;
      switch (op) {
        case X64::JZ_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jz 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JNE_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jne 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::SETE_RM8: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = b8_rex_reg_name;

            OwnedPtr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &bytes);
            printf("sete %s\n", r_string.ptr);
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx\n",
                   maybe_rex, op);

            return;
          }

      }

    }
    else {
      //Non-REX instruction
      switch (maybe_rex) {
        case X64::PUSH_R:
        case (X64::PUSH_R + 1):
        case (X64::PUSH_R + 2):
        case (X64::PUSH_R + 3):
        case (X64::PUSH_R + 4):
        case (X64::PUSH_R + 5):
        case (X64::PUSH_R + 6):
        case (X64::PUSH_R + 7): {
            const char* r_string = x86_64_reg_name_from_num(maybe_rex - X64::PUSH_R);

            printf("push %s\n", r_string);
            break;
          }
        case X64::POP_R:
        case (X64::POP_R + 1):
        case (X64::POP_R + 2):
        case (X64::POP_R + 3):
        case (X64::POP_R + 4):
        case (X64::POP_R + 5):
        case (X64::POP_R + 6):
        case (X64::POP_R + 7): {
            const char* r_string = x86_64_reg_name_from_num(maybe_rex - X64::POP_R);

            printf("pop %s\n", r_string);
            break;
          }
        case X64::JMP_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jmp 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::RET_NEAR: {
            printf("ret\n");
            break;
          }
        case X64::CALL_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("call 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::MOV_IMM32_RM: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = x86_64_reg_name_from_num;

            OwnedPtr<char> rm = rm_reg_string(&p_opts, 0, modrm, &bytes);

            uint32_t val = x32_from_bytes(bytes);
            bytes += 4;

            printf("mov %s, %u\n", rm.ptr, val);
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx\n",
                   maybe_rex);

            return;
          }
      }
    }
  }
}