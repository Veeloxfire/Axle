#include "backends.h"
#include "compiler.h"
#include "calling_conventions.h"
#include "format.h"

#include <stdio.h>

size_t vm_backend_single_func(Array<uint8_t>& out_code, const Function* func, uint64_t labels) {
  size_t* const label_indexes = allocate_default<size_t>(labels);
  Array<size_t> instruction_offsets ={};

  size_t entry_point_label = func->label;

  {
    auto code_i = func->code.begin();
    const auto code_end = func->code.end();

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

  free<size_t>(label_indexes);
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

      auto code_i = func->code.begin();
      const auto code_end = func->code.end();

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

  free<size_t>(label_indexes);
  return entry_point_index;
}

struct R {
  uint8_t r;
};

struct RM {
  uint8_t r;

  bool indirect = false;
  int32_t disp = 0;
};

inline static void mov(Array<uint8_t>& arr,
                       uint8_t from,
                       uint8_t to) {
  arr.insert(X64::REX_W | X64::rex_r_rm(from, to));
  arr.insert(X64::MOV_R_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from, to));
}

inline static void emit_mod_rm(Array<uint8_t>& arr, R r, RM rm) {
  if (!rm.indirect) {
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(r.r, rm.r));
    return;
  }

  switch (rm.r) {
    case RSP.REG:
    case R12.REG: {
        //SIB byte time

        if (rm.disp == 0) {
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

inline static void mov(Array<uint8_t>& arr,
                       R r,
                       RM rm) {

  arr.insert(X64::REX_W | X64::rex_r_rm(r.r, rm.r));
  arr.insert(X64::MOV_R_TO_RM);

  emit_mod_rm(arr, r, rm);
}

inline static void mov(Array<uint8_t>& arr,
                       RM rm,
                       R r) {
  arr.insert(X64::REX_W | X64::rex_r_rm(r.r, rm.r));
  arr.insert(X64::MOV_RM_TO_R);

  emit_mod_rm(arr, r, rm);
}

inline static void mov(Array<uint8_t>& arr,
                       uint8_t r,
                       uint64_t u64) {
  arr.insert(X64::REX_W | X64::rex_b(r));
  arr.insert(X64::MOV_64_TO_R + (r & 0b111));

  arr.reserve_extra(sizeof(uint64_t));
  x64_to_bytes(u64, arr.data + arr.size);
  arr.size += sizeof(uint64_t);
}

inline static void sub(Array<uint8_t>& arr,
                       uint8_t r,
                       uint8_t rm) {
  arr.insert(X64::REX_W | X64::rex_r_rm(r, rm));
  arr.insert(X64::SUB_R_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT
             | X64::modrm_r_rm(r, rm));
}

inline static void sub(Array<uint8_t>& arr,
                       uint8_t rm,
                       int32_t i32) {
  arr.insert(X64::REX_W | X64::rex_rm(rm));
  arr.insert(X64::SUB_32_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(5, rm));

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(i32, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

inline static void push(Array<uint8_t>& arr, uint8_t reg) {
  if ((reg & 0b0000'1000) > 0) {
    arr.insert(X64::REX_W | X64::REX_B);
  }

  arr.insert(X64::PUSH_R + (reg & 0b0000'0111));
}

inline static void pop(Array<uint8_t>& arr, uint8_t reg) {
  if ((reg & 0b0000'1000) > 0) {
    arr.insert(X64::REX_W | X64::REX_B);
  }

  arr.insert(X64::POP_R + (reg & 0b0000'0111));
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

      auto code_i = func->code.begin();
      const auto code_end = func->code.end();

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
              mov(out_code, p.val1, p.val2);
              code_i += ByteCode::SIZE_OF::COPY_R64_TO_R64;
              break;
            }
          case ByteCode::SET_R64_TO_64: {
              const auto p = ByteCode::PARSE::SET_R64_TO_64(code_i);
              mov(out_code, p.val, p.u64);
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

              sub(out_code, p.val1, p.val2);

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
              mov(out_code, RDX.REG, 0ull);

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

              push(out_code, p.val);
              code_i += ByteCode::SIZE_OF::PUSH_R64;
              break;
            }
          case ByteCode::POP_TO_R64: {
              const auto p = ByteCode::PARSE::POP_TO_R64(code_i);

              pop(out_code, p.val);
              code_i += ByteCode::SIZE_OF::POP_TO_R64;
              break;
            }
          case ByteCode::PUSH_FRAME: {
              const auto p = ByteCode::PARSE::PUSH_FRAME(code_i);

              push(out_code, RBP.REG);
              mov(out_code, RSP.REG, RBP.REG);

              code_i += ByteCode::SIZE_OF::PUSH_FRAME;
              break;
            }
          case ByteCode::POP_FRAME: {
              const auto p = ByteCode::PARSE::POP_FRAME(code_i);

              mov(out_code, RBP.REG, RSP.REG);
              pop(out_code, RBP.REG);

              code_i += ByteCode::SIZE_OF::POP_FRAME;
              break;
            }
          case ByteCode::ALLOCATE_STACK: {
              const auto p = ByteCode::PARSE::ALLOCATE_STACK(code_i);

              sub(out_code, RSP.REG, (int32_t)p.u64.sig_val);

              code_i += ByteCode::SIZE_OF::ALLOCATE_STACK;
              break;
            }
          case ByteCode::COPY_R64_TO_STACK_TOP: {
              const auto i = ByteCode::PARSE::COPY_R64_TO_STACK_TOP(code_i);

              mov(out_code, R{ i.val }, RM{ RSP.REG, true, (int32_t)i.u64.sig_val });

              code_i += ByteCode::SIZE_OF::COPY_R64_TO_STACK_TOP;
              break;
            }
          case ByteCode::COPY_R64_TO_STACK: {
              const auto i = ByteCode::PARSE::COPY_R64_TO_STACK(code_i);

              mov(out_code, R{ i.val }, RM{ RBP.REG, true, (int32_t)i.u64.sig_val });

              code_i += ByteCode::SIZE_OF::COPY_R64_TO_STACK;
              break;
            }
          case ByteCode::COPY_R64_FROM_STACK: {
              const auto i = ByteCode::PARSE::COPY_R64_FROM_STACK(code_i);

              mov(out_code, RM{ RBP.REG, true, (int32_t)i.u64.sig_val }, R{ i.val });

              code_i += ByteCode::SIZE_OF::COPY_R64_FROM_STACK;
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
              out_code.insert(X64::RET_NEAR);
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

  free<size_t>(label_indexes);
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

static OwnedPtr<char> rm_reg_string(const FUNCTION_PTR<const char*, uint8_t> rm_name,
                                    uint8_t rex, uint8_t modrm, const uint8_t** rest) {
  uint8_t address_mode = (modrm & 0b11'000000) >> 6;
  uint8_t rm = modrm & X64::MODRM_RM_MASK;

  if ((modrm & 0b11'000000) == 0b11'000000) {
    rm |= ((rex & X64::REX_B) << X64::REX_B_SHIFT);

    return format("{}", rm_name(rm));
  }

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

                return format("[{}]", disp);
              }
              else if (INDEX_RSP) {
                return format("[{}]", rm_name(base));
              }
              else if (BASE_RBP) {
                int32_t disp = x32_from_bytes(*rest);
                *rest += 4;

                char sign = disp >= 0 ? '+' : '-';

                return format("[({} * {}) {} {}]", rm_name(index), scale, sign, absolute(disp));
              }
              else {
                return format("[{} + ({} * {})]",
                              rm_name(base),
                              rm_name(index),
                              scale);
              }
            }
          case 0b01: {
              const uint8_t disp = *(*rest)++;

              char sign = disp >= 0 ? '+' : '-';

              if (INDEX_RSP) {
                return format("[{} {} {}]",
                              rm_name(base),
                              sign, absolute(disp));
              }
              else {
                return format("[{} + ({} * {}) {} {}]",
                              rm_name(base),
                              rm_name(index), scale,
                              sign, absolute(disp));
              }
            }
          case 0b10: {
              int32_t disp = x32_from_bytes(*rest);
              *rest += 4;

              char sign = disp >= 0 ? '+' : '-';

              if (INDEX_RSP) {
                return format("[{} {} {}]",
                              rm_name(base),
                              sign, absolute(disp));
              }
              else {
                return format("[{} + ({} * {}) {} {}]",
                              rm_name(base),
                              rm_name(index), scale,
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

          return format("[RIP {} {}]", sign, absolute(disp));
        }

        goto NORMAL_MODRM;
      }
    default: {
      NORMAL_MODRM:
        rm |= ((rex & X64::REX_B) << X64::REX_B_SHIFT);

        switch (address_mode) {
          case 0b00: {
              return format("[{}]", rm_name(rm));
            }
          case 0b01: {
              int8_t disp = *(*rest)++;

              char sign = disp >= 0 ? '+' : '-';

              return format("[{} {} {}]", rm_name(rm), sign, absolute(disp));
            }
          case 0b10: {
              int32_t disp = x32_from_bytes(*rest);
              *rest += 4;

              char sign = disp >= 0 ? '+' : '-';

              return format("[{} {} {}]", rm_name(rm), sign, absolute(disp));
            }
        }

        throw std::exception("Internal error, should not be here");
      }
  }

  throw std::exception("Internal error, should not be here");
}

static OwnedPtr<char> r_reg_string(const FUNCTION_PTR<const char*, uint8_t> r_name,
                                   uint8_t rex, uint8_t modrm) {
  uint8_t r = ((rex & X64::REX_R) << X64::REX_R_SHIFT)
    | ((modrm & X64::MODRM_REG_MASK) >> X64::MODRM_REG_SHIFT);

  return  format("{}", r_name(r));
}


static RegisterNames register_names(const FUNCTION_PTR<const char*, uint8_t> r_name,
                                    const FUNCTION_PTR<const char*, uint8_t> rm_name,
                                    uint8_t rex, uint8_t modrm, const uint8_t** rest) {

  return { r_reg_string(r_name, rex, modrm), rm_reg_string(rm_name, rex, modrm, rest) };
}

static RegisterNames b64_register_names(uint8_t rex, uint8_t modrm, const uint8_t** rest) {
  return register_names(x86_64_reg_name_from_num, x86_64_reg_name_from_num,
                        rex, modrm, rest);
}

static RegisterNames register_names_R64_M8(uint8_t rex, uint8_t modrm, const uint8_t** rest) {
  return register_names(x86_64_reg_name_from_num, b8_rex_reg_name,
                        rex, modrm, rest);
}

static OwnedPtr<char> b64_register_name_M(uint8_t rex, uint8_t modrm, const uint8_t** rest) {
  return rm_reg_string(x86_64_reg_name_from_num, rex, modrm, rest);
}

static OwnedPtr<char> b64_register_name_R(uint8_t rex, const uint8_t modrm) {
  return r_reg_string(x86_64_reg_name_from_num, rex, modrm);
}

static OwnedPtr<char> b8_register_name_M(uint8_t modrm, const uint8_t** rest) {
  return rm_reg_string(b8_no_rex_reg_name, 0, modrm, rest);
}

static OwnedPtr<char> b8_register_name_M(uint8_t rex, uint8_t modrm, const uint8_t** rest) {
  return rm_reg_string(b8_no_rex_reg_name, rex, modrm, rest);
}

void print_x86_64(const uint8_t* machine_code, size_t size) {
  const uint8_t* bytes = machine_code;
  const uint8_t* const end = machine_code + size;

  while (bytes < end) {
    printf("0x%-4llx: ", bytes - machine_code);

    const uint8_t maybe_rex = *bytes++;
    if ((maybe_rex & 0b1111'1000) == X64::REX_W) {
      //REX_W instruction
      const uint8_t op = *bytes++;
      switch (op) {
        case X64::ADD_R_TO_RM: {
            uint8_t modrm = *bytes++;

            RegisterNames names = b64_register_names(maybe_rex, modrm, &bytes);

            printf("add %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::OR_R_TO_RM: {
            uint8_t modrm = *bytes++;

            RegisterNames names = b64_register_names(maybe_rex, modrm, &bytes);

            printf("or  %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::AND_R_TO_RM: {
            uint8_t modrm = *bytes++;

            RegisterNames names = b64_register_names(maybe_rex, modrm, &bytes);

            printf("and %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::SUB_R_TO_RM: {
            uint8_t modrm = *bytes++;

            RegisterNames names = b64_register_names(maybe_rex, modrm, &bytes);

            printf("sub %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::CMP_R_TO_RM: {
            uint8_t modrm = *bytes++;

            RegisterNames names = b64_register_names(maybe_rex, modrm, &bytes);

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

            OwnedPtr<char> rm_string = b64_register_name_M(maybe_rex, modrm, &bytes);

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

            RegisterNames names = b64_register_names(maybe_rex, modrm, &bytes);

            printf("mov %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::MOV_RM_TO_R: {
            uint8_t modrm = *bytes++;

            RegisterNames names = b64_register_names(maybe_rex, modrm, &bytes);

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

                  RegisterNames names = b64_register_names(maybe_rex, modrm, &bytes);

                  printf("imul %s, %s\n", names.r.ptr, names.rm.ptr);
                  break;
                }
              case X64::MOV_ZX_RM8_TO_R: {
                  uint8_t modrm = *bytes++;

                  RegisterNames names = register_names_R64_M8(maybe_rex, modrm, &bytes);

                  printf("movzx %s, %s\n", names.r.ptr, names.rm.ptr);
                  break;
                }
              case X64::MOV_SX_RM8_TO_R: {
                  uint8_t modrm = *bytes++;

                  RegisterNames names = register_names_R64_M8(maybe_rex, modrm, &bytes);

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

            const uint8_t r = (modrm & 0b0011'1000) >> 3;
            OwnedPtr<char> rm_string = b64_register_name_M(maybe_rex, modrm, &bytes);

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
        case 0x0F: {
            uint8_t op2 = *bytes++;
            switch (op2) {
              case X64::SETE_RM8: {
                  uint8_t modrm = *bytes++;
                  OwnedPtr<char> r_string = b8_register_name_M(maybe_rex, modrm, &bytes);
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

            OwnedPtr<char> r_string = b8_register_name_M(modrm, &bytes);
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
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx\n",
                   maybe_rex);

            return;
          }
      }
    }
  }
}