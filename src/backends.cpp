#include "backends.h"
#include "compiler.h"

size_t vm_backend(Array<uint8_t>& out_code, const Compiler* comp) {
  size_t* const label_indexes = allocate_zerod<size_t>(comp->labels);
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
        if (func->name == comp->build_options.entry_point
            && func->parameter_types.size == 0) {
          entry_point_label = p.u64.val;
        }

        code_i += ByteCode::SIZE_OF::LABEL;
      }

      while (code_i < code_end) {
        switch (*code_i) {
          case ByteCode::LABEL: {
              const auto p = ByteCode::PARSE::LABEL(code_i);

              label_indexes[p.u64.val] = out_code.size;

              code_i += ByteCode::SIZE_OF::LABEL;
              break;
            }
          case ByteCode::JUMP_TO_FIXED: {
              //Can remove forced jumps if next thing is the a label
              const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED(code_i);
              size_t next = p_j.INSTRUCTION_SIZE;

              //Could be series of labels so needs to while loop
              while (code_i + next < code_end && code_i[next] == ByteCode::LABEL) {
                const auto p_l = ByteCode::PARSE::LABEL(code_i + next);

                if (p_l.u64.val == p_j.u64.val) {
                  //About to jump to next instruction - can remove jump
                  code_i += p_j.INSTRUCTION_SIZE;
                  goto SKIP_JUMP;
                }

                //Next might also be a label
                next += ByteCode::SIZE_OF::LABEL;
              }

              goto INSERT_JUMP;
            SKIP_JUMP:
              break;
            }
          case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO:
          case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO:
            //Could try removing these jumps maybe
            //but they shouldnt ever jump to the next instruction anyway
            //Would be checking for something that never happens
          case ByteCode::CALL:
          INSERT_JUMP:
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

inline static void mov(Array<uint8_t>& arr,
                       uint8_t from,
                       uint8_t to) {
  arr.insert(X64::REX_W | X64::rex_r_rm(from, to));
  arr.insert(X64::MOV_R_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from, to));
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

static void check_eq(Array<size_t>& instruction_offsets,
                     Array<uint8_t>& out_code,
                     const uint8_t** code_i_ptr, const uint8_t* end) {

  const uint8_t* const code_i = *code_i_ptr;
  const uint8_t* const code_i_2 = code_i + ByteCode::SIZE_OF::EQ_VALS;

  switch (*code_i) {
    case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
        const auto p_e = ByteCode::PARSE::EQ_VALS(code_i);
        const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i_2);

        out_code.insert(X64::REX_W | X64::rex_r_rm(p_e.val1, p_e.val2));
        out_code.insert(X64::CMP_R_TO_RM);
        out_code.insert(X64::MODRM_MOD_DIRECT
                        | X64::modrm_r_rm(p_e.val1, p_e.val2));

        instruction_offsets.insert(out_code.size);
        jump_zero(out_code, (int32_t)p_j.u64.val);

        *code_i_ptr = code_i_2 + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
        break;
      }
    case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
        const auto p_e = ByteCode::PARSE::EQ_VALS(code_i);
        const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i_2);

        out_code.insert(X64::REX_W | X64::rex_r_rm(p_e.val1, p_e.val2));
        out_code.insert(X64::CMP_R_TO_RM);
        out_code.insert(X64::MODRM_MOD_DIRECT
                        | X64::modrm_r_rm(p_e.val1, p_e.val2));

        instruction_offsets.insert(out_code.size);
        jump_not_equal(out_code, (int32_t)p_j.u64.val);

        *code_i_ptr = code_i_2 + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
        break;
      }

    default: {
        const auto p = ByteCode::PARSE::EQ_VALS(code_i);

        out_code.insert(X64::REX_W | X64::rex_r_rm(p.val1, p.val2));
        out_code.insert(X64::CMP_R_TO_RM);
        out_code.insert(X64::MODRM_MOD_DIRECT
                        | X64::modrm_r_rm(p.val1, p.val2));

        out_code.insert(X64::REX | X64::rex_rm(p.val2));
        out_code.insert(0x0F);
        out_code.insert(X64::SETE);
        out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_rm(p.val2));

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
          //need to recompare
          out_code.insert(X64::REX_W | X64::rex_rm(p.val));
          out_code.insert(X64::CMP_32_TO_RM);
          out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(7, p.val));

          out_code.reserve_extra(sizeof(uint32_t));
          x32_to_bytes((int32_t)0, out_code.data + out_code.size);
          out_code.size += sizeof(uint32_t);
        }


        instruction_offsets.insert(out_code.size);
        jump_zero(out_code, (int32_t)p.u64.val);

        *code_i_ptr = code_i + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
        break;
      }
    case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
        const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

        if (val != p.val) {
          //need to recompare
          out_code.insert(X64::REX_W | X64::rex_rm(p.val));
          out_code.insert(X64::CMP_32_TO_RM);
          out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(7, p.val));

          out_code.reserve_extra(sizeof(uint32_t));
          x32_to_bytes((int32_t)0, out_code.data + out_code.size);
          out_code.size += sizeof(uint32_t);
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

size_t x86_64_backend(Array<uint8_t>& out_code, const Compiler* comp) {
  size_t* const label_indexes = allocate_zerod<size_t>(comp->labels);
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
        if (func->name == comp->build_options.entry_point
            && func->parameter_types.size == 0) {
          entry_point_label = p.u64.val;
        }

        code_i += ByteCode::SIZE_OF::LABEL;
      }

      while (code_i < code_end) {
        switch (*code_i) {
          case ByteCode::COPY_TO_VAL: {
              const auto p = ByteCode::PARSE::COPY_TO_VAL(code_i);
              mov(out_code, p.val1, p.val2);
              code_i += ByteCode::SIZE_OF::COPY_TO_VAL;
              break;
            }
          case ByteCode::SET_VAL_TO_64: {
              const auto p = ByteCode::PARSE::SET_VAL_TO_64(code_i);
              out_code.insert(X64::REX_W | X64::rex_r(p.val));
              out_code.insert(X64::MOV_64_TO_R + (p.val & 0b111));

              out_code.reserve_extra(sizeof(uint64_t));
              x64_to_bytes(p.u64, out_code.data + out_code.size);
              out_code.size += sizeof(uint64_t);

              code_i += ByteCode::SIZE_OF::SET_VAL_TO_64;
              break;
            }
          case ByteCode::ADD_VALS: {
              const auto p = ByteCode::PARSE::ADD_VALS(code_i);

              out_code.insert(X64::REX_W | X64::rex_r_rm(p.val1, p.val2));
              out_code.insert(X64::ADD_R_TO_RM);
              out_code.insert(X64::MODRM_MOD_DIRECT
                              | X64::modrm_r_rm(p.val1, p.val2));

              code_i += ByteCode::SIZE_OF::ADD_VALS;

              check_for_jumps(instruction_offsets, out_code, p.val2, &code_i, code_end);
              break;
            }
          case ByteCode::SUB_VALS: {
              const auto p = ByteCode::PARSE::SUB_VALS(code_i);

              out_code.insert(X64::REX_W | X64::rex_r_rm(p.val1, p.val2));
              out_code.insert(X64::SUB_R_TO_RM);
              out_code.insert(X64::MODRM_MOD_DIRECT
                              | X64::modrm_r_rm(p.val1, p.val2));

              code_i += ByteCode::SIZE_OF::SUB_VALS;

              check_for_jumps(instruction_offsets, out_code, p.val2, &code_i, code_end);
              break;
            }
          case ByteCode::MUL_VALS: {
              const auto p = ByteCode::PARSE::MUL_VALS(code_i);

              out_code.insert(X64::REX_W | X64::rex_r_rm(p.val2, p.val1));
              out_code.insert(0x0F);
              out_code.insert(X64::IMUL_RM_TO_R);
              out_code.insert(X64::MODRM_MOD_DIRECT
                              | X64::modrm_r_rm(p.val2, p.val1));

              code_i += ByteCode::SIZE_OF::MUL_VALS;

              check_for_jumps(instruction_offsets, out_code, p.val2, &code_i, code_end);
              break;
            }
          case ByteCode::DIV_VALS: {
              const auto p = ByteCode::PARSE::DIV_VALS(code_i);

              assert(p.val1 == 0);

              out_code.insert(X64::REX_W | X64::rex_rm(p.val2));
              out_code.insert(X64::DIV_RM_TO_RAX);
              out_code.insert(X64::MODRM_MOD_DIRECT
                              | X64::modrm_r_rm(6, p.val2));


              code_i += ByteCode::SIZE_OF::DIV_VALS;

              check_for_jumps(instruction_offsets, out_code, 0, &code_i, code_end);
              break;
            }
          case ByteCode::OR_VALS: {
              const auto p = ByteCode::PARSE::OR_VALS(code_i);

              assert(p.val1 == 0);

              out_code.insert(X64::REX_W | X64::rex_r_rm(p.val1, p.val2));
              out_code.insert(X64::OR_R_TO_RM);
              out_code.insert(X64::MODRM_MOD_DIRECT
                              | X64::modrm_r_rm(p.val1, p.val2));


              code_i += ByteCode::SIZE_OF::OR_VALS;

              check_for_jumps(instruction_offsets, out_code, 0, &code_i, code_end);
              break;
            }
          case ByteCode::AND_VALS: {
              const auto p = ByteCode::PARSE::AND_VALS(code_i);

              assert(p.val1 == 0);

              out_code.insert(X64::REX_W | X64::rex_r_rm(p.val1, p.val2));
              out_code.insert(X64::AND_R_TO_RM);
              out_code.insert(X64::MODRM_MOD_DIRECT
                              | X64::modrm_r_rm(p.val1, p.val2));


              code_i += ByteCode::SIZE_OF::AND_VALS;

              check_for_jumps(instruction_offsets, out_code, 0, &code_i, code_end);
              break;
            }
          case ByteCode::EQ_VALS: {
              check_eq(instruction_offsets, out_code, &code_i, code_end);
              break;
            }
          case ByteCode::PUSH_VAL: {
              const auto p = ByteCode::PARSE::PUSH_VAL(code_i);

              push(out_code, p.val);
              code_i += ByteCode::SIZE_OF::PUSH_VAL;
              break;
            }
          case ByteCode::POP_TO_VAL: {
              const auto p = ByteCode::PARSE::POP_TO_VAL(code_i);

              pop(out_code, p.val);
              code_i += ByteCode::SIZE_OF::POP_TO_VAL;
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
              size_t next = p_j.INSTRUCTION_SIZE;

              //Could be series of labels so needs to while loop
              while (code_i + next < code_end && code_i[next] == ByteCode::LABEL) {
                const auto p_l = ByteCode::PARSE::LABEL(code_i + next);

                if (p_l.u64.val == p_j.u64.val) {
                  //About to jump to next instruction - can remove jump
                  code_i += p_j.INSTRUCTION_SIZE;
                  goto SKIP_JUMP;
                }

                //Next might also be a label
                next += ByteCode::SIZE_OF::LABEL;
              }

              instruction_offsets.insert(out_code.size);
              jump_near(out_code, (int32_t)p_j.u64.val);

              code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED;
            SKIP_JUMP:
              break;
            }
          case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
              const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

              //need to recompare
              out_code.insert(X64::REX_W | X64::rex_rm(p.val));
              out_code.insert(X64::CMP_32_TO_RM);
              out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(7, p.val));

              out_code.reserve_extra(sizeof(uint32_t));
              x32_to_bytes((int32_t)0, out_code.data);
              out_code.size += sizeof(uint32_t);

              instruction_offsets.insert(out_code.size);
              jump_zero(out_code, (int32_t)p.u64.val);

              code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
              break;
            }
          case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
              const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

              //need to recompare
              out_code.insert(X64::REX_W | X64::rex_rm(p.val));
              out_code.insert(X64::CMP_32_TO_RM);
              out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(7, p.val));

              out_code.reserve_extra(sizeof(uint32_t));
              x32_to_bytes((int32_t)0, out_code.data);
              out_code.size += sizeof(uint32_t);

              instruction_offsets.insert(out_code.size);
              jump_not_equal(out_code, (int32_t)p.u64.val);

              code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
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