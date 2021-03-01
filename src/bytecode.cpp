#include "bytecode.h"
#include "calling_conventions.h"

namespace ByteCode {
  static void print_bytecodeop(FILE* const stream, const uint8_t b) {
    switch (b) {
    #define X(NAME) case NAME: fprintf(stream, #NAME); break;
      BYTECODES_X
     #undef X
    }
  }

  void log_bytecode(const System* system, const uint8_t* bytecode, uint64_t size) {
    print_bytecode(system, stdout, bytecode, size);
  }

  void error_bytecode(const System* system, const uint8_t* bytecode, uint64_t size) {
    print_bytecode(system, stderr, bytecode, size);
  }

  void print_bytecode(const System* system, 
                      FILE* const stream,
                      const uint8_t* bytecode,
                      uint64_t size) {
    uint64_t i = 0;
    while (i < size) {
      switch (bytecode[i]) {
        case LEAVE_THEN_RETURN:
        case ENTER_FUNCTION:
          print_bytecodeop(stream, bytecode[i]);
          fputc('\n', stream);
          i++;
          break;

        case CALL_PTR: {
            const uint64_t res = x64_from_bytes(bytecode + i + 1);

            print_bytecodeop(stream, bytecode[i]);
            fprintf(stream, ": 0x%llx\n", res);
            i += OP_64::INSTRUCTION_SIZE;
            break;
          }
        case CALL_OFFSET:
        case JUMP_BY_I64:
        case JUMP_BY_I64_IF_ZERO:
        case JUMP_BY_I64_IF_NOT_ZERO: {
            const int64_t res = x64_from_bytes(bytecode + i + 1);

            print_bytecodeop(stream, bytecode[i]);
            fprintf(stream, ": %lld\n", res);
            i += OP_64::INSTRUCTION_SIZE;
            break;
          }

        case PUSH_R:
        case POP_R:
        case CALL_R:
        case SET_R_TO_ZF:{
            print_bytecodeop(stream, bytecode[i]);
            const char* const reg_name = system->reg_name_from_num(bytecode[i + 1]);
            fprintf(stream, ": $%s\n", reg_name);
            i += OP_R::INSTRUCTION_SIZE;
            break;
          }
        case MOV_R_TO_R:
        case ADD_R_TO_R:
        case SUB_R_TO_R:
        case CMP_R_TO_R:
        case MUL_R_TO_R: 
        case DIV_R_TO_R:
        case OR_R_TO_R:
        case AND_R_TO_R: {
            print_bytecodeop(stream, bytecode[i]);

            const char* const from_name = system->reg_name_from_num(bytecode[i + 1]);
            const char* const to_name = system->reg_name_from_num(bytecode[i + 2]);
            fprintf(stream, ": $%s to $%s\n", from_name, to_name);
            i += OP_R_R::INSTRUCTION_SIZE;
            break;
          }
        case MOV_64_TO_R:
        case ADD_64_TO_R:
        case SUB_64_TO_R:
        case CMP_64_TO_R:
        case MUL_64_TO_R:
        case DIV_64_TO_R: {
            const uint64_t res = x64_from_bytes(bytecode + i + 1);

            print_bytecodeop(stream, bytecode[i]);

            const char* const to_name = system->reg_name_from_num(bytecode[i + 9]);
            fprintf(stream, ": 0x%llx to $%s\n", res, to_name);

            i += OP_64_R::INSTRUCTION_SIZE;
            break;
          }
        case MOV_64_TO_M: {
            const uint64_t res1 = x64_from_bytes(bytecode + i + 1);
            const int64_t res2 = x64_from_bytes(bytecode + i + 10);

            print_bytecodeop(stream, bytecode[i]);

            const char* const to_name = system->reg_name_from_num(bytecode[i + 9]);

            if(res2 == 0) {
              fprintf(stream, ": 0x%llx to [$%s]\n", res1, to_name);
            }
            else {
              fprintf(stream, ": 0x%llx to [$%s + %lld]\n", res1, to_name, res2);
            }
            i += OP_64_R_64::INSTRUCTION_SIZE;
            break;
          }
        case MOV_R_TO_M: {
            const int64_t res1 = x64_from_bytes(bytecode + i + 3);

            print_bytecodeop(stream, bytecode[i]);

            const char* const from_name = system->reg_name_from_num(bytecode[i + 1]);
            const char* const to_name = system->reg_name_from_num(bytecode[i + 2]);

            if (res1 == 0) {
              fprintf(stream, ": $%s to [$%s]\n", from_name, to_name);
            }
            else {
              fprintf(stream, ": $%s to [$%s + %lld]\n", from_name, to_name, res1);
            }

            i += OP_R_R_64::INSTRUCTION_SIZE;
            break;
          }
        case MOV_M_TO_R: {
            const int64_t res1 = x64_from_bytes(bytecode + i + 3);

            print_bytecodeop(stream, bytecode[i]);

            const char* const from_name = system->reg_name_from_num(bytecode[i + 1]);
            const char* const to_name = system->reg_name_from_num(bytecode[i + 2]);

            if (res1 == 0) {
              fprintf(stream, ": [$%s] to $%s\n", from_name, to_name);
            }
            else {
              fprintf(stream, ": [$%s + %lld] to $%s\n", from_name, res1, to_name);
            }
            
            i += OP_R_R_64::INSTRUCTION_SIZE;
            break;
          }

      }
    }

    fflush(stream);
  }

  void emit_return(Array<uint8_t>& arr) {
    arr.insert(LEAVE_THEN_RETURN);
  }

  void emit_enter(Array<uint8_t>& arr) {
    arr.insert(ENTER_FUNCTION);
  }

  void emit_call_ptr(Array<uint8_t>& arr, const Function* x64) {
    OP_64::emit(arr, CALL_PTR, x64);
  }

  void emit_call_offset(Array<uint8_t>& arr, const int64_t x64) {
    OP_64::emit(arr, CALL_OFFSET, x64);
  }

  void emit_call_r(Array<uint8_t>& arr, const uint8_t reg) {
    OP_R::emit(arr, CALL_R, reg);
  }

  void emit_jump_by_offset(Array<uint8_t>& arr, int64_t x64) {
    OP_64::emit(arr, JUMP_BY_I64, x64);
  }

  void emit_jump_by_offset_if_zero(Array<uint8_t>& arr, int64_t x64) {
    OP_64::emit(arr, JUMP_BY_I64_IF_ZERO, x64);
  }

  void emit_jump_by_offset_if_not_zero(Array<uint8_t>& arr, int64_t x64) {
    OP_64::emit(arr, JUMP_BY_I64_IF_NOT_ZERO, x64);
  }

  void emit_add_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to) {
    OP_R_R::emit(arr, ADD_R_TO_R, from, to);
  }

  void emit_sub_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to) {
    OP_R_R::emit(arr, SUB_R_TO_R, from, to);
  }

  void emit_cmp_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to) {
    OP_R_R::emit(arr, CMP_R_TO_R, from, to);
  }

  void emit_add_64_to_r(Array<uint8_t>& arr, X64_UNION from, uint8_t to) {
    OP_64_R::emit(arr, ADD_64_TO_R, from, to);
  }

  void emit_sub_64_to_r(Array<uint8_t>& arr, X64_UNION from, uint8_t to) {
    OP_64_R::emit(arr, SUB_64_TO_R, from, to);
  }

  void emit_cmp_64_to_r(Array<uint8_t>& arr, X64_UNION from, uint8_t to) {
    OP_64_R::emit(arr, CMP_64_TO_R, from, to);
  }

  void emit_mul_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to) {
    OP_R_R::emit(arr, MUL_R_TO_R, from, to);
  }

  void emit_mul_64_to_r(Array<uint8_t>& arr, X64_UNION from, uint8_t to) {
    OP_64_R::emit(arr, MUL_64_TO_R, from, to);
  }

  void emit_div_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to) {
    OP_R_R::emit(arr, DIV_R_TO_R, from, to);
  }

  void emit_div_64_to_r(Array<uint8_t>& arr, X64_UNION from, uint8_t to) {
    OP_64_R::emit(arr, DIV_64_TO_R, from, to);
  }

  void emit_or_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to) {
    OP_R_R::emit(arr, OR_R_TO_R, from, to);
  }

  void emit_and_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to) {
    OP_R_R::emit(arr, AND_R_TO_R, from, to);
  }

  void emit_mov_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to) {
    OP_R_R::emit(arr, MOV_R_TO_R, from, to);
  }

  void emit_mov_r_to_m(Array<uint8_t>& arr, uint8_t from, uint8_t to, int64_t disp) {
    OP_R_R_64::emit(arr, MOV_R_TO_M, from, to, disp);
  }

  void emit_mov_m_to_r(Array<uint8_t>& arr, uint8_t from, int64_t disp, uint8_t to) {
    OP_R_R_64::emit(arr, MOV_M_TO_R, from, to, disp);
  }

  void emit_mov_64_to_r(Array<uint8_t>& arr, X64_UNION x64, uint8_t to) {
    OP_64_R::emit(arr, MOV_64_TO_R, x64, to);
  }

  void emit_mov_64_to_m(Array<uint8_t>& arr, X64_UNION x64, uint8_t to, int64_t disp) {
    OP_64_R_64::emit(arr, MOV_64_TO_M, x64, to, disp);
  }

  void emit_push_r(Array<uint8_t>& arr, uint8_t reg) {
    OP_R::emit(arr, PUSH_R, reg);
  }

  void emit_pop_r(Array<uint8_t>& arr, uint8_t reg) {
    OP_R::emit(arr, POP_R, reg);
  }

  void emit_set_r_to_zf(Array<uint8_t>& arr, uint8_t reg) {
    OP_R::emit(arr, SET_R_TO_ZF, reg);
  }
}