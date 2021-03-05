#include "machine_code.h"
#include "bytecode.h"


inline static void mov_r_to_r(Array<uint8_t>& arr,
                              uint8_t from,
                              uint8_t to) {
  arr.insert(X64::REX_W
             | X64::rex_r(from)
             | X64::rex_b(to));
  arr.insert(X64::MOV_R_TO_RM);
  arr.insert(X64::MODRM_MOD_INDIRECT
             | X64::modrm_reg(from)
             | X64::modrm_rm(to));
}

inline static void push_r(Array<uint8_t>& arr, uint8_t reg) {
  if ((reg & 0b0000'1000) > 0) {
    arr.insert(X64::REX_W | X64::REX_B);
  }

  arr.insert(X64::PUSH_R + (reg & 0b0000'0111));
}

inline static void pop_r(Array<uint8_t>& arr, uint8_t reg) {
  if ((reg & 0b0000'1000) > 0) {
    arr.insert(X64::REX_W | X64::REX_B);
  }

  arr.insert(X64::POP_R + (reg & 0b0000'0111));
}

void convert_to_x64_machine_code(Array<uint8_t>& arr,
                                 const uint8_t* bytecode, const size_t length) {
  for (size_t i = 0; i < length;) {
    switch (bytecode[i]) {
      case ByteCode::SUB_64_TO_R: {
          uint64_t from = x64_from_bytes(bytecode + i + 1);
          uint8_t to   = bytecode[i + 1 + 8];

          arr.insert(X64::REX_W
                     | X64::rex_b(to));
          arr.insert(X64::SUB_32_TO_RM);
          arr.insert(X64::MODRM_MOD_INDIRECT
                     | X64::modrm_rm(to));

          arr.reserve_extra(4);
          x32_to_bytes(from, arr.data);
          arr.size += 4;

          i += ByteCode::OP_64_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::JUMP_BY_I64: {
          uint64_t from = x64_from_bytes(bytecode + i + 1);

          //Dont need to jump itself
          if (from != ByteCode::OP_64::INSTRUCTION_SIZE) {
            //TODO: Offsets ...
          }

          i += ByteCode::OP_64::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::MOV_R_TO_R: {
          uint8_t from = bytecode[i + 1];
          uint8_t to   = bytecode[i + 2];

          mov_r_to_r(arr, from, to);


          i += ByteCode::OP_R_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::MOV_64_TO_R: {
          uint64_t from = x64_from_bytes(bytecode + i + 1);
          uint8_t to   = bytecode[i + 1 + 8];

          arr.insert(X64::REX_W
                     | X64::rex_r(to));
          arr.insert(X64::MOV_64_TO_R + (to & 0b0000'0111));

          arr.reserve_extra(8);
          x64_to_bytes(from, arr.data + arr.size);
          arr.size += 8;

          i += ByteCode::OP_64_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::LEAVE_THEN_RETURN:
        mov_r_to_r(arr, RBP.REG, RSP.REG);

        pop_r(arr, RBP.REG);

        //Actually return
        arr.insert(X64::SHORT_RET);

        i += 1;
        break;
      case ByteCode::ENTER_FUNCTION:
        push_r(arr, RBP.REG);
        mov_r_to_r(arr, RSP.REG, RBP.REG);
        i += 1;
        break;
    }
  }
}