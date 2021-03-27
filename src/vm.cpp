#include "vm.h"
#include "type.h"

static_assert(sizeof(Reg64_8B) == 8, "Must be 8 bytes");
static_assert(sizeof(Reg64_16B) == 8, "Must be 8 bytes");
static_assert(sizeof(Reg64_32B) == 8, "Must be 8 bytes");
static_assert(sizeof(Reg64_64B) == 8, "Must be 8 bytes");
static_assert(sizeof(Register) == 8, "Must be 8 bytes");

static_assert(sizeof(uint64_t) == sizeof(void*), "MUST BE 64 BIT SYSTEM");

void Reg64_8B::zero_padding() noexcept {
  padding[0] = 0;
  padding[1] = 0;
  padding[2] = 0;
  padding[3] = 0;
  padding[4] = 0;
  padding[5] = 0;
}

void Reg64_16B::zero_padding() noexcept {
  padding[0] = 0;
  padding[1] = 0;
  padding[2] = 0;
  padding[3] = 0;
  padding[4] = 0;
  padding[5] = 0;
}

void Reg64_32B::zero_padding() noexcept {
  padding[0] = 0;
  padding[1] = 0;
  padding[2] = 0;
  padding[3] = 0;
}

ErrorCode vm_rum(VM* const vm, const uint8_t* const code, const size_t entry_point) noexcept {
  
  //Pre entry function - if we ever return to nullptr then we finish execution
  vm->push((uint8_t*)nullptr);
  vm->IP = code + entry_point;

  while (true) {
    switch (vm->IP[0]) {
      case ByteCode::ADD_VALS: {
          const auto i = ByteCode::PARSE::ADD_VALS(vm->IP);

          vm->registers[i.val2].full.reg += vm->registers[i.val1].full.reg;

          vm->IP += ByteCode::SIZE_OF::ADD_VALS;
          break;
        }
      case ByteCode::SUB_VALS: {
          const auto i = ByteCode::PARSE::SUB_VALS(vm->IP);

          vm->registers[i.val2].full.reg -= vm->registers[i.val1].full.reg;

          vm->IP += ByteCode::SIZE_OF::SUB_VALS;
          break;
        }
      case ByteCode::MUL_VALS: {
          const auto i = ByteCode::PARSE::MUL_VALS(vm->IP);

          vm->registers[i.val2].full.reg *= vm->registers[i.val1].full.reg;

          vm->IP += ByteCode::SIZE_OF::MUL_VALS;
          break;
        }
      case ByteCode::DIV_VALS: {
          const auto i = ByteCode::PARSE::DIV_VALS(vm->IP);

          vm->registers[i.val2].full.reg /= vm->registers[i.val1].full.reg;

          vm->IP += ByteCode::SIZE_OF::DIV_VALS;
          break;
        }
      case ByteCode::EQ_VALS: {
          const auto i = ByteCode::PARSE::EQ_VALS(vm->IP);

          vm->registers[i.val2].full.reg = (vm->registers[i.val2].full.reg == vm->registers[i.val1].full.reg);

          vm->IP += ByteCode::SIZE_OF::EQ_VALS;
          break;
        }
      case ByteCode::OR_VALS: {
          const auto i = ByteCode::PARSE::OR_VALS(vm->IP);

          vm->registers[i.val2].full.reg |= vm->registers[i.val1].full.reg;

          vm->IP += ByteCode::SIZE_OF::OR_VALS;
          break;
        }
      case ByteCode::AND_VALS: {
          const auto i = ByteCode::PARSE::AND_VALS(vm->IP);

          vm->registers[i.val2].full.reg &= vm->registers[i.val1].full.reg;

          vm->IP += ByteCode::SIZE_OF::AND_VALS;
          break;
        }
      case ByteCode::SET_VAL_TO_64: {
          const auto i = ByteCode::PARSE::SET_VAL_TO_64(vm->IP);

          vm->registers[i.val].full.reg = i.u64;

          vm->IP += ByteCode::SIZE_OF::SET_VAL_TO_64;
          break;
        }
      case ByteCode::COPY_TO_VAL: {
          const auto i = ByteCode::PARSE::COPY_TO_VAL(vm->IP);

          vm->registers[i.val2].full.reg = vm->registers[i.val1].full.reg;

          vm->IP += ByteCode::SIZE_OF::COPY_TO_VAL;
          break;
        }
      case ByteCode::PUSH_VAL: {
          const auto i = ByteCode::PARSE::PUSH_VAL(vm->IP);

          vm->push(vm->registers[i.val].full.reg);

          vm->IP += ByteCode::SIZE_OF::PUSH_VAL;
          break;
        }
      case ByteCode::POP_TO_VAL: {
          const auto i = ByteCode::PARSE::POP_TO_VAL(vm->IP);

          vm->registers[i.val].full.reg = vm->pop();

          vm->IP += ByteCode::SIZE_OF::POP_TO_VAL;
          break;
        }
      case ByteCode::JUMP_TO_FIXED: {
          const auto i = ByteCode::PARSE::JUMP_TO_FIXED(vm->IP);
          vm->IP = code + i.u64.val;
          break;
        }
      case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
          const auto i = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_ZERO(vm->IP);
          
          //Ugly - dont want an if in the vm to do an if
          if (vm->registers[i.val].full.reg == 0) {
            vm->IP = code + i.u64.val;
          }
          else {
            vm->IP += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_ZERO;
          }
          break;
        }
      case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
          const auto i = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(vm->IP);

          //Ugly - dont want an if in the vm to do an if
          if (vm->registers[i.val].full.reg != 0) {
            vm->IP = code + i.u64.val;
          }
          else {
            vm->IP += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_ZERO;
          }
          break;
        }
      case ByteCode::PUSH_FRAME: {
          vm->push(vm->BP);
          vm->BP = vm->SP;
          vm->IP += ByteCode::SIZE_OF::PUSH_FRAME;
          break;
        }
      case ByteCode::POP_FRAME: {
          vm->SP = vm->BP;
          vm->BP = vm->pop();
          vm->IP += ByteCode::SIZE_OF::POP_FRAME;
          break;
        }
      case ByteCode::CALL: {
          const auto i = ByteCode::PARSE::CALL(vm->IP);

          vm->push(vm->IP + ByteCode::SIZE_OF::CALL);
          vm->IP = code + i.u64.val;
          break;
        }
      case ByteCode::RETURN: {
          const auto i = ByteCode::PARSE::RETURN(vm->IP);

          vm->IP = vm->pop();
          if (vm->IP == nullptr) {
            //End execution
            return ErrorCode::NO_ERROR;
          }
          break;
        }
      default: {
          return ErrorCode::UNDEFINED_INSTRUCTION;
        }
    }
  }
}