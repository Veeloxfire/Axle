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
      case ByteCode::ADD_R64S: {
          const auto i = ByteCode::PARSE::ADD_R64S(vm->IP);

          vm->registers[i.val2].b64.reg += vm->registers[i.val1].b64.reg;

          vm->IP += ByteCode::SIZE_OF::ADD_R64S;
          break;
        }
      case ByteCode::SUB_R64S: {
          const auto i = ByteCode::PARSE::SUB_R64S(vm->IP);

          vm->registers[i.val2].b64.reg -= vm->registers[i.val1].b64.reg;

          vm->IP += ByteCode::SIZE_OF::SUB_R64S;
          break;
        }
      case ByteCode::MUL_R64S: {
          const auto i = ByteCode::PARSE::MUL_R64S(vm->IP);

          vm->registers[i.val2].b64.reg *= vm->registers[i.val1].b64.reg;

          vm->IP += ByteCode::SIZE_OF::MUL_R64S;
          break;
        }
      case ByteCode::DIV_RU64S: {
          const auto i = ByteCode::PARSE::DIV_RU64S(vm->IP);

          vm->registers[i.val2].b64.reg /= vm->registers[i.val1].b64.reg;

          vm->IP += ByteCode::SIZE_OF::DIV_RU64S;
          break;
        }
      case ByteCode::DIV_RI64S: {
          const auto i = ByteCode::PARSE::DIV_RI64S(vm->IP);

          vm->registers[i.val2].b64.reg_s /= vm->registers[i.val1].b64.reg_s;

          vm->IP += ByteCode::SIZE_OF::DIV_RI64S;
          break;
        }
      case ByteCode::EQ_R64S: {
          const auto i = ByteCode::PARSE::EQ_R64S(vm->IP);

          vm->registers[i.val2].b64.reg = (vm->registers[i.val2].b64.reg == vm->registers[i.val1].b64.reg);

          vm->IP += ByteCode::SIZE_OF::EQ_R64S;
          break;
        }
      case ByteCode::OR_R64S: {
          const auto i = ByteCode::PARSE::OR_R64S(vm->IP);

          vm->registers[i.val2].b64.reg |= vm->registers[i.val1].b64.reg;

          vm->IP += ByteCode::SIZE_OF::OR_R64S;
          break;
        }
      case ByteCode::AND_R64S: {
          const auto i = ByteCode::PARSE::AND_R64S(vm->IP);

          vm->registers[i.val2].b64.reg &= vm->registers[i.val1].b64.reg;

          vm->IP += ByteCode::SIZE_OF::AND_R64S;
          break;
        }
      case ByteCode::LOAD_ADDRESS: {
          const auto i = ByteCode::PARSE::LOAD_ADDRESS(vm->IP);

          vm->registers[i.val1].b64.reg = vm->registers[i.val2].b64.reg + vm->registers[i.val3].b64.reg;

          vm->IP += ByteCode::SIZE_OF::LOAD_ADDRESS;
          break;
        }
      case ByteCode::NEG_R64: {
          const auto i = ByteCode::PARSE::NEG_R64(vm->IP);

          vm->registers[i.val].b64.reg_s = -vm->registers[i.val].b64.reg_s;

          vm->IP += ByteCode::SIZE_OF::NEG_R64;
          break;
        }
      case ByteCode::SET_R64_TO_64: {
          const auto i = ByteCode::PARSE::SET_R64_TO_64(vm->IP);

          vm->registers[i.val].b64.reg = i.u64;

          vm->IP += ByteCode::SIZE_OF::SET_R64_TO_64;
          break;
        }
      case ByteCode::COPY_R64_TO_R64: {
          const auto i = ByteCode::PARSE::COPY_R64_TO_R64(vm->IP);

          vm->registers[i.val2].b64.reg = vm->registers[i.val1].b64.reg;

          vm->IP += ByteCode::SIZE_OF::COPY_R64_TO_R64;
          break;
        }
      case ByteCode::PUSH_R64: {
          const auto i = ByteCode::PARSE::PUSH_R64(vm->IP);

          vm->push(vm->registers[i.val].b64.reg);

          vm->IP += ByteCode::SIZE_OF::PUSH_R64;
          break;
        }
      case ByteCode::POP_TO_R64: {
          const auto i = ByteCode::PARSE::POP_TO_R64(vm->IP);

          vm->registers[i.val].b64.reg = vm->pop();

          vm->IP += ByteCode::SIZE_OF::POP_TO_R64;
          break;
        }
      case ByteCode::ALLOCATE_STACK: {
          const auto i = ByteCode::PARSE::ALLOCATE_STACK(vm->IP);

          vm->allocate_stack(i.u64.val);

          vm->IP += ByteCode::SIZE_OF::ALLOCATE_STACK;
          break;
        }
      case ByteCode::COPY_R64_TO_STACK_TOP: {
          const auto i = ByteCode::PARSE::COPY_R64_TO_STACK_TOP(vm->IP);

          x64_to_bytes(vm->registers[i.val].b64.reg, vm->SP + i.u64.sig_val);

          vm->IP += ByteCode::SIZE_OF::COPY_R64_TO_STACK_TOP;
          break;
        }
      case ByteCode::COPY_64_TO_STACK_TOP: {
          const auto i = ByteCode::PARSE::COPY_64_TO_STACK_TOP(vm->IP);

          x64_to_bytes(i.u64_1, vm->SP + i.u64_2.sig_val);

          vm->IP += ByteCode::SIZE_OF::COPY_64_TO_STACK_TOP;
          break;
        }
      case ByteCode::COPY_64_TO_STACK: {
          const auto i = ByteCode::PARSE::COPY_64_TO_STACK(vm->IP);

          x64_to_bytes(i.u64_1, vm->BP + i.u64_2.sig_val);

          vm->IP += ByteCode::SIZE_OF::COPY_64_TO_STACK;
          break;
        }
      case ByteCode::COPY_R64_TO_STACK: {
          const auto i = ByteCode::PARSE::COPY_R64_TO_STACK(vm->IP);

          x64_to_bytes(vm->registers[i.val].b64.reg, vm->BP + i.u64.sig_val);

          vm->IP += ByteCode::SIZE_OF::COPY_R64_TO_STACK;
          break;
        }
      case ByteCode::COPY_R64_FROM_STACK: {
          const auto i = ByteCode::PARSE::COPY_R64_FROM_STACK(vm->IP);

          vm->registers[i.val].b64.reg = x64_from_bytes(vm->BP + i.u64.sig_val);

          vm->IP += ByteCode::SIZE_OF::COPY_R64_FROM_STACK;
          break;
        }
      case ByteCode::COPY_R64_FROM_MEM: {
          const auto i = ByteCode::PARSE::COPY_R64_FROM_MEM(vm->IP);

          vm->registers[i.val1].b64.reg = *(uint64_t*)vm->registers[i.val2].b64.ptr;

          vm->IP += ByteCode::SIZE_OF::COPY_R64_FROM_MEM;
          break;
        }
      case ByteCode::COPY_R64_FROM_MEM_COMPLEX: {
          const auto i = ByteCode::PARSE::COPY_R64_FROM_MEM_COMPLEX(vm->IP);

          uint8_t* const ptr = (vm->registers[i.mem.base].b64.b_ptr
            + i.mem.disp
            + (vm->registers[i.mem.index].b64.reg * i.mem.scale));

          vm->registers[i.val].b64.reg = x64_from_bytes(ptr);

          vm->IP += ByteCode::SIZE_OF::COPY_R64_FROM_MEM_COMPLEX;
          break;
        }
      case ByteCode::CONV_RU8_TO_R64: {
          const auto i = ByteCode::PARSE::CONV_RU8_TO_R64(vm->IP);

          vm->registers[i.val].b64.reg = (uint64_t)vm->registers[i.val].b8s.low_reg;

          vm->IP += ByteCode::SIZE_OF::CONV_RU8_TO_R64;
          break;
        }
      case ByteCode::CONV_RI8_TO_R64: {
          const auto i = ByteCode::PARSE::CONV_RI8_TO_R64(vm->IP);

          vm->registers[i.val].b64.reg = (uint64_t)vm->registers[i.val].b8s.low_reg_s;

          vm->IP += ByteCode::SIZE_OF::CONV_RI8_TO_R64;
          break;
        }
      case ByteCode::JUMP_TO_FIXED: {
          const auto i = ByteCode::PARSE::JUMP_TO_FIXED(vm->IP);
          vm->IP = code + i.u64.val;
          break;
        }
      case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
          const auto i = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_ZERO(vm->IP);

          //Ugly - dont want an 'if' in the vm to do an if
          if (vm->registers[i.val].b64.reg == 0) {
            vm->IP = code + i.u64.val;
          }
          else {
            vm->IP += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_ZERO;
          }
          break;
        }
      case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
          const auto i = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(vm->IP);

          //Ugly - dont want an 'if' in the vm to do an if
          if (vm->registers[i.val].b64.reg != 0) {
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
            return ErrorCode::OK;
          }
          break;
        }
      default: {
          return ErrorCode::UNDEFINED_INSTRUCTION;
        }
    }
  }
}