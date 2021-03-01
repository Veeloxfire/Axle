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

void VM::set_ip_to_reg(const uint8_t reg) noexcept {
  IP = (const uint8_t*)registers[reg].full.reg;
}

void VM::x64_add_regs(uint8_t from, uint8_t to) noexcept {
  registers[to].full.reg += registers[from].full.reg;

  set_value_flags(registers[to].full.reg);
}

void VM::x64_sub_regs(uint8_t from, uint8_t to) noexcept {
  registers[to].full.reg -= registers[from].full.reg;

  set_value_flags(registers[to].full.reg);
}

void VM::x64_cmp_regs(uint8_t from, uint8_t to) noexcept {
  set_value_flags(registers[to].full.reg - registers[from].full.reg);
}

void VM::x64_mul_regs(uint8_t from, uint8_t to) noexcept {
  registers[to].full.reg *= registers[from].full.reg;

  set_value_flags(registers[to].full.reg);
}

void VM::x64_div_regs(uint8_t from, uint8_t to) noexcept {
  registers[to].full.reg /= registers[from].full.reg;

  set_value_flags(registers[to].full.reg);
}

void VM::x64_or_regs(uint8_t from, uint8_t to) noexcept {
  registers[to].full.reg |= registers[from].full.reg;

  set_value_flags(registers[to].full.reg);
}

void VM::x64_and_regs(uint8_t from, uint8_t to) noexcept {
  registers[to].full.reg &= registers[from].full.reg;

  set_value_flags(registers[to].full.reg);
}

void VM::x64_add_64_to_reg(uint64_t from, uint8_t to) noexcept {
  registers[to].full.reg += from;

  set_value_flags(registers[to].full.reg);
}

void VM::x64_sub_64_to_reg(uint64_t from, uint8_t to) noexcept {
  registers[to].full.reg -= from;

  set_value_flags(registers[to].full.reg);
}

void VM::x64_cmp_64_to_reg(uint64_t from, uint8_t to) noexcept {
  set_value_flags(registers[to].full.reg - from);
}

void VM::x64_mul_64_to_reg(uint64_t from, uint8_t to) noexcept {
  registers[to].full.reg *= from;

  set_value_flags(registers[to].full.reg);
}

void VM::x64_div_64_to_reg(uint64_t from, uint8_t to) noexcept {
  registers[to].full.reg /= from;

  set_value_flags(registers[to].full.reg);
}

void VM::x64_mov_regs(uint8_t from, uint8_t to) noexcept {
  registers[to].full.reg = registers[from].full.reg;
}

void VM::x64_mov_reg_to_mem(uint8_t from, uint8_t to, int64_t disp) noexcept {
  //Casting to load value in 'to' as a 64 bit pointer
  *((uint64_t*)(registers[to].full.reg + disp)) = registers[from].full.reg;
}

void VM::x64_mov_mem_to_reg(uint8_t from, int64_t disp, uint8_t to) noexcept {
  //Casting to load value in 'from' as a 64 bit pointer
  registers[to].full.reg = *((uint64_t*)(registers[from].full.reg + disp));
}

void VM::x64_mov_64_to_reg(uint64_t from, uint8_t to) noexcept {
  registers[to].full.reg = from;
}

void VM::x64_mov_64_to_mem(uint64_t from, uint8_t to, int64_t disp) noexcept {
  *((uint64_t*)(registers[to].full.reg + disp)) = from;
}

ErrorCode vm_rum(VM* vm, const Function* function) noexcept {

  constexpr size_t PRE_ENTRY_SIZE = 9;


  //Make PRE ENTRY FUNCTION for easy return
  uint8_t pre_entry_function[PRE_ENTRY_SIZE] ={};

  pre_entry_function[0] = ByteCode::CALL_PTR;
  x64_to_bytes(function, pre_entry_function + 1);

  //Just call into the first function
  //On return to this function then return from vm_run
  vm->IP = pre_entry_function;

#define SP   (vm->registers[RSP.REG].full.ptr)
#define BP   (vm->registers[RBP.REG].full.ptr)

  while (true) {
    switch (vm->IP[0]) {
      case ByteCode::ADD_R_TO_R: {
          uint8_t reg1 = vm->IP[1];
          uint8_t reg2 = vm->IP[2];

          vm->x64_add_regs(reg1, reg2);

          vm->IP += ByteCode::OP_R_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::SUB_R_TO_R: {
          uint8_t reg1 = vm->IP[1];
          uint8_t reg2 = vm->IP[2];

          vm->x64_sub_regs(reg1, reg2);

          vm->IP += ByteCode::OP_R_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::CMP_R_TO_R: {
          uint8_t reg1 = vm->IP[1];
          uint8_t reg2 = vm->IP[2];

          vm->x64_cmp_regs(reg1, reg2);

          vm->IP += ByteCode::OP_R_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::MUL_R_TO_R: {
          uint8_t reg1 = vm->IP[1];
          uint8_t reg2 = vm->IP[2];

          vm->x64_mul_regs(reg1, reg2);

          vm->IP += ByteCode::OP_R_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::DIV_R_TO_R: {
          uint8_t reg1 = vm->IP[1];
          uint8_t reg2 = vm->IP[2];

          vm->x64_div_regs(reg1, reg2);

          vm->IP += ByteCode::OP_R_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::OR_R_TO_R: {
          uint8_t reg1 = vm->IP[1];
          uint8_t reg2 = vm->IP[2];

          vm->x64_or_regs(reg1, reg2);

          vm->IP += ByteCode::OP_R_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::AND_R_TO_R: {
          uint8_t reg1 = vm->IP[1];
          uint8_t reg2 = vm->IP[2];

          vm->x64_and_regs(reg1, reg2);

          vm->IP += ByteCode::OP_R_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::ADD_64_TO_R: {
          uint64_t val = x64_from_bytes(vm->IP + 1);
          uint8_t to = vm->IP[9];

          vm->x64_add_64_to_reg(val, to);

          vm->IP += ByteCode::OP_64_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::SUB_64_TO_R: {
          uint64_t val = x64_from_bytes(vm->IP + 1);
          uint8_t to = vm->IP[9];

          vm->x64_sub_64_to_reg(val, to);

          if(to == RSP.REG) {
            if (SP > BP || SP < vm->stack) {
              return ErrorCode::STACK_OVERFLOW;
            }
          }

          vm->IP += ByteCode::OP_64_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::CMP_64_TO_R: {
          uint64_t val = x64_from_bytes(vm->IP + 1);
          uint8_t to = vm->IP[9];

          vm->x64_cmp_64_to_reg(val, to);

          vm->IP += ByteCode::OP_64_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::MUL_64_TO_R: {
          uint64_t val = x64_from_bytes(vm->IP + 1);
          uint8_t to = vm->IP[9];

          vm->x64_mul_64_to_reg(val, to);

          vm->IP += ByteCode::OP_64_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::DIV_64_TO_R: {
          uint64_t val = x64_from_bytes(vm->IP + 1);
          uint8_t to = vm->IP[9];

          vm->x64_div_64_to_reg(val, to);

          vm->IP += ByteCode::OP_64_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::MOV_R_TO_R: {
          uint8_t reg1 = vm->IP[1];
          uint8_t reg2 = vm->IP[2];

          vm->x64_mov_regs(reg1, reg2);

          vm->IP += ByteCode::OP_R_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::MOV_R_TO_M: {
          const uint8_t reg = vm->IP[1];
          const uint8_t mem = vm->IP[2];

          const int64_t disp = x64_from_bytes(vm->IP + 3);

          vm->x64_mov_reg_to_mem(reg, mem, disp);

          vm->IP += ByteCode::OP_R_R_64::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::MOV_M_TO_R: {
          const uint8_t mem = vm->IP[1];
          const uint8_t reg = vm->IP[2];

          const int64_t disp = x64_from_bytes(vm->IP + 3);

          vm->x64_mov_mem_to_reg(mem, disp, reg);

          vm->IP += ByteCode::OP_R_R_64::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::MOV_64_TO_M: {
          //64 bits could be a floating point number, an int, a ptr etc
          const uint64_t val = x64_from_bytes(vm->IP + 1);

          const uint8_t reg = vm->IP[9];
          const int64_t disp = x64_from_bytes(vm->IP + 10);

          vm->x64_mov_64_to_mem(val, reg, disp);

          vm->IP += 18;
          break;
        }
      case ByteCode::MOV_64_TO_R: {
          //64 bits could be a floating point number, an int, a ptr etc
          uint64_t val = x64_from_bytes(vm->IP + 1);
          uint8_t reg = vm->IP[9];

          vm->x64_mov_64_to_reg(val, reg);

          vm->IP += ByteCode::OP_64_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::PUSH_R: {
          uint8_t reg = vm->IP[1];

          if (vm->decrement_sp(8)) {
            return ErrorCode::STACK_OVERFLOW;
          }
          x64_to_bytes(vm->IP, SP);

          vm->IP += ByteCode::OP_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::POP_R: {
          uint8_t reg = vm->IP[1];

          x64_to_bytes(vm->IP, SP);
          SP += 8;

          vm->IP += ByteCode::OP_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::JUMP_BY_I64: {
          const int64_t offset = x64_from_bytes(vm->IP + 1);
          vm->IP += offset;
          break;
        }
      case ByteCode::JUMP_BY_I64_IF_ZERO: {
          const int64_t offset = x64_from_bytes(vm->IP + 1);
          const bool should_jump = TEST_MASK(vm->flags, VM::ZF_MASK);

          constexpr int64_t normal = ByteCode::OP_64::INSTRUCTION_SIZE;

          vm->IP += (normal * !should_jump) + (offset * should_jump);
          break;
        }
      case ByteCode::JUMP_BY_I64_IF_NOT_ZERO: {
          const int64_t offset = x64_from_bytes(vm->IP + 1);
          const bool should_jump = !TEST_MASK(vm->flags, VM::ZF_MASK);

          constexpr int64_t normal = ByteCode::OP_64::INSTRUCTION_SIZE;

          vm->IP += (normal * !should_jump) + (offset * should_jump);
          break;
        }
      case ByteCode::SET_R_TO_ZF: {
          const uint8_t reg = vm->IP[1];

          vm->set_reg_to_flags(reg, VM::ZF_MASK);

          vm->IP += ByteCode::OP_R::INSTRUCTION_SIZE;
          break;
        }
      case ByteCode::CALL_PTR: {
          const Function* func = x64_from_bytes(vm->IP + 1);

          const uint8_t* buff = func->bytecode.data;

          vm->IP += ByteCode::OP_64::INSTRUCTION_SIZE;

          //Load pointer to next instruction in this function onto the stack
          //Means we just return straight to the pointer
          if (vm->decrement_sp(8)) {
            return ErrorCode::STACK_OVERFLOW;
          }
          x64_to_bytes(vm->IP, SP);

          //Move to function
          vm->IP = buff;
          break;
        }
      case ByteCode::CALL_OFFSET: {
          const int64_t offset = x64_from_bytes(vm->IP + 1);

          //Load pointer to next instruction in this function onto the stack
          //Allows for return
          //Offsets are always from the start of the instruction
          //Means we dont increment IP like normal but instead just add the value on in the save
          if (vm->decrement_sp(8)) {
            return ErrorCode::STACK_OVERFLOW;
          }
          x64_to_bytes(vm->IP + ByteCode::OP_64::INSTRUCTION_SIZE, SP);

          //Move to function
          vm->IP += offset;
          break;
        }
      case ByteCode::CALL_R: {
          uint8_t reg = vm->IP[1];

          vm->IP += ByteCode::OP_R::INSTRUCTION_SIZE;

          //Load pointer to next instruction in this function onto the stack
          //Allows for return
          if (vm->decrement_sp(8)) {
            return ErrorCode::STACK_OVERFLOW;
          }
          x64_to_bytes(vm->IP, SP);

          //Move to function
          vm->set_ip_to_reg(reg);
          break;
        }
      case ByteCode::ENTER_FUNCTION: {
          //Save old base pointer
          if (vm->decrement_sp(8)) {
            return ErrorCode::STACK_OVERFLOW;
          }
          x64_to_bytes(BP, SP);

          //Move base pointer to new stack pointer
          BP = SP;

          vm->IP++;//No operands
          break;
        }
      case ByteCode::LEAVE_THEN_RETURN: {
          //Reset stack pointer to before call
          SP = BP;

          //Load old base pointer
          BP = x64_from_bytes(SP);
          SP += 8;

          //Load instruction pointer
          vm->IP = x64_from_bytes(SP);
          SP += 8;

          if (vm->IP == pre_entry_function + PRE_ENTRY_SIZE) {
            return ErrorCode::NO_ERROR;
          }

          //DONT INCREMENT IP
          //It is already set to the next instruction in CALL
          break;
        }
      default: {
          return ErrorCode::UNDEFINED_INSTRUCTION;
        }
    }
  }

#undef SP
#undef BP
}
