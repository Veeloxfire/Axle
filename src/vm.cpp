#include "vm.h"
#include "type.h"
#include "windows_specifics.h"
#include "trace.h"

static_assert(sizeof(Reg64_8BL) == 8, "Must be 8 bytes");
static_assert(sizeof(Reg64_8BH) == 8, "Must be 8 bytes");
static_assert(sizeof(Reg64_16B) == 8, "Must be 8 bytes");
static_assert(sizeof(Reg64_32B) == 8, "Must be 8 bytes");
static_assert(sizeof(Reg64_64B) == 8, "Must be 8 bytes");
static_assert(sizeof(Register) == 8, "Must be 8 bytes");

static_assert(sizeof(uint64_t) == sizeof(void*), "MUST BE 64 BIT SYSTEM");

void VM::allocate_stack(uint64_t bytes) {
  SP -= bytes;

  if (SP <= stack) {
    errors->register_error(ERROR_CODE::VM_ERROR, Span{},
                           "VM Stack overflow during allocation");
    errors->panic = true;
    return;
  }
}

void VM::push(X64_UNION val) {
  SP -= 8;

  if (SP <= stack) {
    errors->register_error(ERROR_CODE::VM_ERROR, Span{},
                           "VM Stack overflow during 'push' operation");
    errors->panic = true;
    return;
  }

  x64_to_bytes(val, SP);
}

X64_UNION VM::pop() {
  if (SP + 8 >= stack + STACK_SIZE) {
    errors->register_error(ERROR_CODE::VM_ERROR, Span{},
                           "VM Stack underflow during 'pop' operation");
    errors->panic = true;
    return { (uint64_t) 0 };
  }

  X64_UNION val = x64_from_bytes(SP);
  SP += 8;
  return val;
}

uint8_t* VM::load_mem(const MemComplex& mem) {
  uint8_t* ptr_base = (registers[mem.base].b64.b_ptr + mem.disp);

  if (mem.scale > 0) {
    ptr_base += (registers[mem.index].b64.reg * mem.scale);
  }

  return ptr_base;
}

//Written in assembly to directly deal with the stack
extern "C" uint64_t call_native_x64(const void* func, uint64_t* param_registers, uint8_t* stack_top, uint64_t stack_required);

void vm_call_native_x64(VM* const vm, const void* func_ptr, uint64_t stack_required) {
  uint64_t param_registers[4] ={};

  //Load the parameters
  param_registers[0] = vm->registers[1].b64.reg;
  param_registers[1] = vm->registers[2].b64.reg;
  param_registers[2] = vm->registers[3].b64.reg;
  param_registers[3] = vm->registers[4].b64.reg;

  vm->registers[0].b64.reg = call_native_x64(func_ptr, param_registers, vm->SP, stack_required);
}

void vm_rum(VM* const vm, Program* prog) noexcept {
  TRACING_SCOPE("vm exec");

  //Load dlls
  Array<Windows::ActiveDll> actives = {};

  if (prog->imports.ptr != nullptr) {
    actives = Windows::load_dlls(prog);
  }


  //Pre entry function - if we ever return to nullptr then we finish execution
  vm->push((uint8_t*)nullptr);
  vm->IP = prog->code.ptr + prog->entry_point;

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
      case ByteCode::EQ_R8S: {
          const auto i = ByteCode::PARSE::EQ_R8S(vm->IP);

          vm->registers[i.val2].b8l.reg = (vm->registers[i.val2].b8l.reg == vm->registers[i.val1].b8l.reg);

          vm->IP += ByteCode::SIZE_OF::EQ_R8S;
          break;
        }
      case ByteCode::NEQ_R8S: {
          const auto i = ByteCode::PARSE::NEQ_R8S(vm->IP);

          vm->registers[i.val2].b8l.reg = (vm->registers[i.val2].b8l.reg != vm->registers[i.val1].b8l.reg);

          vm->IP += ByteCode::SIZE_OF::NEQ_R8S;
          break;
        }
      case ByteCode::LESS_U64S: {
          const auto i = ByteCode::PARSE::LESS_U64S(vm->IP);

          vm->registers[i.val2].b64.reg = (vm->registers[i.val2].b64.reg < vm->registers[i.val1].b64.reg);

          vm->IP += ByteCode::SIZE_OF::LESS_U64S;
          break;
        }
      case ByteCode::GREAT_U64S: {
          const auto i = ByteCode::PARSE::GREAT_U64S(vm->IP);

          vm->registers[i.val2].b64.reg = (vm->registers[i.val2].b64.reg > vm->registers[i.val1].b64.reg);

          vm->IP += ByteCode::SIZE_OF::GREAT_U64S;
          break;
        }
      case ByteCode::LESS_I64S: {
          const auto i = ByteCode::PARSE::LESS_I64S(vm->IP);

          vm->registers[i.val2].b64.reg = (vm->registers[i.val2].b64.reg_s < vm->registers[i.val1].b64.reg_s);

          vm->IP += ByteCode::SIZE_OF::LESS_I64S;
          break;
        }
      case ByteCode::GREAT_I64S: {
          const auto i = ByteCode::PARSE::GREAT_I64S(vm->IP);

          vm->registers[i.val2].b64.reg = (vm->registers[i.val2].b64.reg_s > vm->registers[i.val1].b64.reg_s);

          vm->IP += ByteCode::SIZE_OF::GREAT_I64S;
          break;
        }
      case ByteCode::OR_R64S: {
          const auto i = ByteCode::PARSE::OR_R64S(vm->IP);

          vm->registers[i.val2].b64.reg |= vm->registers[i.val1].b64.reg;

          vm->IP += ByteCode::SIZE_OF::OR_R64S;
          break;
        }
      case ByteCode::XOR_R64S: {
          const auto i = ByteCode::PARSE::XOR_R64S(vm->IP);

          vm->registers[i.val2].b64.reg ^= vm->registers[i.val1].b64.reg;

          vm->IP += ByteCode::SIZE_OF::XOR_R64S;
          break;
        }
      case ByteCode::AND_R64S: {
          const auto i = ByteCode::PARSE::AND_R64S(vm->IP);

          vm->registers[i.val2].b64.reg &= vm->registers[i.val1].b64.reg;

          vm->IP += ByteCode::SIZE_OF::AND_R64S;
          break;
        }
      case ByteCode::SHIFT_L_BY_R8_R64 : {
          const auto i = ByteCode::PARSE::SHIFT_L_BY_R8_R64(vm->IP);

          vm->registers[i.val2].b64.reg <<= vm->registers[i.val1].b8l.reg;

          vm->IP += ByteCode::SIZE_OF::SHIFT_L_BY_R8_R64;
          break;
        }
      case ByteCode::SHIFT_R_BY_R8_RU64: {
          const auto i = ByteCode::PARSE::SHIFT_R_BY_R8_RU64(vm->IP);

          vm->registers[i.val2].b64.reg_s >>= vm->registers[i.val1].b8l.reg;

          vm->IP += ByteCode::SIZE_OF::SHIFT_R_BY_R8_RU64;
          break;
        }
      case ByteCode::SHIFT_R_BY_R8_RI64: {
          const auto i = ByteCode::PARSE::SHIFT_R_BY_R8_RI64(vm->IP);

          vm->registers[i.val2].b64.reg >>= vm->registers[i.val1].b8l.reg;

          vm->IP += ByteCode::SIZE_OF::SHIFT_R_BY_R8_RI64;
          break;
        }
      case ByteCode::LOAD_ADDRESS: {
          const auto i = ByteCode::PARSE::LOAD_ADDRESS(vm->IP);

          vm->registers[i.val].b64.b_ptr = vm->load_mem(i.mem);

          vm->IP += ByteCode::SIZE_OF::LOAD_ADDRESS;
          break;
        }
      /*case ByteCode::LOAD_GLOBAL_MEM: {
          const auto i = ByteCode::PARSE::LOAD_GLOBAL_MEM(vm->IP);

          vm->registers[i.val].b64.b_ptr = i.u64;

          vm->IP += ByteCode::SIZE_OF::LOAD_GLOBAL_MEM;
          break;
        }*/
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
      case ByteCode::SET_R32_TO_32: {
          const auto i = ByteCode::PARSE::SET_R32_TO_32(vm->IP);

          vm->registers[i.val].b32.reg = i.u32;

          vm->IP += ByteCode::SIZE_OF::SET_R32_TO_32;
          break;
        }
      case ByteCode::SET_R16_TO_16: {
          const auto i = ByteCode::PARSE::SET_R16_TO_16(vm->IP);

          vm->registers[i.val].b16.reg = i.u16;

          vm->IP += ByteCode::SIZE_OF::SET_R16_TO_16;
          break;
        }
      case ByteCode::SET_R8_TO_8: {
          const auto i = ByteCode::PARSE::SET_R8_TO_8(vm->IP);

          vm->registers[i.val].b8l.reg = i.u8;

          vm->IP += ByteCode::SIZE_OF::SET_R8_TO_8;
          break;
        }
      case ByteCode::COPY_R64_TO_R64: {
          const auto i = ByteCode::PARSE::COPY_R64_TO_R64(vm->IP);

          vm->registers[i.val2].b64.reg = vm->registers[i.val1].b64.reg;

          vm->IP += ByteCode::SIZE_OF::COPY_R64_TO_R64;
          break;
        }
      case ByteCode::COPY_R32_TO_R32: {
          const auto i = ByteCode::PARSE::COPY_R32_TO_R32(vm->IP);

          vm->registers[i.val2].b32.reg = vm->registers[i.val1].b32.reg;
          vm->registers[i.val2].b32.padding = 0;

          vm->IP += ByteCode::SIZE_OF::COPY_R32_TO_R32;
          break;
        }
      case ByteCode::COPY_R16_TO_R16: {
          const auto i = ByteCode::PARSE::COPY_R16_TO_R16(vm->IP);

          vm->registers[i.val2].b16.reg = vm->registers[i.val1].b16.reg;

          vm->IP += ByteCode::SIZE_OF::COPY_R16_TO_R16;
          break;
        }
      case ByteCode::COPY_R8_TO_R8: {
          const auto i = ByteCode::PARSE::COPY_R8_TO_R8(vm->IP);

          vm->registers[i.val2].b8l.reg = vm->registers[i.val1].b8l.reg;

          vm->IP += ByteCode::SIZE_OF::COPY_R8_TO_R8;
          break;
        }
      case ByteCode::PUSH_R64: {
          const auto i = ByteCode::PARSE::PUSH_R64(vm->IP);

          vm->push(vm->registers[i.val].b64.reg);
          if (vm->errors->panic) {
            return;
          }

          vm->IP += ByteCode::SIZE_OF::PUSH_R64;
          break;
        }
      case ByteCode::POP_TO_R64: {
          const auto i = ByteCode::PARSE::POP_TO_R64(vm->IP);

          vm->registers[i.val].b64.reg = vm->pop();
          if (vm->errors->panic) {
            return;
          }

          vm->IP += ByteCode::SIZE_OF::POP_TO_R64;
          break;
        }
      case ByteCode::ALLOCATE_STACK: {
          const auto i = ByteCode::PARSE::ALLOCATE_STACK(vm->IP);

          vm->allocate_stack(i.u64.val);
          if (vm->errors->panic) {
            return;
          }

          vm->IP += ByteCode::SIZE_OF::ALLOCATE_STACK;
          break;
        }
      case ByteCode::COPY_64_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_64_TO_MEM(vm->IP);

          x64_to_bytes(i.u64, vm->load_mem(i.mem));

          vm->IP += ByteCode::SIZE_OF::COPY_64_TO_MEM;
          break;
        }
      case ByteCode::COPY_32_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_32_TO_MEM(vm->IP);

          x32_to_bytes(i.u32, vm->load_mem(i.mem));

          vm->IP += ByteCode::SIZE_OF::COPY_32_TO_MEM;
          break;
        }
      case ByteCode::COPY_16_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_16_TO_MEM(vm->IP);

          x16_to_bytes(i.u16, vm->load_mem(i.mem));

          vm->IP += ByteCode::SIZE_OF::COPY_16_TO_MEM;
          break;
        }
      case ByteCode::COPY_8_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_8_TO_MEM(vm->IP);

          vm->load_mem(i.mem)[0] = i.u8;

          vm->IP += ByteCode::SIZE_OF::COPY_8_TO_MEM;
          break;
        }
      case ByteCode::COPY_R64_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_R64_TO_MEM(vm->IP);

          x64_to_bytes(vm->registers[i.val].b64.reg, vm->load_mem(i.mem));

          vm->IP += ByteCode::SIZE_OF::COPY_R64_TO_MEM;
          break;
        }
      case ByteCode::COPY_R32_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_R32_TO_MEM(vm->IP);

          x32_to_bytes(vm->registers[i.val].b32.reg, vm->load_mem(i.mem));

          vm->IP += ByteCode::SIZE_OF::COPY_R32_TO_MEM;
          break;
        }
      case ByteCode::COPY_R16_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_R16_TO_MEM(vm->IP);

          x16_to_bytes(vm->registers[i.val].b16.reg, vm->load_mem(i.mem));

          vm->IP += ByteCode::SIZE_OF::COPY_R16_TO_MEM;
          break;
        }
      case ByteCode::COPY_R8_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_R8_TO_MEM(vm->IP);

          vm->load_mem(i.mem)[0] = vm->registers[i.val].b8l.reg;

          vm->IP += ByteCode::SIZE_OF::COPY_R8_TO_MEM;
          break;
        }
      case ByteCode::COPY_R64_FROM_MEM: {
          const auto i = ByteCode::PARSE::COPY_R64_FROM_MEM(vm->IP);

          vm->registers[i.val].b64.reg = x64_from_bytes(vm->load_mem(i.mem));

          vm->IP += ByteCode::SIZE_OF::COPY_R64_FROM_MEM;
          break;
        }
      case ByteCode::COPY_R32_FROM_MEM: {
          const auto i = ByteCode::PARSE::COPY_R32_FROM_MEM(vm->IP);

          vm->registers[i.val].b32.reg = x32_from_bytes(vm->load_mem(i.mem));

          vm->IP += ByteCode::SIZE_OF::COPY_R32_FROM_MEM;
          break;
        }
      case ByteCode::COPY_R16_FROM_MEM: {
          const auto i = ByteCode::PARSE::COPY_R16_FROM_MEM(vm->IP);

          vm->registers[i.val].b16.reg = x16_from_bytes(vm->load_mem(i.mem));

          vm->IP += ByteCode::SIZE_OF::COPY_R16_FROM_MEM;
          break;
        }
      case ByteCode::COPY_R8_FROM_MEM: {
          const auto i = ByteCode::PARSE::COPY_R8_FROM_MEM(vm->IP);

          vm->registers[i.val].b8l.reg = vm->load_mem(i.mem)[0];

          vm->IP += ByteCode::SIZE_OF::COPY_R8_FROM_MEM;
          break;
        }
      case ByteCode::CONV_RU8_TO_R64: {
          const auto i = ByteCode::PARSE::CONV_RU8_TO_R64(vm->IP);

          vm->registers[i.val].b64.reg = (uint64_t)vm->registers[i.val].b8l.reg;

          vm->IP += ByteCode::SIZE_OF::CONV_RU8_TO_R64;
          break;
        }
      case ByteCode::CONV_RI8_TO_R64: {
          const auto i = ByteCode::PARSE::CONV_RI8_TO_R64(vm->IP);

          vm->registers[i.val].b64.reg = (uint64_t)vm->registers[i.val].b8l.reg_s;

          vm->IP += ByteCode::SIZE_OF::CONV_RI8_TO_R64;
          break;
        }
      case ByteCode::CONV_RU32_TO_R64: {
          const auto i = ByteCode::PARSE::CONV_RU8_TO_R64(vm->IP);

          vm->registers[i.val].b64.reg = (uint64_t)vm->registers[i.val].b32.reg;

          vm->IP += ByteCode::SIZE_OF::CONV_RU8_TO_R64;
          break;
        }
      case ByteCode::CONV_RI32_TO_R64: {
          const auto i = ByteCode::PARSE::CONV_RI8_TO_R64(vm->IP);

          vm->registers[i.val].b64.reg = (uint64_t)vm->registers[i.val].b32.reg_s;

          vm->IP += ByteCode::SIZE_OF::CONV_RI8_TO_R64;
          break;
        }
      case ByteCode::JUMP_TO_FIXED: {
          const auto i = ByteCode::PARSE::JUMP_TO_FIXED(vm->IP);
          vm->IP = prog->code.ptr + i.u64.val;
          break;
        }
      case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
          const auto i = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_ZERO(vm->IP);

          //Ugly - dont want an 'if' in the vm to do an if
          if (vm->registers[i.val].b8l.reg == 0) {
            vm->IP = prog->code.ptr + i.u64.val;
          }
          else {
            vm->IP += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_ZERO;
          }
          break;
        }
      case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
          const auto i = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(vm->IP);

          //Ugly - dont want an 'if' in the vm to do an if
          if (vm->registers[i.val].b8l.reg != 0) {
            vm->IP = prog->code.ptr + i.u64.val;
          }
          else {
            vm->IP += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_ZERO;
          }
          break;
        }
      case ByteCode::PUSH_FRAME: {
          vm->push(vm->BP);
          if (vm->errors->panic) {
            return;
          }

          vm->BP = vm->SP;
          vm->IP += ByteCode::SIZE_OF::PUSH_FRAME;
          break;
        }
      case ByteCode::POP_FRAME: {
          vm->SP = vm->BP;
          vm->BP = vm->pop();
          if (vm->errors->panic) {
            return;
          }

          vm->IP += ByteCode::SIZE_OF::POP_FRAME;
          break;
        }
      case ByteCode::CALL: {
          const auto i = ByteCode::PARSE::CALL(vm->IP);

          vm->push(vm->IP + ByteCode::SIZE_OF::CALL);
          if (vm->errors->panic) {
            return;
          }

          vm->IP = prog->code.ptr + i.u64.val;
          break;
        }
      case ByteCode::CALL_NATIVE_X64: {
          const auto i = ByteCode::PARSE::CALL_NATIVE_X64(vm->IP);

          vm_call_native_x64(vm, *(const void**)i.u64_1, i.u64_2);

          vm->IP += ByteCode::SIZE_OF::CALL_NATIVE_X64;
          break;
        }
      case ByteCode::RETURN: {
          const auto i = ByteCode::PARSE::RETURN(vm->IP);

          vm->IP = vm->pop();
          if (vm->errors->panic || vm->IP == nullptr) {
            return;
          }
          break;
        }
      case ByteCode::LOAD_GLOBAL_MEM://should never actually be called
      default: {
          uint8_t op = vm->IP[0];
          vm->errors->register_error(ERROR_CODE::VM_ERROR, Span{},
                                     "Encountered invalid instruction\n"
                                     "Code: {}\nName: '{}'",
                                     op, ByteCode::bytecode_string((ByteCode::ByteCodeOp)op));
          return;
        }
    } 
  }
}