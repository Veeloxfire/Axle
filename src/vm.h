#pragma once
#include "utility.h"
#include "bytecode.h"
#include "calling_conventions.h"
#include "Program.h"
#include "errors.h"

struct Reg64_8BL {
  union {
    uint8_t reg;
    int8_t reg_s;
  };

  uint8_t padding[7];
};

struct Reg64_8BH {
  uint8_t padding1;

  union {
    uint8_t reg;
    int8_t reg_s;
  };

  uint8_t padding2[6];
};

struct Reg64_16B {
  union {
    uint16_t reg;
    int16_t reg_s;
  };
  uint8_t padding[6];
};

struct Reg64_32B {

  union {
    int32_t reg_s;
    uint32_t reg;
  };

  uint32_t padding;
};

struct Reg64_64B {
  union {
    uint64_t reg;
    int64_t  reg_s;
    void* ptr;
    uint8_t* b_ptr;
  };
};

struct Register {
  union {
    Reg64_8BL b8l;
    Reg64_8BH b8h;
    Reg64_16B b16;
    Reg64_32B b32;
    Reg64_64B b64;
  };
};

struct Function;

struct VM {
  Errors* errors;

  constexpr static size_t STACK_SIZE = 256 * 8;
  uint8_t stack[STACK_SIZE] ={};

  const uint8_t* IP = nullptr;

  Register registers[NUM_VM_REGS + 2] ={};

#define SP registers[VM_SP_R].b64.b_ptr
#define BP registers[VM_BP_R].b64.b_ptr

  VM() {
    SP = stack + STACK_SIZE - 1;
    BP = stack + STACK_SIZE - 1;
  }

  void allocate_stack(uint64_t bytes);
  void push(X64_UNION val);
  X64_UNION pop();
  uint8_t* load_mem(const MemComplex&);
};

struct VM_LOADER {
  VM* vm = nullptr;

  size_t param_reg_itr = 0;
  size_t stack_itr = 0;

  const uint8_t* param_regs = 0;
  size_t num_param_regs = 0;

  constexpr VM_LOADER& operator<<(const X64_UNION& u64) {
    if (param_reg_itr < num_param_regs) {
      const uint8_t reg = param_regs[param_reg_itr];
      vm->registers[reg].b64.reg = u64.val;
      param_reg_itr++;
    }
    else {
      x64_to_bytes(u64, vm->SP + stack_itr * 8);
      stack_itr++;
    }
    return *this;
  }
};

template<typename ... T>
void vm_set_parameters(const CallingConvention* conv, VM* vm, T&& ... t) {
  const size_t num_param_regs = conv->num_parameter_registers;
  
  VM_LOADER loader ={};
  loader.vm = vm;
  loader.param_regs = conv->parameter_registers;
  loader.num_param_regs  = num_param_regs;

  if (sizeof...(T) > num_param_regs) {
    vm->SP -= (sizeof...(T) - num_param_regs) * 8;
  }

  (loader << ... << std::forward<T>(t));
}

void vm_call_native_x64(VM* const vm, const void* func_ptr, uint64_t num_params);
void vm_rum(VM* vm, Program* prog) noexcept;