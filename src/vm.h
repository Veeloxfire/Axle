#pragma once
#include "utility.h"
#include "bytecode.h"
#include "calling_conventions.h"

struct Reg64_8B {
  uint8_t padding[6];
  union {
    uint8_t high_reg;
    int8_t high_reg_s;
  };

  union {
    uint8_t low_reg;
    int8_t low_reg_s;
  };

  void zero_padding() noexcept;
};

struct Reg64_16B {
  uint8_t padding[6];
  union {
    uint16_t reg;
    int16_t reg_s;
  };

  void zero_padding() noexcept;
};

struct Reg64_32B {
  uint8_t padding[4];

  union {
    int32_t reg_s;
    uint32_t reg;
  };

  void zero_padding() noexcept;
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
    Reg64_8B  b8s;
    Reg64_16B b16;
    Reg64_32B b32;
    Reg64_64B b64;
  };
};

struct Function;

struct VM {

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

  void allocate_stack(uint64_t bytes) {
    SP -= bytes;

    if (SP <= stack) {
      throw std::exception("STACK OVERFLOW");
    }
  }

  void push(X64_UNION val) {
    SP -= 8;

    if (SP <= stack) {
      throw std::exception("STACK OVERFLOW");
      return;
    }
     
    x64_to_bytes(val, SP);
  }

  X64_UNION pop() {
    if (SP + 8 >= stack + STACK_SIZE) {
      throw std::exception("STACK UNDERFLOW");
      return { (uint64_t) 0 };
    }

    X64_UNION val = x64_from_bytes(SP);
    SP += 8;
    return val;
  }
};

ErrorCode vm_rum(VM* vm, const uint8_t* code, size_t entry_point) noexcept;