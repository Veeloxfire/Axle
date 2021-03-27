#pragma once
#include "utility.h"
#include "bytecode.h"

struct Reg64_8B {
  uint8_t padding[6];
  uint8_t high_reg;
  uint8_t low_reg;

  void zero_padding() noexcept;
};

struct Reg64_16B {
  uint8_t padding[6];
  uint16_t reg;

  void zero_padding() noexcept;
};

struct Reg64_32B {
  uint8_t padding[4];
  uint32_t reg;

  void zero_padding() noexcept;
};

struct Reg64_64B {
  union {
    uint64_t reg;
    uint8_t* ptr;
  };
};

struct Register {
  union {
    Reg64_8B  low_low_split;
    Reg64_16B low_low_half;
    Reg64_32B low_half;
    Reg64_64B full;
  };
};

struct Function;

struct VM {
  constexpr static size_t STACK_SIZE = 256;
  uint64_t stack[STACK_SIZE] ={};

  uint64_t* BP = stack + STACK_SIZE - 1;
  uint64_t* SP = stack + STACK_SIZE - 1;
  const uint8_t* IP = nullptr;

  Register registers[NUM_VM_REGS] ={};


  VM() = default;

  void push(X64_UNION val) {
    *SP = val.val;

    if (SP <= stack) {
      throw std::exception("STACK OVERFLOW");
    }
    else {
      SP -= 1;
    }
  }

  X64_UNION pop() {
    if (SP >= stack + STACK_SIZE) {
      throw std::exception("STACK UNDERFLOW");
    }
    else {
      SP += 1;
    }
    return *SP;
  }
};

ErrorCode vm_rum(VM* vm, const uint8_t* code, size_t entry_point) noexcept;