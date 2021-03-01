#pragma once
#include "utility.h"
#include "bytecode.h"//for SP_REG and BP_REG

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

struct VM {
  constexpr static size_t STACK_SIZE = 2048;
  uint8_t stack[STACK_SIZE] = {};

  enum FLAGS : uint64_t {
    CF_MASK = 0x0001,
    PF_MASK = 0x0004,
    AF_MASK = 0x0010,
    ZF_MASK = 0x0040,
    SF_MASK = 0x0080,
    TF_MASK = 0x0100,
    IF_MASK = 0x0200,
    DF_MASK = 0x0400,
    OF_MASK = 0x0800,
  };

  uint64_t flags = 0;

  Register registers[14] = {};

  const uint8_t* IP = nullptr;

  VM() {
    registers[RSP.REG].full.ptr = stack + STACK_SIZE;
    registers[RBP.REG].full.ptr = stack + STACK_SIZE;
  }

  //true if stack overflow, false otherwise
  constexpr bool decrement_sp(int64_t t) {
    if (stack + t > registers[RSP.REG].full.ptr) {
      return true;
    }

    registers[RSP.REG].full.ptr -= t;

      return false;
  }
  void set_ip_to_reg(uint8_t reg) noexcept;

  void x64_add_regs(uint8_t from, uint8_t to) noexcept;
  void x64_sub_regs(uint8_t from, uint8_t to) noexcept;
  void x64_cmp_regs(uint8_t from, uint8_t to) noexcept;
  void x64_mul_regs(uint8_t from, uint8_t to) noexcept;
  void x64_div_regs(uint8_t from, uint8_t to) noexcept;
  void x64_or_regs(uint8_t from, uint8_t to) noexcept;
  void x64_and_regs(uint8_t from, uint8_t to) noexcept;

  void x64_add_64_to_reg(uint64_t from, uint8_t to) noexcept;
  void x64_sub_64_to_reg(uint64_t from, uint8_t to) noexcept;
  void x64_cmp_64_to_reg(uint64_t from, uint8_t to) noexcept;
  void x64_mul_64_to_reg(uint64_t from, uint8_t to) noexcept;
  void x64_div_64_to_reg(uint64_t from, uint8_t to) noexcept;

  void x64_mov_regs(uint8_t from, uint8_t to) noexcept;
  void x64_mov_reg_to_mem(uint8_t from, uint8_t to, int64_t disp) noexcept;
  void x64_mov_mem_to_reg(uint8_t from, int64_t disp, uint8_t to) noexcept;
  void x64_mov_64_to_reg(uint64_t from, uint8_t to) noexcept;
  void x64_mov_64_to_mem(uint64_t from, uint8_t to, int64_t disp) noexcept;

  constexpr void set_reg_to_flags(const uint8_t reg, const uint64_t mask) {
    registers[reg].full.reg = (flags & mask) > 0;
  }
  
  constexpr void set_value_flags(const uint64_t val) {
    //Is Signed
    flags = (flags & ~SF_MASK) | (SF_MASK * ((signed)val < 0));

    //Is Zero
    flags = (flags & ~ZF_MASK) | (ZF_MASK * (val == 0));
  }
};

ErrorCode vm_rum(VM* vm, const Function* function) noexcept;