#pragma once
#include "utility.h"

namespace X64 {
  struct SIB {
    bool use_base = false;
    bool use_index = false;

    uint8_t base = 0;
    uint8_t index = 0;
    uint8_t scale = 0;
    int32_t disp = 0;
  };

  struct R {
    uint8_t r;
  };

  struct IMM32 {
    bool sign_extend = false;
    uint32_t imm = 0;
  };

  struct RM {
    uint8_t r;//or base

    bool indirect = false;
    bool use_sib = false;

    union {
      SIB sib;
      int32_t disp = 0;
    };
  };

  inline constexpr uint8_t rex_r(uint8_t reg) {
    return (reg & 0b0000'1000) >> 1;
  }

  inline constexpr uint8_t rex_b(uint8_t reg) {
    return (reg & 0b0000'1000) >> 3;
  }

  inline constexpr uint8_t rex_rm(uint8_t reg) {
    return rex_b(reg);
  }

  inline constexpr uint8_t rex_r_rm(uint8_t r, uint8_t rm) {
    return rex_r(r) | rex_b(rm);
  }

  inline constexpr uint8_t modrm_reg(uint8_t reg) {
    return (reg & 0b0000'0111) << 3;
  }
  inline constexpr uint8_t modrm_rm(uint8_t reg) {
    return (reg & 0b0000'0111);
  }

  inline constexpr uint8_t modrm_r_rm(uint8_t r, uint8_t rm) {
    return modrm_reg(r) | modrm_rm(rm);
  }

  enum SIB_SCALE : uint8_t {
    SIB_SCALE_1 = 0b00'000'000,
    SIB_SCALE_2 = 0b01'000'000,
    SIB_SCALE_4 = 0b10'000'000,
    SIB_SCALE_8 = 0b11'000'000,

    SIB_INDEX_MASK = 0b00'111'000,
    SIB_BASE_MASK  = 0b00'000'111,
  };

  inline constexpr uint8_t sib_scale(uint8_t scale) {
    switch (scale)
    {
      case 1: return SIB_SCALE_1;
      case 2: return SIB_SCALE_2;
      case 4: return SIB_SCALE_4;
      case 8: return SIB_SCALE_8;
      default:
        return 0;
    }
  }

  inline constexpr uint8_t sib_i_b(uint8_t i, uint8_t b) {
    return ((i & 0b111) << 3) | (b & 0b111);
  }

  inline constexpr uint8_t sib(uint8_t s, uint8_t i, uint8_t b) {
    return sib_scale(s) | sib_i_b(i, b);
  }


  enum MODRM_MOD_CODES : uint8_t {
    MODRM_MOD_DIRECT   = 0b11'000'000,
    MODRM_MOD_INDIRECT = 0b00'000'000,

    MODRM_REG_MASK = 0b00'111'000,
    MODRM_RM_MASK  = 0b00'000'111,

    MODRM_REG_SHIFT = 3,
  };

  enum REX_CODES : uint8_t {
    REX   = 0b01000000,
    REX_W = 0b01001000,
    REX_R = 0b00000100,
    REX_X = 0b00000010,
    REX_B = 0b00000001,

    REX_R_SHIFT = 1,
    REX_B_SHIFT = 3,
  };

  enum Opcode : uint8_t {
    ADD_R_TO_RM = 0x01,
    OR_R_TO_RM = 0x09,
    AND_R_TO_RM = 0x21,
    SUB_R_TO_RM = 0x29,
    CMP_R_TO_RM = 0x39,
    PUSH_R = 0x50,//+ register
    POP_R = 0x58,//+ register
    SUB_32_TO_RM = 0x81,// r = 5
    CMP_32_TO_RM = 0x81,// r = 7
    JZ_NEAR = 0x84,
    JNE_NEAR = 0x85,
    MOV_R_TO_RM = 0x89,
    MOV_RM_TO_R = 0x8B,
    SETE_RM8 = 0x94,
    CQO = 0x99,
    IMUL_RM_TO_R = 0xAF,
    MOV_ZX_RM8_TO_R = 0xB6,
    MOV_SX_RM8_TO_R = 0xBE,
    MOV_64_TO_R = 0xB8,//+ register
    RET_NEAR = 0xC3,
    MOV_IMM32_RM = 0xC7,
    CALL_NEAR = 0xE8,
    JMP_NEAR = 0xE9,
    NEG_RM = 0xF7,//        r = 3
    MUL_RM_TO_RAX = 0xF7,// r = 4
    DIV_RM_TO_RAX = 0xF7,// r = 6
    IDIV_RM_TO_RAX = 0xF7,// r = 7
  };

  void mov(Array<uint8_t>& arr,
           uint8_t from,
           uint8_t to);

  void mov(Array<uint8_t>& arr,
           R r,
           const RM& rm);

  void mov(Array<uint8_t>& arr,
           const RM& rm,
           R r);

  void mov(Array<uint8_t>& arr,
           R r,
           uint64_t u64);

  void mov(Array<uint8_t>& arr,
           const RM& rm,
           IMM32 u32);

  void sub(Array<uint8_t>& arr,
           uint8_t r,
           uint8_t rm);

  void sub(Array<uint8_t>& arr,
           uint8_t rm,
           int32_t i32);

  void ret(Array<uint8_t>& arr);

  void push(Array<uint8_t>& arr, uint8_t reg);
  void pop(Array<uint8_t>& arr, uint8_t reg);
}

struct Compiler;
struct CodeBlock;

size_t vm_backend(Array<uint8_t>& out_code, const Compiler*);
size_t vm_backend_single_func(Array<uint8_t>& out_code, const CodeBlock* code, uint64_t labels);

size_t x86_64_machine_code_backend(Array<uint8_t>& out_code, const Compiler* comp);

void print_x86_64(const uint8_t* bytes, size_t size);