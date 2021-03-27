#pragma once
#include "utility.h"
#ifdef MACHINE_CODE

enum struct REGISTER_8B : uint8_t {
  //Low bits
  AL = 0, CL = 1, DL = 2, BL = 3,

  //High bits
  AH = 4, CH = 5, DH = 6, BH = 7,
};
enum struct REGISTER_8B_REX : uint8_t {
  AL = 0, CL = 1, DL = 2, BL = 3,
  SPL = 4, BPL = 5, SIL = 6, DIL = 7,
  R8L = 8, R9L = 9, R10L = 10, R11L = 11,
  R12L = 12, R13L = 13, R14L = 14, R15L = 15,
};

enum struct REGISTER_16B : uint8_t {
  AX = 0, CX = 1, DX = 2, BX = 3,
  SP = 4, BP = 5, SI = 6, DI = 7,
};
enum struct REGISTER_16B_REX : uint8_t {
  R8W = 0, R9W = 1, R10W = 2, R11W = 3,
  R12W = 4, R13W = 5, R14W = 6, R15W = 7,
};

enum struct REGISTER_32B : uint8_t {
  EAX = 0, ECX = 1, EDX = 2, EBX = 3,
  ESP = 4, EBP = 5, ESI = 6, EDI = 7,
};
enum struct REGISTER_32B_REX : uint8_t {
  EAX = 0, ECX = 1, EDX = 2, EBX = 3,
  ESP = 4, EBP = 5, ESI = 6, EDI = 7,
  R8D = 8, R9D = 9, R10D = 10, R11D = 11,
  R12D = 12, R13D = 13, R14D = 14, R15D = 15,
};

enum struct REGISTER_64B : uint8_t {
  RAX = 0, RCX = 1, RDX = 2, RBX = 3,
  RSP = 4, RBP = 5, RSI = 6, RDI = 7,
  R8 = 8, R9 = 9, R10 = 10, R11 = 11,
  R12 = 12, R13 = 13, R14 = 14, R15 = 15,
};


#endif

namespace X64 {

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

  enum MODRM_MOD_CODES : uint8_t {
    MODRM_MOD_DIRECT = 0b11000000,
    MODRM_MOD_INDIRECT = 0b00000000,
  };

  enum REX_CODES : uint8_t {
    REX   = 0b01000000,
    REX_W = 0b01001000,
    REX_R = 0b01000100,
    REX_X = 0b01000010,
    REX_B = 0b01000001,
  };

  enum REGISTER_64B : uint8_t {
    RAX = 0, RCX = 1, RDX = 2, RBX = 3,
    RSP = 4, RBP = 5, RSI = 6, RDI = 7,
    R8 = 8, R9 = 9, R10 = 10, R11 = 11,
    R12 = 12, R13 = 13, R14 = 14, R15 = 15,
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
    SETE = 0x94,
    MOV_64_TO_R = 0xB8,//+ register
    IMUL_RM_TO_R = 0xAF,
    RET_NEAR = 0xC3,
    CALL_NEAR = 0xE8,
    JMP_NEAR = 0xE9,
    DIV_RM_TO_RAX = 0xF7,
  };
}

struct Compiler;

size_t vm_backend(Array<uint8_t>& out_code, const Compiler*);
size_t x86_64_backend(Array<uint8_t>& out_code, const Compiler* comp);