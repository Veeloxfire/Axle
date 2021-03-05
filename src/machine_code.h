#include "utility.h"

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



namespace X64 {

  inline constexpr uint8_t rex_r(uint8_t reg) {
    return (reg & 0b0000'1000) >> 1;
  }

  inline constexpr uint8_t rex_b(uint8_t reg) {
    return (reg & 0b0000'1000) >> 3;
  }

  inline constexpr uint8_t modrm_reg(uint8_t reg) {
    return (reg & 0b0000'0111) << 3;
  }
  inline constexpr uint8_t modrm_rm(uint8_t reg) {
    return (reg & 0b0000'0111);
  }

  enum MODRM_MOD_CODES : uint8_t {
    MODRM_MOD_INDIRECT = 0b11000000,
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
    PUSH_R = 0x50,//+ register
    POP_R = 0x58,//+ register
    SUB_32_TO_RM = 0x81,
    MOV_R_TO_RM = 0x89,
    MOV_64_TO_R = 0xB8,//+ register
    SHORT_RET = 0xC3,

  };
}

void convert_to_x64_machine_code(Array<uint8_t>& arr,
                                 const uint8_t* bytecode, const size_t length);