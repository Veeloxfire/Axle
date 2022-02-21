#pragma once
#include "utility.h"
#include "Program.h"

struct MemComplex;

enum struct RELOCATION_TYPE : u8 {
  U64_LABEL_OFFSET,
  U64_DATA_OFFSET,
  I32_RELATIVE_TO_NEXT,//other_offset is next instruction 
};

struct Relocation {
  RELOCATION_TYPE type;
  size_t value_offset;
  size_t other_offset;
};

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

  struct R8 {
    R r;
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

  struct RM8 {
    RM rm;
  };

  RM rm_from_mem_complex(const MemComplex& mem);

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

  enum OVERIDE_CODE : u8 {
    OVERIDE_OPERAND = 0x66,
    OVERIDE_ADDRESS = 0x67,
  };


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
    XOR_R_TO_RM = 0x33,
    CMP_R_TO_RM = 0x39,
    PUSH_R = 0x50,//+ register
    POP_R = 0x58,//+ register
    SUB_32_TO_RM = 0x81,// r = 5
    CMP_32_TO_RM = 0x81,// r = 7
    JB_NEAR = 0x82,
    JNB_NEAR = 0x83,
    JZ_NEAR = 0x84,
    JE_NEAR = 0x84,
    JNE_NEAR = 0x85,
    JNA_NEAR = 0x86,
    JBE_NEAR = 0x86,
    JA_NEAR = 0x87,
    MOV_R8_TO_RM8 = 0x88,
    MOV_R_TO_RM = 0x89,
    MOV_RM8_TO_R8 = 0x8A,
    MOV_RM_TO_R = 0x8B,
    JL_NEAR = 0x8C,
    LEA_RM_TO_R = 0x8D,
    JNL_NEAR = 0x8D,
    JNG_NEAR = 0x8E,
    JG_NEAR = 0x8F,
    SETE_RM8 = 0x94,
    SETL_RM8 = 0x9C,
    SETG_RM8 = 0x9F,
    CQO = 0x99,
    IMUL_RM_TO_R = 0xAF,
    MOV_ZX_RM8_TO_R = 0xB6,
    MOV_SX_RM8_TO_R = 0xBE,
    MOV_8_TO_R8 = 0xB0,//+ register
    MOV_64_TO_R = 0xB8,//+ register
    RET_NEAR = 0xC3,
    MOV_IMM32_RM = 0xC7,
    SAL_R_BY_CL = 0xD3,// r = 4
    SHR_R_BY_CL = 0xD3,// r = 5
    SAR_R_BY_CL = 0xD3,// r = 7
    CALL_NEAR = 0xE8,
    JMP_NEAR = 0xE9,
    NEG_RM = 0xF7,// r = 3
    MUL_RM_TO_RAX = 0xF7,// r = 4
    DIV_RM_TO_RAX = 0xF7,// r = 6
    IDIV_RM_TO_RAX = 0xF7,// r = 7
    CALL_NEAR_ABS = 0xFF,//r = 2
  };

  void mov(Array<uint8_t>& arr,
           uint8_t from,
           uint8_t to);

  void mov(Array<uint8_t>& arr,
           R r,
           const RM& rm);

  void mov(Array<uint8_t>& arr,
           R8 r8,
           const RM8& rm8);

  void mov(Array<uint8_t>& arr,
           const RM8& rm8,
           R8 r8);

  void mov(Array<uint8_t>& arr,
           const RM& rm,
           R r);

  void lea(Array<uint8_t>& arr,
           const RM& rm,
           R r);

  void mov(Array<uint8_t>& arr,
           R r,
           uint64_t u64);

  void mov(Array<uint8_t>& arr,
           R8 r8,
           uint8_t u8);

  void mov(Array<uint8_t>& arr,
           const RM& rm,
           u16 imm16);

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
struct System;

void compile_backend_single_func(Program* prog,
                                 const CodeBlock* code,
                                 Compiler* const comp,
                                 const System* system);

void compile_backend(Program* prog, Compiler* comp, const System* system);

void nasm_backend(const char* file_name, Compiler* comp);

void vm_backend_code_block(Compiler* const,
                           Program*,
                           Array<uint8_t>&,
                           const CodeBlock*,
                           size_t*,
                           Array<Relocation>&);

void x86_64_backend_code_block(Compiler* const,
                               Program*,
                               Array<uint8_t>&,
                               const CodeBlock*,
                               size_t*,
                               Array<Relocation>&);

void print_x86_64(const uint8_t* bytes, size_t size);