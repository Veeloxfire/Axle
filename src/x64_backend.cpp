#include "ir.h"
#include "compiler.h"
#include "x64_backend.h"

namespace X64 {
  struct JumpRelocation {
    u32 offset_to_immediate;
    IR::LocalLabel jump_to;
  };

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

  struct R8 : R {
    explicit constexpr R8(const R& r_) : R(r_) {}
  };
  struct R16 : R {
    explicit constexpr R16(const R& r_) : R(r_) {}
  };
  struct R32 : R {
    explicit constexpr R32(const R& r_) : R(r_) {}
  };
  struct R64 : R {
    explicit constexpr R64(const R& r_) : R(r_) {}
  };

  struct IMM64 {
    uint64_t imm = 0;
  };

  struct IMM32Extended {
    uint32_t imm = 0;
  };

  struct IMM32 {
    uint32_t imm = 0;
  };

  struct IMM16 {
    uint16_t imm = 0;
  };

  struct IMM8 {
    uint8_t imm = 0;
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

  struct RM8 : RM {
    explicit constexpr RM8(const RM& r_) : RM(r_) {}
  };
  struct RM16 : RM {
    explicit constexpr RM16(const RM& r_) : RM(r_) {}
  };
  struct RM32 : RM {
    explicit constexpr RM32(const RM& r_) : RM(r_) {}
  };
  struct RM64 : RM {
    explicit constexpr RM64(const RM& r_) : RM(r_) {}
  };

  static constexpr uint8_t rex_r(uint8_t reg) {
    return (reg & 0b0000'1000) >> 1;
  }

  static constexpr uint8_t rex_b(uint8_t reg) {
    return (reg & 0b0000'1000) >> 3;
  }

  static constexpr uint8_t rex_rm(uint8_t reg) {
    return rex_b(reg);
  }

  static constexpr bool need_rex(u8 reg) {
    return (reg & 0b1000) == 0b1000;
  }

  static constexpr uint8_t rex_r_rm(uint8_t r, uint8_t rm) {
    return rex_r(r) | rex_b(rm);
  }

  static constexpr uint8_t modrm_reg(uint8_t reg) {
    return (reg & 0b0000'0111) << 3;
  }
  static constexpr uint8_t modrm_rm(uint8_t reg) {
    return (reg & 0b0000'0111);
  }

  static constexpr uint8_t modrm_r_rm(uint8_t r, uint8_t rm) {
    return modrm_reg(r) | modrm_rm(rm);
  }

  enum SIB_SCALE : uint8_t {
    SIB_SCALE_1 = 0b00'000'000,
    SIB_SCALE_2 = 0b01'000'000,
    SIB_SCALE_4 = 0b10'000'000,
    SIB_SCALE_8 = 0b11'000'000,

    SIB_INDEX_MASK = 0b00'111'000,
    SIB_BASE_MASK = 0b00'000'111,
  };

  static constexpr uint8_t sib_scale(uint8_t scale) {
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

  static constexpr uint8_t sib_i_b(uint8_t i, uint8_t b) {
    return ((i & 0b111) << 3) | (b & 0b111);
  }

  static constexpr uint8_t sib(uint8_t s, uint8_t i, uint8_t b) {
    return sib_scale(s) | sib_i_b(i, b);
  }

  static RM memory_rm(u8 reg, i32 disp, u8 index_reg, u8 scale) {
    ASSERT(scale == 1 || scale == 2 || scale == 4 || scale == 8);

    RM rm = {};
    rm.indirect = true;

    rm.r = rsp.REG;
    rm.use_sib = true;

    ASSERT((reg & 0b111) != rsp.REG);//Not possible in x86 architecture


    rm.sib.use_base = true;
    rm.sib.use_index = true;
    rm.sib.base = reg;
    rm.sib.disp = disp;
    rm.sib.scale = scale;
    rm.sib.index = index_reg;
  }

  static RM memory_rm(u8 reg, i32 disp) {
    RM rm = {};
    rm.indirect = true;

    if ((reg & 0b111) != rsp.REG) {
      rm.use_sib = false;
      rm.r = reg;
      rm.disp = disp;
    }
    else {
      //have to encode using SIB unfortunately
      rm.r = rsp.REG;
      rm.use_sib = true;

      rm.sib.use_base = true;
      rm.sib.use_index = false;
      rm.sib.base = rsp.REG;
      rm.sib.disp = disp;
    }

    return rm;
  }

  enum OVERRIDE_CODE : u8 {
    OVERRIDE_OPERAND = 0x66,
    OVERRIDE_ADDRESS = 0x67,
  };


  enum MODRM_MOD_CODES : uint8_t {
    MODRM_MOD_DIRECT = 0b11'000'000,
    MODRM_MOD_INDIRECT = 0b00'000'000,

    MODRM_REG_MASK = 0b00'111'000,
    MODRM_RM_MASK = 0b00'000'111,

    MODRM_REG_SHIFT = 3,
  };

  enum REX_CODES : uint8_t {
    REX = 0b01000000,
    REX_W = REX | 0b1000,
    REX_R = 0b00000100,
    REX_X = 0b00000010,
    REX_B = 0b00000001,

    REX_R_SHIFT = 1,
    REX_B_SHIFT = 3,
  };

  enum Opcode : uint8_t {
    ADD_R8_TO_RM8 = 0x01,
    ADD_R_TO_RM = 0x01,
    OR_R8_TO_RM8 = 0x08,
    OR_R_TO_RM = 0x09,
    AND_R8_TO_RM8 = 0x20,
    AND_R_TO_RM = 0x21,
    SUB_R8_TO_RM8 = 0x28,
    SUB_R_TO_RM = 0x29,
    XOR_R8_TO_RM8 = 0x30,
    XOR_R_TO_RM = 0x31,
    CMP_R8_TO_RM8 = 0x38,
    CMP_R_TO_RM = 0x39,
    CMP_IMM_TO_AX = 0x3C,
    PUSH_R = 0x50,//+ register
    POP_R = 0x58,//+ register
    MOV_SXD_R_TO_R = 0x63,
    CMP_IMM_TO_RM8 = 0x80,// r = 7
    SUB_32_TO_RM = 0x81,// r = 5
    CMP_IMM_TO_RM = 0x81,// r = 7
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
    SETNE_RM8 = 0x95,
    SETL_RM8 = 0x9C,
    SETG_RM8 = 0x9F,
    CQO = 0x99,
    IMUL_RM_TO_R = 0xAF,
    MOV_ZX_RM8_TO_R = 0xB6,
    MOV_ZX_RM16_TO_R = 0xB7,
    MOV_SX_RM8_TO_R = 0xBE,
    MOV_SX_RM16_TO_R = 0xBE,
    MOV_8_TO_R8 = 0xB0,//+ register
    MOV_IMM_TO_R = 0xB8,//+ register
    RET_NEAR = 0xC3,
    MOV_IMM8_RM = 0xC6,
    MOV_IMM32_RM = 0xC7,
    SAL_R_BY_CL = 0xD3,// r = 4
    SHR_R_BY_CL = 0xD3,// r = 5
    SAR_R_BY_CL = 0xD3,// r = 7
    CALL_NEAR = 0xE8,
    JMP_NEAR = 0xE9,
    JMP_ABS_MEM = 0xFF,// r = 4
    NEG_RM = 0xF7,// r = 3
    MUL_RM_TO_RAX = 0xF7,// r = 4
    DIV_RM_TO_RAX = 0xF7,// r = 6
    IDIV_RM_TO_RAX = 0xF7,// r = 7
    CALL_NEAR_ABS = 0xFF,//r = 2
  };

  constexpr bool can_signed_compress(i64 i) {
    u32 i_smol = static_cast<u64>(i) & 0xffffffff;

    return static_cast<i64>(static_cast<i32>(i_smol)) == i;
  }

  struct Instruction {
    u8 count;
    u8 bytes[15];

    u8* sub_range(u8 sub_size) {
      ASSERT(sub_size > 0);
      ASSERT(sub_size <= 15);
      ASSERT(15 - sub_size > count);
      u8* d = bytes + count;
      count += sub_size;
      return d;
    }

    void insert(u8 byte) {
      ASSERT(count < 15);
      bytes[count] = byte;
      count += 1;
    }
  };

  static void emit_sib(Instruction& arr, const X64::SIB& sib, uint8_t mod_byte) {
    if (!sib.use_base) {
      ASSERT(!sib.use_index);//Must use both if using index

      if (!sib.use_index) {
        arr.insert(0b00'000'000 | mod_byte);
        arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(rsp.REG, rbp.REG));

        u8* s = arr.sub_range(4);
        memcpy_s(s, 4, &sib.disp, 4);
      }
      else {
        ASSERT(sib.index != rsp.REG);//Not a valid code unfortunately

        arr.insert(0b00'000'000 | mod_byte);
        arr.insert(X64::sib(sib.scale, sib.index, rbp.REG));

        u8* s = arr.sub_range(4);
        memcpy_s(s, 4, &sib.disp, 4);
      }
    }
    else if (sib.use_base && !sib.use_index) {
      if (sib.disp == 0 && (sib.base & 0b111) != rbp.REG) {
        arr.insert(0b00'000'000 | mod_byte);
        arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(rsp.REG, sib.base));
      }
      else if (-128 <= sib.disp && sib.disp <= 127) {
        arr.insert(0b01'000'000 | mod_byte);
        arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(rsp.REG, sib.base));
        arr.insert((uint8_t)sib.disp);
      }
      else {
        arr.insert(0b10'000'000 | mod_byte);
        arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(rsp.REG, sib.base));

        u8* s = arr.sub_range(4);
        memcpy_s(s, 4, &sib.disp, 4);
      }
    }
    else {
      //use base and index

      if (sib.disp == 0 && (sib.base & 0b111) != rbp.REG) {
        arr.insert(0b00'000'000 | mod_byte);
        arr.insert(X64::sib(sib.scale, sib.index, sib.base));
      }
      else if (-128 <= sib.disp && sib.disp <= 127) {
        arr.insert(0b01'000'000 | mod_byte);
        arr.insert(X64::sib(sib.scale, sib.index, sib.base));
        arr.insert((uint8_t)sib.disp);
      }
      else {
        arr.insert(0b10'000'000 | mod_byte);
        arr.insert(X64::sib(sib.scale, sib.index, sib.base));

        u8* s = arr.sub_range(4);
        memcpy_s(s, 4, &sib.disp, 4);
      }
    }
  }

  static void emit_mod_rm(Instruction& arr, const X64::R r, const X64::RM& rm) {
    if (!rm.indirect) {
      arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(r.r, rm.r));
      return;
    }

    switch (rm.r) {
      case rsp.REG:
      case r12.REG: {
          //SIB byte time

          if (rm.use_sib) {
            emit_sib(arr, rm.sib, X64::modrm_r_rm(r.r, rm.r));
          }
          else if (rm.disp == 0) {
            arr.insert(X64::MODRM_MOD_INDIRECT | X64::modrm_r_rm(r.r, rm.r));
            arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(rsp.REG, rm.r));
          }
          else if (-128 <= rm.disp && rm.disp <= 127) {
            arr.insert(0b01000000 | X64::modrm_r_rm(r.r, rm.r));
            arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(rsp.REG, rm.r));
            arr.insert((int8_t)rm.disp);
          }
          else {
            arr.insert(0b10000000 | X64::modrm_r_rm(r.r, rm.r));
            arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(rsp.REG, rm.r));

            u8* s = arr.sub_range(4);
            memcpy_s(s, 4, &rm.disp, 4);
          }
          break;
        }

      case rbp.REG:
      case r13.REG: {
          //Encode disp 0 by having a 1 byte disp of 0
          goto RM_DEFAULT;
        }

      default: {
          if (rm.disp == 0) {
            arr.insert(X64::MODRM_MOD_INDIRECT | X64::modrm_r_rm(r.r, rm.r));
            break;
          }

        RM_DEFAULT:
          if (-128 <= rm.disp && rm.disp <= 127) {
            arr.insert(0b01000000 | X64::modrm_r_rm(r.r, rm.r));
            arr.insert((int8_t)rm.disp);
            break;
          }
          else {
            arr.insert(0b10000000 | X64::modrm_r_rm(r.r, rm.r));

            u8* s = arr.sub_range(4);
            memcpy_s(s, 4, &rm.disp, 4);
            break;
          }
        }
    }
  }

  static void mov(Instruction& arr, R64 from, R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(from.r, to.r));
    arr.insert(X64::MOV_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void mov(Instruction& arr,
                  R32 from,
                  R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::MOV_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void mov(Instruction& arr,
                  R16 from,
                  R16 to) {
    arr.insert(X64::OVERRIDE_OPERAND);

    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::MOV_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void mov(Instruction& arr,
                  R64 from,
                  const RM64& to) {

    arr.insert(X64::REX_W | X64::rex_r_rm(from.r, to.r));
    arr.insert(X64::MOV_R_TO_RM);

    emit_mod_rm(arr, from, to);
  }

  static void mov(Instruction& arr,
                  R32 from,
                  const RM32& to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::MOV_R_TO_RM);

    emit_mod_rm(arr, from, to);
  }

  static void mov(Instruction& arr,
                  R16 from,
                  const RM16& to) {
    arr.insert(X64::OVERRIDE_OPERAND);

    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::MOV_R_TO_RM);

    emit_mod_rm(arr, from, to);
  }

  static void mov(Instruction& arr,
                  R8 from,
                  R8 to) {

    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::MOV_R8_TO_RM8);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void mov(Instruction& arr,
                  R8 from,
                  const RM8& to) {

    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::MOV_R8_TO_RM8);

    emit_mod_rm(arr, from, to);
  }

  static void mov(Instruction& arr,
                  const RM8& from,
                  R8 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::MOV_RM8_TO_R8);

    emit_mod_rm(arr, to, from);
  }

  static void mov(Instruction& arr,
                  const RM64& rm,
                  R64 r) {
    arr.insert(X64::REX_W | X64::rex_r_rm(r.r, rm.r));
    arr.insert(X64::MOV_RM_TO_R);

    emit_mod_rm(arr, r, rm);
  }

  static void mov(Instruction& arr,
                  const RM32& rm,
                  R32 r) {
    if (need_rex(r.r) || need_rex(rm.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(r.r, rm.r));
    }
    arr.insert(X64::MOV_RM_TO_R);

    emit_mod_rm(arr, r, rm);
  }

  static void mov(Instruction& arr,
                  const RM16& rm,
                  R16 r) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(r.r) || need_rex(rm.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(r.r, rm.r));
    }
    arr.insert(X64::MOV_RM_TO_R);

    emit_mod_rm(arr, r, rm);
  }

  static usize MOV_IM64_OFFSET = 2;
  static void mov(Instruction& arr,
                  R64 r,
                  IMM64 imm64) {
    arr.insert(X64::REX_W | X64::rex_rm(r.r));//this takes rm not r for some reason
    arr.insert(X64::MOV_IMM_TO_R + (r.r & 0b111));

    ASSERT(arr.count == MOV_IM64_OFFSET);
    u8* im = arr.sub_range(8);
    x64_to_bytes(imm64.imm, im);
  }

  static void mov(Instruction& arr,
                  R32 r,
                  IMM32 imm32) {
    if (need_rex(r.r)) {
      arr.insert(X64::REX | X64::rex_rm(r.r));//this takes rm not r for some reason
    }

    arr.insert(X64::MOV_IMM_TO_R + (r.r & 0b111));

    u8* im = arr.sub_range(4);
    x32_to_bytes(imm32.imm, im);
  }

  static void mov(Instruction& arr,
                  R16 r,
                  IMM16 imm16) {
    arr.insert(X64::OVERRIDE_OPERAND);//16 bit mode

    if (need_rex(r.r)) {
      arr.insert(X64::REX | X64::rex_rm(r.r));//this takes rm not r for some reason
    }

    arr.insert(X64::MOV_IMM_TO_R + (r.r & 0b111));

    u8* im = arr.sub_range(2);
    x32_to_bytes(imm16.imm, im);
  }

  static void mov(Instruction& arr,
                  R8 r,
                  IMM8 imm8) {
    if (need_rex(r.r)) {
      arr.insert(X64::REX | X64::rex_rm(r.r));//this takes rm not r for some reason
    }

    arr.insert(X64::MOV_8_TO_R8 + (r.r & 0b111));
    arr.insert(imm8.imm);
  }

  static void mov(Instruction& arr,
                  const RM& rm,
                  IMM32 imm32) {
    if ((rm.r & 0b1000) > 0) {
      arr.insert(X64::REX | X64::rex_b(rm.r));
    }

    arr.insert(X64::MOV_IMM32_RM);

    emit_mod_rm(arr, R{ '\0' }, rm);

    u8* im = arr.sub_range(4);
    x32_to_bytes(imm32.imm, im);
  }

  static void mov(Instruction& arr,
                  const RM& rm,
                  IMM32Extended imm32) {
    arr.insert(X64::REX_W | X64::rex_b(rm.r));
    arr.insert(X64::MOV_IMM32_RM);

    emit_mod_rm(arr, R{ '\0' }, rm);

    u8* im = arr.sub_range(4);
    x32_to_bytes(imm32.imm, im);
  }

  static void mov(Instruction& arr,
                  const RM& rm,
                  IMM16 imm16) {
    //Used to tell the instruction this is a 16 bit operand
    arr.insert(X64::OVERRIDE_OPERAND);

    if ((rm.r & 0b1000) > 0) {
      arr.insert(X64::REX | X64::rex_b(rm.r));
    }

    //this is correct even though it looks wrong
    //the 16 bit version is just the 32 bit version with some other flags
    arr.insert(X64::MOV_IMM32_RM);

    emit_mod_rm(arr, R{ '\0' }, rm);

    u8* im = arr.sub_range(2);
    x32_to_bytes(imm16.imm, im);
  }

  static void mov(Instruction& arr,
                  const RM& rm,
                  IMM8 imm8) {

    if ((rm.r & 0b1000) > 0) {
      arr.insert(X64::REX | X64::rex_b(rm.r));
    }

    arr.insert(X64::MOV_IMM8_RM);
    emit_mod_rm(arr, R{ '\0' }, rm);

    arr.insert(imm8.imm);
  }


  static void movzx(Instruction& arr,
                    R8 from,
                    R16 to) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::MOV_ZX_RM8_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void movzx(Instruction& arr,
                    R8 from,
                    R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::MOV_ZX_RM8_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void movzx(Instruction& arr,
                    R8 from,
                    R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(to.r, from.r));
    arr.insert(0x0F);
    arr.insert(X64::MOV_ZX_RM8_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void movzx(Instruction& arr,
                    R16 from,
                    R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::MOV_ZX_RM16_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void movzx(Instruction& arr,
                    R16 from,
                    R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(to.r, from.r));
    arr.insert(0x0F);
    arr.insert(X64::MOV_ZX_RM16_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void movzx(Instruction& arr,
                    const RM8& from,
                    R16 to) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::MOV_ZX_RM8_TO_R);
    emit_mod_rm(arr, to, from);
  }

  static void movzx(Instruction& arr,
                    const RM8& from,
                    R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::MOV_ZX_RM8_TO_R);
    emit_mod_rm(arr, to, from);
  }

  static void movzx(Instruction& arr,
                    const RM8& from,
                    R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(to.r, from.r));
    arr.insert(0x0F);
    arr.insert(X64::MOV_ZX_RM8_TO_R);
    emit_mod_rm(arr, to, from);
  }

  static void movzx(Instruction& arr,
                    const RM16& from,
                    R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::MOV_ZX_RM16_TO_R);
    emit_mod_rm(arr, to, from);
  }

  static void movzx(Instruction& arr,
                    const RM16& from,
                    R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(to.r, from.r));
    arr.insert(0x0F);
    arr.insert(X64::MOV_ZX_RM16_TO_R);
    emit_mod_rm(arr, to, from);
  }

  static void movsx(Instruction& arr,
                    R8 from,
                    R16 to) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::MOV_SX_RM8_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void movsx(Instruction& arr,
                    R8 from,
                    R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::MOV_SX_RM8_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void movsx(Instruction& arr,
                    R8 from,
                    R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(to.r, from.r));
    arr.insert(0x0F);
    arr.insert(X64::MOV_SX_RM8_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void movsx(Instruction& arr,
                    R16 from,
                    R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::MOV_SX_RM16_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void movsx(Instruction& arr,
                    R16 from,
                    R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(to.r, from.r));
    arr.insert(0x0F);
    arr.insert(X64::MOV_SX_RM16_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void movsx(Instruction& arr,
                    R32 from,
                    R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(to.r, from.r));
    arr.insert(0x0F);
    arr.insert(X64::MOV_SXD_R_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void movsx(Instruction& arr,
                    const RM8& from,
                    R16 to) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::MOV_SX_RM8_TO_R);
    emit_mod_rm(arr, to, from);
  }

  static void movsx(Instruction& arr,
                    const RM8& from,
                    R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::MOV_SX_RM8_TO_R);
    emit_mod_rm(arr, to, from);
  }

  static void movsx(Instruction& arr,
                    const RM8& from,
                    R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(to.r, from.r));
    arr.insert(0x0F);
    arr.insert(X64::MOV_SX_RM8_TO_R);
    emit_mod_rm(arr, to, from);
  }

  static void movsx(Instruction& arr,
                    const RM16& from,
                    R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::MOV_SX_RM16_TO_R);
    emit_mod_rm(arr, to, from);
  }

  static void movsx(Instruction& arr,
                    const RM16& from,
                    R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(to.r, from.r));
    arr.insert(0x0F);
    arr.insert(X64::MOV_SX_RM16_TO_R);
    emit_mod_rm(arr, to, from);
  }

  static void movsx(Instruction& arr,
                    const RM32& from,
                    R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(to.r, from.r));
    arr.insert(0x0F);
    arr.insert(X64::MOV_SXD_R_TO_R);
    emit_mod_rm(arr, to, from);
  }

  static void lea(Instruction& arr,
                  const RM& rm,
                  R r) {
    arr.insert(X64::REX_W | X64::rex_r_rm(r.r, rm.r));
    arr.insert(X64::LEA_RM_TO_R);

    emit_mod_rm(arr, r, rm);
  }

  static void sub(Instruction& arr,
                  R64 rm,
                  IMM32 imm32) {
    arr.insert(X64::REX_W | X64::rex_rm(rm.r));
    arr.insert(X64::SUB_32_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(5, rm.r));

    u8* im = arr.sub_range(4);
    x32_to_bytes(imm32.imm, im);
  }

  static void push(Instruction& arr, R reg) {
    if ((reg.r & 0b0000'1000) > 0) {
      arr.insert(X64::REX_W | X64::REX_B);
    }

    arr.insert(X64::PUSH_R + (reg.r & 0b0000'0111));
  }

  static void pop(Instruction& arr, R reg) {
    if ((reg.r & 0b0000'1000) > 0) {
      arr.insert(X64::REX_W | X64::REX_B);
    }

    arr.insert(X64::POP_R + (reg.r & 0b0000'0111));
  }

  static void ret(Instruction& arr) {
    arr.insert(X64::RET_NEAR);
  }

  static usize CALL_NEAR_OFFSET = 1;

  static void call_near(Instruction& arr, i32 jump_offset) {
    arr.insert(X64::CALL_NEAR);

    ASSERT(CALL_NEAR_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset, l);
  }

  static usize JUMP_NEAR_OFFSET = 1;

  static void jump_near(Instruction& arr, i32 jump_offset) {
    arr.insert(X64::JMP_NEAR);

    ASSERT(JUMP_NEAR_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset, l);
  }

  struct IPOffset {
    i32 disp32;
  };

  static usize JUMP_IP_ABS_OFFSET = 2;

  static void jump_abs(Instruction& arr, IPOffset jump_offset) {
    arr.insert(X64::JMP_ABS_MEM);
    u8 modrm_byte = (4 << 3) | (rbp.REG);//this address it as an IP offset
    arr.insert(modrm_byte);

    ASSERT(JUMP_IP_ABS_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset.disp32, l);
  }

  static usize JUMP_CONDITION_OFFSET = 2;

  static void jump_zero(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JZ_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset, l);
  }

  static constexpr auto jump_equal = jump_zero;

  static void jump_not_equal(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JNE_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset, l);
  }

  static void jump_above(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JA_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset, l);
  }

  static void jump_not_above(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JNA_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset, l);
  }

  static void jump_below(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JB_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset, l);
  }

  static void jump_not_below(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JNB_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset, l);
  }

  static void jump_lesser(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JL_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset, l);
  }

  static void jump_not_lesser(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JNL_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset, l);
  }

  static void jump_greater(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JG_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset, l);
  }

  static void jump_not_greater(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JNG_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    u8* l = arr.sub_range(4);
    x32_to_bytes(jump_offset, l);
  }

  static void cmp(Instruction& arr, R8 rm, IMM8 imm8) {
    if (rm.r == rax.REG) {
      if (need_rex(rm.r)) {
        arr.insert(X64::REX | X64::rex_rm(rm.r));
      }
      arr.insert(X64::CMP_IMM_TO_AX);
      arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(7, rm.r));
      arr.insert(imm8.imm);
    }
    else {
      if (need_rex(rm.r)) {
        arr.insert(X64::REX | X64::rex_rm(rm.r));
      }
      arr.insert(X64::CMP_IMM_TO_RM8);
      arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(7, rm.r));
      arr.insert(imm8.imm);
    }
  }

  static void cmp(Instruction& arr, const RM8& rm, IMM8 imm8) {
    if (need_rex(rm.r)) {
      arr.insert(X64::REX | X64::rex_rm(rm.r));
    }
    arr.insert(X64::CMP_IMM_TO_RM8);
    emit_mod_rm(arr, { 7 }, rm);

    arr.insert(imm8.imm);
  }

  static void sete(Instruction& arr, R8 r) {
    if (need_rex(r.r)) {
      arr.insert(X64::REX | X64::rex_rm(r.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::SETE_RM8);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_rm(r.r));
  }

  static void setne(Instruction& arr, R8 r) {
    if (need_rex(r.r)) {
      arr.insert(X64::REX | X64::rex_rm(r.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::SETNE_RM8);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_rm(r.r));
  }

  static void setl(Instruction& arr, R8 r) {
    if (need_rex(r.r)) {
      arr.insert(X64::REX | X64::rex_rm(r.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::SETL_RM8);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_rm(r.r));
  }

  static void setg(Instruction& arr, R8 r) {
    if (need_rex(r.r)) {
      arr.insert(X64::REX | X64::rex_rm(r.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::SETG_RM8);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_rm(r.r));
  }

  static void add(Instruction& arr, R8 from, R8 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::ADD_R8_TO_RM8);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void add(Instruction& arr, R16 from, R16 to) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::ADD_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void add(Instruction& arr, R32 from, R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }
    arr.insert(X64::ADD_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT
               | X64::modrm_r_rm(from.r, to.r));
  }

  static void add(Instruction& arr, R64 from, R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(from.r, to.r));
    arr.insert(X64::ADD_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT
               | X64::modrm_r_rm(from.r, to.r));
  }

  static void cmp(Instruction& arr, R8 from, R8 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::CMP_R8_TO_RM8);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void cmp(Instruction& arr, R16 from, R16 to) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::CMP_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void cmp(Instruction& arr, R32 from, R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }
    arr.insert(X64::CMP_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT
               | X64::modrm_r_rm(from.r, to.r));
  }

  static void cmp(Instruction& arr, R64 from, R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(from.r, to.r));
    arr.insert(X64::CMP_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT
               | X64::modrm_r_rm(from.r, to.r));
  }

  static void sub(Instruction& arr, R8 from, R8 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::SUB_R8_TO_RM8);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void sub(Instruction& arr, R16 from, R16 to) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::SUB_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void sub(Instruction& arr, R32 from, R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }
    arr.insert(X64::SUB_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT
               | X64::modrm_r_rm(from.r, to.r));
  }

  static void sub(Instruction& arr, R64 from, R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(from.r, to.r));
    arr.insert(X64::SUB_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT
               | X64::modrm_r_rm(from.r, to.r));
  }

  static void and_(Instruction& arr, R8 from, R8 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::AND_R8_TO_RM8);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void and_(Instruction& arr, R16 from, R16 to) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::AND_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void and_(Instruction& arr, R32 from, R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }
    arr.insert(X64::AND_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT
               | X64::modrm_r_rm(from.r, to.r));
  }

  static void and_(Instruction& arr, R64 from, R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(from.r, to.r));
    arr.insert(X64::AND_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT
               | X64::modrm_r_rm(from.r, to.r));
  }

  static void or_(Instruction& arr, R8 from, R8 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::OR_R8_TO_RM8);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void or_(Instruction& arr, R16 from, R16 to) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::OR_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void or_(Instruction& arr, R32 from, R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }
    arr.insert(X64::OR_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT
               | X64::modrm_r_rm(from.r, to.r));
  }

  static void or_(Instruction& arr, R64 from, R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(from.r, to.r));
    arr.insert(X64::OR_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT
               | X64::modrm_r_rm(from.r, to.r));
  }

  static void xor_(Instruction& arr, R8 from, R8 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::XOR_R8_TO_RM8);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void xor_(Instruction& arr, R16 from, R16 to) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }

    arr.insert(X64::XOR_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from.r, to.r));
  }

  static void xor_(Instruction& arr, R32 from, R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(from.r, to.r));
    }
    arr.insert(X64::XOR_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT
               | X64::modrm_r_rm(from.r, to.r));
  }

  static void xor_(Instruction& arr, R64 from, R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(from.r, to.r));
    arr.insert(X64::XOR_R_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT
               | X64::modrm_r_rm(from.r, to.r));
  }

  static void append_instruction(Backend::Program* program, const Instruction& i) {
    program->code_store.push_arr(i.bytes, i.count);
  }
}

constexpr auto load_types_info() {
  struct FormatData {
    usize sizes[9];

    constexpr usize get_size(IR::Format f) const { return sizes[static_cast<usize>(f)]; }
  };

  FormatData d = {};

  d.sizes[static_cast<usize>(IR::Format::opaque)] = 0;
  d.sizes[static_cast<usize>(IR::Format::uint8)] = 1;
  d.sizes[static_cast<usize>(IR::Format::sint8)] = 1;
  d.sizes[static_cast<usize>(IR::Format::uint16)] = 2;
  d.sizes[static_cast<usize>(IR::Format::sint16)] = 2;
  d.sizes[static_cast<usize>(IR::Format::uint32)] = 4;
  d.sizes[static_cast<usize>(IR::Format::sint32)] = 4;
  d.sizes[static_cast<usize>(IR::Format::uint64)] = 8;
  d.sizes[static_cast<usize>(IR::Format::sint64)] = 8;

  return d;
}

static constexpr auto x64_types_info = load_types_info();

struct MemoryView {
  X64::RM rm = {};
  u32 size = {};

  u32 known_alignment = {};
};

enum struct ValueType : u8 {
  Register,
  Memory,
  Address,
};

struct LazyValue {
  ValueType value_type;
  union {
    MemoryView mem = {};
    X64::R reg;
  };
};

namespace Helpers {

  static void copy_address_to_reg(Backend::Program* program, const MemoryView& mem, X64::R r) {
    ASSERT(mem.known_alignment % 8 == 0);
    ASSERT(mem.size == 8);

    X64::Instruction i = {};
    X64::lea(i, mem.rm, r);

    X64::append_instruction(program, i);
  }

  static void copy_reg_to_reg(Backend::Program* program,
                              X64::R from, IR::Format f_format,
                              X64::R to, IR::Format t_format) {
    ASSERT(f_format != IR::Format::opaque && t_format != IR::Format::opaque);

    X64::Instruction inst = {};

    switch (f_format) {
      case IR::Format::uint8: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(inst, X64::R8{ from }, X64::R8{ to });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::movzx(inst, X64::R8{ from }, X64::R16{ to });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::movzx(inst, X64::R8{ from }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movzx(inst, X64::R8{ from }, X64::R64{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::sint8: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(inst, X64::R8{ from }, X64::R8{ to });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::movsx(inst, X64::R8{ from }, X64::R16{ to });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::movsx(inst, X64::R8{ from }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movsx(inst, X64::R8{ from }, X64::R64{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::uint16: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(inst, X64::R8{ from }, X64::R8{ to });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::mov(inst, X64::R16{ from }, X64::R16{ to });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::movzx(inst, X64::R16{ from }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movzx(inst, X64::R16{ from }, X64::R64{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::sint16: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(inst, X64::R8{ from }, X64::R8{ to });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::mov(inst, X64::R16{ from }, X64::R16{ to });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::movsx(inst, X64::R16{ from }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movsx(inst, X64::R16{ from }, X64::R64{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::uint32: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(inst, X64::R8{ from }, X64::R8{ to });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::mov(inst, X64::R16{ from }, X64::R16{ to });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::mov(inst, X64::R32{ from }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::mov(inst, X64::R32{ from }, X64::R32{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::sint32: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(inst, X64::R8{ from }, X64::R8{ to });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::mov(inst, X64::R16{ from }, X64::R16{ to });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::mov(inst, X64::R32{ from }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movsx(inst, X64::R32{ from }, X64::R64{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::uint64:
      case IR::Format::sint64: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(inst, X64::R8{ from }, X64::R8{ to });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::mov(inst, X64::R16{ from }, X64::R16{ to });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::mov(inst, X64::R32{ from }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::mov(inst, X64::R64{ from }, X64::R64{ to });

              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      default:
        INVALID_CODE_PATH("Cant handle other data formats");
    }

    X64::append_instruction(program, inst);
  }

  static void copy_mem_to_reg(Backend::Program* program,
                              const MemoryView& from, IR::Format f_format,
                              X64::R to, IR::Format t_format) {
    ASSERT(f_format != IR::Format::opaque && t_format != IR::Format::opaque);

    ASSERT(from.size == x64_types_info.get_size(f_format));
    ASSERT(from.known_alignment == from.size);

    X64::Instruction inst = {};

    switch (f_format) {
      case IR::Format::uint8: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(inst, X64::RM8{ from.rm }, X64::R8{ to });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::movzx(inst, X64::RM8{ from.rm }, X64::R16{ to });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::movzx(inst, X64::RM8{ from.rm }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movzx(inst, X64::RM8{ from.rm }, X64::R64{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::sint8: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(inst, X64::RM8{ from.rm }, X64::R8{ to });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::movsx(inst, X64::RM8{ from.rm }, X64::R16{ to });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::movsx(inst, X64::RM8{ from.rm }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movsx(inst, X64::RM8{ from.rm }, X64::R64{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::uint16: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::mov(inst, X64::RM16{ from.rm }, X64::R16{ to });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::movzx(inst, X64::RM16{ from.rm }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movzx(inst, X64::RM16{ from.rm }, X64::R64{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::sint16: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::mov(inst, X64::RM16{ from.rm }, X64::R16{ to });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::movsx(inst, X64::RM16{ from.rm }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movsx(inst, X64::RM16{ from.rm }, X64::R64{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::uint32: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
            case IR::Format::uint16:
            case IR::Format::sint16:
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::mov(inst, X64::RM32{ from.rm }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::mov(inst, X64::RM32{ from.rm }, X64::R32{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::sint32: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
            case IR::Format::uint16:
            case IR::Format::sint16:
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::mov(inst, X64::RM32{ from.rm }, X64::R32{ to });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movsx(inst, X64::RM32{ from.rm }, X64::R64{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::uint64:
      case IR::Format::sint64: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
            case IR::Format::uint16:
            case IR::Format::sint16:
            case IR::Format::uint32:
            case IR::Format::sint32:
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::mov(inst, X64::RM64{ from.rm }, X64::R64{ to });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      default:
        INVALID_CODE_PATH("Cant handle other data formats");
    }

    X64::append_instruction(program, inst);
  }

  static void copy_reg_to_mem(Backend::Program* program,
                              X64::R from, IR::Format f_format,
                              const MemoryView& to, IR::Format t_format) {
    ASSERT(f_format != IR::Format::opaque && t_format != IR::Format::opaque);

    ASSERT(to.size == x64_types_info.get_size(t_format));
    ASSERT(to.size == to.known_alignment);

    switch (f_format) {
      case IR::Format::uint8: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8: {
                X64::Instruction i = {};
                X64::mov(i, X64::R8{ from }, X64::RM8{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint16:
            case IR::Format::sint16: {
                X64::Instruction first = {};
                X64::Instruction second = {};
                X64::movzx(first, X64::R8{ from }, X64::R16{ from });
                X64::mov(second, X64::R16{ from }, X64::RM16{ to.rm });
                X64::append_instruction(program, first);
                X64::append_instruction(program, second);
                break;
              }
            case IR::Format::uint32:
            case IR::Format::sint32: {
                X64::Instruction first = {};
                X64::Instruction second = {};
                X64::movzx(first, X64::R8{ from }, X64::R32{ from });
                X64::mov(second, X64::R32{ from }, X64::RM32{ to.rm });
                X64::append_instruction(program, first);
                X64::append_instruction(program, second);
                break;
              }
            case IR::Format::uint64:
            case IR::Format::sint64: {
                X64::Instruction first = {};
                X64::Instruction second = {};
                X64::movzx(first, X64::R8{ from }, X64::R64{ from });
                X64::mov(second, X64::R64{ from }, X64::RM64{ to.rm });
                X64::append_instruction(program, first);
                X64::append_instruction(program, second);
                break;
              }
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::sint8: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8: {
                X64::Instruction i = {};
                X64::mov(i, X64::R8{ from }, X64::RM8{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint16:
            case IR::Format::sint16: {
                X64::Instruction first = {};
                X64::Instruction second = {};
                X64::movsx(first, X64::R8{ from }, X64::R16{ from });
                X64::mov(second, X64::R16{ from }, X64::RM16{ to.rm });
                X64::append_instruction(program, first);
                X64::append_instruction(program, second);
                break;
              }
            case IR::Format::uint32:
            case IR::Format::sint32: {
                X64::Instruction first = {};
                X64::Instruction second = {};
                X64::movsx(first, X64::R8{ from }, X64::R32{ from });
                X64::mov(second, X64::R32{ from }, X64::RM32{ to.rm });
                X64::append_instruction(program, first);
                X64::append_instruction(program, second);
                break;
              }
            case IR::Format::uint64:
            case IR::Format::sint64: {
                X64::Instruction first = {};
                X64::Instruction second = {};
                X64::movsx(first, X64::R8{ from }, X64::R64{ from });
                X64::mov(second, X64::R64{ from }, X64::RM64{ to.rm });
                X64::append_instruction(program, first);
                X64::append_instruction(program, second);
                break;
              }
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::uint16: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8: {
                X64::Instruction i = {};
                X64::mov(i, X64::R8{ from }, X64::RM8{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint16:
            case IR::Format::sint16: {
                X64::Instruction i = {};
                X64::mov(i, X64::R16{ from }, X64::RM16{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint32:
            case IR::Format::sint32: {
                X64::Instruction first = {};
                X64::Instruction second = {};
                X64::movzx(first, X64::R16{ from }, X64::R32{ from });
                X64::mov(second, X64::R32{ from }, X64::RM32{ to.rm });
                X64::append_instruction(program, first);
                X64::append_instruction(program, second);
                break;
              }
            case IR::Format::uint64:
            case IR::Format::sint64: {
                X64::Instruction first = {};
                X64::Instruction second = {};
                X64::movzx(first, X64::R16{ from }, X64::R64{ from });
                X64::mov(second, X64::R64{ from }, X64::RM64{ to.rm });
                X64::append_instruction(program, first);
                X64::append_instruction(program, second);
                break;
              }
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::sint16: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8: {
                X64::Instruction i = {};
                X64::mov(i, X64::R8{ from }, X64::RM8{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint16:
            case IR::Format::sint16: {
                X64::Instruction i = {};
                X64::mov(i, X64::R16{ from }, X64::RM16{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint32:
            case IR::Format::sint32: {
                X64::Instruction first = {};
                X64::Instruction second = {};
                X64::movsx(first, X64::R16{ from }, X64::R32{ from });
                X64::mov(second, X64::R32{ from }, X64::RM32{ to.rm });
                X64::append_instruction(program, first);
                X64::append_instruction(program, second);
                break;
              }
            case IR::Format::uint64:
            case IR::Format::sint64: {
                X64::Instruction first = {};
                X64::Instruction second = {};
                X64::movsx(first, X64::R16{ from }, X64::R64{ from });
                X64::mov(second, X64::R64{ from }, X64::RM64{ to.rm });
                X64::append_instruction(program, first);
                X64::append_instruction(program, second);
                break;
              }
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::uint32: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8: {
                X64::Instruction i = {};
                X64::mov(i, X64::R8{ from }, X64::RM8{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint16:
            case IR::Format::sint16: {
                X64::Instruction i = {};
                X64::mov(i, X64::R16{ from }, X64::RM16{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint32:
            case IR::Format::sint32: {
                X64::Instruction i = {};
                X64::mov(i, X64::R32{ from }, X64::RM32{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint64:
            case IR::Format::sint64: {
                X64::Instruction first = {};
                X64::Instruction second = {};
                X64::mov(first, X64::R32{ from }, X64::R32{ from });
                X64::mov(second, X64::R64{ from }, X64::RM64{ to.rm });
                X64::append_instruction(program, first);
                X64::append_instruction(program, second);
                break;
              }
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::sint32: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8: {
                X64::Instruction i = {};
                X64::mov(i, X64::R8{ from }, X64::RM8{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint16:
            case IR::Format::sint16: {
                X64::Instruction i = {};
                X64::mov(i, X64::R16{ from }, X64::RM16{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint32:
            case IR::Format::sint32: {
                X64::Instruction i = {};
                X64::mov(i, X64::R32{ from }, X64::RM32{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint64:
            case IR::Format::sint64: {
                X64::Instruction first = {};
                X64::Instruction second = {};
                X64::movsx(first, X64::R32{ from }, X64::R64{ from });
                X64::mov(second, X64::R64{ from }, X64::RM64{ to.rm });
                X64::append_instruction(program, first);
                X64::append_instruction(program, second);
                break;
              }
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::uint64:
      case IR::Format::sint64: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8: {
                X64::Instruction i = {};
                X64::mov(i, X64::R8{ from }, X64::RM8{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint16:
            case IR::Format::sint16: {
                X64::Instruction i = {};
                X64::mov(i, X64::R16{ from }, X64::RM16{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint32:
            case IR::Format::sint32: {
                X64::Instruction i = {};
                X64::mov(i, X64::R32{ from }, X64::RM32{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            case IR::Format::uint64:
            case IR::Format::sint64: {
                X64::Instruction i = {};
                X64::mov(i, X64::R64{ from }, X64::RM64{ to.rm });
                X64::append_instruction(program, i);
                break;
              }
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      default:
        INVALID_CODE_PATH("Cant handle other data formats");
    }
  }

  static void load_const_to_reg(Backend::Program* program, X64::R reg, const u8* data, IR::Format format) {
    ASSERT(format != IR::Format::opaque);

    X64::Instruction i = {};

    switch (format) {
      case IR::Format::uint8:
      case IR::Format::sint8: {
          X64::mov(i, X64::R8{ reg }, X64::IMM8{ data[0] });
          break;
        }
      case IR::Format::uint16:
      case IR::Format::sint16: {
          X64::mov(i, X64::R16{reg}, X64::IMM16{ x16_from_bytes(data) });
          break;
        }
      case IR::Format::uint32:
      case IR::Format::sint32: {
          X64::mov(i, X64::R32{reg}, X64::IMM32{ x32_from_bytes(data) });
          break;
        }
      case IR::Format::uint64:
      case IR::Format::sint64: {
          X64::mov(i, X64::R64{reg}, X64::IMM64{ x64_from_bytes(data) });
          break;
        }
      default:
        INVALID_CODE_PATH("Cant handle other data formats");
    }

    X64::append_instruction(program, i);
  }

  static void load_const_to_mem(Backend::Program* program, MemoryView view, const u8* data, IR::Format format) {
    ASSERT(format != IR::Format::opaque);
    ASSERT(x64_types_info.get_size(format) == view.size);

    switch (format) {
      case IR::Format::uint8:
      case IR::Format::sint8: {
          X64::Instruction i = {};
          X64::mov(i, X64::RM8{ view.rm }, X64::IMM8{ data[0] });
          X64::append_instruction(program, i);
          break;
        }
      case IR::Format::uint16:
      case IR::Format::sint16: {
          X64::Instruction i = {};
          X64::mov(i, X64::RM16{ view.rm }, X64::IMM16{ x16_from_bytes(data) });
          X64::append_instruction(program, i);
          break;
        }
      case IR::Format::uint32:
      case IR::Format::sint32: {
          X64::Instruction i = {};
          X64::mov(i, X64::RM32{ view.rm }, X64::IMM32{ x32_from_bytes(data) });
          X64::append_instruction(program, i);
          break;
        }
      case IR::Format::uint64:
      case IR::Format::sint64: {
          u64 val = x64_from_bytes(data);

          if (X64::can_signed_compress(val)) {
            X64::Instruction first = {};

            u32 i = static_cast<u64>(val) & 0xffffffff;

            //Can just load the value as 32 bits and it will be sign extended
            X64::mov(first, view.rm, X64::IMM32Extended{ i });
            X64::append_instruction(program, first);
          }
          else {
            X64::Instruction first = {};
            X64::Instruction second = {};

            u32 low = static_cast<u64>(val) & 0xffffffff;
            u32 high = static_cast<u64>(val) >> 32;

            X64::mov(first, view.rm, X64::IMM32{ low });
            view.rm.disp += 4;
            X64::mov(second, view.rm, X64::IMM32{ high });
            X64::append_instruction(program, first);
            X64::append_instruction(program, second);
          }
          break;
        }
      default:
        INVALID_CODE_PATH("Cant handle other data formats");
    }
  }

  static void load_const_to_mem_opaque(Backend::Program* program, MemoryView view, const u8* data) {
    using PTR = const u8*;

    constexpr static auto COPY8 = [](Backend::Program* program, MemoryView& view, PTR& data) {
      X64::Instruction i = {};
      X64::mov(i, view.rm, X64::IMM8{ data[0] });
      view.rm.disp += 1;
      view.size -= 1;
      data += 1;

      X64::append_instruction(program, i);
    };

    constexpr static auto COPY16 = [](Backend::Program* program, MemoryView& view, PTR& data) {
      X64::Instruction i = {};
      X64::mov(i, view.rm, X64::IMM16{ x16_from_bytes(data) });
      view.rm.disp += 2;
      view.size -= 2;
      data += 2;

      X64::append_instruction(program, i);
    };

    constexpr static auto COPY32 = [](Backend::Program* program, MemoryView& view, PTR& data) {
      X64::Instruction i = {};
      X64::mov(i, view.rm, X64::IMM32{ x32_from_bytes(data) });
      view.rm.disp += 4;
      view.size -= 4;
      data += 4;

      X64::append_instruction(program, i);
    };

    u32 alignment = view.known_alignment;

    while (alignment != 8 && view.size > 0) {
      switch (alignment) {
        case 1: {
            COPY8(program, view, data);
            alignment = 2;
            break;
          }
        case 2: {
            if (view.size == 1) {
              COPY8(program, view, data);
              alignment = 1;//shouldn't matter but its nice to be correct
            }
            else {
              COPY16(program, view, data);
              alignment = 4;
            }
            break;
          }
        case 4: {
            if (view.size == 1) {
              COPY8(program, view, data);
              alignment = 1;//shouldn't matter but its nice to be correct
            }
            else if (view.size == 2) {
              COPY16(program, view, data);
              alignment = 2;//shouldn't matter but its nice to be correct
            }
            else if (view.size == 3) {
              COPY16(program, view, data);
              COPY8(program, view, data);
              alignment = 1;//shouldn't matter but its nice to be correct
            }
            else {
              COPY32(program, view, data);
              alignment = 4;
            }

            break;
          }
        default: INVALID_CODE_PATH("Invalid alignment");
      }
    }

    u32 num_8s = view.size / 8;
    u32 remainder = view.size % 8;

    for (size_t itr = 0; itr < num_8s; itr++) {
      i64 val;
      memcpy_s(&val, sizeof(val), data, 8);
      data += 8;

      //No 64 bit constant loads in x86 for some reason

      if (X64::can_signed_compress(val)) {
        X64::Instruction first = {};

        u32 i = static_cast<u64>(val) & 0xffffffff;

        //Can just load the value as 32 bits and it will be sign extended
        X64::mov(first, view.rm, X64::IMM32Extended{ i });
        view.rm.disp += 8;

        X64::append_instruction(program, first);
      }
      else {
        X64::Instruction first = {};
        X64::Instruction second = {};
        u32 high = static_cast<u64>(val) & 0xffffffff;
        u32 low = static_cast<u64>(val) >> 32;

        X64::mov(first, view.rm, X64::IMM32{ low });
        view.rm.disp += 4;

        X64::mov(second, view.rm, X64::IMM32{ high });
        view.rm.disp += 4;

        X64::append_instruction(program, first);
        X64::append_instruction(program, second);
      }
    }

    switch (remainder) {
      case 0: break;//Already loaded
      case 1:
        COPY8(program, view, data);
        break;
      case 2:
        COPY16(program, view, data);
        break;
      case 3:
        COPY16(program, view, data);
        COPY8(program, view, data);
        break;
      case 4:
        COPY32(program, view, data);
        break;
      case 5:
        COPY32(program, view, data);
        COPY8(program, view, data);
        break;
      case 6:
        COPY32(program, view, data);
        COPY16(program, view, data);
        break;
      case 7:
        COPY32(program, view, data);
        COPY16(program, view, data);
        COPY8(program, view, data);
        break;
      default:
        INVALID_CODE_PATH("Somehow n % 8 was bigger than 8 ... We broke maths");
    }
  }



  static void copy_mem_to_mem_small(Backend::Program* program,
                                    const MemoryView& from, IR::Format f_format,
                                    const MemoryView& to, IR::Format t_format,
                                    X64::R temp) {
    ASSERT(f_format != IR::Format::opaque && t_format != IR::Format::opaque);

    ASSERT(from.size == x64_types_info.get_size(f_format));
    ASSERT(from.size == from.known_alignment);

    ASSERT(to.size == x64_types_info.get_size(t_format));
    ASSERT(to.size == to.known_alignment);

    X64::Instruction first = {};
    X64::Instruction second = {};

    switch (f_format) {
      case IR::Format::uint8: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(first, X64::RM8{ from.rm }, X64::R8{ temp });
              X64::mov(second, X64::R8{ temp }, X64::RM8{ to.rm });
              break;

            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::movzx(first, X64::RM8{ from.rm }, X64::R16{ temp });
              X64::mov(second, X64::R16{ temp }, X64::RM16{ to.rm });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::movzx(first, X64::RM8{ from.rm }, X64::R32{ temp });
              X64::mov(second, X64::R32{ temp }, X64::RM32{ to.rm });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movzx(first, X64::RM8{ from.rm }, X64::R64{ temp });
              X64::mov(second, X64::R64{ temp }, X64::RM64{ to.rm });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::sint8: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(first, X64::RM8{ from.rm }, X64::R8{ temp });
              X64::mov(second, X64::R8{ temp }, X64::RM8{ to.rm });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::movsx(first, X64::RM8{ from.rm }, X64::R16{ temp });
              X64::mov(second, X64::R16{ temp }, X64::RM16{ to.rm });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::movsx(first, X64::RM8{ from.rm }, X64::R32{ temp });
              X64::mov(second, X64::R32{ temp }, X64::RM32{ to.rm });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movsx(first, X64::RM8{ from.rm }, X64::R64{ temp });
              X64::mov(second, X64::R64{ temp }, X64::RM64{ to.rm });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::uint16: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(first, X64::RM16{ from.rm }, X64::R16{ temp });
              X64::mov(second, X64::R8{ temp }, X64::RM8{ to.rm });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::mov(first, X64::RM16{ from.rm }, X64::R16{ temp });
              X64::mov(second, X64::R16{ temp }, X64::RM16{ to.rm });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::movzx(first, X64::RM16{ from.rm }, X64::R32{ temp });
              X64::mov(second, X64::R32{ temp }, X64::RM32{ to.rm });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movzx(first, X64::RM16{ from.rm }, X64::R64{ temp });
              X64::mov(second, X64::R64{ temp }, X64::RM64{ to.rm });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::sint16: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(first, X64::RM16{ from.rm }, X64::R16{ temp });
              X64::mov(second, X64::R8{ temp }, X64::RM8{ to.rm });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::mov(first, X64::RM16{ from.rm }, X64::R16{ temp });
              X64::mov(second, X64::R16{ temp }, X64::RM16{ to.rm });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::movsx(first, X64::RM16{ from.rm }, X64::R32{ temp });
              X64::mov(second, X64::R32{ temp }, X64::RM32{ to.rm });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movsx(first, X64::RM16{ from.rm }, X64::R64{ temp });
              X64::mov(second, X64::R64{ temp }, X64::RM64{ to.rm });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::uint32: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(first, X64::RM32{ from.rm }, X64::R32{ temp });
              X64::mov(second, X64::R8{ temp }, X64::RM8{ to.rm });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::mov(first, X64::RM32{ from.rm }, X64::R32{ temp });
              X64::mov(second, X64::R16{ temp }, X64::RM16{ to.rm });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::mov(first, X64::RM32{ from.rm }, X64::R32{ temp });
              X64::mov(second, X64::R32{ temp }, X64::RM32{ to.rm });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::mov(first, X64::RM32{ from.rm }, X64::R32{ temp });
              X64::mov(second, X64::R64{ temp }, X64::RM64{ to.rm });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::sint32: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(first, X64::RM32{ from.rm }, X64::R32{ temp });
              X64::mov(second, X64::R8{ temp }, X64::RM8{ to.rm });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::mov(first, X64::RM32{ from.rm }, X64::R32{ temp });
              X64::mov(second, X64::R16{ temp }, X64::RM16{ to.rm });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::mov(first, X64::RM32{ from.rm }, X64::R32{ temp });
              X64::mov(second, X64::R32{ temp }, X64::RM32{ to.rm });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::movsx(first, X64::RM32{ from.rm }, X64::R64{ temp });
              X64::mov(second, X64::R64{ temp }, X64::RM64{ to.rm });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      case IR::Format::uint64:
      case IR::Format::sint64: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              X64::mov(first, X64::RM64{ from.rm }, X64::R64{ temp });
              X64::mov(second, X64::R8{ temp }, X64::RM8{ to.rm });
              break;
            case IR::Format::uint16:
            case IR::Format::sint16:
              X64::mov(first, X64::RM64{ from.rm }, X64::R64{ temp });
              X64::mov(second, X64::R16{ temp }, X64::RM16{ to.rm });
              break;
            case IR::Format::uint32:
            case IR::Format::sint32:
              X64::mov(first, X64::RM64{ from.rm }, X64::R64{ temp });
              X64::mov(second, X64::R32{ temp }, X64::RM32{ to.rm });
              break;
            case IR::Format::uint64:
            case IR::Format::sint64:
              X64::mov(first, X64::RM64{ from.rm }, X64::R64{ temp });
              X64::mov(second, X64::R64{ temp }, X64::RM64{ to.rm });
              break;
            default: INVALID_CODE_PATH("Unsupported copy");
          }
          break;
        }
      default:
        INVALID_CODE_PATH("Cant handle other data formats");
    }

    X64::append_instruction(program, first);
    X64::append_instruction(program, second);
  }

  static void copy_mem_to_mem_opaque(Backend::Program* program, MemoryView from, MemoryView to, X64::R temp_reg) {
    constexpr static auto COPY8 = [](Backend::Program* program,
                                     MemoryView& from, X64::R temp, MemoryView& to) {
      X64::Instruction first = {};
      X64::Instruction second = {};
      X64::mov(first, X64::RM8{ from.rm }, X64::R8{ temp });
      from.rm.disp += 1;
      from.size -= 1;

      X64::mov(second, X64::R8{ temp }, X64::RM8{ to.rm });
      to.rm.disp += 1;
      to.size -= 1;

      X64::append_instruction(program, first);
      X64::append_instruction(program, second);
    };

    constexpr static auto COPY16 = [](Backend::Program* program,
                                      MemoryView& from, X64::R temp, MemoryView& to) {
      X64::Instruction first = {};
      X64::Instruction second = {};

      X64::mov(first, X64::RM16{ from.rm }, X64::R16{ temp });
      from.rm.disp += 2;
      from.size -= 2;

      X64::mov(second, X64::R16{ temp }, X64::RM16{ to.rm });
      to.rm.disp += 2;
      to.size -= 2;

      X64::append_instruction(program, first);
      X64::append_instruction(program, second);
    };

    constexpr static auto COPY32 = [](Backend::Program* program,
                                      MemoryView& from, X64::R temp, MemoryView& to) {
      X64::Instruction first = {};
      X64::Instruction second = {};

      X64::mov(first, X64::RM32{ from.rm }, X64::R32{ temp });
      from.rm.disp += 4;
      from.size -= 4;

      X64::mov(second, X64::R32{ temp }, X64::RM32{ to.rm });
      to.rm.disp += 4;
      to.size -= 4;

      X64::append_instruction(program, first);
      X64::append_instruction(program, second);
    };

    ASSERT(from.size == to.size);
    ASSERT(from.known_alignment == to.known_alignment);

    u32 alignment = from.known_alignment;
    while (alignment != 8 && from.size > 0) {
      switch (alignment) {
        case 1: {
            COPY8(program, from, temp_reg, to);
            alignment = 2;
            break;
          }
        case 2: {
            if (from.size == 1) {
              COPY8(program, from, temp_reg, to);
              alignment = 1;//shouldn't matter but its nice to be correct
            }
            else {
              COPY16(program, from, temp_reg, to);
              alignment = 4;
            }
            break;
          }
        case 4: {
            if (from.size == 1) {
              COPY8(program, from, temp_reg, to);
              alignment = 1;//shouldn't matter but its nice to be correct
            }
            else if (from.size == 2) {
              COPY16(program, from, temp_reg, to);
              alignment = 2;//shouldn't matter but its nice to be correct
            }
            else if (from.size == 3) {
              COPY16(program, from, temp_reg, to);
              COPY8(program, from, temp_reg, to);
              alignment = 1;//shouldn't matter but its nice to be correct
            }
            else {
              COPY32(program, from, temp_reg, to);
              alignment = 4;
            }

            break;
          }
        default: INVALID_CODE_PATH("Invalid alignment");
      }
    }

    const size_t num8s = from.size / 8;
    size_t remaining = from.size % 8;

    for (size_t itr = 0; itr < num8s; ++itr) {
      X64::Instruction first = {};
      X64::Instruction second = {};

      X64::mov(first, X64::RM64{ from.rm }, X64::R64{ temp_reg });
      from.rm.disp += 8;
      from.size -= 8;

      X64::mov(second, X64::R64{ temp_reg }, X64::RM64{ to.rm });
      to.rm.disp += 8;
      to.size -= 8;

      X64::append_instruction(program, first);
      X64::append_instruction(program, second);
    }

    switch (remaining) {
      case 0: break;
      case 1: {
          COPY8(program, from, temp_reg, to);
          break;
        }
      case 2: {
          COPY16(program, from, temp_reg, to);
          break;
        }
      case 3: {
          COPY16(program, from, temp_reg, to);
          COPY8(program, from, temp_reg, to);
          break;
        }
      case 4: {
          COPY32(program, from, temp_reg, to);
          break;
        }
      case 5: {
          COPY32(program, from, temp_reg, to);
          COPY8(program, from, temp_reg, to);
          break;
        }
      case 6: {
          COPY32(program, from, temp_reg, to);
          COPY16(program, from, temp_reg, to);
          break;
        }
      case 7: {
          COPY32(program, from, temp_reg, to);
          COPY16(program, from, temp_reg, to);
          COPY8(program, from, temp_reg, to);
          break;
        }
      default:  INVALID_CODE_PATH("Should be impossible to have 8 or more left");
    }
  }

#define EMIT_SYMMETRICAL_HELPER(name)\
  IR::Format emit_ ## name(Backend::Program* program,\
                X64::R left, IR::Format l_format,\
                X64::R right, IR::Format r_format) {\
    ASSERT(l_format == r_format);\
    X64::Instruction i = {};\
    switch (l_format) {\
      case  IR::Format::uint8:\
      case  IR::Format::sint8: {\
          X64:: name (i, X64::R8{right}, X64::R8{left});\
          break;\
        }\
      case  IR::Format::uint16:\
      case  IR::Format::sint16: {\
          X64:: name (i, X64::R16{right}, X64::R16{left});\
          break;\
        }\
      case  IR::Format::uint32:\
      case  IR::Format::sint32: {\
          X64:: name (i, X64::R32{right}, X64::R32{left});\
          break;\
        }\
      case  IR::Format::uint64:\
      case  IR::Format::sint64: {\
          X64:: name (i, X64::R64{right}, X64::R64{left});\
          break;\
        }\
      default: INVALID_CODE_PATH("Invalid " #name " format");\
    }\
    X64::append_instruction(program, i);\
    return l_format;\
  }

  EMIT_SYMMETRICAL_HELPER(add);
  EMIT_SYMMETRICAL_HELPER(sub);
  EMIT_SYMMETRICAL_HELPER(and_);
  EMIT_SYMMETRICAL_HELPER(or_);
  EMIT_SYMMETRICAL_HELPER(xor_);

#undef EMIT_SYMMETRICAL_HELPER

  void emit_cmp(Backend::Program* program,
                X64::R left, IR::Format l_format,
                X64::R right, IR::Format r_format) {
    ASSERT(l_format == r_format);
    X64::Instruction i = {};
    switch (l_format) {
      case  IR::Format::uint8:
      case  IR::Format::sint8: {
          X64::cmp(i, X64::R8{right}, X64::R8{left});
          break;
        }
      case  IR::Format::uint16:
      case  IR::Format::sint16: {
          X64::cmp(i, X64::R16{right}, X64::R16{left});
          break;
        }
      case  IR::Format::uint32:
      case  IR::Format::sint32: {
          X64::cmp(i, X64::R32{right}, X64::R32{left});
          break;
        }
      case  IR::Format::uint64:
      case  IR::Format::sint64: {
          X64::cmp(i, X64::R64{right}, X64::R64{left});
          break;
        }
      default: INVALID_CODE_PATH("Invalid comparable format");
    }

    X64::append_instruction(program, i);
  }

  IR::Format emit_great(Backend::Program* program,
                        X64::R left, IR::Format l_format,
                        X64::R right, IR::Format r_format) {
    emit_cmp(program, left, l_format, right, r_format);
    X64::Instruction i = {};
    X64::setg(i, X64::R8{left});
    X64::append_instruction(program, i);
    return IR::Format::uint8;
  }

  IR::Format emit_less(Backend::Program* program,
                       X64::R left, IR::Format l_format,
                       X64::R right, IR::Format r_format) {
    emit_cmp(program, left, l_format, right, r_format);
    X64::Instruction i = {};
    X64::setl(i, X64::R8{left});
    X64::append_instruction(program, i);
    return IR::Format::uint8;
  }

  IR::Format emit_eq(Backend::Program* program,
                     X64::R left, IR::Format l_format,
                     X64::R right, IR::Format r_format) {
    emit_cmp(program, left, l_format, right, r_format);
    X64::Instruction i = {};
    X64::sete(i, X64::R8{left});
    X64::append_instruction(program, i);
    return IR::Format::uint8;
  }

  IR::Format emit_neq(Backend::Program* program,
                      X64::R left, IR::Format l_format,
                      X64::R right, IR::Format r_format) {
    emit_cmp(program, left, l_format, right, r_format);
    X64::Instruction i = {};
    X64::setne(i, X64::R8{left});
    X64::append_instruction(program, i);
    return IR::Format::uint8;
  }
}

struct Edge {
  u32 a = 0;
  u32 b = 0;
};

struct OrderedValue {
  bool visited;
  bool crosses_call;
  bool chosen_id;
  bool needs_register;

  u32 first_use;
  u32 last_use;

  u8 register_id;
};

struct RegisterResolver {
  const SignatureStructure* sig_struct;
  const CallingConvention* this_convention;


  const IR::GlobalReference* all_globals_used;
  const IR::Variable* all_variables;

  const IR::Temporary* temporaries;
  u32 temporaries_offset;
  u32 num_temporaries;

  const u8* bytecode_start;
  const u8* bytecode_end;

  bool has_called = false;
  u32 last_call = 0;

  u32 call_space_needed = 0;
};

static ValueType visit_ordered_value(Array<OrderedValue>& values, const RegisterResolver* resolver,
                                     IR::ValueIndex index, u32 expr_id) {
  if (index.is_temporary()) {
    usize relative = index.index() - (usize)resolver->temporaries_offset;

    const IR::Temporary& t = resolver->temporaries[relative];
    bool first_visit = false;
    {
      OrderedValue& v = values.data[relative];
      first_visit = !v.visited;
      if (first_visit) {
        v.visited = true;

        v.first_use = expr_id;
        v.last_use = expr_id;

        v.needs_register = t.indirection == IR::Indirection::None;
      }
      else {
        v.last_use = expr_id;
      }

      if (resolver->has_called && v.first_use <= resolver->last_call && resolver->last_call < v.last_use) {
        v.crosses_call = true;
      }
    }

    if (t.indirection == IR::Indirection::Dereference || t.indirection == IR::Indirection::Reference) {
      ValueType vt = visit_ordered_value(values, resolver, t.refers_to, expr_id);

      switch (vt) {
        case ValueType::Register: {
            OrderedValue& v = values.data[relative];
            v.needs_register = true;
            break;
          }
        case ValueType::Address: {
            OrderedValue& v = values.data[relative];
            v.needs_register = true;
            break;
          }
        case ValueType::Memory: {
            break;
          }
      }
    }

    switch (t.indirection) {
      case IR::Indirection::None: return ValueType::Register;
      case IR::Indirection::Reference: return ValueType::Address;
      case IR::Indirection::Dereference: return ValueType::Memory;
    }

    return ValueType::Register;
  }
  else {
    return ValueType::Memory;
  }
}

void new_intermediate(Array<OrderedValue>& values, u32 expr_id) {
  values.insert_uninit(1);
  auto* v = values.back();
  v->visited = true;
  v->needs_register = true;
  v->first_use = expr_id;
  v->last_use = expr_id;
}

void new_fixed_intermediate(Array<OrderedValue>& values, u32 expr_id, u8 chosen) {
  values.insert_uninit(1);
  auto* v = values.back();
  v->visited = true;
  v->needs_register = true;
  v->first_use = expr_id;
  v->last_use = expr_id;

  v->chosen_id = true;
  v->register_id = chosen;
}

Array<OrderedValue> resolve_values(CompilerGlobals* comp,
                                   CompilerThread* comp_thread,
                                   RegisterResolver* resolver,
                                   const CallingConvention* convention,
                                   u32 stack_top) {
  const u8* const bc_start = resolver->bytecode_start;
  const u8* const bc_end = resolver->bytecode_end;

  const u8* bc = bc_start;

  Array<OrderedValue> values = {};
  values.insert_uninit(resolver->num_temporaries);

  resolver->call_space_needed = 0;

  //Create the nodes in the value graph with their usages

  //Need to loop through the bytecode to check if we need any intermediate values
  while (bc < bc_end) {
    u8 op_byte = *bc;
    IR::OpCode op = static_cast<IR::OpCode>(op_byte);

    switch (op) {
      case IR::OpCode::Set: {
          IR::Types::Set set;
          bc = IR::Read::Set(bc, bc_end, set);

          [[maybe_unused]] ValueType vt = visit_ordered_value(values, resolver, set.to, (u32)(bc - bc_start));
          break;
        }
      case IR::OpCode::CopyCast: {
          IR::Types::CopyCast copy;
          bc = IR::Read::CopyCast(bc, bc_end, copy);

          ValueType from_vt = visit_ordered_value(values, resolver, copy.from, (u32)(bc - bc_start));
          ValueType to_vt = visit_ordered_value(values, resolver, copy.to, (u32)(bc - bc_start));

          if (from_vt != ValueType::Register && to_vt != ValueType::Register) {
            new_intermediate(values, (u32)(bc - bc_start));
          }

          break;
        }
      case IR::OpCode::Return: {
          IR::Types::Return ret;
          bc = IR::Read::Return(bc, bc_end, ret);

          [[maybe_unused]] ValueType vt = visit_ordered_value(values, resolver, ret.val, (u32)(bc - bc_start));

          new_fixed_intermediate(values, (u32)(bc - bc_start), convention->return_register);
          break;
        }
      case IR::OpCode::StartFunc: {
          const SignatureStructure* sig = resolver->sig_struct;
          IR::Types::StartFunc start_func;
          bc = IR::Read::StartFunc(bc, bc_end, start_func);

          for (usize i = 0; i < sig->parameter_types.size; ++i) {
            ASSERT(i < convention->num_parameter_registers);

            new_fixed_intermediate(values, (u32)(bc - bc_start), convention->parameter_registers[i]);
          }

          break;
        }
      case IR::OpCode::Call: {
          IR::Types::Call call;
          bc = IR::Read::Call(bc, bc_end, call);

          resolver->has_called = true;
          resolver->last_call = (u32)(bc - bc_start);

          const u8* values_i = call.values;
          const u8* values_end = call.values + (IR::SingleVal::serialize_size() * call.n_values);

          const SignatureStructure* sig_struct = comp->get_label_signature(call.label);
          ASSERT(sig_struct != nullptr);

          bool has_return = sig_struct->return_type != comp_thread->builtin_types->t_void;

          u32 call_space = convention->shadow_space_size;

          for (usize i = 0; i < (call.n_values - has_return); ++i) {
            IR::SingleVal arg;
            values_i += IR::deserialize(values_i, values_end - values_i, arg);

            ValueType vt = visit_ordered_value(values, resolver, arg.v, (u32)(bc - bc_start));

            if (i < convention->num_parameter_registers) {
              new_fixed_intermediate(values, (u32)(bc - bc_start), convention->parameter_registers[i]);
            }
            else {
              call_space += 8;
              if (vt == ValueType::Memory) {
                new_intermediate(values, (u32)(bc - bc_start));
              }
            }
          }

          if (resolver->call_space_needed < call_space) {
            resolver->call_space_needed = call_space;
          }

          if (has_return) {
            IR::SingleVal ret;
            values_i += IR::deserialize(values_i, values_end - values_i, ret);
            [[maybe_unused]] ValueType vt = visit_ordered_value(values, resolver, ret.v, (u32)(bc - bc_start));

            new_fixed_intermediate(values, (u32)(bc - bc_start), convention->return_register);
          }


          break;
        }
      case IR::OpCode::IfSplit: {
          IR::Types::IfSplit ifsplit;
          bc = IR::Read::IfSplit(bc, bc_end, ifsplit);

          [[maybe_unused]] ValueType vt = visit_ordered_value(values, resolver, ifsplit.val, (u32)(bc - bc_start));
          break;
        }
      case IR::OpCode::Jump: {
          IR::Types::Jump jump;
          bc = IR::Read::Jump(bc, bc_end, jump);
          break;
        }

      case IR::OpCode::GlobalAddress: {
          IR::Types::GlobalAddress addr;
          bc = IR::Read::GlobalAddress(bc, bc_end, addr);

          const IR::GlobalReference& g = resolver->all_globals_used[addr.im32];

          ASSERT(addr.val.is_temporary());
          {
            usize relative = addr.val.index() - (usize)resolver->temporaries_offset;

            OrderedValue& ov = values.data[relative];
            const IR::Temporary& t = resolver->temporaries[relative];

            ASSERT(t.indirection == IR::Indirection::None);

            u32 expr_id = (u32)(bc - bc_start);

            if (!ov.visited) {
              ov.visited = true;
              ov.first_use = expr_id;
              ov.last_use = expr_id;
              ov.needs_register = true;
            }
            else {
              ov.last_use = expr_id;
            }

            if (resolver->has_called && ov.first_use <= resolver->last_call && resolver->last_call < ov.last_use) {
              ov.crosses_call = true;
            }
          }

          break;
        }

#define VISIT_BIN_OP(name) \
      case IR::OpCode:: name: {\
          IR::Types:: name bin_op;\
          bc = IR::Read:: name (bc, bc_end, bin_op);\
          ValueType left_vt = visit_ordered_value(values, resolver, bin_op.left, (u32)(bc - bc_start));\
          ValueType right_vt = visit_ordered_value(values, resolver, bin_op.right, (u32)(bc - bc_start));\
          ValueType to_vt = visit_ordered_value(values, resolver, bin_op.to, (u32)(bc - bc_start));\
          ASSERT(to_vt != ValueType::Address);\
          /*always a left register needed*/\
          new_intermediate(values, (u32)(bc - bc_start)); \
          if(right_vt != ValueType::Register) {\
            new_intermediate(values, (u32)(bc - bc_start)); \
          }\
          break;\
        }

                                    VISIT_BIN_OP(Add);
                                    VISIT_BIN_OP(Sub);
                                    VISIT_BIN_OP(Mul);
                                    VISIT_BIN_OP(Div);
                                    VISIT_BIN_OP(Mod);
                                    VISIT_BIN_OP(Eq);
                                    VISIT_BIN_OP(Neq);
                                    VISIT_BIN_OP(Less);
                                    VISIT_BIN_OP(Great);
                                    VISIT_BIN_OP(And);
                                    VISIT_BIN_OP(Xor);

#undef VISIT_BIN_OP
      default: {
          const char* opcode_name = IR::opcode_string(op);
          if (opcode_name == nullptr) {
            comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                                      "Invalid instruction encountered during ir compilation\n"
                                      "Id = {} (a name for this opcode could not be found)",
                                      op_byte);
            return {};
          }
          else {
            comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{}, "Unsupported instruction encountered during ir compilation: \"{}\"",
                                      opcode_name);
            return {};
          }
        }
    }
  }

  // Create then colour the graph

  Array<Edge> edges = {};

  for (usize a = 0; a < values.size; ++a) {
    const OrderedValue& va = values.data[a];
    if (!va.needs_register || va.chosen_id) continue;

    for (usize b = 0; b < values.size; ++b) {
      if (a == b) continue;

      const OrderedValue& vb = values.data[b];
      if (!vb.needs_register) continue;

      if (vb.first_use <= va.last_use) {
        edges.insert({ static_cast<u32>(a), static_cast<u32>(b) });
      }
    }
  }

  // Select from the graph

  const Edge* edge_i = edges.begin();
  const Edge* edge_end = edges.end();

  const usize number_of_registers = convention->num_volatile_registers
    + convention->num_non_volatile_registers;

  for (u32 i = 0; i < static_cast<u32>(values.size); ++i) {
    OrderedValue& v = values.data[i];
    if (!v.needs_register || v.chosen_id) continue;

    ASSERT(edge_i == edge_end || edge_i->a == i);

    u64 used_regs = 0;
    while (edge_i < edge_end && edge_i->a == i) {
      OrderedValue& other = values.data[edge_i->b];
      if (other.needs_register && other.chosen_id) {
        used_regs |= 1llu << other.register_id;
      }

      edge_i += 1;
    }

    u32 reg_index = 0;
    if (v.crosses_call) {
      reg_index = convention->num_volatile_registers;
    }

    for (; reg_index < number_of_registers; ++reg_index) {
      u8 r = convention->all_regs_unordered[reg_index];
      if ((used_regs & (1llu << r)) == 0) {
        break;
      }
    }

    //TODO: spill registers
    ASSERT(reg_index < number_of_registers);

    v.chosen_id = true;
    v.register_id = convention->all_regs_unordered[reg_index];
  }

  return values;
}

struct Selector {

  usize temporary_start;

  const IR::Temporary* temporaries;
  const IR::Variable* variables;

  OwnedArr<const OrderedValue> ordered_values = {};
  u32 ordered_intermediates_start = 0;

  const u32* variable_memory_locations;

  const CallingConvention* convention;

  X64::R get_next_intermediate_reg() {
    ASSERT(ordered_intermediates_start < ordered_values.size);
    X64::R r = { ordered_values.data[ordered_intermediates_start].register_id };
    ordered_intermediates_start += 1;
    return r;
  }

  LazyValue get_lazy_val(Backend::Program* program, IR::ValueIndex i, u32 offset) {
    LazyValue val;

    usize index = i.index();
    if (i.is_temporary()) {
      const IR::Temporary& temp = temporaries[index];

      usize relative = index - temporary_start;
      const OrderedValue& ov = ordered_values.data[relative];

      ASSERT(temp.type.size() <= 8);//temp

      switch (temp.indirection) {
        case IR::Indirection::None: {
            ASSERT(ov.chosen_id);
            val.value_type = ValueType::Register;
            val.reg = X64::R{ ov.register_id };

            return val;
          }
        case IR::Indirection::Reference: {
            val.value_type = ValueType::Address;
            ASSERT(temp.type.struct_format() == IR::Format::uint64);

            LazyValue refers_to = get_lazy_val(program, temp.refers_to, temp.refers_to_offset);


            switch (refers_to.value_type) {
              case ValueType::Register: {
                  ASSERT(ov.needs_register);
                  ASSERT(ov.chosen_id);
                  val.reg.r = ov.register_id;
                  val.reg = refers_to.reg;
                  ASSERT(temp.type.struct_format() == IR::Format::uint64);

                  Helpers::copy_reg_to_reg(program, refers_to.reg, IR::Format::uint64, val.reg, IR::Format::uint64);
                  break;
                }
              case ValueType::Memory: {
                  ASSERT(!ov.needs_register);

                  MemoryView& view = val.mem;
                  view = refers_to.mem;

                  break;
                }
              case ValueType::Address: {
                  ASSERT(ov.needs_register);
                  ASSERT(ov.chosen_id);
                  val.reg.r = ov.register_id;

                  Helpers::copy_address_to_reg(program, refers_to.mem, val.reg);

                  break;
                }
            }

            return val;
          }
        case IR::Indirection::Dereference: {
            val.value_type = ValueType::Memory;

            LazyValue refers_to = get_lazy_val(program, temp.refers_to, temp.refers_to_offset);

            u8 reference_register;

            switch (refers_to.value_type) {
              case ValueType::Register: {
                  reference_register = refers_to.reg.r;
                  break;
                }
              case ValueType::Memory: {
                  ASSERT(ov.needs_register);
                  ASSERT(ov.chosen_id);
                  reference_register = ov.register_id;

                  ASSERT(temp.type.struct_format() == IR::Format::uint64);

                  Helpers::copy_mem_to_reg(program,
                                           refers_to.mem, IR::Format::uint64,
                                           X64::R{reference_register}, IR::Format::uint64);

                  break;
                }
              case ValueType::Address: {
                  ASSERT(ov.needs_register);
                  ASSERT(ov.chosen_id);
                  reference_register = ov.register_id;

                  Helpers::copy_address_to_reg(program, refers_to.mem, X64::R{reference_register});

                  break;
                }
            }

            MemoryView& view = val.mem;
            //Trust!
            view.known_alignment = temp.type.structure->alignment;
            view.rm = X64::memory_rm(reference_register, static_cast<i32>(offset));
            view.size = temp.type.size();

            return val;
          }
      }
    }
    else {
      const IR::Variable& var = variables[index];

      u32 memory_offset = variable_memory_locations[index];

      val.value_type = ValueType::Memory;
      MemoryView& view = val.mem;
      view.known_alignment = var.type.structure->alignment;
      view.rm = X64::memory_rm(convention->base_pointer_reg, -static_cast<i32>(memory_offset));
      view.size = var.type.size();

      return val;
    }

    INVALID_CODE_PATH("Didnt handle a lazy val case");
    return val;
  }

  MemoryView get_variable_mem(usize index) {
    const IR::Variable& var = variables[index];
    u32 memory_offset = variable_memory_locations[index];

    ASSERT(memory_offset % var.type.structure->alignment == 0);

    MemoryView view;
    view.known_alignment = var.type.structure->alignment;
    view.rm = X64::memory_rm(convention->base_pointer_reg, -static_cast<i32>(memory_offset));
    view.size = var.type.size();

    return view;
  }

  MemoryView get_variable_mem(IR::ValueIndex i, u32 offset, IR::Format format) {
    ASSERT(i.is_variable());

    usize index = i.index();

    const IR::Variable& var = variables[index];
    u32 memory_offset = variable_memory_locations[index];

    MemoryView view;
    view.known_alignment = (memory_offset + offset) % 8;
    view.rm = X64::memory_rm(convention->base_pointer_reg, -static_cast<i32>(memory_offset));
    view.size = (u32)x64_types_info.get_size(format);

    return view;
  }
};

u32 relative_offset(usize base, usize top) {
  usize diff = top - base;
  ASSERT(diff < UINT32_MAX);
  return static_cast<u32>(diff);
}

namespace X64 {
  bool try_relative_disp(usize from, usize to, i32* jump) {
    if (from < to) {
      usize diff = to - from;
      if (diff > INT32_MAX) {
        *jump = 0;
        return false;
      }
      else {
        *jump = (i32)diff;
        return true;
      }
    }
    else {
      usize diff = from - to;
      //TODO: we can actually get an extra byte out of this
      if (diff > INT32_MAX) {
        *jump = 0;
        return false;
      }
      else {
        *jump = -(i32)diff;
        return true;
      }
    }
  }
}

void x64_emit_dyn_library_function(CompilerThread* comp_thread, const IR::DynLibraryImport* lib_import, const CallingConvention* convention,
                                   Backend::Program* program) {

  Backend::FunctionMetadata func = {};
  func.code_start = program->code_store.current_location();
  func.code_size = 0;

  {
    u32 import_ref = (u32)program->dyn_imports.size;

    Backend::DynImport import_info = {};
    import_info.library = lib_import->path;
    import_info.function = lib_import->name;

    program->dyn_imports.insert(std::move(import_info));

    Backend::Relocation reloc = {};
    reloc.type = Backend::RelocationType::LibraryLabel;
    reloc.library_call = import_ref;
    reloc.location = program->code_store.total_size + X64::JUMP_IP_ABS_OFFSET;

    program->relocations.insert(std::move(reloc));

    X64::Instruction i = {};

    X64::jump_abs(i, X64::IPOffset{0});
    X64::append_instruction(program, i);

  }

  func.code_size = program->code_store.total_size - func.code_start.actual_location;

  if (program->functions.size <= lib_import->label.label) {
    usize to_append = lib_import->label.label + 1 - program->functions.size;
    program->functions.insert_uninit(to_append);
  }

  program->functions.data[lib_import->label.label] = func;

}

void x64_emit_start(CompilerGlobals* comp,
                    IR::GlobalLabel entry,
                    Backend::Program* program) {
  program->entry_point = entry;
  program->start_code.code_start = program->code_store.current_location();

  X64::Instruction init_stack = {};
  X64::R rbp_r = X64::R{ X64::rbp.REG };
  X64::R rsp_r = X64::R{ X64::rsp.REG };
  X64::mov(init_stack, X64::R64{rsp_r}, X64::R64{rbp_r});

  X64::append_instruction(program, init_stack);

  Backend::Relocation relocation = {};
  relocation.type = Backend::RelocationType::Label;
  relocation.label = entry;
  relocation.location = program->code_store.total_size + X64::JUMP_NEAR_OFFSET;
  program->relocations.insert(std::move(relocation));

  X64::Instruction jump_to_entry = {};
  X64::jump_near(jump_to_entry, 0);

  X64::append_instruction(program, jump_to_entry);

  program->start_code.code_size = program->code_store.total_size - program->start_code.code_start.actual_location;

}

void x64_emit_function(CompilerGlobals* comp, CompilerThread* comp_thread, const IR::Builder* ir, const CallingConvention* convention,
                       Backend::Program* program) {

  //TODO: allow variables in registers
  Array<u32> variables_memory_location;
  variables_memory_location.reserve_total(ir->variables.size);

  u32 stack_top = 0;

  FOR(ir->variables, var) {
    stack_top = ceil_to_n(stack_top, var->type.structure->alignment);
    variables_memory_location.insert(stack_top);
    stack_top += var->type.size();
  }

  Array<X64::JumpRelocation> jump_relocations = {};

  const IR::ControlBlock* blocks = ir->control_blocks.data;
  const IR::ControlBlock* blocks_end = blocks + ir->control_blocks.size;


  const bool has_return_value = ir->signature->return_type != comp->builtin_types->t_void;
  IR::LocalLabel ret_label = { ir->current_block.label + 1 };

  Array<u32> local_label_real_offsets = {};
  local_label_real_offsets.reserve_total(ir->control_blocks.size + has_return_value);

  bool calls = false;
  u32 call_space_used = 0;

  Array<Selector> selectors = {};
  FOR(ir->expression_frames, e) {
    const u8* const bc_start = ir->ir_bytecode.begin() + e->bytecode_start;
    const u8* const bc_end = bc_start + e->bytecode_count;

    RegisterResolver resolver = {};
    resolver.sig_struct = ir->signature;
    resolver.this_convention = convention;
    resolver.bytecode_start = bc_start;
    resolver.bytecode_end = bc_end;

    resolver.temporaries = ir->temporaries.begin() + e->temporary_start;
    resolver.temporaries_offset = e->temporary_start;
    resolver.num_temporaries = e->temporary_count;

    Array<OrderedValue> ordered_values = resolve_values(comp, comp_thread, &resolver, convention, stack_top);
    if (comp_thread->is_panic()) {
      return;
    }

    if (resolver.has_called) {
      calls = resolver.has_called;
    }

    if (call_space_used < resolver.call_space_needed) {
      call_space_used = resolver.call_space_needed;
    }

    selectors.insert_uninit(1);
    Selector* selector = selectors.back();
    selector->variables = ir->variables.data;
    selector->temporaries = ir->temporaries.data;
    selector->temporary_start = e->temporary_start;
    selector->ordered_values = bake_const_arr(std::move(ordered_values));
    selector->variable_memory_locations = variables_memory_location.data;
    selector->convention = convention;

    selector->ordered_intermediates_start = e->temporary_count;
  }

  usize code_base = program->code_store.total_size;

  stack_top = ceil_to_8(stack_top);
  if (calls) {
    stack_top += call_space_used;
    stack_top = ceil_to_n<u32>(stack_top, 16);
  }


  Selector* selector_i = selectors.mut_begin();

  FOR(ir->expression_frames, e) {
    Selector& selector = *selector_i;
    selector_i += 1;

    const u8* const bc_start = ir->ir_bytecode.begin() + e->bytecode_start;
    const u8* const bc_end = bc_start + e->bytecode_count;

    if (blocks < blocks_end) {
      if (blocks->start == e->bytecode_start) {
        u32 relative = relative_offset(code_base, program->code_store.total_size);
        local_label_real_offsets.insert(relative);
        blocks += 1;
      }
      else {
        ASSERT(e->bytecode_start < blocks->start);
      }
    }

    const u8* bc = bc_start;

    while (bc < bc_end) {
      u8 op_byte = *bc;
      IR::OpCode op = static_cast<IR::OpCode>(op_byte);

      switch (op) {
        case IR::OpCode::Set: {
            IR::Types::Set set;
            bc = IR::Read::Set(bc, bc_end, set);

            LazyValue to = selector.get_lazy_val(program, set.to, set.t_offset);

            switch (to.value_type) {
              case ValueType::Register: {
                  Helpers::load_const_to_reg(program, to.reg, set.data, set.t_format);
                  break;
                }
              case ValueType::Memory: {
                  Helpers::load_const_to_mem(program, to.mem, set.data, set.t_format);
                  break;
                }
              case ValueType::Address: {
                  comp_thread->report_error(ERROR_CODE::IR_ERROR, Span{}, "Cannot assign to an reference value type");
                  return;
                }
            }

            break;
          }
        case IR::OpCode::CopyCast: {
            IR::Types::CopyCast copy;
            bc = IR::Read::CopyCast(bc, bc_end, copy);

            LazyValue from = selector.get_lazy_val(program, copy.from, copy.f_offset);
            LazyValue to = selector.get_lazy_val(program, copy.to, copy.t_offset);

            switch (to.value_type) {
              case ValueType::Address: {
                  comp_thread->report_error(ERROR_CODE::IR_ERROR, Span{}, "Cannot assign to an reference value type");
                  return;
                }
              case ValueType::Register: {
                  switch (from.value_type) {
                    case ValueType::Register: {
                        Helpers::copy_reg_to_reg(program,
                                                 from.reg, copy.f_format,
                                                 to.reg, copy.t_format);
                        break;
                      }
                    case ValueType::Address: {
                        ASSERT(copy.f_format == IR::Format::uint64);
                        ASSERT(copy.t_format == IR::Format::uint64);

                        Helpers::copy_address_to_reg(program, from.mem, to.reg);
                        break;
                      }
                    case ValueType::Memory: {
                        Helpers::copy_mem_to_reg(program,
                                                 from.mem, copy.f_format,
                                                 to.reg, copy.t_format);
                        break;
                      }
                  }
                  break;
                }
              case ValueType::Memory: {
                  switch (from.value_type) {
                    case ValueType::Register: {
                        Helpers::copy_reg_to_mem(program,
                                                 from.reg, copy.f_format,
                                                 to.mem, copy.t_format);
                        break;
                      }
                    case ValueType::Address: {
                        ASSERT(copy.f_format == IR::Format::uint64);
                        ASSERT(copy.t_format == IR::Format::uint64);
                        X64::R temp = selector.get_next_intermediate_reg();

                        Helpers::copy_address_to_reg(program, from.mem, temp);
                        Helpers::copy_reg_to_mem(program, temp, copy.f_format, to.mem, copy.t_format);
                        break;
                      }
                    case ValueType::Memory: {
                        X64::R temp = selector.get_next_intermediate_reg();

                        Helpers::copy_mem_to_mem_small(program,
                                                       from.mem, copy.f_format,
                                                       to.mem, copy.t_format,
                                                       temp);
                        break;
                      }
                  }
                  break;
                }
            }

            break;
          }
        case IR::OpCode::GlobalAddress: {
            IR::Types::GlobalAddress addr;
            bc = IR::Read::GlobalAddress(bc, bc_end, addr);

            IR::GlobalReference global_r = ir->globals_used.data[addr.im32];

            INVALID_CODE_PATH("Temporarily unavailable");

#if 0
            ASSERT(addr.val.is_temporary());
            {
              usize relative = addr.val.index() - (usize)selector.temporary_start;

              const OrderedValue& ov = selector.ordered_values[relative];
              const IR::Temporary& t = selector.temporaries[relative];

              ASSERT(ov.chosen_id);

              //Temporary always has a register
              X64::R r = { ov.register_id };


              {
                X64::Instruction move = {};
                //Move the memory location to r
                X64::mov(move, X64::R64{r}, X64::IMM64{global_r.data_member});

                Backend::GlobalReloc global_reloc = {};
                global_reloc.location = program->code_store.total_size + X64::MOV_IM64_OFFSET;

                program->global_relocs.insert(std::move(global_reloc));

                X64::append_instruction(program, move);
      }
    }
#endif

            break;
  }
        case IR::OpCode::Return: {
            IR::Types::Return ret;
            bc = IR::Read::Return(bc, bc_end, ret);

            ASSERT(ret.format != IR::Format::opaque);//temp
            LazyValue from = selector.get_lazy_val(program, ret.val, ret.offset);

            X64::R ret_reg = selector.get_next_intermediate_reg();
            ASSERT(ret_reg.r == convention->return_register);

            switch (from.value_type) {
              case ValueType::Register: {
                  Helpers::copy_reg_to_reg(program, from.reg, ret.format, ret_reg, ret.format);
                  break;
                }
              case ValueType::Address: {
                  ASSERT(ret.format == IR::Format::uint64);
                  Helpers::copy_address_to_reg(program, from.mem, ret_reg);
                  break;
                }
              case ValueType::Memory: {
                  Helpers::copy_mem_to_reg(program, from.mem, ret.format, ret_reg, ret.format);
                  break;
                }
            }

            {
              jump_relocations.insert(X64::JumpRelocation{
                relative_offset(code_base, program->code_store.total_size + X64::JUMP_NEAR_OFFSET),
                  ret_label,
              });

              //TODO: remove unnecessary jumps
              X64::Instruction j = {};
              X64::jump_near(j, 0);
              X64::append_instruction(program, j);
            }
            break;
          }
        case IR::OpCode::StartFunc: {
            const SignatureStructure* sig = ir->signature;

            IR::Types::StartFunc start_func;
            bc = IR::Read::StartFunc(bc, bc_end, start_func);

            if (calls) {
              X64::R rbp = X64::R{ convention->base_pointer_reg };
              X64::R rsp = X64::R{ convention->stack_pointer_reg };

              X64::Instruction save = {};
              X64::push(save, rbp);
              X64::Instruction copy = {};
              X64::mov(copy, X64::R64{rbp}, X64::R64{rsp});
              X64::Instruction move = {};
              X64::sub(move, X64::R64{rsp}, X64::IMM32{stack_top});

              X64::append_instruction(program, save);
              X64::append_instruction(program, copy);
              X64::append_instruction(program, move);
            }
            else {
              ASSERT(call_space_used == 0);
            }

            for (usize i = 0; i < sig->parameter_types.size; ++i) {
              MemoryView p = selector.get_variable_mem(i);
              X64::R r = selector.get_next_intermediate_reg();

              IR::Format f = sig->parameter_types.data[i].struct_format();

              Helpers::copy_reg_to_mem(program, r, f, p, f);
            }

            break;
          }
        case IR::OpCode::Call: {
            IR::Types::Call call;
            bc = IR::Read::Call(bc, bc_end, call);

            const u8* values_i = call.values;
            const u8* values_end = call.values + (IR::SingleVal::serialize_size() * call.n_values);

            const SignatureStructure* sig_struct = comp->get_label_signature(call.label);

            const bool has_return = sig_struct->return_type != comp_thread->builtin_types->t_void;

            u32 call_needed = convention->shadow_space_size;

            for (usize i = 0; i < (call.n_values - has_return); ++i) {
              IR::SingleVal arg;
              values_i += IR::deserialize(values_i, values_end - values_i, arg);

              LazyValue arg_v = selector.get_lazy_val(program, arg.v, arg.v_offset);

              IR::Format f = arg.v_format;

              if (i < convention->num_parameter_registers) {
                X64::R arg_reg = selector.get_next_intermediate_reg();

                switch (arg_v.value_type) {
                  case ValueType::Register: {
                      Helpers::copy_reg_to_reg(program, arg_v.reg, f, arg_reg, f);
                      break;
                    }
                  case ValueType::Address: {
                      ASSERT(f == IR::Format::uint64);
                      Helpers::copy_address_to_reg(program, arg_v.mem, arg_reg);
                      break;
                    }
                  case ValueType::Memory: {
                      Helpers::copy_mem_to_reg(program, arg_v.mem, f, arg_reg, f);
                      break;
                    }
                }
              }
              else {
                MemoryView arg_mem = {};

                const Type& t = sig_struct->parameter_types.data[i];
                arg_mem.known_alignment = t.structure->alignment;
                arg_mem.size = t.size();
                arg_mem.rm = X64::memory_rm(convention->base_pointer_reg, -static_cast<i32>(stack_top - (i * 8)));

                switch (arg_v.value_type) {
                  case ValueType::Register: {
                      Helpers::copy_reg_to_mem(program, arg_v.reg, f, arg_mem, f);
                      break;
                    }
                  case ValueType::Address: {
                      INVALID_CODE_PATH("Cannot assign to an address");
                      break;
                    }
                  case ValueType::Memory: {
                      X64::R temp = selector.get_next_intermediate_reg();

                      Helpers::copy_mem_to_mem_small(program, arg_v.mem, f, arg_mem, f, temp);
                      break;
                    }
                }
              }
            }

            {
              X64::Instruction call_i = {};

              Backend::Relocation reloc = {};
              reloc.type = Backend::RelocationType::Label;
              reloc.label = call.label;
              reloc.location = program->code_store.total_size + X64::CALL_NEAR_OFFSET;

              program->relocations.insert(reloc);

              X64::call_near(call_i, 0);
              X64::append_instruction(program, call_i);
            }

            if (has_return) {
              IR::SingleVal ret;
              values_i += IR::deserialize(values_i, values_end - values_i, ret);

              X64::R reg_reg = selector.get_next_intermediate_reg();

              LazyValue ret_v = selector.get_lazy_val(program, ret.v, ret.v_offset);

              switch (ret_v.value_type) {
                case ValueType::Register: {
                    Helpers::copy_reg_to_reg(program, reg_reg, ret.v_format, ret_v.reg, ret.v_format);
                    break;
                  }
                case ValueType::Memory: {
                    Helpers::copy_reg_to_mem(program, reg_reg, ret.v_format, ret_v.mem, ret.v_format);
                    break;
                  }
                case ValueType::Address: {
                    INVALID_CODE_PATH("Cannot assign to an address");
                    break;
                  }
              }
            }
            break;
          }
        case IR::OpCode::IfSplit: {
            IR::Types::IfSplit ifsplit;
            bc = IR::Read::IfSplit(bc, bc_end, ifsplit);

            LazyValue val = selector.get_lazy_val(program, ifsplit.val, ifsplit.offset);


            ASSERT(ifsplit.format == IR::Format::uint8);
            {
              X64::Instruction compare = {};

              switch (val.value_type) {
                case ValueType::Memory: {
                    X64::cmp(compare, X64::RM8{ val.mem.rm }, X64::IMM8{ 0 });
                    break;
                  }
                case ValueType::Register: {
                    X64::cmp(compare, X64::R8{ val.reg }, X64::IMM8{ 0 });
                    break;
                  }
                case ValueType::Address: {
                    comp_thread->report_error(ERROR_CODE::IR_ERROR, Span{}, "Cannot if based on a pointer");
                    return;
                  }
              }

              X64::append_instruction(program, compare);
            }

            {
              jump_relocations.insert(X64::JumpRelocation{
                relative_offset(code_base, program->code_store.total_size + X64::JUMP_NEAR_OFFSET),
                  ifsplit.label_else
              });

              X64::Instruction j_else = {};
              X64::jump_not_equal(j_else, 0);

              X64::append_instruction(program, j_else);
            }

            {
              jump_relocations.insert(X64::JumpRelocation{
                relative_offset(code_base, program->code_store.total_size + X64::JUMP_NEAR_OFFSET),
                  ifsplit.label_if
              });

              //TODO: remove unnecessary jumps
              X64::Instruction j_if = {};
              X64::jump_near(j_if, 0);

              X64::append_instruction(program, j_if);
            }
            break;
          }
        case IR::OpCode::Jump: {
            IR::Types::Jump jump;
            bc = IR::Read::Jump(bc, bc_end, jump);

            jump_relocations.insert(X64::JumpRelocation{
              relative_offset(code_base, program->code_store.total_size + X64::JUMP_NEAR_OFFSET),
                jump.local_label
            });

            //TODO: remove unnecessary jumps
            X64::Instruction j = {};
            X64::jump_near(j, 0);

            X64::append_instruction(program, j);
            break;
          }

#define EMIT_BIN_OP(name, helper)\
        case IR::OpCode:: name: {\
            IR::Types:: name bin_op;\
            bc = IR::Read:: name (bc, bc_end, bin_op);\
            LazyValue left = selector.get_lazy_val(program, bin_op.left, bin_op.l_offset);\
            LazyValue right = selector.get_lazy_val(program, bin_op.right, bin_op.r_offset);\
            LazyValue to = selector.get_lazy_val(program, bin_op.to, bin_op.t_offset);\
            /*always a left register*/\
            X64::R left_reg = selector.get_next_intermediate_reg();\
            switch (left.value_type) {\
              case ValueType::Address: Helpers::copy_address_to_reg(program, left.mem, left_reg); break;\
              case ValueType::Memory: Helpers::copy_mem_to_reg(program, left.mem, bin_op.l_format, left_reg, bin_op.l_format); break;\
              case ValueType::Register: Helpers::copy_reg_to_reg(program, left.reg, bin_op.l_format, left_reg, bin_op.l_format); break;\
            }\
            X64::R right_reg;\
            switch (right.value_type) {\
              case ValueType::Address: {\
                  right_reg = selector.get_next_intermediate_reg();\
                  Helpers::copy_address_to_reg(program, right.mem, right_reg); break;\
                }\
              case ValueType::Memory: {\
                  right_reg = selector.get_next_intermediate_reg();\
                  Helpers::copy_mem_to_reg(program, right.mem, bin_op.r_format, right_reg, bin_op.r_format); break;\
                }\
              case ValueType::Register: break;\
            }\
            IR::Format new_format = helper (program, left_reg, bin_op.l_format,right_reg, bin_op.r_format);\
            switch (to.value_type) {\
              case ValueType::Register: Helpers::copy_reg_to_reg(program, left_reg, new_format, to.reg, bin_op.t_format); break;\
              case ValueType::Memory: Helpers::copy_reg_to_mem(program, left_reg, new_format, to.mem, bin_op.t_format); break;\
              case ValueType::Address: INVALID_CODE_PATH("Cannot assign to an address"); break;\
            }\
            break;\
          }\

                             EMIT_BIN_OP(Add, Helpers::emit_add);
                             EMIT_BIN_OP(Sub, Helpers::emit_sub);
                             EMIT_BIN_OP(And, Helpers::emit_and_);
                             EMIT_BIN_OP(Or, Helpers::emit_or_);
                             EMIT_BIN_OP(Xor, Helpers::emit_xor_);
                             EMIT_BIN_OP(Great, Helpers::emit_great);
                             EMIT_BIN_OP(Less, Helpers::emit_less);
                             EMIT_BIN_OP(Eq, Helpers::emit_eq);
                             EMIT_BIN_OP(Neq, Helpers::emit_neq);

#undef EMIT_BIN_OP
        default: {
            const char* opcode_name = IR::opcode_string(op);
            if (opcode_name == nullptr) {
              comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                                        "Invalid instruction encountered during ir compilation\n"
                                        "Id = {} (a name for this opcode could not be found)",
                                        op_byte);
              return;
            }
            else {
              comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{}, "Unsupported instruction encountered during ir compilation: {}",
                                        opcode_name);
              return;
            }
          }
}
}
  }

  //Did we find all of them? - could this be an actual error rather than an assert?
  ASSERT(blocks == blocks_end);

  if (has_return_value) {
    u32 relative = relative_offset(code_base, program->code_store.total_size);
    local_label_real_offsets.insert(relative);

    if (calls) {
      X64::R rbp = X64::R{ convention->base_pointer_reg };
      X64::R rsp = X64::R{ convention->stack_pointer_reg };

      X64::Instruction save = {};
      X64::mov(save, X64::R64{rsp}, X64::R64{rbp});
      X64::Instruction load = {};
      X64::pop(save, rbp);

      X64::append_instruction(program, save);
      X64::append_instruction(program, load);
    }

    X64::Instruction r = {};
    X64::ret(r);
    X64::append_instruction(program, r);
  }

  //We're done adding new instructions
  Backend::FunctionMetadata func = {};

  func.code_start = program->code_store.start();
  func.code_size = program->code_store.total_size - func.code_start.actual_location;

  if (program->functions.size <= ir->global_label.label) {
    usize to_append = ir->global_label.label + 1 - program->functions.size;
    program->functions.insert_uninit(to_append);
  }

  program->functions.data[ir->global_label.label] = func;


  //Do the local relocations (global relocations come later)
  {
    auto code_itr = func.code_start;

    FOR(jump_relocations, it) {
      usize immediate_location = func.code_start.actual_location + (usize)it->offset_to_immediate;

      ASSERT(code_itr.actual_location < immediate_location);
      ASSERT(immediate_location + 4 < program->code_store.total_size);

      code_itr.jump_to(immediate_location);

      u32 label_offset = local_label_real_offsets.data[it->jump_to.label];
      u32 jump_from = it->offset_to_immediate + 4;


      u32 offset;
      if (label_offset < jump_from) {
        u32 diff = jump_from - label_offset;
        ASSERT(diff < INT32_MAX);
        offset = (u32)-(i32)diff;
      }
      else {
        offset = label_offset - jump_from;
        ASSERT(offset < INT32_MAX);
      }

      u8 bytes[4];
      x32_to_bytes(offset, bytes);
      code_itr.overwrite_arr(bytes, 4);
    }

    ASSERT(code_itr.actual_location <= program->code_store.total_size);
  }
}

#if 0

struct RegisterNames {
  OwnedPtr<char> r;
  OwnedPtr<char> rm;
};

static const char* b8_no_rex_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "AL";
    case 1: return "CL";
    case 2: return "DL";
    case 3: return "BL";
    case 4: return "AH";
    case 5: return "CH";
    case 6: return "DH";
    case 7: return "BH";
  }

  return "INVALID REGISTER";
}

static const char* b8_rex_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "AL";
    case 1: return "CL";
    case 2: return "DL";
    case 3: return "BL";
    case 4: return "SPL";
    case 5: return "BPL";
    case 6: return "SIL";
    case 7: return "DIL";
    case 8: return "R8L";
    case 9: return "R9L";
    case 10: return "R10L";
    case 11: return "R11L";
    case 12: return "R12L";
    case 13: return "R13L";
    case 14: return "R14L";
    case 15: return "R15L";
  }

  return "INVALID REGISTER";
}

static const char* b16_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "AX";
    case 1: return "CX";
    case 2: return "DX";
    case 3: return "BX";
    case 4: return "SP";
    case 5: return "BP";
    case 6: return "SI";
    case 7: return "DI";
    case 8: return "R8W";
    case 9: return "R9W";
    case 10: return "R10W";
    case 11: return "R11W";
    case 12: return "R12W";
    case 13: return "R13W";
    case 14: return "R14W";
    case 15: return "R15W";
  }

  return "INVALID REGISTER";
}

static const char* b32_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "EAX";
    case 1: return "ECX";
    case 2: return "EDX";
    case 3: return "EBX";
    case 4: return "ESP";
    case 5: return "EBP";
    case 6: return "ESI";
    case 7: return "EDI";
    case 8: return "R8D";
    case 9: return "R9D";
    case 10: return "R10D";
    case 11: return "R11D";
    case 12: return "R12D";
    case 13: return "R13D";
    case 14: return "R14D";
    case 15: return "R15D";
  }

  return "INVALID REGISTER";
}

struct x86PrintOptions {
  FUNCTION_PTR<const char*, uint8_t> r_name = nullptr;
  FUNCTION_PTR<const char*, uint8_t> rm_name = nullptr;
  const char* mem_size = nullptr;
};

static OwnedPtr<char> rm_reg_string(x86PrintOptions* const p_opts,
                                    uint8_t rex, uint8_t modrm, const uint8_t** rest) {
  uint8_t address_mode = (modrm & 0b11'000000) >> 6;
  uint8_t rm = modrm & X64::MODRM_RM_MASK;

  if ((modrm & 0b11'000000) == 0b11'000000) {
    rm |= ((rex & X64::REX_B) << X64::REX_B_SHIFT);

    return format("{}", p_opts->rm_name(rm));
  }

  //from now on use x86_64_reg_name_from_num for mem
  //Memory is always 64 bit addressed

  switch (rm) {
    case RSP.REG: {
        //SIB byte time
        const uint8_t sib = *(*rest)++;

        const uint8_t scale = 1 << ((sib & 0b11'000'000) >> 6);
        const uint8_t index = ((rex & X64::REX_X) << 2) | ((sib & X64::SIB_INDEX_MASK) >> 3);
        const uint8_t base = ((rex & X64::REX_B) << 3) | ((sib & X64::SIB_BASE_MASK));

        const bool INDEX_RSP = index == RSP.REG;
        const bool BASE_RBP = (base & 0b111) == 0b101;

        switch (address_mode) {
          case 0b00: {
              if (INDEX_RSP && BASE_RBP) {
                int32_t disp = x32_from_bytes(*rest);
                *rest += 4;

                return format("{} [{}]", p_opts->mem_size, disp);
              }
              else if (INDEX_RSP) {
                return format("{} [{}]", p_opts->mem_size, x86_64_reg_name_from_num(base));
              }
              else if (BASE_RBP) {
                int32_t disp = x32_from_bytes(*rest);
                *rest += 4;

                char sign = disp >= 0 ? '+' : '-';
                if (scale == 1) {
                  return format("{} [{} {} {}]", p_opts->mem_size, x86_64_reg_name_from_num(index), sign, absolute(disp));
                }
                else {
                  return format("{} [({} * {}) {} {}]", p_opts->mem_size, x86_64_reg_name_from_num(index), scale, sign, absolute(disp));
                }
              }
              else if (scale == 1) {
                return format("{} [{} + {}]",
                              p_opts->mem_size,
                              x86_64_reg_name_from_num(base),
                              x86_64_reg_name_from_num(index));
              }
              else {
                return format("{} [{} + ({} * {})]",
                              p_opts->mem_size,
                              x86_64_reg_name_from_num(base),
                              x86_64_reg_name_from_num(index),
                              scale);
              }
            }
          case 0b01: {
              const int8_t disp = *(*rest)++;

              char sign = disp >= 0 ? '+' : '-';

              if (INDEX_RSP) {
                return format("{} [{} {} {}]",
                              p_opts->mem_size,
                              x86_64_reg_name_from_num(base),
                              sign, absolute(disp));
              }
              else {
                if (scale == 1) {
                  return format("{} [{} {} {} + {}]",
                                p_opts->mem_size,
                                x86_64_reg_name_from_num(base),
                                sign, absolute(disp),
                                x86_64_reg_name_from_num(index));
                }
                else {
                  return format("{} [{} {} {} + ({} * {})]",
                                p_opts->mem_size,
                                x86_64_reg_name_from_num(base),
                                sign, absolute(disp),
                                x86_64_reg_name_from_num(index), scale);
                }
              }
            }
          case 0b10: {
              int32_t disp = x32_from_bytes(*rest);
              *rest += 4;

              char sign = disp >= 0 ? '+' : '-';

              if (INDEX_RSP) {
                return format("{} [{} {} {}]",
                              p_opts->mem_size,
                              x86_64_reg_name_from_num(base),
                              sign, absolute(disp));
              }
              else {
                if (scale == 1) {
                  return format("{} [{} + {} {} {}]",
                                p_opts->mem_size,
                                x86_64_reg_name_from_num(base),
                                x86_64_reg_name_from_num(index),
                                sign, absolute(disp));
                }
                else {
                  return format("{} [{} + ({} * {}) {} {}]",
                                p_opts->mem_size,
                                x86_64_reg_name_from_num(base),
                                x86_64_reg_name_from_num(index), scale,
                                sign, absolute(disp));
                }
              }
            }
        }

        INVALID_CODE_PATH("Internal error. Unrecognised assembly code register format");
      }
    case RBP.REG: {
        if (address_mode == 0b00) {
          int32_t disp = x32_from_bytes(*rest);
          *rest += 4;

          char sign = disp >= 0 ? '+' : '-';

          return format("{} [RIP {} {}]", p_opts->mem_size, sign, absolute(disp));
        }

        goto NORMAL_MODRM;
      }
    default: {
      NORMAL_MODRM:
        rm |= ((rex & X64::REX_B) << X64::REX_B_SHIFT);

        switch (address_mode) {
          case 0b00: {
              return format("{} [{}]", p_opts->mem_size, x86_64_reg_name_from_num(rm));
            }
          case 0b01: {
              int8_t disp = *(*rest)++;

              char sign = disp >= 0 ? '+' : '-';

              return format("{} [{} {} {}]", p_opts->mem_size, x86_64_reg_name_from_num(rm), sign, absolute(disp));
            }
          case 0b10: {
              int32_t disp = x32_from_bytes(*rest);
              *rest += 4;

              char sign = disp >= 0 ? '+' : '-';

              return format("{} [{} {} {}]", p_opts->mem_size, x86_64_reg_name_from_num(rm), sign, absolute(disp));
            }
        }

        INVALID_CODE_PATH("Internal error. Unrecognised assembly code register format");
      }
  }

  INVALID_CODE_PATH("Internal error. Unrecognised assembly code register format");
}

static OwnedPtr<char> r_reg_string(x86PrintOptions* p_opts,
                                   uint8_t rex, uint8_t modrm) {
  uint8_t r = ((rex & X64::REX_R) << X64::REX_R_SHIFT)
    | ((modrm & X64::MODRM_REG_MASK) >> X64::MODRM_REG_SHIFT);

  return  format("{}", p_opts->r_name(r));
}


static RegisterNames register_names(x86PrintOptions* p_opts,
                                    uint8_t rex, uint8_t modrm, const uint8_t** rest) {
  return { r_reg_string(p_opts, rex, modrm), rm_reg_string(p_opts, rex, modrm, rest) };
}

static void load_default_sizes(x86PrintOptions* ops, bool rex_w, bool short_address, bool short_operand) {
  if (short_address) {
    ops->rm_name = b32_reg_name;
  }
  else {
    ops->rm_name = x86_64_reg_name_from_num;
  }

  if (rex_w) {
    ops->r_name = x86_64_reg_name_from_num;
    ops->mem_size = "QWORD PTR";
  }
  else {
    if (short_operand) {
      ops->r_name = b16_reg_name;
      ops->mem_size = "WORD PTR";
    }
    else {
      ops->r_name = b32_reg_name;
      ops->mem_size = "DWORD PTR";
    }
  }
}

void print_x86_64(const uint8_t* machine_code, size_t size) {
  const uint8_t* bytes = machine_code;
  const uint8_t* const end = machine_code + size;

  x86PrintOptions p_opts = {};

  while (bytes < end) {
    printf("0x%-4llx: ", bytes - machine_code);

    bool short_operand = bytes[0] == 0x66;
    if (short_operand) {
      bytes++;
    }

    bool short_address = bytes[0] == 0x67;
    if (short_address) {
      bytes++;
    }

    //check again as it might have been second
    if (!short_operand) {
      short_operand = bytes[0] == 0x66;
      if (short_operand) {
        bytes++;
      }
    }

    const uint8_t maybe_rex = *bytes++;
    if ((maybe_rex & 0b1111'1000) == X64::REX_W) {
      //REX_W instruction
      const uint8_t op = *bytes++;
      switch (op) {
        case X64::ADD_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("add %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::OR_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("or  %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::AND_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("and %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::SUB_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("sub %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::XOR_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("xor %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::CMP_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("cmp %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::PUSH_R:
        case (X64::PUSH_R + 1):
        case (X64::PUSH_R + 2):
        case (X64::PUSH_R + 3):
        case (X64::PUSH_R + 4):
        case (X64::PUSH_R + 5):
        case (X64::PUSH_R + 6):
        case (X64::PUSH_R + 7): {
            const uint8_t reg = (op - X64::PUSH_R) | ((maybe_rex & 0b00000100) << 1);
            const char* r_string = x86_64_reg_name_from_num(reg);

            printf("push %s\n", r_string);
            break;
          }
        case X64::POP_R:
        case (X64::POP_R + 1):
        case (X64::POP_R + 2):
        case (X64::POP_R + 3):
        case (X64::POP_R + 4):
        case (X64::POP_R + 5):
        case (X64::POP_R + 6):
        case (X64::POP_R + 7): {
            const uint8_t reg = (op - X64::POP_R) | ((maybe_rex & 0b00000100) << 1);
            const char* r_string = x86_64_reg_name_from_num(reg);

            printf("pop %s\n", r_string);
            break;
          }
        case 0x81: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            OwnedPtr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            int32_t imm32 = x32_from_bytes(bytes);
            bytes += 4;

            uint8_t r_val = (modrm & 0b0011'1000) >> 3;

            if (r_val == 5) {
              printf("sub %s, 0x%x\n", rm_string.ptr, imm32);
            }
            else if (r_val == 7) {
              printf("cmp %s, 0x%x\n", rm_string.ptr, imm32);
            }
            else {
              printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx.2 0x%.2hhx ...\n",
                     maybe_rex, op, modrm);

              return;
            }
            break;
          }
        case X64::MOV_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("mov %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::MOV_IMM32_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            OwnedPtr<char> rm = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            uint32_t val = x32_from_bytes(bytes);
            bytes += 4;

            printf("mov %s, %u\n", rm.ptr, val);
            break;
          }
        case X64::MOV_RM_TO_R: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("mov %s, %s\n", names.r.ptr, names.rm.ptr);
            break;
          }
        case X64::LEA_RM_TO_R: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("lea %s, %s\n", names.r.ptr, names.rm.ptr);
            break;
          }
        case X64::CQO: {
            printf("cqo\n");
            break;
          }
        case 0x0F: {
            uint8_t op2 = *bytes++;
            switch (op2) {
              case X64::IMUL_RM_TO_R: {
                  uint8_t modrm = *bytes++;

                  load_default_sizes(&p_opts, true, short_address, short_operand);

                  RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

                  printf("imul %s, %s\n", names.r.ptr, names.rm.ptr);
                  break;
                }
              case X64::MOV_ZX_RM8_TO_R: {
                  uint8_t modrm = *bytes++;

                  load_default_sizes(&p_opts, true, short_address, short_operand);
                  //overide
                  p_opts.rm_name = b8_rex_reg_name;

                  RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

                  printf("movzx %s, %s\n", names.r.ptr, names.rm.ptr);
                  break;
                }
              case X64::MOV_SX_RM8_TO_R: {
                  uint8_t modrm = *bytes++;

                  load_default_sizes(&p_opts, true, short_address, short_operand);
                  //overide
                  p_opts.rm_name = b8_rex_reg_name;

                  RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

                  printf("movsx %s, %s\n", names.r.ptr, names.rm.ptr);
                  break;
                }
              default: {
                  printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                         maybe_rex, op, op2);

                  return;
                }
            }
            break;
          }
        case X64::MOV_64_TO_R:
        case (X64::MOV_64_TO_R + 1):
        case (X64::MOV_64_TO_R + 2):
        case (X64::MOV_64_TO_R + 3):
        case (X64::MOV_64_TO_R + 4):
        case (X64::MOV_64_TO_R + 5):
        case (X64::MOV_64_TO_R + 6):
        case (X64::MOV_64_TO_R + 7): {
            const uint8_t reg = (op - X64::MOV_64_TO_R) | ((maybe_rex & 0b0000'0001) << 3);

            load_default_sizes(&p_opts, true, short_address, short_operand);

            const char* r_string = p_opts.r_name(reg);

            uint64_t imm64 = x64_from_bytes(bytes);
            bytes += 8;

            printf("mov %s, 0x%llx\n", r_string, imm64);
            break;
          }
        case 0xF7: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            const uint8_t r = (modrm & 0b0011'1000) >> 3;
            OwnedPtr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            if (r == 3) {
              printf("neg %s\n", rm_string.ptr);
            }
            else if (r == 4) {
              printf("mul %s\n", rm_string.ptr);
            }
            else if (r == 6) {
              printf("div %s\n", rm_string.ptr);
            }
            else if (r == 7) {
              printf("idiv %s\n", rm_string.ptr);
            }
            else {
              printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                     maybe_rex, op, modrm);

              return;
            }
            break;
          }
        case 0xD3: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            const uint8_t r = (modrm & 0b0011'1000) >> 3;
            OwnedPtr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            if (r == 4) {
              printf("sal %s, CL\n", rm_string.ptr);
            }
            else if (r == 5) {
              printf("shr %s, CL\n", rm_string.ptr);
            }
            else if (r == 7) {
              printf("sar %s, CL\n", rm_string.ptr);
            }
            else {
              printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                     maybe_rex, op, modrm);

              return;
            }
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx\n",
                   maybe_rex, op);

            return;
          }
      }
    }
    else if ((maybe_rex & 0b1111'1000) == X64::REX) {
      uint8_t op = *bytes++;
      switch (op) {
        case X64::MOV_8_TO_R8:
        case (X64::MOV_8_TO_R8 + 1):
        case (X64::MOV_8_TO_R8 + 2):
        case (X64::MOV_8_TO_R8 + 3):
        case (X64::MOV_8_TO_R8 + 4):
        case (X64::MOV_8_TO_R8 + 5):
        case (X64::MOV_8_TO_R8 + 6):
        case (X64::MOV_8_TO_R8 + 7): {
            const uint8_t reg = (op - X64::MOV_8_TO_R8) | ((maybe_rex & 0b0000'0001) << 3);
            const char* r_string = b8_rex_reg_name(reg);

            uint8_t imm8 = bytes[0];
            bytes++;

            printf("mov %s, 0x%hhx\n", r_string, imm8);
            break;
          }
        case X64::MOV_IMM32_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, false, short_address, short_operand);

            OwnedPtr<char> rm = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            uint32_t val = x32_from_bytes(bytes);
            bytes += 4;

            printf("mov %s, %u\n", rm.ptr, val);
            break;
          }
        case X64::MOV_R8_TO_RM8: {
            uint8_t modrm = *bytes++;

            //Overide
            p_opts.rm_name = b8_rex_reg_name;
            p_opts.r_name = b8_rex_reg_name;
            p_opts.mem_size = "BYTE PTR";

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("mov %s, %s\n", names.r.ptr, names.rm.ptr);
            break;
          }
        case 0x0F: {
            uint8_t op2 = *bytes++;
            switch (op2) {
              case X64::SETE_RM8: {
                  uint8_t modrm = *bytes++;

                  p_opts.rm_name = b8_rex_reg_name;
                  p_opts.r_name = b8_rex_reg_name;
                  p_opts.mem_size = "BYTE PTR";

                  OwnedPtr<char> r_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);
                  printf("sete %s\n", r_string.ptr);
                  break;
                }
              case X64::SETG_RM8: {
                  uint8_t modrm = *bytes++;

                  p_opts.rm_name = b8_rex_reg_name;
                  p_opts.r_name = b8_rex_reg_name;
                  p_opts.mem_size = "BYTE PTR";

                  OwnedPtr<char> r_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);
                  printf("setg %s\n", r_string.ptr);
                  break;
                }
              case X64::SETL_RM8: {
                  uint8_t modrm = *bytes++;

                  p_opts.rm_name = b8_rex_reg_name;
                  p_opts.r_name = b8_rex_reg_name;
                  p_opts.mem_size = "BYTE PTR";

                  OwnedPtr<char> r_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);
                  printf("setl %s\n", r_string.ptr);
                  break;
                }
              default: {
                  printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                         maybe_rex, op, op2);

                  return;
                }
            }
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx\n",
                   maybe_rex, op);

            return;
          }
      }
    }
    else if (maybe_rex == 0x0F) {
      //0x0F instructions
      uint8_t op = *bytes++;
      switch (op) {
        case X64::JE_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("je 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JNE_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jne 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JB_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jb 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JNB_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jnb 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JA_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("ja 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JNA_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jna 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JL_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jl 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JNL_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jnl 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JG_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jg 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JNG_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jng 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::SETE_RM8: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = b8_rex_reg_name;
            p_opts.mem_size = "BYTE PTR";

            OwnedPtr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &bytes);
            printf("sete %s\n", r_string.ptr);
            break;
          }
        case X64::SETL_RM8: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = b8_rex_reg_name;
            p_opts.mem_size = "BYTE PTR";

            OwnedPtr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &bytes);
            printf("setl %s\n", r_string.ptr);
            break;
          }
        case X64::SETG_RM8: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = b8_rex_reg_name;
            p_opts.mem_size = "BYTE PTR";

            OwnedPtr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &bytes);
            printf("setg %s\n", r_string.ptr);
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx\n",
                   maybe_rex, op);

            return;
          }

      }

    }
    else {
      //Non-REX instruction
      const uint8_t op = maybe_rex;

      switch (op) {
        case X64::MOV_R8_TO_RM8: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = b8_no_rex_reg_name;
            p_opts.r_name = b8_no_rex_reg_name;
            p_opts.mem_size = "BYTE PTR";

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("mov %s, %s\n", names.r.ptr, names.rm.ptr);
            break;
          }
        case X64::MOV_8_TO_R8:
        case (X64::MOV_8_TO_R8 + 1):
        case (X64::MOV_8_TO_R8 + 2):
        case (X64::MOV_8_TO_R8 + 3):
        case (X64::MOV_8_TO_R8 + 4):
        case (X64::MOV_8_TO_R8 + 5):
        case (X64::MOV_8_TO_R8 + 6):
        case (X64::MOV_8_TO_R8 + 7): {
            const uint8_t reg = (op - X64::MOV_8_TO_R8);
            const char* r_string = b8_no_rex_reg_name(reg);

            uint8_t imm8 = bytes[0];
            bytes++;

            printf("mov %s, 0x%hhx\n", r_string, imm8);
            break;
          }
        case X64::PUSH_R:
        case (X64::PUSH_R + 1):
        case (X64::PUSH_R + 2):
        case (X64::PUSH_R + 3):
        case (X64::PUSH_R + 4):
        case (X64::PUSH_R + 5):
        case (X64::PUSH_R + 6):
        case (X64::PUSH_R + 7): {
            //Default to long mode
            const char* r_string = x86_64_reg_name_from_num(op - X64::PUSH_R);

            printf("push %s\n", r_string);
            break;
          }
        case X64::POP_R:
        case (X64::POP_R + 1):
        case (X64::POP_R + 2):
        case (X64::POP_R + 3):
        case (X64::POP_R + 4):
        case (X64::POP_R + 5):
        case (X64::POP_R + 6):
        case (X64::POP_R + 7): {
            //Default to long mode
            const char* r_string = x86_64_reg_name_from_num(op - X64::POP_R);

            printf("pop %s\n", r_string);
            break;
          }
        case X64::JMP_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jmp 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::RET_NEAR: {
            printf("ret\n");
            break;
          }
        case X64::CALL_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("call 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::MOV_IMM32_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, false, short_address, short_operand);

            OwnedPtr<char> rm = rm_reg_string(&p_opts, 0, modrm, &bytes);

            uint32_t val = x32_from_bytes(bytes);
            bytes += 4;

            printf("mov %s, %u\n", rm.ptr, val);
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx\n",
                   maybe_rex);

            return;
          }
  }
    }
  }
}

#endif
