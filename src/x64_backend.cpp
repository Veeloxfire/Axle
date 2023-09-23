#include "ir.h"
#include "compiler.h"
#include "x64_backend.h"
#include "trace.h"

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

  struct RAX {};

  struct R {
    uint8_t r;

    bool operator==(const R& o) {
      return o.r == r;
    }
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
    REX = 0b0100'0000,
    REX_W = REX | 0b1000,
    REX_R = 0b000'00100,
    REX_X = 0b000'00010,
    REX_B = 0b000'00001,

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
    CMP_IMM_TO_AL = 0x3C,
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
    MUL_RM8_TO_RAX = 0xF6,// r = 4
    IMUL_RM8_TO_RAX = 0xF6,// r = 5
    DIV_RM8_TO_RAX = 0xF6,// r = 6
    IDIV_RM8_TO_RAX = 0xF6,// r = 7
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

  static void div(Instruction& arr, R8 from, RAX) {
    if (need_rex(from.r)) {
      arr.insert(X64::REX | X64::rex_rm(from.r));
    }
    arr.insert(X64::DIV_RM8_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(6, from.r));
  }

  static void div(Instruction& arr, R16 from, RAX) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r)) {
      arr.insert(X64::REX | X64::rex_rm(from.r));
    }
    arr.insert(X64::DIV_RM_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(6, from.r));
  }

  static void div(Instruction& arr, R32 from, RAX) {
    if (need_rex(from.r)) {
      arr.insert(X64::REX | X64::rex_rm(from.r));
    }
    arr.insert(X64::DIV_RM_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(6, from.r));
  }

  static void div(Instruction& arr, R64 from, RAX) {
    arr.insert(X64::REX_W | X64::rex_rm(from.r));
    arr.insert(X64::DIV_RM_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(6, from.r));
  }

  static void idiv(Instruction& arr, R8 from, RAX) {
    if (need_rex(from.r)) {
      arr.insert(X64::REX | X64::rex_rm(from.r));
    }
    arr.insert(X64::IDIV_RM8_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(7, from.r));
  }

  static void idiv(Instruction& arr, R16 from, RAX) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r)) {
      arr.insert(X64::REX | X64::rex_rm(from.r));
    }
    arr.insert(X64::IDIV_RM_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(7, from.r));
  }

  static void idiv(Instruction& arr, R32 from, RAX) {
    if (need_rex(from.r)) {
      arr.insert(X64::REX | X64::rex_rm(from.r));
    }
    arr.insert(X64::IDIV_RM_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(7, from.r));
  }

  static void idiv(Instruction& arr, R64 from, RAX) {
    arr.insert(X64::REX_W | X64::rex_rm(from.r));
    arr.insert(X64::IDIV_RM_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(7, from.r));
  }

  static void mul(Instruction& arr, R8 from, RAX) {
    if (need_rex(from.r)) {
      arr.insert(X64::REX | X64::rex_rm(from.r));
    }
    arr.insert(X64::MUL_RM8_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(4, from.r));
  }

  static void mul(Instruction& arr, R16 from, RAX) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r)) {
      arr.insert(X64::REX | X64::rex_rm(from.r));
    }
    arr.insert(X64::MUL_RM_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(4, from.r));
  }

  static void mul(Instruction& arr, R32 from, RAX) {
    if (need_rex(from.r)) {
      arr.insert(X64::REX | X64::rex_rm(from.r));
    }
    arr.insert(X64::MUL_RM_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(4, from.r));
  }

  static void mul(Instruction& arr, R64 from, RAX) {
    arr.insert(X64::REX_W | X64::rex_rm(from.r));
    arr.insert(X64::MUL_RM_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(4, from.r));
  }

  static void imul(Instruction& arr, R8 from, RAX) {
    if (need_rex(from.r)) {
      arr.insert(X64::REX | X64::rex_rm(from.r));
    }
    arr.insert(X64::IMUL_RM8_TO_RAX);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(5, from.r));
  }

  static void imul(Instruction& arr, R16 from, R16 to) {
    arr.insert(OVERRIDE_OPERAND);
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::IMUL_RM_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void imul(Instruction& arr, R32 from, R32 to) {
    if (need_rex(from.r) || need_rex(to.r)) {
      arr.insert(X64::REX | X64::rex_r_rm(to.r, from.r));
    }
    arr.insert(0x0F);
    arr.insert(X64::IMUL_RM_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
  }

  static void imul(Instruction& arr, R64 from, R64 to) {
    arr.insert(X64::REX_W | X64::rex_r_rm(to.r, from.r));
    arr.insert(0x0F);
    arr.insert(X64::IMUL_RM_TO_R);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(to.r, from.r));
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

  static void cmp(Instruction& arr, const RM8& rm, IMM8 imm8) {
    if (need_rex(rm.r)) {
      arr.insert(X64::REX | X64::rex_rm(rm.r));
    }
    arr.insert(X64::CMP_IMM_TO_RM8);
    emit_mod_rm(arr, { 7 }, rm);

    arr.insert(imm8.imm);
  }

  static void cmp(Instruction& arr, R8 rm, IMM8 imm8) {
    if (rm.r == rax.REG) {
      arr.insert(X64::CMP_IMM_TO_AL);
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

  static void append_instruction(X64::Program* program, const Instruction& i) {
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
};

namespace Helpers {
  static void zero_register(X64::Program* program, X64::R r) {
    X64::Instruction i = {};
    X64::xor_(i, X64::R32{r}, X64::R32{r});
    X64::append_instruction(program, i);
  }

  static void sign_extend_rax_rdx(X64::Program* program, IR::Format f) {
    ASSERT(f == IR::Format::sint16 || f == IR::Format::sint32 || f == IR::Format::sint64);

    X64::Instruction i = {};
    if (f == IR::Format::sint16) {
      i.insert(X64::OVERRIDE_OPERAND);
    }
    else if (f == IR::Format::sint64) {
      i.insert(X64::REX_W);
    }
    i.insert(X64::CQO);

    X64::append_instruction(program, i);
  }

  static void copy_address_to_reg(X64::Program* program, const MemoryView& mem, X64::R r) {
    X64::Instruction i = {};
    X64::lea(i, mem.rm, r);

    X64::append_instruction(program, i);
  }

  static void copy_reg_to_reg(X64::Program* program,
                              X64::R from, IR::Format f_format,
                              X64::R to, IR::Format t_format) {
    ASSERT(f_format != IR::Format::opaque && t_format != IR::Format::opaque);

    X64::Instruction inst = {};

    switch (f_format) {
      case IR::Format::uint8: {
          switch (t_format) {
            case IR::Format::uint8:
            case IR::Format::sint8:
              if (from == to) break;
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
              if (from == to) break;
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
              if (from == to) break;
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
              if (from == to) break;
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
              if (from == to) break;
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
              if (from == to) break;
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
              if (from == to) break;
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

  static void copy_mem_to_reg(X64::Program* program,
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

  static void copy_reg_to_mem(X64::Program* program,
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

  static void load_const_to_reg(X64::Program* program, IR::Format f, X64::R reg, const IR::C_ARG& const_arg) {
    ASSERT(f != IR::Format::opaque);

    X64::Instruction i = {};

    switch (f) {
      case IR::Format::uint8:
      case IR::Format::sint8: {
          X64::mov(i, X64::R8{ reg }, X64::IMM8{ const_arg.val[0] });
          break;
        }
      case IR::Format::uint16:
      case IR::Format::sint16: {
          X64::mov(i, X64::R16{reg}, X64::IMM16{ x16_from_bytes(const_arg.val) });
          break;
        }
      case IR::Format::uint32:
      case IR::Format::sint32: {
          X64::mov(i, X64::R32{reg}, X64::IMM32{ x32_from_bytes(const_arg.val) });
          break;
        }
      case IR::Format::uint64:
      case IR::Format::sint64: {
          X64::mov(i, X64::R64{reg}, X64::IMM64{ x64_from_bytes(const_arg.val) });
          break;
        }
      default:
        INVALID_CODE_PATH("Cant handle other data formats");
    }

    X64::append_instruction(program, i);
  }

  static void load_const_to_mem(X64::Program* program, IR::Format f, MemoryView view, const IR::C_ARG& const_arg) {
    ASSERT(f != IR::Format::opaque);

    ASSERT(const_arg.size == view.size);
    ASSERT(x64_types_info.get_size(f) == view.size);

    switch (f) {
      case IR::Format::uint8:
      case IR::Format::sint8: {
          X64::Instruction i = {};
          X64::mov(i, X64::RM8{ view.rm }, X64::IMM8{ const_arg.val[0] });
          X64::append_instruction(program, i);
          break;
        }
      case IR::Format::uint16:
      case IR::Format::sint16: {
          X64::Instruction i = {};
          X64::mov(i, X64::RM16{ view.rm }, X64::IMM16{ x16_from_bytes(const_arg.val) });
          X64::append_instruction(program, i);
          break;
        }
      case IR::Format::uint32:
      case IR::Format::sint32: {
          X64::Instruction i = {};
          X64::mov(i, X64::RM32{ view.rm }, X64::IMM32{ x32_from_bytes(const_arg.val) });
          X64::append_instruction(program, i);
          break;
        }
      case IR::Format::uint64:
      case IR::Format::sint64: {
          u64 val = x64_from_bytes(const_arg.val);

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

  static void load_const_to_mem_opaque(X64::Program* program, MemoryView view, const u8* data) {
    ASSERT(false);//THis implementation is bad, will fix later
    using PTR = const u8*;

    constexpr static auto COPY8 = [](X64::Program* program, MemoryView& view, PTR& data) {
      X64::Instruction i = {};
      X64::mov(i, view.rm, X64::IMM8{ data[0] });
      view.rm.disp += 1;
      view.size -= 1;
      data += 1;

      X64::append_instruction(program, i);
    };

    constexpr static auto COPY16 = [](X64::Program* program, MemoryView& view, PTR& data) {
      X64::Instruction i = {};
      X64::mov(i, view.rm, X64::IMM16{ x16_from_bytes(data) });
      view.rm.disp += 2;
      view.size -= 2;
      data += 2;

      X64::append_instruction(program, i);
    };

    constexpr static auto COPY32 = [](X64::Program* program, MemoryView& view, PTR& data) {
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



  static void copy_mem_to_mem_small(X64::Program* program,
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

  static void copy_mem_to_mem_opaque(X64::Program* program, MemoryView from, MemoryView to, X64::R temp_reg) {
    constexpr static auto COPY8 = [](X64::Program* program,
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

    constexpr static auto COPY16 = [](X64::Program* program,
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

    constexpr static auto COPY32 = [](X64::Program* program,
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
  IR::Format emit_ ## name(X64::Program* program,\
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

  IR::Format emit_mul(X64::Program* program,
                      X64::R left, IR::Format l_format,
                      X64::R right, IR::Format r_format) {
    ASSERT(l_format == r_format);
    X64::Instruction i = {};
    switch (l_format) {
      case  IR::Format::uint8: {
          ASSERT(right.r == X64::rax.REG);
          X64::mul(i, X64::R8{right}, X64::RAX{});
          break;
        }
      case  IR::Format::sint8: {
          ASSERT(left.r == X64::rax.REG);
          X64::imul(i, X64::R8{right}, X64::RAX{});
          break;
        }
      case  IR::Format::uint16: {
          zero_register(program, X64::R{X64::rdx.REG});
          ASSERT(left.r == X64::rax.REG);
          X64::mul(i, X64::R16{right}, X64::RAX{});
          break;
        }
      case  IR::Format::sint16: {
          X64::imul(i, X64::R16{right}, X64::R16{left});
          break;
        }
      case  IR::Format::uint32: {
          zero_register(program, X64::R{X64::rdx.REG});
          ASSERT(left.r == X64::rax.REG);
          X64::mul(i, X64::R32{right}, X64::RAX{});
          break;
        }
      case  IR::Format::sint32: {
          X64::imul(i, X64::R32{right}, X64::R32{left});
          break;
        }
      case  IR::Format::uint64: {
          zero_register(program, X64::R{X64::rdx.REG});
          ASSERT(left.r == X64::rax.REG);
          X64::mul(i, X64::R64{right}, X64::RAX{});
          break;
        }
      case  IR::Format::sint64: {
          X64::imul(i, X64::R64{right}, X64::R64{left});
          break;
        }
      default: INVALID_CODE_PATH("Invalid comparable format");
    }

    X64::append_instruction(program, i);
    return l_format;
  }

  IR::Format emit_div(X64::Program* program,
                      X64::R left, IR::Format l_format,
                      X64::R right, IR::Format r_format) {
    ASSERT(l_format == r_format);
    X64::Instruction i = {};
    switch (l_format) {
      case  IR::Format::uint8: {
          ASSERT(left.r == X64::rax.REG);
          X64::div(i, X64::R8{right}, X64::RAX{});
          break;
        }
      case  IR::Format::sint8: {
          ASSERT(left.r == X64::rax.REG);
          X64::idiv(i, X64::R8{right}, X64::RAX{});
          break;
        }
      case  IR::Format::uint16: {
          zero_register(program, X64::R{X64::rdx.REG});
          ASSERT(left.r == X64::rax.REG);
          X64::div(i, X64::R16{right}, X64::RAX{});
          break;
        }
      case  IR::Format::sint16: {
          sign_extend_rax_rdx(program, l_format);
          ASSERT(left.r == X64::rax.REG);
          X64::idiv(i, X64::R16{right}, X64::RAX{});
          break;
        }
      case  IR::Format::uint32: {
          zero_register(program, X64::R{X64::rdx.REG});
          ASSERT(left.r == X64::rax.REG);
          X64::div(i, X64::R32{right}, X64::RAX{});
          break;
        }
      case  IR::Format::sint32: {
          sign_extend_rax_rdx(program, l_format);
          ASSERT(left.r == X64::rax.REG);
          X64::idiv(i, X64::R32{right}, X64::RAX{});
          break;
        }
      case  IR::Format::uint64: {
          zero_register(program, X64::R{X64::rdx.REG});
          ASSERT(left.r == X64::rax.REG);
          X64::div(i, X64::R64{right}, X64::RAX{});
          break;
        }
      case  IR::Format::sint64: {
          sign_extend_rax_rdx(program, l_format);
          ASSERT(left.r == X64::rax.REG);
          X64::idiv(i, X64::R64{right}, X64::RAX{});
          break;
        }
      default: INVALID_CODE_PATH("Invalid comparable format");
    }

    X64::append_instruction(program, i);
    return l_format;
  }

  void emit_cmp(X64::Program* program,
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

  IR::Format emit_great(X64::Program* program,
                        X64::R left, IR::Format l_format,
                        X64::R right, IR::Format r_format) {
    emit_cmp(program, left, l_format, right, r_format);
    X64::Instruction i = {};
    X64::setg(i, X64::R8{left});
    X64::append_instruction(program, i);
    return IR::Format::uint8;
  }

  IR::Format emit_less(X64::Program* program,
                       X64::R left, IR::Format l_format,
                       X64::R right, IR::Format r_format) {
    emit_cmp(program, left, l_format, right, r_format);
    X64::Instruction i = {};
    X64::setl(i, X64::R8{left});
    X64::append_instruction(program, i);
    return IR::Format::uint8;
  }

  IR::Format emit_eq(X64::Program* program,
                     X64::R left, IR::Format l_format,
                     X64::R right, IR::Format r_format) {
    emit_cmp(program, left, l_format, right, r_format);
    X64::Instruction i = {};
    X64::sete(i, X64::R8{left});
    X64::append_instruction(program, i);
    return IR::Format::uint8;
  }

  IR::Format emit_neq(X64::Program* program,
                      X64::R left, IR::Format l_format,
                      X64::R right, IR::Format r_format) {
    emit_cmp(program, left, l_format, right, r_format);
    X64::Instruction i = {};
    X64::setne(i, X64::R8{left});
    X64::append_instruction(program, i);
    return IR::Format::uint8;
  }
}

struct LifetimeEdge {
  u32 a = 0;
  u32 b = 0;
};

struct ValueLifetime {
  bool visited = false;
  u32 first_use = 0;
  u32 last_use = 0;
};

enum struct MapType {
  Internal = 0,
  External,
  InternalNoMap,
  ExternalNoMap,
};

struct RegisterMapping {
  MapType map_type = MapType::Internal;
  bool crosses_call = false;
  bool known_reg = false;
  u8 reg = 0;
  union {
    u32 expr_id;//Inside Block
    u32 reg_bitmask;//Crossing Blocks
  };
};

struct RegisterResolver {
  const SignatureStructure* sig_struct;
  const CallingConvention* this_convention;

  const IR::ControlBlock* current_block;

  ViewArr<const IR::GlobalReference> globals_used;
  ViewArr<const IR::BlockImport> imports;
  ViewArr<const IR::BlockExport> exports;
  ViewArr<const IR::BlockMerge> merges;

  ViewArr<RegisterMapping> to_map;

  ViewArr<const IR::SSATemp> local_temporaries;

  const u8* bytecode_start;
  const u8* bytecode_end;

  bool has_called = false;
  u32 last_call = 0;

  u32 local_temp_id(IR::ValueIndex val_index) const {
    //TODO: maybe remove this - it now does nothing, but it might do something in the future
    ASSERT(val_index.index < local_temporaries.size);

    return val_index.index;
  }
};


enum struct NeedIntermediate : u8 {
  Yes,
  No,
};

struct VisitRes {
  NeedIntermediate ni;
  Type t;
};

static VisitRes visit_ordered_value(const ViewArr<RegisterMapping>& mappings,
                                    const OwnedArr<ValueLifetime>& lifetimes,
                                    const RegisterResolver* resolver,
                                    const IR::V_ARG& v_arg, u32 expr_id) {
  ASSERT(lifetimes.size == mappings.size);
  const u32 t_index = resolver->local_temp_id(v_arg.val);
  const IR::SSATemp& t = resolver->local_temporaries[t_index];

  const MapType map_type = mappings[t_index].map_type;
  if (map_type == MapType::InternalNoMap
      || map_type == MapType::ExternalNoMap) return { NeedIntermediate::Yes, t.type };//ignored- assumed its a memory location

  ValueLifetime& ov = lifetimes[t_index];
  ASSERT(v_arg.offset == 0);//For now we don't worry about this

  if (ov.visited) {
    ov.last_use = expr_id;

    if (resolver->has_called && (ov.first_use <= resolver->last_call && resolver->last_call <= ov.last_use)) {
      mappings[t_index].crosses_call = true;
    }
  }
  else {
    ov.visited = true;
    ov.first_use = expr_id;
    ov.last_use = expr_id;
  }

  if (t.requirements.has_address() || t.type.size() > 8) {
    return { NeedIntermediate::Yes, t.type };
  }

  if (map_type == MapType::Internal) {
    return { NeedIntermediate::No, t.type };
  }
  else {
    ASSERT(map_type == MapType::External);
    return { NeedIntermediate::Yes, t.type };
  }
}

void new_intermediate(Array<RegisterMapping>& intermediates, u32 expr_id) {
  intermediates.insert_uninit(1);
  auto* v = intermediates.back();
  v->known_reg = false;
  v->reg = 0;
  v->expr_id = expr_id;
}

void new_fixed_intermediate(Array<RegisterMapping>& intermediates, u32 expr_id, u8 reg) {
  intermediates.insert_uninit(1);
  auto* v = intermediates.back();
  v->known_reg = true;
  v->reg = reg;
  v->expr_id = expr_id;
}

struct ResolvedMappings {
  u32 call_space_needed = 0;
  u64 used_registers = 0;

  OwnedArr<RegisterMapping> intermediates;
};

OwnedArr<LifetimeEdge> determine_edges(const ViewArr<ValueLifetime>& lifetimes,
                                       const ViewArr<const ViewArr<RegisterMapping>>& other_mappings) {
  Array<LifetimeEdge> edges = {};

  for (usize a = 0; a < lifetimes.size; ++a) {
    const ValueLifetime& va = lifetimes[a];
    if (!va.visited) continue;

    for (usize b = 0; b < lifetimes.size; ++b) {
      if (a == b) continue;

      const ValueLifetime& vb = lifetimes[b];
      if (!va.visited) continue;

      const bool overlap_range = (vb.first_use <= va.last_use && va.first_use <= vb.first_use)
        || (va.first_use <= vb.last_use && vb.first_use <= va.first_use);

      if (overlap_range) {
        edges.insert({ static_cast<u32>(a), static_cast<u32>(b) });
      }
    }

    usize acc = lifetimes.size;
    for (usize m = 0; m < other_mappings.size; ++m) {
      const ViewArr<RegisterMapping>& mapping = other_mappings[m];

      for (usize i = 0; i < mapping.size; ++i) {
        const RegisterMapping& in = mapping[i];
        ASSERT(in.map_type != MapType::InternalNoMap);
        ASSERT(in.map_type != MapType::ExternalNoMap);
        ASSERT(in.known_reg);

        if (va.first_use <= in.expr_id && in.expr_id <= va.last_use) {
          edges.insert({ static_cast<u32>(a), static_cast<u32>(i + acc) });
        }
      }

      acc += mapping.size;
    }
  }

  return bake_arr(std::move(edges));
}

ResolvedMappings resolve_values(CompilerGlobals* comp,
                                CompilerThread* comp_thread,
                                RegisterResolver* resolver,
                                const CallingConvention* convention) {
  TRACING_FUNCTION();
  const u8* const bc_start = resolver->bytecode_start;
  const u8* const bc_end = resolver->bytecode_end;

  const u8* bc = bc_start;

  ASSERT(resolver->to_map.size == resolver->local_temporaries.size);
  const ViewArr<RegisterMapping>& values = resolver->to_map;
  OwnedArr<ValueLifetime> lifetimes = new_arr<ValueLifetime>(resolver->local_temporaries.size);

  //Is a value too big to fit in a register - don't worry about it
  for (u32 i = 0; i < resolver->local_temporaries.size; ++i) {
    const IR::SSATemp& temp = resolver->local_temporaries[i];
    if (temp.type.size() > x64_types_info.get_size(IR::Format::uint64) || temp.requirements.has_address()) {
      values[i].map_type = MapType::InternalNoMap;
    }
  }

  u32 expr_id = 0;


  FOR(resolver->merges, it) {
    const u32 i_index = resolver->local_temp_id(it->local_temp);
    if (values[i_index].map_type == MapType::InternalNoMap) {
      values[i_index].map_type = MapType::ExternalNoMap;
    }
    else {
      values[i_index].map_type = MapType::External;
    }

    const IR::SSATemp& t = resolver->local_temporaries[i_index];
    //Is used now
    IR::V_ARG artificial = {};
    artificial.offset = 0;
    artificial.size = t.type.size();
    artificial.val = it->local_temp;

    [[maybe_unused]] auto _im = visit_ordered_value(values, lifetimes, resolver, artificial, expr_id);
  }

  //Merges need to happen before imports (might import a merge)
  expr_id += 1;

  FOR(resolver->imports, it) {
    const u32 i_index = resolver->local_temp_id(it->local_temp);
    if (values[i_index].map_type == MapType::InternalNoMap) {
      values[i_index].map_type = MapType::ExternalNoMap;
      continue;
    }

    values[i_index].map_type = MapType::External;

    const IR::SSATemp& t = resolver->local_temporaries[i_index];
    //Is used now
    IR::V_ARG artificial = {};
    artificial.offset = 0;
    artificial.size = t.type.size();
    artificial.val = it->local_temp;

    [[maybe_unused]] auto _im = visit_ordered_value(values, lifetimes, resolver, artificial, expr_id);
  }


  //Don't resolve exports either
  FOR(resolver->exports, it) {
    const u32 i_index = resolver->local_temp_id(it->local_temp);
    if (values[i_index].map_type == MapType::InternalNoMap) {
      values[i_index].map_type = MapType::ExternalNoMap;
    }
    else {
      values[i_index].map_type = MapType::External;
    }
  }

  //Now visit all the values
  Array<RegisterMapping> mangled_registers = {};
  Array<RegisterMapping> intermediates = {};

  u32 call_space_needed = 0;
  u64 used_registers = 0;

  //Need to loop through the bytecode to check if we need any intermediate values
  while (bc < bc_end) {
    const u8 op_byte = *bc;
    const IR::OpCode op = static_cast<IR::OpCode>(op_byte);
    expr_id += 1;

    switch (op) {
      case IR::OpCode::BreakPoint: {
          IR::Types::BreakPoint bp;
          bc = IR::Read::BreakPoint(bc, bc_end, bp);

          //TODO: do breakpoints

          break;
        }

      case IR::OpCode::Set: {
          IR::Types::Set set;
          bc = IR::Read::Set(bc, bc_end, set);

          [[maybe_unused]] auto vr = visit_ordered_value(values, lifetimes, resolver, set.to, expr_id);

          break;
        }
      case IR::OpCode::SetStore: {
          IR::Types::SetStore set;
          bc = IR::Read::SetStore(bc, bc_end, set);

          VisitRes vr = visit_ordered_value(values, lifetimes, resolver, set.to, expr_id);
          if (vr.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id);
          }

          break;
        }
      case IR::OpCode::Copy: {
          IR::Types::Copy copy;
          bc = IR::Read::Copy(bc, bc_end, copy);

          VisitRes from = visit_ordered_value(values, lifetimes, resolver, copy.from, expr_id);
          VisitRes to = visit_ordered_value(values, lifetimes, resolver, copy.to, expr_id + 1);

          if (from.ni == NeedIntermediate::Yes && to.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id);
          }

          expr_id += 1;

          break;
        }
      case IR::OpCode::CopyLoad: {
          IR::Types::CopyLoad copy;
          bc = IR::Read::CopyLoad(bc, bc_end, copy);

          VisitRes from = visit_ordered_value(values, lifetimes, resolver, copy.from, expr_id);
          VisitRes to = visit_ordered_value(values, lifetimes, resolver, copy.to, expr_id + 1);

          if (from.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id);
          }
          if (to.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id + 1);
          }

          expr_id += 1;

          break;
        }
      case IR::OpCode::CopyStore: {
          IR::Types::CopyStore copy;
          bc = IR::Read::CopyStore(bc, bc_end, copy);

          VisitRes from = visit_ordered_value(values, lifetimes, resolver, copy.from, expr_id);
          VisitRes to = visit_ordered_value(values, lifetimes, resolver, copy.to, expr_id + 1);

          if (from.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id);
          }
          if (to.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id + 1);
          }

          expr_id += 1;

          break;
        }
      case IR::OpCode::CopyLoadStore: {
          IR::Types::CopyLoadStore copy;
          bc = IR::Read::CopyLoadStore(bc, bc_end, copy);

          VisitRes from = visit_ordered_value(values, lifetimes, resolver, copy.from, expr_id);
          VisitRes to = visit_ordered_value(values, lifetimes, resolver, copy.to, expr_id + 1);

          new_intermediate(intermediates, expr_id);
          if (from.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id);
          }
          if (to.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id + 1);
          }

          expr_id += 1;

          break;
        }
      case IR::OpCode::AddrOf: {
          IR::Types::AddrOf addr;
          bc = IR::Read::AddrOf(bc, bc_end, addr);

          VisitRes from = visit_ordered_value(values, lifetimes, resolver, addr.from, expr_id);
          ASSERT(from.ni == NeedIntermediate::Yes);

          expr_id += 1;
          VisitRes to = visit_ordered_value(values, lifetimes, resolver, addr.to, expr_id);

          if (to.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id);
          }

          break;
        }
      case IR::OpCode::AddrOfLoad: {
          IR::Types::AddrOfLoad addr;
          bc = IR::Read::AddrOfLoad(bc, bc_end, addr);

          VisitRes from = visit_ordered_value(values, lifetimes, resolver, addr.from, expr_id);
          VisitRes to = visit_ordered_value(values, lifetimes, resolver, addr.to, expr_id + 1);

          if (from.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id);
          }
          if (to.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id + 1);
          }

          expr_id += 1;
          break;
        }

      case IR::OpCode::AddrOfGlobal: {
          IR::Types::AddrOfGlobal addr;
          bc = IR::Read::AddrOfGlobal(bc, bc_end, addr);
          ASSERT(resolver->globals_used.size > addr.im32);
          const IR::GlobalReference& g = resolver->globals_used[addr.im32];

          VisitRes im = visit_ordered_value(values, lifetimes, resolver, addr.val, expr_id);

          if (im.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id);
          }

          break;
        }
      case IR::OpCode::StartFunc: {
          const SignatureStructure* sig = resolver->sig_struct;
          IR::Types::StartFunc start_func;
          bc = IR::Read::StartFunc(bc, bc_end, start_func);

          ASSERT(start_func.n_values == sig->parameter_types.size);

          for (usize i = 0; i < sig->parameter_types.size; ++i) {
            ASSERT(i < convention->num_parameter_registers);//TEMP

            new_fixed_intermediate(intermediates, expr_id, convention->parameter_registers[i]);

            IR::V_ARG arg;
            bc += IR::deserialize(bc, bc_end - bc, arg);

            [[maybe_unused]] VisitRes im = visit_ordered_value(values, lifetimes, resolver, arg, expr_id);
          }

          break;
        }
      case IR::OpCode::Call: {
          IR::Types::Call call;
          bc = IR::Read::Call(bc, bc_end, call);
          ASSERT(call.values == nullptr);

          const SignatureStructure* sig_struct = comp->get_label_signature(call.label);
          ASSERT(sig_struct != nullptr);

          bool has_return = sig_struct->return_type != comp_thread->builtin_types->t_void;

          u32 call_space = convention->shadow_space_size;

          for (usize i = 0; i < ((usize)call.n_values - has_return); ++i) {
            IR::V_ARG arg;
            bc += IR::deserialize(bc, bc_end - bc, arg);

            VisitRes im = visit_ordered_value(values, lifetimes, resolver, arg, expr_id);

            if (i < convention->num_parameter_registers) {
              new_fixed_intermediate(intermediates, expr_id, convention->parameter_registers[i]);
            }
            else {
              call_space += 8;
              if (im.ni == NeedIntermediate::Yes) {
                new_intermediate(intermediates, expr_id);
              }
            }
          }

          expr_id += 1;

          resolver->has_called = true;
          resolver->last_call = expr_id;

          if (call_space_needed < call_space) {
            call_space_needed = call_space;
          }

          expr_id += 1;


          if (has_return) {
            IR::V_ARG ret;
            bc += IR::deserialize(bc, bc_end - bc, ret);
            [[maybe_unused]] VisitRes im = visit_ordered_value(values, lifetimes, resolver, ret, expr_id + 1);

            new_fixed_intermediate(intermediates, expr_id, convention->return_register);

            expr_id += 1;
          }


          break;
        }

#define VISIT_BIN_OP(name) \
      case IR::OpCode:: name: {\
          IR::Types:: name bin_op;\
          bc = IR::Read:: name (bc, bc_end, bin_op);\
          VisitRes left = visit_ordered_value(values, lifetimes, resolver, bin_op.left, expr_id);\
          VisitRes right = visit_ordered_value(values, lifetimes, resolver, bin_op.right, expr_id);\
          VisitRes to = visit_ordered_value(values, lifetimes, resolver, bin_op.to, expr_id + 1);\
          /*temp: these register allocations can be simplified*/\
          new_intermediate(intermediates, expr_id);\
          if (right.ni == NeedIntermediate::Yes) {\
            new_intermediate(intermediates, expr_id);\
          }\
          expr_id += 1;\
          break;\
        }

                           VISIT_BIN_OP(Add);
                           VISIT_BIN_OP(Sub);
                           VISIT_BIN_OP(Eq);
                           VISIT_BIN_OP(Neq);
                           VISIT_BIN_OP(Less);
                           VISIT_BIN_OP(Great);
                           VISIT_BIN_OP(And);
                           VISIT_BIN_OP(Or);
                           VISIT_BIN_OP(Xor);
#undef VISIT_BIN_OP

      case IR::OpCode::Mul: {
          IR::Types::Mul bin_op;
          bc = IR::Read::Mul(bc, bc_end, bin_op);
          VisitRes left = visit_ordered_value(values, lifetimes, resolver, bin_op.left, expr_id);
          VisitRes right = visit_ordered_value(values, lifetimes, resolver, bin_op.right, expr_id);
          VisitRes to = visit_ordered_value(values, lifetimes, resolver, bin_op.to, expr_id + 1);
          /*always a left register needed*/
          switch (left.t.struct_format()) {
            case IR::Format::uint8:
            case IR::Format::sint8: {
                new_fixed_intermediate(intermediates, expr_id, X64::rax.REG);
                break;
              }
            case IR::Format::uint16:
            case IR::Format::uint32:
            case IR::Format::uint64: {
                new_fixed_intermediate(intermediates, expr_id, X64::rax.REG);
                new_fixed_intermediate(mangled_registers, expr_id, X64::rdx.REG);
                break;
              }
            default: {
                new_intermediate(intermediates, expr_id);
                break;
              }
          }

          if (right.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id);
          }

          expr_id += 1;
          break;
        }
      case IR::OpCode::Div: {
          IR::Types::Div bin_op;
          bc = IR::Read::Div(bc, bc_end, bin_op);
          VisitRes left = visit_ordered_value(values, lifetimes, resolver, bin_op.left, expr_id);
          VisitRes right = visit_ordered_value(values, lifetimes, resolver, bin_op.right, expr_id);
          VisitRes to = visit_ordered_value(values, lifetimes, resolver, bin_op.to, expr_id + 1);
          /*always a left register needed*/
          switch (left.t.struct_format()) {
            case IR::Format::uint8:
            case IR::Format::sint8: {
                new_fixed_intermediate(intermediates, expr_id, X64::rax.REG);
                break;
              }
            case IR::Format::sint16:
            case IR::Format::uint16:
            case IR::Format::sint32:
            case IR::Format::uint32:
            case IR::Format::sint64:
            case IR::Format::uint64: {
                new_fixed_intermediate(intermediates, expr_id, X64::rax.REG);
                new_fixed_intermediate(mangled_registers, expr_id, X64::rdx.REG);
                break;
              }
            default: {
                new_intermediate(intermediates, expr_id);
                break;
              }
          }

          if (right.ni == NeedIntermediate::Yes) {
            new_intermediate(intermediates, expr_id);
          }
          expr_id += 1;
          break;
        }

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
            comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                                      "Unsupported instruction encountered during ir compilation: \"{}\"",
                                      opcode_name);
            return {};
          }
        }
    }
  }

  expr_id += 1;

  //Any control flow checks we need
  {
    const IR::ControlBlock* current_block = resolver->current_block;
    switch (current_block->cf_type) {
      case IR::ControlFlowType::Start: break;
      case IR::ControlFlowType::End: break;
      case IR::ControlFlowType::Inline: break;
      case IR::ControlFlowType::Merge: break;

      case IR::ControlFlowType::Return: {
          ASSERT(resolver->sig_struct->return_type.size() <= 8);//TEMP

          IR::V_ARG v = { current_block->cf_return.val, 0, resolver->sig_struct->return_type.size() };

          [[maybe_unused]] auto vr = visit_ordered_value(values, lifetimes, resolver, v, expr_id);

          expr_id += 1;
          new_fixed_intermediate(intermediates, expr_id, convention->return_register);
          break;
        }

      case IR::ControlFlowType::Split: {
          IR::V_ARG v = { current_block->cf_split.condition, 0, comp_thread->builtin_types->t_bool.size() };

          [[maybe_unused]] auto vr = visit_ordered_value(values, lifetimes, resolver, v, expr_id);
          break;
        }
    }
  }

  expr_id += 1;

  //Use the final temporaries
  FOR(resolver->exports, it) {
    const u32 i_index = resolver->local_temp_id(it->local_temp);

    const IR::SSATemp& t = resolver->local_temporaries[i_index];
    //Is used now
    IR::V_ARG artificial = {};
    artificial.offset = 0;
    artificial.size = t.type.size();
    artificial.val = it->local_temp;

    [[maybe_unused]] auto _im = visit_ordered_value(values, lifetimes, resolver, artificial, expr_id);
  }


  // Create then colour the graph

  const usize number_of_registers = (usize)convention->num_volatile_registers
    + (usize)convention->num_non_volatile_registers;

  {
    // Start off doing intermediates since we can do those easily

    RegisterMapping* const i_start = intermediates.mut_begin();
    const RegisterMapping* const i_end = intermediates.end();

    for (RegisterMapping* i = i_start; i < i_end; ++i) {
      if (i->known_reg) {
        continue;
      }

      u64 used_regs = 0;

      for (const RegisterMapping* j = i_start; j < i_end; ++j) {
        if (j->known_reg) {
          used_regs |= 1llu << (usize)j->reg;
        }
      }

      u32 reg_index = 0;

      for (; reg_index < number_of_registers; ++reg_index) {
        u8 r = convention->all_regs_unordered[reg_index];
        if ((used_regs & (1llu << (usize)r)) == 0) {
          break;
        }
      }

      ASSERT(reg_index < number_of_registers);

      auto reg_id = convention->all_regs_unordered[reg_index];

      i->reg = reg_id;
      i->known_reg = true;

      used_registers |= (1llu << reg_id);
    }
  }

  const ViewArr<RegisterMapping> mappings[] = {
    view_arr(intermediates),
    view_arr(mangled_registers),
  };

  const OwnedArr<LifetimeEdge> edges = determine_edges(view_arr(lifetimes), view_arr(mappings));

  // Select the internal registers from the graph

  {
    if (comp_thread->print_options.register_select) {
      IO_Single::lock();
    }

    DEFER(&) {
      //Always needs to be done
      if (comp_thread->print_options.register_select) {
        IO_Single::unlock();
      }
    };

    const LifetimeEdge* edge_i = edges.begin();
    const LifetimeEdge* edge_end = edges.end();

    for (u32 i = 0; i < static_cast<u32>(values.size); ++i) {
      RegisterMapping& v = values[i];

      if (v.map_type != MapType::Internal) {
        while (edge_i < edge_end && edge_i->a == i) {//skip these for now
          edge_i += 1;
        }
        continue;//Only map internal values
      }

      u32 used_regs = 0;

      ASSERT(edge_i == edge_end || edge_i->a >= i);

      while (edge_i < edge_end && edge_i->a == i) {
        u32 b = edge_i->b;

        if (b >= values.size + intermediates.size) {
          u32 i = static_cast<u32>(b - (values.size + intermediates.size));
          ASSERT(mangled_registers.data[i].known_reg);
          auto r = mangled_registers.data[i].reg;
          ASSERT(r < 32);
          used_regs |= 1u << (u32)r;
        }
        else if (b >= values.size) {
          u32 i = static_cast<u32>(b - values.size);
          ASSERT(intermediates.data[i].known_reg);
          auto r = intermediates.data[i].reg;
          ASSERT(r < 32);
          used_regs |= 1u << (u32)r;
        }
        else {
          RegisterMapping& other = values[edge_i->b];
          if (other.known_reg) {
            ASSERT(other.reg < 32);
            used_regs |= 1u << (u32)other.reg;
          }
        }

        edge_i += 1;
      }

      u32 reg_index = 0;
      if (v.crosses_call) {
        reg_index = convention->num_volatile_registers;
      }

      for (; reg_index < number_of_registers; ++reg_index) {
        u8 r = convention->all_regs_unordered[reg_index];
        ASSERT(r < 32);
        if ((used_regs & (1u << r)) == 0) {
          break;
        }
      }

      //TODO: spill registers
      ASSERT(reg_index < number_of_registers);

      auto reg_id = convention->all_regs_unordered[reg_index];

      used_registers |= (1llu << reg_id);

      if (comp_thread->print_options.register_select) {
        format_print_ST("T{} = {}\n", i, X64::all_x64_regs[reg_id].name);
      }

      v.known_reg = true;
      v.reg = reg_id;
    }
  }

  //Save which registers intersect with the external values
  //TODO: pack these tighter, instead of interleaving all of them
  {
    const LifetimeEdge* edge_i = edges.begin();
    const LifetimeEdge* edge_end = edges.end();

    for (u32 i = 0; i < static_cast<u32>(values.size); ++i) {
      RegisterMapping& v = values[i];
      if (v.map_type != MapType::External) {
        while (edge_i < edge_end && edge_i->a == i) {//skip these for now
          edge_i += 1;
        }
        continue;//Only mapping external
      }

      ASSERT(!v.known_reg);

      u32 used_regs = 0;

      ASSERT(edge_i == edge_end || edge_i->a >= i);

      while (edge_i < edge_end && edge_i->a == i) {
        u32 b = edge_i->b;

        if (b >= values.size + intermediates.size) {
          u32 i = static_cast<u32>(b - (values.size + intermediates.size));
          ASSERT(mangled_registers.data[i].known_reg);
          auto r = mangled_registers.data[i].reg;
          ASSERT(r < 32);
          used_regs |= 1u << (u32)r;
        }
        else if (b >= values.size) {
          u32 i = static_cast<u32>(b - values.size);
          ASSERT(intermediates.data[i].known_reg);
          used_regs |= 1u << (u32)intermediates.data[i].reg;
        }
        else {
          RegisterMapping& other = values[edge_i->b];
          if (other.known_reg) {
            ASSERT(other.reg < 32);
            used_regs |= 1u << (u32)other.reg;
          }
        }

        edge_i += 1;
      }

      v.reg_bitmask = used_regs;
    }
  }

  return ResolvedMappings{
    call_space_needed,
    used_registers,

    bake_arr(std::move(intermediates)),
  };
}

struct ActualTemporary {
  bool maybe_intermeidate;
  bool is_register;
  union {
    X64::R reg_id;
    u32 stack_offset;
  };
};

struct X64Value {
  bool expects_intermeidate;
  ValueType value_type;
  Type t;
  union {
    MemoryView mem = {};
    X64::R reg;
  };
};

struct Selector {
  ViewArr<const X64::R> intermediates;

  u32 intermediates_counter = 0;

  u8 base_ptr_reg;

  ViewArr<const ActualTemporary> local_actual_values;
  ViewArr<const IR::SSATemp> local_temporaries;

  X64::R get_next_intermediate_reg() {
    ASSERT(intermediates_counter < intermediates.size);
    return intermediates[intermediates_counter++];
  }

  X64Value get_val(const IR::V_ARG& v) const {
    const u32 u = v.val.index;

    ASSERT(u < local_temporaries.size);
    const IR::SSATemp& temp = local_temporaries[u];

    const ActualTemporary& a_temp = local_actual_values[u];

    if (a_temp.is_register) {
      ASSERT(v.offset == 0);
      ASSERT(temp.type.size() <= 8);
      ASSERT(!temp.requirements.has_address());

      X64Value val = {};
      val.expects_intermeidate = a_temp.maybe_intermeidate;
      val.value_type = ValueType::Register;
      val.t = temp.type;
      val.reg = a_temp.reg_id;
      return val;
    }
    else {
      MemoryView view = {};
      view.rm = X64::memory_rm(base_ptr_reg, (-static_cast<i32>(a_temp.stack_offset)) + static_cast<i32>(v.offset));
      view.size = temp.type.size();
      view.known_alignment = temp.type.structure->alignment;

      X64Value val = {};
      val.expects_intermeidate = a_temp.maybe_intermeidate;
      val.value_type = ValueType::Memory;
      val.t = temp.type;
      val.mem = view;

      return val;
    }
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
                                   Backend::GenericProgram* program_in) {
  TRACING_FUNCTION();

  X64::Program* program = static_cast<X64::Program*>(program_in);

  Backend::FunctionMetadata func = {};
  func.code_start = program->code_store.current_location().actual_location;
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

  func.code_size = program->code_store.total_size - func.code_start;

  if (program->functions.size < lib_import->label.label) {
    usize to_append = lib_import->label.label - program->functions.size;
    program->functions.insert_uninit(to_append);
  }

  program->functions.data[lib_import->label.label - 1] = func;

}

void x64_init(CompilerGlobals* comp, CompilerThread* comp_thread, Backend::GenericProgram* program_in) {
  X64::Program* program = static_cast<X64::Program*>(program_in);


  const SignatureStructure* type;

  comp->dyn_lib_imports.insert_uninit(1);

  IR::DynLibraryImport& lib = *comp->dyn_lib_imports.back();
  {
    AtomicLock<StringInterner> strings;
    AtomicLock<Structures> structs;

    comp->services.get_multiple(&structs, &strings);


    lib.path = strings->intern("kernel32.dll");
    lib.name = strings->intern("ExitProcess");


    Array<Type> params = {};
    params.reserve_total(1);
    params.insert(comp->builtin_types->t_u32);

    type = find_or_make_lamdba_structure(structs._ptr, strings._ptr,
                                         comp->platform_interface.ptr_size, &X64::CONVENTION_microsoft_x64,
                                         std::move(params), comp->builtin_types->t_void);
  }
  lib.label = comp->next_function_label(type);


  x64_emit_dyn_library_function(comp_thread, &lib, type->calling_convention, program_in);
  if (comp_thread->is_panic()) {
    return;
  }

  program->exit_process = lib.label;
}

void x64_emit_start(CompilerGlobals* comp,
                    IR::GlobalLabel entry,
                    Backend::GenericProgram* program_in) {
  TRACING_FUNCTION();
  X64::Program* program = static_cast<X64::Program*>(program_in);

  program->entry_point = entry;
  program->start_code.code_start = program->code_store.current_location().actual_location;

  X64::Instruction init_stack = {};
  X64::R rbp_r = X64::R{ X64::rbp.REG };
  X64::R rsp_r = X64::R{ X64::rsp.REG };
  X64::mov(init_stack, X64::R64{rsp_r}, X64::R64{rbp_r});

  X64::append_instruction(program, init_stack);

  if (comp->build_options.debug_break_on_entry) {
    X64::Instruction trap = {};
    trap.insert(0xCC);
    X64::append_instruction(program, trap);
  }

  {
    Backend::Relocation relocation = {};
    relocation.type = Backend::RelocationType::Label;
    relocation.label = entry;
    relocation.location = program->code_store.total_size + X64::CALL_NEAR_OFFSET;
    program->relocations.insert(std::move(relocation));

    X64::Instruction call_entry = {};
    X64::call_near(call_entry, 0);

    X64::append_instruction(program, call_entry);
  }

  {
    X64::Instruction move_res = {};
    X64::mov(move_res, X64::R32{X64::R{X64::rax.REG}}, X64::R32{X64::R{X64::rcx.REG}});
    X64::append_instruction(program, move_res);
  }

  {
    Backend::Relocation relocation = {};
    relocation.type = Backend::RelocationType::Label;
    relocation.label = program->exit_process;
    relocation.location = program->code_store.total_size + X64::CALL_NEAR_OFFSET;
    program->relocations.insert(std::move(relocation));

    X64::Instruction call_exit = {};
    X64::call_near(call_exit, 0);

    X64::append_instruction(program, call_exit);
  }

  program->start_code.code_size = program->code_store.total_size - program->start_code.code_start;
}

struct VariableResolver {
  ViewArr<const IR::ControlBlock> blocks;
  ViewArr<const u64> temporary_offsets;
  ViewArr<const IR::SSAVar> variables;

  const CallingConvention* this_convention;

  ViewArr<RegisterMapping> temporary_mappings;
};

bool set_parent_intersects(const VariableResolver& resolver, u32 known, BitArray& intersects, const Array<IR::LocalLabel>& current_visit_stack) {
  bool crosses_call = false;

  usize l = known;
  for (; l < current_visit_stack.size - 1; ++l) {
    const auto label = current_visit_stack.data[l];
    ASSERT(label != IR::NULL_LOCAL_LABEL);
    ASSERT(label.label - 1 < intersects.length);

    crosses_call = crosses_call || resolver.blocks[label.label - 1].calls;

    intersects.set(label.label - 1);
  }

  return crosses_call;
}

struct CaptureVisitBranch {
  IR::LocalLabel next;
  u32 stack_height;
};

struct VersionMapping {
  bool crosses_call = false;
  bool known_reg = false;
  u8 reg = 0;
  union {
    u32 expr_id;//Inside Block
    u32 reg_bitmask;//Crossing Blocks
  };
};


static VersionMapping trace_version(const VariableResolver& resolver,
                                    u32 variable_id, const IR::VariableVersion& version,
                                    Array<CaptureVisitBranch>& visit_branches,
                                    Array<IR::LocalLabel>& current_visit_stack,
                                    BitArray& visited,
                                    BitArray& intersects) {
  u32 know_visited = 0;

  VersionMapping mapping = {};
  mapping.crosses_call = false;
  mapping.known_reg = false;
  mapping.reg = 0;
  mapping.reg_bitmask = 0;


  while (current_visit_stack.size > 0) {
    const IR::LocalLabel bl = *current_visit_stack.back();
    ASSERT(bl != IR::NULL_LOCAL_LABEL);
    ASSERT(bl.label - 1 < resolver.blocks.size);
    const u32 b = bl.label - 1;
    const u64 b_offset = resolver.temporary_offsets[b];

    if (visited.test(b)) {
      goto BRANCH_END;
    }

    visited.set(b);

    {
      const IR::ControlBlock& block = resolver.blocks[b];

      {
        FOR(block.enter_merge, it) {
          if (it->variable == variable_id
              && (it->in_versions[0] == version || it->in_versions[1] == version)) {
            const auto& map = resolver.temporary_mappings[b_offset + it->local_temp.index];
            ASSERT(map.map_type != MapType::Internal);
            ASSERT(map.map_type != MapType::InternalNoMap);
            ASSERT(!map.known_reg);
            mapping.reg_bitmask |= map.reg_bitmask;
            mapping.crosses_call = mapping.crosses_call || map.crosses_call;

            intersects.set(b);
            goto BRANCH_END;
          }
        }
      }

      {
        FOR(block.imports, it) {
          if (it->variable == variable_id && it->in_version == version) {
            const auto& map = resolver.temporary_mappings[b_offset + it->local_temp.index];
            ASSERT(map.map_type != MapType::Internal);
            ASSERT(map.map_type != MapType::InternalNoMap);
            ASSERT(!map.known_reg);
            mapping.reg_bitmask |= map.reg_bitmask;
            mapping.crosses_call = mapping.crosses_call || map.crosses_call;

            intersects.set(b);
            bool stack_crossed_call = set_parent_intersects(resolver, know_visited, intersects, current_visit_stack);
            mapping.crosses_call = mapping.crosses_call || stack_crossed_call;

            know_visited = (u32)current_visit_stack.size;
          }
        }
      }

      {
        FOR(block.exports, it) {
          if (it->variable == variable_id && it->version != version.version) {
            const auto& map = resolver.temporary_mappings[b_offset + it->local_temp.index];
            ASSERT(map.map_type != MapType::Internal);
            ASSERT(map.map_type != MapType::InternalNoMap);
            ASSERT(!map.known_reg);
            mapping.reg_bitmask |= map.reg_bitmask;
            mapping.crosses_call = mapping.crosses_call || map.crosses_call;

            intersects.set(b);
            goto BRANCH_END;
          }
        }
      }


      switch (block.cf_type) {
        case IR::ControlFlowType::Start: {
            ASSERT(block.cf_start.child != IR::NULL_LOCAL_LABEL);//catches blocks with missing cf
            current_visit_stack.insert(block.cf_start.child);
            continue;
          }

        case IR::ControlFlowType::Return:
        case IR::ControlFlowType::End: {
            break;
          }

        case IR::ControlFlowType::Inline: {
            current_visit_stack.insert(block.cf_inline.child);
            continue;
          }
        case IR::ControlFlowType::Merge: {
            current_visit_stack.insert(block.cf_inline.child);
            continue;
          }

        case IR::ControlFlowType::Split: {
            visit_branches.insert(CaptureVisitBranch{ block.cf_split.false_branch, (u32)current_visit_stack.size });
            current_visit_stack.insert(block.cf_split.true_branch);
            continue;
          }
      }
    }

  BRANCH_END:
    //Already been down this path
    if (intersects.test(b)) {
      //and the value is used in all of these
      bool stack_crossed_call = set_parent_intersects(resolver, know_visited, intersects, current_visit_stack);
      mapping.crosses_call = mapping.crosses_call || stack_crossed_call;
      know_visited = (u32)current_visit_stack.size;
    }

    if (visit_branches.size == 0) break;//we are done

    const auto& branch = *visit_branches.back();
    ASSERT(branch.stack_height <= current_visit_stack.size);

    if (know_visited > branch.stack_height) know_visited = branch.stack_height;

    current_visit_stack.pop_n(current_visit_stack.size - branch.stack_height);
    current_visit_stack.insert(branch.next);

    visit_branches.pop();
  }

  return mapping;
}

struct VariableVersionMap {
  bool memory;
  u32 versions_start;
  u32 num_versions;
};

struct VariableMappings {
  u32 used_registers;
  OwnedArr<VariableVersionMap> variable_versions;
  OwnedArr<VersionMapping> version_reg_map;
};

constexpr bool ssa_var_in_memory(const IR::SSAVar& var) {
  return var.requirements.has_address() || var.type.size() > 8;
}

VariableMappings resolve_variables(const VariableResolver& resolver) {
  OwnedArr<VariableVersionMap> variables = new_arr<VariableVersionMap>(resolver.variables.size);

  struct VariableVersionInt {
    BitArray intersects = {};
  };

  u32 total_versions = 0;
  for (u32 i = 0; i < variables.size; ++i) {
    const IR::SSAVar& v = resolver.variables[i];
    ASSERT(v.versions > 0);

    if (ssa_var_in_memory(v)) {
      variables[i].memory = true;
      continue;
    }

    VariableVersionMap& v_map = variables[i];
    v_map.versions_start = total_versions;
    v_map.num_versions = v.versions;

    total_versions += v.versions;
  }

  OwnedArr version_intersects = new_arr<VariableVersionInt>(total_versions);
  OwnedArr version_mappings = new_arr<VersionMapping>(total_versions);

  const u32 num_blocks = (u32)resolver.blocks.size;

  {
    Array<CaptureVisitBranch> visit_branches;
    Array<IR::LocalLabel> current_visit_stack;
    BitArray visited = BitArray(num_blocks);
    //DFS since we need to save how we got there

    for (u32 a = 0; a < num_blocks; ++a) {
      IR::LocalLabel a_label = IR::LocalLabel{ a + 1 };
      current_visit_stack.insert(a_label);//reaches self

      const IR::ControlBlock& root = resolver.blocks[a];

      FOR(root.exports, exps) {
        ASSERT(exps->variable < variables.size);
        const VariableVersionMap& map = variables[exps->variable];

        if (map.memory) continue;//no need to worry about these ones

        ASSERT(exps->version < map.num_versions);
        ASSERT(map.versions_start + exps->version < version_mappings.size);

        u32 version_index = map.versions_start + exps->version;
        VariableVersionInt& intersects = version_intersects[version_index];

        ASSERT(intersects.intersects.data == nullptr);
        intersects.intersects = BitArray(num_blocks);
        BitArray& i_bitmask = intersects.intersects;

        IR::VariableVersion version = {
          a_label,
          exps->version,
          exps->local_temp,
        };

        visited.clear();
        visit_branches.clear();
        current_visit_stack.clear();

        current_visit_stack.insert(root.label);

        version_mappings[version_index] = trace_version(resolver, exps->variable, version,
                                                        visit_branches, current_visit_stack, visited, i_bitmask);
      }
    }
  }

  Array<LifetimeEdge> edges;

  for (u32 va = 0; va < variables.size; ++va) {
    if (variables[va].memory) continue;

    const u32 va_start = variables[va].versions_start;
    const u32 va_end = va_start + variables[va].num_versions;

    for (u32 vb = 0; vb < variables.size; ++vb) {
      if (va == vb) continue;

      if (variables[vb].memory) continue;

      const u32 vb_start = variables[vb].versions_start;
      const u32 vb_end = vb_start + variables[vb].num_versions;

      for (u32 a = va_start; a < va_end; ++a) {
        const auto& a_range = version_intersects[a].intersects;
        if (a_range.data == nullptr) continue;

        for (u32 b = vb_start; b < vb_end; ++b) {
          ASSERT(b != a);
          const auto& b_range = version_intersects[b].intersects;
          if (b_range.data == nullptr) continue;

          if (a_range.intersects(b_range)) {
            edges.insert({ a, b });
          }
        }
      }
    }
  }

  u32 used_registers = 0;

  {
    const CallingConvention* convention = resolver.this_convention;

    const usize number_of_registers = (usize)convention->num_volatile_registers
      + (usize)convention->num_non_volatile_registers;

    const LifetimeEdge* edge_i = edges.begin();
    const LifetimeEdge* edge_end = edges.end();

    FOR(variables, var) {
      if (var->memory) continue;
      const u32 i_end = var->versions_start + var->num_versions;

      for (u32 i = var->versions_start; i < i_end; ++i) {
        auto& v = version_mappings[i];


        u32 used_regs = v.reg_bitmask;

        ASSERT(edge_i == edge_end || edge_i->a >= i);

        while (edge_i < edge_end && edge_i->a == i) {
          ASSERT(edge_i->b < version_mappings.size);
          const auto& b = version_mappings[edge_i->b];

          if (b.known_reg) {
            ASSERT(b.reg < 32);
            used_regs |= 1u << (u32)b.reg;
          }

          edge_i += 1;
        }

        u32 reg_index = 0;
        if (v.crosses_call) {
          reg_index = convention->num_volatile_registers;
        }

        for (; reg_index < number_of_registers; ++reg_index) {
          u8 r = convention->all_regs_unordered[reg_index];
          ASSERT(r < 32);
          if ((used_regs & (1u << r)) == 0) {
            break;
          }
        }

        //TODO: spill registers to variables
        ASSERT(reg_index < number_of_registers);

        auto reg_id = convention->all_regs_unordered[reg_index];

        used_registers |= (1llu << reg_id);

        v.known_reg = true;
        v.reg = reg_id;
      }
    }
  }

  return VariableMappings{
    used_registers,
    std::move(variables),
    std::move(version_mappings)
  };
}

void emit_merges(X64::Program* program,
                 const ViewArr<const IR::SSAVar>& ir_variables,
                 IR::LocalLabel from,
                 const ViewArr<const ActualTemporary>& from_actuals,
                 const IR::ControlBlock* to_block,
                 const ViewArr<const ActualTemporary>& to_actuals) {
  FOR(to_block->enter_merge, it) {
    const IR::SSAVar& var = ir_variables[it->variable];

    if (ssa_var_in_memory(var)) continue;

    u32 from_index = 0;
    if (from == it->in_versions[0].block) {
      from_index = 0;
    }
    else if (from == it->in_versions[1].block) {
      from_index = 1;
    }
    else {
      INVALID_CODE_PATH("Must be one of the imports");
    }

    const IR::VariableVersion& from_version = it->in_versions[from_index];

    //TODO: need to do intermediates + lifetimes to make this work for in-memory variables
    ASSERT(from_actuals[from_version.temp.index].is_register);
    ASSERT(to_actuals[it->local_temp.index].is_register);

    X64::R v0 = { from_actuals[from_version.temp.index].reg_id };
    X64::R v1 = { to_actuals[it->local_temp.index].reg_id };

    const IR::Format format = var.type.struct_format();
    Helpers::copy_reg_to_reg(program, X64::R{v0}, format, X64::R{v1}, format);
  }
}

static MemoryView pointer_view(X64::R pointer, const Type& base) {
  MemoryView view = {};
  view.rm = X64::memory_rm(pointer.r, 0);
  view.known_alignment = base.structure->alignment;
  view.size = base.size();

  return view;
}

void x64_emit_function(CompilerGlobals* comp, CompilerThread* comp_thread, const IR::Builder* ir, const CallingConvention* convention,
                       Backend::GenericProgram* program_in) {
  TRACING_FUNCTION();

  ASSERT(ir->global_label != IR::NULL_GLOBAL_LABEL);
  ASSERT(ir->control_blocks.size > 0);//means we did nothing

  X64::Program* program = static_cast<X64::Program*>(program_in);

  //TODO: allow variables in registers
  OwnedArr variables_memory_location = new_arr<i32>(ir->variables.size);

  Array<X64::JumpRelocation> jump_relocations = {};

  IR::LocalLabel ret_label = { static_cast<u32>(ir->control_blocks.size) + 1 };

  OwnedArr local_label_real_offsets = new_arr<u32>(ret_label.label);

  u64 total_temporaries = 0;
  FOR(ir->control_blocks, b) {
    total_temporaries += b->temporaries.size;
  }

  bool calls = false;
  u32 call_space_used = 0;
  u64 used_registers = 0;

  Array<X64::R> intermediates = {};
  OwnedArr<RegisterMapping> temporary_mappings = new_arr<RegisterMapping>(total_temporaries);
  OwnedArr<u64> temporary_offsets = new_arr<u64>(ir->control_blocks.size);

  //Resolve the temporaries first
  {
    u64 temporaries_counter = 0;

    for (u32 i = 0; i < ir->control_blocks.size; ++i) {
      temporary_offsets[i] = temporaries_counter;

      const IR::ControlBlock* b = ir->control_blocks.data + i;

      const u8* const bc_start = b->bytecode.begin();
      const u8* const bc_end = b->bytecode.end();

      RegisterResolver resolver = {};
      resolver.current_block = b;
      resolver.sig_struct = ir->signature;
      resolver.this_convention = convention;
      resolver.bytecode_start = bc_start;
      resolver.bytecode_end = bc_end;

      resolver.local_temporaries = const_view_arr(b->temporaries);
      resolver.imports = const_view_arr(b->imports);
      resolver.exports = const_view_arr(b->exports);
      resolver.merges = const_view_arr(b->enter_merge);
      resolver.globals_used = const_view_arr(ir->globals_used);

      resolver.to_map = view_arr(temporary_mappings, temporaries_counter, b->temporaries.size);

      auto mappings = resolve_values(comp, comp_thread, &resolver, convention);
      if (comp_thread->is_panic()) {
        return;
      }

      temporaries_counter += b->temporaries.size;

      if (resolver.has_called) {
        calls = resolver.has_called;
      }

      if (call_space_used < mappings.call_space_needed) {
        call_space_used = mappings.call_space_needed;
      }

      used_registers |= mappings.used_registers;

      intermediates.reserve_extra(mappings.intermediates.size);
      for (usize in = 0; in < mappings.intermediates.size; ++in) {
        const auto& v = mappings.intermediates[in];
        ASSERT(v.known_reg);
        intermediates.insert(X64::R{v.reg});
      }
    }

    ASSERT(temporaries_counter == total_temporaries);
  }

  //Then resolve the variables

  VariableResolver var_resolver = {};
  var_resolver.temporary_offsets = const_view_arr(temporary_offsets);
  var_resolver.blocks = const_view_arr(ir->control_blocks);
  var_resolver.variables = const_view_arr(ir->variables);
  var_resolver.this_convention = convention;

  var_resolver.temporary_mappings = view_arr(temporary_mappings);

  const auto var_mappings = resolve_variables(var_resolver);

  used_registers |= var_mappings.used_registers;

  //Then start emitting code
  Backend::FunctionMetadata func = {};
  func.code_start = program->code_store.current_location().actual_location;

  const u8* const non_volatile_registers = convention->all_regs_unordered + convention->num_volatile_registers;
  u32 num_registers_to_save = 0;
  {
    for (u32 i = 0; i < convention->num_non_volatile_registers; ++i) {
      if ((used_registers & (1llu << non_volatile_registers[i])) > 0) {
        num_registers_to_save += 1;
      }
    }
  }


  const u32 register_save_space = 8 * num_registers_to_save;
  u32 stack_top = register_save_space;//will remove this size from the actual count, here for alignment reasons

  OwnedArr<ActualTemporary> values = new_arr<ActualTemporary>(temporary_mappings.size);

  {
    OwnedArr<u32> variables_offsets = new_arr<u32>(ir->variables.size);

    for (u32 i = 0; i < ir->variables.size; ++i) {
      const auto& v = ir->variables.data[i];
      if (v.type.size() > 8 || v.requirements.has_address()) {
        ASSERT(var_mappings.variable_versions[i].memory);
        stack_top = ceil_to_n(stack_top, v.type.structure->alignment);
        stack_top += v.type.size();
        variables_offsets[i] = stack_top;
      }
      else {
        ASSERT(!var_mappings.variable_versions[i].memory);
      }
    }

    const u32 basic_stack_size = stack_top;
    u64 temporary_counter = 0;

    FOR(ir->control_blocks, blck) {
      u32 block_stack = basic_stack_size;

      for (u32 i = 0; i < blck->temporaries.size; ++i) {
        const auto& mapping = temporary_mappings[i + temporary_counter];
        auto& actual = values[i + temporary_counter];

        if (mapping.map_type == MapType::Internal) {
          ASSERT(mapping.known_reg);
          actual.maybe_intermeidate = false;
          actual.is_register = true;
          actual.reg_id = X64::R{ mapping.reg };
        }
        else if (mapping.map_type == MapType::InternalNoMap) {
          actual.maybe_intermeidate = true;
          actual.is_register = false;
          const IR::SSATemp& t = blck->temporaries.data[i];

          block_stack = ceil_to_n(stack_top, t.type.structure->alignment);
          block_stack += t.type.size();
          actual.stack_offset = block_stack;
        }
      }

      //Map all the imports
      FOR(blck->enter_merge, m) {
        const VariableVersionMap& map = var_mappings.variable_versions[m->variable];
        auto& actual = values[m->local_temp.index + temporary_counter];
        actual.maybe_intermeidate = true;

        if (map.memory) {
          actual.is_register = false;
          actual.stack_offset = variables_offsets[m->variable];
          continue;
        }

        u32 version_index = map.versions_start + m->version;

        ASSERT(var_mappings.version_reg_map[version_index].known_reg);
        u8 reg = var_mappings.version_reg_map[version_index].reg;

        actual.is_register = true;
        actual.reg_id = X64::R{ reg };
      }

      //Map all the imports
      FOR(blck->imports, i) {
        const VariableVersionMap& map = var_mappings.variable_versions[i->variable];
        auto& actual = values[i->local_temp.index + temporary_counter];
        actual.maybe_intermeidate = true;

        if (map.memory) {
          actual.is_register = false;
          actual.stack_offset = variables_offsets[i->variable];
          continue;
        }

        u32 version_index = map.versions_start + i->in_version.version;

        ASSERT(var_mappings.version_reg_map[version_index].known_reg);
        u8 reg = var_mappings.version_reg_map[version_index].reg;

        actual.is_register = true;
        actual.reg_id = X64::R{ reg };
      }

      //Map all the exports
      FOR(blck->exports, e) {
        const VariableVersionMap& map = var_mappings.variable_versions[e->variable];
        auto& actual = values[e->local_temp.index + temporary_counter];
        actual.maybe_intermeidate = true;

        if (map.memory) {
          actual.is_register = false;
          actual.stack_offset = variables_offsets[e->variable];
          continue;
        }

        ASSERT(e->version < map.num_versions);
        ASSERT(map.versions_start + e->version < var_mappings.version_reg_map.size);

        u32 version_index = map.versions_start + e->version;

        ASSERT(var_mappings.version_reg_map[version_index].known_reg);
        u8 reg = var_mappings.version_reg_map[version_index].reg;

        actual.is_register = true;
        actual.reg_id = X64::R{ reg };
      }

      if (block_stack > stack_top) stack_top = block_stack;

      temporary_counter += blck->temporaries.size;
    }

    ASSERT(temporary_counter == total_temporaries);

    stack_top = ceil_to_8(stack_top);
    if (calls) {
      stack_top += call_space_used;
      stack_top = ceil_to_n<u32>(stack_top, 16);//align to call alignment
    }
  }

  stack_top -= register_save_space;

  Selector selector = {};
  selector.intermediates = const_view_arr(intermediates);
  selector.intermediates_counter = 0;
  selector.base_ptr_reg = convention->base_pointer_reg;

  FOR(ir->control_blocks, blck) {
    IR::LocalLabel blck_label = blck->label;

    {
      u64 actual_values_offset = temporary_offsets[blck_label.label - 1];

      selector.local_actual_values = const_view_arr(values, actual_values_offset, blck->temporaries.size);
    }

    selector.local_temporaries = const_view_arr(blck->temporaries);

    const u8* bc = blck->bytecode.begin();
    const u8* const bc_end = blck->bytecode.end();

    {
      u32 relative = relative_offset(func.code_start, program->code_store.total_size);
      local_label_real_offsets[blck->label.label - 1] = relative;
    }

    {
      //Copy all the imports
      const auto& to_actuals = selector.local_actual_values;
      FOR(blck->imports, it) {
        const IR::SSAVar& var = ir->variables.data[it->variable];

        if (ssa_var_in_memory(var)) continue;

        const IR::VariableVersion& from_version = it->in_version;
        u32 from_b_index = from_version.block.label - 1;

        u64 offset = temporary_offsets[from_b_index];
        usize from_num_temps = ir->control_blocks.data[from_b_index].temporaries.size;

        const auto from_actuals = const_view_arr(values, offset, from_num_temps);

        //TODO: need to do intermediates + lifetimes to make this work for in-memory variables
        ASSERT(from_actuals[from_version.temp.index].is_register);
        ASSERT(to_actuals[it->local_temp.index].is_register);

        X64::R v0 = { from_actuals[from_version.temp.index].reg_id };
        X64::R v1 = { to_actuals[it->local_temp.index].reg_id };

        const IR::Format format = var.type.struct_format();
        Helpers::copy_reg_to_reg(program, X64::R{v0}, format, X64::R{v1}, format);
      }
    }

    while (bc < bc_end) {
      u8 op_byte = *bc;
      IR::OpCode op = static_cast<IR::OpCode>(op_byte);

      switch (op) {
        case IR::OpCode::BreakPoint: {
            IR::Types::BreakPoint bp;
            bc = IR::Read::BreakPoint(bc, bc_end, bp);

            //TODO: do breakpoints

            break;
          }
        case IR::OpCode::Set: {
            IR::Types::Set set;
            bc = IR::Read::Set(bc, bc_end, set);

            X64Value to = selector.get_val(set.to);
            ASSERT(to.t.size() == set.data.size);

            ASSERT(to.t.struct_format() != IR::Format::opaque);

            switch (to.value_type) {
              case ValueType::Register: {
                  Helpers::load_const_to_reg(program, to.t.struct_format(), to.reg, set.data);
                  break;
                }
              case ValueType::Memory: {
                  Helpers::load_const_to_mem(program, to.t.struct_format(), to.mem, set.data);
                  break;
                }
            }

            break;
          }
        case IR::OpCode::SetStore: {
            IR::Types::SetStore set;
            bc = IR::Read::SetStore(bc, bc_end, set);

            X64Value to = selector.get_val(set.to);

            ASSERT(to.t.struct_type() == STRUCTURE_TYPE::POINTER);
            const auto* pt = to.t.unchecked_base<PointerStructure>();
            ASSERT(pt->base.size() == set.data.size);

            switch (to.value_type) {
              case ValueType::Register: {
                  X64::R to_r = to.reg;

                  if (to.expects_intermeidate) {
                    X64::R _temp = selector.get_next_intermediate_reg();
                  }

                  MemoryView view = pointer_view(to.reg, pt->base);

                  Helpers::load_const_to_mem(program, pt->base.struct_format(), view, set.data);
                  break;
                }
              case ValueType::Memory: {
                  X64::R temp = selector.get_next_intermediate_reg();
                  const auto ptr_format = to.t.struct_format();
                  Helpers::copy_mem_to_reg(program, to.mem, ptr_format, temp, ptr_format);

                  MemoryView view = pointer_view(temp, pt->base);

                  Helpers::load_const_to_mem(program, pt->base.struct_format(), view, set.data);
                  break;
                }
            }

            break;
          }
        case IR::OpCode::Copy: {
            IR::Types::Copy copy;
            bc = IR::Read::Copy(bc, bc_end, copy);

            X64Value from = selector.get_val(copy.from);
            X64Value to = selector.get_val(copy.to);

            switch (to.value_type) {
              case ValueType::Register: {
                  switch (from.value_type) {
                    case ValueType::Register: {
                        if (to.expects_intermeidate && from.expects_intermeidate) {
                          X64::R _temp = selector.get_next_intermediate_reg();
                        }

                        Helpers::copy_reg_to_reg(program,
                                                 from.reg, from.t.struct_format(),
                                                 to.reg, to.t.struct_format());
                        break;
                      }
                    case ValueType::Memory: {
                        Helpers::copy_mem_to_reg(program,
                                                 from.mem, from.t.struct_format(),
                                                 to.reg, to.t.struct_format());
                        break;
                      }
                  }
                  break;
                }
              case ValueType::Memory: {
                  switch (from.value_type) {
                    case ValueType::Register: {
                        Helpers::copy_reg_to_mem(program,
                                                 from.reg, from.t.struct_format(),
                                                 to.mem, to.t.struct_format());
                        break;
                      }
                    case ValueType::Memory: {
                        X64::R temp = selector.get_next_intermediate_reg();

                        Helpers::copy_mem_to_mem_small(program,
                                                       from.mem, from.t.struct_format(),
                                                       to.mem, to.t.struct_format(),
                                                       temp);
                        break;
                      }
                  }
                  break;
                }
            }

            break;
          }
        case IR::OpCode::CopyLoad: {
            IR::Types::CopyLoad copy;
            bc = IR::Read::CopyLoad(bc, bc_end, copy);

            X64Value from = selector.get_val(copy.from);
            ASSERT(from.t.struct_type() == STRUCTURE_TYPE::POINTER);
            const auto* pt = from.t.unchecked_base<PointerStructure>();
            const IR::Format from_ptr_format = from.t.struct_format();
            const IR::Format from_format = pt->base.struct_format();

            X64Value to = selector.get_val(copy.to);
            const IR::Format to_format = to.t.struct_format();

            MemoryView view;
            switch (from.value_type) {
              case ValueType::Register: {
                  if (from.expects_intermeidate) {
                    X64::R _temp = selector.get_next_intermediate_reg();
                  }

                  view = pointer_view(from.reg, pt->base);
                  break;
                }
              case ValueType::Memory: {
                  X64::R temp = selector.get_next_intermediate_reg();

                  Helpers::copy_mem_to_reg(program, from.mem, from_ptr_format, temp, from_ptr_format);

                  view = pointer_view(temp, pt->base);
                  break;
                }
            }

            switch (to.value_type) {
              case ValueType::Register: {
                  if (to.expects_intermeidate) {
                    X64::R _temp = selector.get_next_intermediate_reg();
                  }

                  Helpers::copy_mem_to_reg(program,
                                           view, from_format,
                                           to.reg, to_format);
                  break;

                }
              case ValueType::Memory: {
                  X64::R temp = selector.get_next_intermediate_reg();

                  Helpers::copy_mem_to_mem_small(program,
                                                 view, from_format,
                                                 to.mem, to_format,
                                                 temp);
                  break;
                }
            }

            break;
          }
        case IR::OpCode::CopyStore: {
            IR::Types::CopyStore copy;
            bc = IR::Read::CopyStore(bc, bc_end, copy);

            X64Value from = selector.get_val(copy.from);
            const IR::Format from_format = from.t.struct_format();

            X64Value to = selector.get_val(copy.to);
            ASSERT(to.t.struct_type() == STRUCTURE_TYPE::POINTER);
            const auto* pt = to.t.unchecked_base<PointerStructure>();
            const IR::Format to_ptr_format = to.t.struct_format();
            const IR::Format to_format = pt->base.struct_format();

            switch (to.value_type) {
              case ValueType::Register: {
                  switch (from.value_type) {
                    case ValueType::Register: {
                        if (from.expects_intermeidate) {
                          X64::R _temp = selector.get_next_intermediate_reg();
                        }
                        if (to.expects_intermeidate) {
                          X64::R _temp = selector.get_next_intermediate_reg();
                        }

                        MemoryView view = pointer_view(to.reg, pt->base);

                        Helpers::copy_reg_to_mem(program,
                                                 from.reg, from_format,
                                                 view, to_format);
                        break;
                      }
                    case ValueType::Memory: {
                        MemoryView view = pointer_view(to.reg, pt->base);

                        X64::R temp = selector.get_next_intermediate_reg();

                        if (to.expects_intermeidate) {
                          X64::R _temp = selector.get_next_intermediate_reg();
                        }

                        Helpers::copy_mem_to_mem_small(program,
                                                       from.mem, from_format,
                                                       view, to_format, temp);
                        break;
                      }
                  }
                  break;
                }
              case ValueType::Memory: {
                  switch (from.value_type) {
                    case ValueType::Register: {
                        if (from.expects_intermeidate) {
                          X64::R _temp = selector.get_next_intermediate_reg();
                        }

                        X64::R temp_ptr = selector.get_next_intermediate_reg();
                        Helpers::copy_mem_to_reg(program, to.mem, to_ptr_format, temp_ptr, to_ptr_format);
                        MemoryView view = pointer_view(temp_ptr, pt->base);

                        Helpers::copy_reg_to_mem(program,
                                                 from.reg, from_format,
                                                 view, to_format);
                        break;
                      }
                    case ValueType::Memory: {
                        X64::R temp = selector.get_next_intermediate_reg();

                        X64::R temp_ptr = selector.get_next_intermediate_reg();
                        Helpers::copy_mem_to_reg(program, to.mem, to_ptr_format, temp_ptr, to_ptr_format);
                        MemoryView view = pointer_view(temp_ptr, pt->base);


                        Helpers::copy_mem_to_mem_small(program,
                                                       from.mem, from_format,
                                                       view, to_format,
                                                       temp);
                        break;
                      }
                  }
                  break;
                }
            }

            break;
          }
        case IR::OpCode::CopyLoadStore: {
            IR::Types::CopyLoadStore copy;
            bc = IR::Read::CopyLoadStore(bc, bc_end, copy);

            X64Value from = selector.get_val(copy.from);
            ASSERT(from.t.struct_type() == STRUCTURE_TYPE::POINTER);
            const auto* pt_f = from.t.unchecked_base<PointerStructure>();
            const IR::Format from_ptr_format = from.t.struct_format();
            const IR::Format from_format = pt_f->base.struct_format();

            X64Value to = selector.get_val(copy.to);
            ASSERT(to.t.struct_type() == STRUCTURE_TYPE::POINTER);
            const auto* pt_t = to.t.unchecked_base<PointerStructure>();
            const IR::Format to_ptr_format = to.t.struct_format();
            const IR::Format to_format = pt_t->base.struct_format();

            X64::R temp = selector.get_next_intermediate_reg();

            MemoryView from_mem = {};
            switch (from.value_type) {
              case ValueType::Register: {
                  if (from.expects_intermeidate) {
                    X64::R _temp = selector.get_next_intermediate_reg();
                  }

                  from_mem = pointer_view(from.reg, pt_f->base);
                  break;
                }
              case ValueType::Memory: {
                  X64::R temp = selector.get_next_intermediate_reg();
                  Helpers::copy_mem_to_reg(program, from.mem, from_ptr_format, temp, from_ptr_format);
                  from_mem = pointer_view(temp, pt_f->base);
                  break;
                }
            }

            MemoryView to_mem = {};
            switch (to.value_type) {
              case ValueType::Register: {
                  if (to.expects_intermeidate) {
                    X64::R _temp = selector.get_next_intermediate_reg();
                  }

                  to_mem = pointer_view(to.reg, pt_t->base);
                  break;
                }
              case ValueType::Memory: {
                  X64::R m_temp = selector.get_next_intermediate_reg();
                  Helpers::copy_mem_to_reg(program, to.mem, to_ptr_format, m_temp, to_ptr_format);
                  to_mem = pointer_view(temp, pt_t->base);
                  break;
                }
            }

            Helpers::copy_mem_to_mem_small(program,
                                           from_mem, from_format,
                                           to_mem, to_format, temp);
            break;
          }
        case IR::OpCode::AddrOf: {
            IR::Types::AddrOf addr;
            bc = IR::Read::AddrOf(bc, bc_end, addr);

            X64Value f = selector.get_val(addr.from);
            X64Value t = selector.get_val(addr.to);
            ASSERT(t.t.struct_format() == IR::Format::uint64);

            ASSERT(f.value_type == ValueType::Memory);

            switch (t.value_type) {
              case ValueType::Register: {
                  if (t.expects_intermeidate) {
                    X64::R _temp = selector.get_next_intermediate_reg();
                  }

                  Helpers::copy_address_to_reg(program, f.mem, t.reg);
                } break;
              case ValueType::Memory: {
                  X64::R temp = selector.get_next_intermediate_reg();
                  Helpers::copy_address_to_reg(program, f.mem, temp);
                  Helpers::copy_reg_to_mem(program, temp, IR::Format::uint64, t.mem, IR::Format::uint64);
                } break;
            }

            break;
          }
        case IR::OpCode::AddrOfLoad: {
            IR::Types::AddrOfLoad addr;
            bc = IR::Read::AddrOfLoad(bc, bc_end, addr);

            X64Value f = selector.get_val(addr.from);
            ASSERT(f.t.struct_type() == STRUCTURE_TYPE::POINTER);
            const auto* pt_f = f.t.unchecked_base<PointerStructure>();
            const IR::Format from_ptr_format = f.t.struct_format();
            const IR::Format from_format = pt_f->base.struct_format();

            X64Value t = selector.get_val(addr.to);
            ASSERT(t.t.struct_format() == IR::Format::uint64);

            MemoryView from_mem = {};
            switch (f.value_type) {
              case ValueType::Register: {
                  if (f.expects_intermeidate) {
                    X64::R _temp = selector.get_next_intermediate_reg();
                  }
                  from_mem = pointer_view(f.reg, pt_f->base);
                  break;
                }
              case ValueType::Memory: {
                  X64::R temp = selector.get_next_intermediate_reg();
                  Helpers::copy_mem_to_reg(program, f.mem, from_ptr_format, temp, from_ptr_format);
                  from_mem = pointer_view(temp, pt_f->base);
                  break;
                }
            }

            switch (t.value_type) {
              case ValueType::Register: {
                  if (t.expects_intermeidate) {
                    X64::R _temp = selector.get_next_intermediate_reg();
                  }

                  Helpers::copy_address_to_reg(program, f.mem, t.reg);
                } break;
              case ValueType::Memory: {
                  X64::R temp = selector.get_next_intermediate_reg();
                  Helpers::copy_address_to_reg(program, f.mem, temp);
                  Helpers::copy_reg_to_mem(program, temp, IR::Format::uint64, t.mem, IR::Format::uint64);
                } break;
            }

            break;
          }
        case IR::OpCode::AddrOfGlobal: {
            IR::Types::AddrOfGlobal addr;
            bc = IR::Read::AddrOfGlobal(bc, bc_end, addr);

            IR::GlobalReference global_r = ir->globals_used.data[addr.im32];

            X64Value g = selector.get_val(addr.val);
            ASSERT(g.t.struct_format() == IR::Format::uint64);

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
        case IR::OpCode::StartFunc: {
            IR::Types::StartFunc start_func;
            bc = IR::Read::StartFunc(bc, bc_end, start_func);
            ASSERT(start_func.values == nullptr);

            for (u32 i = 0; i < convention->num_non_volatile_registers; ++i) {
              u8 reg = non_volatile_registers[i];
              if ((used_registers & (1llu << reg)) > 0) {
                X64::Instruction save = {};
                X64::push(save, X64::R{reg});
                X64::append_instruction(program, save);
              }
            }

            if (stack_top > 0) {

              X64::R rbp = X64::R{ convention->base_pointer_reg };
              X64::R rsp = X64::R{ convention->stack_pointer_reg };

              X64::Instruction save = {};
              X64::push(save, rbp);
              X64::Instruction copy = {};
              X64::mov(copy, X64::R64{rsp}, X64::R64{rbp});

              X64::append_instruction(program, save);
              X64::append_instruction(program, copy);

              X64::Instruction move = {};
              X64::sub(move, X64::R64{rsp}, X64::IMM32{stack_top});
              X64::append_instruction(program, move);
            }
            else {
              ASSERT(call_space_used == 0);
            }

            ASSERT(ir->signature->parameter_types.size == start_func.n_values);

            for (usize i = 0; i < start_func.n_values; ++i) {
              IR::V_ARG arg;
              bc += IR::deserialize(bc, bc_end - bc, arg);

              const Type& param = ir->signature->parameter_types[i];
              ASSERT(arg.size <= 8);//TODO: parameters of different types
              ASSERT(arg.size == param.size());
              ASSERT(arg.offset == 0);//temp - need to fix this


              X64::R r = selector.get_next_intermediate_reg();

              X64Value to = selector.get_val(arg);
              IR::Format arg_format = to.t.struct_format();
              ASSERT(param == to.t);

              switch (to.value_type) {
                case ValueType::Register: {
                    Helpers::copy_reg_to_reg(program, r, arg_format, to.reg, arg_format);
                    break;
                  }
                case ValueType::Memory: {
                    Helpers::copy_reg_to_mem(program, r, arg_format, to.mem, arg_format);
                    break;
                  }
              }
            }

            break;
          }
        case IR::OpCode::Call: {
            IR::Types::Call call;
            bc = IR::Read::Call(bc, bc_end, call);
            ASSERT(call.values == nullptr);

            const SignatureStructure* sig_struct = comp->get_label_signature(call.label);

            const bool has_return = sig_struct->return_type != comp_thread->builtin_types->t_void;

            u32 call_needed = convention->shadow_space_size;

            for (usize i = 0; i < ((usize)call.n_values - has_return); ++i) {
              IR::V_ARG arg;
              bc += IR::deserialize(bc, bc_end - bc, arg);

              X64Value arg_v = selector.get_val(arg);

              IR::Format f = arg_v.t.struct_format();

              if (i < convention->num_parameter_registers) {
                X64::R arg_reg = selector.get_next_intermediate_reg();

                switch (arg_v.value_type) {
                  case ValueType::Register: {
                      Helpers::copy_reg_to_reg(program, arg_v.reg, f, arg_reg, f);
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

                const Type& t = sig_struct->parameter_types[i];
                arg_mem.known_alignment = t.structure->alignment;
                arg_mem.size = t.size();
                arg_mem.rm = X64::memory_rm(convention->base_pointer_reg, -static_cast<i32>(stack_top - (i * 8)));

                switch (arg_v.value_type) {
                  case ValueType::Register: {
                      if (arg_v.expects_intermeidate) {
                        X64::R _temp = selector.get_next_intermediate_reg();
                      }

                      Helpers::copy_reg_to_mem(program, arg_v.reg, f, arg_mem, f);
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
              IR::V_ARG ret;
              bc += IR::deserialize(bc, bc_end - bc, ret);

              X64::R ret_reg = selector.get_next_intermediate_reg();
              X64Value ret_v = selector.get_val(ret);

              IR::Format ret_format = ret_v.t.struct_format();

              switch (ret_v.value_type) {
                case ValueType::Register: {
                    Helpers::copy_reg_to_reg(program, ret_reg, ret_format, ret_v.reg, ret_format);
                    break;
                  }
                case ValueType::Memory: {
                    Helpers::copy_reg_to_mem(program, ret_reg, ret_format, ret_v.mem, ret_format);
                    break;
                  }
              }
            }
            break;
          }

#define EMIT_BIN_OP(name, helper)\
        case IR::OpCode:: name: {\
            IR::Types:: name bin_op;\
            bc = IR::Read:: name (bc, bc_end, bin_op);\
            X64Value left = selector.get_val(bin_op.left);\
            X64Value right = selector.get_val(bin_op.right);\
            X64Value to = selector.get_val(bin_op.to);\
            IR::Format left_format = left.t.struct_format();\
            IR::Format right_format = right.t.struct_format();\
            IR::Format to_format = to.t.struct_format();\
            /*always a left register*/\
            X64::R left_reg = selector.get_next_intermediate_reg();\
            switch (left.value_type) {\
              case ValueType::Memory: Helpers::copy_mem_to_reg(program, left.mem, left_format, left_reg, left_format); break;\
              case ValueType::Register: Helpers::copy_reg_to_reg(program, left.reg, left_format, left_reg, left_format); break;\
            }\
            X64::R right_reg;\
            switch (right.value_type) {\
              case ValueType::Memory: {\
                  right_reg = selector.get_next_intermediate_reg();\
                  Helpers::copy_mem_to_reg(program, right.mem, right_format, right_reg, right_format); break;\
                }\
              case ValueType::Register: {\
                  if(right.expects_intermeidate) { X64::R _temp = selector.get_next_intermediate_reg(); }\
                  right_reg = right.reg; break;\
                }\
            }\
            const IR::Format new_format = helper (program, left_reg, left_format, right_reg, right_format);\
            switch (to.value_type) {\
              case ValueType::Register: Helpers::copy_reg_to_reg(program, left_reg, new_format, to.reg, to_format); break;\
              case ValueType::Memory: Helpers::copy_reg_to_mem(program, left_reg, new_format, to.mem, to_format); break;\
            }\
            break;\
          }\

                             EMIT_BIN_OP(Add, Helpers::emit_add);
                             EMIT_BIN_OP(Sub, Helpers::emit_sub);
                             EMIT_BIN_OP(Mul, Helpers::emit_mul);
                             EMIT_BIN_OP(Div, Helpers::emit_div);
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

    const auto jump_and_merge = [&](IR::LocalLabel l) {
      ASSERT(l != IR::NULL_LOCAL_LABEL);
      ASSERT(l.label - 1 < ir->control_blocks.size);

      //May need to merge or import some variables at this point
      {
        const auto& next_block = ir->control_blocks.data[l.label - 1];
        const auto& next_actuals = const_view_arr(values, temporary_offsets[l.label - 1], next_block.temporaries.size);
        const auto& curr_actuals = const_view_arr(values, temporary_offsets[blck_label.label - 1], blck->temporaries.size);

        emit_merges(program, const_view_arr(ir->variables),
                    blck_label, curr_actuals,
                    &next_block, next_actuals);
      }

      jump_relocations.insert(X64::JumpRelocation{
        relative_offset(func.code_start, program->code_store.total_size + X64::JUMP_NEAR_OFFSET),
          l
      });

      X64::Instruction j = {};
      X64::jump_near(j, 0);

      X64::append_instruction(program, j);
    };

    //TODO: remove some jumps
    switch (blck->cf_type) {
      case IR::ControlFlowType::Start: {
          jump_and_merge(blck->cf_start.child);
          break;
        }

      case IR::ControlFlowType::Return: {
          X64Value from = selector.get_val(IR::v_arg(blck->cf_return.val, 0, ir->signature->return_type));
          IR::Format format = from.t.struct_format();
          ASSERT(format != IR::Format::opaque);//temp

          X64::R ret_reg = selector.get_next_intermediate_reg();
          ASSERT(ret_reg.r == convention->return_register);

          switch (from.value_type) {
            case ValueType::Register: {
                Helpers::copy_reg_to_reg(program, from.reg, format, ret_reg, format);
                break;
              }
            case ValueType::Memory: {
                Helpers::copy_mem_to_reg(program, from.mem, format, ret_reg, format);
                break;
              }
          }

          jump_relocations.insert(X64::JumpRelocation{
            relative_offset(func.code_start, program->code_store.total_size + X64::JUMP_NEAR_OFFSET),
              ret_label
          });

          X64::Instruction j = {};
          X64::jump_near(j, 0);

          X64::append_instruction(program, j);
          break;
        }
      case IR::ControlFlowType::End: {
          jump_relocations.insert(X64::JumpRelocation{
            relative_offset(func.code_start, program->code_store.total_size + X64::JUMP_NEAR_OFFSET),
              ret_label
          });

          X64::Instruction j = {};
          X64::jump_near(j, 0);

          X64::append_instruction(program, j);
          break;
        }

      case IR::ControlFlowType::Inline: {
          jump_and_merge(blck->cf_inline.child);
          break;
        }
      case IR::ControlFlowType::Merge: {
          jump_and_merge(blck->cf_merge.child);
          break;
        }

      case IR::ControlFlowType::Split: {
          X64Value val = selector.get_val(IR::v_arg(blck->cf_split.condition, 0, comp->builtin_types->t_bool));

          ASSERT(val.t.struct_format() == IR::Format::uint8);

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
            }

            X64::append_instruction(program, compare);
          }

          //No merging and splitting, must be separate blocks
          ASSERT(ir->control_blocks.data[blck->cf_split.false_branch.label - 1].enter_merge.size == 0);
          ASSERT(ir->control_blocks.data[blck->cf_split.true_branch.label - 1].enter_merge.size == 0);

          {
            jump_relocations.insert(X64::JumpRelocation{
              relative_offset(func.code_start, program->code_store.total_size + X64::JUMP_CONDITION_OFFSET),
                blck->cf_split.false_branch
            });

            X64::Instruction j_else = {};
            X64::jump_equal(j_else, 0);

            X64::append_instruction(program, j_else);
          }

          {
            jump_relocations.insert(X64::JumpRelocation{
              relative_offset(func.code_start, program->code_store.total_size + X64::JUMP_NEAR_OFFSET),
                blck->cf_split.true_branch
            });

            //TODO: remove unnecessary jumps
            X64::Instruction j_if = {};
            X64::jump_near(j_if, 0);

            X64::append_instruction(program, j_if);
          }
          break;
        }
    }
  }

  ASSERT(selector.intermediates_counter == selector.intermediates.size);

  {
    u32 relative = relative_offset(func.code_start, program->code_store.total_size);
    local_label_real_offsets[ret_label.label - 1] = relative;

    if (stack_top > 0) {
      X64::R rbp = X64::R{ convention->base_pointer_reg };
      X64::R rsp = X64::R{ convention->stack_pointer_reg };

      X64::Instruction copy = {};
      X64::mov(copy, X64::R64{rbp}, X64::R64{rsp});
      X64::Instruction load = {};
      X64::pop(load, rbp);

      X64::append_instruction(program, copy);
      X64::append_instruction(program, load);
    }

    for (u32 i = 0; i < convention->num_non_volatile_registers; ++i) {
      u8 reg = non_volatile_registers[convention->num_non_volatile_registers - (i + 1)];//reverse order
      if ((used_registers & (1llu << reg)) > 0) {
        X64::Instruction save = {};
        X64::pop(save, X64::R{reg});
        X64::append_instruction(program, save);
      }
    }

    X64::Instruction r = {};
    X64::ret(r);
    X64::append_instruction(program, r);
  }

  //We're done adding new instructions
  func.code_size = program->code_store.total_size - func.code_start;

  if (program->functions.size <= ir->global_label.label) {
    usize to_append = ir->global_label.label - program->functions.size;
    program->functions.insert_uninit(to_append);
  }

  program->functions.data[ir->global_label.label - 1] = func;


  //Do the local relocations (global relocations come later)

  auto code_itr = program->code_store.start();
  code_itr.jump_to(func.code_start);//TODO: save this better so we don't need to jump around so much

  auto start_itr = code_itr;//save for later

  FOR(jump_relocations, it) {
    usize immediate_location = func.code_start + (usize)it->offset_to_immediate;

    ASSERT(code_itr.actual_location < immediate_location);
    ASSERT(immediate_location + 4 < program->code_store.total_size);

    code_itr.jump_to(immediate_location);

    ASSERT(it->jump_to.label <= local_label_real_offsets.size);

    u32 label_offset = local_label_real_offsets[it->jump_to.label - 1];
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

  void print_x86_64(Backend::DataBucketIterator start, const Backend::DataBucketIterator end);
  if (comp_thread->print_options.finished_mc) {
    print_x86_64(start_itr, program->code_store.current_location());
  }
}

struct RegisterNames {
  OwnedArr<char> r;
  OwnedArr<char> rm;
};

static const char* b8_no_rex_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "al";
    case 1: return "cl";
    case 2: return "dl";
    case 3: return "bl";
    case 4: return "ah";
    case 5: return "ch";
    case 6: return "dh";
    case 7: return "bh";
  }

  return "INVALID REGISTER";
}

static const char* b8_rex_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "al";
    case 1: return "cl";
    case 2: return "dl";
    case 3: return "bl";
    case 4: return "spl";
    case 5: return "bpl";
    case 6: return "sil";
    case 7: return "dil";
    case 8: return "r8b";
    case 9: return "r9b";
    case 10: return "r10b";
    case 11: return "r11b";
    case 12: return "r12b";
    case 13: return "r13b";
    case 14: return "r14b";
    case 15: return "r15b";
  }

  return "INVALID REGISTER";
}

static const char* b16_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "ax";
    case 1: return "cx";
    case 2: return "dx";
    case 3: return "bx";
    case 4: return "sp";
    case 5: return "bp";
    case 6: return "si";
    case 7: return "di";
    case 8: return "r8w";
    case 9: return "r9w";
    case 10: return "r10w";
    case 11: return "r11w";
    case 12: return "r12w";
    case 13: return "r13w";
    case 14: return "r14w";
    case 15: return "r15w";
  }

  return "INVALID REGISTER";
}

static const char* b32_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "eax";
    case 1: return "ecx";
    case 2: return "edx";
    case 3: return "ebx";
    case 4: return "esp";
    case 5: return "ebp";
    case 6: return "esi";
    case 7: return "edi";
    case 8: return "r8d";
    case 9: return "r9d";
    case 10: return "r10d";
    case 11: return "r11d";
    case 12: return "r12d";
    case 13: return "r13d";
    case 14: return "r14d";
    case 15: return "r15d";
  }

  return "INVALID REGISTER";
}

static const char* b64_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "rax";
    case 1: return "rcx";
    case 2: return "rdx";
    case 3: return "rbx";
    case 4: return "rsp";
    case 5: return "rbp";
    case 6: return "rsi";
    case 7: return "rdi";
    case 8: return "r8";
    case 9: return "r9";
    case 10: return "r10";
    case 11: return "r11";
    case 12: return "r12";
    case 13: return "r13";
    case 14: return "r14";
    case 15: return "r15";
  }

  return "INVALID REGISTER";
}

struct x86PrintOptions {
  FUNCTION_PTR<const char*, uint8_t> r_name = nullptr;
  FUNCTION_PTR<const char*, uint8_t> rm_name = nullptr;
  FUNCTION_PTR<const char*, uint8_t> mem_r_name = nullptr;
  const char* mem_size = nullptr;
};

u16 x16_from_itr(Backend::DataBucketIterator* itr) {
  u8 bytes[2]{
    itr->read_byte(),
    itr->read_byte(),
  };

  return x16_from_bytes(bytes);
}

u32 x32_from_itr(Backend::DataBucketIterator* itr) {
  u8 bytes[4]{
    itr->read_byte(),
    itr->read_byte(),
    itr->read_byte(),
    itr->read_byte(),
  };

  return x32_from_bytes(bytes);
}

u64 x64_from_itr(Backend::DataBucketIterator* itr) {
  u8 bytes[8]{
    itr->read_byte(),
    itr->read_byte(),
    itr->read_byte(),
    itr->read_byte(),
    itr->read_byte(),
    itr->read_byte(),
    itr->read_byte(),
    itr->read_byte(),
  };

  return x64_from_bytes(bytes);
}

static OwnedArr<char> rm_reg_string(x86PrintOptions* const p_opts,
                                    uint8_t rex, uint8_t modrm, Backend::DataBucketIterator* rest) {
  uint8_t address_mode = (modrm & 0b11'000000) >> 6;
  uint8_t rm = modrm & X64::MODRM_RM_MASK;

  if ((modrm & 0b11'000000) == 0b11'000000) {
    rm |= ((rex & X64::REX_B) << X64::REX_B_SHIFT);

    return format("{}", p_opts->rm_name(rm));
  }

  //from now on use x86_64_reg_name_from_num for mem
  //Memory is always 64 bit addressed

  switch (rm) {
    case X64::rsp.REG: {
        //SIB byte time
        const uint8_t sib = rest->read_byte();

        const uint8_t scale = 1 << ((sib & 0b11'000'000) >> 6);
        const uint8_t index = ((rex & X64::REX_X) << 2) | ((sib & X64::SIB_INDEX_MASK) >> 3);
        const uint8_t base = ((rex & X64::REX_B) << 3) | ((sib & X64::SIB_BASE_MASK));

        const bool INDEX_RSP = index == X64::rsp.REG;
        const bool BASE_RBP = (base & 0b111) == 0b101;

        switch (address_mode) {
          case 0b00: {
              if (INDEX_RSP && BASE_RBP) {
                int32_t disp = x32_from_itr(rest);

                return format("{} [{}]", p_opts->mem_size, disp);
              }
              else if (INDEX_RSP) {
                return format("{} [{}]", p_opts->mem_size, p_opts->mem_r_name(base));
              }
              else if (BASE_RBP) {
                int32_t disp = x32_from_itr(rest);

                char sign = disp >= 0 ? '+' : '-';
                if (scale == 1) {
                  return format("{} [{} {} {}]", p_opts->mem_size, p_opts->mem_r_name(index), sign, absolute(disp));
                }
                else {
                  return format("{} [({} * {}) {} {}]", p_opts->mem_size, p_opts->mem_r_name(index), scale, sign, absolute(disp));
                }
              }
              else if (scale == 1) {
                return format("{} [{} + {}]",
                              p_opts->mem_size,
                              p_opts->mem_r_name(base),
                              p_opts->mem_r_name(index));
              }
              else {
                return format("{} [{} + ({} * {})]",
                              p_opts->mem_size,
                              p_opts->mem_r_name(base),
                              p_opts->mem_r_name(index),
                              scale);
              }
            }
          case 0b01: {
              const int8_t disp = rest->read_byte();

              char sign = disp >= 0 ? '+' : '-';

              if (INDEX_RSP) {
                return format("{} [{} {} {}]",
                              p_opts->mem_size,
                              p_opts->mem_r_name(base),
                              sign, absolute(disp));
              }
              else {
                if (scale == 1) {
                  return format("{} [{} {} {} + {}]",
                                p_opts->mem_size,
                                p_opts->mem_r_name(base),
                                sign, absolute(disp),
                                p_opts->mem_r_name(index));
                }
                else {
                  return format("{} [{} {} {} + ({} * {})]",
                                p_opts->mem_size,
                                p_opts->mem_r_name(base),
                                sign, absolute(disp),
                                p_opts->mem_r_name(index), scale);
                }
              }
            }
          case 0b10: {
              int32_t disp = x32_from_itr(rest);

              char sign = disp >= 0 ? '+' : '-';

              if (INDEX_RSP) {
                return format("{} [{} {} {}]",
                              p_opts->mem_size,
                              p_opts->mem_r_name(base),
                              sign, absolute(disp));
              }
              else {
                if (scale == 1) {
                  return format("{} [{} + {} {} {}]",
                                p_opts->mem_size,
                                p_opts->mem_r_name(base),
                                p_opts->mem_r_name(index),
                                sign, absolute(disp));
                }
                else {
                  return format("{} [{} + ({} * {}) {} {}]",
                                p_opts->mem_size,
                                p_opts->mem_r_name(base),
                                p_opts->mem_r_name(index), scale,
                                sign, absolute(disp));
                }
              }
            }
        }

        INVALID_CODE_PATH("Internal error. Unrecognised assembly code register format");
      }
    case X64::rbp.REG: {
        if (address_mode == 0b00) {
          int32_t disp = x32_from_itr(rest);

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
              return format("{} [{}]", p_opts->mem_size, p_opts->mem_r_name(rm));
            }
          case 0b01: {
              int8_t disp = rest->read_byte();

              char sign = disp >= 0 ? '+' : '-';

              return format("{} [{} {} {}]", p_opts->mem_size, p_opts->mem_r_name(rm), sign, absolute(disp));
            }
          case 0b10: {
              int32_t disp = x32_from_itr(rest);

              char sign = disp >= 0 ? '+' : '-';

              return format("{} [{} {} {}]", p_opts->mem_size, p_opts->mem_r_name(rm), sign, absolute(disp));
            }
        }

        INVALID_CODE_PATH("Internal error. Unrecognised assembly code register format");
      }
  }

  INVALID_CODE_PATH("Internal error. Unrecognised assembly code register format");
  return {};
}

static OwnedArr<char> r_reg_string(x86PrintOptions* p_opts,
                                   uint8_t rex, uint8_t modrm) {
  uint8_t r = ((rex & X64::REX_R) << X64::REX_R_SHIFT)
    | ((modrm & X64::MODRM_REG_MASK) >> X64::MODRM_REG_SHIFT);

  return format("{}", p_opts->r_name(r));
}


static RegisterNames register_names(x86PrintOptions* p_opts,
                                    uint8_t rex, uint8_t modrm, Backend::DataBucketIterator* rest) {
  return {
    r_reg_string(p_opts, rex, modrm),
    rm_reg_string(p_opts, rex, modrm, rest)
  };
}

static void load_8_sizes(x86PrintOptions* ops, bool rex, bool short_address) {
  if (short_address) {
    ops->mem_r_name = b32_reg_name;
  }
  else {
    ops->mem_r_name = b64_reg_name;
  }

  if (rex) {
    ops->r_name = b8_rex_reg_name;
    ops->rm_name = b8_rex_reg_name;
    ops->mem_size = "BYTE PTR";
  }
  else {
    ops->r_name = b8_no_rex_reg_name;
    ops->rm_name = b8_no_rex_reg_name;
    ops->mem_size = "BYTE PTR";
  }
}

static void load_default_sizes(x86PrintOptions* ops, bool rex_w, bool short_address, bool short_operand) {
  if (short_address) {
    ops->mem_r_name = b32_reg_name;
  }
  else {
    ops->mem_r_name = b64_reg_name;
  }

  if (rex_w) {
    ops->r_name = b64_reg_name;
    ops->rm_name = b64_reg_name;
    ops->mem_size = "QWORD PTR";
  }
  else {
    if (short_operand) {
      ops->r_name = b16_reg_name;
      ops->rm_name = b16_reg_name;
      ops->mem_size = "WORD PTR";
    }
    else {
      ops->r_name = b32_reg_name;
      ops->rm_name = b32_reg_name;
      ops->mem_size = "DWORD PTR";
    }
  }
}

void print_x86_64(Backend::DataBucketIterator start, const Backend::DataBucketIterator end) {
  IO_Single::lock();
  DEFER() { IO_Single::unlock(); };

  {
    IO_Single::print("raw = ");

    Backend::DataBucketIterator copy = start;
    while (copy < end) {
      format_print_ST("{} ", PrintHexByte{ copy.read_byte() });
    }

    IO_Single::print("\n");
  }

  x86PrintOptions p_opts = {};

  usize start_idx = start.actual_location;

  while (start < end) {
    printf("0x%-4llx: ", start.actual_location - start_idx);

    u8 op = start.read_byte();

    bool short_operand = op == 0x66;
    if (short_operand) {
      op = start.read_byte();
    }

    bool short_address = op == 0x67;
    if (short_address) {
      op = start.read_byte();
    }

    //check again as it might have been second
    if (!short_operand) {
      short_operand = op == 0x66;
      if (short_operand) {
        op = start.read_byte();
      }
    }

    uint8_t maybe_rex = 0;
    bool rex = false;
    if ((op & 0b1111'0000) == X64::REX) {
      maybe_rex = op;
      rex = true;
      op = start.read_byte();
    }

    bool rex_w = (maybe_rex & X64::REX_W) == X64::REX_W;

    if (op == 0x0F) {
      //0x0F instructions
      uint8_t op = start.read_byte();
      switch (op) {
        case X64::JE_NEAR: {
            int rel32 = x32_from_itr(&start);

            printf("je 0x%llx\n", (start.actual_location - start_idx) + rel32);
            break;
          }
        case X64::JNE_NEAR: {
            int rel32 = x32_from_itr(&start);

            printf("jne 0x%llx\n", (start.actual_location - start_idx) + rel32);
            break;
          }
        case X64::JB_NEAR: {
            int rel32 = x32_from_itr(&start);

            printf("jb 0x%llx\n", (start.actual_location - start_idx) + rel32);
            break;
          }
        case X64::JNB_NEAR: {
            int rel32 = x32_from_itr(&start);

            printf("jnb 0x%llx\n", (start.actual_location - start_idx) + rel32);
            break;
          }
        case X64::JA_NEAR: {
            int rel32 = x32_from_itr(&start);

            printf("ja 0x%llx\n", (start.actual_location - start_idx) + rel32);
            break;
          }
        case X64::JNA_NEAR: {
            int rel32 = x32_from_itr(&start);

            printf("jna 0x%llx\n", (start.actual_location - start_idx) + rel32);
            break;
          }
        case X64::JL_NEAR: {
            int rel32 = x32_from_itr(&start);

            printf("jl 0x%llx\n", (start.actual_location - start_idx) + rel32);
            break;
          }
        case X64::JNL_NEAR: {
            int rel32 = x32_from_itr(&start);

            printf("jnl 0x%llx\n", (start.actual_location - start_idx) + rel32);
            break;
          }
        case X64::JG_NEAR: {
            int rel32 = x32_from_itr(&start);

            printf("jg 0x%llx\n", (start.actual_location - start_idx) + rel32);
            break;
          }
        case X64::JNG_NEAR: {
            int rel32 = x32_from_itr(&start);

            printf("jng 0x%llx\n", (start.actual_location - start_idx) + rel32);
            break;
          }
        case X64::SETE_RM8: {
            uint8_t modrm = start.read_byte();

            load_8_sizes(&p_opts, rex, short_address);

            OwnedArr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &start);
            printf("sete %s\n", r_string.data);
            break;
          }
        case X64::SETL_RM8: {
            uint8_t modrm = start.read_byte();

            load_8_sizes(&p_opts, rex, short_address);

            OwnedArr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &start);
            printf("setl %s\n", r_string.data);
            break;
          }
        case X64::SETG_RM8: {
            uint8_t modrm = start.read_byte();

            load_8_sizes(&p_opts, rex, short_address);

            OwnedArr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &start);
            printf("setg %s\n", r_string.data);
            break;
          }
        case X64::IMUL_RM_TO_R: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("imul %s, %s\n", names.r.data, names.rm.data);
            break;
          }
        case X64::MOV_ZX_RM8_TO_R: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);
            //overide
            p_opts.r_name = b8_rex_reg_name;

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("movzx %s, %s\n", names.r.data, names.rm.data);
            break;
          }
        case X64::MOV_SX_RM8_TO_R: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);
            //overide
            p_opts.r_name = b8_rex_reg_name;

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("movsx %s, %s\n", names.r.data, names.rm.data);
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
      switch (op) {
        case X64::ADD_R_TO_RM: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("add %s, %s\n", names.rm.data, names.r.data);
            break;
          }
        case X64::OR_R8_TO_RM8: {
            uint8_t modrm = start.read_byte();

            load_8_sizes(&p_opts, rex, short_address);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("or  %s, %s\n", names.rm.data, names.r.data);
            break;
          }
        case X64::OR_R_TO_RM: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("or  %s, %s\n", names.rm.data, names.r.data);
            break;
          }
        case X64::AND_R_TO_RM: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("and %s, %s\n", names.rm.data, names.r.data);
            break;
          }
        case X64::SUB_R_TO_RM: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("sub %s, %s\n", names.rm.data, names.r.data);
            break;
          }
        case X64::XOR_R_TO_RM: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("xor %s, %s\n", names.rm.data, names.r.data);
            break;
          }
        case X64::CMP_R_TO_RM: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("cmp %s, %s\n", names.rm.data, names.r.data);
            break;
          }
        case X64::CMP_IMM_TO_AL: {
            ASSERT(!rex);
            uint8_t b = start.read_byte();
            printf("cmp al, %hhu\n", b);
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
            const char* r_string = b64_reg_name(reg);

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
            const char* r_string = b64_reg_name(reg);

            printf("pop %s\n", r_string);
            break;
          }
        case 0x80: {
            uint8_t modrm = start.read_byte();

            load_8_sizes(&p_opts, rex, short_address);

            OwnedArr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &start);

            i8 imm8 = start.read_byte();

            uint8_t r_val = (modrm & 0b0011'1000) >> 3;

            if (r_val == 5) {
              printf("sub %s, 0x%hhx\n", rm_string.data, imm8);
            }
            else if (r_val == 7) {
              printf("cmp %s, 0x%hhx\n", rm_string.data, imm8);
            }
            else {
              printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx.2 0x%.2hhx ...\n",
                     maybe_rex, op, modrm);

              return;
            }
            break;
          }
        case 0x81: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            OwnedArr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &start);

            int32_t imm32 = x32_from_itr(&start);

            uint8_t r_val = (modrm & 0b0011'1000) >> 3;

            if (r_val == 5) {
              printf("sub %s, 0x%x\n", rm_string.data, imm32);
            }
            else if (r_val == 7) {
              printf("cmp %s, 0x%x\n", rm_string.data, imm32);
            }
            else {
              printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx.2 0x%.2hhx ...\n",
                     maybe_rex, op, modrm);

              return;
            }
            break;
          }
        case X64::MOV_R_TO_RM: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("mov %s, %s\n", names.rm.data, names.r.data);
            break;
          }
        case X64::MOV_IMM32_RM: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            OwnedArr<char> rm = rm_reg_string(&p_opts, maybe_rex, modrm, &start);

            uint32_t val = x32_from_itr(&start);

            printf("mov %s, %u\n", rm.data, val);
            break;
          }
        case X64::MOV_IMM_TO_R:
        case (X64::MOV_IMM_TO_R + 1):
        case (X64::MOV_IMM_TO_R + 2):
        case (X64::MOV_IMM_TO_R + 3):
        case (X64::MOV_IMM_TO_R + 4):
        case (X64::MOV_IMM_TO_R + 5):
        case (X64::MOV_IMM_TO_R + 6):
        case (X64::MOV_IMM_TO_R + 7): {
            uint8_t r = (op - X64::MOV_IMM_TO_R) + ((maybe_rex & X64::REX_B) == X64::REX_B) * 8;

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            const char* name = p_opts.r_name(r);

            if (short_operand) {
              uint16_t val = x16_from_itr(&start);

              printf("mov %s, %hu\n", name, val);
            }
            else if (!rex_w) {
              uint32_t val = x32_from_itr(&start);

              printf("mov %s, %u\n", name, val);
            }
            else {
              uint64_t val = x64_from_itr(&start);

              printf("mov %s, %llu\n", name, val);
            }
            break;
          }
        case X64::MOV_RM_TO_R: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("mov %s, %s\n", names.r.data, names.rm.data);
            break;
          }
        case X64::LEA_RM_TO_R: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("lea %s, %s\n", names.r.data, names.rm.data);
            break;
            }
        case X64::CQO: {
            printf("cqo\n");
            break;
          }
#if 0
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
#endif
        case 0xF7: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            const uint8_t r = (modrm & 0b0011'1000) >> 3;
            OwnedArr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &start);

            if (r == 3) {
              printf("neg %s\n", rm_string.data);
            }
            else if (r == 4) {
              printf("mul %s\n", rm_string.data);
            }
            else if (r == 6) {
              printf("div %s\n", rm_string.data);
            }
            else if (r == 7) {
              printf("idiv %s\n", rm_string.data);
            }
            else {
              printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                     maybe_rex, op, modrm);

              return;
            }
            break;
          }
        case 0xD3: {
            uint8_t modrm = start.read_byte();

            load_default_sizes(&p_opts, rex_w, short_address, short_operand);

            const uint8_t r = (modrm & 0b0011'1000) >> 3;
            OwnedArr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &start);

            if (r == 4) {
              printf("sal %s, CL\n", rm_string.data);
            }
            else if (r == 5) {
              printf("shr %s, CL\n", rm_string.data);
            }
            else if (r == 7) {
              printf("sar %s, CL\n", rm_string.data);
            }
            else {
              printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                     maybe_rex, op, modrm);

              return;
            }
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
            const uint8_t reg = (op - X64::MOV_8_TO_R8) | ((maybe_rex & 0b0000'0001) << 3);
            const char* r_string = b8_rex_reg_name(reg);

            uint8_t imm8 = start.read_byte();

            printf("mov %s, 0x%hhx\n", r_string, imm8);
            break;
          }
        case X64::MOV_R8_TO_RM8: {
            uint8_t modrm = start.read_byte();

            load_8_sizes(&p_opts, rex, short_address);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

            printf("mov %s, %s\n", names.rm.data, names.r.data);
            break;
          }
        case X64::JMP_NEAR: {
            int rel32 = x32_from_itr(&start);

            printf("jmp 0x%llx\n", (start.actual_location - start_idx) + rel32);
            break;
          }
        case X64::RET_NEAR: {
            printf("ret\n");
            break;
          }
        case X64::CALL_NEAR: {
            int rel32 = x32_from_itr(&start);

            printf("call 0x%llx ; call will likely be incorrect\n", (start.actual_location - start_idx) + rel32);
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx\n",
                   op);

            return;
          }
          }
      }
    }
  }