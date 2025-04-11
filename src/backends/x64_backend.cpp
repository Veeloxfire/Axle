#include <AxleUtil/io.h>

#include <Axle/backends/x64_backend.h>

#include "AxleUtil/formattable.h"
#include "AxleUtil/safe_lib.h"
#include "AxleUtil/utility.h"
#include "ir.h"
#include "compiler.h"

#include "tracing_wrapper.h"

namespace IO_Single = Axle::IO_Single;

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

    bool operator==(const R& o) const {
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
    }

    INVALID_CODE_PATH("Invalid SIB scale");
  }

  static constexpr uint8_t sib_i_b(uint8_t i, uint8_t b) {
    return ((i & 0b111) << 3) | (b & 0b111);
  }

  static constexpr uint8_t sib(uint8_t s, uint8_t i, uint8_t b) {
    return sib_scale(s) | sib_i_b(i, b);
  }
// currently unused
#if 0
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
    return rm;
  }
#endif

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

    Axle::ViewArr<u8> sub_range(u8 sub_size) {
      ASSERT(sub_size > 0);
      ASSERT(sub_size <= 15);
      ASSERT(15 - sub_size > count);
      u8* d = bytes + count;
      count += sub_size;
      return { d, sub_size };
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

        Axle::ViewArr<u8> s = arr.sub_range(4);
        Axle::serialize_le<i32>(s, sib.disp);
      }
      else {
        ASSERT(sib.index != rsp.REG);//Not a valid code unfortunately

        arr.insert(0b00'000'000 | mod_byte);
        arr.insert(X64::sib(sib.scale, sib.index, rbp.REG));

        Axle::ViewArr<u8> s = arr.sub_range(4);
        Axle::serialize_le<i32>(s, sib.disp);
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

        Axle::ViewArr<u8> s = arr.sub_range(4);
        Axle::serialize_le<i32>(s, sib.disp);
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

        Axle::ViewArr<u8> s = arr.sub_range(4);
        Axle::serialize_le<i32>(s, sib.disp);
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

          Axle::ViewArr<u8> s = arr.sub_range(4);
          Axle::serialize_le<i32>(s, rm.disp);
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

          Axle::ViewArr<u8> s = arr.sub_range(4);
          Axle::serialize_le<i32>(s, rm.disp);
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
    Axle::ViewArr<u8> im = arr.sub_range(8);
    Axle::serialize_le<u64>(im, imm64.imm);
  }

  static void mov(Instruction& arr,
      R32 r,
      IMM32 imm32) {
    if (need_rex(r.r)) {
      arr.insert(X64::REX | X64::rex_rm(r.r));//this takes rm not r for some reason
    }

    arr.insert(X64::MOV_IMM_TO_R + (r.r & 0b111));

    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<u32>(im, imm32.imm);
  }

  static void mov(Instruction& arr,
      R16 r,
      IMM16 imm16) {
    arr.insert(X64::OVERRIDE_OPERAND);//16 bit mode

    if (need_rex(r.r)) {
      arr.insert(X64::REX | X64::rex_rm(r.r));//this takes rm not r for some reason
    }

    arr.insert(X64::MOV_IMM_TO_R + (r.r & 0b111));

    Axle::ViewArr<u8> im = arr.sub_range(2);
    Axle::serialize_le<u16>(im, imm16.imm);
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

    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<u32>(im, imm32.imm);
  }

  static void mov(Instruction& arr,
      const RM& rm,
      IMM32Extended imm32) {
    arr.insert(X64::REX_W | X64::rex_b(rm.r));
    arr.insert(X64::MOV_IMM32_RM);

    emit_mod_rm(arr, R{ '\0' }, rm);

    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<u32>(im, imm32.imm);
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

    Axle::ViewArr<u8> im = arr.sub_range(2);
    Axle::serialize_le<u16>(im, imm16.imm);
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

  constexpr u32 LEA_RM_DISP_OFFSET = 3;

  static void lea(Instruction& arr,
      IMM32 offset,
      R r) {
    arr.insert(X64::REX_W | X64::rex_r_rm(r.r, rbp.REG));
    arr.insert(X64::LEA_RM_TO_R);

    arr.insert(((r.r & 0b111) << 3) | (rbp.REG & 0b111));

    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<u32>(im, offset.imm);
  }

  static void sub(Instruction& arr,
      R64 rm,
      IMM32 imm32) {
    arr.insert(X64::REX_W | X64::rex_rm(rm.r));
    arr.insert(X64::SUB_32_TO_RM);
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(5, rm.r));

    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<u32>(im, imm32.imm);
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
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset);
  }

  static usize JUMP_NEAR_OFFSET = 1;

  static void jump_near(Instruction& arr, i32 jump_offset) {
    arr.insert(X64::JMP_NEAR);

    ASSERT(JUMP_NEAR_OFFSET == arr.count);
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset);
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
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset.disp32);
  }

  static usize JUMP_CONDITION_OFFSET = 2;

  static void jump_zero(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JZ_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset);
  }

  static constexpr auto jump_equal = jump_zero;

  // currently unused
#if 0
  static void jump_not_equal(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JNE_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset);
  }

  static void jump_above(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JA_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset);
  }

  static void jump_not_above(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JNA_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset);
  }

  static void jump_below(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JB_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset);
  }

  static void jump_not_below(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JNB_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset);
  }

  static void jump_lesser(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JL_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset);
  }

  static void jump_not_lesser(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JNL_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset);
  }

  static void jump_greater(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JG_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset);
  }

  static void jump_not_greater(Instruction& arr, i32 jump_offset) {
    arr.insert(0x0F);
    arr.insert(X64::JNG_NEAR);

    ASSERT(JUMP_CONDITION_OFFSET == arr.count);
    Axle::ViewArr<u8> im = arr.sub_range(4);
    Axle::serialize_le<i32>(im, jump_offset);
  }
#endif

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

  static void append_instruction(Backend::ProgramData* program, const Instruction& i) {
    program->code_store.push_arr(i.bytes, i.count);
  }
}

constexpr auto load_types_info() {
  struct SizeAndAlignment {
    u32 size;
    u32 alignment;
  };

  struct FormatData {
    SizeAndAlignment saa[11];

    constexpr u32 get_size(IR::Format f) const {
      ASSERT(static_cast<usize>(f) < Axle::array_size(saa));
      return saa[static_cast<usize>(f)].size;
    }
    constexpr u32 get_alignment(IR::Format f) const {
      ASSERT(static_cast<usize>(f) < Axle::array_size(saa));
      return saa[static_cast<usize>(f)].alignment;
    }
  };

  FormatData d = {};

  d.saa[static_cast<usize>(IR::Format::opaque)] = { 0, 1 };
  d.saa[static_cast<usize>(IR::Format::uint8)] = { 1, 1 };
  d.saa[static_cast<usize>(IR::Format::sint8)] = { 1, 1 };
  d.saa[static_cast<usize>(IR::Format::uint16)] = { 2, 2 };
  d.saa[static_cast<usize>(IR::Format::sint16)] = { 2, 2 };
  d.saa[static_cast<usize>(IR::Format::uint32)] = { 4, 4 };
  d.saa[static_cast<usize>(IR::Format::sint32)] = { 4, 4 };
  d.saa[static_cast<usize>(IR::Format::uint64)] = { 8, 8 };
  d.saa[static_cast<usize>(IR::Format::sint64)] = { 8, 8 };
  d.saa[static_cast<usize>(IR::Format::pointer)] = { 8, 8 };
  d.saa[static_cast<usize>(IR::Format::slice)] = { 16, 8 };

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

namespace IntHelpers {
  static void zero_register(Backend::ProgramData* program, X64::R r) {
    X64::Instruction i = {};
    X64::xor_(i, X64::R32{r}, X64::R32{r});
    X64::append_instruction(program, i);
  }

  static void sign_extend_rax_rdx(Backend::ProgramData* program, IR::Format f) {
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

#pragma warning(push)
#pragma warning(disable: 4061)

  static void copy_reg_to_reg(Backend::ProgramData* program,
      X64::R from, IR::Format f_format,
      X64::R to, IR::Format t_format) {
    ASSERT(f_format != IR::Format::opaque && t_format != IR::Format::opaque);

    X64::Instruction inst = {};

    switch (f_format) {
      case IR::Format::uint8: {
        switch (t_format) {
          case IR::Format::uint8:
          case IR::Format::sint8:
            if (from == to) return;
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
            if (from == to) return;
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
            if (from == to) return;
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
            if (from == to) return;
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
            if (from == to) return;
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
            if (from == to) return;
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
            if (from == to) return;
            X64::mov(inst, X64::R64{ from }, X64::R64{ to });

            break;
          default: INVALID_CODE_PATH("Unsupported copy");
        }
        break;
      }
      case IR::Format::pointer: {
        switch (t_format) {
          case IR::Format::pointer:
            if (from == to) return;
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

  static void copy_mem_to_reg(Backend::ProgramData* program,
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
      case IR::Format::pointer: {
        switch (t_format) {
          case IR::Format::pointer:
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

  static void copy_reg_to_mem(Backend::ProgramData* program,
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
      case IR::Format::pointer: {
        switch (t_format) {
          case IR::Format::pointer: {
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

  static void load_const_to_reg(Backend::ProgramData* program, IR::Format f, X64::R reg, const Axle::ViewArr<const u8>& data) {
    ASSERT(f != IR::Format::opaque);

    X64::Instruction i = {};

    switch (f) {
      case IR::Format::uint8:
      case IR::Format::sint8: {
        X64::mov(i, X64::R8{ reg }, X64::IMM8{ data[0] });
        break;
      }
      case IR::Format::uint16:
      case IR::Format::sint16: {
        u16 v;
        bool res = Axle::deserialize_le<u16>(data, v);
        ASSERT(res);
        X64::mov(i, X64::R16{reg}, X64::IMM16{ v });
        break;
      }
      case IR::Format::uint32:
      case IR::Format::sint32: {
        u32 v;
        bool res = Axle::deserialize_le<u32>(data, v);
        ASSERT(res);
        X64::mov(i, X64::R32{reg}, X64::IMM32{ v });
        break;
      }
      case IR::Format::uint64:
      case IR::Format::sint64:
      case IR::Format::pointer: {
        u64 v;
        bool res = Axle::deserialize_le<u64>(data, v);
        ASSERT(res);
        X64::mov(i, X64::R64{reg}, X64::IMM64{ v });
        break;
      }
      default:
        INVALID_CODE_PATH("Cant handle other data formats");
    }

    X64::append_instruction(program, i);
  }

  static void load_const_to_mem(Backend::ProgramData* program, IR::Format f, MemoryView view, const Axle::ViewArr<const u8>& data) {
    ASSERT(f != IR::Format::opaque);

    ASSERT(x64_types_info.get_size(f) == view.size);

    switch (f) {
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

        u16 v;
        bool res = Axle::deserialize_le<u16>(data, v);
        ASSERT(res);
        X64::mov(i, X64::RM16{ view.rm }, X64::IMM16{ v });
        X64::append_instruction(program, i);
        break;
      }
      case IR::Format::uint32:
      case IR::Format::sint32: {
        X64::Instruction i = {};
        u32 v;
        bool res = Axle::deserialize_le<u32>(data, v);
        ASSERT(res);
        X64::mov(i, X64::RM32{ view.rm }, X64::IMM32{ v });
        X64::append_instruction(program, i);
        break;
      }
      case IR::Format::uint64:
      case IR::Format::sint64:
      case IR::Format::pointer: {
        i64 val;
        bool res = Axle::deserialize_le<i64>(data, val);
        ASSERT(res);

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

  static void copy_mem_to_mem(Backend::ProgramData* program,
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
      case IR::Format::pointer: {
        switch (t_format) {
          case IR::Format::pointer:
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

#define EMIT_SYMMETRICAL_HELPER(name)\
  static X64::R emit_ ## name(Backend::ProgramData* program,\
      X64::R left, IR::Format l_format,\
      X64::R right, IR::Format r_format) {\
    ASSERT(l_format == r_format);\
    X64::Instruction i = {};\
    switch (l_format) {\
      case IR::Format::uint8:\
      case IR::Format::sint8: {\
        X64:: name (i, X64::R8{right}, X64::R8{left});\
        break;\
      }\
      case IR::Format::uint16:\
      case IR::Format::sint16: {\
        X64:: name (i, X64::R16{right}, X64::R16{left});\
        break;\
      }\
      case IR::Format::uint32:\
      case IR::Format::sint32: {\
        X64:: name (i, X64::R32{right}, X64::R32{left});\
        break;\
      }\
      case IR::Format::uint64:\
      case IR::Format::sint64: {\
        X64:: name (i, X64::R64{right}, X64::R64{left});\
        break;\
      }\
      default: INVALID_CODE_PATH("Invalid " #name " format");\
    }\
    X64::append_instruction(program, i);\
    return left;\
  }

  EMIT_SYMMETRICAL_HELPER(and_);
  EMIT_SYMMETRICAL_HELPER(or_);
  EMIT_SYMMETRICAL_HELPER(xor_);

#undef EMIT_SYMMETRICAL_HELPER

  static X64::R emit_add(Backend::ProgramData* program,
      X64::R left, IR::Format l_format,
      X64::R right, IR::Format r_format) {
    if(l_format == r_format) {
      X64::Instruction i = {};
      switch (l_format) {
        case IR::Format::uint8:
        case IR::Format::sint8: {
          X64::add(i, X64::R8{right}, X64::R8{left});
          break;
        }
        case IR::Format::uint16:
        case IR::Format::sint16: {
          X64::add(i, X64::R16{right}, X64::R16{left});
          break;
        }
        case IR::Format::uint32:
        case IR::Format::sint32: {
          X64::add(i, X64::R32{right}, X64::R32{left});
          break;
        }
        case IR::Format::uint64:
        case IR::Format::sint64: {
          X64::add(i, X64::R64{right}, X64::R64{left});
          break;
        }
        default: INVALID_CODE_PATH("Invalid add format");
      }
      X64::append_instruction(program, i);
      return left;
    }
    else if((l_format == IR::Format::pointer && r_format == IR::Format::uint64)
        || (l_format == IR::Format::uint64 && r_format == IR::Format::pointer)) {
      X64::Instruction i = {};
      X64::add(i, X64::R64{right}, X64::R64{left});
      X64::append_instruction(program, i);
      return left;
    }

    INVALID_CODE_PATH("Invalid add format");
  }

  static X64::R emit_sub(Backend::ProgramData* program,
      X64::R left, IR::Format l_format,
      X64::R right, IR::Format r_format) {
    if(l_format == r_format) {
      X64::Instruction i = {};
      switch (l_format) {
        case IR::Format::uint8:
        case IR::Format::sint8: {
          X64::sub(i, X64::R8{right}, X64::R8{left});
          break;
        }
        case IR::Format::uint16:
        case IR::Format::sint16: {
          X64::sub(i, X64::R16{right}, X64::R16{left});
          break;
        }
        case IR::Format::uint32:
        case IR::Format::sint32: {
          X64::sub(i, X64::R32{right}, X64::R32{left});
          break;
        }
        case IR::Format::uint64:
        case IR::Format::sint64: {
          X64::sub(i, X64::R64{right}, X64::R64{left});
          break;
        }
        default: INVALID_CODE_PATH("Invalid sub format");
      }
      X64::append_instruction(program, i);
      return left;
    }
    else if(l_format == IR::Format::pointer && r_format == IR::Format::pointer) {
      X64::Instruction i = {};
      X64::sub(i, X64::R64{right}, X64::R64{left});
      X64::append_instruction(program, i);
      return left;
    }

    INVALID_CODE_PATH("Invalid sub format");
  }

  static X64::R emit_mul(Backend::ProgramData* program,
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
      default: INVALID_CODE_PATH("Invalid multiply format");
    }

    X64::append_instruction(program, i);
    return left;
  }

  static X64::R emit_div(Backend::ProgramData* program,
      IR::Format l_format,
      X64::R right, IR::Format r_format) {
    ASSERT(l_format == r_format);
    X64::Instruction i = {};
    switch (l_format) {
      case  IR::Format::uint8: {
        X64::div(i, X64::R8{right}, X64::RAX{});
        break;
      }
      case  IR::Format::sint8: {
        X64::idiv(i, X64::R8{right}, X64::RAX{});
        break;
      }
      case  IR::Format::uint16: {
        zero_register(program, X64::R{X64::rdx.REG});
        X64::div(i, X64::R16{right}, X64::RAX{});
        break;
      }
      case  IR::Format::sint16: {
        sign_extend_rax_rdx(program, l_format);
        X64::idiv(i, X64::R16{right}, X64::RAX{});
        break;
      }
      case  IR::Format::uint32: {
        zero_register(program, X64::R{X64::rdx.REG});
        X64::div(i, X64::R32{right}, X64::RAX{});
        break;
      }
      case  IR::Format::sint32: {
        sign_extend_rax_rdx(program, l_format);
        X64::idiv(i, X64::R32{right}, X64::RAX{});
        break;
      }
      case  IR::Format::uint64: {
        zero_register(program, X64::R{X64::rdx.REG});
        X64::div(i, X64::R64{right}, X64::RAX{});
        break;
      }
      case  IR::Format::sint64: {
        sign_extend_rax_rdx(program, l_format);
        X64::idiv(i, X64::R64{right}, X64::RAX{});
        break;
      }
      default: INVALID_CODE_PATH("Invalid divide format");
    }

    X64::append_instruction(program, i);
    return X64::R{X64::rax.REG};
  }

  static X64::R emit_mod(Backend::ProgramData* program,
      IR::Format l_format,
      X64::R right, IR::Format r_format) {
    ASSERT(l_format == r_format);
    X64::Instruction i = {};
    switch (l_format) {
      case  IR::Format::uint8: {
        X64::div(i, X64::R8{right}, X64::RAX{});
        break;
      }
      case  IR::Format::sint8: {
        X64::idiv(i, X64::R8{right}, X64::RAX{});
        break;
      }
      case  IR::Format::uint16: {
        zero_register(program, X64::R{X64::rdx.REG});
        X64::div(i, X64::R16{right}, X64::RAX{});
        break;
      }
      case  IR::Format::sint16: {
        sign_extend_rax_rdx(program, l_format);
        X64::idiv(i, X64::R16{right}, X64::RAX{});
        break;
      }
      case  IR::Format::uint32: {
        zero_register(program, X64::R{X64::rdx.REG});
        X64::div(i, X64::R32{right}, X64::RAX{});
        break;
      }
      case  IR::Format::sint32: {
        sign_extend_rax_rdx(program, l_format);
        X64::idiv(i, X64::R32{right}, X64::RAX{});
        break;
      }
      case  IR::Format::uint64: {
        zero_register(program, X64::R{X64::rdx.REG});
        X64::div(i, X64::R64{right}, X64::RAX{});
        break;
      }
      case  IR::Format::sint64: {
        sign_extend_rax_rdx(program, l_format);
        X64::idiv(i, X64::R64{right}, X64::RAX{});
        break;
      }
      default: INVALID_CODE_PATH("Invalid mod format");
    }

    X64::append_instruction(program, i);
    return X64::R{X64::rdx.REG};
  }

  static void emit_cmp(Backend::ProgramData* program,
      X64::R left, IR::Format l_format,
      X64::R right, IR::Format r_format) {
    ASSERT(l_format == r_format);
    X64::Instruction i = {};
    switch (l_format) {
      case IR::Format::uint8:
      case IR::Format::sint8: {
        X64::cmp(i, X64::R8{right}, X64::R8{left});
        break;
      }
      case IR::Format::uint16:
      case IR::Format::sint16: {
        X64::cmp(i, X64::R16{right}, X64::R16{left});
        break;
      }
      case IR::Format::uint32:
      case IR::Format::sint32: {
        X64::cmp(i, X64::R32{right}, X64::R32{left});
        break;
      }
      case IR::Format::uint64:
      case IR::Format::sint64:
      case IR::Format::pointer: {
        X64::cmp(i, X64::R64{right}, X64::R64{left});
        break;
      }
      default: INVALID_CODE_PATH("Invalid comparable format");
    }

    X64::append_instruction(program, i);
  }

  static void emit_great(Backend::ProgramData* program,
      X64::R left, IR::Format l_format,
      X64::R right, IR::Format r_format) {
    emit_cmp(program, left, l_format, right, r_format);
    X64::Instruction i = {};
    X64::setg(i, X64::R8{left});
    X64::append_instruction(program, i);
  }

  static void emit_less(Backend::ProgramData* program,
      X64::R left, IR::Format l_format,
      X64::R right, IR::Format r_format) {
    emit_cmp(program, left, l_format, right, r_format);
    X64::Instruction i = {};
    X64::setl(i, X64::R8{left});
    X64::append_instruction(program, i);
  }

  static void emit_eq(Backend::ProgramData* program,
      X64::R left, IR::Format l_format,
      X64::R right, IR::Format r_format) {
    emit_cmp(program, left, l_format, right, r_format);
    X64::Instruction i = {};
    X64::sete(i, X64::R8{left});
    X64::append_instruction(program, i);
  }

  static void emit_neq(Backend::ProgramData* program,
      X64::R left, IR::Format l_format,
      X64::R right, IR::Format r_format) {
    emit_cmp(program, left, l_format, right, r_format);
    X64::Instruction i = {};
    X64::setne(i, X64::R8{left});
    X64::append_instruction(program, i);
  }

#pragma warning(pop)
}

namespace Helpers {
  static void copy_address_to_reg(Backend::ProgramData* program, const MemoryView& mem, X64::R r) {
    X64::Instruction i = {};
    X64::lea(i, mem.rm, r);

    X64::append_instruction(program, i);
  }

  static void copy_mem_to_mem_opaque(Backend::ProgramData* program, MemoryView from, MemoryView to, X64::R temp_reg) {
    constexpr static auto COPY8 = [](Backend::ProgramData* program,
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

    constexpr static auto COPY16 = [](Backend::ProgramData* program,
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

    constexpr static auto COPY32 = [](Backend::ProgramData* program,
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

    constexpr static auto COPY64 = [](Backend::ProgramData* program,
        MemoryView& from, X64::R temp, MemoryView& to) {
      X64::Instruction first = {};
      X64::Instruction second = {};

      X64::mov(first, X64::RM64{ from.rm }, X64::R64{ temp });
      from.rm.disp += 8;
      from.size -= 8;

      X64::mov(second, X64::R64{ temp }, X64::RM64{ to.rm });
      to.rm.disp += 8;
      to.size -= 8;

      X64::append_instruction(program, first);
      X64::append_instruction(program, second);
    };

    ASSERT(from.size == to.size);
    ASSERT(from.known_alignment == to.known_alignment);

    u32 alignment = from.known_alignment;
    ASSERT(from.size % from.known_alignment == 0);

    u32 count = from.size / alignment;

    switch (alignment) {
      case 1: {
        for (u32 c = 0; c < count; ++c) {
          COPY8(program, from, temp_reg, to);
        }
        break;
      }
      case 2: {
        for (u32 c = 0; c < count; ++c) {
          COPY16(program, from, temp_reg, to);
        }
        break;
      }
      case 4: {
        for (u32 c = 0; c < count; ++c) {
          COPY32(program, from, temp_reg, to);
        }
        break;
      }
      case 8: {
        for (u32 c = 0; c < count; ++c) {
          COPY64(program, from, temp_reg, to);
        }
        break;
      }
      default: INVALID_CODE_PATH("Invalid alignemnt"); return;
    }

    ASSERT(from.size == 0);
    ASSERT(to.size == 0);
  }

  static void load_const_to_mem_opaque(Backend::ProgramData* program, MemoryView view, Axle::ViewArr<const u8> data) {
    Axle::Serializer<Axle::ViewArr<const u8>, Axle::ByteOrder::LittleEndian> dser = {data};
    using DSER = decltype(dser);

    constexpr static auto COPY8 = [](Backend::ProgramData* program, MemoryView& view, DSER& data) {
      X64::Instruction i = {};
      u8 v;
      bool res = Axle::deserialize_le<u8>(data, v);
      ASSERT(res);
      X64::mov(i, view.rm, X64::IMM8{ v });
      view.rm.disp += 1;
      view.size -= 1;

      X64::append_instruction(program, i);
    };

    constexpr static auto COPY16 = [](Backend::ProgramData* program, MemoryView& view, DSER& data) {
      X64::Instruction i = {};
      u16 v;
      bool res = Axle::deserialize_le<u16>(data, v);
      ASSERT(res);
      X64::mov(i, view.rm, X64::IMM16{ v });
      view.rm.disp += 2;
      view.size -= 2;

      X64::append_instruction(program, i);
    };

    constexpr static auto COPY32 = [](Backend::ProgramData* program, MemoryView& view, DSER& data) {
      X64::Instruction i = {};
      u32 v;
      bool res = Axle::deserialize_le<u32>(data, v);
      ASSERT(res);
      X64::mov(i, view.rm, X64::IMM32{ v });
      view.rm.disp += 4;
      view.size -= 4;

      X64::append_instruction(program, i);
    };

    constexpr static auto COPY64 = [](Backend::ProgramData* program, MemoryView& view, DSER& data) {
      i64 val;
      bool res = Axle::deserialize_le<i64>(data, val);
      ASSERT(res);

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
    };

    u32 alignment = view.known_alignment;
    ASSERT(view.size % alignment == 0);
    u32 count = view.size / alignment;

    switch (alignment) {
      case 1: {
        for (u32 c = 0; c < count; ++c) {
          COPY8(program, view, dser);
        }
        break;
      }
      case 2: {
        for (u32 c = 0; c < count; ++c) {
          COPY16(program, view, dser);
        }
        break;
      }
      case 4: {
        for (u32 c = 0; c < count; ++c) {
          COPY32(program, view, dser);
        }
        break;
      }
      case 8: {
        for (u32 c = 0; c < count; ++c) {
          COPY64(program, view, dser);
        }
        break;
      }
      default: INVALID_CODE_PATH("Invalid alignment"); return;
    }
  }

#if 0
  template<typename O>
    static auto dispatch_single(const O& o, const Structure* left) {
      switch (left->type) {
        case STRUCTURE_TYPE::VOID: {
          return o(static_cast<const VoidStructure*>(left));
        }
        case STRUCTURE_TYPE::TYPE: {
          return o(static_cast<const TypeStructure*>(left));
        }
        case STRUCTURE_TYPE::INTEGER: {
          return o(static_cast<const IntegerStructure*>(left));
        }
        case STRUCTURE_TYPE::POINTER: {
          return o(static_cast<const PointerStructure*>(left));
        }
        case STRUCTURE_TYPE::ENUM: {
          return o(static_cast<const EnumStructure*>(left));
        }
        case STRUCTURE_TYPE::COMPOSITE: {
          return o(static_cast<const CompositeStructure*>(left));
        }
        case STRUCTURE_TYPE::FIXED_ARRAY: {
          return o(static_cast<const ArrayStructure*>(left));
        }
        case STRUCTURE_TYPE::TUPLE: {
          return o(static_cast<const TupleStructure*>(left));
        }
        case STRUCTURE_TYPE::LAMBDA: {
          return o(static_cast<const SignatureStructure*>(left));
        }
      }

      INVALID_CODE_PATH("Invalid type");
      return o.invalid();
    }

  template<typename O, typename T>
    static auto dispatch_pair_2(const O& o, const T* left, const Structure* right) {
      switch (right->type) {
        case STRUCTURE_TYPE::VOID: {
          return o(left, static_cast<const VoidStructure*>(right));
        }
        case STRUCTURE_TYPE::TYPE: {
          return o(left, static_cast<const TypeStructure*>(right));
        }
        case STRUCTURE_TYPE::INTEGER: {
          return o(left, static_cast<const IntegerStructure*>(right));
        }
        case STRUCTURE_TYPE::POINTER: {
          return o(left, static_cast<const PointerStructure*>(right));
        }
        case STRUCTURE_TYPE::ENUM: {
          return o(left, static_cast<const EnumStructure*>(right));
        }
        case STRUCTURE_TYPE::COMPOSITE: {
          return o(left, static_cast<const CompositeStructure*>(right));
        }
        case STRUCTURE_TYPE::FIXED_ARRAY: {
          return o(left, static_cast<const ArrayStructure*>(right));
        }
        case STRUCTURE_TYPE::TUPLE: {
          return o(left, static_cast<const TupleStructure*>(right));
        }
        case STRUCTURE_TYPE::LAMBDA: {
          return o(left, static_cast<const SignatureStructure*>(right));
        }
      }

      INVALID_CODE_PATH("Invalid right type");
      return o.invalid();
    }
#endif

  template<typename O>
    static auto dispatch_pair(const O& o, const Structure* left, const Structure* right) {
      return visit_types(o, left, right);
#if 0
      switch (left->type) {
        case STRUCTURE_TYPE::VOID: {
          return dispatch_pair_2(o, static_cast<const VoidStructure*>(left), right);
        }
        case STRUCTURE_TYPE::TYPE: {
          return dispatch_pair_2(o, static_cast<const TypeStructure*>(left), right);
        }
        case STRUCTURE_TYPE::INTEGER: {
          return dispatch_pair_2(o, static_cast<const IntegerStructure*>(left), right);
        }
        case STRUCTURE_TYPE::POINTER: {
          return dispatch_pair_2(o, static_cast<const PointerStructure*>(left), right);
        }
        case STRUCTURE_TYPE::ENUM: {
          return dispatch_pair_2(o, static_cast<const EnumStructure*>(left), right);
        }
        case STRUCTURE_TYPE::COMPOSITE: {
          return dispatch_pair_2(o, static_cast<const CompositeStructure*>(left), right);
        }
        case STRUCTURE_TYPE::FIXED_ARRAY: {
          return dispatch_pair_2(o, static_cast<const ArrayStructure*>(left), right);
        }
        case STRUCTURE_TYPE::TUPLE: {
          return dispatch_pair_2(o, static_cast<const TupleStructure*>(left), right);
        }
        case STRUCTURE_TYPE::LAMBDA: {
          return dispatch_pair_2(o, static_cast<const SignatureStructure*>(left), right);
        }
      }

      INVALID_CODE_PATH("Invalid left type");
      return o.invalid();
#endif
    }

  struct RegRegDispatch {
    Backend::ProgramData* program;
    X64::R from;
    X64::R to;

    //Base case
    void operator()(const Structure*, const Structure*) const {
      INVALID_CODE_PATH("Invalid structures for reg to reg copy");
    }

    void operator()(const Axle::OneOf<IntegerStructure, EnumStructure, PointerStructure> auto* s1,
        const Axle::OneOf<IntegerStructure, EnumStructure, PointerStructure> auto* s2) const {
      return IntHelpers::copy_reg_to_reg(program, from, s1->ir_format, to, s2->ir_format);
    }
  };

  static constexpr bool is_integer_format(IR::Format f) noexcept {
    return f == IR::Format::uint8
        || f == IR::Format::uint16
        || f == IR::Format::uint32
        || f == IR::Format::uint64
        || f == IR::Format::sint8
        || f == IR::Format::sint16
        || f == IR::Format::sint32
        || f == IR::Format::sint64;
  } 

  static constexpr bool is_register_format(IR::Format f) noexcept {
    return is_integer_format(f) || f == IR::Format::pointer;
  }

  static void copy_reg_to_reg(Backend::ProgramData* program,
      X64::R from, IR::Format f_type,
      X64::R to, IR::Format t_type) noexcept {
    ASSERT(is_register_format(f_type));
    ASSERT(is_register_format(t_type));
    
    return IntHelpers::copy_reg_to_reg(program, from, f_type, to, t_type);
  }

  static void load_const_to_mem(Backend::ProgramData* program,
      IR::Format format, const MemoryView& view,
      const Axle::ViewArr<const u8>& data) noexcept {
    if (is_register_format(format)) {
      ASSERT(x64_types_info.get_size(format) == view.size);
      ASSERT(x64_types_info.get_size(format) == data.size);
      return IntHelpers::load_const_to_mem(program, format, view, data);
    }
    else {
      ASSERT(view.size == data.size);
      return load_const_to_mem_opaque(program, view, data);
    }
  }

  static void load_const_to_reg(Backend::ProgramData* program,
      IR::Format format, X64::R reg,
      const Axle::ViewArr<const u8>& data) noexcept {
    ASSERT(is_register_format(format));
    ASSERT(x64_types_info.get_size(format) == data.size);
    
    return IntHelpers::load_const_to_reg(program, format, reg, data);
  }

  static void copy_mem_to_reg(Backend::ProgramData* program,
      const MemoryView& from, IR::Format f_type,
      X64::R to, IR::Format t_type) {
    ASSERT(is_register_format(f_type));
    ASSERT(x64_types_info.get_size(f_type) == from.size);
    ASSERT(is_register_format(t_type));
    return IntHelpers::copy_mem_to_reg(program, from, f_type, to, t_type);
  }

  static void copy_reg_to_mem(Backend::ProgramData* program,
      X64::R from, IR::Format f_type,
      const MemoryView& to, IR::Format t_type) {
    ASSERT(is_register_format(f_type));
    ASSERT(is_register_format(t_type));
    ASSERT(x64_types_info.get_size(t_type) == to.size);
    
    return IntHelpers::copy_reg_to_mem(program, from, f_type, to, t_type);
  }

  struct MemMemDispatch {
    Backend::ProgramData* program;
    const MemoryView& from;
    const MemoryView& to;
    X64::R temp;

    //Base case
    void operator()(const Structure*, const Structure*) const {
      return copy_mem_to_mem_opaque(program, from, to, temp);
    }

    void operator()(const Axle::OneOf<IntegerStructure, EnumStructure> auto* s1,
        const Axle::OneOf<IntegerStructure, EnumStructure> auto* s2) const {
      return IntHelpers::copy_mem_to_mem(program, from, s1->ir_format, to, s2->ir_format, temp);
    }
  };


  static void copy_mem_to_mem(Backend::ProgramData* program,
      const MemoryView& from, IR::Format f_type,
      const MemoryView& to, IR::Format t_type,
      X64::R temp) {
    if (is_register_format(f_type)
     && is_register_format(t_type)) {
      ASSERT(x64_types_info.get_size(t_type) == to.size);
      ASSERT(x64_types_info.get_size(f_type) == from.size);
      return IntHelpers::copy_mem_to_mem(program, from, f_type, to, t_type, temp);
    }
    else {
      ASSERT(from.size == to.size);
      return copy_mem_to_mem_opaque(program, from, to, temp);
    }  
  }

#define OP_FN(name, helper_name)                                            \
  static X64::R name (Backend::ProgramData* program,                        \
      X64::R from, IR::Format f_type,                                       \
      X64::R to, IR::Format t_type) {                                       \
    ASSERT(is_register_format(f_type));                                     \
    ASSERT(is_register_format(t_type));                                     \
    return IntHelpers:: helper_name (program, from, f_type, to, t_type);    \
  }

  OP_FN(emit_add, emit_add);
  OP_FN(emit_sub, emit_sub);
  OP_FN(emit_mul, emit_mul);
  OP_FN(emit_and, emit_and_);
  OP_FN(emit_or, emit_or_);
  OP_FN(emit_xor, emit_xor_);

#undef OP_FN

#define EQ_OP_FN(name, helper_name)                                         \
  static void name (Backend::ProgramData* program,                          \
      X64::R from, IR::Format f_type,                                       \
      X64::R to, IR::Format t_type) {                                       \
    ASSERT(is_register_format(f_type));                                     \
    ASSERT(is_register_format(t_type));                                     \
    return IntHelpers:: helper_name (program, from, f_type, to, t_type);    \
  }

  EQ_OP_FN(emit_great, emit_great);
  EQ_OP_FN(emit_less, emit_less);
  EQ_OP_FN(emit_eq, emit_eq);
  EQ_OP_FN(emit_neq, emit_neq);

#undef EQ_OP_FN

  static X64::R emit_div(Backend::ProgramData* program,
      X64::R from, IR::Format f_type,
      X64::R to, IR::Format t_type) {
    ASSERT(is_register_format(f_type));
    ASSERT(is_register_format(t_type));

    ASSERT(from.r == X64::rax.REG);
    return IntHelpers::emit_div(program, f_type, to, t_type);
  }

  static X64::R emit_mod(Backend::ProgramData* program,
      X64::R from, IR::Format f_type,
      X64::R to, IR::Format t_type) {
    ASSERT(is_register_format(f_type));
    ASSERT(is_register_format(t_type));

    ASSERT(from.r == X64::rax.REG);
    return IntHelpers::emit_mod(program, f_type, to, t_type);
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
  Register = 0,
  Memory,
};

struct RegisterMapping {
  MapType map_type = MapType::Register;
  bool crosses_call = false;
  bool known_reg = false;
  u8 reg = 0;
  union {
    u32 expr_id = 0;
    u32 reg_bitmask;
  };
};

struct RegisterResolver {
  const SignatureStructure* sig_struct;
  const CallingConvention* this_convention;

  const IR::ControlBlock* current_block;

  Axle::ViewArr<const IR::GlobalReference> globals_used;
  Axle::ViewArr<RegisterMapping> to_map;

  Axle::ViewArr<const IR::SSATemp> local_temporaries;

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

static VisitRes visit_ordered_value(const Axle::ViewArr<RegisterMapping>& mappings,
    const Axle::OwnedArr<ValueLifetime>& lifetimes,
    const RegisterResolver* resolver,
    const IR::V_ARG& v_arg, u32 expr_id) {
  ASSERT(lifetimes.size == mappings.size);
  const u32 t_index = resolver->local_temp_id(v_arg.val);
  const IR::SSATemp& t = resolver->local_temporaries[t_index];

  const MapType map_type = mappings[t_index].map_type;
  if (map_type == MapType::Memory) return { NeedIntermediate::Yes, t.type };//ignored- assumed its a memory location

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

  return { NeedIntermediate::No, t.type };
}

static VisitRes visit_ordered_value(const Axle::ViewArr<RegisterMapping>& mappings,
    const Axle::OwnedArr<ValueLifetime>& lifetimes,
    const RegisterResolver* resolver,
    const IR::P_ARG& v_arg, u32 expr_id) {
  ASSERT(lifetimes.size == mappings.size);
  const u32 t_index = resolver->local_temp_id(v_arg.ptr);
  const IR::SSATemp& t = resolver->local_temporaries[t_index];

  const MapType map_type = mappings[t_index].map_type;
  if (map_type == MapType::Memory) return { NeedIntermediate::Yes, t.type };//ignored- assumed its a memory location

  ValueLifetime& ov = lifetimes[t_index];
  ASSERT(v_arg.ptr_offset == 0);//For now we don't worry about this

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

  return { NeedIntermediate::No, t.type };
}

void new_intermediate(Axle::Array<RegisterMapping>& intermediates, u32 expr_id) {
  intermediates.insert_uninit(1);
  auto* v = intermediates.back();
  v->known_reg = false;
  v->reg = 0;
  v->expr_id = expr_id;
}

void new_fixed_intermediate(Axle::Array<RegisterMapping>& intermediates, u32 expr_id, u8 reg) {
  intermediates.insert_uninit(1);
  auto* v = intermediates.back();
  v->known_reg = true;
  v->reg = reg;
  v->expr_id = expr_id;
}

struct ResolvedMappings {
  u32 call_space_needed = 0;
  u32 used_registers = 0;

  Axle::OwnedArr<RegisterMapping> intermediates;
};

Axle::OwnedArr<LifetimeEdge> determine_edges(const Axle::ViewArr<ValueLifetime>& lifetimes,
    const Axle::ViewArr<const Axle::ViewArr<RegisterMapping>>& other_mappings) {
  Axle::Array<LifetimeEdge> edges = {};

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
      const Axle::ViewArr<RegisterMapping>& mapping = other_mappings[m];

      for (usize i = 0; i < mapping.size; ++i) {
        const RegisterMapping& in = mapping[i];
        ASSERT(in.map_type != MapType::Memory);
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

constexpr bool temp_needs_memory(const IR::SSATemp& temp) {
  if (temp.is_variable) return true;
  if (temp.requirements.has_address()) return true;
  if (temp.type.size() > 8) return true;

  switch (temp.type.structure->type) {
    case STRUCTURE_TYPE::VOID: INVALID_CODE_PATH("Cannot have void value");
    case STRUCTURE_TYPE::TYPE: INVALID_CODE_PATH("Cannot have type value at runtime");
    case STRUCTURE_TYPE::LAMBDA: INVALID_CODE_PATH("Cannot have lambda value at runtime");
    
    case STRUCTURE_TYPE::INTEGER: return false;
    case STRUCTURE_TYPE::POINTER: return false;
    case STRUCTURE_TYPE::ENUM: return false;

    //TODO: allow these to be inside registers
    case STRUCTURE_TYPE::COMPOSITE: return true;
    case STRUCTURE_TYPE::FIXED_ARRAY: return true;
    case STRUCTURE_TYPE::TUPLE: return true;
    case STRUCTURE_TYPE::SLICE: return true;
  }

  INVALID_CODE_PATH("Unexpected structure type");
}

ResolvedMappings resolve_values(CompilerGlobals* comp,
    CompilerThread* comp_thread,
    RegisterResolver* resolver,
    const CallingConvention* this_convention) {
  AXLE_TELEMETRY_FUNCTION();
  const u8* const bc_start = resolver->bytecode_start;
  const u8* const bc_end = resolver->bytecode_end;

  const u8* bc = bc_start;

  ASSERT(resolver->to_map.size == resolver->local_temporaries.size);
  const Axle::ViewArr<RegisterMapping>& values = resolver->to_map;
  Axle::OwnedArr<ValueLifetime> lifetimes = Axle::new_arr<ValueLifetime>(resolver->local_temporaries.size);

  //Is a value too big to fit in a register - don't worry about it
  for (u32 i = 0; i < resolver->local_temporaries.size; ++i) {
    const IR::SSATemp& temp = resolver->local_temporaries[i];
    if (temp_needs_memory(temp)) {
      values[i].map_type = MapType::Memory;
    }
    else {
      values[i].map_type = MapType::Register;
    }
  }

  u32 expr_id = 0;

  //Now visit all the values
  Axle::Array<RegisterMapping> mangled_registers = {};
  Axle::Array<RegisterMapping> intermediates = {};

  u32 call_space_needed = 0;
  u32 used_registers = 0;

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

        VisitRes im = visit_ordered_value(values, lifetimes, resolver, addr.val, expr_id);

        if (im.ni == NeedIntermediate::Yes) {
          new_intermediate(intermediates, expr_id);
        }

        break;
      }
      case IR::OpCode::StartFunc: {
        IR::Types::StartFunc start_func;
        bc = IR::Read::StartFunc(bc, bc_end, start_func);

        for (usize i = 0; i < start_func.n_values; ++i) {
          ASSERT(i < this_convention->num_parameter_registers);//TEMP

          new_fixed_intermediate(intermediates, expr_id, this_convention->parameter_registers[i]);

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

        const GlobalLabelInfo l_info = comp->get_label_info(call.label);
        const SignatureStructure* sig_struct = l_info.signature;
        ASSERT(sig_struct != nullptr);

        bool has_return = sig_struct->return_type != comp_thread->builtin_types->t_void;

        const CallingConvention* call_convention = sig_struct->calling_convention;
        ASSERT(call_convention == this_convention);

        u32 call_space = call_convention->shadow_space_size;

        for (usize i = 0; i < ((usize)call.n_values - has_return); ++i) {
          IR::V_ARG arg;
          bc += IR::deserialize(bc, bc_end - bc, arg);

          ASSERT(Helpers::is_register_format(arg.format));

          VisitRes im = visit_ordered_value(values, lifetimes, resolver, arg, expr_id);

          if (i < call_convention->num_parameter_registers) {
            new_fixed_intermediate(intermediates, expr_id, call_convention->parameter_registers[i]);
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

          new_fixed_intermediate(intermediates, expr_id, call_convention->return_register);

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
          case IR::Format::sint16:
          case IR::Format::sint32:
          case IR::Format::sint64: {
            new_intermediate(intermediates, expr_id);
            break;
          }

          case IR::Format::opaque:
          case IR::Format::pointer:
          case IR::Format::slice:
          default: {
            comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                "Invalid Mul operands\n"
                "Left: {}, Right: {}",
                left.t, right.t);
            return {};
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

        ASSERT(left.t.struct_format() == right.t.struct_format());
        ASSERT(to.t.struct_format() == right.t.struct_format());
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
          
          case IR::Format::opaque:
          case IR::Format::pointer:
          case IR::Format::slice:
          default: {
            comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                "Invalid Div/Mod operands\n"
                "Left: {}, Right: {}",
                left.t, right.t);
            return {};
          }
        }

        if (right.ni == NeedIntermediate::Yes) {
          new_intermediate(intermediates, expr_id);
        }
        expr_id += 1;
        break;
      }

      case IR::OpCode::Mod: {
        IR::Types::Mod bin_op;
        bc = IR::Read::Mod(bc, bc_end, bin_op);
        VisitRes left = visit_ordered_value(values, lifetimes, resolver, bin_op.left, expr_id);
        VisitRes right = visit_ordered_value(values, lifetimes, resolver, bin_op.right, expr_id);
        VisitRes to = visit_ordered_value(values, lifetimes, resolver, bin_op.to, expr_id + 1);

        ASSERT(left.t.struct_format() == right.t.struct_format());
        ASSERT(to.t.struct_format() == right.t.struct_format());
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
          
          case IR::Format::opaque:
          case IR::Format::pointer:
          case IR::Format::slice:
          default: {
            comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                "Invalid Div/Mod operands\n"
                "Left: {}, Right: {}",
                left.t, right.t);
            return {};
          }
        }

        if (right.ni == NeedIntermediate::Yes) {
          new_intermediate(intermediates, expr_id);
        }
        expr_id += 1;
        break;
      }

      case IR::OpCode::Neg:// TODO
      case IR::OpCode::Not:// TODO
      default: {
        const Axle::ViewArr<const char> opcode_name = IR::opcode_string(op);
        if (opcode_name.data == nullptr) {
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
        const Type& return_type = resolver->sig_struct->return_type;
        ASSERT(return_type.size() <= 8);//TEMP

        IR::V_ARG v = IR::v_arg(current_block->cf_return.val, 0, return_type);

        [[maybe_unused]] auto vr = visit_ordered_value(values, lifetimes, resolver, v, expr_id);

        expr_id += 1;
        new_fixed_intermediate(intermediates, expr_id, this_convention->return_register);
        break;
      }

      case IR::ControlFlowType::Split: {
        IR::V_ARG v = IR::v_arg(current_block->cf_split.condition, 0, comp_thread->builtin_types->t_bool);

        [[maybe_unused]] auto vr = visit_ordered_value(values, lifetimes, resolver, v, expr_id);
        break;
      }
    }
  }

  // Create then colour the graph

  const usize number_of_registers = (usize)this_convention->num_volatile_registers
    + (usize)this_convention->num_non_volatile_registers;

  {
    // Start off doing intermediates since we can do those easily

    RegisterMapping* const i_start = intermediates.mut_begin();
    const RegisterMapping* const i_end = intermediates.end();

    const RegisterMapping* const i_mangle = mangled_registers.begin();
    const RegisterMapping* const i_mangle_end = mangled_registers.begin();

    for (RegisterMapping* i = i_start; i < i_end; ++i) {
      if (i->known_reg) {
        continue;
      }

      u64 used_regs = 0;

      for (const RegisterMapping* j = i_start; j < i_end; ++j) {
        if (j->expr_id < i->expr_id) continue;
        else if (j->expr_id > i->expr_id) break;

        ASSERT(i->expr_id == i->expr_id);
        if (j->known_reg) {
          used_regs |= 1llu << (usize)j->reg;
        }
      }

      for (const RegisterMapping* j = i_mangle; j < i_mangle_end; ++j) {
        if (j->expr_id < i->expr_id) continue;
        else if (j->expr_id > i->expr_id) break;

        ASSERT(i->expr_id == i->expr_id);
        if (j->known_reg) {
          used_regs |= 1llu << (usize)j->reg;
        }
      }

      u32 reg_index = 0;

      for (; reg_index < number_of_registers; ++reg_index) {
        u8 r = this_convention->all_regs_unordered[reg_index];
        if ((used_regs & (1llu << (usize)r)) == 0) {
          break;
        }
      }

      ASSERT(reg_index < number_of_registers);

      auto reg_id = this_convention->all_regs_unordered[reg_index];

      i->reg = reg_id;
      i->known_reg = true;

      used_registers |= (1llu << reg_id);
    }
  }

  const Axle::ViewArr<RegisterMapping> mappings[] = {
    view_arr(intermediates),
    view_arr(mangled_registers),
  };

  const Axle::OwnedArr<LifetimeEdge> edges = determine_edges(view_arr(lifetimes), view_arr(mappings));

  // Select the internal registers from the graph

  {
    if (comp_thread->print_options.register_select) {
      IO_Single::lock();
      IO_Single::format("L{}:\n", resolver->current_block->label.label - 1);
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

      if (v.map_type != MapType::Register) {
        while (edge_i < edge_end && edge_i->a == i) {//skip these for now
          edge_i += 1;
        }
        continue;//Only map register values
      }

      u32 used_regs = 0;

      ASSERT(edge_i == edge_end || edge_i->a >= i);

      while (edge_i < edge_end && edge_i->a == i) {
        u32 b = edge_i->b;

        if (b >= values.size + intermediates.size) {
          u32 v_i = static_cast<u32>(b - (values.size + intermediates.size));
          ASSERT(mangled_registers.data[v_i].known_reg);
          auto r = mangled_registers.data[v_i].reg;
          ASSERT(r < 32);
          used_regs |= 1u << (u32)r;
        }
        else if (b >= values.size) {
          u32 i_i = static_cast<u32>(b - values.size);
          ASSERT(intermediates.data[i_i].known_reg);
          auto r = intermediates.data[i_i].reg;
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
        reg_index = this_convention->num_volatile_registers;
      }

      for (; reg_index < number_of_registers; ++reg_index) {
        u8 r = this_convention->all_regs_unordered[reg_index];
        ASSERT(r < 32);
        if ((used_regs & (1u << r)) == 0) {
          break;
        }
      }

      //TODO: spill registers
      ASSERT(reg_index < number_of_registers);

      auto reg_id = this_convention->all_regs_unordered[reg_index];

      used_registers |= (1llu << reg_id);

      if (comp_thread->print_options.register_select) {
        IO_Single::format("  T{} = {}\n", i, X64::all_x64_regs[reg_id].name);
      }

      v.known_reg = true;
      v.reg = reg_id;
    }
  }

  return ResolvedMappings{
    call_space_needed,
      used_registers,

      bake_arr(std::move(intermediates)),
  };
}

struct ValueLocation {
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
  IR::Format val_format;
  u32 value_size;
  union {
    MemoryView mem = {};
    X64::R reg;
  };
};

struct X64IndirectValue {
  bool expects_intermeidate;
  ValueType value_type;
  IR::Format val_format;
  u32 value_size;
  u32 value_alignment;
  union {
    MemoryView mem = {};
    X64::R reg;
  };
};

struct Selector {
  Axle::ViewArr<const X64::R> intermediates;

  u32 intermediates_counter = 0;

  u8 base_ptr_reg;

  Axle::ViewArr<const ValueLocation> local_value_locations;
  Axle::ViewArr<const IR::SSATemp> local_temporaries;

  X64::R get_next_intermediate_reg() {
    ASSERT(intermediates_counter < intermediates.size);
    return intermediates[intermediates_counter++];
  }

  X64Value get_val(const IR::V_ARG& v) const {
    const u32 u = v.val.index;

    ASSERT(u < local_temporaries.size);
    const IR::SSATemp& temp = local_temporaries[u];

    const ValueLocation& a_temp = local_value_locations[u];

    X64Value val = {};

    val.val_format = v.format;
    if (v.format == IR::Format::opaque) {
      ASSERT(v.opaque_size > 0);
      val.value_size = v.opaque_size;
    }
    else {
      ASSERT(v.opaque_size == 0);
      val.value_size = x64_types_info.get_size(v.format);
    }

    ASSERT(val.value_size + v.offset <= temp.type.size());
    
    if (a_temp.is_register) {
      ASSERT(!temp.is_variable);
      ASSERT(v.offset == 0);
      ASSERT(temp.type.size() <= 8);
      ASSERT(!temp.requirements.has_address());

      val.expects_intermeidate = a_temp.maybe_intermeidate;
      val.value_type = ValueType::Register;
      val.reg = a_temp.reg_id;
    }
    else {
      MemoryView view = {};
      view.rm = X64::memory_rm(base_ptr_reg, (-static_cast<i32>(a_temp.stack_offset)) + static_cast<i32>(v.offset));
      view.size = val.value_size;
      
      const u32 align_diff = v.offset % temp.type.structure->alignment;
      view.known_alignment = align_diff == 0 ? 
        temp.type.structure->alignment: align_diff;

      val.expects_intermeidate = a_temp.maybe_intermeidate;
      val.value_type = ValueType::Memory;
      val.mem = view;
    }

    return val;
  }

  X64IndirectValue get_indirect_val(const IR::P_ARG& v) const {
    const u32 u = v.ptr.index;

    ASSERT(u < local_temporaries.size);
    const IR::SSATemp& temp = local_temporaries[u];

    const ValueLocation& a_temp = local_value_locations[u];

    ASSERT(temp.type.size() >= x64_types_info.get_size(IR::Format::pointer) + v.ptr_offset);
    
    X64IndirectValue val = {};

    val.val_format = v.val_format;
    if (v.val_format == IR::Format::opaque) {
      ASSERT(v.opaque_val_size > 0);
      val.value_size = v.opaque_val_size;
      val.value_alignment = v.opaque_val_alignment;
    }
    else {
      ASSERT(v.opaque_val_size == 0);
      ASSERT(v.opaque_val_alignment == 0);
      val.value_size = x64_types_info.get_size(v.val_format);
      val.value_alignment = x64_types_info.get_alignment(v.val_format);
    }
    
    if (a_temp.is_register) {
      ASSERT(!temp.is_variable);
      ASSERT(!temp.requirements.has_address());
      ASSERT(v.ptr_offset == 0);

      val.expects_intermeidate = a_temp.maybe_intermeidate;
      val.value_type = ValueType::Register;
      val.reg = a_temp.reg_id;
    }
    else {
      MemoryView view = {};
      view.rm = X64::memory_rm(base_ptr_reg, (-static_cast<i32>(a_temp.stack_offset)) + static_cast<i32>(v.ptr_offset));

      view.size = x64_types_info.get_size(IR::Format::pointer);
      view.known_alignment = 8;

      val.expects_intermeidate = a_temp.maybe_intermeidate;
      val.value_type = ValueType::Memory;
      val.mem = view;
    }

    return val;
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

void x64_emit_dyn_library_function(CompilerThread*, const IR::DynLibraryImport* lib_import, const CallingConvention*, Backend::ProgramData* program) {
  AXLE_TELEMETRY_FUNCTION();
  //X64::ProgramExtra* extra = static_cast<X64::ProgramExtra*>(program->extra);

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

void x64_init(CompilerGlobals* comp, CompilerThread* comp_thread,
    Backend::ProgramData* program) {
  X64::ProgramExtra* extra = static_cast<X64::ProgramExtra*>(program->extra);


  const SignatureStructure* type;

  comp->dyn_lib_imports.insert_uninit(1);

  IR::DynLibraryImport& lib = *comp->dyn_lib_imports.back();
  {
    Axle::AtomicLock<Axle::StringInterner> strings;
    Axle::AtomicLock<Structures> structs;

    comp->services.get_multiple({ .structures = &structs, .strings = &strings});


    lib.path = strings->intern(Axle::lit_view_arr("kernel32.dll"));
    lib.name = strings->intern(Axle::lit_view_arr("ExitProcess"));

    Axle::OwnedArr<Type> params = Axle::copy_arr<Type>({comp->builtin_types->t_u32});

    type = find_or_make_lambda_structure(structs._ptr, strings._ptr,
        &X64::CONVENTION_microsoft_x64,
        std::move(params), comp->builtin_types->t_void);
  }
  lib.label = comp->next_function_label(type, Span{});


  x64_emit_dyn_library_function(comp_thread, &lib, type->calling_convention, program);
  if (comp_thread->is_panic()) {
    return;
  }

  extra->exit_process = lib.label;
}

void x64_emit_start(CompilerGlobals* comp,
    IR::GlobalLabel entry,
    Backend::ProgramData* program) {
  AXLE_TELEMETRY_FUNCTION();
  X64::ProgramExtra* extra = static_cast<X64::ProgramExtra*>(program->extra);

  program->entry_point = entry;
  program->start_code.code_start = program->code_store.current_location().actual_location;

  {
    X64::Instruction init_stack = {};
    X64::R rbp_r = X64::R{ X64::rbp.REG };
    X64::R rsp_r = X64::R{ X64::rsp.REG };
    X64::mov(init_stack, X64::R64{rsp_r}, X64::R64{rbp_r});

    X64::append_instruction(program, init_stack);
  }

  constexpr const CallingConvention* exit_process_convention =
    &X64::CONVENTION_microsoft_x64;

  constexpr u32 shadow_space = exit_process_convention->shadow_space_size;
  static_assert(shadow_space == 32);
  {
    X64::Instruction stack = {};
    X64::R rsp_r = X64::R{ X64::rsp.REG };
    
    static_assert(shadow_space % 16 == 0);
    constexpr u32 stack_space = shadow_space + 8;
    static_assert(stack_space % 16 == 8);
    X64::sub(stack, X64::R64{rsp_r}, X64::IMM32{stack_space});

    X64::append_instruction(program, stack);
  }

  if (comp->build_options.debug_break_on_entry) {
    X64::Instruction trap = {};
    trap.insert(0xCC);
    X64::append_instruction(program, trap);
  }

  program->globals = std::move(comp->dynamic_inits);
  comp->dynamic_inits = {};

  FOR_MUT(program->globals, it) {
    if (program->data_store.total_size % it->alignment != 0) {
      const usize fill = it->alignment - (program->data_store.total_size % it->alignment);
      program->data_store.push_zeros(fill);
    }

    ASSERT(program->data_store.total_size % it->alignment == 0);
    it->data_index = program->data_store.total_size;

    if (it->constant_init) {
      program->data_store.push_arr(it->constant_value, it->size);
    }
    else {
      program->data_store.push_zeros(it->size);

      Backend::Relocation relocation = {};
      relocation.type = Backend::RelocationType::Label;
      relocation.label = it->init_expr_label;
      relocation.location = program->code_store.total_size + X64::CALL_NEAR_OFFSET;

      program->relocations.insert(std::move(relocation));

      X64::Instruction call_entry = {};
      X64::call_near(call_entry, 0);

      X64::append_instruction(program, call_entry);
    }
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
    relocation.label = extra->exit_process;
    relocation.location = program->code_store.total_size + X64::CALL_NEAR_OFFSET;
    program->relocations.insert(std::move(relocation));

    X64::Instruction call_exit = {};
    X64::call_near(call_exit, 0);

    X64::append_instruction(program, call_exit);
  }

  program->start_code.code_size = program->code_store.total_size - program->start_code.code_start;
}

static MemoryView pointer_view(X64::R pointer, u32 value_size, u32 value_alignment) noexcept {
  MemoryView view = {};
  view.rm = X64::memory_rm(pointer.r, 0);
  view.known_alignment = value_alignment;
  view.size = value_size;

  return view;
}

static MemoryView pointer_view(const X64IndirectValue& indirect) noexcept {
  ASSERT(indirect.value_type == ValueType::Register);
  return pointer_view(indirect.reg, indirect.value_size, indirect.value_alignment);
}

struct BlockResolveOutput {
  u32 used_regs;
  u32 temporary_offsets;
};

void x64_emit_function(CompilerGlobals* comp, CompilerThread* comp_thread, const IR::IRStore* ir, const CallingConvention* convention,
    Backend::ProgramData* program) {
  AXLE_TELEMETRY_FUNCTION();
  ASSERT(ir->completed);
  //X64::ProgramExtra* extra = static_cast<X64::ProgramExtra*>(program->extra);

  ASSERT(ir->global_label != IR::NULL_GLOBAL_LABEL);
  ASSERT(ir->control_blocks.size > 0);//means we did nothing

  const u8* const non_volatile_registers = convention->all_regs_unordered + convention->num_volatile_registers;

  //TODO: allow variables in registers
  Axle::OwnedArr variables_memory_location = Axle::new_arr<i32>(ir->variables.size);

  Axle::Array<X64::JumpRelocation> jump_relocations = {};

  IR::LocalLabel ret_label = { static_cast<u32>(ir->control_blocks.size) + 1 };

  Axle::OwnedArr local_label_real_offsets = Axle::new_arr<u32>(ret_label.label);

  u64 total_temporaries = 0;
  FOR(ir->control_blocks, b) {
    total_temporaries += b->temporaries.size;
  }

  bool calls = false;
  u32 call_space_used = 0;
  u32 used_registers = 0;

  Axle::Array<X64::R> intermediates = {};
  Axle::OwnedArr<RegisterMapping> temporary_mappings = Axle::new_arr<RegisterMapping>(total_temporaries);
  Axle::OwnedArr<BlockResolveOutput> resolve_outputs = Axle::new_arr<BlockResolveOutput>(ir->control_blocks.size);

  //Resolve the temporaries first
  {
    u32 temporaries_counter = 0;

    for (u32 i = 0; i < ir->control_blocks.size; ++i) {
      resolve_outputs[i].temporary_offsets = temporaries_counter;

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
      resolver.globals_used = const_view_arr(ir->globals_used);

      resolver.to_map = view_arr(temporary_mappings, temporaries_counter, b->temporaries.size);

      auto mappings = resolve_values(comp, comp_thread, &resolver, convention);
      if (comp_thread->is_panic()) {
        return;
      }

      temporaries_counter += static_cast<u32>(b->temporaries.size);

      if (resolver.has_called) {
        calls = resolver.has_called;
      }

      if (call_space_used < mappings.call_space_needed) {
        call_space_used = mappings.call_space_needed;
      }

      used_registers |= mappings.used_registers;
      resolve_outputs[i].used_regs = mappings.used_registers;

      intermediates.reserve_extra(mappings.intermediates.size);
      for (usize in = 0; in < mappings.intermediates.size; ++in) {
        const auto& v = mappings.intermediates[in];
        ASSERT(v.known_reg);
        intermediates.insert(X64::R{v.reg});
      }
    }

    ASSERT(temporaries_counter == total_temporaries);
  }

  u32 num_registers_to_save = 0;

  {
    for (u32 i = 0; i < convention->num_non_volatile_registers; ++i) {
      if ((used_registers & (1llu << non_volatile_registers[i])) > 0) {
        num_registers_to_save += 1;
      }
    }
  }


  u32 stack_top = ir->max_stack;

  const u32 register_save_space = 8 * num_registers_to_save;
  stack_top += register_save_space;//will remove this size from the actual count later, here for alignment reasons

  //Map the resolved values back to their real locations (and resolve stack values too)

  Axle::OwnedArr<ValueLocation> temp_vals = Axle::new_arr<ValueLocation>(temporary_mappings.size);

  {
    const u32 basic_stack_size = stack_top;
    u64 temporary_counter = 0;

    FOR(ir->control_blocks, blck) {
      u32 block_stack = basic_stack_size;

      for (u32 i = 0; i < blck->temporaries.size; ++i) {
        const auto& mapping = temporary_mappings[i + temporary_counter];
        auto& actual = temp_vals[i + temporary_counter];

        if (blck->temporaries.data[i].is_variable) {
          const auto var_id = blck->temporaries.data[i].var_id;
          const auto& var = ir->variables.data[var_id.variable];

          actual.maybe_intermeidate = true;
          actual.is_register = false;
          actual.stack_offset = var.stack_offset + var.type.size();
        }
        else if (mapping.map_type == MapType::Register) {
          ASSERT(mapping.known_reg);
          actual.maybe_intermeidate = false;
          actual.is_register = true;
          actual.reg_id = X64::R{ mapping.reg };
        }
        else if (mapping.map_type == MapType::Memory) {
          actual.maybe_intermeidate = true;
          actual.is_register = false;
          const IR::SSATemp& t = blck->temporaries.data[i];

          block_stack = Axle::ceil_to_n(stack_top, t.type.structure->alignment);
          block_stack += t.type.size();
          actual.stack_offset = block_stack;
        }
        else {
          INVALID_CODE_PATH("Invalid map type");
        }
      }

      if (block_stack > stack_top) stack_top = block_stack;

      temporary_counter += blck->temporaries.size;
    }

    ASSERT(temporary_counter == total_temporaries);

  }
  
  temporary_mappings.free();//Done with this. I wish there was a way to delete a variable from scope :(

  stack_top = Axle::ceil_to_8(stack_top);
  stack_top += call_space_used;

  if (calls) {
    //align to call alignment
    stack_top = Axle::ceil_to_n<u32>(stack_top + 8, 16) - 8;
    ASSERT(stack_top % 16 == 8);
  }

  stack_top -= register_save_space;

  //Start emitting code
  Backend::FunctionMetadata func = {};
  func.code_start = program->code_store.current_location().actual_location;

  Selector selector = {};
  selector.intermediates = const_view_arr(intermediates);
  selector.intermediates_counter = 0;
  selector.base_ptr_reg = convention->base_pointer_reg;

  FOR(ir->control_blocks, blck) {
    IR::LocalLabel blck_label = blck->label;

    {
      u64 actual_values_offset = resolve_outputs[blck_label.label - 1].temporary_offsets;

      selector.local_value_locations = const_view_arr(temp_vals, actual_values_offset, blck->temporaries.size);
    }

    selector.local_temporaries = const_view_arr(blck->temporaries);

    const u8* bc = blck->bytecode.begin();
    const u8* const bc_end = blck->bytecode.end();

    {
      u32 relative = relative_offset(func.code_start, program->code_store.total_size);
      local_label_real_offsets[blck->label.label - 1] = relative;
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
          ASSERT(to.value_size == set.data.size);

          switch (to.value_type) {
            case ValueType::Register: {
              Helpers::load_const_to_reg(program, to.val_format, to.reg, Axle::view_arr(set.data));
              break;
            }
            case ValueType::Memory: {
              Helpers::load_const_to_mem(program, to.val_format, to.mem, Axle::view_arr(set.data));
              break;
            }
          }

          break;
        }
        case IR::OpCode::SetStore: {
          IR::Types::SetStore set;
          bc = IR::Read::SetStore(bc, bc_end, set);

          X64IndirectValue to = selector.get_indirect_val(set.to);

          ASSERT(to.value_size == set.data.size);

          switch (to.value_type) {
            case ValueType::Register: {
              if (to.expects_intermeidate) {
                X64::R _temp = selector.get_next_intermediate_reg();
              }

              MemoryView view = pointer_view(to);

              Helpers::load_const_to_mem(program, to.val_format, view, Axle::view_arr(set.data));
              break;
            }
            case ValueType::Memory: {
              X64::R temp = selector.get_next_intermediate_reg();
              Helpers::copy_mem_to_reg(program,
                  to.mem, IR::Format::pointer,
                  temp, IR::Format::pointer);

              MemoryView view = pointer_view(temp, to.value_size, to.value_alignment);

              Helpers::load_const_to_mem(program, to.val_format, view, Axle::view_arr(set.data));
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
                      from.reg, from.val_format,
                      to.reg, to.val_format);
                  break;
                }
                case ValueType::Memory: {
                  Helpers::copy_mem_to_reg(program,
                      from.mem, from.val_format,
                      to.reg, to.val_format);
                  break;
                }
              }
              break;
            }
            case ValueType::Memory: {
              switch (from.value_type) {
                case ValueType::Register: {
                  Helpers::copy_reg_to_mem(program,
                      from.reg, from.val_format,
                      to.mem, to.val_format);
                  break;
                }
                case ValueType::Memory: {
                  X64::R temp = selector.get_next_intermediate_reg();

                  Helpers::copy_mem_to_mem(program,
                      from.mem, from.val_format,
                      to.mem, to.val_format,
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

          X64IndirectValue from = selector.get_indirect_val(copy.from);
          X64Value to = selector.get_val(copy.to);

          MemoryView view;
          switch (from.value_type) {
            case ValueType::Register: {
              if (from.expects_intermeidate) {
                X64::R _temp = selector.get_next_intermediate_reg();
              }

              view = pointer_view(from);
              break;
            }
            case ValueType::Memory: {
              X64::R temp = selector.get_next_intermediate_reg();

              IntHelpers::copy_mem_to_reg(program,
                  from.mem, IR::Format::pointer,
                  temp, IR::Format::pointer);

              view = pointer_view(temp, from.value_size, from.value_alignment);
              break;
            }
          }

          switch (to.value_type) {
            case ValueType::Register: {
              if (to.expects_intermeidate) {
                X64::R _temp = selector.get_next_intermediate_reg();
              }

              Helpers::copy_mem_to_reg(program,
                  view, from.val_format,
                  to.reg, to.val_format);
              break;

            }
            case ValueType::Memory: {
              X64::R temp = selector.get_next_intermediate_reg();

              Helpers::copy_mem_to_mem(program,
                  view, from.val_format,
                  to.mem, to.val_format,
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
          X64IndirectValue to = selector.get_indirect_val(copy.to);

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

                  MemoryView view = pointer_view(to);

                  Helpers::copy_reg_to_mem(program,
                      from.reg, from.val_format,
                      view, to.val_format);
                  break;
                }
                case ValueType::Memory: {
                  MemoryView view = pointer_view(to);

                  X64::R temp = selector.get_next_intermediate_reg();

                  if (to.expects_intermeidate) {
                    X64::R _temp = selector.get_next_intermediate_reg();
                  }

                  Helpers::copy_mem_to_mem(program,
                      from.mem, from.val_format,
                      view, to.val_format, temp);
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
                  IntHelpers::copy_mem_to_reg(program,
                      to.mem, IR::Format::pointer,
                      temp_ptr, IR::Format::pointer);

                  MemoryView view = pointer_view(temp_ptr, to.value_size, to.value_alignment);

                  Helpers::copy_reg_to_mem(program,
                      from.reg, from.val_format,
                      view, to.val_format);
                  break;
                }
                case ValueType::Memory: {
                  X64::R temp = selector.get_next_intermediate_reg();

                  X64::R temp_ptr = selector.get_next_intermediate_reg();
                  IntHelpers::copy_mem_to_reg(program,
                      to.mem, IR::Format::pointer,
                      temp_ptr, IR::Format::pointer);

                  MemoryView view = pointer_view(temp_ptr, to.value_size, to.value_alignment);

                  Helpers::copy_mem_to_mem(program,
                      from.mem, from.val_format,
                      view, to.val_format,
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

          X64IndirectValue from = selector.get_indirect_val(copy.from);
          X64IndirectValue to = selector.get_indirect_val(copy.to);

          X64::R temp = selector.get_next_intermediate_reg();

          MemoryView from_mem = {};
          switch (from.value_type) {
            case ValueType::Register: {
              if (from.expects_intermeidate) {
                X64::R _temp = selector.get_next_intermediate_reg();
              }

              from_mem = pointer_view(from);
              break;
            }
            case ValueType::Memory: {
              X64::R m_temp = selector.get_next_intermediate_reg();
              IntHelpers::copy_mem_to_reg(program,
                  from.mem, IR::Format::pointer,
                  m_temp, IR::Format::pointer);
              from_mem = pointer_view(m_temp, from.value_size, from.value_alignment);
              break;
            }
          }

          MemoryView to_mem = {};
          switch (to.value_type) {
            case ValueType::Register: {
              if (to.expects_intermeidate) {
                X64::R _temp = selector.get_next_intermediate_reg();
              }

              to_mem = pointer_view(to);
              break;
            }
            case ValueType::Memory: {
              X64::R m_temp = selector.get_next_intermediate_reg();
              IntHelpers::copy_mem_to_reg(program,
                  to.mem, IR::Format::pointer,
                  m_temp, IR::Format::pointer);
              to_mem = pointer_view(m_temp, to.value_size, to.value_alignment);
              break;
            }
          }

          Helpers::copy_mem_to_mem(program,
              from_mem, from.val_format,
              to_mem, to.val_format, temp);
          break;
        }
        case IR::OpCode::AddrOf: {
          IR::Types::AddrOf addr;
          bc = IR::Read::AddrOf(bc, bc_end, addr);

          X64Value f = selector.get_val(addr.from);
          X64Value t = selector.get_val(addr.to);
          ASSERT(t.val_format == IR::Format::pointer);
          ASSERT(t.value_size == x64_types_info.get_size(IR::Format::pointer));

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

              IntHelpers::copy_reg_to_mem(program,
                  temp, IR::Format::pointer,
                  t.mem, IR::Format::pointer);
            } break;
          }

          break;
        }
        case IR::OpCode::AddrOfLoad: {
          IR::Types::AddrOfLoad addr;
          bc = IR::Read::AddrOfLoad(bc, bc_end, addr);

          X64IndirectValue f = selector.get_indirect_val(addr.from);

          X64Value t = selector.get_val(addr.to);
          ASSERT(t.val_format == IR::Format::pointer);
          ASSERT(t.value_size == x64_types_info.get_size(IR::Format::pointer));

          MemoryView from_mem = {};
          switch (f.value_type) {
            case ValueType::Register: {
              if (f.expects_intermeidate) {
                X64::R _temp = selector.get_next_intermediate_reg();
              }
              from_mem = pointer_view(f);
              break;
            }
            case ValueType::Memory: {
              X64::R temp = selector.get_next_intermediate_reg();
              IntHelpers::copy_mem_to_reg(program,
                  f.mem, IR::Format::pointer,
                  temp, IR::Format::pointer);

              from_mem = pointer_view(temp, f.value_size, f.value_alignment);
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
              IntHelpers::copy_reg_to_mem(program,
                  temp, IR::Format::pointer,
                  t.mem, IR::Format::pointer);
            } break;
          }

          break;
        }
        case IR::OpCode::AddrOfGlobal: {
          IR::Types::AddrOfGlobal addr;
          bc = IR::Read::AddrOfGlobal(bc, bc_end, addr);

          const IR::GlobalReference& global_r = ir->globals_used[addr.im32];

          X64Value g = selector.get_val(addr.val);
          ASSERT(g.val_format == IR::Format::pointer);
          ASSERT(g.value_size == x64_types_info.get_size(IR::Format::pointer));

          switch (g.value_type) {
            case ValueType::Register: {
              if (g.expects_intermeidate) {
                X64::R _temp = selector.get_next_intermediate_reg();
              }

              Backend::Relocation reloc = {};
              reloc.type = Backend::RelocationType::Global;
              reloc.global_index = (u32)global_r.data_member;
              reloc.location = program->code_store.total_size + X64::LEA_RM_DISP_OFFSET;

              program->relocations.insert(reloc);

              X64::Instruction inst = {};
              X64::lea(inst, X64::IMM32{0}, g.reg);
              X64::append_instruction(program, inst);

              break;
            }
            case ValueType::Memory: {
              X64::R temp = selector.get_next_intermediate_reg();

              Backend::Relocation reloc = {};
              reloc.type = Backend::RelocationType::Global;
              reloc.global_index = (u32)global_r.data_member;
              reloc.location = program->code_store.total_size + X64::LEA_RM_DISP_OFFSET;

              program->relocations.insert(reloc);

              X64::Instruction inst = {};

              X64::RM rm = {};
              rm.indirect = true;
              rm.disp = 0;
              rm.r = X64::rbp.REG;

              X64::lea(inst, X64::IMM32{0}, g.reg);
              X64::append_instruction(program, inst);

              IntHelpers::copy_reg_to_mem(program, temp, IR::Format::uint64, g.mem, IR::Format::uint64);
              break;
            }
          }

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
          
            //TODO: parameters of different types
            ASSERT(Helpers::is_register_format(arg.format));
            ASSERT(arg.offset == 0);//temp - need to fix this
            
            X64::R r = selector.get_next_intermediate_reg();
            
            X64Value to = selector.get_val(arg);
            
            {
              const Type& param = ir->signature->parameter_types[i];
              
              if (Eval::must_pass_type_by_reference(convention, param.structure)) {
                ASSERT(to.val_format == IR::Format::pointer);
                ASSERT(to.value_size == x64_types_info.get_size(IR::Format::pointer));
              }
              else {
                ASSERT(to.val_format == param.struct_format());
                ASSERT(to.value_size == param.size());
              }
            }

            switch (to.value_type) {
              case ValueType::Register: {
                Helpers::copy_reg_to_reg(program,
                    r, to.val_format,
                    to.reg, to.val_format);
                break;
              }
              case ValueType::Memory: {
                Helpers::copy_reg_to_mem(program,
                    r, to.val_format,
                    to.mem, to.val_format);
                break;
              }
            }
          }

          break;
        }
        case IR::OpCode::Call: {
          ASSERT((num_registers_to_save * 8 + stack_top) % 16 == 8);
          
          IR::Types::Call call;
          bc = IR::Read::Call(bc, bc_end, call);
          ASSERT(call.values == nullptr);

          const GlobalLabelInfo l_info = comp->get_label_info(call.label);
          const SignatureStructure* sig_struct = l_info.signature;
          const bool has_return = sig_struct->return_type != comp_thread->builtin_types->t_void;
          
          ASSERT(convention == sig_struct->calling_convention);
          ASSERT(call_space_used >= convention->shadow_space_size);

          for (usize i = 0; i < ((usize)call.n_values - has_return); ++i) {
            IR::V_ARG arg;
            bc += IR::deserialize(bc, bc_end - bc, arg);
          
            ASSERT(Helpers::is_register_format(arg.format));

            X64Value arg_v = selector.get_val(arg);
            ASSERT(arg_v.value_size <= 8);

            if (i < convention->num_parameter_registers) {
              X64::R arg_reg = selector.get_next_intermediate_reg();

              switch (arg_v.value_type) {
                case ValueType::Register: {
                  Helpers::copy_reg_to_reg(program,
                      arg_v.reg, arg_v.val_format,
                      arg_reg, arg_v.val_format);
                  break;
                }
                case ValueType::Memory: {
                  Helpers::copy_mem_to_reg(program,
                      arg_v.mem, arg_v.val_format,
                      arg_reg, arg_v.val_format);
                  break;
                }
              }
            }
            else {
              MemoryView arg_mem = {};
              
              const Type& param = sig_struct->parameter_types[i];
              
              if (Eval::must_pass_type_by_reference(convention, param.structure)) {
                ASSERT(arg_v.val_format == IR::Format::pointer);
                ASSERT(arg_v.value_size == x64_types_info.get_size(IR::Format::pointer));
                arg_mem.known_alignment = x64_types_info.get_alignment(IR::Format::pointer);
                arg_mem.size = x64_types_info.get_size(IR::Format::pointer);
              }
              else {
                ASSERT(arg_v.val_format == param.struct_format());
                ASSERT(arg_v.value_size == param.size());
                arg_mem.known_alignment = param.structure->alignment;
                arg_mem.size = param.size();
              }

              arg_mem.rm = X64::memory_rm(convention->base_pointer_reg, -static_cast<i32>(stack_top - (i * 8)));
              
              switch (arg_v.value_type) {
                case ValueType::Register: {
                  if (arg_v.expects_intermeidate) {
                    X64::R _temp = selector.get_next_intermediate_reg();
                  }

                  Helpers::copy_reg_to_mem(program,
                      arg_v.reg, arg_v.val_format,
                      arg_mem, arg_v.val_format);
                  break;
                }
                case ValueType::Memory: {
                  X64::R temp = selector.get_next_intermediate_reg();

                  Helpers::copy_mem_to_mem(program,
                      arg_v.mem, arg_v.val_format,
                      arg_mem, arg_v.val_format, temp);
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

            switch (ret_v.value_type) {
              case ValueType::Register: {
                Helpers::copy_reg_to_reg(program,
                    ret_reg, ret_v.val_format,
                    ret_v.reg, ret_v.val_format);
                break;
              }
              case ValueType::Memory: {
                Helpers::copy_reg_to_mem(program,
                    ret_reg, ret_v.val_format,
                    ret_v.mem, ret_v.val_format);
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
          /*always a left register*/\
          X64::R left_reg = selector.get_next_intermediate_reg();\
          switch (left.value_type) {\
            case ValueType::Memory: Helpers::copy_mem_to_reg(program, left.mem, left.val_format, left_reg, left.val_format); break;\
            case ValueType::Register: Helpers::copy_reg_to_reg(program, left.reg, left.val_format, left_reg, left.val_format); break;\
          }\
          X64::R right_reg;\
          switch (right.value_type) {\
            case ValueType::Memory: {\
              right_reg = selector.get_next_intermediate_reg();\
              Helpers::copy_mem_to_reg(program, right.mem, right.val_format, right_reg, right.val_format); break;\
            }\
            case ValueType::Register: {\
              if(right.expects_intermeidate) {[[maybe_unused]] X64::R _temp = selector.get_next_intermediate_reg(); }\
              right_reg = right.reg; break;\
            }\
          }\
          X64::R out_reg = helper (program, left_reg, left.val_format, right_reg, right.val_format);\
          switch (to.value_type) {\
            case ValueType::Register: Helpers::copy_reg_to_reg(program, out_reg, left.val_format, to.reg, to.val_format); break;\
            case ValueType::Memory: Helpers::copy_reg_to_mem(program, out_reg, left.val_format, to.mem, to.val_format); break;\
          }\
          break;\
        }\

        EMIT_BIN_OP(Add, Helpers::emit_add);
        EMIT_BIN_OP(Sub, Helpers::emit_sub);
        EMIT_BIN_OP(Mul, Helpers::emit_mul);
        EMIT_BIN_OP(And, Helpers::emit_and);
        EMIT_BIN_OP(Div, Helpers::emit_div);
        EMIT_BIN_OP(Mod, Helpers::emit_mod);
        EMIT_BIN_OP(Or, Helpers::emit_or);
        EMIT_BIN_OP(Xor, Helpers::emit_xor);
#undef EMIT_BIN_OP
#define EMIT_BIN_OP_CMP(name, helper)\
        case IR::OpCode:: name: {\
          const IR::Format f_bool = IR::Format::uint8;\
          IR::Types:: name bin_op;\
          bc = IR::Read:: name (bc, bc_end, bin_op);\
          X64Value left = selector.get_val(bin_op.left);\
          X64Value right = selector.get_val(bin_op.right);\
          X64Value to = selector.get_val(bin_op.to);\
          /*always a left register*/\
          X64::R left_reg = selector.get_next_intermediate_reg();\
          switch (left.value_type) {\
            case ValueType::Memory: Helpers::copy_mem_to_reg(program, left.mem, left.val_format, left_reg, left.val_format); break;\
            case ValueType::Register: Helpers::copy_reg_to_reg(program, left.reg, left.val_format, left_reg, left.val_format); break;\
          }\
          X64::R right_reg;\
          switch (right.value_type) {\
            case ValueType::Memory: {\
              right_reg = selector.get_next_intermediate_reg();\
              Helpers::copy_mem_to_reg(program, right.mem, right.val_format, right_reg, right.val_format); break;\
            }\
            case ValueType::Register: {\
              if(right.expects_intermeidate) { [[maybe_unused]]X64::R _temp = selector.get_next_intermediate_reg(); }\
              right_reg = right.reg; break;\
            }\
          }\
          helper (program, left_reg, left.val_format, right_reg, right.val_format);\
          switch (to.value_type) {\
            case ValueType::Register: Helpers::copy_reg_to_reg(program, left_reg, f_bool, to.reg, to.val_format); break;\
            case ValueType::Memory: Helpers::copy_reg_to_mem(program, left_reg, f_bool, to.mem, to.val_format); break;\
          }\
          break;\
        }

        EMIT_BIN_OP_CMP(Great, Helpers::emit_great);
        EMIT_BIN_OP_CMP(Less, Helpers::emit_less);
        EMIT_BIN_OP_CMP(Eq, Helpers::emit_eq);
        EMIT_BIN_OP_CMP(Neq, Helpers::emit_neq);

        case IR::OpCode::Neg:// TODO
        case IR::OpCode::Not:// TODO
        default: {
          const Axle::ViewArr<const char> opcode_name = IR::opcode_string(op);
          if (opcode_name.data == nullptr) {
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

    const auto jump_to = [&](IR::LocalLabel l) {
      ASSERT(l != IR::NULL_LOCAL_LABEL);
      ASSERT(l.label - 1 < ir->control_blocks.size);

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
        jump_to(blck->cf_start.child);
        break;
      }

      case IR::ControlFlowType::Return: {
        X64Value from = selector.get_val(IR::v_arg(blck->cf_return.val, 0, ir->signature->return_type));

        X64::R ret_reg = selector.get_next_intermediate_reg();
        ASSERT(ret_reg.r == convention->return_register);

        switch (from.value_type) {
          case ValueType::Register: {
            Helpers::copy_reg_to_reg(program, from.reg, from.val_format, ret_reg, from.val_format);
            break;
          }
          case ValueType::Memory: {
            Helpers::copy_mem_to_reg(program, from.mem, from.val_format, ret_reg, from.val_format);
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
        jump_to(blck->cf_inline.child);
        break;
      }
      case IR::ControlFlowType::Merge: {
        jump_to(blck->cf_merge.child);
        break;
      }

      case IR::ControlFlowType::Split: {
        X64Value val = selector.get_val(IR::v_arg(blck->cf_split.condition, 0, comp->builtin_types->t_bool));

        ASSERT(val.val_format == IR::Format::uint8);

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
    Axle::serialize_le<u32>(bytes, offset);
    code_itr.overwrite_arr(bytes, 4);
  }

  ASSERT(code_itr.actual_location <= program->code_store.total_size);

  void print_x86_64(const GlobalLabelInfo& l_info, Backend::DataBucketIterator start, const Backend::DataBucketIterator end);
  if (comp_thread->print_options.finished_mc) {
    GlobalLabelInfo l_info = comp->get_label_info(ir->global_label);
    print_x86_64(l_info, start_itr, program->code_store.current_location());
  }
}

struct RegisterNames {
  Axle::OwnedArr<char> r;
  Axle::OwnedArr<char> rm;
};

static Axle::ViewArr<const char> b8_no_rex_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return Axle::lit_view_arr("al");
    case 1: return Axle::lit_view_arr("cl");
    case 2: return Axle::lit_view_arr("dl");
    case 3: return Axle::lit_view_arr("bl");
    case 4: return Axle::lit_view_arr("ah");
    case 5: return Axle::lit_view_arr("ch");
    case 6: return Axle::lit_view_arr("dh");
    case 7: return Axle::lit_view_arr("bh");
  }

  return Axle::lit_view_arr("INVALID REGISTER");
}

static Axle::ViewArr<const char> b8_rex_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return Axle::lit_view_arr("al");
    case 1: return Axle::lit_view_arr("cl");
    case 2: return Axle::lit_view_arr("dl");
    case 3: return Axle::lit_view_arr("bl");
    case 4: return Axle::lit_view_arr("spl");
    case 5: return Axle::lit_view_arr("bpl");
    case 6: return Axle::lit_view_arr("sil");
    case 7: return Axle::lit_view_arr("dil");
    case 8: return Axle::lit_view_arr("r8b");
    case 9: return Axle::lit_view_arr("r9b");
    case 10: return Axle::lit_view_arr("r10b");
    case 11: return Axle::lit_view_arr("r11b");
    case 12: return Axle::lit_view_arr("r12b");
    case 13: return Axle::lit_view_arr("r13b");
    case 14: return Axle::lit_view_arr("r14b");
    case 15: return Axle::lit_view_arr("r15b");
  }

  return Axle::lit_view_arr("INVALID REGISTER");
}

static Axle::ViewArr<const char> b16_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return Axle::lit_view_arr("ax");
    case 1: return Axle::lit_view_arr("cx");
    case 2: return Axle::lit_view_arr("dx");
    case 3: return Axle::lit_view_arr("bx");
    case 4: return Axle::lit_view_arr("sp");
    case 5: return Axle::lit_view_arr("bp");
    case 6: return Axle::lit_view_arr("si");
    case 7: return Axle::lit_view_arr("di");
    case 8: return Axle::lit_view_arr("r8w");
    case 9: return Axle::lit_view_arr("r9w");
    case 10: return Axle::lit_view_arr("r10w");
    case 11: return Axle::lit_view_arr("r11w");
    case 12: return Axle::lit_view_arr("r12w");
    case 13: return Axle::lit_view_arr("r13w");
    case 14: return Axle::lit_view_arr("r14w");
    case 15: return Axle::lit_view_arr("r15w");
  }

  return Axle::lit_view_arr("INVALID REGISTER");
}

static Axle::ViewArr<const char> b32_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return Axle::lit_view_arr("eax");
    case 1: return Axle::lit_view_arr("ecx");
    case 2: return Axle::lit_view_arr("edx");
    case 3: return Axle::lit_view_arr("ebx");
    case 4: return Axle::lit_view_arr("esp");
    case 5: return Axle::lit_view_arr("ebp");
    case 6: return Axle::lit_view_arr("esi");
    case 7: return Axle::lit_view_arr("edi");
    case 8: return Axle::lit_view_arr("r8d");
    case 9: return Axle::lit_view_arr("r9d");
    case 10: return Axle::lit_view_arr("r10d");
    case 11: return Axle::lit_view_arr("r11d");
    case 12: return Axle::lit_view_arr("r12d");
    case 13: return Axle::lit_view_arr("r13d");
    case 14: return Axle::lit_view_arr("r14d");
    case 15: return Axle::lit_view_arr("r15d");
  }

  return Axle::lit_view_arr("INVALID REGISTER");
}

static Axle::ViewArr<const char> b64_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return Axle::lit_view_arr("rax");
    case 1: return Axle::lit_view_arr("rcx");
    case 2: return Axle::lit_view_arr("rdx");
    case 3: return Axle::lit_view_arr("rbx");
    case 4: return Axle::lit_view_arr("rsp");
    case 5: return Axle::lit_view_arr("rbp");
    case 6: return Axle::lit_view_arr("rsi");
    case 7: return Axle::lit_view_arr("rdi");
    case 8: return Axle::lit_view_arr("r8");
    case 9: return Axle::lit_view_arr("r9");
    case 10: return Axle::lit_view_arr("r10");
    case 11: return Axle::lit_view_arr("r11");
    case 12: return Axle::lit_view_arr("r12");
    case 13: return Axle::lit_view_arr("r13");
    case 14: return Axle::lit_view_arr("r14");
    case 15: return Axle::lit_view_arr("r15");
  }

  return Axle::lit_view_arr("INVALID REGISTER");
}

struct x86PrintOptions {
  Axle::FUNCTION_PTR<Axle::ViewArr<const char>, uint8_t> r_name = nullptr;
  Axle::FUNCTION_PTR<Axle::ViewArr<const char>, uint8_t> rm_name = nullptr;
  Axle::FUNCTION_PTR<Axle::ViewArr<const char>, uint8_t> mem_r_name = nullptr;
  Axle::ViewArr<const char> mem_size = {};
};

u16 x16_from_itr(Backend::DataBucketIterator* itr) {
  u16 v;
  bool res = Axle::deserialize_le<u16>(*itr, v);
  ASSERT(res);
  return v;
}

u32 x32_from_itr(Backend::DataBucketIterator* itr) {
  u32 v;
  bool res = Axle::deserialize_le<u32>(*itr, v);
  ASSERT(res);
  return v;
}

u64 x64_from_itr(Backend::DataBucketIterator* itr) {
  u64 v;
  bool res = Axle::deserialize_le<u64>(*itr, v);
  ASSERT(res);
  return v;
}

static Axle::OwnedArr<char> rm_reg_string(x86PrintOptions* const p_opts,
    uint8_t rex, uint8_t modrm, Backend::DataBucketIterator* rest) {
  uint8_t address_mode = (modrm & 0b11'000000) >> 6;
  uint8_t rm = modrm & X64::MODRM_RM_MASK;

  if (address_mode == 0b11) {
    rm |= ((rex & X64::REX_B) << X64::REX_B_SHIFT);

    return Axle::format("{}", p_opts->rm_name(rm));
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

            return Axle::format("{} [{}]", p_opts->mem_size, disp);
          }
          else if (INDEX_RSP) {
            return Axle::format("{} [{}]", p_opts->mem_size, p_opts->mem_r_name(base));
          }
          else if (BASE_RBP) {
            int32_t disp = x32_from_itr(rest);

            char sign = disp >= 0 ? '+' : '-';
            if (scale == 1) {
              return Axle::format("{} [{} {} {}]", p_opts->mem_size, p_opts->mem_r_name(index), sign, Axle::absolute(disp));
            }
            else {
              return Axle::format("{} [({} * {}) {} {}]", p_opts->mem_size, p_opts->mem_r_name(index), scale, sign, Axle::absolute(disp));
            }
          }
          else if (scale == 1) {
            return Axle::format("{} [{} + {}]",
                p_opts->mem_size,
                p_opts->mem_r_name(base),
                p_opts->mem_r_name(index));
          }
          else {
            return Axle::format("{} [{} + ({} * {})]",
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
            return Axle::format("{} [{} {} {}]",
                p_opts->mem_size,
                p_opts->mem_r_name(base),
                sign, Axle::absolute(disp));
          }
          else {
            if (scale == 1) {
              return Axle::format("{} [{} {} {} + {}]",
                  p_opts->mem_size,
                  p_opts->mem_r_name(base),
                  sign, Axle::absolute(disp),
                  p_opts->mem_r_name(index));
            }
            else {
              return Axle::format("{} [{} {} {} + ({} * {})]",
                  p_opts->mem_size,
                  p_opts->mem_r_name(base),
                  sign, Axle::absolute(disp),
                  p_opts->mem_r_name(index), scale);
            }
          }
        }
        case 0b10: {
          int32_t disp = x32_from_itr(rest);

          char sign = disp >= 0 ? '+' : '-';

          if (INDEX_RSP) {
            return Axle::format("{} [{} {} {}]",
                p_opts->mem_size,
                p_opts->mem_r_name(base),
                sign, Axle::absolute(disp));
          }
          else {
            if (scale == 1) {
              return Axle::format("{} [{} + {} {} {}]",
                  p_opts->mem_size,
                  p_opts->mem_r_name(base),
                  p_opts->mem_r_name(index),
                  sign, Axle::absolute(disp));
            }
            else {
              return Axle::format("{} [{} + ({} * {}) {} {}]",
                  p_opts->mem_size,
                  p_opts->mem_r_name(base),
                  p_opts->mem_r_name(index), scale,
                  sign, Axle::absolute(disp));
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

        return Axle::format("{} [RIP {} {}]", p_opts->mem_size, sign, Axle::absolute(disp));
      }

      goto NORMAL_MODRM;
    }
    default: {
    NORMAL_MODRM:
      rm |= ((rex & X64::REX_B) << X64::REX_B_SHIFT);

      switch (address_mode) {
        case 0b00: {
          return Axle::format("{} [{}]", p_opts->mem_size, p_opts->mem_r_name(rm));
        }
        case 0b01: {
          int8_t disp = rest->read_byte();

          char sign = disp >= 0 ? '+' : '-';

          return Axle::format("{} [{} {} {}]", p_opts->mem_size, p_opts->mem_r_name(rm), sign, Axle::absolute(disp));
        }
        case 0b10: {
          int32_t disp = x32_from_itr(rest);

          char sign = disp >= 0 ? '+' : '-';

          return Axle::format("{} [{} {} {}]", p_opts->mem_size, p_opts->mem_r_name(rm), sign, Axle::absolute(disp));
        }
      }

      INVALID_CODE_PATH("Internal error. Unrecognised assembly code register format");
    }
  }
}

static Axle::OwnedArr<char> r_reg_string(x86PrintOptions* p_opts,
    uint8_t rex, uint8_t modrm) {
  uint8_t r = ((rex & X64::REX_R) << X64::REX_R_SHIFT)
    | ((modrm & X64::MODRM_REG_MASK) >> X64::MODRM_REG_SHIFT);

  return Axle::format("{}", p_opts->r_name(r));
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
    ops->mem_size = Axle::lit_view_arr("BYTE PTR");
  }
  else {
    ops->r_name = b8_no_rex_reg_name;
    ops->rm_name = b8_no_rex_reg_name;
    ops->mem_size = Axle::lit_view_arr("BYTE PTR");
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
    ops->mem_size = Axle::lit_view_arr("QWORD PTR");
  }
  else {
    if (short_operand) {
      ops->r_name = b16_reg_name;
      ops->rm_name = b16_reg_name;
      ops->mem_size = Axle::lit_view_arr("WORD PTR");
    }
    else {
      ops->r_name = b32_reg_name;
      ops->rm_name = b32_reg_name;
      ops->mem_size = Axle::lit_view_arr("DWORD PTR");
    }
  }
}

struct HexOffset {
  u64 n;
};

namespace Axle::Format {
  template<>
    struct FormatArg<HexOffset> {
      template<Formatter F>
        constexpr static void load_string(F& res, HexOffset ho) {
          u64 in = ho.n;

          constexpr size_t LEN = 16;

          char string_res[2 + LEN] = {
            '0', '0',
            '0', '0', '0', '0', '0', '0', '0', '0',
            '0', '0', '0', '0', '0', '0', '0', '0'
          };

          for (u32 k = 0; k < 4; ++k) {
            for (u32 i = k * 4; i < (k + 1) * 4; ++i) {
              u8 digit = in & 0xF;
              in >>= 4;

              if (digit >= 10) {
                ASSERT(digit < 16);
                string_res[((LEN - 1) - i) + 2] = ('A' + (digit - 10));
              }
              else {
                string_res[((LEN - 1) - i) + 2] = ('0' + digit);
              }
            }

            if (in == 0) {
              usize size = 2 + (k + 1) * 4;
              usize start = (2 + LEN) - size;
              string_res[start] = '0';
              string_res[start + 1] = 'x';
              res.load_string(string_res + start, size);
              return;
            }
          }


          INVALID_CODE_PATH("Read all the bytes but wasn't 0");
        }
    };
}

void print_x86_64(const GlobalLabelInfo& l_info, Backend::DataBucketIterator start, const Backend::DataBucketIterator end) {
  IO_Single::lock();
  DEFER() { IO_Single::unlock(); };

  IO_Single::format("== Machine for code for {}({}:{}) ==\n",
      l_info.span.full_path, l_info.span.line_start, l_info.span.char_start);

  {
    IO_Single::print("raw = ");

    Backend::DataBucketIterator copy = start;
    while (copy < end) {
      IO_Single::format("{} ", Axle::Format::Hex<u8>{ copy.read_byte() });
    }

    IO_Single::print("\n");
  }

  x86PrintOptions p_opts = {};

  usize start_idx = start.actual_location;

  while (start < end) {
    IO_Single::format("{}: ", HexOffset{ start.actual_location - start_idx });

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
      uint8_t op2 = start.read_byte();
      switch (op2) {
        case X64::JE_NEAR: {
          int rel32 = x32_from_itr(&start);

          IO_Single::format("je {}\n", HexOffset{ (start.actual_location - start_idx) + rel32 });
          break;
        }
        case X64::JNE_NEAR: {
          int rel32 = x32_from_itr(&start);

          IO_Single::format("jne {}\n", HexOffset{ (start.actual_location - start_idx) + rel32 });
          break;
        }
        case X64::JB_NEAR: {
          int rel32 = x32_from_itr(&start);

          IO_Single::format("jb {}\n", HexOffset{ (start.actual_location - start_idx) + rel32 });
          break;
        }
        case X64::JNB_NEAR: {
          int rel32 = x32_from_itr(&start);

          IO_Single::format("jnb {}\n", HexOffset{ (start.actual_location - start_idx) + rel32 });
          break;
        }
        case X64::JA_NEAR: {
          int rel32 = x32_from_itr(&start);

          IO_Single::format("ja {}\n", HexOffset{ (start.actual_location - start_idx) + rel32 });
          break;
        }
        case X64::JNA_NEAR: {
          int rel32 = x32_from_itr(&start);

          IO_Single::format("jna {}\n", HexOffset{ (start.actual_location - start_idx) + rel32 });
          break;
        }
        case X64::JL_NEAR: {
          int rel32 = x32_from_itr(&start);

          IO_Single::format("jl {}\n", HexOffset{ (start.actual_location - start_idx) + rel32 });
          break;
        }
        case X64::JNL_NEAR: {
          int rel32 = x32_from_itr(&start);

          IO_Single::format("jnl {}\n", HexOffset{ (start.actual_location - start_idx) + rel32 });
          break;
        }
        case X64::JG_NEAR: {
          int rel32 = x32_from_itr(&start);

          IO_Single::format("jg {}\n", HexOffset{ (start.actual_location - start_idx) + rel32 });
          break;
        }
        case X64::JNG_NEAR: {
          int rel32 = x32_from_itr(&start);

          IO_Single::format("jng {}\n", HexOffset{ (start.actual_location - start_idx) + rel32 });
          break;
        }
        case X64::SETE_RM8: {
          uint8_t modrm = start.read_byte();

          load_8_sizes(&p_opts, rex, short_address);

          Axle::OwnedArr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &start);
          IO_Single::format("sete {}\n", r_string);
          break;
        }
        case X64::SETL_RM8: {
          uint8_t modrm = start.read_byte();

          load_8_sizes(&p_opts, rex, short_address);

          Axle::OwnedArr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &start);
          IO_Single::format("setl {}\n", r_string);
          break;
        }
        case X64::SETG_RM8: {
          uint8_t modrm = start.read_byte();

          load_8_sizes(&p_opts, rex, short_address);

          Axle::OwnedArr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &start);
          IO_Single::format("setg {}\n", r_string);
          break;
        }
        case X64::IMUL_RM_TO_R: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("imul {}, {}\n", names.r, names.rm);
          break;
        }
        case X64::MOV_ZX_RM8_TO_R: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);
          //overide
          p_opts.r_name = b8_rex_reg_name;

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("movzx {}, {}\n", names.r, names.rm);
          break;
        }
        case X64::MOV_SX_RM8_TO_R: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);
          //overide
          p_opts.r_name = b8_rex_reg_name;

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("movsx {}, {}\n", names.r, names.rm);
          break;
        }
        default: {
          IO_Single::format("UNKNOWN INSTRUCTION: {} {} {}\n",
              Axle::Format::Hex<u8>{ maybe_rex }, Axle::Format::Hex<u8>{ op }, Axle::Format::Hex<u8>{ op2 });

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

          IO_Single::format("add {}, {}\n", names.rm, names.r);
          break;
        }
        case X64::OR_R8_TO_RM8: {
          uint8_t modrm = start.read_byte();

          load_8_sizes(&p_opts, rex, short_address);

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("or  {}, {}\n", names.rm, names.r);
          break;
        }
        case X64::OR_R_TO_RM: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("or  {}, {}\n", names.rm, names.r);
          break;
        }
        case X64::AND_R_TO_RM: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("and {}, {}\n", names.rm, names.r);
          break;
        }
        case X64::SUB_R_TO_RM: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("sub {}, {}\n", names.rm, names.r);
          break;
        }
        case X64::XOR_R_TO_RM: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("xor {}, {}\n", names.rm, names.r);
          break;
        }
        case X64::CMP_R_TO_RM: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("cmp {}, {}\n", names.rm, names.r);
          break;
        }
        case X64::CMP_IMM_TO_AL: {
          ASSERT(!rex);
          uint8_t b = start.read_byte();
          IO_Single::format("cmp al, {}\n", b);
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
          Axle::ViewArr<const char> r_string = b64_reg_name(reg);

          IO_Single::format("push {}\n", r_string);
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
          Axle::ViewArr<const char> r_string = b64_reg_name(reg);

          IO_Single::format("pop {}\n", r_string);
          break;
        }
        case 0x80: {
          uint8_t modrm = start.read_byte();

          load_8_sizes(&p_opts, rex, short_address);

          Axle::OwnedArr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &start);

          i8 imm8 = start.read_byte();

          uint8_t r_val = (modrm & 0b0011'1000) >> 3;

          if (r_val == 5) {
            IO_Single::format("sub {}, {}\n", rm_string, imm8);
          }
          else if (r_val == 7) {
            IO_Single::format("cmp {}, {}\n", rm_string, imm8);
          }
          else {
            IO_Single::format("UNKNOWN INSTRUCTION: {} {} {} ...\n",
                Axle::Format::Hex<u8>{ maybe_rex }, Axle::Format::Hex<u8>{ op }, Axle::Format::Hex<u8>{ modrm });

            return;
          }
          break;
        }
        case 0x81: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          Axle::OwnedArr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &start);

          int32_t imm32 = x32_from_itr(&start);

          uint8_t r_val = (modrm & 0b0011'1000) >> 3;

          if (r_val == 5) {
            IO_Single::format("sub {}, {}\n", rm_string, imm32);
          }
          else if (r_val == 7) {
            IO_Single::format("cmp {}, {}\n", rm_string, imm32);
          }
          else {
            IO_Single::format("UNKNOWN INSTRUCTION: {} {} {} ...\n",
                Axle::Format::Hex<u8>{ maybe_rex }, Axle::Format::Hex<u8>{ op }, Axle::Format::Hex<u8>{ modrm });

            return;
          }
          break;
        }
        case X64::MOV_R_TO_RM: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("mov {}, {}\n", names.rm, names.r);
          break;
        }
        case X64::MOV_IMM8_RM: {
          uint8_t modrm = start.read_byte();

          load_8_sizes(&p_opts, rex, short_address);

          Axle::OwnedArr<char> rm = rm_reg_string(&p_opts, maybe_rex, modrm, &start);

          uint8_t val = start.read_byte();

          IO_Single::format("mov {}, {}\n", rm, val);
          break;
        }
        case X64::MOV_IMM32_RM: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          Axle::OwnedArr<char> rm = rm_reg_string(&p_opts, maybe_rex, modrm, &start);

          uint32_t val = x32_from_itr(&start);

          IO_Single::format("mov {}, {}\n", rm, val);
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

          Axle::ViewArr<const char> name = p_opts.r_name(r);

          if (short_operand) {
            uint16_t val = x16_from_itr(&start);

            IO_Single::format("mov {}, {}\n", name, val);
          }
          else if (!rex_w) {
            uint32_t val = x32_from_itr(&start);

            IO_Single::format("mov {}, {}\n", name, val);
          }
          else {
            uint64_t val = x64_from_itr(&start);

            IO_Single::format("mov {}, {}\n", name, val);
          }
          break;
        }
        case X64::MOV_RM_TO_R: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("mov {}, {}\n", names.r, names.rm);
          break;
        }
        case X64::LEA_RM_TO_R: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("lea {}, {}\n", names.r, names.rm);
          break;
        }
        case X64::CQO: {
          IO_Single::print("cqo\n");
          break;
        }
        case 0xF7: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          const uint8_t r = (modrm & 0b0011'1000) >> 3;
          Axle::OwnedArr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &start);

          if (r == 3) {
            IO_Single::format("neg {}\n", rm_string);
          }
          else if (r == 4) {
            IO_Single::format("mul {}\n", rm_string);
          }
          else if (r == 6) {
            IO_Single::format("div {}\n", rm_string);
          }
          else if (r == 7) {
            IO_Single::format("idiv {}\n", rm_string);
          }
          else {
            IO_Single::format("UNKNOWN INSTRUCTION: {} {} {}\n",
                Axle::Format::Hex<u8>{ maybe_rex }, Axle::Format::Hex<u8>{ op }, Axle::Format::Hex<u8>{ modrm });

            return;
          }
          break;
        }
        case 0xD3: {
          uint8_t modrm = start.read_byte();

          load_default_sizes(&p_opts, rex_w, short_address, short_operand);

          const uint8_t r = (modrm & 0b0011'1000) >> 3;
          Axle::OwnedArr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &start);

          if (r == 4) {
            IO_Single::format("sal {}, CL\n", rm_string);
          }
          else if (r == 5) {
            IO_Single::format("shr {}, CL\n", rm_string);
          }
          else if (r == 7) {
            IO_Single::format("sar {}, CL\n", rm_string);
          }
          else {
            IO_Single::format("UNKNOWN INSTRUCTION: {} {} {}\n",
                Axle::Format::Hex<u8>{ maybe_rex }, Axle::Format::Hex<u8>{ op }, Axle::Format::Hex<u8>{ modrm });

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
          Axle::ViewArr<const char> r_string = b8_rex_reg_name(reg);

          uint8_t imm8 = start.read_byte();

          IO_Single::format("mov {}, {}\n", r_string, imm8);
          break;
        }
        case X64::MOV_R8_TO_RM8: {
          uint8_t modrm = start.read_byte();

          load_8_sizes(&p_opts, rex, short_address);

          RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &start);

          IO_Single::format("mov {}, {}\n", names.rm, names.r);
          break;
        }
        case X64::JMP_NEAR: {
          int rel32 = x32_from_itr(&start);

          IO_Single::format("jmp {}\n", HexOffset{ (start.actual_location - start_idx) + rel32 });
          break;
        }
        case X64::RET_NEAR: {
          IO_Single::print("ret\n");
          break;
        }
        case X64::CALL_NEAR: {
          int rel32 = x32_from_itr(&start);

          IO_Single::format("call {} ; call offset will be incorrect at this point\n",
              HexOffset{ (start.actual_location - start_idx) + rel32 });
          break;
        }
        default: {
          IO_Single::format("UNKNOWN INSTRUCTION: {}\n",
              Axle::Format::Hex<u8>{ op });

          return;
        }
      }
    }
  }
}
