#include "PE_file_format.h"
#include "utility.h"
using TextSegment = Section;

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
  SP = 4 , BP = 5, SI = 6, DI = 7,
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

uint8_t rex_reg_ext(REGISTER_64B reg);
uint8_t rex_reg_ext(REGISTER_32B reg);
uint8_t rex_reg_ext(REGISTER_32B_REX reg);
uint8_t rex_reg_ext(REGISTER_8B reg);
uint8_t rex_reg_ext(REGISTER_8B_REX reg);

uint8_t rex_rm_ext(REGISTER_64B reg);
uint8_t rex_rm_ext(REGISTER_32B reg);
uint8_t rex_rm_ext(REGISTER_32B_REX reg);
uint8_t rex_rm_ext(REGISTER_8B reg);
uint8_t rex_rm_ext(REGISTER_8B_REX reg);

uint8_t mod_rm(REGISTER_64B reg, REGISTER_64B rm);
uint8_t mod_rm(REGISTER_32B reg, REGISTER_32B rm);
uint8_t mod_rm(REGISTER_32B_REX reg, REGISTER_32B_REX rm);
uint8_t mod_rm(REGISTER_8B reg, REGISTER_8B rm);
uint8_t mod_rm(REGISTER_8B_REX reg, REGISTER_32B_REX rm);


enum struct REGISTER_VALUE : uint8_t {
  //// 8 bit registers ////
  //Low bits
  AL = 0, CL = 1, DL = 2, BL = 3,
  //Where any REX prefix is used
  SPL = 4, BPL = 5, SIL = 6, DIL = 7,
  //High bits
  AH = 4, CH = 5, DH = 6, BH = 7,
  //REX bit set
  R8L = 0, R9L = 1, R10L = 2, R11L = 3,
  R12L = 4, R13L = 5, R14L = 6, R15L = 7,

  //// 16 bit registers ////
  AX = 0, CX = 1, DX = 2, BX = 3,
  SP = 4 , BP = 5, SI = 6, DI = 7,
  //REX bit set
  R8W = 0, R9W = 1, R10W = 2, R11W = 3,
  R12W = 4, R13W = 5, R14W = 6, R15W = 7,

  //// 32 bit registers ////
  EAX = 0, ECX = 1, EDX = 2, EBX = 3,
  ESP = 4, EBP = 5, ESI = 6, EDI = 7,
  //REX bit set
  R8D = 0, R9D = 1, R10D = 2, R11D = 3,
  R12D = 4, R13D = 5, R14D = 6, R15D = 7,

  //// 64 bit registers ////
  RAX = 0, RCX = 1, RDX = 2, RBX = 3,
  RSP = 4, RBP = 5, RSI = 6, RDI = 7,
  //REX bit set
  R8 = 0, R9 = 1, R10 = 2, R11 = 3,
  R12 = 4, R13 = 5, R14 = 6, R15 = 7,
};

enum struct REX : uint8_t {
  NONE = 0b0100'0000,

  B = NONE | BIT_MASKS::B0,
  X = NONE | BIT_MASKS::B1,
  R = NONE | BIT_MASKS::B2,
  W = NONE | BIT_MASKS::B3,

  USE_64 = W,
  REG_EXT = R,
  INDEX_EXT = X,
  RM_EXT = B
};

enum struct GROUP1_PREFIX_CODE : uint8_t {
  //Lock and Repeat prefixes
  LOCK  = 0xF0,
  REPNZ = 0xF2,
  REP   = 0xF3,
  //Bounds prefix
  BOUND = 0xF2,
};

enum struct GROUP2_PREFIX_CODE : uint8_t {
  //Segment Overrides prefixes
  CS_OVERRIDE = 0x2E,
  SS_OVERRIDE = 0x36,
  DS_OVERRIDE = 0x3E,
  ES_OVERRIDE = 0x26,
  FS_OVERRIDE = 0x64,
  GS_OVERRIDE = 0x65,
  //Branch Hints prefixes
  BRANCH_NOT_TAKEN = 0x2E,
  BRANCH_TAKEN     = 0x3E,
};

enum struct GROUP3_PREFIX_CODE : uint8_t {
  OPERAND_SIZE_OVERRIDE = 0x66,
};

enum struct GROUP4_PREFIX_CODE : uint8_t {
  ADDRESS_SIZE_OVERRIDE = 0x67,
};

struct Prefixes {
  GROUP1_PREFIX_CODE group1;
  GROUP2_PREFIX_CODE group2;
  GROUP3_PREFIX_CODE group3;
  GROUP4_PREFIX_CODE group4;
};

struct Opcode {
  uint8_t bytes[3];
  uint8_t num = 0;
};

struct MOD_RM {
  enum MASKS : uint8_t {
    FULL_MOD = BIT_MASKS::B7 | BIT_MASKS::B6,
    FULL_REG = BIT_MASKS::B5 | BIT_MASKS::B4 | BIT_MASKS::B3,
    FULL_RM  = BIT_MASKS::B2 | BIT_MASKS::B1 | BIT_MASKS::B0,
  };

  enum struct SHIFT : uint8_t {
    MOD = 6,
    REG = 3,
    RM  = 0,
  };

  uint8_t byte = 0b0000'0000;

  void set_mod(uint8_t val);
  void set_reg(uint8_t val);
  void set_rm(uint8_t val);
};

struct SIB {
  uint8_t byte = 0b0000'0000;
};

struct Displacement {
  uint8_t bytes[8];
};

struct Immediate {
  uint8_t bytes[8];
};

struct Instruction {
  enum struct USES : uint8_t {
    PREFIX_GROUP1 = BIT_MASKS::B0,
    PREFIX_GROUP2 = BIT_MASKS::B1,
    PREFIX_GROUP3 = BIT_MASKS::B2,
    PREFIX_GROUP4 = BIT_MASKS::B3,

    REX    = BIT_MASKS::B4,
    MOD_RM = BIT_MASKS::B5,
    SIB    = BIT_MASKS::B6,
  };

  enum struct NUM_MASKS : uint8_t {
    DISP = 0b0000'1111,
    IMME = 0b1111'0000,
  };

  uint8_t requires = 0b0000'0000;
  uint8_t disp_and_imme_num = 0b0000'0000;

  REX rex = {};
  MOD_RM mod_rm = {};
  SIB sib = {};
  Prefixes prefixes = {};

  Opcode opcode;

  Displacement displacement = {};
  Immediate immediate = {};

  size_t size() const;

  void use_rex();
  void remove_rex();
};

namespace ADD {
  void write_x64(Array<uint8_t>& data, REGISTER_64B reg, REGISTER_64B rm);
  void write_x64(Array<uint8_t>& data, REGISTER_32B_REX reg, REGISTER_32B_REX rm);
  void write_x64(Array<uint8_t>& data, REGISTER_8B_REX reg, REGISTER_8B_REX rm);

  void write_x32(Array<uint8_t>& data, REGISTER_32B reg, REGISTER_32B rm);
  void write_x32(Array<uint8_t>& data, REGISTER_8B reg, REGISTER_8B rm);
}

void set_group1_prefix(Instruction&, GROUP1_PREFIX_CODE);
void reset_group1_prefix(Instruction&);

void emit_instruction(Array<uint8_t>& arr, const Instruction& instruction);
