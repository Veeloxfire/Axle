#include "machine_code.h"

#define REX_REG_EXT_IMPL(TYPE) uint8_t rex_reg_ext(const TYPE rm) {\
  return (TEST_MASK(BYTE(rm), 0b0000'1000) * BYTE(REX::RM_EXT)) | BYTE(REX::NONE);\
}

#define REX_RM_EXT_IMPL(TYPE) uint8_t rex_rm_ext(const TYPE rm) {\
  return (TEST_MASK(BYTE(rm), 0b0000'1000) * BYTE(REX::RM_EXT)) | BYTE(REX::NONE);\
}

#define MOD_RM_IMPL(TYPE) uint8_t mod_rm(const TYPE reg, const TYPE rm) {\
  MOD_RM m;\
  m.set_mod(0b11);\
  m.set_reg(BYTE(reg));\
  m.set_rm(BYTE(rm));\
  return m.byte;\
}

#define MOD_AND_REX_IMPLS(TYPE)\
REX_REG_EXT_IMPL(TYPE)\
REX_RM_EXT_IMPL(TYPE)\
MOD_RM_IMPL(TYPE)

MOD_AND_REX_IMPLS(REGISTER_64B)
MOD_AND_REX_IMPLS(REGISTER_32B)
MOD_AND_REX_IMPLS(REGISTER_32B_REX)
MOD_AND_REX_IMPLS(REGISTER_8B)
MOD_AND_REX_IMPLS(REGISTER_8B_REX)

template<size_t ... I,typename ... Ts>
inline constexpr static void load_instructions(Array<uint8_t>& data,
                                               std::index_sequence<I...>,
                                               Ts&& ... ts) {
  ((data.data[data.size + I] = static_cast<uint8_t>(std::forward<Ts>(ts))),
    ...);
}

template<typename ... Ts>
inline constexpr static void load_instructions(Array<uint8_t>& data,
                              Ts&& ... ts) {
  constexpr size_t SIZE = sizeof...(Ts);
  data.insert_uninit(SIZE);

  load_instructions(data,
                    std::make_index_sequence<SIZE>(),
                    std::forward<Ts>(ts)...);

  data.size += SIZE;
}

namespace ADD {
  void write_x64(Array<uint8_t>& data, REGISTER_64B reg, REGISTER_64B rm) {
    load_instructions(data,
                      BYTE(REX::USE_64) | rex_reg_ext(reg) | rex_rm_ext(rm),
                      BYTE(0x01),
                      mod_rm(reg, rm));
  }

  void write_x64(Array<uint8_t>& data, REGISTER_32B_REX reg, REGISTER_32B_REX rm) {
    load_instructions(data,
                      rex_reg_ext(reg) | rex_rm_ext(rm),
                      BYTE(0x01),
                      mod_rm(reg, rm));
  }

  void write_x64(Array<uint8_t>& data, REGISTER_8B_REX reg, REGISTER_8B_REX rm) {
    load_instructions(data,
                      rex_reg_ext(reg) | rex_rm_ext(rm),
                      BYTE(0x01),
                      mod_rm(reg, rm));
  }

  void write_x32(Array<uint8_t>& data, REGISTER_32B reg, REGISTER_32B rm) {
    load_instructions(data,
                      BYTE(0x00),
                      mod_rm(reg, rm));
  }

  void write_x32(Array<uint8_t>& data, REGISTER_8B reg, REGISTER_8B rm) {
    load_instructions(data,
                      BYTE(0x00),
                      mod_rm(reg, rm));
  }
}

size_t Instruction::size() const {
  return
    //Prefixes (0 - 4)
      (size_t) TEST_MASK(requires, BYTE(USES::PREFIX_GROUP1))
    + (size_t) TEST_MASK(requires, BYTE(USES::PREFIX_GROUP2))
    + (size_t) TEST_MASK(requires, BYTE(USES::PREFIX_GROUP3))
    + (size_t) TEST_MASK(requires, BYTE(USES::PREFIX_GROUP4))
    //Contains REX, ModR/M and/or SIB (0 - 3)
    + (size_t) TEST_MASK(requires, BYTE(USES::REX))
    + (size_t) TEST_MASK(requires, BYTE(USES::MOD_RM))
    + (size_t) TEST_MASK(requires, BYTE(USES::SIB))
    //Opcodes (1 - 3)
    + (size_t) (opcode.num)
    //Displacement size (0 - 8)
    + (size_t) (disp_and_imme_num & BYTE(NUM_MASKS::DISP))
    //Immediate size (0 - 8)
    + (size_t) ((disp_and_imme_num & BYTE(NUM_MASKS::IMME)) >> 4);
}

void MOD_RM::set_mod(uint8_t val) {
  byte |= (val << BYTE(SHIFT::MOD)) & FULL_MOD;
}

void MOD_RM::set_reg(uint8_t val) {
  byte |= (val << BYTE(SHIFT::REG)) & FULL_REG;
}

void MOD_RM::set_rm(uint8_t val) {
  byte |= (val << BYTE(SHIFT::RM)) & FULL_RM;
}

void Instruction::use_rex() {
  SET_MASK(requires, BYTE(USES::REX));
}

void Instruction::remove_rex() {
  RESET_MASK(requires, BYTE(USES::REX));
  rex = REX::NONE;
}

void emit_instruction(Array<uint8_t>& arr, const Instruction& instruction) {
  //Reserve size upfront
  const size_t extra = instruction.size();
  arr.reserve_extra(extra);

  //Load Prefixes
  {
    const Prefixes& prefixes = instruction.prefixes;
    if(TEST_MASK(instruction.requires, BYTE(Instruction::USES::PREFIX_GROUP1))) {
      load_to_bytes(arr, arr.size, prefixes.group1);
    }

    if(TEST_MASK(instruction.requires, BYTE(Instruction::USES::PREFIX_GROUP2))) {
      load_to_bytes(arr, arr.size, prefixes.group2);
    }

    if(TEST_MASK(instruction.requires, BYTE(Instruction::USES::PREFIX_GROUP3))) {
      load_to_bytes(arr, arr.size, prefixes.group3);
    }

    if(TEST_MASK(instruction.requires, BYTE(Instruction::USES::PREFIX_GROUP4))) {
      load_to_bytes(arr, arr.size, prefixes.group4);
    }
  }

  //REX Prefix
  if(TEST_MASK(instruction.requires, BYTE(Instruction::USES::REX))) {
    load_to_bytes(arr, arr.size, instruction.rex);
  }

  //Load Opcode - guaranteed to happen so no need to check
  {
    const Opcode& opcode = instruction.opcode;
    load_to_bytes(arr, arr.size, opcode.bytes, opcode.num);
  }

  //Load ModR/M byte
  if(TEST_MASK(instruction.requires, BYTE(Instruction::USES::MOD_RM)))
    load_to_bytes(arr, arr.size, instruction.mod_rm);

  //Load SIB byte
  if(TEST_MASK(instruction.requires, BYTE(Instruction::USES::SIB)))
    load_to_bytes(arr, arr.size, instruction.sib);

  //Load Displacement
  {
    const Displacement& disp = instruction.displacement;
    const size_t len = instruction.disp_and_imme_num & BYTE(Instruction::NUM_MASKS::DISP);
    if(len > 0)
      load_to_bytes(arr, arr.size, disp.bytes, len);
  }

  //Load immediate
  {
    const Immediate& imm = instruction.immediate;
    const size_t len = (instruction.disp_and_imme_num & BYTE(Instruction::NUM_MASKS::IMME))
                          >> 4;
    if(len > 0)
      load_to_bytes(arr, arr.size, imm.bytes, len);
  }
}
