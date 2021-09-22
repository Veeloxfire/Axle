#pragma once

#include "utility.h"

inline constexpr size_t NUM_VM_REGS = 18;
inline constexpr size_t VM_BP_R = 16;
inline constexpr size_t VM_SP_R = 17;

//Forward decls
struct REGISTER_CONSTANT;
struct Compiler;
struct Program;
struct Structure;
struct State;
struct CodeBlock;

//header

enum struct STACK_DIRECTION : uint8_t {
  LEFT_TO_RIGHT, RIGHT_TO_LEFT
};

enum struct STACK_PASS_TYPE : uint8_t {
  VALUE, POINTER
};

enum struct CLEANUP : uint8_t {
  calleE, calleR
};

struct ForcedColours {
  uint8_t val1;
  uint8_t val2;
};

using REG_NAME_FROM_NUM_PTR = FUNCTION_PTR<const char*, uint8_t>;

using BACKEND_TRANSLATE_PTR = FUNCTION_PTR<
  void, 
  Compiler*,
  Program*,
  Array<uint8_t>&,
  const CodeBlock*,
  size_t*,
  Array<size_t>&
>;

using BACKEND_JUMP_FIX_PTR = FUNCTION_PTR<void, uint8_t*, size_t, size_t*>;

struct System {
#define CONST_NAME(n) static constexpr char n ## _name[] = #n
  CONST_NAME(x86_64);
  CONST_NAME(vm);
#undef CONST_NAME

  const char* name;

  const REGISTER_CONSTANT* all_registers;
  uint8_t num_registers;

  REG_NAME_FROM_NUM_PTR reg_name_from_num;

  BACKEND_TRANSLATE_PTR backend_translate;
  BACKEND_JUMP_FIX_PTR backend_jump_fix;
};

struct CallingConvention {
  static constexpr size_t OFFSET_TO_SHADOW = 8ull + 8ull;

#define CONST_NAME(n) static constexpr char n ## _name[] = #n
  CONST_NAME(x64);
  CONST_NAME(vm);
  CONST_NAME(stdcall);
#undef CONST_NAME

  const char* name;

  uint8_t return_register = 0;//Usually RAX or 0
  uint8_t stack_pointer_reg = 0;
  uint8_t base_pointer_reg = 0;

  uint8_t num_parameter_registers = 0;
  uint8_t num_volatile_registers = 0;
  uint8_t num_non_volatile_registers = 0;

  CLEANUP cleanup = CLEANUP::calleE;
  STACK_PASS_TYPE stack_pass_type = STACK_PASS_TYPE::POINTER;
  STACK_DIRECTION stack_direction = STACK_DIRECTION::RIGHT_TO_LEFT;

  uint8_t shadow_space_size = 0;//bytes

  uint64_t non_volatiles_bit_mask = 0;

  const uint8_t* parameter_registers = nullptr;
  const uint8_t* all_regs_unordered = nullptr; //volatile then non_volatile

  bool is_non_volatile(uint8_t) const;
  bool is_volatile(uint8_t) const;
  size_t num_reg_parameters(size_t total_parameters) const;
};

const char* x86_64_reg_name_from_num(uint8_t reg) noexcept;
const char* vm_regs_name_from_num(uint8_t reg) noexcept;
const char* reg_num_as_string(uint8_t reg) noexcept;

extern const System system_x86_64;
extern const System system_vm;

extern const CallingConvention convention_vm;
extern const CallingConvention convention_microsoft_x64;
extern const CallingConvention convention_stdcall;

bool register_passed_as_pointer(const Structure* type);

struct CallingConvArgIterator {
  const CallingConvention* conv;
  size_t regs_used;
};

struct CallingConvParamIterator {
  const CallingConvention* conv;
  size_t regs_used;
  int32_t stack_passed;
};