#pragma once

#include "utility.h"

//Forward decls
struct REGISTER_CONSTANT;
struct Compiler;

//header

enum struct STACK_DIRECTION : uint8_t {
  LEFT_TO_RIGHT, RIGHT_TO_LEFT
};

struct ForcedColours {
  uint8_t val1;
  uint8_t val2;
};

using REG_NAME_FROM_NUM_PTR = FUNCTION_PTR<const char*, uint8_t>;
using BACKEND_PTR = FUNCTION_PTR<size_t, Array<uint8_t>&, const Compiler*>;

struct System {
#define CONST_NAME(n) static constexpr char n ## _name[] = #n
  CONST_NAME(x86_64);
  CONST_NAME(vm);
#undef CONST_NAME

  const char* name;

  const REGISTER_CONSTANT* all_registers;
  uint8_t num_registers;

  bool stack_pointers_are_regs = false;
  uint8_t stack_pointer;
  uint8_t base_pointer;

  REG_NAME_FROM_NUM_PTR reg_name_from_num;
  BACKEND_PTR backend;
};

const char* x86_64_reg_name_from_num(uint8_t) noexcept;
const char* vm_regs_name_from_num(uint8_t reg) noexcept;

extern const System system_x86_64;
extern const System system_vm;

struct CallingConvention {
  static constexpr size_t OFFSET_TO_SHADOW = 8ull + 8ull;

  uint8_t return_register = 0;//Usually RAX

  uint8_t num_parameter_registers = 0;
  uint8_t num_volatile_registers = 0;
  uint8_t num_non_volatile_registers = 0;
  STACK_DIRECTION stack_direction = STACK_DIRECTION::RIGHT_TO_LEFT;

  uint8_t shadow_space_size = 0;//bytes

  uint64_t non_volatiles_bit_mask = 0;

  const uint8_t* parameter_registers = nullptr;
  const uint8_t* all_regs_unordered = nullptr; //volatile then non_volatile

  bool is_non_volatile(uint8_t) const;
  bool is_volatile(uint8_t) const;
};

extern const CallingConvention convention_vm;
extern const CallingConvention convention_microsoft_x64;

const char* reg_num_as_string(uint8_t reg) noexcept;