#pragma once
#include <stdint.h>
#include "utility.h"
#include "type.h"
#include "ast.h"

enum struct STACK_DIRECTION : uint8_t {
  LEFT_TO_RIGHT, RIGHT_TO_LEFT
};

struct REGISTER_CONSTANT;
struct Compiler;

struct ForcedColours {
  uint8_t val1;
  uint8_t val2;
};

struct System {
  const REGISTER_CONSTANT* all_registers;
  uint8_t num_registers;

  bool stack_pointers_are_regs = false;
  uint8_t stack_pointer;
  uint8_t base_pointer;

  FUNCTION_PTR<const char*, uint8_t> reg_name_from_num;
  FUNCTION_PTR<size_t, Array<uint8_t>&, const Compiler*> backend;
  FUNCTION_PTR<ForcedColours, BINARY_OPERATOR> bin_op_forced;
};

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
};

extern const CallingConvention convention_vm;
extern const CallingConvention convention_microsoft_x64;

const char* reg_num_as_string(uint8_t reg) noexcept;