#pragma once
#include <stdint.h>
#include "utility.h"
#include "type.h"

enum struct STACK_DIRECTION : uint8_t {
  LEFT_TO_RIGHT, RIGHT_TO_LEFT
};

struct REGISTER_CONSTANT;

struct System {
  const REGISTER_CONSTANT* all_registers;
  uint8_t num_registers;

  uint8_t stack_pointer;
  uint8_t base_pointer;

  const char* reg_name_from_num(uint8_t reg) const noexcept;
};

extern const System system_x86_64;

struct CallingConvention {
  static constexpr size_t OFFSET_TO_SHADOW = 8ull + 8ull;

  uint8_t return_register = 0;//Usually RAX
  uint8_t return_parameter = 0;//Usually first parameter

  uint8_t num_parameter_registers = 0;
  uint8_t num_volatile_registers = 0;
  uint8_t num_non_volatile_registers = 0;
  STACK_DIRECTION stack_direction = STACK_DIRECTION::RIGHT_TO_LEFT;

  uint8_t shadow_space_size = 0;//bytes

  const uint8_t* parameter_registers = nullptr;
  const uint8_t* volatile_registers = nullptr;
  const uint8_t* non_volatile_registers = nullptr;

  Location get_argument_location(size_t i, size_t total) const;
};

extern const CallingConvention convention_microsoft_x64;