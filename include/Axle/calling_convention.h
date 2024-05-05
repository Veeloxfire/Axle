#pragma once
#include <AxleUtil/safe_lib.h>

enum struct STACK_DIRECTION : uint8_t {
  LEFT_TO_RIGHT, RIGHT_TO_LEFT
};

enum struct STACK_PASS_TYPE : uint8_t {
  VALUE, POINTER
};

enum struct CLEANUP : uint8_t {
  calleE, calleR
};

struct CallingConvention {
  static constexpr size_t OFFSET_TO_SHADOW = 8ull + 8ull;

  Axle::ViewArr<const char> name;

  uint8_t return_register = 0;//Usually RAX or 0
  uint8_t stack_pointer_reg = 0;
  uint8_t base_pointer_reg = 0;

  uint8_t num_parameter_registers = 0;
  uint8_t num_volatile_registers = 0;
  uint8_t num_non_volatile_registers = 0;
  uint8_t num_available_registers = 0;

  CLEANUP cleanup = CLEANUP::calleE;
  STACK_PASS_TYPE stack_pass_type = STACK_PASS_TYPE::POINTER;
  STACK_DIRECTION stack_direction = STACK_DIRECTION::RIGHT_TO_LEFT;

  uint8_t shadow_space_size = 0;//bytes

  uint64_t non_volatiles_bit_mask = 0;
  uint64_t volatiles_bit_mask = 0;

  const uint8_t* parameter_registers = nullptr;
  const uint8_t* all_regs_unordered = nullptr;//volatile then non_volatile

  constexpr bool is_non_volatile(uint8_t reg) const {
    return (non_volatiles_bit_mask & ((uint64_t)1 << reg)) != 0;
  }

  constexpr bool is_volatile(uint8_t reg) const {
    return (volatiles_bit_mask & ((uint64_t)1 << reg)) != 0;
  }

  constexpr size_t num_reg_parameters(size_t parameters) const {
    return Axle::smaller(parameters, (size_t)num_parameter_registers);
  }
};
