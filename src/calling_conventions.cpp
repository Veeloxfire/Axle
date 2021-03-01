#include "calling_conventions.h"
#include "bytecode.h"

constexpr REGISTER_CONSTANT all_x64_regs[] ={
  RAX,
  RBX,
  RCX,
  RDX,
  RSP,
  RBP,
  RSI,
  RDI,
  R8,
  R9,
  R10,
  R11,
  R12,
  R13,
  R14,
};

static char invalid_reg_string[sizeof("INVALID(000)")];

const char* System::reg_name_from_num(uint8_t reg) const noexcept {
  for (size_t i = 0; i < num_registers; i++) {
    if (all_registers[i].REG == reg) {
      return all_registers[i].name;
    }
  }

  sprintf_s(invalid_reg_string, "INVALID(%hhu)", reg);
  return invalid_reg_string;
}

template<size_t num_registers>
static constexpr System make_system(const REGISTER_CONSTANT(&all_regs)[num_registers],
                                    const REGISTER_CONSTANT& stack_pointer,
                                    const REGISTER_CONSTANT& base_pointer) {
  System system = {};

  system.all_registers = all_regs;
  system.num_registers = num_registers;

  system.stack_pointer = stack_pointer.REG;
  system.base_pointer  = base_pointer.REG;

  return system;
}

const System system_x86_64 = make_system(all_x64_regs, RSP, RBP);



Location CallingConvention::get_argument_location(const size_t index, const size_t total) const {
  Location loc = {};
  
  if (index < num_parameter_registers) {
    loc.type = LOCATION_TYPE::REGISTER;
    loc.reg = parameter_registers[index];
  }
  else {
    loc.type = LOCATION_TYPE::STACK;

    if (stack_direction == STACK_DIRECTION::RIGHT_TO_LEFT) {
      loc.stack_disp = CallingConvention::OFFSET_TO_SHADOW
        + shadow_space_size
        + (8 * (index - num_parameter_registers));
    }
    else {
      loc.stack_disp = CallingConvention::OFFSET_TO_SHADOW
        + shadow_space_size
        + (8 * (total - num_parameter_registers))
        - (8 * (index - num_parameter_registers));
    }
  }

  return loc;
}


namespace MICROSOFT_X64 {
  constexpr uint8_t parameters[] = { RCX.REG, RDX.REG, R8.REG, R9.REG };
  constexpr uint8_t volatiles[] = { RAX.REG, RCX.REG, RDX.REG, R8.REG, R9.REG, R10.REG, R11.REG };
  constexpr uint8_t non_volatiles[] ={ RBX.REG, RSP.REG, RBP.REG, RSI.REG, RDI.REG, R12.REG, R13.REG, R14.REG };
}

template<size_t num_params, size_t num_volatile, size_t num_non_volatile>
static constexpr CallingConvention
make_calling_convention(const uint8_t (&params)[num_params],
                        const uint8_t (&volatiles)[num_volatile],
                        const uint8_t (&non_volatiles)[num_non_volatile],
                        const REGISTER_CONSTANT& ret_reg,
                        const REGISTER_CONSTANT& return_parameter,
                        uint8_t shadow_space_size,
                        STACK_DIRECTION direction) {

  CallingConvention convention ={};

  convention.return_parameter           = return_parameter.REG;
  convention.return_register            = ret_reg.REG;

  convention.parameter_registers        = params;
  convention.num_parameter_registers    = num_params;

  convention.volatile_registers         = volatiles;
  convention.num_volatile_registers     = num_volatile;

  convention.non_volatile_registers     = non_volatiles;
  convention.num_non_volatile_registers = num_non_volatile;

  convention.stack_direction            = direction;
  convention.shadow_space_size          = shadow_space_size;

  return convention;
}

const CallingConvention convention_microsoft_x64
= make_calling_convention(MICROSOFT_X64::parameters,
                          MICROSOFT_X64::volatiles,
                          MICROSOFT_X64::non_volatiles,
                          RAX,
                          RAX,
                          32,
                          STACK_DIRECTION::RIGHT_TO_LEFT);