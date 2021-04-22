#include "calling_conventions.h"
#include "bytecode.h"
#include "backends.h"

#include "ast.h"

constexpr REGISTER_CONSTANT all_x64_regs[] ={
  RAX,
  RCX,
  RDX,
  RBX,
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

constexpr REGISTER_CONSTANT all_vm_regs[NUM_VM_REGS] ={
  {0, "v_0"}, {1, "v_1"}, {2, "v_2"}, {3, "v_3"}, {4, "v_4"}, {5, "v_5"}, {6, "v_6"}, {7, "v_7"},
  {8, "nv_0"}, {9, "nv_1"}, {10, "nv_2"}, {11, "nv_3"}, {12, "nv_4"}, {13, "nv_5"}, {14, "nv_6"}, {15, "nv_7"},
};

static char invalid_reg_string[sizeof("REG(000)")];

const char* reg_num_as_string(uint8_t reg) noexcept {
  sprintf_s(invalid_reg_string, "REG(%hhu)", reg);
  return invalid_reg_string;
}

const char* x86_64_reg_name_from_num(uint8_t reg) noexcept {
  constexpr size_t num_registers = sizeof(all_x64_regs) / sizeof(REGISTER_CONSTANT);

  for (size_t i = 0; i < num_registers; i++) {
    if (all_x64_regs[i].REG == reg) {
      return all_x64_regs[i].name;
    }
  }

  return reg_num_as_string(reg);
}

const char* vm_regs_name_from_num(uint8_t reg) noexcept {
  constexpr size_t num_registers = sizeof(all_vm_regs) / sizeof(REGISTER_CONSTANT);

  for (size_t i = 0; i < num_registers; i++) {
    if (all_vm_regs[i].REG == reg) {
      return all_vm_regs[i].name;
    }
  }

  return reg_num_as_string(reg);
}

template<size_t num_registers>
static constexpr System make_system(const char* name,
                                    const REGISTER_CONSTANT(&all_regs)[num_registers],
                                    const REGISTER_CONSTANT& stack_pointer,
                                    const REGISTER_CONSTANT& base_pointer,
                                    REG_NAME_FROM_NUM_PTR reg_name_from_num,
                                    BACKEND_PTR backend) {
  System system ={};

  system.name = name;

  system.all_registers = all_regs;
  system.num_registers = num_registers;

  system.stack_pointers_are_regs = true;
  system.stack_pointer = stack_pointer.REG;
  system.base_pointer  = base_pointer.REG;

  system.reg_name_from_num = reg_name_from_num;
  system.backend = backend;

  return system;
}

template<size_t num_registers>
static constexpr System make_system(const char* name,
                                    const REGISTER_CONSTANT(&all_regs)[num_registers],
                                    REG_NAME_FROM_NUM_PTR reg_name_from_num,
                                    BACKEND_PTR backend) {
  System system ={};

  system.name = name;

  system.all_registers = all_regs;
  system.num_registers = num_registers;

  system.stack_pointers_are_regs = false;

  system.reg_name_from_num = reg_name_from_num;
  system.backend = backend;

  return system;
}

const System system_x86_64 = make_system(System::x86_64_name,
                                         all_x64_regs, RSP, RBP,
                                         &x86_64_reg_name_from_num,
                                         &x86_64_machine_code_backend);

const System system_vm = make_system(System::vm_name,
                                     all_vm_regs,
                                     &vm_regs_name_from_num,
                                     &vm_backend);

template<typename T, size_t size>
struct ConstArray {
  T arr[size];
};

template<size_t num_volatile, size_t num_non_volatile>
constexpr static ConstArray<uint8_t, num_volatile + num_non_volatile>
combine_regs(const uint8_t(&volatiles)[num_volatile],
             const uint8_t(&non_volatiles)[num_non_volatile]) {
  ConstArray<uint8_t, num_volatile + num_non_volatile> arr ={};

  size_t i = 0;
  for (; i < num_volatile; i++) {
    arr.arr[i] = volatiles[i];
  }

  i = 0;
  for (; i < num_non_volatile; i++) {
    arr.arr[i + num_volatile] = non_volatiles[i];
  }

  return arr;
}

namespace MICROSOFT_X64 {
  constexpr uint8_t parameters[] ={ RCX.REG, RDX.REG, R8.REG, R9.REG };
  constexpr uint8_t volatiles[] ={ RAX.REG, RCX.REG, RDX.REG, R8.REG, R9.REG, R10.REG, R11.REG };
  constexpr uint8_t non_volatiles[] ={ RBX.REG, RSI.REG, RDI.REG, R12.REG, R13.REG, R14.REG };

  constexpr auto all_regs_unordered = combine_regs(volatiles, non_volatiles);
}

template<size_t all, size_t num_params, size_t num_volatile, size_t num_non_volatile>
static constexpr CallingConvention
make_calling_convention(const uint8_t(&all_regs)[all],
                        const uint8_t(&params)[num_params],
                        const uint8_t(&volatiles)[num_volatile],
                        const uint8_t(&non_volatiles)[num_non_volatile],
                        const REGISTER_CONSTANT& ret_reg,
                        uint8_t shadow_space_size,
                        STACK_DIRECTION direction) {

  CallingConvention convention ={};

  convention.return_register            = ret_reg.REG;
  convention.all_regs_unordered         = all_regs;

  convention.parameter_registers        = params;
  convention.num_parameter_registers    = num_params;


  uint64_t mask = 0;
  for (size_t i = 0; i < num_non_volatile; i++) {
    mask |= ((uint64_t)1 << non_volatiles[i]);
  }

  convention.non_volatiles_bit_mask = mask;

  convention.num_volatile_registers     = num_volatile;
  convention.num_non_volatile_registers = num_non_volatile;

  convention.stack_direction            = direction;
  convention.shadow_space_size          = shadow_space_size;

  return convention;
}

const CallingConvention convention_microsoft_x64
= make_calling_convention(MICROSOFT_X64::all_regs_unordered.arr,
                          MICROSOFT_X64::parameters,
                          MICROSOFT_X64::volatiles,
                          MICROSOFT_X64::non_volatiles,
                          RAX,
                          32,
                          STACK_DIRECTION::RIGHT_TO_LEFT);

namespace VM {
#define V_REG(num) all_vm_regs[num].REG
#define NV_REG(num) all_vm_regs[num + 8].REG

  constexpr uint8_t parameters[] ={ V_REG(1), V_REG(2), V_REG(3), V_REG(4) };
  constexpr uint8_t volatiles[] ={ V_REG(0), V_REG(1), V_REG(2), V_REG(3), V_REG(4), V_REG(5), V_REG(6), V_REG(7) };
  constexpr uint8_t non_volatiles[] ={ NV_REG(0), NV_REG(1), NV_REG(2), NV_REG(3), NV_REG(4), NV_REG(5), NV_REG(6), NV_REG(7) };

  constexpr auto all_regs_unordered = combine_regs(volatiles, non_volatiles);

#undef V_REG
#undef NV_REG
}

const CallingConvention convention_vm
= make_calling_convention(VM::all_regs_unordered.arr,
                          VM::parameters,
                          VM::volatiles,
                          VM::non_volatiles,
                          all_vm_regs[0],
                          0,
                          STACK_DIRECTION::RIGHT_TO_LEFT);



bool CallingConvention::is_non_volatile(uint8_t reg) const {
  return (non_volatiles_bit_mask & ((uint64_t)1 << reg)) != 0;
}

bool CallingConvention::is_volatile(uint8_t reg) const {
  return (non_volatiles_bit_mask & ((uint64_t)1 << reg)) == 0;
}