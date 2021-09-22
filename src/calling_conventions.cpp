#include "calling_conventions.h"
#include "bytecode.h"
#include "backends.h"
#include "utility.h"

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

  {VM_BP_R, "r_BP"}, {VM_SP_R, "r_SP"},
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
                                    BACKEND_TRANSLATE_PTR backend_translate,
                                    BACKEND_JUMP_FIX_PTR backend_jump_fix) {
  System system ={};

  system.name = name;

  system.all_registers = all_regs;
  system.num_registers = num_registers;

  system.reg_name_from_num = reg_name_from_num;
  system.backend_translate = backend_translate;
  system.backend_jump_fix = backend_jump_fix;

  return system;
}

const System system_x86_64 = make_system(System::x86_64_name,
                                         all_x64_regs, RSP, RBP,
                                         &x86_64_reg_name_from_num,
                                         &x86_64_backend_code_block,
                                         &x86_64_backend_fix_jump);

const System system_vm = make_system(System::vm_name,
                                     all_vm_regs, all_vm_regs[VM_SP_R], all_vm_regs[VM_BP_R],
                                     &vm_regs_name_from_num,
                                     &vm_backend_code_block,
                                     &vm_backend_fix_jump);

template<typename T, size_t size>
struct ConstArray {
  struct Loader {
    T* arr;

    template<typename U>
    constexpr Loader& operator<<(U&& u) {
      arr[0] = std::forward<U>(u);
      arr++;
      return *this;
    }
  };


  T arr[size];

  template<typename ... U>
  constexpr static auto fill_arr(U&& ... u) {
    ConstArray<T, size> arr ={};

    static_assert(sizeof...(U) == size, "Must be fully filled");


    Loader load{ arr.arr };
    (load << ... << std::forward<U>(u));

    return arr;
  }
};

template<size_t num_volatile, size_t num_non_volatile, typename ... T>
constexpr static auto
combine_regs(const uint8_t(&volatiles)[num_volatile],
             const uint8_t(&non_volatiles)[num_non_volatile],
             T&& ... extras) {
  ConstArray<uint8_t, num_volatile + num_non_volatile + sizeof...(T)> arr ={};

  size_t i = 0;
  for (; i < num_volatile; i++) {
    arr.arr[i] = volatiles[i];
  }

  i = 0;
  for (; i < num_non_volatile; i++) {
    arr.arr[i + num_volatile] = non_volatiles[i];
  }

  if constexpr (sizeof...(T) > 0) {
    using EXTRA_ARR = ConstArray<const REGISTER_CONSTANT*, sizeof...(T)>;
    const auto extras_arr = EXTRA_ARR::fill_arr((&extras)...);

    i = 0;
    for (; i < sizeof...(T); i++) {
      arr.arr[i + num_volatile + num_non_volatile] = extras_arr.arr[i]->REG;
    }
  }

  return arr;
}

template<size_t all, size_t num_volatile, size_t num_non_volatile>
static constexpr CallingConvention
make_calling_convention(const char* name,
                        const uint8_t(&all_regs)[all],
                        const uint8_t* params,
                        size_t num_params,
                        const uint8_t(&volatiles)[num_volatile],
                        const uint8_t(&non_volatiles)[num_non_volatile],
                        const REGISTER_CONSTANT& ret_reg,
                        const REGISTER_CONSTANT& sp_reg,
                        const REGISTER_CONSTANT& bp_reg,
                        uint8_t shadow_space_size,
                        CLEANUP cleanup,
                        STACK_PASS_TYPE pass_type,
                        STACK_DIRECTION direction) {

  static_assert(all == num_non_volatile + num_volatile, "Should be equal");

  CallingConvention convention ={};

  convention.name = name;
  convention.all_regs_unordered         = all_regs;

  convention.parameter_registers        = params;
  convention.num_parameter_registers    = (uint8_t)num_params;


  uint64_t mask = 0;
  for (size_t i = 0; i < num_non_volatile; i++) {
    mask |= ((uint64_t)1 << non_volatiles[i]);
  }

  convention.return_register = ret_reg.REG;
  convention.stack_pointer_reg = sp_reg.REG;
  convention.base_pointer_reg = bp_reg.REG;

  convention.non_volatiles_bit_mask = mask;

  convention.num_volatile_registers     = num_volatile;
  convention.num_non_volatile_registers = num_non_volatile;

  convention.cleanup = cleanup;
  convention.stack_direction   = direction;
  convention.stack_pass_type   = pass_type;
  convention.shadow_space_size = shadow_space_size;

  return convention;
}

namespace MICROSOFT_X64 {
  constexpr uint8_t parameters[] ={ RCX.REG, RDX.REG, R8.REG, R9.REG };
  constexpr size_t num_parameters = sizeof(parameters)/sizeof(uint8_t);

  constexpr uint8_t volatiles[] ={ RAX.REG, RCX.REG, RDX.REG, R8.REG, R9.REG, R10.REG, R11.REG };
  constexpr uint8_t non_volatiles[] ={ RBX.REG, RSI.REG, RDI.REG, R12.REG, R13.REG, R14.REG };

  constexpr auto all_regs_unordered = combine_regs(volatiles, non_volatiles);
}


const CallingConvention convention_microsoft_x64
= make_calling_convention(CallingConvention::x64_name,
                          MICROSOFT_X64::all_regs_unordered.arr,
                          MICROSOFT_X64::parameters,
                          MICROSOFT_X64::num_parameters,
                          MICROSOFT_X64::volatiles,
                          MICROSOFT_X64::non_volatiles,
                          RAX,
                          RSP,
                          RBP,
                          32,
                          CLEANUP::calleE,
                          STACK_PASS_TYPE::POINTER,
                          STACK_DIRECTION::RIGHT_TO_LEFT);

namespace STDCALL {
  constexpr uint8_t volatiles[] ={ RAX.REG, RCX.REG, RDX.REG, R8.REG, R9.REG, R10.REG, R11.REG, R12.REG, R13.REG, R14.REG  };
  constexpr uint8_t non_volatiles[] ={ RBX.REG, RSI.REG, RDI.REG };

  constexpr auto all_regs_unordered = combine_regs(volatiles, non_volatiles);
}


const CallingConvention convention_stdcall
= make_calling_convention(CallingConvention::stdcall_name,
                          STDCALL::all_regs_unordered.arr,
                          nullptr,
                          0,
                          STDCALL::volatiles,
                          STDCALL::non_volatiles,
                          RAX,
                          RSP,
                          RBP,
                          0,
                          CLEANUP::calleE,
                          STACK_PASS_TYPE::VALUE,
                          STACK_DIRECTION::RIGHT_TO_LEFT);

namespace VM {
#define V_REG(num) all_vm_regs[num].REG
#define NV_REG(num) all_vm_regs[num + 8].REG

  constexpr uint8_t parameters[] ={ V_REG(1), V_REG(2), V_REG(3), V_REG(4) };
  constexpr size_t num_parameters = sizeof(parameters)/sizeof(uint8_t);
  constexpr uint8_t volatiles[] ={ V_REG(0), V_REG(1), V_REG(2), V_REG(3), V_REG(4), V_REG(5), V_REG(6), V_REG(7) };
  constexpr uint8_t non_volatiles[] ={ NV_REG(0), NV_REG(1), NV_REG(2), NV_REG(3), NV_REG(4), NV_REG(5), NV_REG(6), NV_REG(7) };

  constexpr auto all_regs_unordered = combine_regs(volatiles, non_volatiles);

#undef V_REG
#undef NV_REG
}

const CallingConvention convention_vm
= make_calling_convention(CallingConvention::vm_name,
                          VM::all_regs_unordered.arr,
                          VM::parameters,
                          VM::num_parameters,
                          VM::volatiles,
                          VM::non_volatiles,
                          all_vm_regs[0],
                          all_vm_regs[VM_SP_R],
                          all_vm_regs[VM_BP_R],
                          0,
                          CLEANUP::calleE,
                          STACK_PASS_TYPE::POINTER,
                          STACK_DIRECTION::RIGHT_TO_LEFT);



bool CallingConvention::is_non_volatile(uint8_t reg) const {
  return (non_volatiles_bit_mask & ((uint64_t)1 << reg)) != 0;
}

bool CallingConvention::is_volatile(uint8_t reg) const {
  return (non_volatiles_bit_mask & ((uint64_t)1 << reg)) == 0;
}

size_t CallingConvention::num_reg_parameters(size_t parameters) const {
  return smaller(parameters, (size_t)num_parameter_registers);
}

bool register_passed_as_pointer(const Structure* type) {
  return type->size() > 8;
}