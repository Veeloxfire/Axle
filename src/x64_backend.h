#pragma once
#include "backends.h"

namespace X64 {
  struct REGISTER_CONSTANT { uint8_t REG; const char* name; };

#define REGISTER_DEFINE(NAME, VAL) inline constexpr REGISTER_CONSTANT NAME = { VAL, #NAME }

  REGISTER_DEFINE(rax, 0);
  REGISTER_DEFINE(rcx, 1);
  REGISTER_DEFINE(rdx, 2);
  REGISTER_DEFINE(rbx, 3);
  REGISTER_DEFINE(rsp, 4);
  REGISTER_DEFINE(rbp, 5);
  REGISTER_DEFINE(rsi, 6);
  REGISTER_DEFINE(rdi, 7);
  REGISTER_DEFINE(r8, 8);
  REGISTER_DEFINE(r9, 9);
  REGISTER_DEFINE(r10, 10);
  REGISTER_DEFINE(r11, 11);
  REGISTER_DEFINE(r12, 12);
  REGISTER_DEFINE(r13, 13);
  REGISTER_DEFINE(r14, 14);

#undef REGISTER_DEFINE

  inline constexpr REGISTER_CONSTANT all_x64_regs[] = {
    rax,
    rcx,
    rdx,
    rbx,
    rsp,
    rbp,
    rsi,
    rdi,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
  };

  constexpr const char* x86_64_reg_name_from_num(uint8_t reg) noexcept {
    constexpr size_t num_registers = sizeof(all_x64_regs) / sizeof(REGISTER_CONSTANT);

    if (reg >= num_registers) {
      return "<INVALID-REGISTER>";
    }
    else {
      return all_x64_regs[reg].name;
    }
  }


  template<size_t num_volatile, size_t num_non_volatile, typename ... T>
  constexpr auto
    combine_regs(const uint8_t(&volatiles)[num_volatile],
                 const uint8_t(&non_volatiles)[num_non_volatile],
                 T&& ... extras) {
    ConstArray<uint8_t, num_volatile + num_non_volatile + sizeof...(T)> arr = {};

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
  constexpr CallingConvention
    make_calling_convention(const char* name,
                            const uint8_t(&all_regs_unordered)[all],
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

    CallingConvention convention = {};

    convention.name = name;
    convention.all_regs_unordered = all_regs_unordered;
    convention.num_available_registers = all;

    convention.parameter_registers = params;
    convention.num_parameter_registers = (uint8_t)num_params;

    {
      uint64_t mask = 0;
      for (size_t i = 0; i < num_non_volatile; i++) {
        mask |= ((uint64_t)1 << non_volatiles[i]);
      }
      convention.non_volatiles_bit_mask = mask;
    }

    {
      uint64_t mask = 0;
      for (size_t i = 0; i < num_volatile; i++) {
        mask |= ((uint64_t)1 << volatiles[i]);
      }
      convention.volatiles_bit_mask = mask;
    }

    convention.return_register = ret_reg.REG;
    convention.stack_pointer_reg = sp_reg.REG;
    convention.base_pointer_reg = bp_reg.REG;


    convention.num_volatile_registers = num_volatile;
    convention.num_non_volatile_registers = num_non_volatile;

    convention.cleanup = cleanup;
    convention.stack_direction = direction;
    convention.stack_pass_type = pass_type;
    convention.shadow_space_size = shadow_space_size;

    return convention;
  }

  namespace MICROSOFT_X64 {
    inline constexpr uint8_t parameters[] = { rcx.REG, rdx.REG, r8.REG, r9.REG };
    inline constexpr size_t num_parameters = sizeof(parameters) / sizeof(uint8_t);

    inline constexpr uint8_t volatiles[] = { rax.REG, rcx.REG, rdx.REG, r8.REG, r9.REG, r10.REG, r11.REG };
    inline constexpr uint8_t non_volatiles[] = { rbx.REG, rsi.REG, rdi.REG, r12.REG, r13.REG, r14.REG };

    inline constexpr auto all_regs_unordered = combine_regs(volatiles, non_volatiles);
  }


  inline constexpr CallingConvention CONVENTION_microsoft_x64
    = make_calling_convention("Microsoft x64",
                              MICROSOFT_X64::all_regs_unordered.arr,
                              MICROSOFT_X64::parameters,
                              MICROSOFT_X64::num_parameters,
                              MICROSOFT_X64::volatiles,
                              MICROSOFT_X64::non_volatiles,
                              rax,
                              rsp,
                              rbp,
                              32,
                              CLEANUP::calleE,
                              STACK_PASS_TYPE::POINTER,
                              STACK_DIRECTION::RIGHT_TO_LEFT);

  namespace STDCALL {
    inline constexpr uint8_t volatiles[] = { rax.REG, rcx.REG, rdx.REG, r8.REG, r9.REG, r10.REG, r11.REG, r12.REG, r13.REG, r14.REG };
    inline constexpr uint8_t non_volatiles[] = { rbx.REG, rsi.REG, rdi.REG };

    inline constexpr auto all_regs_unordered = combine_regs(volatiles, non_volatiles);
  }

  constexpr CallingConvention CONVENTION_stdcall
    = make_calling_convention("stdcall",
                              STDCALL::all_regs_unordered.arr,
                              nullptr,
                              0,
                              STDCALL::volatiles,
                              STDCALL::non_volatiles,
                              rax,
                              rsp,
                              rbp,
                              0,
                              CLEANUP::calleE,
                              STACK_PASS_TYPE::VALUE,
                              STACK_DIRECTION::RIGHT_TO_LEFT);

  inline constexpr const CallingConvention* X64_CALLING_CONVENTIONS[] = {
    &CONVENTION_microsoft_x64,
    &CONVENTION_stdcall,
  };


  inline constexpr const char SYSTEM_NAME[] = "x86_64";

  struct Program : Backend::GenericProgram {
    IR::GlobalLabel exit_process;
  };
}

void x64_emit_dyn_library_function(CompilerThread* comp_thread,
                                   const IR::DynLibraryImport* lib_import,
                                   const CallingConvention* convention,
                                   Backend::GenericProgram* program);

void x64_emit_function(CompilerGlobals* comp,
                       CompilerThread* comp_thread,
                       const IR::Builder* ir,
                       const CallingConvention* convention,
                       Backend::GenericProgram* program);

void x64_emit_start(CompilerGlobals* comp,
                    IR::GlobalLabel entry_point,
                    Backend::GenericProgram* program);

void x64_init(CompilerGlobals* comp, CompilerThread* comp_thread,
              Backend::GenericProgram* program_in);

constexpr Backend::PlatformInterface x86_64_platform_interface() {
  Backend::PlatformInterface in = {};
  in.valid_calling_conventions = X64::X64_CALLING_CONVENTIONS;
  in.num_calling_conventions = static_cast<u32>(array_size(X64::X64_CALLING_CONVENTIONS));
  in.system_name = X64::SYSTEM_NAME;
  in.ptr_size = 8;

  in.init = x64_init;
  in.emit_function = x64_emit_function;
  in.emit_start = x64_emit_start;
  in.emit_dyn_library_function = x64_emit_dyn_library_function;

  return in;
}