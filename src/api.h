#pragma once
#include "comp_utilities.h"

namespace Backend {
  struct ProgramData;

  using PROGRAM_INIT = void(*)(CompilerGlobals* comp,
                               CompilerThread* comp_thread,
                               ProgramData* program);

  using EMIT_FUNCTION = void(*)(CompilerGlobals* comp,
                                CompilerThread* comp_thread,
                                const IR::IRStore* ir,
                                const CallingConvention* convention,
                                ProgramData* program);

  using EMIT_START = void(*)(CompilerGlobals* comp,
                             IR::GlobalLabel entry_point,
                             ProgramData* program);

  using EMIT_DYNAMIC_LIBRARY_FUNCTION = void(*) (CompilerThread* comp_thread,
                                                 const IR::DynLibraryImport* lib_import,
                                                 const CallingConvention* convention,
                                                 ProgramData* program);

  struct PlatformInterface {
    CallingConvention const* const* valid_calling_conventions;
    u32 num_calling_conventions;

    System system;
    ViewArr<const char> system_name;

    usize ptr_size;

    PROGRAM_INIT init;
    EMIT_START emit_start;
    EMIT_FUNCTION emit_function;
    EMIT_DYNAMIC_LIBRARY_FUNCTION emit_dyn_library_function;
  };

  using OUTPUT_EXECUTABLE = void(*) (CompilerThread* comp_thread,
                                     const ProgramData* program,
                                     const InternString* out_name, const InternString* out_folder);

  struct ExecutableFormatInterface {
    OutputFileType type;
    OUTPUT_EXECUTABLE output_executable;
    OUTPUT_EXECUTABLE output_dynamic_library;
  };

  struct ProgramExtra {};
}

struct APIOptimizationOptions {
#if 0
  bool non_stack_locals = false;
#endif
};

struct APIBuildOptions {
  bool debug_break_on_entry = false;
  ViewArr<const char> current_directory = {};

  ViewArr<const char> file_name = {};
  ViewArr<const char> source_folder = {};

  bool library = false;
  ViewArr<const char> entry_point = {};

  ViewArr<const char> std_lib_folder = {};
  ViewArr<const char> lib_folder = {};

  u32 default_calling_convention = 0;

  ViewArr<const char> output_name = {};
  ViewArr<const char> output_folder = {};
  OutputFileType output_file_type;

  u32 extra_threads = 0;
};

struct APIPrintOptions {
  bool ast             = false;
  bool comptime_res    = false;
  bool comptime_exec   = false;
  bool finished_ir     = false;
  bool finished_mc     = false;
  bool run_headers     = false;
  bool register_select = false;
  bool file_loads      = false;
  bool comp_units      = false;
  bool work            = false;
};

struct APIOptions {
  APIOptimizationOptions optimize;
  APIBuildOptions build;
  APIPrintOptions print;

  Backend::ProgramExtra* program_extra;
  const Backend::PlatformInterface* platform_interface;
  const Backend::ExecutableFormatInterface* executable_format_interface;
};

int compile_and_write(const APIOptions& options);