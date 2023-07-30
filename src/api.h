#pragma once
#include "comp_utilities.h"

namespace Backend {
  struct GenericProgram;
  struct PlatformInterface;
  struct ExecutableFormatInterface;
}

struct APIOptimizationOptions {
#if 0
  bool non_stack_locals = false;
#endif
};

struct APIBuildOptions {
  bool debug_break_on_entry = false;
  const char* current_directory = nullptr;

  const char* file_name     = nullptr;
  const char* source_folder = nullptr;

  bool library = false;
  const char* entry_point = nullptr;

  const char* std_lib_folder = nullptr;
  const char* lib_folder = nullptr;

  u32 default_calling_convention = 0;

  const char* output_name = nullptr;
  const char* output_folder = nullptr;
  OutputFileType output_file_type;

  u32 extra_threads = 0;
};

struct APIPrintOptions {
  bool ast             = false;
  bool comptime_res    = false;
  bool comptime_exec   = false;
  bool finished_ir     = false;
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

  Backend::GenericProgram* program;
  const Backend::PlatformInterface* platform_interface;
  const Backend::ExecutableFormatInterface* executable_format_interface;
};

int compile_and_write(const APIOptions& options);