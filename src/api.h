#pragma once
#include "comp_utilities.h"

namespace Backend {
  struct PlatformInterface;
  struct ExecutableFormatInterface;
}

struct APIOptimizationOptions {
#if 0
  bool non_stack_locals = false;
#endif
};

struct APIBuildOptions {
  const char* current_directory = nullptr;
  const char* file_name   = nullptr;
  const char* entry_point = nullptr;

  const char* std_lib_folder = nullptr;
  const char* lib_folder = nullptr;

  u32 default_calling_convention = 0;

  const char* output_file = nullptr;
  OutputFileType output_file_type;
};

struct APIPrintOptions {
  bool ast             = false;
  bool pre_reg_alloc   = false;
  bool comptime_res    = false;
  bool comptime_exec   = false;
  bool finished_ir     = false;
  bool run_headers     = false;
  bool file_loads      = false;
  bool comp_units      = false;
};

struct APIOptions {
  APIOptimizationOptions optimize;
  APIBuildOptions build;
  APIPrintOptions print;

  const Backend::PlatformInterface* platform_interface;
  const Backend::ExecutableFormatInterface* executable_format_interface;
};


int compile_and_write(const APIOptions& options);
int compile_and_print(const APIOptions& options);