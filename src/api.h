#pragma once
#include "safe_lib.h"

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

  Backend::GenericProgram* program;
  const Backend::PlatformInterface* platform_interface;
  const Backend::ExecutableFormatInterface* executable_format_interface;
};

int compile_and_write(const APIOptions& options);