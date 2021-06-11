#pragma once
#include "calling_conventions.h"

struct OptimizationOptions {
  bool non_stack_locals = false;
};

struct BuildOptions {
  const char* file_name   = nullptr;
  const char* entry_point = nullptr;
  const char* output_file = nullptr;

  const System* system = nullptr;
  const CallingConvention* default_calling_convention = nullptr;
};

struct PrintOptions {
  bool ast             = false;
  bool pre_reg_alloc   = false;
  bool comptime_res    = false;
  bool comptime_exec   = false;
  bool normal_bytecode = false;
  bool fully_compiled  = false;
  bool coalesce_values = false;
  bool run_headers     = false;;
};

struct Options {
  OptimizationOptions optimize;
  BuildOptions build;
  PrintOptions print;
};