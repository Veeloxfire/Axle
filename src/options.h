#pragma once
#include "calling_conventions.h"

struct OptimizationOptions {
  bool non_stack_locals = false;
};

struct BuildOptions {
  const char* file_name;
  const char* entry_point;
  const char* output_file = nullptr;

  const System* system;
  const CallingConvention* calling_convention;
};

struct PrintOptions {
  bool ast             = false;
  bool pre_reg_alloc   = false;
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