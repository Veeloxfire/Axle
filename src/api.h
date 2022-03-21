#pragma once
#include "safe_lib.h"
#include "Program.h"

struct APIOptimizationOptions {
  bool non_stack_locals = false;
};

struct APIBuildOptions {
  const char* file_name   = nullptr;
  const char* entry_point = nullptr;
  const char* output_file = nullptr;

  const char* std_lib_folder = nullptr;
  const char* lib_folder = nullptr;

  const char* system_name = nullptr;
  const char* default_calling_convention = nullptr;
};

struct APIPrintOptions {
  bool ast             = false;
  bool pre_reg_alloc   = false;
  bool comptime_res    = false;
  bool comptime_exec   = false;
  bool normal_bytecode = false;
  bool fully_compiled  = false;
  bool coalesce_values = false;
  bool run_headers     = false;
  bool file_loads      = false;
  bool comp_units      = false;
};

struct APIOptions {
  APIOptimizationOptions optimize;
  APIBuildOptions build;
  APIPrintOptions print;
};


struct RunOutput {
  int return_code;
  uint64_t program_return;
};

RunOutput run_as_machine_code(Program* prog);
RunOutput run_in_vm(Program* prog);


RunOutput compile_file_and_run(const APIOptions& options);
int compile_file_and_write(const APIOptions& options);

int compile_file(const APIOptions& options, Program* out_program);
RunOutput run_program(const APIOptions& options, Program* prog);
void print_program(const APIOptions& opts, const Program& prog);