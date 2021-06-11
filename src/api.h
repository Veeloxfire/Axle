#pragma once
#include "safe_lib.h"
#include "options.h"
#include "Program.h"


struct RunOutput {
  int return_code;
  uint64_t program_return;
};

RunOutput run_as_machine_code(Program* prog);
RunOutput run_in_vm(Program* prog);


RunOutput compile_file_and_run(const Options& options);
int compile_file_and_write(const Options& options);

int compile_file(const Options& options, Program* out_program);
RunOutput run_program(const Options& options, Program* prog);
void print_program(const Options& opts, const Program& prog);