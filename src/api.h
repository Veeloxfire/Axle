#pragma once
#include "safe_lib.h"
#include "options.h"

struct Program {
  const uint8_t* code = nullptr;
  size_t size = 0;
  size_t entry = 0;

  constexpr Program() = default;

  constexpr void take(Program&& prog) {
    code = prog.code;
    size = prog.size;
    entry = prog.entry;

    prog.code = nullptr;
    prog.size = 0;
    prog.entry = 0;
  }

  constexpr Program(const uint8_t* const c,
                    const size_t s,
                    const size_t e) noexcept
    : code(c), size(s), entry(e)
  {}

  constexpr Program(Program&& prog) noexcept
  {
    take(std::move(prog));
  }

  constexpr Program& operator=(Program&& prog) noexcept
  {
    take(std::move(prog));
    return *this;
  }

  ~Program() { free_no_destruct(code); }
};

struct RunOutput {
  int return_code;
  uint64_t program_return;
};

RunOutput run_as_machine_code(const Program& prog);
RunOutput run_in_vm(const Program& prog);

int compile_file(const Options& options,
                 Program* out_program);

RunOutput compile_file_and_run(const Options& options);
int compile_file_and_write(const Options& options);