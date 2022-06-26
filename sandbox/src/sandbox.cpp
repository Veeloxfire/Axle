#include "api.h"
#include "format.h"
#include "vm.h"
#include "windows_specifics.h"
#include "backends.h"
#include <iostream>
#include "trace.h"

constexpr char output_file[] = ".\\out\\output.nasm";

int main(int argc, const char** args) {
#ifdef TRACING_ENABLE
  Tracing::start_tracer_threaded("trace.json");
  DEFER() {
    Tracing::end_tracer_threaded();
  };
#endif

  if (argc != 2) {
    std::cerr << "Invalid number of arguments!";
    return 1;
  }

  Windows::MAX_PATH_STR cwd = Windows::get_current_directory();

  APIOptions options = {};

  options.build.current_directory = cwd.str;
  options.build.file_name = args[1];
  options.build.entry_point = "main";
  //options.build.system_name                = "vm";
  //options.build.default_calling_convention = "vm";
  options.build.system_name = "x86_64";
  options.build.default_calling_convention = "x64";
  options.build.output_file = output_file;
  options.build.std_lib_folder = ".\\stdlib";
  options.build.lib_folder = ".\\lib";

  //options.print.ast = true;
  //options.print.pre_reg_alloc = true;
  //options.print.normal_bytecode = true;
  //options.print.comptime_res = true;
  //options.print.coalesce_values = true;
  //options.print.fully_compiled = true;
  //options.print.run_headers = true;
  //options.print.comp_units = true;
  //options.print.comptime_exec = true;

  options.optimize.non_stack_locals = true;

  {
    TRACING_SCOPE("Compiler");
    int out = compile_file_and_write(options);

    if (out != 0) {
      std::cerr << "Error!";
      return out;
    }
  }

  {
    TRACING_SCOPE("Nasm");
    system("nasm -g -fwin64 .\\out\\output.nasm");
  }

  {
    TRACING_SCOPE("Link");
    system("link /LARGEADDRESSAWARE:NO /ENTRY:main /SUBSYSTEM:CONSOLE /OUT:.\\out\\output.exe .\\out\\output.obj ..\\lib\\kernel32.lib");
  }
  return 0;
}