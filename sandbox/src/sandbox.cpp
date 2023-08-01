#include "api.h"
#include "format.h"
#include "windows_specifics.h"
#include "x64_backend.h"
#include "PE_file_format.h"
#include <iostream>
#include "trace.h"

constexpr char output_folder[] = ".\\out";
constexpr char output_name[] = "output";

int main(int argc, const char** args) {
#ifdef TRACING_ENABLE
  Tracing::start_default_tracing_thread("info.trace");
  DEFER() {
    Tracing::end_default_tracing_thread();
  };
#endif

  if (argc != 2) {
    std::cerr << "Invalid number of arguments!";
    return 1;
  }

  Windows::MAX_PATH_STR cwd = Windows::get_current_directory();

  constexpr Backend::PlatformInterface pi = x86_64_platform_interface();
  constexpr Backend::ExecutableFormatInterface efi = pe_plus_file_interface();

  X64::Program program = {};

  APIOptions options = {};

  options.program = &program;

  options.platform_interface = &pi;
  options.executable_format_interface = &efi;

  options.build.default_calling_convention = 0;

  options.build.current_directory = cwd.str;
  options.build.file_name = args[1];

  options.build.library = false;
  if (!options.build.library) {
    options.build.entry_point = "main";
  }

  options.build.output_name = output_name;
  options.build.output_folder = output_folder;
  options.build.output_file_type = efi.type;

  options.build.std_lib_folder = "..\\stdlib";
  options.build.lib_folder = ".";

  options.build.extra_threads = 3;

  options.print.ast = false;
  options.print.comptime_res = false;
  options.print.comptime_exec = false;
  options.print.finished_ir = false;
  options.print.run_headers = false;
  options.print.register_select = false;
  options.print.file_loads = false;
  options.print.comp_units = false;
  options.print.work = false;

  //options.optimize.non_stack_locals = true;
  
  {
    TRACING_SCOPE("Compiler");
    int out = compile_and_write(options);

    if (out != 0) {
      std::cerr << "Error!";
      return out;
    }
  }

  return 0;
}