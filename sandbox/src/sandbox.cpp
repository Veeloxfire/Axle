#include "api.h"
#include "format.h"
#include "windows_specifics.h"
#include "x64_backend.h"
#include "PE_file_format.h"
#include <iostream>
#include "trace.h"

constexpr char output_file[] = ".\\out\\output.exe";

int main(int argc, const char** args) {
#ifdef TRACING_ENABLE
  Tracing::start_tracer_threaded("info.trace");
  DEFER() {
    Tracing::end_tracer_threaded();
  };

  Tracing::new_traced_thread();
#endif

  if (argc != 2) {
    std::cerr << "Invalid number of arguments!";
    return 1;
  }

  Windows::MAX_PATH_STR cwd = Windows::get_current_directory();

  constexpr Backend::PlatformInterface pi = x86_64_platform_interface();
  constexpr Backend::ExecutableFormatInterface efi = pe_plus_file_interface();

  APIOptions options = {};

  options.platform_interface = &pi;
  options.executable_format_interface = &efi;

  options.build.default_calling_convention = 0;

  options.build.current_directory = cwd.str;
  options.build.file_name = args[1];
  options.build.entry_point = "main";

  options.build.output_file = output_file;
  options.build.output_file_type = OutputFileType::PE;

  options.build.std_lib_folder = ".\\stdlib";
  options.build.lib_folder = ".\\lib";

  //options.print.ast = true;
  //options.print.finished_ir = true;
  //options.print.comptime_res = true;
  //options.print.coalesce_values = true;
  //options.print.fully_compiled = true;
  //options.print.reg_mapping = true;
  //options.print.run_headers = true;
  //options.print.comp_units = true;
  //options.print.comptime_exec = true;

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