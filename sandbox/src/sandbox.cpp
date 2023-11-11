#include <AxleUtil/args.h>
#include <AxleUtil/io.h>
#include <AxleUtil/os/os_windows.h>

#include <Axle/api.h>
#include <Axle/backends/x64_backend.h>
#include <Axle/backends/PE_file_format.h>

#ifdef AXLE_TRACING
#include <Tracer/trace.h>
#endif

struct ArgErrors {
  bool errored;

  template<typename ... T>
  constexpr void report_error(const Format::FormatString& fstring, const T& ...ts) {
    IO::err_format(fstring, ts...);
    IO::err_print('\n');
    errored = true;
  }
};

int main(int argc, const char** args) {
#if defined(TRACING_ENABLE) && defined(AXLE_TRACING)
  Tracing::start_default_tracing_thread("info.trace");
  DEFER() {
    Tracing::end_default_tracing_thread();
  };
#endif

  ViewArr<const char> out_folder;
  ViewArr<const char> out_name;
  ViewArr<const char> stdlib;
  ViewArr<const char> lib;
  ViewArr<const char> in;

  {
    ArgErrors errors = {};

    clArg::ArgsList arg_list = { static_cast<usize>(argc), args };
    {
      bool _unused = clArg::parse_arg(errors, arg_list, lit_view_arr("in"), in);
    }
    {
      bool _unused = clArg::parse_arg(errors, arg_list, lit_view_arr("stdlib"), stdlib);
    }
    {
      bool _unused = clArg::parse_arg(errors, arg_list, lit_view_arr("lib"), lib);
    }
    {
      bool _unused = clArg::parse_arg(errors, arg_list, lit_view_arr("out_name"), out_name);
    }
    {
      bool _unused = clArg::parse_arg(errors, arg_list, lit_view_arr("out_folder"), out_folder);
    }

    if (errors.errored) {
      return 1;
    }
  }

  ASSERT(out_folder.data != nullptr);
  ASSERT(out_name.data != nullptr);
  ASSERT(stdlib.data != nullptr);
  ASSERT(lib.data != nullptr);

  Windows::NativePath cwd = Windows::get_current_directory();
  IO::format("CWD: {}\n", cwd.view());

  constexpr Backend::PlatformInterface pi = x86_64_platform_interface();
  constexpr Backend::ExecutableFormatInterface efi = pe_plus_file_interface();

  X64::ProgramExtra program_extra = {};

  APIOptions options = {};

  options.program_extra = &program_extra;

  options.platform_interface = &pi;
  options.executable_format_interface = &efi;

  options.build.default_calling_convention = 0;
  options.build.debug_break_on_entry = true;

  options.build.current_directory = cwd.view();
  options.build.file_name = in;

  options.build.library = false;
  if (!options.build.library) {
    options.build.entry_point = lit_view_arr("main");
  }

  options.build.output_name = out_name;
  options.build.output_folder = out_folder;
  options.build.output_file_type = efi.type;

  options.build.std_lib_folder = stdlib;
  options.build.lib_folder = lib;

  options.build.extra_threads = 3;

  options.print.ast = false;
  options.print.comptime_res = false;
  options.print.comptime_exec = false;
  options.print.finished_ir = false;
  options.print.finished_mc = false;
  options.print.run_headers = false;
  options.print.register_select = false;
  options.print.file_loads = true;
  options.print.comp_units = false;
  options.print.work = false;

  //options.optimize.non_stack_locals = true;
  
  {
  #ifdef AXLE_TRACING
    TRACING_SCOPE("Compiler");
  #endif
    int out = compile_and_write(options);

    if (out != 0) {
      IO::err_print("Error!");
      return out;
    }
  }

  return 0;
}
