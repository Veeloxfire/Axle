
static_assert(sizeof(void*) == 8, "Currently only builds in 64 bit");

#include <Axle/api.h>

#include <AxleUtil/io.h>
#include <AxleUtil/strings.h>
#include <AxleUtil/files.h>


#include "parser.h"
#include "compiler.h"
#include "backends.h"

namespace IO = Axle::IO;

int compile_and_write(const APIOptions& options) {
  AXLE_TELEMETRY_FUNCTION();

  //Setup
  Backend::ProgramData program = {};
  program.extra = options.program_extra;

  Axle::StringInterner strings = {};//strings created first -> strings deleted last
  FileLoader file_loader = {};
  Structures structures = {options.platform_interface->ptr_size, options.platform_interface->ptr_align};
  NameManager names = {};
  Compilation compilation = {};

  BuiltinTypes builtin_types = {};

  CompilerGlobals compiler = {};

  compiler.services.file_loader.set(&file_loader);
  compiler.services.out_program.set(&program);
  compiler.services.structures.set(&structures);
  compiler.services.strings.set(&strings);
  compiler.services.names.set(&names);
  compiler.services.compilation.set(&compilation);

  compiler.builtin_types = &builtin_types;

  compiler.pipelines.depend_check._debug_name = Axle::lit_view_arr("Depend Check");
  compiler.pipelines.type_check._debug_name = Axle::lit_view_arr("Type Check");
  compiler.pipelines.emit._debug_name = Axle::lit_view_arr("Emit");

  compilation.pipes = &compiler.pipelines;

  compiler.active_threads = options.build.extra_threads + 1;

  CompilerThread compiler_thread = {};

  //Load the builtin types
  init_compiler(options, &compiler, &compiler_thread);
  if (compiler_thread.is_panic()) {
    compiler_thread.errors.print_all();
    return -1;
  }

  if (options.program_extra == nullptr) {
    compiler_thread.report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{}, "Program was missing from the api input");
    compiler_thread.errors.print_all();
    return -1;
  }

  options.platform_interface->init(&compiler, &compiler_thread, &program);
  if (compiler_thread.is_panic()) {
    compiler_thread.errors.print_all();
    return -1;
  }

  {
    Axle::FileLocation loc = parse_file_location(view_arr(compiler.build_options.lib_folder),
                                           view_arr(compiler.build_options.file_name),
                                           compiler.services.strings.get()._ptr);

    compiler.build_file_namespace = compiler.new_namespace();

    {
      auto files = compiler.services.file_loader.get();
      files->unparsed_files.insert(FileImport{ loc, compiler.build_file_namespace, Span{} });//use null span
    }

    //Compilation
    compile_all(&compiler, &compiler_thread);
    if (compiler.is_global_panic() || compiler_thread.is_panic()) {
      ERROR_CODE code = print_error_messages(compiler.global_errors);
      IO::err_format("Compilation was not completed due to an error!\nError Code '{}'\n", code);
      return -2;
    }
  }

  {
    AXLE_TELEMETRY_SCOPE("Write output file");

    try {
      if (compiler.build_options.is_library) {
        ASSERT(program.entry_point == IR::NULL_GLOBAL_LABEL);
        if (program.dyn_exports.size == 0) {
          IO::print("Warning: Dynamic Library had 0 exports\n");
        }

        options.executable_format_interface->output_dynamic_library(&compiler_thread, &program,
                                                                    compiler.build_options.output_name, compiler.build_options.output_folder);
      }
      else {
        ASSERT(program.entry_point != IR::NULL_GLOBAL_LABEL);
        options.executable_format_interface->output_executable(&compiler_thread, &program,
                                                               compiler.build_options.output_name, compiler.build_options.output_folder);
      }
    }
    catch (const std::exception& e) {
      const char* message = e.what();
      const Axle::ViewArr<const char> message_view = { message, Axle::strlen_ts(message) };
      compiler_thread.report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Assertion Failed with message: {}", message_view);
    }

    if (compiler_thread.is_panic()) {
      ERROR_CODE code = print_error_messages(compiler_thread.errors.error_messages);
      IO::err_format("Compilation was not completed due to an error!\nError Code '{}'\n", code);
      return -2;
    }
  }
  return 0;
}
