
static_assert(sizeof(void*) == 8, "Currently only builds in 64 bit");

#include <Axle/api.h>

#include <AxleUtil/io.h>
#include <AxleUtil/strings.h>
#include <AxleUtil/files.h>

#ifdef AXLE_TRACING
#include <Tracer/trace.h>
#endif

#include "ast.h"
#include "parser.h"

#include "compiler.h"

#include "backends.h"

#include <chrono>

int compile_and_write(const APIOptions& options) {
#ifdef AXLE_TRACING
  TRACING_FUNCTION();
#endif

  //Setup
  Backend::ProgramData program = {};
  program.extra = options.program_extra;

  StringInterner strings = {};//strings created first -> strings deleted last
  FileLoader file_loader = {};
  Structures structures = {};
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

  compiler.pipelines.comp_structure._debug_name = lit_view_arr("Structure");
  compiler.pipelines.comp_body._debug_name = lit_view_arr("Body");
  compiler.pipelines.comp_signature._debug_name = lit_view_arr("Signature");
  compiler.pipelines.comp_global._debug_name = lit_view_arr("Global");
  compiler.pipelines.comp_import._debug_name = lit_view_arr("Import");
  compiler.pipelines.comp_export._debug_name = lit_view_arr("Export");

  compilation.dependencies.depend_check_pipe = &compiler.pipelines.depend_check;

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
    FileLocation loc = parse_file_location(view_arr(compiler.build_options.lib_folder),
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
#ifdef AXLE_TRACING
    TRACING_SCOPE("Write output file");
#endif

#ifdef ASSERT_EXCEPTIONS
    try {
#endif
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
#ifdef ASSERT_EXCEPTIONS
    }
    catch (const std::exception& e) {
      const char* message = e.what();
      const ViewArr<const char> message_view = { message, strlen_ts(message) };
      compiler_thread.report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Assertion Failed with message: {}", message_view);
    }
#endif

    if (compiler_thread.is_panic()) {
      ERROR_CODE code = print_error_messages(compiler_thread.errors.error_messages);
      IO::err_format("Compilation was not completed due to an error!\nError Code '{}'\n", code);
      return -2;
    }
  }
  return 0;
}
