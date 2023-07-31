
static_assert(sizeof(void*) == 8, "Currently only builds in 64 bit");

#include "PE_file_format.h"

#include "api.h"

#include "parser.h"

#include "compiler.h"
#include "strings.h"
#include "ast.h"

#include "windows_specifics.h"
#include "files.h"
#include "backends.h"

#include "trace.h"

#include <utility>
#include <iostream>

#include <chrono>

#if 0
void print_globals(CompilerGlobals* comp) {
  //theoretically very slow to do

  auto i = comp->globals_single_threaded.begin_const_iter();
  auto end = comp->globals_single_threaded.end_const_iter();

  for (; i != end; i.next()) {
    const Global* g = i.get();

    format_print("{} = {}\n", g->decl.name, g->decl.type.name);

    if (g->decl.type.struct_type() == STRUCTURE_TYPE::LAMBDA) {
      usize label = *(usize*)g->constant_value;
      format_print("{}:\n", label);

      auto fi = comp->functions_single_threaded.begin_const_iter();
      auto fend = comp->functions_single_threaded.end_const_iter();
      for (; fi != fend; fi.next()) {
        if (fi.get()->code_block.label == label) {
          const Function* f = fi.get();
          const CodeBlock* block = &f->code_block;

          ByteCode::print_bytecode(comp->build_options.endpoint_system->reg_name_from_num, stdout, block->code.data, block->code.size);
          break;
        }
      }
    }

    IO::print('\n');
  }
}
#endif

int compile_and_write(const APIOptions& options) {
  TRACING_FUNCTION();

  Backend::GenericProgram* program_in = options.program;

  //Setup
  FileLoader file_loader = {};
  Structures structures = {};
  StringInterner strings = {};
  NameManager names = {};
  Compilation compilation = {};

  BuiltinTypes builtin_types = {};

  CompilerThread compiler_thread = {};
  CompilerGlobals compiler = {};

  compiler.services.file_loader.set(&file_loader);
  compiler.services.out_program.set(program_in);
  compiler.services.structures.set(&structures);
  compiler.services.strings.set(&strings);
  compiler.services.names.set(&names);
  compiler.services.compilation.set(&compilation);

  compiler.builtin_types = &builtin_types;

  compiler.pipelines.comp_structure._debug_name = "Structure";
  compiler.pipelines.comp_body._debug_name = "Body";
  compiler.pipelines.comp_signature._debug_name = "Signature";
  compiler.pipelines.comp_global._debug_name = "Global";
  compiler.pipelines.comp_import._debug_name = "Import";
  compiler.pipelines.comp_export._debug_name = "Export";

  compilation.dependencies.depend_check_pipe = &compiler.pipelines.depend_check;

  compiler.active_threads = options.build.extra_threads + 1;

  //Load the builtin types
  init_compiler(options, &compiler, &compiler_thread);
  if (compiler_thread.is_panic()) {
    compiler_thread.errors.print_all();
    return -1;
  }

  if (program_in == nullptr) {
    compiler_thread.report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{}, "Program was missing from the api input");
    compiler_thread.errors.print_all();
    return -1;
  }

  options.platform_interface->init(&compiler, &compiler_thread, program_in);
  if (compiler_thread.is_panic()) {
    compiler_thread.errors.print_all();
    return -1;
  }

  {
    FileLocation loc = parse_file_location(compiler.build_options.lib_folder->string,
                                           compiler.build_options.file_name->string,
                                           compiler.services.strings.get()._ptr);

    compiler.build_file_namespace = compiler.new_namespace();


    compiler.services.file_loader.get()->unparsed_files.insert(FileImport{ loc, compiler.build_file_namespace, Span{} });//use null span

    //Compilation
    compile_all(&compiler, &compiler_thread);
    if (compiler.is_global_panic() || compiler_thread.is_panic()) {
      ERROR_CODE code = print_error_messages(compiler.global_errors);
      std::cerr << "Compilation was not completed due to an error!\nError Code '"
        << error_code_string(code)
        << "'\n";
      return -2;
    }
  }

  {
    TRACING_SCOPE("Write output file");

#ifdef ASSERT_EXCEPTIONS
    try {
#endif
      if (compiler.build_options.is_library) {
        ASSERT(program_in->entry_point == IR::NULL_GLOBAL_LABEL);
        if (program_in->dyn_exports.size == 0) {
          IO::print("Warning: Dynamic Library had 0 exports\n");
        }

        options.executable_format_interface->output_dynamic_library(&compiler_thread, program_in,
                                                                    compiler.build_options.output_name, compiler.build_options.output_folder);
      }
      else {
        ASSERT(program_in->entry_point != IR::NULL_GLOBAL_LABEL);
        options.executable_format_interface->output_executable(&compiler_thread, program_in,
                                                               compiler.build_options.output_name, compiler.build_options.output_folder);
      }
#ifdef ASSERT_EXCEPTIONS
    }
    catch (const std::exception& e) {
      compiler_thread.report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Assertion Failed with message: {}", e.what());
    }
#endif

    if (compiler_thread.is_panic()) {
      ERROR_CODE code = print_error_messages(compiler_thread.errors.error_messages);
      std::cerr << "Compilation was not completed due to an error!\nError Code '"
        << error_code_string(code)
        << "'\n";
      return -2;
    }
  }
  return 0;
}