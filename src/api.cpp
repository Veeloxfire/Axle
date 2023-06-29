
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
  if (options.build.output_file == nullptr) {
    std::cerr << "No output file specified!";
    return 1;
  }

  TRACING_FUNCTION();

  //Setup
  FileLoader file_loader = {};
  Backend::Program out_program = {};
  Structures structures = {};
  StringInterner strings = {};
  NameManager names = {};
  Compilation compilation = {};

  BuiltinTypes builtin_types = {};

  CompilerThread compiler_thread = {};
  CompilerGlobals compiler = {};

  compiler.services.file_loader.set(&file_loader);
  compiler.services.out_program.set(&out_program);
  compiler.services.structures.set(&structures);
  compiler.services.strings.set(&strings);
  compiler.services.names.set(&names);
  compiler.services.compilation.set(&compilation);

  compiler.builtin_types = &builtin_types;

  compiler.pipelines.depend_check._debug_name = "Depend Check";
  compiler.pipelines.type_check._debug_name = "Type Check";
  compiler.pipelines.emit_function._debug_name = "Emit Function";
  compiler.pipelines.emit_global._debug_name = "Emit Global";
  compiler.pipelines.emit_import._debug_name = "Emit Import";
  compiler.pipelines.exec_ir._debug_name = "Exec IR";
  compiler.pipelines.compile_ir._debug_name = "Compile IR";

  compiler.active_threads = 4;

  //Load the builtin types
  init_compiler(options, &compiler, &compiler_thread);
  if (compiler_thread.is_panic()) {
    compiler_thread.errors.print_all();
    return -1;
  }

  {
    FileLocation loc = parse_file_location(file_loader.cwd.full_name->string, compiler.build_options.file_name->string, compiler.services.strings.get()._ptr);

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

#if 0
  if (compiler.print_options.fully_compiled) {
    print_globals(&compiler);
  }
#endif
  {
    ASSERT(out_program.entry_point.label != 0);

    options.executable_format_interface->output_executable(&compiler_thread,
                                                           &out_program, compiler.build_options.output_file);
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