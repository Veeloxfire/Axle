
static_assert(sizeof(void*) == 8, "Currently only builds in 64 bit");

#include "PE_file_format.h"

#include "api.h"

#include "vm.h"
#include "bytecode.h"
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

void print_globals(CompilerGlobals* comp) {
  //theoretically very slow to do

  auto i = comp->globals_single_threaded.begin_const_iter();
  auto end = comp->globals_single_threaded.end_const_iter();

  for (; i != end; i.next()) {
    const Global* g = i.get();

    format_print("{} = {}\n", g->decl.name, g->decl.type.name);

    if (g->decl.type.struct_type() == STRUCTURE_TYPE::LAMBDA) {
      usize label = *(usize*)g->constant_value.ptr;
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

RunOutput run_in_vm(Program* prog) {
  Errors errors ={};
  VM vm ={};

  vm.errors = &errors;
  vm_rum(&vm, prog);

  if (errors.panic) {
    errors.print_all();
    return { 1, 0 };
  }

  return { 0, vm.registers[convention_vm.return_register].b64.reg };
}

RunOutput run_as_machine_code(Program* prog) {
  //Load possible dlls
  Array<Windows::ActiveDll> dlls = {};
  if (prog->imports.ptr != nullptr) {
    dlls = Windows::load_dlls(prog);
  }

  auto exe = Windows::get_exectuable_memory<uint8_t>(prog->code_size);
  memcpy_ts(exe.ptr, exe.size, prog->code.ptr, prog->code_size);

  exe.entry = prog->entry_point;
  auto res = exe.call<uint64_t>();

  Windows::free_executable_memory(exe);
  return { 0, res };
}

int compile_file(const APIOptions& options,
                 Program* out_program) {

  TRACING_FUNCTION();

  //Setup
  VM vm = {};
  FileLoader file_loader = {};
  Structures structures = {};
  StringInterner strings = {};
  NameManager names = {};
  Compilation compilation = {};

  BuiltinTypes builtin_types = {};

  CompilerThread compiler_thread = {};
  CompilerGlobals compiler = {};

  compiler.services.vm.set(&vm);
  compiler.services.file_loader.set(&file_loader);
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
  compiler.pipelines.exec_code._debug_name = "Exec Code";

  //Load the builtin types
  init_compiler(options, &compiler, &compiler_thread);
  if (compiler_thread.is_panic()) {
    compiler_thread.errors.print_all();
    return -1;
  }

  {
    FileLocation loc = parse_file_location(file_loader.cwd.full_name->string, compiler.build_options.file_name->string, compiler.services.strings.get()._ptr);

    compiler.build_file_namespace = compiler.new_namespace();


    compiler.services.file_loader.get()->unparsed_files.insert(FileImport{loc, compiler.build_file_namespace, Span{}});//use null span

    //Compilation
    compile_all(&compiler, &compiler_thread);
    if (compiler.is_global_panic()) {
      ERROR_CODE code = print_error_messages(compiler.global_errors);
      std::cerr << "Compilation was not completed due to an error!\nError Code '"
        << error_code_string(code)
        << "'\n";
      return -2;
    }
  }

  if (compiler.print_options.fully_compiled) {
    print_globals(&compiler);
  }

  //Backend
  {
    TRACING_SCOPE("Backend");
    build_data_section_for_vm(out_program, &compiler);

    compile_backend(&compiler, &compiler_thread, out_program, compiler.build_options.endpoint_system);
    if (compiler_thread.is_panic()) {
      compiler_thread.errors.print_all();
      return -3;
    }
  }
  return 0;
}

RunOutput run_program(const APIOptions& options, Program* prog) {  
  if (slow_string_eq(options.build.system_name, system_vm.name)) {
    if (options.print.run_headers) {
      std::cout << "\n=== Running in VM ===\n\n";
    }
    RunOutput ret = run_in_vm(prog);
    if (options.print.run_headers) {
      std::cout << "\n=====================\n\n";
    }

    return ret;
  }
  else if (slow_string_eq(options.build.system_name, system_x86_64.name)) {
    if (options.print.run_headers) {
      std::cout << "\n=== Running machine code (JIT) ===\n\n";
    }
    RunOutput ret = run_as_machine_code(prog);
    if (options.print.run_headers) {
      std::cout << "\n==================================\n\n";
    }

    return ret;
  }
  else {
    std::cerr << "Could not run! Invalid convention and system conbination\n";
    return { 1, 0 };
  }
}

RunOutput compile_file_and_run(const APIOptions& options) {
  Program program ={};

  const int res = compile_file(options, &program);

  if (res != 0) {
    return { res,  0 };
  }

  return run_program(options, &program);
}

int compile_file_and_write(const APIOptions& options) {
  if (options.build.output_file == nullptr) {
    std::cerr << "No output file specified!";
    return 1;
  }

  TRACING_FUNCTION();

  //Setup
  VM vm ={};
  FileLoader file_loader ={};
  Structures structures ={};
  StringInterner strings ={};
  NameManager names = {};
  Compilation compilation = {};
  
  BuiltinTypes builtin_types ={};

  CompilerThread compiler_thread = {};
  CompilerGlobals compiler ={};

  compiler.services.vm.set(&vm);
  compiler.services.file_loader.set(&file_loader);
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
  compiler.pipelines.exec_code._debug_name = "Exec Code";

  //Load the builtin types
  init_compiler(options, &compiler, &compiler_thread);
  if (compiler_thread.is_panic()) {
    compiler_thread.errors.print_all();
    return -1;
  }

  {
    FileLocation loc = parse_file_location(file_loader.cwd.full_name->string, compiler.build_options.file_name->string, compiler.services.strings.get()._ptr);

    compiler.build_file_namespace = compiler.new_namespace();


    compiler.services.file_loader.get()->unparsed_files.insert(FileImport{loc, compiler.build_file_namespace, Span{}});//use null span

    //Compilation
    compile_all(&compiler, &compiler_thread);
    if (compiler.is_global_panic()) {
      ERROR_CODE code = print_error_messages(compiler.global_errors);
      std::cerr << "Compilation was not completed due to an error!\nError Code '"
        << error_code_string(code)
        << "'\n";
      return -2;
    }
  }

  ASSERT(slow_string_eq(options.build.system_name, system_x86_64.name));

  if (compiler.print_options.fully_compiled) {
    print_globals(&compiler);
  }
  
  nasm_backend(options.build.output_file, &compiler, &compiler_thread);
  if (compiler_thread.is_panic()) {
    compiler_thread.errors.print_all();
    return -3;
  }

  return 0;
}