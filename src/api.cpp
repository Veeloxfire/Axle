
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

#include <utility>
#include <iostream>

#include <chrono>

void print_program(const APIOptions& opts, const Program& prog) {
  if (slow_string_eq(opts.build.system_name, system_vm.name)) {
    std::cout << "\n=== Print Linked Bytecode ===\n\n";
    std::cout << "Data:\n";
    print_as_bytes(prog.data.ptr, prog.data_size);

    std::cout << "\n\nProgram:\n";
    ByteCode::print_bytecode(system_vm.reg_name_from_num, stdout, prog.code.ptr, prog.code_size);
    std::cout << "\n=============================\n\n";
  }
  else if (slow_string_eq(opts.build.system_name, system_x86_64.name)) {
    std::cout << "\n=== Print x86_64 Machine code ===\n\n";
    std::cout << "Data:\n";
    print_as_bytes(prog.data.ptr, prog.data_size);

    std::cout << "\n\nProgram:\n";
    print_x86_64(prog.code.ptr, prog.code_size);
    std::cout << "\n=================================\n\n";
  }
  else {
    std::cerr << "Incorrect system for printing combined bytecode!\n";
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

  //Setup
  StringInterner strings ={};
  BuiltinTypes builtin_types ={};
  Structures structures ={};
  Lexer lexer ={};
  Parser parser ={};
  VM vm = {};
  NamesHandler names = {};
  Errors errors ={};
  FileLoader file_loader ={};

  Compiler compiler ={};

  compiler.services.lexer = &lexer;
  compiler.services.parser = &parser;
  compiler.services.vm = &vm;
  compiler.services.errors = &errors;
  compiler.services.names = &names;
  compiler.services.file_loader = &file_loader;
  compiler.services.builtin_types = &builtin_types;
  compiler.services.structures = &structures;
  compiler.services.strings = &strings;

  //Load the builtin types
  init_compiler(options, &compiler);
  if (compiler.is_panic()) {
    compiler.services.errors->print_all();
    return 1;
  }

  {
    FileLocation loc = parse_file_location(compiler.build_options.file_name->string, nullptr, compiler.services.strings);


    NamespaceIndex ns_index = compiler.services.names->new_namespace();
    compiler.build_file_namespace = ns_index;


    compiler.services.file_loader->unparsed_files.insert(FileImport{ loc, ns_index, Span{} });//use null span

    ////Parsing/loading
    //ERROR_CODE ret = parse_all_unparsed_files_with_imports(&compiler);
    //if (ret != ERROR_CODE::NO_ERRORS) {
    //  std::cerr << "Parsing was not completed due to an error!\nError Code '"
    //    << error_code_string(ret)
    //    << "'\n";
    //  return 1;
    //}

    //Compilation
    ERROR_CODE ret = compile_all(&compiler);
    if (ret != ERROR_CODE::NO_ERRORS) {
      std::cerr << "Compilation was not completed due to an error!\nError Code '"
        << error_code_string(ret)
        << "'\n";
      return 1;
    }
  }

  if (compiler.is_panic()) {
    std::cerr << "Compiler paniced but did not have an error message\n";
    return -1;
  }

  //Backend
  build_data_section_for_vm(out_program, &compiler);
  
  compile_backend(out_program, &compiler, compiler.build_options.endpoint_system);
  if (compiler.is_panic()) {
    compiler.services.errors->print_all();
    return 1;
  }

  if (compiler.print_options.fully_compiled) {
    print_program(options, *out_program);
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

  Program program ={};

  int res = compile_file(options, &program);

  if (res != 0) {
    return res;
  }

  if (slow_string_eq(options.build.system_name, system_vm.name)) {
    std::cerr << "Cannot write bytecode to a file!";

    return res;
  }
  else if (slow_string_eq(options.build.system_name, system_x86_64.name)) {
    std::cout << "Writing to file \"" << options.build.output_file << "\"\n";

    StringInterner strings ={};

    PE_File_Build pe_file_build ={};

    CodeSection code_section ={};
    code_section.bytes = program.code.ptr;
    code_section.size  = program.code_size;
    code_section.entry_point = program.entry_point;

    ImportTable imports ={};
    ConstantTable constants ={};

    Import* kernel32 = new_import(strings.intern("kernel32.dll"), &imports, &constants);
    //add_name_to_import(kernel32, strings.intern("just_a_test"), &imports);


    pe_file_build.code = &code_section;
    pe_file_build.constants = &constants;
    pe_file_build.imports = &imports;

    return write_portable_executable_to_file(&pe_file_build, options.build.output_file) == ErrorCode::OK ? 0 : 1;
  }
  else {
    std::cerr << "Could write out! Invalid convention and system conbination\n";
    return 1;
  }
}