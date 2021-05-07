
static_assert(sizeof(void*) == 8, "Currently only builds in 64 bit");

#include "PE_file_format.h"

#include "api.h"

#include "vm.h"
#include "bytecode.h"
#include "parser.h"

#include "compiler.h"
#include "ast.h"

#include "windows_specifics.h"
#include "files.h"
#include "backends.h"

#include <utility>
#include <iostream>

#include <chrono>

void print_program(const Options& opts, const Program& prog) {
  if (opts.build.system == &system_vm) {
    std::cout << "\n=== Print Linked Bytecode ===\n\n";
    ByteCode::print_bytecode(opts.build.system->reg_name_from_num, stdout, prog.code, prog.size);
    std::cout << "\n=============================\n\n";
  }
  else if (opts.build.system == &system_x86_64) {
    std::cout << "\n=== Print x86_64 Machine code ===\n\n";
    print_x86_64(prog.code, prog.size);
    std::cout << "\n=================================\n\n";
  }
  else {
    std::cerr << "Incorrect system for printing combined bytecode!\n";
  }
}

RunOutput run_in_vm(const Program& prog) {
  VM vm ={};
  ErrorCode error = vm_rum(&vm, prog.code, prog.entry);

  if (error != ErrorCode::OK) {
    std::cerr << error_code_string(error);
    return { 1, 0 };
  }

  return { 0, vm.registers[convention_vm.return_register].b64.reg };
}

RunOutput run_as_machine_code(const Program& prog) {
  auto exe = Windows::get_exectuable_memory<uint8_t>(prog.size);
  memcpy_ts(exe.ptr, exe.size, prog.code, prog.size);

  exe.entry = prog.entry;
  auto res = exe.call<uint64_t>();

  Windows::free_executable_memory(exe);
  return { 0, res };
}

int compile_file(const Options& options,
                 Program* out_program) {

  //Setup

  StringInterner strings ={};
  Types types ={};

  init_types(&types, &strings);

  State working_state ={};
  VM vm = {};

  Compiler compiler ={};
  compiler.build_options = options.build;
  compiler.print_options = options.print;
  compiler.optimization_options = options.optimize;

  compiler.strings = &strings;
  compiler.types = &types;
  compiler.entry_point = strings.intern(options.build.entry_point);
  compiler.working_state = &working_state;
  compiler.vm = &vm;

  Parser parser ={};
  parser.lexer.strings = &strings;

  const char* text_source = FILES::load_file_to_string(options.build.file_name);

  if (text_source == nullptr) {
    std::cerr << "Error opening file: " << options.build.file_name << '\n';
    return 1;
  }

  init_parser(&parser, options.build.file_name, text_source);

  //Parse
  ASTFile ast_base ={};
  parse_file(&parser, &ast_base);

  //Should have all been copied now :)
  free_no_destruct(text_source);


  if (parser.current.type == TokenType::Error) {
    std::cerr << "PARSE ERROR: " << parser.current.string.string << '\n'
      << "At File: " << parser.current.file_name
      << ", Line: " << parser.current.line
      << ", Character: " << parser.current.character << '\n';
    return 1;
  }

  if (options.print.ast) {
    std::cout << "\n=== Print Parsed AST ===\n\n";
    print_ast(&ast_base);
    std::cout << "\n========================\n\n";
  }

  //Compilation
  build_compilation_units(&compiler, &ast_base);
  const CompileCode ret = compile_all(&compiler);
  if (ret != CompileCode::NO_ERRORS) {
    std::cerr << "Compilation was not completed due to an error!\nError Code '" 
      << compile_code_string(ret)
      << "'\n";
    return 1;
  }

  //Backend - pretty sure this cant error yet
  Array<uint8_t> code ={};
  const size_t entry_index = (options.build.system->backend)(code, &compiler);
  code.shrink();

  if (options.print.fully_compiled) {
    if (options.build.system == &system_vm) {
      std::cout << "\n=== Print Linked Bytecode ===\n\n";
      ByteCode::print_bytecode(options.build.system->reg_name_from_num, stdout, code.data, code.size);
      std::cout << "\n=============================\n\n";
    }
    else if (options.build.system == &system_x86_64) {
      std::cout << "\n=== Print x86_64 Machine code ===\n\n";
      print_x86_64(code.data, code.size);
      std::cout << "\n=================================\n\n";
    }
    else {
      std::cerr << "Incorrect system for printing combined bytecode!\n";
    }
  }

  if (entry_index > code.size) {
    std::cerr << "LINKING ERROR: Could not find entry point!\n";
    return 1;
  }

  *out_program = Program{ code.data, code.size, entry_index };

  code.data = nullptr;
  code.size = 0;
  code.capacity = 0;

  return 0;
}

RunOutput run_program(const Options& options, const Program& prog) {
  if (options.build.system == &system_vm && options.build.calling_convention == &convention_vm) {
    if (options.print.run_headers) {
      std::cout << "\n=== Running in VM ===\n\n";
    }
    RunOutput ret = run_in_vm(prog);
    if (options.print.run_headers) {
      std::cout << "\n=====================\n\n";
    }

    return ret;
  }
  else if (options.build.system == &system_x86_64 && options.build.calling_convention == &convention_microsoft_x64) {
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

RunOutput compile_file_and_run(const Options& options) {
  Program program ={};

  const int res = compile_file(options, &program);

  if (res != 0) {
    return { res,  0 };
  }

  return run_program(options, program);
}

int compile_file_and_write(const Options& options) {
  if (options.build.output_file == nullptr) {
    std::cerr << "No output file specified!";
    return 1;
  }

  Program program ={};

  int res = compile_file(options, &program);

  if (res != 0) {
    return res;
  }

  if (options.build.system == &system_vm && options.build.calling_convention == &convention_vm) {
    std::cerr << "Cannot write bytecode to a file!";

    return res;
  }
  else if (options.build.system == &system_x86_64 && options.build.calling_convention == &convention_microsoft_x64) {
    std::cout << "Writing to file \"" << options.build.output_file << "\"\n";

    StringInterner strings ={};

    PE_File pe_file ={};

    CodeSection code_section ={};
    code_section.bytes = program.code;
    code_section.size  = program.size;
    code_section.entry_point = program.entry;

    ImportTable imports ={};
    ConstantTable constants ={};

    Import* kernel32 = new_import(strings.intern("kernel32.dll"), &imports, &constants);
    //add_name_to_import(kernel32, strings.intern("just_a_test"), &imports);


    pe_file.code = &code_section;
    pe_file.constants = &constants;
    pe_file.imports = &imports;

    return write_portable_executable_to_file(&pe_file, options.build.output_file) == ErrorCode::OK ? 0 : 1;
  }
  else {
    std::cerr << "Could write out! Invalid convention and system conbination\n";
    return 1;
  }
}