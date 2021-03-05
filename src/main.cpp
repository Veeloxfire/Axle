
static_assert(sizeof(void*) == 8, "Currently only builds in 64 bit");

//#include "PE_file_format.h"
#include "vm.h"
#include "bytecode.h"
#include "parser.h"

#include "compiler.h"
#include "ast.h"

#include "machine_code.h"
#include "windows_specifics.h"

#include <utility>
#include <iostream>

#include <chrono>

constexpr char source[] = R"(
function fib(u64 a) -> u64 {
  if (a == 0 | a == 1) {
    return 1;
  }
  else {
    return fib(a - 1) + fib(a - 2);
  }
}

function main() -> u64 {
  return fib(30);
}
)";

auto cpp_fib(uint64_t a) -> uint64_t {
  if (a == 0 | a == 1) {
    return 1;
  }
  else {
    return cpp_fib(a - 1) + cpp_fib(a - 2);
  }
}

auto cpp_main() -> uint64_t {
  return cpp_fib(30);
}

constexpr char source2[] = R"(
function main() -> u64 {
  return 0;
}
)";

int run_compiler(const char* text_source, FUNCTION_PTR<uint64_t> cpp) {
  //Setup

  StringInterner strings ={};
  Lang lang ={};

  Compiler compiler ={};
  compiler.strings            = &strings;
  compiler.lang               = &lang;

  compiler.build_options.entry_point        = strings.intern("main");
  compiler.build_options.system             = &system_x86_64;
  compiler.build_options.calling_convention = &convention_microsoft_x64;

  compiler.run_options.print_ast             = true;
  compiler.run_options.compile               = true;
  compiler.run_options.print_bytecode        = true;
  compiler.run_options.run_in_vm_after_build = true;
  compiler.run_options.benchmark             = false;

  load_language_builtin(&compiler);

  Parser parser ={};
  parser.lexer.strings   = &strings;

  init_parser(&parser, "main.cpp", text_source);

  //Parse
  ASTFile ast_base ={};
  parse_file(&parser, &ast_base);


  if (parser.current.type == TokenType::Error) {
    std::cout << "PARSE ERROR: " << parser.current.string.string << '\n';
    std::cout << "At File: " << parser.current.file_name
      << ", Line: " << parser.current.line
      << ", Character: " << parser.current.character << '\n';
    return 1;
  }

  if (compiler.run_options.print_ast) {
    std::cout << "=== Print Parsed AST ===\n\n";
    print_ast(&ast_base);

    std::cout << "\n========================\n\n";
  }

  if (compiler.run_options.compile) {
    //Compilation
    build_compilation_units(&compiler, &ast_base);
    if (compile_all(&compiler) != CompileCode::NO_ERRORS) {
      std::cout << "COMPILE ERROR";
      return 1;
    }

    if (compiler.run_options.print_bytecode) {
      std::cout << "\n=== Print Compiled Bytecode ===\n\n";

      print_compiled_functions(&compiler);

      std::cout << "\n===============================\n\n";
    }

    const Function* const main_f = find_entry_point(&compiler);

    if (compiler.run_options.run_in_vm_after_build) {
      std::cout << "\n=== Run In VM ===\n\n";

      if (main_f == nullptr) {
        std::cout << "Requested run in vm but could not find entry point";
        return 1;
      }

      auto now = std::chrono::high_resolution_clock::now();
      const uint64_t cpp_test = (cpp)();
      auto end = std::chrono::high_resolution_clock::now();

      if (compiler.run_options.benchmark) {
        std::cout << "CPP Took: " << std::chrono::duration_cast<std::chrono::milliseconds>(end - now).count() << "ms\n";
      }

      VM vm ={};

      now = std::chrono::high_resolution_clock::now();
      ErrorCode error = vm_rum(&vm, main_f);
      end = std::chrono::high_resolution_clock::now();

      std::cout << "VM ran for: " << std::chrono::duration_cast<std::chrono::milliseconds>(end - now).count() << "ms\n";

    #define NO_ERROR_SAVE NO_ERROR
    #undef NO_ERROR

      if (error != ErrorCode::NO_ERROR) {

    #define NO_ERROR NO_ERROR_SAVE
    #undef NO_ERROR_SAVE

        std::cout << error_code_string(error);
      }
      else {
        std::cout << "Program returned: " << vm.registers[RAX.REG].full.reg << '\n';

        const uint64_t vm_test = vm.registers[RAX.REG].full.reg;

        if (vm_test == cpp_test) {
          std::cout << "CPP and VM gave same result! Both: " << vm_test << '\n';
        }
        else {
          std::cerr << "CPP and VM gave different results ... CPP: " << cpp_test << ", VM: " << vm_test << '\n';
        }
      }

      std::cout << "\n=================\n\n";
    }

    Array<uint8_t> machine_code ={};
    convert_to_x64_machine_code(machine_code, main_f->bytecode.data, main_f->bytecode.size);

    auto mem = Windows::get_exectuable_memory<uint8_t>(machine_code.size);

    memcpy_s(mem.ptr, mem.size, machine_code.data, machine_code.size);

    uint64_t res = mem.call<uint64_t>();

  }

}

int main() {

  run_compiler(source2, &cpp_main);



  return 0;
}
