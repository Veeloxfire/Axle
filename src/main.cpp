
static_assert(sizeof(void*) == 8, "Currently only builds in 64 bit");

//#include "PE_file_format.h"
#include "vm.h"
#include "bytecode.h"
#include "parser.h"

#include "compiler.h"
#include "ast.h"

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
  if ((a == 0) | (a == 1)) {
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
  return 0 + 1 + 2 + 3 + 4;
}
)";


static int run_compiler(const char* text_source, FUNCTION_PTR<uint64_t> cpp) {
  //Setup

  StringInterner strings ={};
  Lang lang ={};

  Compiler compiler ={};
  compiler.strings            = &strings;
  compiler.lang               = &lang;

  compiler.build_options.entry_point        = strings.intern("main");
  compiler.build_options.system             = &system_x86_64;
  compiler.build_options.calling_convention = &convention_microsoft_x64;

  compiler.run_options.compile         = true;
  compiler.run_options.run_after_build = true;

  compiler.print_options.ast               = true;
  compiler.print_options.pre_reg_alloc     = true;
  compiler.print_options.normal_bytecode   = true;
  compiler.print_options.combined_bytecode = true;
  compiler.print_options.coalesce_values   = false;

  load_language_builtin(&compiler);

  Parser parser ={};
  parser.lexer.strings   = &strings;

  init_parser(&parser, "main.cpp", text_source);

  //Parse
  ASTFile ast_base ={};
  parse_file(&parser, &ast_base);


  if (parser.current.type == TokenType::Error) {
    std::cerr << "PARSE ERROR: " << parser.current.string.string << '\n'
      << "At File: " << parser.current.file_name
      << ", Line: " << parser.current.line
      << ", Character: " << parser.current.character << '\n';
    return 1;
  }

  if (compiler.print_options.ast) {
    std::cout << "=== Print Parsed AST ===\n\n";
    print_ast(&ast_base);

    std::cout << "\n========================\n\n";
  }

  if (compiler.run_options.compile) {

    //Compilation
    build_compilation_units(&compiler, &ast_base);
    if (compile_all(&compiler) != CompileCode::NO_ERRORS) {
      std::cerr << "COMPILE ERROR";
      return 1;
    }

    //Backend - pretty sure this cant error yet
    Array<uint8_t> code ={};
    const size_t entry_index = (compiler.build_options.system->backend)(code, &compiler);
    code.shrink();

    if (compiler.print_options.combined_bytecode) {
      if (compiler.build_options.system == &system_vm) {
        std::cout << "\n=== Print Combined Bytecode ===\n\n";
        ByteCode::print_bytecode(compiler.build_options.system->reg_name_from_num, stdout, code.data, code.size);
        std::cout << "\n===============================\n\n";
      }
      else {
        std::cerr << "Incorrect system for printing combined bytecode!\n";
      }
    }

    if (entry_index > code.size) {
      std::cerr << "Could not find entry point!";
      return 1;
    }

    if (compiler.run_options.run_after_build) {

      std::cout << "\n=== Run Code ===\n\n";

      auto now = std::chrono::high_resolution_clock::now();
      const uint64_t cpp_test = (cpp)();
      auto end = std::chrono::high_resolution_clock::now();

      std::cout << "CPP Took: " << std::chrono::duration_cast<std::chrono::milliseconds>(end - now).count() << "ms\n";


      uint64_t run_result = 0;

      if (compiler.build_options.system == &system_vm && compiler.build_options.calling_convention == &convention_vm) {
        std::cout << "Running in VM\n";

        VM vm ={};

        now = std::chrono::high_resolution_clock::now();
        ErrorCode error = vm_rum(&vm, code.data, entry_index);
        end = std::chrono::high_resolution_clock::now();

        std::cout << "VM ran for: " << std::chrono::duration_cast<std::chrono::milliseconds>(end - now).count() << "ms\n";

      #define NO_ERROR_SAVE NO_ERROR
      #undef NO_ERROR

        if (error != ErrorCode::NO_ERROR) {

        #define NO_ERROR NO_ERROR_SAVE
        #undef NO_ERROR_SAVE

          std::cout << error_code_string(error);
          return 1;
        }

        const uint8_t return_reg = compiler.build_options.calling_convention->return_register;
        const uint64_t run_result = vm.registers[return_reg].full.reg;
      }
      else if (compiler.build_options.system == &system_x86_64 && compiler.build_options.calling_convention == &convention_microsoft_x64) {
        std::cout << "Running machine code (JIT)\n";

        auto exe = Windows::get_exectuable_memory<uint8_t>(code.size);
        memcpy_ts(exe.ptr, exe.size, code.data, code.size);

        exe.entry = entry_index;

        now = std::chrono::high_resolution_clock::now();
        run_result = exe.call<uint64_t>();
        end = std::chrono::high_resolution_clock::now();

        Windows::free_executable_memory(exe);

        std::cout << "MC ran for: " << std::chrono::duration_cast<std::chrono::milliseconds>(end - now).count() << "ms\n";
      }
      else {
        std::cerr << "Could not run! Invalid convention and system conbination\n";
        return 1;
      }

      std::cout << "Program returned: " << run_result << '\n';

      if (run_result == cpp_test) {
        std::cout << "CPP and VM gave same result! Both: " << run_result << '\n';
      }
      else {
        std::cerr << "CPP and VM gave different results ... CPP: " << cpp_test << ", VM: " << run_result << '\n';
      }

      std::cout << "\n=================\n\n";
    }
  }

  return 0;
}

int main() {

  run_compiler(source, &cpp_main);



  return 0;
}
