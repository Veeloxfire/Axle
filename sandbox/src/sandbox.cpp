#include "api.h"
#include "format.h"
#include "vm.h"
#include "windows_specifics.h"
#include "backends.h"
#include <iostream>
#include "trace.h"

constexpr char output_file[] = "output.exe";

int main(int argc, const char** args) {
  Tracing::start_tracer_threaded("trace.json");
  DEFER() {
    Tracing::end_tracer_threaded();
  };


  if (argc != 2) {
    std::cerr << "Invalid number of arguments!";
    return 1;
  }

  APIOptions options = {};
  
  options.build.file_name       = args[1];
  options.build.entry_point     = "main";
  //options.build.system_name                = "vm";
  //options.build.default_calling_convention = "vm";
  options.build.system_name                = "x86_64";
  options.build.default_calling_convention = "x64";
  options.build.output_file        = output_file;
  options.build.std_lib_folder = "D:\\GitHub\\Compiler\\stdlib";
  options.build.lib_folder = "D:\\GitHub\\Compiler\\lib";
  
  options.print.ast             = false;
  options.print.pre_reg_alloc   = false;
  options.print.normal_bytecode = false;
  options.print.comptime_res    = false;
  options.print.coalesce_values = false;
  options.print.fully_compiled  = false;
  options.print.run_headers     = false;
  options.print.comp_units      = false;
  options.print.comptime_exec   = false;

  options.optimize.non_stack_locals = true;
  
  Program program ={};

  int out = compile_file(options, &program);
  
  if (out == 0) {
    //RunOutput res = run_program(options, &program);
    //std::cout << "Returned: " << res.program_return;
    
    return out;
  }
  else {
    std::cerr << "Error!";
    return out;
  }
  

  //int ret = compile_file_and_write(options);
  //if (ret != 0) {
  //  return ret;
  //}

  //std::cout << "running " << output_file << '\n';

  //uint32_t res = Windows::run_exe(output_file);

  //std::cout << "Res: " << res;
  //return ret;

  //speedtests();

  return 0;
}