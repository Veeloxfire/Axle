#include "api.h"
#include "format.h"
#include "vm.h"
#include "windows_specifics.h"
#include "backends.h"
#include <iostream>
#include <chrono>


constexpr char output_file[] = "output.exe";

int main(int argc, const char** args) {
  if (argc != 2) {
    std::cerr << "Invalid number of arguments!";
    return 1;
  }

  Options options = {};
  
  options.build.file_name          = args[1];
  options.build.entry_point        = "main";
  options.build.system             = &system_vm;
  options.build.calling_convention = &convention_vm;
  //options.build.system             = &system_x86_64;
  //options.build.calling_convention = &convention_microsoft_x64;
  options.build.output_file        = output_file;
  
  options.print.ast             = true;
  options.print.pre_reg_alloc   = true;
  options.print.normal_bytecode = true;
  options.print.comptime_exec   = false;
  options.print.coalesce_values = true;
  options.print.fully_compiled  = true;
  options.print.run_headers     = false;

  options.optimize.non_stack_locals = true;
  
  Program program ={};
  int out = compile_file(options, &program);
  
  if (out == 0) {
    RunOutput res = run_program(options, program);
    std::cout << "Returned: " << res.program_return;
    
    return 0;
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