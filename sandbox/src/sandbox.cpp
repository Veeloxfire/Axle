#include "api.h"
#include "format.h"
#include "vm.h"
#include "windows_specifics.h"
#include <iostream>

constexpr char output_file[] = "output.exe";

int main(int argc, const char** args) {
  if (argc != 2) {
    std::cerr << "Invalid number of arguments!";
    return 1;
  }

  Options options = {};
  
  options.build.file_name          = args[1];
  options.build.entry_point        = "main";
  options.build.system             = &system_x86_64;
  options.build.calling_convention = &convention_microsoft_x64;
  options.build.output_file        = output_file;
  
  options.print.ast             = true;
  options.print.pre_reg_alloc   = true;
  options.print.normal_bytecode = true;
  options.print.coalesce_values = false;
  options.print.fully_compiled  = true;
  options.print.run_headers     = false;
  
  /*Program program ={};
  return compile_file(options, &program);*/
  
  /*RunOutput out = compile_file_and_run(options);
  
  if (out.return_code == 0) {
    std::cout << "Returned: " << out.program_return;
  }
  else {
    std::cerr << "Error!";
  }
  
  return out.return_code;*/

  int ret = compile_file_and_write(options);
  return ret;
}