#include "api.h"
#include "format.h"
#include "vm.h"
#include "windows_specifics.h"
#include "backends.h"
#include <iostream>
#include <chrono>

struct TestResult {
  uint64_t res;
  uint64_t time;
};

TestResult do_test(const Array<uint8_t>& arr) {
  auto mem = Windows::get_exectuable_memory<uint8_t>(arr.size);

  memcpy_ts(mem.ptr, mem.size, arr.data, arr.size);

  FUNCTION_PTR<uint64_t> func = (FUNCTION_PTR<uint64_t>)mem.ptr;

  constexpr size_t NUM = 100'000;
  auto start = std::chrono::high_resolution_clock::now();
  for (size_t i = 0; i < NUM; i++) {
    func();
  }
  auto end = std::chrono::high_resolution_clock::now();

  TestResult t_res ={};
  t_res.res = func();
  t_res.time = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();


  return t_res;
}


void speedtests() {
  {
    Array<uint8_t> double_load ={};

    X64::push(double_load, RBP.REG);
    X64::mov(double_load, RSP.REG, RBP.REG);
    X64::sub(double_load, RSP.REG, 0x8);

    X64::mov(double_load, X64::RM{ RBP.REG, true, -8 }, 1);
    X64::mov(double_load, X64::RM{ RBP.REG, true, -4 }, 2);

    X64::mov(double_load, X64::RM{ RBP.REG, true, -8 }, X64::R{ RAX.REG });

    X64::mov(double_load, RBP.REG, RSP.REG);
    X64::pop(double_load, RBP.REG);
    X64::ret(double_load);

    
    auto r = do_test(double_load);

    printf("RES: 0x%llx, Time(micro): %lldus\n", r.res, r.time);
  }

  {
    Array<uint8_t> single_load ={};

    X64::push(single_load, RBP.REG);
    X64::mov(single_load, RSP.REG, RBP.REG);
    X64::sub(single_load, RSP.REG, 0x8);

    X64::mov(single_load, X64::R{RAX.REG}, 0x200000001);
    X64::mov(single_load, X64::R{ RAX.REG }, X64::RM{ RBP.REG, true, -8 });

    X64::mov(single_load, X64::RM{ RBP.REG, true, -8 }, X64::R{ RAX.REG });

    X64::mov(single_load, RBP.REG, RSP.REG);
    X64::pop(single_load, RBP.REG);
    X64::ret(single_load);

    auto r = do_test(single_load);

    printf("RES: 0x%llx, Time(micro): %lldus\n", r.res, r.time);
  }
}



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
  options.print.comptime_exec   = true;
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