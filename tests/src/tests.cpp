#include <stdint.h>
#include <iostream>
#include <filesystem>
#include <chrono>

#include "api.h"
#include "utility.h"
#include "type.h"

#ifdef COUNT_ALLOC
void print_still_allocated() {
  ALLOC_COUNTER& a = ALLOC_COUNTER::allocated();

  std::cout << "Max Allocated Blocks: " << a.max_allocated_blocks << '\n';
  std::cout << "Max Allocated Size: " << a.max_allocated_size << '\n';
  std::cout << '\n';
  std::cout << "Insert Calls: " << a.insert_calls << '\n';
  std::cout << "Update Calls: " << a.update_calls << '\n';
  std::cout << "Remove Calls: " << a.valid_remove_calls + a.null_remove_calls << '\n';
  std::cout << "- Valid Remove Calls: " << a.valid_remove_calls << '\n';
  std::cout << "- Nullptr Remove Calls: " << a.null_remove_calls << '\n';


  if (a.num_allocs > 0) {
    auto i = a.allocs;
    const auto end = a.allocs +  a.num_allocs;

    std::cout << "Still " << a.num_allocs << " blocks allocated - Possible memory leaks:\n";

    for (; i < end; i++) {
      std::cout << "Type: " << i->type_name << ", Ptr: " << i->mem << ", Bytes: " << i->size << '\n';
    }
  }
}
#endif

bool check_types_free() {
  bool res = true;

  if (!Types::array_structures._debug_all_free()) {
    res = false;
    std::cout << "Some array structures were not freed\n";
  }

  if(!Types::base_structures._debug_all_free()) {
    res = false;
    std::cout << "Some base structures were not freed\n";
  }

  if(!Types::composite_structures._debug_all_free()) {
    res = false;
    std::cout << "Some composite structures were not freed\n";
  }

  if(!Types::enum_structures._debug_all_free()) {
    res = false;
    std::cout << "Some enum structures were not freed\n";
  }

  if(!Types::enum_values._debug_all_free()) {
    res = false;
    std::cout << "Some enum values were not freed\n";
  }

  if(!Types::int_structures._debug_all_free()) {
    res = false;
    std::cout << "Some int structures were not freed\n";
  }

  if(!Types::simple_literal_structures._debug_all_free()) {
    res = false;
    std::cout << "Some simple literal structures were not freed\n";
  }

  if(!Types::tuple_literal_structures._debug_all_free()) {
    res = false;
    std::cout << "Some tuple literal structures were not freed\n";
  }

  if(!Types::pointer_structures._debug_all_free()) {
    res = false;
    std::cout << "Some pointer structures were not freed\n";
  }

  return res;
}

//Loop fibonacci algorithm - slow version wont run in constexpr
constexpr auto constexpr_fibonacci(uint64_t a) -> uint64_t {
  uint64_t n0 = 0;
  uint64_t n1 = 1;

  for (uint64_t i = 0; i < a; i++) {
    uint64_t next = n0 + n1;
    n0 = n1;
    n1 = next;
  }

  return n1;
}

//Should be the equivalent of "fib_recurse.axl" even though written differently
constexpr auto fib_recurse_main() -> uint64_t {
  return constexpr_fibonacci(5);
}

constexpr auto arrays_main() -> uint64_t {
  uint64_t a[4] ={ 3, 2, 0, 1 };

  return a[a[a[a[0]]]];
}

constexpr auto operations_optim() -> uint64_t {
  return 1 + -2 * 3 / 4;
}

constexpr auto operations_unsigned_cpp() -> uint64_t {
  uint64_t one = 1;
  uint64_t minus_two = -2;
  uint64_t three = 3;
  uint64_t four = 4;

  return one + minus_two * three / four;
}

constexpr auto operations_signed_cpp() -> uint64_t {
  int64_t one = 1;
  int64_t minus_two = -2;
  int64_t three = 3;
  int64_t four = 4;

  return static_cast<uint64_t>(one + minus_two * three / four);
}

constexpr auto fnv1_hash(const char* ptr, u64 len) -> u64 {
  u64 hash_v = 0xcbf29ce484222325;

  while(len > 0) {
    hash_v = hash_v * 0x100000001b3;
    hash_v = hash_v ^ CAST(u64, *ptr);

    ptr = ptr + 1;
    len = len - 1;
  }

  return hash_v;
}

struct Environment {
  const char* system_name;
  const char* convention;
};

struct Test {
  const char* test_name;
  const char* file_name;
  uint64_t return_value;
};

#define TEST_DIR(t) "src/" t

//Add tests here to make a new test
static constexpr Test tests[] ={
  Test{"Fibonnaci Recursive", TEST_DIR("fib_recurse.axl"), fib_recurse_main()},
  Test{"Operators Unsigned", TEST_DIR("operators_unsigned.axl"), operations_unsigned_cpp()},
  Test{"Operators Signed", TEST_DIR("operators_signed.axl"), operations_signed_cpp()},
  Test{"Operators Comptime", TEST_DIR("operators_comptime.axl"), operations_optim()},
  Test{"Arrays", TEST_DIR("arrays.axl"), arrays_main()},
  Test{"Pointers", TEST_DIR("pointers.axl"), 3},
  Test{"FNV1 Hash", TEST_DIR("fnv1_hash.axl"), fnv1_hash("hello", 5)},
};

static constexpr size_t num_tests = sizeof(tests)/sizeof(Test);

bool run_test(const APIOptions& opts, const uint64_t res) {
  try {
    //Time all the tests
    const auto now = std::chrono::high_resolution_clock::now();

    //Run the compilation
    Program prog ={};

    RunOutput out ={ 0, 0 };

    out.return_code = compile_file(opts, &prog);

    if (out.return_code == 0) {
      out = run_program(opts, &prog);
    }

    const auto end = std::chrono::high_resolution_clock::now();

    std::cout << "Test ran for: "
      << std::chrono::duration_cast<std::chrono::microseconds>(end - now).count()
      << "us\n";

    if (out.return_code != 0) {
      std::cout << "Return code: " << out.return_code << '\n';
      return false;
    }
    else {
      if (out.program_return == res) {
        return true;
      }
      else {
        std::cout << "Program returned: " << out.program_return << ", Expected: " << res << '\n';
        std::cout << "Printing Program:\n";
        print_program(opts, prog);
        std::cout << "\n\n";
        return false;
      }
    }
  }
  catch (const std::exception& e) {
    std::cerr << "Exception thrown: \"" << e.what() << "\"\n";

    return false;
  }
  catch (...) {
    //Probably should never be here
    std::cerr << "Unknown throw type encountered!!!!!!\n";

    return false;
  }

}

bool run_all_tests_in_env_and_optimization(const Environment& env, const APIOptimizationOptions& optimize) {

  {
    size_t num_optimizations = (size_t)optimize.non_stack_locals;

    if (num_optimizations == 0) {
      std::cout << "\n-----------------------------------\n";
      std::cout <<   "-- Testing with no optimizations --\n";
      std::cout <<   "-----------------------------------\n";
    }
    else {
      std::cout << "\n--------------------------------\n";
      std::cout <<   "-- Testing with optimizations --\n";
      if (optimize.non_stack_locals) {
        std::cout << "--      Non stack locals      --\n"; 
      }
      std::cout <<   "--------------------------------\n";
    }
  }

  Array<const char*> failed_tests ={};

  for (size_t i = 0; i < num_tests; i++) {
    const auto& test = tests[i];

    APIOptions options ={};

    options.optimize = optimize;

    options.build.system_name             = env.system_name;
    options.build.default_calling_convention = env.convention;
    options.build.entry_point        = "main";
    options.build.file_name          = test.file_name;
    options.build.std_lib_folder     = "D:\\Github\\Compiler\\stdlib";

    //options.print.ast             = true;
    //options.print.fully_compiled  = true;
    //options.print.comptime_exec   = true;
    //options.print.comptime_res    = true;
    //options.print.pre_reg_alloc   = true;
    //options.print.coalesce_values = true;
    //options.print.file_loads      = true;

    std::cout << "\nStarting Test: " << test.test_name << "\n";

    const bool passed = run_test(options, test.return_value);

    //Print memory leaks
    //print_still_allocated();

    const bool all_types_freed = check_types_free();

    if (passed && all_types_freed) {
      std::cout << "Test passed!\n";
    }
    else {
      std::cerr << "Test failed!\n";
      failed_tests.insert(test.test_name);
    }

  }

  if (failed_tests.size == 0) {
    std::cout << "\nAll "<< env.system_name << " Tests Passed!" << std::endl;
  }
  else {
    //Show which tests failed

    std::cerr << "\nSome " << env.system_name << " Tests Failed!\nFailed Tests:";

    auto i = failed_tests.begin();
    const auto end = failed_tests.end();

    std::cerr << " \"" << *i << '\"';
    i++;

    for (; i < end; i++) {
      std::cerr << ", \"" << *i << '\"';
    }

    std::cerr << std::endl;
  }

  return failed_tests.size > 0;
}

//Runs all the test with a specific calling convention and system
bool run_all_tests_in_env(const Environment& env) {

  std::cout << "=== Running tests in: " << env.system_name << " === \n";

  bool any_failed = false;

  APIOptimizationOptions opts ={};

  //no optimizations
  any_failed |= run_all_tests_in_env_and_optimization(env, opts);

  opts.non_stack_locals = true;
  any_failed |= run_all_tests_in_env_and_optimization(env, opts);

  std::cout << "\n=== Finished tests in: " << env.system_name << " === \n\n";
  return any_failed;
}

int main() {
  std::cout << "Current Working Directory: " << std::filesystem::current_path() << '\n';

  const Environment env_x86_64 ={ "x86_64", "x64" };
  const Environment env_vm ={ "vm", "vm" };

  bool any_failed = false;

  std::cout << "\n========== Started all tests ==========\n\n";
  any_failed |= run_all_tests_in_env(env_vm);
  any_failed |= run_all_tests_in_env(env_x86_64);
  std::cout << "==========  Ended all tests  ==========\n\n";

  if (any_failed) {
    std::cerr << "Some tests failed!!!" << std::endl;
  }
  else {
    std::cout << "All tests passed!!!" << std::endl;
  }

#ifdef COUNT_ALLOC
  print_still_allocated();
#endif

  return 0;
}