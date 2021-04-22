#include <stdint.h>
#include <iostream>
#include <filesystem>
#include <chrono>

#include "api.h"
#include "utility.h"

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

//Should be the equivalent of "fib_recurse.axl"
constexpr auto fib_recurse_main() -> uint64_t {
  return constexpr_fibonacci(15);
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

struct Environment {
  const System* system;
  const CallingConvention* convention;
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
};

static constexpr size_t num_tests = sizeof(tests)/sizeof(Test);

bool run_test(const Options& opts, const uint64_t res) {
  try {
    //Time all the tests
    const auto now = std::chrono::high_resolution_clock::now();

    //Run the compilation
    RunOutput out = compile_file_and_run(opts);

    const auto end = std::chrono::high_resolution_clock::now();

    std::cout << "Test ran for: "
      << std::chrono::duration_cast<std::chrono::milliseconds>(end - now).count()
      << "ms\n";

    if (out.return_code != 0) {
      std::cerr << "Return code: " << out.return_code << '\n';
      return false;
    }
    else {
      if (out.program_return == res) {
        return true;
      }
      else {
        std::cerr << "Program returned: " << out.program_return << ", Expected: " << res << '\n';
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

bool run_all_tests_in_env_and_optimization(const Environment& env, const OptimizationOptions& optimize) {
  
  {
    size_t num_optimizations = (size_t)optimize.non_stack_locals;

    if (num_optimizations == 0) {
      std::cout << "\n-----------------------------------\n";
      std::cout << "-- Testing with no optimizations --\n";
      std::cout << "-----------------------------------\n";
    }
    else {
      std::cout << "\n--------------------------------\n";
      std::cout << "-- Testing with optimizations --\n";
      if (optimize.non_stack_locals) { std::cout << "--      Non stack locals      --\n"; }
      std::cout << "--------------------------------\n";
    }
  }

  Array<const char*> failed_tests ={};

  for (size_t i = 0; i < num_tests; i++) {
    const auto& test = tests[i];

    Options options ={};

    options.optimize = optimize;

    options.build.system             = env.system;
    options.build.calling_convention = env.convention;
    options.build.entry_point        = "main";
    options.build.file_name          = test.file_name;

    //options.print.fully_compiled = true;

    std::cout << "\nStarting Test: " << test.test_name << "\n";

    const bool passed = run_test(options, test.return_value);    

    if (passed) {
      std::cout << "Test passed!\n";
    }
    else {
      std::cerr << "Test failed!\n";
      failed_tests.insert(test.test_name);
    }
  }

  if (failed_tests.size == 0) {
    std::cout << "\nAll "<< env.system->name << " Tests Passed!" << std::endl;
  }
  else {
    //Show which tests failed

    std::cerr << "\nSome " << env.system->name << " Tests Failed!\nFailed Tests:";

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

  std::cout << "=== Running tests in: " << env.system->name << " === \n";

  bool any_failed = false;

  OptimizationOptions opts ={};

  //no optimizations
  any_failed |= run_all_tests_in_env_and_optimization(env, opts);

  opts.non_stack_locals = true;
  any_failed |= run_all_tests_in_env_and_optimization(env, opts);

  std::cout << "\n=== Finished tests in: " << env.system->name << " === \n\n";
  return any_failed;
}

int main() {
  std::cout << "Current Working Directory: " << std::filesystem::current_path() << '\n';

  const Environment env_x86_64 = {&system_x86_64, &convention_microsoft_x64};
  const Environment env_vm = {&system_vm, &convention_vm};

  bool any_failed = false;

  std::cout << "\n========== Started all tests ==========\n\n";
  any_failed |= run_all_tests_in_env(env_x86_64);
  any_failed |= run_all_tests_in_env(env_vm);
  std::cout << "==========  Ended all tests  ==========\n\n";

  if (any_failed) {
    std::cerr << "Some tests failed!!!" << std::endl;
  }
  else {
    std::cout << "All tests passed!!!" << std::endl;
  }

  return 0;
}