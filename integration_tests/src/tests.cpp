#include <stdint.h>
#include <iostream>
#include <filesystem>
#include <chrono>

#include "api.h"
#include "utility.h"
#include "type.h"
#include "files.h"

#include "windows_specifics.h"
#include "x64_backend.h"
#include "PE_file_format.h"

#include "tester.h"

#ifdef TRACING_ENABLE
#error Cannot trace the tests

#endif

#ifdef COUNT_ALLOC
void print_still_allocated_and_reset() {
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
    const auto end = a.allocs + a.num_allocs;

    std::cout << "Still " << a.num_allocs << " blocks allocated - Possible memory leaks:\n";

    for (; i < end; i++) {
      std::cout << "Type: " << i->type_name << ", Ptr: " << i->mem << ", Count: " << i->count << '\n';
    }
  }

  a.reset();
}
#endif

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

constexpr auto constexpr_fibonacci_recurse_count(uint64_t* c, uint64_t a) -> uint64_t {
  *c += 1;

  if (a == 0 || a == 1) return 1;
  else {
    const auto res1 = constexpr_fibonacci_recurse_count(c, a - 1);
    const auto res2 = constexpr_fibonacci_recurse_count(c, a - 2);
    return res1 + res2;
  }
}

constexpr auto fib_recurse_main() -> uint64_t {
  uint64_t n = 0;
  return constexpr_fibonacci_recurse_count(&n, 5);
}

constexpr auto fib_count_main() -> uint64_t {
  uint64_t n = 0;
  constexpr_fibonacci_recurse_count(&n, 5);

  return n;
}

constexpr auto fib_loop_main() -> uint64_t {
  return constexpr_fibonacci(5);
}

constexpr auto arrays_main() -> uint64_t {
  uint64_t a[4] = { 3, 2, 0, 1 };

  return a[a[a[a[0]]]];
}

constexpr auto operations_optim() -> uint64_t {
  return 1llu + static_cast<uint64_t>(-2ll) * 3llu / 4llu;
}

constexpr auto operations_unsigned_cpp() -> uint64_t {
  constexpr uint64_t one = 1;
  constexpr uint64_t minus_two = static_cast<uint64_t>(-2);
  constexpr uint64_t three = 3;
  constexpr uint64_t four = 4;

  return one + minus_two * three / four;
}

constexpr auto operations_signed_cpp() -> uint64_t {
  constexpr int64_t one = 1;
  constexpr int64_t minus_two = -2;
  constexpr int64_t three = 3;
  constexpr int64_t four = 4;

  return static_cast<uint64_t>(one + minus_two * three / four);
}

constexpr auto fnv1_hash(const char* ptr, u64 len) -> u64 {
  u64 hash_v = 0xcbf29ce484222325;

  while (len > 0) {
    hash_v = hash_v * 0x100000001b3;
    hash_v = hash_v ^ (u64)*ptr;

    ptr = ptr + 1;
    len = len - 1;
  }

  return hash_v;
}

constexpr auto fnv1_hash(const u8* ptr, u64 len) -> u64 {
  u64 hash_v = 0xcbf29ce484222325;

  while (len > 0) {
    hash_v = hash_v * 0x100000001b3;
    hash_v = hash_v ^ (u64)*ptr;

    ptr = ptr + 1;
    len = len - 1;
  }

  return hash_v;
}


struct Tester {
  const Backend::PlatformInterface* pi;
  const Backend::ExecutableFormatInterface* efi;
  u32 extra_threads;
};

struct Test {
  const char* test_name;
  const char* base_name;
  uint64_t return_value;
};

static constexpr char TEST_DIR[] = "src/";
static constexpr char EXE_DIR[] = "out/";

static constexpr u8 ONES_U8[2] = { 1, 1 };

//Add tests here to make a new test
static constexpr Test tests[] = {
  Test{"Operators Unsigned", "operators_unsigned", operations_unsigned_cpp()},
  Test{"Operators Signed", "operators_signed", operations_signed_cpp()},
  Test{"Operators Comptime", "operators_comptime", operations_optim()},
  Test{"Fibonnaci Recursive", "fib_recurse", fib_recurse_main()},
  Test{"Fibonnaci Loop", "fib_loop", fib_loop_main()},
  Test{"Arrays", "arrays", arrays_main()},
  Test{"Pointers", "pointers", 3},
  Test{"FNV1 Hash", "fnv1_hash", fnv1_hash("hello", 5)},
  Test{"FNV1 Hash Single", "fnv1_hash_single", fnv1_hash(ONES_U8, 1)},
  Test{"FNV1 Hash Double", "fnv1_hash_double", fnv1_hash(ONES_U8, 2)},
  Test{"Globals", "globals", fib_count_main()},
  Test{"Structs", "structs", 3},
};

static constexpr size_t NUM_TESTS = sizeof(tests) / sizeof(Test);

static constexpr u32 TIME_TO_WAIT_MS = 1000 * 20;//20 seconds

struct Expected {
  u64 val;
};

enum struct ProgramOutput {
  INVALID_PROGRAM,
  INCORRECT,
  CORRECT,
  TIMED_OUT
};

template<typename T>
bool try_read_shared(T& t, const volatile unsigned char* mem, usize size) {
  if (size != sizeof(T)) return false;
  memcpy(&t, (const void*)mem, size);
  return true;
}

ProgramOutput run_program(const char* name, u64& out, bool debugging) {
  STARTUPINFOA si = {};
  ZeroMemory(&si, sizeof(si));
  si.cb = sizeof(si);

  PROCESS_INFORMATION pi;
  ZeroMemory(&pi, sizeof(pi));

  BOOL finished = CreateProcessA(name, NULL, 0, NULL, FALSE, NORMAL_PRIORITY_CLASS, NULL, NULL, &si, &pi);
  if (finished == 0) {
    return ProgramOutput::INVALID_PROGRAM;
  }

  DEFER(&) {
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
  };

  const u32 wait = debugging ? INFINITE : TIME_TO_WAIT_MS;
  DWORD waited = WaitForSingleObject(pi.hProcess, wait);
  if (waited == WAIT_TIMEOUT) {
    TerminateProcess(pi.hProcess, 1);
    return ProgramOutput::TIMED_OUT;
  }

  SHARED_MEM shared = {};
  get_shared_memory(&shared);

  if (!try_read_shared(out, shared.buffer, shared.size)) {
    return ProgramOutput::INCORRECT;
  }
  else {
    return ProgramOutput::CORRECT;
  }
}

enum struct TestOutcome {
  Pass,
  WrongAnswer,
  CompilationError,
};

TestOutcome run_test(const APIOptions& opts, const char* expected_output_path, const Expected& expected) noexcept {
  //Time all the tests
  const auto start = std::chrono::high_resolution_clock::now();

  const int return_code = compile_and_write(opts);

  const auto compiler_end = std::chrono::high_resolution_clock::now();
  ProgramOutput output = ProgramOutput::INVALID_PROGRAM;

  u64 res = 0;
  if (return_code == 0) {
    output = run_program(expected_output_path, res, opts.build.debug_break_on_entry);
  }

  const auto end = std::chrono::high_resolution_clock::now();

  std::cout << "Test ran for: "
    << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "us (compiler ran for: "
    << std::chrono::duration_cast<std::chrono::microseconds>(compiler_end - start).count() << "us)\n";

  if (return_code != 0) {
    std::cout << "Compiler Error. Return code: " << return_code << '\n';
    return TestOutcome::CompilationError;
  }
  else {
    switch (output) {
      case ProgramOutput::INVALID_PROGRAM: {
          std::cout << "Could not finish the test - program could not be opened\n";
          return TestOutcome::CompilationError;
        }
      case ProgramOutput::TIMED_OUT: {
          std::cout << "Could not finish the test - timed out before program could finish executing\n";
          return TestOutcome::WrongAnswer;
        }
      case ProgramOutput::INCORRECT: {
          std::cout << "Program produced incorrect result format - could not be correct value\n";
          return TestOutcome::WrongAnswer;
        }
      case ProgramOutput::CORRECT: {
          if (res != expected.val) {
            std::cout << "Incorrect Results. Expected: " << expected.val << ". Actual: " << res << '\n';
            return TestOutcome::WrongAnswer;
          }
          else {
            std::cout << "Test Passed!\n";
            return TestOutcome::Pass;
          }
        }
      default: {
          std::cout << "Unexpected Test State\n";
          return TestOutcome::CompilationError;
        }
    }
  }
}

void print_test_collection(const char* system_name, const char* group_name, const char* group_list_name, const char** test_collection, size_t num_tests) {
  const char* indicator = nullptr;
  if (num_tests == 0) {
    indicator = "No";
  }
  else if (num_tests == NUM_TESTS) {
    indicator = "All";
  }
  else {
    indicator = "Some";
  }

  std::cout << '\n' << indicator << ' ' << system_name << ' ' << group_name << '!';

  if (num_tests > 0) {
    std::cout << '\n' << group_list_name << ": ";
    auto i = test_collection;
    const auto end = test_collection + num_tests;

    std::cout << " \"" << *i << '\"';
    i++;

    for (; i < end; i++) {
      std::cout << ", \"" << *i << '\"';
    }
  }

}

bool run_all_tests_with_optimizations(const Tester& tester, const APIOptimizationOptions& optimize) {

#if 0
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
      if (optimize.non_stack_locals) {
        std::cout << "--      Non stack locals      --\n";
      }
      std::cout << "--------------------------------\n";
    }
  }
#endif

  const char* comp_error_tests[NUM_TESTS] = {};
  size_t num_comp_errorr_tests = 0;

  const char* wrong_answer_tests[NUM_TESTS] = {};
  size_t num_wrong_answer_tests = 0;

  const char* passed_tests[NUM_TESTS] = {};
  size_t num_passed_tests = 0;

  const char* system_name = tester.pi->system_name;

  for (size_t i = 0; i < NUM_TESTS; i++) {
    const auto& test = tests[i];

    APIOptions options = {};

    X64::Program program = {};
    options.program = &program;

    options.optimize = optimize;

    const auto file_name_holder = format("{}.axl", test.base_name);
    const auto exe_name_holder = format_file_path(EXE_DIR, test.base_name, "exe");


    options.platform_interface = tester.pi;
    options.executable_format_interface = tester.efi;

    assert(tester.pi->num_calling_conventions > 0);
    options.build.debug_break_on_entry = false;
    options.build.default_calling_convention = 0;
    options.build.entry_point = "main";
    options.build.current_directory = ".";//TODO: actually get this
    options.build.file_name = file_name_holder.data;
    options.build.output_name = test.base_name;
    options.build.output_folder = EXE_DIR;
    options.build.output_file_type = tester.efi->type;
    options.build.std_lib_folder = "..\\stdlib";
    options.build.lib_folder = TEST_DIR;

    options.build.extra_threads = tester.extra_threads;

    options.print.ast = false;
    options.print.comptime_res = false;
    options.print.comptime_exec = false;
    options.print.finished_ir = false;
    options.print.finished_mc = false;
    options.print.run_headers = false;
    options.print.register_select = false;
    options.print.file_loads = false;
    options.print.comp_units = false;
    options.print.work = false;

    std::cout << "\nStarting Test: " << test.test_name << "\n";

    Expected expected = {};
    expected.val = test.return_value;
    const TestOutcome outcome = run_test(options, exe_name_holder.raw.data, expected);

#ifdef COUNT_ALLOC
    //Print memory leaks
    print_still_allocated_and_reset();
#endif

    switch (outcome) {
      case TestOutcome::Pass: {
          passed_tests[num_passed_tests] = test.test_name;
          num_passed_tests++;
          break;
        }
      case TestOutcome::WrongAnswer: {
          wrong_answer_tests[num_wrong_answer_tests] = test.test_name;
          num_wrong_answer_tests++;
          break;
        }
      case TestOutcome::CompilationError: {
          comp_error_tests[num_comp_errorr_tests] = test.test_name;
          num_comp_errorr_tests++;
          break;
        }
    }
  }

  print_test_collection(system_name, "Tests passed", "Passed tests", passed_tests, num_passed_tests);
  std::cout << "\n";
  print_test_collection(system_name, "Tests produced wrong answers", "Wrong answer tests", wrong_answer_tests, num_wrong_answer_tests);
  std::cout << "\n";
  print_test_collection(system_name, "Tests had compile errors", "Compile errorr tests", comp_error_tests, num_comp_errorr_tests);

  return num_passed_tests < NUM_TESTS;
}

//Runs all the test with a specific calling convention and system
bool run_all_tests(const Tester& tester) {
  const char* system_name = tester.pi->system_name;

  std::cout << "=== Running tests in: " << system_name << " === \n";

  bool any_failed = false;

  APIOptimizationOptions opts = {};

  //no optimizations
  any_failed |= run_all_tests_with_optimizations(tester, opts);

  std::cout << "\n=== Finished tests in: " << system_name << " === \n\n";
  return any_failed;
}

int main() {
  std::cout << "Current Working Directory: " << std::filesystem::current_path() << '\n';

  constexpr Backend::PlatformInterface x64_pi = x86_64_platform_interface();
  constexpr Backend::ExecutableFormatInterface PE_efi = pe_plus_file_interface();

  const Tester tester = { &x64_pi, &PE_efi, 3 };

  bool any_failed = false;

  std::cout << "\n========== Started all tests ==========\n\n";
  any_failed |= run_all_tests(tester);
  std::cout << "==========  Ended all tests  ==========\n\n";

  if (any_failed) {
    std::cerr << "Some tests failed!!!" << std::endl;
  }
  else {
    std::cout << "All tests passed!!!" << std::endl;
  }

  std::cout << "\n\n";

  return 0;
}