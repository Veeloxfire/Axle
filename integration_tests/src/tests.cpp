#include <chrono>

#include "io.h"
#include "format.h"
#include "files.h"

#include "api.h"
#include "utility.h"

#include "windows_specifics.h"
#include "x64_backend.h"
#include "PE_file_format.h"

#include "args.h"

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
  uint64_t n = 3;
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
  ViewArr<const char> test_name;
  ViewArr<const char> base_name;
  uint64_t return_value;

  template<usize N, usize M>
  constexpr Test(const char(&t_name)[N], const char(&b_name)[M], uint64_t return_value_)
    : test_name(lit_view_arr(t_name)), base_name(lit_view_arr(b_name)), return_value(return_value_) {}
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

  IO::format("Test ran for: {} us (compiler ran for: {} us)\n",
             std::chrono::duration_cast<std::chrono::microseconds>(end - start).count(),
             std::chrono::duration_cast<std::chrono::microseconds>(compiler_end - start).count());

  if (return_code != 0) {
    IO::format("Compiler Error. Return code: {}\n", return_code);
    return TestOutcome::CompilationError;
  }
  else {
    switch (output) {
      case ProgramOutput::INVALID_PROGRAM: {
          IO::print("Could not finish the test - program could not be opened\n");
          return TestOutcome::CompilationError;
        }
      case ProgramOutput::TIMED_OUT: {
          IO::print("Could not finish the test - timed out before program could finish executing\n");
          return TestOutcome::WrongAnswer;
        }
      case ProgramOutput::INCORRECT: {
          IO::print("Program produced incorrect result format - could not be correct value\n");
          return TestOutcome::WrongAnswer;
        }
      case ProgramOutput::CORRECT: {
          if (res != expected.val) {
            IO::format("Incorrect Results. Expected: {}. Actual: {}\n", expected.val, res);
            return TestOutcome::WrongAnswer;
          }
          else {
            IO::print("Test Passed!\n");
            return TestOutcome::Pass;
          }
        }
      default: {
          IO::print("Unexpected Test State\n");
          return TestOutcome::CompilationError;
        }
    }
  }
}

void print_test_collection(ViewArr<const char> system_name,
                           ViewArr<const char> group_name,
                           ViewArr<const char> group_list_name,
                           ViewArr<const ViewArr<const char>> test_collection) {
  ViewArr<const char> indicator;
  if (test_collection.size == 0) {
    indicator = lit_view_arr("No");
  }
  else if (test_collection.size == NUM_TESTS) {
    indicator = lit_view_arr("All");
  }
  else {
    indicator = lit_view_arr("Some");
  }

  IO::format("\n{} {} {}!", indicator, system_name, group_name);

  if (test_collection.size > 0) {
    IO::format("\n{}: ", group_list_name);

    auto i = test_collection.begin();
    const auto end = test_collection.end();

    IO::format(" \"{}\"", *i);
    i++;

    for (; i < end; i++) {
      IO::format(", \"{}\"", *i);
    }
  }

}

bool run_all_tests_with_optimizations(const Tester& tester, const APIOptimizationOptions& optimize) {
  ViewArr<const char> comp_error_tests[NUM_TESTS] = {};
  size_t num_comp_errorr_tests = 0;

  ViewArr<const char> wrong_answer_tests[NUM_TESTS] = {};
  size_t num_wrong_answer_tests = 0;

  ViewArr<const char> passed_tests[NUM_TESTS] = {};
  size_t num_passed_tests = 0;

  ViewArr<const char> system_name = tester.pi->system_name;

  for (size_t i = 0; i < NUM_TESTS; i++) {
    const auto& test = tests[i];

    APIOptions options = {};

    X64::ProgramExtra program_extra = {};
    options.program_extra = &program_extra;

    options.optimize = optimize;

    const auto file_name_holder = format("{}.axl", test.base_name);
    const auto exe_name_holder = format_file_path(lit_view_arr(EXE_DIR), test.base_name, lit_view_arr("exe"));


    options.platform_interface = tester.pi;
    options.executable_format_interface = tester.efi;

    assert(tester.pi->num_calling_conventions > 0);
    options.build.debug_break_on_entry = false;
    options.build.default_calling_convention = 0;
    options.build.entry_point = lit_view_arr("main");
    options.build.current_directory = lit_view_arr(".");//TODO: actually get this
    options.build.file_name = const_view_arr(file_name_holder);
    options.build.output_name = test.base_name;
    options.build.output_folder = lit_view_arr(EXE_DIR);
    options.build.output_file_type = tester.efi->type;
    options.build.std_lib_folder = lit_view_arr("..\\stdlib");
    options.build.lib_folder = lit_view_arr(TEST_DIR);

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

    IO::format("\nStarting Test: {}\n", test.test_name);

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

  print_test_collection(system_name, lit_view_arr("Tests passed"), lit_view_arr("Passed tests"),
                        view_arr(passed_tests, 0, num_passed_tests));
  IO::print("\n");

  print_test_collection(system_name, lit_view_arr("Tests produced wrong answers"), lit_view_arr("Wrong answer tests"),
                        view_arr(wrong_answer_tests, 0, num_wrong_answer_tests));
  IO::print("\n");

  print_test_collection(system_name, lit_view_arr("Tests had compile errors"), lit_view_arr("Compile errorr tests"),
                        view_arr(comp_error_tests, 0, num_comp_errorr_tests));

  return num_passed_tests < NUM_TESTS;
}

//Runs all the test with a specific calling convention and system
bool run_all_tests(const Tester& tester) {
  ViewArr<const char> system_name = tester.pi->system_name;

  IO::format("=== Running tests in: {} ===\n", system_name);

  bool any_failed = false;

  APIOptimizationOptions opts = {};

  //no optimizations
  any_failed |= run_all_tests_with_optimizations(tester, opts);

  IO::format("\n=== Finished tests in: {} ===\n\n", system_name);
  return any_failed;
}

int main() {
  Windows::set_current_directory(lit_view_arr(ROOT_DIR));
  
  {
    auto current_dir = Windows::get_current_directory();

    IO::format("Current Working Directory: {}\n", current_dir.view());
  }
  constexpr Backend::PlatformInterface x64_pi = x86_64_platform_interface();
  constexpr Backend::ExecutableFormatInterface PE_efi = pe_plus_file_interface();

  const Tester tester = { &x64_pi, &PE_efi, 3 };

  bool any_failed = false;

  IO::print("\n========== Started all tests ==========\n\n");
  any_failed |= run_all_tests(tester);
  IO::print("==========  Ended all tests  ==========\n\n");

  if (any_failed) {
    IO::print("Some tests failed!!!\n");
  }
  else {
    IO::print("All tests passed!!!\n");
  }

  IO::print("\n\n");

  return 0;
}