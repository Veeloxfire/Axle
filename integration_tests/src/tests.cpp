#include <chrono>

#include <AxleUtil/io.h>
#include <AxleUtil/format.h>
#include <AxleUtil/files.h>
#include <AxleUtil/utility.h>
#include <AxleUtil/os/os_windows.h>
#include <AxleUtil/args.h>

#include <Axle/api.h>
#include <AxleTest/unit_tests.h>

#include <Axle/backends/x64_backend.h>
#include <Axle/backends/PE_file_format.h>

#include "tester.h"

namespace IO = Axle::IO;

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
  return 1ll + -2ll * 3ll / 4ll;
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
  Axle::ViewArr<const char> test_name;
  Axle::ViewArr<const char> base_name;
  uint64_t return_value;

  template<usize N, usize M>
  constexpr Test(const char(&t_name)[N], const char(&b_name)[M], uint64_t return_value_)
    : test_name(Axle::lit_view_arr(t_name)), base_name(Axle::lit_view_arr(b_name)), return_value(return_value_) {}
};

static constexpr u8 ONES_U8[2] = { 1, 1 };

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

void run_program(AxleTest::TestErrors* test_errors, const char* name, const Expected& expected, bool debugging) {
  if (!load_shared_memory()) {
    test_errors->report_error("Failed to load shared memory");
  }
  DEFER(&) {
    if (!unload_shared_memory()) {
      test_errors->report_error("Failed to unload shared memory");
    }
  };

  STARTUPINFOA si = {};
  ZeroMemory(&si, sizeof(si));
  si.cb = sizeof(si);

  PROCESS_INFORMATION pi;
  ZeroMemory(&pi, sizeof(pi));

  BOOL finished = CreateProcessA(name, NULL, 0, NULL, false, NORMAL_PRIORITY_CLASS, NULL, NULL, &si, &pi);
  if (finished == 0) {
    test_errors->report_error("Attempted to run invalid program");
    return;
  }

  DEFER(&) {
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
  };

  const u32 wait = debugging ? INFINITE : TIME_TO_WAIT_MS;
  DWORD waited = WaitForSingleObject(pi.hProcess, wait);
  if (waited == WAIT_TIMEOUT) {
    TerminateProcess(pi.hProcess, 1);
    test_errors->report_error("Process timed out");
    return;
  }

  SHARED_MEM shared = {};
  get_shared_memory(&shared);

  u64 result = 0; 
  if (!try_read_shared(result, shared.buffer, shared.size)) {
    test_errors->report_error("Size of output was incorrect");
  }

  // Final check
  TEST_EQ(expected.val, result);
}

void run_test(AxleTest::TestErrors* errors, const APIOptions& opts, const char* expected_output_path, const Expected& expected) {

  const int return_code = compile_and_write(opts);
  if(return_code != 0) {
    errors->report_error("Compile failed (see output). Code: {}", return_code);
    return;
  }

  run_program(errors, expected_output_path, expected, opts.build.debug_break_on_entry);
}

void run_test_with_setup_x64_pe(AxleTest::TestErrors* test_errors, Axle::ViewArr<const char> base_name, const Expected& expected) {
  constexpr Backend::PlatformInterface x64_pi = x86_64_platform_interface();
  constexpr Backend::ExecutableFormatInterface PE_efi = pe_plus_file_interface();


  APIOptions options = {};

  X64::ProgramExtra program_extra = {};
  options.program_extra = &program_extra;
  options.optimize = {};

  const auto file_name_holder = Axle::format("{}.axl", base_name);
  const auto exe_name_holder = Axle::format_file_path(Axle::lit_view_arr(INTEGRATION_TEST_CLIENT_OUT_DIR), base_name, Axle::lit_view_arr("exe"));

  options.platform_interface = &x64_pi;
  options.executable_format_interface = &PE_efi;

  ASSERT(x64_pi.num_calling_conventions > 0);
  options.build.debug_break_on_entry = false;
  options.build.default_calling_convention = 0;
  options.build.entry_point = Axle::lit_view_arr("main");
  options.build.current_directory = Axle::lit_view_arr(".");//TODO: actually get this
  options.build.file_name = Axle::const_view_arr(file_name_holder);
  options.build.output_name = base_name;
  options.build.output_folder = Axle::lit_view_arr(INTEGRATION_TEST_CLIENT_OUT_DIR);
  options.build.output_file_type = PE_efi.type;
  options.build.std_lib_folder = Axle::lit_view_arr(INTEGRATION_TEST_CLIENT_STDLIB_DIR);
  options.build.lib_folder = Axle::lit_view_arr(INTEGRATION_TEST_CLIENT_SRC_DIR);

  options.build.extra_threads = 3;

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

  run_test(test_errors, options, exe_name_holder.raw.data, expected);
}

#define INTEGRATION_TEST(name, expr) TEST_FUNCTION(Integration, name) { \
  constexpr Expected expected = { expr }; \
  run_test_with_setup_x64_pe(test_errors, Axle::lit_view_arr(#name), expected); \
}

INTEGRATION_TEST(operators_unsigned, operations_unsigned_cpp());
INTEGRATION_TEST(operators_signed, operations_signed_cpp());
INTEGRATION_TEST(operators_comptime, operations_optim());
INTEGRATION_TEST(fib_recurse, fib_recurse_main());
INTEGRATION_TEST(fib_loop, fib_loop_main());
INTEGRATION_TEST(arrays, arrays_main());
INTEGRATION_TEST(pointers, 3);
INTEGRATION_TEST(fnv1_hash, fnv1_hash("hello", 5));
INTEGRATION_TEST(fnv1_hash_single, fnv1_hash(ONES_U8, 1));
INTEGRATION_TEST(fnv1_hash_double, fnv1_hash(ONES_U8, 2));
INTEGRATION_TEST(globals, fib_count_main());
INTEGRATION_TEST(structs, 3); 
