#include "tests.h"


Array<UnitTest>& unit_tests_ref() {
  static Array<UnitTest> t ={};
  return t;
}

_TestAdder::_TestAdder(const char* test_name, TEST_FN fn) {
  unit_tests_ref().insert({ test_name, fn });
}

struct FailedTest {
  Errors errors;
  const char* test_name = nullptr;
};

int main() {
  Array<FailedTest> failed_tests ={};

  for (const auto& t : unit_tests_ref()) {

    IO::print("Starting test: ");
    IO::print(t.test_name);

    TestErrors errors = {};
    errors.test_name = t.test_name;

#ifdef ASSERT_EXCEPTIONS
    try {
#endif
      t.test_func(&errors);
#ifdef ASSERT_EXCEPTIONS
    }
    catch (const std::exception& e) {
      errors.report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Test failed with exception: {}", e.what());
    }
#endif
    
    if (errors.is_panic()) {
      IO::print("\t - failed\n");
      failed_tests.insert({ std::move(errors), t.test_name });
    }
    else {
      IO::print("\t - passed\n");
    }
  }

  if (failed_tests.size == 0) {
    IO::print("\nAll tests succeeded!");
  }
  else {
    IO::err_print("\nSome tests failed!\n");

    for (const auto& t : failed_tests) {
      IO::err_print("\n===========\n\n", t.test_name, " failed with errors : \n");

      t.errors.print_all();
      IO::err_print('\n');
    }
  }
}