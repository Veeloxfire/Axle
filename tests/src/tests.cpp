#include "tests.h"


Array<UnitTest>& unit_tests_ref() {
  static Array<UnitTest> t ={};
  return t;
}

int main() {

  Array<const char*> failed_tests ={};

  for (const auto& t : unit_tests_ref()) {

    IO::print("Starting test: ");
    IO::print(t.test_name);
    IO::print('\n');

    try {
      t.test_func();
    }
    catch (std::exception& e) {
      IO::err_print("Test failed with exception: \"");
      IO::err_print(e.what());
      IO::err_print("\"\n");

      failed_tests.insert(t.test_name);
    }
  }

  if (failed_tests.size == 0) {
    IO::print("\nAll tests succeeded!");
  }
  else {
    IO::err_print("\nSome tests failed :(\n");

    for (const char* t : failed_tests) {
      IO::err_print(" - ");
      IO::err_print(t);
      IO::err_print('\n');
    }
  }
}