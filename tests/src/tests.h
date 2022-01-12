#pragma once
#include "utility.h"

using TEST_FN = void(*)();

struct UnitTest {
  const char* test_name;
  TEST_FN test_func;
};

Array<UnitTest>& unit_tests_ref();

struct TestAdder {
  inline TestAdder(const char* test_name, TEST_FN fn) {
    unit_tests_ref().insert({ test_name, fn });
  }
};

#define TEST_FUNCTION(name) void name (); TestAdder JOIN(_test_adder_, __LINE__) = {#name, name }; void name()