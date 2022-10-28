#pragma once
#include "utility.h"
#include "errors.h"

using TEST_FN = void(*)(Errors* _test_errors);

struct UnitTest {
  const char* test_name;
  TEST_FN test_func;
};

Array<UnitTest>& unit_tests_ref();

struct _TestAdder {
  _TestAdder(const char* test_name, TEST_FN fn);
};

#define TEST_FUNCTION(name) void name (Errors*); _TestAdder JOIN(_test_adder_, __LINE__) = {#name, name }; void name(Errors* test_errors)

#define TEST_EQ(expected, actual) do { \
 if((expected) != (actual)) { \
test_errors->report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Test assert failed!\nLine: {}, Test: {}\nExpected: {}\nActual: {}", __LINE__, __func__, #expected, #actual); return; } } while(false)

#define TEST_ARR_EQ(expected, actual, size) do { \
for(usize _n = 0; _n < (size); _n++) { \
if ((expected)[_n] != (actual)[_n]) { \
    test_errors->report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Test assert failed!\nLine: {}, Test: {}\nExpected: {}\nActual: {}", __LINE__, __func__, #expected, #actual); \
    return;\
} \
} \
} while (false)


#define TEST_CHECK_ERRORS() do { if(test_errors->is_panic()) return; } while(false)