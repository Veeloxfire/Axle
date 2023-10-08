#pragma once
#include "utility.h"
#include "errors.h"

struct TestErrors : Errors {
  ViewArr<const char> test_name;
};
using TEST_FN = void(*)(TestErrors* _test_errors);

struct UnitTest {
  ViewArr<const char> test_name;
  TEST_FN test_func;
};

Array<UnitTest>& unit_tests_ref();

struct _TestAdder {
  _TestAdder(const ViewArr<const char>& test_name, TEST_FN fn);
};

#define TEST_FUNCTION(space, name) namespace JOIN( _anon_ns_ ## space, __LINE__) { static void JOIN(_anon_tf_ ## name, __LINE__) (TestErrors*); } static _TestAdder JOIN(_test_adder_, __LINE__) = {lit_view_arr(#space "::" #name), JOIN( _anon_ns_ ## space, __LINE__) :: JOIN(_anon_tf_ ## name, __LINE__) }; static void JOIN( _anon_ns_ ## space, __LINE__) :: JOIN(_anon_tf_ ## name, __LINE__) (TestErrors* test_errors)

template<typename T>
void test_eq(TestErrors* errors, usize line, const char* expected_str, const T& expected, const char* actual_str, const T& actual) {
  if (expected != actual) {
    errors->report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Test assert failed!\nLine: {}, Test: {}\nExpected: {} = {}\nActual: {} = {}",
                         line, errors->test_name, expected_str, expected, actual_str, actual);

  }
}


template<typename T>
void test_eq(TestErrors* errors, usize line, const char* expected_str, T* expected, const char* actual_str, T* actual) {
  if (expected != actual) {
    errors->report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Test assert failed!\nLine: {}, Test: {}\nExpected: {} = {}\nActual: {} = {}",
                         line, errors->test_name, expected_str, PrintPtr{ expected }, actual_str, PrintPtr{ actual });

  }
}

template<typename T>
void test_eq_arr(TestErrors* errors, usize line,
                 const char* expected_str, const T* expected, const char* esize_str, usize e_size,
                 const char* actual_str, const T* actual, const char* asize_str, usize a_size) {
  if (e_size != a_size) {
    goto ERROR;
  }

  for (usize n = 0; n < e_size; ++n) {
    const auto& l = expected[n];
    const auto& r = actual[n];
    if (l != r) {
      goto ERROR;
    }
  }

  return;

ERROR:
  errors->report_error(ERROR_CODE::ASSERT_ERROR, Span{},
                       "Test assert failed!\nLine: {}, Test: {}\n"
                       "Expected Size: {} = {}\nActual Size: {} = {}\n"
                       "Expected Array: {} = {}\n"
                       "Actual Array: {} = {}",
                       line, errors->test_name, esize_str, e_size, asize_str, a_size,
                       expected_str, PrintList<T>{expected, e_size},
                       actual_str, PrintList<T>{actual, a_size});
  return;
}


inline void test_eq_str(TestErrors* errors, usize line,
                 const char* expected_str, const char* expected,
                 const char* actual_str, const char* actual) {
  const char* e = expected;
  const char* a = actual;
  while (true) {
    char ec = e[0];
    char ac = a[0];

    if (ec != ac) {
      goto ERROR;
    }

    if (ec == '\0') {
      return;
    }

    a += 1;
    e += 1;
  }

ERROR:
  errors->report_error(ERROR_CODE::ASSERT_ERROR, Span{},
                       "Test assert failed!\nLine: {}, Test: {}\n"
                       "Expected String: {} = \"{}\"\n"
                       "Actual String: {} = \"{}\"",
                       line, errors->test_name,
                       expected_str, expected,
                       actual_str, actual);
  return;
}

#define TEST_EQ(expected, actual) do {\
test_eq(test_errors, __LINE__, #expected, expected, #actual, actual);\
if (test_errors->is_panic()) return; } while (false)

#define TEST_ARR_EQ(expected, e_size, actual, a_size) do { \
test_eq_arr(test_errors, __LINE__, #expected, expected, #e_size, e_size, #actual, actual, #a_size, a_size);\
if (test_errors->is_panic()) return; } while (false)

#define TEST_STR_EQ(expected, actual) do { \
test_eq_str(test_errors, __LINE__, #expected, expected, #actual, actual);\
if (test_errors->is_panic()) return; } while (false)


#define TEST_CHECK_ERRORS() do { if(test_errors->is_panic()) return; } while(false)