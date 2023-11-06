#pragma once
#include <AxleUtil/utility.h>
#include "errors.h"

namespace UNIT_TESTS {
  struct TestErrors : Errors {
    ViewArr<const char> test_name;
  };
  using TEST_FN = void(*)(TestErrors* _test_errors);

  struct UnitTest {
    ViewArr<const char> test_name;
    TEST_FN test_func;
  };

  Array<UnitTest>& unit_tests_ref();

  struct _testAdder {
    _testAdder(const ViewArr<const char>& test_name, TEST_FN fn);
  };

  template<typename T>
  void test_eq(TestErrors* errors, usize line,
               const ViewArr<const char>& expected_str, const T& expected,
               const ViewArr<const char>& actual_str, const T& actual) {
    if (expected != actual) {
      errors->report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Test assert failed!\nLine: {}, Test: {}\nExpected: {} = {}\nActual: {} = {}",
                           line, errors->test_name, expected_str, expected, actual_str, actual);

    }
  }


  template<typename T>
  void test_eq(TestErrors* errors, usize line,
               const ViewArr<const char>& expected_str, T* expected,
               const ViewArr<const char>& actual_str, T* actual) {
    if (expected != actual) {
      errors->report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Test assert failed!\nLine: {}, Test: {}\nExpected: {} = {}\nActual: {} = {}",
                           line, errors->test_name, expected_str, PrintPtr{ expected }, actual_str, PrintPtr{ actual });

    }
  }

  template<typename T>
  void test_neq(TestErrors* errors, usize line, 
                const ViewArr<const char>& expected_str, const T& expected,
                const ViewArr<const char>& actual_str, const T& actual) {
    if (expected == actual) {
      errors->report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Test assert failed!\nLine: {}, Test: {}\n{} = {}\n{} = {}\nThese should not be equal",
                           line, errors->test_name, expected_str, expected, actual_str, actual);

    }
  }


  template<typename T>
  void test_neq(TestErrors* errors, usize line,
                const ViewArr<const char>& expected_str, T* expected,
                const ViewArr<const char>& actual_str, T* actual) {
    if (expected == actual) {
      errors->report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Test assert failed!\nLine: {}, Test: {}\n{} = {}\n{} = {}\nThese should not be equal",
                           line, errors->test_name, expected_str, PrintPtr{ expected }, actual_str, PrintPtr{ actual });

    }
  }


  template<typename T>
  void test_eq_arr(TestErrors* errors, usize line,
                   const ViewArr<const char>& expected_str, const T* expected,
                   const ViewArr<const char>& esize_str, usize e_size,
                   const ViewArr<const char>& actual_str, const T* actual,
                   const ViewArr<const char>& asize_str, usize a_size) {
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

  inline void test_eq_str_impl(TestErrors* errors, usize line,
                               const ViewArr<const char>& expected_str, const ViewArr<const char>& expected,
                               const ViewArr<const char>& actual_str, const ViewArr<const char>& actual) {
    const size_t e_size = expected.size;
    const size_t a_size = actual.size;
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
                         "Expected String: {} = \"{}\"\n"
                         "Actual String: {} = \"{}\"",
                         line, errors->test_name,
                         expected_str, DisplayString{ expected.data, expected.size },
                         actual_str, DisplayString{ actual.data, actual.size });
    return;
  }

  constexpr const ViewArr<const char> test_str(const ViewArr<const char>& c) {
    return c;
  }

  constexpr const ViewArr<const char> test_str(const ViewArr<char>& c) {
    return c;
  }

  constexpr const ViewArr<const char> test_str(const OwnedArr<char>& c) {
    return const_view_arr(c);
  }

  constexpr const ViewArr<const char> test_str(const OwnedArr<const char>& c) {
    return view_arr(c);
  }

  constexpr const ViewArr<const char> test_str(const InternString* c) {
    return { c->string, c->len };
  }

  template<typename L, typename R>
  inline void test_eq_str(TestErrors* errors, usize line,
                          const ViewArr<const char>& expected_str, const L& expected,
                          const ViewArr<const char>& actual_str, const R& actual) {
    return test_eq_str_impl(errors, line,
                            expected_str, test_str(expected),
                            actual_str, test_str(actual));
  }
}

#define TEST_EQ(expected, actual) do {\
UNIT_TESTS::test_eq(test_errors, __LINE__, lit_view_arr(#expected), expected, lit_view_arr(#actual), actual);\
if (test_errors->is_panic()) return; } while (false)

#define TEST_NEQ(expected, actual) do {\
UNIT_TESTS::test_neq(test_errors, __LINE__, lit_view_arr(#expected), expected, lit_view_arr(#actual), actual);\
if (test_errors->is_panic()) return; } while (false)

#define TEST_ARR_EQ(expected, e_size, actual, a_size) do { \
UNIT_TESTS::test_eq_arr(test_errors, __LINE__, lit_view_arr(#expected), expected, lit_view_arr(#e_size), e_size, lit_view_arr(#actual), actual, lit_view_arr(#a_size), a_size);\
if (test_errors->is_panic()) return; } while (false)

#define TEST_STR_EQ(expected, actual) do { \
UNIT_TESTS::test_eq_str(test_errors, __LINE__, lit_view_arr(#expected), expected, lit_view_arr(#actual), actual);\
if (test_errors->is_panic()) return; } while (false)

#define TEST_FUNCTION(space, name) namespace UNIT_TESTS:: JOIN(_anon_ns_ ## space, __LINE__) { static void JOIN(_anon_tf_ ## name, __LINE__) (UNIT_TESTS::TestErrors*); } static UNIT_TESTS::_testAdder JOIN(_test_adder_, __LINE__) = {lit_view_arr(#space "::" #name), UNIT_TESTS:: JOIN( _anon_ns_ ## space, __LINE__) :: JOIN(_anon_tf_ ## name, __LINE__) }; static void UNIT_TESTS:: JOIN( _anon_ns_ ## space, __LINE__) :: JOIN(_anon_tf_ ## name, __LINE__) (UNIT_TESTS::TestErrors* test_errors)

#define TEST_CHECK_ERRORS() do { if(test_errors->is_panic()) return; } while(false)