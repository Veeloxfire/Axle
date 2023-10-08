#include "tests.h"
#include "format.h"

TEST_FUNCTION(ArrayFormat, strings) {
  const ViewArr<const char> expected = lit_view_arr("hello world");
  OwnedArr<const char> arr = format("hello {}", lit_view_arr("world"));

  TEST_ARR_EQ(expected.data, expected.size, arr.data, arr.size);
}

void test_all_valid_signed_ints(TestErrors* test_errors, const ViewArr<const char>& expected, i64 i) {
  if(SCHAR_MAX >= i && i >= SCHAR_MIN) {
    OwnedArr<const char> sc123 = format("{}", (signed char)i);
    TEST_ARR_EQ(expected.data, expected.size, sc123.data, sc123.size);
  }

  if (SHRT_MAX >= i && i >= SHRT_MIN) {
    OwnedArr<const char> ss123 = format("{}", (signed short)i);
    TEST_ARR_EQ(expected.data, expected.size, ss123.data, ss123.size);
  }

  if (INT_MAX >= i && i >= INT_MIN) {
    OwnedArr<const char> si123 = format("{}", (signed int)i);
    TEST_ARR_EQ(expected.data, expected.size, si123.data, si123.size);
  }

  if (LONG_MAX >= i && i >= LONG_MIN) {
    OwnedArr<const char> sl123 = format("{}", (signed long)i);
    TEST_ARR_EQ(expected.data, expected.size, sl123.data, sl123.size);
  }

  {
    OwnedArr<const char> sll123 = format("{}", (signed long long)i);
    TEST_ARR_EQ(expected.data, expected.size, sll123.data, sll123.size);
  }
}

void test_all_valid_unsigned_ints(TestErrors* test_errors, const ViewArr<const char>& expected, u64 i) {
  if (UCHAR_MAX >= i) {
    OwnedArr<const char> sc123 = format("{}", (unsigned char)i);
    TEST_ARR_EQ(expected.data, expected.size, sc123.data, sc123.size);
  }

  if (USHRT_MAX >= i) {
    OwnedArr<const char> ss123 = format("{}", (unsigned short)i);
    TEST_ARR_EQ(expected.data, expected.size, ss123.data, ss123.size);
  }

  if (UINT_MAX >= i) {
    OwnedArr<const char> si123 = format("{}", (unsigned int)i);
    TEST_ARR_EQ(expected.data, expected.size, si123.data, si123.size);
  }

  if (ULONG_MAX >= i) {
    OwnedArr<const char> sl123 = format("{}", (unsigned long)i);
    TEST_ARR_EQ(expected.data, expected.size, sl123.data, sl123.size);
  }

  {
    OwnedArr<const char> sll123 = format("{}", (unsigned long long)i);
    TEST_ARR_EQ(expected.data, expected.size, sll123.data, sll123.size);
  }
}

TEST_FUNCTION(ArrayFormat, ints) {
  {
    const ViewArr<const char> expected = lit_view_arr("0");
    test_all_valid_signed_ints(test_errors, expected, 0);
    if (test_errors->is_panic()) return;

    test_all_valid_unsigned_ints(test_errors, expected, 0);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("1");
    test_all_valid_signed_ints(test_errors, expected, 1);
    if (test_errors->is_panic()) return;

    test_all_valid_unsigned_ints(test_errors, expected, 1);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("-1");
    test_all_valid_signed_ints(test_errors, expected, -1);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("123");
    test_all_valid_signed_ints(test_errors, expected, 123);
    if (test_errors->is_panic()) return;

    test_all_valid_unsigned_ints(test_errors, expected, 123);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("-123");
    test_all_valid_signed_ints(test_errors, expected, -123);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("-128");
    test_all_valid_signed_ints(test_errors, expected, -128);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("-32768");
    test_all_valid_signed_ints(test_errors, expected, -32768);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("-32768");
    test_all_valid_signed_ints(test_errors, expected, -32768);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("-2147483648");
    test_all_valid_signed_ints(test_errors, expected, -2147483647 - 1);
    if (test_errors->is_panic()) return;
  }
  
  {
    const ViewArr<const char> expected = lit_view_arr("-9223372036854775808");
    test_all_valid_signed_ints(test_errors, expected, -9223372036854775807i64 - 1);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("127");
    test_all_valid_signed_ints(test_errors, expected, 127);
    if (test_errors->is_panic()) return;
    test_all_valid_unsigned_ints(test_errors, expected, 127);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("255");
    test_all_valid_signed_ints(test_errors, expected, 255);
    if (test_errors->is_panic()) return;
    test_all_valid_unsigned_ints(test_errors, expected, 255);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("327687");
    test_all_valid_signed_ints(test_errors, expected, 327687);
    if (test_errors->is_panic()) return;
    test_all_valid_unsigned_ints(test_errors, expected, 327687);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("65535");
    test_all_valid_signed_ints(test_errors, expected, 65535);
    if (test_errors->is_panic()) return;
    test_all_valid_unsigned_ints(test_errors, expected, 65535);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("2147483647");
    test_all_valid_signed_ints(test_errors, expected, 2147483647);
    if (test_errors->is_panic()) return;
    test_all_valid_unsigned_ints(test_errors, expected, 2147483647);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("4294967295");
    test_all_valid_signed_ints(test_errors, expected, 4294967295);
    if (test_errors->is_panic()) return;
    test_all_valid_unsigned_ints(test_errors, expected, 4294967295);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("9223372036854775807");
    test_all_valid_signed_ints(test_errors, expected, 9223372036854775807i64);
    if (test_errors->is_panic()) return;
    test_all_valid_unsigned_ints(test_errors, expected, 9223372036854775807i64);
    if (test_errors->is_panic()) return;
  }

  {
    const ViewArr<const char> expected = lit_view_arr("18446744073709551615");
    test_all_valid_unsigned_ints(test_errors, expected, 18446744073709551615ui64);
    if (test_errors->is_panic()) return;
  }
}