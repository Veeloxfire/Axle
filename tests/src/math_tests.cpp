#include "unit_tests.h"
#include "utility.h"

TEST_FUNCTION(Math, CEIL_TO_N) {
  TEST_EQ(1234, ceil_to_n(1233, 1234));
  TEST_EQ(1234, ceil_to_n(1234, 1234));
  TEST_EQ(2468, ceil_to_n(1235, 1234));
  TEST_EQ(0, ceil_to_n(0, 1234));


  TEST_EQ(0, ceil_to_8(0));
  TEST_EQ(8, ceil_to_8(1));
  TEST_EQ(8, ceil_to_8(2));
  TEST_EQ(8, ceil_to_8(3));
  TEST_EQ(8, ceil_to_8(4));
  TEST_EQ(8, ceil_to_8(5));
  TEST_EQ(8, ceil_to_8(6));
  TEST_EQ(8, ceil_to_8(7));
  TEST_EQ(8, ceil_to_8(8));
  TEST_EQ(16, ceil_to_8(9));
}

TEST_FUNCTION(Math, CEIL_TO_POW_2) {
  TEST_EQ((u64)1, ceil_to_pow_2(1));
  TEST_EQ((u64)2, ceil_to_pow_2(2));

  for (u64 p = 2; p < 63; ++p) {
    u64 n = static_cast<u64>(1) << p;

    TEST_EQ(n, ceil_to_pow_2(n - 1));
    TEST_EQ(n, ceil_to_pow_2(n));
    TEST_EQ(n << 1, ceil_to_pow_2(n + 1));
  }

  TEST_EQ(1llu << 63llu, ceil_to_pow_2((1llu << 63llu) - 1llu));
  TEST_EQ(1llu << 63llu, ceil_to_pow_2(1llu << 63llu));
}

TEST_FUNCTION(Math, bit_fills) {
  TEST_EQ(static_cast<u64>(0), bit_fill_lower<u64>(0));
  TEST_EQ(static_cast<u64>(1), bit_fill_lower<u64>(1));
  TEST_EQ(static_cast<u64>(0x7fffffffffffffff), bit_fill_lower<u64>(63));
  TEST_EQ(static_cast<u64>(0xffffffffffffffff), bit_fill_lower<u64>(64));
  TEST_EQ(static_cast<u8>(0b0001'1111), bit_fill_lower<u8>(5));
  TEST_EQ(static_cast<u8>(0b0011'1111), bit_fill_lower<u8>(6));
  TEST_EQ(static_cast<u8>(0b1111'1111), bit_fill_lower<u8>(8));
  TEST_EQ(static_cast<u8>(0b1111'1111), bit_fill_lower<u8>(9));
  TEST_EQ(static_cast<u16>(0b1'1111'1111), bit_fill_lower<u16>(9));

  TEST_EQ(static_cast<u64>(0), bit_fill_upper<u64>(0));
  TEST_EQ(static_cast<u64>(0x8000000000000000), bit_fill_upper<u64>(1));
  TEST_EQ(static_cast<u64>(0xfffffffffffffffe), bit_fill_upper<u64>(63));
  TEST_EQ(static_cast<u64>(0xffffffffffffffff), bit_fill_upper<u64>(64));
  TEST_EQ(static_cast<u8>(0b1111'1000), bit_fill_upper<u8>(5));
  TEST_EQ(static_cast<u8>(0b1111'1100), bit_fill_upper<u8>(6));
  TEST_EQ(static_cast<u8>(0b1111'1111), bit_fill_upper<u8>(8));
}

TEST_FUNCTION(Math, pows_and_logs) {
  TEST_EQ(0llu, log_2(1llu));

  for (u64 i = 1; i < 63; ++i) {
    u64 v = 1llu << i;
    TEST_EQ(i - 1, log_2(v - 1));
    TEST_EQ(i, log_2(v));
    TEST_EQ(i, log_2(v + 1));
  }

  TEST_EQ(63llu, log_2(UINT64_MAX));
  TEST_EQ(0llu, small_log_2_floor(1llu));

  for (u64 i = 1; i < 63; ++i) {
    u64 v = 1llu << i;
    TEST_EQ(i - 1, small_log_2_floor(v - 1));
    TEST_EQ(i, small_log_2_floor(v));
    TEST_EQ(i, small_log_2_floor(v + 1));
  }

  TEST_EQ(63llu, small_log_2_floor(UINT64_MAX));

  TEST_EQ(0llu, small_log_2_ceil(1llu));
  TEST_EQ(1llu, small_log_2_ceil(2llu));

  for (u64 i = 2; i < 63; ++i) {
    u64 v = 1llu << i;
    TEST_EQ(i, small_log_2_ceil(v - 1));
    TEST_EQ(i, small_log_2_ceil(v));
    TEST_EQ(i + 1, small_log_2_ceil(v + 1));
  }

  TEST_EQ(64llu, small_log_2_ceil(UINT64_MAX));

  {
    u64 v = 1;
    u64 i = 0;
    
    while (true) {
      TEST_EQ(v, pow_10(i));
      TEST_EQ(i, log_10_floor(v));

      if (v > UINT64_MAX / 10) {
        break;
      }
      v *= 10;
      i += 1;
    }
  }

  TEST_EQ(1llu, pow_16(0));
  TEST_EQ(16llu, pow_16(1));
  TEST_EQ(16llu * 16llu, pow_16(2));
  TEST_EQ(16llu * 16llu * 16llu, pow_16(3));
  TEST_EQ(16llu * 16llu * 16llu * 16llu, pow_16(4));
}

TEST_FUNCTION(Math, absolutes) {
  TEST_EQ(static_cast<u8>(23), absolute(static_cast<i8>(-23)));
  TEST_EQ(static_cast<u16>(23), absolute(static_cast<i16>(-23)));
  TEST_EQ(static_cast<u32>(23), absolute(static_cast<i32>(-23)));
  TEST_EQ(static_cast<u64>(23), absolute(static_cast<i64>(-23)));

  TEST_EQ(static_cast<u8>(23), absolute(static_cast<i8>(23)));
  TEST_EQ(static_cast<u16>(23), absolute(static_cast<i16>(23)));
  TEST_EQ(static_cast<u32>(23), absolute(static_cast<i32>(23)));
  TEST_EQ(static_cast<u64>(23), absolute(static_cast<i64>(23)));

  TEST_EQ(static_cast<u8>(0), absolute(static_cast<i8>(0)));
  TEST_EQ(static_cast<u16>(0), absolute(static_cast<i16>(0)));
  TEST_EQ(static_cast<u32>(0), absolute(static_cast<i32>(0)));
  TEST_EQ(static_cast<u64>(0), absolute(static_cast<i64>(0)));

  constexpr u8 max_i8 = INT8_MAX;
  constexpr u16 max_i16 = INT16_MAX;
  constexpr u32 max_i32 = INT32_MAX;
  constexpr u64 max_i64 = INT64_MAX;

  TEST_EQ(static_cast<u8>(max_i8 + 1), absolute(static_cast<i8>(INT8_MIN)));
  TEST_EQ(static_cast<u16>(max_i16 + 1), absolute(static_cast<i16>(INT16_MIN)));
  TEST_EQ(static_cast<u32>(max_i32 + 1), absolute(static_cast<i32>(INT32_MIN)));
  TEST_EQ(static_cast<u64>(max_i64 + 1), absolute(static_cast<i64>(INT64_MIN)));
}

TEST_FUNCTION(Math, gcd) {
  TEST_EQ(3ull, greatest_common_divisor(12, 9));
  TEST_EQ(1ull, greatest_common_divisor(42341, 9823));
  TEST_EQ(564ull, greatest_common_divisor(6768, 80652));
}

