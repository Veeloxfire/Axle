#include <AxleTest/unit_tests.h>
#include "errors.h"

void load_span_single_test(AxleTest::TestErrors* test_errors, const Axle::ViewArr<const char>& src) {
  {
    constexpr Span span = {
      nullptr,
      1,1,
      2,2
    };

    constexpr Axle::ViewArr expected = Axle::lit_view_arr(R"(  hello
/--^
| world
\--^)");

    Axle::OwnedArr final_v = load_span_from_source(span, src);

    TEST_STR_EQ(expected, final_v);
  }

  {
    constexpr Span span = {
      nullptr,
      1,1,
      3,1
    };

    constexpr Axle::ViewArr expected = Axle::lit_view_arr("  hello\n   ^^");

    Axle::OwnedArr final_v = load_span_from_source(span, src);

    TEST_STR_EQ(expected, final_v);
  }
}

TEST_FUNCTION(Errors, load_span) {
  
  load_span_single_test(test_errors, Axle::lit_view_arr("\nhello\nworld\none and\ntwo\nthree\n"));
  TEST_CHECK_ERRORS();

  load_span_single_test(test_errors, Axle::lit_view_arr("\r\nhello\r\nworld\r\none and\r\ntwo\r\nthree\r\n"));
  TEST_CHECK_ERRORS();

  load_span_single_test(test_errors, Axle::lit_view_arr("\n\rhello\n\rworld\n\rone and\n\rtwo\n\rthree\n\r"));
  TEST_CHECK_ERRORS();

  load_span_single_test(test_errors, Axle::lit_view_arr("\rhello\rworld\rone and\rtwo\rthree\r"));
  TEST_CHECK_ERRORS();
}
