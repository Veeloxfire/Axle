#include "tests.h"

#include "type.h"
#include "ast.h"

struct Group {
  VALUE_CATEGORY high_start;
  VALUE_CATEGORY low_start;
  VALUE_CATEGORY low_end;
};
static void test_reduce_vc(TestErrors* test_errors, AST_LOCAL low, AST_LOCAL high, const Group& g) {
  low->value_category = g.low_start;
  high->value_category = g.high_start;

  reduce_category(low, high);

  TEST_EQ(g.high_start, high->value_category);
  TEST_EQ(g.low_end, low->value_category);
}
#define TEST_REDUCE_VC(group) test_reduce_vc(test_errors, &low, &high, (group)); if(test_errors->is_panic()) return

TEST_FUNCTION(Types, ValueCategory) {
  AST high = {};
  AST low = {};

  {
    const VALUE_CATEGORY opts[] = {
      VALUE_CATEGORY::TEMPORARY_CONSTANT,
      VALUE_CATEGORY::TEMPORARY_IMMUTABLE,
      VALUE_CATEGORY::VARIABLE_CONSTANT,
      VALUE_CATEGORY::VARIABLE_IMMUTABLE,
      VALUE_CATEGORY::VARIABLE_MUTABLE,
    };

    for (const auto o : opts) {
      high.value_category = o;
      for (const auto o2 : opts) {
        low.value_category = o;
        same_category(&low, &high);

        TEST_EQ(o, high.value_category);
        TEST_EQ(o, low.value_category);
      }
    }

    for (const auto o : opts) {
      high.value_category = o;
      low.value_category = o;
      
      reduce_category(&low, &high);

      TEST_EQ(o, high.value_category);
      TEST_EQ(o, low.value_category);
    }
  }

  {
    Group g = {};
    g.high_start = VALUE_CATEGORY::TEMPORARY_CONSTANT;
    g.low_start = VALUE_CATEGORY::VARIABLE_CONSTANT;

    g.low_end = VALUE_CATEGORY::VARIABLE_CONSTANT;
    TEST_REDUCE_VC(g);
  }

  {
    Group g = {};
    g.high_start = VALUE_CATEGORY::VARIABLE_CONSTANT;
    g.low_start = VALUE_CATEGORY::TEMPORARY_CONSTANT;

    g.low_end = VALUE_CATEGORY::TEMPORARY_CONSTANT;
    TEST_REDUCE_VC(g);
  }

  {
    Group g = {};
    g.high_start = VALUE_CATEGORY::VARIABLE_MUTABLE;
    g.low_start = VALUE_CATEGORY::VARIABLE_IMMUTABLE;

    g.low_end = VALUE_CATEGORY::VARIABLE_IMMUTABLE;
    TEST_REDUCE_VC(g);
  }
  {
    Group g = {};
    g.high_start = VALUE_CATEGORY::VARIABLE_MUTABLE;
    g.low_start = VALUE_CATEGORY::TEMPORARY_IMMUTABLE;

    g.low_end = VALUE_CATEGORY::TEMPORARY_IMMUTABLE;
    TEST_REDUCE_VC(g);
  }

  {
    Group g = {};
    g.high_start = VALUE_CATEGORY::VARIABLE_MUTABLE;
    g.low_start = VALUE_CATEGORY::TEMPORARY_CONSTANT;

    g.low_end = VALUE_CATEGORY::TEMPORARY_IMMUTABLE;
    TEST_REDUCE_VC(g);
  }
  {
    Group g = {};
    g.high_start = VALUE_CATEGORY::VARIABLE_MUTABLE;
    g.low_start = VALUE_CATEGORY::VARIABLE_CONSTANT;

    g.low_end = VALUE_CATEGORY::VARIABLE_IMMUTABLE;
    TEST_REDUCE_VC(g);
  }
}