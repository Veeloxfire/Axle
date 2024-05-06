#include <AxleTest/unit_tests.h>

#include "type.h"
#include "ast.h"

struct Group {
  VALUE_CATEGORY high_start;
  VALUE_CATEGORY low_start;
  VALUE_CATEGORY low_end;
};
static void test_reduce_vc(AxleTest::TestErrors* test_errors, AST_LOCAL low, AST_LOCAL high, const Group& g) {
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
        low.value_category = o2;
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

TEST_FUNCTION(Types, Builtin) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  BuiltinTypes builtins = STRUCTS::create_builtins(&structures, &strings);

  TEST_NEQ(builtins.t_type, Type{});
  TEST_NEQ(builtins.t_void, Type{});
  TEST_NEQ(builtins.t_u8  , Type{});
  TEST_NEQ(builtins.t_i8  , Type{});
  TEST_NEQ(builtins.t_u16 , Type{});
  TEST_NEQ(builtins.t_i16 , Type{});
  TEST_NEQ(builtins.t_u32 , Type{});
  TEST_NEQ(builtins.t_i32 , Type{});
  TEST_NEQ(builtins.t_u64 , Type{});
  TEST_NEQ(builtins.t_i64 , Type{});
  TEST_NEQ(builtins.t_void_ptr, Type{});
  TEST_NEQ(builtins.t_void_call, Type{});
  TEST_NEQ(builtins.t_ascii, Type{});
  TEST_NEQ(builtins.t_bool, Type{});
  TEST_NEQ(builtins.e_true, static_cast<const EnumValue*>(nullptr));
  TEST_NEQ(builtins.e_false, static_cast<const EnumValue*>(nullptr));
};

TEST_FUNCTION(Types, lambdas) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  BuiltinTypes builtin = STRUCTS::create_builtins(&structures, &strings);

  const SignatureStructure* sig_struct
    = find_or_make_lambda_structure(&structures, &strings,
                                    nullptr, {}, builtin.t_void);


  TEST_EQ(STRUCTURE_TYPE::LAMBDA, sig_struct->expected_type_enum);
  TEST_EQ(STRUCTURE_TYPE::LAMBDA, sig_struct->type);
  TEST_EQ(static_cast<u32>(structures.pointer_size), sig_struct->size);
  TEST_EQ(static_cast<u32>(structures.pointer_size), sig_struct->alignment);
  TEST_EQ(static_cast<usize>(0), sig_struct->parameter_types.size);
  TEST_EQ(builtin.t_void, sig_struct->return_type);

  TEST_EQ(builtin.t_void_call.structure, static_cast<const Structure*>(sig_struct));
}

TEST_FUNCTION(Types, pointers) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  BuiltinTypes builtin = STRUCTS::create_builtins(&structures, &strings);

  const PointerStructure* ptr_struct
    = find_or_make_pointer_structure(&structures, &strings,
                                     builtin.t_void);

  TEST_EQ(STRUCTURE_TYPE::POINTER, ptr_struct->expected_type_enum);
  TEST_EQ(STRUCTURE_TYPE::POINTER, ptr_struct->type);
  TEST_EQ(static_cast<u32>(structures.pointer_size), ptr_struct->size);
  TEST_EQ(static_cast<u32>(structures.pointer_size), ptr_struct->alignment);
  TEST_EQ(builtin.t_void, ptr_struct->base);

  TEST_EQ(builtin.t_void_ptr.structure, static_cast<const Structure*>(ptr_struct));
}

TEST_FUNCTION(Types, arrays) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  BuiltinTypes builtin = STRUCTS::create_builtins(&structures, &strings);

  const u32 ARR_LEN = 13;
  const ArrayStructure* arr_struct
   = STRUCTS::new_array_structure(&structures, &strings,
                                  builtin.t_u32, ARR_LEN);

  TEST_EQ(STRUCTURE_TYPE::FIXED_ARRAY, arr_struct->expected_type_enum);
  TEST_EQ(STRUCTURE_TYPE::FIXED_ARRAY, arr_struct->type);
  TEST_EQ(builtin.t_u32.size() * ARR_LEN, arr_struct->size);
  TEST_EQ(builtin.t_u32.structure->alignment, arr_struct->alignment);
  TEST_EQ(builtin.t_u32, arr_struct->base);

  const ArrayStructure* arr_struct2
   = find_or_make_array_structure(&structures, &strings,
                                 builtin.t_u32, ARR_LEN);
  TEST_EQ(arr_struct, arr_struct2);
}

TEST_FUNCTION(Types, tuples) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  BuiltinTypes builtin = STRUCTS::create_builtins(&structures, &strings);

  const Type example_tup[] = {
    builtin.t_u32,
    builtin.t_u64,
  };

  const TupleStructure* tup_struct
   = STRUCTS::new_tuple_structure(&structures, &strings,
                                  Axle::view_arr(example_tup));

  TEST_EQ(STRUCTURE_TYPE::TUPLE, tup_struct->expected_type_enum);
  TEST_EQ(STRUCTURE_TYPE::TUPLE, tup_struct->type);
  TEST_EQ(static_cast<u32>(16), tup_struct->size);
  TEST_EQ(static_cast<u32>(8), tup_struct->alignment);

  const TupleStructure* tup_struct2
   = find_or_make_tuple_structure(&structures, &strings,
                                  Axle::view_arr(example_tup));
  TEST_EQ(tup_struct, tup_struct2);
}

namespace  {
struct TypeVisitor {
  AxleTest::TestErrors* test_errors;

  void operator()(InvalidTypeVisit) const {
    test_errors->report_error("Had invalid type visit");
  }

  void operator()(
      const auto* t_type,
      const auto* t_void,
      const auto* t_u8
      ) const {
    TEST_EQ(true, (std::same_as<decltype(t_type), const TypeStructure*>));
    TEST_EQ(true, (std::same_as<decltype(t_void), const VoidStructure*>));
    TEST_EQ(true, (std::same_as<decltype(t_u8), const IntegerStructure*>));
  }

  void operator()(
      const TypeStructure* t_type,
      const VoidStructure* t_void,
      const IntegerStructure* t_u8) const {}
};
}

TEST_FUNCTION(Types, visitor) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  const BuiltinTypes builtin = STRUCTS::create_builtins(&structures, &strings);

  visit_types(TypeVisitor{test_errors},
    builtin.t_type.structure,
    builtin.t_void.structure,
    builtin.t_u8.structure);
}
