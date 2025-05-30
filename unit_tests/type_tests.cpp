#include <AxleTest/unit_tests.h>

#include "type.h"
#include "ast.h"

struct Group {
  VALUE_CATEGORY high_start;
  VALUE_CATEGORY low_start;
  VALUE_CATEGORY low_end;
};
static void test_reduce_vc(AxleTest::TestErrors* test_errors, AST* low, AST_LOCAL high, const Group& g) {
  low->value_category = g.low_start;
  high.ast->value_category = g.high_start;

  reduce_category(low, high);

  TEST_EQ(g.high_start, high.ast->value_category);
  TEST_EQ(g.low_end, low->value_category);
}
#define TEST_REDUCE_VC(group) test_reduce_vc(test_errors, &low, { &high }, (group)); if(test_errors->is_panic()) return

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
  TEST_NEQ(builtins.t_ascii, Type{});
  TEST_NEQ(builtins.t_bool, Type{});
  TEST_NEQ(builtins.e_true, static_cast<const EnumValue*>(nullptr));
  TEST_NEQ(builtins.e_false, static_cast<const EnumValue*>(nullptr));
};


template <typename T>
static void test_int_type(AxleTest::TestErrors* test_errors,
                          const Type& ty, IR::Format expected_format) {
  const Structure* s = ty.structure;
  TEST_EQ(STRUCTURE_TYPE::INTEGER, s->type);
  const IntegerStructure* is = static_cast<const IntegerStructure*>(ty.structure);
  TEST_EQ(STRUCTURE_TYPE::INTEGER, is->expected_type_enum);
  TEST_EQ(expected_format, is->ir_format);
  TEST_EQ(static_cast<u32>(sizeof(T)), is->size);
  TEST_EQ(static_cast<u32>(alignof(T)), is->alignment);
  TEST_EQ(std::is_signed_v<T>, is->is_signed);
}

TEST_FUNCTION(Types, integers) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  BuiltinTypes builtin = STRUCTS::create_builtins(&structures, &strings);

  test_int_type<u8>(test_errors, builtin.t_u8, IR::Format::uint8);
  if (test_errors->is_panic()) return;
  test_int_type<i8>(test_errors, builtin.t_i8, IR::Format::sint8);
  if (test_errors->is_panic()) return;
  test_int_type<u16>(test_errors, builtin.t_u16, IR::Format::uint16);
  if (test_errors->is_panic()) return;
  test_int_type<i16>(test_errors, builtin.t_i16, IR::Format::sint16);
  if (test_errors->is_panic()) return;
  test_int_type<u32>(test_errors, builtin.t_u32, IR::Format::uint32);
  if (test_errors->is_panic()) return;
  test_int_type<i32>(test_errors, builtin.t_i32, IR::Format::sint32);
  if (test_errors->is_panic()) return;
  test_int_type<u64>(test_errors, builtin.t_u64, IR::Format::uint64);
  if (test_errors->is_panic()) return;
  test_int_type<i64>(test_errors, builtin.t_i64, IR::Format::sint64);
  if (test_errors->is_panic()) return;
}

TEST_FUNCTION(Types, type) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  BuiltinTypes builtin = STRUCTS::create_builtins(&structures, &strings);

  const Structure* s = builtin.t_type.structure;
  TEST_EQ(STRUCTURE_TYPE::TYPE, s->type);
  const TypeStructure* ts = static_cast<const TypeStructure*>(s);
  TEST_EQ(STRUCTURE_TYPE::TYPE, ts->expected_type_enum);
  TEST_EQ(IR::Format::opaque, ts->ir_format);
  TEST_EQ(static_cast<u32>(sizeof(Type)), ts->size);
  TEST_EQ(static_cast<u32>(alignof(Type)), ts->alignment);
}

TEST_FUNCTION(Types, lambdas) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  BuiltinTypes builtin = STRUCTS::create_builtins(&structures, &strings);

  const SignatureStructure* sig_struct
    = find_or_make_lambda_structure(&structures, &strings,
                                    nullptr, {}, builtin.t_void);


  TEST_EQ(STRUCTURE_TYPE::LAMBDA, sig_struct->expected_type_enum);
  TEST_EQ(STRUCTURE_TYPE::LAMBDA, sig_struct->type);
  TEST_EQ(IR::Format::opaque, sig_struct->ir_format);
  TEST_EQ(static_cast<u32>(structures.pointer_size), sig_struct->size);
  TEST_EQ(static_cast<u32>(structures.pointer_size), sig_struct->alignment);
  TEST_EQ(static_cast<usize>(0), sig_struct->parameter_types.size);
  TEST_EQ(builtin.t_void, sig_struct->return_type);
}

TEST_FUNCTION(Types, pointer_slice_sizes) {
  Axle::StringInterner strings = {};
  Structures structures = {13,16};
  
  TEST_EQ(static_cast<usize>(13), structures.pointer_size);
  TEST_EQ(static_cast<usize>(16), structures.pointer_align);
  TEST_EQ(static_cast<usize>(26), structures.slice_size);
  TEST_EQ(static_cast<usize>(16), structures.slice_align);
}

TEST_FUNCTION(Types, pointers) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};
  
  TEST_EQ(static_cast<usize>(8), structures.pointer_size);
  TEST_EQ(static_cast<usize>(8), structures.pointer_align);
  TEST_EQ(static_cast<usize>(16), structures.slice_size);
  TEST_EQ(static_cast<usize>(8), structures.slice_align);

  BuiltinTypes builtin = STRUCTS::create_builtins(&structures, &strings);

  const PointerStructure* ptr_struct
    = find_or_make_pointer_structure(&structures, &strings,
        { .mut = true, .base = builtin.t_void });

  TEST_EQ(STRUCTURE_TYPE::POINTER, ptr_struct->expected_type_enum);
  TEST_EQ(STRUCTURE_TYPE::POINTER, ptr_struct->type);
  TEST_EQ(IR::Format::pointer, ptr_struct->ir_format);
  TEST_EQ(static_cast<u32>(structures.pointer_size), ptr_struct->size);
  TEST_EQ(static_cast<u32>(structures.pointer_align), ptr_struct->alignment);
  TEST_EQ(true, ptr_struct->mut);
  TEST_EQ(builtin.t_void, ptr_struct->base);

  TEST_EQ(builtin.t_void_ptr.structure, static_cast<const Structure*>(ptr_struct));
}

TEST_FUNCTION(Types, slices) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};
  
  TEST_EQ(static_cast<usize>(8), structures.pointer_size);
  TEST_EQ(static_cast<usize>(8), structures.pointer_align);
  TEST_EQ(static_cast<usize>(16), structures.slice_size);
  TEST_EQ(static_cast<usize>(8), structures.slice_align);

  BuiltinTypes builtin = STRUCTS::create_builtins(&structures, &strings);

  const SliceStructure* slice_struct
    = find_or_make_slice_structure(&structures, &strings,
        { .mut = true, .base = builtin.t_void });

  TEST_EQ(STRUCTURE_TYPE::SLICE, slice_struct->expected_type_enum);
  TEST_EQ(STRUCTURE_TYPE::SLICE, slice_struct->type);
  TEST_EQ(IR::Format::slice, slice_struct->ir_format);
  TEST_EQ(static_cast<u32>(structures.slice_size), slice_struct->size);
  TEST_EQ(static_cast<u32>(structures.slice_align), slice_struct->alignment);
  TEST_EQ(true, slice_struct->mut);
  TEST_EQ(builtin.t_void, slice_struct->base);
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
  TEST_EQ(IR::Format::opaque, arr_struct->ir_format);
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
  TEST_EQ(IR::Format::opaque, tup_struct->ir_format);
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

  void operator()(
      const auto* t_type,
      const auto* t_void,
      const auto* t_u8
      ) const {
    TEST_EQ(true, (std::same_as<decltype(t_type), const TypeStructure*>));
    TEST_EQ(true, (std::same_as<decltype(t_void), const VoidStructure*>));
    TEST_EQ(true, (std::same_as<decltype(t_u8), const IntegerStructure*>));
  }
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
