#include <AxleTest/unit_tests.h>
#include <AxleUtil/utility.h>

#include "ir.h"
#include "backends.h"
#include "type.h"

template<typename T>
void run_test_for_integer(AxleTest::TestErrors* test_errors,
                          const BuiltinTypes* builtin_types,
                          const Type& int_t) {
  IR::IRStore ir = {};
  ir.signature = builtin_types->t_void_call.unchecked_base<SignatureStructure>();
  ir.global_label = { 1 };

  

  IR::LocalLabel start_cf = ir.new_control_block();
  IR::LocalLabel end_cf = ir.new_control_block();
  ir.start_control_block(start_cf);
  ir.set_current_cf(IR::CFStart{ end_cf });

  Eval::RuntimeValue v0 = Eval::as_direct(ir.new_temporary(int_t, {}), int_t);
  {
    u8 c0_data[sizeof(T)] = {};
    Axle::serialize_le<T>(c0_data, T{8});

    u8 c1_data[sizeof(T)] = {};
    Axle::serialize_le<T>(c1_data, T{16});

    Eval::RuntimeValue c0 = Eval::as_constant(c0_data, int_t);
    Eval::RuntimeValue c1 = Eval::as_constant(c1_data, int_t);

    IR::Types::Add add = {};
    add.left = Eval::load_v_arg(&ir, c0);
    add.right = Eval::load_v_arg(&ir, c1);
    add.to = Eval::load_v_arg(&ir, v0);

    IR::Emit::Add(ir.current_bytecode(), add);
  }

  Eval::RuntimeValue v1 = Eval::as_direct(ir.new_temporary(int_t, {}), int_t);
  {
    u8 c0_data[sizeof(T)] = {};
    Axle::serialize_le<T>(c0_data, T{3});

    Eval::RuntimeValue c0 = Eval::as_constant(c0_data, int_t);

    IR::Types::Mul mul = {};
    mul.left = Eval::load_v_arg(&ir, c0);
    mul.right = Eval::load_v_arg(&ir, v0);
    mul.to = Eval::load_v_arg(&ir, v1);

    IR::Emit::Mul(ir.current_bytecode(), mul);
  }

  Eval::RuntimeValue v2 = Eval::as_direct(ir.new_temporary(int_t, {}), int_t);
  {
    u8 c0_data[sizeof(T)] = {};
    Axle::serialize_le<T>(c0_data, T{15});

    Eval::RuntimeValue c0 = Eval::as_constant(c0_data, int_t);

    IR::Types::Div div = {};
    div.left = Eval::load_v_arg(&ir, v1);
    div.right = Eval::load_v_arg(&ir, c0);
    div.to = Eval::load_v_arg(&ir, v2);

    IR::Emit::Div(ir.current_bytecode(), div);
  }

  Eval::RuntimeValue v3 = Eval::as_direct(ir.new_temporary(int_t, {}), int_t);
  {
    u8 c0_data[sizeof(T)] = {};
    Axle::serialize_le<T>(c0_data, T{93});

    Eval::RuntimeValue c0 = Eval::as_constant(c0_data, int_t);

    IR::Types::Sub sub = {};
    sub.left = Eval::load_v_arg(&ir, v2);
    sub.right = Eval::load_v_arg(&ir, c0);
    sub.to = Eval::load_v_arg(&ir, v3);

    IR::Emit::Sub(ir.current_bytecode(), sub);
  }

  Eval::RuntimeValue v4 = Eval::as_direct(ir.new_temporary(int_t, {}), int_t);
  {
    u8 c0_data[sizeof(T)] = {};
    Axle::serialize_le<T>(c0_data, T{13});

    Eval::RuntimeValue c0 = Eval::as_constant(c0_data, int_t);

    IR::Types::Mod mod = {};
    mod.left = Eval::load_v_arg(&ir, v3);
    mod.right = Eval::load_v_arg(&ir, c0);
    mod.to = Eval::load_v_arg(&ir, v4);

    IR::Emit::Mod(ir.current_bytecode(), mod);
  }

  ir.start_control_block(end_cf);
  ir.set_current_cf(IR::CFEnd{ start_cf });

  {
    Errors errors = {};

    const VM::Env env = {
      builtin_types,
      &errors,
    };
    auto sf = VM::new_stack_frame(&ir);
    VM::exec(&env, &sf);

    if (errors.is_panic()) {
      test_errors->first_error = std::move(errors.error_messages.data[0].message);
      return;
    }

    VM::StackFrame::RealValue res_val = sf.get_value(IR::v_arg(v4.value.index, v4.value.offset, v4.type));

    TEST_EQ(res_val.t, int_t);

    u8 final_data[sizeof(T)] = {};
    const T final_v = static_cast<T>(static_cast<T>(static_cast<T>(T{ 3 } * static_cast<T>(T{8} + T{16})) / T{ 15 }) - T{ 93 }) % T{ 13 };
    Axle::serialize_le(final_data, final_v);

    TEST_ARR_EQ(final_data, sizeof(T), res_val.ptr, res_val.t.size());
  }
}

TEST_FUNCTION(IR, basic_math) {
  //Setup
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  BuiltinTypes builtin_types = STRUCTS::create_builtins(&structures, &strings);

  //Check for all integer types
  run_test_for_integer<u8>(test_errors, &builtin_types,
                           builtin_types.t_u8);
  TEST_CHECK_ERRORS();

  run_test_for_integer<i8>(test_errors, &builtin_types,
                           builtin_types.t_i8);
  TEST_CHECK_ERRORS();

  run_test_for_integer<u16>(test_errors, &builtin_types,
                           builtin_types.t_u16);
  TEST_CHECK_ERRORS();

  run_test_for_integer<i16>(test_errors, &builtin_types,
                           builtin_types.t_i16);
  TEST_CHECK_ERRORS();

  run_test_for_integer<u32>(test_errors, &builtin_types,
                           builtin_types.t_u32);
  TEST_CHECK_ERRORS();

  run_test_for_integer<i32>(test_errors, &builtin_types,
                           builtin_types.t_i32);
  TEST_CHECK_ERRORS();

  run_test_for_integer<u64>(test_errors, &builtin_types,
                           builtin_types.t_u64);
  TEST_CHECK_ERRORS();

  run_test_for_integer<i64>(test_errors, &builtin_types,
                           builtin_types.t_i64);
  TEST_CHECK_ERRORS();
}

