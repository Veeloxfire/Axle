#include <AxleTest/unit_tests.h>
#include <AxleUtil/utility.h>

#include "ir.h"
#include "backends.h"
#include "type.h"

#include "compiler.h"

static constexpr CallingConvention TEST_CONVENTION {
  .name = Axle::lit_view_arr("TEST_CONVENTION"),
};

template<typename T>
void run_test_for_integer(AxleTest::TestErrors* test_errors,
                          BuiltinTypes* builtin_types,
                          const SignatureStructure* empty_sig,
                          const Type& int_t) {
  AXLE_TELEMETRY_FUNCTION();
  
  CompilerGlobals comp;
  comp.builtin_types = builtin_types;
  CompilerThread comp_thread;
  comp_thread.builtin_types = builtin_types;

  IR::GlobalLabel label = comp.new_ir_function(empty_sig, {}, UnitID{1});
  
  IR::IRStore& ir = *comp.get_ir(label);

  IR::LocalLabel start_cf = ir.new_control_block();
  IR::LocalLabel end_cf = ir.new_control_block();
  ir.start_control_block(start_cf);
  ir.set_current_cf(IR::CFStart{ end_cf });
  ir.start_control_block(end_cf);
  ir.set_current_cf(IR::CFEnd{ start_cf });

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

  ir.completed = true;

  {
    auto sf = VM::new_stack_frame(&ir);
    VM::exec(&comp, &comp_thread, &sf);

    if (comp_thread.is_panic()) {
      test_errors->first_error = std::move(comp_thread.errors.error_messages.data[0].message);
      return;
    }

    TEST_EQ(static_cast<const IR::ControlBlock*>(&ir.control_blocks[end_cf.label - 1]),
            sf.current_block);

    VM::StackFrame::RealValue res_val = sf.get_value(IR::v_arg(v4.value.index, v4.value.offset, v4.type));

    TEST_EQ(res_val.t, int_t);

    u8 final_data[sizeof(T)] = {};
    const T final_v = static_cast<T>(static_cast<T>(static_cast<T>(T{ 3 } * static_cast<T>(T{8} + T{16})) / T{ 15 }) - T{ 93 }) % T{ 13 };
    Axle::serialize_le(final_data, final_v);

    TEST_ARR_EQ(final_data, sizeof(T), res_val.ptr, res_val.t.size());
  }
}

TEST_FUNCTION(VM, basic_math) {
  //Setup
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  BuiltinTypes builtin_types = STRUCTS::create_builtins(&structures, &strings);

  const SignatureStructure* empty_sig = find_or_make_lambda_structure(&structures, &strings, &TEST_CONVENTION, {}, builtin_types.t_void);

  //Check for all integer types
  run_test_for_integer<u8>(test_errors, &builtin_types,
                           empty_sig,
                           builtin_types.t_u8);
  TEST_CHECK_ERRORS();

  run_test_for_integer<i8>(test_errors, &builtin_types,
                           empty_sig,
                           builtin_types.t_i8);
  TEST_CHECK_ERRORS();

  run_test_for_integer<u16>(test_errors, &builtin_types,
                           empty_sig,
                           builtin_types.t_u16);
  TEST_CHECK_ERRORS();

  run_test_for_integer<i16>(test_errors, &builtin_types,
                           empty_sig,
                           builtin_types.t_i16);
  TEST_CHECK_ERRORS();

  run_test_for_integer<u32>(test_errors, &builtin_types,
                           empty_sig,
                           builtin_types.t_u32);
  TEST_CHECK_ERRORS();

  run_test_for_integer<i32>(test_errors, &builtin_types,
                           empty_sig,
                           builtin_types.t_i32);
  TEST_CHECK_ERRORS();

  run_test_for_integer<u64>(test_errors, &builtin_types,
                           empty_sig,
                           builtin_types.t_u64);
  TEST_CHECK_ERRORS();

  run_test_for_integer<i64>(test_errors, &builtin_types,
                           empty_sig,
                           builtin_types.t_i64);
  TEST_CHECK_ERRORS();
}

TEST_FUNCTION(VM, parameters_and_returns) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  BuiltinTypes builtin_types = STRUCTS::create_builtins(&structures, &strings);

  CompilerGlobals comp;
  comp.builtin_types = &builtin_types;
  CompilerThread comp_thread;
  comp_thread.builtin_types = &builtin_types;

  const SignatureStructure* sig = [&structures, &strings, &builtin_types]() {
    Axle::OwnedArr<Type> params = Axle::new_arr<Type>(2);
    params[0] = builtin_types.t_u64;
    params[1] = builtin_types.t_u64;

    return find_or_make_lambda_structure(&structures, &strings, &TEST_CONVENTION, 
        std::move(params), builtin_types.t_u64);
  }();

  IR::GlobalLabel label = comp.new_ir_function(sig, {}, UnitID{1});
  
  IR::IRStore& ir = *comp.get_ir(label);

  IR::LocalLabel start_cf = ir.new_control_block();
  IR::LocalLabel ret_cf = ir.new_control_block();
  ir.start_control_block(start_cf);
  ir.set_current_cf(IR::CFStart{ ret_cf });

  IR::VariableId a = ir.new_variable(builtin_types.t_u64, {}, false);
  IR::VariableId b = ir.new_variable(builtin_types.t_u64, {}, false);
  {
    IR::V_ARG args[2] = {
      IR::v_arg(ir.new_temporary(a, {}), 0, builtin_types.t_u64),
      IR::v_arg(ir.new_temporary(b, {}), 0, builtin_types.t_u64),
    };

    IR::Types::StartFunc fnc;
    fnc.n_values = 2;
    fnc.values = args;

    IR::Emit::StartFunc(ir.current_bytecode(), fnc);
  }

  TEST_EQ(static_cast<usize>(2), ir.current_control_block()->temporaries.size);

  ir.start_control_block(ret_cf);

  IR::ValueIndex ret_dest = ir.new_temporary(builtin_types.t_u64, {});

  {
    IR::Types::Div div;
    div.left = IR::v_arg(ir.new_temporary(a, {}), 0, builtin_types.t_u64);
    div.right = IR::v_arg(ir.new_temporary(b, {}), 0, builtin_types.t_u64);
    div.to = IR::v_arg(ret_dest, 0, builtin_types.t_u64);

    IR::Emit::Div(ir.current_bytecode(), div);
  }
  
  ir.set_current_cf(IR::CFReturn{ start_cf, ret_dest });
  TEST_EQ(static_cast<usize>(3), ir.current_control_block()->temporaries.size);
  
  TEST_EQ(static_cast<usize>(2), ir.variables.size);
  ir.completed = true;

  constexpr u64 EXPECTED = 1834;
  constexpr u64 B_VAL = 9123;
  constexpr u64 A_VAL = EXPECTED * B_VAL;
  static_assert(EXPECTED == A_VAL / B_VAL);

  {
    auto sf = VM::new_stack_frame(&ir);
    TEST_EQ(true, sf.has_return);
    TEST_EQ(static_cast<u32>(2), sf.num_parameters);

    {
      auto p_a = sf.get_parameter(0);
      TEST_EQ(builtin_types.t_u64, p_a.t);
      TEST_EQ(sf.bytes.data, p_a.ptr);
      Axle::serialize_le(Axle::view_arr(p_a), static_cast<u64>(A_VAL));
    }
    {
      auto p_b = sf.get_parameter(1);
      TEST_EQ(builtin_types.t_u64, p_b.t);
      TEST_EQ(sf.bytes.data + 8, p_b.ptr);
      Axle::serialize_le(Axle::view_arr(p_b), static_cast<u64>(B_VAL));
    }

    VM::exec(&comp, &comp_thread, &sf);

    if (comp_thread.is_panic()) {
      test_errors->first_error = std::move(comp_thread.errors.error_messages.data[0].message);
      return;
    }

    VM::StackFrame::RealValue res_val = sf.get_return_value();
    TEST_EQ(builtin_types.t_u64, res_val.t);
    TEST_EQ(sf.bytes.data + 16, res_val.ptr);

    u64 out_val = 0;
    Axle::deserialize_le(Axle::view_arr(res_val), out_val);

    TEST_EQ(EXPECTED, out_val);
  }
}

TEST_FUNCTION(VM, calls) {
  Axle::StringInterner strings = {};
  Structures structures = {8,8};

  BuiltinTypes builtin_types = STRUCTS::create_builtins(&structures, &strings);

  CompilerGlobals comp;
  comp.builtin_types = &builtin_types;
  CompilerThread comp_thread;
  comp_thread.builtin_types = &builtin_types;

  const SignatureStructure* sig = [&structures, &strings, &builtin_types]() {
    Axle::OwnedArr<Type> params = Axle::new_arr<Type>(2);
    params[0] = builtin_types.t_u64;
    params[1] = builtin_types.t_u64;

    return find_or_make_lambda_structure(&structures, &strings, &TEST_CONVENTION, 
        std::move(params), builtin_types.t_u64);
  }();

  IR::GlobalLabel label_a = comp.new_ir_function(sig, {}, UnitID{1});
  IR::GlobalLabel label_b = comp.new_ir_function(sig, {}, UnitID{2});
  
  // Function that does the math
  {
    IR::IRStore& ir = *comp.get_ir(label_a);

    IR::LocalLabel start_cf = ir.new_control_block();
    IR::LocalLabel ret_cf = ir.new_control_block();
    ir.start_control_block(start_cf);
    ir.set_current_cf(IR::CFStart{ ret_cf });

    IR::VariableId a = ir.new_variable(builtin_types.t_u64, {}, false);
    IR::VariableId b = ir.new_variable(builtin_types.t_u64, {}, false);
    {
      IR::V_ARG args[2] = {
        IR::v_arg(ir.new_temporary(a, {}), 0, builtin_types.t_u64),
        IR::v_arg(ir.new_temporary(b, {}), 0, builtin_types.t_u64),
      };

      IR::Types::StartFunc fnc;
      fnc.n_values = 2;
      fnc.values = args;

      IR::Emit::StartFunc(ir.current_bytecode(), fnc);
    }

    TEST_EQ(static_cast<usize>(2), ir.current_control_block()->temporaries.size);

    ir.start_control_block(ret_cf);

    IR::ValueIndex ret_dest = ir.new_temporary(builtin_types.t_u64, {});

    {
      IR::Types::Div div;
      div.left = IR::v_arg(ir.new_temporary(a, {}), 0, builtin_types.t_u64);
      div.right = IR::v_arg(ir.new_temporary(b, {}), 0, builtin_types.t_u64);
      div.to = IR::v_arg(ret_dest, 0, builtin_types.t_u64);

      IR::Emit::Div(ir.current_bytecode(), div);
    }
    
    ir.set_current_cf(IR::CFReturn{ start_cf, ret_dest });
    TEST_EQ(static_cast<usize>(3), ir.current_control_block()->temporaries.size);
    
    TEST_EQ(static_cast<usize>(2), ir.variables.size);
    ir.completed = true;
  }

  // Function that calls another function
  {
    IR::IRStore& ir = *comp.get_ir(label_b);

    IR::LocalLabel start_cf = ir.new_control_block();
    IR::LocalLabel ret_cf = ir.new_control_block();
    ir.start_control_block(start_cf);
    ir.set_current_cf(IR::CFStart{ ret_cf });

    IR::VariableId a = ir.new_variable(builtin_types.t_u64, {}, false);
    IR::VariableId b = ir.new_variable(builtin_types.t_u64, {}, false);
    {
      IR::V_ARG args[2] = {
        IR::v_arg(ir.new_temporary(a, {}), 0, builtin_types.t_u64),
        IR::v_arg(ir.new_temporary(b, {}), 0, builtin_types.t_u64),
      };

      IR::Types::StartFunc fnc;
      fnc.n_values = 2;
      fnc.values = args;

      IR::Emit::StartFunc(ir.current_bytecode(), fnc);
    }

    TEST_EQ(static_cast<usize>(2), ir.current_control_block()->temporaries.size);

    ir.start_control_block(ret_cf);

    IR::ValueIndex ret_dest = ir.new_temporary(builtin_types.t_u64, {});

    {
      IR::V_ARG args_and_ret[3] = {
        IR::v_arg(ir.new_temporary(a, {}), 0, builtin_types.t_u64),
        IR::v_arg(ir.new_temporary(b, {}), 0, builtin_types.t_u64),
        IR::v_arg(ret_dest, 0, builtin_types.t_u64),
      };

      IR::Types::Call call;
      call.label = label_a;
      call.values = args_and_ret;
      call.n_values = 3;

      IR::Emit::Call(ir.current_bytecode(), call);
    }
    
    ir.set_current_cf(IR::CFReturn{ start_cf, ret_dest });
    TEST_EQ(static_cast<usize>(3), ir.current_control_block()->temporaries.size);
    
    TEST_EQ(static_cast<usize>(2), ir.variables.size);
    ir.completed = true;
  }

  constexpr u64 EXPECTED = 1834;
  constexpr u64 B_VAL = 9123;
  constexpr u64 A_VAL = EXPECTED * B_VAL;
  static_assert(EXPECTED == A_VAL / B_VAL);

  {
    auto sf = VM::new_stack_frame(comp.get_ir(label_b));
    TEST_EQ(true, sf.has_return);
    TEST_EQ(static_cast<u32>(2), sf.num_parameters);

    {
      auto p_a = sf.get_parameter(0);
      TEST_EQ(builtin_types.t_u64, p_a.t);
      TEST_EQ(sf.bytes.data, p_a.ptr);
      Axle::serialize_le(Axle::view_arr(p_a), static_cast<u64>(A_VAL));
    }
    {
      auto p_b = sf.get_parameter(1);
      TEST_EQ(builtin_types.t_u64, p_b.t);
      TEST_EQ(sf.bytes.data + 8, p_b.ptr);
      Axle::serialize_le(Axle::view_arr(p_b), static_cast<u64>(B_VAL));
    }

    VM::exec(&comp, &comp_thread, &sf);

    if (comp_thread.is_panic()) {
      test_errors->first_error = std::move(comp_thread.errors.error_messages.data[0].message);
      return;
    }

    VM::StackFrame::RealValue res_val = sf.get_return_value();
    TEST_EQ(builtin_types.t_u64, res_val.t);
    TEST_EQ(sf.bytes.data + 16, res_val.ptr);

    u64 out_val = 0;
    Axle::deserialize_le(Axle::view_arr(res_val), out_val);

    TEST_EQ(EXPECTED, out_val);
  }
}
