#include "tests.h"

#include "ir.h"

TEST_FUNCTION(IR_Copies) {
  {
    IR::Function function;

    const u64 c = 12;

    IR::ValueIndex constant0 = function.create_constant(IR::Primitive::Int, sizeof(c), &c);
    IR::ValueIndex temporary = function.emit_create_temporary(IR::Primitive::Int, sizeof(c));
    function.emit_copy(temporary, constant0);

    IR::ValueIndex variable0 = function.emit_create_variable(IR::Primitive::Int, sizeof(c));
    function.emit_copy(variable0, temporary);

    IR::ValueIndex variable1 = function.emit_create_variable(IR::Primitive::Int, sizeof(c));
    IR::ValueIndex ref0 = function.create_reference(IR::Primitive::Int, sizeof(c), variable1);
    function.emit_copy(ref0, variable0);

    const u64 offest = 8;

    IR::ValueIndex constant1 = function.create_constant(IR::Primitive::Int, sizeof(offest), &offest);
    IR::ValueIndex variable2 = function.emit_create_variable(IR::Primitive::Raw, 8 + sizeof(c));
    IR::ValueIndex ref1 = function.create_reference(IR::Primitive::Int, sizeof(c), variable2, constant1);
    function.emit_copy(ref1, ref0);

    IR::ValueIndex variable3 = function.emit_create_variable(IR::Primitive::Int, sizeof(c));
    function.emit_copy(variable3, ref1);

    function.emit_return();

    IR::ExecuionThread exec_thread(test_errors, &function);

    exec_thread.execute();
    TEST_CHECK_ERRORS();

    const IR::Value& val = function.values.data[variable3];
    const auto immediate = exec_thread.eval_immediate(val);

    TEST_EQ(sizeof(c), val.size);
    TEST_EQ(12, immediate.small);
  }

  {
    IR::Function function;

    const u8 array_data[18] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17 };

    IR::ValueIndex constant = function.create_constant(IR::Primitive::Raw, 18, array_data);
    IR::ValueIndex temporary = function.emit_create_temporary(IR::Primitive::Raw, 18);
    function.emit_copy(temporary, constant);

    IR::ValueIndex variable0 = function.emit_create_variable(IR::Primitive::Raw, 18);
    function.emit_copy(variable0, temporary);

    IR::ValueIndex variable1 = function.emit_create_variable(IR::Primitive::Raw, 18);
    IR::ValueIndex ref = function.create_reference(IR::Primitive::Raw, 18, variable1);
    function.emit_copy(ref, variable0);

    function.emit_return();

    IR::ExecuionThread exec_thread(test_errors, &function);

    exec_thread.execute();
    TEST_CHECK_ERRORS();

    const IR::Value& val = function.values.data[variable1];
    const auto immediate = exec_thread.eval_immediate(val);

    TEST_EQ(18, val.size);
    TEST_ARR_EQ(array_data, immediate.large, 18);
  }
}

TEST_FUNCTION(IR_Simple_Add) {
  {
    IR::Function function;

    const u64 left_d = 36;
    const u64 right_d = 27;

    IR::ValueIndex right = function.create_constant(IR::Primitive::Int, sizeof(right_d), &right_d);
    IR::ValueIndex left = function.create_constant(IR::Primitive::Int, sizeof(left_d), &left_d);
    IR::ValueIndex res = function.emit_create_variable(IR::Primitive::Int, 8);

    function.emit_add(res, left, right);
    function.emit_return();

    IR::ExecuionThread exec_thread(test_errors, &function);

    exec_thread.execute();
    TEST_CHECK_ERRORS();


    const IR::Value& val = function.values.data[res];
    const auto immediate = exec_thread.eval_immediate(val);

    TEST_EQ(8, val.size);
    TEST_EQ(left_d + right_d, immediate.small);
  }

  //Just trying a difference size
  {
    IR::Function function;

    const u16 left_d = 36;
    const u16 right_d = 27;

    IR::ValueIndex right = function.create_constant(IR::Primitive::Int, sizeof(right_d), &right_d);
    IR::ValueIndex left = function.create_constant(IR::Primitive::Int, sizeof(left_d), &left_d);
    IR::ValueIndex res = function.emit_create_variable(IR::Primitive::Int, 2);

    function.emit_add(res, left, right);
    function.emit_return();

    IR::ExecuionThread exec_thread(test_errors, &function);

    exec_thread.execute();
    TEST_CHECK_ERRORS();


    const IR::Value& val = function.values.data[res];
    const auto immediate = exec_thread.eval_immediate(val);

    TEST_EQ(2, val.size);
    TEST_EQ(left_d + right_d, immediate.small);
  }
}

TEST_FUNCTION(IR_Simple_Sub) {
  IR::Function function;
  const u64 left_d = 36;
  const u64 right_d = 27;

  IR::ValueIndex right = function.create_constant(IR::Primitive::Int, sizeof(right_d), &right_d);
  IR::ValueIndex left = function.create_constant(IR::Primitive::Int, sizeof(left_d), &left_d);
  IR::ValueIndex res = function.emit_create_variable(IR::Primitive::Int, 8);

  function.emit_sub(res, left, right);
  function.emit_return();

  IR::ExecuionThread exec_thread(test_errors, &function);

  exec_thread.execute();
  TEST_CHECK_ERRORS();


  const IR::Value& val = function.values.data[res];
  const auto immediate = exec_thread.eval_immediate(val);

  TEST_EQ(8, val.size);
  TEST_EQ(left_d - right_d, immediate.small);
}

TEST_FUNCTION(IR_Simple_Mul) {
  IR::Function function;
  const u64 left_d = 36;
  const u64 right_d = 27;

  IR::ValueIndex right = function.create_constant(IR::Primitive::Int, sizeof(right_d), &right_d);
  IR::ValueIndex left = function.create_constant(IR::Primitive::Int,sizeof(left_d), &left_d);
  IR::ValueIndex res = function.emit_create_variable(IR::Primitive::Int, 8);

  function.emit_mul(res, left, right);
  function.emit_return();

  IR::ExecuionThread exec_thread(test_errors, &function);

  exec_thread.execute();
  TEST_CHECK_ERRORS();


  const IR::Value& val = function.values.data[res];
  const auto immediate = exec_thread.eval_immediate(val);

  TEST_EQ(8, val.size);
  TEST_EQ(left_d * right_d, immediate.small);
}

TEST_FUNCTION(IR_Simple_Div) {
  IR::Function function;
  const u64 left_d = 36;
  const u64 right_d = 27;

  IR::ValueIndex right = function.create_constant(IR::Primitive::Int, sizeof(right_d), &right_d);
  IR::ValueIndex left = function.create_constant(IR::Primitive::Int, sizeof(left_d), &left_d);
  IR::ValueIndex res = function.emit_create_variable(IR::Primitive::Int, 8);

  function.emit_div(res, left, right);
  function.emit_return();

  IR::ExecuionThread exec_thread(test_errors, &function);

  exec_thread.execute();
  TEST_CHECK_ERRORS();


  const IR::Value& val = function.values.data[res];
  const auto immediate = exec_thread.eval_immediate(val);

  TEST_EQ(8, val.size);
  TEST_EQ(left_d / right_d, immediate.small);
}