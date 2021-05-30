#include "operators.h"
#include "compiler.h"
#include "ast.h"

using UN_OP_EMIT = FUNCTION_PTR<void, Array<uint8_t>&, uint8_t>;
using BIN_OP_EMIT = FUNCTION_PTR<void, Array<uint8_t>&, uint8_t, uint8_t>;

static RuntimeValue bin_op_impl(Compiler* const comp,
                                State* const state,
                                CodeBlock* const code,
                                const Structure* type,
                                const RuntimeValue* left, const RuntimeValue* right,
                                BIN_OP_EMIT func) {

  assert(left->type != RVT::UNKNOWN);
  assert(right->type != RVT::UNKNOWN);

  const auto load_to_reg = [&](const RuntimeValue* v) -> RuntimeValue {

      RuntimeValue temp ={};
      temp.type = RVT::REGISTER;
      temp.reg  = state->new_value();

      copy_runtime_to_runtime(comp, state, code, type, v, &temp);

      return temp;

  };

  RuntimeValue temp_left = load_to_reg(left);
  RuntimeValue temp_right = load_to_reg(right);

  func(code->code, (uint8_t)temp_right.reg.val, (uint8_t)temp_left.reg.val);
 
  state->get_val(temp_left.reg)->is_modified = true;

  state->use_value(temp_left.reg);
  state->use_value(temp_right.reg);

  return temp_left;
}

static RuntimeValue un_op_impl(Compiler* const comp,
                               State* const state,
                               CodeBlock* const code,
                               const Structure* type,
                               const RuntimeValue* val,
                               UN_OP_EMIT func) {

  assert(val->type != RVT::UNKNOWN);

  const auto load_to_reg = [&](const RuntimeValue* v) -> RuntimeValue {

      RuntimeValue temp ={};
      temp.type = RVT::REGISTER;
      temp.reg  = state->new_value();

      copy_runtime_to_runtime(comp, state, code, type, v, &temp);

      return temp;

  };

  RuntimeValue temp = load_to_reg(val);

  func(code->code, (uint8_t)temp.reg.val);

  state->get_val(temp.reg)->is_modified = true;
  state->use_value(temp.reg);

  return temp;
}

RuntimeValue OP::emit_add_64s(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* left, const RuntimeValue* right) {

  return bin_op_impl(comp, state, code, comp->types->s_u64, left, right, ByteCode::EMIT::ADD_R64S);
}

RuntimeValue OP::emit_sub_64s(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* left, const RuntimeValue* right) {

  return bin_op_impl(comp, state, code, comp->types->s_u64, left, right, ByteCode::EMIT::SUB_R64S);
}

RuntimeValue OP::emit_mul_64s(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* left, const RuntimeValue* right) {

  return bin_op_impl(comp, state, code, comp->types->s_u64, left, right, ByteCode::EMIT::MUL_R64S);
}

RuntimeValue OP::emit_div_u64s(Compiler* const comp,
                               State* const state,
                               CodeBlock* const code,
                               const RuntimeValue* left, const RuntimeValue* right) {
  const Structure* type = comp->types->s_u64;

  if (comp->build_options.system == &system_x86_64) {
    ValueIndex save_rdx = state->new_value();
    state->set_value(save_rdx);

    auto* save_val = state->value_tree.values.data + save_rdx.val;
    save_val->value_type = ValueType::FIXED;
    save_val->reg   = RDX.REG;

    //Stop it from being removed
    ByteCode::EMIT::RESERVE(code->code, (uint8_t)save_rdx.val);
    state->use_value(save_rdx);

    state->control_flow.expression_num++;
    RuntimeValue res = bin_op_impl(comp, state, code, type, left, right, ByteCode::EMIT::DIV_RU64S);
    
    state->use_value(save_rdx);
    state->control_flow.expression_num++;

    auto* res_val = state->value_tree.values.data + res.reg.val;
    res_val->value_type = ValueType::FIXED;
    res_val->reg   = RAX.REG;

    return res;
  }
  else {
    return bin_op_impl(comp, state, code, type, left, right, ByteCode::EMIT::DIV_RU64S);
  }
}

RuntimeValue OP::emit_div_i64s(Compiler* const comp,
                               State* const state,
                               CodeBlock* const code,
                               const RuntimeValue* left, const RuntimeValue* right) {
  const Structure* type = comp->types->s_i64;

  if (comp->build_options.system == &system_x86_64) {
    ValueIndex save_rdx = state->new_value();
    state->set_value(save_rdx);

    auto* save_val = state->value_tree.values.data + save_rdx.val;
    save_val->value_type = ValueType::FIXED;
    save_val->reg   = RDX.REG;

    //Stop it from being removed
    ByteCode::EMIT::RESERVE(code->code, (uint8_t)save_rdx.val);
    state->use_value(save_rdx);

    state->control_flow.expression_num++;
    RuntimeValue res = bin_op_impl(comp, state, code, type, left, right, ByteCode::EMIT::DIV_RI64S);

    state->use_value(save_rdx);
    state->control_flow.expression_num++;

    auto* res_val = state->value_tree.values.data + res.reg.val;
    res_val->value_type = ValueType::FIXED;
    res_val->reg   = RAX.REG;

    return res;
  }
  else {
    return bin_op_impl(comp, state, code, type, left, right, ByteCode::EMIT::DIV_RI64S);
  }
}

RuntimeValue OP::emit_eq_64s(Compiler* const comp,
                             State* const state,
                             CodeBlock* const code,
                             const RuntimeValue* left, const RuntimeValue* right) {
  return bin_op_impl(comp, state, code, comp->types->s_u64, left, right, ByteCode::EMIT::EQ_R64S);
}

RuntimeValue OP::emit_or_64s(Compiler* const comp,
                             State* const state,
                             CodeBlock* const code,
                             const RuntimeValue* left, const RuntimeValue* right) {
  return bin_op_impl(comp, state, code, comp->types->s_u64, left, right, ByteCode::EMIT::OR_R64S);
}

RuntimeValue OP::emit_and_64s(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* left, const RuntimeValue* right) {
  return bin_op_impl(comp, state, code, comp->types->s_u64, left, right, ByteCode::EMIT::AND_R64S);
}

RuntimeValue OP::emit_neg_i64(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* val) {
  return un_op_impl(comp, state, code, comp->types->s_i64, val, ByteCode::EMIT::NEG_R64);
}

RuntimeValue OP::emit_address(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* val) {
  assert(val->type == RVT::MEMORY);

  RuntimeValue ptr_val ={};
  ptr_val.type = RVT::REGISTER;
  ptr_val.reg = state->new_value();

  ByteCode::EMIT::LOAD_ADDRESS(code->code, (uint8_t)ptr_val.reg.val, state->get_mem(val->mem)->mem);

  return ptr_val;
}

const Structure* BIN_OP_TESTS::num_int_64_bit(Types* types, const Structure* left, const Structure* right) {
  constexpr auto is_valid_type = [](const Types* types, const Structure* s) {
    return s == types->s_u64 || s == types->s_i64 || s == types->s_int_lit || s == types->s_sint_lit;
  };

  if (!is_valid_type(types, left) || !is_valid_type(types, right)) {
    return nullptr;
  }


  if (left == right) {
    return left;
  }
  else if (can_literal_cast(left, right)) {
    return right;
  }
  else if (can_literal_cast(right, left)) {
    return left;
  }
  else {
    return nullptr;
  }
}

const Structure* BIN_OP_TESTS::num_signed_int_64_bit(Types* types, const Structure* left, const Structure* right) {

  constexpr auto is_valid_type = [](const Types* types, const Structure* s) {
    return s == types->s_i64 || s == types->s_int_lit || s == types->s_sint_lit;
  };

  if (!is_valid_type(types, left) || !is_valid_type(types, right)) {
    return nullptr;
  }

  if (left == right) {
    return left;
  }
  else if (can_literal_cast(left, right)) {
    return right;
  }
  else if (can_literal_cast(right, left)) {
    return left;
  }
  else {
    return nullptr;
  }
}

const Structure* BIN_OP_TESTS::num_unsigned_int_64_bit(Types* types, const Structure* left, const Structure* right) {
  const bool left_lit = types->s_int_lit == left;
  const bool right_lit =  types->s_int_lit == right;

  if ((left == right || right_lit) && (left == types->s_u64 || left_lit)) {
    return left;
  }
  else if (left_lit && (right == types->s_u64 || right_lit)) {
    return right;
  }
  else {
    return nullptr;
  }
}

const Structure* BIN_OP_TESTS::eq_int_64_bit(Types* types, const Structure* left, const Structure* right) {
  constexpr auto is_valid_type = [](const Types* types, const Structure* s) {
    return s == types->s_u64 || s == types->s_i64 || s == types->s_int_lit || s == types->s_sint_lit;
  };

  if (!is_valid_type(types, left) || !is_valid_type(types, right)) {
    return nullptr;
  }

  if (left == right || can_literal_cast(left, right) || can_literal_cast(right, left)) {
    return types->s_bool;
  }
  else {
    return nullptr;
  }
}

const Structure* BIN_OP_TESTS::bools(Types* types, const Structure* left, const Structure* right) {
  if (left == right && left == types->s_bool) {
    return types->s_bool;
  }
  else {
    return nullptr;
  }
}

const Structure* UN_OP_TESTS::signed_int_64_bit(Types* types, const Structure* s) {
  if (types->s_sint_lit == s || types->s_i64 == s) {
    return s;
  }
  else if (types->s_int_lit == s) {
    return types->s_sint_lit;
  }
  else {
    return nullptr;
  }
}

const Structure* UN_OP_TESTS::address(Types* types, const Structure* s) {
  auto i = types->structures.begin();
  const auto end = types->structures.end();

  for (; i < end; i++) {
    const Structure* ptr = *i;
    if (ptr->type == STRUCTURE_TYPE::POINTER && ((const PointerStructure*)ptr)->base == s) {
      return ptr;
    }
  }

  PointerStructure* ptr = types->new_pointer();

  ptr->base = s;
  return ptr;
}

CompileCode find_binary_operator(Types* types,
                                 ASTExpression* expr,
                                 const BinaryOperation* operations,
                                 size_t num_ops) {

  const auto left = expr->bin_op.left->type;
  const auto right = expr->bin_op.right->type;

  assert(left != nullptr);
  assert(right != nullptr);

  auto op = operations;
  const auto end = operations + num_ops;

  for (; op < end; op++) {
    expr->type = op->test(types, left, right);

    if (expr->type != nullptr) {
      expr->bin_op.emit = op->func;

      return CompileCode::NO_ERRORS;
    }
  }

  const char* const op_string = BINARY_OP_STRING::get(expr->bin_op.op);

  printf("TYPE ERROR: No binary operator '%s' with operands left: '%s' and right: '%s'\n",
         op_string, left->name->string, right->name->string);
  return CompileCode::TYPE_CHECK_ERROR;
}

CompileCode find_unary_operator(Types* types,
                                ASTExpression* expr,
                                const UnaryOperation* operations,
                                size_t num_ops) {

  const auto prim = expr->un_op.primary->type;

  assert(prim != nullptr);

  auto op = operations;
  const auto end = operations + num_ops;

  for (; op < end; op++) {
    expr->type = op->test(types, prim);

    if (expr->type != nullptr) {
      expr->un_op.emit = op->func;

      return CompileCode::NO_ERRORS;
    }
  }

  const char* const op_string = UNARY_OP_STRING::get(expr->un_op.op);

  printf("TYPE ERROR: No unary operator '%s' with operand type: '%s'\n",
         op_string, prim->name->string);
  return CompileCode::TYPE_CHECK_ERROR;
}