#include "operators.h"
#include "compiler.h"
#include "ast.h"

void emit_add_64s(const BuildOptions* const,
                  State*, CodeBlock* code,
                  ValueIndex left, ValueIndex right) {
  ByteCode::EMIT::ADD_R64S(code->code, (uint8_t)right.val, (uint8_t)left.val);
}

void emit_sub_64s(const BuildOptions* const,
                  State*, CodeBlock* code,
                  ValueIndex left, ValueIndex right) {
  ByteCode::EMIT::SUB_R64S(code->code, (uint8_t)right.val, (uint8_t)left.val);
}

void emit_mul_64s(const BuildOptions* const,
                  State*, CodeBlock* code,
                  ValueIndex left, ValueIndex right) {
  ByteCode::EMIT::MUL_R64S(code->code, (uint8_t)right.val, (uint8_t)left.val);
}

void emit_div_u64s(const BuildOptions* const build_options,
                   State* state, CodeBlock* code,
                   ValueIndex left, ValueIndex right) {
  if (build_options->system == &system_x86_64) {
    ValueIndex save_rdx = state->new_value();

    auto* save_val = state->value_tree.values.data + save_rdx.val;
    save_val->value_type = ValueType::FIXED;
    save_val->reg   = RDX.REG;

    auto* left_val = state->value_tree.values.data + left.val;
    left_val->value_type = ValueType::FIXED;
    left_val->reg   = RAX.REG;

    //Stop it from being removed
    ByteCode::EMIT::RESERVE(code->code, (uint8_t)save_rdx.val);
    state->use_value(save_rdx);

    state->control_flow.expression_num++;
  }

  ByteCode::EMIT::DIV_RU64S(code->code, (uint8_t)right.val, (uint8_t)left.val);
}

void emit_div_i64s(const BuildOptions* const build_options,
                   State* state, CodeBlock* code,
                   ValueIndex left, ValueIndex right) {

  if (build_options->system == &system_x86_64) {
    ValueIndex save_rdx = state->new_value();

    auto* save_val = state->value_tree.values.data + save_rdx.val;
    save_val->value_type = ValueType::FIXED;
    save_val->reg   = RDX.REG;

    auto* left_val = state->value_tree.values.data + left.val;
    left_val->value_type = ValueType::FIXED;
    left_val->reg   = RAX.REG;

    //Stop it from being removed
    ByteCode::EMIT::RESERVE(code->code, (uint8_t)save_rdx.val);
    state->use_value(save_rdx);
  }

  ByteCode::EMIT::DIV_RI64S(code->code, (uint8_t)right.val, (uint8_t)left.val);
}

void emit_eq_64s(const BuildOptions* const build_options,
                 State* state, CodeBlock* code,
                 ValueIndex left, ValueIndex right) {
  ByteCode::EMIT::EQ_R64S(code->code, (uint8_t)right.val, (uint8_t)left.val);
}

void emit_or_64s(const BuildOptions* const build_options,
                 State* state, CodeBlock* code,
                 ValueIndex left, ValueIndex right) {
  ByteCode::EMIT::OR_R64S(code->code, (uint8_t)right.val, (uint8_t)left.val);
}

void emit_and_64s(const BuildOptions* const build_options,
                  State* state, CodeBlock* code,
                  ValueIndex left, ValueIndex right) {
  ByteCode::EMIT::AND_R64S(code->code, (uint8_t)right.val, (uint8_t)left.val);
}

void emit_neg_i64(const BuildOptions*,
                  State*, CodeBlock* code,
                  ValueIndex prim) {
  ByteCode::EMIT::NEG_R64(code->code, (uint8_t)prim.val);
}

const Structure* BIN_OP_TESTS::num_int_64_bit(const Types* types, const Structure* left, const Structure* right) {
  constexpr auto is_valid_type = [](const Types* types, const Structure* s) {
    return s == types->s_u64 || s == types->s_i64 || s == types->s_int_lit || s == types->s_sint_lit;
  };

  if (!is_valid_type(types, left) || !is_valid_type(types, right)) {
    return nullptr;
  }
  
  
  if(left == right){
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

const Structure* BIN_OP_TESTS::num_signed_int_64_bit(const Types* types, const Structure* left, const Structure* right) {

  constexpr auto is_valid_type = [](const Types* types, const Structure* s) {
    return s == types->s_i64 || s == types->s_int_lit || s == types->s_sint_lit;
  };

  if (!is_valid_type(types, left) || !is_valid_type(types, right)) {
    return nullptr;
  }

  if(left == right){
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

const Structure* BIN_OP_TESTS::num_unsigned_int_64_bit(const Types* types, const Structure* left, const Structure* right) {
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

const Structure* BIN_OP_TESTS::eq_int_64_bit(const Types* types, const Structure* left, const Structure* right) {
  constexpr auto is_valid_type = [](const Types* types, const Structure* s) {
    return s == types->s_u64 || s == types->s_i64 || s == types->s_int_lit || s == types->s_sint_lit;
  };

  if (!is_valid_type(types, left) || !is_valid_type(types, right)) {
    return nullptr;
  }

  if(left == right || can_literal_cast(left, right) || can_literal_cast(right, left)){
    return types->s_bool;
  }
  else {
    return nullptr;
  }
}

const Structure* BIN_OP_TESTS::bools(const Types* types, const Structure* left, const Structure* right) {
  if (left == right && left == types->s_bool) {
    return types->s_bool;
  }
  else {
    return nullptr;
  }
}

const Structure* UN_OP_TESTS::signed_int_64_bit(const Types* types, const Structure* s) {
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

CompileCode find_binary_operator(const Types* types,
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
         op_string, left->name.string, right->name.string);
  return CompileCode::TYPE_CHECK_ERROR;
}

CompileCode find_unary_operator(const Types* types,
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
         op_string, prim->name.string);
  return CompileCode::TYPE_CHECK_ERROR;
}