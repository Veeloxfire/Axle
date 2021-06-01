#include "operators.h"
#include "compiler.h"
#include "ast.h"

using UN_OP_EMIT = FUNCTION_PTR<void, Array<uint8_t>&, uint8_t>;
using BIN_OP_EMIT = FUNCTION_PTR<void, Array<uint8_t>&, uint8_t, uint8_t>;

//Will always move to a register
static RuntimeValue load_to_mod_op(Compiler* comp, State* state, CodeBlock* code, const Structure* ty, const RuntimeValue* v) {
  RuntimeValue temp ={};
  temp.type = RVT::REGISTER;
  temp.reg  = state->new_value();

  copy_runtime_to_runtime(comp, state, code, ty, v, &temp);

  return temp;
};

//Will only move to a new register if its not a register
static RuntimeValue load_to_const_op(Compiler* comp, State* state, CodeBlock* code, const Structure* ty, const RuntimeValue* v) {
  if (v->type != RVT::REGISTER) {
    RuntimeValue temp ={};
    temp.type = RVT::REGISTER;
    temp.reg  = state->new_value();

    copy_runtime_to_runtime(comp, state, code, ty, v, &temp);

    return temp;
  }
  else {
    return *v;
  }
};

static void bin_op_impl(Compiler* const comp,
                        State* const state,
                        CodeBlock* const code,
                        const RuntimeValue* left, const RuntimeValue* right,
                        BIN_OP_EMIT func) {
  assert(left->type == RVT::REGISTER);
  assert(right->type == RVT::REGISTER);

  func(code->code, (uint8_t)right->reg.val, (uint8_t)left->reg.val);
 
  state->get_val(left->reg)->is_modified = true;

  state->use_value(left->reg);
  state->use_value(right->reg);
}

static void un_op_impl(Compiler* const comp,
                       State* const state,
                       CodeBlock* const code,
                       const RuntimeValue* val,
                       UN_OP_EMIT func) {
  assert(val->type == RVT::REGISTER);

  func(code->code, (uint8_t)val->reg.val);

  state->get_val(val->reg)->is_modified = true;
  state->use_value(val->reg);
}

RuntimeValue OP::emit_add_64s(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* left, const RuntimeValue* right) {
  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::ADD_R64S);

  return temp_left;
}

RuntimeValue OP::emit_sub_64s(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* left, const RuntimeValue* right) {

  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SUB_R64S);

  return temp_left;
}

RuntimeValue OP::emit_mul_64s(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* left, const RuntimeValue* right) {

  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::MUL_R64S);

  return temp_left;
}

RuntimeValue OP::emit_div_u64s(Compiler* const comp,
                               State* const state,
                               CodeBlock* const code,
                               const RuntimeValue* left, const RuntimeValue* right) {
  const Structure* type = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, type, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, type, right);

  if (comp->build_options.system == &system_x86_64) {
    {
      auto* res_val       = state->value_tree.values.data + temp_left.reg.val;
      res_val->value_type = ValueType::FIXED;
      res_val->reg        = RAX.REG;
    }

    ValueIndex save_rdx = state->new_value();
    state->set_value(save_rdx);

    auto* save_val       = state->value_tree.values.data + save_rdx.val;
    save_val->value_type = ValueType::FIXED;
    save_val->reg        = RDX.REG;

    ByteCode::EMIT::RESERVE(code->code, (uint8_t)save_rdx.val);//Just show its being reserved

    state->control_flow.expression_num++;
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::DIV_RU64S);
    
    //Stop it from being removed
    state->use_value(save_rdx);
    state->control_flow.expression_num++;


    return temp_left;
  }
  else {
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::DIV_RU64S);

    return temp_left;
  }
}

RuntimeValue OP::emit_div_i64s(Compiler* const comp,
                               State* const state,
                               CodeBlock* const code,
                               const RuntimeValue* left, const RuntimeValue* right) {
  const Structure* type = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, type, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, type, right);

  if (comp->build_options.system == &system_x86_64) {
    {
      auto* res_val       = state->value_tree.values.data + temp_left.reg.val;
      res_val->value_type = ValueType::FIXED;
      res_val->reg        = RAX.REG;
    }

    ValueIndex save_rdx = state->new_value();
    state->set_value(save_rdx);

    auto* save_val       = state->value_tree.values.data + save_rdx.val;
    save_val->value_type = ValueType::FIXED;
    save_val->reg        = RDX.REG;

    ByteCode::EMIT::RESERVE(code->code, (uint8_t)save_rdx.val);//Just show its being reserved

    state->control_flow.expression_num++;
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::DIV_RI64S);

    //Stop it from being removed
    state->use_value(save_rdx);
    state->control_flow.expression_num++;


    return temp_left;
  }
  else {
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::DIV_RI64S);

    return temp_left;
  }
}

RuntimeValue OP::emit_eq_64s(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* left, const RuntimeValue* right) {

  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::EQ_R64S);

  return temp_left;
}

RuntimeValue OP::emit_or_64s(Compiler* const comp,
                             State* const state,
                             CodeBlock* const code,
                             const RuntimeValue* left, const RuntimeValue* right) {

  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::OR_R64S);

  return temp_left;
}

RuntimeValue OP::emit_and_64s(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* left, const RuntimeValue* right) {
  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::AND_R64S);

  return temp_left;
}

RuntimeValue OP::emit_shift_l_64_by_8(Compiler* const comp,
                                      State* const state,
                                      CodeBlock* const code,
                                      const RuntimeValue* left, const RuntimeValue* right) {
  const Structure* left_t = comp->types->s_u64;
  const Structure* right_t = comp->types->s_u8;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, left_t, left);
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, right_t, right);

  if (comp->build_options.system == &system_x86_64) {
    auto* res_val       = state->value_tree.values.data + temp_right.reg.val;
    res_val->value_type = ValueType::FIXED;
    res_val->reg        = RCX.REG;

    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_L_R64_BY_R8);

    return temp_left;
  }
  else {
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_L_R64_BY_R8);

    return temp_left;
  }
}

RuntimeValue OP::emit_shift_r_u64_by_8(Compiler* const comp,
                                      State* const state,
                                      CodeBlock* const code,
                                      const RuntimeValue* left, const RuntimeValue* right) {
  const Structure* left_t = comp->types->s_u64;
  const Structure* right_t = comp->types->s_u8;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, left_t, left);
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, right_t, right);

  if (comp->build_options.system == &system_x86_64) {
    auto* res_val       = state->value_tree.values.data + temp_right.reg.val;
    res_val->value_type = ValueType::FIXED;
    res_val->reg        = RCX.REG;

    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_R_U64_BY_R8);

    return temp_left;
  }
  else {
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_R_U64_BY_R8);

    return temp_left;
  }
}

RuntimeValue OP::emit_shift_r_i64_by_8(Compiler* const comp,
                                      State* const state,
                                      CodeBlock* const code,
                                      const RuntimeValue* left, const RuntimeValue* right) {
  const Structure* left_t = comp->types->s_i64;
  const Structure* right_t = comp->types->s_u8;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, left_t, left);
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, right_t, right);

  if (comp->build_options.system == &system_x86_64) {
    auto* res_val       = state->value_tree.values.data + temp_right.reg.val;
    res_val->value_type = ValueType::FIXED;
    res_val->reg        = RCX.REG;

    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_R_I64_BY_R8);

    return temp_left;
  }
  else {
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_R_I64_BY_R8);

    return temp_left;
  }
}

RuntimeValue OP::emit_neg_i64(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* val) {

  const RuntimeValue temp = load_to_mod_op(comp, state, code, comp->types->s_i64, val);

  un_op_impl(comp, state, code, &temp, ByteCode::EMIT::NEG_R64);

  return temp;
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
  state->set_value(ptr_val.reg);

  return ptr_val;
}

RuntimeValue OP::emit_deref(Compiler* const comp,
                            State* const state,
                            CodeBlock* const code,
                            const RuntimeValue* val) {

  RuntimeValue deref_val ={};
  deref_val.type = RVT::MEMORY;

  switch (val->type) {
    case RVT::REGISTER: {
        MemIndex mi = state->new_mem();
        auto* mem = state->get_mem(mi);

        mem->mem.base = (uint8_t)val->reg.val;
        mem->size = 8;

        deref_val.mem = mi;
        break;
      }
    case RVT::MEMORY: {
        ValueIndex r = state->new_value();

        ByteCode::EMIT::COPY_R64_FROM_MEM(code->code, (uint8_t)r.val, state->get_mem(val->mem)->mem);
        state->set_value(r);

        MemIndex mi = state->new_mem();
        auto* mem = state->get_mem(mi);
        mem->mem.base = (uint8_t)r.val;
        mem->size = 8;

        deref_val.mem = mi;
        break;
      }
    case RVT::CONST: {
        ValueIndex r = state->new_value();

        const uint64_t ptr_v = x64_from_bytes(val->constant.ptr);

        ByteCode::EMIT::SET_R64_TO_64(code->code, (uint8_t)r.val, ptr_v);
        state->set_value(r);

        MemIndex mi = state->new_mem();
        auto* mem = state->get_mem(mi);
        mem->mem.base = (uint8_t)r.val;
        mem->size = 8;

        deref_val.mem = mi;
        break;
      }
  }

  return deref_val;
}

const Structure* BIN_OP_TESTS::num_int_64_bit(Compiler* comp, const Structure* left, const Structure* right) {
  const Types* types = comp->types;
  
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

const Structure* BIN_OP_TESTS::num_signed_int_64_bit(Compiler* comp, const Structure* left, const Structure* right) {
  const Types* types = comp->types;

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

const Structure* BIN_OP_TESTS::num_unsigned_int_64_bit(Compiler* comp, const Structure* left, const Structure* right) {
  const Types* types = comp->types;
  
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

const Structure* BIN_OP_TESTS::eq_int_64_bit(Compiler* comp, const Structure* left, const Structure* right) {
  const Types* types = comp->types;
  
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

const Structure* BIN_OP_TESTS::bools(Compiler* comp, const Structure* left, const Structure* right) {
  const Types* types = comp->types;
  
  if (left == right && left == types->s_bool) {
    return types->s_bool;
  }
  else {
    return nullptr;
  }
}

const Structure* BIN_OP_TESTS::shift_64(Compiler* comp, const Structure* left, const Structure* right) {
  const Types* types = comp->types;

  constexpr auto is_valid_left = [](const Types* types, const Structure* s) {
    return s == types->s_u64 || s == types->s_i64 || s == types->s_int_lit || s == types->s_sint_lit;
  };

  constexpr auto is_valid_right = [](const Types* types, const Structure* s) {
    return s == types->s_u8 || s == types->s_int_lit;
  };

  if (!is_valid_left(types, left) || !is_valid_right(types, right)) {
    return nullptr;
  }

  return left;
}

const Structure* BIN_OP_TESTS::s_shift_64(Compiler* comp, const Structure* left, const Structure* right) {
  const Types* types = comp->types;

  constexpr auto is_valid_left = [](const Types* types, const Structure* s) {
    return s == types->s_i64 || s == types->s_sint_lit;
  };

  constexpr auto is_valid_right = [](const Types* types, const Structure* s) {
    return s == types->s_u8 || s == types->s_int_lit;
  };

  if (!is_valid_left(types, left) || !is_valid_right(types, right)) {
    return nullptr;
  }

  return left;
}

const Structure* BIN_OP_TESTS::u_shift_64(Compiler* comp, const Structure* left, const Structure* right) {
  const Types* types = comp->types;

  constexpr auto is_valid_left = [](const Types* types, const Structure* s) {
    return s == types->s_u64 || s == types->s_int_lit;
  };

  constexpr auto is_valid_right = [](const Types* types, const Structure* s) {
    return s == types->s_u8 || s == types->s_int_lit;
  };

  if (!is_valid_left(types, left) || !is_valid_right(types, right)) {
    return nullptr;
  }

  return left;
}

const Structure* UN_OP_TESTS::signed_int_64_bit(Compiler* comp, const Structure* s) {
  const Types* types = comp->types;
  
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

const Structure* UN_OP_TESTS::address(Compiler* comp, const Structure* s) {
  return find_or_make_pointer_type(comp, s);
}

const Structure* UN_OP_TESTS::deref(Compiler* comp, const Structure* s) {
  if (s->type == STRUCTURE_TYPE::POINTER) {
    return static_cast<const PointerStructure*>(s)->base;
  }
  else {
    return nullptr;
  }
}

CompileCode find_binary_operator(Compiler* comp,
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
    expr->type = op->test(comp, left, right);

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

CompileCode find_unary_operator(Compiler* comp,
                                ASTExpression* expr,
                                const UnaryOperation* operations,
                                size_t num_ops) {

  const auto prim = expr->un_op.primary->type;

  assert(prim != nullptr);

  auto op = operations;
  const auto end = operations + num_ops;

  for (; op < end; op++) {
    expr->type = op->test(comp, prim);

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