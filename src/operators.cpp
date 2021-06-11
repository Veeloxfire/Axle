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
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, type, right);

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
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, type, right);

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

    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_L_BY_R8_R64);

    return temp_left;
  }
  else {
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_L_BY_R8_R64);

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

    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_R_BY_R8_RU64);

    return temp_left;
  }
  else {
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_R_BY_R8_RU64);

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

    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_R_BY_R8_RI64);

    return temp_left;
  }
  else {
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_R_BY_R8_RI64);

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

template<typename L>
void impl_compile_balanced_binary_op(Compiler* comp, ASTExpression* expr, const BalancedBinOpOptions& op, L&& try_emit) {
  assert(expr->expr_type == EXPRESSION_TYPE::BINARY_OPERATOR);

  const Types* const types = comp->types;

  const auto try_normal_options = [&](ASTExpression* main, ASTExpression* other) {
    const Structure* const main_type = main->type;

    if (op.u64_emit != nullptr && main_type == types->s_u64) {
      //is a valid unsigned type
      try_emit(main, other, op.u64_emit);
      if (expr->bin_op.emit != nullptr) {
        return;
      }
    }
    else if (op.i64_emit != nullptr && main_type == types->s_i64) {
      //is a valid signed type
      try_emit(main, other, op.i64_emit);
      if (expr->bin_op.emit != nullptr) {
        return;
      }
    }
    else if (op.bools_emit != nullptr && main_type == types->s_bool) {
      //is a valid bool type
      try_emit(main, other, op.bools_emit);
      if (expr->bin_op.emit != nullptr) {
        return;
      }
    }
  };

  const auto try_literal_options = [&](ASTExpression* main, ASTExpression* other) {
    const Structure* const main_type = main->type;

    if (op.u64_emit != nullptr && main_type == types->s_int_lit) {
      //Left is an unsigned literal type
      try_emit(main, other, op.u64_emit);
      if (expr->bin_op.emit != nullptr) {
        return;
      }
    }
    else if (op.i64_emit != nullptr && main_type == types->s_sint_lit) {
      //Left is a signed literal type
      try_emit(main, other, op.i64_emit);
      if (expr->bin_op.emit != nullptr) {
        return;
      }
    }
  };

  ASTExpression* const left = expr->bin_op.left;
  ASTExpression* const right = expr->bin_op.right;

  try_normal_options(left, right);
  if (expr->bin_op.emit != nullptr) {
    return;
  }

  try_normal_options(right, left);
  if (expr->bin_op.emit != nullptr) {
    return;
  }

  try_literal_options(left, right);
  if (expr->bin_op.emit != nullptr) {
    return;
  }

  try_literal_options(right, left);
  if (expr->bin_op.emit != nullptr) {
    return;
  }

  const char* const op_string = BINARY_OP_STRING::get(expr->bin_op.op);

  comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                     "No binary operator '{}' exists for left type: '{}', and right type: '{}'",
                     op_string, left->type->name, right->type->name);
}

void impl_compile_unbalanced_binary_op(Compiler* comp, ASTExpression* expr, const UnbalancedBinOpOptions& op) {
  assert(expr->expr_type == EXPRESSION_TYPE::BINARY_OPERATOR);

  const Types* const types = comp->types;

  ASTExpression* const left = expr->bin_op.left;
  ASTExpression* const right = expr->bin_op.right;

  if (op.Lu64_Ru8_emit != nullptr && left->type == types->s_u64) {
    compile_implicit_cast(comp, right, types->s_u8);
    expr->bin_op.emit = op.Lu64_Ru8_emit;
    expr->type        = left->type;
    return;
  }
  else if (op.Li64_Ru8_emit != nullptr && left->type == types->s_i64) {
    compile_implicit_cast(comp, right, types->s_u8);
    expr->bin_op.emit = op.Li64_Ru8_emit;
    expr->type        = left->type;
    return;
  }

  const char* const op_string = BINARY_OP_STRING::get(expr->bin_op.op);

  comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                     "No binary operator '{}' exists for left type: '{}', and right type: '{}'",
                     op_string, left->type->name, right->type->name);
}

void impl_compile_unary_op(Compiler* comp, ASTExpression* expr, const UnaryOpOptions& op) {
  assert(expr->expr_type == EXPRESSION_TYPE::UNARY_OPERATOR);

  const Types* const types = comp->types;

  ASTExpression* const prim = expr->un_op.expr;

  if (op.i64_emit != nullptr) {
    if (prim->type == types->s_i64 || prim->type == types->s_sint_lit) {
      expr->un_op.emit = op.i64_emit;
      expr->type        = prim->type;
      return;
    }
    else if (prim->type == types->s_int_lit) {
      //convert to signed type
      expr->un_op.emit = op.i64_emit;
      expr->type       = types->s_sint_lit;
      return;
    }
  }

  const char* const op_string = UNARY_OP_STRING::get(expr->un_op.op);

  comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                     "No unary operator '{}' exists for type: '{}'",
                     op_string, prim->type->name);
}

//Overload for unbalanced operators
void compile_binary_operator(Compiler* comp,
                             ASTExpression* expr,
                             const UnbalancedBinOpOptions& op) {
  //Set up like this to mimic the other types
  impl_compile_unbalanced_binary_op(comp, expr, op);
}

//Overload for unbalanced operators that dont care about left sign
void compile_binary_operator(Compiler* comp,
                             ASTExpression* expr,
                             const UnbalancedLeftSignAgnBin& op) {
  UnbalancedBinOpOptions normal ={};
  normal.Li64_Ru8_emit = op.Lr64_Ru8_emit;
  normal.Lu64_Ru8_emit = op.Lr64_Ru8_emit;

  impl_compile_unbalanced_binary_op(comp, expr, normal);
}


void compile_binary_operator(Compiler* comp,
                             ASTExpression* expr,
                             const EqOpBin& op) {

  //returns bool instead of main->type
  const auto try_emit = [&](ASTExpression* main, ASTExpression* other, BINARY_OPERATOR_FUNCTION emit_op) {
    if (can_implicit_cast(other->type, main->type)) {
      expr->bin_op.emit = emit_op;
      expr->type        = comp->types->s_bool;
    }
    else if (can_comptime_cast(other->type, main->type)) {
      compile_implicit_cast(comp, other, main->type);
      expr->bin_op.emit = emit_op;
      expr->type        = comp->types->s_bool;
    }
  };

  BalancedBinOpOptions balanced_op ={};
  balanced_op.u64_emit = op.r64_emit;
  balanced_op.i64_emit = op.r64_emit;
  balanced_op.bools_emit = op.bools_emit;

  impl_compile_balanced_binary_op(comp, expr, balanced_op, try_emit);
}

void compile_binary_operator(Compiler* comp, ASTExpression* expr, const SignAgnArithBinOp& op) {
  const auto try_emit = [&](ASTExpression* main, ASTExpression* other, BINARY_OPERATOR_FUNCTION emit_op) {
    if (can_implicit_cast(other->type, main->type)) {
      expr->bin_op.emit = emit_op;
      expr->type        = main->type;
    }
    else if (can_comptime_cast(other->type, main->type)) {
      compile_implicit_cast(comp, other, main->type);
      expr->bin_op.emit = emit_op;
      expr->type        = main->type;
    }
  };

  BalancedBinOpOptions balanced_op ={};
  balanced_op.u64_emit = op.r64_emit;
  balanced_op.i64_emit = op.r64_emit;
  balanced_op.bools_emit = op.bools_emit;

  impl_compile_balanced_binary_op(comp, expr, balanced_op, try_emit);
}


void compile_binary_operator(Compiler* comp,
                             ASTExpression* expr,
                             const SignedArithBinOp& op) {

  const auto try_emit = [&](ASTExpression* main, ASTExpression* other, BINARY_OPERATOR_FUNCTION emit_op) {
    if (can_implicit_cast(other->type, main->type)) {
      expr->bin_op.emit = emit_op;
      expr->type        = main->type;
    }
    else if (can_comptime_cast(other->type, main->type)) {
      compile_implicit_cast(comp, other, main->type);
      expr->bin_op.emit = emit_op;
      expr->type        = main->type;
    }
  };

  BalancedBinOpOptions balanced_op ={};
  balanced_op.u64_emit = op.u64_emit;
  balanced_op.i64_emit = op.i64_emit;

  impl_compile_balanced_binary_op(comp, expr, balanced_op, try_emit);
}

//Overload for unary operators
void compile_unary_operator(Compiler* comp,
                            ASTExpression* expr,
                            const UnaryOpOptions& op) {
  //just to match stuff
  impl_compile_unary_op(comp, expr, op);
}

//Overload for taking address
void compile_take_address(Compiler* comp,
                          State* state,
                          ASTExpression* expr) {
  //Cant actually fail
  assert(expr->expr_type == EXPRESSION_TYPE::UNARY_OPERATOR);

  const Structure* base = expr->un_op.expr->type;

  const Structure* ptr = find_or_make_pointer_type(comp, base);
  expr->type = ptr;
  expr->un_op.emit = &OP::emit_address;

  //Can only load the address of somewhere in memory
  set_valid_rvts(expr->un_op.expr, state, (uint8_t)RVT::MEMORY);
}

//Overload for dereferencing
void compile_deref(Compiler* comp,
                   ASTExpression* expr) {
  assert(expr->expr_type == EXPRESSION_TYPE::UNARY_OPERATOR);

  ASTExpression* prim = expr->un_op.expr;

  if (prim->type->type == STRUCTURE_TYPE::POINTER) {
    const PointerStructure* ptr = (const PointerStructure*)prim->type;

    expr->un_op.emit = &OP::emit_deref;
    expr->type = ptr->base;
  }
  else {
    const char* const op_string = UNARY_OP_STRING::get(expr->un_op.op);

    comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                       "No unary operator '{}' exists for type: '{}'",
                       op_string, prim->type->name);
  }
}