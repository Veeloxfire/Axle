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

RuntimeValue BinOpArgs::emit_add_64s() {
  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::ADD_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_sub_64s() {

  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SUB_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_sub_ptrs() {
  RuntimeValue res = emit_sub_64s();

  assert(info->main_type->type == STRUCTURE_TYPE::POINTER);
  const PointerStructure* ptr = (const PointerStructure*)info->main_type;

  uint64_t size_num = ptr->base->size();
  if (size_num > 1) {
    RuntimeValue size = new_const_in_reg(comp, state, code, (uint8_t*)&size_num, 8);

    left = &res;
    right = &size;

    return emit_div_u64s();
  }
  else {
    return res;
  }
}

RuntimeValue BinOpArgs::emit_add_64_to_ptr() {
  const RuntimeValue* save_left = left;
  const RuntimeValue* save_right = right;

  RuntimeValue mult_size ={};

  assert(info->main_type->type == STRUCTURE_TYPE::POINTER);
  const PointerStructure* ptr = (const PointerStructure*)info->main_type;

  uint64_t size_num = ptr->base->size();
  //Multiply up the add if its base is not 1
  if (size_num > 1) {
    RuntimeValue size = new_const_in_reg(comp, state, code, (uint8_t*)&size_num, 8);

    if (info->main_op == MainOp::LEFT) {
      left = save_right;
      right = &size;
      mult_size = emit_mul_64s();

      left = save_left;
      right = &mult_size;
    }
    else {
      left = save_left;
      right = &size;
      mult_size = emit_mul_64s();

      left = save_right;
      right = &mult_size;
    }

  }

  return emit_add_64s();
}

RuntimeValue BinOpArgs::emit_mul_64s() {

  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::MUL_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_div_u64s() {
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

RuntimeValue BinOpArgs::emit_div_i64s() {
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

RuntimeValue BinOpArgs::emit_eq_64s() {

  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::EQ_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_eq_8s() {

  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::EQ_R8S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_neq_8s() {

  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::NEQ_R8S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_lesser_64s() {

  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::LESS_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_greater_64s() {

  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::GREAT_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_or_64s() {

  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::OR_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_and_64s() {
  const Structure* ty = comp->types->s_u64;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::AND_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_shift_l_64_by_8() {
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

RuntimeValue BinOpArgs::emit_shift_r_u64_by_8() {
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

RuntimeValue BinOpArgs::emit_shift_r_i64_by_8() {
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

RuntimeValue UnOpArgs::emit_neg_i64() {

  const RuntimeValue temp = load_to_mod_op(comp, state, code, comp->types->s_i64, prim);

  un_op_impl(comp, state, code, &temp, ByteCode::EMIT::NEG_R64);

  return temp;
}



RuntimeValue UnOpArgs::emit_address() {
  assert(prim->type == RVT::MEMORY);

  RuntimeValue ptr_val ={};
  ptr_val.type = RVT::REGISTER;
  ptr_val.reg = state->new_value();

  ByteCode::EMIT::LOAD_ADDRESS(code->code, (uint8_t)ptr_val.reg.val, state->get_mem(prim->mem)->mem);
  state->set_value(ptr_val.reg);

  return ptr_val;
}

RuntimeValue UnOpArgs::emit_deref() {

  RuntimeValue deref_val ={};
  deref_val.type = RVT::MEMORY;

  switch (prim->type) {
    case RVT::REGISTER: {
        MemIndex mi = state->new_mem();
        auto* mem = state->get_mem(mi);

        mem->mem.base = (uint8_t)prim->reg.val;
        mem->size = 0;

        deref_val.mem = mi;
        break;
      }
    case RVT::MEMORY: {
        ValueIndex r = state->new_value();

        ByteCode::EMIT::COPY_R64_FROM_MEM(code->code, (uint8_t)r.val, state->get_mem(prim->mem)->mem);
        state->set_value(r);

        MemIndex mi = state->new_mem();
        auto* mem = state->get_mem(mi);
        mem->mem.base = (uint8_t)r.val;
        mem->size = 0;

        deref_val.mem = mi;
        break;
      }
    case RVT::CONST: {
        ValueIndex r = state->new_value();

        const uint64_t ptr_v = x64_from_bytes(prim->constant.ptr);

        ByteCode::EMIT::SET_R64_TO_64(code->code, (uint8_t)r.val, ptr_v);
        state->set_value(r);

        MemIndex mi = state->new_mem();
        auto* mem = state->get_mem(mi);
        mem->mem.base = (uint8_t)r.val;
        mem->size = 0;

        deref_val.mem = mi;
        break;
      }
  }

  return deref_val;
}

template<typename L>
void impl_compile_balanced_binary_op(Compiler* comp,
                                     ASTExpression* expr,
                                     const BalancedBinOpOptions& op,
                                     L&& try_emit) {
  assert(expr->expr_type == EXPRESSION_TYPE::BINARY_OPERATOR);

  const Types* const types = comp->types;

  //Reset
  expr->bin_op.emit = nullptr;

#define SHOULD_RET comp->is_panic() || expr->bin_op.emit != nullptr

  const auto try_normal_options = [&](ASTExpression* main, ASTExpression* other) {
    const Structure* const main_type = main->type;
    expr->bin_op.info.main_type = main_type;

    if (op.u64_emit != nullptr && main_type == types->s_u64) {
      //is a valid unsigned type
      try_emit(main, other, main_type, op.u64_emit);
      if (SHOULD_RET) {
        return;
      }
    }

    if (op.ptrs_emit != nullptr && main_type->type == STRUCTURE_TYPE::POINTER) {
      //is a valid unsigned type
      try_emit(main, other, main_type, op.ptrs_emit);
      if (expr->bin_op.emit != nullptr) {
        expr->type = types->s_u64;//overide default expected type
        return;
      }
      else if (comp->is_panic()) {
        return;
      }
    }

    if (op.i64_emit != nullptr && main_type == types->s_i64) {
      //is a valid signed type
      try_emit(main, other, main_type, op.i64_emit);
      if (SHOULD_RET) {
        return;
      }
    }

    if (op.u8_emit != nullptr && main_type == types->s_u8) {
      //is a valid unsigned type
      try_emit(main, other, main_type, op.u8_emit);
      if (SHOULD_RET) {
        return;
      }
    }

    if (op.bools_emit != nullptr && main_type == types->s_bool) {
      //is a valid bool type
      try_emit(main, other, main_type, op.bools_emit);
      if (SHOULD_RET) {
        return;
      }
    }

    if (op.ascii_emit != nullptr && main_type == types->s_ascii) {
      //is a valid bool type
      try_emit(main, other, main_type, op.ascii_emit);
      if (SHOULD_RET) {
        return;
      }
    }
  };

  const auto try_literal_options = [&](ASTExpression* main, ASTExpression* other) {
    const Structure* const main_type = main->type;
    expr->bin_op.info.main_type = main_type;

    if (op.u64_emit != nullptr && main_type == types->s_int_lit) {
      //Left is an unsigned literal type
      try_emit(main, other, types->s_int_lit, op.u64_emit);
      if (SHOULD_RET) {
        return;
      }
    }

    if (op.i64_emit != nullptr && main_type == types->s_sint_lit) {
      //Left is a signed literal type
      try_emit(main, other, types->s_sint_lit, op.i64_emit);
      if (SHOULD_RET) {
        return;
      }
    }
  };

  ASTExpression* const left = expr->bin_op.left;
  ASTExpression* const right = expr->bin_op.right;

  expr->bin_op.info.main_op = MainOp::LEFT;
  try_normal_options(left, right);
  if (SHOULD_RET) {
    return;
  }

  expr->bin_op.info.main_op = MainOp::RIGHT;
  try_normal_options(right, left);
  if (SHOULD_RET) {
    return;
  }

  expr->bin_op.info.main_op = MainOp::LEFT;
  try_literal_options(left, right);
  if (SHOULD_RET) {
    return;
  }

  expr->bin_op.info.main_op = MainOp::RIGHT;
  try_literal_options(right, left);
  if (SHOULD_RET) {
    return;
  }

  const char* const op_string = BINARY_OP_STRING::get(expr->bin_op.op);

  comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                     "No binary operator '{}' exists for left type: '{}', and right type: '{}'",
                     op_string, left->type->name, right->type->name);
}

void impl_compile_unpositioned_binary_op(Compiler* comp, State* state, ASTExpression* expr, const UnpositionedBinOpOptions& op,
                                         const TypeHint* hint) {
  assert(expr->expr_type == EXPRESSION_TYPE::BINARY_OPERATOR);

  //Reset
  expr->bin_op.emit = nullptr;

  const Types* const types = comp->types;

  ASTExpression* const left = expr->bin_op.left;
  ASTExpression* const right = expr->bin_op.right;

  const auto try_non_positioned_options = [&](ASTExpression* main, ASTExpression* other) {
    const Structure* const main_type = main->type;
    const Structure* const other_type = other->type;
    expr->bin_op.info.main_type = main_type;

    if (op.r64_and_r64_emit != nullptr && main_type == types->s_u64) {
      if (can_comptime_cast(other_type, main_type)) {
        if (other_type != main_type) {
          TypeHint inner_hint ={};
          inner_hint.tht = THT::EXACT;
          inner_hint.type = main_type;

          compile_type_of_expression(comp, state, right, &inner_hint);
          if (comp->is_panic()) {
            return;
          }
        }

        expr->bin_op.emit = op.r64_and_r64_emit;
        expr->type        = main_type;
        return;
      }
    }

    if (op.r64_and_r64_emit != nullptr && main_type == types->s_i64) {
      if (can_comptime_cast(other_type, main_type)) {
        if (other_type != main_type) {
          TypeHint inner_hint ={};
          inner_hint.tht = THT::EXACT;
          inner_hint.type = main_type;

          compile_type_of_expression(comp, state, right, &inner_hint);
          if (comp->is_panic()) {
            return;
          }
        }

        expr->bin_op.emit = op.r64_and_r64_emit;
        expr->type        = main_type;
        return;
      }
    }

    if (op.ptr_and_r64_emit != nullptr && main_type->type == STRUCTURE_TYPE::POINTER
        && TYPE_TESTS::is_int(other_type)) {
      if (other->type == types->s_int_lit) {
        TypeHint inner_hint ={};
        inner_hint.tht = THT::EXACT;
        inner_hint.type = types->s_u64;

        compile_type_of_expression(comp, state, right, &inner_hint);
        if (comp->is_panic()) {
          return;
        }
      }
      else if (other->type == types->s_sint_lit) {
        TypeHint inner_hint ={};
        inner_hint.tht = THT::EXACT;
        inner_hint.type = types->s_i64;

        compile_type_of_expression(comp, state, right, &inner_hint);
        if (comp->is_panic()) {
          return;
        }
      }

      expr->bin_op.emit = op.ptr_and_r64_emit;
      expr->type        = main_type;
      return;
    }
  };

  const auto try_literal_non_positioned_options = [&](ASTExpression* main, ASTExpression* other) {
    const Structure* const main_type = main->type;
    const Structure* const other_type = other->type;
    expr->bin_op.info.main_type = main_type;

    if (op.r64_and_r64_emit != nullptr && main_type == types->s_int_lit) {
      if (can_comptime_cast(other_type, main_type)) {
        if (other_type != main_type) {
          TypeHint inner_hint ={};
          inner_hint.tht = THT::EXACT;
          inner_hint.type = main_type;

          compile_type_of_expression(comp, state, right, &inner_hint);
          if (comp->is_panic()) {
            return;
          }
        }

        expr->bin_op.emit = op.r64_and_r64_emit;
        expr->type        = main_type;
        return;
      }
    }

    if (op.r64_and_r64_emit != nullptr && main_type == types->s_sint_lit) {
      if (can_comptime_cast(other_type, main_type)) {
        if (other_type != main_type) {
          TypeHint inner_hint ={};
          inner_hint.tht = THT::EXACT;
          inner_hint.type = main_type;

          compile_type_of_expression(comp, state, right, &inner_hint);
          if (comp->is_panic()) {
            return;
          }
        }

        expr->bin_op.emit = op.r64_and_r64_emit;
        expr->type        = main_type;
        return;
      }
    }
  };

  if (hint != nullptr && left->type->type == STRUCTURE_TYPE::SIMPLE_LITERAL
      && right->type->type == STRUCTURE_TYPE::SIMPLE_LITERAL) {

    compile_type_of_expression(comp, state, left, hint);
    if (comp->is_panic()) {
      return;
    }

    compile_type_of_expression(comp, state, right, hint);
    if (comp->is_panic()) {
      return;
    }
  }

  expr->bin_op.info.main_op = MainOp::LEFT;
  try_non_positioned_options(left, right);
  if (comp->is_panic() || expr->bin_op.emit != nullptr) {
    return;
  }

  expr->bin_op.info.main_op = MainOp::RIGHT;
  try_non_positioned_options(right, left);
  if (comp->is_panic() || expr->bin_op.emit != nullptr) {
    return;
  }

  expr->bin_op.info.main_op = MainOp::LEFT;
  try_literal_non_positioned_options(left, right);
  if (comp->is_panic() || expr->bin_op.emit != nullptr) {
    return;
  }

  expr->bin_op.info.main_op = MainOp::RIGHT;
  try_literal_non_positioned_options(right, left);
  if (comp->is_panic() || expr->bin_op.emit != nullptr) {
    return;
  }
}

void impl_compile_unbalanced_binary_op(Compiler* comp, State* state, ASTExpression* expr, const UnbalancedBinOpOptions& op) {
  assert(expr->expr_type == EXPRESSION_TYPE::BINARY_OPERATOR);

  const Types* const types = comp->types;

  //Reset
  expr->bin_op.emit = nullptr;

  ASTExpression* const left = expr->bin_op.left;
  ASTExpression* const right = expr->bin_op.right;

  if (op.Lu64_Ru8_emit != nullptr && left->type == types->s_u64
      && (right->type == types->s_u8 || right->type == types->s_int_lit)) {

    if (right->type == types->s_int_lit) {
      TypeHint inner_hint ={};
      inner_hint.tht = THT::EXACT;
      inner_hint.type = types->s_u8;

      compile_type_of_expression(comp, state, right, &inner_hint);
      if (comp->is_panic()) {
        return;
      }
    }

    expr->bin_op.emit = op.Lu64_Ru8_emit;
    expr->type        = left->type;
    return;
  }

  if (op.Li64_Ru8_emit != nullptr && left->type == types->s_i64
      && (right->type == types->s_u8 || right->type == types->s_int_lit)) {

    if (right->type == types->s_int_lit) {
      TypeHint inner_hint ={};
      inner_hint.tht = THT::EXACT;
      inner_hint.type = types->s_u8;

      compile_type_of_expression(comp, state, right, &inner_hint);
      if (comp->is_panic()) {
        return;
      }
    }

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

  //Reset
  expr->un_op.emit = nullptr;

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
                             State* state,
                             ASTExpression* expr,
                             const UnpositionedBinOpOptions& op,
                             const TypeHint* hint) {
  impl_compile_unpositioned_binary_op(comp, state, expr, op, hint);
}

//Overload for unbalanced operators
void compile_binary_operator(Compiler* comp,
                             State* state,
                             ASTExpression* expr,
                             const UnbalancedBinOpOptions& op) {

  impl_compile_unbalanced_binary_op(comp, state, expr, op);
}

//Overload for unbalanced operators that dont care about left sign
void compile_binary_operator(Compiler* comp,
                             State* state,
                             ASTExpression* expr,
                             const UnbalancedLeftSignAgnBin& op) {

  UnbalancedBinOpOptions normal ={};
  normal.Li64_Ru8_emit = op.Lr64_Ru8_emit;
  normal.Lu64_Ru8_emit = op.Lr64_Ru8_emit;

  impl_compile_unbalanced_binary_op(comp, state, expr, normal);
}


void compile_binary_operator(Compiler* comp,
                             State* state,
                             ASTExpression* expr,
                             const EqOpBin& op) {

  //returns bool instead of main->type
  const auto try_emit = [&](ASTExpression* main, ASTExpression* other, const Structure* expect,
                            BINARY_OPERATOR_FUNCTION emit_op) {
    if (can_comptime_cast(other->type, expect)) {
      if (expect != other->type) {
        TypeHint hint ={};
        hint.tht = THT::EXACT;
        hint.type = expect;

        compile_type_of_expression(comp, state, other, &hint);
      }

      expr->bin_op.emit = emit_op;
      expr->type        = comp->types->s_bool;
    }
  };

  BalancedBinOpOptions balanced_op ={};
  balanced_op.u64_emit = op.r64_emit;
  balanced_op.i64_emit = op.r64_emit;
  balanced_op.u8_emit  = op.r8_emit;
  balanced_op.bools_emit = op.bools_emit;
  balanced_op.ascii_emit = op.ascii_emit;

  impl_compile_balanced_binary_op(comp, expr, balanced_op, try_emit);
}

void compile_binary_operator(Compiler* comp,
                             State* state,
                             ASTExpression* expr,
                             const SignAgnArithBinOp& op,
                             const TypeHint* hint) {

  const auto try_emit = [&](ASTExpression* main, ASTExpression* other, const Structure* expect,
                            BINARY_OPERATOR_FUNCTION emit_op) {
    if (can_comptime_cast(other->type, expect)) {
      if (expect != other->type) {

        TypeHint hint ={};
        hint.tht = THT::EXACT;
        hint.type = expect;

        compile_type_of_expression(comp, state, other, &hint);
      }

      expr->bin_op.emit = emit_op;
      expr->type        = main->type;
    }
  };

  if (TYPE_TESTS::is_literal(expr->bin_op.left->type) && hint != nullptr) {
    compile_type_of_expression(comp, state, expr->bin_op.left, hint);
    if (comp->is_panic()) {
      return;
    }
  }

  if (TYPE_TESTS::is_literal(expr->bin_op.right->type) && hint != nullptr) {
    compile_type_of_expression(comp, state, expr->bin_op.right, hint);
    if (comp->is_panic()) {
      return;
    }
  }

  BalancedBinOpOptions balanced_op ={};
  balanced_op.ptrs_emit = op.ptrs_emit;
  balanced_op.u64_emit = op.r64_emit;
  balanced_op.i64_emit = op.r64_emit;
  balanced_op.bools_emit = op.bools_emit;

  impl_compile_balanced_binary_op(comp, expr, balanced_op, try_emit);
}


void compile_binary_operator(Compiler* comp,
                             State* state,
                             ASTExpression* expr,
                             const SignedArithBinOp& op,
                             const TypeHint* hint) {
  const auto try_emit = [&](ASTExpression* main, ASTExpression* other, const Structure* expect,
                            BINARY_OPERATOR_FUNCTION emit_op) {
    if (can_comptime_cast(other->type, expect)) {
      if (expect != other->type) {
        TypeHint hint ={};
        hint.tht = THT::EXACT;
        hint.type = expect;

        compile_type_of_expression(comp, state, other, &hint);
      }

      expr->bin_op.emit = emit_op;
      expr->type        = main->type;
    }
  };

  if (TYPE_TESTS::is_literal(expr->bin_op.left->type) && hint != nullptr) {
    compile_type_of_expression(comp, state, expr->bin_op.left, hint);
    if (comp->is_panic()) {
      return;
    }
  }

  if (TYPE_TESTS::is_literal(expr->bin_op.right->type) && hint != nullptr) {
    compile_type_of_expression(comp, state, expr->bin_op.right, hint);
    if (comp->is_panic()) {
      return;
    }
  }

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

  const Structure* ptr = find_or_make_pointer_type(comp, expr->span, base);
  expr->type = ptr;
  expr->un_op.emit = &UnOpArgs::emit_address;

  //Current cant do these at comptime
  expr->comptime_eval = false;

  //Can only load the address of somewhere in memory
  set_runtime_flags(expr->un_op.expr, state, false, (uint8_t)RVT::MEMORY);
}

//Overload for dereferencing
void compile_deref(Compiler* comp,
                   ASTExpression* expr) {
  assert(expr->expr_type == EXPRESSION_TYPE::UNARY_OPERATOR);

  ASTExpression* prim = expr->un_op.expr;

  if (prim->type->type == STRUCTURE_TYPE::POINTER) {
    const PointerStructure* ptr = (const PointerStructure*)prim->type;

    expr->un_op.emit = &UnOpArgs::emit_deref;
    expr->type = ptr->base;
  }
  else {
    const char* const op_string = UNARY_OP_STRING::get(expr->un_op.op);

    comp->report_error(CompileCode::TYPE_CHECK_ERROR, expr->span,
                       "No unary operator '{}' exists for type: '{}'",
                       op_string, prim->type->name);
  }
}