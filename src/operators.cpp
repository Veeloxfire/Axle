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
  ASSERT(left->type == RVT::REGISTER);
  ASSERT(right->type == RVT::REGISTER);

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
  ASSERT(val->type == RVT::REGISTER);

  func(code->code, (uint8_t)val->reg.val);

  state->get_val(val->reg)->is_modified = true;
  state->use_value(val->reg);
}

RuntimeValue BinOpArgs::emit_add_64s() {
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::ADD_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_sub_64s() {
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SUB_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_sub_ptrs() {
  RuntimeValue res = emit_sub_64s();

  ASSERT(info->main_type.struct_type() == STRUCTURE_TYPE::POINTER);
  const auto* ptr = info->main_type.unchecked_base<PointerStructure>();

  uint64_t size_num = ptr->base.structure->size;
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

  ASSERT(info->main_type.struct_type() == STRUCTURE_TYPE::POINTER);
  const auto* ptr = info->main_type.unchecked_base<PointerStructure>();

  uint64_t size_num = ptr->base.structure->size;
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
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::MUL_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_div_u64s() {
  const Structure* type = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, type, left);
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, type, right);

  if (comp->build_options.endpoint_system == &system_x86_64) {
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
  const Structure* type = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, type, left);
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, type, right);

  if (comp->build_options.endpoint_system == &system_x86_64) {
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
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::EQ_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_eq_8s() {
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::EQ_R8S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_neq_8s() {
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::NEQ_R8S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_lesser_u64s() {
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::LESS_U64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_greater_u64s() {
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::GREAT_U64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_lesser_i64s() {
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::LESS_I64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_greater_i64s() {
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::GREAT_I64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_or_64s() {
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::OR_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_xor_64s() {
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::XOR_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_and_64s() {
  const Structure* ty = comp->services.builtin_types->t_u64.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, ty, left);
  const RuntimeValue temp_right = load_to_const_op(comp, state, code, ty, right);

  bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::AND_R64S);

  return temp_left;
}

RuntimeValue BinOpArgs::emit_shift_l_64_by_8() {
  const Structure* left_t = comp->services.builtin_types->t_u64.structure;
  const Structure* right_t = comp->services.builtin_types->t_u8.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, left_t, left);
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, right_t, right);

  if (comp->build_options.endpoint_system == &system_x86_64) {
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
  const Structure* left_t = comp->services.builtin_types->t_u64.structure;
  const Structure* right_t = comp->services.builtin_types->t_u8.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, left_t, left);
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, right_t, right);

  if (comp->build_options.endpoint_system == &system_x86_64) {
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
  const Structure* left_t = comp->services.builtin_types->t_i64.structure;
  const Structure* right_t = comp->services.builtin_types->t_u8.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, left_t, left);
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, right_t, right);

  if (comp->build_options.endpoint_system == &system_x86_64) {
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

  const RuntimeValue temp = load_to_mod_op(comp, state, code, comp->services.builtin_types->t_i64.structure, prim);

  un_op_impl(comp, state, code, &temp, ByteCode::EMIT::NEG_R64);

  return temp;
}



RuntimeValue UnOpArgs::emit_address() {
  ASSERT(prim->type == RVT::MEMORY);

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
  ASSERT(expr->expr_type == EXPRESSION_TYPE::BINARY_OPERATOR);

  const BuiltinTypes* const types = comp->services.builtin_types;

  //Reset
  expr->bin_op.emit = nullptr;

#define SHOULD_RET comp->is_panic() || expr->bin_op.emit != nullptr

  const auto try_normal_options = [&](ASTExpression* main, ASTExpression* other) {
    expr->bin_op.info.main_type = main->type;

    //Main cannot be liteal
    if (TEST_MASK(main->meta_flags, META_FLAG::LITERAL)) return;

    if (op.u64_emit != nullptr && main->type == types->t_u64) {
      //is a valid unsigned type
      try_emit(main, other, main->type, op.u64_emit);
      if (SHOULD_RET) {
        return;
      }
    }

    if (op.ptrs_emit != nullptr && main->type.struct_type() == STRUCTURE_TYPE::POINTER) {
      //is a valid unsigned type
      try_emit(main, other, main->type, op.ptrs_emit);
      if (expr->bin_op.emit != nullptr) {
        expr->type = types->t_u64;//overide default expected type
        return;
      }
      else if (comp->is_panic()) {
        return;
      }
    }

    if (op.i64_emit != nullptr && main->type == types->t_i64) {
      //is a valid signed type
      try_emit(main, other, main->type, op.i64_emit);
      if (SHOULD_RET) {
        return;
      }
    }

    if (op.u8_emit != nullptr && main->type == types->t_u8) {
      //is a valid unsigned type
      try_emit(main, other, main->type, op.u8_emit);
      if (SHOULD_RET) {
        return;
      }
    }

    if (op.bools_emit != nullptr && main->type == types->t_bool) {
      //is a valid bool type
      try_emit(main, other, main->type, op.bools_emit);
      if (SHOULD_RET) {
        return;
      }
    }

    if (op.ascii_emit != nullptr && main->type == types->t_ascii) {
      //is a valid bool type
      try_emit(main, other, main->type, op.ascii_emit);
      if (SHOULD_RET) {
        return;
      }
    }
  };

  const auto try_literal_options = [&](ASTExpression* main, ASTExpression* other) {
    expr->bin_op.info.main_type = main->type;

    ASSERT(TEST_MASK(main->meta_flags, META_FLAG::LITERAL));
    ASSERT(TEST_MASK(other->meta_flags, META_FLAG::LITERAL));

    if (op.u64_emit != nullptr
        && TYPE_TESTS::is_unsigned_int(main->type)
        && TYPE_TESTS::is_unsigned_int(other->type)
        && main->type.structure->size > other->type.structure->size) {

      try_emit(main, other, main->type, op.u64_emit);
      if (SHOULD_RET) {
        return;
      }
    }

    if (op.i64_emit != nullptr
        && TYPE_TESTS::is_int(main->type)
        && TYPE_TESTS::is_int(other->type)
        && main->type.structure->size > other->type.structure->size) {

      try_emit(main, other, main->type, op.i64_emit);
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

  //Check the literal options
  if (TYPE_TESTS::is_literal(left->meta_flags)
      && TYPE_TESTS::is_literal(right->meta_flags)) {
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
  }

  const char* const op_string = BINARY_OP_STRING::get(expr->bin_op.op);

  comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                     "No binary operator '{}' exists for left type: '{}', and right type: '{}'",
                     op_string, left->type.name, right->type.name);
}

void impl_compile_unpositioned_binary_op(Compiler* comp, Context* context, State* state, ASTExpression* expr, const UnpositionedBinOpOptions& op,
                                         const TypeHint* hint) {
  ASSERT(expr->expr_type == EXPRESSION_TYPE::BINARY_OPERATOR);

  //Reset
  expr->bin_op.emit = nullptr;

  const BuiltinTypes* const types = comp->services.builtin_types;

  ASTExpression* const left = expr->bin_op.left;
  ASTExpression* const right = expr->bin_op.right;

  const auto try_non_positioned_options = [&](ASTExpression* main, ASTExpression* other) {
    expr->bin_op.info.main_type = main->type;

    if (op.r64_and_r64_emit != nullptr && main->type == types->t_u64) {
      if (TYPE_TESTS::check_implicit_cast(other->meta_flags, other->type, main->type)) {
        if (other->type != main->type) {
          TypeHint inner_hint ={};
          inner_hint.tht = THT::EXACT;
          inner_hint.type = main->type;

          compile_type_of_expression(comp, context, state, right, &inner_hint);
          if (comp->is_panic()) {
            return;
          }
        }

        expr->bin_op.emit = op.r64_and_r64_emit;
        expr->type        = main->type;
        return;
      }
    }

    if (op.r64_and_r64_emit != nullptr && main->type == types->t_i64) {
      if (TYPE_TESTS::check_implicit_cast(other->meta_flags, other->type, main->type)) {
        if (other->type != main->type) {
          TypeHint inner_hint ={};
          inner_hint.tht = THT::EXACT;
          inner_hint.type = main->type;

          compile_type_of_expression(comp, context, state, right, &inner_hint);
          if (comp->is_panic()) {
            return;
          }
        }

        expr->bin_op.emit = op.r64_and_r64_emit;
        expr->type        = main->type;
        return;
      }
    }

    if (op.ptr_and_r64_emit != nullptr
        && main->type.struct_type() == STRUCTURE_TYPE::POINTER
        && TYPE_TESTS::is_int(other->type)
        && TEST_MASK(other->meta_flags, META_FLAG::LITERAL)) {
      TypeHint inner_hint ={};
      inner_hint.tht = THT::EXACT;
      inner_hint.type = types->t_u64;

      compile_type_of_expression(comp, context, state, right, &inner_hint);
      if (comp->is_panic()) {
        return;
      }

      expr->bin_op.emit = op.ptr_and_r64_emit;
      expr->type        = main->type;
      return;
    }
  };

  const auto try_literal_non_positioned_options = [&](ASTExpression* main, ASTExpression* other) {
    expr->bin_op.info.main_type = main->type;

    if (op.r64_and_r64_emit != nullptr
        && TYPE_TESTS::is_int(main->type)
        && TYPE_TESTS::is_int(other->type)
        && TEST_MASK(other->meta_flags, META_FLAG::LITERAL)) {
      //const auto* const main_int = main->type.unchecked_base<IntegerStructure>();
      //const auto* const other_int = other->type.unchecked_base<IntegerStructure>();

      TypeHint inner_hint ={};
      inner_hint.tht = THT::EXACT;
      inner_hint.type = types->t_u64;

      compile_type_of_expression(comp, context, state, right, &inner_hint);
      if (comp->is_panic()) {
        return;
      }

      expr->bin_op.emit = op.ptr_and_r64_emit;
      expr->type        = main->type;
      return;
    }
  };

  if (hint != nullptr
      && TEST_MASK(left->meta_flags, META_FLAG::LITERAL)
      && TEST_MASK(right->meta_flags, META_FLAG::LITERAL)) {

    compile_type_of_expression(comp, context, state, left, hint);
    if (comp->is_panic()) {
      return;
    }

    compile_type_of_expression(comp, context, state, right, hint);
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

void impl_compile_unbalanced_binary_op(Compiler* comp, Context* context, State* state, ASTExpression* expr, const UnbalancedBinOpOptions& op) {
  ASSERT(expr->expr_type == EXPRESSION_TYPE::BINARY_OPERATOR);

  const BuiltinTypes* const types = comp->services.builtin_types;

  //Reset
  expr->bin_op.emit = nullptr;

  ASTExpression* const left = expr->bin_op.left;
  ASTExpression* const right = expr->bin_op.right;

  if (op.Lu64_Ru8_emit != nullptr
      && left->type == types->t_u64) {
    if (right->type == types->t_u8) {
      expr->bin_op.emit = op.Lu64_Ru8_emit;
      expr->type        = left->type;
      return;
    }
    else if (TYPE_TESTS::check_implicit_cast(right->meta_flags,
                                             right->type,
                                             types->t_u8)) {
      TypeHint inner_hint ={};
      inner_hint.tht = THT::EXACT;
      inner_hint.type = types->t_u8;

      compile_type_of_expression(comp, context, state, right, &inner_hint);
      if (comp->is_panic()) {
        return;
      }

      expr->bin_op.emit = op.Lu64_Ru8_emit;
      expr->type        = left->type;
      return;
    }
  }

  if (op.Li64_Ru8_emit != nullptr
      && left->type == types->t_i64) {
    if (right->type == types->t_u8) {
      expr->bin_op.emit = op.Lu64_Ru8_emit;
      expr->type        = left->type;
      return;
    }
    else if (right->type == types->t_u8) {
      TypeHint inner_hint ={};
      inner_hint.tht = THT::EXACT;
      inner_hint.type = types->t_u8;

      compile_type_of_expression(comp, context, state, right, &inner_hint);
      if (comp->is_panic()) {
        return;
      }

      expr->bin_op.emit = op.Lu64_Ru8_emit;
      expr->type        = left->type;
      return;
    }
  }

  const char* const op_string = BINARY_OP_STRING::get(expr->bin_op.op);

  comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                     "No binary operator '{}' exists for left type: '{}', and right type: '{}'",
                     op_string, left->type.name, right->type.name);
}

void impl_compile_unary_op(Compiler* comp, Context* context, State* state, ASTExpression* expr, const UnaryOpOptions& op) {
  ASSERT(expr->expr_type == EXPRESSION_TYPE::UNARY_OPERATOR);

  const BuiltinTypes* const types = comp->services.builtin_types;

  ASTExpression* const prim = expr->un_op.expr;

  //Reset
  expr->un_op.emit = nullptr;

  if (op.i64_emit != nullptr) {
    if (prim->type == types->t_i64) {
      expr->un_op.emit = op.i64_emit;
      expr->type       =  types->t_i64;
      return;
    }
  }

  if (TEST_MASK(expr->meta_flags, META_FLAG::LITERAL)) {
    if (op.i64_emit != nullptr) {
      if (TYPE_TESTS::check_implicit_cast(expr->meta_flags, prim->type, types->t_i64)) {
        TypeHint inner_hint ={};
        inner_hint.tht = THT::EXACT;
        inner_hint.type = types->t_i64;

        compile_type_of_expression(comp, context, state, prim, &inner_hint);
        if (comp->is_panic()) {
          return;
        }

        expr->un_op.emit = op.i64_emit;
        expr->type       =  types->t_i64;
        return;
      }
    }
  }

  const char* const op_string = UNARY_OP_STRING::get(expr->un_op.op);

  comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                     "No unary operator '{}' exists for type: '{}'",
                     op_string, prim->type.name);
}

//Overload for unbalanced operators
void compile_binary_operator(Compiler* comp,
                             Context* context,
                             State* state,
                             ASTExpression* expr,
                             const UnpositionedBinOpOptions& op,
                             const TypeHint* hint) {
  impl_compile_unpositioned_binary_op(comp, context, state, expr, op, hint);
}

//Overload for unbalanced operators
void compile_binary_operator(Compiler* comp,
                             Context* context,
                             State* state,
                             ASTExpression* expr,
                             const UnbalancedBinOpOptions& op) {

  impl_compile_unbalanced_binary_op(comp, context, state, expr, op);
}

//Overload for unbalanced operators that dont care about left sign
void compile_binary_operator(Compiler* comp,
                             Context* context,
                             State* state,
                             ASTExpression* expr,
                             const UnbalancedLeftSignAgnBin& op) {

  UnbalancedBinOpOptions normal ={};
  normal.Li64_Ru8_emit = op.Lr64_Ru8_emit;
  normal.Lu64_Ru8_emit = op.Lr64_Ru8_emit;

  impl_compile_unbalanced_binary_op(comp, context, state, expr, normal);
}


void compile_binary_operator(Compiler* comp,
                             Context* context,
                             State* state,
                             ASTExpression* expr,
                             const EqOpBin& op) {

  //returns bool instead of main->type
  const auto try_emit = [&](ASTExpression* main, ASTExpression* other,
                            const Type& expect,
                            BINARY_OPERATOR_FUNCTION emit_op)
  {
    if (TYPE_TESTS::check_implicit_cast(other->meta_flags, other->type, expect)) {
      if (expect != other->type) {
        TypeHint hint ={};
        hint.tht = THT::EXACT;
        hint.type = expect;

        compile_type_of_expression(comp, context, state, other, &hint);
      }

      expr->bin_op.emit = emit_op;
      expr->type        = comp->services.builtin_types->t_bool;
    }
  };

  BalancedBinOpOptions balanced_op ={};
  balanced_op.u64_emit = op.u64_emit;
  balanced_op.i64_emit = op.i64_emit;
  balanced_op.u8_emit  = op.r8_emit;
  balanced_op.bools_emit = op.bools_emit;
  balanced_op.ascii_emit = op.ascii_emit;

  impl_compile_balanced_binary_op(comp, expr, balanced_op, try_emit);
}

void compile_binary_operator(Compiler* comp,
                             Context* context,
                             State* state,
                             ASTExpression* expr,
                             const SignAgnArithBinOp& op,
                             const TypeHint* hint) {

  const auto try_emit = [&](ASTExpression* main, ASTExpression* other,
                            const Type& expect,
                            BINARY_OPERATOR_FUNCTION emit_op)
  {
    if (TYPE_TESTS::check_implicit_cast(other->meta_flags, other->type, expect)) {
      if (expect != other->type) {
        TypeHint hint ={};
        hint.tht = THT::EXACT;
        hint.type = expect;

        compile_type_of_expression(comp, context, state, other, &hint);
      }

      expr->bin_op.emit = emit_op;
      expr->type        = main->type;
    }
  };

  if (TYPE_TESTS::is_literal(expr->bin_op.left->meta_flags) && hint != nullptr) {
    compile_type_of_expression(comp, context, state, expr->bin_op.left, hint);
    if (comp->is_panic()) {
      return;
    }
  }

  if (TYPE_TESTS::is_literal(expr->bin_op.right->meta_flags) && hint != nullptr) {
    compile_type_of_expression(comp, context, state, expr->bin_op.right, hint);
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
                             Context* context,
                             State* state,
                             ASTExpression* expr,
                             const SignedArithBinOp& op,
                             const TypeHint* hint) {
  const auto try_emit = [&](ASTExpression* main, ASTExpression* other,
                            const Type& expect,
                            BINARY_OPERATOR_FUNCTION emit_op)
  {
    if (TYPE_TESTS::check_implicit_cast(other->meta_flags, other->type, expect)) {
      if (expect != other->type) {
        TypeHint hint ={};
        hint.tht = THT::EXACT;
        hint.type = expect;

        compile_type_of_expression(comp, context, state, other, &hint);
      }

      expr->bin_op.emit = emit_op;
      expr->type        = main->type;
    }
  };

  if (TYPE_TESTS::is_literal(expr->bin_op.left->meta_flags) && hint != nullptr) {
    compile_type_of_expression(comp, context, state, expr->bin_op.left, hint);
    if (comp->is_panic()) {
      return;
    }
  }

  if (TYPE_TESTS::is_literal(expr->bin_op.right->meta_flags) && hint != nullptr) {
    compile_type_of_expression(comp, context, state, expr->bin_op.right, hint);
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
                            Context* context,
                            State* state,
                            ASTExpression* expr,
                            const UnaryOpOptions& op) {
  //just to match stuff
  impl_compile_unary_op(comp, context, state, expr, op);
}

//Overload for taking address
void compile_take_address(Compiler* comp,
                          Context* context,
                          State* state,
                          ASTExpression* expr) {
  //Cant actually fail
  ASSERT(expr->expr_type == EXPRESSION_TYPE::UNARY_OPERATOR);

  const Structure* ptr = find_or_make_pointer_structure(comp, context, expr->un_op.expr->type);
  expr->type = to_type(ptr);
  expr->un_op.emit = &UnOpArgs::emit_address;

  //Current cant do these at comptime
  expr->meta_flags &= ~META_FLAG::COMPTIME;

  //Can only load the address of somewhere in memory
  set_runtime_flags(expr->un_op.expr, state, false, (uint8_t)RVT::MEMORY);
}

//Overload for dereferencing
void compile_deref(Compiler* comp,
                   ASTExpression* expr) {
  ASSERT(expr->expr_type == EXPRESSION_TYPE::UNARY_OPERATOR);

  ASTExpression* prim = expr->un_op.expr;

  if (prim->type.struct_type() == STRUCTURE_TYPE::POINTER) {
    const auto* ptr = prim->type.unchecked_base<PointerStructure>();

    expr->un_op.emit = &UnOpArgs::emit_deref;
    expr->type = ptr->base;
  }
  else {
    const char* const op_string = UNARY_OP_STRING::get(expr->un_op.op);

    comp->report_error(ERROR_CODE::TYPE_CHECK_ERROR, expr->span,
                       "No unary operator '{}' exists for type: '{}'",
                       op_string, prim->type.name);
  }
}