#include "operators.h"
#include "compiler.h"
#include "ast.h"

using UN_OP_EMIT = FUNCTION_PTR<void, Array<uint8_t>&, uint8_t>;
using BIN_OP_EMIT = FUNCTION_PTR<void, Array<uint8_t>&, uint8_t, uint8_t>;

template<typename BIN_OP_TYPE>
static Eval::RuntimeValue bin_op_impl(Eval::IrBuilder* const builder ,
                                        const Eval::RuntimeValue& left_in, const Eval::RuntimeValue& right_in,
                                        const Type& dest_type,
                                        IR::Emitter<BIN_OP_TYPE> emit) {
  //TODO: we could do constant folding here! But for now I wont

  IR::V_ARG left = Eval::load_v_arg(builder, left_in);
  IR::V_ARG right = Eval::load_v_arg(builder, right_in);

  IR::Builder* ir = builder->ir;
  IR::ValueIndex to = ir->new_temporary(dest_type);

  BIN_OP_TYPE bin_op = {};
  bin_op.to = IR::v_arg(to, 0, dest_type);
  bin_op.left = left;
  bin_op.right = right;

  emit(ir->current_bytecode(), bin_op);

  return Eval::as_direct(to, dest_type);
}

using UN_CONSTANT_FOLD = Eval::RuntimeValue(*)(CompilerGlobals*, const u8*, const Type&);

template<typename UN_OP_TYPE>
static Eval::RuntimeValue un_op_impl(CompilerGlobals* comp,
                                     Eval::IrBuilder* const builder,
                                     const Eval::RuntimeValue& from_in,
                                     const Type& dest_type,
                                     IR::Emitter<UN_OP_TYPE> emit,
                                     UN_CONSTANT_FOLD constant_fold) {
  if (constant_fold != nullptr && from_in.rvt == Eval::RVT::Constant) {
    return constant_fold(comp, from_in.constant, from_in.type);
  }

  IR::V_ARG from = Eval::load_v_arg(builder, from_in);

  IR::Builder* ir = builder->ir;
  IR::ValueIndex to = ir->new_temporary(dest_type);

  UN_OP_TYPE un_op = {};
  un_op.to = IR::v_arg(to, 0, dest_type);
  un_op.from = from;

  emit(ir->current_bytecode(), un_op);

  return Eval::as_direct(to, dest_type);
}

Eval::RuntimeValue BinOpArgs::emit_add_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Add>(builder, left, right, info->dest_type, IR::Emit::Add);
}

Eval::RuntimeValue BinOpArgs::emit_add_int_to_ptr() {
  Type dest_type = info->dest_type;
  ASSERT(dest_type.struct_type() == STRUCTURE_TYPE::POINTER);

  const auto* ptr = dest_type.unchecked_base<PointerStructure>();

  u64 size_holder = ptr->size;
  Type size_type = comp->builtin_types->t_u64;
  const Eval::RuntimeValue ptr_size = Eval::as_constant((const u8*)&size_holder, size_type);

  Eval::RuntimeValue int_val;
  Eval::RuntimeValue ptr_val;

  if (info->main_side == MainSide::LEFT) {
    ASSERT(dest_type == left.type);
    ASSERT(size_type == right.type);

    int_val = right;
    ptr_val = left;
  }
  else {
    ASSERT(info->main_side == MainSide::RIGHT);
    ASSERT(dest_type == right.type);
    ASSERT(size_type == left.type);

    int_val = left;
    ptr_val = right;
  }


  const Eval::RuntimeValue to_add = bin_op_impl<IR::Types::Mul>(builder, right, ptr_size, size_type, IR::Emit::Mul);

  return bin_op_impl<IR::Types::Add>(builder, ptr_val, to_add, size_type, IR::Emit::Add);
}

Eval::RuntimeValue BinOpArgs::emit_sub_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Sub>(builder, left, right, info->dest_type, IR::Emit::Sub);
}

Eval::RuntimeValue BinOpArgs::emit_sub_ptrs() {
  ASSERT(left.type == right.type);
  ASSERT(info->dest_type == comp->builtin_types->t_u64);

  const auto* ptr = left.type.unchecked_base<PointerStructure>();

  u64 size_holder = ptr->size;
  ASSERT(size_holder > 0);

  Type size_type = comp->builtin_types->t_u64;
  const Eval::RuntimeValue ptr_size = Eval::as_constant((const u8*)&size_holder, size_type);

  const Eval::RuntimeValue scaled = bin_op_impl<IR::Types::Sub>(builder, right, ptr_size, size_type, IR::Emit::Sub);

  return bin_op_impl<IR::Types::Div>(builder, scaled, ptr_size, size_type, IR::Emit::Div);
}

Eval::RuntimeValue BinOpArgs::emit_mul_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Mul>(builder, left, right, info->dest_type, IR::Emit::Mul);
}

Eval::RuntimeValue BinOpArgs::emit_div_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Div>(builder, left, right, info->dest_type, IR::Emit::Div);
}

Eval::RuntimeValue BinOpArgs::emit_mod_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Mod>(builder, left, right, info->dest_type, IR::Emit::Mod);
}

Eval::RuntimeValue BinOpArgs::emit_eq_ints() {
  ASSERT(left.type == right.type);
  ASSERT(info->dest_type == comp->builtin_types->t_bool);

  return bin_op_impl<IR::Types::Eq>(builder, left, right, info->dest_type, IR::Emit::Eq);
}

Eval::RuntimeValue BinOpArgs::emit_neq_ints() {
  ASSERT(left.type == right.type);
  ASSERT(info->dest_type == comp->builtin_types->t_bool);

  return bin_op_impl<IR::Types::Neq>(builder, left, right, info->dest_type, IR::Emit::Neq);
}

Eval::RuntimeValue BinOpArgs::emit_lesser_ints() {
  ASSERT(left.type == right.type);
  ASSERT(info->dest_type == comp->builtin_types->t_bool);

  return bin_op_impl<IR::Types::Less>(builder, left, right, info->dest_type, IR::Emit::Less);
}

Eval::RuntimeValue BinOpArgs::emit_greater_ints() {
  ASSERT(left.type == right.type);
  ASSERT(info->dest_type == comp->builtin_types->t_bool);

  return bin_op_impl<IR::Types::Great>(builder, left, right, info->dest_type, IR::Emit::Great);
}

Eval::RuntimeValue BinOpArgs::emit_or_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Or>(builder, left, right, info->dest_type, IR::Emit::Or);
}

Eval::RuntimeValue BinOpArgs::emit_or_enums() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Or>(builder, left, right, info->dest_type, IR::Emit::Or);
}

Eval::RuntimeValue BinOpArgs::emit_and_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::And>(builder, left, right, info->dest_type, IR::Emit::And);
}

Eval::RuntimeValue BinOpArgs::emit_xor_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Xor>(builder, left, right, info->dest_type, IR::Emit::Xor);
}

#if 0
RuntimeValue BinOpArgs::emit_shift_l_64_by_8() {
  const Structure* left_t = comp->builtin_types->t_u64.structure;
  const Structure* right_t = comp->builtin_types->t_u8.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, left_t, left);
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, right_t, right);

  if (comp->build_options.endpoint_system == &system_x86_64) {
    auto* res_val = state->value_tree.values.data + temp_right.reg.val;
    res_val->value_type = ValueType::FIXED;
    res_val->reg = RCX.REG;

    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_L_BY_R8_R64);

    return temp_left;
  }
  else {
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_L_BY_R8_R64);

    return temp_left;
  }
}

RuntimeValue BinOpArgs::emit_shift_r_u64_by_8() {
  const Structure* left_t = comp->builtin_types->t_u64.structure;
  const Structure* right_t = comp->builtin_types->t_u8.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, left_t, left);
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, right_t, right);

  if (comp->build_options.endpoint_system == &system_x86_64) {
    auto* res_val = state->value_tree.values.data + temp_right.reg.val;
    res_val->value_type = ValueType::FIXED;
    res_val->reg = RCX.REG;

    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_R_BY_R8_RU64);

    return temp_left;
  }
  else {
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_R_BY_R8_RU64);

    return temp_left;
  }
}

RuntimeValue BinOpArgs::emit_shift_r_i64_by_8() {
  const Structure* left_t = comp->builtin_types->t_i64.structure;
  const Structure* right_t = comp->builtin_types->t_u8.structure;

  const RuntimeValue temp_left = load_to_mod_op(comp, state, code, left_t, left);
  const RuntimeValue temp_right = load_to_mod_op(comp, state, code, right_t, right);

  if (comp->build_options.endpoint_system == &system_x86_64) {
    auto* res_val = state->value_tree.values.data + temp_right.reg.val;
    res_val->value_type = ValueType::FIXED;
    res_val->reg = RCX.REG;

    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_R_BY_R8_RI64);

    return temp_left;
  }
  else {
    bin_op_impl(comp, state, code, &temp_left, &temp_right, ByteCode::EMIT::SHIFT_R_BY_R8_RI64);

    return temp_left;
  }
}
#endif

static Eval::RuntimeValue constant_fold_neg(CompilerGlobals* comp, const u8* data, const Type& type) {
  ASSERT(type.is_valid());
  ASSERT(TYPE_TESTS::is_signed_int(type));

  const auto* ptr = type.unchecked_base<IntegerStructure>();

  u8* out = comp->new_constant(type.size());

  switch (ptr->size) {
    case 1: {
        i8 i = data[0];
        *out = -i;
        break;
      }
    case 2: {
        i16 i = x16_from_bytes(data);
        x16_to_bytes(-i, out);
        break;
      }
    case 4: {
        i32 i = x32_from_bytes(data);
        x32_to_bytes(-i, out);
        break;
      }
    case 8: {
        i64 i = x64_from_bytes(data);
        x64_to_bytes(-i, out);
        break;
      }
    default: {
        INVALID_CODE_PATH("Invalid integer size");
        break;
      }
  }

  return Eval::as_constant(out, type);
}

Eval::RuntimeValue UnOpArgs::emit_neg_int() {
  ASSERT(info->dest_type == prim.type);
  ASSERT(TYPE_TESTS::is_signed_int(prim.type));

  return un_op_impl<IR::Types::Neg>(comp, builder, prim, prim.type, IR::Emit::Neg, constant_fold_neg);
}

Eval::RuntimeValue UnOpArgs::emit_address() {
  return Eval::addrof(builder, prim, info->dest_type);
}

Eval::RuntimeValue UnOpArgs::emit_deref_ptr() {
  return Eval::deref(builder, prim, info->dest_type);
}

