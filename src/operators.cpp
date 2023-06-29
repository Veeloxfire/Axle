#include "operators.h"
#include "compiler.h"
#include "ast.h"

using UN_OP_EMIT = FUNCTION_PTR<void, Array<uint8_t>&, uint8_t>;
using BIN_OP_EMIT = FUNCTION_PTR<void, Array<uint8_t>&, uint8_t, uint8_t>;

template<typename BIN_OP_TYPE>
static IR::RuntimeReference bin_op_impl(IR::Builder* const ir,
                                        const IR::RuntimeReference& left_in, const IR::RuntimeReference& right_in,
                                        const Type& dest_type,
                                        IR::Emitter<BIN_OP_TYPE> emit) {
  //TODO: we could do constant folding here! But for now I wont

  IR::RuntimeReference left;
  if (left_in.is_constant) {
    left = IR::HELPERS::copy_constant(ir, left_in.constant + left_in.offset, left_in.type);
  }
  else {
    left = left_in;
  }

  IR::RuntimeReference right;
  if (right_in.is_constant) {
    right = IR::HELPERS::copy_constant(ir, right_in.constant + right_in.offset, right_in.type);
  }
  else {
    right = right_in;
  }

  IR::RuntimeReference to = ir->new_temporary(dest_type);

  BIN_OP_TYPE bin_op = {};
  bin_op.to = to.base;
  bin_op.t_offset = to.offset;
  bin_op.t_format = to.type.struct_format();
  bin_op.left = left.base;
  bin_op.l_offset = left.offset;
  bin_op.l_format = left.type.struct_format();
  bin_op.right = right.base;
  bin_op.r_offset = right.offset;
  bin_op.r_format = right.type.struct_format();

  emit(ir->ir_bytecode, bin_op);

  return to;
}

template<typename T>
using CONSTANT_FOLD = IR::RuntimeReference(*)(CompilerGlobals*, const u8*, const Type&);

template<typename UN_OP_TYPE>
static IR::RuntimeReference un_op_impl(CompilerGlobals* comp, 
                                       IR::Builder* const ir,
                                       const IR::RuntimeReference& from_in,
                                       const Type& dest_type,
                                       IR::Emitter<UN_OP_TYPE> emit,
                                       CONSTANT_FOLD<UN_OP_TYPE> constant_fold) {
  IR::RuntimeReference from;
  if (from_in.is_constant) {
    if (constant_fold != nullptr) {
      return constant_fold(comp, from_in.constant, from_in.type);
    
    }

    from = IR::HELPERS::copy_constant(ir, from_in.constant + from_in.offset, from_in.type);
  }
  else {
    from = from_in;
  }

  IR::RuntimeReference to = ir->new_temporary(dest_type);

  UN_OP_TYPE un_op = {};
  un_op.to = to.base;
  un_op.t_offset = to.offset;
  un_op.t_format = to.type.struct_format();
  un_op.from = from.base;
  un_op.f_offset = from.offset;
  un_op.f_format = from.type.struct_format();

  emit(ir->ir_bytecode, un_op);

  return to;
}

IR::RuntimeReference BinOpArgs::emit_add_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Add>(ir, left, right, info->dest_type, IR::Emit::Add);
}

IR::RuntimeReference BinOpArgs::emit_add_int_to_ptr() {
  Type dest_type = info->dest_type;
  ASSERT(dest_type.struct_type() == STRUCTURE_TYPE::POINTER);

  const auto* ptr = dest_type.unchecked_base<PointerStructure>();

  u64 size_holder = ptr->size;
  Type size_type = comp->builtin_types->t_u64;
  const IR::RuntimeReference ptr_size = IR::HELPERS::as_constant(&size_holder, size_type);

  IR::RuntimeReference int_val;
  IR::RuntimeReference ptr_val;

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


  const IR::RuntimeReference to_add = bin_op_impl<IR::Types::Mul>(ir, right, ptr_size, size_type, IR::Emit::Mul);

  return bin_op_impl<IR::Types::Add>(ir, ptr_val, to_add, size_type, IR::Emit::Add);
}

IR::RuntimeReference BinOpArgs::emit_sub_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Sub>(ir, left, right, info->dest_type, IR::Emit::Sub);
}

IR::RuntimeReference BinOpArgs::emit_sub_ptrs() {
  ASSERT(left.type == right.type);
  ASSERT(info->dest_type == comp->builtin_types->t_u64);

  const auto* ptr = left.type.unchecked_base<PointerStructure>();

  u64 size_holder = ptr->size;
  ASSERT(size_holder > 0);

  Type size_type = comp->builtin_types->t_u64;
  const IR::RuntimeReference ptr_size = IR::HELPERS::as_constant(&size_holder, size_type);

  const IR::RuntimeReference scaled = bin_op_impl<IR::Types::Sub>(ir, right, ptr_size, size_type, IR::Emit::Sub);

  return bin_op_impl<IR::Types::Div>(ir, scaled, ptr_size, size_type, IR::Emit::Div);
}

IR::RuntimeReference BinOpArgs::emit_mul_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Mul>(ir, left, right, info->dest_type, IR::Emit::Mul);
}

IR::RuntimeReference BinOpArgs::emit_div_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Div>(ir, left, right, info->dest_type, IR::Emit::Div);
}

IR::RuntimeReference BinOpArgs::emit_mod_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Mod>(ir, left, right, info->dest_type, IR::Emit::Mod);
}

IR::RuntimeReference BinOpArgs::emit_eq_ints() {
  ASSERT(left.type == right.type);
  ASSERT(info->dest_type == comp->builtin_types->t_bool);

  return bin_op_impl<IR::Types::Eq>(ir, left, right, info->dest_type, IR::Emit::Eq);
}

IR::RuntimeReference BinOpArgs::emit_neq_ints() {
  ASSERT(left.type == right.type);
  ASSERT(info->dest_type == comp->builtin_types->t_bool);

  return bin_op_impl<IR::Types::Neq>(ir, left, right, info->dest_type, IR::Emit::Neq);
}

IR::RuntimeReference BinOpArgs::emit_lesser_ints() {
  ASSERT(left.type == right.type);
  ASSERT(info->dest_type == comp->builtin_types->t_bool);

  return bin_op_impl<IR::Types::Less>(ir, left, right, info->dest_type, IR::Emit::Less);
}

IR::RuntimeReference BinOpArgs::emit_greater_ints() {
  ASSERT(left.type == right.type);
  ASSERT(info->dest_type == comp->builtin_types->t_bool);

  return bin_op_impl<IR::Types::Great>(ir, left, right, info->dest_type, IR::Emit::Great);
}

IR::RuntimeReference BinOpArgs::emit_or_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Or>(ir, left, right, info->dest_type, IR::Emit::Or);
}

IR::RuntimeReference BinOpArgs::emit_and_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::And>(ir, left, right, info->dest_type, IR::Emit::And);
}

IR::RuntimeReference BinOpArgs::emit_xor_ints() {
  ASSERT(left.type == right.type);
  ASSERT(left.type == info->dest_type);

  return bin_op_impl<IR::Types::Xor>(ir, left, right, info->dest_type, IR::Emit::Xor);
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

static IR::RuntimeReference constant_fold_neg(CompilerGlobals* comp, const u8* data, const Type& type) {
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

  return IR::HELPERS::as_constant(out, type);
}

IR::RuntimeReference UnOpArgs::emit_neg_int() {
  ASSERT(info->dest_type == prim.type);
  ASSERT(TYPE_TESTS::is_signed_int(prim.type));

  return un_op_impl<IR::Types::Neg>(comp, ir, prim, prim.type, IR::Emit::Neg, constant_fold_neg);
}

IR::RuntimeReference UnOpArgs::emit_address() {
  return IR::HELPERS::take_address(ir, prim, info->dest_type);
}

IR::RuntimeReference UnOpArgs::emit_deref_ptr() {
  return IR::HELPERS::dereference(ir, prim, info->dest_type);
}

