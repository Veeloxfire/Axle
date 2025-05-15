#include "operators.h"
#include "compiler.h"
#include "ast.h"

using UN_OP_EMIT = Axle::FUNCTION_PTR<void, Axle::Array<uint8_t>&, uint8_t>;
using BIN_OP_EMIT = Axle::FUNCTION_PTR<void, Axle::Array<uint8_t>&, uint8_t, uint8_t>;

template<typename BIN_OP_TYPE>
static Eval::RuntimeValue bin_op_impl(IR::IRStore* const ir,
                                      const Eval::RuntimeValue& left_in, const Eval::RuntimeValue& right_in,
                                      const Type& dest_type,
                                      IR::Emitter<BIN_OP_TYPE> emit) {
  AXLE_TELEMETRY_FUNCTION();
  //TODO: we could do constant folding here! But for now I wont

  IR::V_ARG left = Eval::load_v_arg(ir, left_in);
  IR::V_ARG right = Eval::load_v_arg(ir, right_in);

  IR::ValueIndex to = ir->new_temporary(dest_type, {});

  BIN_OP_TYPE bin_op = {};
  bin_op.to = IR::v_arg(to, 0, dest_type);
  bin_op.left = left;
  bin_op.right = right;

  emit(ir->current_bytecode(), bin_op);

  return Eval::as_direct(to, dest_type);
}

using UN_CONSTANT_FOLD = Eval::RuntimeValue(*)(CompilerGlobals*, const Eval::ConstantValue&);

template<typename UN_OP_TYPE>
static Eval::RuntimeValue un_op_impl(CompilerGlobals* comp,
                                     IR::IRStore* const ir,
                                     const Eval::RuntimeValue& from_in,
                                     const Type& dest_type,
                                     IR::Emitter<UN_OP_TYPE> emit,
                                     UN_CONSTANT_FOLD constant_fold) {
  AXLE_TELEMETRY_FUNCTION();
  
  if (constant_fold != nullptr && from_in.rvt == Eval::RVT::Constant) {
    return constant_fold(comp, from_in.constant);
  }

  IR::V_ARG from = Eval::load_v_arg(ir, from_in);

  IR::ValueIndex to = ir->new_temporary(dest_type, {});

  UN_OP_TYPE un_op = {};
  un_op.to = IR::v_arg(to, 0, dest_type);
  un_op.from = from;

  emit(ir->current_bytecode(), un_op);

  return Eval::as_direct(to, dest_type);
}

Eval::RuntimeValue BinOpArgs::emit_add_ints() {
  ASSERT(info.op_full == BinOpFull::add_ints);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(lt == info.dest_type);

  return bin_op_impl<IR::Types::Add>(ir, left, right, info.dest_type, IR::Emit::Add);
}

Eval::RuntimeValue BinOpArgs::emit_add_int_to_ptr() {
  ASSERT(info.op_full == BinOpFull::add_int_to_ptr);
  
  Type ptr_type = info.dest_type;
  ASSERT(ptr_type.struct_type() == STRUCTURE_TYPE::POINTER);

  const Type lt = left.effective_type();
  const Type rt = right.effective_type();


  Eval::RuntimeValue int_val;
  Eval::RuntimeValue ptr_val;
  const Type size_type = comp->builtin_types->t_u64;

  if (info.main_side == MainSide::LEFT) {
    ASSERT(ptr_type == lt);
    ASSERT(size_type == rt);

    int_val = right;
    ptr_val = left;
  }
  else {
    ASSERT(info.main_side == MainSide::RIGHT);
    ASSERT(ptr_type == rt);
    ASSERT(size_type == lt);

    int_val = left;
    ptr_val = right;
  }

  const auto* ptr = ptr_type.unchecked_base<PointerStructure>();
  const u64 size_holder = ptr->base.size();
  ASSERT(size_holder != 0);

  if (size_holder > 1) {
    const Eval::RuntimeValue ptr_size = Eval::as_constant((const u8*)&size_holder, size_type);

    const Eval::RuntimeValue to_add = bin_op_impl<IR::Types::Mul>(ir, right, ptr_size, size_type, IR::Emit::Mul);

    return bin_op_impl<IR::Types::Add>(ir, ptr_val, to_add, ptr_type, IR::Emit::Add);
  }
  else {
    return bin_op_impl<IR::Types::Add>(ir, ptr_val, int_val, ptr_type, IR::Emit::Add);
  }
}

Eval::RuntimeValue BinOpArgs::emit_sub_ints() {
  ASSERT(info.op_full == BinOpFull::sub_ints);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(lt == info.dest_type);

  return bin_op_impl<IR::Types::Sub>(ir, left, right, info.dest_type, IR::Emit::Sub);
}

Eval::RuntimeValue BinOpArgs::emit_sub_ptrs() {
  ASSERT(info.op_full == BinOpFull::sub_ptrs);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(lt == info.dest_type);
  ASSERT(info.dest_type == comp->builtin_types->t_u64);
  Type size_type = comp->builtin_types->t_u64;

  const auto* ptr = lt.unchecked_base<PointerStructure>();

  const Eval::RuntimeValue scaled = bin_op_impl<IR::Types::Sub>(ir, left, right, size_type, IR::Emit::Sub);

  const u64 size_holder = ptr->base.size();
  ASSERT(size_holder > 0);
  if (size_holder > 1) {
    const Eval::RuntimeValue ptr_size = Eval::as_constant((const u8*)&size_holder, size_type);
    return bin_op_impl<IR::Types::Div>(ir, scaled, ptr_size, size_type, IR::Emit::Div);
  }
  else {
    return scaled;
  }
}

Eval::RuntimeValue BinOpArgs::emit_mul_ints() {
  ASSERT(info.op_full == BinOpFull::mul_ints);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(lt == info.dest_type);

  return bin_op_impl<IR::Types::Mul>(ir, left, right, info.dest_type, IR::Emit::Mul);
}

Eval::RuntimeValue BinOpArgs::emit_div_ints() {
  ASSERT(info.op_full == BinOpFull::div_ints);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(lt == info.dest_type);

  return bin_op_impl<IR::Types::Div>(ir, left, right, info.dest_type, IR::Emit::Div);
}

Eval::RuntimeValue BinOpArgs::emit_mod_ints() {
  ASSERT(info.op_full == BinOpFull::mod_ints);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(lt == info.dest_type);

  return bin_op_impl<IR::Types::Mod>(ir, left, right, info.dest_type, IR::Emit::Mod);
}

Eval::RuntimeValue BinOpArgs::emit_eq_ints() {
  ASSERT(info.op_full == BinOpFull::eq_ints);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(info.dest_type == comp->builtin_types->t_bool);

  return bin_op_impl<IR::Types::Eq>(ir, left, right, info.dest_type, IR::Emit::Eq);
}

Eval::RuntimeValue BinOpArgs::emit_neq_ints() {
  ASSERT(info.op_full == BinOpFull::neq_ints);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(info.dest_type == comp->builtin_types->t_bool);

  return bin_op_impl<IR::Types::Neq>(ir, left, right, info.dest_type, IR::Emit::Neq);
}

Eval::RuntimeValue BinOpArgs::emit_lesser_ints() {
  ASSERT(info.op_full == BinOpFull::lesser_ints);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(info.dest_type == comp->builtin_types->t_bool);

  return bin_op_impl<IR::Types::Less>(ir, left, right, info.dest_type, IR::Emit::Less);
}

Eval::RuntimeValue BinOpArgs::emit_greater_ints() {
  ASSERT(info.op_full == BinOpFull::greater_ints);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(info.dest_type == comp->builtin_types->t_bool);

  return bin_op_impl<IR::Types::Great>(ir, left, right, info.dest_type, IR::Emit::Great);
}

Eval::RuntimeValue BinOpArgs::emit_or_ints() {
  ASSERT(info.op_full == BinOpFull::or_ints);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(lt == info.dest_type);

  return bin_op_impl<IR::Types::Or>(ir, left, right, info.dest_type, IR::Emit::Or);
}

Eval::RuntimeValue BinOpArgs::emit_or_enums() {
  ASSERT(info.op_full == BinOpFull::or_enums);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(lt == info.dest_type);

  return bin_op_impl<IR::Types::Or>(ir, left, right, info.dest_type, IR::Emit::Or);
}

Eval::RuntimeValue BinOpArgs::emit_and_ints() {
  ASSERT(info.op_full == BinOpFull::and_ints);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(lt == info.dest_type);

  return bin_op_impl<IR::Types::And>(ir, left, right, info.dest_type, IR::Emit::And);
}

Eval::RuntimeValue BinOpArgs::emit_xor_ints() {
  ASSERT(info.op_full == BinOpFull::xor_ints);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(lt == info.dest_type);

  return bin_op_impl<IR::Types::Xor>(ir, left, right, info.dest_type, IR::Emit::Xor);
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

namespace {
  struct NegVisitor {
    Axle::ViewArr<u8> out;

    template<typename T>
    void operator()(const auto&...) const {
      INVALID_CODE_PATH("Attempted to negate a type which was not a signed integer");
    }

    template<Axle::OneOf<i8, i16, i32, i64> T>
    void operator()(const Axle::ViewArr<const u8> data) const {
      T i;
      bool res = Axle::deserialize_le<T>(data, i);
      ASSERT(res);
      Axle::serialize_le<T>(out, -i);
    }
  };
}

static Eval::RuntimeValue constant_fold_neg(CompilerGlobals* comp, const Eval::ConstantValue& constant) {
  ASSERT(constant.type.is_valid());
  ASSERT(TYPE_TESTS::is_signed_int(constant.type));

  const auto* const int_t = constant.type.unchecked_base<IntegerStructure>();

  u8* out = comp->new_constant(constant.type.size());

  const Axle::ViewArr<const u8> in_ser = Axle::view_arr(constant);
  Axle::ViewArr<u8> out_ser = {out, constant.type.size()};

  visit_ir_type(NegVisitor{out_ser}, int_t->ir_format, in_ser);

  return Eval::as_constant(out, constant.type);
}

Eval::RuntimeValue UnOpArgs::emit_neg_int() {
  ASSERT(info.op_full == UnOpFull::neg_int);

  const Type pet = prim.effective_type();
  ASSERT(info.src_type == pet);
  ASSERT(info.dest_type == pet);

  ASSERT(TYPE_TESTS::is_signed_int(pet));

  return un_op_impl<IR::Types::Neg>(comp, ir, prim, pet, IR::Emit::Neg, constant_fold_neg);
}

Eval::RuntimeValue UnOpArgs::emit_address() {
  ASSERT(info.op_full == UnOpFull::address);
  
  return Eval::addrof(ir, prim, info.dest_type, {});
}

Eval::RuntimeValue UnOpArgs::emit_deref_ptr() {
  ASSERT(info.op_full == UnOpFull::deref_ptr);
  
  return Eval::deref(ir, prim, info.src_type);
}

