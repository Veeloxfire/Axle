#include "operators.h"
#include "compiler.h"

template<typename T>
concept BinOpArgsEmit = requires (const T emit, Eval::IrBuilder* builder, IR::V_ARG l, IR::V_ARG r) {
  { emit(builder, l, r) } -> Axle::IS_SAME_TYPE<Eval::RuntimeValue>;
};

template<typename T>
concept BinOpConstantFoldEmit = BinOpArgsEmit<T> && requires(const T emit, Eval::IrBuilder* builder, Eval::ConstantValue l, Eval::ConstantValue r) {
  { emit(builder, l, r) } -> Axle::IS_SAME_TYPE<Eval::RuntimeValue>;
};

template<typename BIN_OP_TYPE, void(*EMIT)(Axle::Array<u8>&, const BIN_OP_TYPE&)>
Eval::RuntimeValue bin_op_emit_impl(Eval::IrBuilder* const builder,
                                    const IR::V_ARG& l, const IR::V_ARG& r,
                                    const Type& dest_type) {
  AXLE_TELEMETRY_FUNCTION();
  IR::ValueIndex to = builder->ir->new_temporary(dest_type, {});

  BIN_OP_TYPE bin_op = {};
  bin_op.to = IR::v_arg(to, 0, dest_type);
  bin_op.left = l;
  bin_op.right = r;

  EMIT(builder->current_bytecode(), bin_op);

  return Eval::as_direct(to, dest_type);
}

template<BinOpConstantFoldEmit Emit>
static Eval::RuntimeValue bin_op_dispatch(Eval::IrBuilder* builder,
                                      const Eval::RuntimeValue& left_in, const Eval::RuntimeValue& right_in,
                                      const Emit& emit) {
  AXLE_TELEMETRY_FUNCTION();
  if (left_in.rvt == Eval::RVT::Constant && right_in.rvt == Eval::RVT::Constant) {
    // Full constant fold
    return emit(builder, left_in.constant, right_in.constant);
  }
  
  IR::V_ARG left = Eval::load_v_arg(builder->ir, left_in);
  IR::V_ARG right = Eval::load_v_arg(builder->ir, right_in);

  return emit(builder, left, right);
}


template<typename T>
concept UnOpArgsEmit = requires (const T emit, Eval::IrBuilder* builder, IR::V_ARG p) {
  { emit(builder, p) } -> Axle::IS_SAME_TYPE<Eval::RuntimeValue>;
};

template<typename T>
concept UnOpConstantFoldEmit = UnOpArgsEmit<T> && requires(const T emit, Eval::IrBuilder* builder, Eval::ConstantValue p) {
  { emit(builder, p) } -> Axle::IS_SAME_TYPE<Eval::RuntimeValue>;
};


template<typename UN_OP_TYPE, void(*EMIT)(Axle::Array<u8>&, const UN_OP_TYPE&)>
Eval::RuntimeValue un_op_emit_impl(Eval::IrBuilder* const builder,
                                   const IR::V_ARG& p,
                                   const Type& dest_type) {
  AXLE_TELEMETRY_FUNCTION();
  IR::ValueIndex to = builder->ir->new_temporary(dest_type, {});

  UN_OP_TYPE un_op = {};
  un_op.to = IR::v_arg(to, 0, dest_type);
  un_op.from = p;

  EMIT(builder->current_bytecode(), un_op);

  return Eval::as_direct(to, dest_type);
}

template<UnOpConstantFoldEmit Emit>
static Eval::RuntimeValue un_op_dispatch(Eval::IrBuilder* const builder,
                                         const Eval::RuntimeValue& from_in,
                                         const Emit& emit) {
  AXLE_TELEMETRY_FUNCTION();

  if (from_in.rvt == Eval::RVT::Constant) {
    // Full constant fold
    return emit(builder, from_in.constant);
  }

  IR::V_ARG from = Eval::load_v_arg(builder->ir, from_in);

  return emit(builder, from);
}

#define BASIC_INTS_OPERATION(name, eval_name, ir_name) \
namespace { struct ir_name ## IntsDispatch { \
  Errors* errors; \
  Type dest_type; \
  Eval::RuntimeValue operator()(Eval::IrBuilder* builder, const Eval::ConstantValue& l, const Eval::ConstantValue& r) const { \
    u8* constant = builder->new_constant(dest_type); \
    VM:: eval_name (errors, dest_type.struct_format(), Axle::ViewArr<u8>{constant, dest_type.size()}, Axle::view_arr(l), Axle::view_arr(r)); \
    return Eval::as_constant(constant, dest_type); \
  } \
  Eval::RuntimeValue operator()(Eval::IrBuilder* builder, const IR::V_ARG& l, const IR::V_ARG& r) const { \
    return bin_op_emit_impl<IR::Types:: ir_name, IR::Emit:: ir_name>(builder, l, r, dest_type); \
  } \
};} \
Eval::RuntimeValue BinOpArgs:: emit_ ## name () { \
  ASSERT(info.op_full == BinOpFull:: name); \
  const Type lt = left.effective_type(); \
  const Type rt = right.effective_type(); \
  ASSERT(lt == rt); \
  ASSERT(lt == info.dest_type); \
  return bin_op_dispatch(builder, left, right, ir_name ## IntsDispatch { &comp_thread->errors, info.dest_type }); \
}

BASIC_INTS_OPERATION(add_ints, eval_add, Add);
BASIC_INTS_OPERATION(sub_ints, eval_sub, Sub);
BASIC_INTS_OPERATION(mul_ints, eval_mul, Mul);
BASIC_INTS_OPERATION(div_ints, eval_div, Div);
BASIC_INTS_OPERATION(mod_ints, eval_mod, Mod);
BASIC_INTS_OPERATION(or_ints, eval_or, Or);
BASIC_INTS_OPERATION(and_ints, eval_and, And);
BASIC_INTS_OPERATION(xor_ints, eval_xor, Xor);

#undef BASIC_INTS_OPERATION

#define BASIC_CMP_OPERATION(name, eval_name, ir_name) \
namespace { struct ir_name ## IntsDispatch { \
  Errors* errors; \
  Type dest_type; \
  Eval::RuntimeValue operator()(Eval::IrBuilder* builder, const Eval::ConstantValue& l, const Eval::ConstantValue& r) const { \
    u8* constant = builder->new_constant(dest_type); \
    VM:: eval_name (errors, dest_type.struct_format(), Axle::ViewArr<u8>{constant, dest_type.size()}, Axle::view_arr(l), Axle::view_arr(r)); \
    return Eval::as_constant(constant, dest_type); \
  } \
  Eval::RuntimeValue operator()(Eval::IrBuilder* builder, const IR::V_ARG& l, const IR::V_ARG& r) const { \
    return bin_op_emit_impl<IR::Types:: ir_name, IR::Emit:: ir_name>(builder, l, r, dest_type); \
  } \
};} \
Eval::RuntimeValue BinOpArgs:: emit_ ## name () { \
  ASSERT(info.op_full == BinOpFull:: name); \
  const Type lt = left.effective_type(); \
  const Type rt = right.effective_type(); \
  ASSERT(lt == rt); \
  ASSERT(comp_thread->builtin_types->t_bool == info.dest_type); \
  return bin_op_dispatch(builder, left, right, ir_name ## IntsDispatch { &comp_thread->errors, info.dest_type }); \
}

BASIC_CMP_OPERATION(eq_ints, eval_eq, Eq);
BASIC_CMP_OPERATION(neq_ints, eval_neq, Neq);
BASIC_CMP_OPERATION(lesser_ints, eval_less, Less);
BASIC_CMP_OPERATION(greater_ints, eval_great, Great);

#undef BASIC_CMP_OPERATION

Eval::RuntimeValue BinOpArgs::emit_add_int_to_ptr() {
  ASSERT(info.op_full == BinOpFull::add_int_to_ptr);
  
  Type ptr_type = info.dest_type;
  ASSERT(ptr_type.struct_type() == STRUCTURE_TYPE::POINTER);

  const Type lt = left.effective_type();
  const Type rt = right.effective_type();


  Eval::RuntimeValue int_val;
  Eval::RuntimeValue ptr_val;
  const Type size_type = comp_thread->builtin_types->t_u64;

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

    const Eval::RuntimeValue to_add = bin_op_dispatch(builder, int_val, ptr_size, MulIntsDispatch { &comp_thread->errors, size_type });
    if (comp_thread->is_panic()) {
      return Eval::no_value();
    }

    return bin_op_dispatch(builder, ptr_val, to_add, AddIntsDispatch { &comp_thread->errors, ptr_type });
  }
  else {
    return bin_op_dispatch(builder, ptr_val, int_val, AddIntsDispatch { &comp_thread->errors, ptr_type });
  }
}

Eval::RuntimeValue BinOpArgs::emit_sub_ptrs() {
  ASSERT(info.op_full == BinOpFull::sub_ptrs);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(lt == info.dest_type);
  ASSERT(info.dest_type == comp_thread->builtin_types->t_u64);
  Type size_type = comp_thread->builtin_types->t_u64;

  const auto* ptr = lt.unchecked_base<PointerStructure>();
  
  const Eval::RuntimeValue scaled = bin_op_dispatch(builder, left, right, SubIntsDispatch { &comp_thread->errors, size_type });

  const u64 size_holder = ptr->base.size();
  ASSERT(size_holder > 0);
  if (size_holder > 1) {
    const Eval::RuntimeValue ptr_size = Eval::as_constant((const u8*)&size_holder, size_type);
    return bin_op_dispatch(builder, scaled, ptr_size, DivIntsDispatch{ &comp_thread->errors, size_type });
  }
  else {
    return scaled;
  }
}

Eval::RuntimeValue BinOpArgs::emit_or_enums() {
  ASSERT(info.op_full == BinOpFull::or_enums);
  
  const Type lt = left.effective_type();
  const Type rt = right.effective_type();
  ASSERT(lt == rt);
  ASSERT(lt == info.dest_type);

  return bin_op_dispatch(builder, left, right, OrIntsDispatch { &comp_thread->errors, info.dest_type });
}

namespace {
struct NegDispatch {
  Errors* errors;
  Type dest_type;

  Eval::RuntimeValue operator()(Eval::IrBuilder* builder, const Eval::ConstantValue& p) const {
    u8* constant = builder->new_constant(dest_type);
    VM::eval_negate(errors, dest_type.struct_format(), Axle::ViewArr<u8>{constant, dest_type.size()}, Axle::view_arr(p));
    return Eval::as_constant(constant, dest_type);
  }

  Eval::RuntimeValue operator()(Eval::IrBuilder* builder, const IR::V_ARG& p) const {
    return un_op_emit_impl<IR::Types::Neg, IR::Emit::Neg>(builder, p, dest_type);
  }
};
}

Eval::RuntimeValue UnOpArgs::emit_neg_int() {
  ASSERT(info.op_full == UnOpFull::neg_int);

  const Type pet = prim.effective_type();
  ASSERT(info.src_type == pet);
  ASSERT(info.dest_type == pet);

  ASSERT(TYPE_TESTS::is_signed_int(pet));

  return un_op_dispatch(builder, prim, NegDispatch { &comp_thread->errors, info.dest_type });
}

Eval::RuntimeValue UnOpArgs::emit_address() {
  ASSERT(info.op_full == UnOpFull::address);
  
  return Eval::addrof(builder->ir, prim, info.dest_type, {});
}

Eval::RuntimeValue UnOpArgs::emit_deref_ptr() {
  ASSERT(info.op_full == UnOpFull::deref_ptr);
  
  return Eval::deref(builder->ir, prim, info.src_type);
}

