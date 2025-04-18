#pragma once
#include <AxleUtil/utility.h>
#include <Axle/comp_utilities.h>
#include "ir.h"
#include "ir_ast_info.h"

struct BinOpArgs {
  BinOpEmitInfo info;
  CompilerGlobals* comp;
  IR::IRStore* ir;

  const Eval::RuntimeValue& left;
  const Eval::RuntimeValue& right;

  constexpr Eval::RuntimeValue emit();

  //Emits
  Eval::RuntimeValue emit_add_ints();
  Eval::RuntimeValue emit_add_int_to_ptr();
  Eval::RuntimeValue emit_sub_ints();
  Eval::RuntimeValue emit_sub_ptrs();
  Eval::RuntimeValue emit_mul_ints();
  Eval::RuntimeValue emit_div_ints();
  Eval::RuntimeValue emit_mod_ints();
  Eval::RuntimeValue emit_eq_ints();
  Eval::RuntimeValue emit_neq_ints();
  Eval::RuntimeValue emit_lesser_ints();
  Eval::RuntimeValue emit_greater_ints();
  Eval::RuntimeValue emit_or_ints();
  Eval::RuntimeValue emit_or_enums();
  Eval::RuntimeValue emit_xor_ints();
  Eval::RuntimeValue emit_and_ints();
#if 0
  Eval::RuntimeValue emit_shift_l_64_by_8();
  Eval::RuntimeValue emit_shift_r_u64_by_8();
  Eval::RuntimeValue emit_shift_r_i64_by_8();
#endif
};

struct UnOpArgs {
  UnOpEmitInfo info;
  CompilerGlobals* comp;
  IR::IRStore* ir;
  const Eval::RuntimeValue& prim;

  constexpr Eval::RuntimeValue emit();

  //Emits
  Eval::RuntimeValue emit_neg_int();
  Eval::RuntimeValue emit_address();
  Eval::RuntimeValue emit_deref_ptr();
};


inline constexpr Eval::RuntimeValue BinOpArgs::emit() {
  switch (info.op_full) {
    case BinOpFull::add_ints: return emit_add_ints();
    case BinOpFull::add_int_to_ptr: return emit_add_int_to_ptr();
    case BinOpFull::sub_ints: return emit_sub_ints();
    case BinOpFull::sub_ptrs: return emit_sub_ptrs();
    case BinOpFull::mul_ints: return emit_mul_ints();
    case BinOpFull::div_ints: return emit_div_ints();
    case BinOpFull::mod_ints: return emit_mod_ints();
    case BinOpFull::eq_ints: return emit_eq_ints();
    case BinOpFull::neq_ints: return emit_neq_ints();
    case BinOpFull::lesser_ints: return emit_lesser_ints();
    case BinOpFull::greater_ints: return emit_greater_ints();
    case BinOpFull::or_ints: return emit_or_ints();
    case BinOpFull::or_enums: return emit_or_enums();
    case BinOpFull::xor_ints: return emit_xor_ints();
    case BinOpFull::and_ints: return emit_and_ints();
  }

  INVALID_CODE_PATH("Invalid binary operator");
}

inline constexpr Eval::RuntimeValue UnOpArgs::emit() {
  switch (info.op_full) {
    case UnOpFull::neg_int: return emit_neg_int();
    case UnOpFull::address: return emit_address();
    case UnOpFull::deref_ptr: return emit_deref_ptr();
  }
  
  INVALID_CODE_PATH("Invalid unary operator");
}
