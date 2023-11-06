#pragma once
#include <AxleUtil/utility.h>
#include <Axle/comp_utilities.h>
#include "ir.h"

enum struct MainSide : uint8_t {
  LEFT, RIGHT
};

struct BinOpArgs;
struct UnOpArgs;
using BINARY_OPERATOR_FUNCTION = MEMBER<BinOpArgs>::FUNCTION_PTR<Eval::RuntimeValue>;
using UNARY_OPERATOR_FUNCTION = MEMBER<UnOpArgs>::FUNCTION_PTR<Eval::RuntimeValue>;

struct BinOpEmitInfo {
  MainSide main_side;
  Type dest_type;
  BINARY_OPERATOR_FUNCTION func = nullptr;
};

struct BinOpArgs {
  const BinOpEmitInfo* info;
  CompilerGlobals* comp;
  IR::IRStore* ir;

  const Eval::RuntimeValue& left;
  const Eval::RuntimeValue& right;

  inline Eval::RuntimeValue emit() {
    return (this->*(info->func))();
  }

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

struct UnOpEmitInfo {
  Type src_type;
  Type dest_type;
  UNARY_OPERATOR_FUNCTION func = nullptr;
};

struct UnOpArgs {
  const UnOpEmitInfo* info;
  CompilerGlobals* comp;
  IR::IRStore* ir;
  const Eval::RuntimeValue& prim;

  inline Eval::RuntimeValue emit() {
    return (this->*(info->func))();
  }

  //Emits
  Eval::RuntimeValue emit_neg_int();
  Eval::RuntimeValue emit_address();
  Eval::RuntimeValue emit_deref_ptr();
};