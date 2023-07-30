#pragma once
#include "utility.h"
#include "comp_utilities.h"
#include "type.h"
#include "ir.h"

enum struct MainSide : uint8_t {
  LEFT, RIGHT
};

struct BinOpArgs;
struct UnOpArgs;
using BINARY_OPERATOR_FUNCTION = MEMBER<BinOpArgs>::FUNCTION_PTR<IR::RuntimeReference>;
using UNARY_OPERATOR_FUNCTION = MEMBER<UnOpArgs>::FUNCTION_PTR<IR::RuntimeReference>;

struct BinOpEmitInfo {
  MainSide main_side;
  Type dest_type;
  BINARY_OPERATOR_FUNCTION func = nullptr;
};

struct BinOpArgs {
  const BinOpEmitInfo* info;
  CompilerGlobals* comp;
  IR::Builder* ir;

  const IR::RuntimeReference& left;
  const IR::RuntimeReference& right;

  inline IR::RuntimeReference emit() {
    return (this->*(info->func))();
  }

  //Emits
  IR::RuntimeReference emit_add_ints();
  IR::RuntimeReference emit_add_int_to_ptr();
  IR::RuntimeReference emit_sub_ints();
  IR::RuntimeReference emit_sub_ptrs();
  IR::RuntimeReference emit_mul_ints();
  IR::RuntimeReference emit_div_ints();
  IR::RuntimeReference emit_mod_ints();
  IR::RuntimeReference emit_eq_ints();
  IR::RuntimeReference emit_neq_ints();
  IR::RuntimeReference emit_lesser_ints();
  IR::RuntimeReference emit_greater_ints();
  IR::RuntimeReference emit_or_ints();
  IR::RuntimeReference emit_or_enums();
  IR::RuntimeReference emit_xor_ints();
  IR::RuntimeReference emit_and_ints();
#if 0
  IR::RuntimeReference emit_shift_l_64_by_8();
  IR::RuntimeReference emit_shift_r_u64_by_8();
  IR::RuntimeReference emit_shift_r_i64_by_8();
#endif
};

struct UnOpEmitInfo {
  Type dest_type;
  UNARY_OPERATOR_FUNCTION func = nullptr;
};

struct UnOpArgs {
  const UnOpEmitInfo* info;
  CompilerGlobals* comp;
#if 0
  CompilerThread* comp_thread;
#endif
  IR::Builder* ir;
  const IR::RuntimeReference& prim;

  inline IR::RuntimeReference emit() {
    return (this->*(info->func))();
  }

  //Emits
  IR::RuntimeReference emit_neg_int();
  IR::RuntimeReference emit_address();
  IR::RuntimeReference emit_deref_ptr();
};