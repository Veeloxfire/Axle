#pragma once
#include "type.h"

enum struct MainSide : uint8_t {
  LEFT, RIGHT
};

enum struct BinOpFull {
  add_ints,
  add_int_to_ptr,
  sub_ints,
  sub_ptrs,
  mul_ints,
  div_ints,
  mod_ints,
  eq_ints,
  neq_ints,
  lesser_ints,
  greater_ints,
  or_ints,
  or_enums,
  xor_ints,
  and_ints,
};

struct BinOpEmitInfo {
  MainSide main_side;
  Type dest_type;
  BinOpFull op_full;
};

enum struct UnOpFull {
  neg_int,
  address,
  deref_ptr,
};

struct UnOpEmitInfo {
  Type src_type;
  Type dest_type;
  UnOpFull op_full;
};

namespace IR {
  struct IRStore;
  struct FunctionSignature;
  struct Function;
  struct ValueRequirements;
}

namespace Eval {
  struct RuntimeValue;
};

namespace CASTS {
  using CAST_FUNCTION = Eval::RuntimeValue(*) (IR::IRStore* const ir,
                                               const Type& to,
                                               const Eval::RuntimeValue& val,
                                               IR::ValueRequirements reqs);
}
