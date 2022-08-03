#pragma once
#include "utility.h"
#include "comp_utilities.h"
#include "type.h"

enum struct MainOp : uint8_t {
  LEFT, RIGHT
};

struct BinOpArgs;
struct UnOpArgs;
using BINARY_OPERATOR_FUNCTION = MEMBER<BinOpArgs>::FUNCTION_PTR<RuntimeValue>;
using UNARY_OPERATOR_FUNCTION = MEMBER<UnOpArgs>::FUNCTION_PTR<RuntimeValue>;

struct BinOpEmitInfo {
  MainOp main_op;
  Type main_type;
  BINARY_OPERATOR_FUNCTION func = nullptr;
};

struct BinOpArgs {
  const BinOpEmitInfo* info;
  CompilerGlobals* comp;
  //CompilerThread* comp_thread;
  State* state;
  CodeBlock* code;

  const RuntimeValue* left;
  const RuntimeValue* right;

  inline RuntimeValue emit() {
    return (this->*(info->func))();
  }

  //Emits
  RuntimeValue emit_add_64s();
  RuntimeValue emit_add_8s();
  RuntimeValue emit_add_64_to_ptr();
  RuntimeValue emit_sub_64s();
  RuntimeValue emit_sub_ptrs();
  RuntimeValue emit_mul_64s();
  RuntimeValue emit_div_u64s();
  RuntimeValue emit_div_i64s();
  RuntimeValue emit_mod_u64s();
  RuntimeValue emit_eq_64s();
  RuntimeValue emit_neq_64s();
  RuntimeValue emit_eq_8s();
  RuntimeValue emit_neq_8s();
  RuntimeValue emit_lesser_u64s();
  RuntimeValue emit_lesser_i64s();
  RuntimeValue emit_greater_u64s();
  RuntimeValue emit_greater_i64s();
  RuntimeValue emit_or_64s();
  RuntimeValue emit_xor_64s();
  RuntimeValue emit_and_64s();
  RuntimeValue emit_or_8s();
  RuntimeValue emit_xor_8s();
  RuntimeValue emit_and_8s();
  RuntimeValue emit_shift_l_64_by_8();
  RuntimeValue emit_shift_r_u64_by_8();
  RuntimeValue emit_shift_r_i64_by_8();
};

struct UnOpArgs {
  CompilerGlobals* comp;
  //CompilerThread* comp_thread;
  State* state;
  CodeBlock* code;
  const RuntimeValue* prim;

  //Emits
  RuntimeValue emit_neg_i64();
  RuntimeValue emit_address();
  RuntimeValue emit_deref();
};


void compile_binary_operator(CompilerGlobals* comp,
                             CompilerThread* comp_thread,
                             State* state,
                             struct ASTBinaryOperatorExpr* expr);

struct UnaryOpOptions {
  UNARY_OPERATOR_FUNCTION i64_emit = nullptr;
};

struct AddressUnOp {
  UNARY_OPERATOR_FUNCTION emit = nullptr;
};

struct DerefUnOp {
  UNARY_OPERATOR_FUNCTION emit = nullptr;
};

//Overload for unary operators
void compile_unary_operator(CompilerGlobals* comp,
                            CompilerThread* comp_thread,
                            State* state,
                            struct ASTUnaryOperatorExpr* expr,
                            const UnaryOpOptions& op);

//Overload for taking address
void compile_take_address(CompilerGlobals* comp,
                          CompilerThread* comp_thread,
                          State* state,
                          struct ASTUnaryOperatorExpr* expr);

void compile_deref(CompilerGlobals* comp,
                   CompilerThread* comp_thread,
                   struct ASTUnaryOperatorExpr* expr);

inline constexpr UnaryOpOptions neg_operators ={
  &UnOpArgs::emit_neg_i64,
};

inline constexpr AddressUnOp address_operators ={
  &UnOpArgs::emit_address,
};

inline constexpr DerefUnOp deref_operators ={
  &UnOpArgs::emit_deref,
};