#pragma once
#include "utility.h"
#include "runtime_vals.h"

using BINARY_OPERATOR_FUNCTION = FUNCTION_PTR<
  RuntimeValue,
  Compiler* const,
  State* const,
  CodeBlock* const,
  const RuntimeValue*, const RuntimeValue*
>;

using UNARY_OPERATOR_FUNCTION = FUNCTION_PTR<
  RuntimeValue,
  Compiler* const,
  State* const,
  CodeBlock* const,
  const RuntimeValue*
>;

namespace OP {

  RuntimeValue emit_add_64s(Compiler* const comp,
                            State* const state,
                            CodeBlock* const code,
                            const RuntimeValue*, const RuntimeValue*);

  RuntimeValue emit_sub_64s(Compiler* const comp,
                            State* const state,
                            CodeBlock* const code,
                            const RuntimeValue*, const RuntimeValue*);

  RuntimeValue emit_mul_64s(Compiler* const comp,
                            State* const state,
                            CodeBlock* const code,
                            const RuntimeValue*, const RuntimeValue*);

  RuntimeValue emit_div_u64s(Compiler* const comp,
                             State* const state,
                             CodeBlock* const code,
                             const RuntimeValue*, const RuntimeValue*);

  RuntimeValue emit_div_i64s(Compiler* const comp,
                             State* const state,
                             CodeBlock* const code,
                             const RuntimeValue*, const RuntimeValue*);

  RuntimeValue emit_eq_64s(Compiler* const comp,
                           State* const state,
                           CodeBlock* const code,
                           const RuntimeValue*, const RuntimeValue*);

  RuntimeValue emit_or_64s(Compiler* const comp,
                           State* const state,
                           CodeBlock* const code,
                           const RuntimeValue*, const RuntimeValue*);

  RuntimeValue emit_and_64s(Compiler* const comp,
                            State* const state,
                            CodeBlock* const code,
                            const RuntimeValue*, const RuntimeValue*);

  RuntimeValue emit_shift_l_64_by_8(Compiler* const comp,
                                    State* const state,
                                    CodeBlock* const code,
                                    const RuntimeValue*, const RuntimeValue*);

  RuntimeValue emit_shift_r_u64_by_8(Compiler* const comp,
                                     State* const state,
                                     CodeBlock* const code,
                                     const RuntimeValue*, const RuntimeValue*);

  RuntimeValue emit_shift_r_i64_by_8(Compiler* const comp,
                                     State* const state,
                                     CodeBlock* const code,
                                     const RuntimeValue*, const RuntimeValue*);

  RuntimeValue emit_neg_i64(Compiler* const comp,
                            State* const state,
                            CodeBlock* const code,
                            const RuntimeValue*);

  RuntimeValue emit_address(Compiler* const comp,
                            State* const state,
                            CodeBlock* const code,
                            const RuntimeValue*);

  RuntimeValue emit_deref(Compiler* const comp,
                          State* const state,
                          CodeBlock* const code,
                          const RuntimeValue*);
}

//Sign agnostic arithmetic binary operator
struct SignAgnArithBinOp {
  BINARY_OPERATOR_FUNCTION r64_emit = nullptr;
  BINARY_OPERATOR_FUNCTION bools_emit = nullptr;
};

//Signed arithmetic binary operator
struct SignedArithBinOp {
  BINARY_OPERATOR_FUNCTION u64_emit = nullptr;
  BINARY_OPERATOR_FUNCTION i64_emit = nullptr;
};

struct EqOpBin {
  BINARY_OPERATOR_FUNCTION r64_emit = nullptr;
  BINARY_OPERATOR_FUNCTION bools_emit = nullptr;
};

struct BalancedBinOpOptions {
  BINARY_OPERATOR_FUNCTION u64_emit = nullptr;
  BINARY_OPERATOR_FUNCTION i64_emit = nullptr;
  BINARY_OPERATOR_FUNCTION bools_emit = nullptr;
};

struct UnbalancedLeftSignAgnBin {
  BINARY_OPERATOR_FUNCTION Lr64_Ru8_emit = nullptr;
};

struct UnbalancedBinOpOptions {
  //left is u64, right is u8
  BINARY_OPERATOR_FUNCTION Lu64_Ru8_emit = nullptr;

  //left is u64, right is u8
  BINARY_OPERATOR_FUNCTION Li64_Ru8_emit = nullptr;
};

struct UnaryOpOptions {
  UNARY_OPERATOR_FUNCTION i64_emit = nullptr;
};

struct AddressUnOp {
  UNARY_OPERATOR_FUNCTION emit = nullptr;
};

struct DerefUnOp {
  UNARY_OPERATOR_FUNCTION emit = nullptr;
};

//Overload for operators that dont care about sign
void compile_binary_operator(Compiler* comp,
                             ASTExpression* expr,
                             const SignAgnArithBinOp& op);

//Overload for operators that do care about sign
void compile_binary_operator(Compiler* comp,
                             ASTExpression* expr,
                             const SignedArithBinOp& op);

//Overload for operators that return bools
void compile_binary_operator(Compiler* comp,
                             ASTExpression* expr,
                             const EqOpBin& op);

//Overload for unbalanced operators
void compile_binary_operator(Compiler* comp,
                             ASTExpression* expr,
                             const UnbalancedBinOpOptions& op);

//Overload for unbalanced operators that dont care about left sign
void compile_binary_operator(Compiler* comp,
                             ASTExpression* expr,
                             const UnbalancedLeftSignAgnBin& op);

//Overload for unary operators
void compile_unary_operator(Compiler* comp,
                             ASTExpression* expr,
                             const UnaryOpOptions& op);

//Overload for taking address
void compile_take_address(Compiler* comp,
                          State* state,
                          ASTExpression* expr);

void compile_deref(Compiler* comp,
                   ASTExpression* expr);

inline constexpr SignAgnArithBinOp add_operators ={ &OP::emit_add_64s, nullptr };//ADD u64 or i64
inline constexpr SignAgnArithBinOp sub_operators ={ &OP::emit_sub_64s, nullptr };//SUB u64 or i64
inline constexpr SignAgnArithBinOp mul_operators ={ &OP::emit_mul_64s, nullptr };//MUL u64 or i64
inline constexpr SignedArithBinOp div_operators ={
  &OP::emit_div_u64s, &OP::emit_div_i64s,
};

inline constexpr EqOpBin eq_operators = {&OP::emit_eq_64s, &OP::emit_eq_64s};

inline constexpr SignAgnArithBinOp or_operators ={ &OP::emit_or_64s, &OP::emit_or_64s};
inline constexpr SignAgnArithBinOp and_operators ={
  &OP::emit_and_64s,//AND u64 or i64
  &OP::emit_and_64s//AND bools
};

inline constexpr UnbalancedLeftSignAgnBin left_shift_operators ={ &OP::emit_shift_l_64_by_8 };
inline constexpr UnbalancedBinOpOptions right_shift_operators ={
  &OP::emit_shift_r_u64_by_8,
  &OP::emit_shift_r_i64_by_8,
};

inline constexpr UnaryOpOptions neg_operators ={
  &OP::emit_neg_i64,
};

inline constexpr AddressUnOp address_operators ={
  &OP::emit_address,
};

inline constexpr DerefUnOp deref_operators ={
  &OP::emit_deref,
};