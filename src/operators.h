#pragma once
#include "utility.h"
#include "comp_utilities.h"
#include "type.h"

struct TypeHint;

enum struct MainOp : uint8_t {
  LEFT, RIGHT
};

struct BinOpEmitInfo {
  MainOp main_op;
  Type main_type;
};

struct BinOpArgs {
  const BinOpEmitInfo* info;
  Compiler* comp;
  State* state;
  CodeBlock* code;
  const RuntimeValue* left;
  const RuntimeValue* right;

  //Emits
  RuntimeValue emit_add_64s();
  RuntimeValue emit_add_64_to_ptr();
  RuntimeValue emit_sub_64s();
  RuntimeValue emit_sub_ptrs();
  RuntimeValue emit_mul_64s();
  RuntimeValue emit_div_u64s();
  RuntimeValue emit_div_i64s();
  RuntimeValue emit_eq_64s();
  RuntimeValue emit_eq_8s();
  RuntimeValue emit_neq_8s();
  RuntimeValue emit_lesser_u64s();
  RuntimeValue emit_lesser_i64s();
  RuntimeValue emit_greater_u64s();
  RuntimeValue emit_greater_i64s();
  RuntimeValue emit_or_64s();
  RuntimeValue emit_xor_64s();
  RuntimeValue emit_and_64s();
  RuntimeValue emit_shift_l_64_by_8();
  RuntimeValue emit_shift_r_u64_by_8();
  RuntimeValue emit_shift_r_i64_by_8();
};

struct UnOpArgs {
  Compiler* comp;
  State* state;
  CodeBlock* code;
  const RuntimeValue* prim;

  //Emits
  RuntimeValue emit_neg_i64();
  RuntimeValue emit_address();
  RuntimeValue emit_deref();
};

using BINARY_OPERATOR_FUNCTION = MEMBER<BinOpArgs>::FUNCTION_PTR<RuntimeValue>;
using UNARY_OPERATOR_FUNCTION = MEMBER<UnOpArgs>::FUNCTION_PTR<RuntimeValue>;

//Sign agnostic arithmetic binary operator
struct SignAgnArithBinOp {
  BINARY_OPERATOR_FUNCTION ptrs_emit = nullptr;
  BINARY_OPERATOR_FUNCTION r64_emit = nullptr;
  BINARY_OPERATOR_FUNCTION bools_emit = nullptr;
};

//Signed arithmetic binary operator
struct SignedArithBinOp {
  BINARY_OPERATOR_FUNCTION u64_emit = nullptr;
  BINARY_OPERATOR_FUNCTION i64_emit = nullptr;
};

struct EqOpBin {
  BINARY_OPERATOR_FUNCTION u64_emit = nullptr;
  BINARY_OPERATOR_FUNCTION i64_emit = nullptr;
  BINARY_OPERATOR_FUNCTION r8_emit  = nullptr;
  BINARY_OPERATOR_FUNCTION bools_emit = nullptr;
  BINARY_OPERATOR_FUNCTION ascii_emit = nullptr;
};

struct BalancedBinOpOptions {
  BINARY_OPERATOR_FUNCTION ptrs_emit = nullptr;
  BINARY_OPERATOR_FUNCTION u64_emit = nullptr;
  BINARY_OPERATOR_FUNCTION i64_emit = nullptr;
  BINARY_OPERATOR_FUNCTION u8_emit = nullptr;
  BINARY_OPERATOR_FUNCTION bools_emit = nullptr;
  BINARY_OPERATOR_FUNCTION ascii_emit = nullptr;
};

struct UnpositionedBinOpOptions {
  BINARY_OPERATOR_FUNCTION r64_and_r64_emit = nullptr;
  BINARY_OPERATOR_FUNCTION ptr_and_r64_emit = nullptr;
};

struct UnbalancedLeftSignAgnBin {
  BINARY_OPERATOR_FUNCTION Lr64_Ru8_emit = nullptr;
};

struct UnbalancedBinOpOptions {
  BINARY_OPERATOR_FUNCTION Lu64_Ru8_emit = nullptr;
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
                             Context* context,
                             State* state,
                             struct ASTBinaryOperatorExpr* expr,
                             const SignAgnArithBinOp& op,
                             const TypeHint* hint);

//Overload for operators that do care about sign
void compile_binary_operator(Compiler* comp,
                             Context* context,
                             State* state,
                             struct ASTBinaryOperatorExpr* expr,
                             const SignedArithBinOp& op,
                             const TypeHint* hint);

//Overload for operators that return bools
void compile_binary_operator(Compiler* comp,
                             Context* context,
                             State* state,
                             struct ASTBinaryOperatorExpr* expr,
                             const EqOpBin& op);

//Overload for unpositioned operators
void compile_binary_operator(Compiler* comp,
                             Context* context,
                             State* state,
                             struct ASTBinaryOperatorExpr* expr,
                             const UnpositionedBinOpOptions& op,
                             const TypeHint* hint);

//Overload for unbalanced operators
void compile_binary_operator(Compiler* comp,
                             Context* context,
                             State* state,
                             struct ASTBinaryOperatorExpr* expr,
                             const UnbalancedBinOpOptions& op);

//Overload for unbalanced operators that dont care about left sign
void compile_binary_operator(Compiler* comp,
                             Context* context,
                             State* state,
                             struct ASTBinaryOperatorExpr* expr,
                             const UnbalancedLeftSignAgnBin& op);

//Overload for unary operators
void compile_unary_operator(Compiler* comp,
                            Context* context,
                            State* state,
                            struct ASTUnaryOperatorExpr* expr,
                            const UnaryOpOptions& op);

//Overload for taking address
void compile_take_address(Compiler* comp,
                          Context* context,
                          State* state,
                          struct ASTUnaryOperatorExpr* expr);

void compile_deref(Compiler* comp,
                   struct ASTUnaryOperatorExpr* expr);

inline constexpr UnpositionedBinOpOptions add_operators ={ 
  &BinOpArgs::emit_add_64s,
  &BinOpArgs::emit_add_64_to_ptr,
};

inline constexpr SignAgnArithBinOp sub_operators ={
  &BinOpArgs::emit_sub_ptrs,
  &BinOpArgs::emit_sub_64s,
  nullptr,
};

inline constexpr SignAgnArithBinOp mul_operators ={
  nullptr,
  &BinOpArgs::emit_mul_64s,
  nullptr,
};

inline constexpr SignedArithBinOp div_operators ={
  &BinOpArgs::emit_div_u64s,
  &BinOpArgs::emit_div_i64s,
};

inline constexpr EqOpBin eq_operators ={
  &BinOpArgs::emit_eq_64s,
  &BinOpArgs::emit_eq_64s,
  &BinOpArgs::emit_eq_8s,
  &BinOpArgs::emit_eq_8s,
  &BinOpArgs::emit_eq_8s
};

inline constexpr EqOpBin neq_operators ={
  nullptr,
  nullptr,
  &BinOpArgs::emit_neq_8s,
  &BinOpArgs::emit_neq_8s,
  &BinOpArgs::emit_neq_8s
};

inline constexpr EqOpBin lesser_operators ={
  &BinOpArgs::emit_lesser_u64s,
  &BinOpArgs::emit_lesser_i64s,
  nullptr,
  nullptr,
};
inline constexpr EqOpBin greater_operators ={
  &BinOpArgs::emit_greater_u64s,
  &BinOpArgs::emit_greater_i64s,
  nullptr,
  nullptr,
};

inline constexpr SignAgnArithBinOp or_operators ={ 
  nullptr,
  &BinOpArgs::emit_or_64s,
  &BinOpArgs::emit_or_64s,
};

inline constexpr SignAgnArithBinOp xor_operators ={ 
  nullptr,
  &BinOpArgs::emit_xor_64s,
  &BinOpArgs::emit_xor_64s,
};

inline constexpr SignAgnArithBinOp and_operators ={
  nullptr,
  &BinOpArgs::emit_and_64s,
  &BinOpArgs::emit_and_64s,
};

inline constexpr UnbalancedLeftSignAgnBin left_shift_operators ={ 
  &BinOpArgs::emit_shift_l_64_by_8 };
inline constexpr UnbalancedBinOpOptions right_shift_operators ={
  &BinOpArgs::emit_shift_r_u64_by_8,
  &BinOpArgs::emit_shift_r_i64_by_8,
};

inline constexpr UnaryOpOptions neg_operators ={
  &UnOpArgs::emit_neg_i64,
};

inline constexpr AddressUnOp address_operators ={
  &UnOpArgs::emit_address,
};

inline constexpr DerefUnOp deref_operators ={
  &UnOpArgs::emit_deref,
};