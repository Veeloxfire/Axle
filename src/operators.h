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

struct BinaryOperation {
  FUNCTION_PTR<const Structure*, Compiler*, const Structure*, const Structure*> test;
  BINARY_OPERATOR_FUNCTION func;
};

struct UnaryOperation {
  FUNCTION_PTR<const Structure*, Compiler*, const Structure*> test;
  UNARY_OPERATOR_FUNCTION func;
};

namespace BIN_OP_TESTS {
  const Structure* num_int_64_bit(Compiler* comp, const Structure* left, const Structure* right);
  const Structure* num_signed_int_64_bit(Compiler* comp, const Structure* left, const Structure* right);
  const Structure* num_unsigned_int_64_bit(Compiler* comp, const Structure* left, const Structure* right);

  const Structure* eq_int_64_bit(Compiler* comp, const Structure* left, const Structure* right);
  const Structure* shift_64(Compiler* comp, const Structure* left, const Structure* right);

  const Structure* u_shift_64(Compiler* comp, const Structure* left, const Structure* right);
  const Structure* s_shift_64(Compiler* comp, const Structure* left, const Structure* right);

  const Structure* bools(Compiler* comp, const Structure* left, const Structure* right);
}

namespace UN_OP_TESTS {
  const Structure* address(Compiler* comp, const Structure* s);
  const Structure* deref(Compiler* comp, const Structure* s);
  const Structure* signed_int_64_bit(Compiler* comp, const Structure* s);
}

inline constexpr BinaryOperation add_operators[] ={
  {&BIN_OP_TESTS::num_int_64_bit, &OP::emit_add_64s},//ADD u64 or i64
};

inline constexpr BinaryOperation sub_operators[] ={
  {&BIN_OP_TESTS::num_int_64_bit, &OP::emit_sub_64s},//SUB u64 or i64
};

inline constexpr BinaryOperation mul_operators[] ={
  {&BIN_OP_TESTS::num_int_64_bit, &OP::emit_mul_64s},//MUL u64 or i64
};

inline constexpr BinaryOperation div_operators[] ={
  {&BIN_OP_TESTS::num_unsigned_int_64_bit, &OP::emit_div_u64s},//DIV u64
  {&BIN_OP_TESTS::num_signed_int_64_bit, &OP::emit_div_i64s},//DIV i64
};

inline constexpr BinaryOperation eq_operators[] ={
  {&BIN_OP_TESTS::eq_int_64_bit, &OP::emit_eq_64s},//EQ u64 or i64
  {&BIN_OP_TESTS::bools, &OP::emit_eq_64s},
};

inline constexpr BinaryOperation or_operators[] ={
  {&BIN_OP_TESTS::num_int_64_bit, &OP::emit_or_64s},//OR u64 or i64
  {&BIN_OP_TESTS::bools, &OP::emit_or_64s},
};

inline constexpr BinaryOperation and_operators[] ={
  {&BIN_OP_TESTS::num_int_64_bit, &OP::emit_and_64s},//AND u64 or i64
  {&BIN_OP_TESTS::bools, &OP::emit_and_64s},
};

inline constexpr BinaryOperation left_shift[] ={
  {&BIN_OP_TESTS::shift_64, &OP::emit_shift_l_64_by_8},
};

inline constexpr BinaryOperation right_shift[] ={
  {&BIN_OP_TESTS::u_shift_64, &OP::emit_shift_r_u64_by_8},
  {&BIN_OP_TESTS::s_shift_64, &OP::emit_shift_r_i64_by_8},
};

inline constexpr UnaryOperation neg_operators[] ={
  {&UN_OP_TESTS::signed_int_64_bit, &OP::emit_neg_i64},//ADD u64 or i64
};

inline constexpr UnaryOperation address_operators[] ={
  {&UN_OP_TESTS::address, &OP::emit_address},//ADD u64 or i64
};

inline constexpr UnaryOperation deref_operators[] ={
  {&UN_OP_TESTS::deref, &OP::emit_deref},//ADD u64 or i64
};

CompileCode find_binary_operator(Compiler* comp,
                                 ASTExpression* expr,
                                 const BinaryOperation* operations,
                                 size_t num_ops);

CompileCode find_unary_operator(Compiler* comp,
                                ASTExpression* expr,
                                const UnaryOperation* operations,
                                size_t num_ops);