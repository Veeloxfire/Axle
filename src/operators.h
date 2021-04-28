#pragma once
#include "utility.h"

#define BIN_OP_INCS \
MODIFY(ADD, "+")\
MODIFY(SUB, "-")\
MODIFY(MUL, "*")\
MODIFY(DIV, "/")\
MODIFY(LESSER, "<")\
MODIFY(GREATER, ">")\
MODIFY(EQUIVALENT, "==")\
MODIFY(OR, "|")\
MODIFY(AND, "&")

enum struct BINARY_OPERATOR : uint8_t {
#define MODIFY(name, str) name,
  BIN_OP_INCS
#undef MODIFY
};

namespace BINARY_OP_STRING {
#define MODIFY(name, str) inline constexpr auto name = str;
  BIN_OP_INCS;
#undef MODIFY

  constexpr const char* get(BINARY_OPERATOR op) noexcept {
    switch (op)
    {
    #define MODIFY(name, str) case BINARY_OPERATOR:: ## name : return name;
      BIN_OP_INCS;
    #undef MODIFY

      default: return "UNKNOWN OPERATOR";
    }
  }
}

#define UN_OP_INCS \
MODIFY(NEG, "-")

enum struct UNARY_OPERATOR : uint8_t {
#define MODIFY(name, str) name,
  UN_OP_INCS
#undef MODIFY
};

namespace UNARY_OP_STRING {
#define MODIFY(name, str) inline constexpr auto name = str;
  UN_OP_INCS;
#undef MODIFY

  constexpr const char* get(UNARY_OPERATOR op) noexcept {
    switch (op)
    {
    #define MODIFY(name, str) case UNARY_OPERATOR:: ## name : return name;
      UN_OP_INCS;
    #undef MODIFY

      default: return "UNKNOWN OPERATOR";
    }
  }
}

struct Types;
enum struct CompileCode : uint8_t;
struct Structure;
struct BuildOptions;
struct State;
struct CodeBlock;
struct ASTExpression;

using BINARY_OPERATOR_FUNCTION = FUNCTION_PTR<
  void,
  const BuildOptions*,
  State*, CodeBlock*,
  ValueIndex, ValueIndex
>;

using UNARY_OPERATOR_FUNCTION = FUNCTION_PTR<
  void,
  const BuildOptions*,
  State*, CodeBlock*,
  ValueIndex
>;

void emit_add_64s(const BuildOptions*,
                  State*, CodeBlock*,
                  ValueIndex, ValueIndex);

void emit_sub_64s(const BuildOptions*,
                  State*, CodeBlock*,
                  ValueIndex, ValueIndex);

void emit_mul_64s(const BuildOptions*,
                  State*, CodeBlock*,
                  ValueIndex, ValueIndex);

void emit_div_u64s(const BuildOptions*,
                   State*, CodeBlock*,
                   ValueIndex, ValueIndex);

void emit_div_i64s(const BuildOptions*,
                   State*, CodeBlock*,
                   ValueIndex, ValueIndex);

void emit_eq_64s(const BuildOptions*,
                 State*, CodeBlock*,
                 ValueIndex, ValueIndex);

void emit_or_64s(const BuildOptions*,
                 State*, CodeBlock*,
                 ValueIndex, ValueIndex);

void emit_and_64s(const BuildOptions*,
                  State*, CodeBlock*,
                  ValueIndex, ValueIndex);

void emit_neg_i64(const BuildOptions*,
                  State*, CodeBlock*,
                  ValueIndex);

struct BinaryOperation {
  FUNCTION_PTR<const Structure*, const Types*, const Structure*, const Structure*> test;
  BINARY_OPERATOR_FUNCTION func;
};

struct UnaryOperation {
  FUNCTION_PTR<const Structure*, const Types*, const Structure*> test;
  UNARY_OPERATOR_FUNCTION func;
};

namespace BIN_OP_TESTS {
  const Structure* num_int_64_bit(const Types* types, const Structure* left, const Structure* right);
  const Structure* num_signed_int_64_bit(const Types* types, const Structure* left, const Structure* right);
  const Structure* num_unsigned_int_64_bit(const Types* types, const Structure* left, const Structure* right);

  const Structure* eq_int_64_bit(const Types* types, const Structure* left, const Structure* right);

  const Structure* bools(const Types* types, const Structure* left, const Structure* right);
}

namespace UN_OP_TESTS {
  const Structure* signed_int_64_bit(const Types* types, const Structure* s);
}

inline constexpr BinaryOperation add_operators[] ={
  {&BIN_OP_TESTS::num_int_64_bit, &emit_add_64s},//ADD u64 or i64
};

inline constexpr BinaryOperation sub_operators[] ={
  {&BIN_OP_TESTS::num_int_64_bit, &emit_sub_64s},//SUB u64 or i64
};

inline constexpr BinaryOperation mul_operators[] ={
  {&BIN_OP_TESTS::num_int_64_bit, &emit_mul_64s},//MUL u64 or i64
};

inline constexpr BinaryOperation div_operators[] ={
  {&BIN_OP_TESTS::num_unsigned_int_64_bit, &emit_div_u64s},//DIV u64
  {&BIN_OP_TESTS::num_signed_int_64_bit, &emit_div_i64s},//DIV i64
};

inline constexpr BinaryOperation eq_operators[] ={
  {&BIN_OP_TESTS::eq_int_64_bit, &emit_eq_64s},//EQ u64 or i64
  {&BIN_OP_TESTS::bools, &emit_eq_64s},
};

inline constexpr BinaryOperation or_operators[] ={
  {&BIN_OP_TESTS::num_int_64_bit, &emit_or_64s},//OR u64 or i64
  {&BIN_OP_TESTS::bools, &emit_or_64s},
};

inline constexpr BinaryOperation and_operators[] ={
  {&BIN_OP_TESTS::num_int_64_bit, &emit_and_64s},//AND u64 or i64
  {&BIN_OP_TESTS::bools, &emit_and_64s},
};

inline constexpr UnaryOperation neg_operators[] ={
  {&UN_OP_TESTS::signed_int_64_bit, &emit_neg_i64},//ADD u64 or i64
};

CompileCode find_binary_operator(const Types* types,
                                 ASTExpression* expr,
                                 const BinaryOperation* operations,
                                 size_t num_ops);

CompileCode find_unary_operator(const Types* types,
                                ASTExpression* expr,
                                const UnaryOperation* operations,
                                size_t num_ops);