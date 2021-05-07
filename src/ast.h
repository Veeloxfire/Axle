#pragma once
#include "utility.h"
#include "strings.h"
#include "operators.h"
#include "type.h"

struct ASTExpression;
struct ASTStatement;

struct Structure;
struct Function;
struct EnumValue;

struct ASTName {
  InternString name ={};

  const Structure* type = nullptr;
};

enum struct TYPE_TYPE {
  NORMAL, ARRAY
};

struct ASTType;

struct ASTArrayType {
  ASTType* base = nullptr;
  ASTExpression* expr = nullptr;

  ~ASTArrayType() {
    free_destruct_single(base);
    free_destruct_single(expr);
  }
};

struct ASTType {
  TYPE_TYPE type_type;

  union {
    InternString name ={};
    ASTArrayType arr;
  };

  const Structure* type = nullptr;

  ~ASTType();
};

enum struct EXPRESSION_TYPE : uint8_t {
  UNKNOWN = 0,
  CAST,
  UNARY_OPERATOR,
  BINARY_OPERATOR,
  NAME /* = only in type check phase (converted to local or maybe others)*/,
  LOCAL /* = not in type check phase (converted from name) */,
  ENUM,
  VALUE,
  FUNCTION_CALL,
  ARRAY_EXPR,
  ASCII_STRING,
  INDEX,
};

struct BuildOptions;
struct State;

struct BinaryOperatorExpr {
  BINARY_OPERATOR op;

  ASTExpression* left = nullptr;
  ASTExpression* right = nullptr;

  BINARY_OPERATOR_FUNCTION emit;

  ~BinaryOperatorExpr() {
    free_destruct_single(left);
    free_destruct_single(right);
  }
};

struct FunctionCallExpr {
  Array<ASTExpression> arguments ={};
  InternString function_name ={};

  const Function* function = nullptr;
};

struct EnumValueExpr {
  const EnumValue* enum_value = nullptr;
  InternString name ={};
};

struct UnaryOperatorExpr {
  UNARY_OPERATOR op;
  ASTExpression* primary = nullptr;

  UNARY_OPERATOR_FUNCTION emit = nullptr;

  ~UnaryOperatorExpr() {
    free_destruct_single(primary);
  }
};

struct CastExpr {
  ASTType type;
  ASTExpression* expr = nullptr;
  CAST_FUNCTION emit = nullptr;

  ~CastExpr() {
    free_destruct_single(expr);
  }
};

struct IndexExpr {
  ASTExpression* expr = nullptr;
  ASTExpression* index = nullptr;

  ~IndexExpr() {
    free_destruct_single(expr);
    free_destruct_single(index);
  }
};

struct ValueExpr {
  uint64_t value = 0;
  InternString suffix ={};
};

struct ArrayExpr {
  Array<ASTExpression> elements;
};

struct ASTExpression {
  const Structure* type = nullptr;
  bool makes_call = false;
  bool call_leaf = false;
  bool compile_time_constant = false;

  void* const_val = nullptr;

  EXPRESSION_TYPE expr_type = EXPRESSION_TYPE::UNKNOWN;
  union {
    char _dummy = '\0';
    CastExpr cast;
    UnaryOperatorExpr un_op;
    BinaryOperatorExpr bin_op;
    FunctionCallExpr call;
    EnumValueExpr enum_value;
    InternString name;//e.g. local or global variable
    ValueExpr value;
    ArrayExpr array_expr;
    InternString ascii_string;
    IndexExpr index;
  };

  ASTExpression() = default;

  ASTExpression(ASTExpression&& a) noexcept {
    move_from(std::move(a));
  }
  ASTExpression& operator=(ASTExpression&& a) noexcept {
    this->~ASTExpression();
    move_from(std::move(a));
    return *this;
  }

  void move_from(ASTExpression&&) noexcept;
  ~ASTExpression();
};

struct ASTDeclaration {
  ASTExpression expression;
  ASTType type;
  InternString name;
};

enum struct STATEMENT_TYPE : uint8_t {
  EXPRESSION, DECLARATION, RETURN, IF_ELSE, BLOCK,
};

struct ASTIfElse {
  ASTExpression condition;
  ASTStatement* if_statement;
  ASTStatement* else_statement;

  ~ASTIfElse() {
    free_destruct_single(if_statement);
    free_destruct_single(else_statement);
  }
};

struct ASTBlock {
  Array<ASTStatement> block;
};

struct ASTStatement {
  STATEMENT_TYPE type ={};

  union {
    char _dummy ={};
    ASTExpression expression;
    ASTDeclaration declaration;
    ASTIfElse if_else;
    ASTBlock block;
  };

  ASTStatement() = default;
  ~ASTStatement();
};

struct ASTFunctionDeclaration {
  InternString name;
  ASTType return_type;
  Array<ASTDeclaration> parameters;

  ASTBlock body;
};

struct ASTStructureDeclaration {
  InternString name;
  Array<ASTDeclaration> elements;

  Structure* structure;
};

struct ASTFile {
  Array<ASTFunctionDeclaration> functions;
};

void print_ast(const ASTFile* file);