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
  InternString name;

  const Structure* type = nullptr;
};

struct ASTType {
  InternString name;
  const Structure* type = nullptr;
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
  FUNCTION_CALL
};

struct BuildOptions;
struct State;

struct BinaryOperatorExpr {
  BINARY_OPERATOR op;

  ASTExpression* left = nullptr;
  ASTExpression* right = nullptr;

  BINARY_OPERATOR_FUNCTION emit;

  ~BinaryOperatorExpr() {
    free(left);
    free(right);
  }
};

struct FunctionCallExpr {
  Array<ASTExpression> arguments;
  InternString function_name;

  const Function* function = nullptr;
};

struct EnumValueExpr {
  const EnumValue* enum_value;
  InternString name;
};

struct UnaryOperatorExpr {
  UNARY_OPERATOR op;
  ASTExpression* primary;

  UNARY_OPERATOR_FUNCTION emit;

  ~UnaryOperatorExpr() {
    free(primary);
  }
};

struct CastExpr {
  ASTType type;
  ASTExpression* expr;
  CAST_FUNCTION emit;

  ~CastExpr() {
    free(expr);
  }
};

struct ValueExpr {
  uint64_t value;
  InternString suffix;
};

struct ASTExpression {
  const Structure* type = nullptr;
  bool makes_call = false;
  bool call_leaf = false;
  bool compile_time_constant = false;

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
  };

  ASTExpression() : _dummy() {}

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
    free(if_statement);
    free(else_statement);
  }
};

struct ASTBlock {
  Array<ASTStatement> block;
};

struct ASTStatement {
  STATEMENT_TYPE type ={};

  union {
    char _dummy;
    ASTExpression expression;
    ASTDeclaration declaration;
    ASTIfElse if_else;
    ASTBlock block;
  };

  ASTStatement() : _dummy() {}
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