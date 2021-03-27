#pragma once
#include "utility.h"
#include "strings.h"

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
  BINARY_OPERATOR,
  NAME /* = only in type check phase (converted to local or maybe others)*/,
  LOCAL /* = not in type check phase (converted from name) */,
  ENUM,
  VALUE,
  FUNCTION_CALL
};

enum struct BINARY_OPERATOR : uint8_t {
  ADD, SUB, MUL, DIV, LESSER, GREATER, EQUIVALENT, OR, AND
};

struct BinaryOperatorExpr {
  BINARY_OPERATOR op;
  ASTExpression* left = nullptr;
  ASTExpression* right = nullptr;

  ~BinaryOperatorExpr() {
    free(left);
    free(right);
  }
};

struct FunctionCallExpr {
  Array<ASTExpression> arguments;
  InternString function_name;

  Function* function = nullptr;
};

struct EnumValueExpr {
  const EnumValue* enum_value;
  InternString name;
};

struct ASTExpression {
  const Structure* type = nullptr;
  bool makes_call = false;
  bool call_leaf = false;

  EXPRESSION_TYPE expr_type = EXPRESSION_TYPE::UNKNOWN;
  union {
    char _dummy = '\0';
    BinaryOperatorExpr bin_op;
    FunctionCallExpr call;
    EnumValueExpr enum_value;
    InternString name;//e.g. local or global variable
    uint64_t value;
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
  STATEMENT_TYPE type;

  union {
    ASTExpression expression;
    ASTDeclaration declaration;
    ASTIfElse if_else;
    ASTBlock block;
  };

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