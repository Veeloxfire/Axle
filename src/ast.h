#pragma once
#include "utility.h"
#include "strings.h"

#include "type.h"

struct ASTExpression;
struct ASTStatement;

struct ASTName {
  InternString name;

  const Structure* type = nullptr;
  size_t value_index = 0;
};

struct ASTValue {
  uint64_t val;

  const Structure* type = nullptr;
  size_t value_index = 0;
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
  ASTExpression* left;
  ASTExpression* right;
};

struct FunctionCallExpr {
  Array<ASTExpression> arguments;
  InternString function_name;

  Function* function = nullptr;
};

struct EnumValue;

struct EnumValueExpr {
  const EnumValue* enum_value;
  InternString name;
};

struct ASTExpression {
  const Structure* type = nullptr;
  size_t value_index = 0;
  bool makes_call = false;
  bool call_leaf = false;

  EXPRESSION_TYPE expr_type = EXPRESSION_TYPE::UNKNOWN;
  union {
    char _dummy = '\0';
    BinaryOperatorExpr bin_op;
    FunctionCallExpr call;
    EnumValueExpr enum_value;
    InternString name;//e.g. local or global variable - only in type stage
    Location local;
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
  ASTType type;
  InternString name;
  ASTExpression expression;
};

enum struct STATEMENT_TYPE : uint8_t {
  EXPRESSION, DECLARATION, RETURN, IF_ELSE, BLOCK,
};

struct ASTIfElse {
  ASTExpression condition;
  ASTStatement* if_statement;
  ASTStatement* else_statement;
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

struct ASTFunctionSignature {
  ASTType return_type;
  Array<ASTDeclaration> parameters;

  const FunctionSignature* signature;
};

struct ASTFunctionDeclaration {
  InternString name;
  ASTFunctionSignature signature;
  ASTBlock body;

  Function* function;
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