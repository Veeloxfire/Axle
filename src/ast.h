#pragma once
#include "utility.h"
#include "strings.h"
#include "operators.h"
#include "type.h"
#include "parser.h"
#include "comp_utilities.h"

struct ASTType;
struct ASTExpression;
struct ASTStatement;

struct Structure;
struct Function;
struct EnumValue;
struct State;

struct ASTName {
  const InternString* name ={};
  const Structure* type = nullptr;
};

enum struct TYPE_TYPE {
  NORMAL, ARRAY, PTR
};

struct ASTArrayType {
  ASTType* base = nullptr;
  ASTExpression* expr = nullptr;

  ~ASTArrayType() {
    free_destruct_single(base);
    free_destruct_single(expr);
  }
};

struct ASTPtrType {
  ASTType* base = nullptr;

  ~ASTPtrType() {
    free_destruct_single(base);
  }
};

struct ASTType {
  TYPE_TYPE type_type;
  Span span;

  union {
    const InternString* name ={};
    ASTArrayType arr;
    ASTPtrType ptr;
  };

  const Structure* type = nullptr;

  void set_union(TYPE_TYPE);
  void destruct_union();
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
  NULLPTR,
};

struct BinaryOperatorExpr {
  BINARY_OPERATOR op;

  ASTExpression* left = nullptr;
  ASTExpression* right = nullptr;

  BINARY_OPERATOR_FUNCTION emit = nullptr;

  ~BinaryOperatorExpr() {
    free_destruct_single(left);
    free_destruct_single(right);
  }
};

struct FunctionCallExpr {
  Array<ASTExpression> arguments ={};

  const InternString* function_name = nullptr;
  const FunctionBase* function = nullptr;
};

struct EnumValueExpr {
  const EnumValue* enum_value = nullptr;
  const InternString* name ={};
};

struct UnaryOperatorExpr {
  UNARY_OPERATOR op;
  ASTExpression* expr = nullptr;

  UNARY_OPERATOR_FUNCTION emit = nullptr;

  ~UnaryOperatorExpr() {
    free_destruct_single(expr);
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
  const InternString* suffix =nullptr;
};

struct ArrayExpr {
  Array<ASTExpression> elements;
};

struct ASTExpression {
  const Structure* type = nullptr;

  ASTExpression* first = nullptr;
  ASTExpression* next  = nullptr;

  uint8_t valid_rvts = ALL_RVTS;

  bool makes_call = false;
  bool call_leaf = false;
  bool comptime_eval = false;

  uint8_t* const_val = nullptr;

  Span span;

  EXPRESSION_TYPE expr_type = EXPRESSION_TYPE::UNKNOWN;
  union {
    char _dummy = '\0';
    CastExpr cast;
    UnaryOperatorExpr un_op;
    BinaryOperatorExpr bin_op;
    FunctionCallExpr call;
    EnumValueExpr enum_value;
    const InternString* name;
    size_t local;
    ValueExpr value;
    ArrayExpr array_expr;
    const InternString* ascii_string;
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

  void set_union(EXPRESSION_TYPE et) noexcept;
  void destruct_union() noexcept;
  ~ASTExpression();
};

#define MOD_STATEMENTS \
MOD(UNKNOWN) \
MOD(EXPRESSION) \
MOD(LOCAL) \
MOD(RETURN) \
MOD(IF_ELSE) \
MOD(BLOCK)

enum struct STATEMENT_TYPE : uint8_t {
#define MOD(name) name,
  MOD_STATEMENTS
#undef MOD
};

struct ASTLocal {
  ASTExpression expression ={};
  ASTType type ={};
  const InternString* name = nullptr;
  size_t local_index = 0;
};

struct ASTIfElse {
  ASTExpression condition ={};
  ASTStatement* if_statement = nullptr;
  ASTStatement* else_statement = nullptr;

  ~ASTIfElse() {
    free_destruct_single(if_statement);
    free_destruct_single(else_statement);
  }
};

struct ASTBlock {
  Array<ASTStatement> block ={};
};

struct ASTStatement {
  STATEMENT_TYPE type = STATEMENT_TYPE::UNKNOWN;
  Span span;

  union {
    char _dummy ={};
    ASTExpression expression;
    ASTLocal local;
    ASTIfElse if_else;
    ASTBlock block;
  };

  void set_union(STATEMENT_TYPE st) noexcept;
  void destruct_union() noexcept;

  ASTStatement() = default;
  ~ASTStatement();
};

struct ASTFunctionSignature {
  const InternString* convention = nullptr;

  const InternString* name = nullptr;
  ASTType return_type;
  Array<ASTLocal> parameters;

  Span signature_span ={};
};

struct ASTFunctionDeclaration {
  ASTFunctionSignature signature ={};
  ASTBlock body;
};

//struct ASTStructureDeclaration {
//  const InternString* name = nullptr;
//  Array<ASTDeclaration> elements;
//
//  Structure* structure;
//};

struct ASTImport {
  const InternString* relative_path = nullptr;
  Span span ={};

  FileLocation loc ={};
};

enum struct FILE_TYPE : uint8_t {
  NONE, SOURCE, DLL_HEADER
};

struct ASTFileHeader {
  bool is_dll_header;
  ASTImport dll_header;
};

struct ASTFile {
  FileLocation file_loc ={};
  NamespaceIndex namespace_index = {};

  ASTFileHeader header = {};

  Array<ASTImport> imports = {};
  Array<ASTFunctionDeclaration> functions ={};
};

struct ScopeView {
  ASTStatement* i;
  const ASTStatement* end;
};

void print_ast(const ASTFile* file);
void print_ast_expression(const ASTExpression* expr);

void build_expression_linked_list(ASTBlock* scope);
void build_expression_linked_list(ASTExpression* expr);