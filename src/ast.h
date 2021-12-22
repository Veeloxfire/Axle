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
struct ASTLambda;
struct ASTStructBody;

struct Type;
struct Function;
struct EnumValue;
struct State;
struct Global;

struct ASTName {
  const InternString* name ={};
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

struct ASTLambdaType {
  //Last one is the return type
  Array<ASTType> types ={};
};

struct ASTTupleType {
  Array<ASTType> types ={};
};

#define TYPE_TYPE_MOD \
MOD(NORMAL, name) \
MOD(ARRAY, arr) \
MOD(PTR, ptr) \
MOD(TUPLE, tuple) \
MOD(LAMBDA, lambda)

enum struct TYPE_TYPE {
  UNKNOWN = 0,

#define MOD(name, t) name,
  TYPE_TYPE_MOD
#undef MOD
};

struct ASTType {
  TYPE_TYPE type_type = TYPE_TYPE::UNKNOWN;
  Type type ={};
  Span span ={};

  union {
    char _dummy = '\0';
    const InternString* name;
    ASTArrayType arr;
    ASTPtrType ptr;
    ASTTupleType tuple;
    ASTLambdaType lambda;
  };

  ASTType() = default;

  ASTType(ASTType&& a) noexcept {
    move_from(std::move(a));
  }

  ASTType& operator=(ASTType&& a) noexcept {
    this->~ASTType();
    move_from(std::move(a));
    return *this;
  }

  void move_from(ASTType&&) noexcept;

  void set_union(TYPE_TYPE);
  void destruct_union();
  ~ASTType();
};

struct BinaryOperatorExpr {
  BINARY_OPERATOR op;

  ASTExpression* left = nullptr;
  ASTExpression* right = nullptr;

  BinOpEmitInfo info = {};
  BINARY_OPERATOR_FUNCTION emit = nullptr;

  ~BinaryOperatorExpr() {
    free_destruct_single(left);
    free_destruct_single(right);
  }
};

struct TupleLitExpr {
  Array<ASTExpression> elements = {};
};

struct FunctionCallExpr {
  Array<ASTExpression> arguments ={};

  const InternString* function_name = nullptr;
  const SignatureStructure* sig = nullptr;
  //const Function* function = nullptr;
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

  inline CastExpr& operator=(CastExpr&& c) noexcept {
    type = std::move(c.type);
    expr = std::exchange(c.expr, nullptr);
    emit = std::exchange(c.emit, nullptr);
    return *this;
  }

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

struct MemberAccessExpr {
  ASTExpression* expr = nullptr;

  uint32_t offset = 0;
  const InternString* name = nullptr;

  ~MemberAccessExpr() {
    free_destruct_single(expr);
  }
};

struct LambdaExpr {
  ASTLambda* lambda;

  ~LambdaExpr() {
    free_destruct_single(lambda);
  }
};

struct StructExpr {
  ASTStructBody* body;

  ~StructExpr() {
    free_destruct_single(body);
  }
};

struct CompIntrinsicExpr {
  const InternString* name;
};

#define EXPRESSION_TYPE_MODIFY \
MODIFY(CAST, cast) \
MODIFY(UNARY_OPERATOR, un_op) \
MODIFY(BINARY_OPERATOR, bin_op) \
MODIFY(NAME, name) \
MODIFY(LOCAL, local) \
MODIFY(GLOBAL, global) \
MODIFY(ENUM, enum_value) \
MODIFY(VALUE, value) \
MODIFY(FUNCTION_CALL, call) \
MODIFY(TUPLE_LIT, tuple_lit) \
MODIFY(ARRAY_EXPR, array_expr) \
MODIFY(ASCII_STRING, ascii_string) \
MODIFY(ASCII_CHAR, ascii_char) \
MODIFY(INDEX, index) \
MODIFY(NULLPTR, _dummy) \
MODIFY(MEMBER, member) \
MODIFY(LAMBDA, lambda) \
MODIFY(STRUCT, struct_body) \
MODIFY(COMP_INTRINSIC, comp_intrinsic)

enum struct EXPRESSION_TYPE : uint8_t {
  UNKNOWN = 0,
#define MODIFY(name, expr_name) name, 
  EXPRESSION_TYPE_MODIFY
#undef MODIFY
};

struct ASTExpression {
  uint8_t valid_rvts = ALL_RVTS;

  bool makes_call = false;
  bool call_leaf = false;

  META_FLAGS meta_flags;
  Type type;

  uint8_t* const_val = nullptr;

  Span span;

  EXPRESSION_TYPE expr_type = EXPRESSION_TYPE::UNKNOWN;
  union {
    char _dummy = '\0';
    CastExpr cast;
    UnaryOperatorExpr un_op;
    BinaryOperatorExpr bin_op;
    FunctionCallExpr call;
    TupleLitExpr tuple_lit;
    EnumValueExpr enum_value;
    const InternString* name;
    size_t local;
    const Global* global;
    ValueExpr value;
    char ascii_char;
    ArrayExpr array_expr;
    const InternString* ascii_string;
    IndexExpr index;
    MemberAccessExpr member;
    LambdaExpr lambda;
    StructExpr struct_body;
    CompIntrinsicExpr comp_intrinsic;
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
MOD(EXPRESSION, expression) \
MOD(LOCAL, local) \
MOD(RETURN, expression) \
MOD(IF_ELSE, if_else) \
MOD(WHILE, while_loop) \
MOD(BLOCK, block) \
MOD(ASSIGN, assign) \

enum struct STATEMENT_TYPE : uint8_t {
  UNKNOWN = 0,

#define MOD(name, expr_name) name,
  MOD_STATEMENTS
#undef MOD
};

struct ASTBlock {
  Array<ASTStatement> block ={};
};

struct ASTDecl {
  const InternString* name = nullptr;
  bool compile_time_const = false;
  Type type ={};

  //Only used in locals
  size_t local_index = 0;

  ASTType* type_ast ={};
  ASTExpression* expr ={};

  Span span ={};

  ~ASTDecl() {
    free_destruct_single(type_ast);
    free_destruct_single(expr);
  }
};

struct ASTFuncSig {
  FunctionSignature* sig = nullptr;
  const CallingConvention* convention = nullptr;

  ASTType return_type ={};
  Array<ASTDecl> parameters ={};

  Span signature_span ={};
};

struct ASTLambda {
  Function* function = nullptr;

  ASTFuncSig sig ={};
  ASTBlock body ={};
};

struct ASTTypedName {
  ASTType type ={};
  const InternString* name = nullptr;
};

struct ASTStructBody {
  CompilationUnit* compilation_unit;
  Type type{};
  Span span ={};
  Array<ASTTypedName> elements = {};
};

struct ASTWhile {
  ASTExpression* condition ={};
  ASTStatement* statement = nullptr;

  ~ASTWhile() {
    free_destruct_single(condition);
    free_destruct_single(statement);
  }
};

struct ASTIfElse {
  ASTExpression* condition ={};
  ASTStatement* if_statement = nullptr;
  ASTStatement* else_statement = nullptr;

  ~ASTIfElse() {
    free_destruct_single(condition);
    free_destruct_single(if_statement);
    free_destruct_single(else_statement);
  }
};

struct ASTAssign {
  ASTExpression* assign_to ={};
  ASTExpression* value ={};

  ~ASTAssign() {
    free_destruct_single(assign_to);
    free_destruct_single(value);
  }
};

struct ExprHolder {
  ASTExpression* expr = nullptr;

  ~ExprHolder() {
    free_destruct_single(expr);
  }
};


struct ASTStatement {
  STATEMENT_TYPE type = STATEMENT_TYPE::UNKNOWN;
  Span span;

  union {
    char _dummy ={};
    ExprHolder expression;
    ASTDecl local;
    ASTWhile while_loop;
    ASTIfElse if_else;
    ASTBlock block;
    ASTAssign assign;
  };

  void set_union(STATEMENT_TYPE st) noexcept;
  void destruct_union() noexcept;

  ASTStatement() = default;
  ~ASTStatement();
};

struct ASTImport {
  ASTExpression expr_location;

  //If empty then import all
  Array<ASTDecl> imports;

  Span span ={};
};

struct ASTFile {
  FileLocation file_loc ={};
  NamespaceIndex namespace_index = {};

  Array<ASTImport> imports = {};
  Array<ASTDecl> decls = {};
};

struct ScopeView {
  ASTStatement* i;
  const ASTStatement* end;
};

struct Printer {
  size_t tabs = 0;

  void newline() const;
};

void print_ast(const Compiler* comp, const ASTFile* file);
void print_ast_expression(Printer* printer, const ASTExpression* expr);