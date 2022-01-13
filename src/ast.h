#pragma once
#include "utility.h"
#include "strings.h"
#include "operators.h"
#include "type.h"
#include "parser.h"
#include "comp_utilities.h"

struct Type;
struct Function;
struct EnumValue;
struct State;
struct Global;
struct AST;

using AST_LOCAL = AST*;

struct AST_ARR {
  AST_LINKED* start = 0;
  usize count;
};

struct AST_LINKED {
  AST_LOCAL curr = 0;
  AST_LINKED* next = 0;
};

//Anything allocated via this structure will not be destroyed
struct AstStorage {
  constexpr static usize BLOCK_SIZE = 1024 * 8;
  u8 ast_mem[BLOCK_SIZE];
  usize top;

  u8* push_alloc_bytes(usize size, usize align);

  template<typename T>
  T* push() {
    T* ast = (T*)push_alloc_bytes(sizeof(T), alignof(T));
    new (ast) T();
    return ast;
  }
};

enum struct AST_TYPE : u8 {
  NAMED_TYPE,
  ARRAY_TYPE,
  PTR_TYPE,
  LAMBDA_TYPE,
  TUPLE_TYPE,
  CAST,
  UNARY_OPERATOR,
  BINARY_OPERATOR,
  IDENTIFIER_EXPR,
  NUMBER,
  FUNCTION_CALL,
  TUPLE_LIT,
  ARRAY_EXPR,
  ASCII_STRING,
  ASCII_CHAR,
  INDEX_EXPR,
  MEMBER_ACCESS,
  LAMBDA,
  STRUCT,
  DECL,
  TYPED_NAME,
  ASSIGN,
  BLOCK,
  IF_ELSE,
  WHILE,
  RETURN,
  FUNCTION_SIGNATURE,
  IMPORT,
};

struct AST {
  AST_TYPE ast_type;
};

struct ASTTypeBase : public AST {
  Type type ={};
  Span span ={};
};

struct ASTSpanned : public AST {
  Span span;
};

struct ASTExpressionBase : public AST {
  uint8_t valid_rvts = ALL_RVTS;

  bool makes_call = false;
  bool call_leaf = false;

  META_FLAGS meta_flags;
  Type type;

  uint8_t* const_val = nullptr;

  Span span;
};

struct ASTNamedType : public ASTTypeBase {
  const InternString* name ={};
};

struct ASTArrayType : public ASTTypeBase {
  AST_LOCAL base = 0;
  AST_LOCAL expr = 0;
};

struct ASTPtrType : public ASTTypeBase {
  AST_LOCAL base = 0;
};

struct ASTLambdaType : public ASTTypeBase {
  AST_LOCAL ret = 0;
  AST_ARR args ={};
};

struct ASTTupleType : public ASTTypeBase {
  AST_ARR types ={};
};

struct ASTBinaryOperatorExpr : public ASTExpressionBase {
  BINARY_OPERATOR op;

  AST_LOCAL left = 0;
  AST_LOCAL right = 0;

  BinOpEmitInfo info = {};
  BINARY_OPERATOR_FUNCTION emit = nullptr;
};

struct ASTTupleLitExpr : public ASTExpressionBase {
  AST_ARR elements = {};
};

struct ASTFunctionCallExpr : public ASTExpressionBase {
  AST_ARR arguments ={};

  const InternString* function_name = nullptr;
  const SignatureStructure* sig = nullptr;
  //const Function* function = nullptr;
};

struct ASTUnaryOperatorExpr : public ASTExpressionBase {
  UNARY_OPERATOR op;
  AST_LOCAL expr = 0;

  UNARY_OPERATOR_FUNCTION emit = nullptr;
};

struct ASTCastExpr : public ASTExpressionBase {
  AST_LOCAL type = 0;
  AST_LOCAL expr = 0;
  CAST_FUNCTION emit = nullptr;
};

struct ASTIndexExpr : public ASTExpressionBase {
  AST_LOCAL expr = 0;
  AST_LOCAL index = 0;
};

struct ASTNumber : public ASTExpressionBase {
  uint64_t value = 0;
  const InternString* suffix =nullptr;
};

struct ASTArrayExpr : public ASTExpressionBase {
  AST_ARR elements ={};
};

struct ASTIdentifier : public ASTExpressionBase {
  const InternString* name;
};

struct ASTMemberAccessExpr : public ASTExpressionBase {
  AST_LOCAL expr = 0;

  uint32_t offset = 0;
  const InternString* name = nullptr;
};

struct ASTAsciiString : public ASTExpressionBase {
  const InternString* string;
};

struct ASTAsciiChar : public ASTExpressionBase {
  char character;
};

//struct ASTCompIntrinsic : public ASTExpressionBase {
//  const InternString* name;
//};

struct ASTBlock : public AST {
  AST_ARR block ={};
};

struct ASTDecl : public AST {
  const InternString* name = nullptr;
  bool compile_time_const = false;
  Type type ={};

  //Only used in locals
  size_t local_index = 0;

  AST_LOCAL type_ast = 0;
  AST_LOCAL expr = 0;

  Span span ={};
};

struct ASTFuncSig : public AST {
  FunctionSignature* sig = nullptr;
  const CallingConvention* convention = nullptr;

  AST_LOCAL return_type = 0;
  AST_ARR parameters ={};

  Span span ={};
};

struct ASTLambda : public AST {
  Function* function = nullptr;

  AST_LOCAL sig ={};
  AST_LOCAL body ={};
};

struct ASTTypedName : public AST {
  AST_LOCAL type ={};
  const InternString* name = nullptr;
};

struct ASTStructBody : public AST {
  CompilationUnit* compilation_unit;
  Type type{};
  Span span ={};
  AST_ARR elements = {};
};

struct ASTWhile : public ASTSpanned {
  AST_LOCAL condition = 0;
  AST_LOCAL statement = 0;
};

struct ASTIfElse : public ASTSpanned {
  AST_LOCAL condition = 0;
  AST_LOCAL if_statement = 0;
  AST_LOCAL else_statement = 0;
};

struct ASTReturn : public ASTSpanned {
  AST_LOCAL expr = 0;
};

struct ASTAssign : public ASTSpanned {
  AST_LOCAL assign_to = 0;
  AST_LOCAL value = 0;
};

struct ASTImport : public AST {
  AST_LOCAL expr_location;

  Span span ={};
};

struct FileAST {
  FileLocation file_loc ={};
  NamespaceIndex namespace_index = {};

  //can be import or decl
  AST_ARR contents = {};
};

struct Printer {
  size_t tabs = 0;

  void newline() const;
};

void print_ast(const Compiler* comp, const AstStorage* store, const FileAST* file);
void print_ast_expression(Printer* printer, const AstStorage* store, AST_LOCAL expr);