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
struct Namespace;

using AST_LOCAL = AST*;

struct AST_LINKED {
  AST_LOCAL curr = 0;
  AST_LINKED* next = 0;
};

struct AST_ARR {
  AST_LINKED* start = 0;
  usize count;
};

#define FOR_AST(arr, it) \
for(auto [_l, it] = _start_ast_iterate(arr); _l; (_l = _l->next, _l && (it = _l->curr)))

struct _AST_ITERATE_HOLDER {
  AST_LINKED* l;
  AST_LOCAL loc;
};

constexpr inline _AST_ITERATE_HOLDER _start_ast_iterate(const AST_ARR& a) {
  if (a.start == nullptr) {
    return { nullptr, 0 };
  }
  else {
    return {
      a.start,
      a.start->curr,
    };
  }
}

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
  LOCAL,
  GLOBAL,
  NUMBER,
  FUNCTION_CALL,
  TUPLE_LIT,
  ARRAY_EXPR,
  ASCII_STRING,
  ASCII_CHAR,
  INDEX_EXPR,
  MEMBER_ACCESS,
  LAMBDA,
  LAMBDA_EXPR,
  STRUCT,
  STRUCT_EXPR,
  DECL,
  TYPED_NAME,
  ASSIGN,
  BLOCK,
  IF_ELSE,
  WHILE,
  RETURN,
  FUNCTION_SIGNATURE,
  IMPORT,
  LIB_IMPORT,
};

constexpr bool valid_type_node(AST_TYPE t) {
  switch (t) {
    case AST_TYPE::NAMED_TYPE:
    case AST_TYPE::ARRAY_TYPE:
    case AST_TYPE::PTR_TYPE:
    case AST_TYPE::LAMBDA_TYPE:
    case AST_TYPE::TUPLE_TYPE: return true;
    default: return false;
  }
}

struct AST {
  META_FLAGS meta_flags = 0;
  uint8_t valid_rvts = ALL_RVTS;

  void* value = nullptr;

  AST_TYPE ast_type;
  Type node_type ={};
  Span node_span ={};
};

struct ASTNamedType : public AST {
  const InternString* name ={};
};

struct ASTArrayType : public AST {
  AST_LOCAL base = 0;
  AST_LOCAL expr = 0;
};

struct ASTPtrType : public AST {
  AST_LOCAL base = 0;
};

struct ASTLambdaType : public AST {
  AST_LOCAL ret = 0;
  AST_ARR args ={};
};

struct ASTTupleType : public AST {
  AST_ARR types ={};
};

struct ASTBinaryOperatorExpr : public AST {
  BINARY_OPERATOR op;

  AST_LOCAL left = 0;
  AST_LOCAL right = 0;

  BinOpEmitInfo info = {};
  BINARY_OPERATOR_FUNCTION emit = nullptr;
};

struct ASTTupleLitExpr : public AST {
  const InternString* name;
  AST_ARR elements = {};
};

struct ASTFunctionCallExpr : public AST {
  AST_ARR arguments ={};

  const InternString* function_name = nullptr;
  Function* func = nullptr;
};

struct ASTUnaryOperatorExpr : public AST {
  UNARY_OPERATOR op;
  AST_LOCAL expr = 0;

  UNARY_OPERATOR_FUNCTION emit = nullptr;
};

struct ASTCastExpr : public AST {
  AST_LOCAL type = 0;
  AST_LOCAL expr = 0;
  CAST_FUNCTION emit = nullptr;
};

struct ASTIndexExpr : public AST {
  AST_LOCAL expr = 0;
  AST_LOCAL index = 0;
};

struct ASTNumber : public AST {
  uint64_t value = 0;
  const InternString* suffix =nullptr;
};

struct ASTArrayExpr : public AST {
  AST_ARR elements ={};
};

struct ASTIdentifier : public AST {
  const InternString* name;
};

struct ASTMemberAccessExpr : public AST {
  AST_LOCAL expr = 0;

  uint32_t offset = 0;
  const InternString* name = nullptr;
};

struct ASTAsciiString : public AST {
  const InternString* string;
};

struct ASTAsciiChar : public AST {
  char character;
};

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
};

struct ASTFuncSig : public AST {
  FunctionSignature* sig = nullptr;
  const CallingConvention* convention = nullptr;

  AST_LOCAL return_type = 0;
  AST_ARR parameters ={};
};

struct ASTLambdaExpr : public AST {
  AST_LOCAL lambda;
};

struct ASTStructExpr : public AST {
  AST_LOCAL struct_body;
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
  AST_ARR elements = {};
};

struct ASTWhile : public AST {
  AST_LOCAL condition = 0;
  AST_LOCAL statement = 0;
};

struct ASTIfElse : public AST {
  AST_LOCAL condition = 0;
  AST_LOCAL if_statement = 0;
  AST_LOCAL else_statement = 0;
};

struct ASTReturn : public AST {
  AST_LOCAL expr = 0;
};

struct ASTAssign : public AST {
  AST_LOCAL assign_to = 0;
  AST_LOCAL value = 0;
};

struct ASTImport : public AST {
  AST_LOCAL expr_location;
};

struct ASTLibImport : public AST {
  const InternString* lib_file;
  const InternString* name;
  size_t data_holder_index;
};

struct FileAST {
  AST_ARR top_level;

  Namespace* ns;
  FileLocation file_loc;
};

struct Printer {
  size_t tabs = 0;

  void newline() const;
};

void print_full_ast(const FileAST* file);
void print_full_ast(AST_LOCAL expr);