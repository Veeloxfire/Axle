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

#define AST_TYPE_MOD \
MOD(NAMED_TYPE) \
MOD(ARRAY_TYPE) \
MOD(PTR_TYPE) \
MOD(LAMBDA_TYPE) \
MOD(TUPLE_TYPE) \
MOD(CAST) \
MOD(UNARY_OPERATOR) \
MOD(BINARY_OPERATOR) \
MOD(IDENTIFIER_EXPR) \
MOD(LOCAL_DECL) \
MOD(GLOBAL_DECL) \
MOD(NUMBER) \
MOD(FUNCTION_CALL) \
MOD(TUPLE_LIT) \
MOD(ARRAY_EXPR) \
MOD(ASCII_STRING) \
MOD(ASCII_CHAR) \
MOD(INDEX_EXPR) \
MOD(MEMBER_ACCESS) \
MOD(LAMBDA) \
MOD(LAMBDA_EXPR) \
MOD(STRUCT) \
MOD(STRUCT_EXPR) \
MOD(TYPED_NAME) \
MOD(ASSIGN) \
MOD(BLOCK) \
MOD(IF_ELSE) \
MOD(WHILE) \
MOD(RETURN) \
MOD(FUNCTION_SIGNATURE) \
MOD(IMPORT) \
MOD(STATIC_LINK)

enum struct AST_TYPE : u8 {
  INVALID = 0,
#define MOD(n) n,
  AST_TYPE_MOD
#undef MOD
};

constexpr const char* ast_type_string(AST_TYPE ty) {
  switch (ty) {
#define MOD(n) case AST_TYPE :: n : return #n ;
    AST_TYPE_MOD;
#undef MOD
    default: return "invalid-ast-type";
  }
}

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
  Type node_type = {};
  Span node_span = {};
};

struct ASTNamedType : public AST {
  const InternString* name = {};
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
  AST_ARR args = {};
};

struct ASTTupleType : public AST {
  AST_ARR types = {};
};

struct ASTBinaryOperatorExpr : public AST {
  BINARY_OPERATOR op;

  AST_LOCAL left = 0;
  AST_LOCAL right = 0;

  BinOpEmitInfo emit_info = {};
};

struct ASTTupleLitExpr : public AST {
  const InternString* name;

  Type named_type;
  AST_ARR elements = {};
};

struct ASTFunctionCallExpr : public AST {
  AST_ARR arguments = {};

  const InternString* function_name = nullptr;
  const SignatureStructure* sig = nullptr;
  usize label = 0;
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
  uint64_t num_value = 0;
  const InternString* suffix = nullptr;
};

struct ASTArrayExpr : public AST {
  AST_ARR elements = {};
};

struct ASTIdentifier : public AST {
  enum TYPE {
    LOCAL,
    GLOBAL
  };

  TYPE id_type;
  const InternString* name;

  union {
    Local* local;
    Global* global;
  };
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
  AST_ARR block = {};
};

struct ASTTypedName : public AST {
  AST_LOCAL type = {};
  const InternString* name = nullptr;

  Local* local_ptr = nullptr;
};

struct ASTDecl : public AST {
  const InternString* name = nullptr;
  bool compile_time_const = false;
  Type type = {};

  AST_LOCAL type_ast = 0;
  AST_LOCAL expr = 0;
};

struct ASTGlobalDecl : public ASTDecl {
  Global* global_ptr;
};

struct ASTLocalDecl : public ASTDecl {
  Local* local_ptr;
};

struct ASTFuncSig : public AST {
  FunctionSignature* sig = nullptr;
  const CallingConvention* convention = nullptr;

  AST_LOCAL return_type = 0;
  AST_ARR parameters = {};
};

struct ASTLambdaExpr : public AST {
  AST_LOCAL lambda;
};

struct ASTStructExpr : public AST {
  AST_LOCAL struct_body;
};


struct ASTLambda : public AST {
  Function* function = nullptr;

  AST_LOCAL sig = {};
  AST_LOCAL body = {};
};

struct ASTStructBody : public AST {
  UnitID unit_id;
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

struct ASTStaticLink : public AST {
  AST_LOCAL import_type;

  const InternString* lib_file;
  const InternString* name;

  usize import_index;
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