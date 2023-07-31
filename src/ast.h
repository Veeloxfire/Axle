#pragma once
#include "utility.h"
#include "strings.h"
#include "operators.h"
#include "type.h"
#include "ir.h"
#include "parser.h"
#include "comp_utilities.h"

struct AST_LINKED {
  AST_LOCAL curr = 0;
  AST_LINKED* next = 0;
};

struct AST_ARR {
  AST_LINKED* start = 0;
  usize count = 0;
};

#define FOR_AST(arr, it) \
for(auto [_l, it] = _start_ast_iterate(arr); _l; _step_ast_iterate(_l, it))

struct _AST_ITERATE_HOLDER {
  AST_LINKED* l;
  AST_LOCAL loc;
};

constexpr inline _AST_ITERATE_HOLDER _start_ast_iterate(const AST_ARR& a) {
  if (a.start == nullptr) {
    return { nullptr, 0 };
  }
  else {
    ASSERT(a.start != nullptr);
    ASSERT(a.start->curr != nullptr);
    return {
      a.start,
      a.start->curr,
    };
  }
}

constexpr inline void _step_ast_iterate(AST_LINKED*& _l, AST_LOCAL& loc) {
  _l = _l->next;
  if (_l != nullptr) {
    loc = _l->curr;
    ASSERT(loc != nullptr);
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
MOD(EXPORT) \
MOD(EXPORT_SINGLE) \
MOD(LINK)

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

#if 0
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
#endif

struct AST {
  META_FLAGS meta_flags = 0;

  bool can_be_constant;

  AST_TYPE ast_type;
  Type node_type = {};
  Span node_span = {};
};

struct ASTNamedType : public AST {
  Type actual_type = {};
  const InternString* name = nullptr;
};

struct ASTArrayType : public AST {
  Type actual_type = {};
  u64 array_length = 0;
  AST_LOCAL base = 0;
  AST_LOCAL expr = 0;
};

struct ASTPtrType : public AST {
  Type actual_type = {};
  AST_LOCAL base = 0;
};

struct ASTLambdaType : public AST {
  Type actual_type = {};
  AST_LOCAL ret = 0;
  AST_ARR args = {};
};

struct ASTTupleType : public AST {
  Type actual_type = {};
  AST_ARR types = {};
};

struct ASTBinaryOperatorExpr : public AST {
  BINARY_OPERATOR op;

  AST_LOCAL left = 0;
  AST_LOCAL right = 0;

  BinOpEmitInfo emit_info = {};
};

struct ASTTupleLitExpr : public AST {
  Type tuple_type;
  
  AST_LOCAL prefix;
  AST_ARR elements = {};
};

struct ASTFunctionCallExpr : public AST {
  AST_LOCAL function;

  AST_ARR arguments = {};

  const SignatureStructure* sig = nullptr;
  IR::GlobalLabel label = IR::NULL_GLOBAL_LABEL;
};

struct ASTUnaryOperatorExpr : public AST {
  UNARY_OPERATOR op;
  AST_LOCAL expr = 0;

  UnOpEmitInfo emit_info = {};
};

struct ASTCastExpr : public AST {
  AST_LOCAL type = 0;
  AST_LOCAL expr = 0;
  CASTS::CAST_FUNCTION emit = nullptr;
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
  IR::FunctionSignature* sig = nullptr;
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
  IR::Function* function = nullptr;

  AST_LOCAL sig = {};
  AST_LOCAL body = {};
};

struct ASTStructBody : public AST {
  UnitID unit_id;
  AST_ARR elements = {};
  Type actual_type = {};
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
  AST_LOCAL expr_location = 0;
};

struct ASTLink : public AST {
  AST_LOCAL import_type = 0;

  bool dynamic = false;

  const InternString* lib_file = nullptr;
  const InternString* name = nullptr;

  usize import_index = 0;
};

struct ASTExportSingle : public AST {
  const InternString* name = nullptr;
  AST_LOCAL value = 0;

  usize export_index = 0;
};

struct ASTExport : public AST {
  AST_ARR export_list = {};
};

struct FileAST {
  AST_ARR top_level = {};

  Namespace* ns = nullptr;
  FileLocation file_loc = {};
};

struct Printer {
  size_t tabs = 0;

  void newline() const;
};

void print_full_ast(const FileAST* file);
void print_full_ast(AST_LOCAL expr);