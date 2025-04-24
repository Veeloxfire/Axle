#pragma once
#include <AxleUtil/utility.h>
#include <AxleUtil/strings.h>
#include <AxleUtil/files.h>

#include <Axle/comp_utilities.h>

#include "type.h"
#include "errors.h"
#include "ir_ast_info.h"

struct Namespace;

#define AST_TYPE_MOD \
MOD(NAMED_TYPE) \
MOD(ARRAY_TYPE) \
MOD(PTR_TYPE) \
MOD(SLICE_TYPE) \
MOD(LAMBDA_TYPE) \
MOD(TUPLE_TYPE) \
MOD(CAST) \
MOD(UNARY_OPERATOR) \
MOD(BINARY_OPERATOR) \
MOD(IDENTIFIER_EXPR) \
MOD(DECL) \
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

constexpr Axle::ViewArr<const char> ast_type_string(AST_TYPE ty) {
  switch (ty) {
    case AST_TYPE::INVALID: return Axle::lit_view_arr("INVALID");
#define MOD(n) case AST_TYPE :: n : return Axle::lit_view_arr(#n);
    AST_TYPE_MOD;
#undef MOD
  }

  INVALID_CODE_PATH("Invalid ast type");
}

#define AST_VISIT_STEP_MOD \
MOD(NAMED_TYPE_DOWN, NAMED_TYPE) \
MOD(ARRAY_TYPE_UP, ARRAY_TYPE) \
MOD(ARRAY_TYPE_DOWN_LEN, ARRAY_TYPE) \
MOD(ARRAY_TYPE_DOWN, ARRAY_TYPE) \
MOD(PTR_TYPE_UP, PTR_TYPE) \
MOD(PTR_TYPE_DOWN, PTR_TYPE) \
MOD(SLICE_TYPE_UP, SLICE_TYPE) \
MOD(SLICE_TYPE_DOWN, SLICE_TYPE) \
MOD(LAMBDA_TYPE_UP, LAMBDA_TYPE) \
MOD(LAMBDA_TYPE_DOWN, LAMBDA_TYPE) \
MOD(TUPLE_TYPE_UP, TUPLE_TYPE) \
MOD(TUPLE_TYPE_DOWN, TUPLE_TYPE) \
MOD(CAST_UP_TYPE, CAST) \
MOD(CAST_UP_EXPR, CAST) \
MOD(CAST_DOWN, CAST) \
MOD(UNARY_OPERATOR_UP, UNARY_OPERATOR) \
MOD(UNARY_OPERATOR_DOWN, UNARY_OPERATOR) \
MOD(BINARY_OPERATOR_UP, BINARY_OPERATOR) \
MOD(BINARY_OPERATOR_DOWN, BINARY_OPERATOR) \
MOD(IDENTIFIER_EXPR_DOWN, IDENTIFIER_EXPR) \
MOD(DECL_UP_TYPE, DECL) \
MOD(DECL_UP_EXPR, DECL) \
MOD(DECL_DOWN, DECL) \
MOD(NUMBER_DOWN, NUMBER) \
MOD(FUNCTION_CALL_UP_FUNCTION, FUNCTION_CALL) \
MOD(FUNCTION_CALL_UP_ARGS, FUNCTION_CALL) \
MOD(FUNCTION_CALL_DOWN, FUNCTION_CALL) \
MOD(TUPLE_LIT_UP_PREFIX, TUPLE_LIT) \
MOD(TUPLE_LIT_UP_ELEMENTS, TUPLE_LIT) \
MOD(TUPLE_LIT_DOWN, TUPLE_LIT) \
MOD(ARRAY_EXPR_UP_FIRST, ARRAY_EXPR) \
MOD(ARRAY_EXPR_UP_REST, ARRAY_EXPR) \
MOD(ARRAY_EXPR_DOWN, ARRAY_EXPR) \
MOD(ASCII_STRING_DOWN, ASCII_STRING) \
MOD(ASCII_CHAR_DOWN, ASCII_CHAR) \
MOD(INDEX_EXPR_UP, INDEX_EXPR) \
MOD(INDEX_EXPR_DOWN, INDEX_EXPR) \
MOD(MEMBER_ACCESS_UP, MEMBER_ACCESS) \
MOD(MEMBER_ACCESS_DOWN, MEMBER_ACCESS) \
MOD(LAMBDA_UP, LAMBDA) \
MOD(LAMBDA_EXPR_DOWN, LAMBDA_EXPR) \
MOD(STRUCT_UP, STRUCT) \
MOD(STRUCT_DOWN, STRUCT) \
MOD(STRUCT_EXPR_DOWN, STRUCT_EXPR) \
MOD(TYPED_NAME_UP, TYPED_NAME) \
MOD(TYPED_NAME_DOWN, TYPED_NAME) \
MOD(ASSIGN_UP_LEFT, ASSIGN) \
MOD(ASSIGN_UP_RIGHT, ASSIGN) \
MOD(BLOCK_UP, BLOCK) \
MOD(IF_ELSE_UP, IF_ELSE) \
MOD(WHILE_UP, WHILE) \
MOD(RETURN_UP, RETURN) \
MOD(FUNCTION_SIGNATURE_UP, FUNCTION_SIGNATURE) \
MOD(FUNCTION_SIGNATURE_DOWN, FUNCTION_SIGNATURE) \
MOD(IMPORT_UP, IMPORT) \
MOD(IMPORT_DOWN, IMPORT) \
MOD(EXPORT_UP, EXPORT) \
MOD(EXPORT_SINGLE_UP, EXPORT_SINGLE) \
MOD(EXPORT_SINGLE_DOWN, EXPORT_SINGLE) \
MOD(LINK_UP, LINK) \
MOD(LINK_DOWN, LINK) \

enum struct AST_VISIT_STEP : u8 {
#define MOD(n, ...) n,
  AST_VISIT_STEP_MOD
#undef MOD
};

constexpr Axle::ViewArr<const char> ast_visit_step_string(AST_VISIT_STEP ty) {
  switch (ty) {
#define MOD(n, ...) case AST_VISIT_STEP :: n : return Axle::lit_view_arr(#n);
    AST_VISIT_STEP_MOD;
#undef MOD
  }

  INVALID_CODE_PATH("Invalid ast visit step");
}

constexpr AST_TYPE ast_visit_step_ast_type(AST_VISIT_STEP ty) {
  switch (ty) {
#define MOD(n, m, ...) case AST_VISIT_STEP :: n : return AST_TYPE :: m;
    AST_VISIT_STEP_MOD;
#undef MOD
  }

  INVALID_CODE_PATH("Invalid ast visit step");
}

struct AstVisit {
  AST_LOCAL node;
  AST_VISIT_STEP step;
};

struct AST {
  AST_TYPE ast_type = AST_TYPE::INVALID;
  VALUE_CATEGORY value_category = VALUE_CATEGORY::TEMPORARY_CONSTANT;

  Type node_type = {};
  Type node_infer_type = {};
  Span node_span = {};

  u32 start_visit = 0;
  u32 end_visit = 0;
};

template<typename T>
concept IsASTDataNode = requires(T* t_ptr, AST* data_ptr) {
  { static_cast<AST*>(t_ptr) } -> Axle::IS_SAME_TYPE<AST*>;
  { static_cast<T*>(data_ptr) } -> Axle::IS_SAME_TYPE<T*>;
  { T::EXPECTED_AST_TYPE };
};

template<IsASTDataNode T>
constexpr T* downcast_ast(AST_LOCAL ast) {
  ASSERT(ast.ast->ast_type == T::EXPECTED_AST_TYPE);
  return static_cast<T*>(ast.ast);
}

struct ASTNamedType : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::NAMED_TYPE;

  Type actual_type = {};
  const Axle::InternString* name = nullptr;
  const Global* global = nullptr;
};

struct ASTArrayType : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::ARRAY_TYPE;

  Type actual_type = {};
  u64 array_length = 0;
  AST_LOCAL base = NULL_AST_NODE;
  AST_LOCAL expr = NULL_AST_NODE;
};

struct ASTPtrType : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::PTR_TYPE;

  Type actual_type = {};
  AST_LOCAL base = NULL_AST_NODE;
};

struct ASTSliceType : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::SLICE_TYPE;

  Type actual_type = {};
  AST_LOCAL base = NULL_AST_NODE;
};

struct ASTLambdaType : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::LAMBDA_TYPE;

  Type actual_type = {};
  AST_LOCAL ret = NULL_AST_NODE;
  Axle::ViewArr<const AST_LOCAL> args = {};
};

struct ASTTupleType : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::TUPLE_TYPE;
  
  Type actual_type = {};
  Axle::ViewArr<const AST_LOCAL> types = {};
};

struct ASTBinaryOperatorExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::BINARY_OPERATOR;
  
  BINARY_OPERATOR op;

  AST_LOCAL left = NULL_AST_NODE;
  AST_LOCAL right = NULL_AST_NODE;

  BinOpEmitInfo emit_info = {};
};

struct ASTTupleLitExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::TUPLE_LIT;
  AST_LOCAL prefix;
  Axle::ViewArr<const AST_LOCAL> elements = {};
};

struct ASTFunctionCallExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::FUNCTION_CALL;
  AST_LOCAL function;

  Axle::ViewArr<const AST_LOCAL> arguments = {};

  const SignatureStructure* sig = nullptr;
  IR::GlobalLabel label = IR::NULL_GLOBAL_LABEL;
};


struct ASTUnaryOperatorExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::UNARY_OPERATOR;
  UNARY_OPERATOR op;
  AST_LOCAL expr = NULL_AST_NODE;

  UnOpEmitInfo emit_info = {};
};

struct ASTCastExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::CAST;
  AST_LOCAL type = NULL_AST_NODE;
  AST_LOCAL expr = NULL_AST_NODE;
  CASTS::CAST_FUNCTION emit = nullptr;
};

struct ASTIndexExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::INDEX_EXPR;
  AST_LOCAL expr = NULL_AST_NODE;
  Axle::ViewArr<const AST_LOCAL> arguments = {};
};

struct ASTNumber : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::NUMBER;
  uint64_t num_value = 0;
  const Axle::InternString* suffix = nullptr;
};

struct ASTArrayExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::ARRAY_EXPR;
  Axle::ViewArr<const AST_LOCAL> elements = {};
};

struct ASTIdentifier : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::IDENTIFIER_EXPR;
  enum TYPE {
    LOCAL,
    GLOBAL
  };

  TYPE id_type;
  const Axle::InternString* name;

  union {
    Local* local;
    Global* global;
  };
};

namespace Axle::Format {
  template<>
  struct FormatArg<ASTIdentifier::TYPE> {
    template<Formatter F>
    constexpr static void load_string(F& res, ASTIdentifier::TYPE ty) {
      switch (ty) {
        case ASTIdentifier::LOCAL: res.load_string_lit("LOCAL"); return;
        case ASTIdentifier::GLOBAL: res.load_string_lit("GLOBAL"); return;
      }
      
      INVALID_CODE_PATH("Invalid Identifier Type");
    }
  };
}

struct ASTMemberAccessExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::MEMBER_ACCESS;
  AST_LOCAL expr = NULL_AST_NODE;

  uint32_t offset = 0;
  const Axle::InternString* name = nullptr;
};

struct ASTAsciiString : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::ASCII_STRING;
  const Axle::InternString* string;
};

struct ASTAsciiChar : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::ASCII_CHAR;
  char character;
};

struct ASTBlock : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::BLOCK;
  Axle::ViewArr<const AST_LOCAL> block = {};
};

struct ASTTypedName : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::TYPED_NAME;
  AST_LOCAL type = {};
  const Axle::InternString* name = nullptr;

  Local* local_ptr = nullptr;
};

struct ASTDecl : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::DECL;
  enum TYPE {
    LOCAL,
    GLOBAL,
  };

  bool compile_time_const;
  TYPE decl_type;

  const Axle::InternString* name;
  Type type;

  AST_LOCAL type_ast;
  AST_LOCAL expr;

  union {
    Global* global_ptr;
    Local* local_ptr;
  };
};

struct ASTFuncSig : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::FUNCTION_SIGNATURE;
  IR::Function* ir_function = nullptr;
  const CallingConvention* convention = nullptr;

  AST_LOCAL return_type = NULL_AST_NODE;
  Axle::ViewArr<const AST_LOCAL> parameters = {};
};

struct PrintCallSignature {
  const ASTFunctionCallExpr* call;
};

namespace Axle::Format {
  template<>
  struct FormatArg<PrintCallSignature> {
    template<Formatter F>
    constexpr static void load_string(F& res, PrintCallSignature p_call) {
      const ASTFunctionCallExpr* call = p_call.call;

      res.load_char_raw('(');

      if (call->arguments.size > 0) {
        const AST_LOCAL* loc = call->arguments.begin();
        const AST_LOCAL* const end = call->arguments.end();

        FormatArg<const Axle::InternString*>::load_string(res,  loc->ast->node_type.name);
        loc += 1;

        for(; loc < end; ++loc) {
          res.load_string_raw(", ");
          FormatArg<const Axle::InternString*>::load_string(res, loc->ast->node_type.name);
        }
      }

      res.load_char_raw(')');
    }
  };
}

struct ASTLambdaExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::LAMBDA_EXPR;
  AST_LOCAL lambda;
};

struct ASTStructExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::STRUCT_EXPR;
  AST_LOCAL struct_body;
};

struct ASTLambda : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::LAMBDA;
  IR::Function* function = nullptr;

  ASTFuncSig* sig = nullptr;
  AST_LOCAL body = {};
};

struct ASTStructBody : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::STRUCT;
  UnitID unit_id;
  Axle::ViewArr<const AST_LOCAL> elements = {};
  Type actual_type = {};
};

struct ASTWhile : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::WHILE;
  AST_LOCAL condition = NULL_AST_NODE;
  AST_LOCAL statement = NULL_AST_NODE;
};

struct ASTIfElse : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::IF_ELSE;
  AST_LOCAL condition = NULL_AST_NODE;
  AST_LOCAL if_statement = NULL_AST_NODE;
  AST_LOCAL else_statement = NULL_AST_NODE;
};

struct ASTReturn : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::RETURN;
  AST_LOCAL expr = NULL_AST_NODE;
};

struct ASTAssign : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::ASSIGN;
  AST_LOCAL assign_to = NULL_AST_NODE;
  AST_LOCAL value = NULL_AST_NODE;
};

struct ASTImport : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::IMPORT;
  AST_LOCAL expr_location = NULL_AST_NODE;
};

struct ASTLink : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::LINK;
  AST_LOCAL import_type = NULL_AST_NODE;

  bool dynamic = false;

  const Axle::InternString* lib_file = nullptr;
  const Axle::InternString* name = nullptr;

  usize import_index = 0;
};

struct ASTExportSingle : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::EXPORT_SINGLE;
  const Axle::InternString* name = nullptr;
  AST_LOCAL value = NULL_AST_NODE;

  usize export_index = 0;
};

struct ASTExport : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::EXPORT;
  Axle::ViewArr<const AST_LOCAL> export_list = {};
};

struct FileAST {
  Axle::GrowingMemoryPool<32 * 1024> ast_store = {};
  
  Axle::ViewArr<const AST_LOCAL> top_level = {};

  Namespace* ns = nullptr;
  Axle::FileLocation file_loc = {};
};

struct Printer {
  size_t tabs = 0;

  void newline() const;
};

void print_full_ast(const FileAST* file);
void print_full_ast(AST_LOCAL expr);

constexpr void same_category(AST* low, const AST* high) {
  low->value_category = high->value_category;
}

constexpr void same_category(AST* low, AST_LOCAL high) {
  same_category(low, high.ast);
}

constexpr void reduce_category(AST* low, const AST* high) {
  switch (high->value_category) {
    case VALUE_CATEGORY::TEMPORARY_CONSTANT:
    case VALUE_CATEGORY::VARIABLE_CONSTANT: return;

    case VALUE_CATEGORY::TEMPORARY_IMMUTABLE:
    case VALUE_CATEGORY::VARIABLE_IMMUTABLE:
    case VALUE_CATEGORY::VARIABLE_MUTABLE:
      switch (low->value_category) {
        case VALUE_CATEGORY::TEMPORARY_CONSTANT:
          low->value_category = VALUE_CATEGORY::TEMPORARY_IMMUTABLE;
          return;
        case VALUE_CATEGORY::VARIABLE_CONSTANT:
          low->value_category = VALUE_CATEGORY::VARIABLE_IMMUTABLE;
          return;

        case VALUE_CATEGORY::TEMPORARY_IMMUTABLE:
        case VALUE_CATEGORY::VARIABLE_IMMUTABLE:
        case VALUE_CATEGORY::VARIABLE_MUTABLE:
          return;
      }


      INVALID_CODE_PATH("Invalid value category");
  }

  INVALID_CODE_PATH("Invalid value category");
}

constexpr void reduce_category(AST* low, AST_LOCAL high) {
  reduce_category(low, high.ast);
}
