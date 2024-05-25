#pragma once
#include <AxleUtil/utility.h>
#include <AxleUtil/strings.h>

#include <Axle/comp_utilities.h>

#include "operators.h"
#include "type.h"
#include "ir.h"
#include "parser.h"

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

struct AST {
  AST_TYPE ast_type;
  VALUE_CATEGORY value_category;

  Type node_type = {};
  Span node_span = {};

  constexpr AST() = default;
};

template<typename T>
constexpr T* downcast_ast(AST* ast) {
  ASSERT(ast->ast_type == T::EXPECTED_AST_TYPE);
  return static_cast<T*>(ast);
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
  AST_LOCAL base = 0;
  AST_LOCAL expr = 0;
};

struct ASTPtrType : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::PTR_TYPE;

  Type actual_type = {};
  AST_LOCAL base = 0;
};

struct ASTSliceType : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::SLICE_TYPE;

  Type actual_type = {};
  AST_LOCAL base = 0;
};

struct ASTLambdaType : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::LAMBDA_TYPE;

  Type actual_type = {};
  AST_LOCAL ret = 0;
  AST_ARR args = {};
};

struct ASTTupleType : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::TUPLE_TYPE;
  Type actual_type = {};
  AST_ARR types = {};
};

struct ASTBinaryOperatorExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::BINARY_OPERATOR;
  BINARY_OPERATOR op;

  AST_LOCAL left = 0;
  AST_LOCAL right = 0;

  BinOpEmitInfo emit_info = {};
};

struct ASTTupleLitExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::TUPLE_LIT;
  AST_LOCAL prefix;
  AST_ARR elements = {};
};

struct ASTFunctionCallExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::FUNCTION_CALL;
  AST_LOCAL function;

  AST_ARR arguments = {};

  const SignatureStructure* sig = nullptr;
  IR::GlobalLabel label = IR::NULL_GLOBAL_LABEL;
};

struct ASTUnaryOperatorExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::UNARY_OPERATOR;
  UNARY_OPERATOR op;
  AST_LOCAL expr = 0;

  UnOpEmitInfo emit_info = {};
};

struct ASTCastExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::CAST;
  AST_LOCAL type = 0;
  AST_LOCAL expr = 0;
  CASTS::CAST_FUNCTION emit = nullptr;
};

struct ASTIndexExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::INDEX_EXPR;
  AST_LOCAL expr = 0;
  AST_ARR arguments = {};
};

struct ASTNumber : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::NUMBER;
  uint64_t num_value = 0;
  const Axle::InternString* suffix = nullptr;
};

struct ASTArrayExpr : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::ARRAY_EXPR;
  AST_ARR elements = {};
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
  AST_LOCAL expr = 0;

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
  AST_ARR block = {};
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
  IR::FunctionSignature* sig = nullptr;
  const CallingConvention* convention = nullptr;

  AST_LOCAL return_type = 0;
  AST_ARR parameters = {};
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

      auto l = call->arguments.start;

      if (l) {
        FormatArg<const Axle::InternString*>::load_string(res, l->curr->node_type.name);
        l = l->next;

        while (l) {
          res.load_string_raw(", ");
          FormatArg<const Axle::InternString*>::load_string(res, l->curr->node_type.name);
          l = l->next;
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

  AST_LOCAL sig = {};
  AST_LOCAL body = {};
};

struct ASTStructBody : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::STRUCT;
  UnitID unit_id;
  AST_ARR elements = {};
  Type actual_type = {};
};

struct ASTWhile : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::WHILE;
  AST_LOCAL condition = 0;
  AST_LOCAL statement = 0;
};

struct ASTIfElse : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::IF_ELSE;
  AST_LOCAL condition = 0;
  AST_LOCAL if_statement = 0;
  AST_LOCAL else_statement = 0;
};

struct ASTReturn : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::RETURN;
  AST_LOCAL expr = 0;
};

struct ASTAssign : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::ASSIGN;
  AST_LOCAL assign_to = 0;
  AST_LOCAL value = 0;
};

struct ASTImport : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::IMPORT;
  AST_LOCAL expr_location = 0;
};

struct ASTLink : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::LINK;
  AST_LOCAL import_type = 0;

  bool dynamic = false;

  const Axle::InternString* lib_file = nullptr;
  const Axle::InternString* name = nullptr;

  usize import_index = 0;
};

struct ASTExportSingle : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::EXPORT_SINGLE;
  const Axle::InternString* name = nullptr;
  AST_LOCAL value = 0;

  usize export_index = 0;
};

struct ASTExport : public AST {
  constexpr static AST_TYPE EXPECTED_AST_TYPE = AST_TYPE::EXPORT;
  AST_ARR export_list = {};
};

struct FileAST {
  AST_ARR top_level = {};

  Namespace* ns = nullptr;
  Axle::FileLocation file_loc = {};
};

struct Printer {
  size_t tabs = 0;

  void newline() const;
};

void print_full_ast(const FileAST* file);
void print_full_ast(AST_LOCAL expr);

constexpr void same_category(AST_LOCAL low, AST_LOCAL high) {
  low->value_category = high->value_category;
}

constexpr void reduce_category(AST_LOCAL low, AST_LOCAL high) {
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
