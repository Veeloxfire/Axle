#pragma once
#include "utility.h"
#include "bytecode.h"
#include "strings.h"
#include "comp_utilities.h"
#include "names.h"
#include "errors.h"

struct Structure;
struct State;
struct Function;
struct CompilationUnit;
struct CallingConvention;
struct Span;

struct ASTFuncSig;
struct ASTLambda;
struct ASTStructBody;

#define FLAGS_DECL(num) (1 << num)

enum struct META_FLAGS : u8 {
  NORMAL   = 0,
  LITERAL  = FLAGS_DECL(1),
  CONST    = FLAGS_DECL(2),
};

inline constexpr auto operator|(META_FLAGS t, u8 u) -> u8 {
  return u | ((u8)t);
}

inline constexpr auto operator|(u8 u, META_FLAGS t) -> u8 {
  return u | ((u8)t);
}

inline constexpr auto operator|(META_FLAGS u, META_FLAGS t) -> u8 {
  return (u8)u | (u8)t;
}

inline constexpr auto operator|=(u8& u, META_FLAGS t) -> u8& {
  u |= ((u8)t);
  return u;
}

inline constexpr auto operator&(META_FLAGS u, META_FLAGS t) -> u8 {
  return (u8)u & (u8)t;
}

inline constexpr auto operator&(META_FLAGS t, u8 u) -> u8 {
  return u & ((u8)t);
}

inline constexpr auto operator&(u8 u, META_FLAGS t) -> u8 {
  return u & ((u8)t);
}

inline constexpr auto operator&=(u8& u, META_FLAGS t) -> u8& {
  u &= ((u8)t);
  return u;
}

struct Type {
  u8 meta_flags;
  const Structure* base_structure;
};

using CAST_FUNCTION = FUNCTION_PTR<RuntimeValue, Compiler*, State*, CodeBlock*, const RuntimeValue*>;

enum struct STRUCTURE_TYPE : u8 {
  VOID = 0,
  BOOL,
  INTEGER,
  POINTER,
  COMPOSITE,
  ENUM,
  FIXED_ARRAY,
  ASCII_CHAR,
  TUPLE,
  LAMBDA,
};

struct Structure {
  STRUCTURE_TYPE type = STRUCTURE_TYPE::VOID;
  const InternString* name = nullptr;

  u32 size;//bytes
  u32 alignment;//bytes
};

struct PointerStructure : public Structure {
  const Structure* base = nullptr;

  static OwnedPtr<char> gen_name(const Structure* base);
};

struct ArrayStructure : public Structure {
  const Structure* base = nullptr;
  size_t length = 0;

  static OwnedPtr<char> gen_name(const Structure* base, size_t length);
};

struct IntegerStructure : public Structure {
  bool is_signed = false;
};

struct EnumStructure;

struct EnumValue {
  const EnumStructure* type = nullptr;
  const InternString* name = nullptr;
  uint64_t representation = 0;
};

struct EnumStructure : public Structure {
  const IntegerStructure* base = nullptr;
  Array<const EnumValue*> enum_values ={};
};

struct TupleElement {
  uint32_t offset = 0;
  const Structure* type = nullptr;
};

struct TupleStructure : public Structure {
  Array<TupleElement> elements ={};
};

struct StructElement {
  const InternString* name ={};
  uint32_t offset = 0;
  const Structure* type = nullptr;
};

struct CompositeStructure : public Structure {
  const ASTStructBody* declaration = nullptr;

  Array<StructElement> elements ={};
};

struct SignatureStructure : public Structure {
  const CallingConvention* calling_convention = nullptr;

  Array<const Structure*> parameter_types ={};
  const Structure* return_type = nullptr;

  bool return_via_addres = false;
  Array<const Structure*> actual_parameter_types ={};
};

struct FunctionSignature {
  const ASTFuncSig* declaration = nullptr;
  const SignatureStructure* sig_struct = nullptr;

  const InternString* name ={};
};

enum struct FUNCTION_TYPE {
  DEFAULT, EXTERN
};

struct Function {
  bool is_called = false;

  const ASTLambda* declaration = nullptr;

  FunctionSignature signature ={};
  CompilationUnit* compilation_unit = nullptr;

  FUNCTION_TYPE func_type = FUNCTION_TYPE::DEFAULT;

  size_t data_index = 0;
  CodeBlock code_block;
};


struct Structures {
  static FreelistBlockAllocator<TupleStructure> tuple_structures;
  static FreelistBlockAllocator<IntegerStructure> int_structures;
  static FreelistBlockAllocator<CompositeStructure> composite_structures;
  static FreelistBlockAllocator<EnumStructure> enum_structures;
  static FreelistBlockAllocator<Structure> base_structures;
  static FreelistBlockAllocator<ArrayStructure> array_structures;
  static FreelistBlockAllocator<PointerStructure> pointer_structures;
  static FreelistBlockAllocator<SignatureStructure> lambda_structures;
  static FreelistBlockAllocator<EnumValue> enum_values;

  const Structure* s_bool = nullptr;

  const Structure* s_u8   = nullptr;
  const Structure* s_i8   = nullptr;
  const Structure* s_u32  = nullptr;
  const Structure* s_i32  = nullptr;
  const Structure* s_u64  = nullptr;
  const Structure* s_i64  = nullptr;

  const Structure* s_int_lit = nullptr;
  const Structure* s_sint_lit = nullptr;
  const Structure* s_empty_arr = nullptr;
  const Structure* s_lit_ptr = nullptr;

  const Structure* s_struct = nullptr;
  const Structure* s_void = nullptr;
  const Structure* s_void_ptr = nullptr;
  const Structure* s_ascii = nullptr;

  const EnumValue* e_false = nullptr;
  const EnumValue* e_true  = nullptr;

  Array<const EnumValue*> enums;
  Array<const Structure*> structures;

  ~Structures();

  //Returns nullptr for types that dont have a signed version or are already signed
  const Structure* get_signed_of(const Structure* s);
};

struct StructCreator {
  Compiler* comp;
  const Structure* meta_struct;
  NamespaceIndex current_namespace;

  void add_type_to_namespace(const Structure* s, const InternString* name, const Span& span);

  TupleStructure* new_tuple_type(const Span& span,
                                 Array<const Structure*>&& types);

  IntegerStructure* new_int_type(const Span& span,
                                 const InternString* name);

  CompositeStructure* new_composite_type(const Span& span,
                                         const InternString* name);

  CompositeStructure* new_composite_type(const InternString* name,
                                         const ASTStructBody* ast_struct);

  EnumStructure* new_enum_type(const Span& span,
                               const InternString* name);

  Structure* new_base_type(const Span& span,
                           const InternString* name);

  ArrayStructure* new_array_type(const Span& span,
                                 const Structure* base,
                                 size_t length);

  PointerStructure* new_pointer_type(const Span& span,
                                     const Structure* base);

  SignatureStructure* new_lambda_type(const Span& span,
                                      const CallingConvention* conv,
                                      Array<const Structure*>&& params,
                                      const Structure* ret_type);

  EnumValue* new_enum_value(const Span& span,
                            EnumStructure* enum_s,
                            const InternString* name);
};

namespace CASTS {
  RuntimeValue u8_to_r64(Compiler*, State*, CodeBlock*, const RuntimeValue*);
  RuntimeValue i8_to_r64(Compiler*, State*, CodeBlock*, const RuntimeValue*);
  RuntimeValue u32_to_r64(Compiler*, State*, CodeBlock*, const RuntimeValue*);
  RuntimeValue i32_to_r64(Compiler*, State*, CodeBlock*, const RuntimeValue*);
  RuntimeValue no_op(Compiler*, State*, CodeBlock*, const RuntimeValue*);
}

namespace TYPE_TESTS {
  //Can cast without any value modification or checks
  bool can_implicit_cast(const Structure* from, const Structure* to);
  
  //Can cast with modification that can only be done at compile time
  bool can_comptime_cast(const Structure* from, const Structure* to);

  bool is_negatable(const Structure* s);
  bool is_logical(const Structure* s);
  bool is_numeric(const Structure* s);

  bool is_literal(const Structure*);
  bool is_array(const Structure*);
  bool is_pointer(const Structure*);
  bool can_index(const Structure*);

  bool is_int(const Structure*);
  bool is_signed_int(const Structure*);

  bool is_64_bit_int(const Structure*);
  bool is_signed_64_bit_int(const Structure*);
  bool is_unsigned_64_bit_int(const Structure*);

  bool is_8_bit_int(const Structure*);
  bool is_signed_8_bit_int(const Structure*);
  bool is_unsigned_8_bit_int(const Structure*);
}