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

enum struct META_TYPE : u8 {
  NORMAL   = 0,
  COMPTIME = FLAGS_DECL(1),
  CONST    = FLAGS_DECL(2),
};

constexpr auto operator|(META_TYPE t, u8 u) -> u8 {
  return u | ((u8)t);
}

constexpr auto operator|(u8 u, META_TYPE t) -> u8 {
  return u | ((u8)t);
}

constexpr auto operator|=(u8& u, META_TYPE t) -> u8& {
  u |= ((u8)t);
  return u;
}

constexpr auto operator&(META_TYPE t, u8 u) -> u8 {
  return u & ((u8)t);
}

constexpr auto operator&(u8 u, META_TYPE t) -> u8 {
  return u & ((u8)t);
}

constexpr auto operator&=(u8& u, META_TYPE t) -> u8& {
  u &= ((u8)t);
  return u;
}

enum struct STRUCTURE_TYPE : uint8_t {
  VOID = 0,
  STRUCT,//Meta type for other types
  INTEGER,
  POINTER,
  SIMPLE_LITERAL,
  COMPOSITE,
  ENUM,
  FIXED_ARRAY,
  ASCII_CHAR,
  TUPLE_LITERAL,
  LAMBDA,
};

using CAST_FUNCTION = FUNCTION_PTR<RuntimeValue, Compiler*, State*, CodeBlock*, const RuntimeValue*>;


struct Structure {
  STRUCTURE_TYPE type = STRUCTURE_TYPE::VOID;
  const InternString* name = nullptr;

  uint32_t size() const;
  uint32_t alignment() const;
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
  uint32_t bytes = 0;
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

enum struct SIMPLE_LITERAL_TYPE {
  INTEGER, SIGNED_INTEGER, EMPTY_ARR, POINTER, STRUCT_INIT
};

struct SimpleLiteralStructure : public Structure {
  SIMPLE_LITERAL_TYPE literal_type = SIMPLE_LITERAL_TYPE::INTEGER;
};

struct TupleElement {
  uint32_t offset = 0;
  const Structure* type = nullptr;
};

struct TupleLiteralStructure : public Structure {
  uint32_t cached_size = 0;
  uint32_t cached_alignment = 0;

  Array<TupleElement> elements ={};
};

struct StructElement {
  const InternString* name ={};
  uint32_t offset = 0;
  const Structure* type = nullptr;
};

struct CompositeStructure : public Structure {
  const ASTStructBody* declaration = nullptr;

  uint32_t cached_size = 0;
  uint32_t cached_alignment = 0;
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

constexpr bool is_negatable(const Structure* s) {
  const bool signed_int = s->type == STRUCTURE_TYPE::INTEGER &&
    static_cast<const IntegerStructure*>(s)->is_signed;
  const bool int_lit = s->type == STRUCTURE_TYPE::SIMPLE_LITERAL &&
    static_cast<const SimpleLiteralStructure*>(s)->literal_type == SIMPLE_LITERAL_TYPE::INTEGER;

  return signed_int || int_lit;
}

constexpr bool is_numeric_type(const Structure* s) {
  const bool int_lit = s->type == STRUCTURE_TYPE::SIMPLE_LITERAL &&
    static_cast<const SimpleLiteralStructure*>(s)->literal_type == SIMPLE_LITERAL_TYPE::INTEGER;


  return s->type == STRUCTURE_TYPE::INTEGER || int_lit;
}

struct Types {
  static FreelistBlockAllocator<SimpleLiteralStructure> simple_literal_structures;
  static FreelistBlockAllocator<TupleLiteralStructure> tuple_literal_structures;
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

  ~Types();

  constexpr bool is_logical_type(const Structure* s) const {
    return s == s_bool || is_numeric_type(s);
  }
};

struct TypeCreator {
  Compiler* comp;
  const Structure* meta_struct;
  NamespaceIndex current_namespace;

  void add_type_to_namespace(const Structure* s, const InternString* name, const Span& span);


  SimpleLiteralStructure* new_simple_literal_type(const Span& span,
                                                  const InternString* name);

  TupleLiteralStructure* new_tuple_literal_type(const Span& span,
                                                Array<const Structure*>&& types);

  IntegerStructure* new_int_type(const Span& span,
                                 const InternString* name);

  CompositeStructure* new_composite_type(const Span& span,
                                         const InternString* name);

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


//Can cast without any value modification or checks
constexpr bool can_implicit_cast(const Structure* from, const Structure* to) {
  if (from == to) return true;
  else if (from->type == STRUCTURE_TYPE::ASCII_CHAR) {
    return to->type == STRUCTURE_TYPE::INTEGER
      && ((const IntegerStructure*)to)->bytes == 1
      && !((const IntegerStructure*)to)->is_signed;
  }
  else if (from->type == STRUCTURE_TYPE::SIMPLE_LITERAL) {
    //both literals
    const SimpleLiteralStructure* f_ls = (const SimpleLiteralStructure*)from;

    if (to->type == STRUCTURE_TYPE::SIMPLE_LITERAL) {
      const SimpleLiteralStructure* t_ls = (const SimpleLiteralStructure*)to;

      //can cast unsigned literal to signed literal
      return f_ls->literal_type == SIMPLE_LITERAL_TYPE::INTEGER
        && t_ls->literal_type == SIMPLE_LITERAL_TYPE::SIGNED_INTEGER;
    }
    else if (to->type == STRUCTURE_TYPE::POINTER) {
      //Can cast a pointer literal (e.g. nullptr) to any pointer type
      return f_ls->literal_type == SIMPLE_LITERAL_TYPE::POINTER;
    }
  }
  else if (from->type == STRUCTURE_TYPE::TUPLE_LITERAL
           && to->type == STRUCTURE_TYPE::COMPOSITE) {
    const TupleLiteralStructure* f_ts = (const TupleLiteralStructure*)from;
    const CompositeStructure* t_cs = (const CompositeStructure*)to;

    //Size same?
    if(f_ts->elements.size != t_cs->elements.size) { return false;}

    auto f_i = f_ts->elements.begin();
    auto t_i = t_cs->elements.begin();
    const auto f_end = f_ts->elements.end();

    for (; f_i < f_end; f_i++, t_i++) {
      //Must be implicit type and offset
      if(f_i->offset != t_i->offset || !can_implicit_cast(f_i->type, t_i->type)) { 
        return false;
      }
    }

    return true;
  }


  return false;
}

//Can cast with modification that can only be done at compile time
bool can_comptime_cast(const Structure* from, const Structure* to);

//Returns nullptr for types that dont have a signed version or are already signed
const Structure* get_signed_type_of(const Types* types, const Structure* s);

namespace CASTS {
  RuntimeValue u8_to_r64(Compiler*, State*, CodeBlock*, const RuntimeValue*);
  RuntimeValue i8_to_r64(Compiler*, State*, CodeBlock*, const RuntimeValue*);
  RuntimeValue u32_to_r64(Compiler*, State*, CodeBlock*, const RuntimeValue*);
  RuntimeValue i32_to_r64(Compiler*, State*, CodeBlock*, const RuntimeValue*);
  RuntimeValue no_op(Compiler*, State*, CodeBlock*, const RuntimeValue*);
}

namespace TYPE_TESTS {
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