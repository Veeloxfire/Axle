#pragma once
#include "utility.h"
#include "bytecode.h"
#include "strings.h"
#include "runtime_vals.h"

struct ASTFunctionDeclaration;
struct ASTStructureDeclaration;

struct Structure;
struct State;
struct Function;


enum struct STRUCTURE_TYPE : uint8_t {
  INTEGER, POINTER, LITERAL, COMPOSITE, VOID, ENUM, FIXED_ARRAY, ASCII_CHAR
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

enum struct LITERAL_TYPE {
  INTEGER, SIGNED_INTEGER, EMPTY_ARR
};

struct LiteralStructure : public Structure {
  LITERAL_TYPE literal_type = LITERAL_TYPE::INTEGER;
  size_t str_length = 0;
};

struct StructElement {
  const InternString* name ={};
  const Structure* element = nullptr;
};

struct CompositeStructure : public Structure {
  const ASTStructureDeclaration* raw = nullptr;

  uint32_t total_size = 0;
  uint32_t total_alignment = 0;
  Array<StructElement> elements ={};
};

struct Function {
  const InternString* name ={};
  Array<const Structure*> parameter_types ={};
  const Structure* return_type = nullptr;

  bool return_via_ptr = false;
  const ASTFunctionDeclaration* declaration = nullptr;

  CodeBlock code_block ={};
};

constexpr bool is_negatable(const Structure* s) {
  const bool signed_int = s->type == STRUCTURE_TYPE::INTEGER &&
    static_cast<const IntegerStructure*>(s)->is_signed;
  const bool int_lit = s->type == STRUCTURE_TYPE::LITERAL &&
    static_cast<const LiteralStructure*>(s)->literal_type == LITERAL_TYPE::INTEGER;

  return signed_int || int_lit;
}

constexpr bool is_numeric_type(const Structure* s) {
  const bool int_lit = s->type == STRUCTURE_TYPE::LITERAL &&
    static_cast<const LiteralStructure*>(s)->literal_type == LITERAL_TYPE::INTEGER;


  return s->type == STRUCTURE_TYPE::INTEGER || int_lit;
}

struct Types {
  static FreelistBlockAllocator<LiteralStructure> literal_structures;
  static FreelistBlockAllocator<IntegerStructure> int_structures;
  static FreelistBlockAllocator<CompositeStructure> composite_structures;
  static FreelistBlockAllocator<EnumStructure> enum_structures;
  static FreelistBlockAllocator<Structure> base_structures;
  static FreelistBlockAllocator<ArrayStructure> array_structures;
  static FreelistBlockAllocator<PointerStructure> pointer_structures;
  static FreelistBlockAllocator<EnumValue> enum_values;

  const Structure* s_bool = nullptr;

  const Structure* s_u8   = nullptr;
  const Structure* s_i8   = nullptr;
  const Structure* s_u64  = nullptr;
  const Structure* s_i64  = nullptr;

  const Structure* s_int_lit = nullptr;
  const Structure* s_sint_lit = nullptr;
  const Structure* s_empty_arr = nullptr;

  const Structure* s_void = nullptr;
  const Structure* s_ascii = nullptr;

  const EnumValue* e_false = nullptr;
  const EnumValue* e_true  = nullptr;

  Array<const EnumValue*> enums;
  Array<const Structure*> structures;

  ~Types();

  constexpr bool is_logical_type(const Structure* s) const {
    return s == s_bool || is_numeric_type(s);
  }

  const Structure* structure_by_name(const InternString*) const;
  const EnumValue* enum_by_name(const InternString*) const;
};

void init_types(Compiler* comp);

LiteralStructure* new_literal_type(Compiler* const comp,
                                   const InternString* name);

IntegerStructure* new_int_type(Compiler* const comp,
                               const InternString* name);

CompositeStructure* new_composite_type(Compiler* const comp,
                                       const InternString* name);

EnumStructure* new_enum_type(Compiler* const comp,
                             const InternString* name);

Structure* new_base_type(Compiler* const comp,
                         const InternString* name);

ArrayStructure* new_array_type(Compiler* const comp,
                               const Structure* base,
                               size_t length);

PointerStructure* new_pointer_type(Compiler* const comp,
                                   const Structure* base);

EnumValue* new_enum_value(Compiler* const comp,
                          EnumStructure* enum_s,
                          const InternString* name);


//Can cast without any value modification or checks
constexpr bool can_implicit_cast(const Structure* from, const Structure* to) {
  if(from == to) return true;
  else if (from->type == STRUCTURE_TYPE::LITERAL 
           && to->type == STRUCTURE_TYPE::LITERAL) {
    //both literals
    const LiteralStructure* f_ls = (const LiteralStructure*)from;
    const LiteralStructure* t_ls = (const LiteralStructure*)to;

    //can cast unsigned literal to signed literal
    return f_ls->literal_type == LITERAL_TYPE::INTEGER
      && t_ls->literal_type == LITERAL_TYPE::SIGNED_INTEGER;
  }
  else {
    return false;
  }
}

//Can cast with modification that can only be done at compile time
bool can_comptime_cast(const Structure* from, const Structure* to);

namespace CASTS {
  RuntimeValue u8_to_r64(Compiler*, State*, CodeBlock*, const RuntimeValue*);
  RuntimeValue i8_to_r64(Compiler*, State*, CodeBlock*, const RuntimeValue*);
  RuntimeValue no_op(Compiler*, State*, CodeBlock*, const RuntimeValue*);
}

namespace TYPE_TESTS {
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