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

using CAST_TEST     = FUNCTION_PTR<bool, const Structure*>;
using CAST_FUNCTION = FUNCTION_PTR<RuntimeValue, Compiler*, State*, CodeBlock*, RuntimeValue*>;

struct Cast {
  CAST_TEST test;
  CAST_FUNCTION cast;
};

struct Structure {
  STRUCTURE_TYPE type = STRUCTURE_TYPE::VOID;
  InternString name = { nullptr };
  Array<Cast> casts ={};

  uint32_t size() const;
  uint32_t alignment() const;
};

struct PointerStructure : public Structure {
  const Structure* base = nullptr;
};

struct ArrayStructure : public Structure {
  const Structure* base = nullptr;
  size_t length = 0;
};

struct IntegerStructure : public Structure {
  bool is_signed = false;
  uint32_t bytes = 0;
};

struct EnumStructure;

struct EnumValue {
  const EnumStructure* type = nullptr;
  InternString name ={};
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
  InternString name ={};
  const Structure* element = nullptr;
};

struct CompositeStructure : public Structure {
  const ASTStructureDeclaration* raw = nullptr;

  uint32_t total_size = 0;
  uint32_t total_alignment = 0;
  Array<StructElement> elements = {};
};

struct Function {
  InternString name ={};
  Array<const Structure*> parameter_types ={};

  bool return_via_ptr = false;
  const Structure* return_type = nullptr;

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

  LiteralStructure* new_literal();
  PointerStructure* new_pointer();
  IntegerStructure* new_int();
  CompositeStructure* new_composite();
  EnumStructure* new_enum();
  ArrayStructure* new_array();

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

  const Structure* structure_by_name(InternString) const;
  const EnumValue* enum_by_name(InternString) const;
};

void init_types(Types* types, StringInterner* strings);


//Can cast from a literal to a fixed type
bool can_literal_cast(const Structure* from, const Structure* to);

namespace CASTS {
  RuntimeValue u8_to_r64(Compiler*, State*,  CodeBlock*, RuntimeValue*);
  RuntimeValue i8_to_r64(Compiler*, State*,  CodeBlock*, RuntimeValue*);
  RuntimeValue no_cast(Compiler*, State*,  CodeBlock*, RuntimeValue*);
}

namespace TYPE_TESTS {
  bool is_array(const Structure*);

  bool is_int(const Structure*);
  bool is_signed_int(const Structure*);

  bool is_64_bit_int(const Structure*);
  bool is_signed_64_bit_int(const Structure*);
  bool is_unsigned_64_bit_int(const Structure*);

  bool is_8_bit_int(const Structure*);
  bool is_signed_8_bit_int(const Structure*);
  bool is_unsigned_8_bit_int(const Structure*);
}