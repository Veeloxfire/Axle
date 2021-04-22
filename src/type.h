#pragma once
#include "utility.h"
#include "bytecode.h"
#include "strings.h"

struct ASTFunctionDeclaration;
struct ASTStructureDeclaration;

struct Structure;


enum struct STRUCTURE_TYPE : uint8_t {
  UNKNOWN, INTEGER, LITERAL, COMPOSITE, VOID, ENUM
};

struct State;
struct Function;
using CAST_FUNCTION = FUNCTION_PTR<void, State*, Function*, ValueIndex>;

struct Cast {
  const Structure* s;
  CAST_FUNCTION cast;
};

struct Structure {
  STRUCTURE_TYPE type = STRUCTURE_TYPE::UNKNOWN;
  InternString name = { nullptr };
  Array<Cast> casts ={};

  uint32_t size() const;
  uint32_t alignment() const;
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
  INTEGER, SIGNED_INTEGER,
};

struct LiteralStructure : public Structure {
  LITERAL_TYPE literal_type;
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

struct PositionedElement {
  const Structure* structure = nullptr;
  uint8_t val = 0;
};

struct Function {
  InternString name ={};
  Array<const Structure*> parameter_types ={};
  const Structure* return_type = nullptr;

  const ASTFunctionDeclaration* declaration = nullptr;

  uint64_t label = 0;
  Array<uint8_t> code ={};
};

struct StructAllocator {
  static FreelistBlockAllocator<LiteralStructure> literal_structures;
  static FreelistBlockAllocator<IntegerStructure> int_structures;
  static FreelistBlockAllocator<CompositeStructure> composite_structures;
  static FreelistBlockAllocator<EnumStructure> enum_structures;
  static FreelistBlockAllocator<Structure> base_structures;
  static FreelistBlockAllocator<EnumValue> enum_values;

  static LiteralStructure* new_literal() {
    LiteralStructure* const type = literal_structures.allocate();
    type->type = STRUCTURE_TYPE::LITERAL;

    return type;
  }

  static IntegerStructure* new_int() {
    IntegerStructure* const type = int_structures.allocate();
    type->type = STRUCTURE_TYPE::INTEGER;

    return type;
  }

  static CompositeStructure* new_composite() {
    CompositeStructure* const type = composite_structures.allocate();
    type->type = STRUCTURE_TYPE::COMPOSITE;

    return type;
  }

  static EnumStructure* new_enum() {
    EnumStructure* const type = enum_structures.allocate();
    type->type = STRUCTURE_TYPE::ENUM;

    return type;
  }

  static void free_structure(const Structure* s) {
    switch (s->type) {
      case STRUCTURE_TYPE::LITERAL:
        literal_structures.free((const LiteralStructure*)s);
        break;
      case STRUCTURE_TYPE::INTEGER:
        int_structures.free((const IntegerStructure*)s);
        break;
      case STRUCTURE_TYPE::COMPOSITE:
        composite_structures.free((const CompositeStructure*)s);
        break;
      case STRUCTURE_TYPE::ENUM:
        enum_structures.free((const EnumStructure*)s);
        break;
      case STRUCTURE_TYPE::VOID:
        base_structures.free(s);
        break;
    }
  }
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
  const Structure* s_bool = nullptr;

  const Structure* s_u8   = nullptr;
  const Structure* s_i8   = nullptr;
  const Structure* s_u64  = nullptr;
  const Structure* s_i64  = nullptr;

  const Structure* s_int_lit = nullptr;
  const Structure* s_sint_lit = nullptr;

  const Structure* s_void = nullptr;

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
  void u8_to_r64(State*, Function*, ValueIndex);
  void i8_to_r64(State*, Function*, ValueIndex);
  void no_cast(State*, Function*, ValueIndex);
}