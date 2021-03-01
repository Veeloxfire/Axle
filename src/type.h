#pragma once
#include "utility.h"
#include "bytecode.h"
#include "strings.h"

struct ASTFunctionSignature;
struct ASTFunctionDeclaration;
struct ASTStructureDeclaration;

struct Structure;

enum struct LOCATION_TYPE : uint8_t {
  UNKNOWN, REGISTER, STACK, ADDRESS
};

struct Location {
  LOCATION_TYPE type = LOCATION_TYPE::UNKNOWN;

  union {
    uint8_t reg;
    int64_t stack_disp = 0;
  };

  constexpr bool operator==(const Location& loc) const {
    if (type != loc.type) {
      return false;
    }
 
    switch (type) {
      case LOCATION_TYPE::ADDRESS:
      case LOCATION_TYPE::REGISTER: return reg == loc.reg;
      case LOCATION_TYPE::STACK: return stack_disp == loc.stack_disp;
      case LOCATION_TYPE::UNKNOWN: return false;
    }
  }

  constexpr bool operator!=(const Location& loc) const {
    return !(operator==(loc));
  }
};

enum struct STRUCTURE_TYPE : uint8_t {
  INTEGER, FLOATING, COMPOSITE, UNKNOWN, VOID, ENUM
};

struct IntegerStructure {
  bool is_signed;
  uint32_t size;
};

struct StructElement {
  InternString name;
  Structure* element;
};

struct CompositeStructure {
  const ASTStructureDeclaration* raw = nullptr;

  uint32_t size;
  uint32_t alignment;
  Array<StructElement> elements;
};

struct Structure {
  STRUCTURE_TYPE type = STRUCTURE_TYPE::UNKNOWN;
  InternString name = { nullptr };

  union {
    char _dummpy = '\0';
    IntegerStructure integer;
    CompositeStructure composite;
    uint64_t num_enum_elements;
  };

  uint32_t size() const;
  uint32_t alignment() const;

  ~Structure();
};

struct PositionedElement {
  Structure* structure;
  Location location;
};

struct FunctionSignature {
  InternString name;
  Array<PositionedElement> parameter_types;
  PositionedElement return_type;
};

struct Function {
  FunctionSignature signature = {};

  Array<uint8_t> bytecode = {};
};