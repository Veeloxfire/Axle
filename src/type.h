#pragma once
#include "utility.h"
#include "bytecode.h"
#include "strings.h"

struct ASTFunctionDeclaration;
struct ASTStructureDeclaration;

struct Structure;


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
  uint8_t val;
};

struct Function {
  InternString name;
  Array<const Structure*> parameter_types;
  const Structure* return_type;

  const ASTFunctionDeclaration* declaration;

  uint64_t label = 0;
  Array<uint8_t> code;
};