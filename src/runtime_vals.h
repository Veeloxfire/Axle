#pragma once
#include <stdint.h>

//Important forward declarations
struct Compiler;
struct State;
struct Structure;
struct CodeBlock;
struct Types;
struct ASTExpression;
struct Local;

//New structures

struct ValueIndex {
  uint64_t val;

  constexpr bool operator==(const ValueIndex v) const {
    return v.val == val;
  }
};

struct ConstantVal {
  uint8_t* ptr;
  size_t size;

  constexpr bool operator==(const ConstantVal& cv) const {
    return cv.ptr == ptr && cv.size == size;
  }
};

struct MemIndex {
  size_t index;
  //size_t array_index;//used if the stack type is an array

  constexpr bool operator==(const MemIndex& si) const {
    return index == si.index ;//&& array_index == si.array_index;
  }
};

//Runtime value type
enum struct RVT : uint8_t {
  UNKNOWN  = 0,
  REGISTER = 1,
  MEMORY   = 2,
  CONST    = 4,
};

constexpr uint8_t operator|(RVT l, RVT r) {
  return (uint8_t)l | (uint8_t)r;
}

constexpr uint8_t operator|(uint8_t l, RVT r) {
  return l | (uint8_t)r;
}

constexpr uint8_t operator|(RVT l, uint8_t r) {
  return (uint8_t)l | r;
}

constexpr uint8_t operator|=(uint8_t& l, RVT r) {
  l |= (uint8_t)r;
  return l;
}

constexpr uint8_t operator&(RVT l, RVT r) {
  return (uint8_t)l & (uint8_t)r;
}

constexpr uint8_t operator&(uint8_t l, RVT r) {
  return l & (uint8_t)r;
}

constexpr uint8_t operator&(RVT l, uint8_t r) {
  return (uint8_t)l & r;
}

constexpr uint8_t operator&=(uint8_t& l, RVT r) {
  l &= (uint8_t)r;
  return l;
}

constexpr uint8_t NON_CONST_RVTS = RVT::REGISTER | RVT::MEMORY;
constexpr uint8_t ALL_RVTS = NON_CONST_RVTS | RVT::CONST;


struct RuntimeValue {
  RVT type = RVT::UNKNOWN;//Type in value union - not the structure

  union {
    ValueIndex reg;//index into the values array
    MemIndex mem;//index into the stack_values array
    ConstantVal constant;// a pointer to a constant - same type as the structure
  };

  constexpr bool operator==(const RuntimeValue& rv) const {
    if (rv.type != type) {
      return false;
    }

    switch (type) {
      case RVT::REGISTER: return rv.reg == reg;
      case RVT::CONST: return rv.constant == constant;
      case RVT::MEMORY: return rv.mem == mem;
      default: return true;
    }
  }
};

struct RuntimeHint {
  bool is_hint = true;
  union {
    uint8_t hint_types;
    RuntimeValue val;
  };
};

#define COMPCODEINC \
MOD(NO_ERRORS)\
MOD(UNFOUND_DEPENDENCY)\
MOD(FOUND_DEPENDENCY)\
MOD(CIRCULAR_DEPENDENCY)\
MOD(TYPE_CHECK_ERROR)\
MOD(NAME_ERROR)\
MOD(INTERNAL_ERROR)

enum struct CompileCode : uint8_t {
#define MOD(E) E,
  COMPCODEINC
#undef MOD
};

constexpr const char* compile_code_string(CompileCode c) {
  switch (c) {
  #define MOD(E) case CompileCode:: ## E: return #E;
    COMPCODEINC
    #undef MOD
  }

  return "Invalid code";
}

#define BIN_OP_INCS \
MODIFY(ADD, "+")\
MODIFY(SUB, "-")\
MODIFY(MUL, "*")\
MODIFY(DIV, "/")\
MODIFY(LESSER, "<")\
MODIFY(GREATER, ">")\
MODIFY(EQUIVALENT, "==")\
MODIFY(OR, "|")\
MODIFY(AND, "&")

enum struct BINARY_OPERATOR : uint8_t {
#define MODIFY(name, str) name,
  BIN_OP_INCS
#undef MODIFY
};

namespace BINARY_OP_STRING {
#define MODIFY(name, str) inline constexpr auto name = str;
  BIN_OP_INCS;
#undef MODIFY

  constexpr const char* get(BINARY_OPERATOR op) noexcept {
    switch (op)
    {
    #define MODIFY(name, str) case BINARY_OPERATOR:: ## name : return name;
      BIN_OP_INCS;
    #undef MODIFY

      default: return "UNKNOWN OPERATOR";
    }
  }
}

#define UN_OP_INCS \
MODIFY(NEG, "-") \
MODIFY(ADDRESS, "&")

enum struct UNARY_OPERATOR : uint8_t {
#define MODIFY(name, str) name,
  UN_OP_INCS
#undef MODIFY
};

namespace UNARY_OP_STRING {
#define MODIFY(name, str) inline constexpr auto name = str;
  UN_OP_INCS;
#undef MODIFY

  constexpr const char* get(UNARY_OPERATOR op) noexcept {
    switch (op)
    {
    #define MODIFY(name, str) case UNARY_OPERATOR:: ## name : return name;
      UN_OP_INCS;
    #undef MODIFY

      default: return "UNKNOWN OPERATOR";
    }
  }
}