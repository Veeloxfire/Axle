#pragma once
#include <cstdint>

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;
using usize = size_t;

//Important forward declarations
struct CompilerGlobals;
struct CompilerThread;
struct AST;
struct Local;
struct Global;
struct InternString;
struct Span;

using AST_LOCAL = AST*;

#define COMPCODEINC \
MOD(NO_ERRORS)\
MOD(ASSERT_ERROR)\
MOD(UNFOUND_DEPENDENCY)\
MOD(FOUND_DEPENDENCY)\
MOD(SYNTAX_ERROR)\
MOD(LEXING_ERROR)\
MOD(LINK_ERROR)\
MOD(TYPE_CHECK_ERROR)\
MOD(NAME_ERROR)\
MOD(IR_ERROR)\
MOD(FILE_ERROR)\
MOD(INTERNAL_ERROR) \
MOD(CONST_ERROR) \
MOD(VM_ERROR)

enum struct ERROR_CODE : uint8_t {
#define MOD(E) E,
  COMPCODEINC
#undef MOD
};

constexpr const char* error_code_string(ERROR_CODE c) {
  switch (c) {
  #define MOD(E) case ERROR_CODE ::  E : return #E;
    COMPCODEINC
    #undef MOD
  }

  return "Invalid code";
}

#define BIN_OP_INCS \
MODIFY(ADD, "+", 3)\
MODIFY(SUB, "-", 3)\
MODIFY(MUL, "*", 4)\
MODIFY(DIV, "/", 4)\
MODIFY(MOD, "%", 4)\
MODIFY(LESSER, "<", 2)\
MODIFY(GREATER, ">", 2)\
MODIFY(EQUIVALENT, "==", 2)\
MODIFY(NOT_EQ, "!=", 2)\
MODIFY(OR, "|", 1)\
MODIFY(AND, "&", 1)\
MODIFY(XOR, "^", 1)\
MODIFY(RIGHT_SHIFT, ">>", 4)\
MODIFY(LEFT_SHIFT, "<<", 4)

enum struct BINARY_OPERATOR : uint8_t {
#define MODIFY(name, str, prec) name,
  BIN_OP_INCS
#undef MODIFY
};

namespace BINARY_OP_STRING {
#define MODIFY(name, str, prec) inline constexpr auto name = str;
  BIN_OP_INCS;
#undef MODIFY

  constexpr const char* get(BINARY_OPERATOR op) noexcept {
    switch (op)
    {
    #define MODIFY(name, str, prec) case BINARY_OPERATOR :: name : return name;
      BIN_OP_INCS;
    #undef MODIFY

      default: return "UNKNOWN OPERATOR";
    }
  }
}

#define UN_OP_INCS \
MODIFY(NEG, "-") \
MODIFY(ADDRESS, "&")\
MODIFY(DEREF, "*")

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
    #define MODIFY(name, str) case UNARY_OPERATOR :: name : return name;
      UN_OP_INCS;
    #undef MODIFY

      default: return "UNKNOWN OPERATOR";
    }
  }
}

#define INTRINSIC_MODS \
MOD(import) \
MOD(dynamic_import) \
MOD(dynamic_export) \
MOD(type) \

struct Intrinsics {
#define MOD(n) const InternString* n = nullptr;
  INTRINSIC_MODS;
#undef MOD
};

using UnitID = u64;
inline constexpr UnitID NULL_ID = 0;

namespace IR {
  enum struct Format : u8 {
    opaque = 0,
    uint8,
    uint16,
    uint32,
    uint64,
    sint8,
    sint16,
    sint32,
    sint64,
  };

  struct LocalLabel {
    u32 label = 0;

    constexpr bool operator==(const LocalLabel& l) const { return label == l.label; }
    constexpr bool operator!=(const LocalLabel& l) const { return label != l.label; }
  };

  inline constexpr LocalLabel NULL_LOCAL_LABEL = {};

  struct GlobalLabel {
    uintptr_t label = 0;

    constexpr bool operator==(const GlobalLabel& g) const { return label == g.label; }
    constexpr bool operator!=(const GlobalLabel& g) const { return label != g.label; }
  };

  inline constexpr GlobalLabel NULL_GLOBAL_LABEL = {};
}

enum struct System : u8 {
  X86_64,
};

enum struct OutputFileType : u8 {
  PE,
};