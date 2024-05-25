#pragma once
#include <AxleUtil/safe_lib.h>
#include <AxleUtil/formattable.h>
#include <AxleUtil/stacktrace.h>

using namespace Axle::Primitives;

#ifdef AXLE_TRACING
#define TELEMETRY_FUNCTION() STACKTRACE_FUNCTION(); TRACING_FUNCTION()
#define TELEMETRY_SCOPE(name) STACKTRACE_SCOPE(name); TRACING_FUNCTION(name)
#else
#define TELEMETRY_FUNCTION() STACKTRACE_FUNCTION()
#define TELEMETRY_SCOPE(name) STACKTRACE_SCOPE(name)
#endif


//Important forward declarations
struct CompilerGlobals;
struct CompilerThread;
struct AST;
struct Local;
struct Global;
struct Span;
struct CallingConvention;

namespace Axle {
  struct InternString;
};

using AST_LOCAL = AST*;

struct AST_LINKED {
  AST_LOCAL curr = 0;
  AST_LINKED* next = 0;
};

struct AST_ARR {
  AST_LINKED* start = 0;
  usize count = 0;
};

#define FOR_AST(arr, it) \
  for(auto [_l, it] = _start_ast_iterate(arr); _l; _step_ast_iterate(_l, it))

struct AST_ITERATE_HOLDER {
  AST_LINKED* l;
  AST_LOCAL loc;
};

constexpr AST_ITERATE_HOLDER _start_ast_iterate(const AST_ARR& a) {
  if (a.start == nullptr) {
    return { nullptr, 0 };
  }
  else {
    ASSERT(a.start != nullptr);
    ASSERT(a.start->curr != nullptr);
    return {
      a.start,
        a.start->curr,
    };
  }
}

constexpr void _step_ast_iterate(AST_LINKED*& _l, AST_LOCAL& loc) {
  _l = _l->next;
  if (_l != nullptr) {
    loc = _l->curr;
    ASSERT(loc != nullptr);
  }
}


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

constexpr Axle::ViewArr<const char> error_code_string(ERROR_CODE c) {
  switch (c) {
#define MOD(E) case ERROR_CODE ::  E : return Axle::lit_view_arr(#E);
    COMPCODEINC
#undef MOD
  }

  INVALID_CODE_PATH("Invalid error code");
}

namespace Axle::Format {
  template<>
    struct FormatArg<ERROR_CODE> {
      template<Formatter F>
        constexpr static void load_string(F& res, ERROR_CODE er) {
          ViewArr<const char> err_str = error_code_string(er);
          res.load_string(err_str.data, err_str.size);
        }
    };
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
#define MODIFY(name, str, prec) inline constexpr char name[] = str;
  BIN_OP_INCS;
#undef MODIFY

  constexpr Axle::ViewArr<const char> get(BINARY_OPERATOR op) noexcept {
    switch (op)
    {
#define MODIFY(name, str, prec) case BINARY_OPERATOR :: name : return Axle::lit_view_arr(name);
      BIN_OP_INCS;
#undef MODIFY
    }

    INVALID_CODE_PATH("Invalid binary operator");
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
#define MODIFY(name, str) inline constexpr char name[] = str;
  UN_OP_INCS;
#undef MODIFY

  constexpr Axle::ViewArr<const char> get(UNARY_OPERATOR op) noexcept {
    switch (op)
    {
#define MODIFY(name, str) case UNARY_OPERATOR :: name :  return Axle::lit_view_arr(name);
      UN_OP_INCS;
#undef MODIFY
    }

    INVALID_CODE_PATH("Invalid unary operator");
  }
}

#define INTRINSIC_MODS \
  MOD(import) \
  MOD(dynamic_import) \
  MOD(dynamic_export) \
  MOD(type) \

struct Intrinsics {
#define MOD(n) const Axle::InternString* n = nullptr;
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
    pointer,
    slice,
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

  struct ValueRequirements {
#define FLAGS_DECL(num) (1 << num)
    constexpr static u8 Address = FLAGS_DECL(0);
#undef FLAGS_DECL

    constexpr ValueRequirements() = default;
    constexpr ValueRequirements(u8 f) : flags(f) {}

    u8 flags = 0;

    constexpr bool has_address() const { return (flags & Address) == Address; }
    constexpr void add_address() { flags |= Address; }
    constexpr void clear() { flags = 0; }

    constexpr ValueRequirements& operator|=(const ValueRequirements& vr) {
      flags |= vr.flags;
      return *this;
    }
  };

  static constexpr ValueRequirements operator|(ValueRequirements left, ValueRequirements right) {
    return left.flags | right.flags;
  }

  static constexpr ValueRequirements operator|(u8 left, ValueRequirements right) {
    return left | right.flags;
  }

  static constexpr ValueRequirements operator|(ValueRequirements left, u8 right) {
    return left.flags | right;
  }

  static constexpr ValueRequirements operator&(ValueRequirements left, ValueRequirements right) {
    return left.flags | right.flags;
  }

  static constexpr ValueRequirements operator~(ValueRequirements prim) {
    return ~prim.flags;
  }

  struct DynLibraryImport;
  struct IRStore;
}

enum struct System : u8 {
  X86_64,
};

enum struct OutputFileType : u8 {
  PE,
};
