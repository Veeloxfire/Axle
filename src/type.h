#pragma once
#include "utility.h"
#include "strings.h"
#include "comp_utilities.h"
#include "names.h"
#include "errors.h"

struct CompilationUnit;
struct CallingConvention;
struct Span;

struct ASTFuncSig;
struct ASTLambda;
struct ASTStructBody;


using META_FLAGS = u8;

#define FLAGS_DECL(num) (1 << num)
enum struct META_FLAG : META_FLAGS {
  CONST      = FLAGS_DECL(0),
  COMPTIME   = FLAGS_DECL(1),
  //LITERAL    = FLAGS_DECL(2),
  ASSIGNABLE = FLAGS_DECL(3),
  CALL_LEAF  = FLAGS_DECL(4),
  MAKES_CALL = FLAGS_DECL(5),
};
#undef FLAGS_DECL

/*
  WARNING: For the following functions be careful of
  META_FLAG vs META_FLAGS (notice the trailing s)
*/

inline constexpr auto operator~(META_FLAG t) -> META_FLAGS {
  return ~(META_FLAGS)t;
}

inline constexpr auto operator|(META_FLAG t, META_FLAGS u) -> META_FLAGS {
  return u | ((META_FLAGS)t);
}

inline constexpr auto operator|(META_FLAGS u, META_FLAG t) -> META_FLAGS {
  return u | ((META_FLAGS)t);
}

inline constexpr auto operator|(META_FLAG u, META_FLAG t) -> META_FLAGS {
  return (META_FLAGS)u | (META_FLAGS)t;
}

inline constexpr auto operator|=(META_FLAGS& u, META_FLAG t) -> META_FLAGS& {
  u |= ((META_FLAGS)t);
  return u;
}

inline constexpr auto operator&(META_FLAG u, META_FLAG t) -> META_FLAGS {
  return (META_FLAGS)u & (META_FLAGS)t;
}

inline constexpr auto operator&(META_FLAG t, META_FLAGS u) -> META_FLAGS {
  return u & ((META_FLAGS)t);
}

inline constexpr auto operator&(META_FLAGS u, META_FLAG t) -> META_FLAGS {
  return u & ((META_FLAGS)t);
}

inline constexpr auto operator&=(META_FLAGS& u, META_FLAG t) -> META_FLAGS& {
  u &= ((META_FLAGS)t);
  return u;
}

constexpr void balance_flags(META_FLAGS* a, META_FLAGS* b) {
  constexpr META_FLAGS balanced_flags = 0
    | META_FLAG::CONST
    | META_FLAG::COMPTIME
    | META_FLAG::ASSIGNABLE;

  META_FLAGS balanced = (*a & *b) & balanced_flags;

  *a = (*a & ~balanced_flags) | balanced;
  *b = (*b & ~balanced_flags) | balanced;
}

constexpr void pass_meta_flags_up(META_FLAGS* low, META_FLAGS* high) {
  //balance_flags(low, high);

  constexpr META_FLAGS pass_flags = 0
    | META_FLAG::CALL_LEAF;

  *high |= *low & pass_flags;
}

constexpr void pass_meta_flags_down(META_FLAGS* low, META_FLAGS* high) {
  balance_flags(low, high);

  constexpr META_FLAGS pass_flags = 0
    | META_FLAG::MAKES_CALL;


  *low |= *high & pass_flags;
}

enum struct STRUCTURE_TYPE : u8 {
  VOID = 0,
  TYPE,
  INTEGER,
  POINTER,
  ASCII_CHAR,
  ENUM,
  COMPOSITE,
  FIXED_ARRAY,
  TUPLE,
  LAMBDA,
};

struct Structure {
  STRUCTURE_TYPE type = STRUCTURE_TYPE::VOID;
  IR::Format ir_format;

  u32 size;//bytes
  u32 alignment;//bytes
  const InternString* struct_name;
};

struct Type {
  const InternString* name;
  const Structure* structure;

  inline constexpr bool operator==(const Type& n) const {
    return (name == n.name) && (structure == n.structure);
  }

  inline constexpr bool operator!=(const Type& n) const {
    return (name != n.name) || (structure != n.structure);
  }

  /// Helper functions ///

  inline constexpr STRUCTURE_TYPE struct_type() const {
    return structure->type;
  }

  inline constexpr IR::Format struct_format() const {
    return structure->ir_format;
  }

  inline constexpr u32 size() const {
    return structure->size;
  }

#if 0
  inline constexpr const Structure* structure() const {
    return named_struct.structure;
  }
#endif

  template<typename T>
  inline constexpr const T* unchecked_base() const {
    return static_cast<const T*>(structure);
  }

  template<typename T>
  inline constexpr T* unchecked_base_mut() const {
    return static_cast<T*>(structure);
  }

  template<typename T>
  inline constexpr const T* extract_base() const {
    if (T::expected_type_enum == struct_type()) {
      return static_cast<const T*>(structure);
    }
    else {
      return nullptr;
    }
  }

  inline constexpr bool is_valid() const {
    return structure != nullptr;
  }
};

constexpr Type to_type(const Structure* s) {
  return { s->struct_name, s };
}

struct TypeAndFlags {
  META_FLAGS flags;
  Type type;
};

struct PointerStructure : public Structure {
  Type base ={};

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::POINTER;
  static OwnedArr<char> gen_name(const Type& nt);
};

struct ArrayStructure : public Structure {
  Type base ={};
  size_t length = 0;

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::FIXED_ARRAY;
  static OwnedArr<char> gen_name(const Type& nt, size_t length);
};

struct IntegerStructure : public Structure {
  bool is_signed = false;

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::INTEGER;
};

struct EnumStructure;

struct EnumValue {
  Type type ={};
  const InternString* name = nullptr;
  uint64_t representation = 0;
};

struct EnumStructure : public Structure {
  Type base ={};
  Array<const EnumValue*> enum_values ={};

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::ENUM;
  static OwnedArr<char> gen_name(const Type& nt);
};

struct TupleElement {
  uint32_t offset = 0;
  Type type ={};
};

struct TupleStructure : public Structure {
  Array<TupleElement> elements ={};

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::TUPLE;
};

struct StructElement {
  const InternString* name ={};
  uint32_t offset = 0;
  Type type ={};
};

struct CompositeStructure : public Structure {
  const ASTStructBody* declaration = nullptr;

  Array<StructElement> elements ={};

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::COMPOSITE;
};

struct SignatureStructure : public Structure {
  const CallingConvention* calling_convention = nullptr;

  Array<Type> parameter_types ={};
  Type return_type ={};

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::LAMBDA;
};

struct BuiltinTypes {
  Type t_bool ={};

  Type t_u8   ={};
  Type t_i8   ={};
  Type t_u16  ={};
  Type t_i16  ={};
  Type t_u32  ={};
  Type t_i32  ={};
  Type t_u64  ={};
  Type t_i64  ={};

  Type t_type ={};
  Type t_void ={};
  Type t_void_ptr ={};
  Type t_void_call = {};
  Type t_ascii ={};

  const EnumValue* e_false = nullptr;
  const EnumValue* e_true  = nullptr;

  //Returns nullptr for types that dont have a signed version or are already signed
  Type get_signed_of(const Type& ty);
};

struct Structures {
  FreelistBlockAllocator<TupleStructure> tuple_structures;
  FreelistBlockAllocator<IntegerStructure> int_structures;
  FreelistBlockAllocator<CompositeStructure> composite_structures;
  FreelistBlockAllocator<EnumStructure> enum_structures;
  FreelistBlockAllocator<Structure> base_structures;
  FreelistBlockAllocator<ArrayStructure> array_structures;
  FreelistBlockAllocator<PointerStructure> pointer_structures;
  FreelistBlockAllocator<SignatureStructure> lambda_structures;
  FreelistBlockAllocator<EnumValue> enum_values;

  Array<const EnumValue*> enums;
  Array<const Structure*> structures;

  ~Structures();
};

namespace STRUCTS {
  TupleStructure* new_tuple_structure(Structures* comp, StringInterner* strings, Array<Type>&& types);
  IntegerStructure* new_int_structure(Structures* comp, const InternString* name);
  CompositeStructure* new_composite_structure(Structures* comp, StringInterner* strings);
  Structure* new_base_structure(Structures* comp, const InternString* name);
  ArrayStructure* new_array_structure(Structures* comp, StringInterner* strings,
                                      const Type& base,
                                      size_t length);
  PointerStructure* new_pointer_structure(Structures* comp, StringInterner* strings, usize ptr_size, const Type& base);
  SignatureStructure* new_lambda_structure(Structures* comp, StringInterner* strings,
                                           usize ptr_size, const CallingConvention* conv,
                                           Array<Type>&& params,
                                           Type ret_type);
  EnumStructure* new_enum_structure(Structures* comp, StringInterner* strings, const Type&);
  EnumValue* new_enum_value(Structures* comp,
                            EnumStructure* enum_s,
                            const InternString* enum_name,
                            const InternString* value_name);
}

const ArrayStructure* find_or_make_array_structure(Structures* comp, StringInterner* strings, const Type& base, size_t length);

const PointerStructure* find_or_make_pointer_structure(Structures* comp, StringInterner* strings, usize ptr_size, const Type& base);

const TupleStructure* find_or_make_tuple_structure(Structures* comp, StringInterner* strings, Array<Type>&& types);

namespace TYPE_TESTS {
  constexpr inline bool match_sizes(const Structure* a, const Structure* b) {
    return a->size == b->size && a->alignment == b->alignment;
  }

  //No longer support implicit casts?
  //bool check_implicit_cast(META_FLAGS flags, const Type& from, const Type& to);

  bool is_negatable(const Structure* s);
  //bool is_logical(const Structure* s);
  bool is_numeric(const Structure* s);

  bool is_array(const Structure*);
  bool is_pointer(const Structure*);
  bool can_index(const Structure*);

  bool is_int(const Structure*);
  bool is_signed_int(const Structure*);
  bool is_unsigned_int(const Structure*);

  bool is_64_bit_int(const Structure*);
  bool is_signed_64_bit_int(const Structure*);
  bool is_unsigned_64_bit_int(const Structure*);

  bool is_8_bit_int(const Structure*);
  bool is_signed_8_bit_int(const Structure*);
  bool is_unsigned_8_bit_int(const Structure*);

  inline bool match_sizes(const Type& a, const Type& b) {
    return match_sizes(a.structure, b.structure);
  }

  inline bool can_index(const Type& t) {
    return can_index(t.structure);
  }

  inline bool is_pointer(const Type& t) {
    return is_pointer(t.structure);
  }

  inline bool is_array(const Type& t) {
    return is_array(t.structure);
  }

  inline bool is_int(const Type& t) {
    return is_int(t.structure);
  }

  inline bool is_signed_int(const Type& t) {
    return is_signed_int(t.structure);
  }

  inline bool is_unsigned_int(const Type& t) {
    return is_unsigned_int(t.structure);
  }

  inline bool is_64_bit_int(const Type& t) {
    return is_64_bit_int(t.structure);
  }

  inline bool is_unsigned_64_bit_int(const Type& t) {
    return is_unsigned_64_bit_int(t.structure);
  }

  inline bool is_signed_64_bit_int(const Type& t) {
    return is_signed_64_bit_int(t.structure);
  }
}