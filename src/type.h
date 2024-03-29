#pragma once
#include <AxleUtil/utility.h>
#include <AxleUtil/strings.h>
#include <AxleUtil/formattable.h>

#include <Axle/comp_utilities.h>
#include "names.h"
#include "errors.h"

struct CompilationUnit;
struct CallingConvention;
struct Span;

struct ASTFuncSig;
struct ASTLambda;
struct ASTStructBody;

enum struct VALUE_CATEGORY : u8 {
  TEMPORARY_CONSTANT,
  TEMPORARY_IMMUTABLE,

  VARIABLE_CONSTANT,
  VARIABLE_IMMUTABLE,
  VARIABLE_MUTABLE,
};

namespace VC {
  constexpr ViewArr<const char> category_name(VALUE_CATEGORY vc) {
    switch (vc) {
      case VALUE_CATEGORY::TEMPORARY_CONSTANT: return lit_view_arr("Temporary Constant");
      case VALUE_CATEGORY::TEMPORARY_IMMUTABLE: return lit_view_arr("Temporary Immutable");

      case VALUE_CATEGORY::VARIABLE_CONSTANT: return lit_view_arr("Variable Constant");
      case VALUE_CATEGORY::VARIABLE_IMMUTABLE: return lit_view_arr("Variable Immutable");
      case VALUE_CATEGORY::VARIABLE_MUTABLE: return lit_view_arr("Variable Mutable");
    }

    return {};
  }

  constexpr bool is_mutable(VALUE_CATEGORY vc) {
    return vc == VALUE_CATEGORY::VARIABLE_MUTABLE;
  }

  constexpr bool is_addressable(VALUE_CATEGORY vc) {
    switch (vc) {
      case VALUE_CATEGORY::TEMPORARY_CONSTANT:
      case VALUE_CATEGORY::TEMPORARY_IMMUTABLE: return false;

      case VALUE_CATEGORY::VARIABLE_CONSTANT: 
      case VALUE_CATEGORY::VARIABLE_IMMUTABLE:
      case VALUE_CATEGORY::VARIABLE_MUTABLE: return true;
    }

    return false;
  }

  constexpr bool is_variable(VALUE_CATEGORY vc) {
    switch (vc) {
      case VALUE_CATEGORY::TEMPORARY_CONSTANT:
      case VALUE_CATEGORY::TEMPORARY_IMMUTABLE: return false;

      case VALUE_CATEGORY::VARIABLE_CONSTANT:
      case VALUE_CATEGORY::VARIABLE_IMMUTABLE:
      case VALUE_CATEGORY::VARIABLE_MUTABLE: return true;
    }

    return false;
  }

  constexpr bool is_comptime(VALUE_CATEGORY vc) {
    switch (vc) {
      case VALUE_CATEGORY::TEMPORARY_CONSTANT:
      case VALUE_CATEGORY::VARIABLE_CONSTANT: return true;

      case VALUE_CATEGORY::TEMPORARY_IMMUTABLE:
      case VALUE_CATEGORY::VARIABLE_IMMUTABLE:
      case VALUE_CATEGORY::VARIABLE_MUTABLE: return false;
    }

    return false;
  }
}

namespace Format {
  template<>
  struct FormatArg<VALUE_CATEGORY> {
    template<Formatter F>
    constexpr static void load_string(F& res, VALUE_CATEGORY vc) {
      ViewArr<const char> str = VC::category_name(vc);
      res.load_string(str.data, str.size);
    }
  };
}

enum struct STRUCTURE_TYPE : u8 {
  VOID = 0,
  TYPE,
  INTEGER,
  POINTER,
  ENUM,
  COMPOSITE,
  FIXED_ARRAY,
  TUPLE,
  LAMBDA,
};

namespace Format {
  template<>
  struct FormatArg<STRUCTURE_TYPE> {
    template<Formatter F>
    constexpr static void load_string(F& res, STRUCTURE_TYPE st) {
      switch(st) {
        case STRUCTURE_TYPE::VOID: res.load_string_lit("STRUCTURE_TYPE::VOID"); return;
        case STRUCTURE_TYPE::TYPE: res.load_string_lit("STRUCTURE_TYPE::TYPE"); return;
        case STRUCTURE_TYPE::INTEGER: res.load_string_lit("STRUCTURE_TYPE::INTEGER"); return;
        case STRUCTURE_TYPE::POINTER: res.load_string_lit("STRUCTURE_TYPE::POINTER"); return;
        case STRUCTURE_TYPE::ENUM: res.load_string_lit("STRUCTURE_TYPE::ENUM"); return;
        case STRUCTURE_TYPE::COMPOSITE: res.load_string_lit("STRUCTURE_TYPE::COMPOSITE"); return;
        case STRUCTURE_TYPE::FIXED_ARRAY: res.load_string_lit("STRUCTURE_TYPE::FIXED_ARRAY"); return;
        case STRUCTURE_TYPE::TUPLE: res.load_string_lit("STRUCTURE_TYPE::TUPLE"); return;
        case STRUCTURE_TYPE::LAMBDA: res.load_string_lit("STRUCTURE_TYPE::LAMBDA"); return;
      }

      res.load_string_lit("STRUCTURE_TYPE::<unknown>");
    }
  };
}

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

  template<typename T>
  inline constexpr const T* unchecked_base() const {
    ASSERT(structure->type == T::expected_type_enum);
    return static_cast<const T*>(structure);
  }

  template<typename T>
  inline constexpr T* unchecked_base_mut() const {
    ASSERT(structure->type == T::expected_type_enum);
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

namespace Format {
  template<>
  struct FormatArg<Type> {
    template<Formatter F>
    constexpr static void load_string(F& res, const Type& ty) {
      if(ty.structure == nullptr) {
        res.load_string_lit("<invalid-type>");
      }
      else {
        FormatArg<const InternString*>::load_string(res, ty.name);
      }
    }
  };
}

constexpr Type to_type(const Structure* s) {
  return { s->struct_name, s };
}

struct VoidStructure : public Structure {
  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::VOID;
};

struct TypeStructure : public Structure {
  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::TYPE;
};

struct PointerStructure : public Structure {
  Type base ={};
  bool is_mut = false;//TODO: move this to be part of the type

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::POINTER;
};

struct ArrayStructure : public Structure {
  Type base ={};
  size_t length = 0;

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::FIXED_ARRAY;
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
};

struct TupleElement {
  uint32_t offset = 0;
  Type type ={};
};

struct TupleStructure : public Structure {
  OwnedArr<TupleElement> elements ={};

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::TUPLE;
};

struct StructElement {
  const InternString* name ={};
  uint32_t offset = 0;
  Type type ={};
};

struct CompositeStructure : public Structure {
  const ASTStructBody* declaration = nullptr;

  OwnedArr<StructElement> elements ={};

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::COMPOSITE;
};

struct SignatureStructure : public Structure {
  const CallingConvention* calling_convention = nullptr;

  OwnedArr<Type> parameter_types ={};
  Type return_type ={};

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::LAMBDA;
};

struct PrintSignatureType {
  const SignatureStructure* sig;
};

namespace Format {
  template<>
  struct FormatArg<PrintSignatureType> {
    template<Formatter F>
    constexpr static void load_string(F& res, PrintSignatureType p_sig) {
      const SignatureStructure* sig = p_sig.sig;

      res.load_char('(');

      auto i = sig->parameter_types.begin();
      const auto end = sig->parameter_types.end();

      if (i < end) {
        for (; i < (end - 1); i++) {
          FormatArg<const InternString*>::load_string(res, i->name);
          res.load_string_lit(", ");
        }

        FormatArg<const InternString*>::load_string(res, i->name);
      }

      res.load_string_lit(") -> ");
      FormatArg<const InternString*>::load_string(res, sig->return_type.name);
    }
  };
}

struct BuiltinTypes {
  Type t_type ={};
  Type t_void ={};

  Type t_u8   ={};
  Type t_i8   ={};
  Type t_u16  ={};
  Type t_i16  ={};
  Type t_u32  ={};
  Type t_i32  ={};
  Type t_u64  ={};
  Type t_i64  ={};

  Type t_void_ptr ={};
  Type t_void_call = {};
  Type t_ascii ={};

  Type t_bool ={};
  const EnumValue* e_true  = nullptr;
  const EnumValue* e_false = nullptr;
};

struct Structures {
  usize pointer_size;

  VoidStructure s_void;
  TypeStructure s_type;

  FreelistBlockAllocator<TupleStructure> tuple_structures;
  FreelistBlockAllocator<IntegerStructure> int_structures;
  FreelistBlockAllocator<CompositeStructure> composite_structures;
  FreelistBlockAllocator<EnumStructure> enum_structures;
  FreelistBlockAllocator<ArrayStructure> array_structures;
  FreelistBlockAllocator<PointerStructure> pointer_structures;
  FreelistBlockAllocator<SignatureStructure> lambda_structures;
  FreelistBlockAllocator<EnumValue> enum_values;

  Array<const EnumValue*> enums;
  Array<const Structure*> structures;

  ~Structures();
};

namespace STRUCTS {
  TupleStructure* new_tuple_structure(Structures* comp, StringInterner* strings, const ViewArr<const Type>& types);
  IntegerStructure* new_int_structure(Structures* comp, const InternString* name);
  CompositeStructure* new_composite_structure(Structures* comp, StringInterner* strings);
  Structure* new_base_structure(Structures* comp, const InternString* name);
  ArrayStructure* new_array_structure(Structures* comp, StringInterner* strings,
                                      const Type& base,
                                      size_t length);
  PointerStructure* new_pointer_structure(Structures* comp, StringInterner* strings, const Type& base);
  SignatureStructure* new_lambda_structure(Structures* comp, StringInterner* strings,
                                           const CallingConvention* conv,
                                           OwnedArr<Type>&& params,
                                           Type ret_type);
  EnumStructure* new_enum_structure(Structures* comp, StringInterner* strings, const Type&);
  EnumValue* new_enum_value(Structures* comp,
                            EnumStructure* enum_s,
                            const InternString* enum_name,
                            const InternString* value_name);

  BuiltinTypes create_builtins(Structures* structures, StringInterner* strings);
}

const ArrayStructure* find_or_make_array_structure(Structures* comp, StringInterner* strings, const Type& base, size_t length);

const PointerStructure* find_or_make_pointer_structure(Structures* comp, StringInterner* strings, const Type& base);

const TupleStructure* find_or_make_tuple_structure(Structures* comp, StringInterner* strings,
                                                   const ViewArr<const Type>& types);

const SignatureStructure* find_or_make_lambda_structure(Structures* const structures,
                                                        StringInterner* strings,
                                                        const CallingConvention* conv,
                                                        OwnedArr<Type>&& params,
                                                        Type ret_type);

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
