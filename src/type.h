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
  constexpr Axle::ViewArr<const char> category_name(VALUE_CATEGORY vc) {
    switch (vc) {
      case VALUE_CATEGORY::TEMPORARY_CONSTANT: return Axle::lit_view_arr("Temporary Constant");
      case VALUE_CATEGORY::TEMPORARY_IMMUTABLE: return Axle::lit_view_arr("Temporary Immutable");

      case VALUE_CATEGORY::VARIABLE_CONSTANT: return Axle::lit_view_arr("Variable Constant");
      case VALUE_CATEGORY::VARIABLE_IMMUTABLE: return Axle::lit_view_arr("Variable Immutable");
      case VALUE_CATEGORY::VARIABLE_MUTABLE: return Axle::lit_view_arr("Variable Mutable");
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

namespace Axle::Format {
  template<>
  struct FormatArg<VALUE_CATEGORY> {
    template<Formatter F>
    constexpr static void load_string(F& res, VALUE_CATEGORY vc) {
      Axle::ViewArr<const char> str = VC::category_name(vc);
      res.load_string(str.data, str.size);
    }
  };
}

enum struct STRUCTURE_TYPE : u8 {
  VOID = 0,
  TYPE,
  INTEGER,
  POINTER,
  SLICE,
  ENUM,
  COMPOSITE,
  FIXED_ARRAY,
  TUPLE,
  LAMBDA,
};

namespace Axle::Format {
  template<>
  struct FormatArg<STRUCTURE_TYPE> {
    template<Formatter F>
    constexpr static void load_string(F& res, STRUCTURE_TYPE st) {
      switch(st) {
        case STRUCTURE_TYPE::VOID: res.load_string_lit("STRUCTURE_TYPE::VOID"); return;
        case STRUCTURE_TYPE::TYPE: res.load_string_lit("STRUCTURE_TYPE::TYPE"); return;
        case STRUCTURE_TYPE::INTEGER: res.load_string_lit("STRUCTURE_TYPE::INTEGER"); return;
        case STRUCTURE_TYPE::POINTER: res.load_string_lit("STRUCTURE_TYPE::POINTER"); return;
        case STRUCTURE_TYPE::SLICE: res.load_string_lit("STRUCTURE_TYPE::SLICE"); return;
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
  const Axle::InternString* struct_name;
};

struct Type {
  const Axle::InternString* name;
  const Structure* structure;

  inline constexpr bool operator==(const Type& n) const {
    return (name == n.name) && (structure == n.structure);
  }

  inline constexpr bool operator!=(const Type& n) const {
    return (name != n.name) || (structure != n.structure);
  }

  /// Helper functions ///

  inline constexpr STRUCTURE_TYPE struct_type() const {
    ASSERT(structure != nullptr);
    return structure->type;
  }

  inline constexpr IR::Format struct_format() const {
    ASSERT(structure != nullptr);
    return structure->ir_format;
  }

  inline constexpr u32 size() const {
    ASSERT(structure != nullptr);
    return structure->size;
  }

  template<typename T>
  inline constexpr const T* unchecked_base() const {
    ASSERT(structure != nullptr);
    ASSERT(structure->type == T::expected_type_enum);
    return static_cast<const T*>(structure);
  }

  template<typename T>
  inline constexpr T* unchecked_base_mut() const {
    ASSERT(structure != nullptr);
    ASSERT(structure->type == T::expected_type_enum);
    return static_cast<T*>(structure);
  }

  inline constexpr bool is_valid() const {
    return structure != nullptr;
  }
};

namespace Axle::Format {
  template<>
  struct FormatArg<Type> {
    template<Formatter F>
    constexpr static void load_string(F& res, const Type& ty) {
      if(ty.structure == nullptr) {
        res.load_string_lit("<invalid-type>");
      }
      else {
        FormatArg<const Axle::InternString*>::load_string(res, ty.name);
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

struct SliceStructure : public Structure {
  Type base ={};
  bool is_mut = false;//TODO: move this to be part of the type

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::SLICE;
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
  const Axle::InternString* name = nullptr;
  uint64_t representation = 0;
};

struct EnumStructure : public Structure {
  Type base ={};
  Axle::Array<const EnumValue*> enum_values ={};

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::ENUM;
};

struct TupleElement {
  uint32_t offset = 0;
  Type type ={};
};

struct TupleStructure : public Structure {
  Axle::OwnedArr<TupleElement> elements ={};

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::TUPLE;
};

struct StructElement {
  const Axle::InternString* name ={};
  uint32_t offset = 0;
  Type type ={};
};

struct CompositeStructure : public Structure {
  const ASTStructBody* declaration = nullptr;

  Axle::OwnedArr<StructElement> elements ={};

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::COMPOSITE;
};

struct SignatureStructure : public Structure {
  const CallingConvention* calling_convention = nullptr;

  Axle::OwnedArr<Type> parameter_types ={};
  Type return_type ={};

  constexpr static STRUCTURE_TYPE expected_type_enum = STRUCTURE_TYPE::LAMBDA;
};

namespace visit_detail {
  template<typename ... T>
  struct TypeArr {};

  template<typename Holder, typename Holder2>
  struct visitor;
  
  template<typename ... Ts1, typename T, typename ... Ts2>
  struct visitor<TypeArr<Ts1...>, TypeArr<T, Ts2...>> {
    
    template<typename T_new>
    using Next = visitor<TypeArr<Ts1..., T_new>, TypeArr<Ts2...>>;

    template<typename V>
    static constexpr auto visit(V&& v, const Ts1& ... pre_args, const T& t, const Ts2& ... post_args) {
      using PtrType = std::remove_const_t<std::remove_pointer_t<T>>;
      static_assert(std::derived_from<PtrType, Structure>, "Must be a strucutre");
      
      if constexpr (std::same_as<const Structure*, T>) {
        switch (t->type) {
#define FORWARD_T(case_t, cast_t) \
case case_t: \
return Next<cast_t>::visit(std::forward<V>(v), pre_args..., static_cast<cast_t>(t), post_args...)

          FORWARD_T(STRUCTURE_TYPE::VOID, const VoidStructure*);
          FORWARD_T(STRUCTURE_TYPE::TYPE, const TypeStructure*);
          FORWARD_T(STRUCTURE_TYPE::INTEGER, const IntegerStructure*);
          FORWARD_T(STRUCTURE_TYPE::POINTER, const PointerStructure*);
          FORWARD_T(STRUCTURE_TYPE::ENUM, const EnumStructure*);
          FORWARD_T(STRUCTURE_TYPE::COMPOSITE, const CompositeStructure*);
          FORWARD_T(STRUCTURE_TYPE::FIXED_ARRAY, const ArrayStructure*);
          FORWARD_T(STRUCTURE_TYPE::TUPLE, const TupleStructure*);
          FORWARD_T(STRUCTURE_TYPE::LAMBDA, const SignatureStructure*);
          FORWARD_T(STRUCTURE_TYPE::SLICE, const SliceStructure*);
        }
      
        INVALID_CODE_PATH("Invalid structure type");
      }
      else {
        return Next<T>::visit(std::forward<V>(v), pre_args..., t, post_args...);
      }
    }

  };

  template<typename ... Ts>
  struct visitor<TypeArr<Ts...>, TypeArr<>> {

    template<typename V>
    static constexpr auto visit(V&& v, const Ts& ... args) {
      return std::forward<V>(v)(args...);  
    }
  };
}

template<typename V, typename ... Ts>
constexpr auto visit_types(V&& visitor, const Ts& ... structs) {
  return visit_detail::visitor<visit_detail::TypeArr<>, visit_detail::TypeArr<Ts...>>::visit(std::forward<V>(visitor), structs...);
}

template<typename V>
constexpr auto visit_ir_type(V&& visitor, IR::Format format, Axle::ViewArr<const u8> data) {
  switch(format) {
    case IR::Format::uint8: {
      return std::forward<V>(visitor).template operator()<u8>(data);
    }
    case IR::Format::uint16: {
      return std::forward<V>(visitor).template operator()<u16>(data);
    }
    case IR::Format::uint32: {
      return std::forward<V>(visitor).template operator()<u32>(data);
    }
    case IR::Format::uint64: {
      return std::forward<V>(visitor).template operator()<u64>(data);
    }
    case IR::Format::sint8: {
      return std::forward<V>(visitor).template operator()<i8>(data);
    }
    case IR::Format::sint16: {
      return std::forward<V>(visitor).template operator()<i16>(data);
    }
    case IR::Format::sint32: {
      return std::forward<V>(visitor).template operator()<i32>(data);
    }
    case IR::Format::sint64: {
      return std::forward<V>(visitor).template operator()<i64>(data);
    }
    case IR::Format::opaque:
    case IR::Format::pointer:
    case IR::Format::slice: {
      INVALID_CODE_PATH("TODO: dont yet support visiting these");
    }
  }

  INVALID_CODE_PATH("Unexpected IR type");
}

struct PrintSignatureType {
  const SignatureStructure* sig;
};

namespace Axle::Format {
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
          FormatArg<const Axle::InternString*>::load_string(res, i->name);
          res.load_string_lit(", ");
        }

        FormatArg<const Axle::InternString*>::load_string(res, i->name);
      }

      res.load_string_lit(") -> ");
      FormatArg<const Axle::InternString*>::load_string(res, sig->return_type.name);
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
  usize pointer_align;
  usize slice_size;
  usize slice_align;

  VoidStructure s_void;
  TypeStructure s_type;

  Axle::FreelistBlockAllocator<TupleStructure> tuple_structures;
  Axle::FreelistBlockAllocator<IntegerStructure> int_structures;
  Axle::FreelistBlockAllocator<CompositeStructure> composite_structures;
  Axle::FreelistBlockAllocator<EnumStructure> enum_structures;
  Axle::FreelistBlockAllocator<ArrayStructure> array_structures;
  Axle::FreelistBlockAllocator<PointerStructure> pointer_structures;
  Axle::FreelistBlockAllocator<SliceStructure> slice_structures;
  Axle::FreelistBlockAllocator<SignatureStructure> lambda_structures;
  Axle::FreelistBlockAllocator<EnumValue> enum_values;

  Axle::Array<const EnumValue*> enums;
  Axle::Array<const Structure*> structures;

  constexpr Structures(usize ptr_s, usize ptr_a) :
    pointer_size{ptr_s}, pointer_align{ptr_a},
    slice_size{Axle::ceil_to_n<usize>(ptr_s + 8, 8)},
    slice_align{Axle::larger<usize>(ptr_a, 8)}
  {
    ASSERT(slice_align % 8 == 0);
    ASSERT(slice_align % ptr_a == 0);
  }

  ~Structures();
};

namespace STRUCTS {
  TupleStructure* new_tuple_structure(Structures* comp, Axle::StringInterner* strings, const Axle::ViewArr<const Type>& types);
  IntegerStructure* new_int_structure(Structures* comp, const Axle::InternString* name);
  CompositeStructure* new_composite_structure(Structures* comp, Axle::StringInterner* strings);
  Structure* new_base_structure(Structures* comp, const Axle::InternString* name);
  ArrayStructure* new_array_structure(Structures* comp, Axle::StringInterner* strings,
                                      const Type& base,
                                      size_t length);
  PointerStructure* new_pointer_structure(Structures* comp, Axle::StringInterner* strings, const Type& base);
  SliceStructure* new_slice_structure(Structures* comp, Axle::StringInterner* strings, const Type& base);
  SignatureStructure* new_lambda_structure(Structures* comp, Axle::StringInterner* strings,
                                           const CallingConvention* conv,
                                           Axle::OwnedArr<Type>&& params,
                                           Type ret_type);
  EnumStructure* new_enum_structure(Structures* comp, Axle::StringInterner* strings, const Type&);
  EnumValue* new_enum_value(Structures* comp,
                            EnumStructure* enum_s,
                            const Axle::InternString* enum_name,
                            const Axle::InternString* value_name);

  BuiltinTypes create_builtins(Structures* structures, Axle::StringInterner* strings);
}

const ArrayStructure* find_or_make_array_structure(Structures* comp, Axle::StringInterner* strings, const Type& base, size_t length);

const PointerStructure* find_or_make_pointer_structure(Structures* comp, Axle::StringInterner* strings, const Type& base);
const SliceStructure* find_or_make_slice_structure(Structures* comp, Axle::StringInterner* strings, const Type& base);

const TupleStructure* find_or_make_tuple_structure(Structures* comp, Axle::StringInterner* strings,
                                                   const Axle::ViewArr<const Type>& types);

const SignatureStructure* find_or_make_lambda_structure(Structures* const structures,
                                                        Axle::StringInterner* strings,
                                                        const CallingConvention* conv,
                                                        Axle::OwnedArr<Type>&& params,
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
  bool is_slice(const Structure*);
  bool can_index(const Structure*);
  bool can_slice(const Structure*);

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

  inline bool can_slice(const Type& t) {
    return can_slice(t.structure);
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
