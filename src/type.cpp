#include <AxleUtil/format.h>

#include "compiler.h"
#include "type.h"

#ifdef AXLE_TRACING
#include <Tracer/trace.h>
#endif

namespace Format = Axle::Format;

TupleStructure* STRUCTS::new_tuple_structure(Structures* structures, Axle::StringInterner* strings,
                                             const Axle::ViewArr<const Type>& types) {
  TELEMETRY_FUNCTION();

  TupleStructure* const type = structures->tuple_structures.allocate();

  type->type = STRUCTURE_TYPE::TUPLE;
  type->ir_format = IR::Format::opaque;

  //Load the name
  {
    type->elements = Axle::new_arr<TupleElement>(types.size);

    uint32_t current_size = 0;
    uint32_t current_align = 0;

    Format::ArrayFormatter name = {};

    auto i = types.begin();
    const auto end = types.end();
    auto tup_el = type->elements.mut_begin();
    auto tup_el_end = type->elements.mut_end();

    if (i < end) {
      ASSERT(tup_el < tup_el_end);

      Format::format_to(name, "({}", i->name);

      tup_el->type = *i;
      tup_el->offset = current_size;

      current_size = Axle::ceil_to_n(current_size, i->structure->alignment);
      current_size += i->structure->size;

      current_align = Axle::larger(i->structure->alignment, current_align);

      i++;
      tup_el++;

      for (; i < end; (i++, tup_el++)) {
        ASSERT(tup_el < tup_el_end);
        Format::format_to(name, ", {}", i->name);

        tup_el->type = *i;
        tup_el->offset = current_size;

        current_size = Axle::ceil_to_n(current_size, i->structure->alignment);
        current_size += i->structure->size;

        current_align = Axle::larger(i->structure->alignment, current_align);
      }

      Format::format_to(name, ")");
    }
    else {
      Format::format_to(name, "()");
    }

    name.load_char('\n');

    type->size = current_size;
    type->alignment = current_align;

    type->struct_name = strings->intern(Axle::view_arr(name));
  }

  structures->structures.insert(type);

  return type;
}

SignatureStructure* STRUCTS::new_lambda_structure(Structures* structures, Axle::StringInterner* strings,
                                                  const CallingConvention* conv,
                                                  Axle::OwnedArr<Type>&& params,
                                                  Type ret_type) {
  TELEMETRY_FUNCTION();
  SignatureStructure* type = structures->lambda_structures.allocate();
  type->type = STRUCTURE_TYPE::LAMBDA;
  type->ir_format = IR::Format::opaque;
  type->parameter_types = std::move(params);
  type->return_type = ret_type;
  type->calling_convention = conv;
  type->size = (u32)structures->pointer_size;
  type->alignment = (u32)structures->pointer_size;

  {
    Format::ArrayFormatter name = {};

    auto i = type->parameter_types.begin();
    auto end = type->parameter_types.end();

    name.load_char('(');

    if (i < end) {
      Format::format_to(name, "{}", i->name);
      i++;
    }

    for (; i < end; i++) {
      Format::format_to(name, ", {}", i->name);
    }

    Format::format_to(name, ") -> {}", type->return_type.name);

    type->struct_name = strings->intern(Axle::view_arr(name));
  }

  structures->structures.insert(type);

  return type;
}


IntegerStructure* STRUCTS::new_int_structure(Structures* structures, const Axle::InternString* name) {
  TELEMETRY_FUNCTION();

  IntegerStructure* const type = structures->int_structures.allocate();
  type->type = STRUCTURE_TYPE::INTEGER;
  type->struct_name = name;
  structures->structures.insert(type);

  return type;
}

CompositeStructure* STRUCTS::new_composite_structure(Structures* structures, Axle::StringInterner* strings) {
  TELEMETRY_FUNCTION();

  CompositeStructure* const type = structures->composite_structures.allocate();
  type->type = STRUCTURE_TYPE::COMPOSITE;
  type->ir_format = IR::Format::opaque;
  type->struct_name = strings->intern(Axle::lit_view_arr("anoymous-struct"));

  structures->structures.insert(type);

  return type;
}

EnumStructure* STRUCTS::new_enum_structure(Structures* structures, Axle::StringInterner* strings, const Type& base) {
  TELEMETRY_FUNCTION();


  EnumStructure* const type = structures->enum_structures.allocate();
  type->type = STRUCTURE_TYPE::ENUM;
  type->ir_format = base.struct_format();
  type->struct_name = strings->format_intern("anoymous-enum({})", base.name);
  type->base = base;

  type->size = base.structure->size;
  type->alignment = base.structure->alignment;

  structures->structures.insert(type);

  return type;
}

ArrayStructure* STRUCTS::new_array_structure(Structures* structures, Axle::StringInterner* strings, const Type& base,
                                             size_t length) {
  TELEMETRY_FUNCTION();

  ArrayStructure* const type = structures->array_structures.allocate();
  type->type = STRUCTURE_TYPE::FIXED_ARRAY;
  type->ir_format = IR::Format::opaque;
  type->base = base;
  type->length = length;
  type->struct_name = strings->format_intern("[{}; {}]", base.name, length);
  type->size = base.structure->size * (u32)length;
  type->alignment = base.structure->alignment;

  structures->structures.insert(type);

  return type;
}

PointerStructure* STRUCTS::new_pointer_structure(Structures* structures,
                                                 Axle::StringInterner* strings,
                                                 const Type& base) {
  TELEMETRY_FUNCTION();
  
  PointerStructure* const type = structures->pointer_structures.allocate();
  type->type = STRUCTURE_TYPE::POINTER;
  type->ir_format = IR::Format::pointer;
  type->base = base;
  type->struct_name = strings->format_intern("*{}", base.name);
  type->size = (u32)structures->pointer_size;
  type->alignment = (u32)structures->pointer_align;

  structures->structures.insert(type);

  return type;
}

SliceStructure* STRUCTS::new_slice_structure(Structures* structures,
                                             Axle::StringInterner* strings,
                                             const Type& base) {
  TELEMETRY_FUNCTION();
  
  SliceStructure* const type = structures->slice_structures.allocate();
  type->type = STRUCTURE_TYPE::SLICE;
  type->ir_format = IR::Format::slice;
  type->base = base;
  type->struct_name = strings->format_intern("[]{}", base.name);
  type->size = (u32)structures->slice_size;
  type->alignment = (u32)structures->slice_align;

  structures->structures.insert(type);

  return type;
}

EnumValue* STRUCTS::new_enum_value(Structures* structures,
                                   EnumStructure* enum_s,
                                   const Axle::InternString* enum_name,
                                   const Axle::InternString* value_name) {
  TELEMETRY_FUNCTION();

  EnumValue* const val = structures->enum_values.allocate();
  val->type = Type{ enum_name, enum_s };
  val->name = value_name;

  enum_s->enum_values.insert(val);
  structures->enums.insert(val);

  return val;
}

const ArrayStructure* find_or_make_array_structure(Structures* const structures,
                                                   Axle::StringInterner* strings,
                                                   const Type& base, size_t length) {
  TELEMETRY_FUNCTION();

  {
    auto i = structures->structures.begin();
    const auto end = structures->structures.end();

    for (; i < end; i++) {
      const Structure* s = *i;
      if (s->type == STRUCTURE_TYPE::FIXED_ARRAY) {
        //Is array
        const ArrayStructure* as = static_cast<const ArrayStructure*>(s);
        if (as->base == base
            && as->length == length) {
          //Is same
          return as;
        }
      }
    }
  }

  //Doesnt exist - need to make new type
  return STRUCTS::new_array_structure(structures, strings, base, length);
}

const PointerStructure* find_or_make_pointer_structure(Structures* const structures, Axle::StringInterner* strings,
                                                       const Type& base) {
  TELEMETRY_FUNCTION();

  {
    auto i = structures->structures.begin();
    const auto end = structures->structures.end();

    for (; i < end; i++) {
      const Structure* s = *i;
      if (s->type == STRUCTURE_TYPE::POINTER) {
        //Is pointer
        const PointerStructure* ps = static_cast<const PointerStructure*>(s);
        if (ps->base == base) {
          //Is same
          return ps;
        }
      }
    }
  }

  //Doesnt exist - need to make new type
  return STRUCTS::new_pointer_structure(structures, strings, base);
}

const SliceStructure* find_or_make_slice_structure(Structures* const structures, Axle::StringInterner* strings,
                                                       const Type& base) {
  TELEMETRY_FUNCTION();

  {
    auto i = structures->structures.begin();
    const auto end = structures->structures.end();

    for (; i < end; i++) {
      const Structure* s = *i;
      if (s->type == STRUCTURE_TYPE::SLICE) {
        //Is pointer
        const SliceStructure* ps = static_cast<const SliceStructure*>(s);
        if (ps->base == base) {
          //Is same
          return ps;
        }
      }
    }
  }

  //Doesnt exist - need to make new type
  return STRUCTS::new_slice_structure(structures, strings, base);
}

const TupleStructure* find_or_make_tuple_structure(Structures* const structures, Axle::StringInterner* strings,
                                                   const Axle::ViewArr<const Type>& els) {
  TELEMETRY_FUNCTION();

  {
    auto i = structures->structures.begin();
    const auto end = structures->structures.end();

    for (; i < end; i++) {
      const Structure* s = *i;
      if (s->type == STRUCTURE_TYPE::TUPLE) {
        const TupleStructure* tls = static_cast<const TupleStructure*>(s);

        //Not same size
        if (els.size != tls->elements.size) { continue; }

        //empty
        if (els.size == 0) { return tls; }

        auto el_i = els.begin();
        auto tl_i = tls->elements.begin();

        const auto el_end = els.end();

        for (; el_i < el_end; tl_i++, el_i++) {
          if (*el_i != tl_i->type) {
            goto NOT_SAME;
          }
        }

        return tls;
      }

    NOT_SAME:
      continue;
    }
  }


  //Doesnt exist - need to make new type
  return STRUCTS::new_tuple_structure(structures, strings, els);
}

const SignatureStructure* find_or_make_lambda_structure(Structures* const structures,
                                                        Axle::StringInterner* strings,
                                                        const CallingConvention* conv,
                                                        Axle::OwnedArr<Type>&& params,
                                                        Type ret_type) {
  TELEMETRY_FUNCTION();

  {
    auto i = structures->structures.begin();
    auto end = structures->structures.end();

    for (; i < end; i++) {
      const Structure* s = *i;
      if (s->type != STRUCTURE_TYPE::LAMBDA) { continue; }

      const SignatureStructure* sig_struct = (const SignatureStructure*)s;
      if (sig_struct->calling_convention != conv) { continue; }
      if (sig_struct->return_type != ret_type) { continue; }
      if (sig_struct->parameter_types.size != params.size) { continue; }

      {
        auto p_i = sig_struct->parameter_types.begin();
        auto p_end = sig_struct->parameter_types.end();
        auto pin_i = params.begin();

        for (; p_i < p_end; p_i++, pin_i++) {
          if (*p_i != *pin_i) {
            goto NOT_SAME;
          }
        }
      }

      //Is same!
      return sig_struct;

    NOT_SAME:
      continue;
    }
  }

  return STRUCTS::new_lambda_structure(structures, strings, conv, std::move(params), ret_type);
}



BuiltinTypes STRUCTS::create_builtins(Structures* structures, Axle::StringInterner* strings) {
  BuiltinTypes builtin_types = {};

  {
    TypeStructure* const s_type = &structures->s_type;
    s_type->type = s_type->expected_type_enum;
    s_type->struct_name = strings->intern("type", 4);

    s_type->size = sizeof(Type);
    s_type->alignment = alignof(Type);


    builtin_types.t_type = to_type(s_type);
  }

  {
    VoidStructure* s_void = &structures->s_void;
    s_void->type = s_void->expected_type_enum;
    s_void->struct_name = strings->intern("void", 4);
    s_void->size = 0;
    s_void->alignment = s_void->alignment;

    builtin_types.t_void = to_type(s_void);
  }

  {
    const auto int_type = [&](const auto& name, bool is_signed, u32 size, IR::Format ir_format, Type* t) {
      IntegerStructure* s = STRUCTS::new_int_structure(structures, strings->intern(Axle::lit_view_arr(name)));
      s->is_signed = is_signed;
      s->size = size;
      s->alignment = size;
      s->ir_format = ir_format;

      *t = to_type(s);
    };

    int_type("u8", false, 1, IR::Format::uint8, &builtin_types.t_u8);
    int_type("ascii", false, 1, IR::Format::uint8, &builtin_types.t_ascii);

    int_type("i8", true, 1, IR::Format::sint8, &builtin_types.t_i8);
    int_type("u16", false, 2, IR::Format::uint16, &builtin_types.t_u16);
    int_type("i16", true, 2, IR::Format::sint16, &builtin_types.t_i16);
    int_type("u32", false, 4, IR::Format::uint32, &builtin_types.t_u32);
    int_type("i32", true, 4, IR::Format::sint32, &builtin_types.t_i32);
    int_type("u64", false, 8, IR::Format::uint64, &builtin_types.t_u64);
    int_type("i64", true, 8, IR::Format::sint64, &builtin_types.t_i64);
  }

  {
    Structure* const s_void_ptr = STRUCTS::new_pointer_structure(structures, strings, 
                                                                 builtin_types.t_void);
    builtin_types.t_void_ptr = to_type(s_void_ptr);
  }

  {
    Structure* const s_void_call = STRUCTS::new_lambda_structure(structures, strings,
                                                                 nullptr, {}, builtin_types.t_void);
    builtin_types.t_void_call = to_type(s_void_call);
  }

  {
    EnumStructure* const s_bool = STRUCTS::new_enum_structure(structures, strings, builtin_types.t_u8);
    const Axle::InternString* bool_name = strings->intern("bool", 4);

    builtin_types.t_bool = to_type(s_bool);
    s_bool->enum_values.reserve_extra(2);
    {
      EnumValue* const e_true = STRUCTS::new_enum_value(structures, s_bool, bool_name,
                                                        strings->intern("true", 4));
      e_true->representation = 1;
      builtin_types.e_true = e_true;

      EnumValue* const e_false = STRUCTS::new_enum_value(structures, s_bool, bool_name,
                                                         strings->intern("false", 5));

      e_true->representation = 0;
      builtin_types.e_false = e_false;
    }
    s_bool->enum_values.shrink();
  }

  return builtin_types;
}
Structures::~Structures() {
  TELEMETRY_FUNCTION();
  
  {

    auto i = structures.begin();
    const auto end = structures.end();

    for (; i < end; i++) {
      const Structure* s = *i;

      switch (s->type) {
        case STRUCTURE_TYPE::TYPE:
        case STRUCTURE_TYPE::VOID:
          INVALID_CODE_PATH("Should not find any of these");
          break;

        case STRUCTURE_TYPE::FIXED_ARRAY:
          array_structures.free((const ArrayStructure*)s);
          break;
        case STRUCTURE_TYPE::POINTER:
          pointer_structures.free((const PointerStructure*)s);
          break;
        case STRUCTURE_TYPE::SLICE:
          slice_structures.free((const SliceStructure*)s);
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
        case STRUCTURE_TYPE::TUPLE:
          tuple_structures.free((const TupleStructure*)s);
          break;
        case STRUCTURE_TYPE::LAMBDA:
          lambda_structures.free((const SignatureStructure*)s);
          break;
        default: INVALID_CODE_PATH("All structures should be covered");
      }
    }
  }

  {
    auto i = enums.begin();
    const auto end = enums.end();

    for (; i < end; i++) {
      enum_values.free(*i);
    }
  }
}

//Can cast without any value modification or checks
static bool can_implicit_cast(const Type& from, const Type& to) {
  if (from == to) {
    return true;
  }

  switch (from.struct_type()) {
    case STRUCTURE_TYPE::FIXED_ARRAY: {
        if (to.structure->type != STRUCTURE_TYPE::FIXED_ARRAY) {
          return false;
        }

        const auto* arr_f = from.unchecked_base<ArrayStructure>();
        const auto* arr_t = to.unchecked_base<ArrayStructure>();

        return arr_f->length == arr_t->length && can_implicit_cast(arr_f->base, arr_t->base);
      }
    case STRUCTURE_TYPE::TUPLE: {
        if (to.struct_type() != STRUCTURE_TYPE::TUPLE) {
          return false;
        }

        const auto* from_tup = from.unchecked_base<TupleStructure>();
        const auto* to_comp = to.unchecked_base<TupleStructure>();

        //Size same?
        if (from_tup->elements.size != to_comp->elements.size) { return false; }

        auto f_i = from_tup->elements.begin();
        auto t_i = to_comp->elements.begin();
        const auto f_end = from_tup->elements.end();

        for (; f_i < f_end; f_i++, t_i++) {
          //Must be implicit type and offset
          if (f_i->offset != t_i->offset || !can_implicit_cast(f_i->type, t_i->type)) {
            return false;
          }
        }

        return true;
      }

    default: return false;
  }
}

//Can cast without any value modification or checks
[[maybe_unused]] static bool can_literal_cast(const Type& from, const Type& to) {
  if (can_implicit_cast(from, to)) {
    return true;
  }

  switch (from.struct_type()) {
    case STRUCTURE_TYPE::INTEGER: {
        const auto* int_f = from.unchecked_base<IntegerStructure>();
        auto to_type = to.struct_type();

        if (to_type == STRUCTURE_TYPE::INTEGER) {
          const auto* int_t = to.unchecked_base<IntegerStructure>();

          //Can only cast from unsigned to signed not the other way around without a cast
          return !int_f->is_signed || int_t->is_signed;
        }

        return false;
      }
    case STRUCTURE_TYPE::FIXED_ARRAY: {
        if (to.structure->type != STRUCTURE_TYPE::FIXED_ARRAY) {
          return false;
        }

        const auto* arr_f = from.unchecked_base<ArrayStructure>();
        const auto* arr_t = to.unchecked_base<ArrayStructure>();

        return arr_f->length == arr_t->length && can_literal_cast(arr_f->base, arr_t->base);
      }
    case STRUCTURE_TYPE::TUPLE: {
        switch (to.struct_type()) {
          case STRUCTURE_TYPE::TUPLE: {
              const auto* from_tup = from.unchecked_base<TupleStructure>();
              const auto* to_comp = to.unchecked_base<TupleStructure>();

              //Size same?
              if (from_tup->elements.size != to_comp->elements.size) { return false; }

              auto f_i = from_tup->elements.begin();
              auto t_i = to_comp->elements.begin();
              const auto f_end = from_tup->elements.end();

              for (; f_i < f_end; f_i++, t_i++) {
                if (!can_literal_cast(f_i->type, t_i->type)) {
                  return false;
                }
              }

              break;
            }
          case STRUCTURE_TYPE::COMPOSITE: {
              //Can cast a tuple literal to a composite type

              const auto* from_tup = from.unchecked_base<TupleStructure>();
              const auto* to_comp = to.unchecked_base<CompositeStructure>();

              //Size same?
              if (from_tup->elements.size != to_comp->elements.size) { return false; }

              auto f_i = from_tup->elements.begin();
              auto t_i = to_comp->elements.begin();
              const auto f_end = from_tup->elements.end();

              for (; f_i < f_end; f_i++, t_i++) {
                if (!can_literal_cast(f_i->type, t_i->type)) {
                  return false;
                }
              }

              break;
            }

          default: break;
        }

        return true;
      }

    default: break;
  }

  return false;
}


//No longer supported
//bool TYPE_TESTS::check_implicit_cast(META_FLAGS flags, const Type& from, const Type& to) {
//  if (TEST_MASK(flags, META_FLAG::LITERAL)
//      && can_literal_cast(from, to)) {
//    return true;
//  }
//  else if (can_implicit_cast(from, to)) {
//     return true;
//  }
//  
//  return false;
//}

bool TYPE_TESTS::is_negatable(const Structure* s) {
  return is_signed_int(s);
}

bool TYPE_TESTS::is_numeric(const Structure* s) {
  return is_int(s);
}

bool TYPE_TESTS::is_pointer(const Structure* s) {
  return s->type == STRUCTURE_TYPE::POINTER;
}

bool TYPE_TESTS::is_slice(const Structure* s) {
  return s->type == STRUCTURE_TYPE::SLICE;
}

bool TYPE_TESTS::can_index(const Structure* s) {
  return is_pointer(s) || is_array(s) || is_slice(s);
}

bool TYPE_TESTS::can_slice(const Structure* s) {
  return is_array(s) || is_slice(s);//TODO: maybe slice pointers
}

bool TYPE_TESTS::is_array(const Structure* s) {
  return s->type == STRUCTURE_TYPE::FIXED_ARRAY;
}

bool TYPE_TESTS::is_int(const Structure* s) {
  return s->type == STRUCTURE_TYPE::INTEGER;
}

bool TYPE_TESTS::is_signed_int(const Structure* s) {
  return (s->type == STRUCTURE_TYPE::INTEGER
          && ((const IntegerStructure*)s)->is_signed);
}

bool TYPE_TESTS::is_unsigned_int(const Structure* s) {
  return (s->type == STRUCTURE_TYPE::INTEGER
          && !((const IntegerStructure*)s)->is_signed);
}

bool TYPE_TESTS::is_64_bit_int(const Structure* s) {
  return s->type == STRUCTURE_TYPE::INTEGER && s->size == 8;
}

bool TYPE_TESTS::is_signed_64_bit_int(const Structure* s) {
  return s->type == STRUCTURE_TYPE::INTEGER
    && s->size == 8
    && static_cast<const IntegerStructure*>(s)->is_signed;
}

bool TYPE_TESTS::is_unsigned_64_bit_int(const Structure* s) {
  return s->type == STRUCTURE_TYPE::INTEGER
    && s->size == 8
    && !static_cast<const IntegerStructure*>(s)->is_signed;
}

bool TYPE_TESTS::is_8_bit_int(const Structure* s) {
  return s->type == STRUCTURE_TYPE::INTEGER && s->size == 1;
}

bool TYPE_TESTS::is_signed_8_bit_int(const Structure* s) {
  return s->type == STRUCTURE_TYPE::INTEGER
    && s->size == 1
    && static_cast<const IntegerStructure*>(s)->is_signed;
}

bool TYPE_TESTS::is_unsigned_8_bit_int(const Structure* s) {
  return s->type == STRUCTURE_TYPE::INTEGER
    && s->size == 1
    && !static_cast<const IntegerStructure*>(s)->is_signed;
}
