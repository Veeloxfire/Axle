#include <AxleUtil/format.h>

#include "compiler.h"
#include "type.h"

#include <Tracer/trace.h>

TupleStructure* STRUCTS::new_tuple_structure(Structures* structures, StringInterner* strings, Array<Type>&& types) {
  TRACING_FUNCTION();

  TupleStructure* const type = structures->tuple_structures.allocate();

  type->type = STRUCTURE_TYPE::TUPLE;
  type->ir_format = IR::Format::opaque;

  //Load the name
  {
    type->elements = new_arr<TupleElement>(types.size);

    uint32_t current_size = 0;
    uint32_t current_align = 0;

    Format::ArrayFormatter name = {};

    auto i = types.begin();
    const auto end = types.end();
    auto tup_el = type->elements.mut_begin();
    auto tup_el_end = type->elements.mut_end();

    if (i < end) {
      ASSERT(tup_el < tup_el_end);

      Format::format_to_formatter(name, "({}", i->name);

      tup_el->type = *i;
      tup_el->offset = current_size;

      current_size = ceil_to_n(current_size, i->structure->alignment);
      current_size += i->structure->size;

      current_align = larger(i->structure->alignment, current_align);

      i++;
      tup_el++;

      for (; i < end; (i++, tup_el++)) {
        ASSERT(tup_el < tup_el_end);
        Format::format_to_formatter(name, ", {}", i->name);

        tup_el->type = *i;
        tup_el->offset = current_size;

        current_size = ceil_to_n(current_size, i->structure->alignment);
        current_size += i->structure->size;

        current_align = larger(i->structure->alignment, current_align);
      }

      Format::format_to_formatter(name, ")");
    }
    else {
      Format::format_to_formatter(name, "()");
    }

    name.load_char('\n');

    type->size = current_size;
    type->alignment = current_align;

    type->struct_name = strings->intern(name.view());
  }

  structures->structures.insert(type);

  return type;
}


SignatureStructure* STRUCTS::new_lambda_structure(Structures* structures, StringInterner* strings, usize ptr_size, const CallingConvention* conv,
                                                  Array<Type>&& params,
                                                  Type ret_type) {
  TRACING_FUNCTION();

  SignatureStructure* type = structures->lambda_structures.allocate();
  type->type = STRUCTURE_TYPE::LAMBDA;
  type->ir_format = IR::Format::opaque;
  type->parameter_types = bake_arr(std::move(params));
  type->return_type = ret_type;
  type->calling_convention = conv;
  type->size = (u32)ptr_size;
  type->alignment = (u32)ptr_size;

  {
    Format::ArrayFormatter name = {};

    auto i = type->parameter_types.begin();
    auto end = type->parameter_types.end();

    name.load_char('(');

    if (i < end) {
      Format::format_to_formatter(name, "{}", i->name);
      i++;
    }

    for (; i < end; i++) {
      Format::format_to_formatter(name, ", {}", i->name);
    }

    Format::format_to_formatter(name, ") -> {}", type->return_type.name);

    type->struct_name = strings->intern(name.view());
  }

  structures->structures.insert(type);

  return type;
}


IntegerStructure* STRUCTS::new_int_structure(Structures* structures, const InternString* name) {
  TRACING_FUNCTION();

  IntegerStructure* const type = structures->int_structures.allocate();
  type->type = STRUCTURE_TYPE::INTEGER;
  type->struct_name = name;
  structures->structures.insert(type);

  return type;
}

CompositeStructure* STRUCTS::new_composite_structure(Structures* structures, StringInterner* strings) {
  TRACING_FUNCTION();

  CompositeStructure* const type = structures->composite_structures.allocate();
  type->type = STRUCTURE_TYPE::COMPOSITE;
  type->ir_format = IR::Format::opaque;
  type->struct_name = strings->intern(lit_view_arr("anoymous-struct"));

  structures->structures.insert(type);

  return type;
}

EnumStructure* STRUCTS::new_enum_structure(Structures* structures, StringInterner* strings, const Type& base) {
  TRACING_FUNCTION();


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

ArrayStructure* STRUCTS::new_array_structure(Structures* structures, StringInterner* strings, const Type& base,
                                             size_t length) {
  TRACING_FUNCTION();


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

PointerStructure* STRUCTS::new_pointer_structure(Structures* structures, StringInterner* strings, usize ptr_size, const Type& base) {
  TRACING_FUNCTION();
  
  PointerStructure* const type = structures->pointer_structures.allocate();
  type->type = STRUCTURE_TYPE::POINTER;
  type->ir_format = IR::Format::uint64;
  type->base = base;
  type->struct_name = strings->format_intern("*{}", base.name);
  type->size = (u32)ptr_size;
  type->alignment = (u32)ptr_size;

  structures->structures.insert(type);

  return type;
}

EnumValue* STRUCTS::new_enum_value(Structures* structures,
                                   EnumStructure* enum_s,
                                   const InternString* enum_name,
                                   const InternString* value_name) {
  TRACING_FUNCTION();

  EnumValue* const val = structures->enum_values.allocate();
  val->type = Type{ enum_name, enum_s };
  val->name = value_name;

  enum_s->enum_values.insert(val);
  structures->enums.insert(val);

  return val;
}

Structures::~Structures() {
  {
    TRACING_FUNCTION();

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

bool TYPE_TESTS::can_index(const Structure* s) {
  return is_pointer(s) || is_array(s);
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