#include "compiler.h"
#include "format.h"

#include "type.h"

//TODO: Do we actually need this?
Type BuiltinTypes::get_signed_of(const Type& ty) {
  ASSERT(ty.struct_type() == STRUCTURE_TYPE::INTEGER);

  switch (ty.structure->size) {
    case 1: return t_i8;
      //case 2: return t_i16;
    case 4: return t_i32;
    case 8: return t_i64;
    default: INVALID_CODE_PATH("Should always exist"); return {};
  }
}

//void StructCreator::add_type_to_namespace(const Structure* s, const InternString* name, const Span& span) {
//  NamedElement* el = comp->services.names->create_name(current_namespace, name);
//  if (el == nullptr) {
//    comp->report_error(ERROR_CODE::NAME_ERROR, span,
//                       "Tried to make type '{}' but it conflicted with existing names",
//                       name);
//    return;
//  }
//
//  Global* glob = comp->globals.insert();
//  glob->decl.name = name;
//  glob->decl.type = meta_struct;//meta type for all types
//  glob->constant_value.size = 8;
//  glob->constant_value.ptr = comp->constants.alloc_no_construct(glob->constant_value.size);
//  memcpy_ts((const Structure**)glob->constant_value.ptr, 1, &s, 1);
//
//  el->globals.insert(glob);
//}


TupleStructure* STRUCTS::new_tuple_structure(Structures* structures, StringInterner* strings, Array<Type>&& types) {
  TupleStructure* const type = structures->tuple_structures.allocate();

  type->type = STRUCTURE_TYPE::TUPLE;

  //Load the name
  {
    type->elements.reserve_total(types.size);

    uint32_t current_size = 0;
    uint32_t current_align = 0;

    Array<char> name ={};

    auto i = types.begin();
    const auto end = types.end();

    if (i < end) {
      type->elements.insert_uninit(1);
      TupleElement* tup_el = type->elements.back();

      format_to_array(name, "({}", i->name);

      tup_el->type = *i;
      tup_el->offset = current_size;

      current_size = ceil_to_n(current_size, i->structure->alignment);
      current_size += i->structure->size;

      current_align = larger(i->structure->alignment, current_align);

      i++;

      for (; i < end; i++) {
        type->elements.insert_uninit(1);
        tup_el = type->elements.back();
        format_to_array(name, ", {}", i->name);

        tup_el->type = *i;
        tup_el->offset = current_size;

        current_size = ceil_to_n(current_size, i->structure->alignment);
        current_size += i->structure->size;

        current_align = larger(i->structure->alignment, current_align);
      }

      format_to_array(name, ")");
    }
    else {
      format_to_array(name, "()");
    }

    name.insert('\n');

    type->size = current_size;
    type->alignment = current_align;
    type->struct_name = strings->intern(name.data);
  }

  structures->structures.insert(type);

  return type;
}


SignatureStructure* STRUCTS::new_lambda_structure(Structures* structures, StringInterner* strings, usize ptr_size, const CallingConvention* conv,
                                                  Array<Type>&& params,
                                                  Type ret_type) {
  SignatureStructure* type = structures->lambda_structures.allocate();
  type->type = STRUCTURE_TYPE::LAMBDA;
  type->parameter_types = std::move(params);
  type->return_type = ret_type;
  type->calling_convention = conv;
  type->size = (u32)ptr_size;
  type->alignment = (u32)ptr_size;

  {
    Array<char> name ={};

    auto i = type->parameter_types.begin();
    auto end = type->parameter_types.end();

    name.insert('(');

    if (i < end) {
      format_to_array(name, "{}", i->name);
      i++;
    }

    for (; i < end; i++) {
      format_to_array(name, ", {}", i->name);
    }

    format_to_array(name, ") -> {}", type->return_type.name);
    name.insert('\0');

    type->struct_name = strings->intern(name.data);
  }

  structures->structures.insert(type);

  return type;
}


IntegerStructure* STRUCTS::new_int_structure(Structures* structures, const InternString* name) {
  IntegerStructure* const type = structures->int_structures.allocate();
  type->type = STRUCTURE_TYPE::INTEGER;
  type->struct_name = name;
  structures->structures.insert(type);

  return type;
}

CompositeStructure* STRUCTS::new_composite_structure(Structures* structures, StringInterner* strings) {
  CompositeStructure* const type = structures->composite_structures.allocate();
  type->type = STRUCTURE_TYPE::COMPOSITE;
  type->struct_name = strings->intern("anoymous-struct");

  structures->structures.insert(type);

  return type;
}

EnumStructure* STRUCTS::new_enum_structure(Structures* structures, StringInterner* strings, const Type& base) {
  EnumStructure* const type = structures->enum_structures.allocate();
  type->type = STRUCTURE_TYPE::ENUM;
  type->struct_name = strings->intern(EnumStructure::gen_name(base).ptr);
  type->base = base;

  type->size = base.structure->size;
  type->alignment = base.structure->alignment;

  structures->structures.insert(type);

  return type;
}

Structure* STRUCTS::new_base_structure(Structures* structures, const InternString* name) {
  Structure* const type = structures->base_structures.allocate();
  type->struct_name = name;

  structures->structures.insert(type);

  return type;
}

ArrayStructure* STRUCTS::new_array_structure(Structures* structures, StringInterner* strings, const Type& base,
                                             size_t length) {
  const InternString* name = strings->intern(ArrayStructure::gen_name(base, length).ptr);

  ArrayStructure* const type = structures->array_structures.allocate();
  type->type = STRUCTURE_TYPE::FIXED_ARRAY;
  type->base = base;
  type->length = length;
  type->struct_name = name;

  type->size = base.structure->size * (u32)length;
  type->alignment = base.structure->alignment;

  structures->structures.insert(type);

  return type;
}

PointerStructure* STRUCTS::new_pointer_structure(Structures* structures, StringInterner* strings, usize ptr_size, const Type& base) {
  const InternString* name = strings->intern(PointerStructure::gen_name(base).ptr);

  PointerStructure* const type = structures->pointer_structures.allocate();
  type->type = STRUCTURE_TYPE::POINTER;
  type->base = base;
  type->struct_name = name;

  type->size = (u32)ptr_size;
  type->alignment = (u32)ptr_size;

  structures->structures.insert(type);

  return type;
}

EnumValue* STRUCTS::new_enum_value(Structures* structures,
                                   EnumStructure* enum_s,
                                   const InternString* enum_name,
                                   const InternString* value_name) {
  EnumValue* const val = structures->enum_values.allocate();
  val->type = Type{ enum_name, enum_s };
  val->name = value_name;

  enum_s->enum_values.insert(val);
  structures->enums.insert(val);

  //NamedElement* el = comp->services.names->create_name(current_namespace, name);
  //if (el == nullptr) {
  //  comp->report_error(ERROR_CODE::NAME_ERROR, span,
  //                     "Tried to make type '{}' but it conflicted with existing names",
  //                     name);
  //}
  //else {
  //  Global* glob = comp->globals.insert();
  //  glob->decl.type = enum_s;
  //  glob->decl.name = name;
  //  glob->constant_value.size = 8;
  //  glob->constant_value.ptr = comp->constants.alloc_no_construct(glob->constant_value.size);
  //  memcpy_ts((EnumValue**)glob->constant_value.ptr, 1, &val, 1);

  //  el->globals.insert(glob);
  //}


  return val;
}


OwnedPtr<char> PointerStructure::gen_name(const Type& base) {
  return format("*{}", base.name);
}

OwnedPtr<char> ArrayStructure::gen_name(const Type& base, size_t length) {
  return format("[{}, {}]", base.name, length);
}


OwnedPtr<char> EnumStructure::gen_name(const Type& base) {
  return format("anoymous-enum({})", base.name);
}


Structures::~Structures() {
  {
    auto i = structures.begin();
    const auto end = structures.end();

    for (; i < end; i++) {
      const Structure* s = *i;

      switch (s->type) {
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
        case STRUCTURE_TYPE::TYPE:
        case STRUCTURE_TYPE::VOID:
        case STRUCTURE_TYPE::ASCII_CHAR:
          base_structures.free(s);
          break;
        case STRUCTURE_TYPE::TUPLE:
          tuple_structures.free((const TupleStructure*)s);
          break;
        case STRUCTURE_TYPE::LAMBDA:
          lambda_structures.free((const SignatureStructure*)s);
          break;
        default: INVALID_CODE_PATH_ABORT("All structures should be covered");
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

using CAST_BYTECODE = FUNCTION_PTR<void, Array<uint8_t>&, uint8_t>;

RuntimeValue impl_single_cast(CompilerGlobals* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const Structure* type,
                              const RuntimeValue* const val,
                              CAST_BYTECODE cast) {
  RuntimeValue reg ={};
  reg.type = RVT::REGISTER;
  reg.reg = state->new_value();

  copy_runtime_to_runtime(comp, state, code, type, val, &reg);

  cast(code->code, (uint8_t)reg.reg.val);

  state->get_val(reg.reg)->is_modified = true;
  state->use_value(reg.reg);

  return reg;
}

RuntimeValue CASTS::u8_to_r64(CompilerGlobals* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* const val) {
  return impl_single_cast(comp, state, code, comp->builtin_types->t_u8.structure, val, ByteCode::EMIT::CONV_RU8_TO_R64);
}

RuntimeValue CASTS::i8_to_r64(CompilerGlobals* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* const val) {
  return impl_single_cast(comp, state, code, comp->builtin_types->t_i8.structure, val, ByteCode::EMIT::CONV_RI8_TO_R64);
}

RuntimeValue CASTS::u32_to_r64(CompilerGlobals* const comp,
                               State* const state,
                               CodeBlock* const code,
                               const RuntimeValue* const val) {
  return impl_single_cast(comp, state, code, comp->builtin_types->t_u8.structure, val, ByteCode::EMIT::CONV_RU32_TO_R64);
}

RuntimeValue CASTS::i32_to_r64(CompilerGlobals* const comp,
                               State* const state,
                               CodeBlock* const code,
                               const RuntimeValue* const val) {
  return impl_single_cast(comp, state, code, comp->builtin_types->t_i8.structure, val, ByteCode::EMIT::CONV_RI32_TO_R64);
}

RuntimeValue CASTS::no_op(CompilerGlobals* const comp,
                          State* const state,
                          CodeBlock* const code,
                          const RuntimeValue* const val) {
  return *val;
}

//Can cast without any value modification or checks
static bool can_implicit_cast(const Type& from, const Type& to) {
  if (from == to) {
    return true;
  }

  switch (from.struct_type()) {
    case STRUCTURE_TYPE::ASCII_CHAR: {
        const auto* to_int = to.extract_base<IntegerStructure>();

        return to_int != nullptr && to_int->size == 1;
      }
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
  }

  return false;
}

//Can cast without any value modification or checks
static bool can_literal_cast(const Type& from, const Type& to) {
  if (can_implicit_cast(from, to)) {
    return true;
  }

  switch (from.struct_type()) {
    case STRUCTURE_TYPE::INTEGER: {
        if (to.struct_type() == STRUCTURE_TYPE::INTEGER) {
          const auto* int_f = from.unchecked_base<IntegerStructure>();
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
        }       

        return true;
      }
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