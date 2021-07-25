#include "type.h"
#include "compiler.h"
#include "format.h"

FreelistBlockAllocator<SimpleLiteralStructure> Types::simple_literal_structures ={};
FreelistBlockAllocator<TupleLiteralStructure> Types::tuple_literal_structures ={};
FreelistBlockAllocator<IntegerStructure> Types::int_structures ={};
FreelistBlockAllocator<CompositeStructure> Types::composite_structures ={};
FreelistBlockAllocator<EnumStructure> Types::enum_structures ={};
FreelistBlockAllocator<Structure> Types::base_structures ={};
FreelistBlockAllocator<EnumValue> Types::enum_values ={};
FreelistBlockAllocator<ArrayStructure> Types::array_structures ={};
FreelistBlockAllocator<PointerStructure> Types::pointer_structures ={};

uint32_t Structure::size() const {
  switch (type) {
    case STRUCTURE_TYPE::ASCII_CHAR: return 1;
    case STRUCTURE_TYPE::POINTER: return 8;
    case STRUCTURE_TYPE::COMPOSITE: return static_cast<const CompositeStructure*>(this)->cached_size;
    case STRUCTURE_TYPE::TUPLE_LITERAL: return static_cast<const TupleLiteralStructure*>(this)->cached_size;
    case STRUCTURE_TYPE::INTEGER: return static_cast<const IntegerStructure*>(this)->bytes;
    case STRUCTURE_TYPE::ENUM: return static_cast<const EnumStructure*>(this)->base->bytes;
    case STRUCTURE_TYPE::FIXED_ARRAY: {
        const ArrayStructure* const fa = static_cast<const ArrayStructure*>(this);
        return fa->base->size() * (uint32_t)fa->length;
      }
    case STRUCTURE_TYPE::SIMPLE_LITERAL: {
        const SimpleLiteralStructure* const ls = static_cast<const SimpleLiteralStructure*>(this);

        switch (ls->literal_type) {
          case SIMPLE_LITERAL_TYPE::INTEGER:
          case SIMPLE_LITERAL_TYPE::SIGNED_INTEGER: return 8;
          case SIMPLE_LITERAL_TYPE::EMPTY_ARR: return 0;
          case SIMPLE_LITERAL_TYPE::POINTER: return 8;
        }

        break;
      }
    case STRUCTURE_TYPE::VOID: return 0;
  }

  assert(false);
  return (uint32_t)-1;
}

uint32_t Structure::alignment() const {
  switch (type) {
    case STRUCTURE_TYPE::ASCII_CHAR: return 1;
    case STRUCTURE_TYPE::POINTER: return 8;
    case STRUCTURE_TYPE::COMPOSITE: return static_cast<const CompositeStructure*>(this)->cached_alignment;
    case STRUCTURE_TYPE::TUPLE_LITERAL: return static_cast<const TupleLiteralStructure*>(this)->cached_alignment;
    case STRUCTURE_TYPE::INTEGER: return static_cast<const IntegerStructure*>(this)->bytes;
    case STRUCTURE_TYPE::ENUM: return static_cast<const EnumStructure*>(this)->base->bytes;
    case STRUCTURE_TYPE::FIXED_ARRAY: return static_cast<const ArrayStructure*>(this)->base->alignment();
    case STRUCTURE_TYPE::SIMPLE_LITERAL: {
        const SimpleLiteralStructure* ls = static_cast<const SimpleLiteralStructure*>(this);

        switch (ls->literal_type) {
          case SIMPLE_LITERAL_TYPE::INTEGER:
          case SIMPLE_LITERAL_TYPE::SIGNED_INTEGER: return 8;
          case SIMPLE_LITERAL_TYPE::EMPTY_ARR: return 0;
          case SIMPLE_LITERAL_TYPE::POINTER: return 8;
        }

        break;
      }
    case STRUCTURE_TYPE::VOID: return 0;
  }

  assert(false);
  return (uint32_t)-1;
}

bool can_comptime_cast(const Structure* from, const Structure* to) {
  if (can_implicit_cast(from, to)) return true;

  switch (from->type) {
    case STRUCTURE_TYPE::FIXED_ARRAY: {
        if (to->type == STRUCTURE_TYPE::FIXED_ARRAY) {
          const ArrayStructure* arr_f = (const ArrayStructure*)from;
          const ArrayStructure* arr_t = (const ArrayStructure*)to;

          return arr_f->length == arr_t->length && can_comptime_cast(arr_f->base, arr_t->base);
        }
        else {
          return false;
        }
      }
    case STRUCTURE_TYPE::SIMPLE_LITERAL: {
        const SimpleLiteralStructure* const from_lit = (const SimpleLiteralStructure*)from;

        switch (from_lit->literal_type) {
          case SIMPLE_LITERAL_TYPE::INTEGER:
            //Signed/Size checks will be done later
            return to->type == STRUCTURE_TYPE::INTEGER;
          case SIMPLE_LITERAL_TYPE::SIGNED_INTEGER:
            //Signed/Size checks will be done later
            return to->type == STRUCTURE_TYPE::INTEGER
              && static_cast<const IntegerStructure*>(to)->is_signed;

          case SIMPLE_LITERAL_TYPE::EMPTY_ARR://cast to any array
            return to->type == STRUCTURE_TYPE::FIXED_ARRAY;
        }

        return false;
      }
    case STRUCTURE_TYPE::TUPLE_LITERAL: {
        const TupleLiteralStructure* f_ts = (const TupleLiteralStructure*)from;
        if (to->type == STRUCTURE_TYPE::COMPOSITE) {
          const CompositeStructure* t_cs = (const CompositeStructure*)to;

          //Size same?
          if (f_ts->elements.size != t_cs->elements.size) { return false; }

          auto f_i = f_ts->elements.begin();
          auto t_i = t_cs->elements.begin();
          const auto f_end = f_ts->elements.end();

          for (; f_i < f_end; f_i++, t_i++) {
            //Must be comptime type and offset
            if (!can_comptime_cast(f_i->type, t_i->type)
                || f_i->offset != t_i->offset) {
              return false;
            }
          }

          return true;
        }

        return false;
      }
    default:
      return false;
  }
}

const Structure* get_signed_type_of(const Types* types, const Structure* s) {
  if (s->type == STRUCTURE_TYPE::INTEGER) {
    const IntegerStructure* is = (const IntegerStructure*)s;

    switch (is->bytes) {
      case 1: return types->s_i8;
        //case 2: return types->s_i16;
      case 4: return types->s_i32;
      case 8: return types->s_i64;
      default: return nullptr;
    }
  }
  else if (s->type == STRUCTURE_TYPE::SIMPLE_LITERAL) {
    const SimpleLiteralStructure* sls = (const SimpleLiteralStructure*)s;

    if (sls->literal_type == SIMPLE_LITERAL_TYPE::INTEGER
        || sls->literal_type == SIMPLE_LITERAL_TYPE::SIGNED_INTEGER) {
      return types->s_sint_lit;
    }
  }

  return nullptr;
}

void TypeCreator::add_type_to_namespace(const Structure* s, const InternString* name, const Span& span) {
  NamedElement* el = comp->names->create_name(current_namespace, name);
  if (el == nullptr) {
    comp->report_error(ERROR_CODE::NAME_ERROR, span,
                       "Tried to make type '{}' but it conflicted with existing names",
                       name);
    return;
  }

  el->structure = s;
}

SimpleLiteralStructure* TypeCreator::new_simple_literal_type(const Span& span,
                                                             const InternString* name) {
  SimpleLiteralStructure* const type = comp->types->simple_literal_structures.allocate();
  type->type = STRUCTURE_TYPE::SIMPLE_LITERAL;
  type->name = name;

  comp->types->structures.insert(type);

  add_type_to_namespace((const Structure*)type, name, span);

  return type;
}

TupleLiteralStructure* TypeCreator::new_tuple_literal_type(const Span& span,
                                                           Array<const Structure*>&& types) {
  TupleLiteralStructure* const type = comp->types->tuple_literal_structures.allocate();

  type->type = STRUCTURE_TYPE::TUPLE_LITERAL;

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

      format_to_array(name, "({}", (*i)->name);

      tup_el->type = *i;
      tup_el->offset = current_size;
      uint32_t align = (*i)->alignment();
      uint32_t size = (*i)->size();

      current_size = ceil_to_n(current_size, align);
      current_size += size;

      current_align = larger(align, current_align);

      i++;

      for (; i < end; i++) {
        type->elements.insert_uninit(1);
        tup_el = type->elements.back();
        format_to_array(name, ", {}", (*i)->name);

        tup_el->type = *i;
        tup_el->offset = current_size;
        align = (*i)->alignment();
        size = (*i)->size();

        current_size = ceil_to_n(current_size, align);
        current_size += size;

        current_align = larger(align, current_align);
      }

      format_to_array(name, ")");
    }
    else {
      format_to_array(name, "()");
    }

    name.insert('\n');

    type->cached_size = current_size;
    type->cached_alignment = current_align;
    type->name = comp->strings->intern(name.data);
  }

  comp->types->structures.insert(type);

  add_type_to_namespace((const Structure*)type, type->name, span);

  return type;
}


IntegerStructure* TypeCreator::new_int_type(const Span& span,
                                            const InternString* name) {
  IntegerStructure* const type = comp->types->int_structures.allocate();
  type->type = STRUCTURE_TYPE::INTEGER;
  type->name = name;

  comp->types->structures.insert(type);

  add_type_to_namespace((const Structure*)type, name, span);

  return type;
}

CompositeStructure* TypeCreator::new_composite_type(const Span& span,
                                                    const InternString* name) {
  CompositeStructure* const type = comp->types->composite_structures.allocate();
  type->type = STRUCTURE_TYPE::COMPOSITE;
  type->name = name;

  comp->types->structures.insert(type);

  add_type_to_namespace((const Structure*)type, name, span);

  return type;
}

EnumStructure* TypeCreator::new_enum_type(const Span& span,
                                          const InternString* name) {
  EnumStructure* const type = comp->types->enum_structures.allocate();
  type->type = STRUCTURE_TYPE::ENUM;
  type->name = name;

  comp->types->structures.insert(type);

  add_type_to_namespace((const Structure*)type, name, span);

  return type;
}

Structure* TypeCreator::new_base_type(const Span& span,
                                      const InternString* name) {
  Structure* const type = comp->types->base_structures.allocate();
  type->name = name;

  comp->types->structures.insert(type);

  add_type_to_namespace((const Structure*)type, name, span);

  return type;
}

ArrayStructure* TypeCreator::new_array_type(const Span& span,
                                            const Structure* base,
                                            size_t length) {
  const InternString* name = comp->strings->intern(ArrayStructure::gen_name(base, length).ptr);

  ArrayStructure* const type = comp->types->array_structures.allocate();
  type->type = STRUCTURE_TYPE::FIXED_ARRAY;
  type->base = base;
  type->length = length;
  type->name = name;

  comp->types->structures.insert(type);

  add_type_to_namespace((const Structure*)type, name, span);

  return type;
}

PointerStructure* TypeCreator::new_pointer_type(const Span& span,
                                                const Structure* base) {
  const InternString* name = comp->strings->intern(PointerStructure::gen_name(base).ptr);

  PointerStructure* const type = comp->types->pointer_structures.allocate();
  type->type = STRUCTURE_TYPE::POINTER;
  type->base = base;
  type->name = name;

  comp->types->structures.insert(type);

  add_type_to_namespace((const Structure*)type, name, span);

  return type;
}

EnumValue* TypeCreator::new_enum_value(const Span& span,
                                       EnumStructure* enum_s,
                                       const InternString* name) {
  EnumValue* const val = comp->types->enum_values.allocate();
  val->type = enum_s;
  val->name = name;

  enum_s->enum_values.insert(val);
  comp->types->enums.insert(val);

  NamedElement* el = comp->names->create_name(current_namespace, name);
  if (el == nullptr) {
    comp->report_error(ERROR_CODE::NAME_ERROR, span,
                       "Tried to make type '{}' but it conflicted with existing names",
                       name);

  }
  else {
    el->enum_value = val;
  }


  return val;
}


OwnedPtr<char> PointerStructure::gen_name(const Structure* base) {
  return format("*{}", base->name);
}

OwnedPtr<char> ArrayStructure::gen_name(const Structure* base, size_t length) {
  return format("[{}, {}]", base->name, length);
}

Types::~Types() {
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
        case STRUCTURE_TYPE::SIMPLE_LITERAL:
          simple_literal_structures.free((const SimpleLiteralStructure*)s);
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
        case STRUCTURE_TYPE::VOID:
        case STRUCTURE_TYPE::ASCII_CHAR:
          base_structures.free(s);
          break;
        case STRUCTURE_TYPE::TUPLE_LITERAL:
          tuple_literal_structures.free((const TupleLiteralStructure*)s);
          break;
        default: assert(false);
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

RuntimeValue impl_single_cast(Compiler* const comp,
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


RuntimeValue CASTS::u8_to_r64(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* const val) {
  return impl_single_cast(comp, state, code, comp->types->s_u8, val, ByteCode::EMIT::CONV_RU8_TO_R64);
}

RuntimeValue CASTS::i8_to_r64(Compiler* const comp,
                              State* const state,
                              CodeBlock* const code,
                              const RuntimeValue* const val) {
  return impl_single_cast(comp, state, code, comp->types->s_i8, val, ByteCode::EMIT::CONV_RI8_TO_R64);
}

RuntimeValue CASTS::u32_to_r64(Compiler* const comp,
                               State* const state,
                               CodeBlock* const code,
                               const RuntimeValue* const val) {
  return impl_single_cast(comp, state, code, comp->types->s_u8, val, ByteCode::EMIT::CONV_RU32_TO_R64);
}

RuntimeValue CASTS::i32_to_r64(Compiler* const comp,
                               State* const state,
                               CodeBlock* const code,
                               const RuntimeValue* const val) {
  return impl_single_cast(comp, state, code, comp->types->s_i8, val, ByteCode::EMIT::CONV_RI32_TO_R64);
}

RuntimeValue CASTS::no_op(Compiler* const comp,
                          State* const state,
                          CodeBlock* const code,
                          const RuntimeValue* const val) {
  return *val;
}

bool TYPE_TESTS::is_pointer(const Structure* s) {
  return s->type == STRUCTURE_TYPE::POINTER;
}

bool TYPE_TESTS::can_index(const Structure* s) {
  return is_pointer(s) || is_array(s);
}

bool TYPE_TESTS::is_array(const Structure* s) {
  return s->type == STRUCTURE_TYPE::FIXED_ARRAY ||
    (s->type == STRUCTURE_TYPE::SIMPLE_LITERAL
     && static_cast<const SimpleLiteralStructure*>(s)->literal_type == SIMPLE_LITERAL_TYPE::EMPTY_ARR);
}

bool TYPE_TESTS::is_int(const Structure* s) {
  return s->type == STRUCTURE_TYPE::INTEGER
    || (s->type == STRUCTURE_TYPE::SIMPLE_LITERAL &&
        (((const SimpleLiteralStructure*)s)->literal_type == SIMPLE_LITERAL_TYPE::INTEGER
        || ((const SimpleLiteralStructure*)s)->literal_type == SIMPLE_LITERAL_TYPE::SIGNED_INTEGER));
}

bool TYPE_TESTS::is_signed_int(const Structure* s) {
  return (s->type == STRUCTURE_TYPE::INTEGER
          && ((const IntegerStructure*)s)->is_signed)
    || (s->type == STRUCTURE_TYPE::SIMPLE_LITERAL
        &&  ((const SimpleLiteralStructure*)s)->literal_type == SIMPLE_LITERAL_TYPE::SIGNED_INTEGER);
}

bool TYPE_TESTS::is_64_bit_int(const Structure* s) {
  switch (s->type) {
    case STRUCTURE_TYPE::INTEGER: return static_cast<const IntegerStructure*>(s)->bytes == 8;
    case STRUCTURE_TYPE::SIMPLE_LITERAL: {
        auto l =  static_cast<const SimpleLiteralStructure*>(s)->literal_type;

        return l == SIMPLE_LITERAL_TYPE::INTEGER || l == SIMPLE_LITERAL_TYPE::SIGNED_INTEGER;
      }
    default: return false;
  }
}

bool TYPE_TESTS::is_signed_64_bit_int(const Structure* s) {
  switch (s->type) {
    case STRUCTURE_TYPE::INTEGER: {
        auto* i =  static_cast<const IntegerStructure*>(s);
        return i->bytes == 8 && i->is_signed;
      }
    case STRUCTURE_TYPE::SIMPLE_LITERAL: {
        auto l =  static_cast<const SimpleLiteralStructure*>(s)->literal_type;
        return l == SIMPLE_LITERAL_TYPE::INTEGER || l == SIMPLE_LITERAL_TYPE::SIGNED_INTEGER;
      }
    default: return false;
  }
}

bool TYPE_TESTS::is_unsigned_64_bit_int(const Structure* s) {
  switch (s->type) {
    case STRUCTURE_TYPE::INTEGER: {
        auto* i =  static_cast<const IntegerStructure*>(s);
        return i->bytes == 8 && !i->is_signed;
      }
    case STRUCTURE_TYPE::SIMPLE_LITERAL: {
        auto l =  static_cast<const SimpleLiteralStructure*>(s)->literal_type;
        return l == SIMPLE_LITERAL_TYPE::INTEGER;
      }
    default: return false;
  }
}

bool TYPE_TESTS::is_8_bit_int(const Structure* s) {
  switch (s->type) {
    case STRUCTURE_TYPE::INTEGER: return static_cast<const IntegerStructure*>(s)->bytes == 1;
    case STRUCTURE_TYPE::SIMPLE_LITERAL: {
        auto l =  static_cast<const SimpleLiteralStructure*>(s)->literal_type;

        return l == SIMPLE_LITERAL_TYPE::INTEGER || l == SIMPLE_LITERAL_TYPE::SIGNED_INTEGER;
      }
    default: return false;
  }
}

bool TYPE_TESTS::is_signed_8_bit_int(const Structure* s) {
  switch (s->type) {
    case STRUCTURE_TYPE::INTEGER: {
        auto* i =  static_cast<const IntegerStructure*>(s);
        return i->bytes == 1 && i->is_signed;
      }
    case STRUCTURE_TYPE::SIMPLE_LITERAL: {
        auto l =  static_cast<const SimpleLiteralStructure*>(s)->literal_type;
        return l == SIMPLE_LITERAL_TYPE::INTEGER || l == SIMPLE_LITERAL_TYPE::SIGNED_INTEGER;
      }
    default: return false;
  }
}

bool TYPE_TESTS::is_unsigned_8_bit_int(const Structure* s) {
  switch (s->type) {
    case STRUCTURE_TYPE::INTEGER: {
        auto* i =  static_cast<const IntegerStructure*>(s);
        return i->bytes == 1 && !i->is_signed;
      }
    case STRUCTURE_TYPE::SIMPLE_LITERAL: {
        auto l =  static_cast<const SimpleLiteralStructure*>(s)->literal_type;
        return l == SIMPLE_LITERAL_TYPE::INTEGER;
      }
    default: return false;
  }
}

bool TYPE_TESTS::is_literal(const Structure* s) {
  return s->type == STRUCTURE_TYPE::SIMPLE_LITERAL
    || s->type == STRUCTURE_TYPE::TUPLE_LITERAL;
}