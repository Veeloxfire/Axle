#include "type.h"
#include "compiler.h"
#include "format.h"

FreelistBlockAllocator<LiteralStructure> Types::literal_structures ={};
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
    case STRUCTURE_TYPE::COMPOSITE: return static_cast<const CompositeStructure*>(this)->total_size;
    case STRUCTURE_TYPE::INTEGER: return static_cast<const IntegerStructure*>(this)->bytes;
    case STRUCTURE_TYPE::ENUM: return static_cast<const EnumStructure*>(this)->base->bytes;
    case STRUCTURE_TYPE::FIXED_ARRAY: {
        const ArrayStructure* const fa = static_cast<const ArrayStructure*>(this);
        return fa->base->size() * (uint32_t)fa->length;
      }
    case STRUCTURE_TYPE::LITERAL: {
        const LiteralStructure* const ls = static_cast<const LiteralStructure*>(this);

        switch (ls->literal_type) {
          case LITERAL_TYPE::INTEGER:
          case LITERAL_TYPE::SIGNED_INTEGER: return 8;
          case LITERAL_TYPE::EMPTY_ARR: return 0;
          case LITERAL_TYPE::POINTER: return 8;
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
    case STRUCTURE_TYPE::COMPOSITE: return static_cast<const CompositeStructure*>(this)->total_alignment;
    case STRUCTURE_TYPE::INTEGER: return static_cast<const IntegerStructure*>(this)->bytes;
    case STRUCTURE_TYPE::ENUM: return static_cast<const EnumStructure*>(this)->base->bytes;
    case STRUCTURE_TYPE::FIXED_ARRAY: return static_cast<const ArrayStructure*>(this)->base->alignment();
    case STRUCTURE_TYPE::LITERAL: {
        const LiteralStructure* ls = static_cast<const LiteralStructure*>(this);

        switch (ls->literal_type) {
          case LITERAL_TYPE::INTEGER:
          case LITERAL_TYPE::SIGNED_INTEGER: return 8;
          case LITERAL_TYPE::EMPTY_ARR: return 0;
          case LITERAL_TYPE::POINTER: return 8;
        }

        break;
      }
    case STRUCTURE_TYPE::VOID: return 0;
  }

  assert(false);
  return (uint32_t)-1;
}

bool can_comptime_cast(const Structure* from, const Structure* to) {
  if(can_implicit_cast(from, to)) return true;

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
    case STRUCTURE_TYPE::LITERAL: {
        const LiteralStructure* const from_lit = (const LiteralStructure*)from;

        switch (from_lit->literal_type) {
          case LITERAL_TYPE::INTEGER:
          case LITERAL_TYPE::SIGNED_INTEGER:
            //Signed/Size checks will be done later
            return to->type == STRUCTURE_TYPE::INTEGER;

          case LITERAL_TYPE::EMPTY_ARR://cast to any array
            return to->type == STRUCTURE_TYPE::FIXED_ARRAY;
        }

        return false;
      }

    default:
      return false;
  }
}

LiteralStructure* new_literal_type(Compiler* const comp, const InternString* name) {
  LiteralStructure* const type = comp->types->literal_structures.allocate();
  type->type = STRUCTURE_TYPE::LITERAL;
  type->name = name;

  comp->types->structures.insert(type);

  NamedElement el ={};
  el.set_union(NamedElementType::STRUCTURE);
  el.structure = (const Structure*)type;

  Namespace* ns = comp->all_namespaces.data + comp->builtin_namespace.index;
  ns->names.insert(name, std::move(el));
  return type;
}

IntegerStructure* new_int_type(Compiler* comp, const InternString* name) {
  IntegerStructure* const type = comp->types->int_structures.allocate();
  type->type = STRUCTURE_TYPE::INTEGER;
  type->name = name;

  comp->types->structures.insert(type);

  NamedElement el ={};
  el.set_union(NamedElementType::STRUCTURE);
  el.structure = (const Structure*)type;

  Namespace* ns = comp->all_namespaces.data + comp->builtin_namespace.index;
  ns->names.insert(name, std::move(el));
  return type;
}

CompositeStructure* new_composite_type(Compiler* const comp,
                                       const InternString* name) {
  CompositeStructure* const type = comp->types->composite_structures.allocate();
  type->type = STRUCTURE_TYPE::COMPOSITE;
  type->name = name;

  comp->types->structures.insert(type);

  NamedElement el ={};
  el.set_union(NamedElementType::STRUCTURE);
  el.structure = (const Structure*)type;

  Namespace* ns = comp->all_namespaces.data + comp->builtin_namespace.index;
  ns->names.insert(name, std::move(el));
  return type;
}

EnumStructure* new_enum_type(Compiler* const comp,
                             const InternString* name) {
  EnumStructure* const type = comp->types->enum_structures.allocate();
  type->type = STRUCTURE_TYPE::ENUM;
  type->name = name;

  comp->types->structures.insert(type);

  NamedElement el ={};
  el.set_union(NamedElementType::STRUCTURE);
  el.structure = (const Structure*)type;

  Namespace* ns = comp->all_namespaces.data + comp->builtin_namespace.index;
  ns->names.insert(name, std::move(el));
  return type;
}

Structure* new_base_type(Compiler* const comp,
                         const InternString* name) {
  Structure* const type = comp->types->base_structures.allocate();
  type->name = name;

  comp->types->structures.insert(type);

  NamedElement el ={};
  el.set_union(NamedElementType::STRUCTURE);
  el.structure = (const Structure*)type;

  Namespace* ns = comp->all_namespaces.data + comp->builtin_namespace.index;
  ns->names.insert(name, std::move(el));
  return type;
}

ArrayStructure* new_array_type(Compiler* const comp,
                               const Structure* base,
                               size_t length) {
  const InternString* name = comp->strings->intern(ArrayStructure::gen_name(base, length).ptr);

  ArrayStructure* const type = comp->types->array_structures.allocate();
  type->type = STRUCTURE_TYPE::FIXED_ARRAY;
  type->base = base;
  type->length = length;
  type->name = name;

  comp->types->structures.insert(type);

  NamedElement el ={};
  el.set_union(NamedElementType::STRUCTURE);
  el.structure = (const Structure*)type;

  Namespace* ns = comp->all_namespaces.data + comp->builtin_namespace.index;
  ns->names.insert(name, std::move(el));
  return type;
}

PointerStructure* new_pointer_type(Compiler* const comp,
                                   const Structure* base) {
  const InternString* name = comp->strings->intern(PointerStructure::gen_name(base).ptr);

  PointerStructure* const type = comp->types->pointer_structures.allocate();
  type->type = STRUCTURE_TYPE::POINTER;
  type->base = base;
  type->name = name;

  comp->types->structures.insert(type);

  NamedElement el ={};
  el.set_union(NamedElementType::STRUCTURE);
  el.structure = (const Structure*)type;

  Namespace* ns = comp->all_namespaces.data + comp->builtin_namespace.index;
  ns->names.insert(name, std::move(el));
  return type;
}

EnumValue* new_enum_value(Compiler* const comp,
                          EnumStructure* enum_s,
                          const InternString* name) {
  EnumValue* const val = comp->types->enum_values.allocate();
  val->type = enum_s;
  val->name = name;

  enum_s->enum_values.insert(val);
  comp->types->enums.insert(val);

  NamedElement el ={};
  el.set_union(NamedElementType::ENUM);
  el.structure = (const Structure*)val;

  Namespace* ns = comp->all_namespaces.data + comp->builtin_namespace.index;
  ns->names.insert(name, std::move(el));
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
        case STRUCTURE_TYPE::LITERAL:
          literal_structures.free((const LiteralStructure*)s);
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
    (s->type == STRUCTURE_TYPE::LITERAL && static_cast<const LiteralStructure*>(s)->literal_type == LITERAL_TYPE::EMPTY_ARR);
}

bool TYPE_TESTS::is_int(const Structure* s) {
  return s->type == STRUCTURE_TYPE::INTEGER
    || (s->type == STRUCTURE_TYPE::LITERAL &&
        (((const LiteralStructure*)s)->literal_type == LITERAL_TYPE::INTEGER
        || ((const LiteralStructure*)s)->literal_type == LITERAL_TYPE::SIGNED_INTEGER));
}

bool TYPE_TESTS::is_signed_int(const Structure* s) {
  return (s->type == STRUCTURE_TYPE::INTEGER && ((const IntegerStructure*)s)->is_signed)
    || (s->type == STRUCTURE_TYPE::LITERAL &&  ((const LiteralStructure*)s)->literal_type == LITERAL_TYPE::SIGNED_INTEGER);
}

bool TYPE_TESTS::is_64_bit_int(const Structure* s) {
  switch (s->type) {
    case STRUCTURE_TYPE::INTEGER: return static_cast<const IntegerStructure*>(s)->bytes == 8;
    case STRUCTURE_TYPE::LITERAL: {
        auto l =  static_cast<const LiteralStructure*>(s)->literal_type;

        return l == LITERAL_TYPE::INTEGER || l == LITERAL_TYPE::SIGNED_INTEGER;
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
    case STRUCTURE_TYPE::LITERAL: {
        auto l =  static_cast<const LiteralStructure*>(s)->literal_type;
        return l == LITERAL_TYPE::INTEGER || l == LITERAL_TYPE::SIGNED_INTEGER;
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
    case STRUCTURE_TYPE::LITERAL: {
        auto l =  static_cast<const LiteralStructure*>(s)->literal_type;
        return l == LITERAL_TYPE::INTEGER;
      }
    default: return false;
  }
}

bool TYPE_TESTS::is_8_bit_int(const Structure* s) {
  switch (s->type) {
    case STRUCTURE_TYPE::INTEGER: return static_cast<const IntegerStructure*>(s)->bytes == 1;
    case STRUCTURE_TYPE::LITERAL: {
        auto l =  static_cast<const LiteralStructure*>(s)->literal_type;

        return l == LITERAL_TYPE::INTEGER || l == LITERAL_TYPE::SIGNED_INTEGER;
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
    case STRUCTURE_TYPE::LITERAL: {
        auto l =  static_cast<const LiteralStructure*>(s)->literal_type;
        return l == LITERAL_TYPE::INTEGER || l == LITERAL_TYPE::SIGNED_INTEGER;
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
    case STRUCTURE_TYPE::LITERAL: {
        auto l =  static_cast<const LiteralStructure*>(s)->literal_type;
        return l == LITERAL_TYPE::INTEGER;
      }
    default: return false;
  }
}
