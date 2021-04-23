#include "type.h"
#include "compiler.h"

FreelistBlockAllocator<LiteralStructure> StructAllocator::literal_structures ={};
FreelistBlockAllocator<IntegerStructure> StructAllocator::int_structures ={};
FreelistBlockAllocator<CompositeStructure> StructAllocator::composite_structures ={};
FreelistBlockAllocator<EnumStructure> StructAllocator::enum_structures ={};
FreelistBlockAllocator<Structure> StructAllocator::base_structures ={};
FreelistBlockAllocator<EnumValue> StructAllocator::enum_values ={};

uint32_t Structure::size() const {
  switch (type) {
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
        }

        break;
      }
    case STRUCTURE_TYPE::VOID: return 0;
  }

  return (uint32_t)-1;
}

uint32_t Structure::alignment() const {
  switch (type) {
    case STRUCTURE_TYPE::COMPOSITE: return static_cast<const CompositeStructure*>(this)->total_alignment;
    case STRUCTURE_TYPE::INTEGER: return static_cast<const IntegerStructure*>(this)->bytes;
    case STRUCTURE_TYPE::ENUM: return static_cast<const EnumStructure*>(this)->base->bytes;
    case STRUCTURE_TYPE::FIXED_ARRAY: return static_cast<const ArrayStructure*>(this)->base->alignment();
    case STRUCTURE_TYPE::LITERAL: {
        const LiteralStructure* ls = static_cast<const LiteralStructure*>(this);

        switch (ls->literal_type) {
          case LITERAL_TYPE::INTEGER:
          case LITERAL_TYPE::SIGNED_INTEGER: return 8;
        }

        break;
      }
    case STRUCTURE_TYPE::VOID: return 0;
  }

  return (uint32_t)-1;
}

bool can_literal_cast(const Structure* from, const Structure* to) {
  if (from->type != STRUCTURE_TYPE::LITERAL) {
    return false;
  }

  const LiteralStructure* const from_lit = (const LiteralStructure*)from;

  switch (from_lit->literal_type) {
    case LITERAL_TYPE::INTEGER:
      return to->type == STRUCTURE_TYPE::INTEGER
        || (to->type == STRUCTURE_TYPE::LITERAL
            && static_cast<const LiteralStructure*>(to)->literal_type == LITERAL_TYPE::SIGNED_INTEGER);

    case LITERAL_TYPE::SIGNED_INTEGER:
      return to->type == STRUCTURE_TYPE::INTEGER
        && static_cast<const IntegerStructure*>(to)->is_signed;
  }

  return false;
}

void init_types(Types* types, StringInterner* strings) {

  Structure* const s_void = StructAllocator::base_structures.allocate();
  types->s_void = s_void;
  types->structures.insert(s_void);

  s_void->type = STRUCTURE_TYPE::VOID;
  s_void->name = strings->intern("void");

  LiteralStructure* const int_lit = StructAllocator::new_literal();
  types->s_int_lit = int_lit;
  types->structures.insert(int_lit);

  int_lit->name         = strings->intern("integer literal");
  int_lit->literal_type = LITERAL_TYPE::INTEGER;

  LiteralStructure* const sint_lit = StructAllocator::new_literal();
  types->s_sint_lit = sint_lit;
  types->structures.insert(sint_lit);

  sint_lit->name         = strings->intern("signed integer literal");
  sint_lit->literal_type = LITERAL_TYPE::SIGNED_INTEGER;


  IntegerStructure* const u8 = StructAllocator::new_int();
  types->s_u8 = u8;
  types->structures.insert(u8);

  u8->name      = strings->intern("u8");
  u8->is_signed = false;
  u8->bytes     = 1;

  IntegerStructure* const i8 = StructAllocator::new_int();
  types->s_i8 = i8;
  types->structures.insert(i8);

  u8->name      = strings->intern("i8");
  u8->is_signed = true;
  u8->bytes     = 1;

  IntegerStructure* const u64 = StructAllocator::new_int();
  types->s_u64 = u64;
  types->structures.insert(u64);

  u64->name      = strings->intern("u64");
  u64->is_signed = false;
  u64->bytes      = 8;

  IntegerStructure* const i64 = StructAllocator::new_int();
  types->s_i64 = i64;
  types->structures.insert(i64);

  i64->name      = strings->intern("i64");
  i64->is_signed = true;
  i64->bytes     = 8;

  EnumStructure* const s_bool = StructAllocator::new_enum();
  types->s_bool = s_bool;
  types->structures.insert(s_bool);

  s_bool->base = u8;
  s_bool->name = strings->intern("bool");

  s_bool->enum_values.insert_uninit(2);
  s_bool->enum_values.shrink();

  EnumValue* const e_true = StructAllocator::enum_values.allocate();
  types->e_true = e_true;
  types->enums.insert(e_true);

  s_bool->enum_values.data[0] = e_true;
  e_true->type = s_bool;
  e_true->name = strings->intern("true");
  e_true->representation = 1;

  EnumValue* const e_false = StructAllocator::enum_values.allocate();
  types->e_false = e_false;
  types->enums.insert(e_false);

  s_bool->enum_values.data[1] = e_false;
  e_false->type = s_bool;
  e_false->name = strings->intern("false");
  e_false->representation = 0;

  /////// CASTS ///////

  u8->casts.insert(Cast{ &TYPE_TESTS::is_8_bit_int, &CASTS::no_cast });
  u8->casts.insert(Cast{ &TYPE_TESTS::is_64_bit_int, &CASTS::u8_to_r64 });

  i8->casts.insert(Cast{ &TYPE_TESTS::is_8_bit_int, &CASTS::no_cast });
  i8->casts.insert(Cast{ &TYPE_TESTS::is_64_bit_int, &CASTS::i8_to_r64 });

  u64->casts.insert(Cast{ &TYPE_TESTS::is_64_bit_int, &CASTS::no_cast });
  u64->casts.insert(Cast{ &TYPE_TESTS::is_8_bit_int, &CASTS::no_cast });

  i64->casts.insert(Cast{ &TYPE_TESTS::is_64_bit_int, &CASTS::no_cast });
  i64->casts.insert(Cast{ &TYPE_TESTS::is_8_bit_int, &CASTS::no_cast });

  sint_lit->casts.insert(Cast{ &TYPE_TESTS::is_unsigned_64_bit_int, &CASTS::no_cast });
}

Types::~Types() {
  StructAllocator::free_structure(s_bool);
  StructAllocator::free_structure(s_u8);
  StructAllocator::free_structure(s_i8);
  StructAllocator::free_structure(s_u64);
  StructAllocator::free_structure(s_i64);
  StructAllocator::free_structure(s_int_lit);
  StructAllocator::free_structure(s_sint_lit);
  StructAllocator::free_structure(s_void);

  StructAllocator::enum_values.free(e_false);
  StructAllocator::enum_values.free(e_true);
}

const Structure* Types::structure_by_name(const InternString name) const {
  auto i = structures.begin();
  const auto end = structures.end();

  for (; i < end; i++) {
    const Structure* s = *i;
    if (s->name == name) {
      return s;
    }
  }

  return nullptr;
}

const EnumValue* Types::enum_by_name(const InternString name) const {
  auto i = enums.begin();
  const auto end = enums.end();

  for (; i < end; i++) {
    const EnumValue* e = *i;
    if (e->name == name) {
      return e;
    }
  }

  return nullptr;
}


void CASTS::u8_to_r64(State* state, Function* func, ValueIndex val) {
  ByteCode::EMIT::CONV_RU8_TO_R64(func->code, (uint8_t)val.val);

  state->use_value(val);
  state->value_tree.values.data[val.val].is_modified = true;
}

void CASTS::i8_to_r64(State* state, Function* func, ValueIndex val) {
  ByteCode::EMIT::CONV_RI8_TO_R64(func->code, (uint8_t)val.val);

  state->use_value(val);
  state->value_tree.values.data[val.val].is_modified = true;
}

void CASTS::no_cast(State*, Function*, ValueIndex) {
  return;
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
