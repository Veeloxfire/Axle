#include "ir.h"

namespace IR {
  RuntimeReference Builder::new_temporary(const Type& t) {
    usize i = temporaries.size;
    temporaries.insert_uninit(1);
    Temporary* temp = temporaries.back();
    temp->type = t;
    temp->indirection = IR::Indirection::None;

    ASSERT(i <= ValueIndex::INDEX_MASK);
    ValueIndex base = { static_cast<u32>(i) | ValueIndex::TEMP_MASK };

    RuntimeReference ref = {};
    ref.is_constant = false;
    ref.base = base;
    ref.offset = 0;
    ref.type = t;

    return ref;
  }

  IR::ValueIndex Builder::new_variable(const Type& t) {
    usize i = variables.size;
    variables.insert_uninit(1);
    Variable* var = variables.back();
    var->type = t;

    ASSERT(i <= ValueIndex::INDEX_MASK);
    ValueIndex base = { static_cast<u32>(i) };

    return base;
  }

  u32 Builder::new_global_reference(const IR::GlobalReference& ref) {
    u32 i = (u32)globals_used.size;
    globals_used.insert(ref);
    return i;
  }

  LocalLabel Builder::new_control_block() {
    usize i = control_blocks.size;
    control_blocks.insert(ControlBlock{});

    ASSERT(i <= 0xffffffff);

    return LocalLabel{ (u32)i };
  }

  void Builder::start_control_block(LocalLabel label) {
    ASSERT(control_blocks.size > label.label);

    ControlBlock* block = control_blocks.data + label.label;
    block->start = ir_bytecode.size;
    block->size = 0;

    current_block = label;
  }

  void Builder::end_control_block() {
    ASSERT(control_blocks.size > current_block.label);

    ControlBlock* block = control_blocks.data + current_block.label;
    block->size = ir_bytecode.size - block->start;
  }

  void Builder::start_expression() {
    ExpressionFrame frame = {};
    frame.temporary_start = (u32)temporaries.size;
    frame.temporary_count = 0;
    frame.bytecode_start = (u32)ir_bytecode.size;
    frame.bytecode_count = 0;

    expression_frames.insert(frame);
  }

  void Builder::end_expression() {
    ASSERT(expression_frames.size > 0);

    ExpressionFrame* f = expression_frames.back();
    f->temporary_count = (u32)temporaries.size - f->temporary_start;
    f->bytecode_count = (u32)ir_bytecode.size - f->bytecode_start;
  }
}

struct Serializer {
  u8* data_base;
  usize remaining_size;
  usize index = 0;

  constexpr Serializer(u8* base, usize remaining) : data_base(base), remaining_size(remaining) {}

  void write(const void* in_data, usize in_size) {
    ASSERT(remaining_size >= in_size);
    memcpy_s(data_base + index, remaining_size, in_data, in_size);
    index += in_size;
    remaining_size -= in_size;
  }

  void write(u8 b) {
    ASSERT(remaining_size > 0);
    data_base[index] = b;
    index += 1;
    remaining_size -= 1;
  }

  template<typename T>
  void write(const T& t) {
    write(static_cast<const void*>(&t), sizeof(t));
  }
};


struct Deserializer {
  const u8* data_base;
  usize remaining_size;
  usize index = 0;

  constexpr Deserializer(const u8* base, usize remaining) : data_base(base), remaining_size(remaining) {}

  void read(void* out_data, usize out_size) {
    ASSERT(remaining_size >= out_size);
    memcpy_s(out_data, out_size, data_base + index, out_size);
    index += out_size;
    remaining_size -= out_size;
  }

  const u8* bytes_slice(usize out_size) {
    ASSERT(remaining_size >= out_size);

    const u8* res = data_base + index;

    index += out_size;
    remaining_size -= out_size;

    return res;
  }


  void read(u8& b) {
    ASSERT(remaining_size > 0);
    b = data_base[index];
    index += 1;
    remaining_size -= 1;
  }

  template<typename T>
  void read(T& t) {
    read(static_cast<void*>(&t), sizeof(t));
  }
};

usize IR::serialize(u8* data, usize remaining_size, const CODE_IM32& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.im32);

  return ser.index;
}


usize IR::serialize(u8* data, usize remaining_size, const CODE_V& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.val);
  ser.write(i.offset);
  ser.write(i.format);

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_V_IM32& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.val);
  ser.write(i.offset);
  ser.write(i.format);
  ser.write(i.im32);

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_V_C& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.to);
  ser.write(i.t_offset);
  ser.write(i.t_format);
  ser.write(i.d_size);
  ser.write(i.data, i.d_size);

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_V_V& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.to);
  ser.write(i.t_offset);
  ser.write(i.t_format);
  ser.write(i.from);
  ser.write(i.f_offset);
  ser.write(i.f_format);

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_V_V_V& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.to);
  ser.write(i.t_offset);
  ser.write(i.t_format);
  ser.write(i.left);
  ser.write(i.l_offset);
  ser.write(i.l_format);
  ser.write(i.right);
  ser.write(i.r_offset);
  ser.write(i.r_format);

  return ser.index;
}


usize IR::serialize(u8* data, usize remaining_size, const CODE_GL_NV& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.label);
  ser.write(i.n_values);

  for (u32 c = 0; c < i.n_values; ++c) {
    SingleVal* s = ((SingleVal*)i.values) + c;
    ser.write(s->v);
    ser.write(s->v_offset);
    ser.write(s->v_format);
  }

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_V_LL& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.val);
  ser.write(i.offset);
  ser.write(i.format);
  ser.write(i.label_if);
  ser.write(i.label_else);

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_L& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.local_label);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_IM32& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.im32);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_V& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.val);
  ser.read(i.offset);
  ser.read(i.format);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_V_IM32& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.val);
  ser.read(i.offset);
  ser.read(i.format);
  ser.read(i.im32);

  return ser.index;
}


usize IR::deserialize(const u8* data, usize remaining_size, CODE_V_C& i) {
  ASSERT(remaining_size >= i.static_serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.to);
  ser.read(i.t_offset);
  ser.read(i.t_format);
  ser.read(i.d_size);

  i.data = ser.bytes_slice(i.d_size);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_V_V& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.to);
  ser.read(i.t_offset);
  ser.read(i.t_format);
  ser.read(i.from);
  ser.read(i.f_offset);
  ser.read(i.f_format);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_V_V_V& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.to);
  ser.read(i.t_offset);
  ser.read(i.t_format);
  ser.read(i.left);
  ser.read(i.l_offset);
  ser.read(i.l_format);
  ser.read(i.right);
  ser.read(i.r_offset);
  ser.read(i.r_format);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_GL_NV& i) {
  ASSERT(remaining_size >= i.static_serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.label);
  ser.read(i.n_values);

  i.values = ser.bytes_slice(SingleVal::serialize_size() * (usize)i.n_values);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, SingleVal& v) {
  ASSERT(remaining_size >= v.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(v.v);
  ser.read(v.v_offset);
  ser.read(v.v_format);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_V_LL& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.val);
  ser.read(i.offset);
  ser.read(i.format);
  ser.read(i.label_if);
  ser.read(i.label_else);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_L& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.local_label);

  return ser.index;
}

IR::RuntimeReference IR::HELPERS::no_value() {
  IR::RuntimeReference r = {};
  r.is_constant = false;
  r.type = Type{};
  r.constant = nullptr;
  r.offset = 0;

  return r;
}

IR::RuntimeReference IR::HELPERS::as_reference(IR::ValueIndex val, const Type& type) {
  IR::RuntimeReference r = {};
  r.is_constant = false;
  r.base = val;
  r.offset = 0;
  r.type = type;

  return r;
}

IR::RuntimeReference IR::HELPERS::as_constant(void* constant, const Type& type) {
  IR::RuntimeReference r = {};
  r.is_constant = true;
  r.constant = static_cast<u8*>(constant);
  r.offset = 0;
  r.type = type;

  return r;
}

IR::RuntimeReference IR::HELPERS::sub_object(const IR::RuntimeReference& val, usize offset, const Type& type) {
  IR::RuntimeReference r = {};
  r.is_constant = val.is_constant;
  if (val.is_constant) {
    r.constant = val.constant;
  }
  else {
    r.base = val.base;
  }

  ASSERT(offset + type.size() <= val.type.size());

  r.offset = val.offset + (u32)offset;
  r.type = type;

  return r;
}

void IR::HELPERS::copycast_value(IR::Builder* builder,
                                 const IR::RuntimeReference& to, IR::RuntimeReference from) {
  ASSERT(!to.is_constant);

  ASSERT(to.type.size() > 0);
  ASSERT(from.type.size() > 0);
  ASSERT(to.type.size() <= 8);
  ASSERT(from.type.size() <= 8);

  if (from.is_constant) {
    ASSERT(to.type == from.type);

    IR::Types::Set set = {};
    set.to = to.base;
    set.t_offset = to.offset;
    set.t_format = to.type.struct_format();
    set.data = from.constant + from.offset;
    set.d_size = from.type.size();

    IR::Emit::Set(builder->ir_bytecode, set);
  }
  else {
    ASSERT(!from.is_constant);

    if (from.base.is_variable() && to.base.is_variable() && builder->require_variable_intermediates) {
      IR::RuntimeReference intermediate = builder->new_temporary(to.type);

      IR::Types::CopyCast first = {};
      first.from = from.base;
      first.f_offset = from.offset;
      first.f_format = from.type.struct_format();
      first.to = intermediate.base;
      first.t_offset = intermediate.offset;
      first.t_format = intermediate.type.struct_format();

      IR::Emit::CopyCast(builder->ir_bytecode, first);

      IR::Types::CopyCast second = {};
      second.from = intermediate.base;
      second.f_offset = intermediate.offset;
      second.f_format = intermediate.type.struct_format();
      second.to = to.base;
      second.t_offset = to.offset;
      second.t_format = to.type.struct_format();

      IR::Emit::CopyCast(builder->ir_bytecode, second);
    }
    else {
      IR::Types::CopyCast c = {};
      c.from = from.base;
      c.f_offset = from.offset;
      c.f_format = from.type.struct_format();
      c.to = to.base;
      c.t_offset = to.offset;
      c.t_format = to.type.struct_format();


      IR::Emit::CopyCast(builder->ir_bytecode, c);
    }
  }
}


IR::RuntimeReference IR::HELPERS::copy_constant(IR::Builder* ir, const void* constant, const Type& type) {
  IR::RuntimeReference r = ir->new_temporary(type);

  IR::Types::Set set = {};
  set.to = r.base;
  set.t_offset = r.offset;
  set.t_format = r.type.struct_format();
  set.data = static_cast<const u8*>(constant);
  set.d_size = type.size();

  IR::Emit::Set(ir->ir_bytecode, set);

  return r;
}

IR::RuntimeReference IR::HELPERS::take_address(IR::Builder* const builder,
                                               const IR::RuntimeReference& val, Type ptr_type) {
  ASSERT(!val.is_constant);
  ASSERT(ptr_type.struct_type() == STRUCTURE_TYPE::POINTER);
  ASSERT(val.type == ptr_type.unchecked_base<PointerStructure>()->base);

  IR::RuntimeReference address_val = builder->new_temporary(ptr_type);

  Temporary& address_temp = builder->temporaries.data[address_val.base.index()];

  address_temp.indirection = IR::Indirection::Reference;
  address_temp.refers_to_offset = val.offset;
  address_temp.refers_to = val.base;

  return address_val;
}

IR::RuntimeReference IR::HELPERS::arr_to_ptr(IR::Builder* const builder,
                                               const IR::RuntimeReference& val, Type ptr_type) {
  ASSERT(!val.is_constant);
  ASSERT(ptr_type.struct_type() == STRUCTURE_TYPE::POINTER);
  ASSERT(val.type.struct_type() == STRUCTURE_TYPE::FIXED_ARRAY);
  ASSERT(val.type.unchecked_base<ArrayStructure>()->base == ptr_type.unchecked_base<PointerStructure>()->base);

  IR::RuntimeReference address_val = builder->new_temporary(ptr_type);

  Temporary& address_temp = builder->temporaries.data[address_val.base.index()];

  address_temp.indirection = IR::Indirection::Reference;
  address_temp.refers_to_offset = val.offset;
  address_temp.refers_to = val.base;

  return address_val;
}


IR::RuntimeReference IR::HELPERS::dereference(IR::Builder* const builder,
                                              const IR::RuntimeReference& val, Type deref_type) {
  ASSERT(!val.is_constant);
  ASSERT(val.type.struct_type() == STRUCTURE_TYPE::POINTER);
  ASSERT(val.type.unchecked_base<PointerStructure>()->base == deref_type);


  IR::RuntimeReference address_val = builder->new_temporary(deref_type);

  Temporary& address_temp = builder->temporaries.data[address_val.base.index()];

  address_temp.indirection = IR::Indirection::Dereference;
  address_temp.refers_to_offset = val.offset;
  address_temp.refers_to = val.base;

  return address_val;

}

const char* format_name(IR::Format f) {
  switch (f) {
    case IR::Format::opaque: return "opaque";
    case IR::Format::uint8: return "uint8";
    case IR::Format::sint8: return "sint8";
    case IR::Format::uint16: return "uint16";
    case IR::Format::sint16: return "sint16";
    case IR::Format::uint32: return "uint32";
    case IR::Format::sint32: return "sint32";
    case IR::Format::uint64: return "uint64";
    case IR::Format::sint64: return "sint64";
  }

  return "[invalid format]";
}

void print_value(IR::ValueIndex v, u32 offset, IR::Format format) {
  if (v.is_temporary()) {
    format_print_ST("T({}) [{}, {}]", v.index(), offset, format_name(format));
  }
  else {
    ASSERT(v.is_variable());
    format_print_ST("V({}) [{}, {}]", v.index(), offset, format_name(format));
  }
}

void IR::print_ir(const IR::Builder* builder) {
  IO_Single::lock();
  DEFER() { IO_Single::unlock(); };


  format_print_ST("== Function block GL{} ==\n", builder->global_label.label);
  format_print_ST("Signature =  {}\n", PrintSignatureType{ builder->signature });

  u32 num_params = (u32)builder->signature->parameter_types.size;

  {
    u32 counter = 0;
    format_print_ST("Variables ({}):\n", builder->variables.size);
    FOR(builder->variables, it) {
      format_print_ST("  {}: {}", counter, it->type.name);
      if (counter < num_params) {
        format_print_ST(" = param({})", counter);
      }

      IO_Single::print('\n');
      counter += 1;
    }
  }

  {
    u32 counter = 0;
    const ExpressionFrame* frames_start = builder->expression_frames.begin();
    const ExpressionFrame* frames_i = frames_start;
    const ExpressionFrame* frames_end = builder->expression_frames.end();

    format_print_ST("Temporaries ({}):\n", builder->temporaries.size);
    FOR(builder->temporaries, it) {
      while (frames_i < frames_end && frames_i->temporary_start == counter) {
        format_print_ST(" E{}:\n", frames_i - frames_start);
        frames_i += 1;
      }

      format_print_ST("  {}: {}", counter, it->type.name);
      counter += 1;

      switch (it->indirection) {
        case IR::Indirection::None: break;
        case IR::Indirection::Reference: IO_Single::print(" -> "); print_value(it->refers_to, it->refers_to_offset, IR::Format::opaque); break;
        case IR::Indirection::Dereference: IO_Single::print(" -| "); print_value(it->refers_to, it->refers_to_offset, IR::Format::opaque); break;
      }

      IO_Single::print('\n');
    }
  }

  {
    u32 counter = 0;
    format_print_ST("Captured Globals ({}):\n", builder->globals_used.size);
    FOR(builder->globals_used, it) {
      format_print_ST("  {}: {} -> Data Member {}\n", counter, it->type.name, it->data_member);

      counter += 1;
    }
  }


  const ControlBlock* block_start = builder->control_blocks.begin();
  const ControlBlock* block_i = block_start;
  const ControlBlock* block_end = builder->control_blocks.end();

  const ExpressionFrame* frames_start = builder->expression_frames.begin();
  const ExpressionFrame* frames_i = frames_start;
  const ExpressionFrame* frames_end = builder->expression_frames.end();

  const u8* bc_start = builder->ir_bytecode.data;
  const u8* bc = bc_start;
  const u8* bc_end = bc + builder->ir_bytecode.size;

  while (bc < bc_end) {
    u8 op_byte = *bc;
    OpCode op = static_cast<OpCode>(op_byte);

    while (block_i < block_end && block_i->start <= (usize)(bc - bc_start)) {
      format_print_ST("L{}:\n", block_i - block_start);
      block_i += 1;
    }

    while (frames_i < frames_end && frames_i->bytecode_start <= (bc - bc_start)) {
      format_print_ST(" E{}:\n", frames_i - frames_start);
      frames_i += 1;
    }

    IO_Single::print("  ");

    switch (op) {
      case IR::OpCode::Set: {
          IR::Types::Set set;
          bc = IR::Read::Set(bc, bc_end, set);

          print_value(set.to, set.t_offset, set.t_format);

          ByteArray arr = {};
          arr.ptr = set.data;
          arr.size = set.d_size;

          format_print_ST(" = raw [ {} ]\n", arr);

          break;
        }
      case IR::OpCode::CopyCast: {
          IR::Types::CopyCast copy;
          bc = IR::Read::CopyCast(bc, bc_end, copy);

          print_value(copy.to, copy.t_offset, copy.t_format);
          IO_Single::print(" = ");
          print_value(copy.from, copy.f_offset, copy.f_format);
          IO_Single::print("\n");

          break;
        }
      case IR::OpCode::GlobalAddress: {
          IR::Types::GlobalAddress glob;
          bc = IR::Read::GlobalAddress(bc, bc_end, glob);

          print_value(glob.val, glob.offset, glob.format);
          format_print_ST(" <- G({})\n", glob.im32);
          break;
        }
      case IR::OpCode::StartFunc: {
          IR::Types::StartFunc st;
          bc = IR::Read::StartFunc(bc, bc_end, st);

          IO_Single::print("Start Function: [");
          if (num_params > 0) {
            IO_Single::print("V(0)");
            for (u32 i = 1; i < num_params; ++i) {
              format_print_ST(", V({})", i);
            }
          }
          IO_Single::print("]\n");
          break;
        }
      case IR::OpCode::Call: {
          IR::Types::Call cl;
          bc = IR::Read::Call(bc, bc_end, cl);

          const u8* sv_i = cl.values;
          const u8* sv_end = sv_i + (SingleVal::serialize_size() * cl.n_values);

          format_print_ST("Call GL{} [", cl.label.label);
          if (cl.n_values > 0) {
            IR::SingleVal sv;
            sv_i += deserialize(sv_i, sv_end - sv_i, sv);

            print_value(sv.v, sv.v_offset, sv.v_format);

            for (u32 i = 1; i < cl.n_values; ++i) {
              sv_i += deserialize(sv_i, sv_end - sv_i, sv);

              IO_Single::print(", ");
              print_value(sv.v, sv.v_offset, sv.v_format);
            }
          }
          IO_Single::print("]\n");
          break;
        }
      case IR::OpCode::Return: {
          IR::Types::Return r;
          bc = IR::Read::Return(bc, bc_end, r);

          IO_Single::print("return ");
          print_value(r.val, r.offset, r.format);
          IO_Single::print('\n');
          break;
        }
      case IR::OpCode::IfSplit: {
          IR::Types::IfSplit r;
          bc = IR::Read::IfSplit(bc, bc_end, r);

          IO_Single::print("if (");
          print_value(r.val, r.offset, r.format);
          format_print_ST(") then jump L{} else jump L{}\n", r.label_if.label, r.label_else.label);
          break;
        }
      case IR::OpCode::Jump: {
          IR::Types::Jump j;
          bc = IR::Read::Jump(bc, bc_end, j);

          format_print_ST("jump L{}\n", j.local_label.label);
          break;
        }
#define BIN_OP_PRINT(name, op) \
      case IR::OpCode:: name : {\
          IR::Types:: name bin_op;\
          bc = IR::Read:: name (bc, bc_end, bin_op);\
          print_value(bin_op.to, bin_op.t_offset, bin_op.t_format);\
          IO_Single::print(" = ");\
          print_value(bin_op.left, bin_op.l_offset, bin_op.l_format);\
          IO_Single::print(" " #op " ");\
          print_value(bin_op.right, bin_op.r_offset, bin_op.r_format);\
          IO_Single::print('\n');\
          break;\
        }

                           BIN_OP_PRINT(Add, +);
                           BIN_OP_PRINT(Sub, -);
                           BIN_OP_PRINT(Mul, *);
                           BIN_OP_PRINT(Div, / );
                           BIN_OP_PRINT(Mod, %);
                           BIN_OP_PRINT(Eq, == );
                           BIN_OP_PRINT(Neq, != );
                           BIN_OP_PRINT(Less, < );
                           BIN_OP_PRINT(Great, > );
                           BIN_OP_PRINT(And, &);
                           BIN_OP_PRINT(Or, | );
                           BIN_OP_PRINT(Xor, ^);
#undef BIN_OP_PRINT

      case IR::OpCode::Neg: {
          IR::Types::Neg un_op;
          bc = IR::Read::Neg(bc, bc_end, un_op);

          print_value(un_op.to, un_op.t_offset, un_op.t_format);
          IO_Single::print(" = -");
          print_value(un_op.from, un_op.f_offset, un_op.f_format);
          IO_Single::print('\n');
          break;
        }
      case IR::OpCode::Not: {
          IR::Types::Neg un_op;
          bc = IR::Read::Neg(bc, bc_end, un_op);

          print_value(un_op.to, un_op.t_offset, un_op.t_format);
          IO_Single::print(" = !");
          print_value(un_op.from, un_op.f_offset, un_op.f_format);
          IO_Single::print('\n');
          break;
        }
      default: {
          IO_Single::print("---- INVALID INSTRUCTION ----\n");
          return;
        }
    }
  }

  IO_Single::print("=========================\n");
}