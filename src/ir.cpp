#include "ir.h"
#include "compiler.h"

namespace IR {
  IR::ValueIndex Builder::new_temporary(const Type& t) {
    IR::ControlBlock* cb = current_control_block();
    ASSERT(cb != nullptr);

    usize i = cb->temporaries.size;
    cb->temporaries.insert_uninit(1);
    SSATemp* temp = cb->temporaries.back();
    temp->type = t;
    
    return { static_cast<u32>(i) };
  }

  u32 Builder::new_variable(const Type& t) {
    usize i = variables.size;
    variables.insert_uninit(1);
    IR::SSAVar* var = variables.back();
    var->type = t;
    var->origin = current_block;
    var->versions = 0;

    return static_cast<u32>(i);
  }

  u32 Builder::new_global_reference(const IR::GlobalReference& ref) {
    u32 i = (u32)globals_used.size;
    globals_used.insert(ref);
    return i;
  }

  LocalLabel Builder::new_control_block() {
    ASSERT(control_blocks.size < 0xffffffff);

    control_blocks.insert_uninit(1);
    LocalLabel l = { static_cast<u32>(control_blocks.size) };
    control_blocks.back()->label = l;

    return l;
  }

  IR::ControlBlock* Builder::current_control_block() {
    ASSERT(current_block != IR::NULL_LOCAL_LABEL);
    return control_blocks.data + (current_block.label - 1);
  }

  Array<u8>& Builder::current_bytecode() {
    return current_control_block()->bytecode;
  }

  void Builder::start_control_block(LocalLabel label) {
    ASSERT(label != IR::NULL_LOCAL_LABEL);
    ASSERT(label.label <= control_blocks.size);
    current_block = label;
  }

  void Builder::end_control_block() {
    //TODO: maybe remove this, currently does nothing
  }

  void Builder::set_current_cf(const CFStart& st) {
    ASSERT(st.child != IR::NULL_LOCAL_LABEL);

    IR::ControlBlock* cb = current_control_block();
    cb->cf_type = st.CF_TYPE;
    cb->cf_start = st;
  }

  void Builder::set_current_cf(const CFEnd& e) {
    ASSERT(e.parent != IR::NULL_LOCAL_LABEL);

    IR::ControlBlock* cb = current_control_block();
    cb->cf_type = e.CF_TYPE;
    cb->cf_end = e;
  }
  void Builder::set_current_cf(const CFReturn& r) {
    ASSERT(r.parent != IR::NULL_LOCAL_LABEL);

    IR::ControlBlock* cb = current_control_block();
    cb->cf_type = r.CF_TYPE;
    cb->cf_return = r;
  }
  void Builder::set_current_cf(const CFInline& i) {
    ASSERT(i.parent != IR::NULL_LOCAL_LABEL);
    ASSERT(i.child != IR::NULL_LOCAL_LABEL);

    IR::ControlBlock* cb = current_control_block();
    cb->cf_type = i.CF_TYPE;
    cb->cf_inline = i;
  }
  void Builder::set_current_cf(const CFSplt& s) {
    ASSERT(s.parent != IR::NULL_LOCAL_LABEL);
    ASSERT(s.true_branch != IR::NULL_LOCAL_LABEL);
    ASSERT(s.false_branch != IR::NULL_LOCAL_LABEL);

    IR::ControlBlock* cb = current_control_block();
    cb->cf_type = s.CF_TYPE;
    cb->cf_split = s;
  }
  void Builder::set_current_cf(const CFMerge& m) {
    ASSERT(m.parents[0] != IR::NULL_LOCAL_LABEL);
    ASSERT(m.parents[1] != IR::NULL_LOCAL_LABEL);
    ASSERT(m.child != IR::NULL_LOCAL_LABEL);

    IR::ControlBlock* cb = current_control_block();
    cb->cf_type = m.CF_TYPE;
    cb->cf_merge = m;
  }
}

struct Serializer {
  u8* data_base;
  usize remaining_size;
  usize index = 0;

  constexpr Serializer(u8* base, usize remaining) : data_base(base), remaining_size(remaining) {}

  void write(const u8* in_data, usize in_size) {
    ASSERT(remaining_size >= in_size);
    memcpy_s(data_base + index, remaining_size, in_data, in_size);
    index += in_size;
    remaining_size -= in_size;
  }

  void write(u8 b) {
    ASSERT(remaining_size >= 1);
    data_base[index] = b;
    index += 1;
    remaining_size -= 1;
  }

  void write(u16 u){
    ASSERT(remaining_size >= 2);
    data_base[index] = u & 0xff;
    data_base[index + 1] = (u >> 8) & 0xff;

    index += 2;
    remaining_size -= 2;
  }

  void write(u32 u) {
    ASSERT(remaining_size >= 4);
    data_base[index] = u & 0xff;
    data_base[index + 1] = (u >> 8) & 0xff;
    data_base[index + 2] = (u >> 16) & 0xff;
    data_base[index + 3] = (u >> 24) & 0xff;

    index += 4;
    remaining_size -= 4;
  }

  void write(u64 u) {
    ASSERT(remaining_size >= 8);
    data_base[index] = u & 0xff;
    data_base[index + 1] = (u >> 8) & 0xff;
    data_base[index + 2] = (u >> 16) & 0xff;
    data_base[index + 3] = (u >> 24) & 0xff;
    data_base[index + 4] = (u >> 32) & 0xff;
    data_base[index + 5] = (u >> 40) & 0xff;
    data_base[index + 6] = (u >> 48) & 0xff;
    data_base[index + 7] = (u >> 56) & 0xff;

    index += 8;
    remaining_size -= 8;
  }

  void write(IR::ValueIndex v) {
    write(v.index);
  }

  void write(IR::LocalLabel l) {
    write(l.label);
  }

  void write(IR::GlobalLabel gl) {
    write(gl.label);
  }

  void write(const IR::V_ARG& v) {
    write(v.val);
    write(v.offset);
    write(v.size);
  }

  void write(const IR::C_ARG& c) {
    write(c.size);
    write(c.val, static_cast<usize>(c.size));
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
    ASSERT(remaining_size >= 1);
    b = data_base[index];
    index += 1;
    remaining_size -= 1;
  }

  void read(u16& u) {
    ASSERT(remaining_size >= 2);
    u = data_base[index]
      | (data_base[index + 1] << 16);

    index += 2;
    remaining_size -= 2;
  }

  void read(u32& u) {
    ASSERT(remaining_size >= 4);
    u = data_base[index]
      | (data_base[index + 1] << 8)
      | (data_base[index + 2] << 8)
      | (data_base[index + 3] << 8);

    index += 4;
    remaining_size -= 4;
  }

  void read(u64& u) {
    ASSERT(remaining_size >= 8);
    u = (u64)data_base[index]
      | ((u64)data_base[index + 1] << 8)
      | ((u64)data_base[index + 2] << 16)
      | ((u64)data_base[index + 3] << 24)
      | ((u64)data_base[index + 4] << 32)
      | ((u64)data_base[index + 5] << 40)
      | ((u64)data_base[index + 6] << 48)
      | ((u64)data_base[index + 7] << 56);

    index += 8;
    remaining_size -= 8;
  }

  void read(IR::ValueIndex& v) {
    read(v.index);
  }

  void read(IR::LocalLabel& l) {
    read(l.label);
  }

  void read(IR::GlobalLabel& gl) {
    read(gl.label);
  }

  void read(IR::V_ARG& v) {
    read(v.val);
    read(v.offset);
    read(v.size);
  }

  void read(IR::C_ARG& c) {
    read(c.size);
    c.val = bytes_slice(c.size);
  }
};


usize IR::serialize(u8* data, usize remaining_size, const CODE_V& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.val);

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_V_IM32& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.val);
  ser.write(i.im32);

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_V_C& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.to);
  ser.write(i.data);

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_V_V& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.to);
  ser.write(i.from);

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_V_V_V& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.to);
  ser.write(i.left);
  ser.write(i.right);

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_NV& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.n_values);

  for (u32 c = 0; c < i.n_values; ++c) {
    const V_ARG* v = i.values + c;
    ser.write(*v);
  }

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_GL_NV& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.label);
  ser.write(i.n_values);

  for (u32 c = 0; c < i.n_values; ++c) {
    const V_ARG* v = i.values + c;
    ser.write(*v);
  }

  return ser.index;
}

usize IR::serialize(u8* data, usize remaining_size, const CODE_V_LL& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Serializer ser = { data, remaining_size };

  ser.write(i.val);
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


usize IR::deserialize(const u8* data, usize remaining_size, CODE_V& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.val);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_V_IM32& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.val);
  ser.read(i.im32);

  return ser.index;
}


usize IR::deserialize(const u8* data, usize remaining_size, CODE_V_C& i) {
  ASSERT(remaining_size >= i.static_serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.to);
  ser.read(i.data);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_V_V& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.to);
  ser.read(i.from);

  return ser.index;
}


usize IR::deserialize(const u8* data, usize remaining_size, CODE_V_V_V& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.to);
  ser.read(i.left);
  ser.read(i.right);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_NV& i) {
  ASSERT(remaining_size >= i.static_serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.n_values);
  i.values = nullptr;

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_GL_NV& i) {
  ASSERT(remaining_size >= i.static_serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.label);
  ser.read(i.n_values);
  i.values = nullptr;

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, V_ARG& v) {
  ASSERT(remaining_size >= v.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(v);

  return ser.index;
}

usize IR::deserialize(const u8* data, usize remaining_size, CODE_V_LL& i) {
  ASSERT(remaining_size >= i.serialize_size());
  Deserializer ser = { data, remaining_size };

  ser.read(i.val);
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

Eval::RuntimeValue Eval::no_value() {
  return {};
}

Eval::RuntimeValue Eval::as_direct(IR::ValueIndex val, const Type& type) {
  Eval::RuntimeValue r = {};
  r.rvt = RVT::Direct;
  r.type = type;
  r.value = { val, 0 };

  return r;
}

Eval::RuntimeValue Eval::as_indirect(IR::ValueIndex val, const Type& type) {
  ASSERT(type.struct_type() == STRUCTURE_TYPE::POINTER);
  Eval::RuntimeValue r = {};
  r.rvt = RVT::Indirect;
  r.type = type;
  r.value = { val, 0 };

  return r;
}

Eval::RuntimeValue Eval::as_constant(const u8* constant, const Type& type) {
  Eval::RuntimeValue r = {};
  r.rvt = RVT::Constant;
  r.type = type;
  r.constant = constant;

  return r;
}

void Eval::assign(Eval::IrBuilder* builder, const Eval::RuntimeValue& to, const Eval::RuntimeValue& from) {
  ASSERT(to.rvt != RVT::Constant);

  ASSERT(to.type.size() > 0);
  ASSERT(from.type.size() > 0);
  ASSERT(to.type.size() <= 8);
  ASSERT(from.type.size() <= 8);

  switch (to.rvt) {
    case Eval::RVT::Constant: INVALID_CODE_PATH("Cannot assign to a constant"); return;

    case Eval::RVT::Direct: {
        switch (from.rvt) {
        case Eval::RVT::Constant: {
              IR::Types::Set set = {};
              set.data = IR::c_arg(from.constant, from.type);
              set.to = IR::v_arg(to.value.index, to.value.offset, to.type);

              IR::Emit::Set(builder->current_bytecode(), set);
              return;
            }
            case Eval::RVT::Direct: {
              IR::Types::Copy ccd = {};
              ccd.from = IR::v_arg(from.value.index, from.value.offset, from.type);
              ccd.to = IR::v_arg(to.value.index, to.value.offset, to.type);

              IR::Emit::Copy(builder->current_bytecode(), ccd);
              return;
            }
            case Eval::RVT::Indirect: {
                IR::Types::CopyLoad ccd = {};
                ccd.from = IR::v_arg(from.value.index, from.value.offset, from.type);
                ccd.to = IR::v_arg(to.value.index, to.value.offset, to.type);

                IR::Emit::CopyLoad(builder->current_bytecode(), ccd);
                return;
              }
        }
        break;
    }
    case Eval::RVT::Indirect: {
        switch (from.rvt) {
          case Eval::RVT::Constant: {
              IR::Types::SetStore set = {};
              set.data = IR::c_arg(from.constant, from.type);
              set.to = IR::v_arg(to.value.index, to.value.offset, to.type);

              IR::Emit::SetStore(builder->current_bytecode(), set);
              return;
            }
          case Eval::RVT::Direct: {
              IR::Types::CopyStore ccd = {};
              ccd.from = IR::v_arg(from.value.index, from.value.offset, from.type);
              ccd.to = IR::v_arg(to.value.index, to.value.offset, to.type);

              IR::Emit::CopyStore(builder->current_bytecode(), ccd);
              return;
            }
          case Eval::RVT::Indirect: {
              IR::Types::CopyLoadStore ccd = {};
              ccd.from = IR::v_arg(from.value.index, from.value.offset, from.type);
              ccd.to = IR::v_arg(to.value.index, to.value.offset, to.type);

              IR::Emit::CopyLoadStore(builder->current_bytecode(), ccd);
              return;
            }
        }
        break;
      }
  }

  INVALID_CODE_PATH("Invalid RVT");
}

Eval::RuntimeValue Eval::IrBuilder::new_variable(u32 id) {
  ASSERT(id < ir->variables.size);
  const Type& t = ir->variables.data[id].type;

#ifndef NDEBUG
  FOR_MUT(variables_state, it) {
    if (it->id == id) {
      INVALID_CODE_PATH("Duplicate imported variable");
      return Eval::no_value();
    }
  }
#endif

  ir->variables.data[id].versions += 1;//There is 1 vesion

  IR::ValueIndex v = ir->new_temporary(t);

  Eval::VariableState var = {};
  var.id = id;
  var.version = 0;
  var.imported = true;
  var.modified = true;
  var.current_temp = v;

  variables_state.insert(var);

  return Eval::as_direct(v, t);
}

Eval::RuntimeValue Eval::IrBuilder::import_variable(u32 id) {
  ASSERT(id < ir->variables.size);
  const Type& t = ir->variables.data[id].type;
  
  FOR_MUT(variables_state, it) {
    if (it->id == id) {

      if (it->imported) {
        ASSERT(it->current_temp.index < ir->current_control_block()->temporaries.size);
        return Eval::as_direct(it->current_temp, t);
      }

      IR::ValueIndex v = ir->new_temporary(t);
      it->imported = true;
      it->current_temp = v;

      return Eval::as_direct(v, t);
      
    }
  }

  INVALID_CODE_PATH("Did not find imported variable");
  return Eval::no_value();
}

void Eval::IrBuilder::set_variable(u32 id, IR::ValueIndex index) {
  ASSERT(id < ir->variables.size);
  auto& var = ir->variables.data[id];

  FOR_MUT(variables_state, it) {
    if (it->id == id) {
      it->modified = true;
      it->version = var.versions;
      var.versions += 1;
      it->current_temp = index;
    }
  }

  INVALID_CODE_PATH("Did not find imported variable");
}

void Eval::IrBuilder::switch_control_block(IR::LocalLabel index, IR::LocalLabel _parent) {
  ir->end_control_block();
  ir->start_control_block(index);
  parent = _parent;
}

void Eval::IrBuilder::export_variables() {
  auto* current_block = ir->current_control_block();
  ASSERT(current_block != nullptr);
  ASSERT(current_block->exports.size == 0);

  for (usize i = 0; i < variables_state.size; ++i) {
    auto& var = variables_state.data[i];
    if (var.modified) {
      auto e = IR::BlockExport{
        (u32)i,
        var.version,
        var.current_temp,
      };
      current_block->exports.insert(std::move(e));

      var.modified = false;//clear this for the next pass
    }

    var.imported = false;//Next block
  }

  current_block->exports.shrink();
}

void Eval::IrBuilder::rescope_variables(usize new_size) {
  ASSERT(variables_state.size >= new_size);
  variables_state.pop_n(variables_state.size - new_size);
}

void Eval::merge_variables(Eval::IrBuilder* builder, MergeRules rule,
                           IR::LocalLabel l0, Array<Eval::VariableState>&& in_v0,
                           IR::LocalLabel l1, Array<Eval::VariableState>&& in_v1) {
  ASSERT(&in_v0 != &in_v1);

  Array v0_temp = std::move(in_v0);
  Array v1 = std::move(in_v1);

  ASSERT(l0 != l1);
  ASSERT(v0_temp.size == v1.size);
  const usize n = v0_temp.size;

  builder->variables_state = std::move(v0_temp);//can't do this at the start because the moves might overlap
  auto& v0 = builder->variables_state;

  IR::ControlBlock* cb = builder->ir->current_control_block();

  for (usize i = 0; i < n; ++i) {
    auto& var0 = v0.data[i];
    const auto& var1 = v1.data[i];
    if (var0.version != var1.version) {
      //Need to merge

      switch (rule) {
        case MergeRules::New: {
            const auto next = builder->ir->variables.data[i].versions++;

            cb->enter_merge.insert(IR::BlockMerge{
              static_cast<u32>(i), { var0.version, var1.version }, next,
            });

            var0.version = next;
            break;
          }
        case MergeRules::UseFirst: {
            cb->enter_merge.insert(IR::BlockMerge{
              static_cast<u32>(i), { var0.version, var1.version }, var0.version,
            });
            break;
          }
      }

    }
  }
}

IR::V_ARG Eval::load_v_arg(Eval::IrBuilder* builder, const Eval::RuntimeValue& rv) {
  switch (rv.rvt) {
    case RVT::Constant: {
        auto* ir = builder->ir;
        IR::ValueIndex v = ir->new_temporary(rv.type);

        IR::Types::Set set = {};
        set.to = IR::v_arg(v, 0, rv.type);
        set.data = IR::c_arg(rv.constant, rv.type);

        IR::Emit::Set(ir->current_bytecode(), set);

        return IR::v_arg(v, 0, rv.type);
      }

    case RVT::Direct: {
        return IR::v_arg(rv.value.index, rv.value.offset, rv.type);
      }
    case RVT::Indirect: {
        auto* ir = builder->ir;
        IR::ValueIndex v = ir->new_temporary(rv.type);

        IR::Types::CopyLoad cpy = {};
        cpy.to = IR::v_arg(v, 0, rv.type);
        cpy.from = IR::v_arg(rv.value.index, rv.value.offset, rv.type);

        IR::Emit::CopyLoad(ir->current_bytecode(), cpy);

        return IR::v_arg(v, 0, rv.type);
      }
  }

  INVALID_CODE_PATH("Invalid RVT");
  return {};
}

Eval::RuntimeValue Eval::addrof(Eval::IrBuilder* const builder, const Eval::RuntimeValue& val, const Type& ptr_type) {
  ASSERT(ptr_type.struct_type() == STRUCTURE_TYPE::POINTER);

  switch (val.rvt) {
    case RVT::Constant: {
        INVALID_CODE_PATH("Cannot take the address of a constant");
        return Eval::no_value();
    }

    case RVT::Direct: {
        ASSERT(val.type == ptr_type.unchecked_base<PointerStructure>()->base);

        auto* ir = builder->ir;
        {
          const auto* cb = ir->current_control_block();
          ASSERT(cb->temporaries.size > val.value.index.index);
          cb->temporaries.data[val.value.index.index].requirements.add_address();
        }

        IR::ValueIndex v = ir->new_temporary(ptr_type);

        IR::Types::AddrOf addr = {};
        addr.from = IR::v_arg(val.value.index, val.value.offset, val.type);
        addr.to = IR::v_arg(v, 0, ptr_type);

        IR::Emit::AddrOf(ir->current_bytecode(), addr);

        return Eval::as_direct(v, ptr_type);
      }
    case RVT::Indirect: {
        ASSERT(val.type.struct_type() == STRUCTURE_TYPE::POINTER);
        ASSERT(val.type == ptr_type);

        const auto* ip = ptr_type.unchecked_base<PointerStructure>();

        Eval::RuntimeValue res = val;
        res.rvt = RVT::Direct;
        res.type = ip->base;

        return res;
      }
  }

  INVALID_CODE_PATH("Illegal RVT type");
  return Eval::no_value();
}

Eval::RuntimeValue Eval::arr_to_ptr(Eval::IrBuilder* const builder,
                                    const Eval::RuntimeValue& val, const Type& ptr_type) {
  ASSERT(ptr_type.struct_type() == STRUCTURE_TYPE::POINTER);

  switch (val.rvt) {
    case RVT::Constant: {
        INVALID_CODE_PATH("Cannot take the address of a constant");
        return Eval::no_value();
      }
    case RVT::Direct: {
        ASSERT(val.type.struct_type() == STRUCTURE_TYPE::FIXED_ARRAY);
        ASSERT(val.type.unchecked_base<ArrayStructure>()->base == ptr_type.unchecked_base<PointerStructure>()->base);

        auto* ir = builder->ir;
        {
          const auto* cb = ir->current_control_block();
          ASSERT(cb->temporaries.size > val.value.index.index);
          cb->temporaries.data[val.value.index.index].requirements.add_address();
        }

        IR::ValueIndex v = ir->new_temporary(ptr_type);

        IR::Types::AddrOf addr = {};
        addr.from = IR::v_arg(val.value.index, val.value.offset, val.type);
        addr.to = IR::v_arg(v, 0, ptr_type);

        IR::Emit::AddrOf(ir->current_bytecode(), addr);

        return Eval::as_direct(v, ptr_type);
      }
    case RVT::Indirect: {
        ASSERT(val.type.struct_type() == STRUCTURE_TYPE::POINTER);

        const auto* ip = val.type.unchecked_base<PointerStructure>();
        const Type& to_type = ip->base;

        ASSERT(to_type.struct_type() == STRUCTURE_TYPE::FIXED_ARRAY);
        ASSERT(to_type.unchecked_base<ArrayStructure>()->base == ptr_type.unchecked_base<PointerStructure>()->base);

        const auto* ptr_s = ptr_type.unchecked_base<PointerStructure>();

        Eval::RuntimeValue res = val;
        res.rvt = RVT::Direct;
        res.type = ptr_s->base;
        return res;
      }
  }


  INVALID_CODE_PATH("Illegal RVT type");
  return Eval::no_value();
}


Eval::RuntimeValue Eval::deref(Eval::IrBuilder* const builder,
                               const Eval::RuntimeValue& val, const Type& ptr_type) {
  ASSERT(val.type.struct_type() == STRUCTURE_TYPE::POINTER);
  ASSERT(ptr_type.struct_type() == STRUCTURE_TYPE::POINTER);

  switch (val.rvt) {
    case RVT::Constant: {
        INVALID_CODE_PATH("Cannot take the address of a constant");
        return Eval::no_value();
      }

    case RVT::Direct: {
        ASSERT(val.type == ptr_type);

        Eval::RuntimeValue res = val;
        res.rvt = RVT::Indirect;
        return res;
      }
    case RVT::Indirect: {
        ASSERT(val.type.unchecked_base<PointerStructure>()->base == ptr_type);

        auto* ir = builder->ir;

        IR::ValueIndex v = ir->new_temporary(ptr_type);

        IR::Types::AddrOfLoad addr = {};
        addr.from = IR::v_arg(val.value.index, val.value.offset, val.type);
        addr.to = IR::v_arg(v, 0, ptr_type);

        IR::Emit::AddrOfLoad(ir->current_bytecode(), addr);

        return Eval::as_indirect(v, ptr_type);
      }
  }

  INVALID_CODE_PATH("Illegal RVT type");
  return Eval::no_value();
}

Eval::RuntimeValue Eval::sub_object(Eval::IrBuilder* const builder,
                                    const Eval::RuntimeValue& val, const Eval::RuntimeValue& offset, const Type& ptr_type) {
  ASSERT(ptr_type.struct_type() == STRUCTURE_TYPE::POINTER);
  const auto* pt = ptr_type.unchecked_base<PointerStructure>();
  const Type& base = pt->base;

  if (offset.rvt == RVT::Constant) {
    //Special cases for a constant sub-object

    u64 offset_v = 0;
    memcpy_s(&offset_v, sizeof(offset_v), offset.constant, sizeof(offset_v));

    switch (val.rvt) {
      case RVT::Constant: {
          return Eval::as_constant(val.constant + offset_v, base);
        }

      case RVT::Direct: {
          Eval::RuntimeValue res = val;
          res.type = base;
          res.value.offset += (u32)offset_v;

          return res;
        }
      case RVT::Indirect: {
          break;
        }
    }
  }

  auto* ir = builder->ir;

  IR::V_ARG ptr_arg;
  
  switch (val.rvt) {
    case RVT::Constant: {
        INVALID_CODE_PATH("Cannot dynamically index a constant");
        return Eval::no_value();
      }
    case RVT::Direct: {
        IR::ValueIndex v = ir->new_temporary(ptr_type);

        ptr_arg = IR::v_arg(v, 0, ptr_type);

        ir->current_control_block()->temporaries.data[val.value.index.index].requirements.add_address();

        IR::Types::AddrOf addrof = {};
        addrof.from = IR::v_arg(val.value.index, val.value.offset, val.type);
        addrof.to = ptr_arg;

        IR::Emit::AddrOf(ir->current_bytecode(), addrof);
        break;
      }
    case RVT::Indirect: {
        ptr_arg = IR::v_arg(val.value.index, val.value.offset, val.type);
        break;
      }
    default: {
        INVALID_CODE_PATH("Illegal RVT");
        return Eval::no_value();
      }
  }

  IR::ValueIndex v = ir->new_temporary(ptr_type);

  IR::V_ARG offset_arg = Eval::load_v_arg(builder, offset);

  IR::Types::Add add = {};
  add.left = ptr_arg;
  add.right = offset_arg;
  add.to = IR::v_arg(v, 0, ptr_type);


  IR::Emit::Add(ir->current_bytecode(), add);

  return Eval::as_indirect(v, ptr_type);
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

void print_value(const IR::V_ARG& arg) {
  format_print_ST("T{} [{}..{}]", arg.val.index, arg.offset, arg.offset + arg.size);
}

void IR::print_ir(const IR::Builder* builder) {
  IO_Single::lock();
  DEFER() { IO_Single::unlock(); };


  format_print_ST("== Function block GL{} ==\n", builder->global_label.label - 1);
  format_print_ST("Signature =  {}\n", PrintSignatureType{ builder->signature });

  u32 num_params = (u32)builder->signature->parameter_types.size;

  if(builder->variables.size > 0) {
    u32 counter = 0;
    format_print_ST("Variables ({}):\n", builder->variables.size);
    FOR(builder->variables, it) {
      format_print_ST("  V{}: {} (versions = {})", counter, it->type.name, it->versions);
      if (counter < num_params) {
        format_print_ST(" = param({})", counter);
      }

      IO_Single::print('\n');
      counter += 1;
    }
  }

  if(builder->globals_used.size > 0){
    u32 counter = 0;
    format_print_ST("Captured Globals ({}):\n", builder->globals_used.size);
    FOR(builder->globals_used, it) {
      format_print_ST("  {}: {} -> Data Member {}\n", counter, it->type.name, it->data_member);

      counter += 1;
    }
  }


  FOR(builder->control_blocks, block) {
    IO_Single::print("\n");

    const u8* bc = block->bytecode.begin();
    const u8* bc_end = block->bytecode.end();

    const char* name = "<invalid>";
    switch (block->cf_type) {
      case ControlFlowType::Start: name = "Start"; break;
      case ControlFlowType::End: name = "End"; break;
      case ControlFlowType::Return: name = "Return"; break;
      case ControlFlowType::Inline: name = "Inline"; break;
      case ControlFlowType::Split: name = "Split"; break;
      case ControlFlowType::Merge: name = "Merge"; break;
    }

    format_print_ST("L{} (Type = \"{}\")\n", block->label.label - 1, name);

    if (block->enter_merge.size > 0) {
      format_print_ST("Merges ({}):\n", block->enter_merge.size);
      FOR(block->enter_merge, m) {
        format_print_ST("  V{} = {} <- phi[ {}, {} ]\n", m->variable, m->out_version, m->in_versions[0], m->in_versions[1]);
      }
    }

    if (block->temporaries.size > 0) {
      format_print_ST("Temporaries ({}):\n", block->temporaries.size);
      for (u32 i = 0; i < block->temporaries.size; ++i) {
        format_print_ST("  T{}: {}\n", i, block->temporaries.data[i].type.name);
      }
    }

    if (block->imports.size > 0) {
      format_print_ST("Imports ({}):\n", block->imports.size);
      FOR(block->imports, imp) {
        format_print_ST("  T{} = V{} ({})\n", imp->in_temp.index, imp->variable, imp->version);
      }
    }

    if (block->exports.size > 0) {
      format_print_ST("Exports ({}):\n", block->exports.size);
      FOR(block->exports, e) {
        format_print_ST("  V{} ({}) = T{}\n", e->variable, e->version, e->out_temp.index);
      }
    }

    IO_Single::print("Bytecode:\n");

    while (bc < bc_end) {
      u8 op_byte = *bc;
      OpCode op = static_cast<OpCode>(op_byte);

      IO_Single::print("  ");
      IO_Single::print(opcode_string(op), ":\t");

      switch (op) {
        case IR::OpCode::Set: {
            IR::Types::Set set;
            bc = IR::Read::Set(bc, bc_end, set);

            print_value(set.to);

            ByteArray arr = {};
            arr.ptr = set.data.val;
            arr.size = set.data.size;

            format_print_ST(" = raw [ {} ]\n", arr);

            break;
          }
        case IR::OpCode::SetStore: {
            IR::Types::SetStore set;
            bc = IR::Read::SetStore(bc, bc_end, set);

            IO_Single::print("[ ");
            print_value(set.to);
            IO_Single::print(" ]");

            ByteArray arr = {};
            arr.ptr = set.data.val;
            arr.size = set.data.size;

            format_print_ST(" = raw [ {} ]\n", arr);

            break;
          }
        case IR::OpCode::Copy: {
            IR::Types::Copy copy;
            bc = IR::Read::Copy(bc, bc_end, copy);

            print_value(copy.to);
            IO_Single::print(" = ");
            print_value(copy.from);
            IO_Single::print("\n");

            break;
          }
        case IR::OpCode::CopyLoad: {
            IR::Types::CopyLoad copy;
            bc = IR::Read::CopyLoad(bc, bc_end, copy);

            print_value(copy.to);
            IO_Single::print(" = [ ");
            print_value(copy.from);
            IO_Single::print(" ]\n");
            break;
          }
        case IR::OpCode::CopyStore: {
            IR::Types::CopyStore copy;
            bc = IR::Read::CopyStore(bc, bc_end, copy);

            IO_Single::print("[ ");
            print_value(copy.to);
            IO_Single::print(" ] = ");
            print_value(copy.from);
            IO_Single::print("\n");
            break;
          }
        case IR::OpCode::CopyLoadStore: {
            IR::Types::CopyLoadStore copy;
            bc = IR::Read::CopyLoadStore(bc, bc_end, copy);

            IO_Single::print("[ ");
            print_value(copy.to);
            IO_Single::print(" ] = [ ");
            print_value(copy.from);
            IO_Single::print(" ]\n");
            break;
          }
        case IR::OpCode::AddrOf: {
            IR::Types::AddrOf addr;
            bc = IR::Read::AddrOf(bc, bc_end, addr);

            print_value(addr.to);
            IO_Single::print(" = & ");
            print_value(addr.from);
            IO_Single::print("\n");
            break;
          }
        case IR::OpCode::AddrOfLoad: {
            IR::Types::AddrOf addr;
            bc = IR::Read::AddrOf(bc, bc_end, addr);

            print_value(addr.to);
            IO_Single::print(" = & [ ");
            print_value(addr.from);
            IO_Single::print(" ]\n");
            break;
          }
        case IR::OpCode::AddrOfGlobal: {
            IR::Types::AddrOfGlobal glob;
            bc = IR::Read::AddrOfGlobal(bc, bc_end, glob);

            print_value(glob.val);
            format_print_ST(" = & G({})\n", glob.im32);
            break;
          }
        case IR::OpCode::StartFunc: {
            IR::Types::StartFunc st;
            bc = IR::Read::StartFunc(bc, bc_end, st);
            ASSERT(st.n_values == num_params);

            IO_Single::print("Start Function: [ ");
            if (num_params > 0) {
              IR::V_ARG sv;
              bc += deserialize(bc, bc_end - bc, sv);

              print_value(sv);
              for (u32 i = 1; i < num_params; ++i) {
                bc += deserialize(bc, bc_end - bc, sv);

                IO_Single::print(", ");
                print_value(sv);
              }
            }
            IO_Single::print(" ]\n");
            break;
          }
        case IR::OpCode::Call: {
            IR::Types::Call cl;
            bc = IR::Read::Call(bc, bc_end, cl);

            format_print_ST("Call GL{} [", cl.label.label - 1);
            if (cl.n_values > 0) {
              IR::V_ARG sv;
              bc += deserialize(bc, bc_end - bc, sv);

              print_value(sv);

              for (u32 i = 1; i < cl.n_values; ++i) {
                bc += deserialize(bc, bc_end - bc, sv);

                IO_Single::print(", ");
                print_value(sv);
              }
            }
            IO_Single::print("]\n");
            break;
          }
#define BIN_OP_PRINT(name, op) \
      case IR::OpCode:: name : {\
          IR::Types:: name bin_op;\
          bc = IR::Read:: name (bc, bc_end, bin_op);\
          print_value(bin_op.to);\
          IO_Single::print(" = ");\
          print_value(bin_op.left);\
          IO_Single::print(" " #op " ");\
          print_value(bin_op.right);\
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

            print_value(un_op.to);
            IO_Single::print(" = -");
            print_value(un_op.from);
            IO_Single::print('\n');
            break;
          }
        case IR::OpCode::Not: {
            IR::Types::Neg un_op;
            bc = IR::Read::Neg(bc, bc_end, un_op);

            print_value(un_op.to);
            IO_Single::print(" = !");
            print_value(un_op.from);
            IO_Single::print('\n');
            break;
          }
        default: {
            IO_Single::print("---- INVALID INSTRUCTION ----\n");
            return;
          }
      }
    }

    switch (block->cf_type) {
      case ControlFlowType::Start: {
          format_print_ST("goto L{}\n", block->cf_start.child.label - 1);
          break;
        }
      case ControlFlowType::Inline: {
          format_print_ST("goto L{}\n", block->cf_inline.child.label - 1);
          break;
        }
      case ControlFlowType::End: {
          IO_Single::print("end\n");
          break;
        }
      case ControlFlowType::Return: {
          format_print_ST("return T{}\n", block->cf_return.val.index);
          IO_Single::print('\n');
          break;
        }
      case ControlFlowType::Split: {
          IR::ValueIndex cond = block->cf_split.condition;
          IR::LocalLabel true_label = block->cf_split.true_branch;
          IR::LocalLabel false_label = block->cf_split.false_branch;

          format_print_ST("if T{} then goto L{} else goto L{}\n", cond.index, true_label.label - 1, false_label.label - 1);
          break;
        }
      case ControlFlowType::Merge: {
          format_print_ST("goto L{}\n", block->cf_merge.child.label - 1);
          break;
        }
    }
  }

  IO_Single::print("=========================\n");
}