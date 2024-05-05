#include "ir.h"
#include "compiler.h"

#include <AxleUtil/io.h>

namespace IO_Single = Axle::IO_Single;

namespace IR {
  IR::ValueIndex IRStore::new_temporary(const VariableId& v_id, ValueRequirements requirements) {
    IR::ControlBlock* cb = current_control_block();
    ASSERT(cb != nullptr);

    const SSAVar& var = variables.data[v_id.variable];

    usize i = cb->temporaries.size;
    cb->temporaries.insert_uninit(1);
    SSATemp* temp = cb->temporaries.back();
    temp->type = var.type;
    temp->requirements = requirements;
    temp->is_variable = true;
    temp->var_id = v_id;

    return { static_cast<u32>(i) };
  }

  IR::ValueIndex IRStore::new_temporary(const Type& t, ValueRequirements requirements) {
    IR::ControlBlock* cb = current_control_block();
    ASSERT(cb != nullptr);

    usize i = cb->temporaries.size;
    cb->temporaries.insert_uninit(1);
    SSATemp* temp = cb->temporaries.back();
    temp->type = t;
    temp->requirements = requirements;

    return { static_cast<u32>(i) };
  }

  VariableId IRStore::new_variable(const Type& t, ValueRequirements requirements) {
    usize i = variables.size;
    variables.insert_uninit(1);
    IR::SSAVar* var = variables.back();
    var->type = t;
    var->requirements = requirements;

    return VariableId{ static_cast<u32>(i) };
  }

  u32 IRStore::new_global_reference(const IR::GlobalReference& ref) {
    u32 i = (u32)globals_used.size;
    globals_used.insert(ref);
    return i;
  }

  LocalLabel IRStore::new_control_block() {
    ASSERT(control_blocks.size < 0xffffffff);

    control_blocks.insert_uninit(1);
    LocalLabel l = { static_cast<u32>(control_blocks.size) };
    control_blocks.back()->label = l;

    return l;
  }

  IR::ControlBlock* IRStore::current_control_block() {
    ASSERT(current_block != IR::NULL_LOCAL_LABEL);
    return control_blocks.data + (current_block.label - 1);
  }

  Axle::Array<u8>& IRStore::current_bytecode() {
    return current_control_block()->bytecode;
  }

  void IRStore::start_control_block(LocalLabel label) {
    ASSERT(label != IR::NULL_LOCAL_LABEL);
    ASSERT(label.label <= control_blocks.size);
    current_block = label;
  }

  void IRStore::end_control_block() {
    //TODO: maybe remove this, currently does nothing
  }

  void IRStore::set_current_cf(const CFStart& st) {
    ASSERT(st.child != IR::NULL_LOCAL_LABEL);

    IR::ControlBlock* cb = current_control_block();
    cb->cf_type = st.CF_TYPE;
    cb->cf_start = st;
  }

  void IRStore::set_current_cf(const CFEnd& e) {
    ASSERT(e.parent != IR::NULL_LOCAL_LABEL);

    IR::ControlBlock* cb = current_control_block();
    cb->cf_type = e.CF_TYPE;
    cb->cf_end = e;
  }
  void IRStore::set_current_cf(const CFReturn& r) {
    ASSERT(r.parent != IR::NULL_LOCAL_LABEL);

    IR::ControlBlock* cb = current_control_block();
    cb->cf_type = r.CF_TYPE;
    cb->cf_return = r;
  }
  void IRStore::set_current_cf(const CFInline& i) {
    ASSERT(i.parent != IR::NULL_LOCAL_LABEL);
    ASSERT(i.child != IR::NULL_LOCAL_LABEL);

    IR::ControlBlock* cb = current_control_block();
    cb->cf_type = i.CF_TYPE;
    cb->cf_inline = i;
  }
  void IRStore::set_current_cf(const CFSplt& s) {
    ASSERT(s.parent != IR::NULL_LOCAL_LABEL);
    ASSERT(s.true_branch != IR::NULL_LOCAL_LABEL);
    ASSERT(s.false_branch != IR::NULL_LOCAL_LABEL);

    IR::ControlBlock* cb = current_control_block();
    cb->cf_type = s.CF_TYPE;
    cb->cf_split = s;
  }
  void IRStore::set_current_cf(const CFMerge& m) {
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

  void write(u16 u) {
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


Eval::IrBuilder Eval::start_builder(Eval::Time eval_time, IR::IRStore* ir) {
  return start_builder(eval_time, ir, {});
}

Eval::IrBuilder Eval::start_builder(Eval::Time eval_time, IR::IRStore* ir, AST_ARR params) {
  ASSERT(ir != nullptr);
  ASSERT(ir->signature != nullptr);
  ASSERT(ir->signature->parameter_types.size == params.count);

  Eval::IrBuilder builder = {};
  builder.ir = ir;
  builder.eval_time = eval_time;

  IR::LocalLabel startup = ir->new_control_block();
  IR::LocalLabel first = ir->new_control_block();
  ir->start_control_block(startup);
  ir->set_current_cf(IR::CFStart{
    first,
                     });

  if (params.count > 0) {
    Axle::OwnedArr<IR::V_ARG> args = Axle::new_arr<IR::V_ARG>(params.count);

    {
      const Type* parameters = ir->signature->parameter_types.begin();
      const Type* const parameters_end = ir->signature->parameter_types.end();
      IR::V_ARG* va = args.mut_begin();

      FOR_AST(params, p) {
        ASSERT(parameters < parameters_end);
        ASSERT(p->node_type == *parameters);
        ASSERT(p->ast_type == AST_TYPE::TYPED_NAME);
        ASTTypedName* n = (ASTTypedName*)p;
        Local* local_ptr = n->local_ptr;

        auto id = builder.new_variable(*parameters, {});
        local_ptr->variable_id = id;
        *va = Eval::load_v_arg(ir, builder.import_variable(id, {}));
        va += 1;
        parameters += 1;
      }

      ASSERT(parameters == parameters_end);
    }

    IR::Types::StartFunc sf = {};
    sf.n_values = (u32)args.size;
    sf.values = args.data;

    IR::Emit::StartFunc(ir->current_bytecode(), sf);
  }
  else {
    IR::Types::StartFunc sf = {};
    sf.n_values = 0;
    sf.values = nullptr;

    IR::Emit::StartFunc(ir->current_bytecode(), sf);
  }

  builder.reset_variables();
  builder.switch_control_block(first, startup);

  return builder;
}

void Eval::end_builder(Eval::IrBuilder* builder) {
  IR::IRStore* ir = builder->ir;

  if (ir->current_block != IR::NULL_LOCAL_LABEL) {
    ASSERT(builder->parent != IR::NULL_LOCAL_LABEL);
    ir->set_current_cf(IR::CFEnd{
      builder->parent,
                       });
  }
}

void Eval::assign(IR::IRStore* const ir, const Eval::RuntimeValue& to, const Eval::RuntimeValue& from) {
  ASSERT(to.rvt != RVT::Constant);

  ASSERT(to.type.size() > 0);
  ASSERT(from.type.size() > 0);

  switch (to.rvt) {
    case Eval::RVT::Constant: INVALID_CODE_PATH("Cannot assign to a constant"); return;

    case Eval::RVT::Direct: {
        switch (from.rvt) {
          case Eval::RVT::Constant: {
              IR::Types::Set set = {};
              set.data = IR::c_arg(from.constant, from.type);
              set.to = IR::v_arg(to.value.index, to.value.offset, to.type);

              IR::Emit::Set(ir->current_bytecode(), set);
              return;
            }
          case Eval::RVT::Direct: {
              IR::Types::Copy ccd = {};
              ccd.from = IR::v_arg(from.value.index, from.value.offset, from.type);
              ccd.to = IR::v_arg(to.value.index, to.value.offset, to.type);

              IR::Emit::Copy(ir->current_bytecode(), ccd);
              return;
            }
          case Eval::RVT::Indirect: {
              IR::Types::CopyLoad ccd = {};
              ccd.from = IR::v_arg(from.value.index, from.value.offset, from.type);
              ccd.to = IR::v_arg(to.value.index, to.value.offset, to.type);

              IR::Emit::CopyLoad(ir->current_bytecode(), ccd);
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

              IR::Emit::SetStore(ir->current_bytecode(), set);
              return;
            }
          case Eval::RVT::Direct: {
              IR::Types::CopyStore ccd = {};
              ccd.from = IR::v_arg(from.value.index, from.value.offset, from.type);
              ccd.to = IR::v_arg(to.value.index, to.value.offset, to.type);

              IR::Emit::CopyStore(ir->current_bytecode(), ccd);
              return;
            }
          case Eval::RVT::Indirect: {
              IR::Types::CopyLoadStore ccd = {};
              ccd.from = IR::v_arg(from.value.index, from.value.offset, from.type);
              ccd.to = IR::v_arg(to.value.index, to.value.offset, to.type);

              IR::Emit::CopyLoadStore(ir->current_bytecode(), ccd);
              return;
            }
        }
        break;
      }
  }

  INVALID_CODE_PATH("Invalid RVT");
}

IR::VariableId Eval::IrBuilder::new_variable(const Type& t, IR::ValueRequirements reqs) {
  IR::VariableId id = ir->new_variable(t, reqs);

  {
    IR::SSAVar& v = ir->variables.data[id.variable];
    u32 offset = Axle::ceil_to_n(curr_stack, v.type.size());
    v.stack_offset = offset;

    curr_stack = offset + v.type.size();

    if (curr_stack > ir->max_stack) ir->max_stack = curr_stack;
  }

  Eval::VariableState var = {};
  var.id = id;
  var.imported = false;

  variables_state.insert(var);

  return id;
}

Eval::RuntimeValue Eval::IrBuilder::import_variable(const IR::VariableId& id, IR::ValueRequirements reqs) {
  ASSERT(id.variable < ir->variables.size);
  IR::SSAVar& sv = ir->variables.data[id.variable];
  sv.requirements |= reqs;

  FOR_MUT(variables_state, it) {
    if (it->id.variable == id.variable) {

      if (it->imported) {
        const IR::ControlBlock* cb = ir->current_control_block();
        ASSERT(it->current_temp.index < cb->temporaries.size);

        cb->temporaries.data[it->current_temp.index].requirements |= reqs;

        return Eval::as_direct(it->current_temp, sv.type);
      }

      IR::ValueIndex v = ir->new_temporary(id, reqs);

      it->imported = true;
      it->current_temp = v;

      return Eval::as_direct(v, sv.type);
    }
  }

  INVALID_CODE_PATH("Did not find imported variable");
  return Eval::no_value();
}

void Eval::IrBuilder::switch_control_block(IR::LocalLabel index, IR::LocalLabel _parent) {
  ir->end_control_block();
  ir->start_control_block(index);
  parent = _parent;
}

void Eval::IrBuilder::rescope_variables(usize new_size) {
  ASSERT(variables_state.size >= new_size);
  variables_state.pop_n(variables_state.size - new_size);

}
void Eval::IrBuilder::reset_variables() {
  FOR_MUT(variables_state, it) {
    it->imported = false;
  }
}

IR::V_ARG Eval::load_v_arg(IR::IRStore* ir, const Eval::RuntimeValue& rv) {
  switch (rv.rvt) {
    case RVT::Constant: {
        IR::ValueIndex v = ir->new_temporary(rv.type, {});

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
        IR::ValueIndex v = ir->new_temporary(rv.type, {});

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

Eval::RuntimeValue Eval::addrof(IR::IRStore* const ir, const Eval::RuntimeValue& val, const Type& ptr_type) {
  ASSERT(ptr_type.struct_type() == STRUCTURE_TYPE::POINTER);

  switch (val.rvt) {
    case RVT::Constant: {
        INVALID_CODE_PATH("Cannot take the address of a constant");
        return Eval::no_value();
      }

    case RVT::Direct: {
        ASSERT(val.type == ptr_type.unchecked_base<PointerStructure>()->base);

        {
          const auto* cb = ir->current_control_block();
          ASSERT(cb->temporaries.size > val.value.index.index);
          ASSERT(cb->temporaries.data[val.value.index.index].requirements.has_address());
        }

        IR::ValueIndex v = ir->new_temporary(ptr_type, {});

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

Eval::RuntimeValue Eval::arr_to_ptr(IR::IRStore* const ir,
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

        {
          const auto* cb = ir->current_control_block();
          ASSERT(cb->temporaries.size > val.value.index.index);
          ASSERT(cb->temporaries.data[val.value.index.index].requirements.has_address());
        }

        IR::ValueIndex v = ir->new_temporary(ptr_type, {});

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


Eval::RuntimeValue Eval::deref(IR::IRStore* const ir,
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

        IR::ValueIndex v = ir->new_temporary(ptr_type, {});

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

Eval::RuntimeValue Eval::sub_object(IR::IRStore* const ir,
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

  IR::V_ARG ptr_arg;

  switch (val.rvt) {
    case RVT::Constant: {
        INVALID_CODE_PATH("Cannot dynamically index a constant");
        return Eval::no_value();
      }
    case RVT::Direct: {
        IR::ValueIndex v = ir->new_temporary(ptr_type, {});

        ptr_arg = IR::v_arg(v, 0, ptr_type);

        ASSERT(ir->current_control_block()->temporaries.data[val.value.index.index].requirements.has_address());

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

  IR::ValueIndex v = ir->new_temporary(ptr_type, {});

  IR::V_ARG offset_arg = Eval::load_v_arg(ir, offset);

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
  IO_Single::format("T{} [{}..{}]", arg.val.index, arg.offset, arg.offset + arg.size);
}

void IR::print_ir(CompilerGlobals* const comp, const IR::IRStore* builder) {
  IO_Single::lock();
  DEFER() { IO_Single::unlock(); };

  {
    const GlobalLabelInfo l_info = comp->get_label_info(builder->global_label);
    if(l_info.span.full_path != nullptr) {
      IO_Single::format("== Format block GL{} | {}({}:{}) ==\n",
                        builder->global_label.label - 1, l_info.span.full_path,
                        l_info.span.line_start, l_info.span.char_start);
    }
    else {
      IO_Single::format("== Function block GL{} ==\n", builder->global_label.label - 1);
    }
  }

  IO_Single::format("Signature =  {}\n", PrintSignatureType{ builder->signature });

  u32 num_params = (u32)builder->signature->parameter_types.size;

  if (builder->variables.size > 0) {
    u32 counter = 0;
    IO_Single::format("Variables ({}):\n", builder->variables.size);
    FOR(builder->variables, it) {
      IO_Single::format("  V{}: {}", counter, it->type.name);
      if (counter < num_params) {
        IO_Single::format(" = param({})", counter);
      }

      IO_Single::print('\n');
      counter += 1;
    }
  }

  if (builder->globals_used.size > 0) {
    u32 counter = 0;
    IO_Single::format("Captured Globals ({}):\n", builder->globals_used.size);
    FOR(builder->globals_used, it) {
      IO_Single::format("  {}: {} -> Data Member {}\n", counter, it->type.name, it->data_member);

      counter += 1;
    }
  }


  FOR(builder->control_blocks, block) {
    IO_Single::print("\n");

    const u8* bc = block->bytecode.begin();
    const u8* bc_end = block->bytecode.end();

    Axle::ViewArr<const char> name = Axle::lit_view_arr("<invalid>");
    switch (block->cf_type) {
      case ControlFlowType::Start: name = Axle::lit_view_arr("Start"); break;
      case ControlFlowType::End: name = Axle::lit_view_arr("End"); break;
      case ControlFlowType::Return: name = Axle::lit_view_arr("Return"); break;
      case ControlFlowType::Inline: name = Axle::lit_view_arr("Inline"); break;
      case ControlFlowType::Split: name = Axle::lit_view_arr("Split"); break;
      case ControlFlowType::Merge: name = Axle::lit_view_arr("Merge"); break;
    }

    IO_Single::format("L{} (Type = \"{}\")\n", block->label.label - 1, name);

    if (block->temporaries.size > 0) {
      IO_Single::format("Temporaries ({}):\n", block->temporaries.size);
      for (u32 i = 0; i < block->temporaries.size; ++i) {
        const SSATemp& temp = block->temporaries.data[i];
        if (temp.is_variable) {
          IO_Single::format("  T{}: {} = V{}\n", i, temp.type.name, temp.var_id.variable);
        }
        else {
          IO_Single::format("  T{}: {}\n", i, temp.type.name);
        }
      }
    }

    IO_Single::print("Bytecode:\n");

    while (bc < bc_end) {
      u8 op_byte = *bc;
      OpCode op = static_cast<OpCode>(op_byte);

      IO_Single::format("  {}:\t", op);

      switch (op) {
        case IR::OpCode::Set: {
            IR::Types::Set set;
            bc = IR::Read::Set(bc, bc_end, set);

            print_value(set.to);

            Axle::ByteArray arr = {};
            arr.ptr = set.data.val;
            arr.size = set.data.size;

            IO_Single::format(" = raw [ {} ]\n", arr);

            break;
          }
        case IR::OpCode::SetStore: {
            IR::Types::SetStore set;
            bc = IR::Read::SetStore(bc, bc_end, set);

            IO_Single::print("[ ");
            print_value(set.to);
            IO_Single::print(" ]");

            Axle::ByteArray arr = {};
            arr.ptr = set.data.val;
            arr.size = set.data.size;

            IO_Single::format(" = raw [ {} ]\n", arr);

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
            IO_Single::format(" = & G({})\n", glob.im32);
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

            IO_Single::format("Call GL{} [", cl.label.label - 1);
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
          IO_Single::format("goto L{}\n", block->cf_start.child.label - 1);
          break;
        }
      case ControlFlowType::Inline: {
          IO_Single::format("goto L{}\n", block->cf_inline.child.label - 1);
          break;
        }
      case ControlFlowType::End: {
          IO_Single::print("end\n");
          break;
        }
      case ControlFlowType::Return: {
          IO_Single::format("return T{}\n", block->cf_return.val.index);
          IO_Single::print('\n');
          break;
        }
      case ControlFlowType::Split: {
          IR::ValueIndex cond = block->cf_split.condition;
          IR::LocalLabel true_label = block->cf_split.true_branch;
          IR::LocalLabel false_label = block->cf_split.false_branch;

          IO_Single::format("if T{} then goto L{} else goto L{}\n", cond.index, true_label.label - 1, false_label.label - 1);
          break;
        }
      case ControlFlowType::Merge: {
          IO_Single::format("goto L{}\n", block->cf_merge.child.label - 1);
          break;
        }
    }
  }

  IO_Single::print("=========================\n");
}
