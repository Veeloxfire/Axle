#include "ir.h"

#include <AxleTest/unit_tests.h>

#define SETUP_TEST_TYPES(strings, structures, builtin_types)\
Axle::StringInterner strings = {};\
Structures structures = {8,8};\
BuiltinTypes builtin_types = STRUCTS::create_builtins(&structures, &strings)

#define SETUP_TEST_IR(ir, start_block)\
IR::IRStore ir = {};\
ir.signature = builtin_types.t_void_call.unchecked_base<SignatureStructure>();\
ir.global_label = { 1 };\
IR::LocalLabel start_block = ir.new_control_block();\
ir.start_control_block(start_block)

TEST_FUNCTION(IR, control_flow) {
  SETUP_TEST_TYPES(strings, structures, builtin_types);
  IR::IRStore ir = {};
  
  TEST_EQ(IR::NULL_GLOBAL_LABEL, ir.global_label);
  TEST_EQ(static_cast<const SignatureStructure*>(nullptr), ir.signature);

  ir.signature = builtin_types.t_void_call.unchecked_base<SignatureStructure>();
  ir.global_label = { 1 };
  
  TEST_EQ(IR::NULL_LOCAL_LABEL, ir.current_block);
  TEST_EQ(static_cast<usize>(0), ir.variables.size);
  TEST_EQ(static_cast<u32>(0), ir.max_stack);
  TEST_EQ(static_cast<usize>(0), ir.globals_used.size);
  TEST_EQ(static_cast<usize>(0), ir.control_flow_labels.size);
  TEST_EQ(static_cast<usize>(0), ir.control_blocks.size);

  IR::LocalLabel start_block = ir.new_control_block();
  TEST_NEQ(IR::NULL_LOCAL_LABEL, start_block);
  TEST_EQ(IR::NULL_LOCAL_LABEL, ir.current_block);

  IR::LocalLabel end_block = ir.new_control_block();
  TEST_NEQ(IR::NULL_LOCAL_LABEL, end_block);
  TEST_EQ(IR::NULL_LOCAL_LABEL, ir.current_block);

  TEST_NEQ(start_block, end_block);

  ir.start_control_block(start_block);
  TEST_EQ(start_block, ir.current_block);
  TEST_NEQ(end_block, ir.current_block);
  TEST_NEQ(IR::NULL_LOCAL_LABEL, ir.current_block);

  {
    Axle::Array<u8>& cb = ir.current_bytecode();
    TEST_EQ(static_cast<usize>(0), cb.size);
  }

  {
    IR::CFStart cf = {};
    cf.child = end_block;
    ir.set_current_cf(cf);
  }

  {
    IR::ControlBlock* cb = ir.current_control_block();
    TEST_NEQ(static_cast<IR::ControlBlock*>(nullptr), cb);
    TEST_EQ(start_block, cb->label);
    TEST_EQ(false, cb->calls);
    TEST_EQ(IR::ControlFlowType::Start, cb->cf_type);
    TEST_EQ(end_block, cb->cf_start.child);
    TEST_EQ(static_cast<usize>(0), cb->bytecode.size);
    TEST_EQ(static_cast<usize>(0), cb->temporaries.size);
  }

  ir.end_control_block();
  ir.start_control_block(end_block);

  TEST_EQ(end_block, ir.current_block);
  TEST_NEQ(start_block, ir.current_block);
  TEST_NEQ(IR::NULL_LOCAL_LABEL, ir.current_block);

  {
    Axle::Array<u8>& cb = ir.current_bytecode();
    TEST_EQ(static_cast<usize>(0), cb.size);
  }

  {
    IR::CFEnd cf = {};
    cf.parent = start_block;
    ir.set_current_cf(cf);
  }

  {
    IR::ControlBlock* cb = ir.current_control_block();
    TEST_NEQ(static_cast<IR::ControlBlock*>(nullptr), cb);
    TEST_EQ(end_block, cb->label);
    TEST_EQ(false, cb->calls);
    TEST_EQ(IR::ControlFlowType::End, cb->cf_type);
    TEST_EQ(start_block, cb->cf_end.parent);
    TEST_EQ(static_cast<usize>(0), cb->bytecode.size);
    TEST_EQ(static_cast<usize>(0), cb->temporaries.size);
  }

  ir.end_control_block();

  TEST_EQ(static_cast<usize>(2), ir.control_blocks.size);
}


TEST_FUNCTION(IR, temporaries) {
  SETUP_TEST_TYPES(strings, structures, builtin_types);
  SETUP_TEST_IR(ir, start_block);

  IR::ValueIndex id1 = ir.new_temporary(builtin_types.t_u64, {});
  TEST_EQ(static_cast<u32>(0), id1.index);
  IR::ValueIndex id2 = ir.new_temporary(builtin_types.t_u64, {});
  TEST_EQ(static_cast<u32>(1), id2.index);
  IR::ValueIndex id3 = ir.new_temporary(builtin_types.t_u64, {});
  TEST_EQ(static_cast<u32>(2), id3.index);

  TEST_EQ(static_cast<usize>(3), ir.current_control_block()->temporaries.size);
}

TEST_FUNCTION(Eval, value_creation) {
  SETUP_TEST_TYPES(strings, structures, builtin_types);
  SETUP_TEST_IR(ir, start_block);
  {
    Eval::RuntimeValue rv = Eval::no_value();

    TEST_EQ(static_cast<const Structure*>(nullptr), rv.type.structure);
    TEST_EQ(static_cast<const Axle::InternString*>(nullptr), rv.type.name);
  }

  IR::ValueIndex id1 = ir.new_temporary(builtin_types.t_u64, {});
  TEST_EQ(static_cast<u32>(0), id1.index);

  {
    Eval::RuntimeValue v = Eval::as_direct(id1, builtin_types.t_u64);

    TEST_EQ(Eval::RVT::Direct, v.rvt);
    TEST_EQ(builtin_types.t_u64, v.type);
    TEST_EQ(builtin_types.t_u64, v.effective_type());
    TEST_EQ(id1, v.value.index);
    TEST_EQ(static_cast<u32>(0), v.value.offset);
  }

  {
    const PointerStructure* ps = STRUCTS::new_pointer_structure(&structures, &strings, builtin_types.t_u64);

    Type ps_t = to_type(ps);
    Eval::RuntimeValue v = Eval::as_indirect(id1, ps_t);

    TEST_EQ(Eval::RVT::Indirect, v.rvt);
    TEST_EQ(ps_t, v.type);
    TEST_EQ(builtin_types.t_u64, v.effective_type());
    TEST_EQ(id1, v.value.index);
    TEST_EQ(static_cast<u32>(0), v.value.offset);
  }
}
