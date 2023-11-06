#include "unit_tests.h"
#include "ir.h"
#include "backends.h"

#if 0

TEST_FUNCTION(IR, basic_math) {
  //Setup

  StringInterner strings = {};

  IntegerStructure u64_s = {};
  {
    u64_s.type = IntegerStructure::expected_type_enum;
    u64_s.size = 8;
    u64_s.alignment = 8;
    u64_s.ir_format = IR::Format::uint64;
    u64_s.is_signed = false;
    u64_s.struct_name = strings.intern(lit_view_arr("u64"));
  }
  Type u64_t = to_type(&u64_s);

  SignatureStructure void_call_s = {};
  {
    void_call_s.type = SignatureStructure::expected_type_enum;
    void_call_s.size = 8;
    void_call_s.alignment = 8;
    void_call_s.ir_format = IR::Format::opaque;
    void_call_s.calling_convention = nullptr;
    void_call_s.parameter_types = {};
    void_call_s.return_type = {};
    void_call_s.struct_name = strings.intern(lit_view_arr("void-call"));
  }
  Type void_call_t = to_type(&u64_s);

  IR::IRStore ir = {};
  ir.signature = &void_call_s;
  ir.global_label = { 1 };

  IR::LocalLabel start_cf = ir.new_control_block();
  IR::LocalLabel end_cf = ir.new_control_block();
  ir.start_control_block(start_cf);
  ir.set_current_cf(IR::CFStart{ end_cf });

  Eval::RuntimeValue v0 = Eval::as_direct(ir.new_temporary(u64_t, {}), u64_t);
  {
    u8 c0_data[8] = {};
    x64_to_bytes(8llu, c0_data);

    u8 c1_data[8] = {};
    x64_to_bytes(16llu, c1_data);

    Eval::RuntimeValue c0 = Eval::as_constant(c0_data, u64_t);
    Eval::RuntimeValue c1 = Eval::as_constant(c1_data, u64_t);

    IR::Types::Add add = {};
    add.left = Eval::load_v_arg(&ir, c0);
    add.right = Eval::load_v_arg(&ir, c1);
    add.to = Eval::load_v_arg(&ir, v0);

    IR::Emit::Add(ir.current_bytecode(), add);
  }

  Eval::RuntimeValue v1 = Eval::as_direct(ir.new_temporary(u64_t, {}), u64_t);
  {
    u8 c0_data[8] = {};
    x64_to_bytes(75llu, c0_data);

    Eval::RuntimeValue c0 = Eval::as_constant(c0_data, u64_t);

    IR::Types::Mul mul = {};
    mul.left = Eval::load_v_arg(&ir, c0);
    mul.right = Eval::load_v_arg(&ir, v0);
    mul.to = Eval::load_v_arg(&ir, v1);

    IR::Emit::Mul(ir.current_bytecode(), mul);
  }

  Eval::RuntimeValue v2 = Eval::as_direct(ir.new_temporary(u64_t, {}), u64_t);
  {
    u8 c0_data[8] = {};
    x64_to_bytes(12llu, c0_data);

    Eval::RuntimeValue c0 = Eval::as_constant(c0_data, u64_t);

    IR::Types::Div div = {};
    div.left = Eval::load_v_arg(&ir, c0);
    div.right = Eval::load_v_arg(&ir, v1);
    div.to = Eval::load_v_arg(&ir, v2);

    IR::Emit::Div(ir.current_bytecode(), div);
  }

  Eval::RuntimeValue v3 = Eval::as_direct(ir.new_temporary(u64_t, {}), u64_t);
  {
    u8 c0_data[8] = {};
    x64_to_bytes(1llu, c0_data);

    Eval::RuntimeValue c0 = Eval::as_constant(c0_data, u64_t);

    IR::Types::Sub sub = {};
    sub.left = Eval::load_v_arg(&ir, c0);
    sub.right = Eval::load_v_arg(&ir, v1);
    sub.to = Eval::load_v_arg(&ir, v2);

    IR::Emit::Sub(ir.current_bytecode(), sub);
  }

  Eval::RuntimeValue v4 = Eval::as_direct(ir.new_temporary(u64_t, {}), u64_t);
  {
    u8 c0_data[8] = {};
    x64_to_bytes(1llu, c0_data);

    Eval::RuntimeValue c0 = Eval::as_constant(c0_data, u64_t);

    IR::Types::Mod mod = {};
    mod.left = Eval::load_v_arg(&ir, c0);
    mod.right = Eval::load_v_arg(&ir, v3);
    mod.to = Eval::load_v_arg(&ir, v4);

    IR::Emit::Mod(ir.current_bytecode(), mod);
  }

  ir.start_control_block(end_cf);
  ir.set_current_cf(IR::CFEnd{ start_cf });

  {
    VM::Env env = {};


    auto sf = VM::new_stack_frame(&ir);
    VM::exec()
  }
}
#endif