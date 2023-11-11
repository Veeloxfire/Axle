#pragma once
#include <AxleUtil/utility.h>
#include "type.h"

namespace IR {
  struct EvalPromise {
    u8* data;
    Type type;
  };

  void eval_ast(CompilerGlobals* comp, CompilerThread* comp_thread, AST_LOCAL root, EvalPromise* eval);

  struct ValueIndex {
    u32 index;
  };

  constexpr bool operator==(const ValueIndex& a, const ValueIndex& b) {
    return a.index == b.index;
  }

  constexpr bool operator!=(const ValueIndex& a, const ValueIndex& b) {
    return a.index != b.index;
  }

  struct VariableId {
    u32 variable;
  };

  constexpr bool operator==(const VariableId& a, const VariableId& b) {
    return (a.variable == b.variable);
  }

  constexpr bool operator!=(const VariableId& a, const VariableId& b) {
    return (a.variable != b.variable);
  }

  struct SSATemp {
    ValueRequirements requirements;
    Type type = {};

    bool is_variable;
    VariableId var_id;
  };

  struct SSAVar {
    ValueRequirements requirements;
    Type type = {};

    u32 stack_offset;
  };

  enum struct ControlFlowType {
    Start = 0,
    End,
    Return,
    Inline,
    Split,
    Merge,
  };

  struct CFStart {
    static constexpr ControlFlowType CF_TYPE = ControlFlowType::Start;
    LocalLabel child = IR::NULL_LOCAL_LABEL;
  };

  struct CFEnd {
    static constexpr ControlFlowType CF_TYPE = ControlFlowType::End;
    LocalLabel parent = IR::NULL_LOCAL_LABEL;
  };

  struct CFReturn {
    static constexpr ControlFlowType CF_TYPE = ControlFlowType::Return;
    LocalLabel parent = IR::NULL_LOCAL_LABEL;
    ValueIndex val;
  };

  struct CFInline {
    static constexpr ControlFlowType CF_TYPE = ControlFlowType::Inline;
    LocalLabel parent = IR::NULL_LOCAL_LABEL;
    LocalLabel child = IR::NULL_LOCAL_LABEL;
  };

  struct CFSplt {
    static constexpr ControlFlowType CF_TYPE = ControlFlowType::Split;
    LocalLabel parent;

    ValueIndex condition;
    LocalLabel true_branch;
    LocalLabel false_branch;
  };

  struct CFMerge {
    static constexpr ControlFlowType CF_TYPE = ControlFlowType::Merge;
    LocalLabel parents[2];
    
    LocalLabel child;
  };

  struct ControlBlock {
    LocalLabel label = NULL_LOCAL_LABEL;
    bool calls = false;

    ControlFlowType cf_type = ControlFlowType::Start;
    union {
      CFStart cf_start = {};
      CFEnd cf_end;
      CFReturn cf_return;
      CFInline cf_inline;
      CFSplt cf_split;
      CFMerge cf_merge;
    };

    Array<u8> bytecode = {};
    Array<SSATemp> temporaries = {};
  };

  struct GlobalReference {
    Type type = {};
    IR::ValueRequirements requirements;
    usize data_member = 0;
  };

  struct DynLibraryImport {
    GlobalLabel label = NULL_GLOBAL_LABEL;
    const InternString* path = nullptr;
    const InternString* name = nullptr;
  };

  struct IRStore {
    const SignatureStructure* signature = nullptr;

    GlobalLabel global_label = NULL_GLOBAL_LABEL;
    LocalLabel current_block = NULL_LOCAL_LABEL;

    Array<SSAVar> variables = {};
    u32 max_stack;

    Array<GlobalReference> globals_used = {};
    Array<LocalLabel> control_flow_labels = {};
    
    Array<ControlBlock> control_blocks = {};

    VariableId new_variable(const Type& t, ValueRequirements requirements);
    ValueIndex new_temporary(const Type& t, ValueRequirements requirements);
    ValueIndex new_temporary(const VariableId& var, ValueRequirements requirements);

    u32 new_global_reference(const IR::GlobalReference& ref);

    LocalLabel new_control_block();
    ControlBlock* current_control_block();

    void start_control_block(LocalLabel label);
    void end_control_block();

    Array<u8>& current_bytecode();

    void set_current_cf(const CFStart&);
    void set_current_cf(const CFEnd&);
    void set_current_cf(const CFReturn&);
    void set_current_cf(const CFInline&);
    void set_current_cf(const CFSplt&);
    void set_current_cf(const CFMerge&);
  };

  struct FunctionSignature {
    const ASTFuncSig* declaration = nullptr;
    const SignatureStructure* sig_struct = nullptr;

    const InternString* name = {};

    IR::GlobalLabel label = IR::NULL_GLOBAL_LABEL;
  };

  struct Function {
    const ASTLambda* declaration = nullptr;

    FunctionSignature signature = {};
    UnitID sig_unit_id = 0;
  };

#define OPCODES_MOD \
MOD(BreakPoint, CODE_EMPTY)\
MOD(Set, CODE_V_C)\
MOD(SetStore, CODE_V_C)\
MOD(Copy, CODE_V_V)\
MOD(CopyLoad, CODE_V_V)\
MOD(CopyStore, CODE_V_V)\
MOD(CopyLoadStore, CODE_V_V)\
MOD(AddrOf, CODE_V_V)\
MOD(AddrOfLoad, CODE_V_V)\
MOD(AddrOfGlobal, CODE_V_IM32)\
MOD(StartFunc, CODE_NV)\
MOD(Call, CODE_GL_NV)\
MOD(Add, CODE_V_V_V)\
MOD(Sub, CODE_V_V_V)\
MOD(Mul, CODE_V_V_V)\
MOD(Div, CODE_V_V_V)\
MOD(Mod, CODE_V_V_V)\
MOD(Eq, CODE_V_V_V)\
MOD(Neq, CODE_V_V_V)\
MOD(Less, CODE_V_V_V)\
MOD(Great, CODE_V_V_V)\
MOD(And, CODE_V_V_V)\
MOD(Or, CODE_V_V_V)\
MOD(Xor, CODE_V_V_V)\
MOD(Neg, CODE_V_V)\
MOD(Not, CODE_V_V)

  enum struct OpCode : u8 {
#define MOD(n, ...) n,
    OPCODES_MOD
#undef MOD
  };

  constexpr ViewArr<const char> opcode_string(OpCode c) {
    switch (c) {
#define MOD(n, ...) case OpCode:: n: return lit_view_arr(#n);
      OPCODES_MOD;
#undef MOD
    }

    return {};
  }

  struct V_ARG {
    ValueIndex val;
    u32 offset;
    u32 size;

    static constexpr usize serialize_size() {
      return sizeof(val) + sizeof(offset) + sizeof(size);
    }
  };

  constexpr V_ARG v_arg(ValueIndex index, u32 offset, const Type& type) {
    return V_ARG{
      index, offset, type.size(),
    };
  }

  struct C_ARG {
    u32 size;
    const u8* val;

    static constexpr usize static_serialize_size() {
      return sizeof(size);
    }
  };

  constexpr C_ARG c_arg(const u8* data, const Type& type) {
    return C_ARG{
      type.size(), data,
    };
  }

  struct CODE_V {
    V_ARG val;

    static constexpr usize serialize_size() {
      return decltype(val)::serialize_size();
    }
  };

  struct CODE_V_IM32 {
    V_ARG val;
    u32 im32;

    static constexpr usize serialize_size() {
      return decltype(val)::serialize_size() + sizeof(im32);
    }
  };

  //Op val val
  struct CODE_V_C {
    V_ARG to;
    C_ARG data;

    static constexpr usize static_serialize_size() {
      return decltype(to)::serialize_size() + C_ARG::static_serialize_size();
    }

    constexpr usize serialize_size() const {
      return static_serialize_size() + data.size;
    }
  };


  //Op val val
  struct CODE_V_V {
    V_ARG to;
    V_ARG from;

    static constexpr usize serialize_size() {
      return decltype(to)::serialize_size() + decltype(from)::serialize_size();
    }
  };

  //Op val val val
  struct CODE_V_V_V {
    V_ARG to;
    V_ARG left;
    V_ARG right;

    static constexpr usize serialize_size() {
      return decltype(to)::serialize_size() 
        + decltype(left)::serialize_size() 
        + decltype(right)::serialize_size();
    }
  };

  //Op n * values
  struct CODE_NV {
    u32 n_values;
    const V_ARG* values;

    static constexpr usize static_serialize_size() {
      return sizeof(n_values);
    }

    constexpr usize serialize_size() const {
      return static_serialize_size() + (usize)n_values * V_ARG::serialize_size();
    }
  };


  //Op global_label n*values
  struct CODE_GL_NV {
    IR::GlobalLabel label;
    u32 n_values;
    const V_ARG* values;

    static constexpr usize static_serialize_size() {
      return sizeof(label) + sizeof(n_values);
    }

    constexpr usize serialize_size() const {
      return static_serialize_size() + (usize)n_values * V_ARG::serialize_size();
    }
  };

  struct CODE_V_LL {
    V_ARG val;
    LocalLabel label_if;
    LocalLabel label_else;

    static constexpr usize serialize_size() {
      return V_ARG::serialize_size() + sizeof(label_if) + sizeof(label_else);
    }
  };

  struct CODE_L {
    LocalLabel local_label;

    static constexpr usize serialize_size() {
      return sizeof(local_label);
    }
  };

  struct CODE_EMPTY {
    static constexpr usize serialize_size() { return 0; }
  };

  constexpr usize serialize(const u8*, usize, const CODE_EMPTY&) { return 0; }
  usize serialize(u8* data, usize remaining_size, const CODE_V& i);
  usize serialize(u8* data, usize remaining_size, const CODE_V_IM32& i);
  usize serialize(u8* data, usize remaining_size, const CODE_V_C& i);
  usize serialize(u8* data, usize remaining_size, const CODE_V_V& i);
  usize serialize(u8* data, usize remaining_size, const CODE_V_V_V& i);
  usize serialize(u8* data, usize remaining_size, const CODE_GL_NV& i);
  usize serialize(u8* data, usize remaining_size, const CODE_NV& i);
  usize serialize(u8* data, usize remaining_size, const CODE_V_LL& i);
  usize serialize(u8* data, usize remaining_size, const CODE_L& i);

  constexpr usize deserialize(const u8*, usize, CODE_EMPTY&) { return 0; }
  usize deserialize(const u8* data, usize remaining_size, CODE_V& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_V_IM32& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_V_C& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_V_V& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_V_V_V& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_V_LL& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_L& i);
  usize deserialize(const u8* data, usize remaining_size, V_ARG& i);

  usize deserialize(const u8* data, usize remaining_size, CODE_GL_NV& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_NV& i);

  void print_ir(const IR::IRStore* builder);

  namespace Types {
#define MOD(n, layout, ...) struct n : layout { static constexpr OpCode OPCODE = OpCode:: n; };
    OPCODES_MOD;
#undef MOD
  }

  template<typename INSTRUCTION>
  using Emitter = void(*)(Array<u8>&, const INSTRUCTION&);

  namespace Emit {
    template<typename INSTRUCTION>
    void impl(Array<u8>& arr, const INSTRUCTION& i) {
      usize curr = arr.size;
      arr.insert_uninit(1 + i.serialize_size());
      arr.data[curr] = static_cast<u8>(INSTRUCTION::OPCODE);
      usize s = serialize(arr.data + curr + 1, arr.size - (curr + 1), i);
      ASSERT(curr + s + 1 == arr.size);
    }

#define MOD(n, ...) inline constexpr Emitter<Types :: n> n = impl<Types :: n>;
    OPCODES_MOD;
#undef MOD
  }

  namespace Read {
    template<typename INSTRUCTION>
    const u8* impl(const u8* arr, const u8* end, INSTRUCTION& i) {
      ASSERT(arr < end);
      ASSERT(static_cast<OpCode>(*arr) == INSTRUCTION::OPCODE);
      arr += 1;
      arr += deserialize(arr, end - arr, i);
      return arr;
    }

#define MOD(n, ...) inline constexpr auto n = impl<Types :: n>;
    OPCODES_MOD;
#undef MOD
  }

#undef OPCODES_MOD
}

struct PrintFuncSignature {
  const IR::Function* func;
};

namespace Format {
  template<>
  struct FormatArg<IR::OpCode> {
    template<Formatter F>
    constexpr static void load_string(F& res, IR::OpCode op) {
      const ViewArr<const char> str = IR::opcode_string(op);
      return res.load_string(str.data, str.size);
    }
  };

  template<>
  struct FormatArg<PrintFuncSignature> {
    template<Formatter F>
    constexpr static void load_string(F& res, PrintFuncSignature p_func) {
      return load_string(res, PrintSignatureType{ p_func.func->signature.sig_struct });
    }
  };
}

namespace Eval {
  struct VariableState {
    IR::VariableId id;

    bool imported;
    IR::ValueIndex current_temp;
  };

  enum struct RVT {
    Constant, Direct, Indirect
  };

  struct RuntimeValue {
    RVT rvt;
    Type type;

    union {
      const u8* constant;
      struct {
        IR::ValueIndex index;
        u32 offset;
      } value;
    };

    constexpr const Type& effective_type() const {
      if (rvt == RVT::Indirect) {
        ASSERT(type.struct_type() == STRUCTURE_TYPE::POINTER);
        return type.unchecked_base<PointerStructure>()->base;
      }
      else {
        return type;
      }
    }
  };

  enum struct Time {
    Runtime, CompileTime,
  };

  struct IrBuilder {
    IR::IRStore* ir;
    Time eval_time;

    IR::LocalLabel parent = IR::NULL_LOCAL_LABEL;

    Array<VariableState> variables_state;
    u32 curr_stack = 0;

    IR::VariableId new_variable(const Type& t, IR::ValueRequirements reqs);
    RuntimeValue import_variable(const IR::VariableId& id, IR::ValueRequirements reqs);

    void switch_control_block(IR::LocalLabel index, IR::LocalLabel parent);

    //Releases the variables that are no longer in scope
    void rescope_variables(usize new_size);
    void reset_variables();

    inline Array<u8>& current_bytecode() const { return ir->current_bytecode(); }
  };

  IrBuilder start_builder(Eval::Time eval_time, IR::IRStore* ir);
  IrBuilder start_builder(Eval::Time eval_time, IR::IRStore* ir, AST_ARR params);
  void end_builder(IrBuilder* builder);

  RuntimeValue sub_object(IR::IRStore* const ir,
                          const RuntimeValue& val,
                          const RuntimeValue& offset,
                          const Type& ptr_type);


  RuntimeValue addrof(IR::IRStore* const ir, const RuntimeValue& val, const Type& ptr_type);
  RuntimeValue deref(IR::IRStore* const ir, const RuntimeValue& val, const Type& ptr_type);

  RuntimeValue no_value();
  RuntimeValue as_direct(IR::ValueIndex val, const Type& type);
  RuntimeValue as_indirect(IR::ValueIndex val, const Type& ptr_type);
  RuntimeValue as_constant(const u8* constant, const Type& type);

  IR::V_ARG load_v_arg(IR::IRStore* ir, const RuntimeValue& rv);

  void assign(IR::IRStore* const ir, const RuntimeValue& to, const RuntimeValue& from);

  RuntimeValue arr_to_ptr(IR::IRStore* const ir, const RuntimeValue& val, const Type& ptr_type);
}

namespace CASTS {
  using CAST_FUNCTION = Eval::RuntimeValue(*) (IR::IRStore* const ir,
                                            const Type& to,
                                            const Eval::RuntimeValue& val);

  Eval::RuntimeValue int_to_int(IR::IRStore* const ir,
                                const Type& to,
                                const Eval::RuntimeValue& val);

  Eval::RuntimeValue no_op(IR::IRStore* const ir,
                           const Type& to,
                           const Eval::RuntimeValue& val);

  Eval::RuntimeValue take_address(IR::IRStore* const ir,
                                  const Type& to,
                                  const Eval::RuntimeValue& val);
}

namespace VM {
  struct Value {
    u32 data_offset = 0;
    Type type = {};
  };

  struct StackFrame {
    OwnedArr<u8> bytes;
    OwnedArr<Value> temporaries = {};
    Value return_val;

    const IR::IRStore* ir;
    const IR::ControlBlock* current_block;
    const u8* IP;
    const u8* IP_END;

    struct RealValue {
      u8* ptr;
      Type t;
    };

    RealValue get_value(const IR::V_ARG& arg);
    RealValue get_indirect_value(const IR::V_ARG& arg);
  };

  struct Env {
    const BuiltinTypes* builtin_types;
    Errors* errors;
  };

  StackFrame new_stack_frame(const IR::IRStore* ir);

  void exec(const Env* env, StackFrame* stack_frame);
}
