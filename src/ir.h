#pragma once

#include "safe_lib.h"
#include "utility.h"
#include "memory.h"
#include "compiler.h"

namespace IR {
  enum struct Op : u8 {
    Nul = 0,
    Create,
    Copy,
    Add,
    Sub,
    Mul,
    Div,
    Call,
    Return,
    Jump,
  };

  struct ValueIndex {
    constexpr static usize INVALID_INDEX = static_cast<usize>(-1);

    usize index = 0;

    constexpr ValueIndex() = default;

    constexpr operator usize() const {
      return index;
    }
  };

  constexpr ValueIndex INVALID_VALUE = ValueIndex{ ValueIndex::INVALID_INDEX };

  struct FunctionIndex {
    usize index = 0;

    constexpr FunctionIndex() = default;

    constexpr operator usize() const {
      return index;
    }
  };

  struct NulInst {
    constexpr static Op op = Op::Nul;
  };

  struct CreateInst {
    constexpr static Op op = Op::Create;

    ValueIndex dest;
  };

  struct CopyInst {
    constexpr static Op op = Op::Copy;

    ValueIndex dest;
    ValueIndex source;
  };

  struct AddInst {
    constexpr static Op op = Op::Add;

    ValueIndex dest;
    ValueIndex left;
    ValueIndex right;
  };

  struct SubInst {
    constexpr static Op op = Op::Sub;

    ValueIndex dest;
    ValueIndex left;
    ValueIndex right;
  };

  struct MulInst {
    constexpr static Op op = Op::Mul;

    ValueIndex dest;
    ValueIndex left;
    ValueIndex right;
  };

  struct DivInst {
    constexpr static Op op = Op::Div;

    ValueIndex dest;
    ValueIndex left;
    ValueIndex right;
  };

  struct CallInst {
    constexpr static Op op = Op::Call;

    FunctionIndex func;
    usize n_params;
    usize n_results;
  };

  struct JumpInst {
    constexpr static Op op = Op::Jump;

    usize jump_to;
  };

  struct ReturnInst {
    constexpr static Op op = Op::Return;
  };

  struct Instruction {
    Op op;
    union {
      NulInst nul;
      CreateInst create;
      CopyInst copy;
      AddInst add;
      SubInst sub;
      MulInst mul;
      DivInst div;
      CallInst call;
      ReturnInst ret;
      JumpInst jump;
    };

    constexpr Instruction() : Instruction(NulInst{}) {}

    constexpr Instruction(const NulInst& n) : op(NulInst::op), nul(n) {}
    constexpr Instruction(const CreateInst& c) : op(CreateInst::op), create(c) {}
    constexpr Instruction(const CopyInst& c) : op(CopyInst::op), copy(c) {}
    constexpr Instruction(const AddInst& a) : op(AddInst::op), add(a) {}
    constexpr Instruction(const SubInst& s) : op(SubInst::op), sub(s) {}
    constexpr Instruction(const MulInst& m) : op(MulInst::op), mul(m) {}
    constexpr Instruction(const DivInst& d) : op(DivInst::op), div(d) {}
    constexpr Instruction(const CallInst& c) : op(CallInst::op), call(c) {}
    constexpr Instruction(const ReturnInst& r) : op(ReturnInst::op), ret(r) {}
    constexpr Instruction(const JumpInst& j) : op(JumpInst::op), jump(j) {}
  };

  enum struct ValueSource : u8 {
    Invalid = 0,
    Temporary,
    Variable,
    Reference,
    Constant,
  };

  enum struct Primitive : u8 {
    Invalid = 0,
    Raw,//Could be any type
    Int,
  };

  constexpr const char* primitive_string(Primitive p) {
    switch (p) {
      case Primitive::Raw: return "Raw";
      case Primitive::Int: return "Int";
      case Primitive::Invalid: return "Invalid";
    }

    return "unknown-primitive-type";
  }

  struct Value {
    ValueSource source;
    Primitive primitive;
    usize size;
    usize index;
  };

  struct Constant {
    u8* data = nullptr;
  };

  struct Variable {
  };

  struct Temporary {
    u32 compressed_temporary_index;
  };

  struct Reference {
    ValueIndex source;
    ValueIndex offset;
  };

  struct Function {
    Array<Reference> references = {};

    GrowingMemoryPool<1024 * 8> constants_pool = {};
    Array<Constant> constants = {};

    u32 max_temporaries = 0;
    u32 temporaries_counter = 0;
    Array<Temporary> temporaries = {};
   
    Array<Variable> variables = {};
    
    Array<Value> values = {};
    
    Array<Instruction> instructions = {};

    ValueIndex emit_create_variable(Primitive primitive, usize size);
    ValueIndex emit_create_temporary(Primitive primitive, usize size);
    ValueIndex create_constant(Primitive primitive, usize size, const void* data);
    ValueIndex create_reference(Primitive primitive, usize size, ValueIndex ref, ValueIndex offset = INVALID_VALUE);

    void emit_add(ValueIndex dest, ValueIndex left, ValueIndex right);
    void emit_sub(ValueIndex dest, ValueIndex left, ValueIndex right);
    void emit_mul(ValueIndex dest, ValueIndex left, ValueIndex right);
    void emit_div(ValueIndex dest, ValueIndex left, ValueIndex right);
    void emit_copy(ValueIndex dest, ValueIndex source);
    void emit_return();
  };

  struct Program {
    Array<Variable> globals = {};
    Array<Function> functions = {};
  };

  struct ExecuionThread {
    struct RuntimeVariable {
      usize size = 0;

      //Store in u64 if value is small, else need to allocate
      union {
        u64 small = 0;
        u8* large;
      };

      ~RuntimeVariable() {
        if (size > 8llu) {
          delete[] large;
        }
      }
    };

    struct RuntimeImmediate {
      //usize size = 0; - size determined by the passed in type (and verified in evals)

      //Store in u64 if value is small, else need to allocate
      union {
        u64 small = 0;
        u8* large;
      };

      RuntimeImmediate() = default;

      RuntimeImmediate(const RuntimeVariable& v) {
        if (v.size > 8) {
          large = v.large;
        }
        else {
          small = v.small;
        }
      }
    };

    struct RuntimeReference {
      RuntimeVariable* ref_to;
      usize offset;
    };

    Errors* errors;
    IR::Function* func;

    RuntimeVariable* temporaries = nullptr;
    RuntimeVariable* variables = nullptr;


    ExecuionThread(Errors* errors_, IR::Function* func_)
      : errors(errors_), func(func_),
      temporaries(new RuntimeVariable[func_->max_temporaries]),
      variables(new RuntimeVariable[func_->variables.size])
    {}

    ExecuionThread(CompilerThread* comp_thread, IR::Function* func_)
      : ExecuionThread(&comp_thread->errors, func_)
    {}

    RuntimeImmediate eval_immediate(const IR::Value& val);
    RuntimeReference eval_reference(const IR::Value& val);
    void execute();

    ~ExecuionThread() {
      delete[] temporaries;
      delete[] variables;
    }
  };
}



