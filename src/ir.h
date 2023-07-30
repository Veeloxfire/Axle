#pragma once
#include "utility.h"
#include "comp_utilities.h"
#include "type.h"

namespace IR {
  struct ValueIndex {
    static constexpr u32 TEMP_MASK = 0b1 << 31;

    static constexpr u32 INDEX_MASK = ~TEMP_MASK;

    u32 encoding;

    constexpr bool is_variable() const {
      return (encoding & TEMP_MASK) == 0;
    }

    constexpr bool is_temporary() const {
      return (encoding & TEMP_MASK) == TEMP_MASK;
    }

    constexpr u32 index() const {
      return (encoding & INDEX_MASK);
    }
  };

  constexpr bool operator==(const ValueIndex& a, const ValueIndex& b) {
    return a.encoding == b.encoding;
  }

  struct Variable {
    Type type = {};
  };

  enum struct Indirection {
    None, Reference, Dereference
  };

  struct Temporary {
    Indirection indirection = Indirection::None;
    ValueIndex refers_to = {};//only used if some form of indirection
    u32 refers_to_offset = 0;
    Type type = {};
  };

  struct ExpressionFrame {
    u32 temporary_start = 0;
    u32 temporary_count = 0;

    u32 bytecode_start = 0;
    u32 bytecode_count = 0;
  };

  struct RuntimeReference {
    bool is_constant = false;
    u32 offset = 0;
    Type type = {};

    union {
      ValueIndex base = {};
      u8* constant;
    };
  };

  struct ControlBlock {
    usize start = 0;
    usize size = 0;
  };

  struct GlobalReference {
    Type type = {};
    usize data_member = 0;
  };

  struct DynLibraryImport {
    GlobalLabel label = { 0 };
    const InternString* path = nullptr;
    const InternString* name = nullptr;
  };

  struct Builder {
    bool comptime_compilation = false;
    bool require_variable_intermediates = false;

    IR::GlobalLabel global_label = { 0 };

    Array<Temporary> temporaries = {};
    Array<Variable> variables = {};
    Array<GlobalReference> globals_used = {};

    LocalLabel current_block = { 0 };

    Array<ControlBlock> control_blocks = {};
    Array<ExpressionFrame> expression_frames = {};
    Array<u8> ir_bytecode = {};

    const SignatureStructure* signature = nullptr;

    RuntimeReference new_temporary(const Type& t);
    ValueIndex new_variable(const Type& t);

    u32 new_global_reference(const IR::GlobalReference& ref);
    LocalLabel new_control_block();

    void start_control_block(LocalLabel label);
    void end_control_block();

    void start_expression();
    void end_expression();
  };

  namespace HELPERS {
    RuntimeReference no_value();
    RuntimeReference as_reference(ValueIndex val, const Type& type);
    RuntimeReference as_constant(void* constant, const Type& type);
    RuntimeReference sub_object(const RuntimeReference& val, usize offset, const Type& type);

    RuntimeReference copy_constant(Builder* ir, const void* constant, const Type& type);
    void copycast_value(IR::Builder* ir,
                        const IR::RuntimeReference& to, IR::RuntimeReference from);

    RuntimeReference take_address(Builder* const ir, const RuntimeReference& val, Type ptr_type);
    RuntimeReference dereference(Builder* const ir, const RuntimeReference& val, Type deref_type);

    RuntimeReference arr_to_ptr(Builder* const ir, const RuntimeReference& val, Type ptr_type);
  }

  struct FunctionSignature {
    const ASTFuncSig* declaration = nullptr;
    const SignatureStructure* sig_struct = nullptr;

    const InternString* name = {};

    IR::GlobalLabel label = { 0 };
  };

  struct Function {
    const ASTLambda* declaration = nullptr;

    FunctionSignature signature = {};
    UnitID sig_unit_id= 0;
  };

#define OPCODES_MOD \
MOD(Set, CODE_V_C)\
MOD(CopyCast, CODE_V_V)\
MOD(GlobalAddress, CODE_V_IM32)\
MOD(StartFunc, CODE_EMPTY)\
MOD(Call, CODE_GL_NV)\
MOD(Return, CODE_V)\
MOD(IfSplit, CODE_V_LL)\
MOD(Jump, CODE_L)\
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

  constexpr const char* opcode_string(OpCode c) {
    switch (c) {
#define MOD(n, ...) case OpCode:: n: return #n;
      OPCODES_MOD;
#undef MOD
    }

    return nullptr;
  }

  struct CODE_IM32 {
    u32 im32;

    static constexpr usize serialize_size() {
      return sizeof(im32);
    }
  };


  struct CODE_V {
    ValueIndex val;
    u32 offset;
    Format format;

    static constexpr usize serialize_size() {
      return sizeof(val) + sizeof(offset) + sizeof(format);
    }
  };

  struct CODE_V_IM32 {
    ValueIndex val;
    u32 offset;
    Format format;
    u32 im32;

    static constexpr usize serialize_size() {
      return sizeof(val) + sizeof(offset) + sizeof(format) + sizeof(im32);
    }
  };

  //Op value constant
  struct CODE_V_C {
    ValueIndex to;
    u32 t_offset;
    Format t_format;
    u32 d_size;
    const u8* data;

    static constexpr usize static_serialize_size() {
      return sizeof(to) + sizeof(t_offset) + sizeof(t_format) + sizeof(d_size);
    }

    constexpr usize serialize_size() const {
      return static_serialize_size() + d_size;
    }
  };

  //Op value value
  struct CODE_V_V {
    ValueIndex to;
    u32 t_offset;
    Format t_format;
    ValueIndex from;
    u32 f_offset;
    Format f_format;

    static constexpr usize serialize_size() {
      return sizeof(to) + sizeof(t_offset) + sizeof(t_format)
        + sizeof(from) + sizeof(f_offset) + sizeof(f_format);
    }
  };

  //Op value value value
  struct CODE_V_V_V {
    ValueIndex to;
    u32 t_offset;
    Format t_format;
    ValueIndex left;
    u32 l_offset;
    Format l_format;
    ValueIndex right;
    u32 r_offset;
    Format r_format;

    static constexpr usize serialize_size() {
      return sizeof(to) + sizeof(t_offset) + sizeof(t_format)
        + sizeof(left) + sizeof(l_offset) + sizeof(l_format)
        + sizeof(right) + sizeof(l_offset) + sizeof(r_format);
    }
  };

  struct SingleVal {
    ValueIndex v;
    u32 v_offset;
    Format v_format;

    static constexpr usize serialize_size() {
      return sizeof(v) + sizeof(v_offset) + sizeof(v_format);
    }
  };

  //Op global_label n*values
  struct CODE_GL_NV {
    IR::GlobalLabel label;
    u32 n_values;
    const u8* values;

    static constexpr usize static_serialize_size() {
      return sizeof(label) + sizeof(n_values);
    }

    constexpr usize serialize_size() const {
      return static_serialize_size() + (usize)n_values * SingleVal::serialize_size();
    }
  };

  struct CODE_V_LL {
    ValueIndex val;
    u32 offset;
    Format format;
    LocalLabel label_if;
    LocalLabel label_else;

    static constexpr usize serialize_size() {
      return sizeof(val) + sizeof(offset) + sizeof(format)
        + sizeof(label_if) + sizeof(label_else);
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
  usize serialize(u8* data, usize remaining_size, const CODE_IM32& i);
  usize serialize(u8* data, usize remaining_size, const CODE_V& i);
  usize serialize(u8* data, usize remaining_size, const CODE_V_IM32& i);
  usize serialize(u8* data, usize remaining_size, const CODE_V_C& i);
  usize serialize(u8* data, usize remaining_size, const CODE_V_V& i);
  usize serialize(u8* data, usize remaining_size, const CODE_V_V_V& i);
  usize serialize(u8* data, usize remaining_size, const CODE_GL_NV& i);
  usize serialize(u8* data, usize remaining_size, const CODE_V_LL& i);
  usize serialize(u8* data, usize remaining_size, const CODE_L& i);

  constexpr usize deserialize(const u8*, usize, CODE_EMPTY&) { return 0; }
  usize deserialize(const u8* data, usize remaining_size, CODE_IM32& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_V& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_V_IM32& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_V_C& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_V_V& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_V_V_V& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_V_LL& i);
  usize deserialize(const u8* data, usize remaining_size, CODE_L& i);

  usize deserialize(const u8* data, usize remaining_size, CODE_GL_NV& i);
  usize deserialize(const u8* data, usize remaining_size, SingleVal& v);

  void print_ir(const IR::Builder* builder);

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

namespace CASTS {
  using CAST_FUNCTION = IR::RuntimeReference(*) (IR::Builder* const builder,
                                                 const Type& from, const Type& to,
                                                 const IR::RuntimeReference& val);

  IR::RuntimeReference int_to_int(IR::Builder* const builder,
                                  const Type& from, const Type& to,
                                  const IR::RuntimeReference& val);

  IR::RuntimeReference no_op(IR::Builder* const builder,
                             const Type& from, const Type& to,
                             const IR::RuntimeReference& val);

  IR::RuntimeReference take_address(IR::Builder* const builder,
                                    const Type& from, const Type& to,
                                    const IR::RuntimeReference& val);
}

void vm_run(Errors* errors, const IR::Builder* builder) noexcept;