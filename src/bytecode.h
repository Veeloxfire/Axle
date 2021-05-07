#pragma once
#include "utility.h"
#include <stdio.h>

struct REGISTER_CONSTANT { uint8_t REG; const char* name; };

#define REGISTER_LOAD(NAME, VAL) inline constexpr REGISTER_CONSTANT NAME = { VAL, #NAME }

REGISTER_LOAD(RAX, 0);
REGISTER_LOAD(RCX, 1);
REGISTER_LOAD(RDX, 2);
REGISTER_LOAD(RBX, 3);
REGISTER_LOAD(RSP, 4);
REGISTER_LOAD(RBP, 5);
REGISTER_LOAD(RSI, 6);
REGISTER_LOAD(RDI, 7);
REGISTER_LOAD(R8, 8);
REGISTER_LOAD(R9, 9);
REGISTER_LOAD(R10, 10);
REGISTER_LOAD(R11, 11);
REGISTER_LOAD(R12, 12);
REGISTER_LOAD(R13, 13);
REGISTER_LOAD(R14, 14);

#undef REGISTER_LOAD

struct System;

struct MemComplex {
  uint8_t base;
  uint8_t index;
  uint8_t scale;//must be power of 2
  int32_t disp;

  static constexpr size_t SERIAL_SIZE = 7;

  static constexpr MemComplex parse(const uint8_t* ptr) {
    MemComplex mem ={};

    mem.base = ptr[0];
    mem.index = ptr[1];
    mem.scale = ptr[2];
    mem.disp = x32_from_bytes(ptr + 3);

    return mem;
  }

  constexpr static void write(uint8_t* const ptr, uint8_t base, uint8_t index, uint8_t scale, int32_t disp) {
    ptr[0] = base;
    ptr[1] = index;
    ptr[2] = scale;
    x32_to_bytes(disp, ptr);
  }

  constexpr static void write(uint8_t* const ptr, const MemComplex& mem) {
    ptr[0] = mem.base;
    ptr[1] = mem.index;
    ptr[2] = mem.scale;
    x32_to_bytes(mem.disp, ptr);
  }

  inline static void emit(Array<uint8_t>& arr, uint8_t base, uint8_t index, uint8_t scale, int32_t disp) {
    arr.reserve_extra(SERIAL_SIZE);

    write(arr.data + arr.size, base, index, scale, disp);

    arr.size += SERIAL_SIZE;
  }
};

#define BYTECODES_X \
X(RETURN, OP)\
X(CALL, OP_64)\
X(LABEL, OP_64)\
X(PUSH_R64, OP_R)\
X(POP_TO_R64, OP_R)\
X(PUSH_FRAME, OP)\
X(POP_FRAME, OP)\
X(ALLOCATE_STACK, OP_64)\
X(COPY_R64_TO_STACK, OP_R_64)\
X(COPY_R64_FROM_STACK, OP_R_64)\
X(COPY_R64_FROM_MEM, OP_R_R)\
X(COPY_R64_FROM_MEM_COMPLEX, OP_R_MEM)\
X(COPY_64_TO_STACK, OP_64_64)\
X(COPY_64_TO_STACK_TOP, OP_64_64)\
X(COPY_8_TO_STACK, OP_8_64)\
X(COPY_R8_TO_STACK, OP_R_64)\
X(COPY_R8_FROM_STACK, OP_R_64)\
X(COPY_R64_TO_STACK_TOP, OP_R_64)\
X(COPY_R64_TO_R64, OP_R_R)\
X(SET_R64_TO_64, OP_R_64)\
X(LOAD_ADDRESS, OP_R_R_R)\
X(ADD_R64S, OP_R_R)\
X(SUB_R64S, OP_R_R)\
X(MUL_R64S, OP_R_R)\
X(DIV_RU64S, OP_R_R)\
X(DIV_RI64S, OP_R_R)\
X(EQ_R64S, OP_R_R)\
X(OR_R64S, OP_R_R)\
X(AND_R64S, OP_R_R)\
X(CONV_RU8_TO_R64, OP_R)\
X(CONV_RI8_TO_R64, OP_R)\
X(RESERVE, OP_R)\
X(NEG_R64, OP_R)\
X(JUMP_TO_FIXED, OP_64)\
X(JUMP_TO_FIXED_IF_VAL_ZERO, OP_R_64)\
X(JUMP_TO_FIXED_IF_VAL_NOT_ZERO, OP_R_64)


//      REMOVED
// X(CMP_R64S, OP_R_R)
// X(CMP_64_TO_R64, OP_R_64)

namespace ByteCode {

  enum ByteCodeOp : uint8_t {
  #define X(NAME, structure) NAME,
    BYTECODES_X
  #undef X
  };

  using REG_NAME = FUNCTION_PTR<const char*, uint8_t>;

  void print_bytecode(REG_NAME reg_name_from_num,
                      FILE* const stream,
                      const uint8_t* bytecode,
                      uint64_t size);

  void log_bytecode(REG_NAME reg_name_from_num, const uint8_t* bytecode, uint64_t size);
  void error_bytecode(REG_NAME reg_name_from_num, const uint8_t* bytecode, uint64_t size);

  struct OP {
    uint8_t op;

    static constexpr OP parse(const uint8_t* bytecode) {
      return { bytecode[0] };
    }

    static constexpr size_t INSTRUCTION_SIZE = 1;

    inline static void write(uint8_t* ptr, uint8_t op) {
      *ptr = op;
    }

    inline static void emit(Array<uint8_t>& arr, uint8_t op) {
      arr.insert(op);
    }
  };

  struct OP_R_R {
    uint8_t op;
    uint8_t val1;
    uint8_t val2;

    static constexpr OP_R_R parse(const uint8_t* bytecode) {
      OP_R_R ret ={};

      ret.op = bytecode[0];
      ret.val1 = bytecode[1];
      ret.val2 = bytecode[2];

      return ret;
    }

    static constexpr size_t INSTRUCTION_SIZE = 1 + 1 + 1;

    constexpr static void write(uint8_t* ptr, uint8_t op, uint8_t val1, uint8_t val2) {
      ptr[0] = op;
      ptr[1] = val1;
      ptr[2] = val2;
    }

    static void emit(Array<uint8_t>& arr, uint8_t op, uint8_t val1, uint8_t val2) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      write(arr.data + arr.size, op, val1, val2);

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_R_R_R {
    uint8_t op;
    uint8_t val1;
    uint8_t val2;
    uint8_t val3;

    static constexpr OP_R_R_R parse(const uint8_t* bytecode) {
      OP_R_R_R ret ={};

      ret.op = bytecode[0];
      ret.val1 = bytecode[1];
      ret.val2 = bytecode[2];
      ret.val3 = bytecode[3];

      return ret;
    }

    static constexpr size_t INSTRUCTION_SIZE = 1 + 1 + 1 + 1;

    constexpr static void write(uint8_t* ptr, uint8_t op, uint8_t val1, uint8_t val2, uint8_t val3) {
      ptr[0] = op;
      ptr[1] = val1;
      ptr[2] = val2;
      ptr[3] = val3;
    }

    static void emit(Array<uint8_t>& arr, uint8_t op, uint8_t val1, uint8_t val2, uint8_t val3) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      write(arr.data + arr.size, op, val1, val2, val3);

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_R {
    uint8_t op;
    uint8_t val;

    static constexpr OP_R parse(const uint8_t* bytecode) {
      OP_R ret ={};

      ret.op = bytecode[0];
      ret.val = bytecode[1];

      return ret;
    }

    constexpr static void write(uint8_t* ptr, uint8_t op, uint8_t val) {
      ptr[0] = op;
      ptr[1] = val;
    }

    static constexpr size_t INSTRUCTION_SIZE = 1 + 1;
    inline static void emit(Array<uint8_t>& arr, uint8_t opcode, uint8_t val) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      write(arr.data + arr.size, opcode, val);

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_64 {
    uint8_t op;
    X64_UNION u64;

    static constexpr OP_64 parse(const uint8_t* bytecode) {
      OP_64 ret ={};

      ret.op = bytecode[0];
      ret.u64 = x64_from_bytes(bytecode + 1);

      return ret;
    }

    static constexpr size_t INSTRUCTION_SIZE = 1 + 8;

    constexpr static void write(uint8_t* ptr, uint8_t op, X64_UNION x64) {
      ptr[0] = op;
      x64_to_bytes(x64, ptr + 1);
    }

    inline static void emit(Array<uint8_t>& arr, uint8_t opcode, X64_UNION x64) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      write(arr.data + arr.size, opcode, std::move(x64));

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_R_64 {
    uint8_t op;
    uint8_t val;
    X64_UNION u64;

    static constexpr OP_R_64 parse(const uint8_t* bytecode) {
      OP_R_64 ret ={};

      ret.op = bytecode[0];
      ret.val = bytecode[1];
      ret.u64 = x64_from_bytes(bytecode + 2);

      return ret;
    }

    static constexpr size_t INSTRUCTION_SIZE = 1 + 1 + 8;

    constexpr static void write(uint8_t* ptr, uint8_t op, uint8_t val, X64_UNION x64) {
      ptr[0] = op;
      ptr[1] = val;
      x64_to_bytes(x64, ptr + 2);
    }

    inline static void emit(Array<uint8_t>& arr, uint8_t opcode, uint8_t val, X64_UNION x64) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      write(arr.data + arr.size, opcode, val, std::move(x64));

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_R_MEM {
    uint8_t op;
    uint8_t val;
    MemComplex mem;

    static constexpr OP_R_MEM parse(const uint8_t* bytecode) {
      OP_R_MEM ret ={};

      ret.op = bytecode[0];
      ret.val = bytecode[1];
      ret.mem = MemComplex::parse(bytecode + 2);

      return ret;
    }

    static constexpr size_t INSTRUCTION_SIZE = 1 + 1 + MemComplex::SERIAL_SIZE;

    constexpr static void write(uint8_t* ptr, uint8_t op, uint8_t val, const MemComplex& mem) {
      ptr[0] = op;
      ptr[1] = val;
      MemComplex::write(ptr + 2, mem);
    }

    inline static void emit(Array<uint8_t>& arr, uint8_t opcode, uint8_t val, const MemComplex& mem) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      write(arr.data + arr.size, opcode, val, mem);

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_8_64 {
    uint8_t op;
    uint8_t u8;
    X64_UNION u64;

    static constexpr OP_8_64 parse(const uint8_t* bytecode) {
      OP_8_64 ret ={};

      ret.op = bytecode[0];
      ret.u8 = bytecode[1];
      ret.u64 = x64_from_bytes(bytecode + 2);

      return ret;
    }

    static constexpr size_t INSTRUCTION_SIZE = 1 + 1 + 8;

    constexpr static void write(uint8_t* ptr, uint8_t op, uint8_t u8, X64_UNION x64) {
      ptr[0] = op;
      ptr[1] = u8;
      x64_to_bytes(x64, ptr + 2);
    }

    inline static void emit(Array<uint8_t>& arr, uint8_t opcode, uint8_t u8, X64_UNION x64) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      write(arr.data + arr.size, opcode, u8, std::move(x64));

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_64_64 {
    uint8_t op;
    X64_UNION u64_1;
    X64_UNION u64_2;

    static constexpr OP_64_64 parse(const uint8_t* bytecode) {
      OP_64_64 ret = {};

      ret.op = bytecode[0];
      ret.u64_1 = x64_from_bytes(bytecode + 1);
      ret.u64_2 = x64_from_bytes(bytecode + 1 + 8);

      return ret;
    }

    static constexpr size_t INSTRUCTION_SIZE = 1 + 8 + 8;

    constexpr static void write(uint8_t* ptr, uint8_t op, X64_UNION x64_1, X64_UNION x64_2) {
      ptr[0] = op;
      x64_to_bytes(x64_1, ptr + 1);
      x64_to_bytes(x64_2, ptr + 1 + 8);
    }

    inline static void emit(Array<uint8_t>& arr, uint8_t opcode, X64_UNION x64_1, X64_UNION x64_2) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      write(arr.data + arr.size, opcode,  std::move(x64_1), std::move(x64_2));

      arr.size += INSTRUCTION_SIZE;
    }
  };

  namespace EMIT {
  #define OP_64_64(name) inline void name (Array<uint8_t>& arr, X64_UNION x64_1, X64_UNION x64_2) {\
    ByteCode::OP_64_64::emit(arr, ByteCode:: ## name, std::move(x64_1), std::move(x64_2));\
    }

  #define OP_R_64(name) inline void name (Array<uint8_t>& arr, uint8_t val, X64_UNION x64) {\
    ByteCode::OP_R_64::emit(arr, ByteCode:: ## name, val, x64);\
    }

  #define OP_8_64(name) inline void name (Array<uint8_t>& arr, uint8_t u8, X64_UNION x64) {\
    ByteCode::OP_8_64::emit(arr, ByteCode:: ## name, u8, x64);\
    }

  #define OP_64(name) inline void name (Array<uint8_t>& arr, X64_UNION x64) {\
    ByteCode::OP_64::emit(arr, ByteCode:: ## name, x64);\
    }

  #define OP(name) inline void name (Array<uint8_t>& arr) {\
    ByteCode::OP::emit(arr, ByteCode:: ## name);\
    }
    
  #define OP_R(name) inline void name (Array<uint8_t>& arr, uint8_t val) {\
    ByteCode::OP_R::emit(arr, ByteCode:: ## name, val);\
    }

  #define OP_R_R(name) inline void name (Array<uint8_t>& arr, uint8_t val1, uint8_t val2) {\
    ByteCode::OP_R_R::emit(arr, ByteCode:: ## name, val1, val2);\
    }

  #define OP_R_MEM(name) inline void name (Array<uint8_t>& arr, uint8_t val, const MemComplex& mem) {\
    ByteCode::OP_R_MEM::emit(arr, ByteCode:: ## name, val, mem);\
    }
  
  #define OP_R_R_R(name) inline void name (Array<uint8_t>& arr, uint8_t val1, uint8_t val2, uint8_t val3) {\
    ByteCode::OP_R_R_R::emit(arr, ByteCode:: ## name, val1, val2, val3);\
    }

  #define X(name, structure) structure(name)
    BYTECODES_X
    #undef X

    #undef OP_64_64
    #undef OP_R_64
    #undef OP_R_MEM
    #undef OP_8_64
    #undef OP_64
    #undef OP
    #undef OP_R
    #undef OP_R_R
    #undef OP_R_R_R
  }

  namespace PARSE {
  #define X(name, structure) inline constexpr auto name = structure ## ::parse; 
    BYTECODES_X
    #undef X
  }

  namespace SIZE_OF {
  #define X(name, structure) inline constexpr auto name = structure ## ::INSTRUCTION_SIZE; 
    BYTECODES_X
    #undef X
  }

  constexpr size_t instruction_size(uint8_t op) {
  #define X(name, s) case name: return SIZE_OF:: ## name;
    switch ((ByteCodeOp)op) {
      BYTECODES_X
    }
  #undef X

    return -1;
  }

  namespace WRITE {
  #define OP_64_64(name) inline void name (uint8_t* ptr, X64_UNION x64_1, X64_UNION x64_2) {\
    ByteCode::OP_64_64::write(ptr, ByteCode:: ## name, std::move(x64_1), std::move(x64_2));\
    }

  #define OP_R_64(name) inline void name (uint8_t* ptr, uint8_t val, X64_UNION x64) {\
    ByteCode::OP_R_64::write(ptr, ByteCode:: ## name, val, x64);\
    }

  #define OP_8_64(name) inline void name (uint8_t* ptr, uint8_t u8, X64_UNION x64) {\
    ByteCode::OP_8_64::write(ptr, ByteCode:: ## name, u8, x64);\
    }

  #define OP_64(name) inline void name (uint8_t* ptr, X64_UNION x64) {\
    ByteCode::OP_64::write(ptr, ByteCode:: ## name, x64);\
    }

  #define OP(name) inline void name (uint8_t* ptr) {\
    ByteCode::OP::write(ptr, ByteCode:: ## name);\
    }

  #define OP_R(name) inline void name (uint8_t* ptr, uint8_t val) {\
    ByteCode::OP_R::write(ptr, ByteCode:: ## name, val);\
    }

  #define OP_R_R(name) inline void name (uint8_t* ptr, uint8_t val1, uint8_t val2) {\
    ByteCode::OP_R_R::write(ptr, ByteCode:: ## name, val1, val2);\
    }
    
  #define OP_R_MEM(name) inline void name (uint8_t* ptr, uint8_t val, const MemComplex& mem) {\
    ByteCode::OP_R_MEM::write(ptr, ByteCode:: ## name, val, mem);\
    }

   #define OP_R_R_R(name) inline void name (uint8_t* ptr, uint8_t val1, uint8_t val2, uint8_t val3) {\
    ByteCode::OP_R_R_R::write(ptr, ByteCode:: ## name, val1, val2, val3);\
    }

  #define X(name, structure) structure(name)
    BYTECODES_X
    #undef X

    #undef OP_64_64
    #undef OP_R_64
    #undef OP_R_MEM
    #undef OP_8_64
    #undef OP_64
    #undef OP
    #undef OP_R
    #undef OP_R_R
    #undef OP_R_R_R
  }
}

struct CodeBlock {
  uint64_t label = 0;
  Array<uint8_t> code ={};
};