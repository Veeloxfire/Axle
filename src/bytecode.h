#pragma once
#include "utility.h"
#include <stdio.h>

#include "bytecode_incs.h"

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
REGISTER_LOAD(R8,  8);
REGISTER_LOAD(R9,  9);
REGISTER_LOAD(R10, 10);
REGISTER_LOAD(R11, 11);
REGISTER_LOAD(R12, 12);
REGISTER_LOAD(R13, 13);
REGISTER_LOAD(R14, 14);

#undef REGISTER_LOAD

struct System;

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
      return {bytecode[0]};
    }

    static constexpr size_t INSTRUCTION_SIZE = 1;

    inline static void write(uint8_t* ptr, uint8_t op) {
      *ptr = op;
    }

    inline static void emit(Array<uint8_t>& arr, uint8_t op) {
      arr.insert(op);
    }
  };

  struct OP_VAL_VAL {
    uint8_t op;
    uint8_t val1;
    uint8_t val2;

    static constexpr OP_VAL_VAL parse(const uint8_t* bytecode) {
      OP_VAL_VAL ret ={};

      ret.op = bytecode[0];
      ret.val1 = bytecode[1];
      ret.val2 = bytecode[2];

      return ret;
    }

    static constexpr size_t INSTRUCTION_SIZE = 1 + 1 + 1;

    inline static void write(uint8_t* ptr, uint8_t op, uint8_t val1, uint8_t val2) {
      ptr[0] = op;
      ptr[1] = val1;
      ptr[2] = val2;
    }

    inline static void emit(Array<uint8_t>& arr, uint8_t op, uint8_t val1, uint8_t val2) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      write(arr.data + arr.size, op, val1, val2);

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_VAL {
    uint8_t op;
    uint8_t val;

    static constexpr OP_VAL parse(const uint8_t* bytecode) {
      OP_VAL ret ={};

      ret.op = bytecode[0];
      ret.val = bytecode[1];

      return ret;
    }

    inline static void write(uint8_t* ptr, uint8_t op, uint8_t val) {
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

    inline static void write(uint8_t* ptr, uint8_t op, X64_UNION x64) {
      ptr[0] = op;
      x64_to_bytes(x64, ptr + 1);
    }

    inline static void emit(Array<uint8_t>& arr, uint8_t opcode, X64_UNION x64) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      write(arr.data + arr.size, opcode, std::move(x64));

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_VAL_64 {
    uint8_t op;
    uint8_t val;
    X64_UNION u64;

    static constexpr OP_VAL_64 parse(const uint8_t* bytecode) {
      OP_VAL_64 ret ={};

      ret.op = bytecode[0];
      ret.val = bytecode[1];
      ret.u64 = x64_from_bytes(bytecode + 2);

      return ret;
    }

    static constexpr size_t INSTRUCTION_SIZE = 1 + 1 + 8;

    inline static void write(uint8_t* ptr, uint8_t op, uint8_t val, X64_UNION x64) {
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

  namespace EMIT {
  #define OP_VAL_64(name) inline void name (Array<uint8_t>& arr, uint8_t val, X64_UNION x64) {\
    ByteCode::OP_VAL_64::emit(arr, ByteCode:: ## name, val, x64);\
    }

  #define OP_64(name) inline void name (Array<uint8_t>& arr, X64_UNION x64) {\
    ByteCode::OP_64::emit(arr, ByteCode:: ## name, x64);\
    }

  #define OP(name) inline void name (Array<uint8_t>& arr) {\
    ByteCode::OP::emit(arr, ByteCode:: ## name);\
    }

  #define OP_VAL(name) inline void name (Array<uint8_t>& arr, uint8_t val) {\
    ByteCode::OP_VAL::emit(arr, ByteCode:: ## name, val);\
    }

  #define OP_VAL_VAL(name) inline void name (Array<uint8_t>& arr, uint8_t val1, uint8_t val2) {\
    ByteCode::OP_VAL_VAL::emit(arr, ByteCode:: ## name, val1, val2);\
    }

  #define X(name, structure) structure(name)
   BYTECODES_X
  #undef X

   #undef OP_VAL_64
   #undef OP_64
   #undef OP
   #undef OP_VAL
   #undef OP_VAL_VAL
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
  #define OP_VAL_64(name) inline void name (uint8_t* ptr, uint8_t val, X64_UNION x64) {\
    ByteCode::OP_VAL_64::write(ptr, ByteCode:: ## name, val, x64);\
    }

  #define OP_64(name) inline void name (uint8_t* ptr, X64_UNION x64) {\
    ByteCode::OP_64::write(ptr, ByteCode:: ## name, x64);\
    }

  #define OP(name) inline void name (uint8_t* ptr) {\
    ByteCode::OP::write(ptr, ByteCode:: ## name);\
    }

  #define OP_VAL(name) inline void name (uint8_t* ptr, uint8_t val) {\
    ByteCode::OP_VAL::write(ptr, ByteCode:: ## name, val);\
    }

  #define OP_VAL_VAL(name) inline void name (uint8_t* ptr, uint8_t val1, uint8_t val2) {\
    ByteCode::OP_VAL_VAL::write(ptr, ByteCode:: ## name, val1, val2);\
    }

  #define X(name, structure) structure(name)
    BYTECODES_X
    #undef X

    #undef OP_VAL_64
    #undef OP_64
    #undef OP
    #undef OP_VAL
    #undef OP_VAL_VAL
  }
}
