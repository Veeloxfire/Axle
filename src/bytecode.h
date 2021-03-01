#pragma once
#include "utility.h"
#include <stdio.h>

#include "bytecode_incs.h"

struct REGISTER_CONSTANT { uint8_t REG; const char* name; };

#define REGISTER_LOAD(NAME, VAL) inline constexpr REGISTER_CONSTANT NAME = { VAL, #NAME }

REGISTER_LOAD(RAX, 0);
REGISTER_LOAD(RBX, 1);
REGISTER_LOAD(RCX, 2);
REGISTER_LOAD(RDX, 3);
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
struct Function;

namespace ByteCode {

  enum ByteCodeOp : uint8_t {
  #define X(NAME) NAME,
    BYTECODES_X
  #undef X
  };

  void print_bytecode(const System* system,
                      FILE* const stream,
                      const uint8_t* bytecode,
                      uint64_t size);

  void log_bytecode(const System* system, const uint8_t* bytecode, uint64_t size);
  void error_bytecode(const System* system, const uint8_t* bytecode, uint64_t size);

  struct OP_R_R {
    static constexpr size_t INSTRUCTION_SIZE = 1 + 1 + 1;
    inline static void emit(Array<uint8_t>& arr, uint8_t op, uint8_t r1, uint8_t r2) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      arr.data[arr.size]     = op;
      arr.data[arr.size + 1] = r1;
      arr.data[arr.size + 2] = r2;

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_R {
    static constexpr size_t INSTRUCTION_SIZE = 1 + 1;
    inline static void emit(Array<uint8_t>& arr, uint8_t op, uint8_t r1) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      arr.data[arr.size]     = op;
      arr.data[arr.size + 1] = r1;

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_64 {
    static constexpr size_t INSTRUCTION_SIZE = 1 + 8;
    inline static void emit(Array<uint8_t>& arr, uint8_t opcode, X64_UNION x64) {

      arr.reserve_extra(INSTRUCTION_SIZE);

      arr.data[arr.size] = opcode;
      x64_to_bytes(x64, arr.data + arr.size + 1);

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_64_R {
    static constexpr size_t INSTRUCTION_SIZE = 1 + 8 + 1;
    inline static void emit(Array<uint8_t>& arr, uint8_t opcode, X64_UNION x64, uint8_t reg) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      arr.data[arr.size] = opcode;
      x64_to_bytes(x64, arr.data + arr.size + 1);

      arr.data[arr.size + 9] = reg;

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_64_R_64 {
    static constexpr size_t INSTRUCTION_SIZE = 1 + 8 + 1 + 8;
    inline static void emit(Array<uint8_t>& arr, uint8_t opcode, X64_UNION x64_1, uint8_t reg, X64_UNION x64_2) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      arr.data[arr.size] = opcode;
      x64_to_bytes(x64_1, arr.data + arr.size + 1);

      arr.data[arr.size + 9] = reg;
      x64_to_bytes(x64_2, arr.data + arr.size + 10);

      arr.size += INSTRUCTION_SIZE;
    }
  };

  struct OP_R_R_64 {
    static constexpr size_t INSTRUCTION_SIZE = 1 + 1 + 1 + 8;
    inline static void emit(Array<uint8_t>& arr, uint8_t opcode, uint8_t r1, uint8_t r2, X64_UNION x64) {
      arr.reserve_extra(INSTRUCTION_SIZE);

      arr.data[arr.size] = opcode;
      arr.data[arr.size + 1] = r1;
      arr.data[arr.size + 2] = r2;

      x64_to_bytes(x64, arr.data + arr.size + 3);

      arr.size += INSTRUCTION_SIZE;
    }
  };

  void emit_return(Array<uint8_t>& arr);
  void emit_enter(Array<uint8_t>& arr);

  void emit_call_ptr(Array<uint8_t>& arr, const Function* x64);
  void emit_call_offset(Array<uint8_t>& arr, int64_t x64);
  void emit_call_r(Array<uint8_t>& arr, uint8_t reg);
  void emit_jump_by_offset(Array<uint8_t>& arr, int64_t x64);
  void emit_jump_by_offset_if_zero(Array<uint8_t>& arr, int64_t x64);
  void emit_jump_by_offset_if_not_zero(Array<uint8_t>& arr, int64_t x64);

  void emit_push_r(Array<uint8_t>& arr, uint8_t reg);
  void emit_pop_r(Array<uint8_t>& arr, uint8_t reg);

  void emit_set_r_to_zf(Array<uint8_t>& arr, uint8_t reg);

  void emit_add_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to);
  void emit_add_64_to_r(Array<uint8_t>& arr, X64_UNION from, uint8_t to);
  void emit_sub_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to);
  void emit_sub_64_to_r(Array<uint8_t>& arr, X64_UNION from, uint8_t to);
  void emit_cmp_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to);
  void emit_cmp_64_to_r(Array<uint8_t>& arr, X64_UNION from, uint8_t to);

  void emit_mul_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to);
  void emit_mul_64_to_r(Array<uint8_t>& arr, X64_UNION from, uint8_t to);
  void emit_div_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to);
  void emit_div_64_to_r(Array<uint8_t>& arr, X64_UNION from, uint8_t to);

  void emit_or_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to);
  void emit_and_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to);

  void emit_mov_r_to_r(Array<uint8_t>& arr, uint8_t from, uint8_t to);
  void emit_mov_r_to_m(Array<uint8_t>& arr, uint8_t from, uint8_t to, int64_t disp);
  void emit_mov_m_to_r(Array<uint8_t>& arr, uint8_t from, int64_t disp, uint8_t to);
  void emit_mov_64_to_r(Array<uint8_t>& arr, X64_UNION x64, uint8_t to);
  void emit_mov_64_to_m(Array<uint8_t>& arr, X64_UNION x64, uint8_t to, int64_t disp);
}
