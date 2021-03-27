#pragma once
#include "calling_conventions.h"

struct Structure;
struct EnumValue;

struct Lang {
  const Structure* s_bool = nullptr;
  const Structure* s_u8   = nullptr;
  const Structure* s_u64  = nullptr;
  const Structure* s_void = nullptr;

  const EnumValue* e_false = nullptr;
  const EnumValue* e_true  = nullptr;
};

struct BuildOptions {
  InternString entry_point;
  const System* system;
  const CallingConvention* calling_convention;
};

struct RunOptions {
  bool compile;
  bool run_after_build;
};

struct PrintOptions {
  bool ast;
  bool pre_reg_alloc;
  bool normal_bytecode;
  bool combined_bytecode;
  bool coalesce_values;
};