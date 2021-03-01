#pragma once
#include "utility.h"
#include "strings.h"
#include "calling_conventions.h"

#include "type.h"

struct ASTType;
struct ASTFunctionDeclaration;
struct ASTStructureDeclaration;
struct ASTStatement;
struct ASTExpression;
struct ASTDeclaration;
struct ASTFile;

enum struct CompileCode {
  NO_ERRORS = 0,
  UNFOUND_DEPENDENCY,
  TYPE_CHECK_ERROR,
};

struct Local_Variable {
  InternString name;
  const Structure* type;
  Location location;
};

enum struct MODIFICATION_TYPE : uint8_t {
  UNMODIFIED,
  MANAGED,
  MODIFIED,
};

struct Local_Register {
  MODIFICATION_TYPE modified = MODIFICATION_TYPE::UNMODIFIED;
  uint8_t occupied_size = 0;
};

enum struct VALUE_MODIFIERS : uint8_t {
  NONE /* can change place */,
  LAZY /* can change place but must stay same location type */,
  FIXED /* cannot move */,
  RESERVED /* dont need to find new home for this */,
};

struct ValueLocation {
  VALUE_MODIFIERS modifiers = VALUE_MODIFIERS::NONE;

  const Structure* type;
  Location location;
};

struct Local_State {
  static constexpr size_t NUM_REGS = 15;

  Local_Register registers[NUM_REGS] ={};
  Array<Local_Variable> locals ={};
  Array<ValueLocation> values ={};
  int64_t next_free_stack_offset = 0;
  int64_t max_stack = 0;
  int64_t max_call_stack = 0;

  uint8_t get_free_reg_from_set(const uint8_t* regs, size_t num_regs);

  uint8_t get_free_reg();
  void free_reg(uint8_t reg);

  int64_t get_stack_location(const Structure*);

  void free_value(size_t i);

  Location get_free_space(const Structure*);

  ValueLocation* get_value_location(size_t i) {
    return values.data + i - 1;
  }

  const ValueLocation* get_value_location(size_t i) const {
    return values.data + i - 1;
  }

};

enum struct COMPILATION_TYPE : uint8_t {
  SIGNATURE, FUNCTION, STRUCTURE, NONE
};

enum struct FUNCTION_COMPILE_STAGE : uint8_t {
  SIGNATURE, BODY, FINISHED,
};

enum struct COMPILE_STATUS : uint8_t {
  OK, HAD_ERROR,
};

struct CompilationUnitCarrier {
  COMPILATION_TYPE type;
  size_t index;
};

struct FunctionUnit {
  FUNCTION_COMPILE_STAGE stage;
  COMPILE_STATUS status;

  ASTFunctionDeclaration* source;
  Function* destination;

  Local_State state;
};

struct EnumValue {
  const Structure* type;
  InternString name;
  uint64_t value;
};

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
  bool print_ast;
  bool compile;
  bool print_bytecode;
  bool run_in_vm_after_build;
  bool benchmark;
};

struct Compiler {
  Array<FunctionUnit> function_units;

  Array<CompilationUnitCarrier> compiling;

  LinkedList<Structure> structures;
  LinkedList<Function> functions;
  LinkedList<EnumValue> enums;

  Lang* lang;
  StringInterner* strings;

  RunOptions run_options;
  BuildOptions build_options;

  Structure* structure_by_name(InternString);
  EnumValue* enum_by_name(InternString);

  Structure* new_composite_structure();
  Function* new_function();
};

void load_language_builtin(Compiler*);

CompileCode compile_all(Compiler* const comp);
void build_compilation_units(Compiler* const comp, ASTFile* const func);
void print_compiled_functions(Compiler* const comp);
const Function* find_entry_point(Compiler* const comp);