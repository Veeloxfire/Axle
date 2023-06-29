#include "ir.h"
#include "type.h"
#include "windows_specifics.h"
#include "trace.h"

static constexpr usize VM_STACK_SIZE = 8 * 256;

struct VM {
  u8* stack;
  u8* stack_end;

  u8* SP;
  u8* BP;
  const u8* IP_BASE;
  const u8* IP;
  const u8* IP_END;

  u32* variable_offsets;
  u32 num_variables;
  u32 num_temporaries;

  Errors* errors;

  ~VM() {
    delete[] variable_offsets;
  }

  void allocate_stack(u64 bytes);
  void push(X64_UNION val);
  X64_UNION pop();
};

constexpr auto load_types_info() {
  struct FormatData {
    usize sizes[9];

    constexpr usize get_size(IR::Format f) const { return sizes[static_cast<usize>(f)]; }
  };

  FormatData d = {};

  d.sizes[static_cast<usize>(IR::Format::opaque)] = 0;
  d.sizes[static_cast<usize>(IR::Format::uint8)] = sizeof(u8);
  d.sizes[static_cast<usize>(IR::Format::sint8)] = sizeof(i8);
  d.sizes[static_cast<usize>(IR::Format::uint16)] = sizeof(u16);
  d.sizes[static_cast<usize>(IR::Format::sint16)] = sizeof(i16);
  d.sizes[static_cast<usize>(IR::Format::uint32)] = sizeof(u32);
  d.sizes[static_cast<usize>(IR::Format::sint32)] = sizeof(i32);
  d.sizes[static_cast<usize>(IR::Format::uint64)] = sizeof(u64);
  d.sizes[static_cast<usize>(IR::Format::sint64)] = sizeof(i64);

  return d;
}

static constexpr auto vm_types_info = load_types_info();

struct IndirectionInfo {
  IR::Indirection ind;
  IR::ValueIndex indirect_to;
  u32 indirect_to_offset;
};

static IndirectionInfo get_indirection(const IR::Builder* ir, IR::ValueIndex v) {
  if (v.is_temporary()) {
    IR::Temporary& t = ir->temporaries.data[v.index()];
    IndirectionInfo info = {};
    info.ind = t.indirection;
    info.indirect_to = t.refers_to;
    info.indirect_to_offset = t.refers_to_offset;

    return info;
  }

  return { IR::Indirection::None };
}


u8* value_location(const VM* vm, const IR::Builder* ir,
                   IR::ValueIndex index, u32 offset) {
  u32 i = index.index();
  u32 v_offset = vm->variable_offsets[i + (index.is_variable() ? 0 : vm->num_variables)] + offset;

  u8* val = vm->stack + v_offset;

  IndirectionInfo indirection = get_indirection(ir, index);
  switch (indirection.ind) {
    case IR::Indirection::None: break;
    case IR::Indirection::Reference: {
        u8* ref = value_location(vm, ir, indirection.indirect_to, indirection.indirect_to_offset);
        x64_to_bytes(ref, val);
        break;
      }
    case IR::Indirection::Dereference: {
        u8* deref = value_location(vm, ir, indirection.indirect_to, indirection.indirect_to_offset);
        x64_to_bytes(x64_from_bytes(deref), val);
        break;
      }
  }

  return val;
}

void VM::allocate_stack(u64 bytes) {
  SP -= bytes;

  if (SP <= stack) {
    errors->report_error(ERROR_CODE::VM_ERROR, Span{},
                         "VM Stack overflow during allocation");
    return;
  }
}

void VM::push(X64_UNION val) {
  SP -= 8;

  if (SP <= stack) {
    errors->report_error(ERROR_CODE::VM_ERROR, Span{},
                         "VM Stack overflow during 'push' operation");
    return;
  }

  x64_to_bytes(val, SP);
}

X64_UNION VM::pop() {
  if (SP + 8 >= stack_end) {
    errors->report_error(ERROR_CODE::VM_ERROR, Span{},
                         "VM Stack underflow during 'pop' operation");
    return { (uint64_t)0 };
  }

  X64_UNION val = x64_from_bytes(SP);
  SP += 8;
  return val;
}

//TODO: this can work we just need to do it correctly with the new system
//Written in assembly to directly deal with the stack
//extern "C" uint64_t call_native_x64(const void* func, uint64_t * param_registers, uint8_t * stack_top, uint64_t stack_required);
//
//void vm_call_native_x64(VM* const vm, const void* func_ptr, uint64_t stack_required) {
//  uint64_t param_registers[4] = {};
//
//  //Load the parameters
//  param_registers[0] = vm->registers[1].b64.reg;
//  param_registers[1] = vm->registers[2].b64.reg;
//  param_registers[2] = vm->registers[3].b64.reg;
//  param_registers[3] = vm->registers[4].b64.reg;
//
//  vm->registers[0].b64.reg = call_native_x64(func_ptr, param_registers, vm->SP, stack_required);
//}

VM init_vm(Errors* errors, const IR::Builder* builder, u8* stack_holder, u32 stack_size) {
  const u8* bytecode_start = builder->ir_bytecode.data;

  VM vm = {};
  vm.stack = stack_holder;
  vm.stack_end = vm.stack + stack_size;
  vm.BP = vm.stack;
  vm.SP = vm.stack;
  vm.IP_BASE = bytecode_start;
  vm.IP = bytecode_start;
  vm.IP_END = bytecode_start + builder->ir_bytecode.size;


  vm.num_variables = (u32)builder->variables.size;
  vm.num_temporaries = (u32)builder->temporaries.size;
  vm.variable_offsets = new u32[vm.num_variables + vm.num_temporaries];

  u32 running_offset = 0;

  {
    u32* variables = vm.variable_offsets;
    FOR(builder->variables, it) {
      running_offset = ceil_to_n(running_offset, it->type.structure->alignment);

      *variables = running_offset;
      variables += 1;

      running_offset += it->type.structure->size;
    }
  }

  {
    u32* variables = vm.variable_offsets + vm.num_variables;
    FOR(builder->temporaries, it) {
      running_offset = ceil_to_n(running_offset, it->type.structure->alignment);

      *variables = running_offset;
      variables += 1;

      running_offset += it->type.structure->size;
    }
  }

  return vm;
}

static void copy_values(u8* to, IR::Format t_format,
                        const u8* from, IR::Format f_format) {
  ASSERT(f_format != IR::Format::opaque && t_format != IR::Format::opaque);

  switch (f_format) {
      case IR::Format::uint8: {
        switch (t_format) {
          case IR::Format::uint8:
          case IR::Format::sint8:
            *to = *from;
            break;
          case IR::Format::uint16:
          case IR::Format::sint16:
            x16_to_bytes((u16)*from, to);
            break;
          case IR::Format::uint32:
          case IR::Format::sint32:
            x32_to_bytes((u32)*from, to);
            break;
          case IR::Format::uint64:
          case IR::Format::sint64:
            x64_to_bytes((u64)*from, to);
            break;
          default: INVALID_CODE_PATH("Unsupported copy");
        }
        break;
      }
    case IR::Format::sint8: {
        switch (t_format) {
          case IR::Format::uint8:
          case IR::Format::sint8:
            *to = *from;
            break;
          case IR::Format::uint16:
          case IR::Format::sint16:
            x16_to_bytes((i16)*from, to);
            break;
          case IR::Format::uint32:
          case IR::Format::sint32:
            x32_to_bytes((i32)*from, to);
            break;
          case IR::Format::uint64:
          case IR::Format::sint64:
            x64_to_bytes((i64)*from, to);
            break;
          default: INVALID_CODE_PATH("Unsupported copy");
        }
        break;
      }
    case IR::Format::uint16: {
        switch (t_format) {
          case IR::Format::uint8:
          case IR::Format::sint8: {
              u16 u = x16_from_bytes(from);
              *to = u & 0xff;
              break;
            }
          case IR::Format::uint16:
          case IR::Format::sint16:
            x16_to_bytes(x16_from_bytes(from), to);
            break;
          case IR::Format::uint32:
          case IR::Format::sint32:
            x32_to_bytes((u32)x16_from_bytes(from), to);
            break;
          case IR::Format::uint64:
          case IR::Format::sint64:
            x64_to_bytes((u64)x16_from_bytes(from), to);
            break;
          default: INVALID_CODE_PATH("Unsupported copy");
        }
        break;
      }
    case IR::Format::sint16: {
        switch (t_format) {
          case IR::Format::uint8:
          case IR::Format::sint8: {
              i16 u = x16_from_bytes(from);
              *to = u & 0xff;
              break;
            }
          case IR::Format::uint16:
          case IR::Format::sint16:
            x16_to_bytes(x16_from_bytes(from), to);
            break;
          case IR::Format::uint32:
          case IR::Format::sint32:
            x32_to_bytes((i32)x16_from_bytes(from), to);
            break;
          case IR::Format::uint64:
          case IR::Format::sint64:
            x64_to_bytes((i64)x16_from_bytes(from), to);
            break;
          default: INVALID_CODE_PATH("Unsupported copy");
        }
        break;
      }
    case IR::Format::uint32: {
        switch (t_format) {
          case IR::Format::uint8:
          case IR::Format::sint8: {
              u32 u = x32_from_bytes(from);
              *to = u & 0xff;
              break;
            }
          case IR::Format::uint16:
          case IR::Format::sint16:
            x16_to_bytes(x32_from_bytes(from) & 0xffff, to);
            break;
          case IR::Format::uint32:
          case IR::Format::sint32:
            x32_to_bytes(x32_from_bytes(from), to);
            break;
          case IR::Format::uint64:
          case IR::Format::sint64:
            x64_to_bytes((u64)x32_from_bytes(from), to);
            break;
          default: INVALID_CODE_PATH("Unsupported copy");
        }
        break;
      }
    case IR::Format::sint32: {
        switch (t_format) {
          case IR::Format::uint8:
          case IR::Format::sint8: {
              i32 u = x32_from_bytes(from);
              *to = u & 0xff;
              break;
            }
          case IR::Format::uint16:
          case IR::Format::sint16:
            x16_to_bytes(x32_from_bytes(from) & 0xffff, to);
            break;
          case IR::Format::uint32:
          case IR::Format::sint32:
            x32_to_bytes(x32_from_bytes(from), to);
            break;
          case IR::Format::uint64:
          case IR::Format::sint64:
            x64_to_bytes((i64)x32_from_bytes(from), to);
            break;
          default: INVALID_CODE_PATH("Unsupported copy");
        }
        break;
      }
    case IR::Format::uint64:
    case IR::Format::sint64: {
        switch (t_format) {
          case IR::Format::uint8:
          case IR::Format::sint8: {
              u64 u = x32_from_bytes(from);
              *to = u & 0xff;
              break;
            }
          case IR::Format::uint16:
          case IR::Format::sint16:
            x16_to_bytes((u64)x64_from_bytes(from) & 0xffff, to);
            break;
          case IR::Format::uint32:
          case IR::Format::sint32:
            x32_to_bytes((u64)x64_from_bytes(from) & 0xffffffff, to);
            break;
          case IR::Format::uint64:
          case IR::Format::sint64:
            x64_to_bytes(x64_from_bytes(from), to);
            break;
          default: INVALID_CODE_PATH("Unsupported copy");
        }
        break;
      }
    default:
      INVALID_CODE_PATH("Cant handle other data formats");
  }
}

void vm_run(Errors* errors, const IR::Builder* builder) noexcept {
  TRACING_SCOPE("vm exec");

  u8 stack_holder[VM_STACK_SIZE] = {};

  VM vm = init_vm(errors, builder, stack_holder, VM_STACK_SIZE);

  //Pre entry function - if we ever return to nullptr then we finish execution
  vm.push((uint8_t*)nullptr);

  while (true) {
    const auto opcode = static_cast<IR::OpCode>(vm.IP[0]);

    switch (opcode) {
      case IR::OpCode::Set: {
          IR::Types::Set set;
          vm.IP = IR::Read::Set(vm.IP, vm.IP_END, set);

          u8* to = value_location(&vm, builder, set.to, set.t_offset);

          //maybe have some issues here with different platforms sizing things differently
          ASSERT(set.d_size == vm_types_info.get_size(set.t_format));


          copy_values(to, set.t_format, set.data, set.t_format);
          break;
        }

      case IR::OpCode::CopyCast: {
          IR::Types::CopyCast copy;
          vm.IP = IR::Read::CopyCast(vm.IP, vm.IP_END, copy);

          u8* from = value_location(&vm, builder, copy.from, copy.f_offset);
          u8* to = value_location(&vm, builder, copy.to, copy.t_offset);

          copy_values(to, copy.t_format, from, copy.f_format);
          break;
        }

#define BIN_OP_CASE(name, op_symbol) \
      case IR::OpCode:: name: { \
          IR::Types:: name op; \
          vm.IP = IR::Read:: name(vm.IP, vm.IP_END, op); \
          u8* to = value_location(&vm, builder, op.to, op.t_offset); \
          u8* left = value_location(&vm, builder, op.left, op.l_offset); \
          u8* right = value_location(&vm, builder, op.right, op.r_offset); \
          ASSERT(op.r_format == op.l_format); \
          ASSERT(op.t_format == op.l_format); \
          switch (op.r_format) \
          { \
            case IR::Format::uint8: { \
                u8 l = *left; \
                u8 r = *right; \
                *to = static_cast<u8>(l op_symbol r); \
                break; \
              } \
            case IR::Format::sint8: { \
                i8 l = *left; \
                i8 r = *right; \
                *to = static_cast<i8>(l op_symbol r); \
                break; \
              } \
            case IR::Format::uint16: { \
                u16 l = x16_from_bytes(left); \
                u16 r = x16_from_bytes(right); \
                x16_to_bytes(static_cast<u16>(l op_symbol r), to); \
                break; \
              } \
            case IR::Format::sint16: { \
                i16 l = x16_from_bytes(left); \
                i16 r = x16_from_bytes(right); \
                x16_to_bytes(static_cast<i16>(l op_symbol r), to); \
                break; \
              } \
            case IR::Format::uint32: { \
                u32 l = x32_from_bytes(left); \
                u32 r = x32_from_bytes(right); \
                x32_to_bytes(static_cast<u32>(l op_symbol r), to); \
                break; \
              } \
            case IR::Format::sint32: { \
                i32 l = x32_from_bytes(left); \
                i32 r = x32_from_bytes(right); \
                x32_to_bytes(static_cast<i32>(l op_symbol r), to); \
                break; \
              } \
            case IR::Format::uint64: { \
                u64 l = x64_from_bytes(left); \
                u64 r = x64_from_bytes(right); \
                x64_to_bytes(static_cast<u64>(l op_symbol r), to); \
                break; \
              } \
            case IR::Format::sint64: { \
                i64 l = x64_from_bytes(left); \
                i64 r = x64_from_bytes(right); \
                x64_to_bytes(static_cast<i64>(l op_symbol r), to); \
                break; \
              } \
            default: { \
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Unsupported format for binary operator " #op_symbol); \
                return; \
              } \
          } \
        } break

                               //Binary Operators
                               BIN_OP_CASE(Add, +);
                               BIN_OP_CASE(Sub, -);
                               BIN_OP_CASE(Mul, *);
                               BIN_OP_CASE(Div, / );
                               BIN_OP_CASE(Mod, / );
                               BIN_OP_CASE(Eq, == );
                               BIN_OP_CASE(Neq, != );
                               BIN_OP_CASE(Less, < );
                               BIN_OP_CASE(Great, > );
                               BIN_OP_CASE(And, &);
                               BIN_OP_CASE(Or, | );
                               BIN_OP_CASE(Xor, ^);

#undef BIN_OP_CASE

      case IR::OpCode::Neg: {
          IR::Types::Neg op;
          vm.IP = IR::Read::Neg(vm.IP, vm.IP_END, op);
          u8* to = value_location(&vm, builder, op.to, op.t_offset);
          u8* from = value_location(&vm, builder, op.from, op.f_offset);
          ASSERT(op.t_format == op.f_format);
          switch (op.f_format)
          {
            case IR::Format::sint8: {
                *to = -static_cast<i8>(*from);
                break;
              }
            case IR::Format::sint16: {
                x16_to_bytes(-static_cast<i16>(x16_from_bytes(from)), to);
                break;
              }
            case IR::Format::sint32: {
                x32_to_bytes(-static_cast<i32>(x32_from_bytes(from)), to);
                break;
              }
            case IR::Format::sint64: {
                x64_to_bytes(static_cast<u64>(-static_cast<i64>(static_cast<u64>(x64_from_bytes(from)))), to);
                break;
              }
            default: {
                errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Unsupported format for unary negate (maybe we an unsigned integer)");
                return;
              }
          }
        } break;

      case IR::OpCode::Not: {
          IR::Types::Not op;
          vm.IP = IR::Read::Not(vm.IP, vm.IP_END, op);
          u8* to = value_location(&vm, builder, op.to, op.t_offset);
          u8* from = value_location(&vm, builder, op.from, op.f_offset);
          if (op.t_format == IR::Format::uint8) {
            *to = static_cast<u8>(!static_cast<bool>(*from));
          }
          else {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Unsupported format for unary logical not (only supported format is uint8)");
          }
        } break;

      case IR::OpCode::IfSplit: {
          IR::Types::IfSplit is;
          vm.IP = IR::Read::IfSplit(vm.IP, vm.IP_END, is);

          ASSERT(is.format == IR::Format::uint8);
          u8* val = value_location(&vm, builder, is.val, is.offset);
          if (*val != 0) {
            vm.IP = vm.IP_BASE + builder->control_blocks.data[is.label_if.label].start;
          }
          else {
            vm.IP = vm.IP_BASE + builder->control_blocks.data[is.label_else.label].start;
          }

        } break;

      case IR::OpCode::Jump: {
          IR::Types::Jump jump;
          vm.IP = IR::Read::Jump(vm.IP, vm.IP_END, jump);

          vm.IP = vm.IP_BASE + builder->control_blocks.data[jump.local_label.label].start;
          break;
        }

      default: {
          errors->report_error(ERROR_CODE::VM_ERROR, Span{},
                               "Encountered invalid/unsupported instruction during ir execution\n"
                               "Code: {}\nName: '{}'",
                               (u8)opcode, IR::opcode_string(opcode));
          return;
        }
    }
  }
}