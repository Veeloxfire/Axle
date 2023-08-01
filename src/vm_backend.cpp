#include "ir.h"
#include "type.h"
#include "trace.h"

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
  else {
    return { IR::Indirection::None };
  }
}

u8* VM::StackFrame::get_value(IR::ValueIndex index, u32 offset) {
  const u32 i = index.index();
  auto& val_info = variables.data[i + (index.is_variable() ? 0 : num_variables)];

  u8* const val = bytes.data + (val_info.data_offset + offset);

  IndirectionInfo indirection = get_indirection(ir, index);
  switch (indirection.ind) {
    case IR::Indirection::None: break;
    case IR::Indirection::Reference: {
        u8* ref = get_value(indirection.indirect_to, indirection.indirect_to_offset);
        x64_to_bytes(ref, val);
        break;
      }
    case IR::Indirection::Dereference: {
        u8* deref = get_value(indirection.indirect_to, indirection.indirect_to_offset);
        x64_to_bytes(x64_from_bytes(deref), val);
        break;
      }
  }

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

VM::StackFrame VM::new_stack_frame(const IR::Builder* builder) {
  u32 size_needed = 0;
  OwnedArr variables = new_arr<VM::Value>(builder->variables.size + builder->temporaries.size);

  for (usize i = 0; i < builder->variables.size; ++i) {
    const auto& var = builder->variables.data[i];
    auto& vm_var = variables.data[i];

    size_needed = ceil_to_n(size_needed, var.type.structure->alignment);

    vm_var.data_offset = size_needed;
    size_needed += var.type.structure->size;
  }

  const u32 size_base = size_needed;

  FOR(builder->expression_frames, it) {
    u32 temp_size_needed = size_base;
    for (usize i = 0; i < it->temporary_count; ++i) {
      const auto& temp = builder->temporaries.data[i];
      auto& vm_var = variables.data[builder->variables.size + i];

      temp_size_needed = ceil_to_n(temp_size_needed, temp.type.structure->alignment);
      vm_var.data_offset = temp_size_needed;
      temp_size_needed += temp.type.structure->size;
    }

    if (temp_size_needed > size_needed) size_needed = temp_size_needed;
  }

  VM::StackFrame vm = {};
  vm.bytes = new_arr<u8>(size_needed);
  vm.variables = std::move(variables);
  vm.num_variables = (u32)builder->variables.size;
  vm.num_temporaries = (u32)builder->temporaries.size;
  vm.ir = builder;
  vm.IP_BASE = builder->ir_bytecode.begin();
  vm.IP = vm.IP_BASE;
  vm.IP_END = builder->ir_bytecode.end();

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

void VM::exec(Errors* errors, VM::StackFrame* stack_frame) {
  TRACING_FUNCTION();

  while (true) {
    const auto opcode = static_cast<IR::OpCode>(stack_frame->IP[0]);

    switch (opcode) {
      case IR::OpCode::Set: {
          IR::Types::Set set;
          stack_frame->IP = IR::Read::Set(stack_frame->IP, stack_frame->IP_END, set);

          u8* to = stack_frame->get_value(set.to, set.t_offset);

          //maybe have some issues here with different platforms sizing things differently
          ASSERT(set.d_size == vm_types_info.get_size(set.t_format));


          copy_values(to, set.t_format, set.data, set.t_format);
          break;
        }

      case IR::OpCode::CopyCast: {
          IR::Types::CopyCast copy;
          stack_frame->IP = IR::Read::CopyCast(stack_frame->IP, stack_frame->IP_END, copy);

          u8* from = stack_frame->get_value(copy.from, copy.f_offset);
          u8* to = stack_frame->get_value(copy.to, copy.t_offset);

          copy_values(to, copy.t_format, from, copy.f_format);
          break;
        }

#define BIN_OP_CASE(name, op_symbol) \
      case IR::OpCode:: name: { \
          IR::Types:: name op; \
          stack_frame->IP = IR::Read:: name(stack_frame->IP, stack_frame->IP_END, op); \
          u8* to = stack_frame->get_value(op.to, op.t_offset); \
          u8* left = stack_frame->get_value(op.left, op.l_offset); \
          u8* right = stack_frame->get_value(op.right, op.r_offset); \
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
          stack_frame->IP = IR::Read::Neg(stack_frame->IP, stack_frame->IP_END, op);
          u8* to = stack_frame->get_value( op.to, op.t_offset);
          u8* from = stack_frame->get_value(op.from, op.f_offset);
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
          stack_frame->IP = IR::Read::Not(stack_frame->IP, stack_frame->IP_END, op);
          u8* to = stack_frame->get_value(op.to, op.t_offset);
          u8* from = stack_frame->get_value(op.from, op.f_offset);
          if (op.t_format == IR::Format::uint8) {
            *to = static_cast<u8>(!static_cast<bool>(*from));
          }
          else {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Unsupported format for unary logical not (only supported format is uint8)");
          }
        } break;

      case IR::OpCode::IfSplit: {
          IR::Types::IfSplit is;
          stack_frame->IP = IR::Read::IfSplit(stack_frame->IP, stack_frame->IP_END, is);

          ASSERT(is.format == IR::Format::uint8);
          u8* val = stack_frame->get_value(is.val, is.offset);
          const IR::Builder* ir = stack_frame->ir;
          if (*val != 0) {
            stack_frame->IP = stack_frame->IP_BASE + ir->control_blocks.data[is.label_if.label].start;
          }
          else {
            stack_frame->IP = stack_frame->IP_BASE + ir->control_blocks.data[is.label_else.label].start;
          }

        } break;

      case IR::OpCode::Jump: {
          IR::Types::Jump jump;
          stack_frame->IP = IR::Read::Jump(stack_frame->IP, stack_frame->IP_END, jump);
          const IR::Builder* ir = stack_frame->ir;

          stack_frame->IP = stack_frame->IP_BASE + ir->control_blocks.data[jump.local_label.label].start;
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