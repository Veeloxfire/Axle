#include "ir.h"
#include "type.h"
#include "trace.h"

#include "compiler.h"

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

using RealValue = VM::StackFrame::RealValue;

RealValue VM::StackFrame::get_value(const IR::V_ARG& arg) {
  const u32 i = arg.val.index;
  const auto& val_info = temporaries[i];

  return {
    bytes.data + val_info.data_offset,
    val_info.type,
  };
}

RealValue VM::StackFrame::get_indirect_value(const IR::V_ARG& arg) {
  const u32 i = arg.val.index;
  const auto& val_info = temporaries[i];

  ASSERT(val_info.type.struct_type() == STRUCTURE_TYPE::POINTER);
  const auto* pt = val_info.type.unchecked_base<PointerStructure>();

  const u8* src = bytes.data + val_info.data_offset;
  const u8* ptr = nullptr;
  memcpy_s(&ptr, sizeof(ptr), src, sizeof(ptr));

  return {
    bytes.data + val_info.data_offset,
    pt->base,
  };
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
  ASSERT(builder->control_blocks.size > 0);

  u32 size_needed = 0;
  OwnedArr variables = new_arr<VM::Value>(builder->variables.size);

  u64 total_temporaries = 0;
  FOR(builder->control_blocks, b) {
    total_temporaries += b->temporaries.size;
  }

  OwnedArr temporaries = new_arr<VM::Value>(total_temporaries);

  for (usize i = 0; i < builder->variables.size; ++i) {
    const auto& var = builder->variables.data[i];
    auto& vm_var = variables[i];

    size_needed = ceil_to_n(size_needed, var.type.structure->alignment);

    vm_var.data_offset = size_needed;
    vm_var.type = var.type;
    size_needed += var.type.structure->size;
  }

  const u32 size_base = size_needed;
  u64 temporaries_counter = 0;

  FOR(builder->control_blocks, it) {
    FOR(it->imports, v_imp) {
      temporaries[v_imp->in_temp.index + temporaries_counter] = variables[v_imp->variable];
    }

    FOR(it->exports, v_exp) {
      temporaries[v_exp->out_temp.index + temporaries_counter] = variables[v_exp->variable];
    }

    u32 temp_size_needed = size_base;
    const usize num_temps = it->temporaries.size;
    for (usize i = 0; i < num_temps; ++i) {
      auto& temp = it->temporaries.data[i];
      auto& vm_temp = temporaries[i + temporaries_counter];
      if (vm_temp.type.is_valid()) continue;

      temp_size_needed = ceil_to_n(temp_size_needed, temp.type.structure->alignment);
      vm_temp.data_offset = temp_size_needed;
      vm_temp.type = temp.type;
      temp_size_needed += temp.type.structure->size;
    }

    if (temp_size_needed > size_needed) size_needed = temp_size_needed;
  }

  VM::StackFrame vm = {};
  vm.bytes = new_arr<u8>(size_needed);
  vm.variables = std::move(variables);
  vm.temporaries = std::move(temporaries);
  vm.ir = builder;
  vm.current_block = builder->control_blocks.data;
  vm.IP = vm.current_block->bytecode.begin();
  vm.IP_END = vm.current_block->bytecode.end();

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

void VM::exec(CompilerThread* comp_thread, VM::StackFrame* stack_frame) {
  TRACING_FUNCTION();

  while (true) {
    while (stack_frame->IP < stack_frame->IP_END) {
      const auto opcode = static_cast<IR::OpCode>(stack_frame->IP[0]);

      switch (opcode) {
        case IR::OpCode::Set: {
            IR::Types::Set set;
            stack_frame->IP = IR::Read::Set(stack_frame->IP, stack_frame->IP_END, set);

            auto to = stack_frame->get_value(set.to);

            //maybe have some issues here with different platforms sizing things differently
            const IR::Format f = to.t.struct_format();
            ASSERT(set.data.size == vm_types_info.get_size(f));

            copy_values(to.ptr, f, set.data.val, f);
            break;
          }
        case IR::OpCode::SetStore: {
            IR::Types::Set set;
            stack_frame->IP = IR::Read::Set(stack_frame->IP, stack_frame->IP_END, set);

            auto to = stack_frame->get_indirect_value(set.to);

            //maybe have some issues here with different platforms sizing things differently
            const IR::Format f = to.t.struct_format();
            ASSERT(set.data.size == vm_types_info.get_size(f));

            copy_values(to.ptr, f, set.data.val, f);
            break;
          }

        case IR::OpCode::Copy: {
            IR::Types::Copy copy;
            stack_frame->IP = IR::Read::Copy(stack_frame->IP, stack_frame->IP_END, copy);

            auto from = stack_frame->get_value(copy.from);
            auto to = stack_frame->get_value(copy.to);
            const IR::Format t_format = to.t.struct_format();
            const IR::Format f_format = from.t.struct_format();

            copy_values(to.ptr, t_format, from.ptr, f_format);
            break;
          }
        case IR::OpCode::CopyLoad: {
            IR::Types::CopyLoad copy;
            stack_frame->IP = IR::Read::CopyLoad(stack_frame->IP, stack_frame->IP_END, copy);

            auto from = stack_frame->get_indirect_value(copy.from);
            auto to = stack_frame->get_value(copy.to);
            const IR::Format t_format = to.t.struct_format();
            const IR::Format f_format = from.t.struct_format();

            copy_values(to.ptr, t_format, from.ptr, f_format);
            break;
          }
        case IR::OpCode::CopyStore: {
            IR::Types::CopyStore copy;
            stack_frame->IP = IR::Read::CopyStore(stack_frame->IP, stack_frame->IP_END, copy);

            auto from = stack_frame->get_value(copy.from);
            auto to = stack_frame->get_indirect_value(copy.to);
            const IR::Format t_format = to.t.struct_format();
            const IR::Format f_format = from.t.struct_format();

            copy_values(to.ptr, t_format, from.ptr, f_format);
            break;
          }
        case IR::OpCode::CopyLoadStore: {
            IR::Types::CopyStore copy;
            stack_frame->IP = IR::Read::CopyStore(stack_frame->IP, stack_frame->IP_END, copy);

            auto from = stack_frame->get_indirect_value(copy.from);
            auto to = stack_frame->get_indirect_value(copy.to);
            const IR::Format t_format = to.t.struct_format();
            const IR::Format f_format = from.t.struct_format();

            copy_values(to.ptr, t_format, from.ptr, f_format);
            break;
          }

        case IR::OpCode::StartFunc: {
            IR::Types::StartFunc start;
            stack_frame->IP = IR::Read::StartFunc(stack_frame->IP, stack_frame->IP_END, start);

            if (start.n_values != 0) {
              comp_thread->report_error(ERROR_CODE::VM_ERROR, Span{}, "Functions do not support parameters in vm");
              return;
            }
            break;
          }

#define BIN_OP_CASE(name, op_symbol) \
      case IR::OpCode:: name: { \
          IR::Types:: name op; \
          stack_frame->IP = IR::Read:: name(stack_frame->IP, stack_frame->IP_END, op); \
          auto to = stack_frame->get_value(op.to); \
          auto left = stack_frame->get_value(op.left); \
          auto right = stack_frame->get_value(op.right); \
          const IR::Format t_format = to.t.struct_format(); \
          const IR::Format l_format = left.t.struct_format(); \
          const IR::Format r_format = right.t.struct_format(); \
          ASSERT(r_format == t_format); \
          ASSERT(l_format == t_format); \
          switch (t_format) \
          { \
            case IR::Format::uint8: { \
                u8 l = *left.ptr; \
                u8 r = *right.ptr; \
                *to.ptr = static_cast<u8>(l op_symbol r); \
                break; \
              } \
            case IR::Format::sint8: { \
                i8 l = *left.ptr; \
                i8 r = *right.ptr; \
                *to.ptr = static_cast<i8>(l op_symbol r); \
                break; \
              } \
            case IR::Format::uint16: { \
                u16 l = x16_from_bytes(left.ptr); \
                u16 r = x16_from_bytes(right.ptr); \
                x16_to_bytes(static_cast<u16>(l op_symbol r), to.ptr); \
                break; \
              } \
            case IR::Format::sint16: { \
                i16 l = x16_from_bytes(left.ptr); \
                i16 r = x16_from_bytes(right.ptr); \
                x16_to_bytes(static_cast<i16>(l op_symbol r), to.ptr); \
                break; \
              } \
            case IR::Format::uint32: { \
                u32 l = x32_from_bytes(left.ptr); \
                u32 r = x32_from_bytes(right.ptr); \
                x32_to_bytes(static_cast<u32>(l op_symbol r), to.ptr); \
                break; \
              } \
            case IR::Format::sint32: { \
                i32 l = x32_from_bytes(left.ptr); \
                i32 r = x32_from_bytes(right.ptr); \
                x32_to_bytes(static_cast<i32>(l op_symbol r), to.ptr); \
                break; \
              } \
            case IR::Format::uint64: { \
                u64 l = x64_from_bytes(left.ptr); \
                u64 r = x64_from_bytes(right.ptr); \
                x64_to_bytes(static_cast<u64>(l op_symbol r), to.ptr); \
                break; \
              } \
            case IR::Format::sint64: { \
                i64 l = x64_from_bytes(left.ptr); \
                i64 r = x64_from_bytes(right.ptr); \
                x64_to_bytes(static_cast<i64>(l op_symbol r), to.ptr); \
                break; \
              } \
            default: { \
            comp_thread->report_error(ERROR_CODE::IR_ERROR, Span{}, "Unsupported format for binary operator " #op_symbol); \
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
            auto to = stack_frame->get_value(op.to);
            auto from = stack_frame->get_value(op.from);
            const IR::Format t_format = to.t.struct_format();
            const IR::Format f_format = from.t.struct_format();
            ASSERT(t_format == f_format);
            switch (f_format)
            {
              case IR::Format::sint8: {
                  *to.ptr = -static_cast<i8>(*from.ptr);
                  break;
                }
              case IR::Format::sint16: {
                  x16_to_bytes(-static_cast<i16>(x16_from_bytes(from.ptr)), to.ptr);
                  break;
                }
              case IR::Format::sint32: {
                  x32_to_bytes(-static_cast<i32>(x32_from_bytes(from.ptr)), to.ptr);
                  break;
                }
              case IR::Format::sint64: {
                  x64_to_bytes(static_cast<u64>(-static_cast<i64>(static_cast<u64>(x64_from_bytes(from.ptr)))), to.ptr);
                  break;
                }
              default: {
                  comp_thread->report_error(ERROR_CODE::IR_ERROR, Span{}, "Unsupported format for unary negate (maybe we an unsigned integer)");
                  return;
                }
            }
          } break;

        case IR::OpCode::Not: {
            IR::Types::Not op;
            stack_frame->IP = IR::Read::Not(stack_frame->IP, stack_frame->IP_END, op);
            auto to = stack_frame->get_value(op.to);
            auto from = stack_frame->get_value(op.from);
            const IR::Format t_format = to.t.struct_format();
            const IR::Format f_format = from.t.struct_format();
            if (t_format == IR::Format::uint8) {
              *to.ptr = static_cast<u8>(!static_cast<bool>(*from.ptr));
            }
            else {
              comp_thread->report_error(ERROR_CODE::IR_ERROR, Span{}, "Unsupported format for unary logical not (only supported format is uint8)");
            }
          } break;
        default: {
            comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                                      "Encountered invalid/unsupported instruction during ir execution\n"
                                      "Code: {}\nName: '{}'",
                                      (u8)opcode, IR::opcode_string(opcode));
            return;
          }
      }
    }

    const auto goto_block = [&](IR::LocalLabel l) {
      ASSERT(l != IR::NULL_LOCAL_LABEL);
      const IR::Builder* ir = stack_frame->ir;
      const IR::ControlBlock* next = ir->control_blocks.data + (l.label - 1);

      stack_frame->current_block = next;
      stack_frame->IP = next->bytecode.begin();
      stack_frame->IP_END = next->bytecode.end();
    };

    switch (stack_frame->current_block->cf_type) {
      case IR::ControlFlowType::Start: {
          goto_block(stack_frame->current_block->cf_start.child);
          break;
        }
      case IR::ControlFlowType::End:
      case IR::ControlFlowType::Return: {
          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{}, "Currently don't support call control flow inside the vm");
          return;
        }

      case IR::ControlFlowType::Inline: {
          goto_block(stack_frame->current_block->cf_inline.child);
          break;
        }
      case IR::ControlFlowType::Split: {
          auto val = stack_frame->get_value(
            IR::v_arg(stack_frame->current_block->cf_split.condition,
                      0,
                      comp_thread->builtin_types->t_bool)
          );

          ASSERT(val.t.struct_format() == IR::Format::uint8);

          if (*val.ptr) {
            goto_block(stack_frame->current_block->cf_split.true_branch);
          }
          else {
            goto_block(stack_frame->current_block->cf_split.false_branch);
          }
        }
      case IR::ControlFlowType::Merge: break;
    }
  }
}