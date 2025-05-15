#include "ir.h"
#include "type.h"

#include <Axle/tracing_wrapper.h>

#include "compiler.h"

namespace {
  struct NegVisitor {
    Errors* errors;
    Axle::ViewArr<u8> out;

    template<typename T>
    void operator()(const auto&...) const {
      errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Unsupported format for unary negate (maybe was an unsigned integer)");
    }

    template<Axle::OneOf<i8, i16, i32, i64> T>
    void operator()(const Axle::ViewArr<const u8> data) const {
      T i;
      bool res = Axle::deserialize_le<T>(data, i);
      ASSERT(res);
      Axle::serialize_le<T>(out, -i);
    }
  };
}

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

RealValue VM::StackFrame::get_parameter(u32 param_c) {
  AXLE_TELEMETRY_FUNCTION();
  ASSERT(param_c < num_parameters);
  const auto& val_info = values[param_c];

  return {
    bytes.data + val_info.data_offset,
    val_info.type,
  };
}

RealValue VM::StackFrame::get_return_value() {
  AXLE_TELEMETRY_FUNCTION();
  ASSERT(has_return);
  const auto& val_info = values[num_parameters];

  return {
    bytes.data + val_info.data_offset,
    val_info.type,
  };
}

RealValue VM::StackFrame::get_value(const IR::V_ARG& arg) {
  AXLE_TELEMETRY_FUNCTION();
  const u32 i = arg.val.index;
  ASSERT(i < current_block->temporaries.size);
  const auto& val_info = values[i + current_block_temporaries_offset];

  return {
    bytes.data + val_info.data_offset,
    val_info.type,
  };
}

RealValue VM::StackFrame::get_indirect_value(const IR::P_ARG& arg) {
  AXLE_TELEMETRY_FUNCTION();
  const u32 i = arg.ptr.index;
  ASSERT(i < current_block->temporaries.size);
  const auto& val_info = values[i + current_block_temporaries_offset];

  ASSERT(val_info.type.struct_type() == STRUCTURE_TYPE::POINTER);
  const auto* pt = val_info.type.unchecked_base<PointerStructure>();

  const u8* src = bytes.data + val_info.data_offset;
  u8* ptr = nullptr;
  memcpy_s(&ptr, sizeof(ptr), src, sizeof(ptr));

  return {
    ptr,
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

VM::StackFrame VM::new_stack_frame(const IR::IRStore* ir) {
  AXLE_TELEMETRY_FUNCTION();
  ASSERT(ir->completed);
  ASSERT(ir->control_blocks.size > 0);
  
  const SignatureStructure* signature = ir->signature;

  const u32 num_parameters = static_cast<u32>(signature->parameter_types.size);
  
  const Type return_type = ir->signature->return_type;
  const bool has_return = return_type.struct_type() != STRUCTURE_TYPE::VOID;

  const u32 num_variables = static_cast<u32>(ir->variables.size);

  u64 total_temporaries = 0;

  FOR(ir->control_blocks, b) {
    total_temporaries += b->temporaries.size;
  }

  const u64 total_values = num_parameters + has_return + num_variables + total_temporaries;

  Axle::OwnedArr values = Axle::new_arr<VM::Value>(total_values);

  u32 size_needed = ir->max_stack;
  
  u32 values_counter = 0;

  for (Type ty: signature->parameter_types) {
    auto& vm_temp = values[values_counter];
    size_needed = Axle::ceil_to_n(size_needed, ty.structure->alignment);

    vm_temp.data_offset = size_needed;
    vm_temp.type = ty;

    size_needed += ty.size();
    values_counter += 1;
  }

  if (has_return) {
    auto& vm_temp = values[values_counter];
    size_needed = Axle::ceil_to_n(size_needed, return_type.structure->alignment);

    vm_temp.data_offset = size_needed;
    vm_temp.type = return_type;

    size_needed += return_type.size();
    values_counter += 1;
  }

  const u64 variables_offset = values_counter;

  for (const IR::SSAVar& var: ir->variables) {
    auto& vm_temp = values[values_counter];
    size_needed = Axle::ceil_to_n(size_needed, var.type.structure->alignment);

    vm_temp.data_offset = size_needed;
    vm_temp.type = var.type;

    size_needed += var.type.size();
    values_counter += 1;
  }

  ASSERT(values_counter == num_parameters + has_return + num_variables);
  
  const u32 size_base = size_needed;

  Axle::OwnedArr<u32> temp_offsets = Axle::new_arr<u32>(ir->control_blocks.size);

  const usize num_control_blocks = ir->control_blocks.size;
  for (usize cb = 0; cb < num_control_blocks; ++cb) {
    const IR::ControlBlock* it = &ir->control_blocks[cb];
    temp_offsets[cb] = values_counter;

    u32 temp_size_needed = size_base;
    const usize num_temps = it->temporaries.size;

    ASSERT(values_counter + num_temps <= total_values);

    for (usize i = 0; i < num_temps; ++i) {
      const auto& temp = it->temporaries.data[i];
      auto& vm_temp = values[i + values_counter];

      if (temp.is_variable) {
        const auto& var = ir->variables.data[temp.var_id.variable];
        ASSERT(var.type == temp.type);
        // Copy the variable location
        vm_temp = values[variables_offset + temp.var_id.variable];
      }
      else {
        temp_size_needed = Axle::ceil_to_n(temp_size_needed, temp.type.structure->alignment);
        vm_temp.data_offset = temp_size_needed;
        vm_temp.type = temp.type;
        temp_size_needed += temp.type.size();
      }
    }

    values_counter += static_cast<u32>(num_temps);

    if (temp_size_needed > size_needed) size_needed = temp_size_needed;
  }

  ASSERT(values_counter == total_values);
  ASSERT(temp_offsets[0] == num_parameters + has_return + num_variables);

  const u32 first_offset = temp_offsets[0];
  const IR::ControlBlock* first_block = ir->control_blocks.data;

  return VM::StackFrame {
    .num_parameters = num_parameters,
    .has_return = has_return,
    .num_variables = num_variables,
    .variables_offset = num_parameters + has_return,
    .bytes = Axle::new_arr<u8>(size_needed),
    .values = std::move(values),
    .block_temporary_offsets = std::move(temp_offsets),
    .ir = ir,
    .current_block = first_block,
    .current_block_temporaries_offset = first_offset,
    .IP = first_block->bytecode.begin(),
    .IP_END = first_block->bytecode.end(),
  };
}

void VM::copy_values(Axle::ViewArr<u8> to, IR::Format t_format,
                     Axle::ViewArr<const u8> from, IR::Format f_format) {
  AXLE_TELEMETRY_FUNCTION();
  
  constexpr auto int_dispatch = []<typename T>(Axle::ViewArr<u8> to, IR::Format t_format, Axle::ViewArr<const u8> from) {
    T i;
    bool res = Axle::deserialize_le<T>(from, i);
    ASSERT(res);

    switch(t_format) {
      case IR::Format::uint8: return Axle::serialize_le<u8>(to, static_cast<u8>(i));
      case IR::Format::sint8: return Axle::serialize_le<i8>(to, static_cast<i8>(i));
      case IR::Format::uint16: return Axle::serialize_le<u16>(to, static_cast<u16>(i));
      case IR::Format::sint16: return Axle::serialize_le<i16>(to, static_cast<i16>(i));
      case IR::Format::uint32: return Axle::serialize_le<u32>(to, static_cast<u32>(i));
      case IR::Format::sint32: return Axle::serialize_le<i32>(to, static_cast<i32>(i));
      
      case IR::Format::pointer:
      case IR::Format::uint64: return Axle::serialize_le<u64>(to, static_cast<u64>(i));

      case IR::Format::sint64: return Axle::serialize_le<i64>(to, static_cast<i64>(i));
      
      case IR::Format::slice:
      case IR::Format::opaque:
        INVALID_CODE_PATH("Unsupported copy");
    }
  };

  switch(f_format) {
    case IR::Format::uint8: return int_dispatch.operator()<u8>(to, t_format, from);
    case IR::Format::sint8: return int_dispatch.operator()<i8>(to, t_format, from);
    case IR::Format::uint16: return int_dispatch.operator()<u16>(to, t_format, from);
    case IR::Format::sint16: return int_dispatch.operator()<i16>(to, t_format, from);
    case IR::Format::uint32: return int_dispatch.operator()<u32>(to, t_format, from);
    case IR::Format::sint32: return int_dispatch.operator()<i32>(to, t_format, from);
    
    case IR::Format::pointer:
    case IR::Format::uint64: return int_dispatch.operator()<u64>(to, t_format, from);
    
    case IR::Format::sint64: return int_dispatch.operator()<i64>(to, t_format, from);

    case IR::Format::slice: {
      ASSERT(t_format == IR::Format::slice);
      ASSERT(from.size == 16);
      ASSERT(to.size == 16);

      Axle::memcpy_ts(to, from);
      return;
    }

    case IR::Format::opaque: {
      ASSERT(t_format == IR::Format::opaque);
      ASSERT(to.size == from.size);

      Axle::memcpy_ts(to, from);
    }
  }
}


void VM::StackFrame::jump_to_label(IR::LocalLabel l) {
  AXLE_TELEMETRY_FUNCTION();
  ASSERT(l != IR::NULL_LOCAL_LABEL);
  const IR::ControlBlock* next = ir->control_blocks.data + (l.label - 1);
  const u32 offset = block_temporary_offsets[(l.label - 1)];

  ASSERT(offset + next->temporaries.size <= values.size);

  current_block = next;
  current_block_temporaries_offset = offset;
  IP = next->bytecode.begin();
  IP_END = next->bytecode.end();
};

void VM::exec(CompilerGlobals* comp, CompilerThread* comp_thread, VM::StackFrame* stack_frame) {
  AXLE_TELEMETRY_FUNCTION();

  while (true) {
    while (stack_frame->IP < stack_frame->IP_END) {
      const IR::OpCode opcode 
        = static_cast<IR::OpCode>(stack_frame->IP[0]);

#ifdef STACKTRACE_ENABLE
      Axle::Stacktrace::ScopedExecTrace opcode_stacktrace = opcode_string(opcode);
#endif

      switch (opcode) {
        case IR::OpCode::BreakPoint: {
            IR::Types::BreakPoint break_point;
            stack_frame->IP = IR::Read::BreakPoint(stack_frame->IP, stack_frame->IP_END, break_point);

            if (IsDebuggerPresent()) {
              DebugBreak();
            }
            break;
          }
        case IR::OpCode::Set: {
            IR::Types::Set set;
            stack_frame->IP = IR::Read::Set(stack_frame->IP, stack_frame->IP_END, set);

            auto to = stack_frame->get_value(set.to);

            //maybe have some issues here with different platforms sizing things differently
            const IR::Format f = to.t.struct_format();
            ASSERT(set.data.size == vm_types_info.get_size(f));

            copy_values(Axle::view_arr(to), f, Axle::view_arr(set.data), f);
            break;
          }
        case IR::OpCode::SetStore: {
            IR::Types::SetStore set;
            stack_frame->IP = IR::Read::SetStore(stack_frame->IP, stack_frame->IP_END, set);

            auto to = stack_frame->get_indirect_value(set.to);

            //maybe have some issues here with different platforms sizing things differently
            const IR::Format f = to.t.struct_format();
            ASSERT(set.data.size == vm_types_info.get_size(f));

            copy_values(Axle::view_arr(to), f, Axle::view_arr(set.data), f);
            break;
          }

        case IR::OpCode::Copy: {
            IR::Types::Copy copy;
            stack_frame->IP = IR::Read::Copy(stack_frame->IP, stack_frame->IP_END, copy);

            auto from = stack_frame->get_value(copy.from);
            auto to = stack_frame->get_value(copy.to);
            const IR::Format t_format = to.t.struct_format();
            const IR::Format f_format = from.t.struct_format();

            copy_values(Axle::view_arr(to), t_format, Axle::view_arr(from), f_format);
            break;
          }
        case IR::OpCode::CopyLoad: {
            IR::Types::CopyLoad copy;
            stack_frame->IP = IR::Read::CopyLoad(stack_frame->IP, stack_frame->IP_END, copy);

            auto from = stack_frame->get_indirect_value(copy.from);
            auto to = stack_frame->get_value(copy.to);
            const IR::Format t_format = to.t.struct_format();
            const IR::Format f_format = from.t.struct_format();

            copy_values(Axle::view_arr(to), t_format, Axle::view_arr(from), f_format);
            break;
          }
        case IR::OpCode::CopyStore: {
            IR::Types::CopyStore copy;
            stack_frame->IP = IR::Read::CopyStore(stack_frame->IP, stack_frame->IP_END, copy);

            auto from = stack_frame->get_value(copy.from);
            auto to = stack_frame->get_indirect_value(copy.to);
            const IR::Format t_format = to.t.struct_format();
            const IR::Format f_format = from.t.struct_format();

            copy_values(Axle::view_arr(to), t_format, Axle::view_arr(from), f_format);
            break;
          }
        case IR::OpCode::CopyLoadStore: {
            IR::Types::CopyLoadStore copy;
            stack_frame->IP = IR::Read::CopyLoadStore(stack_frame->IP, stack_frame->IP_END, copy);

            auto from = stack_frame->get_indirect_value(copy.from);
            auto to = stack_frame->get_indirect_value(copy.to);
            const IR::Format t_format = to.t.struct_format();
            const IR::Format f_format = from.t.struct_format();

            copy_values(Axle::view_arr(to), t_format, Axle::view_arr(from), f_format);
            break;
          }

        case IR::OpCode::StartFunc: {
            IR::Types::StartFunc start;
            stack_frame->IP = IR::Read::StartFunc(stack_frame->IP, stack_frame->IP_END, start);

            ASSERT(start.values == nullptr);

            const SignatureStructure* sig = stack_frame->ir->signature;
            ASSERT(sig->parameter_types.size == stack_frame->num_parameters);
            ASSERT(start.n_values == stack_frame->num_parameters);

            for (u32 i = 0; i < start.n_values; ++i) {
              IR::V_ARG arg;
              stack_frame->IP += IR::deserialize(stack_frame->IP, stack_frame->IP_END - stack_frame->IP, arg);

              const Type param_ty = sig->parameter_types[i];

              RealValue incoming_param = stack_frame->get_parameter(i);
              ASSERT(param_ty == incoming_param.t);

              RealValue arg_val = stack_frame->get_value(arg);

              //TODO: fix
              ASSERT(!Eval::must_pass_type_by_reference(sig->calling_convention, param_ty.structure));

              copy_values(Axle::view_arr(arg_val), arg.format,
                          Axle::view_arr(incoming_param), param_ty.struct_format());
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
          Axle::ViewArr<u8> to_ser = Axle::view_arr(to); \
          constexpr auto do_op = []<typename T>(auto& ser, Axle::ViewArr<const u8> left, Axle::ViewArr<const u8> right) { \
            T l, r;\
            bool res_l = Axle::deserialize_le<T>(left, l); \
            ASSERT(res_l); \
            bool res_r = Axle::deserialize_le<T>(right, r); \
            ASSERT(res_r); \
            Axle::serialize_le<T>(ser, static_cast<T>(l op_symbol r)); \
          }; \
          switch (t_format) \
          { \
            case IR::Format::uint8: do_op.operator()<u8>(to_ser, Axle::view_arr(left), Axle::view_arr(right)); break; \
            case IR::Format::sint8: do_op.operator()<i8>(to_ser, Axle::view_arr(left), Axle::view_arr(right)); break; \
            case IR::Format::uint16: do_op.operator()<u16>(to_ser, Axle::view_arr(left), Axle::view_arr(right)); break; \
            case IR::Format::sint16: do_op.operator()<i16>(to_ser, Axle::view_arr(left), Axle::view_arr(right)); break; \
            case IR::Format::uint32: do_op.operator()<u32>(to_ser, Axle::view_arr(left), Axle::view_arr(right)); break; \
            case IR::Format::sint32: do_op.operator()<i32>(to_ser, Axle::view_arr(left), Axle::view_arr(right)); break; \
            case IR::Format::uint64: do_op.operator()<u64>(to_ser, Axle::view_arr(left), Axle::view_arr(right)); break; \
            case IR::Format::sint64: do_op.operator()<i64>(to_ser, Axle::view_arr(left), Axle::view_arr(right)); break; \
            case IR::Format::opaque: \
            case IR::Format::pointer: \
            case IR::Format::slice: \
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
          BIN_OP_CASE(Mod, % );
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
            Axle::ViewArr<u8> to_ser = Axle::view_arr(to);

            visit_ir_type(NegVisitor{&comp_thread->errors, to_ser}, f_format, Axle::view_arr(from));
            if(comp_thread->is_panic()) {
              return;
            }
        } break;

        case IR::OpCode::Not: {
            IR::Types::Not op;
            stack_frame->IP = IR::Read::Not(stack_frame->IP, stack_frame->IP_END, op);
            auto to = stack_frame->get_value(op.to);
            auto from = stack_frame->get_value(op.from);
            const IR::Format t_format = to.t.struct_format();
            const IR::Format f_format = from.t.struct_format();
            ASSERT(t_format == f_format);
            Axle::ViewArr<u8> to_ser = Axle::view_arr(to);
            if (t_format == IR::Format::uint8) {
              u8 raw;
              bool res = Axle::deserialize_le<u8>(Axle::view_arr(from), raw);
              ASSERT(res);
              Axle::serialize_le<i8>(to_ser, static_cast<u8>(!static_cast<bool>(raw)));
            }
            else {
              comp_thread->report_error(ERROR_CODE::IR_ERROR, Span{}, "Unsupported format for unary logical not (only supported format is uint8)");
              return;
            }
          } break;
        
        case IR::OpCode::Call: {
            IR::Types::Call op;
            stack_frame->IP = IR::Read::Call(stack_frame->IP, stack_frame->IP_END, op);

            ASSERT(op.values == nullptr);

            GlobalLabelInfo label_info = comp->get_label_info(op.label);
            const IR::IRStore* store = label_info.ir;

            if (store == nullptr) {
              comp_thread->report_error(ERROR_CODE::CONST_ERROR, label_info.span, "Attempted to call on runtime function at compile time");
              return ;
            }

            ASSERT(store->completed);

            StackFrame callframe = new_stack_frame(store);

            const SignatureStructure* sig = label_info.signature;

            ASSERT(callframe.num_parameters == sig->parameter_types.size);
            const u32 num_params = callframe.num_parameters;
            const bool has_return = sig->return_type != comp_thread->builtin_types->t_void;

            ASSERT(op.n_values == num_params + has_return);

            for (u32 p = 0; p < sig->parameter_types.size; ++p) {
              IR::V_ARG arg;
              stack_frame->IP += IR::deserialize(stack_frame->IP, stack_frame->IP_END - stack_frame->IP, arg);

              const Type& ty = sig->parameter_types[p];
              IR::Format format = ty.struct_format();
              
              RealValue p_val = callframe.get_parameter(p);
              RealValue a_val = stack_frame->get_value(arg);

              //TODO: fix
              ASSERT(!Eval::must_pass_type_by_reference(sig->calling_convention, ty.structure));

              copy_values(Axle::view_arr(p_val), format,
                          Axle::view_arr(a_val), arg.format);
            }

            // Recurse
            exec(comp, comp_thread, &callframe);
            if (comp_thread->is_panic()) {
              return;
            }

            ASSERT(has_return == callframe.has_return);
            if (has_return) {
              IR::V_ARG arg;
              stack_frame->IP += IR::deserialize(stack_frame->IP, stack_frame->IP_END - stack_frame->IP, arg);
              
              auto call_ret = callframe.get_return_value();
              auto op_ret = stack_frame->get_value(arg);

              const Type& ty = sig->return_type;
              IR::Format format = ty.struct_format();

              copy_values(Axle::view_arr(op_ret), arg.format,
                          Axle::view_arr(call_ret), format);
            }
          } break;

        case IR::OpCode::AddrOf:
        case IR::OpCode::AddrOfLoad:
        case IR::OpCode::AddrOfGlobal: {
            comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                                      "Encountered invalid/unsupported instruction during ir execution\n"
                                      "Code: {}\nName: '{}'",
                                      (u8)opcode, IR::opcode_string(opcode));
            return;
          }
      }
    }

    switch (stack_frame->current_block->cf_type) {
      case IR::ControlFlowType::Start: {
          stack_frame->jump_to_label(stack_frame->current_block->cf_start.child);
          break;
        }
      case IR::ControlFlowType::End: {
          ASSERT(!stack_frame->has_return);
          return;
        }
      case IR::ControlFlowType::Return: {
          ASSERT(stack_frame->has_return);
          
          auto ret_index = stack_frame->current_block->cf_return.val;
          
          const Type return_type = stack_frame->ir->signature->return_type;
          const IR::Format format = return_type.struct_format();

          RealValue curr_ret_val = stack_frame->get_value(
            IR::v_arg(ret_index, 0, return_type)
          );

          RealValue stackframe_ret_val = stack_frame->get_return_value();
          
          copy_values(Axle::view_arr(stackframe_ret_val), format,
                      Axle::view_arr(curr_ret_val), format);

          return;
        }

      case IR::ControlFlowType::Inline: {
          stack_frame->jump_to_label(stack_frame->current_block->cf_inline.child);
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
            stack_frame->jump_to_label(stack_frame->current_block->cf_split.true_branch);
          }
          else {
            stack_frame->jump_to_label(stack_frame->current_block->cf_split.false_branch);
          }
          break;
        }
      case IR::ControlFlowType::Merge: break;
    }
  }
}
