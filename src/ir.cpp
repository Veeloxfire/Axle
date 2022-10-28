#include "ir.h"

IR::ValueIndex IR::Function::emit_create_variable(IR::Primitive primitive, usize size) {
  ValueIndex v = { values.size };

  Value val = {};
  val.primitive = primitive;
  val.size = size;
  val.source = ValueSource::Variable;

  val.index = variables.size;
  
  variables.insert(Variable{});
  values.insert(val);

  instructions.insert(CreateInst{ v });

  return v;
}

IR::ValueIndex IR::Function::emit_create_temporary(IR::Primitive primitive, usize size) {
  ValueIndex v = { values.size };

  Value val = {};
  val.primitive = primitive;
  val.size = size;
  val.source = ValueSource::Temporary;

  val.index = temporaries.size;

  temporaries.insert(Temporary{ temporaries_counter });
  temporaries_counter += 1;
  if (temporaries_counter > max_temporaries) {
    max_temporaries = temporaries_counter;
  }

  values.insert(val);


  instructions.insert(CreateInst{ v });

  return v;
}

IR::ValueIndex IR::Function::create_constant(IR::Primitive primitive, usize size, const void* d) {
  ValueIndex v = { values.size };

  Value val = {};
  val.primitive = primitive;
  val.size = size;
  val.source = ValueSource::Constant;

  val.index = constants.size;

  u8* c = constants_pool.push_unaligned_bytes(size);

  memcpy_s(c, size, d, size);

  constants.insert(IR::Constant{ c });

  values.insert(val);

  return v;
}

IR::ValueIndex IR::Function::create_reference(Primitive primitive, usize size, IR::ValueIndex ref, IR::ValueIndex offset) {
  ASSERT(ref != INVALID_VALUE);

  ValueIndex v = { values.size };

  Value val = {};
  val.primitive = primitive;
  val.size = size;
  val.source = ValueSource::Reference;

  val.index = references.size;
  references.insert({ ref, offset });

  values.insert(val);

  return v;
}

void IR::Function::emit_add(IR::ValueIndex dest, IR::ValueIndex left, IR::ValueIndex right) {
  ASSERT(dest != INVALID_VALUE);
  ASSERT(left != INVALID_VALUE);
  ASSERT(right != INVALID_VALUE);

  instructions.insert(AddInst{ dest, left, right });
}

void IR::Function::emit_sub(IR::ValueIndex dest, IR::ValueIndex left, IR::ValueIndex right) {
  ASSERT(dest != INVALID_VALUE);
  ASSERT(left != INVALID_VALUE);
  ASSERT(right != INVALID_VALUE);

  instructions.insert(SubInst{ dest, left, right });
}

void IR::Function::emit_mul(IR::ValueIndex dest, IR::ValueIndex left, IR::ValueIndex right) {
  ASSERT(dest != INVALID_VALUE);
  ASSERT(left != INVALID_VALUE);
  ASSERT(right != INVALID_VALUE);

  instructions.insert(MulInst{ dest, left, right });
}

void IR::Function::emit_div(IR::ValueIndex dest, IR::ValueIndex left, IR::ValueIndex right) {
  ASSERT(dest != INVALID_VALUE);
  ASSERT(left != INVALID_VALUE);
  ASSERT(right != INVALID_VALUE);

  instructions.insert(DivInst{ dest, left, right });
}

void IR::Function::emit_copy(IR::ValueIndex dest, IR::ValueIndex source) {
  ASSERT(dest != INVALID_VALUE);
  ASSERT(source != INVALID_VALUE);

  instructions.insert(CopyInst{ dest, source });
}

void IR::Function::emit_return() {
  instructions.insert(ReturnInst{});
}

IR::ExecuionThread::RuntimeReference IR::ExecuionThread::eval_reference(const IR::Value& val) {
  RuntimeReference ref = {};

  switch (val.source) {
    case IR::ValueSource::Temporary: {
        const IR::Temporary& tmp = func->temporaries.data[val.index];

        RuntimeVariable* var = temporaries + tmp.compressed_temporary_index;

        if (var->size != val.size) {
          errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Size mismatchf for temporary reference\nExpected Size: {}, Actual Size: {}", val.size, var->size);
          return {};
        }

        ref.ref_to = var;
        ref.offset = 0;

        return ref;
      }
    case IR::ValueSource::Variable: {
        RuntimeVariable* var = variables + val.index;

        if (var->size != val.size) {
          errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Size mismatch for variable reference\nExpected Size: {}, Actual Size: {}", val.size, var->size);
          return {};
        }

        ref.ref_to = var;
        ref.offset = 0;

        return ref;
      }

    case IR::ValueSource::Constant: {
        errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Cannot take a reference to a constant");
        return {};
      }

    case IR::ValueSource::Reference: {
        const IR::Reference& ir_ref = func->references.data[val.index];
        const IR::Value& source_v = func->values.data[ir_ref.source];

        RuntimeReference source_ref = eval_reference(source_v);
        if (errors->is_panic()) return {};

        u64 offset = 0;

        if (ir_ref.offset != IR::INVALID_VALUE) {
          const IR::Value& offset_v = func->values.data[ir_ref.offset];

          RuntimeImmediate offset_immediate = eval_immediate(offset_v);
          if (errors->is_panic()) return {};

          if (offset_v.size > 8) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Invalid offset type size: {}\nMust be 8 or less", offset_v.size);
            return {};
          }

          offset = offset_immediate.small;
        }

        if (offset + val.size > source_v.size) {
          errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Reading out of bounds for reference\nReference Size: {}\nExpected minimum size: {}", source_v.size, offset + val.size);
          return {};
        }

        ref.ref_to = source_ref.ref_to;
        ref.offset = offset + source_ref.offset;

        return ref;
      }
    case IR::ValueSource::Invalid:
    default: {
        errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Invalid value source type");
        return {};
      }
  }
}

IR::ExecuionThread::RuntimeImmediate IR::ExecuionThread::eval_immediate(const IR::Value& val) {
  switch (val.source) {
    case IR::ValueSource::Temporary: {
        const IR::Temporary& tmp = func->temporaries.data[val.index];

        RuntimeVariable* var = temporaries + tmp.compressed_temporary_index;

        if (var->size != val.size) {
          errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Size mismatch for temporary immediate\nExpected Size: {}, Actual Size: {}", val.size, var->size);
          return {};
        }

        return RuntimeImmediate(*var);
      }
    case IR::ValueSource::Variable: {
        RuntimeVariable* var = variables + val.index;

        if (var->size != val.size) {
          errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Size mismatch for variable immediate\nExpected Size: {}, Actual Size: {}", val.size, var->size);
          return {};
        }

        return RuntimeImmediate(*var);
      }
    case IR::ValueSource::Constant: {
        const IR::Constant& cnst = func->constants.data[val.index];
        RuntimeImmediate result = {};
        switch (val.size) {
          case 0: {
              errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Cannot have empty value");
              return {};
            }

          {//Store in value when the size is small (8 or less) - just a simple way of doing that
          case 8:
            result.small |= ((u64)cnst.data[7]) << (7 * 8);
          case 7:
            result.small |= ((u64)cnst.data[6]) << (6 * 8);
          case 6:
            result.small |= ((u64)cnst.data[5]) << (5 * 8);
          case 5:
            result.small |= ((u64)cnst.data[4]) << (4 * 8);
          case 4:
            result.small |= ((u64)cnst.data[3]) << (3 * 8);
          case 3:
            result.small |= ((u64)cnst.data[2]) << (2 * 8);
          case 2:
            result.small |= ((u64)cnst.data[1]) << (1 * 8);
          case 1:
            result.small |= ((u64)cnst.data[0]);
            break;
          }

          //If its a valid size + too big for .value then we need to allocate and store
          default: {
              result.large = cnst.data;
              break;
            }
        }

        return result;
      }
    case IR::ValueSource::Reference: {
        const IR::Reference& ref = func->references.data[val.index];
        const IR::Value& source_v = func->values.data[ref.source];
        

        RuntimeReference source_ref = eval_reference(source_v);
        if (errors->is_panic()) return {};

        u64 offset = 0;

        if (ref.offset != IR::INVALID_VALUE) {
          const IR::Value& offset_v = func->values.data[ref.offset];

          RuntimeImmediate offset_immediate = eval_immediate(offset_v);
          if (errors->is_panic()) return {};

          if (offset_v.size > 8) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Invalid offset type size: {}\nMust be 8 or less", offset_v.size);
            return {};
          }

          offset = offset_immediate.small;
        }

        if (offset + val.size > source_v.size) {
          errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Reading out of bounds for reference\nReference Size: {}\nExpected minimum size: {}", source_v.size, offset + val.size);
          return {};
        }

        RuntimeImmediate result = {};

        RuntimeVariable* source_variable = source_ref.ref_to;

        //Switch is just for working on the correct part (small or large)
        switch (source_variable->size) {
          case 0: {
              errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Cannot have empty value");
              return {};
            }

          case 8:
          case 7:
          case 6:
          case 5:
          case 4:
          case 3:
          case 2:
          case 1: {
              //result.size must be less then source->size
              u64 source_mask = bit_fill_lower<u64>((source_variable->size - offset) * 8);
              u64 val_mask = bit_fill_lower<u64>(val.size * 8);
              u64 mask = source_mask & val_mask;
              result.small = (source_variable->small >> (offset * 8)) & mask;
              break;
            }
          default: {
              switch (val.size) {
                case 0: {
                    errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Cannot have empty value");
                    return {};
                  }

                case 8: result.small |= ((u64)source_variable->large[offset + 7]) << ((offset + 7) * 8);
                case 7: result.small |= ((u64)source_variable->large[offset + 6]) << ((offset + 6) * 8);
                case 6: result.small |= ((u64)source_variable->large[offset + 5]) << ((offset + 5) * 8);
                case 5: result.small |= ((u64)source_variable->large[offset + 4]) << ((offset + 4) * 8);
                case 4: result.small |= ((u64)source_variable->large[offset + 3]) << ((offset + 3) * 8);
                case 3: result.small |= ((u64)source_variable->large[offset + 2]) << ((offset + 2) * 8);
                case 2: result.small |= ((u64)source_variable->large[offset + 1]) << ((offset + 1) * 8);
                case 1: result.small |= ((u64)source_variable->large[offset + 0]) << ((offset + 0) * 8); break;
                default: {
                    result.large = source_variable->large + (source_ref.offset + offset);
                    break;
                  }
              }
              
              break;
            }
        }

        return result;
      }
    case IR::ValueSource::Invalid:
    default: {
        errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Invalid source type");
        return {};
      }
  }
}

void validate_type_and_size(Errors* errors, IR::Primitive primitive, usize size) {
  if (size == 0) {
    errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Size cannot be 0");
    return;
  }

  switch (primitive) {
    case IR::Primitive::Int: {
        switch (size) {
          case 1:
          case 2:
          case 4:
          case 8: return;

          default: {
              errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Int types can only have specified sizes\nValid sizes: 1, 2, 4, 8\nSpecified size: {}", size);
              return;
            }
        }
      }

    case IR::Primitive::Invalid: {
        errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Invalid primitive type");
        return;
      }
  }
}

void copy_number_to_reference_value(u64 small, const IR::Value& value, const IR::ExecuionThread::RuntimeReference& ref) {
  IR::ExecuionThread::RuntimeVariable* var = ref.ref_to;

  ASSERT(var->size != 0);

  if (var->size <= 8) {
    //Should all be small if the dest base is small
    u64 mask = bit_fill_lower<u64>(value.size);

    //Mask off the parts of the value so that only the specific bytes are changed
    var->small = ((var->small >> (ref.offset * 8)) & mask) | (mask & small);
  }
  else {
    //Shift out the data from small
    for (u64 i = 0; i < value.size; i++, small >>= 8) {
      var->large[ref.offset + i] = small & 0xff;
    }
  }
}

void IR::ExecuionThread::execute() {
  usize i = 0;
  while (true) {
    if (i >= func->instructions.size) {
      errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Instruction pointer went out of bounds without finishing execution");
      return;
    }

    const IR::Instruction& inst = func->instructions.data[i];
    i += 1;

    switch (inst.op) {
      case IR::Op::Nul: {
          errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Nul instruction found");
          return;
        }
      case IR::Op::Create: {
          const IR::CreateInst& create = inst.create;

          const IR::Value& dest = func->values.data[create.dest];

          validate_type_and_size(errors, dest.primitive, dest.size);
          if (errors->is_panic()) return;

          switch (dest.source) {
            case IR::ValueSource::Temporary: {
                const IR::Temporary& t = func->temporaries.data[dest.index];
                RuntimeVariable* temp = temporaries + t.compressed_temporary_index;

                //May need to reset temporaries
                if (temp->size > 8) {
                  delete[] temp->large;
                }

                if (dest.size > 8) {
                  temp->large = new u8[dest.size];
                  memset(temp->large, 0, dest.size);
                }
                else {
                  temp->small = 0;
                }

                temp->size = dest.size;
                break;
              }
            case IR::ValueSource::Variable: {
                RuntimeVariable* var = variables + dest.index;

                if (var->size != 0) {
                  errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Cannot re-create pesistent variables\nTried to re-create persisten variable: {}", dest.index);
                  return;
                }

                if (dest.size > 8) {
                  var->large = new u8[dest.size];
                  memset(var->large, 0, dest.size);
                }
                else {
                  var->small = 0;
                }

                var->size = dest.size;
                break;
              }
            case IR::ValueSource::Constant:
            case IR::ValueSource::Reference: {
                errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Cannot use create instruction with constants or references");
                return;
              }

            case IR::ValueSource::Invalid:
            default: {
                errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Invalid value source type");
                return;
              }
          }
          break;
        }
      case IR::Op::Copy: {
          const IR::CopyInst& copy = inst.copy;

          const IR::Value& dest = func->values.data[copy.dest];
          const IR::Value& source = func->values.data[copy.source];

          if (dest.primitive != source.primitive) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Copy instruction expects same primitive types\nGot dest: {} and source: {}", primitive_string(dest.primitive), primitive_string(source.primitive));
            return;
          }

          if (dest.size != source.size) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Copy instruction expects same sizes\nDestination: {}, Source: {}", dest.size, source.size);
            return;
          }

          validate_type_and_size(errors, dest.primitive, dest.size);
          if (errors->is_panic()) return;

          RuntimeImmediate source_imm = eval_immediate(source);
          if (errors->is_panic()) return;

          RuntimeReference dest_ref = eval_reference(dest);
          if (errors->is_panic()) return;

          RuntimeVariable* dest_base = dest_ref.ref_to;

          ASSERT(dest_base->size >= dest.size);
          ASSERT(dest_base->size >= source.size);

          //Actual work

          switch (dest_base->size) {
            case 0: {
                errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Variable size cannot be 0");
                return;
              }

            case 8:
            case 7:
            case 6:
            case 5:
            case 4:
            case 3:
            case 2:
            case 1: {
                //Should all be small if the dest base is small
                u64 mask = bit_fill_lower<u64>(dest.size);

                //Mask off the parts of the value so that only the specific bytes are changed
                dest_base->small = ((dest_base->small >> (dest_ref.offset * 8)) & mask) | (mask & source_imm.small);
                break;
              }

            default: {
                if (source.size > 8) {
                  for (usize i = 0; i < source.size; i++) {
                    dest_base->large[dest_ref.offset + i] = source_imm.large[i];
                  }
                }
                else {
                  u64 val = source_imm.small;
                  //Remove the data as if it were little endian, the shift
                  for (u64 i = 0; i < source.size; i++, val >>= 8) {
                    dest_base->large[dest_ref.offset + i] = val & 0xff;
                  }
                }

                break;
              }
          }

          break;
        }
      case IR::Op::Add: {
          const IR::AddInst& add = inst.add;

          const IR::Value& dest = func->values.data[add.dest];
          const IR::Value& left = func->values.data[add.left];
          const IR::Value& right = func->values.data[add.right];

          IR::Primitive primitive = dest.primitive;
          usize size = dest.size;

          if (primitive != left.primitive || primitive != right.primitive) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Add instruction expects same primitive types\nDestination: {}, Left: {}, Right: {}", primitive_string(dest.primitive), primitive_string(left.primitive), primitive_string(right.primitive));
            return;
          }

          if (size != left.size || size != right.size) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Add instruction expects same sizes\nDestination: {}, Left: {}, Right: {}", dest.size, left.size, right.size);
            return;
          }

          validate_type_and_size(errors, primitive, size);
          if (errors->is_panic()) return;

          if (primitive != IR::Primitive::Int) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Add instruction expects integers (for now)");
            return;
          }

          RuntimeImmediate left_imm = eval_immediate(left);
          if (errors->is_panic()) return;

          RuntimeImmediate right_imm = eval_immediate(right);
          if (errors->is_panic()) return;

          ASSERT(size <= 8);

          const u64 mask = bit_fill_lower<u64>(size * 8);
          const u64 result = (left_imm.small + right_imm.small) & mask;

          RuntimeReference dest_ref = eval_reference(dest);
          if (errors->is_panic()) return;

          copy_number_to_reference_value(result, dest, dest_ref);

          break;
        }
      case IR::Op::Sub: {
          const IR::AddInst& add = inst.add;

          const IR::Value& dest = func->values.data[add.dest];
          const IR::Value& left = func->values.data[add.left];
          const IR::Value& right = func->values.data[add.right];

          IR::Primitive primitive = dest.primitive;
          usize size = dest.size;

          if (primitive != left.primitive || primitive != right.primitive) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Sub instruction expects same primitive types\nDestination: {}, Left: {}, Right: {}", primitive_string(dest.primitive), primitive_string(left.primitive), primitive_string(right.primitive));
            return;
          }

          if (size != left.size || size != right.size) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Sub instruction expects same sizes\nDestination: {}, Left: {}, Right: {}", dest.size, left.size, right.size);
            return;
          }

          validate_type_and_size(errors, primitive, size);
          if (errors->is_panic()) return;

          if (primitive != IR::Primitive::Int) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Sub instruction expects integers (for now)");
            return;
          }

          RuntimeImmediate left_imm = eval_immediate(left);
          if (errors->is_panic()) return;

          RuntimeImmediate right_imm = eval_immediate(right);
          if (errors->is_panic()) return;

          ASSERT(size <= 8);

          const u64 mask = bit_fill_lower<u64>(size * 8);
          const u64 result = (left_imm.small - right_imm.small) & mask;

          RuntimeReference dest_ref = eval_reference(dest);
          if (errors->is_panic()) return;

          copy_number_to_reference_value(result, dest, dest_ref);

          break;
        }
      case IR::Op::Mul: {
          const IR::AddInst& add = inst.add;

          const IR::Value& dest = func->values.data[add.dest];
          const IR::Value& left = func->values.data[add.left];
          const IR::Value& right = func->values.data[add.right];

          IR::Primitive primitive = dest.primitive;
          usize size = dest.size;

          if (primitive != left.primitive || primitive != right.primitive) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Mul instruction expects same primitive types\nDestination: {}, Left: {}, Right: {}", primitive_string(dest.primitive), primitive_string(left.primitive), primitive_string(right.primitive));
            return;
          }

          if (size != left.size || size != right.size) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Mul instruction expects same sizes\nDestination: {}, Left: {}, Right: {}", dest.size, left.size, right.size);
            return;
          }

          validate_type_and_size(errors, primitive, size);
          if (errors->is_panic()) return;

          if (primitive != IR::Primitive::Int) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Mul instruction expects integers (for now)");
            return;
          }

          RuntimeImmediate left_imm = eval_immediate(left);
          if (errors->is_panic()) return;

          RuntimeImmediate right_imm = eval_immediate(right);
          if (errors->is_panic()) return;

          ASSERT(size <= 8);

          const u64 mask = bit_fill_lower<u64>(size * 8llu);
          const u64 result = (left_imm.small * right_imm.small) & mask;

          RuntimeReference dest_ref = eval_reference(dest);
          if (errors->is_panic()) return;

          copy_number_to_reference_value(result, dest, dest_ref);

          break;
        }
      case IR::Op::Div: {
          const IR::AddInst& add = inst.add;

          const IR::Value& dest = func->values.data[add.dest];
          const IR::Value& left = func->values.data[add.left];
          const IR::Value& right = func->values.data[add.right];

          IR::Primitive primitive = dest.primitive;
          usize size = dest.size;

          if (primitive != left.primitive || primitive != right.primitive) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Add instruction expects same primitive types\nDestination: {}, Left: {}, Right: {}", primitive_string(dest.primitive), primitive_string(left.primitive), primitive_string(right.primitive));
            return;
          }

          if (size != left.size || size != right.size) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Add instruction expects same sizes\nDestination: {}, Left: {}, Right: {}", dest.size, left.size, right.size);
            return;
          }

          validate_type_and_size(errors, primitive, size);
          if (errors->is_panic()) return;

          if (primitive != IR::Primitive::Int) {
            errors->report_error(ERROR_CODE::IR_ERROR, Span{}, "Add instruction expects integers (for now)");
            return;
          }

          RuntimeImmediate left_imm = eval_immediate(left);
          if (errors->is_panic()) return;

          RuntimeImmediate right_imm = eval_immediate(right);
          if (errors->is_panic()) return;

          ASSERT(size <= 8);

          const u64 mask = bit_fill_lower<u64>(size * 8);
          const u64 result = (left_imm.small / right_imm.small) & mask;

          RuntimeReference dest_ref = eval_reference(dest);
          if (errors->is_panic()) return;

          copy_number_to_reference_value(result, dest, dest_ref);

          break;
        }
      case IR::Op::Call:
      case IR::Op::Return: {
          //TODO call stack
          //For now just return out of this function
          return;
        }
      default: {
          errors->report_error(ERROR_CODE::INTERNAL_ERROR, Span{}, "Unsupported instruction with opcode {}", (u32)inst.op);
          return;
        }
    }
  }
}