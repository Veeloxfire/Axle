#include "backends.h"
#include "compiler.h"
#include "calling_conventions.h"
#include "format.h"

#include <stdio.h>

static void relocation_fix(uint8_t* code,
                           const uint8_t* data_base,
                           const Relocation* reloc,
                           size_t* label_indexes) {
  u8* ptr = code + reloc->value_offset;

  switch (reloc->type) {
    case RELOCATION_TYPE::U64_LABEL_OFFSET: {
        const u64 label = x64_from_bytes(ptr);
        x64_to_bytes(label_indexes[label], ptr);
        break;
      }
    case RELOCATION_TYPE::U64_DATA_OFFSET: {
        //Shouldnt be using non-const globals if we have no data
        assert(data_base != nullptr);
        const u64 offset = x64_from_bytes(ptr);
        x64_to_bytes(data_base + offset, ptr);
        break;
      }
    case RELOCATION_TYPE::I32_RELATIVE_TO_NEXT: {
        const u32 label = x32_from_bytes(ptr);
        x32_to_bytes((i32)((i64)label_indexes[label] - (i64)reloc->other_offset), ptr);
        break;
      }
    default: {
        //Should not be here
        assert(false);
      }
  }
}


void compile_backend_single_func(Program* prog, const CodeBlock* code, Compiler* const comp, const System* system) {
  Array<uint8_t> out_code ={};

  size_t* const label_indexes = allocate_default<size_t>(comp->labels);
  DEFER(&) { free_no_destruct(label_indexes); };

  Array<Relocation> relocations ={};

  size_t entry_point_label = 0;

  system->backend_translate(comp, prog, out_code, code, label_indexes, relocations);
  if (comp->is_panic()) {
    return;
  }


  {
    auto reloc_i = relocations.begin();
    const auto reloc_end = relocations.end();

    for (; reloc_i < reloc_end; reloc_i++) {
      relocation_fix(out_code.data, nullptr, reloc_i, label_indexes);
    }
  }

  prog->entry_point = label_indexes[code->label];


  prog->code_size = out_code.size;
  prog->code = std::move(out_code);
}

void vm_backend_code_block(Compiler* const,//compiler is currently not used in this version but is in the x86_64
                           Program* prog,
                           Array<uint8_t>& out_code,
                           const CodeBlock* code,
                           size_t* label_indexes,
                           Array<Relocation>& relocations) {

  auto code_i = code->code.begin();
  const auto code_end = code->code.end();

  while (code_i < code_end) {
    switch (*code_i) {
      case ByteCode::LOAD_GLOBAL_MEM: {
          //Load the global as an index into the data
          const auto p_g = ByteCode::PARSE::LOAD_GLOBAL_MEM(code_i);

          const Global* glob = p_g.u64;

          const auto offset = out_code.size + 2;
          ByteCode::EMIT::SET_R64_TO_64(out_code, p_g.val, glob->data_index);
          relocations.insert({ RELOCATION_TYPE::U64_DATA_OFFSET, offset, out_code.size });

          code_i += ByteCode::SIZE_OF::LOAD_GLOBAL_MEM;
          break;
        }
      case ByteCode::RESERVE: {
          code_i += ByteCode::SIZE_OF::RESERVE;
          break;
        }
      case ByteCode::LABEL: {
          const auto p = ByteCode::PARSE::LABEL(code_i);

          label_indexes[p.u64.val] = out_code.size;

          code_i += ByteCode::SIZE_OF::LABEL;
          break;
        }
      case ByteCode::JUMP_TO_FIXED: {
          const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED(code_i);
          code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED;

          //Can ignore anything between a fixed jump and a label - it wont every be reached
          while (code_i < code_end && code_i[0] != ByteCode::LABEL) {
            code_i += ByteCode::instruction_size(code_i[0]);
          }

          size_t next = 0;

          //Could be series of labels so needs to while loop
          while (code_i + next < code_end && code_i[next] == ByteCode::LABEL) {
            const auto p_l = ByteCode::PARSE::LABEL(code_i + next);

            if (p_l.u64.val == p_j.u64.val) {
              //About to jump to next instruction - can ignore jump
              goto SKIP_JUMP;
            }

            //Next might also be a label
            next += ByteCode::SIZE_OF::LABEL;
          }

          {
            const size_t offset = out_code.size + 1;
            ByteCode::EMIT::JUMP_TO_FIXED(out_code, p_j.u64);
            relocations.insert({ RELOCATION_TYPE::U64_LABEL_OFFSET, offset, out_code.size });
          }

        SKIP_JUMP:
          break;
        }
      /*case ByteCode::CALL_NATIVE_X64: {
          const auto p_c = ByteCode::PARSE::CALL_NATIVE_X64(code_i);


          relocations.insert({RELOCATION_TYPE::U64_DATA_OFFSET, out_code.size, 1});

          ByteCode::EMIT::CALL_NATIVE_X64(out_code, p_c.u64_1, p_c.u64_2);
          code_i += ByteCode::SIZE_OF::CALL_NATIVE_X64;
          break;
        }*/
      case ByteCode::CALL: {
          const auto p_c = ByteCode::PARSE::CALL(code_i);
          const Function* func = p_c.u64;

          {
            const size_t offset = out_code.size + 1;
            //Switch to a code label rather than func ptr
            ByteCode::EMIT::CALL(out_code, func->code_block.label);
            relocations.insert({ RELOCATION_TYPE::U64_LABEL_OFFSET, offset, out_code.size });
          }


          code_i += ByteCode::SIZE_OF::CALL;
          break;
        }
      case ByteCode::CALL_LABEL: {
          const auto p_c = ByteCode::PARSE::CALL_LABEL(code_i);

          {
            const size_t offset = out_code.size + 1;
            ByteCode::EMIT::CALL(out_code, p_c.u64);
            relocations.insert({ RELOCATION_TYPE::U64_LABEL_OFFSET, offset, out_code.size });
          }

          code_i += ByteCode::SIZE_OF::CALL_LABEL;
          break;
        }
      case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO:
      case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
          const auto p_j = ByteCode::OP_R_64::parse(code_i);

          //Could try checking for redundant jumps maybe
          //but they shouldnt ever jump to the next instruction anyway
          //Would be checking for something that probably never happens

          const size_t offset = out_code.size + 2;
          p_j.emit(out_code, p_j.op, p_j.val, p_j.u64);
          relocations.insert({ RELOCATION_TYPE::U64_LABEL_OFFSET, offset, out_code.size });
          code_i += p_j.INSTRUCTION_SIZE;
          break;
        }
      default: {
          const size_t i_size = ByteCode::instruction_size(*code_i);
          out_code.insert_uninit(i_size);
          memcpy_ts(out_code.data + out_code.size - i_size, i_size, code_i, i_size);

          code_i += i_size;
          break;
        }
    }
  }
}

void compile_backend(Program* prog, Compiler* comp, const System* system) {
  Array<uint8_t> out_code ={};

  CodeBlock actual_entry_function ={};
  actual_entry_function.label = comp->labels++;

  size_t* const label_indexes = allocate_default<size_t>(comp->labels);
  DEFER(&) { free_no_destruct(label_indexes); };

  Array<Relocation> relocations ={};

  size_t entry_point_label = 0;

  //Find the entry point first - allows us to say its called
  {
    const InternString* entry_name = comp->build_options.entry_point;

    NamedElement* el = comp->services.names->find_unimported_name(comp->build_file_namespace, entry_name);

    if (el == nullptr) {
      CallSignature sig ={};
      sig.name = entry_name;

      comp->report_error(ERROR_CODE::NAME_ERROR, Span{},
                         "No function '{}' exists in the build file to be the entry point",
                         sig);
      return;
    }

    if (el->unknowns > 0) {
      comp->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                         "Some declarations with name '{}' were not compiled\n"
                         "The compiler should not be linking if this is the case ...",
                         entry_name);
      return;
    }

    Function* entry_point_func = nullptr;

    {
      auto i = el->globals.begin();
      auto end = el->globals.end();

      for (; i < end; i++) {
        const Global* g = *i;

        if (g->decl.type->type != STRUCTURE_TYPE::LAMBDA) {
          continue;
        }

        const SignatureStructure* sig = (const SignatureStructure*)g->decl.type;

        if (!(sig->parameter_types.size == 0 && sig->return_type == comp->services.types->s_u64)) {
          continue;
        }

        //The function will be correct from now on

        if (entry_point_func != nullptr) {
          comp->report_error(ERROR_CODE::NAME_ERROR, Span{},
                             "There are multiple functions elligble to be the entry point");
          return;
        }

        entry_point_func = *(Function**)g->constant_value.ptr;
      }
    }

    if (entry_point_func == nullptr) {
      comp->report_error(ERROR_CODE::NAME_ERROR, Span{},
                         "Could not find a function '{}' in the build file for the entry point",
                         entry_name);
      return;
    }

    entry_point_func->is_called = true;//makes it compile
    entry_point_label = entry_point_func->code_block.label;
  }

  {
    auto func_i = comp->functions.begin_const_iter();
    const auto func_end = comp->functions.end_const_iter();

    for (; func_i != func_end; func_i.next()) {
      const Function* const func = func_i.get();
      if (!func->is_called) {
        //Isnt called
        continue;
      }

      if (func->func_type == FUNCTION_TYPE::DEFAULT) {
        const CodeBlock* const code = &func->code_block;

        system->backend_translate(comp, prog, out_code, code, label_indexes, relocations);
        if (comp->is_panic()) {
          return;
        }
      }
      else if (func->func_type == FUNCTION_TYPE::EXTERN
               && system == &system_vm) {
        CodeBlock block ={};
        block.label = comp->labels++;

        u64 stack_size = 0;
        {
          const u64 num_params = func->signature.sig_struct->actual_parameter_types.size;
          const u64 num_regs = convention_microsoft_x64.num_parameter_registers;
          if (num_params > num_regs) {
            stack_size = 8 * (num_params - num_regs);
          }
        }

        ByteCode::EMIT::CALL_NATIVE_X64(block.code,
                                        prog->data.ptr + func->data_index,
                                        stack_size);
        ByteCode::EMIT::RETURN(block.code);

        system->backend_translate(comp, prog, out_code, &block, label_indexes, relocations);
        if (comp->is_panic()) {
          return;
        }
      }
      else {
        assert(false);
      }

    }

    //Init all the globals - then call the main function
    {
      auto i = comp->globals.begin_iter();
      auto end = comp->globals.end_iter();

      for (; i != end; i.next()) {
        Global* glob = i.get();

        if (glob->constant_value.ptr == nullptr) {
          system->backend_translate(comp, prog, out_code, &glob->init, label_indexes, relocations);
          if (comp->is_panic()) {
            return;
          }
        }
      }

      //Just init calls
      i = comp->globals.begin_iter();
      for (; i != end; i.next()) {
        Global* glob = i.get();

        if (glob->constant_value.ptr == nullptr) {
          ByteCode::EMIT::CALL_LABEL(actual_entry_function.code, glob->init.label);
        }
      }

      ByteCode::EMIT::CALL_LABEL(actual_entry_function.code, entry_point_label);
      ByteCode::EMIT::RETURN(actual_entry_function.code);


      //Entry before emitting possible inits
      prog->entry_point = out_code.size;
      system->backend_translate(comp, prog, out_code, &actual_entry_function, label_indexes, relocations);
      if (comp->is_panic()) {
        return;
      }
    }
  }

  {
    auto reloc_i = relocations.begin();
    const auto reloc_end = relocations.end();

    for (; reloc_i < reloc_end; reloc_i++) {
      relocation_fix(out_code.data, prog->data.ptr, reloc_i, label_indexes);
    }
  }


  prog->code_size = out_code.size;
  prog->code = std::move(out_code);
}

void X64::mov(Array<uint8_t>& arr,
              uint8_t from,
              uint8_t to) {
  arr.insert(X64::REX_W | X64::rex_r_rm(from, to));
  arr.insert(X64::MOV_R_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(from, to));
}

static void emit_sib(Array<uint8_t>& arr, const X64::SIB& sib, uint8_t mod_byte) {
  if (!sib.use_base) {
    assert(!sib.use_index);//Must use both if using index

    if (!sib.use_index) {
      arr.insert(0b00'000'000 | mod_byte);
      arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, RBP.REG));

      arr.reserve_extra(4);

      memcpy_ts(arr.data + arr.size, arr.capacity - arr.size,
                (const uint8_t*)&sib.disp, 4);

      arr.size += 4;
    }
    else {
      assert(sib.index != RSP.REG);//Not a valid code unfortunately

      arr.insert(0b00'000'000 | mod_byte);
      arr.insert(X64::sib(sib.scale, sib.index, RBP.REG));

      arr.reserve_extra(4);

      memcpy_ts(arr.data + arr.size, arr.capacity - arr.size,
                (const uint8_t*)&sib.disp, 4);

      arr.size += 4;
    }
  }
  else if (sib.use_base && !sib.use_index) {
    if (sib.disp == 0 && (sib.base & 0b111) != RBP.REG) {
      arr.insert(0b00'000'000 | mod_byte);
      arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, sib.base));
    }
    else if (-128 <= sib.disp  && sib.disp <= 127) {
      arr.insert(0b01'000'000 | mod_byte);
      arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, sib.base));
      arr.insert((uint8_t)sib.disp);
    }
    else {
      arr.insert(0b10'000'000 | mod_byte);
      arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, sib.base));

      arr.reserve_extra(4);

      memcpy_ts(arr.data + arr.size, arr.capacity - arr.size,
                (const uint8_t*)&sib.disp, 4);

      arr.size += 4;
    }
  }
  else {
    //use base and index

    if (sib.disp == 0 && (sib.base & 0b111) != RBP.REG) {
      arr.insert(0b00'000'000 | mod_byte);
      arr.insert(X64::sib(sib.scale, sib.index, sib.base));
    }
    else if (-128 <= sib.disp  && sib.disp <= 127) {
      arr.insert(0b01'000'000 | mod_byte);
      arr.insert(X64::sib(sib.scale, sib.index, sib.base));
      arr.insert((uint8_t)sib.disp);
    }
    else {
      arr.insert(0b10'000'000 | mod_byte);
      arr.insert(X64::sib(sib.scale, sib.index, sib.base));

      arr.reserve_extra(4);

      memcpy_ts(arr.data + arr.size, arr.capacity - arr.size,
                (const uint8_t*)&sib.disp, 4);

      arr.size += 4;
    }
  }
}

static void emit_mod_rm(Array<uint8_t>& arr, const X64::R r, const X64::RM& rm) {
  if (!rm.indirect) {
    arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(r.r, rm.r));
    return;
  }

  switch (rm.r) {
    case RSP.REG:
    case R12.REG: {
        //SIB byte time

        if (rm.use_sib) {
          emit_sib(arr, rm.sib, X64::modrm_r_rm(r.r, rm.r));
        }
        else if (rm.disp == 0) {
          arr.insert(X64::MODRM_MOD_INDIRECT | X64::modrm_r_rm(r.r, rm.r));
          arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, rm.r));
        }
        else if (-128 <= rm.disp  && rm.disp <= 127) {
          arr.insert(0b01000000 | X64::modrm_r_rm(r.r, rm.r));
          arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, rm.r));
          arr.insert((int8_t)rm.disp);
        }
        else {
          arr.insert(0b10000000 | X64::modrm_r_rm(r.r, rm.r));
          arr.insert(X64::SIB_SCALE_1 | X64::sib_i_b(RSP.REG, rm.r));
          arr.reserve_extra(4);

          memcpy_ts(arr.data + arr.size, arr.capacity - arr.size,
                    (const uint8_t*)&rm.disp, 4);
          arr.size += 4;
        }
        break;
      }

    case RBP.REG:
    case R13.REG: {
        //Encode disp 0 by having a 1 byte disp of 0
        goto RM_DEFAULT;
      }

    default: {
        if (rm.disp == 0) {
          arr.insert(X64::MODRM_MOD_INDIRECT | X64::modrm_r_rm(r.r, rm.r));
          break;
        }

      RM_DEFAULT:
        if (-128 <= rm.disp  && rm.disp <= 127) {
          arr.insert(0b01000000 | X64::modrm_r_rm(r.r, rm.r));
          arr.insert((int8_t)rm.disp);
          break;
        }
        else {
          arr.insert(0b10000000 | X64::modrm_r_rm(r.r, rm.r));
          arr.reserve_extra(4);

          memcpy_ts(arr.data + arr.size, arr.capacity - arr.size,
                    (const uint8_t*)&rm.disp, 4);
          arr.size += 4;
          break;
        }
      }
  }
}

X64::RM X64::rm_from_mem_complex(const MemComplex& mem) {
  X64::RM rm ={};
  rm.indirect = true;

  if (mem.scale == 0) {
    if ((mem.base & 0b111) != RSP.REG) {
      rm.use_sib = false;
      rm.r = mem.base;
      rm.disp = mem.disp;
    }
    else {
      //have to encode using SIB unfortunately
      rm.r = RSP.REG;
      rm.use_sib = true;

      rm.sib.use_base = true;
      rm.sib.use_index = false;
      rm.sib.base = RSP.REG;
      rm.sib.disp = mem.disp;
    }
  }
  else {
    rm.r = RSP.REG;
    rm.use_sib = true;

    assert((mem.base & 0b111) != RSP.REG);//Not possible in x86 architecture


    rm.sib.use_base = true;
    rm.sib.use_index = true;
    rm.sib.base = mem.base;
    rm.sib.disp = mem.disp;
    rm.sib.scale = mem.scale;
    rm.sib.index = mem.index;
  }

  return rm;
}

void X64::mov(Array<uint8_t>& arr,
              R r,
              const RM& rm) {

  arr.insert(X64::REX_W | X64::rex_r_rm(r.r, rm.r));
  arr.insert(X64::MOV_R_TO_RM);

  emit_mod_rm(arr, r, rm);
}

void X64::mov(Array<uint8_t>& arr,
              R8 r8,
              const RM8& rm8) {
  R r = r8.r;
  const RM& rm = rm8.rm;

  if ((r.r & 0b1000) > 0 || (rm.r & 0b1000) > 0) {
    arr.insert(X64::REX | X64::rex_r_rm(r.r, rm.r));
  }

  arr.insert(X64::MOV_R8_TO_RM8);

  emit_mod_rm(arr, r, rm);
}

void X64::mov(Array<uint8_t>& arr,
              const RM8& rm8,
              R8 r8) {
  R r = r8.r;
  const RM& rm = rm8.rm;

  if ((r.r & 0b1000) > 0 || (rm.r & 0b1000) > 0) {
    arr.insert(X64::REX | X64::rex_r_rm(r.r, rm.r));
  }

  arr.insert(X64::MOV_RM8_TO_R8);

  emit_mod_rm(arr, r, rm);
}

void X64::lea(Array<uint8_t>& arr,
              const RM& rm,
              R r) {
  arr.insert(X64::REX_W | X64::rex_r_rm(r.r, rm.r));
  arr.insert(X64::LEA_RM_TO_R);

  emit_mod_rm(arr, r, rm);
}

void X64::mov(Array<uint8_t>& arr,
              const RM& rm,
              R r) {
  arr.insert(X64::REX_W | X64::rex_r_rm(r.r, rm.r));
  arr.insert(X64::MOV_RM_TO_R);

  emit_mod_rm(arr, r, rm);
}

void X64::mov(Array<uint8_t>& arr,
              R r,
              uint64_t u64) {
  arr.insert(X64::REX_W | X64::rex_rm(r.r));//yes rm is correct
  arr.insert(X64::MOV_64_TO_R + (r.r & 0b111));

  arr.reserve_extra(sizeof(uint64_t));
  x64_to_bytes(u64, arr.data + arr.size);
  arr.size += sizeof(uint64_t);
}

void X64::mov(Array<uint8_t>& arr,
              R8 r8,
              uint8_t u8) {
  R r = r8.r;

  if ((r.r & 0b1000) > 0) {
    arr.insert(X64::REX | X64::rex_rm(r.r));//yes rm is correct
  }

  arr.insert(X64::MOV_8_TO_R8 + (r.r & 0b111));
  arr.insert(u8);
}

void X64::mov(Array<uint8_t>& arr,
              const RM& rm,
              IMM32 u32) {
  if ((rm.r & 0b1000) > 0 || u32.sign_extend) {
    const uint8_t rex = (uint8_t)(u32.sign_extend ? X64::REX_W : X64::REX);
    arr.insert(rex | X64::rex_b(rm.r));
  }

  arr.insert(X64::MOV_IMM32_RM);

  emit_mod_rm(arr, R{ '\0' }, rm);

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(u32.imm, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

void X64::mov(Array<uint8_t>& arr,
              const RM& rm,
              u16 imm16) {
  arr.insert(X64::OVERIDE_OPERAND);

  if ((rm.r & 0b1000) > 0) {
    arr.insert(X64::REX | X64::rex_b(rm.r));
  }

  arr.insert(X64::MOV_IMM32_RM);

  emit_mod_rm(arr, R{ '\0' }, rm);

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(imm16, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

void X64::sub(Array<uint8_t>& arr,
              uint8_t r,
              uint8_t rm) {
  arr.insert(X64::REX_W | X64::rex_r_rm(r, rm));
  arr.insert(X64::SUB_R_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT
             | X64::modrm_r_rm(r, rm));
}

void X64::sub(Array<uint8_t>& arr,
              uint8_t rm,
              int32_t i32) {
  arr.insert(X64::REX_W | X64::rex_rm(rm));
  arr.insert(X64::SUB_32_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(5, rm));

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(i32, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

void X64::push(Array<uint8_t>& arr, uint8_t reg) {
  if ((reg & 0b0000'1000) > 0) {
    arr.insert(X64::REX_W | X64::REX_B);
  }

  arr.insert(X64::PUSH_R + (reg & 0b0000'0111));
}

void X64::pop(Array<uint8_t>& arr, uint8_t reg) {
  if ((reg & 0b0000'1000) > 0) {
    arr.insert(X64::REX_W | X64::REX_B);
  }

  arr.insert(X64::POP_R + (reg & 0b0000'0111));
}

void X64::ret(Array<uint8_t>& arr) {
  arr.insert(X64::RET_NEAR);
}

inline static void jump_near(Array<Relocation>& relocs, Array<uint8_t>& arr, int32_t relative) {
  arr.reserve_extra(1 + sizeof(relative));

  arr.insert(X64::JMP_NEAR);

  relocs.insert({ RELOCATION_TYPE::I32_RELATIVE_TO_NEXT, arr.size, arr.size + 4 });

  x32_to_bytes((uint32_t)relative, arr.data + arr.size);
  arr.size += sizeof(relative);
}

inline static void call_near(Array<Relocation>& relocs, Array<uint8_t>& arr, int32_t relative) {
  arr.reserve_extra(1 + sizeof(relative));

  arr.insert(X64::CALL_NEAR);

  relocs.insert({ RELOCATION_TYPE::I32_RELATIVE_TO_NEXT, arr.size, arr.size + 4 });

  x32_to_bytes((uint32_t)relative, arr.data + arr.size);
  arr.size += sizeof(relative);
}

static void jump_zero(Array<Relocation>& relocs, Array<uint8_t>& arr, int32_t relative) {
  arr.insert(0x0F);
  arr.insert(X64::JZ_NEAR);

  relocs.insert({ RELOCATION_TYPE::I32_RELATIVE_TO_NEXT, arr.size, arr.size + 4 });

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(relative, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

static constexpr auto jump_equal = jump_zero;

static void jump_not_equal(Array<Relocation>& relocs, Array<uint8_t>& arr, int32_t relative) {
  arr.insert(0x0F);
  arr.insert(X64::JNE_NEAR);

  relocs.insert({ RELOCATION_TYPE::I32_RELATIVE_TO_NEXT, arr.size, arr.size + 4 });

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(relative, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

static void jump_above(Array<Relocation>& relocs, Array<uint8_t>& arr, int32_t relative) {
  arr.insert(0x0F);
  arr.insert(X64::JA_NEAR);

  relocs.insert({ RELOCATION_TYPE::I32_RELATIVE_TO_NEXT, arr.size, arr.size + 4 });

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(relative, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

static void jump_not_above(Array<Relocation>& relocs, Array<uint8_t>& arr, int32_t relative) {
  arr.insert(0x0F);
  arr.insert(X64::JNA_NEAR);

  relocs.insert({ RELOCATION_TYPE::I32_RELATIVE_TO_NEXT, arr.size, arr.size + 4 });

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(relative, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

static void jump_below(Array<Relocation>& relocs, Array<uint8_t>& arr, int32_t relative) {
  arr.insert(0x0F);
  arr.insert(X64::JB_NEAR);

  relocs.insert({ RELOCATION_TYPE::I32_RELATIVE_TO_NEXT, arr.size, arr.size + 4 });

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(relative, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

static void jump_not_below(Array<Relocation>& relocs, Array<uint8_t>& arr, int32_t relative) {
  arr.insert(0x0F);
  arr.insert(X64::JNB_NEAR);

  relocs.insert({ RELOCATION_TYPE::I32_RELATIVE_TO_NEXT, arr.size, arr.size + 4 });

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(relative, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

static void jump_lesser(Array<Relocation>& relocs, Array<uint8_t>& arr, int32_t relative) {
  arr.insert(0x0F);
  arr.insert(X64::JL_NEAR);

  relocs.insert({ RELOCATION_TYPE::I32_RELATIVE_TO_NEXT, arr.size, arr.size + 4 });

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(relative, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

static void jump_not_lesser(Array<Relocation>& relocs, Array<uint8_t>& arr, int32_t relative) {
  arr.insert(0x0F);
  arr.insert(X64::JNL_NEAR);

  relocs.insert({ RELOCATION_TYPE::I32_RELATIVE_TO_NEXT, arr.size, arr.size + 4 });

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(relative, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

static void jump_greater(Array<Relocation>& relocs, Array<uint8_t>& arr, int32_t relative) {
  arr.insert(0x0F);
  arr.insert(X64::JG_NEAR);

  relocs.insert({ RELOCATION_TYPE::I32_RELATIVE_TO_NEXT, arr.size, arr.size + 4 });

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(relative, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

static void jump_not_greater(Array<Relocation>& relocs, Array<uint8_t>& arr, int32_t relative) {
  arr.insert(0x0F);
  arr.insert(X64::JNG_NEAR);

  relocs.insert({ RELOCATION_TYPE::I32_RELATIVE_TO_NEXT, arr.size, arr.size + 4 });

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(relative, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

static void cmp(Array<uint8_t>& arr, uint8_t r, uint8_t rm) {
  arr.insert(X64::REX_W | X64::rex_r_rm(r, rm));
  arr.insert(X64::CMP_R_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT
             | X64::modrm_r_rm(r, rm));
}

static void cmp(Array<uint8_t>& arr, uint8_t rm, int32_t i32) {
  arr.insert(X64::REX_W | X64::rex_rm(rm));
  arr.insert(X64::CMP_32_TO_RM);
  arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(7, rm));

  arr.reserve_extra(sizeof(uint32_t));
  x32_to_bytes(i32, arr.data + arr.size);
  arr.size += sizeof(uint32_t);
}

static void sete(Array<u8>& arr, u8 r) {
  arr.insert(X64::REX | X64::rex_rm(r));
  arr.insert(0x0F);
  arr.insert(X64::SETE_RM8);
  arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_rm(r));
}

static void setl(Array<u8>& arr, u8 r) {
  arr.insert(X64::REX | X64::rex_rm(r));
  arr.insert(0x0F);
  arr.insert(X64::SETL_RM8);
  arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_rm(r));
}

static void setg(Array<u8>& arr, u8 r) {
  arr.insert(X64::REX | X64::rex_rm(r));
  arr.insert(0x0F);
  arr.insert(X64::SETG_RM8);
  arr.insert(X64::MODRM_MOD_DIRECT | X64::modrm_rm(r));
}

static void check_cmp_jump(Array<Relocation>& relocs,
                           Array<uint8_t>& out_code,
                           const uint8_t** code_i_ptr, const uint8_t* end,
                           FUNCTION_PTR<void, Array<Relocation>&, Array<u8>&, i32> success_jump_func,
                           FUNCTION_PTR<void, Array<Relocation>&, Array<u8>&, i32> fail_jump_func,
                           FUNCTION_PTR<void, Array<u8>&, u8> no_jump) {

  const uint8_t* const code_i = *code_i_ptr;
  const uint8_t* const code_i_2 = code_i + ByteCode::SIZE_OF::EQ_R64S;

  switch (*code_i_2) {
    case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
        const auto p_e = ByteCode::OP_R_R::parse(code_i);
        const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_ZERO(code_i_2);

        cmp(out_code, p_e.val1, p_e.val2);

        fail_jump_func(relocs, out_code, (int32_t)p_j.u64.val);

        *code_i_ptr = code_i_2 + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_ZERO;
        break;
      }
    case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
        const auto p_e = ByteCode::OP_R_R::parse(code_i);
        const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i_2);

        cmp(out_code, p_e.val1, p_e.val2);

        success_jump_func(relocs, out_code, (int32_t)p_j.u64.val);

        *code_i_ptr = code_i_2 + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
        break;
      }
    default: {
        const auto p = ByteCode::OP_R_R::parse(code_i);

        cmp(out_code, p.val1, p.val2);
        no_jump(out_code, p.val2);

        //Clear the top of the register
        out_code.insert(X64::REX_W | X64::rex_r_rm(p.val2, p.val2));
        out_code.insert(0x0F);
        out_code.insert(X64::MOV_ZX_RM8_TO_R);
        out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(p.val2, p.val2));


        *code_i_ptr = code_i_2;
        break;
      }
  }
}

static void check_for_jumps(Array<Relocation>& relocs, Array<uint8_t>& out_code, uint8_t val, const uint8_t** code_i_ptr, const uint8_t* end) {

  const uint8_t* code_i = *code_i_ptr;

  if (code_i >= end) {
    return;
  }

  switch (*code_i) {
    case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
        const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

        if (val != p.val) {
          cmp(out_code, p.val, (int32_t)0);
        }

        jump_zero(relocs, out_code, (int32_t)p.u64.val);

        *code_i_ptr = code_i + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
        break;
      }
    case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
        const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

        if (val != p.val) {
          cmp(out_code, p.val, (int32_t)0);
        }

        jump_not_equal(relocs, out_code, (int32_t)p.u64.val);

        *code_i_ptr = code_i + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
        break;
      }

    default:
      return;
  }
}

void x86_64_backend_code_block(Compiler* const comp,
                               Program* prog,
                               Array<uint8_t>& out_code,
                               const CodeBlock* code,
                               size_t* label_indexes,
                               Array<Relocation>& relocs) {
  auto code_i = code->code.begin();
  const auto code_end = code->code.end();

  while (code_i < code_end) {
    switch (*code_i) {
      case ByteCode::RESERVE: {
          code_i += ByteCode::SIZE_OF::RESERVE;
          break;
        }
      case ByteCode::COPY_R64_TO_R64: {
          const auto p = ByteCode::PARSE::COPY_R64_TO_R64(code_i);
          X64::mov(out_code, p.val1, p.val2);
          code_i += ByteCode::SIZE_OF::COPY_R64_TO_R64;
          break;
        }
      case ByteCode::COPY_R8_TO_R8: {
          const auto p = ByteCode::PARSE::COPY_R8_TO_R8(code_i);

          X64::RM rm ={};
          rm.r = p.val2;
          rm.indirect = false;

          X64::mov(out_code, X64::R8{ X64::R{p.val1} }, X64::RM8{ rm });
          code_i += ByteCode::SIZE_OF::COPY_R8_TO_R8;
          break;
        }
      case ByteCode::LOAD_GLOBAL_MEM: {
          const auto p = ByteCode::PARSE::SET_R64_TO_64(code_i);
          const Global* glob = (const Global*)p.u64.vptr;

          assert(glob->constant_value.ptr == nullptr);
          assert( glob->data_index != 0);

          X64::mov(out_code, X64::R{ p.val }, glob->data_index);

          relocs.insert({RELOCATION_TYPE::U64_DATA_OFFSET, out_code.size - 8, out_code.size});

          code_i += ByteCode::SIZE_OF::SET_R64_TO_64;
          break;
        }
      case ByteCode::SET_R64_TO_64: {
          const auto p = ByteCode::PARSE::SET_R64_TO_64(code_i);
          X64::mov(out_code, X64::R{ p.val }, p.u64);
          code_i += ByteCode::SIZE_OF::SET_R64_TO_64;
          break;
        }
      case ByteCode::SET_R8_TO_8: {
          const auto p = ByteCode::PARSE::SET_R8_TO_8(code_i);

          X64::mov(out_code, X64::R8{ X64::R{ p.val } }, p.u8);
          code_i += ByteCode::SIZE_OF::SET_R8_TO_8;
          break;
        }
      case ByteCode::ADD_R64S: {
          const auto p = ByteCode::PARSE::ADD_R64S(code_i);

          out_code.insert(X64::REX_W | X64::rex_r_rm(p.val1, p.val2));
          out_code.insert(X64::ADD_R_TO_RM);
          out_code.insert(X64::MODRM_MOD_DIRECT
                          | X64::modrm_r_rm(p.val1, p.val2));

          code_i += ByteCode::SIZE_OF::ADD_R64S;

          check_for_jumps(relocs, out_code, p.val2, &code_i, code_end);
          break;
        }
      case ByteCode::SUB_R64S: {
          const auto p = ByteCode::PARSE::SUB_R64S(code_i);

          X64::sub(out_code, p.val1, p.val2);

          code_i += ByteCode::SIZE_OF::SUB_R64S;

          check_for_jumps(relocs, out_code, p.val2, &code_i, code_end);
          break;
        }
      case ByteCode::MUL_R64S: {
          const auto p = ByteCode::PARSE::MUL_R64S(code_i);

          //Can use IMUL here even though it might be unsigned (only important if you need the overflow??)
          out_code.insert(X64::REX_W | X64::rex_r_rm(p.val2, p.val1));
          out_code.insert(0x0F);
          out_code.insert(X64::IMUL_RM_TO_R);
          out_code.insert(X64::MODRM_MOD_DIRECT
                          | X64::modrm_r_rm(p.val2, p.val1));

          code_i += ByteCode::SIZE_OF::MUL_R64S;

          check_for_jumps(relocs, out_code, p.val2, &code_i, code_end);
          break;
        }
      case ByteCode::DIV_RU64S: {
          const auto p = ByteCode::PARSE::DIV_RU64S(code_i);

          assert(p.val2 == RAX.REG);

          //Set RDX to 0
          X64::mov(out_code, X64::R{ RDX.REG }, 0ull);

          out_code.insert(X64::REX_W | X64::rex_rm(p.val1));
          out_code.insert(X64::DIV_RM_TO_RAX);
          out_code.insert(X64::MODRM_MOD_DIRECT
                          | X64::modrm_r_rm(6, p.val1));

          code_i += ByteCode::SIZE_OF::DIV_RU64S;

          check_for_jumps(relocs, out_code, 0, &code_i, code_end);
          break;
        }
      case ByteCode::DIV_RI64S: {
          const auto p = ByteCode::PARSE::DIV_RI64S(code_i);

          assert(p.val2 == RAX.REG);

          //Sign extend to RDX
          out_code.insert(X64::REX_W);
          out_code.insert(X64::CQO);

          out_code.insert(X64::REX_W | X64::rex_rm(p.val1));
          out_code.insert(X64::IDIV_RM_TO_RAX);
          out_code.insert(X64::MODRM_MOD_DIRECT
                          | X64::modrm_r_rm(7, p.val1));

          code_i += ByteCode::SIZE_OF::DIV_RI64S;

          check_for_jumps(relocs, out_code, 0, &code_i, code_end);
          break;
        }
      case ByteCode::SHIFT_L_BY_R8_R64: {
          const auto p = ByteCode::PARSE::SHIFT_L_BY_R8_R64(code_i);

          assert(p.val1 == RCX.REG);

          out_code.insert(X64::REX_W | X64::rex_rm(p.val2));
          out_code.insert(X64::SAL_R_BY_CL);
          out_code.insert(X64::MODRM_MOD_DIRECT
                          | X64::modrm_r_rm(7, p.val2));

          code_i += ByteCode::SIZE_OF::SHIFT_L_BY_R8_R64;

          check_for_jumps(relocs, out_code, 0, &code_i, code_end);
          break;
        }
      case ByteCode::SHIFT_R_BY_R8_RU64: {
          const auto p = ByteCode::PARSE::SHIFT_R_BY_R8_RU64(code_i);

          assert(p.val1 == RCX.REG);

          out_code.insert(X64::REX_W | X64::rex_rm(p.val2));
          out_code.insert(X64::SHR_R_BY_CL);
          out_code.insert(X64::MODRM_MOD_DIRECT
                          | X64::modrm_r_rm(5, p.val2));

          code_i += ByteCode::SIZE_OF::SHIFT_R_BY_R8_RU64;

          check_for_jumps(relocs, out_code, 0, &code_i, code_end);
          break;
        }
      case ByteCode::SHIFT_R_BY_R8_RI64: {
          const auto p = ByteCode::PARSE::SHIFT_R_BY_R8_RI64(code_i);

          assert(p.val1 == RCX.REG);

          out_code.insert(X64::REX_W | X64::rex_rm(p.val2));
          out_code.insert(X64::SAR_R_BY_CL);
          out_code.insert(X64::MODRM_MOD_DIRECT
                          | X64::modrm_r_rm(7, p.val2));

          code_i += ByteCode::SIZE_OF::SHIFT_R_BY_R8_RI64;

          check_for_jumps(relocs, out_code, 0, &code_i, code_end);
          break;
        }
      case ByteCode::OR_R64S: {
          const auto p = ByteCode::PARSE::OR_R64S(code_i);

          out_code.insert(X64::REX_W | X64::rex_r_rm(p.val2, p.val1));
          out_code.insert(X64::OR_R_TO_RM);
          out_code.insert(X64::MODRM_MOD_DIRECT
                          | X64::modrm_r_rm(p.val2, p.val1));

          code_i += ByteCode::SIZE_OF::OR_R64S;

          check_for_jumps(relocs, out_code, 0, &code_i, code_end);
          break;
        }
      case ByteCode::XOR_R64S: {
          const auto p = ByteCode::PARSE::XOR_R64S(code_i);

          out_code.insert(X64::REX_W | X64::rex_r_rm(p.val2, p.val1));
          out_code.insert(X64::XOR_R_TO_RM);
          out_code.insert(X64::MODRM_MOD_DIRECT
                          | X64::modrm_r_rm(p.val2, p.val1));

          code_i += ByteCode::SIZE_OF::XOR_R64S;

          check_for_jumps(relocs, out_code, 0, &code_i, code_end);
          break;
        }
      case ByteCode::AND_R64S: {
          const auto p = ByteCode::PARSE::AND_R64S(code_i);

          out_code.insert(X64::REX_W | X64::rex_r_rm(p.val2, p.val1));
          out_code.insert(X64::AND_R_TO_RM);
          out_code.insert(X64::MODRM_MOD_DIRECT
                          | X64::modrm_r_rm(p.val2, p.val1));

          code_i += ByteCode::SIZE_OF::AND_R64S;

          check_for_jumps(relocs, out_code, 0, &code_i, code_end);
          break;
        }
      case ByteCode::EQ_R64S: {
          check_cmp_jump(relocs, out_code, &code_i, code_end,
                         jump_equal, jump_not_equal, sete);
          break;
        }
      case ByteCode::LESS_U64S: {
          check_cmp_jump(relocs, out_code, &code_i, code_end,
                         jump_below, jump_not_below, setl);
          break;
        }
      case ByteCode::GREAT_U64S: {
          check_cmp_jump(relocs, out_code, &code_i, code_end,
                         jump_above, jump_not_above, setg);
          break;
        }
      case ByteCode::LESS_I64S: {
          check_cmp_jump(relocs, out_code, &code_i, code_end,
                         jump_lesser, jump_not_lesser, setl);
          break;
        }
      case ByteCode::GREAT_I64S: {
          check_cmp_jump(relocs, out_code, &code_i, code_end,
                         jump_greater, jump_not_greater, setg);
          break;
        }
      case ByteCode::NEG_R64: {
          const auto p = ByteCode::PARSE::NEG_R64(code_i);

          out_code.insert(X64::REX_W | X64::rex_rm(p.val));
          out_code.insert(X64::NEG_RM);
          out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(3, p.val));

          code_i += ByteCode::SIZE_OF::NEG_R64;

          check_for_jumps(relocs, out_code, 0, &code_i, code_end);
          break;
        }
      case ByteCode::PUSH_R64: {
          const auto p = ByteCode::PARSE::PUSH_R64(code_i);

          X64::push(out_code, p.val);
          code_i += ByteCode::SIZE_OF::PUSH_R64;
          break;
        }
      case ByteCode::POP_TO_R64: {
          const auto p = ByteCode::PARSE::POP_TO_R64(code_i);

          X64::pop(out_code, p.val);
          code_i += ByteCode::SIZE_OF::POP_TO_R64;
          break;
        }
      case ByteCode::PUSH_FRAME: {
          const auto p = ByteCode::PARSE::PUSH_FRAME(code_i);

          X64::push(out_code, RBP.REG);
          X64::mov(out_code, RSP.REG, RBP.REG);

          code_i += ByteCode::SIZE_OF::PUSH_FRAME;
          break;
        }
      case ByteCode::POP_FRAME: {
          const auto p = ByteCode::PARSE::POP_FRAME(code_i);

          X64::mov(out_code, RBP.REG, RSP.REG);
          X64::pop(out_code, RBP.REG);

          code_i += ByteCode::SIZE_OF::POP_FRAME;
          break;
        }
      case ByteCode::ALLOCATE_STACK: {
          const auto p = ByteCode::PARSE::ALLOCATE_STACK(code_i);

          X64::sub(out_code, RSP.REG, (int32_t)p.u64.sig_val);

          code_i += ByteCode::SIZE_OF::ALLOCATE_STACK;
          break;
        }
      case ByteCode::COPY_R64_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_R64_TO_MEM(code_i);

          X64::RM rm = X64::rm_from_mem_complex(i.mem);

          X64::mov(out_code, X64::R{ i.val }, rm);

          code_i += ByteCode::SIZE_OF::COPY_R64_TO_MEM;
          break;
        }
      case ByteCode::COPY_64_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_64_TO_MEM(code_i);

          X64::RM rm = X64::rm_from_mem_complex(i.mem);

          const uint32_t low = i.u64.val & 0xFFFFFFFF;
          const uint32_t high = (i.u64.val >> (8 * 4)) & 0xFFFFFFFF;


          if (can_be_from_sign_extension(i.u64.val)) {

            //Sign extend
            X64::mov(out_code, rm, X64::IMM32{ true, low });
          }
          else {
            rm.disp += 4;
            X64::mov(out_code, rm, X64::IMM32{ false, high });

            rm.disp -= 4;
            X64::mov(out_code, rm, X64::IMM32{ false, low });
          }

          code_i += ByteCode::SIZE_OF::COPY_64_TO_MEM;
          break;
        }
      case ByteCode::COPY_32_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_32_TO_MEM(code_i);

          X64::RM rm = X64::rm_from_mem_complex(i.mem);

          X64::mov(out_code, rm, X64::IMM32{ false, i.u32 });

          code_i += ByteCode::SIZE_OF::COPY_32_TO_MEM;
          break;
        }
      case ByteCode::COPY_16_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_16_TO_MEM(code_i);

          X64::RM rm = X64::rm_from_mem_complex(i.mem);

          X64::mov(out_code, rm, i.u16);

          code_i += ByteCode::SIZE_OF::COPY_16_TO_MEM;
          break;
        }
      case ByteCode::LOAD_ADDRESS: {
          const auto i = ByteCode::PARSE::LOAD_ADDRESS(code_i);

          X64::RM rm = X64::rm_from_mem_complex(i.mem);

          X64::lea(out_code, rm, X64::R{ i.val });


          code_i += ByteCode::SIZE_OF::LOAD_ADDRESS;
          break;
        }
      case ByteCode::COPY_R8_FROM_MEM: {
          const auto p = ByteCode::PARSE::COPY_R8_FROM_MEM(code_i);

          X64::RM rm = X64::rm_from_mem_complex(p.mem);

          X64::mov(out_code, X64::RM8{ rm }, X64::R8{ X64::R{p.val} });
          code_i += ByteCode::SIZE_OF::COPY_R8_FROM_MEM;
          break;
        }
      case ByteCode::COPY_R64_FROM_MEM: {
          const auto i = ByteCode::PARSE::COPY_R64_FROM_MEM(code_i);

          X64::RM rm = X64::rm_from_mem_complex(i.mem);

          X64::mov(out_code, rm, X64::R{ i.val });

          code_i += ByteCode::SIZE_OF::COPY_R64_FROM_MEM;
          break;
        }
      case ByteCode::CONV_RU8_TO_R64: {
          const auto i = ByteCode::PARSE::CONV_RU8_TO_R64(code_i);

          out_code.insert(X64::REX_W | X64::rex_r_rm(i.val, i.val));
          out_code.insert(0x0F);
          out_code.insert(X64::MOV_ZX_RM8_TO_R);
          out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(i.val, i.val));

          code_i += ByteCode::SIZE_OF::CONV_RU8_TO_R64;
          break;
        }
      case ByteCode::CONV_RI8_TO_R64: {
          const auto i = ByteCode::PARSE::CONV_RI8_TO_R64(code_i);

          out_code.insert(X64::REX_W | X64::rex_r_rm(i.val, i.val));
          out_code.insert(0x0F);
          out_code.insert(X64::MOV_SX_RM8_TO_R);
          out_code.insert(X64::MODRM_MOD_DIRECT | X64::modrm_r_rm(i.val, i.val));

          code_i += ByteCode::SIZE_OF::CONV_RI8_TO_R64;
          break;
        }
      case ByteCode::LABEL: {
          const auto p = ByteCode::PARSE::LABEL(code_i);

          label_indexes[p.u64.val] = out_code.size;

          code_i += ByteCode::SIZE_OF::LABEL;
          break;
        }
      case ByteCode::RETURN: {
          X64::ret(out_code);
          code_i += ByteCode::SIZE_OF::RETURN;
          break;
        }
      case ByteCode::CALL: {
          const auto p = ByteCode::PARSE::CALL(code_i);
          const Function* func = p.u64;
          //Switch to a code label rather than func ptr
          call_near(relocs, out_code, (int32_t)func->code_block.label);


          code_i += ByteCode::SIZE_OF::CALL;
          break;
        }
      case ByteCode::CALL_LABEL: {
          const auto p = ByteCode::PARSE::CALL_LABEL(code_i);
          const u64 label = p.u64;

          call_near(relocs, out_code, (int32_t)label);

          code_i += ByteCode::SIZE_OF::CALL_LABEL;
          break;
        }
      case ByteCode::JUMP_TO_FIXED: {
          const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED(code_i);
          code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED;

          //Can ignore anything between a fixed jump and a label - it wont every be reached
          while (code_i < code_end && code_i[0] != ByteCode::LABEL) {
            code_i += ByteCode::instruction_size(code_i[0]);
          }

          size_t next = 0;

          //Could be series of labels so needs to while loop
          while (code_i + next < code_end && code_i[next] == ByteCode::LABEL) {
            const auto p_l = ByteCode::PARSE::LABEL(code_i + next);

            if (p_l.u64.val == p_j.u64.val) {
              //About to jump to next instruction - can ignore jump
              goto SKIP_JUMP;
            }

            //Next might also be a label
            next += ByteCode::SIZE_OF::LABEL;
          }

          jump_near(relocs, out_code, (int32_t)p_j.u64.val);

        SKIP_JUMP:
          break;
        }
      case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
          const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

          //need to recompare
          cmp(out_code, p.val, (int32_t)0);

          jump_zero(relocs, out_code, (int32_t)p.u64.val);

          code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
          break;
        }
      case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
          const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

          //need to recompare
          cmp(out_code, p.val, (int32_t)0);

          jump_not_equal(relocs, out_code, (int32_t)p.u64.val);

          code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
          break;
        }
      default: {
          uint8_t op = *code_i;
          comp->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                             "Backend found unsupported bytecode instruction\n"
                             "Code: {}, Name: {}",
                             op, ByteCode::bytecode_string((ByteCode::ByteCodeOp)op));
          return;
        }
    }
  }
}

//void x86_64_backend_fix_jump(uint8_t* code, const Relocation* reloc, size_t* label_indexes) {
//  if (code[index] == 0x0F) index++;
//
//  //jumps are all from the end of the instruction
//  //all jumps are a op + imm32 (so far)
//
//  const uint64_t j_index = x32_from_bytes(code + index + 1);
//  const int32_t jump = (int32_t)label_indexes[j_index] - (int32_t)(index + 1 + sizeof(int32_t));
//
//  x32_to_bytes(jump, code + index + 1);
//}

struct RegisterNames {
  OwnedPtr<char> r;
  OwnedPtr<char> rm;
};

const char* b8_no_rex_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "AL";
    case 1: return "CL";
    case 2: return "DL";
    case 3: return "BL";
    case 4: return "AH";
    case 5: return "CH";
    case 6: return "DH";
    case 7: return "BH";
  }

  return "INVALID REGISTER";
}

const char* b8_rex_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "AL";
    case 1: return "CL";
    case 2: return "DL";
    case 3: return "BL";
    case 4: return "SPL";
    case 5: return "BPL";
    case 6: return "SIL";
    case 7: return "DIL";
    case 8: return "R8L";
    case 9: return "R9L";
    case 10: return "R10L";
    case 11: return "R11L";
    case 12: return "R12L";
    case 13: return "R13L";
    case 14: return "R14L";
    case 15: return "R15L";
  }

  return "INVALID REGISTER";
}

const char* b16_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "AX";
    case 1: return "CX";
    case 2: return "DX";
    case 3: return "BX";
    case 4: return "SP";
    case 5: return "BP";
    case 6: return "SI";
    case 7: return "DI";
    case 8: return "R8W";
    case 9: return "R9W";
    case 10: return "R10W";
    case 11: return "R11W";
    case 12: return "R12W";
    case 13: return "R13W";
    case 14: return "R14W";
    case 15: return "R15W";
  }

  return "INVALID REGISTER";
}

const char* b32_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "EAX";
    case 1: return "ECX";
    case 2: return "EDX";
    case 3: return "EBX";
    case 4: return "ESP";
    case 5: return "EBP";
    case 6: return "ESI";
    case 7: return "EDI";
    case 8: return "R8D";
    case 9: return "R9D";
    case 10: return "R10D";
    case 11: return "R11D";
    case 12: return "R12D";
    case 13: return "R13D";
    case 14: return "R14D";
    case 15: return "R15D";
  }

  return "INVALID REGISTER";
}

struct x86PrintOptions {
  FUNCTION_PTR<const char*, uint8_t> r_name = nullptr;
  FUNCTION_PTR<const char*, uint8_t> rm_name = nullptr;
  const char* mem_size = nullptr;
};

static OwnedPtr<char> rm_reg_string(x86PrintOptions* const p_opts,
                                    uint8_t rex, uint8_t modrm, const uint8_t** rest) {
  uint8_t address_mode = (modrm & 0b11'000000) >> 6;
  uint8_t rm = modrm & X64::MODRM_RM_MASK;

  if ((modrm & 0b11'000000) == 0b11'000000) {
    rm |= ((rex & X64::REX_B) << X64::REX_B_SHIFT);

    return format("{}", p_opts->rm_name(rm));
  }

  //from now on use x86_64_reg_name_from_num for mem
  //Memory is always 64 bit addressed

  switch (rm) {
    case RSP.REG: {
        //SIB byte time
        const uint8_t sib = *(*rest)++;

        const uint8_t scale = 1 << ((sib & 0b11'000'000) >> 6);
        const uint8_t index = ((rex & X64::REX_X) << 2) | ((sib & X64::SIB_INDEX_MASK) >> 3);
        const uint8_t base  = ((rex & X64::REX_B) << 3) | ((sib & X64::SIB_BASE_MASK));

        const bool INDEX_RSP = index == RSP.REG;
        const bool BASE_RBP  = (base & 0b111) == 0b101;

        switch (address_mode) {
          case 0b00: {
              if (INDEX_RSP && BASE_RBP) {
                int32_t disp = x32_from_bytes(*rest);
                *rest += 4;

                return format("{} [{}]", p_opts->mem_size, disp);
              }
              else if (INDEX_RSP) {
                return format("{} [{}]", p_opts->mem_size, x86_64_reg_name_from_num(base));
              }
              else if (BASE_RBP) {
                int32_t disp = x32_from_bytes(*rest);
                *rest += 4;

                char sign = disp >= 0 ? '+' : '-';
                if (scale == 1) {
                  return format("{} [{} {} {}]", p_opts->mem_size, x86_64_reg_name_from_num(index), sign, absolute(disp));
                }
                else {
                  return format("{} [({} * {}) {} {}]", p_opts->mem_size, x86_64_reg_name_from_num(index), scale, sign, absolute(disp));
                }
              }
              else if (scale == 1) {
                return format("{} [{} + {}]",
                              p_opts->mem_size,
                              x86_64_reg_name_from_num(base),
                              x86_64_reg_name_from_num(index));
              }
              else {
                return format("{} [{} + ({} * {})]",
                              p_opts->mem_size,
                              x86_64_reg_name_from_num(base),
                              x86_64_reg_name_from_num(index),
                              scale);
              }
            }
          case 0b01: {
              const int8_t disp = *(*rest)++;

              char sign = disp >= 0 ? '+' : '-';

              if (INDEX_RSP) {
                return format("{} [{} {} {}]",
                              p_opts->mem_size,
                              x86_64_reg_name_from_num(base),
                              sign, absolute(disp));
              }
              else {
                if (scale == 1) {
                  return format("{} [{} {} {} + {}]",
                                p_opts->mem_size,
                                x86_64_reg_name_from_num(base),
                                sign, absolute(disp),
                                x86_64_reg_name_from_num(index));
                }
                else {
                  return format("{} [{} {} {} + ({} * {})]",
                                p_opts->mem_size,
                                x86_64_reg_name_from_num(base),
                                sign, absolute(disp),
                                x86_64_reg_name_from_num(index), scale);
                }
              }
            }
          case 0b10: {
              int32_t disp = x32_from_bytes(*rest);
              *rest += 4;

              char sign = disp >= 0 ? '+' : '-';

              if (INDEX_RSP) {
                return format("{} [{} {} {}]",
                              p_opts->mem_size,
                              x86_64_reg_name_from_num(base),
                              sign, absolute(disp));
              }
              else {
                if (scale == 1) {
                  return format("{} [{} + {} {} {}]",
                                p_opts->mem_size,
                                x86_64_reg_name_from_num(base),
                                x86_64_reg_name_from_num(index),
                                sign, absolute(disp));
                }
                else {
                  return format("{} [{} + ({} * {}) {} {}]",
                                p_opts->mem_size,
                                x86_64_reg_name_from_num(base),
                                x86_64_reg_name_from_num(index), scale,
                                sign, absolute(disp));
                }
              }
            }
        }

        throw std::exception("Internal error, should not be here");
      }
    case RBP.REG: {
        if (address_mode == 0b00) {
          int32_t disp = x32_from_bytes(*rest);
          *rest += 4;

          char sign = disp >= 0 ? '+' : '-';

          return format("{} [RIP {} {}]", p_opts->mem_size, sign, absolute(disp));
        }

        goto NORMAL_MODRM;
      }
    default: {
      NORMAL_MODRM:
        rm |= ((rex & X64::REX_B) << X64::REX_B_SHIFT);

        switch (address_mode) {
          case 0b00: {
              return format("{} [{}]", p_opts->mem_size, x86_64_reg_name_from_num(rm));
            }
          case 0b01: {
              int8_t disp = *(*rest)++;

              char sign = disp >= 0 ? '+' : '-';

              return format("{} [{} {} {}]", p_opts->mem_size, x86_64_reg_name_from_num(rm), sign, absolute(disp));
            }
          case 0b10: {
              int32_t disp = x32_from_bytes(*rest);
              *rest += 4;

              char sign = disp >= 0 ? '+' : '-';

              return format("{} [{} {} {}]", p_opts->mem_size, x86_64_reg_name_from_num(rm), sign, absolute(disp));
            }
        }

        throw std::exception("Internal error, should not be here");
      }
  }

  throw std::exception("Internal error, should not be here");
}

static OwnedPtr<char> r_reg_string(x86PrintOptions* p_opts,
                                   uint8_t rex, uint8_t modrm) {
  uint8_t r = ((rex & X64::REX_R) << X64::REX_R_SHIFT)
    | ((modrm & X64::MODRM_REG_MASK) >> X64::MODRM_REG_SHIFT);

  return  format("{}", p_opts->r_name(r));
}


static RegisterNames register_names(x86PrintOptions* p_opts,
                                    uint8_t rex, uint8_t modrm, const uint8_t** rest) {
  return { r_reg_string(p_opts, rex, modrm), rm_reg_string(p_opts, rex, modrm, rest) };
}

static void load_default_sizes(x86PrintOptions* ops, bool rex_w, bool short_address, bool short_operand) {
  if (short_address) {
    ops->rm_name = b32_reg_name;
  }
  else {
    ops->rm_name = x86_64_reg_name_from_num;
  }

  if (rex_w) {
    ops->r_name = x86_64_reg_name_from_num;
    ops->mem_size = "QWORD PTR";
  }
  else {
    if (short_operand) {
      ops->r_name = b16_reg_name;
      ops->mem_size = "WORD PTR";
    }
    else {
      ops->r_name = b32_reg_name;
      ops->mem_size = "DWORD PTR";
    }
  }
}

void print_x86_64(const uint8_t* machine_code, size_t size) {
  const uint8_t* bytes = machine_code;
  const uint8_t* const end = machine_code + size;

  x86PrintOptions p_opts ={};

  while (bytes < end) {
    printf("0x%-4llx: ", bytes - machine_code);

    bool short_operand = bytes[0] == 0x66;
    if (short_operand) {
      bytes++;
    }

    bool short_address = bytes[0] == 0x67;
    if (short_address) {
      bytes++;
    }

    //check again as it might have been second
    if (!short_operand) {
      short_operand = bytes[0] == 0x66;
      if (short_operand) {
        bytes++;
      }
    }

    const uint8_t maybe_rex = *bytes++;
    if ((maybe_rex & 0b1111'1000) == X64::REX_W) {
      //REX_W instruction
      const uint8_t op = *bytes++;
      switch (op) {
        case X64::ADD_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("add %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::OR_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("or  %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::AND_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("and %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::SUB_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("sub %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::XOR_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("xor %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::CMP_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("cmp %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::PUSH_R:
        case (X64::PUSH_R + 1):
        case (X64::PUSH_R + 2):
        case (X64::PUSH_R + 3):
        case (X64::PUSH_R + 4):
        case (X64::PUSH_R + 5):
        case (X64::PUSH_R + 6):
        case (X64::PUSH_R + 7): {
            const uint8_t reg = (op - X64::PUSH_R) | ((maybe_rex & 0b00000100) << 1);
            const char* r_string = x86_64_reg_name_from_num(reg);

            printf("push %s\n", r_string);
            break;
          }
        case X64::POP_R:
        case (X64::POP_R + 1):
        case (X64::POP_R + 2):
        case (X64::POP_R + 3):
        case (X64::POP_R + 4):
        case (X64::POP_R + 5):
        case (X64::POP_R + 6):
        case (X64::POP_R + 7): {
            const uint8_t reg = (op - X64::POP_R) | ((maybe_rex & 0b00000100) << 1);
            const char* r_string = x86_64_reg_name_from_num(reg);

            printf("pop %s\n", r_string);
            break;
          }
        case 0x81: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            OwnedPtr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            int32_t imm32 = x32_from_bytes(bytes);
            bytes += 4;

            uint8_t r_val = (modrm & 0b0011'1000) >> 3;

            if (r_val == 5) {
              printf("sub %s, 0x%x\n", rm_string.ptr, imm32);
            }
            else if (r_val == 7) {
              printf("cmp %s, 0x%x\n", rm_string.ptr, imm32);
            }
            else {
              printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx.2 0x%.2hhx ...\n",
                     maybe_rex, op, modrm);

              return;
            }
            break;
          }
        case X64::MOV_R_TO_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("mov %s, %s\n", names.rm.ptr, names.r.ptr);
            break;
          }
        case X64::MOV_IMM32_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            OwnedPtr<char> rm = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            uint32_t val = x32_from_bytes(bytes);
            bytes += 4;

            printf("mov %s, %u\n", rm.ptr, val);
            break;
          }
        case X64::MOV_RM_TO_R: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("mov %s, %s\n", names.r.ptr, names.rm.ptr);
            break;
          }
        case X64::LEA_RM_TO_R: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("lea %s, %s\n", names.r.ptr, names.rm.ptr);
            break;
          }
        case X64::CQO: {
            printf("cqo\n");
            break;
          }
        case 0x0F: {
            uint8_t op2 = *bytes++;
            switch (op2) {
              case X64::IMUL_RM_TO_R: {
                  uint8_t modrm = *bytes++;

                  load_default_sizes(&p_opts, true, short_address, short_operand);

                  RegisterNames names =  register_names(&p_opts, maybe_rex, modrm, &bytes);

                  printf("imul %s, %s\n", names.r.ptr, names.rm.ptr);
                  break;
                }
              case X64::MOV_ZX_RM8_TO_R: {
                  uint8_t modrm = *bytes++;

                  load_default_sizes(&p_opts, true, short_address, short_operand);
                  //overide
                  p_opts.rm_name = b8_rex_reg_name;

                  RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

                  printf("movzx %s, %s\n", names.r.ptr, names.rm.ptr);
                  break;
                }
              case X64::MOV_SX_RM8_TO_R: {
                  uint8_t modrm = *bytes++;

                  load_default_sizes(&p_opts, true, short_address, short_operand);
                  //overide
                  p_opts.rm_name = b8_rex_reg_name;

                  RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

                  printf("movsx %s, %s\n", names.r.ptr, names.rm.ptr);
                  break;
                }
              default: {
                  printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                         maybe_rex, op, op2);

                  return;
                }
            }
            break;
          }
        case X64::MOV_64_TO_R:
        case (X64::MOV_64_TO_R + 1):
        case (X64::MOV_64_TO_R + 2):
        case (X64::MOV_64_TO_R + 3):
        case (X64::MOV_64_TO_R + 4):
        case (X64::MOV_64_TO_R + 5):
        case (X64::MOV_64_TO_R + 6):
        case (X64::MOV_64_TO_R + 7): {
            const uint8_t reg = (op - X64::MOV_64_TO_R) | ((maybe_rex & 0b0000'0001) << 3);

            load_default_sizes(&p_opts, true, short_address, short_operand);

            const char* r_string = p_opts.r_name(reg);

            uint64_t imm64 = x64_from_bytes(bytes);
            bytes += 8;

            printf("mov %s, 0x%llx\n", r_string, imm64);
            break;
          }
        case 0xF7: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            const uint8_t r = (modrm & 0b0011'1000) >> 3;
            OwnedPtr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            if (r == 3) {
              printf("neg %s\n", rm_string.ptr);
            }
            else if (r == 4) {
              printf("mul %s\n", rm_string.ptr);
            }
            else if (r == 6) {
              printf("div %s\n", rm_string.ptr);
            }
            else if (r == 7) {
              printf("idiv %s\n", rm_string.ptr);
            }
            else {
              printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                     maybe_rex, op, modrm);

              return;
            }
            break;
          }
        case 0xD3: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, true, short_address, short_operand);

            const uint8_t r = (modrm & 0b0011'1000) >> 3;
            OwnedPtr<char> rm_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            if (r == 4) {
              printf("sal %s, CL\n", rm_string.ptr);
            }
            else if (r == 5) {
              printf("shr %s, CL\n", rm_string.ptr);
            }
            else if (r == 7) {
              printf("sar %s, CL\n", rm_string.ptr);
            }
            else {
              printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                     maybe_rex, op, modrm);

              return;
            }
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx\n",
                   maybe_rex, op);

            return;
          }
      }
    }
    else if ((maybe_rex & 0b1111'1000) == X64::REX) {
      uint8_t op = *bytes++;
      switch (op) {
        case X64::MOV_8_TO_R8:
        case (X64::MOV_8_TO_R8 + 1):
        case (X64::MOV_8_TO_R8 + 2):
        case (X64::MOV_8_TO_R8 + 3):
        case (X64::MOV_8_TO_R8 + 4):
        case (X64::MOV_8_TO_R8 + 5):
        case (X64::MOV_8_TO_R8 + 6):
        case (X64::MOV_8_TO_R8 + 7): {
            const uint8_t reg = (op - X64::MOV_8_TO_R8) | ((maybe_rex & 0b0000'0001) << 3);
            const char* r_string = b8_rex_reg_name(reg);

            uint8_t imm8 = bytes[0];
            bytes++;

            printf("mov %s, 0x%hhx\n", r_string, imm8);
            break;
          }
        case X64::MOV_IMM32_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, false, short_address, short_operand);

            OwnedPtr<char> rm = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);

            uint32_t val = x32_from_bytes(bytes);
            bytes += 4;

            printf("mov %s, %u\n", rm.ptr, val);
            break;
          }
        case X64::MOV_R8_TO_RM8: {
            uint8_t modrm = *bytes++;

            //Overide
            p_opts.rm_name = b8_rex_reg_name;
            p_opts.r_name = b8_rex_reg_name;
            p_opts.mem_size = "BYTE PTR";

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("mov %s, %s\n", names.r.ptr, names.rm.ptr);
            break;
          }
        case 0x0F: {
            uint8_t op2 = *bytes++;
            switch (op2) {
              case X64::SETE_RM8: {
                  uint8_t modrm = *bytes++;

                  p_opts.rm_name = b8_rex_reg_name;
                  p_opts.r_name = b8_rex_reg_name;
                  p_opts.mem_size = "BYTE PTR";

                  OwnedPtr<char> r_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);
                  printf("sete %s\n", r_string.ptr);
                  break;
                }
              case X64::SETG_RM8: {
                  uint8_t modrm = *bytes++;

                  p_opts.rm_name = b8_rex_reg_name;
                  p_opts.r_name = b8_rex_reg_name;
                  p_opts.mem_size = "BYTE PTR";

                  OwnedPtr<char> r_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);
                  printf("setg %s\n", r_string.ptr);
                  break;
                }
              case X64::SETL_RM8: {
                  uint8_t modrm = *bytes++;

                  p_opts.rm_name = b8_rex_reg_name;
                  p_opts.r_name = b8_rex_reg_name;
                  p_opts.mem_size = "BYTE PTR";

                  OwnedPtr<char> r_string = rm_reg_string(&p_opts, maybe_rex, modrm, &bytes);
                  printf("setl %s\n", r_string.ptr);
                  break;
                }
              default: {
                  printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx 0x%.2hhx\n",
                         maybe_rex, op, op2);

                  return;
                }
            }
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx\n",
                   maybe_rex, op);

            return;
          }
      }
    }
    else if (maybe_rex == 0x0F) {
      //0x0F instructions
      uint8_t op = *bytes++;
      switch (op) {
        case X64::JE_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("je 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JNE_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jne 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JB_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jb 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JNB_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jnb 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JA_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("ja 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JNA_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jna 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JL_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jl 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JNL_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jnl 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JG_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jg 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::JNG_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jng 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::SETE_RM8: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = b8_rex_reg_name;
            p_opts.mem_size = "BYTE PTR";

            OwnedPtr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &bytes);
            printf("sete %s\n", r_string.ptr);
            break;
          }
        case X64::SETL_RM8: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = b8_rex_reg_name;
            p_opts.mem_size = "BYTE PTR";

            OwnedPtr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &bytes);
            printf("setl %s\n", r_string.ptr);
            break;
          }
        case X64::SETG_RM8: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = b8_rex_reg_name;
            p_opts.mem_size = "BYTE PTR";

            OwnedPtr<char> r_string = rm_reg_string(&p_opts, 0, modrm, &bytes);
            printf("setg %s\n", r_string.ptr);
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx 0x%.2hhx\n",
                   maybe_rex, op);

            return;
          }

      }

    }
    else {
      //Non-REX instruction
      const uint8_t op = maybe_rex;

      switch (op) {
        case X64::MOV_R8_TO_RM8: {
            uint8_t modrm = *bytes++;

            p_opts.rm_name = b8_no_rex_reg_name;
            p_opts.r_name = b8_no_rex_reg_name;
            p_opts.mem_size = "BYTE PTR";

            RegisterNames names = register_names(&p_opts, maybe_rex, modrm, &bytes);

            printf("mov %s, %s\n", names.r.ptr, names.rm.ptr);
            break;
          }
        case X64::MOV_8_TO_R8:
        case (X64::MOV_8_TO_R8 + 1):
        case (X64::MOV_8_TO_R8 + 2):
        case (X64::MOV_8_TO_R8 + 3):
        case (X64::MOV_8_TO_R8 + 4):
        case (X64::MOV_8_TO_R8 + 5):
        case (X64::MOV_8_TO_R8 + 6):
        case (X64::MOV_8_TO_R8 + 7): {
            const uint8_t reg = (op - X64::MOV_8_TO_R8);
            const char* r_string = b8_no_rex_reg_name(reg);

            uint8_t imm8 = bytes[0];
            bytes++;

            printf("mov %s, 0x%hhx\n", r_string, imm8);
            break;
          }
        case X64::PUSH_R:
        case (X64::PUSH_R + 1):
        case (X64::PUSH_R + 2):
        case (X64::PUSH_R + 3):
        case (X64::PUSH_R + 4):
        case (X64::PUSH_R + 5):
        case (X64::PUSH_R + 6):
        case (X64::PUSH_R + 7): {
            //Default to long mode
            const char* r_string = x86_64_reg_name_from_num(op - X64::PUSH_R);

            printf("push %s\n", r_string);
            break;
          }
        case X64::POP_R:
        case (X64::POP_R + 1):
        case (X64::POP_R + 2):
        case (X64::POP_R + 3):
        case (X64::POP_R + 4):
        case (X64::POP_R + 5):
        case (X64::POP_R + 6):
        case (X64::POP_R + 7): {
            //Default to long mode
            const char* r_string = x86_64_reg_name_from_num(op - X64::POP_R);

            printf("pop %s\n", r_string);
            break;
          }
        case X64::JMP_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("jmp 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::RET_NEAR: {
            printf("ret\n");
            break;
          }
        case X64::CALL_NEAR: {
            int rel32 = x32_from_bytes(bytes);
            bytes += 4;

            printf("call 0x%llx\n", bytes - machine_code + rel32);
            break;
          }
        case X64::MOV_IMM32_RM: {
            uint8_t modrm = *bytes++;

            load_default_sizes(&p_opts, false, short_address, short_operand);

            OwnedPtr<char> rm = rm_reg_string(&p_opts, 0, modrm, &bytes);

            uint32_t val = x32_from_bytes(bytes);
            bytes += 4;

            printf("mov %s, %u\n", rm.ptr, val);
            break;
          }
        default: {
            printf("UNKNOWN INSTRUCTION: 0x%.2hhx\n",
                   maybe_rex);

            return;
          }
      }
    }
  }
}