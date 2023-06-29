#include "compiler.h"
#include "trace.h"

#if 0
static const char* b8_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "al";
    case 1: return "cl";
    case 2: return "dl";
    case 3: return "bl";
    case 4: return "spl";
    case 5: return "bpl";
    case 6: return "sil";
    case 7: return "dil";
    case 8: return "r8b";
    case 9: return "r9b";
    case 10: return "r10b";
    case 11: return "r11b";
    case 12: return "r12b";
    case 13: return "r13b";
    case 14: return "r14b";
    case 15: return "r15b";
  }

  INVALID_CODE_PATH("INVALID REGISTER");
  return nullptr;
}

static const char* b16_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "ax";
    case 1: return "cx";
    case 2: return "dx";
    case 3: return "bx";
    case 4: return "sp";
    case 5: return "bp";
    case 6: return "si";
    case 7: return "di";
    case 8: return "r8w";
    case 9: return "r9w";
    case 10: return "r10w";
    case 11: return "r11w";
    case 12: return "r12w";
    case 13: return "r13w";
    case 14: return "r14w";
    case 15: return "r15w";
  }

  INVALID_CODE_PATH("INVALID REGISTER");
  return nullptr;
}

static const char* b32_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "eax";
    case 1: return "ecx";
    case 2: return "edx";
    case 3: return "ebx";
    case 4: return "esp";
    case 5: return "ebp";
    case 6: return "esi";
    case 7: return "edi";
    case 8: return "r8d";
    case 9: return "r9d";
    case 10: return "r10d";
    case 11: return "r11d";
    case 12: return "r12d";
    case 13: return "r13d";
    case 14: return "r14d";
    case 15: return "r15d";
  }

  INVALID_CODE_PATH("INVALID REGISTER");
  return nullptr;
}

static const char* b64_reg_name(uint8_t reg) {
  switch (reg) {
    case 0: return "rax";
    case 1: return "rcx";
    case 2: return "rdx";
    case 3: return "rbx";
    case 4: return "rsp";
    case 5: return "rbp";
    case 6: return "rsi";
    case 7: return "rdi";
    case 8: return "r8";
    case 9: return "r9";
    case 10: return "r10";
    case 11: return "r11";
    case 12: return "r12";
    case 13: return "r13";
    case 14: return "r14";
    case 15: return "r15";
  }

  INVALID_CODE_PATH("INVALID REGISTER");
  return nullptr;
}

//Buffered file
struct FileData {
  static constexpr usize BUFFER_SIZE = 2048;

  FILES::FileData* data = nullptr;

  usize top = 0;
  u8 buffer[BUFFER_SIZE] = {};
};


void write(FileData* d, const u8* data, usize len) {
  usize i = 0;
  for (;;) {
    if (i == len) {
      return;
    }

    d->buffer[d->top] = data[i];

    d->top++;
    i++;

    if (d->top == d->BUFFER_SIZE) {
      FILES::write(d->data, d->buffer, d->BUFFER_SIZE);
      d->top = 0;
    }
  }
}

inline void write_reg(FileData* f, u8 reg, FUNCTION_PTR<const char*, u8> name) {
  const char* n = name(reg);
  write(f, (const u8*)n, strlen_ts(n));
}

inline void write_str(FileData* f, const InternString* s) {
  write(f, (const u8*)s->string, s->len);
}

inline void write_str(FileData* f, const char* s) {
  write(f, (const u8*)s, strlen_ts(s));
}


void write_label(FileData* f, u64 num) {
  char str[] = "_label_0000000000000000";

  u64 mask = 0x000000000000000f;
  for (u32 i = 0; i < 16; i++) {
    u64 p = (num & mask) >> (i * 4);

    if (p >= 0xallu) {
      str[sizeof(str) - 2 - i] = 'a' + (char)(p - 0xallu);
    }
    else {
      str[sizeof(str) - 2 - i] = '0' + (char)p;
    }

    mask <<= 4;
  }

  write_str(f, str);
}

void write_u64(FileData* f, u64 num) {
  char str[] = "0x0000000000000000";

  u64 mask = 0x000000000000000f;
  for (u32 i = 0; i < 16; i++) {
    u64 p = (num & mask) >> (i * 4);

    if (p >= 0xallu) {
      str[sizeof(str) - 2 - i] = 'a' + (char)(p - 0xallu);
    }
    else {
      str[sizeof(str) - 2 - i] = '0' + (char)p;
    }

    mask <<= 4;
  }

  write_str(f, str);
}

void write_u32(FileData* f, u32 num) {
  char str[] = "0x00000000";

  u32 mask = 0x00000000f;
  for (u32 i = 0; i < 8; i++) {
    u32 p = (num & mask) >> (i * 4);

    if (p >= 0xa) {
      str[sizeof(str) - 2 - i] = 'a' + (char)(p - 0xa);
    }
    else {
      str[sizeof(str) - 2 - i] = '0' + (char)p;
    }

    mask <<= 4;
  }

  write_str(f, str);
}

void write_u16(FileData* f, u16 num) {
  char str[] = "0x0000";

  u16 mask = 0x000f;
  for (u16 i = 0; i < 4; i++) {
    u16 p = (num & mask) >> (i * 4);

    if (p >= 0xa) {
      str[(sizeof(str) - 2) - i] = 'a' + (char)(p - 0xa);
    }
    else {
      str[(sizeof(str) - 2) - i] = '0' + (char)p;
    }

    mask <<= 4;
  }

  write_str(f, str);
}



void write_i64(FileData* f, i64 num) {
  if (num < 0) {
    write_str(f, "-");
    write_u64(f, (u64)-num);
  }
  else {
    write_u64(f, (u64)num);
  }
}


void write_u8(FileData* f, u8 num) {
  char str[] = "0x00";

  u8 mask = 0x0f;
  for (u32 i = 0; i < 2; i++) {
    u8 p = (num & mask) >> (i * 4);

    if (p >= 0xa) {
      str[sizeof(str) - 2 - i] = 'a' + (char)(p - 0xa);
    }
    else {
      str[sizeof(str) - 2 - i] = '0' + (char)p;
    }

    mask <<= 4;
  }

  write_str(f, str);
}

static void jump_zero(FileData* f, u64 label) {
  write_str(f, "jz ");
  write_label(f, label);
  write_str(f, "\n");
}

static constexpr auto jump_equal = jump_zero;

static void jump_not_equal(FileData* f, u64 label) {
  write_str(f, "jne ");
  write_label(f, label);
  write_str(f, "\n");
}

static void jump_above(FileData* f, u64 label) {
  write_str(f, "ja ");
  write_label(f, label);
  write_str(f, "\n");
}

static void jump_not_above(FileData* f, u64 label) {
  write_str(f, "jna ");
  write_label(f, label);
  write_str(f, "\n");
}

static void jump_below(FileData* f, u64 label) {
  write_str(f, "jb ");
  write_label(f, label);
  write_str(f, "\n");
}

static void jump_not_below(FileData* f, u64 label) {
  write_str(f, "jnb ");
  write_label(f, label);
  write_str(f, "\n");
}

static void jump_lesser(FileData* f, u64 label) {
  write_str(f, "jl ");
  write_label(f, label);
  write_str(f, "\n");
}

static void jump_not_lesser(FileData* f, u64 label) {
  write_str(f, "jnl ");
  write_label(f, label);
  write_str(f, "\n");
}

static void jump_greater(FileData* f, u64 label) {
  write_str(f, "jg ");
  write_label(f, label);
  write_str(f, "\n");
}

static void jump_not_greater(FileData* f, u64 label) {
  write_str(f, "jng ");
  write_label(f, label);
  write_str(f, "\n");
}

static void sete(FileData* f, u8 r) {
  write_str(f, "sete ");
  write_reg(f, r, b8_reg_name);
  write_str(f, "\n");
}

static void setne(FileData* f, u8 r) {
  write_str(f, "setne ");
  write_reg(f, r, b8_reg_name);
  write_str(f, "\n");
}

static void setl(FileData* f, u8 r) {
  write_str(f, "setl ");
  write_reg(f, r, b8_reg_name);
  write_str(f, "\n");
}

static void setg(FileData* f, u8 r) {
  write_str(f, "setg ");
  write_reg(f, r, b8_reg_name);
  write_str(f, "\n");
}

static void seta(FileData* f, u8 r) {
  write_str(f, "seta ");
  write_reg(f, r, b8_reg_name);
  write_str(f, "\n");
}

static void setb(FileData* f, u8 r) {
  write_str(f, "setb ");
  write_reg(f, r, b8_reg_name);
  write_str(f, "\n");
}

static void check_cmp_jump(FileData* f,
                           const uint8_t** code_i_ptr, const uint8_t* end,
                           FUNCTION_PTR<void, FileData*, u64> success_jump_func,
                           FUNCTION_PTR<void, FileData*, u64> fail_jump_func,
                           FUNCTION_PTR<void, FileData*, u8> no_jump) {

  const uint8_t* const code_i = *code_i_ptr;
  const uint8_t* const code_i_2 = code_i + ByteCode::OP_R_R::INSTRUCTION_SIZE;

  const auto p_e = ByteCode::OP_R_R::parse(code_i);

  write_str(f, "cmp ");
  write_reg(f, p_e.val2, b64_reg_name);
  write_str(f, ", ");
  write_reg(f, p_e.val1, b64_reg_name);
  write_str(f, "\n");

  switch (*code_i_2) {
    case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
        const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_ZERO(code_i_2);

        fail_jump_func(f, p_j.u64.val);

        *code_i_ptr = code_i_2 + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_ZERO;
        break;
      }
    case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
        const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i_2);

        success_jump_func(f, p_j.u64.val);

        *code_i_ptr = code_i_2 + ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
        break;
      }
    default: {
        no_jump(f, p_e.val2);

        //Clear the top of the register
        write_str(f, "movzx ");
        write_reg(f, p_e.val2, b64_reg_name);
        write_str(f, ", ");
        write_reg(f, p_e.val2, b8_reg_name);
        write_str(f, "\n");


        *code_i_ptr = code_i_2;
        break;
      }
  }
}

void write_complex_mem(FileData* f, const MemComplex& mem) {
  write_str(f, "[");
  write_reg(f, mem.base, b64_reg_name);

  if (mem.disp > 0) {
    write_str(f, " + ");
    write_u32(f, (u32)mem.disp);
  }
  else if (mem.disp < 0) {
    write_str(f, " - ");
    write_u32(f, (u32)-mem.disp);
  }
  else {}

  if (mem.scale != 0) {
    write_str(f, " + (");
    write_reg(f, mem.index, b64_reg_name);

    switch (mem.scale) {
      case 1: write_str(f, ")"); break;
      case 2: write_str(f, " * 2)"); break;
      case 4: write_str(f, " * 4)"); break;
      case 8: write_str(f, " * 8)"); break;
      default:
        INVALID_CODE_PATH("Invalid value");
    }
  }

  write_str(f, "]");
}

void write_code(FileData* f, CompilerGlobals* const comp, CompilerThread* const comp_thread, const CodeBlock* code) {
  TRACING_FUNCTION();

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

          write_str(f, "mov ");
          write_reg(f, p.val2, b64_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_R64_TO_R64;
          break;
        }
      case ByteCode::COPY_R32_TO_R32: {
          const auto p = ByteCode::PARSE::COPY_R32_TO_R32(code_i);

          write_str(f, "mov ");
          write_reg(f, p.val2, b32_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val1, b32_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_R32_TO_R32;
          break;
        }
      case ByteCode::COPY_R8_TO_R8: {
          const auto p = ByteCode::PARSE::COPY_R8_TO_R8(code_i);

          write_str(f, "mov ");
          write_reg(f, p.val2, b8_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val1, b8_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_R8_TO_R8;
          break;
        }
      case ByteCode::LOAD_DATA_MEM: {
          const auto p = ByteCode::PARSE::SET_R64_TO_64(code_i);
          const DataHolder* d = comp->data_holders.data + p.u64;

          write_str(f, "lea ");
          write_reg(f, p.val, b64_reg_name);
          write_str(f, ", [");
          write_str(f, d->name);
          write_str(f, "]");
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::SET_R64_TO_64;
          break;
        }
      case ByteCode::SET_R64_TO_64: {
          const auto p = ByteCode::PARSE::SET_R64_TO_64(code_i);

          write_str(f, "mov ");
          write_reg(f, p.val, b64_reg_name);
          write_str(f, ", ");
          write_u64(f, p.u64.val);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::SET_R64_TO_64;
          break;
        }
      case ByteCode::SET_R32_TO_32: {
          const auto p = ByteCode::PARSE::SET_R32_TO_32(code_i);

          write_str(f, "mov ");
          write_reg(f, p.val, b32_reg_name);
          write_str(f, ", ");
          write_u32(f, p.u32);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::SET_R32_TO_32;
          break;
        }

      case ByteCode::SET_R8_TO_8: {
          const auto p = ByteCode::PARSE::SET_R8_TO_8(code_i);

          write_str(f, "mov ");
          write_reg(f, p.val, b8_reg_name);
          write_str(f, ", ");
          write_u8(f, p.u8);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::SET_R8_TO_8;
          break;
        }
      case ByteCode::ADD_R64S: {
          const auto p = ByteCode::PARSE::ADD_R64S(code_i);

          write_str(f, "add ");
          write_reg(f, p.val2, b64_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::ADD_R64S;
          break;
        }
      case ByteCode::ADD_R8S: {
          const auto p = ByteCode::PARSE::ADD_R8S(code_i);

          write_str(f, "add ");
          write_reg(f, p.val2, b8_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val1, b8_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::ADD_R8S;
          break;
        }
      case ByteCode::SUB_R64S: {
          const auto p = ByteCode::PARSE::SUB_R64S(code_i);

          write_str(f, "sub ");
          write_reg(f, p.val2, b64_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::SUB_R64S;
          break;
        }
      case ByteCode::MUL_R64S: {
          const auto p = ByteCode::PARSE::MUL_R64S(code_i);

          write_str(f, "imul ");
          write_reg(f, p.val2, b64_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::MUL_R64S;
          break;
        }
      case ByteCode::DIV_RU64S: {
          const auto p = ByteCode::PARSE::DIV_RU64S(code_i);

          ASSERT(p.val2 == RAX.REG);

          write_str(f, "xor edx, edx\ndiv ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::DIV_RU64S;
          break;
        }
      case ByteCode::MOD_RU64S: {
          const auto p = ByteCode::PARSE::MOD_RU64S(code_i);

          ASSERT(p.val2 == RAX.REG);

          write_str(f, "xor edx, edx\ndiv ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::MOD_RU64S;
          break;
        }
      case ByteCode::DIV_RI64S: {
          const auto p = ByteCode::PARSE::DIV_RI64S(code_i);

          ASSERT(p.val2 == RAX.REG);

          write_str(f, "cdq\nidiv ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::DIV_RI64S;
          break;
        }
      case ByteCode::SHIFT_L_BY_R8_R64: {
          const auto p = ByteCode::PARSE::SHIFT_L_BY_R8_R64(code_i);

          ASSERT(p.val1 == RCX.REG);

          write_str(f, "sal ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val2, b8_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::SHIFT_L_BY_R8_R64;
          break;
        }
      case ByteCode::SHIFT_R_BY_R8_RU64: {
          const auto p = ByteCode::PARSE::SHIFT_R_BY_R8_RU64(code_i);

          ASSERT(p.val1 == RCX.REG);

          write_str(f, "shr ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val2, b8_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::SHIFT_R_BY_R8_RU64;
          break;
        }
      case ByteCode::SHIFT_R_BY_R8_RI64: {
          const auto p = ByteCode::PARSE::SHIFT_R_BY_R8_RI64(code_i);

          ASSERT(p.val1 == RCX.REG);

          write_str(f, "sar ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val2, b8_reg_name);
          write_str(f, "\n");


          code_i += ByteCode::SIZE_OF::SHIFT_R_BY_R8_RI64;
          break;
        }
      case ByteCode::OR_R64S: {
          const auto p = ByteCode::PARSE::OR_R64S(code_i);

          write_str(f, "or ");
          write_reg(f, p.val2, b64_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::OR_R64S;
          break;
        }
      case ByteCode::XOR_R64S: {
          const auto p = ByteCode::PARSE::XOR_R64S(code_i);

          write_str(f, "xor ");
          write_reg(f, p.val2, b64_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::XOR_R64S;
          break;
        }
      case ByteCode::AND_R64S: {
          const auto p = ByteCode::PARSE::AND_R64S(code_i);

          write_str(f, "and ");
          write_reg(f, p.val2, b64_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val1, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::AND_R64S;
          break;
        }
      case ByteCode::OR_R8S: {
          const auto p = ByteCode::PARSE::OR_R8S(code_i);

          write_str(f, "or ");
          write_reg(f, p.val2, b8_reg_name);
          write_str(f, ", ");
          write_reg(f, p.val1, b8_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::OR_R8S;
          break;
        }
        case ByteCode::XOR_R8S: {
            const auto p = ByteCode::PARSE::XOR_R8S(code_i);

            write_str(f, "xor ");
            write_reg(f, p.val2, b8_reg_name);
            write_str(f, ", ");
            write_reg(f, p.val1, b8_reg_name);
            write_str(f, "\n");

            code_i += ByteCode::SIZE_OF::XOR_R8S;
            break;
          }
        case ByteCode::AND_R8S: {
            const auto p = ByteCode::PARSE::AND_R8S(code_i);

            write_str(f, "and ");
            write_reg(f, p.val2, b8_reg_name);
            write_str(f, ", ");
            write_reg(f, p.val1, b8_reg_name);
            write_str(f, "\n");

            code_i += ByteCode::SIZE_OF::AND_R8S;
            break;
          }
      case ByteCode::EQ_R64S: {
          check_cmp_jump(f, &code_i, code_end,
                         jump_equal, jump_not_equal, sete);
          break;
        }
      case ByteCode::NEQ_R64S: {
          check_cmp_jump(f, &code_i, code_end,
                         jump_not_equal, jump_equal, setne);
          break;
        }
      case ByteCode::LESS_U64S: {
          check_cmp_jump(f, &code_i, code_end,
                         jump_below, jump_not_below, setb);
          break;
        }
      case ByteCode::GREAT_U64S: {
          check_cmp_jump(f, &code_i, code_end,
                         jump_above, jump_not_above, seta);
          break;
        }
      case ByteCode::LESS_I64S: {
          check_cmp_jump(f, &code_i, code_end,
                         jump_lesser, jump_not_lesser, setl);
          break;
        }
      case ByteCode::GREAT_I64S: {
          check_cmp_jump(f, &code_i, code_end,
                         jump_greater, jump_not_greater, setg);
          break;
        }
      case ByteCode::NEG_R64: {
          const auto p = ByteCode::PARSE::NEG_R64(code_i);

          write_str(f, "neg ");
          write_reg(f, p.val, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::NEG_R64;
          break;
        }
      case ByteCode::PUSH_R64: {
          const auto p = ByteCode::PARSE::PUSH_R64(code_i);

          write_str(f, "push ");
          write_reg(f, p.val, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::PUSH_R64;
          break;
        }
      case ByteCode::POP_TO_R64: {
          const auto p = ByteCode::PARSE::POP_TO_R64(code_i);

          write_str(f, "pop ");
          write_reg(f, p.val, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::POP_TO_R64;
          break;
        }
      case ByteCode::PUSH_FRAME: {
          //const auto p = ByteCode::PARSE::PUSH_FRAME(code_i);

          write_str(f, "push rbp\nmov rbp, rsp\n");

          code_i += ByteCode::SIZE_OF::PUSH_FRAME;
          break;
        }
      case ByteCode::POP_FRAME: {
          //const auto p = ByteCode::PARSE::POP_FRAME(code_i);

          write_str(f, "mov rsp, rbp\npop rbp\n");

          code_i += ByteCode::SIZE_OF::POP_FRAME;
          break;
        }
      case ByteCode::ALLOCATE_STACK: {
          const auto p = ByteCode::PARSE::ALLOCATE_STACK(code_i);

          write_str(f, "sub rsp, ");
          write_u64(f, p.u64.val);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::ALLOCATE_STACK;
          break;
        }
      case ByteCode::COPY_R64_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_R64_TO_MEM(code_i);

          write_str(f, "mov QWORD ");
          write_complex_mem(f, i.mem);
          write_str(f, ", ");
          write_reg(f, i.val, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_R64_TO_MEM;
          break;
        }
      case ByteCode::COPY_R32_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_R32_TO_MEM(code_i);

          write_str(f, "mov DWORD ");
          write_complex_mem(f, i.mem);
          write_str(f, ", ");
          write_reg(f, i.val, b32_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_R32_TO_MEM;
          break;
        }
      case ByteCode::COPY_R16_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_R16_TO_MEM(code_i);

          write_str(f, "mov WORD ");
          write_complex_mem(f, i.mem);
          write_str(f, ", ");
          write_reg(f, i.val, b16_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_R16_TO_MEM;
          break;
        }
      case ByteCode::COPY_R8_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_R8_TO_MEM(code_i);

          write_str(f, "mov BYTE ");
          write_complex_mem(f, i.mem);
          write_str(f, ", ");
          write_reg(f, i.val, b8_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_R8_TO_MEM;
          break;
        }
      case ByteCode::COPY_64_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_64_TO_MEM(code_i);

          write_str(f, "mov QWORD ");
          write_complex_mem(f, i.mem);
          write_str(f, ", ");
          write_u64(f, i.u64.val);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_64_TO_MEM;
          break;
        }
      case ByteCode::COPY_32_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_32_TO_MEM(code_i);

          write_str(f, "mov DWORD ");
          write_complex_mem(f, i.mem);
          write_str(f, ", ");
          write_u32(f, i.u32);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_32_TO_MEM;
          break;
        }
      case ByteCode::COPY_16_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_16_TO_MEM(code_i);

          write_str(f, "mov WORD ");
          write_complex_mem(f, i.mem);
          write_str(f, ", ");
          write_u16(f, i.u16);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_16_TO_MEM;
          break;
        }
      case ByteCode::COPY_8_TO_MEM: {
          const auto i = ByteCode::PARSE::COPY_8_TO_MEM(code_i);

          write_str(f, "mov BYTE ");
          write_complex_mem(f, i.mem);
          write_str(f, ", ");
          write_u8(f, i.u8);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_8_TO_MEM;
          break;
        }
      case ByteCode::LOAD_ADDRESS: {
          const auto i = ByteCode::PARSE::LOAD_ADDRESS(code_i);

          write_str(f, "lea ");
          write_reg(f, i.val, b64_reg_name);
          write_str(f, ", ");
          write_complex_mem(f, i.mem);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::LOAD_ADDRESS;
          break;
        }
      case ByteCode::COPY_R8_FROM_MEM: {
          const auto p = ByteCode::PARSE::COPY_R8_FROM_MEM(code_i);

          write_str(f, "mov ");
          write_reg(f, p.val, b8_reg_name);
          write_str(f, ", BYTE ");
          write_complex_mem(f, p.mem);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_R8_FROM_MEM;
          break;
        }
      case ByteCode::COPY_R64_FROM_MEM: {
          const auto i = ByteCode::PARSE::COPY_R64_FROM_MEM(code_i);

          write_str(f, "mov ");
          write_reg(f, i.val, b64_reg_name);
          write_str(f, ", QWORD ");
          write_complex_mem(f, i.mem);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_R64_FROM_MEM;
          break;
        }
      case ByteCode::COPY_R32_FROM_MEM: {
          const auto i = ByteCode::PARSE::COPY_R32_FROM_MEM(code_i);

          write_str(f, "mov ");
          write_reg(f, i.val, b32_reg_name);
          write_str(f, ", DWORD ");
          write_complex_mem(f, i.mem);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::COPY_R32_FROM_MEM;
          break;
        }
      case ByteCode::CONV_RU32_TO_R64: {
          //const auto i = ByteCode::PARSE::CONV_RU8_TO_R64(code_i);

          //write_str(f, "mov ");
          //write_reg(f, i.val, b64_reg_name);
          //write_str(f, ", ");
          //write_reg(f, i.val, b32_reg_name);
          //write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::CONV_RU8_TO_R64;
          break;
        }
      case ByteCode::CONV_RU8_TO_R64: {
          const auto i = ByteCode::PARSE::CONV_RU8_TO_R64(code_i);

          write_str(f, "movzx ");
          write_reg(f, i.val, b8_reg_name);
          write_str(f, ", ");
          write_reg(f, i.val, b64_reg_name);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::CONV_RU8_TO_R64;
          break;
        }
      case ByteCode::CONV_RI8_TO_R64: {
          const auto i = ByteCode::PARSE::CONV_RI8_TO_R64(code_i);

          write_str(f, "movsx ");
          write_reg(f, i.val, b8_reg_name);
          write_str(f, ", ");
          write_reg(f, i.val, b64_reg_name);
          write_str(f, "\n");


          code_i += ByteCode::SIZE_OF::CONV_RI8_TO_R64;
          break;
        }
      case ByteCode::LABEL: {
          const auto p = ByteCode::PARSE::LABEL(code_i);

          write_label(f, p.u64.val);
          write_str(f, ":\n");

          code_i += ByteCode::SIZE_OF::LABEL;
          break;
        }
      case ByteCode::RETURN: {
          write_str(f, "ret\n");

          code_i += ByteCode::SIZE_OF::RETURN;
          break;
        }
                           //case ByteCode::CALL_CONST: {
                           //    const auto p = ByteCode::PARSE::CALL_CONST(code_i);
                           //    const Function* func = p.u64;
                           //    
                           //    write_str(f, "call ");
                           //    write_label(f, func->code_block.label);
                           //    write_str(f, "\n");

                           //    code_i += ByteCode::SIZE_OF::CALL_CONST;
                           //    break;
                           //  }
                           //case ByteCode::CALL_MEM: {
                           //    const auto p = ByteCode::PARSE::CALL_MEM(code_i);

                           //    write_str(f, "call ");
                           //    write_complex_mem(f, p.mem);
                           //    write_str(f, "\n");

                           //    code_i += ByteCode::SIZE_OF::CALL_MEM;
                           //    break;
                           //  }
      case ByteCode::CALL_LABEL: {
          const auto p = ByteCode::PARSE::CALL_LABEL(code_i);
          const u64 label = p.u64;

          write_str(f, "call ");
          write_label(f, label);
          write_str(f, "\n");

          code_i += ByteCode::SIZE_OF::CALL_LABEL;
          break;
        }
      case ByteCode::JUMP_TO_FIXED: {
          const auto p_j = ByteCode::PARSE::JUMP_TO_FIXED(code_i);
          code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED;

          write_str(f, "jmp ");
          write_label(f, p_j.u64.val);
          write_str(f, "\n");
          break;
        }
      case ByteCode::JUMP_TO_FIXED_IF_VAL_ZERO: {
          const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

          write_str(f, "cmp ");
          write_reg(f, p.val, b64_reg_name);
          write_str(f, ", 0\n");

          jump_zero(f, p.u64.val);

          code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
          break;
        }
      case ByteCode::JUMP_TO_FIXED_IF_VAL_NOT_ZERO: {
          const auto p = ByteCode::PARSE::JUMP_TO_FIXED_IF_VAL_NOT_ZERO(code_i);

          write_str(f, "cmp ");
          write_reg(f, p.val, b64_reg_name);
          write_str(f, ", 0\n");

          jump_not_equal(f, p.u64.val);

          code_i += ByteCode::SIZE_OF::JUMP_TO_FIXED_IF_VAL_NOT_ZERO;
          break;
        }
      default: {
          uint8_t op = *code_i;
          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{},
                                    "NASM backend found unsupported bytecode instruction\n"
                                    "Code: {}, Name: {}",
                                    op, ByteCode::bytecode_string((ByteCode::ByteCodeOp)op));
          return;
        }
    }
  }
}

void nasm_backend(const char* file_name, CompilerGlobals* comp, CompilerThread* comp_thread) {
  FILES::OpenedFile f = FILES::replace(file_name, FILES::OPEN_MODE::WRITE);

  ASSERT(f.error_code == ErrorCode::OK);


  FileData file = {};
  file.data = f.file;
  file.top = 0;

  DEFER(&file) {
    //flush and close
    FILES::write(file.data, file.buffer, file.top);
    FILES::close(file.data);
  };

  write_str(&file, "segment .bss\n");

  {
    comp->globals_mutex.acquire();

    auto i = comp->globals_single_threaded.begin_const_iter();
    auto end = comp->globals_single_threaded.end_const_iter();

    for (; i != end; i.next()) {
      const Global* g = i.get();

      if ((g->decl.meta_flags & META_FLAG::COMPTIME) == 0) {
        ASSERT(g->decl.type.structure->size == 8);//TEMP

        write_str(&file, g->decl.name);
        write_str(&file, ":\n RESQ 1\n");
      }
    }

    comp->globals_mutex.release();
  }

  write_str(&file, "segment .text\n");

  write_str(&file, "global ");
  write_str(&file, comp->build_options.entry_point);
  write_str(&file, "\n");

  {
    auto* i = comp->lib_import.begin();
    auto* end = comp->lib_import.end();

    for (; i < end; i++) {
      write_str(&file, "extern ");
      write_str(&file, i->name);
      write_str(&file, "\n");
    }
  }

  {
    auto* i = comp->lib_import.begin();
    auto* end = comp->lib_import.end();

    for (; i < end; i++) {
      write_label(&file, i->label);
      write_str(&file, ":\njmp ");
      write_str(&file, i->name);
      write_str(&file, "\n");
    }
  }

  {
    comp->globals_mutex.acquire();

    auto i = comp->globals_single_threaded.begin_const_iter();
    auto end = comp->globals_single_threaded.end_const_iter();

    for (; i != end; i.next()) {
      const Global* g = i.get();

      if ((g->decl.meta_flags & META_FLAG::COMPTIME) == 0) {
        ASSERT(g->decl.type.structure->size == 8);//TEMP

        write_code(&file, comp, comp_thread, &g->init);
        if (comp_thread->is_panic()) {
          return;
        }
      }
    }

    comp->globals_mutex.release();
  }

  {
    comp->functions_mutex.acquire();

    auto i = comp->functions_single_threaded.begin_const_iter();
    auto end = comp->functions_single_threaded.end_const_iter();

    for (; i != end; i.next()) {
      const Function* f = i.get();

      //write_label(&file, f->code_block.label);
      //write_str(&file, ":\n");
      write_code(&file, comp, comp_thread, &f->code_block);
      if (comp_thread->is_panic()) {
        return;
      }
    }

    comp->functions_mutex.release();
  }

  {



    write_str(&file, comp->build_options.entry_point);
    write_str(&file, ":\n");
    {
      comp->globals_mutex.acquire();

      auto i = comp->globals_single_threaded.begin_const_iter();
      auto end = comp->globals_single_threaded.end_const_iter();

      for (; i != end; i.next()) {
        const Global* g = i.get();

        if ((g->decl.meta_flags & META_FLAG::COMPTIME) == 0) {
          ASSERT(g->decl.type.structure->size == 8);//TEMP

          write_str(&file, "call ");
          write_label(&file, g->init.label);
          write_str(&file, "\n");
        }
      }

      comp->globals_mutex.release();
    }



    write_str(&file, "jmp ");
    usize label;
    {
      auto names = comp->services.names.get();
      const GlobalName* n = names->find_global_name(comp->build_file_namespace, comp->build_options.entry_point);
      ASSERT(n != nullptr);
      label = *(usize*)n->global->constant_value.ptr;
    }
    write_label(&file, label);
    write_str(&file, "\n");
  }
}

#endif