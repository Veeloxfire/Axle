#include "bytecode.h"
#include "calling_conventions.h"

namespace ByteCode {
  static void print_bytecodeop(FILE* const stream, const uint8_t b) {
    switch (b) {
    #define X(NAME, statement) case NAME: fprintf(stream, #NAME); break;
      BYTECODES_X
      #undef X
    }
  }

  void log_bytecode(REG_NAME reg_name_from_num, const uint8_t* bytecode, uint64_t size) {
    print_bytecode(reg_name_from_num, stdout, bytecode, size);
  }

  void error_bytecode(REG_NAME reg_name_from_num, const uint8_t* bytecode, uint64_t size) {
    print_bytecode(reg_name_from_num, stderr, bytecode, size);
  }

  static void print_mem_complex(REG_NAME reg_name_from_num, const MemComplex& mem) {
    const char* reg_name = reg_name_from_num(mem.base);
    char c = mem.disp > 0 ? '+' : '-';
    printf("[$%s %c 0x%lx", reg_name, c, absolute(mem.disp));

    if (mem.scale > 0) {
      reg_name = reg_name_from_num(mem.index);
      printf(" + ($%s * 0x%hhx)]", reg_name, mem.scale);
    }
    else {
      printf("]");
    }
  }

  void print_bytecode(REG_NAME reg_name_from_num,
                      FILE* const stream,
                      const uint8_t* bytecode,
                      uint64_t size) {

  #define OP_64_MEM printf(": 0x%llx ", p.u64.val);\
                    print_mem_complex(reg_name_from_num, p.mem);\
                    printf("\n")

  #define OP_32_MEM printf(": 0x%lx ", p.u32);\
                    print_mem_complex(reg_name_from_num, p.mem);\
                    printf("\n")

  #define OP_16_MEM printf(": 0x%hx ", p.u16);\
                    print_mem_complex(reg_name_from_num, p.mem);\
                    printf("\n")

  #define OP_8_MEM printf(": 0x%hhx ", p.u8);\
                   print_mem_complex(reg_name_from_num, p.mem);\
                   printf("\n")

  #define OP_R_64 const char* const reg_name = reg_name_from_num(p.val);\
                       printf(": $%s 0x%llx\n", reg_name, p.u64.val)

  #define OP_R_32 const char* const reg_name = reg_name_from_num(p.val);\
                       printf(": $%s 0x%lx\n", reg_name, p.u32)

  #define OP_R_16 const char* const reg_name = reg_name_from_num(p.val);\
                       printf(": $%s 0x%hx\n", reg_name, p.u16)

  #define OP_R_8 const char* const reg_name = reg_name_from_num(p.val);\
                       printf(": $%s 0x%hhx\n", reg_name, p.u8)

  #define OP_R_MEM const char* const reg_name = reg_name_from_num(p.val);\
                        printf(": $%s ", reg_name);\
                        print_mem_complex(reg_name_from_num, p.mem);\
                        printf("\n")

  #define OP_64 printf(": 0x%llx\n", p.u64.val)

  #define OP printf("\n")

  #define OP_R const char* const reg_name = reg_name_from_num(p.val);\
                    printf(": $%s\n", reg_name)

  #define OP_R_R const char* reg_name = reg_name_from_num(p.val1);\
                        printf(": $%s", reg_name);\
                        reg_name = reg_name_from_num(p.val2);\
                        printf(" $%s\n", reg_name)


  #define X(name, structure) case name: {\
      auto p = ByteCode::PARSE:: ## name ## (bytecode + i);\
      printf("0x%-4llx: ", i);\
      print_bytecodeop(stream, p.op);\
      structure;\
      i += ByteCode::SIZE_OF:: ## name;\
      break;\
    }

    uint64_t i = 0;
    while (i < size) {
      switch (bytecode[i]) {
        BYTECODES_X
        default:
          printf("ERROR INSTRUCTION\n");
          return;
      }
    }

  #undef OP_R_64
  #undef OP_R_MEM
  #undef OP_8_MEM
  #undef OP_16_MEM
  #undef OP_32_MEM
  #undef OP_64_MEM
  #undef OP_64
  #undef OP
  #undef OP_R
  #undef OP_R_R

    fflush(stream);
  }
}