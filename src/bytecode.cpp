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

  void print_bytecode(REG_NAME reg_name_from_num,
                      FILE* const stream,
                      const uint8_t* bytecode,
                      uint64_t size) {

  #define OP_64_64 printf(": 0x%llx 0x%llx\n", p.u64_1.val, p.u64_2.val)

  #define OP_8_64 printf(": 0x%hhx 0x%llx\n", p.u8, p.u64.val)

  #define OP_R_64 const char* const reg_name = reg_name_from_num(p.val);\
                       printf(": $%s 0x%llx\n", reg_name, p.u64.val)

  #define OP_64 printf(": 0x%llx\n", p.u64.val)

  #define OP printf("\n")

  #define OP_R const char* const reg_name = reg_name_from_num(p.val);\
                    printf(": $%s\n", reg_name)

  #define OP_R_R const char* const reg_name1 = reg_name_from_num(p.val1);\
                        printf(": $%s", reg_name1);\
                        const char* const reg_name2 = reg_name_from_num(p.val2);\
                        printf(" $%s\n", reg_name2)

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
    #undef OP_8_64
    #undef OP_64_64
    #undef OP_64
    #undef OP
    #undef OP_R
    #undef OP_R_R

    fflush(stream);
  }
}