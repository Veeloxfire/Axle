#pragma once
#include "safe_lib.h"
#include "utility.h"

struct System;
struct CallingConvention;

struct Program {
  const System* sys = nullptr;
  const CallingConvention* default_conv = nullptr;
  
  OwnedPtr<uint8_t> imports = nullptr;

  OwnedPtr<uint8_t> data = nullptr;
  size_t data_size = 0;

  OwnedPtr<uint8_t> code = nullptr;
  size_t code_size = 0;

  size_t entry_point = 0;
};

struct DllImportIterator {
  const uint8_t* imports = nullptr;
};

struct DllImportFile {
  const char* file_name = nullptr;
  const uint8_t* imports = nullptr;
};

struct DllImportFunction {
  const char* name;
  uint8_t* load_to;
};

DllImportFile next_import_file(const Program* prog, DllImportIterator*);
DllImportFunction next_import(const Program* prog, DllImportFile*);