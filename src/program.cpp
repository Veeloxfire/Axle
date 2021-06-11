#include "Program.h"
#include "utility.h"

DllImportFile next_import_file(const Program* prog, DllImportIterator* imports) {
  
  uint64_t offset = x64_from_bytes(imports->imports);

  //Finished
  if (offset == 0) {
    return { nullptr, nullptr };
  }

  const char* dll_name = (const char*)(prog->data.ptr + offset);
  imports->imports += 8;
  const uint8_t* imports_src = imports->imports;

  offset = x64_from_bytes(imports->imports);

  //Load to the next dll
  while(offset != 0) {
    imports->imports += 16;
    offset = x64_from_bytes(imports->imports);
  }

  imports->imports += 8;

  //Null terminated so the next one will be after the last null terminate

  return {dll_name, imports_src };
}

DllImportFunction next_import(const Program* prog, DllImportFile* f_import) {
  uint64_t offset = x64_from_bytes(f_import->imports);

  //null terminated
  if (offset == 0) {
    return { nullptr, 0 };
  }

  const char* name = (const char*)(prog->data.ptr + offset);

  f_import->imports += 8;
  offset = x64_from_bytes(f_import->imports);
  f_import->imports += 8;

  uint8_t* load_index = prog->data.ptr + offset;

  return { name, load_index };
}