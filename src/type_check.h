#pragma once
#include "utility.h"
#include "type.h"
#include "comp_utilities.h"

namespace TC {
  void type_check_ast(CompilerGlobals* comp,
                      CompilerThread* comp_thread,
                      Namespace* ns, AST_LOCAL root, const Type& infer);
}