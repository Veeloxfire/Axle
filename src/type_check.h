#pragma once
#include "type.h"
#include <Axle/comp_utilities.h>

struct Namespace;

namespace TC {
  void type_check_ast(CompilerGlobals* comp,
                      CompilerThread* comp_thread,
                      Namespace* ns, AST_LOCAL root, const Type& infer);
}
