#pragma once
#include <Axle/comp_utilities.h>

#include "ast.h"

struct Namespace;

namespace TC {
  void type_check_ast(CompilerGlobals* comp,
                      CompilerThread* comp_thread,
                      Namespace* ns,
                      const Axle::ViewArr<const AstVisit> visit_arr);
}
