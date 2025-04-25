#pragma once

#include <Axle/comp_utilities.h>
#include <AxleUtil/utility.h>

#include "ast.h"

namespace DC {
  Axle::OwnedArr<AstVisit> type_dependency_check_ast(
    CompilerGlobals* const comp,
    CompilerThread* const comp_thread,
    Namespace* const available_names,
    AST_LOCAL a
  ) noexcept;

  void eval_dependency_check_ast(
    CompilerGlobals* const comp,
    CompilerThread* const comp_thread,
    AST_LOCAL a
  ) noexcept;
}
