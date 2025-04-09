#pragma once

#include <Axle/comp_utilities.h>
#include <AxleUtil/utility.h>

#include "ast.h"

namespace DC {
  Axle::OwnedArr<AstVisit>
    dependency_check_ast(CompilerGlobals* const comp,
                         CompilerThread* const comp_thread,
                         Namespace* const available_names,
                         AST_LOCAL a);
}
