#pragma once
#include <Axle/comp_utilities.h>

#include "ast.h"

struct Namespace;

namespace TC {
  struct TypeCheckContext {
    usize next_index;
    Axle::ViewArr<const AstVisit> visit_arr;
    Namespace* ns;

    constexpr bool finished() const {
      return next_index == visit_arr.size;
    }
  };
  
  void type_check_ast(CompilerGlobals* comp,
                      CompilerThread* comp_thread,
                      TypeCheckContext& context);
}
