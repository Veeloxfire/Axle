#include "names.h"
#include "compiler.h"

GlobalName* add_global_name(Compiler* const comp, Namespace* ns, const InternString* name, CompilationUnit* unit, Global* g) {
  ASSERT(name != nullptr);
  //CAN BE NULL
  //ASSERT(unit != nullptr;
  ASSERT(g != nullptr);
  
  GlobalName* n = find_global_name(ns, name);

  if (n != nullptr) {
    comp->report_error(ERROR_CODE::NAME_ERROR, g->decl.span,
                       "Attempted to shadow name '{}'",
                       name);
    return nullptr;
  }

  ns->globals.insert_uninit(1);
  n = ns->globals.back();
  n->name = name;
  n->unit = unit;
  n->global = g;

  return n;
}

void add_global_import(Compiler* const comp, Namespace* ns, Namespace* imp, const Span& s) {
  FOR(ns->imported, it) {
    if (*it == imp) {
      comp->report_error(ERROR_CODE::NAME_ERROR, s,
                         "Attempted to import the same namespace multiple times");
      return;
    }
  }

  ns->imported.insert(imp);
}

static GlobalName* find_owned_global_name(Namespace* ns, const InternString* name) {
  FOR_MUT(ns->globals, it) {
    if (it->name == name) {
      return it;
    }
  }

  return nullptr;
}

GlobalName* find_global_name(Namespace* ns, const InternString* name) {
  ASSERT(name != nullptr);

  GlobalName* n = find_owned_global_name(ns, name);
  if (n != nullptr) {
    return n;
  }

  FOR(ns->imported, it) {
    n = find_owned_global_name(*it, name);
    if (n != nullptr) {
      return n;
    }
  }

  return nullptr;
}