#include "names.h"
#include "compiler.h"

#include <AxleUtil/io.h>

const GlobalName* NameManager::add_global_name_impl(Errors* const errors, Namespace* ns, const Axle::InternString* name, Global* g) const {
  ASSERT(name != nullptr);
  ASSERT(g != nullptr);
  
  {
    const GlobalName* n = find_direct_global_name(ns, name);

    if (n != nullptr) {
      errors->report_error(ERROR_CODE::NAME_ERROR, g->decl.span,
                                "Attempted to shadow name '{}'",
                                name);
      return nullptr;
    }
  }

  ns->globals.insert_uninit(1);
  GlobalName* n = ns->globals.back();
  n->name = name;
  n->global = g;

  return n;
}

void NameManager::add_global_import_impl(Errors* const errors, Namespace* ns, const Namespace* imp, const Span& s) const {
  FOR(ns->imported, it) {
    if (*it == imp) {
      errors->report_error(ERROR_CODE::INTERNAL_ERROR, s,
                         "Attempted to import the same namespace multiple times");
      return;
    }
  }

  ns->imported.insert(imp);
}

const GlobalName* add_global_name(CompilerGlobals* comp, CompilerThread* comp_thread, NameManager* names, Namespace* ns, const Axle::InternString* name, Global* g) {
  if (!comp->names_updated) {
    comp->names_updated = true;
    ++comp->available_work_counter;

    if (comp->print_options.work) {
      Axle::IO::print("Work + | Added names\n");
    }
  }

  Axle::IO::format("Names | \"{}\" created\n", name);

  return names->add_global_name_impl(&comp_thread->errors, ns, name, g);
}

void add_global_import(CompilerGlobals* comp, CompilerThread* comp_thread, NameManager* names, Namespace* ns, const Namespace* imp, const Span& s) {
  if (!comp->names_updated) {
    comp->names_updated = true;
    ++comp->available_work_counter;

    if (comp->print_options.work) {
      Axle::IO::print("Work + | Added names\n");
    }
  }

  return names->add_global_import_impl(&comp_thread->errors, ns, imp, s);
}

const GlobalName* NameManager::find_direct_global_name(const Namespace* ns, const Axle::InternString* name) const {
  FOR(ns->globals, it) {
    if (it->name == name) {
      return it;
    }
  }

  return nullptr;
}

const GlobalName* NameManager::find_global_name(const Namespace* ns, const Axle::InternString* name) const {
  ASSERT(name != nullptr);

  const GlobalName* n = find_direct_global_name(ns, name);
  if (n != nullptr) {
    return n;
  }

  FOR(ns->imported, it) {
    n = find_direct_global_name(*it, name);
    if (n != nullptr) {
      return n;
    }
  }

  return nullptr;
}


NameFindItr NameManager::global_name_iterator(const Namespace* ns, const Axle::InternString* name) const {
  return { ns, name, 0, 0 };
}

const GlobalName* NameManager::next_name(NameFindItr& i) const {
  const Namespace* ns = i.ns;
  
  if(i.import_index == 0) {
    for(; i.name_index < ns->globals.size;) {
      GlobalName* gn = &ns->globals.data[i.name_index];
      i.name_index += 1;

      if(gn->name == i.target) {
        return gn;
      }
    }

    i.import_index = 1;
    i.name_index = 0;
  }

  for(; i.import_index < (ns->imported.size + 1);) {
    const Namespace* curr_ns = ns->imported.data[i.import_index - 1];
    for(; i.name_index < curr_ns->globals.size;) {
      GlobalName* gn = &curr_ns->globals.data[i.name_index];
      i.name_index += 1;

      if(gn->name == i.target) {
        return gn;
      }
    }

    i.import_index += 1;
    i.name_index = 0;
  }

  return nullptr;
}
