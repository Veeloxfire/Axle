#pragma once

#include <AxleUtil/utility.h>
#include <Axle/comp_utilities.h>
#include "errors.h"

struct GlobalName {
  const Axle::InternString* name;
  Global* global;
};

struct Namespace {
  Axle::Array<GlobalName> globals ={};
  Axle::Array<const Namespace*> imported ={};
};

struct NameFindItr {
  const Namespace* ns;
  const Axle::InternString* target;
  usize import_index;
  usize name_index;
};

struct NameManager {
  const GlobalName* add_global_name_impl(Errors* const errors, Namespace* ns, const Axle::InternString* name, Global* g) const;

  void add_global_import_impl(Errors* const errors, Namespace* ns, const Namespace* imp, const Span& s) const;

  const GlobalName* find_global_name(const Namespace* ns, const Axle::InternString* name) const;
  const GlobalName* find_direct_global_name(const Namespace* ns, const Axle::InternString* name) const;

  NameFindItr global_name_iterator(const Namespace* ns, const Axle::InternString* name) const;
  const GlobalName* next_name(NameFindItr&) const;
};

const GlobalName* add_global_name(CompilerGlobals* comp, CompilerThread* comp_thread, NameManager* names, Namespace* ns, const Axle::InternString* name, Global* g);

void add_global_import(CompilerGlobals* comp, CompilerThread* comp_thread, NameManager* names, Namespace* ns, const Namespace* imp, const Span& s);
