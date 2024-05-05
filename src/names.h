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
  Axle::Array<Namespace*> imported ={};
};

struct NameFindItr {
  Namespace* ns;
  const Axle::InternString* target;
  usize import_index;
  usize name_index;
};

//Is a struct because its easier to multithread names this way
struct NameManager {
  GlobalName* add_global_name(Errors* const errors, Namespace* ns, const Axle::InternString* name, Global* g) const;

  void add_global_import(Errors* const errors, Namespace* ns, Namespace* imp, const Span& s) const;

  GlobalName* find_global_name(Namespace* ns, const Axle::InternString* name) const;
  GlobalName* find_direct_global_name(Namespace* ns, const Axle::InternString* name) const;

  NameFindItr global_name_iterator(Namespace* ns, const Axle::InternString* name) const;
  GlobalName* next_name(NameFindItr&) const;
};
