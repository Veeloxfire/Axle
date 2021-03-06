#pragma once

#include "utility.h"
#include "comp_utilities.h"

struct GlobalName {
  //Can be null in some cases
  UnitID unit_id;
  const InternString* name;
  Global* global;
};

struct Namespace {
  Array<GlobalName> globals ={};
  Array<Namespace*> imported ={};
};

//Exists for mutex stuff
struct NameManager {
  GlobalName* add_global_name(CompilerThread* const comp, Namespace* ns, const InternString* name, UnitID unit_id, Global* g);

  void add_global_import(CompilerThread* const comp, Namespace* ns, Namespace* imp, const Span& s);

  GlobalName* find_global_name(Namespace* ns, const InternString* name);
};