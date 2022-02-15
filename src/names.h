#pragma once

#include "utility.h"

struct Compiler;
struct InternString;
struct Span;
struct Global;
struct CompilationUnit;

struct GlobalName {
  //Can be null in some cases
  CompilationUnit* unit;
  const InternString* name;
  Global* global;
};

struct Namespace {
  Array<GlobalName> globals ={};
  Array<Namespace*> imported ={};
};

GlobalName* add_global_name(Compiler* const comp, Namespace* ns, const InternString* name, CompilationUnit* unit, Global* g);

void add_global_import(Compiler* const comp, Namespace* ns, Namespace* imp, const Span& s);

GlobalName* find_global_name(Namespace* ns, const InternString* name);