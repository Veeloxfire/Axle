#pragma once
#include "utility.h"
#include "comp_utilities.h"
#include "strings.h"

struct Function;
struct FunctionPointer;
struct Structure;
struct EnumValue;
struct Global;
struct ASTDecl;


struct NamedElement {
  Array<Function*> overloads;
  FunctionPointer* func_pointer;
  const Structure* structure;
  const EnumValue* enum_value;
  const Global* global;

  Array<const ASTDecl*> unknowns;

  bool is_valid() const noexcept;
};

struct Namespace {
  bool is_sub_namespace = false;
  NamespaceIndex inside ={};

  InternHashTable<NamedElement> names ={};
  Array<NamespaceIndex> imported ={};
};

struct NamesHandler {
  //Namespace that is available everywhere in the program (contains the language primitives)
  NamespaceIndex builtin_namespace ={};
  Array<Namespace> all_namespaces ={};

  NamespaceIndex new_namespace();
  Namespace* get_raw_namespace(NamespaceIndex);

  NamedElement* find_name(NamespaceIndex ns_index, const InternString* name) const noexcept;
  NamedElement* find_unimported_name(NamespaceIndex ns_index, const InternString* name) const noexcept;

  //Same as 'find_name' but returns all of the possible options - useful for overload sets
  Array<NamedElement*> find_all_names(NamespaceIndex ns_index, const InternString* name) const noexcept;

  NamedElement* create_name(NamespaceIndex ns_index, const InternString* name) noexcept;
};

void init_names_handler(NamesHandler* uninit_names_handler);
void assert_no_shadow(Compiler* comp, const Span& span, NamespaceIndex ns, const InternString* name);
