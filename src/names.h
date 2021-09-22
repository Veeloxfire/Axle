#pragma once
#include "utility.h"
#include "comp_utilities.h"
#include "strings.h"
#include "type.h"

struct Global;
struct ASTDecl;
struct Span;

struct NamedElement {
  Array<const Global*> globals;

  u64 unknowns;
};

struct Namespace {
  bool is_sub_namespace = false;
  NamespaceIndex inside ={};

  InternHashTable<NamedElement> names ={};
  Array<NamespaceIndex> imported ={};
};

struct NamespaceElement {
  NamedElement* named_element;
  NamespaceIndex ns_index;
};

struct NamesHandler {
  //Namespace that is available everywhere in the program (contains the language primitives)
  NamespaceIndex builtin_namespace ={};
  Array<Namespace> all_namespaces ={};

  NamesHandler();

  NamespaceIndex new_namespace();
  Namespace* get_raw_namespace(NamespaceIndex index);

  NamedElement* find_name(NamespaceIndex ns_index, const InternString* name) const noexcept;
  NamedElement* find_unimported_name(NamespaceIndex ns_index, const InternString* name) const noexcept;

  //Same as 'find_name' but returns all of the possible options - useful for overload sets
  Array<NamespaceElement> find_all_names(NamespaceIndex ns_index, const InternString* name) const noexcept;

  NamedElement* create_name(NamespaceIndex ns_index, const InternString* name) noexcept;
};

void assert_empty_name(Compiler* comp, const Span& span, NamespaceIndex ns, const InternString* name);
