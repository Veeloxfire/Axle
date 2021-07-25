#include "names.h"
#include "compiler.h"

bool NamedElement::is_valid() const noexcept {
  return ((int)(overloads.size > 0)
          + (int)(func_pointer != nullptr)
          + (int)(structure != nullptr)
          + (int)(enum_value != nullptr)
          + (int)(global != nullptr)) == 1;
}

NamedElement* NamesHandler::find_name(NamespaceIndex ns_index, const InternString* name) const noexcept {
  //first try the builtin namespace for primitive and stuff
  const Namespace* ns  = all_namespaces.data + builtin_namespace.index;
  NamedElement* el = ns->names.get_val(name);

  if (el != nullptr) {
    return el;
  }

  //now test the actual namespace
  ns = all_namespaces.data + ns_index.index;

  while (true) {
    el = ns->names.get_val(name);

    if (el != nullptr) {
      return el;
    }

    //Check from imported namespaces
    auto i = ns->imported.begin();
    const auto end = ns->imported.end();

    for (; i < end; i++) {
      const Namespace* imported = all_namespaces.data + i->index;

      el = imported->names.get_val(name);
      if (el != nullptr) {
        return el;
      }
    }


    if (ns->is_sub_namespace) {
      //Instead of recursion
      ns = all_namespaces.data + ns->inside.index;
    }
    else {
      break;
    }
  }

  //Didnt find it at all
  return nullptr;
}

NamedElement* NamesHandler::find_unimported_name(NamespaceIndex ns_index, const InternString* name) const noexcept {
  //first try the builtin namespace for primitive and stuff
  const Namespace* ns = all_namespaces.data + builtin_namespace.index;
  NamedElement* el = ns->names.get_val(name);

  if (el != nullptr) {
    return el;
  }

  //now test the actual namespace
  ns = all_namespaces.data + ns_index.index;
  return ns->names.get_val(name);
}

Array<NamedElement*> NamesHandler::find_all_names(NamespaceIndex ns_index, const InternString* name) const noexcept {
  Array<NamedElement*> all_names ={};

  //first try the builtin namespace for primitive and stuff
  const Namespace* ns = all_namespaces.data + builtin_namespace.index;
  NamedElement* el = ns->names.get_val(name);

  if (el != nullptr) {
    all_names.insert(el);
  }

  //now test the actual namespace
  ns = all_namespaces.data + ns_index.index;

  while (true) {
    el = ns->names.get_val(name);

    if (el != nullptr) {
      all_names.insert(el);
    }

    //Load from imported namespaces
    auto i = ns->imported.begin();
    const auto end = ns->imported.end();

    for (; i < end; i++) {
      const Namespace* imported = all_namespaces.data + i->index;

      el = imported->names.get_val(name);
      if (el != nullptr) {
        all_names.insert(el);
      }
    }


    if (ns->is_sub_namespace) {
      //Instead of recursion
      ns = all_namespaces.data + ns->inside.index;
    }
    else {
      break;
    }
  }

  //Finally return
  return all_names;
}

NamedElement* NamesHandler::create_name(NamespaceIndex ns_index, const InternString* name) noexcept {
  NamedElement* pos_empty = find_name(ns_index, name);

  if (pos_empty == nullptr) {
    return (all_namespaces.data + ns_index.index)->names.insert(name);
  }
  else {
    //Wasnt empty
    return nullptr;
  }
}

void init_names_handler(NamesHandler* uninit_names_handler) {
  uninit_names_handler->builtin_namespace = NamespaceIndex{ 0 };

  assert(uninit_names_handler->all_namespaces.size == 0);
  uninit_names_handler->all_namespaces.insert_uninit(1);
}

void assert_empty_name(Compiler* comp, const Span& span, NamespaceIndex ns, const InternString* name) {
  const NamedElement* possible_name = comp->names->find_name(ns, name);

  if (possible_name != nullptr) {
    //Is actually the name for something else

    if (possible_name->structure != nullptr) {
      comp->report_error(CompileCode::NAME_ERROR, span,
                         "Attempted to shadow the type '{}'",
                         name);
    }
    else if (possible_name->enum_value != nullptr) {
      comp->report_error(CompileCode::NAME_ERROR, span,
                         "Attempted to shadow the enum value '{}'",
                         name);
    }
    else if (possible_name->func_pointer != nullptr || possible_name->overloads.size > 0) {
      comp->report_error(CompileCode::NAME_ERROR, span,
                         "Attempted to shadow the function '{}'",
                         name);
    }
    else if (possible_name->global != nullptr) {
      comp->report_error(CompileCode::NAME_ERROR, span,
                         "Attempted to shadow a global value '{}'",
                         name);
    }

    //Missing an option
    assert(false);
  }
}