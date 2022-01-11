#include "names.h"
#include "compiler.h"

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

Array<NamespaceElement> NamesHandler::find_all_names(NamespaceIndex ns_index, const InternString* name) const noexcept {
  Array<NamespaceElement> all_names ={};

  //first try the builtin namespace for primitive and stuff
  NamespaceIndex current = builtin_namespace;
  const Namespace* ns = all_namespaces.data + current.index;
  NamedElement* el = ns->names.get_val(name);

  if (el != nullptr) {
    all_names.insert({ el, current });
  }

  //now test the actual namespace
  current = ns_index;
  ns = all_namespaces.data + current.index;

  while (true) {
    el = ns->names.get_val(name);

    if (el != nullptr) {
      all_names.insert({ el, current });
    }

    //Load from imported namespaces
    auto i = ns->imported.begin();
    const auto end = ns->imported.end();

    for (; i < end; i++) {
      current = *i;
      const Namespace* imported = all_namespaces.data + current.index;

      el = imported->names.get_val(name);
      if (el != nullptr) {
        all_names.insert({ el, current });
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

NamesHandler::NamesHandler() {
  builtin_namespace = NamespaceIndex{ 0 };
  all_namespaces.insert_uninit(1);
}

void assert_empty_name(Compiler* comp, const Span& span, NamespaceIndex ns, const InternString* name) {
  const NamedElement* possible_name = comp->services.names->find_name(ns, name);

  if (possible_name != nullptr) {
    //Is actually the name for something else
    comp->report_error(ERROR_CODE::NAME_ERROR, span,
                       "Attempted to shadow '{}'", name);
  }
}

NamespaceIndex NamesHandler::new_namespace() {
  NamespaceIndex ret ={ all_namespaces.size };
  all_namespaces.insert_uninit(1);

  return ret;
}
Namespace* NamesHandler::get_raw_namespace(NamespaceIndex index) {
  return all_namespaces.data + index.index;
}