#include <AxleTest/unit_tests.h>

#include "names.h"
#include "compiler.h"

TEST_FUNCTION(names, add_names) {
  NameManager name_manager = {};
  StringInterner strings = {};

  Namespace ns = {};

  Global n0_g = {};
  const InternString* n0_name = strings.intern(lit_view_arr("hello"));
  Global n1_g = {};
  const InternString* n1_name = strings.intern(lit_view_arr("world"));

  Errors errors = {};
  {
    GlobalName* gn = name_manager.add_global_name(&errors, &ns, n0_name, &n0_g);
    if(errors.is_panic()) {
      test_errors->report_error("Failed to add global name\nReason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }
   
    TEST_NEQ(gn, static_cast<GlobalName*>(nullptr));
    TEST_EQ(gn->name, n0_name);
    TEST_EQ(gn->global, &n0_g);
  }
  {
    GlobalName* gn = name_manager.add_global_name(&errors, &ns, n1_name, &n1_g);
    if(errors.is_panic()) {
      test_errors->report_error("Failed to add global name\nReason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }

    TEST_NEQ(gn, static_cast<GlobalName*>(nullptr));
    TEST_EQ(gn->name, n1_name);
    TEST_EQ(gn->global, &n1_g);
  }

  {
    GlobalName* gn0 = name_manager.find_global_name(&ns, n0_name);
    GlobalName* gn1 = name_manager.find_global_name(&ns, n0_name);
    TEST_EQ(gn0->name, n0_name);
    TEST_EQ(gn0->global, &n0_g);
    TEST_EQ(gn1->name, n0_name);
    TEST_EQ(gn1->global, &n0_g);
    TEST_EQ(gn0, gn1);

    GlobalName* gn2 = name_manager.find_direct_global_name(&ns, n0_name);
    GlobalName* gn3 = name_manager.find_direct_global_name(&ns, n0_name);
    TEST_EQ(gn2->name, n0_name);
    TEST_EQ(gn2->global, &n0_g);
    TEST_EQ(gn3->name, n0_name);
    TEST_EQ(gn3->global, &n0_g);
    TEST_EQ(gn2, gn3);

    TEST_EQ(gn0, gn2);
  }
  {
    GlobalName* gn = name_manager.add_global_name(&errors, &ns, n0_name, &n0_g);
    if(!errors.is_panic()) {
      test_errors->report_error("Shadowing was allowed");
      return;
    }
    TEST_EQ(gn, static_cast<GlobalName*>(nullptr));
  }
}

TEST_FUNCTION(names, add_import) {
  NameManager name_manager = {};

  Namespace ns = {};
  Namespace ns1 = {};
  Namespace ns2 = {};
  Namespace ns3 = {};

 
  {
    Errors errors = {};
    name_manager.add_global_import(&errors, &ns, &ns1, Span{});
    if(errors.is_panic()) {
      test_errors->report_error("Import failed. Reason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }

    TEST_EQ(static_cast<usize>(1), ns.imported.size);
    TEST_EQ(&ns1, ns.imported[0]);
  }

  {
    Errors errors = {};
    name_manager.add_global_import(&errors, &ns, &ns1, Span{});
    if(!errors.is_panic()) {
      test_errors->report_error("Import doubling was allowed");
      return;
    }

    TEST_EQ(static_cast<usize>(1), ns.imported.size);
    TEST_EQ(&ns1, ns.imported[0]);
  }

  {
    Errors errors = {};
    name_manager.add_global_import(&errors, &ns, &ns2, Span{});
    if(errors.is_panic()) {
      test_errors->report_error("Import failed. Reason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }

    TEST_EQ(static_cast<usize>(2), ns.imported.size);
    TEST_EQ(&ns1, ns.imported[0]);
    TEST_EQ(&ns2, ns.imported[1]);
  }

  {
    Errors errors = {};
    name_manager.add_global_import(&errors, &ns2, &ns3, Span{});
    if(errors.is_panic()) {
      test_errors->report_error("Import failed. Reason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }

    TEST_EQ(static_cast<usize>(2), ns.imported.size);
    TEST_EQ(&ns1, ns.imported[0]);
    TEST_EQ(&ns2, ns.imported[1]);

    TEST_EQ(static_cast<usize>(1), ns2.imported.size);
    TEST_EQ(&ns3, ns2.imported[0]);
  }
}

TEST_FUNCTION(names, importing_names) {
  NameManager name_manager = {};
  StringInterner strings = {};

  Namespace ns0 = {};
  Namespace ns1 = {};
  Namespace ns2 = {};

  Global n0_g = {};
  const InternString* n0_name = strings.intern(lit_view_arr("hello"));
  Global n1_g = {};
  const InternString* n1_name = strings.intern(lit_view_arr("world"));
  Global n2_g = {};
  const InternString* n2_name = strings.intern(lit_view_arr("1234"));

  Errors errors = {};
  {
    GlobalName* gn = name_manager.add_global_name(&errors, &ns0, n0_name, &n0_g);
    if(errors.is_panic()) {
      test_errors->report_error("Failed to add global name\nReason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }
   
    TEST_NEQ(gn, static_cast<GlobalName*>(nullptr));
    TEST_EQ(gn->name, n0_name);
    TEST_EQ(gn->global, &n0_g);
  }
  {
    GlobalName* gn = name_manager.add_global_name(&errors, &ns1, n1_name, &n1_g);
    if(errors.is_panic()) {
      test_errors->report_error("Failed to add global name\nReason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }

    TEST_NEQ(gn, static_cast<GlobalName*>(nullptr));
    TEST_EQ(gn->name, n1_name);
    TEST_EQ(gn->global, &n1_g);
  }
  {
    GlobalName* gn = name_manager.add_global_name(&errors, &ns2, n2_name, &n2_g);
    if(errors.is_panic()) {
      test_errors->report_error("Failed to add global name\nReason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }

    TEST_NEQ(gn, static_cast<GlobalName*>(nullptr));
    TEST_EQ(gn->name, n2_name);
    TEST_EQ(gn->global, &n2_g);
  }
  
  {
    name_manager.add_global_import(&errors, &ns0, &ns1, Span{});
    if(errors.is_panic()) {
      test_errors->report_error("Import failed. Reason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }

    TEST_EQ(static_cast<usize>(1), ns0.imported.size);
    TEST_EQ(&ns1, ns0.imported[0]);
  }

  {
    name_manager.add_global_import(&errors, &ns1, &ns2, Span{});
    if(errors.is_panic()) {
      test_errors->report_error("Import failed. Reason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }

    TEST_EQ(static_cast<usize>(1), ns1.imported.size);
    TEST_EQ(&ns2, ns1.imported[0]);

    TEST_EQ(static_cast<usize>(1), ns0.imported.size);
    TEST_EQ(&ns1, ns0.imported[0]);
  }

  // n0
  {
    GlobalName* gn0 = name_manager.find_global_name(&ns0, n0_name);
    TEST_EQ(gn0->name, n0_name);
    TEST_EQ(gn0->global, &n0_g);
    GlobalName* gn1 = name_manager.find_global_name(&ns1, n0_name);
    TEST_EQ(gn1, static_cast<GlobalName*>(nullptr));
    GlobalName* gn2 = name_manager.find_global_name(&ns2, n0_name);
    TEST_EQ(gn2, static_cast<GlobalName*>(nullptr));
  }

  // n1
  {
    GlobalName* gn0 = name_manager.find_global_name(&ns0, n1_name);
    TEST_EQ(gn0->name, n1_name);
    TEST_EQ(gn0->global, &n1_g);
    GlobalName* gn1 = name_manager.find_global_name(&ns1, n1_name);
    TEST_EQ(gn1->name, n1_name);
    TEST_EQ(gn1->global, &n1_g);

    TEST_EQ(gn0, gn1);

    GlobalName* gn2 = name_manager.find_global_name(&ns2, n1_name);
    TEST_EQ(gn2, static_cast<GlobalName*>(nullptr));
  }

  // n2
  {
    GlobalName* gn0 = name_manager.find_global_name(&ns0, n2_name);
    TEST_EQ(gn0, static_cast<GlobalName*>(nullptr));

    GlobalName* gn1 = name_manager.find_global_name(&ns1, n2_name);
    TEST_EQ(gn1->name, n2_name);
    TEST_EQ(gn1->global, &n2_g);
    GlobalName* gn2 = name_manager.find_global_name(&ns2, n2_name);
    TEST_EQ(gn1->name, n2_name);
    TEST_EQ(gn2->global, &n2_g);

    TEST_EQ(gn1, gn2);
  }
}

TEST_FUNCTION(names, duplicate_names) {
  NameManager name_manager = {};
  StringInterner strings = {};

  Namespace ns0 = {};
  Namespace ns1 = {};

  Global n0_g = {};
  const InternString* n0_name = strings.intern(lit_view_arr("hello"));
  Global n1_g = {};

  Errors errors = {};

  {
    name_manager.add_global_import(&errors, &ns0, &ns1, Span{});
    if(errors.is_panic()) {
      test_errors->report_error("Import failed. Reason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }

    TEST_EQ(static_cast<usize>(1), ns0.imported.size);
    TEST_EQ(&ns1, ns0.imported[0]);
  }

  { 
    GlobalName* gn = name_manager.add_global_name(&errors, &ns0, n0_name, &n0_g);
    if(errors.is_panic()) {
      test_errors->report_error("Failed to add global name\nReason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }
   
    TEST_NEQ(gn, static_cast<GlobalName*>(nullptr));
    TEST_EQ(gn->name, n0_name);
    TEST_EQ(gn->global, &n0_g);
  }
  {
    GlobalName* gn = name_manager.add_global_name(&errors, &ns1, n0_name, &n1_g);
    if(errors.is_panic()) {
      test_errors->report_error("Failed to add global name\nReason: \"{}\"",
                                errors.error_messages[0].message);
      return;
    }

    TEST_NEQ(gn, static_cast<GlobalName*>(nullptr));
    TEST_EQ(gn->name, n0_name);
    TEST_EQ(gn->global, &n1_g);
  }
   
  {
    NameFindItr finder = name_manager.global_name_iterator(&ns0, n0_name);
    
    GlobalName* n = name_manager.next_name(finder);
    TEST_NEQ(n, static_cast<GlobalName*>(nullptr));
    TEST_EQ(n->name, n0_name);
    TEST_EQ(n->global, &n0_g);

    n = name_manager.next_name(finder);
    TEST_NEQ(n, static_cast<GlobalName*>(nullptr));
    TEST_EQ(n->name, n0_name);
    TEST_EQ(n->global, &n1_g);

    n = name_manager.next_name(finder);
    TEST_EQ(n, static_cast<GlobalName*>(nullptr));
    n = name_manager.next_name(finder);
    TEST_EQ(n, static_cast<GlobalName*>(nullptr));
    n = name_manager.next_name(finder);
    TEST_EQ(n, static_cast<GlobalName*>(nullptr));
    n = name_manager.next_name(finder);
    TEST_EQ(n, static_cast<GlobalName*>(nullptr));
  }
}
