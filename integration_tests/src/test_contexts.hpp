#include <AxleTest/ipc.h>

namespace TestContext {
  struct Setup {

  };
};

namespace AxleTest::IPC {
  template<>
  struct ContextName<TestContext::Setup> {
    constexpr static auto NAME = Axle::lit_view_arr("Setup");
  };
}
