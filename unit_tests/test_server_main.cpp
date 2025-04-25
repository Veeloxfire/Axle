#include <AxleTest/ipc.h>

int main() {
  Axle::u32 timeout_ms = static_cast<Axle::u32>(-1);
  Axle::ViewArr<AxleTest::IPC::OpaqueContext> contexts = {};

  bool r = AxleTest::IPC::server_main(Axle::lit_view_arr(UNIT_TEST_CLIENT_EXE), contexts, timeout_ms);
  if(!r) return -1;
}
