#include <AxleTest/ipc.h>

#include <AxleUtil/os/os_windows.h>
#include <debugapi.h>

int main() {
  Axle::u32 timeout_ms = IsDebuggerPresent() ? INFINITE : 1000;
  Axle::ViewArr<AxleTest::IPC::OpaqueContext> contexts = {};
  
  bool r = AxleTest::IPC::server_main(Axle::lit_view_arr(INTEGRATION_TEST_CLIENT_EXE), contexts, timeout_ms);
  if(!r) return -1;
}
