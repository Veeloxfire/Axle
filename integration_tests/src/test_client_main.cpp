#include <AxleTest/ipc.h>

int main() {
  bool r = AxleTest::IPC::client_main(Axle::lit_view_arr(INTEGRATION_TEST_CLIENT_DIR));
  if(!r) return -1;
}
