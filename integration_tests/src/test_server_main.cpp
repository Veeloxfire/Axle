#include <AxleTest/ipc.h>

int main() {
  bool r = AxleTest::IPC::server_main(Axle::lit_view_arr(UNIT_TEST_CLIENT_EXE));
  if(!r) return -1;
}
