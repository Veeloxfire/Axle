#include <AxleTest/ipc.h>

int main() {
  bool r = AxleTest::IPC::client_main({});
  if(!r) return -1;
}
