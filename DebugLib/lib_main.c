unsigned long long int GetStdHandle(unsigned int);

typedef unsigned int u32;
typedef unsigned long long int u64;

u32 WriteConsoleA(u64, void*, u32, u32*, void*);

unsigned long long int debug_test(char* data, unsigned int chars, unsigned long long int handle) {
  unsigned long long int d = GetStdHandle(-11);
  WriteConsoleA(d, data, chars, 0, 0);

  return d;
}