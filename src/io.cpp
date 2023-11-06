#include "io.h"
#include "threading.h"
#include <cstdio>

static SpinLockMutex io_mutex = {};

void IO_Single::lock() {
  io_mutex.acquire();
}

void IO_Single::unlock() {
  io_mutex.release();
}

void IO_Single::print_impl(const char* string, usize n) {
  TRACING_FUNCTION();
  fwrite(string, 1, n, stdout);
}

void IO_Single::print_impl(const ViewArr<char>& string) {
  TRACING_FUNCTION();
  fwrite(string.data, 1, string.size, stdout);
}

void IO_Single::print_impl(const ViewArr<const char>& string) {
  TRACING_FUNCTION();
  fwrite(string.data, 1, string.size, stdout);
}

void IO_Single::print_impl(const char c) {
  TRACING_FUNCTION();
  fputc(c, stdout);
}

void IO_Single::err_print_impl(const char* string, usize n) {
  TRACING_FUNCTION();
  fwrite(string, 1, n, stderr);
}

void IO_Single::err_print_impl(const ViewArr<char>& string) {
  TRACING_FUNCTION();
  fwrite(string.data, 1, string.size, stderr);
}

void IO_Single::err_print_impl(const ViewArr<const char>& string) {
  TRACING_FUNCTION();
  fwrite(string.data, 1, string.size, stderr);
}

void IO_Single::err_print_impl(const char c) {
  TRACING_FUNCTION();
  fputc(c, stderr);
}