#include "utility.h"

void load_to_bytes(Array<uint8_t>& bytes,
                          const size_t offset,
                          const uint8_t* in_bytes,
                          const size_t len) {
  bytes.reserve_total(offset + len);
  memcpy_ts(bytes.data + offset, bytes.capacity - offset, in_bytes, len);
}