#include "type.h"

uint32_t Structure::size() const {
  switch (type) {
    case STRUCTURE_TYPE::COMPOSITE: return composite.size;
    case STRUCTURE_TYPE::INTEGER: return integer.size;
  }
}

uint32_t Structure::alignment() const {
  switch (type) {
    case STRUCTURE_TYPE::COMPOSITE: return composite.alignment;
    case STRUCTURE_TYPE::INTEGER: return integer.size;
  }
}

Structure::~Structure() {
  switch (type) {
    case STRUCTURE_TYPE::COMPOSITE: {
        composite.~CompositeStructure();
        break;
      }
    case STRUCTURE_TYPE::INTEGER: {
        integer.~IntegerStructure();
        break;
      }
  }
}

InternString u8_name() {
  static const char u8[] = "u8";
  return { u8 };
}

InternString u64_name() {
  static const char u64[] = "u64";
  return { u64 };
}