#include "PE_file_format.h"

#define COFF_MASK(enumeration) if(enabled) { mask |= constant; } else { mask &= ~enumeration }

static_assert(sizeof(COFF_characteristics) == 2, "Must be 2 bytes");

void COFF_Characteristics::relocs_stripped(const bool enabled) {
  COFF_MASK(RELOCS_STRIPPED)
}

void COFF_Characteristics::executable_image(const bool enabled) {
  COFF_MASK(EXECUTABLE_IMAGE)
}

void COFF_Characteristics::line_nums_stripped(const bool enabled) {
  COFF_MASK(LINE_NUMS_STRIPPED)
}

void COFF_Characteristics::local_syms_stripped(const bool enabled) {
  COFF_MASK(LOCAL_SYMS_STRIPPED)
}

void COFF_Characteristics::aggressive_ws_trim(const bool enabled) {
  COFF_MASK(AGGRESSIVE_WS_TRIM)
}

void COFF_Characteristics::large_address_aware(const bool enabled) {
  COFF_MASK(LARGE_ADDRESS_AWARE)
}

void COFF_Characteristics::byte_reversed_lo(const bool enabled) {
  COFF_MASK(BYTES_REVERSED_LO)
}

void COFF_Characteristics::is_32_bit_machine(const bool enabled) {
  COFF_MASK(IS_32_BIT_MACHINE)
}

void COFF_Characteristics::debug_stripped(const bool enabled) {
  COFF_MASK(DEBUG_STRIPPED)
}

void COFF_Characteristics::removable_run_from_swap(const bool enabled) {
  COFF_MASK(REMOVABLE_RUN_FROM_SWAP)
}

void COFF_Characteristics::net_run_from_swap(const bool enabled) {
  COFF_MASK(NET_RUN_FROM_SWAP)
}

void COFF_Characteristics::system(const bool enabled) {
  COFF_MASK(SYSTEM)
}

void COFF_Characteristics::dll(const bool enabled) {
  COFF_MASK(DLL)
}

void COFF_Characteristics::up_system_only(const bool enabled) {
  COFF_MASK(UP_SYSTEM_ONLY)
}

void COFF_Characteristics::bytes_reversed_hi(const bool enabled) {
  COFF_MASK(BYTES_REVERSED_HI)
}

#undef COFF_MASK
