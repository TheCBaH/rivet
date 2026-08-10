#!/usr/bin/env bash
# The shared target matrix. Sourced by tools/compcert-cross-smoke.sh and by the
# asm/ fixture and oracle tools, so the four targets are described in exactly
# one place: a fixture generated against one toolchain prefix and an oracle
# produced against another would compare cleanly and mean nothing.
#
# ALL_TARGETS is the canonical order, and it is the order every generated
# artifact and manifest uses.
ALL_TARGETS=(x86_32 x86_64 arm aarch64)

# Sets CONFIGURE_TARGET, TOOLPREFIX, QEMU_BIN, QEMU_SYSROOT,
# CCOMP_EXTRA_ARGS, READELF_MACHINE for the given target. Even x86_32/x86_64
# use a dedicated cross-gcc package, not the host's native gcc -m32/-m64:
# gcc-multilib conflicts with the arm/aarch64 cross-gcc packages.
#
# LINK_*_ADDR are the controlled-link addresses M2's differential gate uses.
# They are the same numbers as asm/test/oracle/abi.ml's code_addr, rodata_addr
# and data_addr, because the GNU reference link, our own binder and the QEMU
# manifest must place a section at one address or the post-link byte comparison
# compares two different programs. abi.ml stays the definition; these mirror it
# for the shell, and asm/test/oracle/test_record.ml asserts they still agree.
target_config() {
  CONFIGURE_TARGET=""
  TOOLPREFIX=""
  QEMU_BIN=""
  QEMU_SYSROOT=""
  CCOMP_EXTRA_ARGS=()
  READELF_MACHINE=""
  LINK_TEXT_ADDR=""
  LINK_RODATA_ADDR=""
  LINK_DATA_ADDR=""
  case "$1" in
    x86_32)
      CONFIGURE_TARGET="x86_32-linux"
      TOOLPREFIX="i686-linux-gnu-"
      QEMU_BIN="qemu-i386"
      QEMU_SYSROOT="/usr/i686-linux-gnu"
      READELF_MACHINE="Intel 80386"
      ;;
    x86_64)
      CONFIGURE_TARGET="x86_64-linux"
      TOOLPREFIX="x86_64-linux-gnu-"
      QEMU_BIN="qemu-x86_64"
      QEMU_SYSROOT="/usr/x86_64-linux-gnu"
      READELF_MACHINE="Advanced Micro Devices X86-64"
      ;;
    arm)
      CONFIGURE_TARGET="arm-linux"
      TOOLPREFIX="arm-linux-gnueabihf-"
      QEMU_BIN="qemu-arm"
      QEMU_SYSROOT="/usr/arm-linux-gnueabihf"
      CCOMP_EXTRA_ARGS=(-marm)
      READELF_MACHINE="ARM"
      ;;
    aarch64)
      CONFIGURE_TARGET="aarch64-linux"
      TOOLPREFIX="aarch64-linux-gnu-"
      QEMU_BIN="qemu-aarch64"
      QEMU_SYSROOT="/usr/aarch64-linux-gnu"
      READELF_MACHINE="AArch64"
      ;;
    *)
      usage
      Fatal "unknown target '$1'"
      ;;
  esac

  # abi.ml's window_base: 0x30000000 for the 32-bit profiles, 0x40000000 for
  # the 64-bit ones - 0x40000000 does not fit a 31-bit native int, which is why
  # the split exists at all. rodata sits at +0x10000 and data at +0x20000.
  local base
  case "$1" in
    x86_32 | arm) base=$((0x30000000)) ;;
    *) base=$((0x40000000)) ;;
  esac
  LINK_TEXT_ADDR=$(printf '0x%x' "$base")
  LINK_RODATA_ADDR=$(printf '0x%x' $((base + 0x10000)))
  LINK_DATA_ADDR=$(printf '0x%x' $((base + 0x20000)))
}
