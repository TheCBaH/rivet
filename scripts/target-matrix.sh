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
target_config() {
  CONFIGURE_TARGET=""
  TOOLPREFIX=""
  QEMU_BIN=""
  QEMU_SYSROOT=""
  CCOMP_EXTRA_ARGS=()
  READELF_MACHINE=""
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
}
