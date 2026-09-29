#!/usr/bin/env bash
# GENERATED FILE - do not edit. Regenerate with `make tools-matrix`.
#
# The source of truth is tools/lib/target.ml. This file exists so that shell
# consumers - scripts/asm-helpers.sh and the Makefile - read the same definition
# the OCaml tooling does, without any of them acquiring a run-time dependency on
# a built executable. `make tools-matrix-diff` regenerates it and fails if the working
# tree changed, so an edit to target.ml that is not reflected here is caught in
# CI rather than at whichever consumer next disagreed.
#
# All targets are described in exactly one place because a fixture generated
# against one toolchain prefix and an oracle produced against another would
# compare cleanly and mean nothing.
#
# Capability sets are explicit: fixture work is freestanding and must not
# accidentally acquire a libc requirement.
ASSEMBLER_TARGETS=(x86_32 x86_64 arm aarch64 riscv32 riscv64)
FIXTURE_TARGETS=(x86_32 x86_64 arm aarch64 riscv32 riscv64)
ALL_TARGETS=("${ASSEMBLER_TARGETS[@]}")

# Sets TOOLPREFIX, QEMU_BIN, QEMU_SYSROOT, GCC_FIXTURE_ARGS, READELF_MACHINE for
# the given target. Even x86_32/x86_64
# use a dedicated cross-gcc package, not the host's native gcc -m32/-m64:
# gcc-multilib conflicts with the arm/aarch64 cross-gcc packages.
#
# LINK_*_ADDR are the controlled-link addresses M2's differential gate uses.
# They are the same numbers as test/oracle/abi.ml's code_addr, rodata_addr
# and data_addr (and, for LINK_BSS_ADDR, abi_v2.ml's bss_addr - M3), because
# the GNU reference link, our own binder and the QEMU manifest must place a
# section at one address or the post-link byte comparison compares two
# different programs. abi.ml/abi_v2.ml stay the definition; Target.link
# mirrors them, and test/oracle/test_record.ml asserts they still agree.
#
# The addresses are emitted per target rather than computed here, so the
# derivation lives in one language instead of two. It is abi.ml's window_base:
# 0x30000000 for the 32-bit profiles and 0x40000000 for the 64-bit ones - and
# the split exists at all because 0x40000000 does not fit a 31-bit native int -
# with rodata at +0x10000, data at +0x20000, and bss at +0x80000 (the first
# byte after the largest stack abi_v2.ml's stack_size_max permits).
target_config() {
  TOOLPREFIX=""
  QEMU_BIN=""
  QEMU_SYSROOT=""
  GCC_FIXTURE_ARGS=()
  AS_FLAGS=()
  LD_FLAGS=()
  LINKER_EMULATION=""
  ELF_CLASS=""
  WORD_SIZE=""
  HAS_SYSROOT=false
  READELF_MACHINE=""
  LINK_TEXT_ADDR=""
  LINK_RODATA_ADDR=""
  LINK_DATA_ADDR=""
  LINK_BSS_ADDR=""
  case "$1" in
    x86_32)
      TOOLPREFIX="i686-linux-gnu-"
      QEMU_BIN="qemu-i386"
      QEMU_SYSROOT="/usr/i686-linux-gnu"
      GCC_FIXTURE_ARGS=(-O1 -fno-inline -fno-section-anchors -ffreestanding -fno-pic -fno-pie -fno-asynchronous-unwind-tables)
      READELF_MACHINE="Intel 80386"
      ELF_CLASS="ELF32"; WORD_SIZE=4; HAS_SYSROOT=true
      LINK_TEXT_ADDR="0x30000000"
      LINK_RODATA_ADDR="0x30010000"
      LINK_DATA_ADDR="0x30020000"
      LINK_BSS_ADDR="0x30080000"
      ;;
    x86_64)
      TOOLPREFIX="x86_64-linux-gnu-"
      QEMU_BIN="qemu-x86_64"
      QEMU_SYSROOT="/usr/x86_64-linux-gnu"
      GCC_FIXTURE_ARGS=(-O1 -fno-inline -fno-section-anchors -ffreestanding -fno-pic -fno-pie -fno-asynchronous-unwind-tables)
      READELF_MACHINE="Advanced Micro Devices X86-64"
      ELF_CLASS="ELF64"; WORD_SIZE=8; HAS_SYSROOT=true
      LINK_TEXT_ADDR="0x40000000"
      LINK_RODATA_ADDR="0x40010000"
      LINK_DATA_ADDR="0x40020000"
      LINK_BSS_ADDR="0x40080000"
      ;;
    arm)
      TOOLPREFIX="arm-linux-gnueabihf-"
      QEMU_BIN="qemu-arm"
      QEMU_SYSROOT="/usr/arm-linux-gnueabihf"
      GCC_FIXTURE_ARGS=(-marm -O1 -fno-inline -fno-section-anchors -ffreestanding -fno-pic -fno-pie -fno-asynchronous-unwind-tables)
      AS_FLAGS=(-march=armv7-a)
      READELF_MACHINE="ARM"
      ELF_CLASS="ELF32"; WORD_SIZE=4; HAS_SYSROOT=true
      LINK_TEXT_ADDR="0x30000000"
      LINK_RODATA_ADDR="0x30010000"
      LINK_DATA_ADDR="0x30020000"
      LINK_BSS_ADDR="0x30080000"
      ;;
    aarch64)
      TOOLPREFIX="aarch64-linux-gnu-"
      QEMU_BIN="qemu-aarch64"
      QEMU_SYSROOT="/usr/aarch64-linux-gnu"
      GCC_FIXTURE_ARGS=(-O1 -fno-inline -fno-section-anchors -ffreestanding -fno-pic -fno-pie -fno-asynchronous-unwind-tables)
      READELF_MACHINE="AArch64"
      ELF_CLASS="ELF64"; WORD_SIZE=8; HAS_SYSROOT=true
      LINK_TEXT_ADDR="0x40000000"
      LINK_RODATA_ADDR="0x40010000"
      LINK_DATA_ADDR="0x40020000"
      LINK_BSS_ADDR="0x40080000"
      ;;
    riscv32)
      TOOLPREFIX="riscv32-linux-gnu-"
      QEMU_BIN="qemu-riscv32"
      QEMU_SYSROOT="/usr/riscv32-linux-gnu"
      GCC_FIXTURE_ARGS=(-O1 -fno-inline -fno-section-anchors -ffreestanding -fno-pic -fno-pie -fno-asynchronous-unwind-tables -march=rv32imafd -mabi=ilp32d -mno-relax -msmall-data-limit=0)
      AS_FLAGS=(-march=rv32imafd -mabi=ilp32d -mno-relax)
      LINKER_EMULATION="elf32lriscv"
      LD_FLAGS=(-m elf32lriscv --no-relax)
      READELF_MACHINE="RISC-V"
      ELF_CLASS="ELF32"; WORD_SIZE=4; HAS_SYSROOT=true
      LINK_TEXT_ADDR="0x30000000"
      LINK_RODATA_ADDR="0x30010000"
      LINK_DATA_ADDR="0x30020000"
      LINK_BSS_ADDR="0x30080000"
      ;;
    riscv64)
      TOOLPREFIX="riscv64-linux-gnu-"
      QEMU_BIN="qemu-riscv64"
      QEMU_SYSROOT="/usr/riscv64-linux-gnu"
      GCC_FIXTURE_ARGS=(-O1 -fno-inline -fno-section-anchors -ffreestanding -fno-pic -fno-pie -fno-asynchronous-unwind-tables -march=rv64imafd -mabi=lp64d -mno-relax -msmall-data-limit=0)
      AS_FLAGS=(-march=rv64imafd -mabi=lp64d -mno-relax)
      LINKER_EMULATION="elf64lriscv"
      LD_FLAGS=(-m elf64lriscv --no-relax)
      READELF_MACHINE="RISC-V"
      ELF_CLASS="ELF64"; WORD_SIZE=8; HAS_SYSROOT=true
      LINK_TEXT_ADDR="0x40000000"
      LINK_RODATA_ADDR="0x40010000"
      LINK_DATA_ADDR="0x40020000"
      LINK_BSS_ADDR="0x40080000"
      ;;
    *)
      usage
      Fatal "unknown target '$1'"
      ;;
  esac
}

# When executed rather than sourced, print a named target set one per line, so
# the Makefile reads the same definition the scripts do instead of keeping its
# own copy of the list.
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  case "${1:-fixture}" in
    fixture)   printf '%s\n' "${FIXTURE_TARGETS[@]}" ;;
    assembler) printf '%s\n' "${ASSEMBLER_TARGETS[@]}" ;;
    *) echo "usage: $0 [fixture|assembler]" >&2; exit 2 ;;
  esac
fi
