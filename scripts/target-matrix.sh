#!/usr/bin/env bash
# The shared target matrix. Sourced by tools/compcert-cross-smoke.sh and by the
# asm/ fixture and oracle tools, so all targets are described in exactly
# one place: a fixture generated against one toolchain prefix and an oracle
# produced against another would compare cleanly and mean nothing.
#
# Capability sets are explicit: fixture work is freestanding and must not
# accidentally acquire the libc requirement of the cross-smoke suite.
ASSEMBLER_TARGETS=(x86_32 x86_64 arm aarch64 riscv32 riscv64)
FIXTURE_TARGETS=(x86_32 x86_64 arm aarch64 riscv32 riscv64)
LIBC_SMOKE_TARGETS=(x86_32 x86_64 arm aarch64)
ALL_TARGETS=("${ASSEMBLER_TARGETS[@]}")

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
  COMPCERT_CONFIGURE_ARGS=()
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
  case "$1" in
    x86_32)
      CONFIGURE_TARGET="x86_32-linux"
      TOOLPREFIX="i686-linux-gnu-"
      QEMU_BIN="qemu-i386"
      QEMU_SYSROOT="/usr/i686-linux-gnu"
      READELF_MACHINE="Intel 80386"
      ELF_CLASS="ELF32"; WORD_SIZE=4; HAS_SYSROOT=true
      ;;
    x86_64)
      CONFIGURE_TARGET="x86_64-linux"
      TOOLPREFIX="x86_64-linux-gnu-"
      QEMU_BIN="qemu-x86_64"
      QEMU_SYSROOT="/usr/x86_64-linux-gnu"
      READELF_MACHINE="Advanced Micro Devices X86-64"
      ELF_CLASS="ELF64"; WORD_SIZE=8; HAS_SYSROOT=true
      ;;
    arm)
      CONFIGURE_TARGET="arm-linux"
      TOOLPREFIX="arm-linux-gnueabihf-"
      QEMU_BIN="qemu-arm"
      QEMU_SYSROOT="/usr/arm-linux-gnueabihf"
      CCOMP_EXTRA_ARGS=(-marm)
      READELF_MACHINE="ARM"
      AS_FLAGS=(-march=armv7-a)
      ELF_CLASS="ELF32"; WORD_SIZE=4; HAS_SYSROOT=true
      ;;
    aarch64)
      CONFIGURE_TARGET="aarch64-linux"
      TOOLPREFIX="aarch64-linux-gnu-"
      QEMU_BIN="qemu-aarch64"
      QEMU_SYSROOT="/usr/aarch64-linux-gnu"
      READELF_MACHINE="AArch64"
      ELF_CLASS="ELF64"; WORD_SIZE=8; HAS_SYSROOT=true
      ;;
    riscv32)
      CONFIGURE_TARGET="rv32-linux"
      TOOLPREFIX="riscv64-linux-gnu-"
      QEMU_BIN="qemu-riscv32"
      COMPCERT_CONFIGURE_ARGS=(-no-runtime-lib -no-standard-headers)
      AS_FLAGS=(-march=rv32imafd -mabi=ilp32d -mno-relax)
      LINKER_EMULATION="elf32lriscv"
      LD_FLAGS=(-m elf32lriscv --no-relax)
      READELF_MACHINE="RISC-V"
      ELF_CLASS="ELF32"; WORD_SIZE=4
      ;;
    riscv64)
      CONFIGURE_TARGET="rv64-linux"
      TOOLPREFIX="riscv64-linux-gnu-"
      QEMU_BIN="qemu-riscv64"
      COMPCERT_CONFIGURE_ARGS=(-no-runtime-lib -no-standard-headers)
      AS_FLAGS=(-march=rv64imafd -mabi=lp64d -mno-relax)
      LINKER_EMULATION="elf64lriscv"
      LD_FLAGS=(-m elf64lriscv --no-relax)
      READELF_MACHINE="RISC-V"
      ELF_CLASS="ELF64"; WORD_SIZE=8
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
    x86_32 | arm | riscv32) base=$((0x30000000)) ;;
    *) base=$((0x40000000)) ;;
  esac
  LINK_TEXT_ADDR=$(printf '0x%x' "$base")
  LINK_RODATA_ADDR=$(printf '0x%x' $((base + 0x10000)))
  LINK_DATA_ADDR=$(printf '0x%x' $((base + 0x20000)))
}
