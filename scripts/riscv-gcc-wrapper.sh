#!/usr/bin/env bash
# Compiler probe wrapper used only while configuring freestanding CompCert
# RISC-V fixture compilers.  Invoke through a symlink whose basename contains
# riscv32 or riscv64; this keeps the selected ABI in the configured compiler
# path instead of depending on a transient environment variable. Each profile
# execs its own dedicated cross-gcc (target.ml's toolprefix) with the exact
# march/mabi this project's assembler/linker emit, rather than relying on
# multilib support in the other profile's toolchain.
set -euo pipefail

case "$(basename "$0")" in
  *riscv32*) exec riscv32-linux-gnu-gcc -march=rv32imafd -mabi=ilp32d "$@" ;;
  *riscv64*) exec riscv64-linux-gnu-gcc -march=rv64imafd -mabi=lp64d "$@" ;;
  *) echo "riscv-gcc-wrapper: invoke through a riscv32/riscv64-named link" >&2; exit 2 ;;
esac
