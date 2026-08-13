#!/usr/bin/env bash
# Compiler probe wrapper used only while configuring freestanding CompCert
# RISC-V fixture compilers.  Invoke through a symlink whose basename contains
# riscv32 or riscv64; this keeps the selected ABI in the configured compiler
# path instead of depending on a transient environment variable.
set -euo pipefail

case "$(basename "$0")" in
  *riscv32*) profile_flags=(-march=rv32imafd -mabi=ilp32d) ;;
  *riscv64*) profile_flags=(-march=rv64imafd -mabi=lp64d) ;;
  *) echo "riscv-gcc-wrapper: invoke through a riscv32/riscv64-named link" >&2; exit 2 ;;
esac

exec riscv64-linux-gnu-gcc "${profile_flags[@]}" "$@"
