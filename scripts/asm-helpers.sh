#!/usr/bin/env bash
# Build the execution ABI user-mode helpers: the four legacy assembly sources
# in v1 and v2 modes, and the shared freestanding RISC-V source in v2 mode.
#
# The legacy helpers are checked-in target assembly and the RISC-V helper is
# freestanding C with its target call boundary expressed as inline assembly.
# All are linked directly by the reference GNU ld with no libc, start files,
# runtime, or dynamic loader. `-static -nostdlib` is not enough on its own: gcc
# would still add startup objects if it performed the link.
#
# Toolchain prefixes come from tools/target-matrix.sh, the same place
# the fixture and oracle tools read them from. A helper assembled with one
# prefix and executed under another emulator would fail in ways that look like
# ABI defects, so there is exactly one definition of the matrix.
#
# Output goes to .asm-helpers/v<abi>/<profile>/helper (gitignored build state,
# like .cross-smoke-work): the helpers are sources, not artifacts, and the ELF is
# reproducible from them at any time.
#
# Usage: tools/asm-helpers.sh [all | <profile>...]
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/target-matrix.sh

OUT_DIR=${ASM_HELPERS_DIR:-.asm-helpers}
SRC_DIR=asm/helpers

Fatal() { echo "asm-helpers: $*" >&2; exit 1; }
usage() { echo "usage: tools/asm-helpers.sh [all | ${ALL_TARGETS[*]}]" >&2; }

build_one() {
  local abi=$1 t=$2
  target_config "$t"
  local src="$SRC_DIR/$t.s"
  [ -f "$src" ] || Fatal "no helper source $src"

  local as="${TOOLPREFIX}as" ld="${TOOLPREFIX}ld"
  command -v "$as" >/dev/null || Fatal "$as not found (cross binutils missing)"
  command -v "$ld" >/dev/null || Fatal "$ld not found (cross binutils missing)"

  local dir="$OUT_DIR/v$abi/$t"
  mkdir -p "$dir"
  # -I so the shared .include resolves from the source directory rather than
  # from wherever this script happens to be run.
  "$as" "${AS_FLAGS[@]}" --defsym "ABI_VERSION=$abi" -I "$SRC_DIR" -o "$dir/helper.o" "$src"
  # -static and no start files. The entry symbol is _start, which is ld's
  # default, but naming it means a renamed entry fails loudly instead of
  # silently linking to address 0.
  "$ld" "${LD_FLAGS[@]}" -static -e _start -o "$dir/helper" "$dir/helper.o"
  # The emulator name comes from the matrix, and the OCaml runner reads it back
  # from here rather than deriving it again. Two derivations would be two places
  # to get "qemu-i386 for x86_32" wrong, and a helper run under the wrong
  # emulator fails in ways that look like ABI defects.
  echo "$QEMU_BIN" > "$dir/qemu"

  # M0.3 requires helper hashes and QEMU versions in provenance. The hash is of
  # the *sources* - the helper and the shared include - not of the ELF: the ELF
  # is regenerated build state, while the two sources are what is reviewed and
  # committed, and what a conformance result is evidence about. The tool
  # versions go beside them because the APT packages are range-checked rather
  # than digest-pinned, so a base-image update can move them under us.
  {
    echo "abi-version	$abi"
    echo "profile	$t"
    echo "helper.sha256	$(sha256sum "$src" | cut -d' ' -f1)"
    echo "abi-v1.inc.sha256	$(sha256sum "$SRC_DIR/abi-v1.inc" | cut -d' ' -f1)"
    echo "as	$("$as" --version | head -1)"
    echo "ld	$("$ld" --version | head -1)"
    echo "qemu	$("$QEMU_BIN" --version 2>/dev/null | head -1)"
  } > "$dir/provenance.txt"

  echo "asm-helpers: ABI v$abi $t -> $dir/helper ($QEMU_BIN)"
}

build_riscv_v2() {
  local t=$1
  target_config "$t"
  local src="$SRC_DIR/riscv.c"
  local cc="${TOOLPREFIX}gcc" ld="${TOOLPREFIX}ld"
  command -v "$cc" >/dev/null || Fatal "$cc not found (cross compiler missing)"
  command -v "$ld" >/dev/null || Fatal "$ld not found (cross binutils missing)"

  local dir="$OUT_DIR/v2/$t"
  mkdir -p "$dir"
  "$cc" "${AS_FLAGS[@]}" -O2 -ffreestanding -fno-builtin -fno-stack-protector \
    -fno-pic -fomit-frame-pointer -nostdlib -c -o "$dir/helper.o" "$src"
  "$ld" "${LD_FLAGS[@]}" -z separate-code -static -e _start -o "$dir/helper" "$dir/helper.o"
  echo "$QEMU_BIN" > "$dir/qemu"
  {
    echo "abi-version	2"
    echo "profile	$t"
    echo "helper.sha256	$(sha256sum "$src" | cut -d' ' -f1)"
    echo "cc	$("$cc" --version | head -1)"
    echo "ld	$("$ld" --version | head -1)"
    echo "qemu	$("$QEMU_BIN" --version 2>/dev/null | head -1)"
  } > "$dir/provenance.txt"
  echo "asm-helpers: ABI v2 $t -> $dir/helper ($QEMU_BIN)"
}

targets=()
if [ $# -eq 0 ] || [ "${1:-}" = all ]; then
  targets=("${ALL_TARGETS[@]}")
else
  targets=("$@")
fi

for t in "${targets[@]}"; do
  case "$t" in
    riscv32 | riscv64)
      build_riscv_v2 "$t"
      continue
      ;;
  esac
  if [ ! -f "$SRC_DIR/$t.s" ]; then
    echo "asm-helpers: $t skipped (no $SRC_DIR/$t.s yet)"
    continue
  fi
  build_one 1 "$t"
  build_one 2 "$t"
done
