#!/usr/bin/env bash
# Assemble and link the ABI v1 user-mode helpers (asm/docs/exec-abi-v1.md).
#
# Each helper is checked-in target assembly, built by the reference GNU as/ld
# with no C, no libc and no dynamic loader - so `ld` here is the whole link, not
# a compiler driver that would quietly pull in crt files. `-static -nostdlib`
# is not enough on its own: gcc would still add its own startup objects, which
# is why this calls ld directly.
#
# The four toolchain prefixes come from tools/target-matrix.sh, the same place
# the fixture and oracle tools read them from. A helper assembled with one
# prefix and executed under another emulator would fail in ways that look like
# ABI defects, so there is exactly one definition of the matrix.
#
# Output goes to .asm-helpers/<profile>/helper (gitignored build state, like
# .cross-smoke-work): the helpers are sources, not artifacts, and the ELF is
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
  local t=$1
  target_config "$t"
  local src="$SRC_DIR/$t.s"
  [ -f "$src" ] || Fatal "no helper source $src"

  local as="${TOOLPREFIX}as" ld="${TOOLPREFIX}ld"
  command -v "$as" >/dev/null || Fatal "$as not found (cross binutils missing)"
  command -v "$ld" >/dev/null || Fatal "$ld not found (cross binutils missing)"

  local dir="$OUT_DIR/$t"
  mkdir -p "$dir"
  # -I so the shared .include resolves from the source directory rather than
  # from wherever this script happens to be run.
  "$as" -I "$SRC_DIR" -o "$dir/helper.o" "$src"
  # -static and no start files. The entry symbol is _start, which is ld's
  # default, but naming it means a renamed entry fails loudly instead of
  # silently linking to address 0.
  "$ld" -static -e _start -o "$dir/helper" "$dir/helper.o"
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
    echo "profile	$t"
    echo "helper.sha256	$(sha256sum "$src" | cut -d' ' -f1)"
    echo "abi-v1.inc.sha256	$(sha256sum "$SRC_DIR/abi-v1.inc" | cut -d' ' -f1)"
    echo "as	$("$as" --version | head -1)"
    echo "ld	$("$ld" --version | head -1)"
    echo "qemu	$("$QEMU_BIN" --version 2>/dev/null | head -1)"
  } > "$dir/provenance.txt"

  echo "asm-helpers: $t -> $dir/helper ($QEMU_BIN)"
}

targets=()
if [ $# -eq 0 ] || [ "${1:-}" = all ]; then
  targets=("${ALL_TARGETS[@]}")
else
  targets=("$@")
fi

for t in "${targets[@]}"; do
  # A profile whose helper has not been written yet is skipped rather than
  # failing the build: the four are landing one at a time, and the conformance
  # driver reports which profiles it actually exercised.
  if [ ! -f "$SRC_DIR/$t.s" ]; then
    echo "asm-helpers: $t skipped (no $SRC_DIR/$t.s yet)"
    continue
  fi
  build_one "$t"
done
