#!/usr/bin/env bash
# Build the execution ABI user-mode helpers: the four legacy assembly sources
# in v1 and v2 modes, plus a v3 helper for every profile that has one - the
# generator (.ai/asm_plan.md M4 Phase 3) for the four legacy profiles, and
# the shared freestanding RISC-V source (built at both v2 and v3, an
# ABI_VERSION #define selecting the path within the one checked-in file -
# asm/helpers/riscv.c's own header comment).
#
# The legacy v1/v2 helpers are checked-in target assembly and the RISC-V
# helper is freestanding C with its target call boundary expressed as inline
# assembly. The legacy profiles' v3 helpers are GAS text produced at build
# time by test/oracle/abi_gen_main.exe (build_generated_one below) rather
# than checked in - v1/v2 stay frozen and untouched (.ai/asm_plan.md M4
# Phase 3.6) - while RISC-V's v3 helper is the same checked-in riscv.c
# recompiled with -DABI_VERSION=3 (build_riscv below), since C has no
# equivalent need for a generated/frozen split. This script is where all of
# these build boundaries meet: the same reference GNU as/ld links every
# kind, with no libc, start files, runtime, or dynamic loader.
# `-static -nostdlib` is not enough on its own: gcc would still add startup
# objects if it performed the link.
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

# Legacy profiles the generator currently implements (.ai/asm_plan.md M4
# Phase 3.4: x86_64 first, proven end to end, then generalized). A profile
# absent from this list gets no v3 helper yet and `make asm-exec`/
# `make asm-abi-conform`'s v3 leg simply has one fewer profile available,
# exactly like an unbuilt v1/v2 profile does today.
GENERATOR_PROFILES=(x86_64 x86_32 arm aarch64)

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

# The frozen command pair (.ai/asm_plan.md M4 Phase 3.2): the script has
# already `cd`d to the repo root above, and there is no root-level Dune
# project (only compcert-lib/, asm/ and asm/tools/ each have their own), so
# the build step must name asm/ explicitly rather than assume the generator
# is reachable from the script's own cwd.
build_generated_one() {
  local abi=$1 t=$2
  target_config "$t"
  local as="${TOOLPREFIX}as" ld="${TOOLPREFIX}ld"
  command -v "$as" >/dev/null || Fatal "$as not found (cross binutils missing)"
  command -v "$ld" >/dev/null || Fatal "$ld not found (cross binutils missing)"

  local dir="$OUT_DIR/v$abi/$t"
  mkdir -p "$dir"
  (cd asm && opam exec -- dune build test/oracle/abi_gen_main.exe)
  asm/_build/default/test/oracle/abi_gen_main.exe --profile "$t" --abi-version "$abi" \
    > "$dir/generated.s"
  "$as" "${AS_FLAGS[@]}" --defsym "ABI_VERSION=$abi" -I "$SRC_DIR" -o "$dir/helper.o" "$dir/generated.s"
  "$ld" "${LD_FLAGS[@]}" -static -e _start -o "$dir/helper" "$dir/helper.o"
  echo "$QEMU_BIN" > "$dir/qemu"

  # Provenance for a generated helper covers both halves of what produced it:
  # the generator's own sources (so a generator change is attributable) and
  # the generated output itself (so the exact assembled text is), the same
  # way build_one's provenance covers both the checked-in source and
  # abi-v1.inc.
  {
    echo "abi-version	$abi"
    echo "profile	$t"
    echo "generated.sha256	$(sha256sum "$dir/generated.s" | cut -d' ' -f1)"
    echo "abi_gen.ml.sha256	$(sha256sum asm/test/oracle/abi_gen.ml | cut -d' ' -f1)"
    echo "abi_gen_text.ml.sha256	$(sha256sum asm/test/oracle/abi_gen_text.ml | cut -d' ' -f1)"
    echo "abi_gen_main.ml.sha256	$(sha256sum asm/test/oracle/abi_gen_main.ml | cut -d' ' -f1)"
    echo "as	$("$as" --version | head -1)"
    echo "ld	$("$ld" --version | head -1)"
    echo "qemu	$("$QEMU_BIN" --version 2>/dev/null | head -1)"
  } > "$dir/provenance.txt"

  echo "asm-helpers: ABI v$abi (generated) $t -> $dir/helper ($QEMU_BIN)"
}

# RISC-V's helper is one C source guarded by an ABI_VERSION #define
# (asm/helpers/riscv.c's own header comment) rather than a GAS text/generator
# split like the legacy profiles: -DABI_VERSION=$abi is what selects v2's
# frozen path or v3's additive one, and both are built from the same
# checked-in file, so there is no "generated.s" provenance half here - only
# the one source's own hash.
build_riscv() {
  local abi=$1 t=$2
  target_config "$t"
  local src="$SRC_DIR/riscv.c"
  local cc="${TOOLPREFIX}gcc" ld="${TOOLPREFIX}ld"
  command -v "$cc" >/dev/null || Fatal "$cc not found (cross compiler missing)"
  command -v "$ld" >/dev/null || Fatal "$ld not found (cross binutils missing)"

  local dir="$OUT_DIR/v$abi/$t"
  mkdir -p "$dir"
  "$cc" "${AS_FLAGS[@]}" -DABI_VERSION=$abi -O2 -ffreestanding -fno-builtin -fno-stack-protector \
    -fno-pic -fomit-frame-pointer -nostdlib -c -o "$dir/helper.o" "$src"
  "$ld" "${LD_FLAGS[@]}" -z separate-code -static -e _start -o "$dir/helper" "$dir/helper.o"
  echo "$QEMU_BIN" > "$dir/qemu"
  {
    echo "abi-version	$abi"
    echo "profile	$t"
    echo "helper.sha256	$(sha256sum "$src" | cut -d' ' -f1)"
    echo "cc	$("$cc" --version | head -1)"
    echo "ld	$("$ld" --version | head -1)"
    echo "qemu	$("$QEMU_BIN" --version 2>/dev/null | head -1)"
  } > "$dir/provenance.txt"
  echo "asm-helpers: ABI v$abi $t -> $dir/helper ($QEMU_BIN)"
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
      build_riscv 2 "$t"
      build_riscv 3 "$t"
      continue
      ;;
  esac
  if [ ! -f "$SRC_DIR/$t.s" ]; then
    echo "asm-helpers: $t skipped (no $SRC_DIR/$t.s yet)"
    continue
  fi
  build_one 1 "$t"
  build_one 2 "$t"
  for p in "${GENERATOR_PROFILES[@]}"; do
    if [ "$p" = "$t" ]; then
      build_generated_one 3 "$t"
      break
    fi
  done
done
