#!/usr/bin/env bash
# Cross-reference this project's assembler against GNU as.
#
# asm/test/differential compares one CompCert fixture per target against a
# committed GNU oracle. That establishes agreement on 24 encoding forms and
# nothing else, because 24 forms is all the fixtures contain. This corpus is
# the same claim over inputs the fixtures do not reach, and it grows on its own:
# every entry test/snippets can assemble becomes a case here.
#
# The inputs are *generated from the AST corpus*, not hand-written. One AST
# yields our bytes, the hexdump baseline in test/snippets, and the .s file GNU
# as reads - all through the same canonical printer. A disagreement is then
# attributable to the two encoders, because the input is not in question. Hand
# writing the .s would have reintroduced exactly the second description this
# whole exercise removes.
#
# Two modes, and the split is asm/Makefile's §12 policy rather than a
# convenience (see the comment above asm-fixtures-check):
#
#   --regen   needs the four cross binutils. Rebuilds the corpus and its
#             manifest. Runs from `make asm-gas-xref-regen`, downstream of
#             asm-cross-setup.
#   --check   hashes the committed corpus and nothing else. No toolchain, no
#             emulator, so `make asm-test` acquires neither.
#
# The normalizations are asm-fixture-oracle.sh's, for its reasons: absolute
# paths and the object filename are stripped so the artifacts do not encode
# where this checkout lives, and .text is dumped in **memory order**, because
# objdump prints ARM and AArch64 instruction *words* and comparing word order
# against a byte-oriented encoder would appear to work and be wrong.
set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd -- "$SCRIPT_DIR/.." && pwd)
# shellcheck source=target-matrix.sh
. "$SCRIPT_DIR/target-matrix.sh"

CORPUS_ROOT="$REPO_ROOT/asm/fixtures/gas-xref"
MANIFEST="$CORPUS_ROOT/manifest.txt"
ASM_DIR="$REPO_ROOT/asm"

Fatal() { echo "FATAL: $*" 1>&2; exit 1; }
usage() { echo "Usage: $0 --check | --regen" 1>&2; }

hash_of() { sha256sum "$1" | cut -d' ' -f1; }

# 16 bytes per line, lower case, no offset column and no duplicate-line marker.
# Byte-for-byte the format asm-fixture-oracle.sh writes, so the OCaml side reads
# both corpora with one parser.
hexdump_memory_order() {
  od -An -v -tx1 "$1" | tr -s ' ' | sed -e 's/^ //' -e 's/ $//' | awk 'NF'
}

corpus_files() {
  (cd "$CORPUS_ROOT" && find . -type f ! -name 'manifest.txt*' | sed 's|^\./||' | LC_ALL=C sort)
}

# {1 --regen}

# {2 The frontier sources}
#
# Every assembly file in this tree that a real toolchain reads, as opposed to
# the ones this project generated for itself. Three groups, and they answer
# different questions:
#
#   fixture   the four committed CompCert outputs. Positive controls: this
#             assembler already handles them, and a frontier report where they
#             stopped working would be reporting a regression rather than a
#             boundary.
#   helper    our own four ABI helpers. Hand-written, ~35 KB each, and far
#             outside M1 - but they are *ours*, so how far our own assembler
#             gets on them is a fact worth knowing rather than guessing.
#   runtime   CompCert's runtime library, the largest body of real assembly
#             available here. C-preprocessed, because that is how the compiler
#             feeds them to gas and comparing against the unpreprocessed text
#             would be comparing against a file nothing assembles.
#
# CompCert compiles its runtime with -DMODEL_/-DABI_/-DENDIANNESS_/-DSYS_
# (modules/CompCert/runtime/Makefile:62), and the values are the ones its own
# configure picks for each of our four targets (configure:169-253). Without
# them the FUNCTION macro is left undefined and every file preprocesses to text
# no assembler can read - which would have been recorded as a frontier finding
# and would have been an artifact of how we invoked cpp.
runtime_defines() {
  case "$1" in
    arm) echo "-DMODEL_armv7a -DABI_hardfloat -DENDIANNESS_little -DSYS_linux" ;;
    x86_32) echo "-DMODEL_32sse2 -DABI_standard -DENDIANNESS_little -DSYS_linux" ;;
    x86_64) echo "-DMODEL_64 -DABI_standard -DENDIANNESS_little -DSYS_linux" ;;
    aarch64) echo "-DMODEL_default -DABI_standard -DENDIANNESS_little -DSYS_linux" ;;
  esac
}

# The A32 runtime carries no .arch of its own - CompCert passes the model on the
# command line - and this toolchain's default -march has no ARM mode, so without
# this gas reports every instruction as "an ARM instruction on a Thumb-only
# processor". The committed fixtures are unaffected: they carry .arch/.arm
# inline, which is why they assembled cleanly before this was noticed.
gas_flags() { case "$1" in arm) echo "-march=armv7-a" ;; *) echo "" ;; esac; }

# Emitted as tab-separated <target> <group> <id> <path>.
frontier_sources() {
  local t dir f
  for t in "${ALL_TARGETS[@]}"; do
    printf '%s\tfixture\tasm_test_entry\t%s\n' "$t" \
      "asm/fixtures/compcert-3.17/return42/$t/asm_test_entry.s"
    printf '%s\thelper\thelper\t%s\n' "$t" "asm/helpers/$t.s"
    dir="modules/CompCert/runtime/$t"
    [ -d "$REPO_ROOT/$dir" ] || continue
    for f in "$REPO_ROOT/$dir"/*.S; do
      [ -e "$f" ] || continue
      printf '%s\truntime\t%s\t%s\n' "$t" "$(basename "$f" .S)" "$dir/$(basename "$f")"
    done
  done
}

# Assemble one prepared input and record what GNU as said about it. Both
# outcomes are recorded, because "gas rejects this too" is as much a fact about
# a frontier case as the bytes are: a file neither assembler accepts says
# nothing about the difference between them.
record_gas() {
  local dir=$1 src=$2 work rc
  work=$(mktemp -d)
  if "${TOOLPREFIX}as" ${GAS_FLAGS:-} ${GAS_INCLUDE:+-I "$GAS_INCLUDE"} -o "$work/obj.o" "$src" \
      > "$work/stderr.txt" 2>&1; then
    echo "ok" > "$dir/gas.txt"
    "${TOOLPREFIX}objdump" -dr "$work/obj.o" | sed -e "s|$work/||g" -e '1,2d' > "$dir/objdump.txt"
    "${TOOLPREFIX}objcopy" --only-section=.text -O binary "$work/obj.o" "$work/text.bin"
    hexdump_memory_order "$work/text.bin" > "$dir/text.hex"
    rc=0
  else
    # The message only, with the input's path stripped: the path is the case's
    # own identity and repeating it inside the record would make every line
    # depend on where this checkout lives.
    {
      echo "error"
      sed -e "s|$src||g" -e "s|$work/||g" "$work/stderr.txt" | grep -E 'Error|Warning' | head -3
    } > "$dir/gas.txt"
    rc=1
  fi
  rm -rf "$work"
  return "$rc"
}

do_regen() {
  local t
  for t in "${ALL_TARGETS[@]}"; do
    target_config "$t"
    command -v "${TOOLPREFIX}as" >/dev/null 2>&1 \
      || Fatal "${TOOLPREFIX}as not installed - the cross-reference needs the cross binutils"
  done

  rm -rf "$CORPUS_ROOT"
  mkdir -p "$CORPUS_ROOT/generated" "$CORPUS_ROOT/frontier"

  # {2 Generated cases}
  #
  # The emitter is this project's own build, so a corpus can never be
  # regenerated from an assembler that does not compile.
  (cd "$ASM_DIR" && opam exec -- dune exec test/snippets/snippet_emit.exe -- \
    "$CORPUS_ROOT/generated") > /dev/null

  local n=0 src dir target
  while IFS= read -r src; do
    dir=$(dirname "$src")
    target=$(basename "$(dirname "$dir")")
    target_config "$target"
    GAS_FLAGS=$(gas_flags "$target")
    GAS_INCLUDE=""
    # A generated case that GNU as refuses is a bug in our canonical printer,
    # not a finding about the corpus: we produced that text.
    record_gas "$dir" "$src" \
      || Fatal "GNU as rejected generated case $dir - see $dir/gas.txt"
    n=$((n + 1))
  done < <(find "$CORPUS_ROOT/generated" -name input.s | LC_ALL=C sort)

  [ "$n" -gt 0 ] || Fatal "the emitter produced no cases"

  # {2 Frontier cases}
  local group id path fdir m=0
  while IFS=$'\t' read -r target group id path; do
    [ -f "$REPO_ROOT/$path" ] || continue
    target_config "$target"
    GAS_FLAGS=$(gas_flags "$target")
    fdir="$CORPUS_ROOT/frontier/$target/$group-$id"
    mkdir -p "$fdir"

    case "$path" in
      *.S)
        # -P suppresses the line markers, which carry absolute include paths
        # and would put this checkout's location into a committed artifact.
        "${TOOLPREFIX}gcc" -E -P $(runtime_defines "$target") -I "$REPO_ROOT/$(dirname "$path")" \
          "$REPO_ROOT/$path" > "$fdir/input.s" 2>/dev/null \
          || { rm -rf "$fdir"; continue; }
        ;;
      *) cp "$REPO_ROOT/$path" "$fdir/input.s" ;;
    esac
    printf '%s\n' "$path" > "$fdir/origin.txt"

    # The helpers .include a shared file, which gas resolves relative to -I.
    case "$group" in
      helper) GAS_INCLUDE="$REPO_ROOT/asm/helpers" ;;
      *) GAS_INCLUDE="" ;;
    esac
    record_gas "$fdir" "$fdir/input.s" || true
    m=$((m + 1))
  done < <(frontier_sources)

  {
    for t in "${ALL_TARGETS[@]}"; do
      target_config "$t"
      printf 'as:%s\t%s\n' "$t" "$("${TOOLPREFIX}as" --version | head -1)"
      printf 'objdump:%s\t%s\n' "$t" "$("${TOOLPREFIX}objdump" --version | head -1)"
    done
  } > "$CORPUS_ROOT/tool-versions.txt"

  write_manifest
  echo "gas-xref: $n generated cases, $m frontier cases"
}

write_manifest() {
  local f
  {
    echo "generator	tools/asm-gas-xref.sh"
    echo "inputs	generated by asm/test/snippets/snippet_emit.ml from the AST corpus"
    while IFS= read -r f; do
      printf 'sha256:%s\t%s\n' "$f" "$(hash_of "$CORPUS_ROOT/$f")"
    done < <(corpus_files)
  } | LC_ALL=C sort > "$MANIFEST.new"
  mv "$MANIFEST.new" "$MANIFEST"
}

# {1 --check}
#
# Identical in mechanism to asm-fixture-gen.sh --check, including the
# UNRECORDED rule: a file present in the tree but absent from the manifest is
# just as much a divergence as a changed one, because it would otherwise ride
# along unreviewed.

do_check() {
  [ -f "$MANIFEST" ] || Fatal "no manifest at $MANIFEST - run --regen first"
  local rc=0 seen=0 key value path f
  while IFS=$'\t' read -r key value; do
    case "$key" in
      sha256:*)
        path=${key#sha256:}
        seen=$((seen + 1))
        if [ ! -f "$CORPUS_ROOT/$path" ]; then
          echo "MISSING $path" >&2; rc=1
        elif [ "$(hash_of "$CORPUS_ROOT/$path")" != "$value" ]; then
          echo "CHANGED $path" >&2; rc=1
        fi
        ;;
    esac
  done < "$MANIFEST"

  # A file present in the tree but absent from the manifest is just as much a
  # divergence as a changed one: it would otherwise ride along unreviewed.
  local present=0
  while IFS= read -r f; do
    present=$((present + 1))
    grep -q "^sha256:$f	" "$MANIFEST" || { echo "UNRECORDED $f" >&2; rc=1; }
  done < <(corpus_files)

  # The scan above runs in a process substitution, where a failure would not
  # reach `set -e` - the loop would simply not execute and --check would pass
  # having compared nothing in that direction. Counting is what makes the
  # difference between "no unrecorded files" and "the scan did not run".
  [ "$present" -gt 0 ] || Fatal "found no files under $CORPUS_ROOT"
  [ "$seen" -gt 0 ] || Fatal "manifest records no files"
  [ "$rc" -eq 0 ] && echo "gas-xref: $seen files match the manifest"
  return "$rc"
}

case "${1:-}" in
  --check) do_check ;;
  --regen) do_regen ;;
  *) usage; exit 2 ;;
esac
