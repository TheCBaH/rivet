#!/usr/bin/env bash
# GNU as reads Rivet's GNU print of the RV32 isa-difficult corpus (test/gnu_text) as the bytes it
# made from each case's original text. Needs a RISC-V binutils; not part of `make test`.
#   scripts/gnu-text-oracle.sh [AS] [OBJCOPY]
set -euo pipefail
cd "$(dirname "$0")/.."
AS=${1:-riscv64-linux-gnu-as}
OBJCOPY=${2:-riscv64-linux-gnu-objcopy}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
opam exec -- dune build test/gnu_text/gnu_text_emit.exe
./_build/default/test/gnu_text/gnu_text_emit.exe fixtures/isa-difficult/cases.jsonl "$work"
total=0 bad=0 skipped=0
for s in "$work"/*.s; do
  b=${s%.s}
  total=$((total + 1))
  mapfile -t argv < "$b.argv"
  # A case this assembler refuses as written (a newer binutils than the corpus's) says nothing
  # about the print.
  if ! "$AS" "${argv[@]}" -o "$b.orig.o" "$b.orig" 2> /dev/null; then
    skipped=$((skipped + 1)); continue
  fi
  if ! "$AS" "${argv[@]}" -o "$b.o" "$s" 2> "$b.err"; then
    bad=$((bad + 1)); echo "$(cat "$b.id"): GNU as refused: $(sed -n 2p "$b.err")"; continue
  fi
  "$OBJCOPY" -O binary -j .text "$b.o" "$b.bin"
  got=$(od -An -v -tx1 "$b.bin" | tr -s ' \n' ' ' | sed 's/^ //; s/ $//')
  want=$(cat "$b.bytes")
  if [ "$got" != "$want" ]; then
    bad=$((bad + 1)); echo "$(cat "$b.id"): GNU as made '$got', expected '$want'"
  fi
done
echo "gnu-text-oracle: $total cases, $skipped refused as written by this assembler, $bad differ"
[ "$bad" -eq 0 ]
