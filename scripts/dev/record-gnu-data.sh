#!/usr/bin/env bash
# Record the raw GNU binutils output that tools/test/unit/test_gnu_output.ml
# parses: objdump -h and -r and readelf -SW and -sW of the object assembled from
# a generated fixture. The test compares the parsers' result with the oracle
# artifacts committed beside the fixture, so the two must be recorded from the
# same fixture and the same binutils, in one commit:
#
#   scripts/dev/record-gnu-data.sh
#
# Needs every listed target's cross binutils, and generated fixtures
# (make fixtures-regen).
set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)
CORPUS="$REPO_ROOT/fixtures/gcc-14"
OUT="$REPO_ROOT/tools/test/data/gnu"

usage() { echo "Usage: $0" >&2; }
Fatal() { echo "FATAL: $*" >&2; exit 1; }

# shellcheck source=../target-matrix.sh
. "$REPO_ROOT/scripts/target-matrix.sh"

# The pairs test_gnu_output.ml checks: two cases, on every target but riscv32.
PAIRS=(
  x86_32:global_ldst x86_32:direct_call
  x86_64:global_ldst x86_64:direct_call
  arm:global_ldst arm:direct_call
  aarch64:global_ldst aarch64:direct_call
  riscv64:global_ldst riscv64:direct_call
)

mkdir -p "$OUT"
scratch=$(mktemp -d)
trap 'rm -rf -- "$scratch"' EXIT

for pair in "${PAIRS[@]}"; do
  t=${pair%%:*}
  c=${pair#*:}
  target_config "$t"
  src="$CORPUS/$c/$t/$c.s"
  [ -f "$src" ] || Fatal "missing generated fixture $src"
  obj="$scratch/$t-$c.o"
  "${TOOLPREFIX}as" "${AS_FLAGS[@]}" -o "$obj" "$src"
  "${TOOLPREFIX}objdump" -h "$obj" > "$OUT/objdump-h.$t.$c.txt"
  "${TOOLPREFIX}objdump" -r "$obj" > "$OUT/objdump-r.$t.$c.txt"
  "${TOOLPREFIX}readelf" -SW "$obj" > "$OUT/readelf-SW.$t.$c.txt"
  "${TOOLPREFIX}readelf" -sW "$obj" > "$OUT/readelf-sW.$t.$c.txt"
done
echo "recorded ${#PAIRS[@]} objects under $OUT"
