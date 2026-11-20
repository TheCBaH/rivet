#!/usr/bin/env bash
# js_of_ocaml sizes of a print-and-disassemble program per closure (test/jsoo_size), release
# profile: bytes, and bytes after gzip -9.
set -euo pipefail
cd "$(dirname "$0")/.."
names="size_driver size_text size_direct size_direct_no_x86"
targets=$(for n in $names; do printf ' test/jsoo_size/%s.bc.js' "$n"; done)
# shellcheck disable=SC2086
RIVET_JSOO_SIZE=true opam exec -- dune build --profile release $targets
printf '%-20s %12s %12s\n' program bytes gzip
for n in $names; do
  f=_build/default/test/jsoo_size/$n.bc.js
  printf '%-20s %12d %12d\n' "$n" "$(wc -c < "$f")" "$(gzip -9c < "$f" | wc -c)"
done
