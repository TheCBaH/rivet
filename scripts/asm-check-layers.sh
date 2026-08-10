#!/usr/bin/env bash
# Layer audit: §5.1's one-way dependency from generic machinery to target
# descriptions, and §3.7's rule that no generic source branches on a target's
# identity. Run over the resolved dependency graph, not over conventions.
set -euo pipefail
cd "$(dirname "$0")/.."
ASM_DIR=${ASM_DIR:-asm}
desc=$(mktemp); trap 'rm -f "$desc"' EXIT
( cd "$ASM_DIR" && opam exec -- dune describe --lang 0.1 ) > "$desc"
opam exec -- ocaml tools/asm_audit.ml layers "$desc" "$ASM_DIR"
