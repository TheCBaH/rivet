#!/usr/bin/env bash
# Transitive purity audit of asm/'s production closure (.ai/asm_plan.md §1,
# §2.2, §3.7). See tools/asm_audit.ml for what each mechanism does and does not
# establish - in particular, `dune describe` carries no foreign-object
# information, so the no-C claim rests on the directory and stanza scans.
set -euo pipefail
cd "$(dirname "$0")/.."
ASM_DIR=${ASM_DIR:-asm}
desc=$(mktemp); trap 'rm -f "$desc"' EXIT
( cd "$ASM_DIR" && opam exec -- dune describe --lang 0.1 ) > "$desc"
opam exec -- ocaml tools/asm_audit.ml purity "$desc" "$ASM_DIR"
