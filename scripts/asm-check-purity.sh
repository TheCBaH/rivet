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

# The tool project's dependency declaration, over the SAME describe output - no
# second dune run. It rides here rather than on a target of its own because
# asm-purity already depends on asm-build and is already enforced by CI, and a
# rule that only ever ran against planted mutations would never notice a real
# undeclared dependency, a stale row, or a floor that became false.
opam exec -- ocaml tools/asm_audit.ml tool-dependencies "$desc" "$ASM_DIR"
