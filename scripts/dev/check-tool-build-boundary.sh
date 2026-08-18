#!/usr/bin/env bash
# The focused tool build must compile no local library outside tools/.
#
# This is what keeps tool.md gate 6 true for the toolchain-free check targets:
# they gain a tools-build edge, and if that edge dragged in an assembler library
# then `make asm-fixtures-check` would need an assembler build.
#
# {1 Why not the obvious checks}
#
# Inspecting the ordinary _build tree proves nothing: it may hold artifacts from
# an earlier asm-build, so a file's presence says nothing about what THIS build
# compiled. Grepping `--display=short` on a warm cache proves nothing either:
# rules that do not rerun emit no lines, so absence is not evidence.
#
# So: a cold build into a PRIVATE build directory, then the resolved library
# closure of exactly one executable root, audited against an allowlist. A cold
# build makes presence meaningful; a UID closure makes the claim precise; and
# the private directory keeps the developer's ordinary _build untouched.
#
# The claim is specifically about LIBRARIES. It does not cover arbitrary rules
# or generated artifacts - note that the copy_files# SOURCES of err_trace appear
# in the build directory under vendor/, which is why this inspects the closure
# and not the tree.
set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)
ASM_DIR="$REPO_ROOT/asm"

BUILD_DIR=$(mktemp -d)
DESC=$(mktemp)
trap 'rm -rf -- "$BUILD_DIR" "$DESC"' EXIT

cd "$ASM_DIR"

# Cold, targeted, and into a directory whose name is neither predictable nor
# left behind.
opam exec -- dune build --build-dir="$BUILD_DIR" tools/bin/compcert_tools.exe

# The describe must come from the SAME build context, or the paths the auditor
# normalizes against would not be the ones it just built.
opam exec -- dune describe --lang 0.1 --build-dir="$BUILD_DIR" > "$DESC"

cd "$REPO_ROOT"
opam exec -- ocaml tools/asm_audit.ml tool-boundary "$DESC" "$ASM_DIR"
