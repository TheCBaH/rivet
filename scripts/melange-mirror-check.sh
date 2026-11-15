#!/usr/bin/env bash
# Fails when a library or its module list differs between the native tree and the Melange
# mirror under melange/. Needs no Melange; see scripts/melange_mirror.ml.
set -euo pipefail
cd "$(dirname "$0")/.."
exec opam exec -- ocaml scripts/melange_mirror.ml "${RIVET_DIR:-.}"
