#!/usr/bin/env bash
# Builds and runs test/consumer, a consumer that supplies its own Fmt
# (RIVET_EXTERNAL_DEPS=true), against a copy of this tree vendored
# into a throwaway workspace: the arrangement a real consumer has, including
# that the vendored tree must define nothing a consumer defines itself.
set -euo pipefail
cd "$(dirname "$0")/.."

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cp -r test/consumer/. "$work/"
mkdir "$work/rivet"
tar -c --exclude=./_build --exclude=./.git --exclude=./test/consumer \
  --exclude=./fixtures --exclude=./vendor/isa-data --exclude=./browser/node_modules . |
  tar -x -C "$work/rivet"
RIVET_EXTERNAL_DEPS=true opam exec -- dune build --root "$work" @all @runtest
