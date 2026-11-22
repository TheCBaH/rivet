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

# One Fmt in the resolved closure: rivet.fmt only re-exports opam fmt, so the
# vendored tree builds no Fmt unit, and every object of the workspace that
# imports Fmt imports opam fmt's interface.
built=$(find "$work/_build/default/rivet" -iname 'fmt.cm*')
if [ -n "$built" ]; then
  printf 'external-deps: the vendored tree built its own Fmt:\n%s\n' "$built" >&2
  exit 1
fi
fmt_digest() { # interface digests only: a .cmx also lists implementation digests
  awk '/^[A-Z]/ { iface = ($0 ~ /^Interfaces imported/) } iface && $2 == "Fmt" && $1 !~ /^-+$/ { print $1 }'
}
want=$(opam exec -- ocamlobjinfo "$(opam exec -- ocamlfind query fmt)/fmt.cmi" | fmt_digest)
got=$(find "$work/_build/default" \( -name '*.cmi' -o -name '*.cmo' -o -name '*.cmx' \) -print0 |
  xargs -0 opam exec -- ocamlobjinfo | fmt_digest | sort -u)
consumer=$(find "$work/_build/default" -name '*Consumer.cm[ox]' -print0 | xargs -0 opam exec -- ocamlobjinfo | fmt_digest | sort -u)
if [ -z "$want" ] || [ "$got" != "$want" ] || [ "$consumer" != "$want" ]; then
  printf 'external-deps: Fmt digests: opam fmt %s, consumer %s, workspace:\n%s\n' \
    "${want:-none}" "${consumer:-none}" "${got:-none}" >&2
  exit 1
fi
echo "external-deps: one Fmt ($want), opam fmt's"
