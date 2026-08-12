#!/usr/bin/env bash
# Planted-violation tests for the purity and layer audits.
#
# .ai/asm_plan.md's M0 exit criteria require the audits to "pass and reject every
# planted violation" - an audit that has never been observed to fail proves
# nothing. Each case copies asm/ to a scratch tree, introduces one violation,
# and requires the audit to exit non-zero *and* to name the right thing.
#
# Violations are planted both directly and transitively, per the plan: a direct
# dependency edge is the easy case, and the one a reviewer would have caught
# anyway.
set -euo pipefail
cd "$(dirname "$0")/.."
ASM_SRC=${ASM_DIR:-asm}

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
pass=0
fail=0

# Runs one case. $1 name, $2 audit mode, $3 substring the report must contain,
# $4 shell body that mutates the copied tree at $asm.
plant() {
  local name=$1 mode=$2 expect=$3 body=$4
  local asm="$work/$name"
  rm -rf "$asm"
  cp -r "$ASM_SRC" "$asm"
  rm -rf "$asm/_build"
  ( cd "$asm" && eval "$body" )

  local desc="$work/$name.sexp" out="$work/$name.out" rc=0
  # A planted violation may or may not still build; either way the audit must
  # not silently pass. A tree that cannot even be described counts as rejected,
  # but is reported distinctly so it is never mistaken for a real detection.
  if ! ( cd "$asm" && opam exec -- dune describe --lang 0.1 ) > "$desc" 2>"$out"; then
    echo "  ok   $name (rejected at describe time)"
    pass=$((pass + 1))
    return
  fi
  opam exec -- ocaml tools/asm_audit.ml "$mode" "$desc" "$asm" >"$out" 2>&1 && rc=0 || rc=$?

  if [ "$rc" -eq 0 ]; then
    echo "  FAIL $name: audit passed a planted violation" >&2
    sed 's/^/       /' "$out" >&2
    fail=$((fail + 1))
  elif ! grep -qF "$expect" "$out"; then
    echo "  FAIL $name: rejected, but not for the planted reason (wanted \"$expect\")" >&2
    sed 's/^/       /' "$out" >&2
    fail=$((fail + 1))
  else
    echo "  ok   $name"
    pass=$((pass + 1))
  fi
}

echo "planted violations:"

# A direct edge to the one C-backed package §5.3 names. It is installed in this
# switch, so its absence would otherwise be an accident of provisioning rather
# than a property the audit enforces.
plant zarith-direct purity 'reaches "zarith"' \
  "sed -i 's/(libraries fmt err_trace)/(libraries fmt err_trace zarith)/' lib/foundation/dune"

# The same edge one hop away, which is the case a per-package review misses.
plant zarith-transitive purity 'reaches "zarith"' "
  mkdir -p lib/relay
  printf 'let n = Z.of_int 1\n' > lib/relay/relay.ml
  printf '(library (name relay) (modes byte native) (libraries zarith))\n' > lib/relay/dune
  sed -i 's/(libraries fmt err_trace)/(libraries fmt err_trace relay)/' lib/foundation/dune"

# Unix reached from a production library. §3.7 bans it outright; it is also the
# edge most likely to appear by habit when someone needs to read a file.
plant unix-edge purity 'reaches "unix"' \
  "sed -i 's/(libraries fmt err_trace)/(libraries fmt err_trace unix)/' lib/foundation/dune"

# A package that compiles cleanly under js_of_ocaml because it ships a
# JavaScript runtime replacement for its C primitives, and still breaks the
# source-level no-C rule. This is why a successful JS build is not evidence of
# purity (plan guardrail 6). Planted as a local package so the case does not
# depend on such a package being installed.
plant js-runtime-stub purity 'ships a foreign object' "
  mkdir -p lib/fastpath
  printf 'external hash : string -> int = \"asm_fast_hash\"\n' > lib/fastpath/fastpath.ml
  printf 'int asm_fast_hash(void) { return 0; }\n' > lib/fastpath/stubs.c
  printf '//Provides: asm_fast_hash\nfunction asm_fast_hash(){return 0}\n' > lib/fastpath/runtime.js
  printf '(library (name fastpath) (modes byte native))\n' > lib/fastpath/dune
  sed -i 's/(libraries fmt err_trace)/(libraries fmt err_trace fastpath)/' lib/foundation/dune"

# The test-only ppx tree leaking into production. ppx_expect is native-only and
# pulls in time_now, which has C stubs, so this is a live risk rather than a
# hypothetical one.
plant ppx-in-production purity 'reaches "ppx_expect"' \
  "sed -i 's/(libraries fmt err_trace)/(libraries fmt err_trace ppx_expect)/' lib/foundation/dune"

# The same leak arriving through a vendored submodule rather than through one of
# our own stanzas. asm/vendor/err_trace is a whole upstream repository, and its
# own test/dune declares an inline-test library over ppx_expect; vendor/dune
# marks the checkout `data_only_dirs` so dune never reads it. Without that one
# line, `core_tests` becomes a production root - anything under vendor/ is
# production by `production_dirs` - and drags base, time_now and the ppx_expect
# runtime into the closure, which grows from 22 libraries to 42.
#
# Planted by deleting the file rather than by editing a stanza, because the
# protection *is* the file's existence: nothing else in the tree would fail if
# it were dropped in a rebase.
plant vendor-submodule-visible purity 'reaches "base"' "rm -f vendor/dune"

# A production library depending on test code. Not a C problem - a closure
# problem, and the one that would let a test helper end up in a shipped image.
plant test-in-production purity 'non-production library' "
  mkdir -p test/helper
  printf 'let scratch = 1\n' > test/helper/helper.ml
  printf '(library (name helper) (modes byte native))\n' > test/helper/dune
  sed -i 's/(libraries fmt err_trace)/(libraries fmt err_trace helper)/' lib/foundation/dune"

# §5.1's one-way rule, inverted: generic machinery reaching a target package.
plant generic-to-target layers 'depends on target' "
  mkdir -p targets/aarch64
  printf 'let name = \"aarch64\"\n' > targets/aarch64/aarch64_target.ml
  printf '(library (name aarch64_target) (modes byte native))\n' > targets/aarch64/dune
  sed -i 's/(libraries fmt err_trace)/(libraries fmt err_trace aarch64_target)/' lib/foundation/dune"

# A generic source branching on a target's identity. Compiles fine, which is
# exactly why this one has to be checked textually rather than through the graph.
plant target-name-in-generic layers 'mentions target name' \
  "printf '\nlet is_64bit t = t = \"x86_64\"\n' >> lib/foundation/span.ml"

echo "planted: $pass ok, $fail failed"
[ "$fail" -eq 0 ]
