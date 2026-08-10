#!/usr/bin/env bash
# The Melange opt-in, verified rather than asserted (.ai/asm_plan.md §3.2, an
# M0 exit criterion).
#
# The claim has two halves, and only the first is obvious:
#
#   with ASM_MELANGE unset, `dune rules @all` contains ZERO melc entries and
#   `dune build @all` succeeds - including where Melange is not available at
#   all;
#
#   with ASM_MELANGE=true, the melange rules appear and build.
#
# The second half matters because a check with only the first would pass on a
# tree where every Melange stanza had been deleted.
#
# Why "zero melc entries" rather than "the build succeeded": declaring melange
# mode on a library schedules melc into @all whether or not anything emits
# JavaScript, so on a machine that *has* Melange a mis-gated tree builds fine
# and nobody notices until an OCaml 5.x leg - where Melange 7.0.1-414 cannot be
# installed - fails instead. Reading the rules is what detects the mis-gating on
# the machine that would not otherwise care.
set -euo pipefail
cd "$(dirname "$0")/.."
ASM_DIR=${ASM_DIR:-asm}
failures=0
shim=""

cleanup() { [ -n "$shim" ] && rm -rf "$shim"; return 0; }
trap cleanup EXIT

note() { printf '  %-46s %s\n' "$1" "$2"; }
fail() { printf '  FAIL %-41s %s\n' "$1" "$2"; failures=$((failures + 1)); }

# `dune rules` prints every rule in the alias's closure as an s-expression; melc
# appears as an action's program. Counting matches is enough - what is asserted
# is zero versus nonzero, not which rules.
rules_melc_count() {
  ( cd "$ASM_DIR" && opam exec -- dune rules @all 2>/dev/null || true ) | grep -c melc || true
}

# melc lives on the opam switch's PATH, not the ambient one, so it has to be
# looked up the way dune will find it. Using the ambient PATH would report "not
# installed" on every switch that has it and silently downgrade the check.
melc_path="$(opam exec -- sh -c 'command -v melc' 2>/dev/null || true)"

echo "== ASM_MELANGE unset =="
unset ASM_MELANGE || true
n="$(rules_melc_count)"
if [ "$n" -eq 0 ]; then
  note "dune rules @all melc entries" "0"
else
  fail "dune rules @all melc entries" "$n (expected 0; a stanza is mis-gated)"
fi

if [ -n "$melc_path" ]; then
  # "No Melange installed" is simulated with a shim directory holding a symlink
  # to every tool on the switch *except* melc, rather than by dropping the
  # switch's bin directory from PATH - dune itself lives there, so dropping it
  # would test a switch with no build system rather than one with no Melange.
  # Uninstalling for real is not an option either: this has to be runnable on a
  # developer's machine.
  bin="$(dirname "$melc_path")"
  shim="$(mktemp -d)"
  for f in "$bin"/*; do
    [ "$(basename "$f")" = melc ] || ln -s "$f" "$shim/" 2>/dev/null || true
  done
  note "melc hidden behind a shim of" "$bin"
  if [ -e "$shim/melc" ]; then
    fail "shim" "melc is still reachable through the shim"
  elif opam exec -- env PATH="$shim:/usr/local/bin:/usr/bin:/bin" \
      sh -c "cd '$ASM_DIR' && dune build @all" >/dev/null 2>&1; then
    note "dune build @all with melc unavailable" "ok"
  else
    fail "dune build @all with melc unavailable" "failed"
  fi
else
  note "melc not installed on this switch" "the unset configuration is already clean"
  if ( cd "$ASM_DIR" && opam exec -- dune build @all ) >/dev/null 2>&1; then
    note "dune build @all" "ok"
  else
    fail "dune build @all" "failed"
  fi
fi

echo "== ASM_MELANGE=true =="
if [ -z "$melc_path" ]; then
  # Not a skip that hides a failure: with no melc there is nothing that could
  # run these rules, and saying so is more useful than pretending to check. The
  # pinned CI leg is where this half is required to run.
  note "melc not installed" "the opt-in half cannot be verified on this switch"
else
  n="$(ASM_MELANGE=true rules_melc_count)"
  if [ "$n" -gt 0 ]; then
    note "dune rules @all melc entries" "$n"
  else
    fail "dune rules @all melc entries" "0 (the opt-in schedules nothing)"
  fi
  if ( cd "$ASM_DIR" && ASM_MELANGE=true opam exec -- dune build @all ) >/dev/null 2>&1; then
    note "dune build @all" "ok"
  else
    fail "dune build @all" "failed"
  fi
fi

if [ "$failures" -eq 0 ]; then
  echo "melange-optin: ok"
else
  echo "melange-optin: $failures failure(s)"
  exit 1
fi
