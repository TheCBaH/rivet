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

# Runs one case.
#   $1 name
#   $2 audit mode
#   $3 outcome contract, one of:
#        reject-either      describe OR the audit rejects it. The weak legacy
#                           contract, kept only for the purity cases whose
#                           stated property genuinely allows either stage
#        audit-fail         describe SUCCEEDS and the audit rejects it. Required
#                           whenever a case claims a specific auditor rule -
#                           otherwise a mutation that merely broke a stanza
#                           would satisfy it without the auditor ever running
#        describe-fail      `dune describe` itself rejects it
#        build-fail         `dune build` rejects it. MEASURED, not assumed:
#                           `dune describe` does NOT fail on an unresolvable
#                           library - it reports the graph it managed to
#                           resolve - so a cross-project library edge is caught
#                           at BUILD time and is invisible to any describe-based
#                           audit. Requiring a specific message is what
#                           distinguishes "dune rejected the right thing" from
#                           "the tree happened not to build"
#        audit-pass         describe succeeds AND the audit exits 0. Needed for
#                           the cases that prove a check is not over-broad
#   $4 substring the output must contain (ignored for audit-pass)
#   $5 shell body that mutates the copied tree at $asm
# {1 Applying a mutation}
#
# `sed -i` SUCCEEDS when its pattern matches nothing, so a plant whose pattern
# has gone stale silently mutates nothing and the case then fails with whatever
# unrelated thing the audit noticed next. That is exactly what happened when a
# library was added to tools/lib/dune: three cases broke, and not one of them
# said so. edit() compares the file before and after and fails loudly instead.
edit() {
  local file=$1 expr=$2 before
  before=$(cat "$file")
  sed -i "$expr" "$file"
  if [ "$before" = "$(cat "$file")" ]; then
    echo "PLANT-PATTERN-STALE: $expr did not match anything in $file" >&2
    return 1
  fi
}

# {2 Typed mutations}
#
# `edit` makes a stale pattern loud, but it cannot make one unnecessary. Seventeen
# cases used to spell their mutation as a sed expression containing a COPY of the
# stanza's real library list - `(libraries err_trace bos fpath digestif.ocaml
# mtime.clock.os unix)` appeared in four of them - so adding one library to
# tools/lib/dune meant editing four patterns in lockstep. That is exactly what
# went wrong in Phase 6.
#
# These say what the mutation IS. add_library finds the field by matching
# parentheses rather than by matching its contents, so it is immune to the
# contents changing - which is the only property that made the sed patterns
# fragile.

# add_library <dune-file> <library>
add_library() {
  local file=$1 lib=$2 text
  # `$(cat)` strips trailing newlines; the X sentinel preserves the file's exact
  # bytes, so a plant cannot quietly reformat the stanza it is mutating.
  text=$(cat "$file"; printf X); text=${text%X}

  local n=${#text} start=0 i=-1
  while :; do
    local rest=${text:start} pre
    pre=${rest%%'(libraries'*}
    [ "$pre" = "$rest" ] && break
    local cand=$((start + ${#pre})) head this_line
    head=${text:0:cand}
    this_line=${head##*$'\n'}
    # A ';' earlier on the same line means this occurrence is inside a dune
    # comment and is not a field at all.
    case "$this_line" in
      *';'*) start=$((cand + 1)); continue ;;
    esac
    i=$cand
    break
  done
  if [ "$i" -lt 0 ]; then
    echo "PLANT-PATTERN-STALE: no (libraries ...) field in $file" >&2
    return 1
  fi

  local depth=0 j c
  for ((j = i; j < n; j++)); do
    c=${text:j:1}
    if [ "$c" = '(' ]; then
      depth=$((depth + 1))
    elif [ "$c" = ')' ]; then
      depth=$((depth - 1))
      [ "$depth" -eq 0 ] && break
    fi
  done
  if [ "$depth" -ne 0 ]; then
    echo "PLANT-PATTERN-STALE: unbalanced (libraries ...) in $file" >&2
    return 1
  fi
  printf '%s %s%s' "${text:0:j}" "$lib" "${text:j}" > "$file"
}

# new_library <dir> <name> [library...] - a throwaway library in the copied tree.
new_library() {
  local dir=$1 name=$2
  shift 2
  mkdir -p "$dir"
  if [ "$#" -gt 0 ]; then
    printf '(library (name %s) (modes byte native) (libraries %s))\n' "$name" "$*" > "$dir/dune"
  else
    printf '(library (name %s) (modes byte native))\n' "$name" > "$dir/dune"
  fi
}

plant() {
  local name=$1 mode=$2 outcome=$3 expect=$4 body=$5
  local asm="$work/$name"
  rm -rf "$asm"
  cp -r "$ASM_SRC" "$asm"
  rm -rf "$asm/_build"
  # `set -e` inside the subshell, so a stale edit anywhere in a multi-command
  # body aborts it. Without this only the LAST command's status would be read,
  # and a stale edit followed by a working one would report success.
  if ! ( cd "$asm" && set -e && eval "$body" ); then
    echo "  FAIL $name: the planted mutation did not apply" >&2
    fail=$((fail + 1))
    return
  fi

  local desc="$work/$name.sexp" out="$work/$name.out" rc=0
  local described=0

  if [ "$outcome" = build-fail ]; then
    if ( cd "$asm" && opam exec -- dune build @all ) >"$out" 2>&1; then
      echo "  FAIL $name: expected dune build to reject this, but it succeeded" >&2
      fail=$((fail + 1))
    elif ! grep -qF "$expect" "$out"; then
      echo "  FAIL $name: dune build failed, but not for the planted reason (wanted \"$expect\")" >&2
      sed 's/^/       /' "$out" >&2
      fail=$((fail + 1))
    else
      echo "  ok   $name (dune build rejected it: $expect)"
      pass=$((pass + 1))
    fi
    return
  fi

  if ( cd "$asm" && opam exec -- dune describe --lang 0.1 ) > "$desc" 2>"$out"; then
    described=1
  fi

  bad() {
    echo "  FAIL $name: $1" >&2
    sed 's/^/       /' "$out" >&2
    fail=$((fail + 1))
  }

  if [ "$described" -eq 0 ]; then
    case "$outcome" in
      reject-either)
        echo "  ok   $name (rejected at describe time)"; pass=$((pass + 1)) ;;
      describe-fail)
        if grep -qF "$expect" "$out"; then
          echo "  ok   $name (dune rejected it: $expect)"; pass=$((pass + 1))
        else
          bad "dune rejected it, but not for the planted reason (wanted \"$expect\")"
        fi ;;
      *) bad "expected $outcome, but the tree could not be described" ;;
    esac
    return
  fi

  if [ "$outcome" = describe-fail ]; then
    bad "expected dune to reject this, but describe succeeded"
    return
  fi

  opam exec -- ocaml tools/asm_audit.ml "$mode" "$desc" "$asm" >"$out" 2>&1 && rc=0 || rc=$?

  case "$outcome" in
    audit-pass)
      if [ "$rc" -eq 0 ]; then
        echo "  ok   $name (audit stayed green, as required)"; pass=$((pass + 1))
      else
        bad "the audit rejected a case that must stay green"
      fi ;;
    *)
      if [ "$rc" -eq 0 ]; then
        bad "audit passed a planted violation"
      elif ! grep -qF "$expect" "$out"; then
        bad "rejected, but not for the planted reason (wanted \"$expect\")"
      else
        echo "  ok   $name"; pass=$((pass + 1))
      fi ;;
  esac
}

echo "planted violations:"

# A direct edge to the one C-backed package §5.3 names. It is installed in this
# switch, so its absence would otherwise be an accident of provisioning rather
# than a property the audit enforces.
plant zarith-direct purity reject-either 'reaches "zarith"' \
  "add_library lib/foundation/dune zarith"

# The same edge one hop away, which is the case a per-package review misses.
plant zarith-transitive purity reject-either 'reaches "zarith"' "
  new_library lib/relay relay zarith
  printf 'let n = Z.of_int 1\n' > lib/relay/relay.ml
  add_library lib/foundation/dune relay"

# Unix reached from a production library. §3.7 bans it outright; it is also the
# edge most likely to appear by habit when someone needs to read a file.
plant unix-edge purity reject-either 'reaches "unix"' \
  "add_library lib/foundation/dune unix"

# A package that compiles cleanly under js_of_ocaml because it ships a
# JavaScript runtime replacement for its C primitives, and still breaks the
# source-level no-C rule. This is why a successful JS build is not evidence of
# purity (plan guardrail 6). Planted as a local package so the case does not
# depend on such a package being installed.
plant js-runtime-stub purity reject-either 'ships a foreign object' "
  mkdir -p lib/fastpath
  printf 'external hash : string -> int = \"asm_fast_hash\"\n' > lib/fastpath/fastpath.ml
  printf 'int asm_fast_hash(void) { return 0; }\n' > lib/fastpath/stubs.c
  printf '//Provides: asm_fast_hash\nfunction asm_fast_hash(){return 0}\n' > lib/fastpath/runtime.js
  new_library lib/fastpath fastpath
  add_library lib/foundation/dune fastpath"

# The test-only ppx tree leaking into production. ppx_expect is native-only and
# pulls in time_now, which has C stubs, so this is a live risk rather than a
# hypothetical one.
plant ppx-in-production purity reject-either 'reaches "ppx_expect"' \
  "add_library lib/foundation/dune ppx_expect"

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
plant vendor-submodule-visible purity reject-either 'reaches "base"' "rm -f vendor/dune"

# A production library depending on test code. Not a C problem - a closure
# problem, and the one that would let a test helper end up in a shipped image.
plant test-in-production purity reject-either 'non-production library' "
  new_library test/helper helper
  printf 'let scratch = 1\n' > test/helper/helper.ml
  add_library lib/foundation/dune helper"

# §5.1's one-way rule, inverted: generic machinery reaching a target package.
plant generic-to-target layers reject-either 'depends on target' "
  new_library targets/aarch64 aarch64_target
  printf 'let name = \"aarch64\"\n' > targets/aarch64/aarch64_target.ml
  add_library lib/foundation/dune aarch64_target"

# A generic source branching on a target's identity. Compiles fine, which is
# exactly why this one has to be checked textually rather than through the graph.
plant target-name-in-generic layers reject-either 'mentions target name' \
  "printf '\nlet is_64bit t = t = \"x86_64\"\n' >> lib/foundation/span.ml"

# {1 The tool project boundary (tool.md §5)}
#
# The nested dune-project makes cross-project LIBRARY edges structurally
# impossible, and these two cases are how that claim stops being a claim. Both
# are describe-fail rather than audit-fail: dune rejects them before the auditor
# ever runs, which is a stronger outcome - but it also means the auditor's own
# allowlist branch would go unexercised, which is what the third case is for.

plant tool-reaches-assembler tool-boundary build-fail 'Library "asm_core" not found' \
  "add_library tools/lib/dune asm_core"

plant assembler-reaches-tool purity build-fail 'Library "compcert_tools" not found' \
  "add_library lib/foundation/dune compcert_tools"

# Project scope stops local libraries; it does NOT stop opam packages. An
# assembler stanza naming an external tool package builds fine, and only the
# purity audit catches it - which is why the structural guarantee above does not
# make this case redundant.
plant assembler-reaches-tool-package purity audit-fail 'ships a foreign object' \
  "add_library lib/foundation/dune bos"

# The auditor's own fail-closed branch. Its ALLOWLIST branch turns out to be
# unreachable by planting: a local library the tool project can name must live
# inside the tool project, and the tool project's directory IS tools/, so
# anything in scope is inside the allowlist by construction. That is a property
# of the nested-project layout rather than a gap - the allowlist would start
# rejecting things the moment the layout changed, which is why it is kept - but
# it does mean the branch cannot be exercised from here honestly.
#
# What IS reachable, and worth pinning, is the fail-closed behavior when the
# expected executable root is gone: without it the boundary check would pass
# vacuously on a tree where the tools were never built.
plant tool-boundary-root-missing tool-boundary audit-fail 'no executable with implementation' \
  "rm -rf tools/bin"

# And the root selector. Adding a dependency to an UNRELATED tool executable
# must leave the focused boundary green, because tools-build builds one
# executable and auditing every tool root would stop proving that target's
# boundary.
#
# The first edit here used to be a no-op BY CONSTRUCTION - its pattern and
# replacement were the same string - and it had also gone stale, since the
# names list is now multi-line. edit() surfaced both. Only the second edit ever
# did anything, so the case is now stated as the one mutation it actually is.
plant tool-boundary-unrelated-root tool-boundary audit-pass '' \
  "add_library tools/test/unit/dune cmdliner"

# {2 The dependency rules}

plant dep-undeclared-in-lib tool-dependencies audit-fail 'rule 1' \
  "add_library tools/lib/dune logs"

# The case that fails today for want of executable parsing: bin/dune is where
# cmdliner is declared, so a closure taken over libraries alone cannot see it.
plant dep-undeclared-in-bin tool-dependencies audit-fail 'rule 1' \
  "add_library tools/bin/dune logs"

plant dep-undeclared-in-test tool-dependencies audit-fail 'rule 1' \
  "add_library tools/test/unit/dune logs"

# A row whose libraries no longer appear anywhere: the stale declaration this
# rule exists to catch.
plant dep-stale-row tool-dependencies audit-fail 'rule 2' \
  "printf '(logs (>= 0.7.0) runtime (logs))\n' >> tools/dependencies.sexp"

plant dep-floor-too-high tool-dependencies audit-fail 'below the declared floor' \
  "edit tools/dependencies.sexp 's/(bos      (>= 0.2.1)/(bos      (>= 99.0.0)/'"

# A distinct library name, so this exercises rule 3 rather than tripping the
# duplicate-library-mapping check first.
plant dep-not-installed tool-dependencies audit-fail 'not installed' \
  "printf '(this-package-does-not-exist (>= 1.0.0) runtime (this-library-does-not-exist))\n' >> tools/dependencies.sexp"

# The strict parser, which must fail BEFORE any rule runs and must say where.
plant dep-parse-stray-paren tool-dependencies audit-fail 'stray' \
  "printf ')\n' >> tools/dependencies.sexp"

plant dep-parse-bad-operator tool-dependencies audit-fail 'only >= is accepted' \
  "edit tools/dependencies.sexp 's/(bos      (>= 0.2.1)/(bos      (> 0.2.1)/'"

plant dep-parse-multi-clause tool-dependencies audit-fail 'trailing item' \
  "edit tools/dependencies.sexp 's/(bos      (>= 0.2.1)/(bos      (>= 0.2.1 \& < 9.0)/'"

plant dep-parse-unknown-scope tool-dependencies audit-fail 'unknown scope' \
  "edit tools/dependencies.sexp 's/(bos      (>= 0.2.1) runtime/(bos      (>= 0.2.1) sometimes/'"

plant dep-parse-duplicate-package tool-dependencies audit-fail 'duplicate package' \
  "printf '(bos (>= 0.2.1) runtime (bos))\n' >> tools/dependencies.sexp"

plant dep-parse-bare-digestif tool-dependencies audit-fail 'bare `digestif`' \
  "edit tools/dependencies.sexp 's/(digestif.ocaml)/(digestif)/'"

# Rule 4: a transitive-only edge to a library outside the containment list. The
# graph is mutated rather than the auditor's constant, because plant copies only
# asm/ and then runs the REPOSITORY's original asm_audit.ml - a test that
# deleted an allowlist entry from a copied auditor would not be running the
# auditor it thinks it is.
plant dep-transitive-outside-allowlist tool-dependencies audit-fail 'rule 4' "
  printf '(zarith (>= 1.0) runtime (zarith))\n' >> tools/dependencies.sexp
  add_library tools/lib/dune zarith"

# An unrelated installed package that nothing uses must change NOTHING. Extra
# installed packages are normal; the two real errors are an undeclared used
# package and a declared unused one, and neither is this.
plant dep-unrelated-installed-package tool-dependencies audit-pass '' "true"

echo "planted: $pass ok, $fail failed"
[ "$fail" -eq 0 ]
