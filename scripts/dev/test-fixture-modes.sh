#!/usr/bin/env bash
# The OCaml fixture modes against fake compilers, in a throwaway corpus.
#
# This is the Phase 3 counterpart of tools/dev/test-asm-fixture-gen.sh, and it
# asserts the SAME control-flow properties against the new implementation - the
# ones Phase 0A found the shell getting wrong.
#
# Fake compilers are installed at $FIXTURE_WORK/install/<t>/bin/ccomp, NOT on
# PATH: every call site spells that absolute path, so a PATH shim would never be
# consulted.
set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)
EXE="$REPO_ROOT/asm/_build/default/tools/bin/compcert_tools.exe"
[ -x "$EXE" ] || { echo "FATAL: compcert-tools not built - run 'make tools-build'" >&2; exit 1; }

SCRATCH=$(mktemp -d)
trap 'rm -rf -- "$SCRATCH"' EXIT
pass=0; fail=0
ok()  { printf '  ok   %s\n' "$1"; pass=$((pass + 1)); }
bad() { printf '  FAIL %s: %s\n' "$1" "$2" >&2; fail=$((fail + 1)); }

new_repo() {
  local repo=$1; shift
  mkdir -p "$repo/tools" "$repo/asm"
  cp "$REPO_ROOT/tools/target-matrix.sh" "$repo/tools/"
  : > "$repo/Makefile"
  : > "$repo/asm/dune-project"
  local c
  for c in "$@"; do
    mkdir -p "$repo/asm/fixtures/compcert-3.17/$c/source"
    printf 'int asm_test_entry(void) { return 42; }\n' > "$repo/asm/fixtures/compcert-3.17/$c/source/$c.c"
  done
}

install_fake_ccomp() {
  local work=$1 target=$2 mode=$3
  mkdir -p "$work/install/$target/bin"
  cat > "$work/install/$target/bin/ccomp" <<EOF
#!/usr/bin/env bash
set -u
mode=$mode
# Recorded FIRST, before the -version early exit and before the shift loop
# below consumes "\$@" - tool.md gate 3. argv is the load-bearing half of the
# invocation shape: the flags decide the committed bytes, and CompCert writes
# its own command line into a banner that is then hashed, so a wrong flag or an
# absolutized path would change every hash in the case.
#
# Arguments are separated by \\x1f rather than spaces, because a space-separated
# log cannot distinguish one argument containing a space from two arguments -
# exactly the confusion an argv assertion exists to rule out.
#
# An if statement rather than a test-and-brace: the latter would be the last
# command in some paths, so its status would become this script's, and an unset
# FAKE_LOG would then make every compile "fail". (No backticks in this comment
# either - the heredoc delimiter is unquoted, so they would be substitution.)
# ONE line per invocation, cwd and argv together. Two lines would have to be
# re-paired by the reader, and the -version calls (which run from wherever the
# process happens to be, and correctly so - version output does not depend on
# cwd) would then be indistinguishable from compiles when asserting cwd.
if [ -n "\${FAKE_LOG:-}" ]; then
  { printf 'cwd=%s\targv=' "\$PWD"; printf '%s\x1f' "\$@"; printf '\n'
  } >> "\$FAKE_LOG"
fi
if [ "\${1:-}" = "-version" ]; then
  [ "\$mode" = fail-version ] && { echo "no version" >&2; exit 1; }
  echo "The CompCert C compiler, version 3.17 (fake $target)"; exit 0
fi
[ "\$mode" = fail-compile ] && { echo "compilation failed" >&2; exit 1; }
out=""
while [ "\$#" -gt 0 ]; do
  [ "\$1" = "-o" ] && { out=\$2; shift 2; continue; }
  shift
done
mkdir -p "\$(dirname "\$out")"
if [ "\$mode" = gate-violation ]; then
  printf '\t.text\nnot_the_entry:\n\tret\n' > "\$out"
else
  printf '\t.text\nasm_test_entry:\n\tret\n' > "\$out"
fi
exit 0
EOF
  chmod +x "$work/install/$target/bin/ccomp"
}

install_all() {
  local work=$1 mode=$2 odd=${3:-} odd_mode=${4:-}
  for t in x86_32 x86_64 arm aarch64 riscv32 riscv64; do
    if [ "$t" = "$odd" ]; then install_fake_ccomp "$work" "$t" "$odd_mode"
    else install_fake_ccomp "$work" "$t" "$mode"; fi
  done
}

# FAKE_LOG is always exported, so every fake compiler in every scenario records
# its cwd and argv. The invocation-gate test below is the only one that reads
# the log, but recording unconditionally means it cannot silently stop being
# written by a scenario that forgets to opt in.
run() {
  local repo=$1; shift
  ( cd "$repo" && COMPCERT_REPO_ROOT="$repo" FIXTURE_WORK="$repo/work" \
      FAKE_LOG="$repo/fake.log" "$EXE" "$@" ) \
    >"$SCRATCH/out" 2>"$SCRATCH/err"
}

assert_no_manifest() {
  local name=$1 root=$2
  for f in manifest.txt manifest.txt.new manifest.txt.diff; do
    [ -e "$root/$f" ] && { bad "$name" "$root/$f exists and must not"; return 1; }
  done
  return 0
}

stderr_has() {
  grep -qF -- "$2" "$SCRATCH/err" || { bad "$1" "stderr lacks '$2'"; sed 's/^/       /' "$SCRATCH/err" >&2; return 1; }
}

# 1. Clean regen writes a manifest with a hash for every file.
t_clean() {
  local name=clean-regen
  local repo="$SCRATCH/$name"
  new_repo "$repo" alpha; install_all "$repo/work" good
  local rc=0; run "$repo" fixture regen -- alpha || rc=$?
  local root="$repo/asm/fixtures/compcert-3.17/alpha"
  [ "$rc" = 0 ] || { bad "$name" "exit $rc"; return; }
  [ -f "$root/manifest.txt" ] || { bad "$name" "no manifest"; return; }
  [ -e "$root/manifest.txt.new" ] && { bad "$name" ".new left behind"; return; }
  [ -e "$root/manifest.txt.diff" ] && { bad "$name" ".diff left behind"; return; }
  local n; n=$(grep -c '^sha256:' "$root/manifest.txt")
  [ "$n" = 7 ] || { bad "$name" "expected 7 sha256 records, got $n"; return; }
  grep -qE '^sha256:[^\t]*\t$' "$root/manifest.txt" && { bad "$name" "an empty hash"; return; }
  local v; v=$(grep -c '^ccomp-version:' "$root/manifest.txt")
  [ "$v" = 6 ] || { bad "$name" "expected 6 ccomp-version records, got $v"; return; }
  ok "$name"
}

# 2. Compiler fails on target 3 of 6: no manifest, and 4-6 never attempted.
t_compile_fail() {
  local name=compiler-fails-midway
  local repo="$SCRATCH/$name"
  new_repo "$repo" alpha; install_all "$repo/work" good arm fail-compile
  local rc=0; run "$repo" fixture regen -- alpha || rc=$?
  local root="$repo/asm/fixtures/compcert-3.17/alpha"
  [ "$rc" = 1 ] || { bad "$name" "exit $rc, wanted 1"; return; }
  stderr_has "$name" "fixture compilation failed for alpha/arm" || return
  assert_no_manifest "$name" "$root" || return
  [ -e "$root/aarch64/alpha.s" ] && { bad "$name" "targets after the failure were attempted"; return; }
  ok "$name"
}

# 3. Gate violation: the case fails and NO manifest is written. This is the
#    Phase 0A defect, asserted against the new implementation.
t_gate() {
  local name=gate-violation-writes-nothing
  local repo="$SCRATCH/$name"
  new_repo "$repo" alpha; install_all "$repo/work" good riscv64 gate-violation
  local rc=0; run "$repo" fixture regen -- alpha || rc=$?
  [ "$rc" = 1 ] || { bad "$name" "exit $rc, wanted 1"; return; }
  stderr_has "$name" "GATE alpha/riscv64: no asm_test_entry label" || return
  assert_no_manifest "$name" "$repo/asm/fixtures/compcert-3.17/alpha" || return
  ok "$name"
}

# 4. Two cases, the first failing: the second still runs.
t_two_cases() {
  local name=first-fails-second-runs
  local repo="$SCRATCH/$name"
  new_repo "$repo" alpha beta; install_all "$repo/work" good riscv64 gate-violation
  local rc=0; run "$repo" fixture regen -- alpha beta || rc=$?
  [ "$rc" = 1 ] || { bad "$name" "exit $rc"; return; }
  stderr_has "$name" "GATE alpha/riscv64" || return
  stderr_has "$name" "GATE beta/riscv64" || return
  ok "$name"
}

# 5. ccomp -version fails while the compiler is otherwise present: no manifest,
#    and specifically not one carrying an EMPTY ccomp-version value.
t_version_fail() {
  local name=version-failure-installs-nothing
  local repo="$SCRATCH/$name"
  new_repo "$repo" alpha; install_all "$repo/work" good x86_64 fail-version
  local rc=0; run "$repo" fixture regen -- alpha || rc=$?
  [ "$rc" = 1 ] || { bad "$name" "exit $rc, wanted 1"; return; }
  assert_no_manifest "$name" "$repo/asm/fixtures/compcert-3.17/alpha" || return
  ok "$name"
}

# 6. A scope change is INSTALLED and THEN reported as a failure.
t_scope_change() {
  local name=rehash-installs-and-fails
  local repo="$SCRATCH/$name"
  new_repo "$repo" alpha; install_all "$repo/work" good
  run "$repo" fixture regen -- alpha
  local root="$repo/asm/fixtures/compcert-3.17/alpha"
  printf 'stray\n' > "$root/x86_64/stray.s"
  local rc=0; run "$repo" fixture rehash -- alpha || rc=$?
  [ "$rc" = 1 ] || { bad "$name" "exit $rc, wanted 1"; return; }
  stderr_has "$name" "manifest CHANGED - review the differences" || return
  grep -q 'sha256:x86_64/stray.s' "$root/manifest.txt" || { bad "$name" "the change was NOT installed"; return; }
  [ -e "$root/manifest.txt.new" ] && { bad "$name" ".new left behind"; return; }
  [ -e "$root/manifest.txt.diff" ] && { bad "$name" ".diff left behind"; return; }
  ok "$name"
}

# 7. verify: byte-identical, and a mismatch puts the diff on STDOUT with the
#    fatal on stderr while later cases keep running.
t_verify() {
  local name=verify
  local repo="$SCRATCH/$name"
  new_repo "$repo" alpha beta; install_all "$repo/work" good
  run "$repo" fixture regen -- alpha beta
  local rc=0; run "$repo" fixture verify x86_64 -- alpha beta || rc=$?
  [ "$rc" = 0 ] || { bad "$name" "clean verify exited $rc"; sed 's/^/       /' "$SCRATCH/err" >&2; return; }
  grep -q 'alpha/x86_64 regenerates byte-identically' "$SCRATCH/out" || { bad "$name" "no success line"; return; }
  # Now tamper with alpha only; beta must still be reported.
  printf 'tampered\n' >> "$repo/asm/fixtures/compcert-3.17/alpha/x86_64/alpha.s"
  rc=0; run "$repo" fixture verify x86_64 -- alpha beta || rc=$?
  [ "$rc" = 1 ] || { bad "$name" "tampered verify exited $rc, wanted 1"; return; }
  grep -q '^---' "$SCRATCH/out" || { bad "$name" "the diff is not on stdout"; return; }
  stderr_has "$name" "fixture regeneration differs for alpha/x86_64" || return
  grep -q 'beta/x86_64 regenerates byte-identically' "$SCRATCH/out" || { bad "$name" "the later case did not run"; return; }
  ok "$name"
}

# 8. The two target exits, which a naive CLI wiring collapses into one.
#
# The --legacy-prog usage text this used to assert retired with the launcher in
# Phase 8. The DISTINCTION did not: no target at all is a malformed command line
# and exits 2, while a target that is not one of the six is a rejected VALUE and
# exits 1. A Cmdliner converter would have made both parse errors and exited 2
# for each, which is exactly why the target is validated inside the term.
t_usage() {
  local name=target-exit-codes
  local repo="$SCRATCH/$name"
  new_repo "$repo" alpha; install_all "$repo/work" good
  local rc=0; run "$repo" fixture verify || rc=$?
  [ "$rc" = 2 ] || { bad "$name" "no-target exited $rc, wanted 2"; return; }
  grep -q 'FATAL' "$SCRATCH/err" && { bad "$name" "no-target must NOT print a FATAL"; return; }
  rc=0; run "$repo" fixture verify sparc || rc=$?
  [ "$rc" = 1 ] || { bad "$name" "bad-target exited $rc, wanted 1"; return; }
  stderr_has "$name" "FATAL: --verify supports x86_32 x86_64 arm aarch64 riscv32 riscv64, got 'sparc'" || return
  ok "$name"
}

# 9. tool.md gate 3 - the invocation shape the fake compilers actually observe.
#
# Everything else in this file asserts what ended up on disk. This asserts what
# was ASKED FOR, which is the only way to catch a difference that a fake
# compiler is too simple to expose: a fake writes the same bytes whatever flags
# it is handed, so a wrong -marm, an absolutized -o, or a missing -S would leave
# every other test in this file green while changing every hash the real
# compiler would produce.
#
# Three properties, all of which the shell had and none of which is optional:
#
#   -S                     assembly out, not an object;
#   ccomp_args per target  arm alone carries -marm;
#   relative paths, both   CompCert embeds its command line in a banner that is
#                          then hashed, so an absolute -o or source argument
#                          writes this checkout's location into committed bytes.
#
# cwd is asserted too, and it differs by mode on purpose: regen compiles in the
# fixture root because that is where the outputs belong, verify compiles in a
# private scratch directory because it must not touch the corpus.
t_invocation() {
  local name=invocation-shape
  local repo="$SCRATCH/$name"
  new_repo "$repo" alpha; install_all "$repo/work" good

  # {1 regen: all six targets, cwd = the fixture root}
  local rc=0; run "$repo" fixture regen -- alpha || rc=$?
  [ "$rc" = 0 ] || { bad "$name" "regen exited $rc"; return; }
  local root="$repo/asm/fixtures/compcert-3.17/alpha"

  # Compiles only: -o is what distinguishes them from the -version calls, which
  # run from the process's own cwd and correctly do not care where that is.
  local compiles; compiles=$(grep -F -- '-o' "$repo/fake.log" || true)

  local n; n=$(printf '%s\n' "$compiles" | grep -c . || true)
  [ "$n" = 6 ] || { bad "$name" "expected 6 compiles, logged $n"; return; }

  local t want got
  for t in x86_32 x86_64 arm aarch64 riscv32 riscv64; do
    case "$t" in
      arm) want=$(printf 'cwd=%s\targv=-S\x1f-marm\x1f-o\x1f%s/alpha.s\x1fsource/alpha.c\x1f' "$root" "$t") ;;
      *)   want=$(printf 'cwd=%s\targv=-S\x1f-o\x1f%s/alpha.s\x1fsource/alpha.c\x1f' "$root" "$t") ;;
    esac
    got=$(printf '%s\n' "$compiles" | grep -F "$t/alpha.s" | head -1)
    if [ "$got" != "$want" ]; then
      bad "$name" "$t: got $(printf '%q' "$got"), wanted $(printf '%q' "$want")"
      return
    fi
  done

  # {2 verify: cwd is a scratch directory, NOT the corpus}
  rm -f "$repo/fake.log"
  rc=0; run "$repo" fixture verify -- x86_64 alpha || rc=$?
  [ "$rc" = 0 ] || { bad "$name" "verify exited $rc"; return; }

  local vline; vline=$(grep -F -- '-o' "$repo/fake.log" | head -1)
  want=$(printf -- '-S\x1f-o\x1fx86_64/alpha.s\x1fsource/alpha.c\x1f')
  got=${vline#*$'\t'argv=}
  [ "$got" = "$want" ] || { bad "$name" "verify argv: got $(printf '%q' "$got")"; return; }

  # Relative in verify too, and compiled OUTSIDE the corpus. A verify that ran
  # in the fixture root would overwrite the very bytes it is comparing against,
  # so this assertion is about safety as much as about provenance.
  local vcwd; vcwd=${vline%%$'\t'argv=*}; vcwd=${vcwd#cwd=}
  case "$vcwd" in
    "$repo/asm/fixtures"*) bad "$name" "verify compiled inside the corpus: $vcwd"; return ;;
    "") bad "$name" "verify recorded no cwd"; return ;;
  esac
  ok "$name"
}

echo "fixture modes (fake compilers):"
# `|| true` on each: without it `set -e` kills the runner at the first failing
# test and hides every one after it.
t_clean || true
t_compile_fail || true
t_gate || true
t_two_cases || true
t_version_fail || true
t_scope_change || true
t_verify || true
t_usage || true
t_invocation || true
printf 'fixture-modes: %d ok, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
