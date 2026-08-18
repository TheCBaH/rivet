#!/usr/bin/env bash
# Record the observable behavior of the toolchain-free shell entry points, so
# the OCaml replacements in Phase 2 can be compared against something written
# down rather than against memory.
#
# Only the toolchain-free scenarios are here. --regen and --verify need six
# installed CompCert cross compilers, so they are characterized in Phase 3
# against fake compilers instead; recording them here would produce snapshots
# that only reproduce on a machine that has run `make asm-cross-setup`.
#
# {1 Why a detached worktree}
#
# FIXTURE_WORK and CROSS_SMOKE_WORK redirect *build* roots, not corpora: all
# four fixture scripts spell $REPO_ROOT/asm/fixtures/... directly. But every
# script derives REPO_ROOT from BASH_SOURCE, so a detached worktree gives a
# disposable corpus with no new public interface and no risk to the working
# checkout - which matters, because several scenarios below deliberately mutate
# the corpus and one of them (--rehash after adding a file) INSTALLS a rewritten
# manifest.
#
# {2 The three comparison classes}
#
#   exact        compared byte for byte: exit-status, argv, manifests, tree.tsv,
#                artifact-hashes.tsv
#   placeholder  compared after substitution: stdout, stderr, argv. Absolute
#                paths and diff mtimes vary per run and per checkout; the $0
#                spelling does NOT - see normalize()
#   structural   compared field-shape only: diff-headers.txt. `diff -u` headers
#                carry absolute paths AND mtimes, so they can never be exact -
#                which is also why the OCaml port keeps invoking external diff
#                rather than formatting its own (decision P1)
#
# Usage:
#   characterize.sh record    write/refresh the snapshots
#   characterize.sh verify    re-record into a temporary tree and compare
set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)
SNAP_ROOT="$REPO_ROOT/asm/tools/test/data/characterization"

MODE=${1:-record}
case "$MODE" in
  record|verify) ;;
  *) echo "Usage: $0 record | verify" >&2; exit 2 ;;
esac

SCRATCH=$(mktemp -d)
trap 'rm -rf -- "$SCRATCH"' EXIT

if [ "$MODE" = verify ]; then
  OUT_ROOT="$SCRATCH/snapshots"
else
  OUT_ROOT="$SNAP_ROOT"
fi
mkdir -p "$OUT_ROOT"

# {3 The disposable checkout}
#
# HEAD, not the working tree: a snapshot of uncommitted edits would compare
# against whatever happened to be unsaved when it was taken.
REF="$SCRATCH/ref"
git -C "$REPO_ROOT" worktree add --detach "$REF" HEAD >/dev/null 2>&1
cleanup_worktree() {
  git -C "$REPO_ROOT" worktree remove --force "$REF" >/dev/null 2>&1 || true
  rm -rf -- "$SCRATCH"
}
trap cleanup_worktree EXIT

# {3a What `verify` proves after the Phase 3 cutover}
#
# The snapshots under $SNAP_ROOT were RECORDED FROM THE SHELL implementation.
# tools/asm-fixture-gen.sh is a launcher now, so replaying them exercises the
# OCaml program instead - and requiring every recorded byte to reproduce is
# exactly the equivalence gate, in the strongest form available: reference bytes
# committed before the port existed, reproduced by the port.
#
# That only works if the launcher can find an executable inside the worktree,
# which is a fresh checkout with no _build. Staging the already-built one is a
# harness detail and deliberately NOT an environment override in the launcher:
# the launcher's only way to locate the executable stays the one D3 documents,
# so the "not built" contract keeps its single code path.
#
# Absent, this fails loudly rather than recording the hint as though it were the
# implementation's output - a snapshot of "FATAL: compcert-tools not built"
# would reproduce perfectly and prove nothing.
EXE_REL="asm/_build/default/tools/bin/compcert_tools.exe"
[ -x "$REPO_ROOT/$EXE_REL" ] || {
  echo "FATAL: compcert-tools not built - run 'make tools-build'" >&2
  exit 1
}
mkdir -p "$REF/$(dirname "$EXE_REL")"
cp "$REPO_ROOT/$EXE_REL" "$REF/$EXE_REL"

# {4 Placeholder substitution}
#
# Longest paths first: $REF contains $SCRATCH as a prefix, so substituting
# %%SCRATCH%% first would leave "%%SCRATCH%%/ref" and never produce %%REPO%%.
# The order is therefore a correctness requirement, not a tidiness one.
#
# The mtime rule is not a path rule and is easy to miss. `diff -u` writes
# `--- <path>\t<timestamp>` into the *stream*, so a scenario whose stderr
# carries a diff body can never be exact no matter how paths are handled. The
# structural element is stripped here rather than the whole stream being demoted
# to structural, because everything else in that stderr - the CHANGED notice and
# the hunk bodies - is exactly comparable and worth comparing.
# There is deliberately NO %%PROG%% placeholder, though the plan called for one.
# Substituting $0 destroys the distinction these snapshots exist to record: with
# it, `tools/asm-fixture-gen.sh` and `./tools/asm-fixture-gen.sh` normalize to
# identical bytes, so the two scenarios that prove usage() interpolates $0 would
# prove nothing. It was also inconsistent - the absolute spelling never produced
# %%PROG%% at all, because the $REF substitution above had already rewritten it.
#
# Path substitution alone gives all three spellings distinct, reproducible
# forms: `tools/...`, `./tools/...` and `%%REPO%%/tools/...`.
normalize() {
  sed -e "s|$REF|%%REPO%%|g" \
      -e "s|$SCRATCH|%%SCRATCH%%|g" \
      -e "s|${TMPDIR:-/tmp}|%%TMPDIR%%|g" \
      -e "s|/tmp|%%TMPDIR%%|g" \
      -e 's@^\(---\|+++\) \([^	]*\)	.*$@\1 \2	%%MTIME%%@'
}

# {5 Recording one scenario}
#
# argv is recorded exactly, including the $0 spelling, because the legacy usage
# text interpolates $0: `tools/asm-fixture-gen.sh`, `./tools/asm-fixture-gen.sh`
# and an absolute path produce different bytes, and the Phase 2 launcher has to
# reproduce whichever one the caller used (see the --legacy-prog mechanism).
record_case() {
  local name=$1 cwd=$2; shift 2
  local dir="$OUT_ROOT/$name"
  rm -rf -- "$dir"
  mkdir -p "$dir"

  # argv is normalized too, not recorded raw: the absolute-$0 scenario embeds
  # this run's worktree path, which would differ every run. Normalizing keeps
  # the distinction that actually matters - `tools/x.sh` vs `./tools/x.sh` vs
  # `%%REPO%%/tools/x.sh` - which is precisely what usage() interpolates.
  printf '%s\n' "$@" | normalize > "$dir/argv"

  local rc=0
  ( cd "$cwd" && "$@" ) > "$SCRATCH/out" 2> "$SCRATCH/err" || rc=$?
  printf '%d\n' "$rc" > "$dir/exit-status"

  normalize < "$SCRATCH/out" > "$dir/stdout"
  normalize < "$SCRATCH/err" > "$dir/stderr"
}

# The corpus shape after a scenario ran. Mode is included because Phase 1's
# staging layer must preserve an existing manifest's mode on replacement (D13),
# and a snapshot that dropped mode could not witness a regression there.
record_tree() {
  local name=$1 root=$2
  # -mindepth 1 drops the root entry, whose %P is empty and which would
  # therefore record a line with trailing whitespace. It carries no information
  # anyway: it is always the corpus root itself.
  ( cd "$root" && find . -mindepth 1 -printf '%y %m %P\n' | LC_ALL=C sort ) \
    > "$OUT_ROOT/$name/tree.tsv"
  ( cd "$root" && find . -type f -exec sha256sum {} + | sed 's|  \./|\t|' | LC_ALL=C sort ) \
    > "$OUT_ROOT/$name/artifact-hashes.tsv"
}

# `diff -u` headers carry absolute paths and mtimes. Only the FIELD SHAPE is
# comparable: which marker, how many tab-separated fields, and whether a
# timestamp is present at all.
record_diff_headers() {
  local name=$1
  awk '/^(---|\+\+\+) / { n = split($0, f, "\t"); printf "%s fields=%d\n", $1, n }' \
    "$OUT_ROOT/$name/stdout" "$OUT_ROOT/$name/stderr" \
    > "$OUT_ROOT/$name/diff-headers.txt"
}

# The native executable, invoked with native argv. Until Phase 8 these went
# through tools/asm-fixture-gen.sh and tools/asm-gas-xref.sh; both are gone, and
# the recorded SNAPSHOTS are unaffected because none of the surviving scenarios
# puts $0 in its output. That is exactly why the six that did - the three $0
# spellings, the two verify-usage cases and the gas-xref usage case - retired
# with the shim rather than being re-recorded: re-recording them would have
# compared the OCaml against itself.
FIXTURE_GEN="$EXE_REL"
GAS_XREF="$EXE_REL"
CORPUS="asm/fixtures/compcert-3.17"

# {6 The scenarios}

# --- clean paths -----------------------------------------------------------

record_case fixture-check-all "$REF" "./$FIXTURE_GEN" fixture check
record_tree fixture-check-all "$REF/$CORPUS"

record_case fixture-check-one-case "$REF" "./$FIXTURE_GEN" fixture check -- return42

# P4: streaming validation. return42's success line is printed FIRST, then the
# run fails on the unknown case. An implementation that validated the whole case
# list up front would print the failure alone, which is a different transcript
# even though the exit status matches.
record_case fixture-check-unknown-case "$REF" "./$FIXTURE_GEN" fixture check -- return42 does_not_exist

# --- findings --------------------------------------------------------------
#
# Each mutates the disposable corpus and is reverted afterwards, so the
# scenarios stay independent of each other's order.

mutate_and_record() {
  local name=$1 prepare=$2
  ( cd "$REF" && eval "$prepare" )
  record_case "$name" "$REF" "./$FIXTURE_GEN" fixture check -- return42
  record_tree "$name" "$REF/$CORPUS"
  git -C "$REF" checkout -- "$CORPUS"
  git -C "$REF" clean -qfd -- "$CORPUS"
}

mutate_and_record fixture-check-changed \
  "printf 'tampered\n' >> $CORPUS/return42/x86_64/asm_test_entry.s"
mutate_and_record fixture-check-missing \
  "rm $CORPUS/return42/x86_64/asm_test_entry.s"
mutate_and_record fixture-check-unrecorded \
  "printf 'stray\n' > $CORPUS/return42/x86_64/stray.s"

# D1: `grep -q "^sha256:$f\t"` interpolates the path as a REGEX, so a file whose
# name contains a regex metacharacter is matched loosely. This records what the
# shell actually does today; the OCaml port uses exact set membership and is
# expected to differ here.
mutate_and_record fixture-check-regex-metachar-path \
  "printf 'stray\n' > '$CORPUS/return42/x86_64/a.b.s'"

# --- rehash ----------------------------------------------------------------
#
# P6/D9: with a manifest already present and nothing changed, --rehash rewrites
# it identically and reports "manifest up to date", exit 0.
record_case fixture-rehash-unchanged "$REF" "./$FIXTURE_GEN" fixture rehash -- return42
record_tree fixture-rehash-unchanged "$REF/$CORPUS"
git -C "$REF" checkout -- "$CORPUS"

# The most important behavior in the file: a scope change is INSTALLED and THEN
# reported as a failure, which is how it is forced through review. The snapshot
# must therefore capture both the exit status and the rewritten manifest.
( cd "$REF" && printf 'stray\n' > "$CORPUS/return42/x86_64/stray.s" )
record_case fixture-rehash-changed "$REF" "./$FIXTURE_GEN" fixture rehash -- return42
record_tree fixture-rehash-changed "$REF/$CORPUS"
record_diff_headers fixture-rehash-changed
cp "$REF/$CORPUS/return42/manifest.txt" "$OUT_ROOT/fixture-rehash-changed/manifest.txt"
git -C "$REF" checkout -- "$CORPUS"
git -C "$REF" clean -qfd -- "$CORPUS"

# --- gas-xref --------------------------------------------------------------

record_case gas-xref-check "$REF" "./$GAS_XREF" gas-xref check
record_tree gas-xref-check "$REF/asm/fixtures/gas-xref"

# {7 Verification}

if [ "$MODE" = record ]; then
  n=$(find "$OUT_ROOT" -mindepth 1 -maxdepth 1 -type d | wc -l)
  printf 'characterize: recorded %d scenarios under %s\n' "$n" "${SNAP_ROOT#"$REPO_ROOT"/}"
  exit 0
fi

# verify: every recorded file must reproduce. Files are compared in full; the
# placeholder substitution has already been applied to both sides, and the
# structural files were already reduced to field shape when written, so a plain
# comparison is the right check for all three classes at this point.
missing=0
differing=0
compared=0
while IFS= read -r rel; do
  compared=$((compared + 1))
  if [ ! -f "$SNAP_ROOT/$rel" ]; then
    printf '  MISSING %s\n' "$rel" >&2; missing=$((missing + 1)); continue
  fi
  if ! cmp -s "$OUT_ROOT/$rel" "$SNAP_ROOT/$rel"; then
    printf '  DIFFERS %s\n' "$rel" >&2
    diff -u "$SNAP_ROOT/$rel" "$OUT_ROOT/$rel" | sed 's/^/      /' >&2 || true
    differing=$((differing + 1))
  fi
done < <(cd "$OUT_ROOT" && find . -type f | sed 's|^\./||' | LC_ALL=C sort)

# A snapshot present in the tree but not produced by this run is just as much a
# divergence as a changed one - it would otherwise linger as a stale expectation
# that nothing ever checks. This is the same UNRECORDED rule the corpus checks
# use, applied to the snapshots themselves.
stale=0
while IFS= read -r rel; do
  if [ ! -f "$OUT_ROOT/$rel" ]; then
    printf '  STALE   %s\n' "$rel" >&2; stale=$((stale + 1))
  fi
done < <(cd "$SNAP_ROOT" && find . -type f | sed 's|^\./||' | LC_ALL=C sort)

printf 'characterize: %d files compared, %d differing, %d missing, %d stale\n' \
  "$compared" "$differing" "$missing" "$stale"
[ "$((differing + missing + stale))" -eq 0 ]
