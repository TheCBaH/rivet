#!/usr/bin/env bash
# The behavioral tool gate (.ai/asm_plan.md §3.2, an M0 exit criterion).
#
# APT tooling is range-checked rather than digest-pinned: devcontainer-lock.json
# pins feature references, not the mutable debian:13 base. Version numbers alone
# do not establish compatibility where behaviour matters, so this gate asks the
# tools to *do* the things the project depends on, in a freshly built container,
# and records what answered.
#
# Five checks, and the boundaries between them are the point:
#
#   1. Each of the four qemu-user runners executes a self-contained reference
#      ELF that exits through a direct exit_group syscall. NO dependency on
#      ABI v1, on the transport helpers, or on the OCaml runner - none of which
#      existed when this gate was specified, and none of which should be able to
#      make it pass. This establishes E2 tool compatibility ONLY; M0.3's
#      return/fault/timeout suite is the distinct E4 transport proof and lives
#      in make asm-abi-conform.
#
#   2. A minimal helper is assembled *and linked* with every cross toolchain.
#      Check 1 already does both, so this is check 1's linking half stated as
#      its own claim rather than as a side effect - a gate that only assembled
#      would pass with a broken ld.
#
#   3. The selected qemu-system machine aliases are enumerated and asserted
#      present, so a retired *-10.0 alias fails loudly here instead of much
#      later in whatever first needs it.
#
#   4. The GDB batch/RSP handshake completes against a paused qemu-system.
#
#   5. Exact versions are recorded in provenance regardless of outcome, so a
#      base-image update that moved a tool under us is visible in the artifact.
#
# Usage: tools/asm-tool-gate.sh [all|user|system|gdb]
set -euo pipefail
cd "$(dirname "$0")/.."
REPO_ROOT="$PWD"
. tools/target-matrix.sh

WORK="${ASM_TOOL_GATE_WORK:-$REPO_ROOT/.tool-gate}"
PROVENANCE="$WORK/provenance.txt"
failures=0

note() { printf '  %-34s %s\n' "$1" "$2"; }
fail() { printf '  FAIL %-29s %s\n' "$1" "$2"; failures=$((failures + 1)); }
record() { printf '%s\t%s\n' "$1" "$2" >>"$PROVENANCE"; }

rm -rf "$WORK"
mkdir -p "$WORK"
: >"$PROVENANCE"

# {1} The reference snippet, per target.
#
# Each exits with a *different* status, so a mixed-up binary - the wrong
# emulator, or a stale artifact from another target - fails rather than passing
# on a shared value. The syscall is issued directly: there is no libc here, no
# crt startup, and no dynamic loader, which is what makes this a test of the
# emulator rather than of a sysroot.
snippet_for() {
  case "$1" in
    x86_32)  cat <<'EOF'
	.text
	.globl _start
_start:
	movl	$252, %eax	/* exit_group */
	movl	$11, %ebx
	int	$0x80
EOF
;;
    x86_64)  cat <<'EOF'
	.text
	.globl _start
_start:
	movq	$231, %rax	/* exit_group */
	movq	$12, %rdi
	syscall
EOF
;;
    arm)     cat <<'EOF'
	.syntax unified
	.arch armv7-a
	.arm
	.text
	.globl _start
_start:
	mov	r7, #248	@ exit_group
	mov	r0, #13
	svc	#0
EOF
;;
    aarch64) cat <<'EOF'
	.text
	.globl _start
_start:
	mov	x8, #94		// exit_group
	mov	x0, #14
	svc	#0
EOF
;;
  esac
}

expected_status_for() {
  case "$1" in
    x86_32) echo 11 ;; x86_64) echo 12 ;; arm) echo 13 ;; aarch64) echo 14 ;;
  esac
}

user_gate() {
  echo "== qemu-user: reference snippet, direct exit_group, no ABI v1 =="
  for t in "${ALL_TARGETS[@]}"; do
    target_config "$t"
    local dir="$WORK/$t"
    mkdir -p "$dir"
    snippet_for "$t" >"$dir/ref.s"

    if ! command -v "${TOOLPREFIX}as" >/dev/null; then
      fail "$t" "no ${TOOLPREFIX}as"
      continue
    fi
    if ! "${TOOLPREFIX}as" -o "$dir/ref.o" "$dir/ref.s" 2>"$dir/as.log"; then
      fail "$t assemble" "$(tr '\n' ' ' <"$dir/as.log")"
      continue
    fi
    # Static, no libc, explicit entry: linking is a separate failure mode from
    # assembling and gets its own message.
    if ! "${TOOLPREFIX}ld" -static -e _start -o "$dir/ref" "$dir/ref.o" 2>"$dir/ld.log"; then
      fail "$t link" "$(tr '\n' ' ' <"$dir/ld.log")"
      continue
    fi
    record "$t.as" "$("${TOOLPREFIX}as" --version | head -1)"
    record "$t.ld" "$("${TOOLPREFIX}ld" --version | head -1)"

    if ! command -v "$QEMU_BIN" >/dev/null; then
      fail "$t" "no $QEMU_BIN"
      continue
    fi
    record "$t.qemu" "$("$QEMU_BIN" --version | head -1)"

    local want status=0
    want="$(expected_status_for "$t")"
    timeout 10s "$QEMU_BIN" "$dir/ref" >/dev/null 2>&1 || status=$?
    if [ "$status" -eq 124 ]; then
      fail "$t run" "timed out after 10s"
    elif [ "$status" -ne "$want" ]; then
      fail "$t run" "exit status $status, expected $want"
    else
      note "$t" "assembled, linked, exit $status under $QEMU_BIN"
    fi
  done
}

# {3} Machine aliases. Asserting the *alias* rather than the versioned name is
# deliberate: the alias is what a command line would name, and it is the alias
# that disappears when a machine is retired.
SYSTEM_MACHINES=(
  "qemu-system-x86_64:q35"
  "qemu-system-x86_64:pc"
  "qemu-system-arm:virt"
  "qemu-system-aarch64:virt"
)

system_gate() {
  echo "== qemu-system: machine aliases present =="
  for entry in "${SYSTEM_MACHINES[@]}"; do
    local bin="${entry%%:*}" machine="${entry##*:}"
    if ! command -v "$bin" >/dev/null; then
      fail "$bin" "not installed"
      continue
    fi
    record "$bin" "$("$bin" --version | head -1)"
    local line
    line="$("$bin" -M help 2>/dev/null | awk -v m="$machine" '$1 == m')"
    if [ -z "$line" ]; then
      fail "$bin -M $machine" "alias absent from -M help"
    else
      note "$bin -M $machine" "$(echo "$line" | sed 's/  */ /g')"
    fi
  done
}

# {4} The GDB handshake. A paused qemu-system with -S -gdb tcp::PORT, then a
# batch GDB that connects, reads a register, and detaches. What is being checked
# is that the two agree on the RSP dialect at all - not that any guest code
# runs, which is why the machine boots nothing.
gdb_gate() {
  echo "== gdb: batch/RSP handshake against a paused qemu-system =="
  if ! command -v gdb >/dev/null; then
    fail "gdb" "not installed"
    return
  fi
  record "gdb" "$(gdb --version | head -1)"

  local port=${ASM_TOOL_GATE_PORT:-14730}
  local log="$WORK/gdb.log" qlog="$WORK/qemu-system.log"
  qemu-system-aarch64 -M virt -cpu cortex-a57 -m 128 -nographic -S \
    -gdb "tcp::$port" -monitor none -serial none >"$qlog" 2>&1 &
  local qpid=$!
  # Wait for the stub to accept, rather than sleeping a fixed interval: a fixed
  # sleep is either flaky or slow, and on a loaded CI runner it is both.
  local ready=0
  for _ in $(seq 1 50); do
    if gdb --batch -ex "set confirm off" -ex "target remote :$port" -ex detach \
        >"$log" 2>&1; then
      ready=1
      break
    fi
    sleep 0.2
  done
  if [ "$ready" -eq 1 ]; then
    # Connected once; now do the real handshake and read state back, so the
    # check is "the two speak RSP" and not merely "a socket accepted".
    if gdb --batch -ex "set confirm off" -ex "target remote :$port" \
        -ex "info registers pc" -ex detach >"$log" 2>&1 \
        && grep -q '^pc' "$log"; then
      note "gdb <-> qemu-system-aarch64" "connected, read pc, detached"
    else
      fail "gdb handshake" "connected but could not read pc: $(tr '\n' ' ' <"$log" | tail -c 200)"
    fi
  else
    fail "gdb handshake" "no connection to :$port after 10s"
  fi
  kill "$qpid" 2>/dev/null || true
  wait "$qpid" 2>/dev/null || true
}

case "${1:-all}" in
  all)    user_gate; system_gate; gdb_gate ;;
  user)   user_gate ;;
  system) system_gate ;;
  gdb)    gdb_gate ;;
  *)      echo "usage: $0 [all|user|system|gdb]" >&2; exit 2 ;;
esac

echo "== provenance ($PROVENANCE) =="
sed 's/^/  /' "$PROVENANCE"

if [ "$failures" -eq 0 ]; then
  echo "tool-gate: ok"
else
  echo "tool-gate: $failures failure(s)"
  exit 1
fi
