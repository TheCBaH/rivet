#!/usr/bin/env bash
# Execute every RISC-V CompCert fixture through the explicitly named qemu-user
# emulator.  The fixture .text/.data remain at the controlled oracle addresses;
# a tiny freestanding _start lives in a separate section and converts the
# fixture's return value into exit_group status.
set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd -- "$SCRIPT_DIR/.." && pwd)
CORPUS_ROOT="$REPO_ROOT/asm/fixtures/compcert-3.17"
ARTIFACT_ROOT="${RISC_EXEC_ARTIFACTS:-$REPO_ROOT/.riscv-exec-artifacts}"

# shellcheck source=target-matrix.sh
. "$SCRIPT_DIR/target-matrix.sh"
Fatal() { echo "FATAL: $*" >&2; exit 1; }
usage() { echo "Usage: $0 <riscv32|riscv64|all> [case ...]" >&2; }

all_cases() {
  (cd "$CORPUS_ROOT" && find . -mindepth 2 -maxdepth 2 -type d -name source \
    | sed -e 's|^./||' -e 's|/source$||' | LC_ALL=C sort)
}

stem_for() {
  local manifest="$CORPUS_ROOT/$1/manifest.txt" source=""
  [ -f "$manifest" ] && source=$(awk -F'\t' '$1 == "source" { print $2; exit }' "$manifest")
  [ -n "$source" ] || source="source/$1.c"
  basename "$source" .c
}

hex_dump() {
  od -An -v -tx1 "$1" | tr -s ' ' | sed -e 's/^ //' -e 's/ $//' | awk 'NF'
}

run_one() {
  local target=$1 case_name=$2 stem
  target_config "$target"
  stem=$(stem_for "$case_name")
  local fixture="$CORPUS_ROOT/$case_name/$target/$stem.s"
  local oracle="$CORPUS_ROOT/$case_name/$target/oracle/linked"
  [ -f "$fixture" ] || Fatal "missing fixture $fixture"
  command -v "$QEMU_BIN" >/dev/null 2>&1 || Fatal "$QEMU_BIN is required"
  command -v "${TOOLPREFIX}as" >/dev/null 2>&1 || Fatal "${TOOLPREFIX}as is required"

  local work start_addr
  work="$ARTIFACT_ROOT/$target/$case_name"
  rm -rf "$work"
  mkdir -p "$work"
  start_addr=$(printf '0x%x' $((LINK_TEXT_ADDR - 0x10000)))

  cat > "$work/start.s" <<'ASM'
        .option nopic
        .option norelax
        .section .text.startup,"ax",@progbits
        .balign 4
        .globl _start
_start:
        call asm_test_entry
        li a7, 94
        ecall
ASM
  cat > "$work/link.ld" <<LD
ENTRY(_start)
SECTIONS
{
  . = $start_addr;
  .start : { start.o(.text.startup) }
  . = $LINK_TEXT_ADDR;
  .text : { fixture.o(.text) }
  . = $LINK_RODATA_ADDR;
  .rodata : { fixture.o(.rodata .rodata.*) }
  . = $LINK_DATA_ADDR;
  .data : { fixture.o(.data) }
  /DISCARD/ : { *(.eh_frame) *(.note.GNU-stack) *(.comment) *(.riscv.attributes) }
}
LD

  "${TOOLPREFIX}as" "${AS_FLAGS[@]}" -o "$work/start.o" "$work/start.s"
  "${TOOLPREFIX}as" "${AS_FLAGS[@]}" -o "$work/fixture.o" "$fixture"
  (cd "$work" && "${TOOLPREFIX}ld" "${LD_FLAGS[@]}" -static -T link.ld \
    -o fixture.elf start.o fixture.o)

  local class machine
  class=$("${TOOLPREFIX}readelf" -h "$work/fixture.elf" | sed -n 's/^ *Class: *//p')
  machine=$("${TOOLPREFIX}readelf" -h "$work/fixture.elf" | sed -n 's/^ *Machine: *//p')
  "${TOOLPREFIX}readelf" -hSWsr "$work/fixture.elf" > "$work/readelf.txt"
  "${TOOLPREFIX}objdump" -M no-aliases,numeric -dr "$work/fixture.elf" > "$work/objdump.txt"
  [ "$class" = "$ELF_CLASS" ] || Fatal "$case_name/$target: expected $ELF_CLASS, got $class"
  [ "$machine" = "$READELF_MACHINE" ] || Fatal "$case_name/$target: expected $READELF_MACHINE, got $machine"

  # Prove that the bytes about to execute are the same controlled-link bytes
  # accepted by the GNU differential oracle.
  local section
  for section in text rodata data; do
    if [ -f "$oracle/$section.hex" ]; then
      "${TOOLPREFIX}objcopy" --only-section=".$section" -O binary \
        "$work/fixture.elf" "$work/$section.bin"
      hex_dump "$work/$section.bin" > "$work/$section.hex"
      cmp -s "$work/$section.hex" "$oracle/$section.hex" \
        || Fatal "$case_name/$target: executed .$section differs from controlled-link oracle"
    fi
  done

  echo "+ timeout 10s $QEMU_BIN $work/fixture.elf"
  local status=0
  timeout 10s "$QEMU_BIN" "$work/fixture.elf" >"$work/stdout" 2>"$work/stderr" || status=$?
  printf '%s\n' "$status" > "$work/exit-status.txt"
  if [ "$status" -eq 124 ]; then Fatal "$case_name/$target: QEMU timed out"; fi
  if [ "$status" -ne 42 ]; then
    cat "$work/stderr" >&2
    Fatal "$case_name/$target: expected exit status 42, got $status"
  fi
  echo "qemu: $case_name/$target: $class $machine, exit 42"
}

requested=${1:-}
[ -n "$requested" ] || { usage; exit 2; }
shift || true
case "$requested" in
  all) targets=(riscv32 riscv64) ;;
  riscv32|riscv64) targets=("$requested") ;;
  *) usage; Fatal "unknown execution profile '$requested'" ;;
esac

cases=("$@")
if [ "${#cases[@]}" -eq 0 ]; then mapfile -t cases < <(all_cases); fi
for target in "${targets[@]}"; do
  for case_name in "${cases[@]}"; do
    [[ "$case_name" =~ ^[A-Za-z0-9_-]+$ ]] || Fatal "invalid fixture name '$case_name'"
    [ -d "$CORPUS_ROOT/$case_name/source" ] || Fatal "unknown fixture '$case_name'"
    run_one "$target" "$case_name"
  done
done
