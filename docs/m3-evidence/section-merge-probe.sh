#!/bin/bash
# M3 §3 GNU merge-behavior probe. Real as/ld measurement, all six targets.
set -u
cd "$(dirname "$0")"

run_target() {
  local label="$1" prefix="$2" as_extra="$3" ld_extra="$4" comment_sigil="$5"
  echo "=================================================================="
  echo "TARGET: $label   (prefix=$prefix)"
  echo "=================================================================="

  # --- Dim 1+2: offset + fill for a merge-boundary gap ---
  cat > d12_a.s << EOF
	.text
	.byte 0x11, 0x12, 0x13, 0x14
EOF
  cat > d12_b.s << EOF
	.text
	.balign 16
	.byte 0x22
EOF
  ${prefix}-as $as_extra -o d12_a.o d12_a.s 2>&1
  ${prefix}-as $as_extra -o d12_b.o d12_b.s 2>&1
  echo "-- d12_a.o .text (own size/align, pre-link) --"
  ${prefix}-readelf -S d12_a.o 2>&1 | grep -A1 " \.text "
  echo "-- d12_b.o .text (own size/align, pre-link) --"
  ${prefix}-readelf -S d12_b.o 2>&1 | grep -A1 " \.text "
  ${prefix}-ld $ld_extra -static -T script.ld d12_a.o d12_b.o -o d12_out.elf 2>&1
  ${prefix}-objcopy -O binary --only-section=.text d12_out.elf d12_out.bin 2>&1
  echo "-- linked .text bytes --"
  od -An -tx1 -v d12_out.bin

  # --- Dim 3: same name, different kind (.mix progbits vs nobits) ---
  cat > d3_a.s << EOF
	.section .mix,"aw",${comment_sigil}progbits
	.byte 0xaa, 0xbb, 0xcc, 0xdd
EOF
  cat > d3_b.s << EOF
	.section .mix,"aw",${comment_sigil}nobits
	.skip 4
EOF
  ${prefix}-as $as_extra -o d3_a.o d3_a.s 2>&1
  ${prefix}-as $as_extra -o d3_b.o d3_b.s 2>&1
  ${prefix}-ld $ld_extra -static -T script_mix.ld d3_a.o d3_b.o -o d3_out.elf 2>&1
  echo "-- linked section headers (looking for .mix) --"
  ${prefix}-readelf -S d3_out.elf 2>&1 | grep -B1 -A1 "\.mix"

  # --- Dim 4: flag mismatch (read-only vs read-write, same name) ---
  cat > d4_a.s << EOF
	.section .flagtest,"a",${comment_sigil}progbits
	.byte 0x01, 0x02, 0x03, 0x04
EOF
  cat > d4_b.s << EOF
	.section .flagtest,"aw",${comment_sigil}progbits
	.byte 0x05, 0x06, 0x07, 0x08
EOF
  ${prefix}-as $as_extra -o d4_a.o d4_a.s 2>&1
  ${prefix}-as $as_extra -o d4_b.o d4_b.s 2>&1
  echo "-- ld flag-mismatch link (a=r-only, b=rw) --"
  ${prefix}-ld $ld_extra -static -T script_flag.ld d4_a.o d4_b.o -o d4_out.elf 2>&1
  echo "exit=$?"
  ${prefix}-readelf -S d4_out.elf 2>&1 | grep -A1 "flagtest"
  echo
}

cat > script.ld << 'EOF'
SECTIONS
{
  . = 0x10000;
  .text : { *(.text) }
}
EOF
cat > script_mix.ld << 'EOF'
SECTIONS
{
  . = 0x10000;
  .mix : { *(.mix) }
}
EOF
cat > script_flag.ld << 'EOF'
SECTIONS
{
  . = 0x10000;
  .flagtest : { *(.flagtest) }
}
EOF

run_target "x86_64"  "x86_64-linux-gnu"        ""                                                    ""                "@"
run_target "x86_32"  "i686-linux-gnu"          ""                                                    ""                "@"
run_target "arm"     "arm-linux-gnueabihf"     ""                                                    ""                "%"
run_target "aarch64" "aarch64-linux-gnu"       ""                                                    ""                "%"
run_target "riscv64" "riscv64-linux-gnu"       "-march=rv64imafd -mabi=lp64d -mno-relax"             "-m elf64lriscv --no-relax" "@"
run_target "riscv32" "riscv64-linux-gnu"       "-march=rv32imafd -mabi=ilp32d -mno-relax"            "-m elf32lriscv --no-relax" "@"
