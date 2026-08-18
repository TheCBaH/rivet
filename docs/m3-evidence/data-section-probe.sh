#!/bin/bash
# Supplementary dimension-2 probe: non-executable PROGBITS (.data) merge gap.
set -u
cd "$(dirname "$0")"
cat > d2data_a.s << 'EOF'
	.data
	.byte 0x11, 0x12, 0x13, 0x14
EOF
cat > d2data_b.s << 'EOF'
	.data
	.balign 16
	.byte 0x22
EOF
cat > script_data.ld << 'EOF'
SECTIONS
{
  . = 0x10000;
  .data : { *(.data) }
}
EOF
for t in x86_64-linux-gnu i686-linux-gnu arm-linux-gnueabihf aarch64-linux-gnu; do
  echo "=== $t .data (non-executable PROGBITS) merge gap ==="
  ${t}-as -o d2data_a.o d2data_a.s
  ${t}-as -o d2data_b.o d2data_b.s
  ${t}-ld -static -T script_data.ld d2data_a.o d2data_b.o -o d2data_out.elf
  ${t}-objcopy -O binary --only-section=.data d2data_out.elf d2data_out.bin
  od -An -tx1 -v d2data_out.bin
done
echo "=== riscv64/riscv32 .data ==="
for m in "riscv64:-march=rv64imafd -mabi=lp64d -mno-relax:-m elf64lriscv --no-relax" \
         "riscv32:-march=rv32imafd -mabi=ilp32d -mno-relax:-m elf32lriscv --no-relax"; do
  IFS=: read -r label as_extra ld_extra <<< "$m"
  echo "-- $label --"
  riscv64-linux-gnu-as $as_extra -o d2data_a.o d2data_a.s
  riscv64-linux-gnu-as $as_extra -o d2data_b.o d2data_b.s
  riscv64-linux-gnu-ld $ld_extra -static -T script_data.ld d2data_a.o d2data_b.o -o d2data_out.elf
  riscv64-linux-gnu-objcopy -O binary --only-section=.data d2data_out.elf d2data_out.bin
  od -An -tx1 -v d2data_out.bin
done
