#!/usr/bin/env bash
# What GNU ld does with a branch to an undefined weak symbol, per target (the table in
# docs/contracts.md section 5.8). Needs the cross binutils; prints the linked disassembly.
set -u
cd "$(mktemp -d)" || exit 2
probe() { # target prefix as-flags ld-flags text-base source
	printf '%b' "$6" >t.s
	"$2"as $3 -o t.o t.s || return
	echo "--- $1, text at $5"
	"$2"ld $4 -Ttext="$5" -o t.elf t.o 2>&1 | head -2
	[ -f t.elf ] && "$2"objdump -d t.elf | sed -n '/<_start>:/,$p' | head -6
	rm -f t.elf
}
body='\t.text\n\t.globl _start\n\t.weak foo\n_start:\n'
R="-march=rv64imafd -mabi=lp64d -mno-relax"
probe x86_64 x86_64-linux-gnu- "" "" 0x400000 "${body}\tcall foo\n\tjmp foo\n\tret\n"
probe x86_64 x86_64-linux-gnu- "" "" 0x80000000 "${body}\tcall foo\n\tret\n"
probe aarch64 aarch64-linux-gnu- "" "" 0x40000000 "${body}\tbl foo\n\tb foo\n\tcbz x0, foo\n\tb.eq foo\n\tret\n"
probe arm arm-linux-gnueabihf- "-march=armv7-a" "" 0x400000 "${body}\tbl foo\n\tbeq foo\n\tbx lr\n"
probe riscv64 riscv64-linux-gnu- "$R" "--no-relax" 0x400000 "${body}\tcall foo\n\tret\n"
probe riscv64 riscv64-linux-gnu- "$R" "--no-relax" 0x10000 "${body}\tj foo\n\tret\n"
probe riscv64 riscv64-linux-gnu- "$R" "--no-relax" 0x400000 "${body}\tj foo\n\tret\n"
