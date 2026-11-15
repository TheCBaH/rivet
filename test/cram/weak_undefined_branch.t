A branch to an undefined weak symbol binds to address 0 and is range-checked
like any other PC-relative fixup. GNU ld agrees on x86 and for the RISC-V `jal`,
and differs on AArch64 and ARM (docs/contracts.md 5.8); this pins today's
behaviour.

  $ asm() { ../../tool/asm.exe "$@"; }
  $ weak() { printf '\t.text\n\t.globl _start\n\t.weak foo\n_start:\n%b\t%s\n' "$1" "${2:-ret}" > w.s; }

x86: the same bytes as ld produces at 0x400000, an error beyond 2 GiB.

  $ weak '\tcall foo\n\tjmp foo\n'
  $ asm --target x86_64 --fixed-base 0x400000 --dump-bytes w.s
  e8 fb ff bf ff e9 f6 ff bf ff c3
  $ weak '\tcall foo\n'
  $ asm --target x86_64 --fixed-base 0x80000000 w.s
  <synthesized by image>: error[x86.fixup]: displacement does not fit 32 bits
  [1]

AArch64 and ARM: ld writes a no-op; this assembler encodes the branch to 0.

  $ weak '\tbl foo\n'
  $ asm --target aarch64 --fixed-base 0x40000000 w.s
  <synthesized by image>: error[aarch64.fixup]: branch target is out of range
  [1]
  $ weak '\tbl foo\n' 'bx lr'
  $ asm --target arm --fixed-base 0x400000 --dump-bytes w.s
  fe ff ef eb 1e ff 2f e1

RISC-V: the jal is range-checked as in ld, the call reaches 0 by auipc/jalr.

  $ weak '\tj foo\n'
  $ asm --target riscv64 --fixed-base 0x10000 --dump-bytes w.s
  6f 00 0f 80 67 80 00 00
  $ asm --target riscv64 --fixed-base 0x400000 w.s
  <synthesized by image>: error[riscv64.fixup]: jal immediate is out of range
  [1]
  $ weak '\tcall foo\n'
  $ asm --target riscv64 --fixed-base 0x400000 --dump-bytes w.s
  97 00 c0 ff e7 80 00 00 67 80 00 00
