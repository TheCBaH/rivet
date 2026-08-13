# Execution ABI v2

ABI v2 is an additive successor to the frozen ABI v1. Unless this document
overrides a rule, every v1 field offset, size, validation stage, result state,
error class, and subcode retains its exact value and meaning.

The manifest and result magic values end in byte `2`, and `abi_version` is 2.
Profile IDs 1–4 retain their v1 meanings. ID 5 is `riscv32`; ID 6 is
`riscv64`.

RISC-V entries are four-byte aligned. Guest stacks are 16-byte aligned. The
return value is `x10`/`a0`, interpreted by sign-extending its low 32 bits.
Helpers check `x8`, `x9`, and `x18`–`x27` as callee-saved integer registers.

After copying executable bytes and before publishing the running commit, a
RISC-V helper invokes Linux `riscv_flush_icache` over the executable range with
flags zero. A failed syscall reports cache-sync class 67, subcode 3
`riscv_flush_icache`. A local `fence.i` is not a substitute because a process
may resume on another hart.

The normative address windows remain word-size based: `0x30000000` for RV32
and `0x40000000` for RV64, with the unchanged v1 section/result/guard/stack
offsets.

Implementation status: IDs 1–4 are dual-built from the shared helper sources
and run through the complete conformance corpus in both v1 and v2 modes. The
RV32/RV64 helper sources, cache-flush syscall path, and six-profile v2
conformance run are not implemented yet; the RISC-V assembler fixture execution
gate is independent and runs controlled linked images directly under QEMU.
