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

The RV32/RV64 transport is one XLEN-conditional freestanding source. It is
compiled with the matrix's explicit `-march`/`-mabi`/`-mno-relax` flags and
linked directly with GNU `ld`: no libc, start files, compiler runtime, or
dynamic loader is present. It uses direct Linux syscalls throughout. `statx`
is used to obtain the manifest size because the new RV32 Linux syscall ABI does
not expose the legacy `fstat` entry.

The RISC-V guest-call boundary saves and checks all twelve integer callee-saved
registers, publishes release-ordered state transitions, captures `a0` and the
returned SP before restoring helper state, and sign-extends the low 32 return
bits on both XLENs. Every executable segment is passed through
`riscv_flush_icache` before the validated/running transition.

Implementation status: complete. IDs 1–4 are dual-built and run through the
complete conformance corpus in both v1 and v2 modes. IDs 5–6 are built in v2
mode only. The full v2 run covers all six profiles, including return-42/41,
GNU-verified RISC-V illegal-instruction bytes, timeout, stack boundaries and
corruption, 16-byte SP alignment, and callee-saved corruption.
