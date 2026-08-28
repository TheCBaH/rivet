# Measured CompCert RISC-V inventory

This inventory is the scope authority for the initial RISC-V implementation.
It was generated from CompCert 3.17 on 2026-08-13; proposed instruction lists
elsewhere do not widen this measured corpus.

## Profiles and tools

| Profile | CompCert target | Assembly flags | Link flags | ELF | QEMU |
|---|---|---|---|---|---|
| `riscv32` | `rv32-linux` | `-march=rv32imafd -mabi=ilp32d -mno-relax` | `-m elf32lriscv --no-relax` | ELF32, RISC-V | `qemu-riscv32` |
| `riscv64` | `rv64-linux` | `-march=rv64imafd -mabi=lp64d -mno-relax` | `-m elf64lriscv --no-relax` | ELF64, RISC-V | `qemu-riscv64` |

Both CompCert configurations use `-no-runtime-lib -no-standard-headers` and a
project-owned GCC probe wrapper. Fixture generation stops at `ccomp -S`.
RV32 uses the published crosstool-NG toolchain (GCC 14.2.0, Binutils 2.43.1)
and RV64 uses Debian's GNU Binutils 2.44; QEMU is 10.0.11 in the recorded
run. The exact tool banners are stored beside every fixture in
`oracle/tool-versions.txt`.

Controlled section addresses are `0x30000000` for `.text`, `0x30010000` for
`.rodata`, and `0x30020000` for `.data`. All GNU assembly and links disable
relaxation. Disassembly uses `objdump -M no-aliases,numeric`.

## Corpus

| Fixture | RV32 `.text` | RV64 `.text` | Measured purpose |
|---|---:|---:|---|
| `return42` | 32 bytes | 32 bytes | prologue, `addi`, load/store, return |
| `args_arith` | 128 bytes | 128 bytes | arguments, stack traffic, add/sub/multiply; RV64 W forms |
| `cond_select` | 108 bytes | 108 bytes | comparisons and forward branches |
| `loop` | 64 bytes | 64 bytes | forward and backward branches |
| `direct_call` | 76 bytes | 76 bytes | unrelaxed AUIPC/JALR call pair |
| `global_ldst` | 48 bytes | 48 bytes | PC-relative address pair and four-byte `.data` |

The numeric real-instruction union is:

- RV32: `add`, `addi`, `auipc`, `beq`, `bge`, `blt`, `jal`, `jalr`, `lw`,
  `mul`, `slli`, `sub`, `sw`.
- RV64: `addi`, `addiw`, `addw`, `auipc`, `beq`, `bge`, `blt`, `jal`,
  `jalr`, `ld`, `lw`, `mulw`, `sd`, `slliw`, `subw`, `sw`.

The source additionally measures the input pseudos `mv`, `j`, `jr`, and
`call`. The relocation union is `R_RISCV_CALL_PLT`, `R_RISCV_PCREL_HI20`,
`R_RISCV_PCREL_LO12_I`, and `R_RISCV_PCREL_LO12_S`. Same-section B/J branches
are resolved by the assembler and have no object relocation.

## Artifact contract

The artifact set, manifest format, regeneration and execution contract are not
RISC-V-specific: they are the common six-target pipeline documented in
[fixture-oracle.md](fixture-oracle.md). `make asm-fixture-oracle-riscv32` and
`make asm-fixture-oracle-riscv64` run the two RISC-V legs of it.

Only the settings above are RISC-V's own: the `-march`/`-mabi` pairs, the
`elf32lriscv`/`elf64lriscv` link emulations, relaxation being disabled
everywhere, the `-M no-aliases,numeric` disassembly, the freestanding
`-no-runtime-lib -no-standard-headers` CompCert configuration, and the
project-owned GCC probe wrapper that selects the exact ABI behind each
profile's own tool prefix.  Development images install the published
`riscv32-linux-gnu-` glibc toolchain for RV32, while Debian's packaged
`riscv64-linux-gnu-` toolchain remains the RV64 source. This fixture-oracle
workflow is independent of the libc-smoke suite (`.ai/cross-plan.md`), which
both RV32 and RV64 also join with their own real glibc sysroots
(`/usr/riscv32-linux-gnu`, `/usr/riscv64-linux-gnu`); the Dockerfile installs
`libc6-dev-riscv64-cross` explicitly since Debian only `Recommends`, not
`Depends`, it from `gcc-riscv64-linux-gnu`.
