# The six-target fixture oracle

One reproducible evidence pipeline for every assembler profile: `x86_32`,
`x86_64`, `arm`, `aarch64`, `riscv32`, `riscv64`.

This is the *common* contract. Genuinely per-target detail lives elsewhere:
RISC-V ISA, ABI and tool settings are in [riscv-inventory.md](riscv-inventory.md),
and the target table itself is `scripts/target-matrix.sh`, generated from
`tools/lib/target.ml`.

It provides evidence for the assembler by combining exact regeneration of a
compiler's freestanding output, GNU binutils differential artifacts, and QEMU
execution.

## The evidence chain

Each target leg establishes, in this order:

```text
freestanding cross gcc, one fixed flag set per target
  -> byte-identical fixture regeneration
  -> GNU assemble and controlled link
  -> ELF, section, symbol, relocation, and linked-byte checks
  -> freestanding QEMU execution of those accepted bytes
  -> committed expected exit status
  -> manifest completeness and a clean fixture-tree diff
```

The order is what makes it evidence rather than six independent checks. In
particular the executed image is not merely *a* build of the fixture: every
allocated section of the linked ELF is compared byte-for-byte with the committed
`oracle/linked/<section>.hex` *before* QEMU is invoked, so what runs is provably
what the GNU differential oracle accepted.

## Layout

| Path | Holds |
|---|---|
| `fixtures/c/<case>/` | the case's C sources and `expected-status.txt`, written by hand |
| `fixtures/gcc-14/<case>/<target>/` | what the cross gcc generated, and its `oracle/` artifacts |
| `fixtures/gcc-14/<case>/manifest.txt` | the hash of every file above, and the compiler's provenance |

`gcc-14` names the compiler and its major version: another compiler's output is
another corpus. The manifest records a case's sources under the logical paths
`source/<file>` and `expected-status.txt`, wherever they are kept.

## Commands

| Command | What it needs |
|---|---|
| `make fixtures-check` | nothing — hashes the committed corpus |
| `make fixtures-verify-<target>` | that target's cross gcc; regenerates and byte-compares |
| `make fixture-exec-<target>` | binutils + qemu; runs all cases |
| `make fixture-oracle-<target>` | the complete leg for one target |
| `make fixture-oracle` | the complete six-target oracle |
| `make fixtures-regen`, `make oracle` | regenerate, then record the oracle artifacts |

`make fixture-oracle-<target>` is the authoritative ordered flow, and CI runs
exactly that goal, so local and CI sequencing have a single definition. A
regeneration difference is a reviewed failure, not a refresh: it can change
accepted syntax, relocations, or instruction coverage.

Nothing in this pipeline skips. A missing compiler, binutils, or emulator is a
failure: converting missing evidence into a skip is how a gate silently stops
gating.

## Compiler flags

The flags come from `Target.gcc_fixture_args`, after `-S`:

- `-O1 -fno-inline`: real code, but a call written in the source stays a call;
- `-ffreestanding -fno-pic -fno-pie`: no dynamic linker and no GOT exists here,
  so a GOT-indirected access could not be resolved;
- `-fno-asynchronous-unwind-tables -fno-section-anchors`: no CFI tables and no
  `.set` anchor assignments;
- ARM adds `-marm`; RISC-V pins the profile's `-march`/`-mabi`, adds `-mno-relax`,
  and disables the small-data area (`-msmall-data-limit=0`), because the
  oracle's scope is `.text`, `.data` and `.bss`.

The commands run from a private scratch directory with relative paths, so a
compiler that records its command line in the output cannot write this
checkout's location into committed bytes.

## Work roots

| Root | Env var | Owner |
|---|---|---|
| `.fixture-exec-artifacts/` | `FIXTURE_EXEC_ARTIFACTS` | execution artifacts and provenance |

Fixture work is freestanding: `gcc -S` only, no libc, no driver link.

## Expected exit status

Each case commits `fixtures/c/<case>/expected-status.txt`. It is hashed into the
manifest like any other committed file.

Format, validated before QEMU is invoked:

- exactly one line, one canonical decimal integer — no sign, no leading zeros,
  no surrounding whitespace, and a terminating newline. The whole file is
  compared against the canonical serialization of the parsed value, so
  `42\njunk` without a trailing newline is rejected rather than silently read as
  `42`;
- **range `0-123`.** The host runner owns everything above: `timeout` reports
  `124` for a timeout and `125-127` for its own failures, and `128+n` is a
  signal death of the QEMU process. Allowing those would make a guest result
  observationally identical to a harness failure.

Every case returns `42` by construction, except `cross_bss`, which returns `22`.

## Startup

The executor links a tiny freestanding `_start` that calls `asm_test_entry` and
passes its return value straight to `exit_group`. It lives in its own
`.text.startup` section, placed one 64 KiB page below `.text`, and the linker
script selects sections per object — so the startup cannot land in the bytes
being compared with the oracle.

The stub is written per target rather than templated, for two reasons worth
remembering: ARM's comment character is `@`, so its section flags must be
spelled `%progbits` where the others use `@progbits`; and x86 must align the
stack before the call, while `arm`, `aarch64` and both RISC-V profiles inherit a
16-byte-aligned `sp` from QEMU and already hold the return value in the first
argument register.

## Artifacts

Per case, under `fixtures/gcc-14/<case>/<target>/oracle/`: raw section bytes,
ELF section/symbol/relocation data, canonical GNU disassembly,
relocation/symbol summaries, controlled-link section bytes with a
section-address manifest, and exact tool banners.

Per execution, under `.fixture-exec-artifacts/<target>/`: `tool-versions.txt`
(assembler, linker, **and the emulator that actually ran the bytes**) once per
target, and per case the startup source, linker script, linked ELF, readelf and
objdump reports, section hex, stdout, stderr, and the observed exit status —
written before the assertion, so a failing run still uploads what it saw.

CI uploads these as `fixture-oracle-<target>-provenance` on success and
`fixture-oracle-<target>-failure` on failure.
