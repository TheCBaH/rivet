# The whole-ISA instruction/extension inventory

This is the schema for the "Parallel track: whole-ISA instruction and
extension inventory" section of `.ai/asm_plan.md`. This document only
defines the *format* the pilot's two vendored sources feed into, not where
they came from.

Do not confuse this with `asm/docs/riscv-inventory.md`, which is a narrower,
already-existing document scoped to Milestone 5's CompCert corpus. This
document's inventory is normative per architecture — every mnemonic the
reference source recognizes, whether or not CompCert or gcc ever emit it —
where `riscv-inventory.md` is scoped to what Milestone 5 actually measured.

## Why a schema document before a generator

`.ai/asm_plan.md`'s own description of this track specifies five things an
entry must carry (extension/level, CompCert/gcc-emitted, implementation
state, deferral reason, promotion trigger) but not a file format. Fixing the
format here, against the two vendored sources
(`asm/vendor/isa-data/riscv-opcodes`, `asm/vendor/isa-data/xed`) before
writing the generator, follows the same discipline `asm/docs/contracts.md`
already applies to the pipeline's other boundary formats: the format is
frozen and reviewable independent of the tool that produces it.

## Two sourcing phases, not one

The five required fields split into two groups with very different sourcing
difficulty, and the schema keeps that split explicit rather than pretending
both are available from day one:

- **Phase A — mnemonic and extension, from the vendored sources alone.**
  `riscv-opcodes`' `extensions/<name>` files and XED's
  `datafiles/<extension>/*-isa.xed.txt` files already are, one line/record
  per mnemonic, a normative machine-readable list. Producing this half of
  the inventory needs nothing beyond the two submodules already vendored —
  no cross toolchain, no CompCert build, no corpus regen.
- **Phase B — CompCert-/gcc-emitted, from the Milestone 5 corpora.** This is
  *not* currently derivable from what M5 already publishes:
  `asm/fixtures/corpus/*/​<target>/manifest.txt` records a `generated-sha256`
  of each corpus file's compiler output, not its content, so today there is
  no checked-in record of *which mnemonics* CompCert/gcc actually emitted.
  Producing this half needs a new, cross-toolchain-gated instrumentation
  step alongside `classify-c-<target>`/`classify-c-gcc-<target>` that
  additionally collects the mnemonic set seen across the corpus and
  publishes it as its own checked-in artifact (see "Follow-up: sourcing
  Phase B" below) — the same `*-regen` (needs toolchain) /
  `*-check` (does not) split every other corpus artifact already uses.
  Implementation state (opcode defined / lowered / encoded / decoded /
  byte-checked) is also Phase B in practice: it means walking this
  project's own target modules, not the vendored data, and is deferred for
  the same reason.

Until Phase B tooling lands, every entry's `compcert-emitted`/`gcc-emitted`/
`state` fields are literally the string `unknown` — an explicit placeholder,
not a silent gap, per `.ai/asm_plan.md`'s own "explicit deferral, not a
silent gap" rule for this track. A generator that only ever produces Phase A
is still a complete, valid inventory under this schema; Phase B tightens it
in place rather than changing its shape.

## File layout

One file per target, alongside the existing per-target corpus outputs:

```
asm/fixtures/isa-inventory/<target>/manifest.txt
asm/fixtures/isa-inventory/<target>/summary.txt
```

`<target>` is one of the six profiles (`x86_32`, `x86_64`, `arm`, `aarch64`,
`riscv32`, `riscv64`). RISC-V's source is already split by XLEN applicability
in the upstream data itself (`rv_*` extension files apply to both
`riscv32`/`riscv64`, `rv32_*`/`rv64_*` to one only), so no additional
filtering step is needed to produce two separate target files from the one
submodule. XED's data is not split by `x86_32`/`x86_64` as cleanly (most
extensions apply to both, some are 64-bit-mode-only) — see "Open question:
x86 mode applicability" below.

## `manifest.txt` format

Header lines first (`key:value`, one per line, matching
`asm/fixtures/corpus/*/manifest.txt`'s own header convention), then one
record line per mnemonic entry. Header:

```
schema-version:1
target:<target>
source:<riscv-opcodes|xed>
source-commit:<sha of asm/vendor/isa-data/<source>/upstream>
source-license:<SPDX id, e.g. BSD-3-Clause or Apache-2.0>
```

Record line, tab-separated fields, alphabetically-sorted by `mnemonic` then
`extension` for a stable diff:

```
mnemonic:<lowercase mnemonic as spelled by the source>	extension:<source's own extension/table identifier>	compcert-emitted:<yes|no|unknown>	gcc-emitted:<yes|no|unknown>	state:<undefined|opcode-defined|lowered|encoded|decoded|byte-checked|unknown>	deferral-reason:<free text, or ->	promotion-trigger:<free text, or ->
```

Field notes:

- `mnemonic` — the source's own spelling, lowercase. `riscv-opcodes` already
  lowercases; XED's `ICLASS:` lines are uppercase and must be lowercased on
  ingestion so RISC-V and x86 entries are directly comparable.
- `extension` — `riscv-opcodes`: the `extensions/<name>` filename verbatim
  (`rv_i`, `rv32_zbb`, `rv64_zba`, `rv_zicsr`, ...). XED: the
  `datafiles/<extension>/` directory name verbatim (`avx512f`, `avx-vnni`,
  `apx-f`, ...). Never renamed or normalized to this project's own
  vocabulary — a reviewer must be able to `grep` the source tree for the
  exact string.
- `$pseudo_op` lines in `riscv-opcodes` (a pseudo-instruction expressed as a
  macro over a real one, e.g. `zext.h.rv32` over `pack`) get their own
  record with the pseudo-mnemonic as `mnemonic` and the *defining*
  extension file's name as `extension` (not the extension of the
  instruction it expands to) — pseudo-instructions are still something this
  project's parser must accept as an assembler mnemonic.
- `deferral-reason`/`promotion-trigger` are `-` for any entry with
  `compcert-emitted:yes` or `gcc-emitted:yes` (never deferred — M5 already
  requires these), and required (non-`-`) for any entry with `state` other
  than `byte-checked`/`encoded`/`decoded`/`lowered`/`opcode-defined` once
  Phase B lands. While Phase B is unimplemented and the field reads
  `unknown`, `deferral-reason`/`promotion-trigger` stay `-` — an `unknown`
  is a tooling gap, not a deferral decision, and the two must not be
  conflated.

## `summary.txt` format

Plain counts, matching the shape of `asm/fixtures/corpus/*/summary.txt`:

```
target:<target>
total:<n>
by-extension:<extension>:<n>            # one line per extension, sorted
by-state:<state>:<n>                    # one line per state value, sorted
compcert-emitted:<n>
gcc-emitted:<n>
```

## Generation and the diff-gate

**Implemented (2026-09-04), Phase A, both sources.** `compcert-tools
isa-inventory regen` (`Isa_inventory_riscv` for `riscv32`/`riscv64`,
`Isa_inventory_xed` for `x86_32`/`x86_64` - two independent modules; their
ingestion formats turned out too different to unify, per the `.mli`s' own
headers) writes all four manifests in one invocation. Wired as
`tools-isa-inventory`/`tools-isa-inventory-diff` in the root `Makefile`,
following the same generate-then-diff-gate pattern `tools-matrix`/
`tools-matrix-diff` and `tools-oracle-diff`/`tools-gasxref-diff` already
establish: `regen` overwrites the checked-in files, the `-diff` target
requires `git status --porcelain` to come back empty against them, so the
inventory cannot silently drift from the vendored submodules' pinned
commits. Both sources are toolchain-free (no compiler, no cross assembler),
so `tools-isa-inventory-diff` is on the plain `asm-ci` path with no
`asm-build` edge. `arm`/`aarch64` have no generator yet - see "Not yet
started" below. Phase B's corpus-derived fields, once implemented, stay on
the toolchain-gated `*-oracle`/`*-regen` side of that split like every other
corpus artifact, with a toolchain-free `*-check` counterpart.

### Not yet started

ARM/AArch64 sourcing (QEMU's `target/arm/tcg/*.decode` plus binutils'
`AARCH64_FEATURE(...)` tables) - neither is vendored yet, unlike
`riscv-opcodes`/`xed` above.

## Follow-up: sourcing Phase B

Concretely, Phase B needs `corpus_classify_cmd.ml`/
`corpus_classify_gcc_cmd.ml` (or a sibling) to additionally walk each
generated `.s` file's parsed AST — which the pipeline already builds, since
`classify-c-<target>` runs `--dump-source-ast` — and collect the set of
mnemonics used, publishing it as a new checked-in artifact (e.g.
`asm/fixtures/corpus/mnemonics/<target>.txt`, one mnemonic per line) rather
than reusing today's hash-only manifest. The `isa-inventory` generator would
then read that artifact the same toolchain-free way it reads the vendored
submodules. This is deliberately not attempted in this pass.

## XED's data shape, verified against the vendored checkout (2026-09-04)

The RISC-V pilot (`Isa_inventory_riscv`) is done; this section replaces the
original "open question" placeholder with what live inspection of
`asm/vendor/isa-data/xed/upstream/datafiles/` actually shows, since it turned
out messier than the sourcing survey assumed - read this before writing the
x86 generator rather than re-deriving it.

- **110 directories, not all of them extensions.** `datafiles/` has 110
  subdirectories. Only 82 contain any instruction data at all; the other 28
  (`adl`, `skl`, `tgl`, `spr`, `gnr`, ... - Intel microarchitecture
  codenames - plus `avx-common-types`, `avx10-1`, `vex-map5`, `vex-map7`,
  `evex-map5-6`, `xsave-regs`) hold chip/type metadata only (which ISA_SETs a
  given processor supports, or shared field/register definitions) and must
  be recognized by *content*, not by name - see the next point.
- **Two file dialects share one directory, and filename alone does not tell
  them apart.** Some directories carry a "generated" dialect
  (`<name>-isa.xed.txt`, e.g. `avx-vnni/avx-vnni-isa.xed.txt`,
  `apx-f/apx-f-isa.xed.txt`): one `PATTERN`/`OPERANDS` pair per `{ ... }`
  block, `ICLASS:` with a fixed two-space gutter. Others - `avx/avx-isa.txt`,
  `rdrand/rdrand-isa.xed.txt`, and every AMD-only extension - carry XED's own
  hand-authored source dialect: a `{ ... }` block can hold *several*
  `PATTERN`/`OPERANDS` pairs under one `ICLASS`, and the field gutter is
  irregular (`ICLASS    : VADDPD`, `ICLASS:      VPDPBUSD`). A correct
  generator finds instruction files by scanning every `.txt` file in a
  directory for an `^ICLASS\s*:` line (case- and gutter-tolerant), not by a
  `*-isa.xed.txt` filename pattern - that pattern alone silently misses
  `avx/` itself, `rdrand`, and every AMD extension.
- **Mode applicability is real, per-`PATTERN`-line data, not a missing
  field.** The original "open question" above assumed XED had no clean
  `x86_32`/`x86_64` split; it does, just not as its own field. Each
  `PATTERN` line is itself whitespace-tokenized and can carry a bare `not64`
  token (this form is invalid in 64-bit mode - `x86_32` only, 123 hits
  project-wide) or a bare `mode64` token (valid only in 64-bit mode -
  `x86_64` only, 2830 hits, e.g. all of `apx-f`); a `PATTERN` with neither
  applies to both. Because one hand-authored-dialect block can hold several
  `PATTERN` lines with *different* restrictions (e.g. `monitorx`'s AMD forms:
  one `not64 eamode32` line and one `not64 eamode16` line, both under the
  same `ICLASS`), applicability must be computed per `ICLASS` block by OR-ing
  across all its `PATTERN` lines - "applies to x86_64" iff at least one
  `PATTERN` in the block lacks `not64`; "applies to x86_32" iff at least one
  lacks `mode64` - not by inspecting a single line in isolation.
- Given the above, `extension` for XED should stay the directory name (as
  already specified above), and a per-target XED generator produces its own
  `x86_32`/`x86_64` manifest by filtering on the OR'd applicability just
  described - no separate schema field is needed after all; the original
  "duplicate into both, mark one undefined" idea from the sourcing survey is
  unnecessary once mode data is read from the source directly.

### Correction (2026-09-05): `datafiles/` traversal must reach two more places

The first pass above scanned only the immediate `.txt` files of each
subdirectory of `datafiles/` and missed two categories of real instruction
data, which a directory-only traversal cannot see:

- **`datafiles/` itself carries loose files, not just subdirectories.**
  `xed-isa.txt` alone holds 1228 `ICLASS` blocks - the base ISA: `MOV`,
  `PUSH`, `CPUID`, and everything else with no dedicated extension
  directory - plus a handful of other root files (`xed-nops.txt`,
  `xed-amd-prefetch.txt`). These are read separately from the per-directory
  scan, filed under the synthetic extension `base` (the label XED's own
  `EXTENSION:` field on these blocks overwhelmingly uses), since there is no
  directory name to reuse.
- **An extension directory can itself nest subdirectories with more
  instruction data.** `amd/amdxop/` holds AMD's XOP/FMA4 forms (e.g.
  `VPCMOV`) as siblings of `amd/`'s own `.txt` files, invisible to a scan of
  `amd/`'s immediate files only. The generator now reads every `.txt` file
  under an extension directory recursively, keyed by the top-level directory
  name exactly as before (so `amd/amdxop/amd-xop-isa.txt`'s entries are
  still filed under extension `amd`, not `amdxop`). A couple of other
  directories (`avx512f/tests/`, `mpx/tests/`) also nest a subdirectory, but
  it holds `xed`-CLI regression fixtures with no `ICLASS` block, so recursing
  into it is harmless.

### Correction (2026-09-05): `MODE` is three-way, not a 64-bit bit-flag

The "Mode applicability" bullet above only accounted for `not64` and
`mode64`, treating "not `not64`" as sufficient for x86_64 applicability. XED's
`MODE` field actually has three values - `mode16`, `mode32`, `mode64` - for
which CPU mode a `PATTERN` requires, and a `PATTERN` restricted to `mode16`
or `mode32` is just as excluded from x86_64 as one explicitly tagged `not64`;
it just never needed the `not64` tag because it already names a specific
non-64-bit mode. `PUSHA`/`POPA`/`BOUND` (and `PUSHAD`/`POPAD`/`PUSHFD`/
`POPFD`) are exactly this shape: two `PATTERN` lines, one `mode16` and one
`mode32`, neither carrying `not64` or `mode64` - so the original "lacks
`not64`" rule wrongly kept them in the x86_64 manifest. The fix treats
`mode16`/`mode32`/`not64` alike: any one of them on a `PATTERN` line excludes
that line from x86_64 applicability, `mode64` still being the only thing
that excludes a line from x86_32. (Don't confuse `mode16`/`mode32`/`mode64`
- the `MODE` field - with `eamode16`/`eamode32`/`eamode64`, the separate
`EAMODE` address-size field seen on the same lines, e.g. `monitorx`'s
`not64 eamode32`.)
