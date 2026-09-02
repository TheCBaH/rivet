# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository overview

This repo actually holds two largely independent projects, tied together only
by the root `Makefile` and a shared `modules/CompCert` submodule:

1. **CompCert build tooling** (root-level `Makefile` targets) — a devcontainer
   for configuring, building, and proof-checking
   [CompCert](https://compcert.org/) (vendored as the `modules/CompCert`
   submodule), plus tooling to repackage its extracted OCaml sources as a
   plain dune library (`compcert-lib/`) or a redistributable archive, without
   needing Rocq for the OCaml-only steps.
2. **The retargetable assembler** (`asm/`) — a standalone dune project,
   independent of the CompCert build (no Rocq, no cross toolchain needed to
   build/test it), implementing an in-memory, retargetable assembler/linker
   for the architectures CompCert targets. Its design is specified in
   `.ai/asm_plan.md`.

CompCert itself is not free software (non-commercial use only — see
`modules/CompCert/LICENSE`); this repo's own `LICENSE` covers only the
devcontainer and build tooling, not `modules/CompCert` or `asm/`.

A fresh clone needs submodules initialized before building anything under
`asm/`: `git submodule update --init` (this pulls in `modules/CompCert` as
well as `asm/vendor/err_trace/upstream` and `asm/vendor/fmt/upstream`, which
`asm/` vendors rather than takes from opam).

## Common commands

### CompCert (root Makefile, needs Rocq/opam)

- `make compcert` — configure and build CompCert (proof + `ccomp`) for the
  native architecture.
- `make compcert-check-proof` — recheck the proof with `coqchk`/`rocqchk`.
- `make compcert-test` — run CompCert's own test suite.
- `make compcert-extraction-archive` then `make compcert-build-from-archive`
  — the Rocq-free split build: extract once (needs Rocq), then build `ccomp`
  from the archived sources alone (plain OCaml).
- `make compcert-lib-build` / `make compcert-lib-run ARGS=-version` — sync
  CompCert's sources into `compcert-lib/` (a standalone dune project, not
  committed) and build/run it as an ordinary OCaml library + driver.
- `make compcert-export-archive` / `make compcert-export-run ARGS=-version`
  — package `compcert-lib/` plus CompCert's `LICENSE` into
  `compcert-export.tar.gz`, buildable with just `dune build`, no clone or
  Rocq required.
- `make compcert-lib-build-<target>` (`<target>` is one of the six
  `asm-fixture-oracle` targets, e.g. `aarch64`) — sync/build a *specific*
  architecture's CompCert into `compcert-lib-<target>/`, independent of
  whatever `ARCH` is currently configured in `modules/CompCert/Makefile.config`.

### Assembler (`asm/`, plain OCaml + dune, no Rocq)

- `make asm-ci` — what CI runs; run this before pushing changes under `asm/`.
- `make asm-build` — `dune build @all`.
- `make asm-test` — build, then run the checked-in-fixture test suite
  (`dune build @runtest`) plus the fixture/gas-xref manifest checks.
- `make asm-fmt` / `make asm-fmt-check` — format (auto-promote) / check
  formatting via `dune build @fmt`, using the `ocamlformat` version pinned in
  `asm/.ocamlformat`.
- Working directly with dune from `asm/` (its own `dune-project`, so this
  never touches `compcert-lib/`):
  - `cd asm && opam exec -- dune build @all`
  - `cd asm && opam exec -- dune build @runtest` — full test suite.
  - `cd asm && opam exec -- dune build test/cram/<name>.t` — a single cram
    test (e.g. `test/cram/bigint_dump.t`); add `--auto-promote` to accept a
    changed output.
  - `cd asm && opam exec -- dune build @test/lib/<lib>/runtest` — the unit
    tests for one library under `asm/test/lib/`, which mirrors `asm/lib/`.
- Fixtures (checked-in bytes, no toolchain needed): `make asm-fixtures-check`.
  Regenerating them (`asm-fixtures-regen`, `asm-oracle`, or one target's full
  `make asm-fixture-oracle-<target>` / all six via `make asm-fixture-oracle`)
  needs the cross toolchains and QEMU and is not part of `asm-test`/`asm-ci`.
- `asm/tools` is a *separate* dune project providing the `compcert_tools.exe`
  CLI (fixture/corpus/gas-xref/target-matrix/tool-gate subcommands). Build/test
  it via `make tools-build`, `make tools-test`, `make tools-integration`,
  `make tools-boundary` — kept deliberately decoupled from the assembler build
  (see "Architecture" below).

## Architecture

### CompCert side

- `modules/CompCert` is the vendored upstream submodule; nothing there should
  be treated as part of this repo's own tooling.
- `compcert-lib/`, `compcert-export/`, and `compcert-lib-<target>/` are
  generated/synced trees (not committed — see `.gitignore`), rebuilt from
  CompCert's extracted sources via `tools/modorder`'s link order. The set of
  files copied is exactly what CompCert links into `ccomp`, not everything
  under CompCert's source directories (some of which don't even build under
  dune, e.g. stale `.mli`s for long-removed code).
- `driver/Driver.ml` (CompCert's CLI entry point) is excluded from the
  library and copied to `bin/main.ml`, so `compcert-lib`'s `compcert` library
  is just the compiler, with the CLI kept separate.

### Assembler side (`asm/`)

The pipeline, per `.ai/asm_plan.md` §3.2, has independently callable/testable
stages with explicit boundary types (frozen and documented in
`asm/docs/contracts.md`):

```
text source -> parser -> source AST -> normalize -> semantic AST
  -> target lowering -> lowered module AST (sections, fragments, symbols, fixups)
  -> [one or more lowered modules] -> symbol resolution, layout, relaxation,
     encoding, fixups -> linked logical image -> address-bound in-memory image
```

A frontend does not have to go through the text parser — a producer (e.g. the
CompCert adapter) can construct the source/semantic/lowered AST directly.

- Retargeting unit is *ISA x ABI x textual dialect x endianness x feature
  set*, not just ISA — the architecture-independent engine has no central
  per-ISA register/opcode/relocation listing; that lives in target modules.
- `asm/lib/{foundation,asm_core,asm_syntax,codec,image,target_intf}` — the
  architecture-independent engine (parsing/normalization/codec/image
  machinery and the `TARGET`-style module interfaces targets implement).
- `asm/targets/{x86_32,x86_64,arm,aarch64,riscv32,riscv64}` — one target per
  architecture, each usually split into an `_encode` library (mode + encoder)
  and a front-end library (parses text, implements the target interface);
  `x86_family`/`riscv_family` hold logic shared across that ISA's variants.
- `asm/driver` — target-agnostic pipeline glue (`pipeline.ml`,
  `registry.ml`, `directives.ml`) that wires a chosen target into the stages
  above; `asm/tool/asm.ml` is the CLI built on top of it.
- `asm/compcert_adapter` — bridges CompCert's own `Asm.program` into this
  pipeline (a later-stage integration, gated behind `ASM_COMPCERT_ADAPTER`;
  not on the `asm-test`/`asm-ci` path since it needs a Rocq-built
  `compcert-lib-aarch64`).
- `asm/test/` mirrors `asm/lib/` and `asm/targets/` (e.g.
  `asm/test/lib/asm_syntax/`), plus pipeline-level suites: `test/cram/`
  (golden-output dumps at each pipeline boundary), `test/oracle/` (fixture
  oracle / ABI-conformance runners), `test/differential/`, `test/coherence/`,
  `test/errors/`, `test/xref/`, `test/snippets/`, `test/browser/`.
- `asm/vendor/{err_trace,fmt}/upstream` — git submodules, vendored (not taken
  from opam) rather than committed with their sources.
- `asm/melange/` mirrors the main source tree via `copy_files` so the
  Melange (JS) build can be gated behind `ASM_MELANGE`/`ASM_BACKEND` env vars
  without dune's per-library `modes` conditionals; native, js_of_ocaml, and
  Melange are required to produce byte-identical output.

### The fixture oracle (evidence pipeline behind the six `asm-fixture-oracle-*` targets)

Per target: exact CompCert regeneration -> GNU binutils differential
artifacts -> freestanding QEMU execution of the assembled bytes. See
`asm/docs/fixture-oracle.md`. Most of these checks follow the same two-mode
split: a `*-check` variant compares against checked-in bytes and needs no
toolchain (safe for every CI leg), while the matching `*-regen`/`*-oracle`
variant needs the cross toolchains/QEMU and is deliberately kept off the
`asm-test`/`asm-ci` critical path.

`asm/tools` (the `compcert_tools.exe` CLI) is intentionally built with a
*targeted* dune invocation (`dune build tools/bin/compcert_tools.exe`), never
`@all`, so that toolchain-free checks like `asm-fixtures-check` never
transitively require building the whole assembler.

### Key docs

- `.ai/asm_plan.md` — the full design plan/spec for the assembler (large;
  search it rather than reading it end to end).
- `asm/docs/contracts.md` — frozen dump formats, `form_id` scheme, directive
  table, support matrix, and fixup/relaxation contract that tests compare
  against.
- `asm/docs/exec-abi-v1.md`, `-v2.md`, `-v3.md` — the execution ABI versions
  exercised by `asm-abi-conform`/`asm-exec`.
- `asm/docs/fixture-oracle.md`, `asm/docs/corpus.md` — the fixture and
  CompCert-test-corpus classification pipelines.

## Commit messages

- Keep commit messages concise and to the point: a short summary line, plus only the detail actually needed to understand the change.
- Never reference files, paths, or documents that are not tracked in this repository (e.g. scratch notes, local-only files, untracked working directories).

## Code comments

- Never reference files, paths, or documents that are not tracked in this repository. Comments must make sense to anyone who clones the repo, not just to someone with access to your local working tree.
