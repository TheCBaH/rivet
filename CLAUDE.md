# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository overview

A retargetable assembler and linker in pure OCaml, for x86-32, x86-64, ARM,
AArch64, RV32 and RV64: an in-memory pipeline from GNU-syntax text to an image
that can be bound at chosen addresses. Its design is specified in
`docs/design.md`.

The repository holds two dune projects:

1. **The assembler** (the root project) — libraries, targets, the `tool/asm.exe`
   command line, and the tests. It needs no C code and no cross toolchain to
   build and test.
2. **`tools/`** — a *separate* nested project providing the `rivet-tools`
   command line: fixture, manifest, oracle, gas-xref and ISA-inventory
   commands. Kept apart on purpose (see "Architecture").

A fresh clone needs submodules initialized before building:
`git submodule update --init` pulls in `vendor/fmt/upstream`, which is vendored
rather than taken from opam. `err_trace` is an opam package, pinned in the
devcontainer.

## Common commands

- `make ci` — what CI runs; run this before pushing.
- `make build` — `dune build @all`.
- `make test` — build, then run the checked-in-fixture test suite
  (`dune build @runtest`) plus the fixture/gas-xref manifest checks.
- `make fmt` / `make fmt-check` — format (auto-promote) / check formatting via
  `dune build @fmt`, using the `ocamlformat` version pinned in `.ocamlformat`.
- Working directly with dune:
  - `opam exec -- dune build @runtest` — full test suite.
  - `opam exec -- dune build test/cram/<name>.t` — a single cram test (e.g.
    `test/cram/bigint_dump.t`); add `--auto-promote` to accept a changed output.
  - `opam exec -- dune build @test/lib/<lib>/runtest` — the unit tests for one
    library under `test/lib/`, which mirrors `lib/`.
- Fixtures (checked-in bytes, no toolchain needed): `make fixtures-check`.
  Regenerating them (`fixtures-regen`, `oracle`, or one target's full
  `make fixture-oracle-<target>` / all six via `make fixture-oracle`) needs the
  cross toolchains and QEMU and is not part of `test`/`ci`.
- `tools/` builds and tests through `make tools-build`, `make tools-test`,
  `make tools-integration`, `make tools-boundary`.

## Architecture

The pipeline has independently callable/testable stages with explicit boundary
types (frozen and documented in `docs/contracts.md`):

```
text source -> parser -> source AST -> normalize -> semantic AST
  -> target lowering -> lowered module AST (sections, fragments, symbols, fixups)
  -> [one or more lowered modules] -> symbol resolution, layout, relaxation,
     encoding, fixups -> linked logical image -> address-bound in-memory image
```

A frontend does not have to go through the text parser — a producer can
construct the source/semantic/lowered AST directly.

- Retargeting unit is *ISA x ABI x textual dialect x endianness x feature set*,
  not just ISA — the architecture-independent engine has no central per-ISA
  register/opcode/relocation listing; that lives in target modules.
- `lib/{foundation,asm_core,asm_syntax,codec,image,target_intf}` — the
  architecture-independent engine (parsing/normalization/codec/image machinery
  and the `TARGET`-style module interfaces targets implement).
- `targets/{x86_32,x86_64,arm,aarch64,riscv32,riscv64}` — one target per
  architecture, each usually split into an `_encode` library (mode + encoder)
  and a front-end library (parses text, implements the target interface);
  `x86_family`/`riscv_family` hold logic shared across that ISA's variants.
- `driver` — target-agnostic pipeline glue (`pipeline.ml`, `registry.ml`,
  `directives.ml`) that wires a chosen target into the stages above;
  `tool/asm.ml` is the CLI built on top of it.
- `native_exec` — runs a laid-out image natively, in-process. It ships C stubs,
  so it is gated by `RIVET_NATIVE_EXEC` and outside every default build.
- `test/` mirrors `lib/` and `targets/` (e.g. `test/lib/asm_syntax/`), plus
  pipeline-level suites: `test/cram/` (golden-output dumps at each pipeline
  boundary), `test/oracle/` (fixture oracle / ABI-conformance runners),
  `test/differential/`, `test/coherence/`, `test/errors/`, `test/xref/`,
  `test/snippets/`, `test/browser/`.
- `vendor/fmt/upstream` — git submodule, vendored (not taken from
  opam) rather than committed with their sources.
- `melange/` mirrors the main source tree via `copy_files` so the Melange (JS)
  build can be gated behind `RIVET_MELANGE`/`RIVET_BACKEND` env vars without
  dune's per-library `modes` conditionals; native, js_of_ocaml, and Melange are
  required to produce byte-identical output.
- Every library has a public name in the `rivet` package (`rivet.<library>`), so
  a consuming project can vendor this repository and depend on it.

### The fixture oracle

Per target: exact regeneration of the cross gcc's freestanding output -> GNU
binutils differential artifacts -> freestanding QEMU execution of the assembled
bytes. See `docs/fixture-oracle.md`. Most of these checks follow the same
two-mode split: a `*-check` variant compares against checked-in bytes and needs
no toolchain (safe for every CI leg), while the matching `*-regen`/`*-oracle`
variant needs the cross toolchains/QEMU and is deliberately kept off the
`test`/`ci` critical path.

`tools/` (the `rivet_tools.exe` CLI) is intentionally built with a *targeted*
dune invocation (`dune build tools/bin/rivet_tools.exe`), never `@all`, so that
toolchain-free checks like `fixtures-check` never transitively require building
the whole assembler. It is a nested project because bos requires the opam `fmt`,
while the assembler vendors its own private `fmt`; one project cannot hold both.

The fixture compiler is a parameter of the tools' command line
(`Rivet_tools.Compiler.t`): `tools/bin` runs it with gcc, and another project can
run `Rivet_tools_cli.Cli.main` with its own compiler and extra subcommands.

### Key docs

- `docs/design.md` — the full design plan/spec (large; search it rather than
  reading it end to end).
- `docs/isa-inventory.md` — the file format/schema for the whole-ISA inventory
  (per-target `manifest.txt`/`summary.txt`, sourced from the vendored
  `vendor/isa-data/{riscv-opcodes,xed}` submodules); not to be confused with the
  narrower `docs/riscv-inventory.md`.
- `docs/contracts.md` — frozen dump formats, `form_id` scheme, directive table,
  support matrix, and fixup/relaxation contract that tests compare against.
- `docs/exec-abi-v1.md`, `-v2.md`, `-v3.md` — the execution ABI versions
  exercised by `abi-conform`/`exec`.
- `docs/fixture-oracle.md` — the fixture and oracle pipeline.

## Commit messages

- Keep commit messages concise and to the point: a short summary line, plus only the detail actually needed to understand the change.
- Never reference files, paths, or documents that are not tracked in this repository (e.g. scratch notes, local-only files, untracked working directories).

## Code comments

- Never reference files, paths, or documents that are not tracked in this repository. Comments must make sense to anyone who clones the repo, not just to someone with access to your local working tree.
