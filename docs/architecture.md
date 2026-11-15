# Architecture as built

`docs/design.md` is the plan the assembler was built to and is kept as history.
This document says what the tree is now. Where the two differ, this one and
`docs/contracts.md` are authoritative.

## Libraries

Every library has a public name `rivet.<library>` in the `rivet` package, apart from
`native_exec`.

| Directory | Libraries | Role |
| --- | --- | --- |
| `lib/foundation` | `foundation` | Bigint, bit vectors, diagnostics, `Safe_list`; no other dependency |
| `lib/asm_core` | `asm_core` | Source and normalized ASTs, `Expr`, `Lowered_ast`, directives, the common grammar |
| `lib/asm_syntax` | `asm_syntax` | Lexer and lexical profiles |
| `lib/codec` | `codec` | The typed bidirectional codec tree, `Codec.check`, `Codec.inspect` |
| `lib/target_intf` | `target_intf`, `target_encode`, `target_error`, `target_features` | The interfaces a target implements (below) and feature configuration |
| `lib/image` | `image` | Symbol resolution, layout, relaxation, fixups, address binding |
| `lib/compute` | `compute` | Symbolic and concrete evaluation of machine semantics (not part of assembling) |
| `text_pipeline` | `text_pipeline` | Parse, normalize and lower for any target; names no target |
| `driver`, `driver_direct` | `driver`, `driver_direct` | The registry that links every target, the erased `Portable` API, and the direct pipeline stages for producers that build ASTs. Planning options (entry, features, externs, exported, max size, RWX) travel as one `Target.request`, binding options (imports, overlays, load addresses) as `Portable.bind_request`; the CLI has an option for each |
| `targets/*` | per target | See below |
| `tool` | | The `asm.exe` command line |
| `browser/{jsoo,melange}` | | Adapters exposing `Portable` to a browser |
| `native_exec` | `rivet-native-exec` | In-process execution of a laid-out image; C stubs; its own opam package |
| `tools/` | | A separate dune project: `rivet-tools` (fixtures, oracle, gas-xref, ISA inventory) |

## Targets

Each of x86-32, x86-64, ARM, AArch64, RV32 and RV64 has an `_encode` library
(mode, instruction types, encoder and decoder, implementing `ENCODE`, no lexer)
and a front-end library (lexical profile, operand parser, directive handler,
implementing `TARGET`). `x86_family` and `riscv_family` hold what the variants
share. Beside the encoders are the typed builder libraries (`arm_asm`,
`aarch64_asm`, `riscv_asm`, `riscv32_asm`, `riscv64_asm`, `x86_asm`, `x86_32_asm`,
`x86_64_asm`), which depend on the encoder only.

The Intel 8080 (`targets/i8080`) is a seventh target, outside the six of the
original plan. It is held to the public contracts: its closure may contain
generic machinery, `text_pipeline` and itself, never another target or the
driver. It has two text dialects (`i8080_text`, `i8080_as`), builders
(`i8080_asm`) and a behavioural model (`i8080_sem`, `i8080_run`) over
`rivet.compute`. The model is the one place the repository simulates a machine;
the six main targets have no model.

## Interfaces

`ENCODE` (`lib/target_intf/target_encode.ml`) is what an encoder offers: its
instruction, opcode and error types, `lower_instruction`, `encode`, `decode`,
fixup evaluation, padding, the `.align` unit and the published `codec`.
`TARGET` (`lib/target_intf/target.ml`) is `ENCODE` plus the text side:
lexical profile, operand parsing and `handle_directive`. The registry names
`TARGET`; a producer that builds instructions as values needs only `ENCODE`.

The design document's single `TARGET` sketch (§5.1) is superseded by this split.

## Instruction descriptions

The per-opcode tables (`*_ops`) and the generated row tables are the canonical
description of a target; the codec is a view of them (`docs/contracts.md` §1.7).
Row-level checks cover what the codec tree cannot: mask/match overlap for
RISC-V, encoding-position overlap for x86.

## Closure rules

Enforced by scripts run in `make ci`, not by convention:

- `scripts/asm-check-layers.sh`: generic libraries never depend on a target;
  `i8080` reaches only public contracts; no generic source names a target.
- `scripts/asm-check-purity.sh`: the production closure has no C stubs and no
  C-backed dependency. This holds for the whole `rivet` package. C code
  exists only in `native_exec`, which is the separate package
  `rivet-native-exec`, gated by `RIVET_NATIVE_EXEC` and outside every default
  build; `make native-exec` builds and tests it, on a host of the image's ISA.
- `scripts/asm-melange-optin.sh`: with `RIVET_MELANGE` unset no Melange rule
  exists, so Melange is not required to build or test.
- `scripts/external-deps.sh`: a consumer can vendor the tree and supply its own `Fmt`.

## Three builds

Native OCaml, js_of_ocaml and Melange must give byte-identical images. Melange
cannot be a `modes` condition, so `melange/` mirrors every library stanza and
copies the sources with `copy_files`. `scripts/melange-mirror-check.sh` fails
when a library or its module list differs between the two trees.

## What is deliberately not here

No debug information, unwind tables or object files; images are in memory.
Execution of generated code is out of the pure packages: `rivet-native-exec`
runs it in the calling process (`run ~isolate:true`, the default, forks and
enforces a time limit) and the fixture oracle runs it under QEMU.
