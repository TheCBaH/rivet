# Library namespacing: inventory

OCaml links one flat namespace of compilation units per program, and a findlib
name (`rivet.image`) does not namespace anything: `(name image)` installs a
global unit `Image`. Rivet installs 53 top-level units today, none with a
`rivet` prefix, and a consumer with its own `Pipeline`, `Image` or `Codec`
clashes with them at link time. This document is the inventory the rename
works from: every library, what it is called now and after, which of its
inner `Lib__X` units are contract, and everything outside the library that
names it.

## Rules

- **Every installed unit is `Rivet_<x>` or `Rivet_<x>__X`.** Library `x`
  becomes `(name rivet_<x>) (public_name rivet.<x>)`, where `<x>` is the
  findlib suffix, so the module is `Rivet_<x>`. Where the dune name and the
  findlib name differ today, the findlib name wins: `asm_oracle`
  (`rivet.oracle`) becomes `rivet_oracle`, `test_coherence`
  (`rivet.test_support`) becomes `rivet_test_support`.
- **Findlib names do not change.** Consumers' dune files stay as they are;
  only their OCaml paths change.
- **Inside the `rivet` package, dune files name libraries by local name**
  (`rivet_foundation`), because with `RIVET_MELANGE=true` only the mirror
  stanzas exist and they have no public names. Stanzas in another package or
  workspace keep public names: `native_exec/dune` (`-p rivet-native-exec`
  resolves `rivet` from the install), `test/consumer`, `test/install`,
  `test/asm/neg` (`requires:` lines).
- **The main module is renamed with the stanza.** Dune picks the main module
  by basename, so renaming only `(name ...)` of `lib/image` would give
  `Rivet_image.Image.t`. Each library is one of: *explicit* (rename `x.ml(i)`
  to `rivet_x.ml(i)`), *generated* (no file named after the library; dune
  generates the alias module, nothing to rename), or *façade* (add an
  explicit main module, needed only where private modules are wanted and a
  generated alias would expose them).
- **Directories do not move.** Audits, planted fixtures and Makefile targets
  that address libraries by directory are unaffected.
- **`rivet.fmt` keeps its shape**: dune name `rivet_fmt`, unit `Fmt` in the
  standalone and Melange builds, no modules in the opam build (it re-exports
  opam `fmt`). It is the one allowed exception.
- **Libraries that are not installed keep their names**: the test
  libraries without a public name (`test_asm`, `smoke`, `smoke_fixtures`,
  `foundation_corpus`, `test_coherence_paths`, ...), `browser_jsoo`,
  `browser_melange`, `i8080_corpus`, and `test_native_exec`.
- **`rivet-tools`** (the nested `tools/` project) already conforms:
  `Rivet_tools`, `Rivet_tools_cli`.

## Libraries, old and new

Kind: *explicit* = main module file `x.ml` (with `.mli` where marked),
*generated* = dune-generated alias. `Lib__X` counts the inner units a
consumer can reach today. Step: the rename step that moves the library
(see "Steps").

| Findlib name | Dune name now → after | Module now → after | Kind now; action | `Lib__X` | Step |
| --- | --- | --- | --- | --- | --- |
| `rivet.foundation` | `foundation` → `rivet_foundation` | `Foundation` → `Rivet_foundation` | generated; keep | 8 | R3 |
| `rivet.asm_core` | `asm_core` → `rivet_asm_core` | `Asm_core` → `Rivet_asm_core` | generated; keep | 6 | R3 |
| `rivet.asm_syntax` | `asm_syntax` → `rivet_asm_syntax` | `Asm_syntax` → `Rivet_asm_syntax` | generated; keep (façade only if `Grammar` goes private) | 9 | R3 |
| `rivet.codec` | `codec` → `rivet_codec` | `Codec` → `Rivet_codec` | explicit `codec.ml`; rename | 0 | R3 |
| `rivet.image` | `image` → `rivet_image` | `Image` → `Rivet_image` | explicit `image.ml`+`.mli`; rename | 1 | R3 |
| `rivet.compute` | `compute` → `rivet_compute` | `Compute` → `Rivet_compute` | explicit `compute.ml`; rename | 0 | R3 |
| `rivet.target_intf` | `target_intf` → `rivet_target_intf` | `Target_intf` → `Rivet_target_intf` (`.Target`) | generated; keep | 1 | R3 |
| `rivet.target_encode` | `target_encode` → `rivet_target_encode` | `Target_encode` → `Rivet_target_encode` | explicit `target_encode.ml`; rename | 0 | R3 |
| `rivet.target_error` | `target_error` → `rivet_target_error` | `Target_error` → `Rivet_target_error` | explicit `target_error.ml`; rename | 0 | R3 |
| `rivet.target_features` | `target_features` → `rivet_target_features` | unwrapped `Target_component`, `Target_config` → `Rivet_target_features.Component`, `.Config` | `(wrapped false)`; drop it, rename `target_component.ml(i)` → `component.ml(i)`, `target_config.ml(i)` → `config.ml(i)`, generated | 0 | R3 |
| `rivet.text_pipeline` | `text_pipeline` → `rivet_text_pipeline` | unwrapped `Pipeline`, `Directives` → `Rivet_text_pipeline.Pipeline`, `.Directives` | `(wrapped false)`; drop it, generated | 0 | R4 |
| `rivet.driver` | `driver` → `rivet_driver` | `Driver` → `Rivet_driver` | generated; keep | 2 | R4 |
| `rivet.driver_direct` | `driver_direct` → `rivet_driver_direct` | `Driver_direct` → `Rivet_driver_direct` | generated; keep | 1 | R4 |
| `rivet.aarch64_encode` | `aarch64_encode` → `rivet_aarch64_encode` | → `Rivet_aarch64_encode` | explicit; rename | 2 | R5 |
| `rivet.aarch64` | `aarch64` → `rivet_aarch64` | → `Rivet_aarch64` | explicit; rename | 0 | R5 |
| `rivet.aarch64_asm` | `aarch64_asm` → `rivet_aarch64_asm` | → `Rivet_aarch64_asm` | explicit +`.mli`; rename | 0 | R5 |
| `rivet.arm_encode` | `arm_encode` → `rivet_arm_encode` | → `Rivet_arm_encode` | explicit; rename | 2 | R5 |
| `rivet.arm` | `arm` → `rivet_arm` | → `Rivet_arm` | explicit; rename | 0 | R5 |
| `rivet.arm_asm` | `arm_asm` → `rivet_arm_asm` | → `Rivet_arm_asm` | explicit +`.mli`; rename | 0 | R5 |
| `rivet.riscv_family_encode` | `riscv_family_encode` → `rivet_riscv_family_encode` | → `Rivet_riscv_family_encode` | explicit; rename | 22 | R5 |
| `rivet.riscv_family` | `riscv_family` → `rivet_riscv_family` | → `Rivet_riscv_family` | explicit; rename | 0 | R5 |
| `rivet.riscv32_encode` | `riscv32_encode` → `rivet_riscv32_encode` | → `Rivet_riscv32_encode` | explicit; rename | 0 | R5 |
| `rivet.riscv32` | `riscv32` → `rivet_riscv32` | → `Rivet_riscv32` | explicit; rename | 0 | R5 |
| `rivet.riscv64_encode` | `riscv64_encode` → `rivet_riscv64_encode` | → `Rivet_riscv64_encode` | explicit; rename | 0 | R5 |
| `rivet.riscv64` | `riscv64` → `rivet_riscv64` | → `Rivet_riscv64` | explicit; rename | 0 | R5 |
| `rivet.riscv_asm` | `riscv_asm` → `rivet_riscv_asm` | → `Rivet_riscv_asm` | explicit +`.mli`; rename | 1 | R5 |
| `rivet.riscv32_asm` | `riscv32_asm` → `rivet_riscv32_asm` | → `Rivet_riscv32_asm` | explicit +`.mli`; rename | 1 | R5 |
| `rivet.riscv64_asm` | `riscv64_asm` → `rivet_riscv64_asm` | → `Rivet_riscv64_asm` | explicit +`.mli`; rename | 1 | R5 |
| `rivet.x86_family_encode` | `x86_family_encode` → `rivet_x86_family_encode` | → `Rivet_x86_family_encode` | explicit; rename | 5 | R5 |
| `rivet.x86_family` | `x86_family` → `rivet_x86_family` | → `Rivet_x86_family` | explicit; rename | 0 | R5 |
| `rivet.x86_32_encode` | `x86_32_encode` → `rivet_x86_32_encode` | → `Rivet_x86_32_encode` | explicit; rename | 0 | R5 |
| `rivet.x86_32` | `x86_32` → `rivet_x86_32` | → `Rivet_x86_32` | explicit; rename | 0 | R5 |
| `rivet.x86_64_encode` | `x86_64_encode` → `rivet_x86_64_encode` | → `Rivet_x86_64_encode` | explicit; rename | 0 | R5 |
| `rivet.x86_64` | `x86_64` → `rivet_x86_64` | → `Rivet_x86_64` | explicit; rename | 0 | R5 |
| `rivet.x86_asm` | `x86_asm` → `rivet_x86_asm` | → `Rivet_x86_asm` | explicit +`.mli`; rename | 0 | R5 |
| `rivet.x86_32_asm` | `x86_32_asm` → `rivet_x86_32_asm` | → `Rivet_x86_32_asm` | explicit +`.mli`; rename | 1 | R5 |
| `rivet.x86_64_asm` | `x86_64_asm` → `rivet_x86_64_asm` | → `Rivet_x86_64_asm` | explicit +`.mli`; rename | 1 | R5 |
| `rivet.i8080_ops` | `i8080_ops` → `rivet_i8080_ops` | → `Rivet_i8080_ops` | explicit; rename | 0 | R5 |
| `rivet.i8080_encode` | `i8080_encode` → `rivet_i8080_encode` | → `Rivet_i8080_encode` | explicit; rename | 0 | R5 |
| `rivet.i8080` | `i8080` → `rivet_i8080` | → `Rivet_i8080` | explicit; rename | 0 | R5 |
| `rivet.i8080_asm` | `i8080_asm` → `rivet_i8080_asm` | → `Rivet_i8080_asm` | explicit +`.mli`; rename | 0 | R5 |
| `rivet.i8080_text` | `i8080_text` → `rivet_i8080_text` | → `Rivet_i8080_text` | explicit; rename | 0 | R5 |
| `rivet.i8080_as` | `i8080_as` → `rivet_i8080_as` | → `Rivet_i8080_as` | explicit; rename | 0 | R5 |
| `rivet.i8080_sem` | `i8080_sem` → `rivet_i8080_sem` | → `Rivet_i8080_sem` | explicit; rename | 0 | R5 |
| `rivet.i8080_run` | `i8080_run` → `rivet_i8080_run` | → `Rivet_i8080_run` | explicit; rename | 0 | R5 |
| `rivet.oracle` | `asm_oracle` → `rivet_oracle` | `Asm_oracle` → `Rivet_oracle` | generated; keep | 6 | R6 |
| `rivet.oracle_run` | `asm_oracle_run` → `rivet_oracle_run` | `Asm_oracle_run` → `Rivet_oracle_run` | generated; keep | 1 | R6 |
| `rivet.differential` | `differential` → `rivet_differential` | `Differential` → `Rivet_differential` | explicit +`.mli`; rename | 0 | R6 |
| `rivet.test_support` | `test_coherence` → `rivet_test_support` | `Test_coherence` → `Rivet_test_support` | explicit `test_coherence.ml`; rename to `rivet_test_support.ml` | 0 | R6 |
| `rivet.test_dump_inputs` | `test_dump_inputs` → `rivet_test_dump_inputs` (both trees) | `Test_dump_inputs` → `Rivet_test_dump_inputs` | explicit; rename | 0 | R6 |
| `rivet-native-exec` | `native_exec` → `rivet_native_exec` | `Native_exec` → `Rivet_native_exec` | explicit +`.mli`; rename (C stub file and primitive names unchanged) | 0 | R6 |
| `rivet.fmt` | `rivet_fmt` (unchanged) | `Fmt` (unchanged) | `(wrapped false)`, vendored; exception | 0 | — |

51 libraries in the `rivet` package (53 top-level units: `target_features`
and `text_pipeline` install two each), plus `rivet-native-exec`. No new
name clashes: no existing unit starts with `Rivet_` apart from the
`rivet-tools` ones, and none of the new names is shared with them. After
the rename the only `(wrapped false)` left is `rivet.fmt` and its mirror.

Directories that hold several libraries with explicit `(modules ...)`
lists, all of which must stay in step with the renamed files, in both
trees: `lib/target_intf` (4), `targets/i8080` (8), `targets/riscv_asm` (3),
`targets/x86_asm` (3), `targets/{aarch64,arm,riscv32,riscv64,x86_32,x86_64,riscv_family,x86_family}`
(2 each), `test/oracle` (2 published, plus a test library),
`test/dump` (2), `test/coherence` (2).

## Inventories per configuration

The namespace check reads one inventory per build configuration and fails
when an expected provider is missing. Provider = findlib sub-directory for
installed trees, mirror library for Melange.

| Configuration | Built by | Libraries | Units today |
| --- | --- | --- | --- |
| `standalone` | `dune build @install` → `_build/install/default/lib/rivet` | all 51 in the table above | 53 (incl. `Fmt`) |
| `opam` | `RIVET_EXTERNAL_DEPS=true dune build -p rivet @install` | the same 51; `fmt` is present but empty (re-export) | 52 (no `Fmt`) |
| `native-exec` | `RIVET_NATIVE_EXEC=true RIVET_EXTERNAL_DEPS=true dune build -p rivet-native-exec @install` against an installed `rivet` | `rivet-native-exec` | 1 |
| `melange` | `RIVET_MELANGE=true dune build` (`make melange`) → each mirror's `.objs/melange` | 51 mirror stanzas: the 45 production libraries, `rivet_fmt`, `test_dump_inputs`, and the unpublished `browser_jsoo`, `browser_melange`, `smoke_fixtures`, `foundation_corpus` | |

So the expected-library list for `opam` must not require units from `fmt`,
and the `melange` list has to say whether the four unpublished mirror
libraries are checked (they are linked only into Rivet's own test and
browser programs) or excluded. Not mirrored, because Melange does not build
them: `oracle`, `oracle_run`, `differential`, `test_support`,
`rivet-native-exec`, `i8080_corpus` and the test libraries.

`make melange-mirror` compares library names, module lists, `modes`,
`wrapped` and `private_modules` for the 47 libraries under `lib`, `targets`,
`driver`, `driver_direct`, `text_pipeline` and `browser`. The mirrors under
`melange/test/dump`, `melange/test/lib/foundation` and `melange/vendor/fmt`
are outside it; renaming `test_dump_inputs` (R6) in one tree only is caught
by `make js` (`test/dump` and `test/browser` build in both configurations),
not by the mirror check.

## The 71 `Lib__X` units

Reach: how a consumer gets at the unit through the main module today.
*alias* = generated alias module; *re-export* = `module X = X` in the main
module; *include* = the main module does `include X`, which turns X's
submodules into aliases of `Lib__X.Sub`; *signature* = a public signature
names a type of X without re-exporting X; *none* = reachable only by the
`Lib__X` spelling. Used outside: named from another library, a test or a
consumer of this repository. No consumer names a `Lib__X` unit directly.

Class, for applying `private_modules` later (R7): **public** = must stay
reachable (a hidden `.cmi` breaks a module alias, so a re-exported or
included unit cannot be private); **private candidate** = no public
signature reaches it; **needs re-export** = a public signature reaches it,
so it can be private only after the main module (or a façade) re-exports
it, otherwise it stays public.

| Library | Units | Reach | Used outside | Class |
| --- | --- | --- | --- | --- |
| `foundation` (8) | `Bigint`, `Byte_cursor`, `Diag`, `Diagnostic`, `Err_policy`, `Origin`, `Safe_list`, `Span` | alias | all | public |
| `asm_core` (6) | `Directive`, `Expr`, `Gnu_module`, `Lowered_ast`, `Normalized_ast`, `Perms` | alias | all | public |
| `asm_syntax` (9) | `Lexer`, `Lexical_profile`, `Local_labels`, `Parse_lines`, `Set_symbols`, `Source_ast`, `Statement`, `Token` | alias | all | public |
| | `Grammar` (Menhir output) | alias; signature: `Parse_lines` has no `.mli`, its inferred `to_menhir : Token.t -> Grammar.token` names it | no | needs re-export, or a `parse_lines.mli` that hides `to_menhir`, then private |
| `driver` (2) | `Portable`, `Registry` | alias | all | public |
| `driver_direct` (1) | `Pipeline_direct` | alias | yes | public |
| `target_intf` (1) | `Target` | alias | yes | public |
| `image` (1) | `Symtab` | re-export (`image.mli`) | yes | public |
| `aarch64_encode` (2) | `Aarch64_types` | include | via the main module | public |
| | `Aarch64_ops` | re-export | yes | public |
| `arm_encode` (2) | `Arm_types` | include | via the main module | public |
| | `Arm_ops` | re-export | yes | public |
| `riscv_asm` (1) | `Riscv_asm_rowkit` | re-export as `Rowkit` | yes | public |
| `riscv32_asm` (1), `riscv64_asm` (1) | `Riscv_asm_rows32`, `Riscv_asm_rows64` (generated) | include in the `.mli` (`include module type of struct ... end`) | no | private candidate |
| `x86_32_asm` (1), `x86_64_asm` (1) | `X86_asm_rows32`, `X86_asm_rows64` (generated) | include in the `.mli` | no | private candidate |
| `x86_family_encode` (5) | `X86_types` | include | via the main module | public |
| | `X86_ops`, `X86_table_row`, `X86_table_rows` | re-export | yes | public |
| | `X86_x87` | signature: `... -> X86_x87.operand_shape option` in the inferred interface | no | needs re-export |
| `riscv_family_encode` (22) | `Riscv_types`, `Riscv_ops`, `Riscv_ext_m`, `Riscv_csr`, `Riscv_table_row`, `Riscv_table_rows`, `Riscv_overlap`, `Riscv_extensions`, `Riscv_isa`, `Riscv_std_rows32`, `Riscv_std_rows64` | re-export | most | public |
| | `Riscv_cls`, `Riscv_row`, `Riscv_std_rows` | signature: `cls : t -> Riscv_cls.t`, `row_ids : Riscv_row.t array`, `module Std_row : Riscv_std_rows.S` (main, `Riscv_types`, `Riscv_extensions`, the std rows) | no | needs re-export |
| | `Riscv_ext`, `Riscv_ext_a`, `Riscv_ext_b`, `Riscv_ext_c`, `Riscv_ext_fd`, `Riscv_ext_fp`, `Riscv_ext_sys`, `Riscv_ext_v` | none | no | private candidate |
| `oracle` (6) | `Abi`, `Abi_v2`, `Abi_v3`, `Manifest`, `Record`, `Snippet_bytes` | alias | yes | public |
| `oracle_run` (1) | `Qemu_user` | alias | yes | public |

Totals: 71 units; 54 public, 12 private candidates, 5 needing a re-export
or an `.mli` first (`Riscv_cls`, `Riscv_row`, `Riscv_std_rows`, `X86_x87`,
`Grammar`). Inferred interfaces matter beyond
`Lib__X`: `i8080_encode.ml` has no `.mli` and exports `module Ops =
I8080_ops`, so `rivet.i8080_ops` is public by signature (its descriptor
table is `Opcode.t = Ops.entry`); `asm_syntax` is public by signature
through `Target.TARGET.lexical_profile`. Both stay public libraries.

The generated alias units (`Image__`, `Riscv_family_encode__`, ...) become
`Rivet_image__` and so on with the rename and need no decision.

## What moves with each library

Counts are word matches of the module names in tracked `.ml`/`.mli`/`.mly`
files (an upper bound; the x86 count is dominated by the generated
`x86_asm_rows*` files) and dune files that name the library.

### R3: engine and interfaces

`foundation`, `asm_core`, `asm_syntax`, `codec`, `image`, `compute`,
`target_intf`, `target_encode`, `target_error`, `target_features`. About
7,500 matches in 169 OCaml files; 80 dune files.

- Mirror stanzas under `melange/lib/*`; `target_features` drops
  `(wrapped false)` in both trees (the mirror check compares it).
- `native_exec/test/dune` and `test/trampolines/dune` use local names
  (gated by `RIVET_NATIVE_EXEC`, built by `make native-exec`);
  `native_exec/dune` keeps `rivet.foundation rivet.asm_core rivet.image`.
- Installed consumers: `test/consumer/consumer.ml` (`Foundation.Bigint`,
  `make external-deps`), `test/install/check.ml` (`Foundation`, `Image`; the
  opam workflow), `test/asm/neg` (no engine names).
- Generators that write module names into committed sources:
  `tools/lib/isa_x86_table_emit.ml` emits `Asm_core.Expr.t`; regenerate or
  rewrite `targets/x86_asm/x86_asm_rows*.ml(i)` in the same commit, or
  the generated-table checks drift.
- `scripts/asm-check-planted.sh`: `tool-reaches-assembler` plants
  `asm_core` into `tools/lib/dune` and expects `Library "asm_core" not found`.
  After the rename that still fails, but against a name that no longer
  exists; plant `rivet_asm_core` instead. The other plants edit
  `lib/foundation/dune` by path and are unaffected.
- `scripts/asm_audit.ml` classifies by directory; no library names change
  there.
- Docs: `README.md`, `docs/architecture.md`, `docs/consumers.md`
  (`Target_config.parse_spec`, `Image.bind_image`), `docs/contracts.md`
  (`Target_component`, `Target_config`, `Codec`, `Image`), `docs/errors.md`,
  `docs/extensions.md`, `docs/i8080-gaps.md`, `docs/i8080-asm.md`,
  `docs/i8080-oracle.md`, `CLAUDE.md`. `docs/design.md` is history and keeps
  the old names.

### R4: pipelines

`text_pipeline` (drops `(wrapped false)`, both trees), `driver`,
`driver_direct`. About 170 matches in 39 OCaml files; 39 dune files.

- `scripts/asm-check-planted.sh`: `i8080-reaches-driver` plants `driver`
  into `targets/i8080/dune`; plant `rivet_driver`.
- `test/browser/dune` (`smoke`, no `enabled_if`, both configurations) and
  `test/dump/dune` executables depend on `driver`.
- Docs: `docs/consumers.md` (`Pipeline.Make (C)`,
  `Driver_direct.Pipeline_direct.Make`, `Driver.Portable`), `README.md`,
  `docs/i8080-gaps.md` row 8.

### R5: targets

The 32 target libraries, i8080 group included. About 98,000 matches
(mostly generated tables) in 154 OCaml files; 48 dune files.

- Generators: `tools/lib/isa_x86_table_emit.ml` (`open X86_family_encode`,
  `open X86_asm`) and `tools/lib/isa_riscv_table.ml` (`open Riscv_asm.Rowkit`,
  `module R = Riscv_family_encode.Riscv_std_rows%d`); the committed
  `targets/x86_asm/x86_asm_rows*.ml(i)` and `targets/riscv_asm/riscv_asm_rows*.ml(i)`
  (the `.mli` are `ocamlc -i` output and name `Riscv_family_encode.Riscv_types`,
  `X86_family_encode.Mem` and so on throughout).
- `scripts/asm-check-planted.sh`: `i8080-reaches-target` plants `arm`; plant
  `rivet_arm`. `scripts/asm_audit.ml`'s `target_names` are target name
  strings, not libraries, and stay.
- `test/asm/neg/*.ml`: compiled against the install through `requires:`
  findlib names (unchanged); their sources name `Aarch64_asm`, `Arm_asm`,
  `I8080_asm`, `Riscv32_asm`, `Riscv64_asm`, `Riscv_asm`, `X86_*_asm`. The
  `error:` mentions are substrings of type errors (`Xreg`, `` `W ``) and
  should survive the prefix.
- Target name strings in scripts, Makefile, CI and cram tests (`aarch64`,
  `x86_64`, ...) are CLI/target names, not library names, and stay.
- Docs: `docs/consumers.md` vendor-extension example (`Riscv_family.Make_with`,
  `Riscv32_encode.Profile`, `Riscv_family_encode.Extensions.make`),
  `docs/extensions.md`, `docs/i8080-asm.md`, `docs/i8080-oracle.md`,
  `docs/i8080-gaps.md`, `CLAUDE.md` (`Riscv_ops`, `Aarch64_asm`, `I8080_asm`).

### R6: test support and native_exec

`oracle`, `oracle_run`, `differential`, `test_support`, `test_dump_inputs`
(both trees, `melange/test/dump`), `rivet-native-exec`. About 120 matches
in 18 OCaml files; 13 dune files.

- `test/install/check.ml` (`Test_coherence`, the opam workflow) and
  `test/coherence/test_coherence_paths.ml`.
- `native_exec/test`, `test/trampolines` (`native_exec` local name inside
  `rivet-native-exec`).
- Docs: `README.md`, `docs/contracts.md` (`Native_exec`),
  `docs/architecture.md` (`Differential.Make`).

### R7: private modules and the check

Applies the class column above: `private_modules` for the 12 private
candidates; for the four "needs re-export" units either a re-export in the
main module or they stay public; `Grammar` after `parse_lines.mli`. The
`*_types` units stay public while the encoders `include` them. Each change
is checked by compiling a consumer against an install
(`docs/consumers.md`'s vendor-extension example and `test/asm/neg` are the
existing ones) before it lands.

## Applied (R7)

The 12 private candidates are `private_modules` in both trees. `Riscv_cls`,
`Riscv_row`, `Riscv_std_rows` and `X86_x87` are re-exported by their main
modules and stay public; `parse_lines.mli` hides `to_menhir` and `supplier`,
so `Grammar` is private. The `*_types` units stay public: besides the
encoders' `include`, the inferred interfaces of `rivet_aarch64_asm`,
`rivet_arm_asm` and the `x86_asm` builders name them as `Rivet_<lib>__X`.
`test/asm/neg` compiles the promised paths (`Rivet_image.t`,
`Rivet_text_pipeline.Pipeline.Make`, `Rivet_target_features.Config.t`, an
`ENCODE`) and the vendor-extension example against the install, and is
refused one unit hidden by each new `private_modules`.
