# Intel 8080 target: 8-bit gaps in the core

Surveyed before `targets/i8080` was written. The target is in-tree but may use
only the public contracts (`ENCODE`, `TARGET`, `Codec`, `Lowered_ast`, `Image`
and the text pipeline); `scripts/asm-check-layers.sh` enforces it. Each row
names the smallest change, if any.

| # | Topic | Finding | Smallest change |
| --- | --- | --- | --- |
| 1 | 16-bit address space | Addresses are `int64`; `bind_image` has no width limit. A 16-bit fixup is expressible: `Lowered_ast.range` `Unsigned 16`/`Bitpattern 16`, container of 2 bytes, little-endian patching (`Image.patch_container`) is what the 8080 needs. `layout_policy.max_size` is fixed by the pipeline's own policy. | None in the core. The target reports `Abs8`/`Abs16` fixups; the text wrapper rejects an image that leaves the 64 KiB space after binding. |
| 2 | 8-bit immediates | `Codec.field ~width:8` and `Codec.fits` exist; operand immediates are `Bigint.t`, narrowed in the target. `Bitpattern 8` is exactly the assembler convention `-128..255`. | None. |
| 3 | Low/high-byte fixups | `Expr` has `Shr` and `And`, and a fixup kind chooses its own `evaluate_fixup`, so `Low8`/`High8` need no core support. Text syntax would need `modifier_syntax` names. | None in the core. **Deferred**: not in the first milestone; only `Abs8` and `Abs16` are produced. |
| 4 | Intel literals | The lexer reads `0x..`/`0b..` (GAS spellings) and decimal. `NNH`, `$`, `'c'` are not lexed. | None for the milestone (`0x` literals only, as planned). Other spellings would be a lexer-profile option. |
| 5 | Instruction-shaped pseudo-ops | `db`, `dw` and `org` carry no leading dot, so the lexer yields them as mnemonics, not directives; `handle_directive` never sees them. | Target-level pseudo-instructions (`Db`, `Dw`, `Org`) in the target's own `Instruction`/`Lowered` types. |
| 6 | Section requirement | `lower_with` rejects an instruction before any section directive (`lower.no-section`); Intel sources have none. | None: the text wrapper inserts `.text` at the head of the source AST when the first item is not a section directive. |
| 7 | Explicit origin | The core has no absolute origin: addresses are chosen by the host (`plan_image` / `bind_image`, §9). | None: `org N` lowers to nothing; the wrapper reads it from the normalized AST (the instruction type is the target's own), requires it before any emitting statement, and binds the section there. One `org`, one section. |
| 8 | Pipeline location | `Pipeline.Make` and `Directives` live in `driver`, which links all six targets, so any target using them has the other six in its closure and the boundary cannot be stated. | Extract them into the library `text_pipeline` (no target dependencies); `driver` keeps the registry and portable API and depends on it. Tests move from `Driver.Pipeline` to `Pipeline`. |
| 9 | Padding | `nop_bytes` and `merge_fill` are per target; the 8080 has a one-byte `NOP`. | None. |
| 10 | Relaxation | The 8080 has no short/long forms or PC-relative branches. | None: every instruction is `Fixed`. |
| 11 | Case | Intel mnemonics and registers are case-insensitive. | Target parser lowercases; the canonical printer is lowercase. |

Aliases (12 undocumented bytes) have no mnemonic: the assembler accepts them
only as `db 0xNN`; the decoder maps them to their documented form's semantics
and the canonical printer prints `db 0xNN`, so canonical output re-assembles to
the same bytes.

Decisions taken while implementing:

- An alias byte whose documented twin takes a 16-bit operand (`cb`, `dd`,
  `ed`, `fd`) decodes together with those two bytes and prints as
  `db 0xcb, 0x00, 0x20`, so disassembly keeps instruction boundaries.
- `nop_bytes` is an error: the 8080 has no alignment padding, and a zero fill
  would be printed back as `.balign`. `.align` with a gap is refused.
- `mov` is fourteen disjoint codec forms so that none overlaps `hlt`
  (`Codec.check` reports no problems).
- The boundary is enforced by `scripts/asm-check-layers.sh` (planted cases
  `i8080-reaches-target`, `i8080-reaches-driver`).
