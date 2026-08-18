# M3 §3 GNU merge-behavior probe: evidence

Real `as`/`ld` (GNU Binutils for Debian 2.44), all six assembler profiles,
run directly against the toolprefixes and flags `asm/tools/lib/target.ml`
uses (`riscv32` sharing `riscv64-linux-gnu-` binutils with
`-march=rv32imafd -mabi=ilp32d -mno-relax` / `-m elf32lriscv --no-relax`).
The linker script shape in both probe scripts matches this project's own
controlled-link methodology: one explicit `SECTIONS` stanza per output
name, wildcard-matching all same-named inputs (`Link_script.oracle`'s and
`.exec`'s actual shape) — not a bare `ld -r` partial link and not an
unscripted default link, both of which behave differently.

Files:

- `section-merge-probe.sh` / `section-merge-probe-output-2026-08-17.txt` —
  the primary probe: merge-gap offset/fill for an executable (`.text`)
  contribution, same-name PROGBITS-vs-NOBITS coexistence (`.mix`), and
  same-name flag-mismatch handling (`.flagtest`), on all six targets.
- `data-section-probe.sh` / `data-section-probe-output-2026-08-17.txt` —
  supplementary: merge-gap fill for a non-executable PROGBITS (`.data`)
  contribution, on all six targets.

## Findings (pinned contract for asm/lib/image/image.ml's M3 merge logic)

1. **Merge-gap offsets**: a contribution whose input section declares
   `sh_addralign = 16` (via a leading `.balign 16`) is placed by `ld` at
   the next 16-byte-aligned offset in the merged output section, on all
   six targets.

2. **Merge-gap fill bytes** — target- and content-class-dependent:
   - **Executable (`.text`) gap**: `x86_64` and `x86_32` fill with real,
     optimal-length NOP instructions chosen by `ld` itself (observed: a
     12-byte gap became a 10-byte long NOP + a 2-byte NOP on `x86_64`, and
     six 2-byte NOPs on `x86_32`). `arm`, `aarch64`, `riscv64`, `riscv32`
     all fill with plain zero bytes.
   - **Non-executable (`.data`) gap**: **all six targets**, x86 included,
     fill with plain zero bytes. The x86 NOP-fill behavior is specific to
     executable-section gaps, not a general x86 default.
   - Note: RISC-V's *own assembler* separately pads the *end of a single
     input section* with real `c.nop`/`nop` instructions when trailing
     content doesn't reach that section's declared alignment (observed:
     `.text` containing only `.balign 16` + one `.byte` became a
     16-byte object on its own, before any linking, with the 15 padding
     bytes matching `00 01 00` (byte-align pad + `c.nop`) followed by
     three 4-byte `nop`s). This is `as`-time padding within one module,
     already covered by the existing `Align`-fragment `fills` mechanism
     (`lowered_ast.ml:131-135`, `image.ml:486-497`) — it is **not** the
     linker-inserted merge-gap case this probe is measuring, and needs no
     new code.

3. **Same-name, different-kind sections (`.mix`, PROGBITS in one input,
   NOBITS in another)**: under this project's controlled-link
   methodology (one explicit wildcard stanza per name), `ld` **merges**
   both contributions into **one** output section, coerced to a single
   type (observed as PROGBITS on all six targets) — the NOBITS
   contribution's reserved space is silently realized as real zero bytes
   in the linked file. Confirmed by a second run that a bare `ld -r`
   partial link *does* keep them as two separate output sections (`.mix`
   PROGBITS and `.mix` NOBITS) — that is the mode an earlier, second-hand
   claim was evidently based on, and it is not the mode this project uses.
   **Conclusion for the assembler's own internal linker**: sections are
   identified by name alone (not `(name, kind)`); a same-name kind
   mismatch across contributing modules is a dedicated rejection
   diagnostic, deliberately diverging from GNU's silent coercion because
   that coercion is unsafe for the NOBITS model this milestone builds.

4. **Same-name flag/permission mismatch** (`.flagtest`, `"a"` read-only in
   one input, `"aw"` read-write in another): links cleanly with no error on
   all six targets (`ld` exit 0), silently unioned to the more permissive
   result (`WA`). No rejection needed for this dimension — matches GNU
   behavior directly.

See `create-implementation-plan-for-golden-kettle.md` (the M3 plan) §3-§5
for how these findings are incorporated into the design.
