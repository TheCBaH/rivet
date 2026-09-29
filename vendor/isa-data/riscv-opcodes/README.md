# Vendored `riscv-opcodes`

[`./upstream`](./upstream) is the `riscv-opcodes` git submodule checked out
from <https://github.com/riscv/riscv-opcodes>, `main` branch, RISC-V
International's own permissive license (see `upstream/LICENSE`).

Unlike [`../err_trace`](../err_trace/README.md) and
[`../fmt`](../fmt/README.md), nothing here is built as an OCaml library or
copied out at build time — this is reference *data*, not source code. The
whole point of vendoring it is to pin an exact, reproducible commit of the
upstream instruction tables; `git submodule status` answers "which version"
the same way it does for the other two vendored trees.

## What it is for

This is the primary source for the RISC-V leg of the whole-ISA
instruction/extension inventory track (`docs/design.md`'s "Parallel track:
whole-ISA instruction and extension inventory" section; sourcing survey:
`docs/isa-inventory.md`; schema: `docs/isa-inventory.md`). It is
**not** the same thing as `docs/riscv-inventory.md`, which is the
narrower, compiler-corpus-scoped RISC-V document from Milestone 5 — do not
conflate the two.

`upstream/extensions/` holds one file per extension (`rv_i`, `rv32_zbb`,
`rv64_zba`, `rv_zicsr`, ...), each a plain-text table of
`mnemonic operands bit=val...` lines — the normative list of every
instruction RISC-V International's own tooling recognizes per extension,
independent of what a compiler actually chooses to emit.

## Why vendored rather than fetched on demand

Same reasoning as `err_trace`/`fmt`: a submodule pins an exact commit so the
generated inventory is reproducible, and the repo is small (~1.2MB) and
permissively licensed enough to just check in the gitlink outright, per the
vendoring-tier recommendation in `docs/isa-inventory.md`.

This tree carries no dune files (it is a Python/data project, not OCaml), so
it needs no `data_only_dirs` marker of its own — `dune`'s
`(vendored_dirs vendor)` already keeps the whole `vendor/` subtree,
`isa-data` included, out of dune's watch and `@fmt`.

## Upgrading

```sh
git -C vendor/isa-data/riscv-opcodes/upstream fetch origin main
git -C vendor/isa-data/riscv-opcodes/upstream checkout origin/main
git add vendor/isa-data/riscv-opcodes/upstream   # commits the new gitlink
```

Then regenerate the inventory (see `docs/isa-inventory.md` for the
generation command, once the pilot's tooling step lands) and re-check its
diff-gate, the same way a `target-matrix.sh` regeneration is checked.
