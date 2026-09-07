# Vendored Intel XED

[`./upstream`](./upstream) is the Intel XED git submodule checked out from
<https://github.com/intelxed/xed>, `main` branch, Apache-2.0 licensed (see
`upstream/LICENSE`).

As with [`../riscv-opcodes`](../riscv-opcodes/README.md), and unlike
[`../../err_trace`](../../err_trace/README.md)/[`../../fmt`](../../fmt/README.md),
nothing here is built as an OCaml library — this is reference data for the
whole-ISA inventory track, not source code the assembler links against.

## What it is for

This is the primary source for the x86_32/x86_64 leg of the whole-ISA
instruction/extension inventory track (`.ai/asm_plan.md`'s "Parallel track:
whole-ISA instruction and extension inventory" section; sourcing survey:
`.ai/isa-inventory-sources.md`; schema: `asm/docs/isa-inventory.md`).

`upstream/datafiles/` is literally one directory per extension (`avx512f`,
`avx-vnni`, `apx-f`, `cet`, `amx-*`, ...) — the cleanest x86-specific
extension split found in the sourcing survey, and the reason XED was picked
over binutils' `i386-opc.tbl` (a single flat table) as the primary x86
source for this track. binutils remains the byte-oracle this project already
trusts for `as`/`objdump` differential testing; XED is used here only for its
per-extension instruction/mnemonic partitioning.

## Why vendored rather than fetched on demand

Same reasoning as `riscv-opcodes`: small (~29MB full checkout, `datafiles/`
itself ~6.8MB), permissively licensed, and a submodule pins the exact commit
the generated inventory was produced from, per the vendoring-tier
recommendation in `.ai/isa-inventory-sources.md`.

This tree carries no dune files, so it needs no `data_only_dirs` marker —
`asm/dune`'s `(vendored_dirs vendor)` already covers the whole `asm/vendor/`
subtree.

## Upgrading

```sh
git -C asm/vendor/isa-data/xed/upstream fetch origin main
git -C asm/vendor/isa-data/xed/upstream checkout origin/main
git add asm/vendor/isa-data/xed/upstream   # commits the new gitlink
```

Then regenerate the inventory (see `asm/docs/isa-inventory.md` for the
generation command, once the pilot's tooling step lands) and re-check its
diff-gate, the same way a `target-matrix.sh` regeneration is checked.
