# ISA database producer (prototype, XED + riscv-opcodes only)

This is build-out phase 1/2 of the independent ISA database producer:
a source-qualified "source record" layer over instruction-set data, kept
separate from and more detailed than the `manifest.txt`/`summary.txt`
mnemonic inventory in [`asm/docs/isa-inventory.md`](../asm/docs/isa-inventory.md).

Scope of this slice, by explicit decision: **XED and riscv-opcodes only.**
LLVM and QEMU adapters (the design doc's phases 3-4) are not started here.
Do not add them to this directory without first re-reading the design doc's
phased plan; each new source is its own adapter plus its own worked-example
regression tests, not an extension of the existing two.

This is a standalone Python project, deliberately with **zero third-party
dependencies** (stdlib only - no `pytest`, no `jsonschema`; neither was
available in this environment, and the design doc itself says production
assembler libraries should not acquire producer build dependencies). It is
not wired into `asm-ci` or the root `Makefile`: per the design doc, producer
CI is a separate concern from this repo's ordinary, offline, toolchain-free
checks.

## Layout

- `schema/source_record.schema.v1.json` - JSON Schema (draft 2020-12) for a
  source record. Documentation-grade: there is no `jsonschema` dependency
  here, so `normalize/schema_check.py` checks only the required top-level
  shape it describes, not the full schema.
- `sources.lock.json` - pins the two vendored submodule commits this slice
  reads, matching `git -C asm/vendor/isa-data/<name>/upstream rev-parse HEAD`
  at the time this slice was written.
- `normalize/model.py` - the shared, source-agnostic pieces: the source
  record shape, the `fixed_bits`/`opaque` encoding tagged union, the mode
  applicability expression (`all`/`any`/`mode`) and its evaluator, and JSON
  serialization helpers.
- `adapters/xed/reader.py` - reads the vendored XED `datafiles/` tree
  (content-based block recognition, exactly like
  `asm/tools/lib/isa_inventory_xed.ml`) into block-level source records, and
  a `compat_entries` view that reproduces that OCaml module's
  `(mnemonic, extension, applies_32, applies_64)` output exactly (verified by
  `tests/test_xed_reader.py` against the checked-in
  `asm/fixtures/isa-inventory/{x86_32,x86_64}/manifest.txt`).
- `adapters/riscv_opcodes/reader.py` - reads the vendored riscv-opcodes
  `extensions/` files (ratified only, `rv_`/`rv32_`/`rv64_`-prefixed, per
  `asm/tools/lib/isa_inventory_riscv.ml`) into per-line source records,
  including `$import` and `$pseudo_op` as explicit typed relationships
  (`imports` / `specializes`) rather than folding them into a plain mnemonic
  list, plus a `compat_entries` view verified the same way against
  `asm/fixtures/isa-inventory/{riscv32,riscv64}/manifest.txt`.
- `adapters/riscv_opcodes/reference.py` - `operand_vocab`: the arg_lut
  operand-name -> canonical bit range vocabulary, read from
  `upstream.arg_lut()`'s live dict rather than a naive re-parse
  of `arg_lut.csv` (the raw CSV alone omits a handful of names
  riscv-opcodes' own `constants.py` hardcodes at import time - see the
  module's docstring). `csrs.csv`/`csrs32.csv`/`causes.csv` are not ingested.
- `adapters/xed/upstream.py` - thin wrapper around XED's own stronger reader
  path (`gen_setup`/`read_xed_db.xed_reader_t`, the same one
  `pysrc/xed_to_db.py` is built on), reached via the `intelxed/mbuild`
  submodule vendored for exactly this.
  `records()` returns fully resolved `inst_t` objects (UDELETE/version-delete
  already applied, real `iform`/`space`/`map`/`opcode`/`parsed_operands`),
  regenerating `obj/dgen/` on demand rather than checking it in.
- `adapters/xed/upstream_reader.py` - maps those `inst_t` records into
  `SourceRecord`s with a new `x86_encoding` tagged-union encoding kind
  (lean shape, 2026-09-05 user sign-off):
  `space`/`opcode_map`/`opcode`/`pattern` plus a resolved operand
  list. **Additive, not a replacement** for `adapters/xed/reader.py`'s coarse
  block-scan adapter - `export/xed/*.jsonl` and `compat_entries_for_profile`
  (still regression-tested against the checked-in
  `asm/fixtures/isa-inventory` manifests, still what `asm/tools`'s
  `Isa_db_cross_validate` reads) are untouched. The reason: that OCaml check
  keys XED matching on `provenance.group` (the `datafiles/` directory an
  instruction lives under), and the resolved reader path cannot recover that
  - verified in this session that neither `iclass` alone (566/1995 iclasses
  span multiple directory groups) nor `(iclass, EXTENSION)` (still 307
  ambiguous pairs, mostly APX-F duplicating BASE-extension instructions
  under the same `EXTENSION` value) gives a well-defined join back to it.
  These records go to a separate export lane instead - see below.

- `export/writer.py` - one checked-in JSON Lines file per (source, profile):
  `export/riscv_opcodes/{riscv32,riscv64}.jsonl`,
  `export/xed/{x86_32,x86_64}.jsonl`. Each line is one
  `SourceRecord.to_dict()`, `json.dumps(..., sort_keys=True)`, sorted by
  `record_id`, so re-generation is byte-stable and diffs stay reviewable.
  Also writes one auxiliary reference file per source that has one -
  currently just `export/riscv_opcodes/arg-lut.jsonl`, one
  `{source, snapshot, name, lsb, width}` line per operand name, sorted by
  `name` since these aren't source records (no `record_id`).
  Also writes one resolved-path file per (source, profile) that has one -
  currently just `export/xed_resolved/{x86_32,x86_64}.jsonl`, from
  `adapters/xed/upstream_reader.py` above - additive alongside
  `export/xed/*.jsonl`, not a replacement for it.
  Regenerate with `python3 -m export.regen` (run from `isa-db/`, matching
  the test-invocation convention below) after any adapter change;
  `tests/test_export.py`'s `TestCheckedInExportUpToDate` fails if the
  checked-in files and a fresh regeneration disagree. Checked in rather than
  regenerated on demand, by explicit decision.

## What this slice deliberately does not do

- No reconciliation/mapping layer across sources (there is only one source
  per architecture family so far - XED for x86, riscv-opcodes for RISC-V -
  so there is nothing to reconcile yet).
- No SQLite convenience database - `export/` is JSON Lines only.
- No general typed expression algebra (`feature`, `xlen`, `opaque`
  requirement kinds beyond x86's `mode`) - RISC-V source records currently
  carry their extension name as a plain string, not yet a `feature`
  applicability node, since a second source (LLVM) is what actually forces
  that generalization; adding it speculatively here would be exactly the
  kind of premature abstraction the design doc's own "preserve facts before
  reconciling" principle warns against building ahead of need.
- `asm/tools` reads this project's export/ only for cross-validation
  (`compcert-tools isa-inventory cross-validate`, run as part of
  `make tools-integration`/`asm-ci`): it asserts every
  checked-in manifest row has a matching isa-db record, but
  `Isa_inventory_xed`/`Isa_inventory_riscv` are otherwise untouched and remain
  the source of truth for the committed `asm/fixtures/isa-inventory/`
  manifests - replacing them with a JSONL-reading consumer is explicitly a
  later, separate decision.

## Running the tests

```sh
python3 -m unittest discover -s isa-db/tests -t isa-db
```

The tests read the vendored submodules directly (`asm/vendor/isa-data/...`),
so they need `git submodule update --init` to have been run, the same
precondition `asm/`'s own isa-inventory tooling already has.
