# Native capture contract, version 1

`export/capture-manifest.v1.json` is the envelope for every checked-in capture
run. It identifies the source-record schema (`source_record.schema.v1.json`),
source and build-dependency commits, selected-input Merkle hashes, a content
hash of the producer, and the `refuse` dirty-source policy. Its JSON keys are
sorted; JSONL records are sorted by `record_id`; and the manifest has no clock
or host-specific field, so a clean pinned regeneration is byte-stable.

Each source record preserves source-native facts in `provenance` rather than
claiming a common assembly meaning. RISC-V records retain the raw line and
tokens plus an upstream-resolved mask, match value, and variable-field list.
`$import` and `$pseudo_op` records retain their textual references and a
relationship-resolution report. XED coarse records retain raw `PATTERN`
lines, directory group, `EXTENSION`, and `ISA_SET`; resolved XED records
retain `IFORM`, category, extension, ISA set, and native mode restriction.

The version-1 loss and unknown report is deliberately per record:

- Coarse XED records report that operands and UDELETE suppression are not
  interpreted. Their raw patterns remain available for a later consumer.
- Resolved XED records report their aggregate origin and absent directory
  group/original source location; they do not fabricate either through an
  ambiguous mnemonic join.
- RISC-V relationships are rewritten to an exact record ID only when the
  selected snapshot has one candidate. Missing cross-profile references and
  multiple same-name forms remain in the relationship-resolution report and
  `unresolved` list.
- An unrecognized XED mode-restriction candidate is represented as an
  `unknown` applicability node. It cannot be selected for either profile.
- A RISC-V encoding whose low discriminator cannot prove a 16- or 32-bit
  length is rejected by the producer rather than recorded with a guessed
  width.

`python3 -m export.regen` first verifies every lockfile checkout is clean and
at the recorded commit, then writes all artifacts. `python3 -m export.verify`
performs that validation and compares the recorded envelope without writing.
The producer freshness and source checks require Python and vendored source
trees, so they remain outside the portable `asm-ci` lane.
