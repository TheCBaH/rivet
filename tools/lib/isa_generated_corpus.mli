(** The persisted, checked-in corpus of the oracle's runs against
    the frozen S3 pilot manifest ([asm/fixtures/isa-generated/cases.jsonl]),
    and the toolchain-free replay that checks it: replaying committed cases
    offline without producer/toolchain dependencies, rejecting unexplained
    missing cases or tools in required tiers, with wrong-byte, wrong-form and
    unexpected-relocation controls plus actionable minimal failure
    reproduction.

    {!gas_finding} remains what the oracle measures on the GAS side
    alone: GAS's own artifact, and the observed-form check against the case's
    normalized encoding - a necessary but not sufficient half of the
    two independent assertions. Schema version 2 adds the other
    half: {!Isa_generated_case.artifact} for THIS project's own assembler
    ({!Isa_gen_ours}, [None] exactly when GAS itself rejected the case - there
    is nothing to run "ours" against then) and the real, populated
    {!Isa_generated_case.verdict} ({!Isa_gen_verdict}), comparing raw
    assembled bytes directly rather than re-deriving which form each tool
    picked. *)

type gas_finding =
  | Matches_normalized_encoding
      (** GAS accepted the case and the assembled bytes satisfy
          {!Isa_gen_oracle.observed_form_check} against the case's own
          normalized encoding. *)
  | Different_observed_form of { detail : string }
      (** GAS accepted the case, but the assembled bytes do not match the
          expected encoding - see {!Isa_gen_oracle.outcome}'s
          [Assembled_mismatched]. *)
  | Unexpected_relocation of { relocations : string list }
      (** GAS accepted the case, but its object's [.text] section
          carries at least one relocation record - see
          {!Isa_gen_oracle.outcome}'s [Unexpected_relocation]. *)
  | Gas_rejected of { diagnostic : string }
      (** GAS rejected the case; carries {!Gnu_tools.gas_outcome}'s trimmed
          diagnostic body. *)

val finding_of_outcome : Isa_gen_oracle.outcome -> gas_finding
(** The direct translation of {!Isa_gen_oracle.outcome} into this module's
    persisted vocabulary - a rename, not a reclassification. *)

val finding_description : gas_finding -> string
(** One short report label: ["PASS"], ["DIFFERENT-FORM (<detail>)"],
    ["UNEXPECTED-RELOCATION (<relocations>)"], or ["GAS-REJECTED <diagnostic>"]
    - matching [isa-generated regen]'s existing per-case report vocabulary. *)

type record = {
  case : Isa_generated_case.case;
  gas : Isa_generated_case.artifact;
  ours : Isa_generated_case.artifact option;
      (** {!Isa_gen_ours}'s artifact from running [case.rendered_source] -
          the SAME text handed to GAS - through this project's own assembler.
          [None] if and only if [gas.bytes] is [None] (GAS itself rejected the
          case): there is nothing for "ours" to run against then, mirroring
          {!Isa_generated_case.observation}'s own documented invariant. *)
  finding : gas_finding;
  verdict : Isa_generated_case.verdict;
      (** {!Isa_gen_verdict.classify}'s result: rows 1-5 of the outcome table
          only (see {!Isa_gen_verdict}'s own header - no pilot case is
          negative yet, so rows 6-8 have no driving example). *)
}

val schema_version : int
(** The version stamped into every encoded record's [schema_version] member,
    and required to match exactly on decode. Bumped to 2 for the [ours]/
    [verdict] fields - a schema-1 file (the committed corpus, before
    those fields were added) is rejected by name rather than silently read with those
    fields defaulted or absent, per {!Isa_norm_jsonl}'s "a future incompatible
    model change can break intentionally instead of silently" precedent. *)

val to_json : record -> Jsont.json
val of_json : Jsont.json -> (record, string) result

val encode_line : record -> (string, Tool_error.t) Err.t
(** One compact JSON object (RFC 8259 minified), no trailing newline - the
    same per-line shape as {!Isa_norm_jsonl.encode_line}. *)

val decode_line : string -> (record, Tool_error.t) Err.t

val write : Fpath.t -> record list -> (unit, Tool_error.t) Err.t
(** One line per record (via {!encode_line}), sorted by [case.case_id] so the
    committed file is deterministic regardless of the order [records] arrives
    in (e.g. {!Isa_gen_pilot.all}'s own order). *)

val load : Fpath.t -> (record list, Tool_error.t) Err.t
(** The inverse of {!write}. Rejects a line that does not decode or a
    [case_id] that repeats within the file, mirroring
    {!Isa_source_snapshot_diff.load}'s duplicate-id rejection. *)

val replay : record -> Isa_norm_model.encoding -> (unit, string) result
(** Recomputes, WITHOUT invoking any tool, everything that would otherwise
    need a real toolchain:

    - [record.gas.argv] equals {!Isa_gen_oracle.normalized_argv} of
      [record.case], and - by decoding [record.gas.bytes] and calling
      {!Isa_gen_oracle.observed_form_check} against [encoding] - that
      [record.finding] is exactly what that check produces today (a record
      whose [gas.bytes] is [None] must carry {!Gas_rejected}, checked instead
      of the byte comparison) - the original checks, unchanged.
    - [record.ours] is [None] exactly when [record.gas.bytes] is
      [None], never the other combination; when [record.ours] is [Some o],
      [o.argv] equals {!Isa_gen_ours.normalized_argv} of [record.case]; and
      re-deriving an {!Isa_gen_verdict.outcome} from [record.gas.bytes] and
      [record.ours] (using [known_syntax_gap] only when "ours" actually
      rejected) and classifying it reproduces [record.verdict] exactly.

    This is the toolchain-free "replay" this corpus asks for: it never
    assembles anything on either side, it only recomputes over already-
    committed data. *)
