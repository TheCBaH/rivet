(** Shared per-case driving logic between the frozen
    isa-generated pilot command ({!Isa_generated_cmd}) and the
    isa-difficult command ({!Isa_difficult_cmd}): given an already-built
    {!Isa_generated_case.case} and its owning form's encoding, runs
    {!Isa_gen_oracle}, then (when GAS itself accepted the case with no
    relocation) {!Isa_gen_ours}/{!Isa_gen_verdict}, and renders the exact same
    per-case report-line/record shape [isa-generated regen] already used
    (byte-for-byte identical wording) before this module was extracted from
    it - a behavior-preserving extraction, not a new pipeline. *)

type result = { command : Command.t; record : Isa_generated_corpus.record option }

val verdict_tag : Isa_generated_case.verdict -> string
(** ["PASS"], ["BYTE-MISMATCH"], ["REGRESSION"], ["FRONTIER-GAP"],
    ["GAS-REJECTED"], ["ORACLE-UNAVAILABLE"], ["BLOCKED"], or
    ["NEGATIVE-CASE-ACCEPTED"], one per {!Isa_generated_case.verdict} row. *)

val run_case :
  prefix:string ->
  label:string ->
  Repo.t ->
  Isa_generated_case.case ->
  Isa_norm_model.encoding ->
  result
(** [run_case ~prefix ~label repo case encoding] runs [case] through
    {!Isa_gen_oracle}, then - only when GAS accepted with no unexpected
    relocation - through {!Isa_gen_ours}/{!Isa_gen_verdict}, and reports one
    [<prefix>: <label>: ...]-prefixed stdout line ([prefix] is each caller's
    own {!Isa_generated_case.cli_group_name}/{!Isa_gen_difficult.cli_group_name},
    [label] a caller-chosen ["<target>/<id>"] string) plus, on success, an
    {!Isa_generated_corpus.record} ready to persist. *)
