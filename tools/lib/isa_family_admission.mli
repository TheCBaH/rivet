(** A complete, source-derived family-admission matrix.

    This is deliberately an accounting report, not an optimistic support
    table. Every selected source record belongs to exactly one source-native
    family and exactly one admission state. A normalized form reaches
    [Promoted_support] only when a committed differential case (the
    [isa-generated] pilot or the [isa-difficult] corpus) names that exact
    source record and normalized form and gives it positive support credit; a
    form with positive cases none of which earns credit remains
    [Gas_generatable]. All other successfully normalized forms are
    [Normalized_only]. A normalization diagnostic is preserved as its specific
    [Blocked] rule, except that the normalizers' catch-all "no rule" diagnostic
    is refined into the record's first unknown construct ({!Isa_construct}).
    Credit is derived from the corpora, never restated. *)

type state =
  | Normalized_only
  | Gas_generatable
  | Promoted_support
  | Oracle_unavailable of string
  | Blocked of string

type tally = {
  normalized_only : int;
  gas_generatable : int;
  promoted_support : int;
  oracle_unavailable : int;
  blocked : (string * int) list;
}

type family = { name : string; total : int; tally : tally }

type construct_count = {
  construct : string;  (** {!Isa_construct.to_string} *)
  needed : int;  (** records without a normalizer rule that need this construct *)
  sole : int;  (** of those, records for which it is the only unknown construct *)
}

type summary = {
  total : int;
  families : family list;
  unruled : int;  (** records the normalizer has no rule for *)
  known_only : int;  (** of those, records whose every construct is already known *)
  constructs : construct_count list;  (** most records unblocked first *)
}

val summarize : Repo.t -> source:string -> Target.t -> (summary, Tool_error.t) Err.t
(** Read one checked-in export and classify every record. [source] is
    ["riscv_opcodes"] or ["xed_resolved"]. *)

val tally_total : tally -> int
val report_lines : label:string -> summary -> string list
val run : Repo.t -> Command.t

val record_lines : Repo.t -> Command.t
(** One line per record of every export:
    [<source>/<target> <family> <state> <lookup_key> <form_id|-> <record_id>],
    where [<state>] is [normalized-only], [gas-generatable],
    [promoted-support], [oracle-unavailable:<reason>] or [blocked:<rule>]. *)
