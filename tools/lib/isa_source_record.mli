(** Full jsont decoder for [isa-db/export/*.jsonl] source records
    (isa-db/schema/source_record.schema.v1.json, isa-db/docs/capture-contract.md).

    Distinct from {!Isa_db_jsonl}: that module is a deliberately narrow
    projection ([record_id]/[source]/[kind]/[native_name]/[provenance]'s
    [extension]/[group] only) built for cross-validation against
    [asm/fixtures/isa-inventory]. This module decodes the [encoding] and the
    source-specific [provenance] facts too, because the OCaml normalization
    layer needs them to
    reconstruct assembly-level operand semantics and three-valued
    requirements. *)

(** [applicability]'s three-valued expression tree (isa-db/schema
    §applicability). An empty [App_all []] is unconditional. *)
type applicability =
  | App_all of applicability list
  | App_any of applicability list
  | App_mode of { mode : string; equals : bool }
      (** [{kind: "mode", equals|not_equals: <mode>}]; [equals] is [true] for
          an [equals] key, [false] for [not_equals]. *)
  | App_unknown of string  (** an [applicability.kind] this decoder does not recognize *)

type field = { field_name : string; lsb : int; width : int }
(** One [encoding.fields] entry (riscv-opcodes [fixed_bits] only). *)

type x86_operand = {
  op_name : string;  (** e.g. ["REG0"], ["IMM0"] *)
  op_type : string;  (** e.g. ["nt_lookup_fn"], ["imm_const"], ["reg"] *)
  lookupfn_name : string option;
  oc2 : string option;
  bits : string option;
  rw : string;  (** ["r"], ["w"], or ["rw"] *)
  visibility : string;  (** e.g. ["DEFAULT"], ["IMPLICIT"], ["SUPPRESSED"] *)
}
(** One [encoding.operands] entry (XED only). Preserved verbatim so OCaml
    normalization rules can read a form's read/write role and native
    register/immediate typing from the record itself instead of asserting it
    as external knowledge (§3.4). *)

type encoding =
  | Fixed_bits of { width_bits : int; mask : string; value : string; fields : field list }
  | X86_encoding of {
      space : string;
      opcode_map : int;
      opcode : string;
      pattern : string;
      operands : x86_operand list;
    }
  | Opaque of { note : string }
  | Unknown_encoding of string  (** an [encoding.kind] this decoder does not recognize *)

type relationship_status =
  | Exact
  | Ambiguous
  | Missing
  | Unknown_status of string
      (** [provenance."relationship-resolution"[i].status]: how many candidate
    target records the riscv-opcodes adapter found for one [$import]/
    [$pseudo_op] reference - [Exact] (one), [Ambiguous] (more than one,
    preserved in full in [candidates] - a genuine one-to-many mapping, not
    collapsed to a guess), or [Missing] (none). *)

type relationship = { rel_kind : string; status : relationship_status; candidates : string list }
(** One [provenance."relationship-resolution"] entry. [rel_kind] is
    ["imports"] or ["specializes"]; [candidates] holds every matching
    [record_id], in source order - a list even when [status = Exact] (then a
    singleton), so callers never special-case cardinality. *)

type riscv_provenance = {
  extension : string option;
  raw_tokens : string list;  (** [provenance.raw.tokens] *)
  resolved_mask : string option;  (** [provenance."upstream-resolved".mask] *)
  resolved_match : string option;  (** [provenance."upstream-resolved".match] *)
  variable_fields : string list;  (** [provenance."upstream-resolved".variable_fields] *)
  relationships : relationship list;  (** [provenance."relationship-resolution"], if present *)
}

type xed_provenance = {
  category : string option;
  extension : string option;
  iform : string option;
  isa_set : string option;
  mode_restriction : string option;
}

type provenance = Riscv_provenance of riscv_provenance | Xed_provenance of xed_provenance | Other
type origin = { path : string; line : int option }

type t = {
  record_id : string;
  source : string;  (** ["xed"] or ["riscv_opcodes"] *)
  kind : string;  (** ["instruction-form"], ["pseudo-op"], or ["import"] *)
  native_name : string;
  snapshot : string;
  origin : origin;
  encoding : encoding;
  provenance : provenance;
  applicability : applicability;
  unresolved : string list;
}

val of_line : string -> (t, string) result
(** Decode one JSONL line. Exposed (not just {!read_file}) so unit tests can
    exercise decoding against inline record text, matching the rest of this
    library's convention of testing pure/text-only logic without a
    filesystem dependency (see e.g. [Isa_inventory_riscv]'s [.mli]). *)

val read_file : Fpath.t -> (t list, Tool_error.t) Err.t
(** Read and decode every line of one checked-in
    [isa-db/export/<source>/<profile>.jsonl] file, in file order. *)
