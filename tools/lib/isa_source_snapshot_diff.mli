(** Cross-snapshot record-identity mapping report: [record_ids are]
    snapshot-local references, not reliable cross-update logical identities.
    Keep them as evidence keys, add fingerprints and reviewed migration
    mappings, and detect ambiguity.

    riscv-opcodes IDs embed a source line number and resolved XED IDs embed
    an iteration-order occurrence counter, so neither promises that the same
    [record_id] denotes the same fact across an upstream snapshot bump -
    reusing an id for different content is exactly the ambiguity that must
    be surfaced, not silently accepted. This module compares two record sets
    by exact [record_id] and a content fingerprint of each record's own raw
    JSONL line, classifying every id as added, removed, unchanged, or
    changed - the last being the id-reuse case called out above by name.

    Read-only and offline: {!load} reads one already-checked-in
    [isa-db/export/<source>/<profile>.jsonl] file; nothing here regenerates
    an export, invokes a producer, or touches the isa-data submodules. *)

type status = Added | Removed | Unchanged | Changed

type entry = {
  record_id : string;
  status : status;
  old_fingerprint : string option;  (** [None] iff [status = Added] *)
  new_fingerprint : string option;  (** [None] iff [status = Removed] *)
}

type report = {
  entries : entry list;  (** sorted by [record_id], for a deterministic report *)
  added : int;
  removed : int;
  unchanged : int;
  changed : int;
}

val diff : old_:(string * string) list -> new_:(string * string) list -> report
(** [diff ~old_ ~new_] compares two [(record_id, raw_line) list]s, each
    assumed already free of duplicate ids within itself (as {!load}
    guarantees; a hand-built input that violates this has its later
    duplicate silently win, matching {!Stdlib.List.assoc}-style shadowing -
    callers that need duplicates rejected should route through {!load}). A
    [record_id] present in only one side is [Added]/[Removed]; present in
    both with byte-identical raw lines is [Unchanged]; present in both with
    different raw lines is [Changed]. *)

val load : Fpath.t -> ((string * string) list, Tool_error.t) Err.t
(** Read one checked-in JSONL export file as [(record_id, raw_line)] pairs,
    in file order. Fails if a line does not decode (via
    {!Isa_source_record.of_line}, used only to obtain its [record_id]) or if
    a [record_id] repeats within the file. *)

val diff_files : Fpath.t -> Fpath.t -> (report, Tool_error.t) Err.t
(** {!load} both files, then {!diff} them. *)

val report_lines : label:string -> report -> string list
(** Render a report as one summary line plus one line per non-[Unchanged]
    entry, each showing its short fingerprint(s) - an unabridged unchanged
    listing would dominate the output for no benefit once a real snapshot
    bump is diffed, since the large majority of ids are expected to persist
    unchanged. *)
