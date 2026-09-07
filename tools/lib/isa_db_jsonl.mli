(** Minimal jsont codec for [isa-db/export/*.jsonl] source records
    (cross-validation only). Reads only the fields
    {!Isa_db_cross_validate} needs - [record_id], [source], [kind],
    [native_name], and [provenance]'s ["extension"]/["group"] - leaving every
    other member ([encoding], [applicability], [snapshot], [origin],
    [relationships], [unresolved]) unread. That is jsont's default
    skip-unknown-members behavior, not a partial-schema limitation: see
    isa-db/schema/source_record.schema.v1.json for the full shape this is a
    projection of. *)

type provenance = { extension : string option; group : string option }

type record = {
  record_id : string;
  source : string;
  kind : string;
  native_name : string;
  provenance : provenance;
}

val read_file : Fpath.t -> (record list, Tool_error.t) Err.t
(** Read and decode every line of one checked-in
    [isa-db/export/<source>/<profile>.jsonl] file, in file order. *)
