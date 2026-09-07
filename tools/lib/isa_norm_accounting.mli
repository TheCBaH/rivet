(** Complete record accounting:
    run {!Isa_norm_riscv.normalize}/{!Isa_norm_xed.normalize} over every
    record in the four checked-in [isa-db/export/{riscv_opcodes,xed_resolved}/
    {riscv32,riscv64,x86_32,x86_64}.jsonl] files and report, per (source,
    target), how many records normalized to a form versus which explicit
    diagnostic rule turned each remaining one away - so "every record either
    got a form or a reported reason, none silently dropped" is machine-checked
    rather than asserted. Read-only: reads only the checked-in exports, never
    the isa-data submodules or a producer tool. *)

type summary = {
  total : int;  (** every record read; always [normalized + diagnosed] *)
  normalized : int;
  by_rule : (string * int) list;  (** diagnostic rule -> count, descending by count *)
  by_name : (string * int) list;
      (** unhandled [native_name]/[iform] -> count, descending by count *)
}

val summarize : Repo.t -> source:string -> Target.t -> (summary, Tool_error.t) Err.t
(** [source] is ["riscv_opcodes"] or ["xed_resolved"]; [target] selects which
    checked-in [.jsonl] file per {!Repo.isa_db_export}. Exposed (not just
    {!run}) so a repository-level regression test can assert exact counts
    against real captured data, not just that the command exits [Success]. *)

val run : Repo.t -> Command.t
(** The CLI-facing command: {!summarize} over all four (source, target)
    pairs, rendered as one report per pair. *)
