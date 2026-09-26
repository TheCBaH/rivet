(** Emitting and checking the generated x86 rows
    ([asm/targets/x86_family/x86_table_rows.ml], DEC-X86-TABLE): one row per
    {!Isa_x86_table.spec} of a record that no hand-written rule of
    {!Isa_norm_xed} claims, from both committed XED exports. *)

val rows_path : Repo.t -> Fpath.t

val specs : Repo.t -> Target.t -> (Isa_x86_table.spec list, Tool_error.t) Err.t
(** The table specs of one export's records that the hand-written rules leave unhandled. *)

val all_specs : Repo.t -> Target.t -> (Isa_x86_table.spec list, Tool_error.t) Err.t
(** The table specs of every record of one export, hand-written ones included. *)

val emit : Repo.t -> (string, Tool_error.t) Err.t
val run_emit : Repo.t -> Command.t
val run_check : Repo.t -> Command.t
