(** Emitting and checking the generated x86 rows
    ([targets/x86_family/x86_table_rows.ml], DEC-X86-TABLE): one row per
    {!Isa_x86_table.spec} of a record that no hand-written rule of
    {!Isa_norm_xed} claims, from both committed XED exports; and per mode the rows' typed
    builders ([targets/x86_asm/x86_asm_rows{32,64}.ml]). *)

val rows_path : Repo.t -> Fpath.t
val builders_path : Repo.t -> int -> Fpath.t

val specs : Repo.t -> Target.t -> (Isa_x86_table.spec list, Tool_error.t) Err.t
(** The table specs of one export's records that the hand-written rules leave unhandled. *)

val all_specs : Repo.t -> Target.t -> (Isa_x86_table.spec list, Tool_error.t) Err.t
(** The table specs of every record of one export, hand-written ones included. *)

val emit : Repo.t -> ((Fpath.t * string) list, Tool_error.t) Err.t
(** Each generated file and its text. *)

val run_emit : Repo.t -> Command.t
val run_check : Repo.t -> Command.t
