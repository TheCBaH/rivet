(** Construction shared by the RISC-V extension components. *)

val component :
  id:string ->
  feature:string ->
  ?requires:string list ->
  ?conflicts:string list ->
  ?implies:string list ->
  ?conditional:(Rivet_target_features.Component.cond * string) list ->
  ?default_on:bool ->
  ?xlens:int list ->
  ?rows:(Riscv_table_row.row -> bool) ->
  ?legal:(string -> Rivet_target_features.Component.Req.t option) ->
  summary:string ->
  (Riscv_types.Opcode.t * string) list ->
  Rivet_target_features.Component.t
(** [component ~id ~feature ~summary forms] owns [forms] (opcode and riscv-opcodes record) and the
    generated table rows selected by [rows], by default those whose feature is [feature]. [legal] gives a form's predicate when it is not just the component's feature. *)
