(** Construction shared by the RISC-V extension components. *)

val component :
  id:string ->
  feature:string ->
  ?requires:string list ->
  ?implies:string list ->
  ?legal:(string -> Target_component.Req.t option) ->
  summary:string ->
  (string * string) list ->
  Target_component.t
(** [component ~id ~feature ~summary forms] owns [forms] (mnemonic and riscv-opcodes record) and every generated table row whose feature is
    [feature]. [legal] gives a form's predicate when it is not just the component's feature. *)
