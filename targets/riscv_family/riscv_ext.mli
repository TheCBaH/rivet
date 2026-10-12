(** Construction shared by the RISC-V extension components. *)

val component :
  id:string ->
  feature:string ->
  ?requires:string list ->
  summary:string ->
  (string * string) list ->
  Target_component.t
(** [component ~id ~feature ~summary forms] owns [forms] (mnemonic and riscv-opcodes record) and every generated table row whose feature is
    [feature]. *)
