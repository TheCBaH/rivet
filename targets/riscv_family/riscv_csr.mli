(** Standard CSR names, as GNU [as] spells them. Coprocessor- and vendor-specific CSRs are not
    named here; they stay numeric. *)

val number : string -> int option
(** The 12-bit address a name stands for. *)

val name : int -> string option
(** The name of an address, when it has exactly one. *)

val all : (string * int) list
