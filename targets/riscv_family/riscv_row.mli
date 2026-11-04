(** A generated or vendor table row as an opcode names it: its index in an encoder instance's
    rows, and the row. Only {!Riscv_extensions} mints one, when it builds an instance's rows. *)

type t

val mint : xlen:int -> int -> Riscv_table_row.row -> t
(** The class is read off the row's [match_] for an instance of this XLEN. *)

val index : t -> int
(** The row's position in its instance's rows, standard rows first. *)

val row : t -> Riscv_table_row.row
val mnemonic : t -> string
val feature : t -> string
val source : t -> string
val cls : t -> Riscv_cls.t
val equal : t -> t -> bool
val compare : t -> t -> int
