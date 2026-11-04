(** A generated or vendor table row as an opcode names it: its index in an encoder instance's
    rows, and the row. Only {!Riscv_extensions} mints one, when it builds an instance's rows. *)

type t

val mint : int -> Riscv_table_row.row -> t

val index : t -> int
(** The row's position in its instance's rows, standard rows first. *)

val row : t -> Riscv_table_row.row
val mnemonic : t -> string
val feature : t -> string
val source : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
