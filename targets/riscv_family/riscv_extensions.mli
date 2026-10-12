(** The extensions a RISC-V encoder is built from: the standard components and generated rows, and
    optionally a vendor's. A composition is validated once, by {!make}, and only a validated one
    reaches the encoder functors. *)

type definition = { components : Target_component.t list; rows : Riscv_table_row.row array }
(** A vendor's contribution. Its components are added after the standard ones and must be off by
    default; its rows are the only forms they own. *)

module Validated : sig
  type t
  (** A composition checked for one XLEN. *)

  val xlen : t -> int

  val components : t -> Target_component.t list
  (** Standard components, then the vendor's, specialized to the XLEN. *)

  val rows : t -> Riscv_table_row.row array
  (** Standard generated rows, then the vendor's. *)
end

val standard : xlen:int -> Validated.t
(** The standard extensions alone. *)

val make : handwritten:string list -> xlen:int -> definition -> (Validated.t, string list) result
(** Compose [definition] with the standard extensions. [handwritten] lists the mnemonics of the
    forms the encoder spells out in code. Every problem found is reported: component structure
    (duplicate features or mnemonics, unknown names, conflicts), vendor components on by default,
    rows whose mnemonic collides with a hand-written or standard form, rows outside the
    custom-0..3 opcode space or not 32 bits wide, overlapping rows, and rows no vendor component
    lists. *)
