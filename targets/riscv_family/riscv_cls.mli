(** What kind of work a RISC-V instruction does, read off its encoding, so hand-written forms,
    generated rows and vendor rows are classified alike. A vendor that wants finer classes keys
    on the row's feature. *)

type t =
  | Alu
  | Fp
  | Load  (** integer and scalar floating-point loads *)
  | Store
  | Atomic
  | Branch
  | Jump
  | Csr  (** SYSTEM with a non-zero [funct3] *)
  | System  (** SYSTEM with [funct3 = 0], and MISC-MEM *)
  | Vector
  | Vector_load
  | Vector_store
  | Custom  (** custom-0..3 *)

val all : t list
val name : t -> string

val of_word : xlen:int -> int64 -> t
(** A 32-bit word by major opcode (and [funct3] for SYSTEM, LOAD-FP and STORE-FP); a 16-bit one
    by quadrant and [funct3]. [xlen] decides quadrant 1's [funct3 = 1]: [c.jal] on RV32,
    [c.addiw] on RV64. *)
