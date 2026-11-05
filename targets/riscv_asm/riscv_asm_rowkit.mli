(* The operands of the generated row builders, one function per kind of row operand: registers
   and immediates as values, a vector mask as a [bool], a vtype as its keywords, an [fli]
   constant and a Zcmp register list as their spellings. Fixed registers and keywords are added
   by the builder. *)

val mk :
  Riscv_family_encode.Riscv_types.Row.t ->
  Riscv_family_encode.Riscv_types.Operand.t list list ->
  Riscv_family_encode.Riscv_types.Instruction.t

val x : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Operand.t list
val f : Riscv_family_encode.Riscv_types.Freg.t -> Riscv_family_encode.Riscv_types.Operand.t list
val v : Riscv_family_encode.Riscv_types.Vreg.t -> Riscv_family_encode.Riscv_types.Operand.t list
val creg : Riscv_family_encode.Riscv_types.Creg.t -> Riscv_family_encode.Riscv_types.Operand.t list

val cfreg :
  Riscv_family_encode.Riscv_types.Cfreg.t -> Riscv_family_encode.Riscv_types.Operand.t list

val imm : int -> Riscv_family_encode.Riscv_types.Operand.t list
val mem : Riscv_family_encode.Riscv_types.Mem.t -> Riscv_family_encode.Riscv_types.Operand.t list
val cmem : Riscv_family_encode.Riscv_ops.Cmem.t -> Riscv_family_encode.Riscv_types.Operand.t list
val spmem : int -> Riscv_family_encode.Riscv_types.Operand.t list
val word : string -> Riscv_family_encode.Riscv_types.Operand.t list
val rm : Riscv_family_encode.Riscv_types.Rm.t -> Riscv_family_encode.Riscv_types.Operand.t list
val vm : bool -> Riscv_family_encode.Riscv_types.Operand.t list
val vtype : string list -> Riscv_family_encode.Riscv_types.Operand.t list
val fixed_x : int -> Riscv_family_encode.Riscv_types.Operand.t list
val fixed_v : int -> Riscv_family_encode.Riscv_types.Operand.t list
