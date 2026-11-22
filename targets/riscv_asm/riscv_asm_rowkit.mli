(* The operands of the generated row builders, one function per kind of row operand: registers
   and immediates as values, a vector mask as a [bool], a vtype as its keywords, an [fli]
   constant and a Zcmp register list as their spellings. Fixed registers and keywords are added
   by the builder. *)

val mk :
  Rivet_riscv_family_encode.Riscv_types.Row.t ->
  Rivet_riscv_family_encode.Riscv_types.Operand.t list list ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val x :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Operand.t list

val f :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Operand.t list

val v :
  Rivet_riscv_family_encode.Riscv_types.Vreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Operand.t list

val creg :
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_riscv_family_encode.Riscv_types.Operand.t list

val cfreg :
  Rivet_riscv_family_encode.Riscv_types.Cfreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Operand.t list

val imm : int -> Rivet_riscv_family_encode.Riscv_types.Operand.t list

val mem :
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Operand.t list

val cmem :
  Rivet_riscv_family_encode.Riscv_ops.Cmem.t -> Rivet_riscv_family_encode.Riscv_types.Operand.t list

val spmem : int -> Rivet_riscv_family_encode.Riscv_types.Operand.t list
val word : string -> Rivet_riscv_family_encode.Riscv_types.Operand.t list

val rm :
  Rivet_riscv_family_encode.Riscv_types.Rm.t -> Rivet_riscv_family_encode.Riscv_types.Operand.t list

val vm : bool -> Rivet_riscv_family_encode.Riscv_types.Operand.t list
val vtype : string list -> Rivet_riscv_family_encode.Riscv_types.Operand.t list
val fixed_x : int -> Rivet_riscv_family_encode.Riscv_types.Operand.t list
val fixed_v : int -> Rivet_riscv_family_encode.Riscv_types.Operand.t list
