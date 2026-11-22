(* Typed builders for the hand-written RISC-V instructions, one per opcode and named by its
   mnemonic ([.] becomes [_], a keyword gains [_]): [add x1 x2 x3], [lw a0 (off sp 8)]. Immediates
   are [int] ([int64] for [li]); the [_e] builder of a form with an immediate takes an
   {!Rivet_asm_core.Expr.t} instead. [Rv32] and [Rv64] add the builders only one XLEN has. Each builder
   returns the {!Riscv_types.Instruction.t} the encoders lower; lowering still checks ranges and
   enabled extensions. *)

module Common : sig
  module Creg = Rivet_riscv_family_encode.Riscv_types.Creg
  module Cfreg = Rivet_riscv_family_encode.Riscv_types.Cfreg

  val x0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x7 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x8 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x9 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x10 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x11 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x12 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x13 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x14 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x15 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x16 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x17 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x18 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x19 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x20 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x21 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x22 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x23 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x24 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x25 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x26 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x27 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x28 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x29 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x30 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x31 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val zero : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val ra : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val sp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val gp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val tp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val fp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a7 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s7 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s8 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s9 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s10 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s11 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val f0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f8 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f9 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f10 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f11 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f12 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f13 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f14 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f15 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f16 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f17 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f18 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f19 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f20 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f21 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f22 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f23 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f24 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f25 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f26 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f27 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f28 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f29 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f30 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f31 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs8 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs9 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs10 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs11 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft8 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft9 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft10 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft11 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val v0 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v1 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v2 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v3 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v4 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v5 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v6 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v7 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v8 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v9 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v10 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v11 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v12 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v13 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v14 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v15 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v16 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v17 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v18 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v19 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v20 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v21 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v22 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v23 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v24 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v25 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v26 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v27 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v28 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v29 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v30 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v31 : Rivet_riscv_family_encode.Riscv_types.Vreg.t

  val off :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t

  val off_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t

  val cmem :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_ops.Cmem.t

  val label : string -> Rivet_asm_core.Expr.t
  val rne : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val rtz : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val rdn : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val rup : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val rmm : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val dyn : Rivet_riscv_family_encode.Riscv_types.Rm.t

  val add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sub :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sll :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slt :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sltu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val snez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val neg :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val seqz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sltz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sgtz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val zext_b :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xor :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srl :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sra :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val or_ :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val and_ :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mul :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val remu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mulh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mulhsu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mulhu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val div :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val divu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rem :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh1add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh2add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh3add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val min :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val minu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val max :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val maxu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val andn :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val orn :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xnor :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rol :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ror :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bclr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bext :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val binv :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bset :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bclri :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bclri_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bexti :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bexti_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val binvi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val binvi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bseti :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bseti_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ctz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val cpop :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sext_b :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sext_h :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val orc_b :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val brev8 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rev8 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val pack :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val packh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rori :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rori_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val zext_h :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clmul :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clmulh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clmulr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val czero_eqz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val czero_nez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm3p0 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm3p1 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm4ed :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm4ed_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm4ks :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm4ks_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xperm4 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xperm8 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha256sum0 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha256sum1 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha256sig0 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha256sig1 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrw_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrs :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrs_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrc :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrc_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrwi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrwi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrsi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrsi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrci :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrci_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrr_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrw :
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrw_e :
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrs :
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrs_e :
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrc :
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrc_e :
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrwi : int -> int -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrwi_e :
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrsi : int -> int -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrsi_e :
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrci : int -> int -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrci_e :
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoswap_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoadd_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoxor_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoand_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoor_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amomin_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amomax_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amominu_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amomaxu_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lr_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sc_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val addi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val addi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_addi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_addi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_and :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_or :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_xor :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_sub :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_jr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_jalr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_mv :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_ebreak : Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_lw :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_ops.Cmem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_sw :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_ops.Cmem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_lwsp :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_swsp :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_beqz :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_j : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_bnez :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slti :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slti_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sltiu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sltiu_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xori :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xori_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ori :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ori_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val andi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val andi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slli :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slli_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srli :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srli_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srai :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srai_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lb :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lbu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lhu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sb :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val beq :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bne :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val blt :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bge :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bltu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgeu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgtu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bleu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgt :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ble :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val beqz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bnez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bltz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val blez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgtz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lui :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lui_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val auipc :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val auipc_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val jal : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val jalr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mv :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val nop : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val j : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val jr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val call : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val tail : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val la :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lla :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val li :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int64 ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val li_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ret : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val ecall : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val ebreak : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val unimp : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val fence_i : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val fence : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val fence_tso : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val pause : Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fld :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val flw :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsd :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsw :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fadd_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fadd_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsub_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsub_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmul_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmul_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fdiv_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fdiv_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fneg_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fneg_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fabs_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fabs_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_s_x :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnj_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnj_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjn_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjn_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjx_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjx_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmin_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmax_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmin_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmax_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsqrt_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsqrt_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fclass_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fclass_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmadd_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmadd_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmsub_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmsub_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fnmsub_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fnmsub_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fnmadd_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fnmadd_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val feq_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fle_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val flt_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val flt_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val feq_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fle_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_w_x :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_w_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_wu_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_w :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_wu :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_w :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_wu :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_w_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_wu_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t
end

module Rv32 : sig
  module Creg = Rivet_riscv_family_encode.Riscv_types.Creg
  module Cfreg = Rivet_riscv_family_encode.Riscv_types.Cfreg

  val x0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x7 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x8 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x9 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x10 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x11 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x12 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x13 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x14 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x15 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x16 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x17 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x18 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x19 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x20 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x21 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x22 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x23 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x24 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x25 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x26 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x27 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x28 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x29 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x30 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x31 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val zero : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val ra : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val sp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val gp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val tp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val fp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a7 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s7 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s8 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s9 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s10 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s11 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val f0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f8 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f9 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f10 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f11 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f12 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f13 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f14 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f15 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f16 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f17 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f18 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f19 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f20 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f21 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f22 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f23 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f24 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f25 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f26 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f27 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f28 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f29 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f30 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f31 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs8 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs9 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs10 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs11 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft8 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft9 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft10 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft11 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val v0 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v1 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v2 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v3 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v4 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v5 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v6 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v7 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v8 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v9 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v10 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v11 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v12 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v13 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v14 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v15 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v16 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v17 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v18 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v19 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v20 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v21 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v22 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v23 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v24 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v25 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v26 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v27 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v28 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v29 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v30 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v31 : Rivet_riscv_family_encode.Riscv_types.Vreg.t

  val off :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t

  val off_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t

  val cmem :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_ops.Cmem.t

  val label : string -> Rivet_asm_core.Expr.t
  val rne : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val rtz : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val rdn : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val rup : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val rmm : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val dyn : Rivet_riscv_family_encode.Riscv_types.Rm.t

  val add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sub :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sll :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slt :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sltu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val snez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val neg :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val seqz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sltz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sgtz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val zext_b :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xor :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srl :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sra :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val or_ :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val and_ :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mul :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val remu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mulh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mulhsu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mulhu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val div :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val divu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rem :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh1add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh2add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh3add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val min :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val minu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val max :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val maxu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val andn :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val orn :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xnor :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rol :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ror :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bclr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bext :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val binv :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bset :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bclri :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bclri_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bexti :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bexti_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val binvi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val binvi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bseti :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bseti_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ctz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val cpop :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sext_b :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sext_h :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val orc_b :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val brev8 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rev8 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val pack :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val packh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rori :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rori_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val zext_h :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clmul :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clmulh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clmulr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val czero_eqz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val czero_nez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm3p0 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm3p1 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm4ed :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm4ed_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm4ks :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm4ks_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xperm4 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xperm8 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha256sum0 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha256sum1 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha256sig0 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha256sig1 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrw_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrs :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrs_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrc :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrc_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrwi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrwi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrsi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrsi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrci :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrci_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrr_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrw :
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrw_e :
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrs :
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrs_e :
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrc :
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrc_e :
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrwi : int -> int -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrwi_e :
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrsi : int -> int -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrsi_e :
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrci : int -> int -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrci_e :
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoswap_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoadd_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoxor_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoand_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoor_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amomin_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amomax_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amominu_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amomaxu_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lr_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sc_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val addi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val addi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_addi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_addi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_and :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_or :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_xor :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_sub :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_jr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_jalr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_mv :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_ebreak : Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_lw :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_ops.Cmem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_sw :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_ops.Cmem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_lwsp :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_swsp :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_beqz :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_j : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_bnez :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slti :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slti_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sltiu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sltiu_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xori :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xori_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ori :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ori_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val andi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val andi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slli :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slli_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srli :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srli_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srai :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srai_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lb :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lbu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lhu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sb :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val beq :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bne :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val blt :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bge :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bltu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgeu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgtu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bleu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgt :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ble :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val beqz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bnez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bltz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val blez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgtz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lui :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lui_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val auipc :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val auipc_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val jal : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val jalr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mv :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val nop : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val j : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val jr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val call : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val tail : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val la :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lla :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val li :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int64 ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val li_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ret : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val ecall : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val ebreak : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val unimp : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val fence_i : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val fence : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val fence_tso : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val pause : Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fld :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val flw :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsd :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsw :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fadd_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fadd_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsub_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsub_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmul_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmul_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fdiv_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fdiv_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fneg_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fneg_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fabs_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fabs_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_s_x :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnj_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnj_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjn_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjn_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjx_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjx_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmin_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmax_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmin_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmax_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsqrt_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsqrt_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fclass_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fclass_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmadd_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmadd_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmsub_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmsub_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fnmsub_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fnmsub_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fnmadd_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fnmadd_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val feq_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fle_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val flt_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val flt_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val feq_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fle_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_w_x :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_w_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_wu_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_w :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_wu :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_w :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_wu :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_w_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_wu_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  module Rowkit = Riscv_asm_rowkit

  val zip :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val unzip :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha512sum0r :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha512sum1r :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha512sig0l :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha512sig1l :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha512sig0h :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha512sig1h :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes32dsi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes32dsi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes32dsmi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes32dsmi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes32esi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes32esi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes32esmi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes32esmi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_jal : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t
end

module Rv64 : sig
  module Creg = Rivet_riscv_family_encode.Riscv_types.Creg
  module Cfreg = Rivet_riscv_family_encode.Riscv_types.Cfreg

  val x0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x7 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x8 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x9 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x10 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x11 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x12 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x13 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x14 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x15 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x16 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x17 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x18 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x19 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x20 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x21 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x22 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x23 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x24 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x25 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x26 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x27 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x28 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x29 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x30 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val x31 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val zero : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val ra : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val sp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val gp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val tp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val fp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val a7 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s7 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s8 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s9 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s10 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val s11 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val t6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
  val f0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f8 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f9 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f10 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f11 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f12 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f13 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f14 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f15 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f16 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f17 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f18 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f19 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f20 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f21 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f22 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f23 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f24 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f25 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f26 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f27 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f28 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f29 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f30 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val f31 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fa7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs8 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs9 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs10 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val fs11 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft8 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft9 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft10 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val ft11 : Rivet_riscv_family_encode.Riscv_types.Freg.t
  val v0 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v1 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v2 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v3 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v4 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v5 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v6 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v7 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v8 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v9 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v10 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v11 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v12 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v13 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v14 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v15 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v16 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v17 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v18 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v19 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v20 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v21 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v22 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v23 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v24 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v25 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v26 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v27 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v28 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v29 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v30 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
  val v31 : Rivet_riscv_family_encode.Riscv_types.Vreg.t

  val off :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t

  val off_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t

  val cmem :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_ops.Cmem.t

  val label : string -> Rivet_asm_core.Expr.t
  val rne : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val rtz : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val rdn : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val rup : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val rmm : Rivet_riscv_family_encode.Riscv_types.Rm.t
  val dyn : Rivet_riscv_family_encode.Riscv_types.Rm.t

  val add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sub :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sll :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slt :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sltu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val snez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val neg :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val seqz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sltz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sgtz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val zext_b :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xor :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srl :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sra :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val or_ :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val and_ :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mul :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val remu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mulh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mulhsu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mulhu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val div :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val divu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rem :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh1add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh2add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh3add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val min :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val minu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val max :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val maxu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val andn :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val orn :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xnor :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rol :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ror :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bclr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bext :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val binv :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bset :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bclri :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bclri_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bexti :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bexti_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val binvi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val binvi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bseti :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bseti_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ctz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val cpop :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sext_b :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sext_h :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val orc_b :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val brev8 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rev8 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val pack :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val packh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rori :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rori_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val zext_h :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clmul :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clmulh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clmulr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val czero_eqz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val czero_nez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm3p0 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm3p1 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm4ed :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm4ed_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm4ks :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sm4ks_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xperm4 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xperm8 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha256sum0 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha256sum1 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha256sig0 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha256sig1 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrw_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrs :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrs_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrc :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrc_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrwi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrwi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrsi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrsi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrci :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrrci_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrr_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrw :
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrw_e :
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrs :
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrs_e :
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrc :
    int ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrc_e :
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrwi : int -> int -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrwi_e :
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrsi : int -> int -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrsi_e :
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrci : int -> int -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val csrci_e :
    Rivet_asm_core.Expr.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoswap_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoadd_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoxor_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoand_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoor_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amomin_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amomax_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amominu_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amomaxu_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lr_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sc_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val addi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val addi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_addi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_addi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_and :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_or :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_xor :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_sub :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_jr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_jalr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_mv :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_add :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_ebreak : Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_lw :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_ops.Cmem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_sw :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_ops.Cmem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_lwsp :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_swsp :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_beqz :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_j : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_bnez :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slti :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slti_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sltiu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sltiu_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xori :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val xori_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ori :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ori_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val andi :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val andi_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slli :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slli_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srli :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srli_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srai :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srai_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lb :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lbu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lhu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sb :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val beq :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bne :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val blt :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bge :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bltu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgeu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgtu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bleu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgt :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ble :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val beqz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bnez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bltz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val blez :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val bgtz :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lui :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lui_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val auipc :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val auipc_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val jal : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val jalr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mv :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val nop : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val j : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val jr :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val call : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val tail : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val la :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lla :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val li :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int64 ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val li_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ret : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val ecall : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val ebreak : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val unimp : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val fence_i : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val fence : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val fence_tso : Rivet_riscv_family_encode.Riscv_types.Instruction.t
  val pause : Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fld :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val flw :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsd :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsw :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fadd_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fadd_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsub_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsub_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmul_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmul_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fdiv_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fdiv_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fneg_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fneg_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fabs_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fabs_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_s_x :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnj_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnj_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjn_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjn_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjx_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjx_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmin_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmax_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmin_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmax_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsqrt_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fsqrt_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fclass_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fclass_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmadd_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmadd_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmsub_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmsub_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fnmsub_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fnmsub_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fnmadd_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fnmadd_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val feq_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fle_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val flt_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val flt_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val feq_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fle_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_w_x :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_w_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_wu_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_w :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_wu :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_w :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_wu :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_w_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_wu_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_d :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_s :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  module Rowkit = Riscv_asm_rowkit

  val sh1add_uw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh2add_uw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sh3add_uw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val clzw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ctzw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val cpopw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val packw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rolw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val rorw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val roriw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val roriw_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha512sum0 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha512sum1 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha512sig0 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sha512sig1 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes64ds :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes64dsm :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes64es :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes64esm :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes64ks2 :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes64im :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes64ks1i :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val aes64ks1i_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoswap_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoadd_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoxor_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoand_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amoor_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amomin_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amomax_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amominu_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val amomaxu_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lr_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sc_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val addw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val subw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sllw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srlw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sraw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val mulw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val divw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val divuw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val remw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val remuw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_addw :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_subw :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_ld :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_ops.Cmem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_sd :
    Rivet_riscv_family_encode.Riscv_types.Creg.t ->
    Rivet_riscv_family_encode.Riscv_ops.Cmem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_ldsp :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val c_sdsp :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val addiw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val addiw_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slliw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val slliw_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srliw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val srliw_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sraiw :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sraiw_e :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_asm_core.Expr.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sext_w :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val lwu :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val ld :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val sd :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Mem.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_l_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_l :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_lu_d :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_l :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_lu :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_l_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_lu_s :
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_lu :
    Rivet_riscv_family_encode.Riscv_types.Freg.t ->
    Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
    Rivet_riscv_family_encode.Riscv_types.Instruction.t
end

module Creg = Rivet_riscv_family_encode.Riscv_types.Creg
module Cfreg = Rivet_riscv_family_encode.Riscv_types.Cfreg

val x0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x7 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x8 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x9 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x10 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x11 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x12 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x13 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x14 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x15 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x16 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x17 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x18 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x19 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x20 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x21 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x22 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x23 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x24 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x25 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x26 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x27 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x28 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x29 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x30 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val x31 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val zero : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val ra : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val sp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val gp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val tp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val t0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val t1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val t2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val s0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val fp : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val s1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val a0 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val a1 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val a2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val a3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val a4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val a5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val a6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val a7 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val s2 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val s3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val s4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val s5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val s6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val s7 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val s8 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val s9 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val s10 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val s11 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val t3 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val t4 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val t5 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val t6 : Rivet_riscv_family_encode.Riscv_types.Xreg.t
val f0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f8 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f9 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f10 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f11 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f12 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f13 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f14 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f15 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f16 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f17 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f18 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f19 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f20 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f21 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f22 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f23 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f24 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f25 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f26 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f27 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f28 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f29 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f30 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val f31 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val ft0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val ft1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val ft2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val ft3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val ft4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val ft5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val ft6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val ft7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fs0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fs1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fa0 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fa1 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fa2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fa3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fa4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fa5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fa6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fa7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fs2 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fs3 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fs4 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fs5 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fs6 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fs7 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fs8 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fs9 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fs10 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val fs11 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val ft8 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val ft9 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val ft10 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val ft11 : Rivet_riscv_family_encode.Riscv_types.Freg.t
val v0 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v1 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v2 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v3 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v4 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v5 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v6 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v7 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v8 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v9 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v10 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v11 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v12 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v13 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v14 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v15 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v16 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v17 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v18 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v19 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v20 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v21 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v22 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v23 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v24 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v25 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v26 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v27 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v28 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v29 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v30 : Rivet_riscv_family_encode.Riscv_types.Vreg.t
val v31 : Rivet_riscv_family_encode.Riscv_types.Vreg.t

val off :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t -> int -> Rivet_riscv_family_encode.Riscv_types.Mem.t

val off_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t

val cmem :
  Rivet_riscv_family_encode.Riscv_types.Creg.t -> int -> Rivet_riscv_family_encode.Riscv_ops.Cmem.t

val label : string -> Rivet_asm_core.Expr.t
val rne : Rivet_riscv_family_encode.Riscv_types.Rm.t
val rtz : Rivet_riscv_family_encode.Riscv_types.Rm.t
val rdn : Rivet_riscv_family_encode.Riscv_types.Rm.t
val rup : Rivet_riscv_family_encode.Riscv_types.Rm.t
val rmm : Rivet_riscv_family_encode.Riscv_types.Rm.t
val dyn : Rivet_riscv_family_encode.Riscv_types.Rm.t

val add :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sub :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sll :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val slt :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sltu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val snez :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val neg :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val seqz :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sltz :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sgtz :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val zext_b :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val xor :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val srl :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sra :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val or_ :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val and_ :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val mul :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val remu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val mulh :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val mulhsu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val mulhu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val div :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val divu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val rem :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sh1add :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sh2add :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sh3add :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val min :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val minu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val max :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val maxu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val andn :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val orn :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val xnor :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val rol :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val ror :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bclr :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bext :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val binv :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bset :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bclri :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bclri_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bexti :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bexti_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val binvi :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val binvi_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bseti :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bseti_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val clz :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val ctz :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val cpop :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sext_b :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sext_h :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val orc_b :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val brev8 :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val rev8 :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val pack :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val packh :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val rori :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val rori_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val zext_h :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val clmul :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val clmulh :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val clmulr :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val czero_eqz :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val czero_nez :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sm3p0 :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sm3p1 :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sm4ed :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sm4ed_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sm4ks :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sm4ks_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val xperm4 :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val xperm8 :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sha256sum0 :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sha256sum1 :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sha256sig0 :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sha256sig1 :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrrw :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrrw_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrrs :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrrs_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrrc :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrrc_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrrwi :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrrwi_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrrsi :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrrsi_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrrci :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrrci_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrr :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrr_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrw :
  int ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrw_e :
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrs :
  int ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrs_e :
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrc :
  int ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrc_e :
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrwi : int -> int -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrwi_e :
  Rivet_asm_core.Expr.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrsi : int -> int -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrsi_e :
  Rivet_asm_core.Expr.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrci : int -> int -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

val csrci_e :
  Rivet_asm_core.Expr.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val amoswap_w :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val amoadd_w :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val amoxor_w :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val amoand_w :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val amoor_w :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val amomin_w :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val amomax_w :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val amominu_w :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val amomaxu_w :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val lr_w :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sc_w :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val addi :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val addi_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_addi :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_addi_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_and :
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_or :
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_xor :
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_sub :
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_jr :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_jalr :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_mv :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_add :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_ebreak : Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_lw :
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_riscv_family_encode.Riscv_ops.Cmem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_sw :
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_riscv_family_encode.Riscv_ops.Cmem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_lwsp :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_swsp :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_beqz :
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_j : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

val c_bnez :
  Rivet_riscv_family_encode.Riscv_types.Creg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val slti :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val slti_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sltiu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sltiu_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val xori :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val xori_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val ori :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val ori_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val andi :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val andi_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val slli :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val slli_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val srli :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val srli_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val srai :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val srai_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val lb :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val lh :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val lw :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val lbu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val lhu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sb :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sh :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val sw :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val beq :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bne :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val blt :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bge :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bltu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bgeu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bgtu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bleu :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bgt :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val ble :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val beqz :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bnez :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bgez :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bltz :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val blez :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val bgtz :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val lui :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val lui_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val auipc :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val auipc_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val jal : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

val jalr :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val mv :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val nop : Rivet_riscv_family_encode.Riscv_types.Instruction.t
val j : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

val jr :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val call : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t
val tail : Rivet_asm_core.Expr.t -> Rivet_riscv_family_encode.Riscv_types.Instruction.t

val la :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val lla :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val li :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  int64 ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val li_e :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val ret : Rivet_riscv_family_encode.Riscv_types.Instruction.t
val ecall : Rivet_riscv_family_encode.Riscv_types.Instruction.t
val ebreak : Rivet_riscv_family_encode.Riscv_types.Instruction.t
val unimp : Rivet_riscv_family_encode.Riscv_types.Instruction.t
val fence_i : Rivet_riscv_family_encode.Riscv_types.Instruction.t
val fence : Rivet_riscv_family_encode.Riscv_types.Instruction.t
val fence_tso : Rivet_riscv_family_encode.Riscv_types.Instruction.t
val pause : Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fld :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val flw :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fsd :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fsw :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Mem.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fadd_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fadd_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fsub_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fsub_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmul_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmul_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fdiv_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fdiv_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fneg_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fneg_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fabs_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fabs_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmv_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmv_x_s :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmv_s_x :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fsgnj_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fsgnj_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fsgnjn_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fsgnjn_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fsgnjx_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fsgnjx_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmin_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmax_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmin_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmax_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fsqrt_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fsqrt_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fclass_s :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fclass_d :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmadd_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmadd_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmsub_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmsub_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fnmsub_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fnmsub_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fnmadd_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fnmadd_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmv_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val feq_d :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fle_d :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val flt_d :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val flt_s :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val feq_s :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fle_s :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmv_x_w :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fmv_w_x :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fcvt_w_d :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fcvt_wu_d :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fcvt_d_w :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fcvt_d_wu :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fcvt_s_w :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fcvt_s_wu :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fcvt_w_s :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fcvt_wu_s :
  Rivet_riscv_family_encode.Riscv_types.Xreg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fcvt_s_d :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

val fcvt_d_s :
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Freg.t ->
  Rivet_riscv_family_encode.Riscv_types.Instruction.t

module Rowkit = Riscv_asm_rowkit
