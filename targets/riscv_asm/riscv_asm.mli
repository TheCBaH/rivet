(* Typed builders for the hand-written RISC-V instructions, one per opcode and named by its
   mnemonic ([.] becomes [_], a keyword gains [_]): [add x1 x2 x3], [lw a0 (off sp 8)]. Immediates
   are [int] ([int64] for [li]); the [_e] builder of a form with an immediate takes an
   {!Asm_core.Expr.t} instead. [Rv32] and [Rv64] add the builders only one XLEN has. Each builder
   returns the {!Riscv_types.Instruction.t} the encoders lower; lowering still checks ranges and
   enabled extensions. *)

module Common : sig
  module Creg = Riscv_family_encode.Riscv_types.Creg
  module Cfreg = Riscv_family_encode.Riscv_types.Cfreg

  val x0 : Riscv_family_encode.Riscv_types.Xreg.t
  val x1 : Riscv_family_encode.Riscv_types.Xreg.t
  val x2 : Riscv_family_encode.Riscv_types.Xreg.t
  val x3 : Riscv_family_encode.Riscv_types.Xreg.t
  val x4 : Riscv_family_encode.Riscv_types.Xreg.t
  val x5 : Riscv_family_encode.Riscv_types.Xreg.t
  val x6 : Riscv_family_encode.Riscv_types.Xreg.t
  val x7 : Riscv_family_encode.Riscv_types.Xreg.t
  val x8 : Riscv_family_encode.Riscv_types.Xreg.t
  val x9 : Riscv_family_encode.Riscv_types.Xreg.t
  val x10 : Riscv_family_encode.Riscv_types.Xreg.t
  val x11 : Riscv_family_encode.Riscv_types.Xreg.t
  val x12 : Riscv_family_encode.Riscv_types.Xreg.t
  val x13 : Riscv_family_encode.Riscv_types.Xreg.t
  val x14 : Riscv_family_encode.Riscv_types.Xreg.t
  val x15 : Riscv_family_encode.Riscv_types.Xreg.t
  val x16 : Riscv_family_encode.Riscv_types.Xreg.t
  val x17 : Riscv_family_encode.Riscv_types.Xreg.t
  val x18 : Riscv_family_encode.Riscv_types.Xreg.t
  val x19 : Riscv_family_encode.Riscv_types.Xreg.t
  val x20 : Riscv_family_encode.Riscv_types.Xreg.t
  val x21 : Riscv_family_encode.Riscv_types.Xreg.t
  val x22 : Riscv_family_encode.Riscv_types.Xreg.t
  val x23 : Riscv_family_encode.Riscv_types.Xreg.t
  val x24 : Riscv_family_encode.Riscv_types.Xreg.t
  val x25 : Riscv_family_encode.Riscv_types.Xreg.t
  val x26 : Riscv_family_encode.Riscv_types.Xreg.t
  val x27 : Riscv_family_encode.Riscv_types.Xreg.t
  val x28 : Riscv_family_encode.Riscv_types.Xreg.t
  val x29 : Riscv_family_encode.Riscv_types.Xreg.t
  val x30 : Riscv_family_encode.Riscv_types.Xreg.t
  val x31 : Riscv_family_encode.Riscv_types.Xreg.t
  val zero : Riscv_family_encode.Riscv_types.Xreg.t
  val ra : Riscv_family_encode.Riscv_types.Xreg.t
  val sp : Riscv_family_encode.Riscv_types.Xreg.t
  val gp : Riscv_family_encode.Riscv_types.Xreg.t
  val tp : Riscv_family_encode.Riscv_types.Xreg.t
  val t0 : Riscv_family_encode.Riscv_types.Xreg.t
  val t1 : Riscv_family_encode.Riscv_types.Xreg.t
  val t2 : Riscv_family_encode.Riscv_types.Xreg.t
  val s0 : Riscv_family_encode.Riscv_types.Xreg.t
  val fp : Riscv_family_encode.Riscv_types.Xreg.t
  val s1 : Riscv_family_encode.Riscv_types.Xreg.t
  val a0 : Riscv_family_encode.Riscv_types.Xreg.t
  val a1 : Riscv_family_encode.Riscv_types.Xreg.t
  val a2 : Riscv_family_encode.Riscv_types.Xreg.t
  val a3 : Riscv_family_encode.Riscv_types.Xreg.t
  val a4 : Riscv_family_encode.Riscv_types.Xreg.t
  val a5 : Riscv_family_encode.Riscv_types.Xreg.t
  val a6 : Riscv_family_encode.Riscv_types.Xreg.t
  val a7 : Riscv_family_encode.Riscv_types.Xreg.t
  val s2 : Riscv_family_encode.Riscv_types.Xreg.t
  val s3 : Riscv_family_encode.Riscv_types.Xreg.t
  val s4 : Riscv_family_encode.Riscv_types.Xreg.t
  val s5 : Riscv_family_encode.Riscv_types.Xreg.t
  val s6 : Riscv_family_encode.Riscv_types.Xreg.t
  val s7 : Riscv_family_encode.Riscv_types.Xreg.t
  val s8 : Riscv_family_encode.Riscv_types.Xreg.t
  val s9 : Riscv_family_encode.Riscv_types.Xreg.t
  val s10 : Riscv_family_encode.Riscv_types.Xreg.t
  val s11 : Riscv_family_encode.Riscv_types.Xreg.t
  val t3 : Riscv_family_encode.Riscv_types.Xreg.t
  val t4 : Riscv_family_encode.Riscv_types.Xreg.t
  val t5 : Riscv_family_encode.Riscv_types.Xreg.t
  val t6 : Riscv_family_encode.Riscv_types.Xreg.t
  val f0 : Riscv_family_encode.Riscv_types.Freg.t
  val f1 : Riscv_family_encode.Riscv_types.Freg.t
  val f2 : Riscv_family_encode.Riscv_types.Freg.t
  val f3 : Riscv_family_encode.Riscv_types.Freg.t
  val f4 : Riscv_family_encode.Riscv_types.Freg.t
  val f5 : Riscv_family_encode.Riscv_types.Freg.t
  val f6 : Riscv_family_encode.Riscv_types.Freg.t
  val f7 : Riscv_family_encode.Riscv_types.Freg.t
  val f8 : Riscv_family_encode.Riscv_types.Freg.t
  val f9 : Riscv_family_encode.Riscv_types.Freg.t
  val f10 : Riscv_family_encode.Riscv_types.Freg.t
  val f11 : Riscv_family_encode.Riscv_types.Freg.t
  val f12 : Riscv_family_encode.Riscv_types.Freg.t
  val f13 : Riscv_family_encode.Riscv_types.Freg.t
  val f14 : Riscv_family_encode.Riscv_types.Freg.t
  val f15 : Riscv_family_encode.Riscv_types.Freg.t
  val f16 : Riscv_family_encode.Riscv_types.Freg.t
  val f17 : Riscv_family_encode.Riscv_types.Freg.t
  val f18 : Riscv_family_encode.Riscv_types.Freg.t
  val f19 : Riscv_family_encode.Riscv_types.Freg.t
  val f20 : Riscv_family_encode.Riscv_types.Freg.t
  val f21 : Riscv_family_encode.Riscv_types.Freg.t
  val f22 : Riscv_family_encode.Riscv_types.Freg.t
  val f23 : Riscv_family_encode.Riscv_types.Freg.t
  val f24 : Riscv_family_encode.Riscv_types.Freg.t
  val f25 : Riscv_family_encode.Riscv_types.Freg.t
  val f26 : Riscv_family_encode.Riscv_types.Freg.t
  val f27 : Riscv_family_encode.Riscv_types.Freg.t
  val f28 : Riscv_family_encode.Riscv_types.Freg.t
  val f29 : Riscv_family_encode.Riscv_types.Freg.t
  val f30 : Riscv_family_encode.Riscv_types.Freg.t
  val f31 : Riscv_family_encode.Riscv_types.Freg.t
  val ft0 : Riscv_family_encode.Riscv_types.Freg.t
  val ft1 : Riscv_family_encode.Riscv_types.Freg.t
  val ft2 : Riscv_family_encode.Riscv_types.Freg.t
  val ft3 : Riscv_family_encode.Riscv_types.Freg.t
  val ft4 : Riscv_family_encode.Riscv_types.Freg.t
  val ft5 : Riscv_family_encode.Riscv_types.Freg.t
  val ft6 : Riscv_family_encode.Riscv_types.Freg.t
  val ft7 : Riscv_family_encode.Riscv_types.Freg.t
  val fs0 : Riscv_family_encode.Riscv_types.Freg.t
  val fs1 : Riscv_family_encode.Riscv_types.Freg.t
  val fa0 : Riscv_family_encode.Riscv_types.Freg.t
  val fa1 : Riscv_family_encode.Riscv_types.Freg.t
  val fa2 : Riscv_family_encode.Riscv_types.Freg.t
  val fa3 : Riscv_family_encode.Riscv_types.Freg.t
  val fa4 : Riscv_family_encode.Riscv_types.Freg.t
  val fa5 : Riscv_family_encode.Riscv_types.Freg.t
  val fa6 : Riscv_family_encode.Riscv_types.Freg.t
  val fa7 : Riscv_family_encode.Riscv_types.Freg.t
  val fs2 : Riscv_family_encode.Riscv_types.Freg.t
  val fs3 : Riscv_family_encode.Riscv_types.Freg.t
  val fs4 : Riscv_family_encode.Riscv_types.Freg.t
  val fs5 : Riscv_family_encode.Riscv_types.Freg.t
  val fs6 : Riscv_family_encode.Riscv_types.Freg.t
  val fs7 : Riscv_family_encode.Riscv_types.Freg.t
  val fs8 : Riscv_family_encode.Riscv_types.Freg.t
  val fs9 : Riscv_family_encode.Riscv_types.Freg.t
  val fs10 : Riscv_family_encode.Riscv_types.Freg.t
  val fs11 : Riscv_family_encode.Riscv_types.Freg.t
  val ft8 : Riscv_family_encode.Riscv_types.Freg.t
  val ft9 : Riscv_family_encode.Riscv_types.Freg.t
  val ft10 : Riscv_family_encode.Riscv_types.Freg.t
  val ft11 : Riscv_family_encode.Riscv_types.Freg.t
  val v0 : Riscv_family_encode.Riscv_types.Vreg.t
  val v1 : Riscv_family_encode.Riscv_types.Vreg.t
  val v2 : Riscv_family_encode.Riscv_types.Vreg.t
  val v3 : Riscv_family_encode.Riscv_types.Vreg.t
  val v4 : Riscv_family_encode.Riscv_types.Vreg.t
  val v5 : Riscv_family_encode.Riscv_types.Vreg.t
  val v6 : Riscv_family_encode.Riscv_types.Vreg.t
  val v7 : Riscv_family_encode.Riscv_types.Vreg.t
  val v8 : Riscv_family_encode.Riscv_types.Vreg.t
  val v9 : Riscv_family_encode.Riscv_types.Vreg.t
  val v10 : Riscv_family_encode.Riscv_types.Vreg.t
  val v11 : Riscv_family_encode.Riscv_types.Vreg.t
  val v12 : Riscv_family_encode.Riscv_types.Vreg.t
  val v13 : Riscv_family_encode.Riscv_types.Vreg.t
  val v14 : Riscv_family_encode.Riscv_types.Vreg.t
  val v15 : Riscv_family_encode.Riscv_types.Vreg.t
  val v16 : Riscv_family_encode.Riscv_types.Vreg.t
  val v17 : Riscv_family_encode.Riscv_types.Vreg.t
  val v18 : Riscv_family_encode.Riscv_types.Vreg.t
  val v19 : Riscv_family_encode.Riscv_types.Vreg.t
  val v20 : Riscv_family_encode.Riscv_types.Vreg.t
  val v21 : Riscv_family_encode.Riscv_types.Vreg.t
  val v22 : Riscv_family_encode.Riscv_types.Vreg.t
  val v23 : Riscv_family_encode.Riscv_types.Vreg.t
  val v24 : Riscv_family_encode.Riscv_types.Vreg.t
  val v25 : Riscv_family_encode.Riscv_types.Vreg.t
  val v26 : Riscv_family_encode.Riscv_types.Vreg.t
  val v27 : Riscv_family_encode.Riscv_types.Vreg.t
  val v28 : Riscv_family_encode.Riscv_types.Vreg.t
  val v29 : Riscv_family_encode.Riscv_types.Vreg.t
  val v30 : Riscv_family_encode.Riscv_types.Vreg.t
  val v31 : Riscv_family_encode.Riscv_types.Vreg.t
  val off : Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Mem.t

  val off_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Mem.t

  val cmem : Riscv_family_encode.Riscv_types.Creg.t -> int -> Riscv_family_encode.Riscv_ops.Cmem.t
  val label : string -> Asm_core.Expr.t
  val rne : Riscv_family_encode.Riscv_types.Rm.t
  val rtz : Riscv_family_encode.Riscv_types.Rm.t
  val rdn : Riscv_family_encode.Riscv_types.Rm.t
  val rup : Riscv_family_encode.Riscv_types.Rm.t
  val rmm : Riscv_family_encode.Riscv_types.Rm.t
  val dyn : Riscv_family_encode.Riscv_types.Rm.t

  val add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sub :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sll :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slt :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sltu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val snez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val neg :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val seqz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sltz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sgtz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val zext_b :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xor :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srl :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sra :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val or_ :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val and_ :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mul :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val remu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mulh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mulhsu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mulhu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val div :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val divu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rem :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh1add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh2add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh3add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val min :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val minu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val max :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val maxu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val andn :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val orn :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xnor :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rol :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ror :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bclr :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bext :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val binv :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bset :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bclri :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bclri_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bexti :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bexti_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val binvi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val binvi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bseti :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bseti_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ctz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val cpop :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sext_b :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sext_h :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val orc_b :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val brev8 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rev8 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val pack :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val packh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rori :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rori_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val zext_h :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clmul :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clmulh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clmulr :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val czero_eqz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val czero_nez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm3p0 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm3p1 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm4ed :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm4ed_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm4ks :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm4ks_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xperm4 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xperm8 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha256sum0 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha256sum1 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha256sig0 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha256sig1 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrw_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrs :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrs_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrc :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrc_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrwi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrwi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrsi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrsi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrci :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrci_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrr :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val csrr_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrw :
    int -> Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val csrw_e :
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrs :
    int -> Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val csrs_e :
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrc :
    int -> Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val csrc_e :
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrwi : int -> int -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrwi_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrsi : int -> int -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrsi_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrci : int -> int -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrci_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val amoswap_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoadd_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoxor_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoand_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoor_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amomin_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amomax_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amominu_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amomaxu_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lr_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sc_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val addi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val addi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_addi :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_addi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_and :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_or :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_xor :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_sub :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_jr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_jalr :
    Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_mv :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_ebreak : Riscv_family_encode.Riscv_types.Instruction.t

  val c_lw :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_ops.Cmem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_sw :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_ops.Cmem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_lwsp :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_swsp :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_beqz :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_j : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_bnez :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slti :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slti_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sltiu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sltiu_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xori :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xori_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ori :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ori_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val andi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val andi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slli :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slli_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srli :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srli_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srai :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srai_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lb :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lbu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lhu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sb :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val beq :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bne :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val blt :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bge :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bltu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgeu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgtu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bleu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgt :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ble :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val beqz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bnez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bltz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val blez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgtz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lui :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val lui_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val auipc :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val auipc_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val jal : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val jalr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val mv :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val nop : Riscv_family_encode.Riscv_types.Instruction.t
  val j : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val jr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val call : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val tail : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val la :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lla :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val li :
    Riscv_family_encode.Riscv_types.Xreg.t -> int64 -> Riscv_family_encode.Riscv_types.Instruction.t

  val li_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ret : Riscv_family_encode.Riscv_types.Instruction.t
  val ecall : Riscv_family_encode.Riscv_types.Instruction.t
  val ebreak : Riscv_family_encode.Riscv_types.Instruction.t
  val unimp : Riscv_family_encode.Riscv_types.Instruction.t
  val fence_i : Riscv_family_encode.Riscv_types.Instruction.t
  val fence : Riscv_family_encode.Riscv_types.Instruction.t
  val fence_tso : Riscv_family_encode.Riscv_types.Instruction.t
  val pause : Riscv_family_encode.Riscv_types.Instruction.t

  val fld :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val flw :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsd :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsw :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fadd_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fadd_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsub_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsub_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmul_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmul_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fdiv_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fdiv_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fneg_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fneg_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fabs_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fabs_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_s_x :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnj_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnj_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjn_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjn_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjx_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjx_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmin_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmax_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmin_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmax_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsqrt_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsqrt_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fclass_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fclass_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmadd_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmadd_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmsub_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmsub_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fnmsub_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fnmsub_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fnmadd_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fnmadd_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val feq_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fle_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val flt_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val flt_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val feq_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fle_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_w_x :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_w_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_wu_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_w :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_wu :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_w :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_wu :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_w_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_wu_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t
end

module Rv32 : sig
  module Creg = Riscv_family_encode.Riscv_types.Creg
  module Cfreg = Riscv_family_encode.Riscv_types.Cfreg

  val x0 : Riscv_family_encode.Riscv_types.Xreg.t
  val x1 : Riscv_family_encode.Riscv_types.Xreg.t
  val x2 : Riscv_family_encode.Riscv_types.Xreg.t
  val x3 : Riscv_family_encode.Riscv_types.Xreg.t
  val x4 : Riscv_family_encode.Riscv_types.Xreg.t
  val x5 : Riscv_family_encode.Riscv_types.Xreg.t
  val x6 : Riscv_family_encode.Riscv_types.Xreg.t
  val x7 : Riscv_family_encode.Riscv_types.Xreg.t
  val x8 : Riscv_family_encode.Riscv_types.Xreg.t
  val x9 : Riscv_family_encode.Riscv_types.Xreg.t
  val x10 : Riscv_family_encode.Riscv_types.Xreg.t
  val x11 : Riscv_family_encode.Riscv_types.Xreg.t
  val x12 : Riscv_family_encode.Riscv_types.Xreg.t
  val x13 : Riscv_family_encode.Riscv_types.Xreg.t
  val x14 : Riscv_family_encode.Riscv_types.Xreg.t
  val x15 : Riscv_family_encode.Riscv_types.Xreg.t
  val x16 : Riscv_family_encode.Riscv_types.Xreg.t
  val x17 : Riscv_family_encode.Riscv_types.Xreg.t
  val x18 : Riscv_family_encode.Riscv_types.Xreg.t
  val x19 : Riscv_family_encode.Riscv_types.Xreg.t
  val x20 : Riscv_family_encode.Riscv_types.Xreg.t
  val x21 : Riscv_family_encode.Riscv_types.Xreg.t
  val x22 : Riscv_family_encode.Riscv_types.Xreg.t
  val x23 : Riscv_family_encode.Riscv_types.Xreg.t
  val x24 : Riscv_family_encode.Riscv_types.Xreg.t
  val x25 : Riscv_family_encode.Riscv_types.Xreg.t
  val x26 : Riscv_family_encode.Riscv_types.Xreg.t
  val x27 : Riscv_family_encode.Riscv_types.Xreg.t
  val x28 : Riscv_family_encode.Riscv_types.Xreg.t
  val x29 : Riscv_family_encode.Riscv_types.Xreg.t
  val x30 : Riscv_family_encode.Riscv_types.Xreg.t
  val x31 : Riscv_family_encode.Riscv_types.Xreg.t
  val zero : Riscv_family_encode.Riscv_types.Xreg.t
  val ra : Riscv_family_encode.Riscv_types.Xreg.t
  val sp : Riscv_family_encode.Riscv_types.Xreg.t
  val gp : Riscv_family_encode.Riscv_types.Xreg.t
  val tp : Riscv_family_encode.Riscv_types.Xreg.t
  val t0 : Riscv_family_encode.Riscv_types.Xreg.t
  val t1 : Riscv_family_encode.Riscv_types.Xreg.t
  val t2 : Riscv_family_encode.Riscv_types.Xreg.t
  val s0 : Riscv_family_encode.Riscv_types.Xreg.t
  val fp : Riscv_family_encode.Riscv_types.Xreg.t
  val s1 : Riscv_family_encode.Riscv_types.Xreg.t
  val a0 : Riscv_family_encode.Riscv_types.Xreg.t
  val a1 : Riscv_family_encode.Riscv_types.Xreg.t
  val a2 : Riscv_family_encode.Riscv_types.Xreg.t
  val a3 : Riscv_family_encode.Riscv_types.Xreg.t
  val a4 : Riscv_family_encode.Riscv_types.Xreg.t
  val a5 : Riscv_family_encode.Riscv_types.Xreg.t
  val a6 : Riscv_family_encode.Riscv_types.Xreg.t
  val a7 : Riscv_family_encode.Riscv_types.Xreg.t
  val s2 : Riscv_family_encode.Riscv_types.Xreg.t
  val s3 : Riscv_family_encode.Riscv_types.Xreg.t
  val s4 : Riscv_family_encode.Riscv_types.Xreg.t
  val s5 : Riscv_family_encode.Riscv_types.Xreg.t
  val s6 : Riscv_family_encode.Riscv_types.Xreg.t
  val s7 : Riscv_family_encode.Riscv_types.Xreg.t
  val s8 : Riscv_family_encode.Riscv_types.Xreg.t
  val s9 : Riscv_family_encode.Riscv_types.Xreg.t
  val s10 : Riscv_family_encode.Riscv_types.Xreg.t
  val s11 : Riscv_family_encode.Riscv_types.Xreg.t
  val t3 : Riscv_family_encode.Riscv_types.Xreg.t
  val t4 : Riscv_family_encode.Riscv_types.Xreg.t
  val t5 : Riscv_family_encode.Riscv_types.Xreg.t
  val t6 : Riscv_family_encode.Riscv_types.Xreg.t
  val f0 : Riscv_family_encode.Riscv_types.Freg.t
  val f1 : Riscv_family_encode.Riscv_types.Freg.t
  val f2 : Riscv_family_encode.Riscv_types.Freg.t
  val f3 : Riscv_family_encode.Riscv_types.Freg.t
  val f4 : Riscv_family_encode.Riscv_types.Freg.t
  val f5 : Riscv_family_encode.Riscv_types.Freg.t
  val f6 : Riscv_family_encode.Riscv_types.Freg.t
  val f7 : Riscv_family_encode.Riscv_types.Freg.t
  val f8 : Riscv_family_encode.Riscv_types.Freg.t
  val f9 : Riscv_family_encode.Riscv_types.Freg.t
  val f10 : Riscv_family_encode.Riscv_types.Freg.t
  val f11 : Riscv_family_encode.Riscv_types.Freg.t
  val f12 : Riscv_family_encode.Riscv_types.Freg.t
  val f13 : Riscv_family_encode.Riscv_types.Freg.t
  val f14 : Riscv_family_encode.Riscv_types.Freg.t
  val f15 : Riscv_family_encode.Riscv_types.Freg.t
  val f16 : Riscv_family_encode.Riscv_types.Freg.t
  val f17 : Riscv_family_encode.Riscv_types.Freg.t
  val f18 : Riscv_family_encode.Riscv_types.Freg.t
  val f19 : Riscv_family_encode.Riscv_types.Freg.t
  val f20 : Riscv_family_encode.Riscv_types.Freg.t
  val f21 : Riscv_family_encode.Riscv_types.Freg.t
  val f22 : Riscv_family_encode.Riscv_types.Freg.t
  val f23 : Riscv_family_encode.Riscv_types.Freg.t
  val f24 : Riscv_family_encode.Riscv_types.Freg.t
  val f25 : Riscv_family_encode.Riscv_types.Freg.t
  val f26 : Riscv_family_encode.Riscv_types.Freg.t
  val f27 : Riscv_family_encode.Riscv_types.Freg.t
  val f28 : Riscv_family_encode.Riscv_types.Freg.t
  val f29 : Riscv_family_encode.Riscv_types.Freg.t
  val f30 : Riscv_family_encode.Riscv_types.Freg.t
  val f31 : Riscv_family_encode.Riscv_types.Freg.t
  val ft0 : Riscv_family_encode.Riscv_types.Freg.t
  val ft1 : Riscv_family_encode.Riscv_types.Freg.t
  val ft2 : Riscv_family_encode.Riscv_types.Freg.t
  val ft3 : Riscv_family_encode.Riscv_types.Freg.t
  val ft4 : Riscv_family_encode.Riscv_types.Freg.t
  val ft5 : Riscv_family_encode.Riscv_types.Freg.t
  val ft6 : Riscv_family_encode.Riscv_types.Freg.t
  val ft7 : Riscv_family_encode.Riscv_types.Freg.t
  val fs0 : Riscv_family_encode.Riscv_types.Freg.t
  val fs1 : Riscv_family_encode.Riscv_types.Freg.t
  val fa0 : Riscv_family_encode.Riscv_types.Freg.t
  val fa1 : Riscv_family_encode.Riscv_types.Freg.t
  val fa2 : Riscv_family_encode.Riscv_types.Freg.t
  val fa3 : Riscv_family_encode.Riscv_types.Freg.t
  val fa4 : Riscv_family_encode.Riscv_types.Freg.t
  val fa5 : Riscv_family_encode.Riscv_types.Freg.t
  val fa6 : Riscv_family_encode.Riscv_types.Freg.t
  val fa7 : Riscv_family_encode.Riscv_types.Freg.t
  val fs2 : Riscv_family_encode.Riscv_types.Freg.t
  val fs3 : Riscv_family_encode.Riscv_types.Freg.t
  val fs4 : Riscv_family_encode.Riscv_types.Freg.t
  val fs5 : Riscv_family_encode.Riscv_types.Freg.t
  val fs6 : Riscv_family_encode.Riscv_types.Freg.t
  val fs7 : Riscv_family_encode.Riscv_types.Freg.t
  val fs8 : Riscv_family_encode.Riscv_types.Freg.t
  val fs9 : Riscv_family_encode.Riscv_types.Freg.t
  val fs10 : Riscv_family_encode.Riscv_types.Freg.t
  val fs11 : Riscv_family_encode.Riscv_types.Freg.t
  val ft8 : Riscv_family_encode.Riscv_types.Freg.t
  val ft9 : Riscv_family_encode.Riscv_types.Freg.t
  val ft10 : Riscv_family_encode.Riscv_types.Freg.t
  val ft11 : Riscv_family_encode.Riscv_types.Freg.t
  val v0 : Riscv_family_encode.Riscv_types.Vreg.t
  val v1 : Riscv_family_encode.Riscv_types.Vreg.t
  val v2 : Riscv_family_encode.Riscv_types.Vreg.t
  val v3 : Riscv_family_encode.Riscv_types.Vreg.t
  val v4 : Riscv_family_encode.Riscv_types.Vreg.t
  val v5 : Riscv_family_encode.Riscv_types.Vreg.t
  val v6 : Riscv_family_encode.Riscv_types.Vreg.t
  val v7 : Riscv_family_encode.Riscv_types.Vreg.t
  val v8 : Riscv_family_encode.Riscv_types.Vreg.t
  val v9 : Riscv_family_encode.Riscv_types.Vreg.t
  val v10 : Riscv_family_encode.Riscv_types.Vreg.t
  val v11 : Riscv_family_encode.Riscv_types.Vreg.t
  val v12 : Riscv_family_encode.Riscv_types.Vreg.t
  val v13 : Riscv_family_encode.Riscv_types.Vreg.t
  val v14 : Riscv_family_encode.Riscv_types.Vreg.t
  val v15 : Riscv_family_encode.Riscv_types.Vreg.t
  val v16 : Riscv_family_encode.Riscv_types.Vreg.t
  val v17 : Riscv_family_encode.Riscv_types.Vreg.t
  val v18 : Riscv_family_encode.Riscv_types.Vreg.t
  val v19 : Riscv_family_encode.Riscv_types.Vreg.t
  val v20 : Riscv_family_encode.Riscv_types.Vreg.t
  val v21 : Riscv_family_encode.Riscv_types.Vreg.t
  val v22 : Riscv_family_encode.Riscv_types.Vreg.t
  val v23 : Riscv_family_encode.Riscv_types.Vreg.t
  val v24 : Riscv_family_encode.Riscv_types.Vreg.t
  val v25 : Riscv_family_encode.Riscv_types.Vreg.t
  val v26 : Riscv_family_encode.Riscv_types.Vreg.t
  val v27 : Riscv_family_encode.Riscv_types.Vreg.t
  val v28 : Riscv_family_encode.Riscv_types.Vreg.t
  val v29 : Riscv_family_encode.Riscv_types.Vreg.t
  val v30 : Riscv_family_encode.Riscv_types.Vreg.t
  val v31 : Riscv_family_encode.Riscv_types.Vreg.t
  val off : Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Mem.t

  val off_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Mem.t

  val cmem : Riscv_family_encode.Riscv_types.Creg.t -> int -> Riscv_family_encode.Riscv_ops.Cmem.t
  val label : string -> Asm_core.Expr.t
  val rne : Riscv_family_encode.Riscv_types.Rm.t
  val rtz : Riscv_family_encode.Riscv_types.Rm.t
  val rdn : Riscv_family_encode.Riscv_types.Rm.t
  val rup : Riscv_family_encode.Riscv_types.Rm.t
  val rmm : Riscv_family_encode.Riscv_types.Rm.t
  val dyn : Riscv_family_encode.Riscv_types.Rm.t

  val add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sub :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sll :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slt :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sltu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val snez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val neg :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val seqz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sltz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sgtz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val zext_b :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xor :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srl :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sra :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val or_ :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val and_ :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mul :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val remu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mulh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mulhsu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mulhu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val div :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val divu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rem :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh1add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh2add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh3add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val min :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val minu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val max :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val maxu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val andn :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val orn :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xnor :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rol :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ror :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bclr :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bext :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val binv :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bset :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bclri :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bclri_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bexti :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bexti_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val binvi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val binvi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bseti :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bseti_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ctz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val cpop :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sext_b :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sext_h :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val orc_b :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val brev8 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rev8 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val pack :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val packh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rori :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rori_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val zext_h :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clmul :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clmulh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clmulr :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val czero_eqz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val czero_nez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm3p0 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm3p1 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm4ed :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm4ed_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm4ks :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm4ks_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xperm4 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xperm8 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha256sum0 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha256sum1 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha256sig0 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha256sig1 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrw_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrs :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrs_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrc :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrc_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrwi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrwi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrsi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrsi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrci :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrci_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrr :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val csrr_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrw :
    int -> Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val csrw_e :
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrs :
    int -> Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val csrs_e :
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrc :
    int -> Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val csrc_e :
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrwi : int -> int -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrwi_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrsi : int -> int -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrsi_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrci : int -> int -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrci_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val amoswap_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoadd_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoxor_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoand_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoor_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amomin_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amomax_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amominu_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amomaxu_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lr_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sc_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val addi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val addi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_addi :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_addi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_and :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_or :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_xor :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_sub :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_jr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_jalr :
    Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_mv :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_ebreak : Riscv_family_encode.Riscv_types.Instruction.t

  val c_lw :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_ops.Cmem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_sw :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_ops.Cmem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_lwsp :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_swsp :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_beqz :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_j : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_bnez :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slti :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slti_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sltiu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sltiu_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xori :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xori_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ori :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ori_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val andi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val andi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slli :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slli_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srli :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srli_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srai :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srai_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lb :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lbu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lhu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sb :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val beq :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bne :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val blt :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bge :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bltu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgeu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgtu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bleu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgt :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ble :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val beqz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bnez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bltz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val blez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgtz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lui :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val lui_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val auipc :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val auipc_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val jal : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val jalr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val mv :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val nop : Riscv_family_encode.Riscv_types.Instruction.t
  val j : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val jr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val call : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val tail : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val la :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lla :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val li :
    Riscv_family_encode.Riscv_types.Xreg.t -> int64 -> Riscv_family_encode.Riscv_types.Instruction.t

  val li_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ret : Riscv_family_encode.Riscv_types.Instruction.t
  val ecall : Riscv_family_encode.Riscv_types.Instruction.t
  val ebreak : Riscv_family_encode.Riscv_types.Instruction.t
  val unimp : Riscv_family_encode.Riscv_types.Instruction.t
  val fence_i : Riscv_family_encode.Riscv_types.Instruction.t
  val fence : Riscv_family_encode.Riscv_types.Instruction.t
  val fence_tso : Riscv_family_encode.Riscv_types.Instruction.t
  val pause : Riscv_family_encode.Riscv_types.Instruction.t

  val fld :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val flw :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsd :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsw :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fadd_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fadd_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsub_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsub_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmul_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmul_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fdiv_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fdiv_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fneg_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fneg_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fabs_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fabs_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_s_x :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnj_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnj_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjn_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjn_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjx_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjx_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmin_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmax_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmin_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmax_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsqrt_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsqrt_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fclass_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fclass_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmadd_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmadd_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmsub_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmsub_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fnmsub_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fnmsub_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fnmadd_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fnmadd_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val feq_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fle_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val flt_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val flt_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val feq_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fle_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_w_x :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_w_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_wu_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_w :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_wu :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_w :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_wu :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_w_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_wu_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  module Rowkit = Riscv_asm_rowkit

  val zip :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val unzip :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha512sum0r :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha512sum1r :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha512sig0l :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha512sig1l :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha512sig0h :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha512sig1h :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes32dsi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes32dsi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes32dsmi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes32dsmi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes32esi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes32esi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes32esmi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes32esmi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_jal : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
end

module Rv64 : sig
  module Creg = Riscv_family_encode.Riscv_types.Creg
  module Cfreg = Riscv_family_encode.Riscv_types.Cfreg

  val x0 : Riscv_family_encode.Riscv_types.Xreg.t
  val x1 : Riscv_family_encode.Riscv_types.Xreg.t
  val x2 : Riscv_family_encode.Riscv_types.Xreg.t
  val x3 : Riscv_family_encode.Riscv_types.Xreg.t
  val x4 : Riscv_family_encode.Riscv_types.Xreg.t
  val x5 : Riscv_family_encode.Riscv_types.Xreg.t
  val x6 : Riscv_family_encode.Riscv_types.Xreg.t
  val x7 : Riscv_family_encode.Riscv_types.Xreg.t
  val x8 : Riscv_family_encode.Riscv_types.Xreg.t
  val x9 : Riscv_family_encode.Riscv_types.Xreg.t
  val x10 : Riscv_family_encode.Riscv_types.Xreg.t
  val x11 : Riscv_family_encode.Riscv_types.Xreg.t
  val x12 : Riscv_family_encode.Riscv_types.Xreg.t
  val x13 : Riscv_family_encode.Riscv_types.Xreg.t
  val x14 : Riscv_family_encode.Riscv_types.Xreg.t
  val x15 : Riscv_family_encode.Riscv_types.Xreg.t
  val x16 : Riscv_family_encode.Riscv_types.Xreg.t
  val x17 : Riscv_family_encode.Riscv_types.Xreg.t
  val x18 : Riscv_family_encode.Riscv_types.Xreg.t
  val x19 : Riscv_family_encode.Riscv_types.Xreg.t
  val x20 : Riscv_family_encode.Riscv_types.Xreg.t
  val x21 : Riscv_family_encode.Riscv_types.Xreg.t
  val x22 : Riscv_family_encode.Riscv_types.Xreg.t
  val x23 : Riscv_family_encode.Riscv_types.Xreg.t
  val x24 : Riscv_family_encode.Riscv_types.Xreg.t
  val x25 : Riscv_family_encode.Riscv_types.Xreg.t
  val x26 : Riscv_family_encode.Riscv_types.Xreg.t
  val x27 : Riscv_family_encode.Riscv_types.Xreg.t
  val x28 : Riscv_family_encode.Riscv_types.Xreg.t
  val x29 : Riscv_family_encode.Riscv_types.Xreg.t
  val x30 : Riscv_family_encode.Riscv_types.Xreg.t
  val x31 : Riscv_family_encode.Riscv_types.Xreg.t
  val zero : Riscv_family_encode.Riscv_types.Xreg.t
  val ra : Riscv_family_encode.Riscv_types.Xreg.t
  val sp : Riscv_family_encode.Riscv_types.Xreg.t
  val gp : Riscv_family_encode.Riscv_types.Xreg.t
  val tp : Riscv_family_encode.Riscv_types.Xreg.t
  val t0 : Riscv_family_encode.Riscv_types.Xreg.t
  val t1 : Riscv_family_encode.Riscv_types.Xreg.t
  val t2 : Riscv_family_encode.Riscv_types.Xreg.t
  val s0 : Riscv_family_encode.Riscv_types.Xreg.t
  val fp : Riscv_family_encode.Riscv_types.Xreg.t
  val s1 : Riscv_family_encode.Riscv_types.Xreg.t
  val a0 : Riscv_family_encode.Riscv_types.Xreg.t
  val a1 : Riscv_family_encode.Riscv_types.Xreg.t
  val a2 : Riscv_family_encode.Riscv_types.Xreg.t
  val a3 : Riscv_family_encode.Riscv_types.Xreg.t
  val a4 : Riscv_family_encode.Riscv_types.Xreg.t
  val a5 : Riscv_family_encode.Riscv_types.Xreg.t
  val a6 : Riscv_family_encode.Riscv_types.Xreg.t
  val a7 : Riscv_family_encode.Riscv_types.Xreg.t
  val s2 : Riscv_family_encode.Riscv_types.Xreg.t
  val s3 : Riscv_family_encode.Riscv_types.Xreg.t
  val s4 : Riscv_family_encode.Riscv_types.Xreg.t
  val s5 : Riscv_family_encode.Riscv_types.Xreg.t
  val s6 : Riscv_family_encode.Riscv_types.Xreg.t
  val s7 : Riscv_family_encode.Riscv_types.Xreg.t
  val s8 : Riscv_family_encode.Riscv_types.Xreg.t
  val s9 : Riscv_family_encode.Riscv_types.Xreg.t
  val s10 : Riscv_family_encode.Riscv_types.Xreg.t
  val s11 : Riscv_family_encode.Riscv_types.Xreg.t
  val t3 : Riscv_family_encode.Riscv_types.Xreg.t
  val t4 : Riscv_family_encode.Riscv_types.Xreg.t
  val t5 : Riscv_family_encode.Riscv_types.Xreg.t
  val t6 : Riscv_family_encode.Riscv_types.Xreg.t
  val f0 : Riscv_family_encode.Riscv_types.Freg.t
  val f1 : Riscv_family_encode.Riscv_types.Freg.t
  val f2 : Riscv_family_encode.Riscv_types.Freg.t
  val f3 : Riscv_family_encode.Riscv_types.Freg.t
  val f4 : Riscv_family_encode.Riscv_types.Freg.t
  val f5 : Riscv_family_encode.Riscv_types.Freg.t
  val f6 : Riscv_family_encode.Riscv_types.Freg.t
  val f7 : Riscv_family_encode.Riscv_types.Freg.t
  val f8 : Riscv_family_encode.Riscv_types.Freg.t
  val f9 : Riscv_family_encode.Riscv_types.Freg.t
  val f10 : Riscv_family_encode.Riscv_types.Freg.t
  val f11 : Riscv_family_encode.Riscv_types.Freg.t
  val f12 : Riscv_family_encode.Riscv_types.Freg.t
  val f13 : Riscv_family_encode.Riscv_types.Freg.t
  val f14 : Riscv_family_encode.Riscv_types.Freg.t
  val f15 : Riscv_family_encode.Riscv_types.Freg.t
  val f16 : Riscv_family_encode.Riscv_types.Freg.t
  val f17 : Riscv_family_encode.Riscv_types.Freg.t
  val f18 : Riscv_family_encode.Riscv_types.Freg.t
  val f19 : Riscv_family_encode.Riscv_types.Freg.t
  val f20 : Riscv_family_encode.Riscv_types.Freg.t
  val f21 : Riscv_family_encode.Riscv_types.Freg.t
  val f22 : Riscv_family_encode.Riscv_types.Freg.t
  val f23 : Riscv_family_encode.Riscv_types.Freg.t
  val f24 : Riscv_family_encode.Riscv_types.Freg.t
  val f25 : Riscv_family_encode.Riscv_types.Freg.t
  val f26 : Riscv_family_encode.Riscv_types.Freg.t
  val f27 : Riscv_family_encode.Riscv_types.Freg.t
  val f28 : Riscv_family_encode.Riscv_types.Freg.t
  val f29 : Riscv_family_encode.Riscv_types.Freg.t
  val f30 : Riscv_family_encode.Riscv_types.Freg.t
  val f31 : Riscv_family_encode.Riscv_types.Freg.t
  val ft0 : Riscv_family_encode.Riscv_types.Freg.t
  val ft1 : Riscv_family_encode.Riscv_types.Freg.t
  val ft2 : Riscv_family_encode.Riscv_types.Freg.t
  val ft3 : Riscv_family_encode.Riscv_types.Freg.t
  val ft4 : Riscv_family_encode.Riscv_types.Freg.t
  val ft5 : Riscv_family_encode.Riscv_types.Freg.t
  val ft6 : Riscv_family_encode.Riscv_types.Freg.t
  val ft7 : Riscv_family_encode.Riscv_types.Freg.t
  val fs0 : Riscv_family_encode.Riscv_types.Freg.t
  val fs1 : Riscv_family_encode.Riscv_types.Freg.t
  val fa0 : Riscv_family_encode.Riscv_types.Freg.t
  val fa1 : Riscv_family_encode.Riscv_types.Freg.t
  val fa2 : Riscv_family_encode.Riscv_types.Freg.t
  val fa3 : Riscv_family_encode.Riscv_types.Freg.t
  val fa4 : Riscv_family_encode.Riscv_types.Freg.t
  val fa5 : Riscv_family_encode.Riscv_types.Freg.t
  val fa6 : Riscv_family_encode.Riscv_types.Freg.t
  val fa7 : Riscv_family_encode.Riscv_types.Freg.t
  val fs2 : Riscv_family_encode.Riscv_types.Freg.t
  val fs3 : Riscv_family_encode.Riscv_types.Freg.t
  val fs4 : Riscv_family_encode.Riscv_types.Freg.t
  val fs5 : Riscv_family_encode.Riscv_types.Freg.t
  val fs6 : Riscv_family_encode.Riscv_types.Freg.t
  val fs7 : Riscv_family_encode.Riscv_types.Freg.t
  val fs8 : Riscv_family_encode.Riscv_types.Freg.t
  val fs9 : Riscv_family_encode.Riscv_types.Freg.t
  val fs10 : Riscv_family_encode.Riscv_types.Freg.t
  val fs11 : Riscv_family_encode.Riscv_types.Freg.t
  val ft8 : Riscv_family_encode.Riscv_types.Freg.t
  val ft9 : Riscv_family_encode.Riscv_types.Freg.t
  val ft10 : Riscv_family_encode.Riscv_types.Freg.t
  val ft11 : Riscv_family_encode.Riscv_types.Freg.t
  val v0 : Riscv_family_encode.Riscv_types.Vreg.t
  val v1 : Riscv_family_encode.Riscv_types.Vreg.t
  val v2 : Riscv_family_encode.Riscv_types.Vreg.t
  val v3 : Riscv_family_encode.Riscv_types.Vreg.t
  val v4 : Riscv_family_encode.Riscv_types.Vreg.t
  val v5 : Riscv_family_encode.Riscv_types.Vreg.t
  val v6 : Riscv_family_encode.Riscv_types.Vreg.t
  val v7 : Riscv_family_encode.Riscv_types.Vreg.t
  val v8 : Riscv_family_encode.Riscv_types.Vreg.t
  val v9 : Riscv_family_encode.Riscv_types.Vreg.t
  val v10 : Riscv_family_encode.Riscv_types.Vreg.t
  val v11 : Riscv_family_encode.Riscv_types.Vreg.t
  val v12 : Riscv_family_encode.Riscv_types.Vreg.t
  val v13 : Riscv_family_encode.Riscv_types.Vreg.t
  val v14 : Riscv_family_encode.Riscv_types.Vreg.t
  val v15 : Riscv_family_encode.Riscv_types.Vreg.t
  val v16 : Riscv_family_encode.Riscv_types.Vreg.t
  val v17 : Riscv_family_encode.Riscv_types.Vreg.t
  val v18 : Riscv_family_encode.Riscv_types.Vreg.t
  val v19 : Riscv_family_encode.Riscv_types.Vreg.t
  val v20 : Riscv_family_encode.Riscv_types.Vreg.t
  val v21 : Riscv_family_encode.Riscv_types.Vreg.t
  val v22 : Riscv_family_encode.Riscv_types.Vreg.t
  val v23 : Riscv_family_encode.Riscv_types.Vreg.t
  val v24 : Riscv_family_encode.Riscv_types.Vreg.t
  val v25 : Riscv_family_encode.Riscv_types.Vreg.t
  val v26 : Riscv_family_encode.Riscv_types.Vreg.t
  val v27 : Riscv_family_encode.Riscv_types.Vreg.t
  val v28 : Riscv_family_encode.Riscv_types.Vreg.t
  val v29 : Riscv_family_encode.Riscv_types.Vreg.t
  val v30 : Riscv_family_encode.Riscv_types.Vreg.t
  val v31 : Riscv_family_encode.Riscv_types.Vreg.t
  val off : Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Mem.t

  val off_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Mem.t

  val cmem : Riscv_family_encode.Riscv_types.Creg.t -> int -> Riscv_family_encode.Riscv_ops.Cmem.t
  val label : string -> Asm_core.Expr.t
  val rne : Riscv_family_encode.Riscv_types.Rm.t
  val rtz : Riscv_family_encode.Riscv_types.Rm.t
  val rdn : Riscv_family_encode.Riscv_types.Rm.t
  val rup : Riscv_family_encode.Riscv_types.Rm.t
  val rmm : Riscv_family_encode.Riscv_types.Rm.t
  val dyn : Riscv_family_encode.Riscv_types.Rm.t

  val add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sub :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sll :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slt :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sltu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val snez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val neg :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val seqz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sltz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sgtz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val zext_b :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xor :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srl :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sra :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val or_ :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val and_ :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mul :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val remu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mulh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mulhsu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mulhu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val div :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val divu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rem :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh1add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh2add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh3add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val min :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val minu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val max :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val maxu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val andn :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val orn :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xnor :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rol :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ror :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bclr :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bext :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val binv :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bset :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bclri :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bclri_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bexti :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bexti_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val binvi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val binvi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bseti :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bseti_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ctz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val cpop :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sext_b :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sext_h :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val orc_b :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val brev8 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rev8 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val pack :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val packh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rori :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rori_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val zext_h :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clmul :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clmulh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clmulr :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val czero_eqz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val czero_nez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm3p0 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm3p1 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm4ed :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm4ed_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm4ks :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sm4ks_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xperm4 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xperm8 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha256sum0 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha256sum1 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha256sig0 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha256sig1 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrw_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrs :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrs_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrc :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrc_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrwi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrwi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrsi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrsi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrci :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrrci_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrr :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val csrr_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrw :
    int -> Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val csrw_e :
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrs :
    int -> Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val csrs_e :
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrc :
    int -> Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val csrc_e :
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val csrwi : int -> int -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrwi_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrsi : int -> int -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrsi_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrci : int -> int -> Riscv_family_encode.Riscv_types.Instruction.t
  val csrci_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val amoswap_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoadd_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoxor_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoand_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoor_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amomin_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amomax_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amominu_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amomaxu_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lr_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sc_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val addi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val addi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_addi :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_addi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_and :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_or :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_xor :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_sub :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_jr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_jalr :
    Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_mv :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_add :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_ebreak : Riscv_family_encode.Riscv_types.Instruction.t

  val c_lw :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_ops.Cmem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_sw :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_ops.Cmem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_lwsp :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_swsp :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_beqz :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_j : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_bnez :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slti :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slti_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sltiu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sltiu_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xori :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val xori_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ori :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ori_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val andi :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val andi_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slli :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slli_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srli :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srli_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srai :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srai_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lb :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lbu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lhu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sb :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val beq :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bne :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val blt :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bge :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bltu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgeu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgtu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bleu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgt :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ble :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val beqz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bnez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bltz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val blez :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val bgtz :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lui :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val lui_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val auipc :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val auipc_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val jal : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val jalr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val mv :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val nop : Riscv_family_encode.Riscv_types.Instruction.t
  val j : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val jr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val call : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
  val tail : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t

  val la :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lla :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val li :
    Riscv_family_encode.Riscv_types.Xreg.t -> int64 -> Riscv_family_encode.Riscv_types.Instruction.t

  val li_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ret : Riscv_family_encode.Riscv_types.Instruction.t
  val ecall : Riscv_family_encode.Riscv_types.Instruction.t
  val ebreak : Riscv_family_encode.Riscv_types.Instruction.t
  val unimp : Riscv_family_encode.Riscv_types.Instruction.t
  val fence_i : Riscv_family_encode.Riscv_types.Instruction.t
  val fence : Riscv_family_encode.Riscv_types.Instruction.t
  val fence_tso : Riscv_family_encode.Riscv_types.Instruction.t
  val pause : Riscv_family_encode.Riscv_types.Instruction.t

  val fld :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val flw :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsd :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsw :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fadd_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fadd_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsub_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsub_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmul_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmul_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fdiv_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fdiv_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fneg_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fneg_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fabs_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fabs_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_s_x :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnj_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnj_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjn_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjn_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjx_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsgnjx_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmin_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmax_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmin_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmax_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsqrt_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fsqrt_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fclass_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fclass_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmadd_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmadd_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmsub_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmsub_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fnmsub_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fnmsub_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fnmadd_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fnmadd_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val feq_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fle_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val flt_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val flt_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val feq_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fle_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_w_x :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_w_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_wu_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_w :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_wu :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_w :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_wu :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_w_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_wu_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_d :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_s :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  module Rowkit = Riscv_asm_rowkit

  val sh1add_uw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh2add_uw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sh3add_uw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val clzw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ctzw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val cpopw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val packw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rolw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val rorw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val roriw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val roriw_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha512sum0 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha512sum1 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha512sig0 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sha512sig1 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes64ds :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes64dsm :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes64es :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes64esm :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes64ks2 :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes64im :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes64ks1i :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val aes64ks1i_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoswap_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoadd_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoxor_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoand_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amoor_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amomin_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amomax_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amominu_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val amomaxu_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lr_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sc_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val addw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val subw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sllw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srlw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sraw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val mulw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val divw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val divuw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val remw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val remuw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_addw :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_subw :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_ld :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_ops.Cmem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_sd :
    Riscv_family_encode.Riscv_types.Creg.t ->
    Riscv_family_encode.Riscv_ops.Cmem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val c_ldsp :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val c_sdsp :
    Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

  val addiw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val addiw_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slliw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val slliw_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srliw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val srliw_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sraiw :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    int ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sraiw_e :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Asm_core.Expr.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sext_w :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val lwu :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val ld :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val sd :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Mem.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fmv_x_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_l_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_l :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_lu_d :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_l :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_d_lu :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_l_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_lu_s :
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t

  val fcvt_s_lu :
    Riscv_family_encode.Riscv_types.Freg.t ->
    Riscv_family_encode.Riscv_types.Xreg.t ->
    Riscv_family_encode.Riscv_types.Instruction.t
end

module Creg = Riscv_family_encode.Riscv_types.Creg
module Cfreg = Riscv_family_encode.Riscv_types.Cfreg

val x0 : Riscv_family_encode.Riscv_types.Xreg.t
val x1 : Riscv_family_encode.Riscv_types.Xreg.t
val x2 : Riscv_family_encode.Riscv_types.Xreg.t
val x3 : Riscv_family_encode.Riscv_types.Xreg.t
val x4 : Riscv_family_encode.Riscv_types.Xreg.t
val x5 : Riscv_family_encode.Riscv_types.Xreg.t
val x6 : Riscv_family_encode.Riscv_types.Xreg.t
val x7 : Riscv_family_encode.Riscv_types.Xreg.t
val x8 : Riscv_family_encode.Riscv_types.Xreg.t
val x9 : Riscv_family_encode.Riscv_types.Xreg.t
val x10 : Riscv_family_encode.Riscv_types.Xreg.t
val x11 : Riscv_family_encode.Riscv_types.Xreg.t
val x12 : Riscv_family_encode.Riscv_types.Xreg.t
val x13 : Riscv_family_encode.Riscv_types.Xreg.t
val x14 : Riscv_family_encode.Riscv_types.Xreg.t
val x15 : Riscv_family_encode.Riscv_types.Xreg.t
val x16 : Riscv_family_encode.Riscv_types.Xreg.t
val x17 : Riscv_family_encode.Riscv_types.Xreg.t
val x18 : Riscv_family_encode.Riscv_types.Xreg.t
val x19 : Riscv_family_encode.Riscv_types.Xreg.t
val x20 : Riscv_family_encode.Riscv_types.Xreg.t
val x21 : Riscv_family_encode.Riscv_types.Xreg.t
val x22 : Riscv_family_encode.Riscv_types.Xreg.t
val x23 : Riscv_family_encode.Riscv_types.Xreg.t
val x24 : Riscv_family_encode.Riscv_types.Xreg.t
val x25 : Riscv_family_encode.Riscv_types.Xreg.t
val x26 : Riscv_family_encode.Riscv_types.Xreg.t
val x27 : Riscv_family_encode.Riscv_types.Xreg.t
val x28 : Riscv_family_encode.Riscv_types.Xreg.t
val x29 : Riscv_family_encode.Riscv_types.Xreg.t
val x30 : Riscv_family_encode.Riscv_types.Xreg.t
val x31 : Riscv_family_encode.Riscv_types.Xreg.t
val zero : Riscv_family_encode.Riscv_types.Xreg.t
val ra : Riscv_family_encode.Riscv_types.Xreg.t
val sp : Riscv_family_encode.Riscv_types.Xreg.t
val gp : Riscv_family_encode.Riscv_types.Xreg.t
val tp : Riscv_family_encode.Riscv_types.Xreg.t
val t0 : Riscv_family_encode.Riscv_types.Xreg.t
val t1 : Riscv_family_encode.Riscv_types.Xreg.t
val t2 : Riscv_family_encode.Riscv_types.Xreg.t
val s0 : Riscv_family_encode.Riscv_types.Xreg.t
val fp : Riscv_family_encode.Riscv_types.Xreg.t
val s1 : Riscv_family_encode.Riscv_types.Xreg.t
val a0 : Riscv_family_encode.Riscv_types.Xreg.t
val a1 : Riscv_family_encode.Riscv_types.Xreg.t
val a2 : Riscv_family_encode.Riscv_types.Xreg.t
val a3 : Riscv_family_encode.Riscv_types.Xreg.t
val a4 : Riscv_family_encode.Riscv_types.Xreg.t
val a5 : Riscv_family_encode.Riscv_types.Xreg.t
val a6 : Riscv_family_encode.Riscv_types.Xreg.t
val a7 : Riscv_family_encode.Riscv_types.Xreg.t
val s2 : Riscv_family_encode.Riscv_types.Xreg.t
val s3 : Riscv_family_encode.Riscv_types.Xreg.t
val s4 : Riscv_family_encode.Riscv_types.Xreg.t
val s5 : Riscv_family_encode.Riscv_types.Xreg.t
val s6 : Riscv_family_encode.Riscv_types.Xreg.t
val s7 : Riscv_family_encode.Riscv_types.Xreg.t
val s8 : Riscv_family_encode.Riscv_types.Xreg.t
val s9 : Riscv_family_encode.Riscv_types.Xreg.t
val s10 : Riscv_family_encode.Riscv_types.Xreg.t
val s11 : Riscv_family_encode.Riscv_types.Xreg.t
val t3 : Riscv_family_encode.Riscv_types.Xreg.t
val t4 : Riscv_family_encode.Riscv_types.Xreg.t
val t5 : Riscv_family_encode.Riscv_types.Xreg.t
val t6 : Riscv_family_encode.Riscv_types.Xreg.t
val f0 : Riscv_family_encode.Riscv_types.Freg.t
val f1 : Riscv_family_encode.Riscv_types.Freg.t
val f2 : Riscv_family_encode.Riscv_types.Freg.t
val f3 : Riscv_family_encode.Riscv_types.Freg.t
val f4 : Riscv_family_encode.Riscv_types.Freg.t
val f5 : Riscv_family_encode.Riscv_types.Freg.t
val f6 : Riscv_family_encode.Riscv_types.Freg.t
val f7 : Riscv_family_encode.Riscv_types.Freg.t
val f8 : Riscv_family_encode.Riscv_types.Freg.t
val f9 : Riscv_family_encode.Riscv_types.Freg.t
val f10 : Riscv_family_encode.Riscv_types.Freg.t
val f11 : Riscv_family_encode.Riscv_types.Freg.t
val f12 : Riscv_family_encode.Riscv_types.Freg.t
val f13 : Riscv_family_encode.Riscv_types.Freg.t
val f14 : Riscv_family_encode.Riscv_types.Freg.t
val f15 : Riscv_family_encode.Riscv_types.Freg.t
val f16 : Riscv_family_encode.Riscv_types.Freg.t
val f17 : Riscv_family_encode.Riscv_types.Freg.t
val f18 : Riscv_family_encode.Riscv_types.Freg.t
val f19 : Riscv_family_encode.Riscv_types.Freg.t
val f20 : Riscv_family_encode.Riscv_types.Freg.t
val f21 : Riscv_family_encode.Riscv_types.Freg.t
val f22 : Riscv_family_encode.Riscv_types.Freg.t
val f23 : Riscv_family_encode.Riscv_types.Freg.t
val f24 : Riscv_family_encode.Riscv_types.Freg.t
val f25 : Riscv_family_encode.Riscv_types.Freg.t
val f26 : Riscv_family_encode.Riscv_types.Freg.t
val f27 : Riscv_family_encode.Riscv_types.Freg.t
val f28 : Riscv_family_encode.Riscv_types.Freg.t
val f29 : Riscv_family_encode.Riscv_types.Freg.t
val f30 : Riscv_family_encode.Riscv_types.Freg.t
val f31 : Riscv_family_encode.Riscv_types.Freg.t
val ft0 : Riscv_family_encode.Riscv_types.Freg.t
val ft1 : Riscv_family_encode.Riscv_types.Freg.t
val ft2 : Riscv_family_encode.Riscv_types.Freg.t
val ft3 : Riscv_family_encode.Riscv_types.Freg.t
val ft4 : Riscv_family_encode.Riscv_types.Freg.t
val ft5 : Riscv_family_encode.Riscv_types.Freg.t
val ft6 : Riscv_family_encode.Riscv_types.Freg.t
val ft7 : Riscv_family_encode.Riscv_types.Freg.t
val fs0 : Riscv_family_encode.Riscv_types.Freg.t
val fs1 : Riscv_family_encode.Riscv_types.Freg.t
val fa0 : Riscv_family_encode.Riscv_types.Freg.t
val fa1 : Riscv_family_encode.Riscv_types.Freg.t
val fa2 : Riscv_family_encode.Riscv_types.Freg.t
val fa3 : Riscv_family_encode.Riscv_types.Freg.t
val fa4 : Riscv_family_encode.Riscv_types.Freg.t
val fa5 : Riscv_family_encode.Riscv_types.Freg.t
val fa6 : Riscv_family_encode.Riscv_types.Freg.t
val fa7 : Riscv_family_encode.Riscv_types.Freg.t
val fs2 : Riscv_family_encode.Riscv_types.Freg.t
val fs3 : Riscv_family_encode.Riscv_types.Freg.t
val fs4 : Riscv_family_encode.Riscv_types.Freg.t
val fs5 : Riscv_family_encode.Riscv_types.Freg.t
val fs6 : Riscv_family_encode.Riscv_types.Freg.t
val fs7 : Riscv_family_encode.Riscv_types.Freg.t
val fs8 : Riscv_family_encode.Riscv_types.Freg.t
val fs9 : Riscv_family_encode.Riscv_types.Freg.t
val fs10 : Riscv_family_encode.Riscv_types.Freg.t
val fs11 : Riscv_family_encode.Riscv_types.Freg.t
val ft8 : Riscv_family_encode.Riscv_types.Freg.t
val ft9 : Riscv_family_encode.Riscv_types.Freg.t
val ft10 : Riscv_family_encode.Riscv_types.Freg.t
val ft11 : Riscv_family_encode.Riscv_types.Freg.t
val v0 : Riscv_family_encode.Riscv_types.Vreg.t
val v1 : Riscv_family_encode.Riscv_types.Vreg.t
val v2 : Riscv_family_encode.Riscv_types.Vreg.t
val v3 : Riscv_family_encode.Riscv_types.Vreg.t
val v4 : Riscv_family_encode.Riscv_types.Vreg.t
val v5 : Riscv_family_encode.Riscv_types.Vreg.t
val v6 : Riscv_family_encode.Riscv_types.Vreg.t
val v7 : Riscv_family_encode.Riscv_types.Vreg.t
val v8 : Riscv_family_encode.Riscv_types.Vreg.t
val v9 : Riscv_family_encode.Riscv_types.Vreg.t
val v10 : Riscv_family_encode.Riscv_types.Vreg.t
val v11 : Riscv_family_encode.Riscv_types.Vreg.t
val v12 : Riscv_family_encode.Riscv_types.Vreg.t
val v13 : Riscv_family_encode.Riscv_types.Vreg.t
val v14 : Riscv_family_encode.Riscv_types.Vreg.t
val v15 : Riscv_family_encode.Riscv_types.Vreg.t
val v16 : Riscv_family_encode.Riscv_types.Vreg.t
val v17 : Riscv_family_encode.Riscv_types.Vreg.t
val v18 : Riscv_family_encode.Riscv_types.Vreg.t
val v19 : Riscv_family_encode.Riscv_types.Vreg.t
val v20 : Riscv_family_encode.Riscv_types.Vreg.t
val v21 : Riscv_family_encode.Riscv_types.Vreg.t
val v22 : Riscv_family_encode.Riscv_types.Vreg.t
val v23 : Riscv_family_encode.Riscv_types.Vreg.t
val v24 : Riscv_family_encode.Riscv_types.Vreg.t
val v25 : Riscv_family_encode.Riscv_types.Vreg.t
val v26 : Riscv_family_encode.Riscv_types.Vreg.t
val v27 : Riscv_family_encode.Riscv_types.Vreg.t
val v28 : Riscv_family_encode.Riscv_types.Vreg.t
val v29 : Riscv_family_encode.Riscv_types.Vreg.t
val v30 : Riscv_family_encode.Riscv_types.Vreg.t
val v31 : Riscv_family_encode.Riscv_types.Vreg.t
val off : Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Mem.t

val off_e :
  Riscv_family_encode.Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Mem.t

val cmem : Riscv_family_encode.Riscv_types.Creg.t -> int -> Riscv_family_encode.Riscv_ops.Cmem.t
val label : string -> Asm_core.Expr.t
val rne : Riscv_family_encode.Riscv_types.Rm.t
val rtz : Riscv_family_encode.Riscv_types.Rm.t
val rdn : Riscv_family_encode.Riscv_types.Rm.t
val rup : Riscv_family_encode.Riscv_types.Rm.t
val rmm : Riscv_family_encode.Riscv_types.Rm.t
val dyn : Riscv_family_encode.Riscv_types.Rm.t

val add :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sub :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sll :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val slt :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sltu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val snez :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val neg :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val seqz :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sltz :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sgtz :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val zext_b :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val xor :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val srl :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sra :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val or_ :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val and_ :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val mul :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val remu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val mulh :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val mulhsu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val mulhu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val div :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val divu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val rem :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sh1add :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sh2add :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sh3add :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val min :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val minu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val max :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val maxu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val andn :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val orn :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val xnor :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val rol :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val ror :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bclr :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bext :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val binv :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bset :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bclri :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bclri_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bexti :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bexti_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val binvi :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val binvi_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bseti :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bseti_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val clz :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val ctz :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val cpop :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sext_b :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sext_h :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val orc_b :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val brev8 :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val rev8 :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val pack :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val packh :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val rori :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val rori_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val zext_h :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val clmul :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val clmulh :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val clmulr :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val czero_eqz :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val czero_nez :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sm3p0 :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sm3p1 :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sm4ed :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sm4ed_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sm4ks :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sm4ks_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val xperm4 :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val xperm8 :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sha256sum0 :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sha256sum1 :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sha256sig0 :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sha256sig1 :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrrw :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrrw_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrrs :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrrs_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrrc :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrrc_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrrwi :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrrwi_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrrsi :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrrsi_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrrci :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrrci_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrr :
  Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

val csrr_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrw :
  int -> Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

val csrw_e :
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrs :
  int -> Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

val csrs_e :
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrc :
  int -> Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

val csrc_e :
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val csrwi : int -> int -> Riscv_family_encode.Riscv_types.Instruction.t
val csrwi_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
val csrsi : int -> int -> Riscv_family_encode.Riscv_types.Instruction.t
val csrsi_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
val csrci : int -> int -> Riscv_family_encode.Riscv_types.Instruction.t
val csrci_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t

val amoswap_w :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val amoadd_w :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val amoxor_w :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val amoand_w :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val amoor_w :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val amomin_w :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val amomax_w :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val amominu_w :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val amomaxu_w :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val lr_w :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sc_w :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val addi :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val addi_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val c_addi :
  Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

val c_addi_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val c_and :
  Riscv_family_encode.Riscv_types.Creg.t ->
  Riscv_family_encode.Riscv_types.Creg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val c_or :
  Riscv_family_encode.Riscv_types.Creg.t ->
  Riscv_family_encode.Riscv_types.Creg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val c_xor :
  Riscv_family_encode.Riscv_types.Creg.t ->
  Riscv_family_encode.Riscv_types.Creg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val c_sub :
  Riscv_family_encode.Riscv_types.Creg.t ->
  Riscv_family_encode.Riscv_types.Creg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val c_jr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t
val c_jalr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

val c_mv :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val c_add :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val c_ebreak : Riscv_family_encode.Riscv_types.Instruction.t

val c_lw :
  Riscv_family_encode.Riscv_types.Creg.t ->
  Riscv_family_encode.Riscv_ops.Cmem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val c_sw :
  Riscv_family_encode.Riscv_types.Creg.t ->
  Riscv_family_encode.Riscv_ops.Cmem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val c_lwsp :
  Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

val c_swsp :
  Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

val c_beqz :
  Riscv_family_encode.Riscv_types.Creg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val c_j : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t

val c_bnez :
  Riscv_family_encode.Riscv_types.Creg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val slti :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val slti_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sltiu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sltiu_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val xori :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val xori_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val ori :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val ori_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val andi :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val andi_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val slli :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val slli_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val srli :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val srli_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val srai :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  int ->
  Riscv_family_encode.Riscv_types.Instruction.t

val srai_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val lb :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val lh :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val lw :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val lbu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val lhu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sb :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sh :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val sw :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val beq :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bne :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val blt :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bge :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bltu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bgeu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bgtu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bleu :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bgt :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val ble :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val beqz :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bnez :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bgez :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bltz :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val blez :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val bgtz :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val lui :
  Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

val lui_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val auipc :
  Riscv_family_encode.Riscv_types.Xreg.t -> int -> Riscv_family_encode.Riscv_types.Instruction.t

val auipc_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val jal : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
val jalr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t

val mv :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val nop : Riscv_family_encode.Riscv_types.Instruction.t
val j : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
val jr : Riscv_family_encode.Riscv_types.Xreg.t -> Riscv_family_encode.Riscv_types.Instruction.t
val call : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t
val tail : Asm_core.Expr.t -> Riscv_family_encode.Riscv_types.Instruction.t

val la :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val lla :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val li :
  Riscv_family_encode.Riscv_types.Xreg.t -> int64 -> Riscv_family_encode.Riscv_types.Instruction.t

val li_e :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val ret : Riscv_family_encode.Riscv_types.Instruction.t
val ecall : Riscv_family_encode.Riscv_types.Instruction.t
val ebreak : Riscv_family_encode.Riscv_types.Instruction.t
val unimp : Riscv_family_encode.Riscv_types.Instruction.t
val fence_i : Riscv_family_encode.Riscv_types.Instruction.t
val fence : Riscv_family_encode.Riscv_types.Instruction.t
val fence_tso : Riscv_family_encode.Riscv_types.Instruction.t
val pause : Riscv_family_encode.Riscv_types.Instruction.t

val fld :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val flw :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fsd :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fsw :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Mem.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fadd_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fadd_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fsub_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fsub_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmul_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmul_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fdiv_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fdiv_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fneg_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fneg_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fabs_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fabs_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmv_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmv_x_s :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmv_s_x :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fsgnj_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fsgnj_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fsgnjn_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fsgnjn_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fsgnjx_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fsgnjx_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmin_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmax_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmin_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmax_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fsqrt_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fsqrt_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fclass_s :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fclass_d :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmadd_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmadd_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmsub_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmsub_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fnmsub_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fnmsub_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fnmadd_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fnmadd_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmv_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val feq_d :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fle_d :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val flt_d :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val flt_s :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val feq_s :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fle_s :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmv_x_w :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fmv_w_x :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fcvt_w_d :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fcvt_wu_d :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fcvt_d_w :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fcvt_d_wu :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fcvt_s_w :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fcvt_s_wu :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fcvt_w_s :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fcvt_wu_s :
  Riscv_family_encode.Riscv_types.Xreg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fcvt_s_d :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

val fcvt_d_s :
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Freg.t ->
  Riscv_family_encode.Riscv_types.Instruction.t

module Rowkit = Riscv_asm_rowkit
