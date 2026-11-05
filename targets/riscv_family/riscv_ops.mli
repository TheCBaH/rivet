(* One definition per hand-written RISC-V opcode: the operands it takes, its encoding, the
   components that publish it, the XLEN it exists for, its class, the rounding mode it takes when
   none is written, and whether its lowering stays code (pseudo expansions and irregular forms).
   Each definition is also the opcode's typed builder. The encoder does not read it yet: a test
   holds it to the encoder's own tables until each consumer switches to it. *)

module Kind : sig
  type t = X | F | V | Creg | Imm | Mem | Cmem | Spmem | Sym | Rm

  val name : t -> string
end

module Cmem : sig
  type t = { base : Riscv_types.Creg.t; offset : int }
end

module Shape : sig
  type ('f, 'g, 'r) t =
    | Done : ('r, 'r, 'r) t
    | X : ('f, 'g, 'r) t -> (Riscv_types.Xreg.t -> 'f, Riscv_types.Xreg.t -> 'g, 'r) t
    | F : ('f, 'g, 'r) t -> (Riscv_types.Freg.t -> 'f, Riscv_types.Freg.t -> 'g, 'r) t
    | V : ('f, 'g, 'r) t -> (Riscv_types.Vreg.t -> 'f, Riscv_types.Vreg.t -> 'g, 'r) t
    | Creg : ('f, 'g, 'r) t -> (Riscv_types.Creg.t -> 'f, Riscv_types.Creg.t -> 'g, 'r) t
    | Imm : ('f, 'g, 'r) t -> (int -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | Imm64 : ('f, 'g, 'r) t -> (int64 -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | Mem : ('f, 'g, 'r) t -> (Riscv_types.Mem.t -> 'f, Riscv_types.Mem.t -> 'g, 'r) t
    | Cmem : ('f, 'g, 'r) t -> (Cmem.t -> 'f, Cmem.t -> 'g, 'r) t
    | Spmem : ('f, 'g, 'r) t -> (int -> 'f, int -> 'g, 'r) t
    | Sym : ('f, 'g, 'r) t -> (Asm_core.Expr.t -> 'f, Asm_core.Expr.t -> 'g, 'r) t
    | Rm : ('f, 'g, 'r) t -> (Riscv_types.Rm.t -> 'f, Riscv_types.Rm.t -> 'g, 'r) t

  val kinds : ('f, 'g, 'r) t -> Kind.t list
  val int : int -> Riscv_types.Operand.t
  val expr : Asm_core.Expr.t -> Riscv_types.Operand.t
  val x : Riscv_types.Xreg.t -> Riscv_types.Operand.t
  val mem : base:Riscv_types.Xreg.t -> int -> Riscv_types.Operand.t
  val cmem : Cmem.t -> Riscv_types.Operand.t
  val rm : Riscv_types.Rm.t -> Riscv_types.Operand.t
  val build : ('f, 'g, 'r) t -> (Riscv_types.Operand.t list -> 'r) -> 'f
  val build_e : ('f, 'g, 'r) t -> (Riscv_types.Operand.t list -> 'r) -> 'g
  val sample : ('f, 'g, 'r) t -> int -> int -> 'f -> 'r
end

module Enc : sig
  type t =
    | R of { opcode : int; funct3 : int; funct7 : int; rs2 : int option }
    | Rbs of { funct7 : int }
    | R4 of { opcode : int; fmt : int }
    | Unary of { opcode : int; funct3 : int; funct12 : int }
    | I of { opcode : int; funct3 : int }
    | Shift of { funct3 : int; funct6 : int }
    | Shiftw of { funct3 : int; funct7 : int }
    | S of { opcode : int; funct3 : int }
    | B of { funct3 : int }
    | U of { opcode : int }
    | Ca of { funct6 : int; funct2 : int }
    | Cr of { funct4 : int }
    | C of { funct3 : int }
end

type xlen = Any | Rv32 | Rv64

type entry = {
  op : Riscv_types.Opcode.t;
  shape : Kind.t list;
  also : Kind.t list list;
  enc : Enc.t option;
  enc64 : Enc.t option;
  ext : string list;
  src : string option;
  alt : string list;
  xlen : xlen;
  cls : Riscv_types.Cls.t;
  rm : Riscv_types.Rm.t option;
  lower : bool;
  sample : int -> Riscv_types.Instruction.t;
}

val registry : entry list ref

val def :
  ?also:Kind.t list list ->
  ?enc:Enc.t ->
  ?enc64:Enc.t ->
  ?ext:string list ->
  ?src:string ->
  ?alt:string list ->
  ?xlen:xlen ->
  ?rm:Riscv_types.Rm.t ->
  ?lower:bool ->
  cls:Riscv_types.Cls.t ->
  Riscv_types.Opcode.t ->
  ('a, 'b, Riscv_types.Instruction.t) Shape.t ->
  'a * 'b

val add :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sub :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sll :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val slt :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sltu :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val snez : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val neg : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val seqz : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val sltz : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val sgtz : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val zext_b : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val xor :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val srl :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sra :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val or_ :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val and_ :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val mul :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val remu :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val mulh :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val mulhsu :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val mulhu :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val div :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val divu :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val rem :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sh1add :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sh2add :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sh3add :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val min :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val minu :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val max :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val maxu :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val andn :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val orn :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val xnor :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val rol :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val ror :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val bclr :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val bext :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val binv :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val bset :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val bclri : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val bclri_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val bexti : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val bexti_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val binvi : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val binvi_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val bseti : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val bseti_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val sh1add_uw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sh2add_uw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sh3add_uw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val clz : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val ctz : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val cpop : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val sext_b : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val sext_h : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val orc_b : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val clzw : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val ctzw : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val cpopw : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val brev8 : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val rev8 : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val pack :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val packh :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val packw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val zip : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val unzip : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val rolw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val rorw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val rori : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val rori_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val roriw : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val roriw_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val zext_h : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val clmul :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val clmulh :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val clmulr :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val czero_eqz :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val czero_nez :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sm3p0 : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val sm3p1 : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sm4ed :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val sm4ed_e :
  Riscv_types.Xreg.t ->
  Riscv_types.Xreg.t ->
  Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_types.Instruction.t

val sm4ks :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val sm4ks_e :
  Riscv_types.Xreg.t ->
  Riscv_types.Xreg.t ->
  Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_types.Instruction.t

val xperm4 :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val xperm8 :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sha256sum0 : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val sha256sum1 : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val sha256sig0 : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val sha256sig1 : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val sha512sum0 : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val sha512sum1 : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val sha512sig0 : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val sha512sig1 : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sha512sum0r :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sha512sum1r :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sha512sig0l :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sha512sig1l :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sha512sig0h :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sha512sig1h :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val aes64ds :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val aes64dsm :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val aes64es :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val aes64esm :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val aes64ks2 :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val aes64im : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val aes64ks1i : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val aes64ks1i_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val aes32dsi :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val aes32dsi_e :
  Riscv_types.Xreg.t ->
  Riscv_types.Xreg.t ->
  Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_types.Instruction.t

val aes32dsmi :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val aes32dsmi_e :
  Riscv_types.Xreg.t ->
  Riscv_types.Xreg.t ->
  Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_types.Instruction.t

val aes32esi :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val aes32esi_e :
  Riscv_types.Xreg.t ->
  Riscv_types.Xreg.t ->
  Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_types.Instruction.t

val aes32esmi :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val aes32esmi_e :
  Riscv_types.Xreg.t ->
  Riscv_types.Xreg.t ->
  Riscv_types.Xreg.t ->
  Asm_core.Expr.t ->
  Riscv_types.Instruction.t

val csrrw : Riscv_types.Xreg.t -> int -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val csrrw_e :
  Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val csrrs : Riscv_types.Xreg.t -> int -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val csrrs_e :
  Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val csrrc : Riscv_types.Xreg.t -> int -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val csrrc_e :
  Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val csrrwi : Riscv_types.Xreg.t -> int -> int -> Riscv_types.Instruction.t
val csrrwi_e : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val csrrsi : Riscv_types.Xreg.t -> int -> int -> Riscv_types.Instruction.t
val csrrsi_e : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val csrrci : Riscv_types.Xreg.t -> int -> int -> Riscv_types.Instruction.t
val csrrci_e : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val csrr : Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t
val csrr_e : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val csrw : int -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val csrw_e : Asm_core.Expr.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val csrs : int -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val csrs_e : Asm_core.Expr.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val csrc : int -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val csrc_e : Asm_core.Expr.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val csrwi : int -> int -> Riscv_types.Instruction.t
val csrwi_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val csrsi : int -> int -> Riscv_types.Instruction.t
val csrsi_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val csrci : int -> int -> Riscv_types.Instruction.t
val csrci_e : Asm_core.Expr.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val amoswap_w :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amoadd_w :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amoxor_w :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amoand_w :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amoor_w :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amomin_w :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amomax_w :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amominu_w :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amomaxu_w :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val lr_w : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val sc_w :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amoswap_d :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amoadd_d :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amoxor_d :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amoand_d :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amoor_d :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amomin_d :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amomax_d :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amominu_d :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val amomaxu_d :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val lr_d : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val sc_d :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val addw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val subw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sllw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val srlw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val sraw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val mulw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val divw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val divuw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val remw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val remuw :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val addi : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val addi_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val c_addi : Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t
val c_addi_e : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val c_and : Riscv_types.Creg.t -> Riscv_types.Creg.t -> Riscv_types.Instruction.t
val c_or : Riscv_types.Creg.t -> Riscv_types.Creg.t -> Riscv_types.Instruction.t
val c_xor : Riscv_types.Creg.t -> Riscv_types.Creg.t -> Riscv_types.Instruction.t
val c_sub : Riscv_types.Creg.t -> Riscv_types.Creg.t -> Riscv_types.Instruction.t
val c_addw : Riscv_types.Creg.t -> Riscv_types.Creg.t -> Riscv_types.Instruction.t
val c_subw : Riscv_types.Creg.t -> Riscv_types.Creg.t -> Riscv_types.Instruction.t
val c_jr : Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val c_jalr : Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val c_mv : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val c_add : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val c_ebreak : Riscv_types.Instruction.t
val c_lw : Riscv_types.Creg.t -> Cmem.t -> Riscv_types.Instruction.t
val c_sw : Riscv_types.Creg.t -> Cmem.t -> Riscv_types.Instruction.t
val c_lwsp : Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t
val c_swsp : Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t
val c_ld : Riscv_types.Creg.t -> Cmem.t -> Riscv_types.Instruction.t
val c_sd : Riscv_types.Creg.t -> Cmem.t -> Riscv_types.Instruction.t
val c_ldsp : Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t
val c_sdsp : Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t
val c_beqz : Riscv_types.Creg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val c_j : Asm_core.Expr.t -> Riscv_types.Instruction.t
val c_jal : Asm_core.Expr.t -> Riscv_types.Instruction.t
val c_bnez : Riscv_types.Creg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val slti : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val slti_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val sltiu : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val sltiu_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val xori : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val xori_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val ori : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t
val ori_e : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val andi : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val andi_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val slli : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val slli_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val srli : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val srli_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val srai : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val srai_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val addiw : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val addiw_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val slliw : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val slliw_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val srliw : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val srliw_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val sraiw : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t

val sraiw_e :
  Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t

val sext_w : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val lb : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val lh : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val lw : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val lbu : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val lhu : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val lwu : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val ld : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val sb : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val sh : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val sw : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val sd : Riscv_types.Xreg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val beq : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val bne : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val blt : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val bge : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val bltu : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val bgeu : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val bgtu : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val bleu : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val bgt : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val ble : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val beqz : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val bnez : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val bgez : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val bltz : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val blez : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val bgtz : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val lui : Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t
val lui_e : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val auipc : Riscv_types.Xreg.t -> int -> Riscv_types.Instruction.t
val auipc_e : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val jal : Asm_core.Expr.t -> Riscv_types.Instruction.t
val jalr : Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val mv : Riscv_types.Xreg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val nop : Riscv_types.Instruction.t
val j : Asm_core.Expr.t -> Riscv_types.Instruction.t
val jr : Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val call : Asm_core.Expr.t -> Riscv_types.Instruction.t
val tail : Asm_core.Expr.t -> Riscv_types.Instruction.t
val la : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val lla : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val li : Riscv_types.Xreg.t -> int64 -> Riscv_types.Instruction.t
val li_e : Riscv_types.Xreg.t -> Asm_core.Expr.t -> Riscv_types.Instruction.t
val ret : Riscv_types.Instruction.t
val ecall : Riscv_types.Instruction.t
val ebreak : Riscv_types.Instruction.t
val unimp : Riscv_types.Instruction.t
val fence_i : Riscv_types.Instruction.t
val fence : Riscv_types.Instruction.t
val fence_tso : Riscv_types.Instruction.t
val pause : Riscv_types.Instruction.t
val fld : Riscv_types.Freg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val flw : Riscv_types.Freg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val fsd : Riscv_types.Freg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t
val fsw : Riscv_types.Freg.t -> Riscv_types.Mem.t -> Riscv_types.Instruction.t

val fadd_d :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fadd_s :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fsub_d :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fsub_s :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fmul_d :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fmul_s :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fdiv_d :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fdiv_s :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fneg_d : Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fneg_s : Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fabs_d : Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fabs_s : Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fmv_s : Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fmv_x_s : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fmv_s_x : Riscv_types.Freg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t

val fsgnj_s :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fsgnj_d :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fsgnjn_s :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fsgnjn_d :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fsgnjx_s :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fsgnjx_d :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fmin_s :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fmax_s :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fmin_d :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fmax_d :
  Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fsqrt_s : Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fsqrt_d : Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fclass_s : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fclass_d : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fmadd_s :
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Instruction.t

val fmadd_d :
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Instruction.t

val fmsub_s :
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Instruction.t

val fmsub_d :
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Instruction.t

val fnmsub_s :
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Instruction.t

val fnmsub_d :
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Instruction.t

val fnmadd_s :
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Instruction.t

val fnmadd_d :
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Freg.t ->
  Riscv_types.Instruction.t

val fmv_d : Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fmv_x_d : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val feq_d :
  Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fle_d :
  Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val flt_d :
  Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val flt_s :
  Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val feq_s :
  Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fle_s :
  Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t

val fmv_x_w : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fmv_w_x : Riscv_types.Freg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val fcvt_w_d : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fcvt_wu_d : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fcvt_l_d : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fcvt_d_w : Riscv_types.Freg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val fcvt_d_wu : Riscv_types.Freg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val fcvt_s_w : Riscv_types.Freg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val fcvt_s_wu : Riscv_types.Freg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val fcvt_w_s : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fcvt_wu_s : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fcvt_s_d : Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fcvt_d_s : Riscv_types.Freg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fcvt_s_l : Riscv_types.Freg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val fcvt_lu_d : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fcvt_d_l : Riscv_types.Freg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val fcvt_d_lu : Riscv_types.Freg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val fcvt_l_s : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fcvt_lu_s : Riscv_types.Xreg.t -> Riscv_types.Freg.t -> Riscv_types.Instruction.t
val fcvt_s_lu : Riscv_types.Freg.t -> Riscv_types.Xreg.t -> Riscv_types.Instruction.t
val all : entry array
val by_op : (Riscv_types.Opcode.t, entry) Hashtbl.t
val find : Riscv_types.Opcode.t -> entry option
val forms : string -> (Riscv_types.Opcode.t * string) list
val alternatives : (string * string list) list
