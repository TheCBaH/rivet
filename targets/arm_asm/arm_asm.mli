(* Typed builders for the hand-written ARM (A32) instructions, one per operand shape of each
   opcode and named by the mnemonic ([.] becomes [_], a keyword gains [_]) with a suffix for the
   shape: [_i] an immediate, [_sh] a shifted register, [_post] a post-indexed address, [_s] a
   label or symbol expression where the shape also takes a number, [_l] a literal label, [_z]
   zero. Every builder of an instruction with a condition takes [?cond]; a flag-setting form is an
   opcode of its own ([adds]). Immediates are [int] ([int64] for a modified immediate); lowering
   still checks ranges. Core registers are {!Arm_encode.Reg.t}, double-precision ones
   {!Arm_encode.Dreg.t} and single-precision ones {!Arm_encode.Sreg.t}, so a [d] register where an
   [s] one is expected does not compile. *)

module Cond = Arm_encode.Cond
module Reg = Arm_encode.Reg
module Dreg = Arm_encode.Dreg
module Sreg = Arm_encode.Sreg
module Lo = Arm_encode.Lo

type shift = Arm_encode.Arm_ops.Shift.kind = Lsl | Lsr | Asr | Ror
type shifted = Arm_encode.Arm_ops.Shift.t

val shifted : Arm_encode.Arm_ops.Shift.kind -> Arm_encode.Reg.t -> int -> Arm_encode.Arm_ops.Shift.t
val mem : ?off:int -> Arm_encode__Arm_types.Reg.t -> Arm_encode.Mem.t
val mem_wb : ?off:int -> Arm_encode__Arm_types.Reg.t -> Arm_encode.Mem.t

val mem_reg :
  ?negate:bool -> Arm_encode__Arm_types.Reg.t -> Arm_encode__Arm_types.Reg.t -> Arm_encode.Mem.t

val mem_shifted :
  ?negate:bool ->
  Arm_encode__Arm_types.Reg.t ->
  Arm_encode__Arm_types.Reg.t ->
  Arm_encode.Arm_ops.Shift.kind ->
  int ->
  Arm_encode.Mem.t

val r0 : Reg.t
val r1 : Reg.t
val r2 : Reg.t
val r3 : Reg.t
val r4 : Reg.t
val r5 : Reg.t
val r6 : Reg.t
val r7 : Reg.t
val r8 : Reg.t
val r9 : Reg.t
val r10 : Reg.t
val r11 : Reg.t
val r12 : Reg.t
val r13 : Reg.t
val r14 : Reg.t
val r15 : Reg.t
val ip : Reg.t
val fp : Reg.t
val sl : Reg.t
val sp : Reg.t
val lr : Reg.t
val pc : Reg.t
val d0 : Dreg.t
val d1 : Dreg.t
val d2 : Dreg.t
val d3 : Dreg.t
val d4 : Dreg.t
val d5 : Dreg.t
val d6 : Dreg.t
val d7 : Dreg.t
val d8 : Dreg.t
val d9 : Dreg.t
val d10 : Dreg.t
val d11 : Dreg.t
val d12 : Dreg.t
val d13 : Dreg.t
val d14 : Dreg.t
val d15 : Dreg.t
val d16 : Dreg.t
val d17 : Dreg.t
val d18 : Dreg.t
val d19 : Dreg.t
val d20 : Dreg.t
val d21 : Dreg.t
val d22 : Dreg.t
val d23 : Dreg.t
val d24 : Dreg.t
val d25 : Dreg.t
val d26 : Dreg.t
val d27 : Dreg.t
val d28 : Dreg.t
val d29 : Dreg.t
val d30 : Dreg.t
val d31 : Dreg.t
val s0 : Sreg.t
val s1 : Sreg.t
val s2 : Sreg.t
val s3 : Sreg.t
val s4 : Sreg.t
val s5 : Sreg.t
val s6 : Sreg.t
val s7 : Sreg.t
val s8 : Sreg.t
val s9 : Sreg.t
val s10 : Sreg.t
val s11 : Sreg.t
val s12 : Sreg.t
val s13 : Sreg.t
val s14 : Sreg.t
val s15 : Sreg.t
val s16 : Sreg.t
val s17 : Sreg.t
val s18 : Sreg.t
val s19 : Sreg.t
val s20 : Sreg.t
val s21 : Sreg.t
val s22 : Sreg.t
val s23 : Sreg.t
val s24 : Sreg.t
val s25 : Sreg.t
val s26 : Sreg.t
val s27 : Sreg.t
val s28 : Sreg.t
val s29 : Sreg.t
val s30 : Sreg.t
val s31 : Sreg.t

val uncond :
  name:string ->
  enc:Arm_encode.Arm_ops.Enc.t ->
  Arm_encode.Opcode.t ->
  ('a, Arm_encode.Instruction.t) Arm_encode.Arm_ops.Shape.t ->
  'a

val dp_ : ?s:bool -> ?form:Arm_encode.Arm_ops.Enc.dp_form -> int -> Arm_encode.Arm_ops.Enc.t

val rrr :
  (Arm_encode.Reg.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> 'a, 'a) Arm_encode.Arm_ops.Shape.t

val rri : (Arm_encode.Reg.t -> Arm_encode.Reg.t -> int64 -> 'a, 'a) Arm_encode.Arm_ops.Shape.t

val rrsh :
  ( Arm_encode.Reg.t -> Arm_encode.Reg.t -> Arm_encode.Arm_ops.Shift.t -> 'a,
    'a )
  Arm_encode.Arm_ops.Shape.t

val rr : (Arm_encode.Reg.t -> Arm_encode.Reg.t -> 'a, 'a) Arm_encode.Arm_ops.Shape.t
val shorthand : Arm_encode.Arm_ops.Kind.t list list
val three_move : Arm_encode.Arm_ops.Kind.t list list

val mov :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> Arm_encode.Instruction.t

val mov_i : ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> int64 -> Arm_encode.Instruction.t

val mov_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val mvn :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> Arm_encode.Instruction.t

val mvn_i : ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> int64 -> Arm_encode.Instruction.t

val mvn_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val add :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val add_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val add_i :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int64 ->
  Arm_encode.Instruction.t

val sub :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val sub_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val sub_i :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int64 ->
  Arm_encode.Instruction.t

val and_ :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val and_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val and_i :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int64 ->
  Arm_encode.Instruction.t

val eor :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val eor_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val eor_i :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int64 ->
  Arm_encode.Instruction.t

val rsb :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val rsb_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val rsb_i :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int64 ->
  Arm_encode.Instruction.t

val orr :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val orr_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val orr_i :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int64 ->
  Arm_encode.Instruction.t

val bic :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val bic_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val bic_i :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int64 ->
  Arm_encode.Instruction.t

val adc :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val adc_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val adc_i :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int64 ->
  Arm_encode.Instruction.t

val sbc :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val sbc_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val cmp :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> Arm_encode.Instruction.t

val cmp_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val cmp_i : ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> int64 -> Arm_encode.Instruction.t

val cmn :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> Arm_encode.Instruction.t

val cmn_sh :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Arm_ops.Shift.t ->
  Arm_encode.Instruction.t

val cmn_i : ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> int64 -> Arm_encode.Instruction.t

val adds :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val adds_i :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int64 ->
  Arm_encode.Instruction.t

val subs :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val subs_i :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int64 ->
  Arm_encode.Instruction.t

val rsbs_i :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int64 ->
  Arm_encode.Instruction.t

val orrs :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val adcs :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val sbcs :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val shift_reg :
  (Arm_encode.Reg.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> 'a, 'a) Arm_encode.Arm_ops.Shape.t

val shift_imm : (Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> 'a, 'a) Arm_encode.Arm_ops.Shape.t

val shift :
  name:string ->
  int ->
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val shift_i :
  int ->
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int ->
  Arm_encode.Instruction.t

val shifts :
  name:string ->
  int ->
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int ->
  Arm_encode.Instruction.t

val lsl_ :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val lsl_i :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val lsr_ :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val lsr_i :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val asr_ :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val asr_i :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val ror :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val ror_i :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val lsls_i :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val lsrs_i :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val asrs_i :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val rors_i :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val rrx :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> Arm_encode.Instruction.t

val rrxs :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> Arm_encode.Instruction.t

val mul :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val mla :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val umull :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val ldst :
  name:string ->
  load:bool ->
  byte:bool ->
  Arm_encode.Opcode.t ->
  (?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t)
  * (?cond:Arm_encode.Cond.t ->
    Arm_encode.Reg.t ->
    Arm_encode.Reg.t ->
    int ->
    Arm_encode.Instruction.t)

val str :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t

val str_post :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val ldr :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t

val ldr_post :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val strb :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t

val strb_post :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val ldrb :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t

val ldrb_post :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val ldst_x :
  name:string ->
  int ->
  int64 * int64 * int64 ->
  Arm_encode.Opcode.t ->
  (?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t)
  * (?cond:Arm_encode.Cond.t ->
    Arm_encode.Reg.t ->
    Arm_encode.Reg.t ->
    int ->
    Arm_encode.Instruction.t)

val strh :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t

val strh_post :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val ldrh :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t

val ldrh_post :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val ldrsb :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t

val ldrsb_post :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val ldrsh :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t

val ldrsh_post :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val rd_rm :
  name:string ->
  int64 ->
  int64 ->
  Arm_encode.Opcode.t ->
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val clz :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> Arm_encode.Instruction.t

val rev :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> Arm_encode.Instruction.t

val rev16 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t -> Arm_encode.Instruction.t

val sbfx :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  int ->
  int ->
  Arm_encode.Instruction.t

val bx : ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Instruction.t
val b : ?cond:Arm_encode.Cond.t -> Asm_core.Expr.t -> Arm_encode.Instruction.t
val bl : ?cond:Arm_encode.Cond.t -> Asm_core.Expr.t -> Arm_encode.Instruction.t
val movw : ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val movw_s :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Asm_core.Expr.t -> Arm_encode.Instruction.t

val movt : ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> int -> Arm_encode.Instruction.t

val movt_s :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Asm_core.Expr.t -> Arm_encode.Instruction.t

val nop : ?cond:Arm_encode.Cond.t -> unit -> Arm_encode.Instruction.t
val udf : int -> Arm_encode.Instruction.t
val ite : unit -> Arm_encode.Instruction.t
val push : ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t list -> Arm_encode.Instruction.t
val pop : ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t list -> Arm_encode.Instruction.t

val stmia :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t list -> Arm_encode.Instruction.t

val ldmia :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Reg.t list -> Arm_encode.Instruction.t

val vfp :
  ?lower:bool ->
  name:string ->
  enc:Arm_encode.Arm_ops.Enc.t ->
  Arm_encode.Opcode.t ->
  ('a, Arm_encode.Instruction.t) Arm_encode.Arm_ops.Shape.t ->
  ?cond:Arm_encode.Cond.t ->
  'a

val ddd :
  (Arm_encode.Dreg.t -> Arm_encode.Dreg.t -> Arm_encode.Dreg.t -> 'a, 'a) Arm_encode.Arm_ops.Shape.t

val sss :
  (Arm_encode.Sreg.t -> Arm_encode.Sreg.t -> Arm_encode.Sreg.t -> 'a, 'a) Arm_encode.Arm_ops.Shape.t

val dd : (Arm_encode.Dreg.t -> Arm_encode.Dreg.t -> 'a, 'a) Arm_encode.Arm_ops.Shape.t
val ss : (Arm_encode.Sreg.t -> Arm_encode.Sreg.t -> 'a, 'a) Arm_encode.Arm_ops.Shape.t

val vadd_f64 :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Instruction.t

val vsub_f64 :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Instruction.t

val vmul_f64 :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Instruction.t

val vdiv_f64 :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Instruction.t

val vmla_f64 :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Instruction.t

val vadd_f32 :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Instruction.t

val vsub_f32 :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Instruction.t

val vmul_f32 :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Instruction.t

val vdiv_f32 :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Instruction.t

val vmla_f32 :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Sreg.t ->
  Arm_encode.Instruction.t

val vmov_f64 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Dreg.t -> Arm_encode.Dreg.t -> Arm_encode.Instruction.t

val vmov_f64_i : ?cond:Arm_encode.Cond.t -> Arm_encode.Dreg.t -> float -> Arm_encode.Instruction.t

val vmov_f32 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Sreg.t -> Arm_encode.Sreg.t -> Arm_encode.Instruction.t

val vmov_f32_i : ?cond:Arm_encode.Cond.t -> Arm_encode.Sreg.t -> float -> Arm_encode.Instruction.t

val vmov_to_s :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Sreg.t -> Arm_encode.Reg.t -> Arm_encode.Instruction.t

val vmov_from_s :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Reg.t -> Arm_encode.Sreg.t -> Arm_encode.Instruction.t

val vmov_to_d :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Instruction.t

val vmov_from_d :
  ?cond:Arm_encode.Cond.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Reg.t ->
  Arm_encode.Dreg.t ->
  Arm_encode.Instruction.t

val vneg_f64 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Dreg.t -> Arm_encode.Dreg.t -> Arm_encode.Instruction.t

val vneg_f32 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Sreg.t -> Arm_encode.Sreg.t -> Arm_encode.Instruction.t

val vcmp_f64 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Dreg.t -> Arm_encode.Dreg.t -> Arm_encode.Instruction.t

val vcmp_f64_z : ?cond:Arm_encode.Cond.t -> Arm_encode.Dreg.t -> Arm_encode.Instruction.t

val vcmp_f32 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Sreg.t -> Arm_encode.Sreg.t -> Arm_encode.Instruction.t

val vcmp_f32_z : ?cond:Arm_encode.Cond.t -> Arm_encode.Sreg.t -> Arm_encode.Instruction.t
val vmrs : ?cond:Arm_encode.Cond.t -> unit -> Arm_encode.Instruction.t

val vcvt_f32_f64 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Sreg.t -> Arm_encode.Dreg.t -> Arm_encode.Instruction.t

val vcvt_f64_f32 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Dreg.t -> Arm_encode.Sreg.t -> Arm_encode.Instruction.t

val vcvt_f32_s32 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Sreg.t -> Arm_encode.Sreg.t -> Arm_encode.Instruction.t

val vcvt_f64_s32 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Dreg.t -> Arm_encode.Sreg.t -> Arm_encode.Instruction.t

val vcvt_f64_u32 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Dreg.t -> Arm_encode.Sreg.t -> Arm_encode.Instruction.t

val vcvt_s32_f64 :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Sreg.t -> Arm_encode.Dreg.t -> Arm_encode.Instruction.t

val vldr_d :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Dreg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t

val vldr_s :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Sreg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t

val vldr_d_l :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Dreg.t -> Asm_core.Expr.t -> Arm_encode.Instruction.t

val vldr_s_l :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Sreg.t -> Asm_core.Expr.t -> Arm_encode.Instruction.t

val vstr_d :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Dreg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t

val vstr_s :
  ?cond:Arm_encode.Cond.t -> Arm_encode.Sreg.t -> Arm_encode.Mem.t -> Arm_encode.Instruction.t

val vpush_64 : ?cond:Arm_encode.Cond.t -> Arm_encode.Dreg.t list -> Arm_encode.Instruction.t
