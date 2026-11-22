(* Typed builders for the hand-written ARM (A32) instructions, one per operand shape of each
   opcode and named by the mnemonic ([.] becomes [_], a keyword gains [_]) with a suffix for the
   shape: [_i] an immediate, [_sh] a shifted register, [_post] a post-indexed address, [_s] a
   label or symbol expression where the shape also takes a number, [_l] a literal label, [_z]
   zero. Every builder of an instruction with a condition takes [?cond]; a flag-setting form is an
   opcode of its own ([adds]). Immediates are [int] ([int64] for a modified immediate); lowering
   still checks ranges. Core registers are {!Rivet_arm_encode.Reg.t}, double-precision ones
   {!Rivet_arm_encode.Dreg.t} and single-precision ones {!Rivet_arm_encode.Sreg.t}, so a [d] register where an
   [s] one is expected does not compile. *)

module Cond = Rivet_arm_encode.Cond
module Reg = Rivet_arm_encode.Reg
module Dreg = Rivet_arm_encode.Dreg
module Sreg = Rivet_arm_encode.Sreg
module Lo = Rivet_arm_encode.Lo

type shift = Rivet_arm_encode.Arm_ops.Shift.kind = Lsl | Lsr | Asr | Ror
type shifted = Rivet_arm_encode.Arm_ops.Shift.t

val shifted :
  Rivet_arm_encode.Arm_ops.Shift.kind ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Arm_ops.Shift.t

val mem : ?off:int -> Rivet_arm_encode__Arm_types.Reg.t -> Rivet_arm_encode.Mem.t
val mem_wb : ?off:int -> Rivet_arm_encode__Arm_types.Reg.t -> Rivet_arm_encode.Mem.t

val mem_reg :
  ?negate:bool ->
  Rivet_arm_encode__Arm_types.Reg.t ->
  Rivet_arm_encode__Arm_types.Reg.t ->
  Rivet_arm_encode.Mem.t

val mem_shifted :
  ?negate:bool ->
  Rivet_arm_encode__Arm_types.Reg.t ->
  Rivet_arm_encode__Arm_types.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.kind ->
  int ->
  Rivet_arm_encode.Mem.t

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
  enc:Rivet_arm_encode.Arm_ops.Enc.t ->
  Rivet_arm_encode.Opcode.t ->
  ('a, Rivet_arm_encode.Instruction.t) Rivet_arm_encode.Arm_ops.Shape.t ->
  'a

val dp_ :
  ?s:bool -> ?form:Rivet_arm_encode.Arm_ops.Enc.dp_form -> int -> Rivet_arm_encode.Arm_ops.Enc.t

val rrr :
  ( Rivet_arm_encode.Reg.t -> Rivet_arm_encode.Reg.t -> Rivet_arm_encode.Reg.t -> 'a,
    'a )
  Rivet_arm_encode.Arm_ops.Shape.t

val rri :
  ( Rivet_arm_encode.Reg.t -> Rivet_arm_encode.Reg.t -> int64 -> 'a,
    'a )
  Rivet_arm_encode.Arm_ops.Shape.t

val rrsh :
  ( Rivet_arm_encode.Reg.t -> Rivet_arm_encode.Reg.t -> Rivet_arm_encode.Arm_ops.Shift.t -> 'a,
    'a )
  Rivet_arm_encode.Arm_ops.Shape.t

val rr :
  (Rivet_arm_encode.Reg.t -> Rivet_arm_encode.Reg.t -> 'a, 'a) Rivet_arm_encode.Arm_ops.Shape.t

val shorthand : Rivet_arm_encode.Arm_ops.Kind.t list list
val three_move : Rivet_arm_encode.Arm_ops.Kind.t list list

val mov :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val mov_i :
  ?cond:Rivet_arm_encode.Cond.t -> Rivet_arm_encode.Reg.t -> int64 -> Rivet_arm_encode.Instruction.t

val mov_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val mvn :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val mvn_i :
  ?cond:Rivet_arm_encode.Cond.t -> Rivet_arm_encode.Reg.t -> int64 -> Rivet_arm_encode.Instruction.t

val mvn_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val add :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val add_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val add_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int64 ->
  Rivet_arm_encode.Instruction.t

val sub :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val sub_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val sub_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int64 ->
  Rivet_arm_encode.Instruction.t

val and_ :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val and_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val and_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int64 ->
  Rivet_arm_encode.Instruction.t

val eor :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val eor_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val eor_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int64 ->
  Rivet_arm_encode.Instruction.t

val rsb :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val rsb_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val rsb_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int64 ->
  Rivet_arm_encode.Instruction.t

val orr :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val orr_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val orr_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int64 ->
  Rivet_arm_encode.Instruction.t

val bic :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val bic_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val bic_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int64 ->
  Rivet_arm_encode.Instruction.t

val adc :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val adc_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val adc_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int64 ->
  Rivet_arm_encode.Instruction.t

val sbc :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val sbc_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val cmp :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val cmp_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val cmp_i :
  ?cond:Rivet_arm_encode.Cond.t -> Rivet_arm_encode.Reg.t -> int64 -> Rivet_arm_encode.Instruction.t

val cmn :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val cmn_sh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Arm_ops.Shift.t ->
  Rivet_arm_encode.Instruction.t

val cmn_i :
  ?cond:Rivet_arm_encode.Cond.t -> Rivet_arm_encode.Reg.t -> int64 -> Rivet_arm_encode.Instruction.t

val adds :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val adds_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int64 ->
  Rivet_arm_encode.Instruction.t

val subs :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val subs_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int64 ->
  Rivet_arm_encode.Instruction.t

val rsbs_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int64 ->
  Rivet_arm_encode.Instruction.t

val orrs :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val adcs :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val sbcs :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val shift_reg :
  ( Rivet_arm_encode.Reg.t -> Rivet_arm_encode.Reg.t -> Rivet_arm_encode.Reg.t -> 'a,
    'a )
  Rivet_arm_encode.Arm_ops.Shape.t

val shift_imm :
  ( Rivet_arm_encode.Reg.t -> Rivet_arm_encode.Reg.t -> int -> 'a,
    'a )
  Rivet_arm_encode.Arm_ops.Shape.t

val shift :
  name:string ->
  int ->
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val shift_i :
  int ->
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val shifts :
  name:string ->
  int ->
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val lsl_ :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val lsl_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val lsr_ :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val lsr_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val asr_ :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val asr_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val ror :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val ror_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val lsls_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val lsrs_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val asrs_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val rors_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val rrx :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val rrxs :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val mul :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val mla :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val umull :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val ldst :
  name:string ->
  load:bool ->
  byte:bool ->
  Rivet_arm_encode.Opcode.t ->
  (?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t)
  * (?cond:Rivet_arm_encode.Cond.t ->
    Rivet_arm_encode.Reg.t ->
    Rivet_arm_encode.Reg.t ->
    int ->
    Rivet_arm_encode.Instruction.t)

val str :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t

val str_post :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val ldr :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t

val ldr_post :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val strb :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t

val strb_post :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val ldrb :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t

val ldrb_post :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val ldst_x :
  name:string ->
  int ->
  int64 * int64 * int64 ->
  Rivet_arm_encode.Opcode.t ->
  (?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t)
  * (?cond:Rivet_arm_encode.Cond.t ->
    Rivet_arm_encode.Reg.t ->
    Rivet_arm_encode.Reg.t ->
    int ->
    Rivet_arm_encode.Instruction.t)

val strh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t

val strh_post :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val ldrh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t

val ldrh_post :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val ldrsb :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t

val ldrsb_post :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val ldrsh :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t

val ldrsh_post :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  Rivet_arm_encode.Instruction.t

val rd_rm :
  name:string ->
  int64 ->
  int64 ->
  Rivet_arm_encode.Opcode.t ->
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val clz :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val rev :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val rev16 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val sbfx :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  int ->
  int ->
  Rivet_arm_encode.Instruction.t

val bx : ?cond:Rivet_arm_encode.Cond.t -> Rivet_arm_encode.Reg.t -> Rivet_arm_encode.Instruction.t
val b : ?cond:Rivet_arm_encode.Cond.t -> Rivet_asm_core.Expr.t -> Rivet_arm_encode.Instruction.t
val bl : ?cond:Rivet_arm_encode.Cond.t -> Rivet_asm_core.Expr.t -> Rivet_arm_encode.Instruction.t

val movw :
  ?cond:Rivet_arm_encode.Cond.t -> Rivet_arm_encode.Reg.t -> int -> Rivet_arm_encode.Instruction.t

val movw_s :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_arm_encode.Instruction.t

val movt :
  ?cond:Rivet_arm_encode.Cond.t -> Rivet_arm_encode.Reg.t -> int -> Rivet_arm_encode.Instruction.t

val movt_s :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_arm_encode.Instruction.t

val nop : ?cond:Rivet_arm_encode.Cond.t -> unit -> Rivet_arm_encode.Instruction.t
val udf : int -> Rivet_arm_encode.Instruction.t
val ite : unit -> Rivet_arm_encode.Instruction.t

val push :
  ?cond:Rivet_arm_encode.Cond.t -> Rivet_arm_encode.Reg.t list -> Rivet_arm_encode.Instruction.t

val pop :
  ?cond:Rivet_arm_encode.Cond.t -> Rivet_arm_encode.Reg.t list -> Rivet_arm_encode.Instruction.t

val stmia :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t list ->
  Rivet_arm_encode.Instruction.t

val ldmia :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t list ->
  Rivet_arm_encode.Instruction.t

val vfp :
  ?lower:bool ->
  name:string ->
  enc:Rivet_arm_encode.Arm_ops.Enc.t ->
  Rivet_arm_encode.Opcode.t ->
  ('a, Rivet_arm_encode.Instruction.t) Rivet_arm_encode.Arm_ops.Shape.t ->
  ?cond:Rivet_arm_encode.Cond.t ->
  'a

val ddd :
  ( Rivet_arm_encode.Dreg.t -> Rivet_arm_encode.Dreg.t -> Rivet_arm_encode.Dreg.t -> 'a,
    'a )
  Rivet_arm_encode.Arm_ops.Shape.t

val sss :
  ( Rivet_arm_encode.Sreg.t -> Rivet_arm_encode.Sreg.t -> Rivet_arm_encode.Sreg.t -> 'a,
    'a )
  Rivet_arm_encode.Arm_ops.Shape.t

val dd :
  (Rivet_arm_encode.Dreg.t -> Rivet_arm_encode.Dreg.t -> 'a, 'a) Rivet_arm_encode.Arm_ops.Shape.t

val ss :
  (Rivet_arm_encode.Sreg.t -> Rivet_arm_encode.Sreg.t -> 'a, 'a) Rivet_arm_encode.Arm_ops.Shape.t

val vadd_f64 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Instruction.t

val vsub_f64 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Instruction.t

val vmul_f64 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Instruction.t

val vdiv_f64 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Instruction.t

val vmla_f64 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Instruction.t

val vadd_f32 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vsub_f32 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vmul_f32 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vdiv_f32 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vmla_f32 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vmov_f64 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Instruction.t

val vmov_f64_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  float ->
  Rivet_arm_encode.Instruction.t

val vmov_f32 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vmov_f32_i :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  float ->
  Rivet_arm_encode.Instruction.t

val vmov_to_s :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val vmov_from_s :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vmov_to_d :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Instruction.t

val vmov_from_d :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Reg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Instruction.t

val vneg_f64 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Instruction.t

val vneg_f32 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vcmp_f64 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Instruction.t

val vcmp_f64_z :
  ?cond:Rivet_arm_encode.Cond.t -> Rivet_arm_encode.Dreg.t -> Rivet_arm_encode.Instruction.t

val vcmp_f32 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vcmp_f32_z :
  ?cond:Rivet_arm_encode.Cond.t -> Rivet_arm_encode.Sreg.t -> Rivet_arm_encode.Instruction.t

val vmrs : ?cond:Rivet_arm_encode.Cond.t -> unit -> Rivet_arm_encode.Instruction.t

val vcvt_f32_f64 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Instruction.t

val vcvt_f64_f32 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vcvt_f32_s32 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vcvt_f64_s32 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vcvt_f64_u32 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Instruction.t

val vcvt_s32_f64 :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Instruction.t

val vldr_d :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t

val vldr_s :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t

val vldr_d_l :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_arm_encode.Instruction.t

val vldr_s_l :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_arm_encode.Instruction.t

val vstr_d :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Dreg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t

val vstr_s :
  ?cond:Rivet_arm_encode.Cond.t ->
  Rivet_arm_encode.Sreg.t ->
  Rivet_arm_encode.Mem.t ->
  Rivet_arm_encode.Instruction.t

val vpush_64 :
  ?cond:Rivet_arm_encode.Cond.t -> Rivet_arm_encode.Dreg.t list -> Rivet_arm_encode.Instruction.t
