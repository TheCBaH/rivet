(* One definition per hand-written ARM (A32) opcode: its spelling, the operand lists it takes, the
   fields of its encoding class, whether it carries a condition and sets the flags, and whether its
   lowering stays code. Each definition is also the opcode's typed builder: one per operand shape,
   named by the mnemonic with a suffix for the shape ([add r0 r1 r2], [add_i r0 r1 4L],
   [add_sh r0 r1 (shifted Lsl r2 2)], [ldr r0 (mem r1)], [ldr_post r0 r1 4]); a conditional one
   takes [?cond]. A flag-setting form is an opcode of its own ([adds]). An instruction that
   lowering reads as VFP is told apart from the integer ones by its register classes: {!Dreg.t} and
   {!Sreg.t} are different types, so a [d] register where an [s] one is expected does not compile. *)

module Shift : sig
  type kind = Lsl | Lsr | Asr | Ror

  val code : kind -> int
  val of_code : int -> kind

  type t = { reg : Arm_types.Reg.t; kind : kind; amount : int }
end

module Kind : sig
  type t = R | D | S | Imm | Fimm | Mem | Sym | Sh | Reglist | Dreglist | Wb

  val name : t -> string
  val of_operand : Arm_types.Operand.t -> t
end

module Shape : sig
  type ('f, 'r) t =
    | Done : ('r, 'r) t
    | Unit : (unit -> 'r, 'r) t
    | Vmrs : (unit -> 'r, 'r) t
    | R : ('f, 'r) t -> (Arm_types.Reg.t -> 'f, 'r) t
    | D : ('f, 'r) t -> (Arm_types.Dreg.t -> 'f, 'r) t
    | S : ('f, 'r) t -> (Arm_types.Sreg.t -> 'f, 'r) t
    | Imm : ('f, 'r) t -> (int -> 'f, 'r) t
    | Imm64 : ('f, 'r) t -> (int64 -> 'f, 'r) t
    | Zero : ('f, 'r) t -> ('f, 'r) t
    | Fimm : ('f, 'r) t -> (float -> 'f, 'r) t
    | Mem : ('f, 'r) t -> (Arm_types.Mem.t -> 'f, 'r) t
    | Post : ('f, 'r) t -> (Arm_types.Reg.t -> int -> 'f, 'r) t
    | Sym : ('f, 'r) t -> (Asm_core.Expr.t -> 'f, 'r) t
    | Lo16 : ('f, 'r) t -> (Asm_core.Expr.t -> 'f, 'r) t
    | Hi16 : ('f, 'r) t -> (Asm_core.Expr.t -> 'f, 'r) t
    | Sh : ('f, 'r) t -> (Shift.t -> 'f, 'r) t
    | Reglist : ('f, 'r) t -> (Arm_types.Reg.t list -> 'f, 'r) t
    | Dreglist : ('f, 'r) t -> (Arm_types.Dreg.t list -> 'f, 'r) t
    | Wb : ('f, 'r) t -> (Arm_types.Reg.t -> 'f, 'r) t

  val kinds : ('f, 'r) t -> Kind.t list
  val int : int -> Arm_types.Operand.t
  val mem0 : Arm_encode__Arm_types.Reg.t -> Arm_types.Operand.t
  val numbers : Arm_types.Reg.t list -> int list
  val dnumbers : Arm_types.Dreg.t list -> int list
  val build : ('f, 'r) t -> (Arm_types.Operand.t list -> 'r) -> 'f
  val sample : ('f, 'r) t -> int -> int -> 'f -> 'r
end

module Enc : sig
  type dp_form = Normal | Move | Compare
  type cvt = F32_f64 | F64_f32 | F32_s32 | F64_s32 | F64_u32 | S32_f64

  type t =
    | Dp of { dp : int; s : bool; form : dp_form }
    | Shift_op of { kind : int; s : bool }
    | Rrx of { s : bool }
    | Mul
    | Mla
    | Umull
    | Rd_rm of { hi : int64; lo : int64 }
    | Sbfx
    | Bx
    | B
    | Bl
    | Nop
    | Udf
    | Movw
    | Movt
    | It
    | Ldst of { load : bool; byte : bool }
    | Ldst_x of { kind : int; bits : int64 * int64 * int64 }
    | Push
    | Pop
    | Ldm of { load : bool }
    | V3 of { dbl : bool; sel : int64 * int64 * int64 * int64 }
    | Vmov of { dbl : bool }
    | Vmov_core
    | Vneg of { dbl : bool }
    | Vcmp of { dbl : bool }
    | Vmrs
    | Vcvt of cvt
    | Vmem of { load : bool }
    | Vpush
end

type group = Core | Vfp

type shape = {
  kinds : Kind.t list;
  sample : cond:Arm_types.Cond.t -> int -> Arm_types.Instruction.t;
}

type entry = {
  op : Arm_types.Opcode.t;
  name : string;
  group : group;
  enc : Enc.t;
  conditional : bool;
  sets_flags : bool;
  ext : string list;
  lower : bool;
  mutable shapes : shape list;
  also : Kind.t list list;
}

val registry : entry list ref

val builder :
  Arm_encode__Arm_types.Opcode.t ->
  ('a, Arm_types.Instruction.t) Shape.t ->
  ?cond:Arm_types.Cond.t ->
  'a

val shape_of : Arm_encode__Arm_types.Opcode.t -> ('a, Arm_types.Instruction.t) Shape.t -> shape

val def :
  ?group:group ->
  ?conditional:bool ->
  ?sets_flags:bool ->
  ?lower:bool ->
  ?also:Kind.t list list ->
  name:string ->
  enc:Enc.t ->
  Arm_types.Opcode.t ->
  ('a, Arm_types.Instruction.t) Shape.t ->
  ?cond:Arm_types.Cond.t ->
  'a

val alt :
  Arm_types.Opcode.t -> ('a, Arm_types.Instruction.t) Shape.t -> ?cond:Arm_types.Cond.t -> 'a

module Builders : sig
  val uncond :
    name:string -> enc:Enc.t -> Arm_types.Opcode.t -> ('a, Arm_types.Instruction.t) Shape.t -> 'a

  val dp_ : ?s:bool -> ?form:Enc.dp_form -> int -> Enc.t
  val rrr : (Arm_types.Reg.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> 'a, 'a) Shape.t
  val rri : (Arm_types.Reg.t -> Arm_types.Reg.t -> int64 -> 'a, 'a) Shape.t
  val rrsh : (Arm_types.Reg.t -> Arm_types.Reg.t -> Shift.t -> 'a, 'a) Shape.t
  val rr : (Arm_types.Reg.t -> Arm_types.Reg.t -> 'a, 'a) Shape.t
  val shorthand : Kind.t list list
  val three_move : Kind.t list list
  val mov : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> Arm_types.Instruction.t
  val mov_i : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t
  val mov_sh : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Shift.t -> Arm_types.Instruction.t
  val mvn : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> Arm_types.Instruction.t
  val mvn_i : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t
  val mvn_sh : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Shift.t -> Arm_types.Instruction.t

  val add :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val add_sh :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Shift.t ->
    Arm_types.Instruction.t

  val add_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t

  val sub :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val sub_sh :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Shift.t ->
    Arm_types.Instruction.t

  val sub_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t

  val and_ :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val and_sh :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Shift.t ->
    Arm_types.Instruction.t

  val and_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t

  val eor :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val eor_sh :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Shift.t ->
    Arm_types.Instruction.t

  val eor_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t

  val rsb :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val rsb_sh :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Shift.t ->
    Arm_types.Instruction.t

  val rsb_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t

  val orr :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val orr_sh :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Shift.t ->
    Arm_types.Instruction.t

  val orr_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t

  val bic :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val bic_sh :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Shift.t ->
    Arm_types.Instruction.t

  val bic_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t

  val adc :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val adc_sh :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Shift.t ->
    Arm_types.Instruction.t

  val adc_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t

  val sbc :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val sbc_sh :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Shift.t ->
    Arm_types.Instruction.t

  val cmp : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> Arm_types.Instruction.t
  val cmp_sh : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Shift.t -> Arm_types.Instruction.t
  val cmp_i : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t
  val cmn : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> Arm_types.Instruction.t
  val cmn_sh : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Shift.t -> Arm_types.Instruction.t
  val cmn_i : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t

  val adds :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val adds_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t

  val subs :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val subs_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t

  val rsbs_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int64 -> Arm_types.Instruction.t

  val orrs :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val adcs :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val sbcs :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val shift_reg : (Arm_types.Reg.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> 'a, 'a) Shape.t
  val shift_imm : (Arm_types.Reg.t -> Arm_types.Reg.t -> int -> 'a, 'a) Shape.t

  val shift :
    name:string ->
    int ->
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val shift_i :
    int ->
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    int ->
    Arm_types.Instruction.t

  val shifts :
    name:string ->
    int ->
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    int ->
    Arm_types.Instruction.t

  val lsl_ :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val lsl_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val lsr_ :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val lsr_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val asr_ :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val asr_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val ror :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val ror_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val lsls_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val lsrs_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val asrs_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val rors_i :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val rrx : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> Arm_types.Instruction.t
  val rrxs : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> Arm_types.Instruction.t

  val mul :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val mla :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val umull :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val ldst :
    name:string ->
    load:bool ->
    byte:bool ->
    Arm_types.Opcode.t ->
    (?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t)
    * (?cond:Arm_types.Cond.t ->
      Arm_types.Reg.t ->
      Arm_types.Reg.t ->
      int ->
      Arm_types.Instruction.t)

  val str : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t

  val str_post :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val ldr : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t

  val ldr_post :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val strb : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t

  val strb_post :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val ldrb : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t

  val ldrb_post :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val ldst_x :
    name:string ->
    int ->
    int64 * int64 * int64 ->
    Arm_types.Opcode.t ->
    (?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t)
    * (?cond:Arm_types.Cond.t ->
      Arm_types.Reg.t ->
      Arm_types.Reg.t ->
      int ->
      Arm_types.Instruction.t)

  val strh : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t

  val strh_post :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val ldrh : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t

  val ldrh_post :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val ldrsb :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t

  val ldrsb_post :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val ldrsh :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t

  val ldrsh_post :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val rd_rm :
    name:string ->
    int64 ->
    int64 ->
    Arm_types.Opcode.t ->
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val clz : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> Arm_types.Instruction.t
  val rev : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> Arm_types.Instruction.t

  val rev16 :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t -> Arm_types.Instruction.t

  val sbfx :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    int ->
    int ->
    Arm_types.Instruction.t

  val bx : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Instruction.t
  val b : ?cond:Arm_types.Cond.t -> Asm_core.Expr.t -> Arm_types.Instruction.t
  val bl : ?cond:Arm_types.Cond.t -> Asm_core.Expr.t -> Arm_types.Instruction.t
  val movw : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val movw_s :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Asm_core.Expr.t -> Arm_types.Instruction.t

  val movt : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> int -> Arm_types.Instruction.t

  val movt_s :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Asm_core.Expr.t -> Arm_types.Instruction.t

  val nop : ?cond:Arm_types.Cond.t -> unit -> Arm_types.Instruction.t
  val udf : int -> Arm_types.Instruction.t
  val ite : unit -> Arm_types.Instruction.t
  val push : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t list -> Arm_types.Instruction.t
  val pop : ?cond:Arm_types.Cond.t -> Arm_types.Reg.t list -> Arm_types.Instruction.t

  val stmia :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t list -> Arm_types.Instruction.t

  val ldmia :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Reg.t list -> Arm_types.Instruction.t

  val vfp :
    ?lower:bool ->
    name:string ->
    enc:Enc.t ->
    Arm_types.Opcode.t ->
    ('a, Arm_types.Instruction.t) Shape.t ->
    ?cond:Arm_types.Cond.t ->
    'a

  val ddd : (Arm_types.Dreg.t -> Arm_types.Dreg.t -> Arm_types.Dreg.t -> 'a, 'a) Shape.t
  val sss : (Arm_types.Sreg.t -> Arm_types.Sreg.t -> Arm_types.Sreg.t -> 'a, 'a) Shape.t
  val dd : (Arm_types.Dreg.t -> Arm_types.Dreg.t -> 'a, 'a) Shape.t
  val ss : (Arm_types.Sreg.t -> Arm_types.Sreg.t -> 'a, 'a) Shape.t

  val vadd_f64 :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Dreg.t ->
    Arm_types.Dreg.t ->
    Arm_types.Dreg.t ->
    Arm_types.Instruction.t

  val vsub_f64 :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Dreg.t ->
    Arm_types.Dreg.t ->
    Arm_types.Dreg.t ->
    Arm_types.Instruction.t

  val vmul_f64 :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Dreg.t ->
    Arm_types.Dreg.t ->
    Arm_types.Dreg.t ->
    Arm_types.Instruction.t

  val vdiv_f64 :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Dreg.t ->
    Arm_types.Dreg.t ->
    Arm_types.Dreg.t ->
    Arm_types.Instruction.t

  val vmla_f64 :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Dreg.t ->
    Arm_types.Dreg.t ->
    Arm_types.Dreg.t ->
    Arm_types.Instruction.t

  val vadd_f32 :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Sreg.t ->
    Arm_types.Sreg.t ->
    Arm_types.Sreg.t ->
    Arm_types.Instruction.t

  val vsub_f32 :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Sreg.t ->
    Arm_types.Sreg.t ->
    Arm_types.Sreg.t ->
    Arm_types.Instruction.t

  val vmul_f32 :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Sreg.t ->
    Arm_types.Sreg.t ->
    Arm_types.Sreg.t ->
    Arm_types.Instruction.t

  val vdiv_f32 :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Sreg.t ->
    Arm_types.Sreg.t ->
    Arm_types.Sreg.t ->
    Arm_types.Instruction.t

  val vmla_f32 :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Sreg.t ->
    Arm_types.Sreg.t ->
    Arm_types.Sreg.t ->
    Arm_types.Instruction.t

  val vmov_f64 :
    ?cond:Arm_types.Cond.t -> Arm_types.Dreg.t -> Arm_types.Dreg.t -> Arm_types.Instruction.t

  val vmov_f64_i : ?cond:Arm_types.Cond.t -> Arm_types.Dreg.t -> float -> Arm_types.Instruction.t

  val vmov_f32 :
    ?cond:Arm_types.Cond.t -> Arm_types.Sreg.t -> Arm_types.Sreg.t -> Arm_types.Instruction.t

  val vmov_f32_i : ?cond:Arm_types.Cond.t -> Arm_types.Sreg.t -> float -> Arm_types.Instruction.t

  val vmov_to_s :
    ?cond:Arm_types.Cond.t -> Arm_types.Sreg.t -> Arm_types.Reg.t -> Arm_types.Instruction.t

  val vmov_from_s :
    ?cond:Arm_types.Cond.t -> Arm_types.Reg.t -> Arm_types.Sreg.t -> Arm_types.Instruction.t

  val vmov_to_d :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Dreg.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Instruction.t

  val vmov_from_d :
    ?cond:Arm_types.Cond.t ->
    Arm_types.Reg.t ->
    Arm_types.Reg.t ->
    Arm_types.Dreg.t ->
    Arm_types.Instruction.t

  val vneg_f64 :
    ?cond:Arm_types.Cond.t -> Arm_types.Dreg.t -> Arm_types.Dreg.t -> Arm_types.Instruction.t

  val vneg_f32 :
    ?cond:Arm_types.Cond.t -> Arm_types.Sreg.t -> Arm_types.Sreg.t -> Arm_types.Instruction.t

  val vcmp_f64 :
    ?cond:Arm_types.Cond.t -> Arm_types.Dreg.t -> Arm_types.Dreg.t -> Arm_types.Instruction.t

  val vcmp_f64_z : ?cond:Arm_types.Cond.t -> Arm_types.Dreg.t -> Arm_types.Instruction.t

  val vcmp_f32 :
    ?cond:Arm_types.Cond.t -> Arm_types.Sreg.t -> Arm_types.Sreg.t -> Arm_types.Instruction.t

  val vcmp_f32_z : ?cond:Arm_types.Cond.t -> Arm_types.Sreg.t -> Arm_types.Instruction.t
  val vmrs : ?cond:Arm_types.Cond.t -> unit -> Arm_types.Instruction.t

  val vcvt_f32_f64 :
    ?cond:Arm_types.Cond.t -> Arm_types.Sreg.t -> Arm_types.Dreg.t -> Arm_types.Instruction.t

  val vcvt_f64_f32 :
    ?cond:Arm_types.Cond.t -> Arm_types.Dreg.t -> Arm_types.Sreg.t -> Arm_types.Instruction.t

  val vcvt_f32_s32 :
    ?cond:Arm_types.Cond.t -> Arm_types.Sreg.t -> Arm_types.Sreg.t -> Arm_types.Instruction.t

  val vcvt_f64_s32 :
    ?cond:Arm_types.Cond.t -> Arm_types.Dreg.t -> Arm_types.Sreg.t -> Arm_types.Instruction.t

  val vcvt_f64_u32 :
    ?cond:Arm_types.Cond.t -> Arm_types.Dreg.t -> Arm_types.Sreg.t -> Arm_types.Instruction.t

  val vcvt_s32_f64 :
    ?cond:Arm_types.Cond.t -> Arm_types.Sreg.t -> Arm_types.Dreg.t -> Arm_types.Instruction.t

  val vldr_d :
    ?cond:Arm_types.Cond.t -> Arm_types.Dreg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t

  val vldr_s :
    ?cond:Arm_types.Cond.t -> Arm_types.Sreg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t

  val vldr_d_l :
    ?cond:Arm_types.Cond.t -> Arm_types.Dreg.t -> Asm_core.Expr.t -> Arm_types.Instruction.t

  val vldr_s_l :
    ?cond:Arm_types.Cond.t -> Arm_types.Sreg.t -> Asm_core.Expr.t -> Arm_types.Instruction.t

  val vstr_d :
    ?cond:Arm_types.Cond.t -> Arm_types.Dreg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t

  val vstr_s :
    ?cond:Arm_types.Cond.t -> Arm_types.Sreg.t -> Arm_types.Mem.t -> Arm_types.Instruction.t

  val vpush_64 : ?cond:Arm_types.Cond.t -> Arm_types.Dreg.t list -> Arm_types.Instruction.t
end

val all : entry list
val find_table : (Arm_types.Opcode.t, entry) Hashtbl.t
val find : Arm_types.Opcode.t -> entry
val find_opt : Arm_types.Opcode.t -> entry option
val opcodes : Arm_types.Opcode.t list
val name : Arm_types.Opcode.t -> string
val core_spellings : (string, Arm_types.Opcode.t) Hashtbl.t
val of_mnemonic : string -> Arm_types.Opcode.t option
val vfp_spellings : (string * string list, Arm_types.Opcode.t) Hashtbl.t
val split_name : string -> string * string list
val vfp_bases : string list
val vfp_opcode_of : string -> string list -> Arm_types.Opcode.t option
val split_vfp_mnemonic : string -> (string * Arm_types.Cond.t * string list) option
val to_dp : Arm_types.Opcode.t -> int
val of_dp : int -> Arm_types.Opcode.t option
val is_compare : Arm_types.Opcode.t -> bool
val x_entries : (int * string * (int64 * int64 * int64)) list
val x_name : int -> string
val x_bits : int -> int64 * int64 * int64
val x_kind_of_bits : int64 * int64 * int64 -> int option
val rd_rm : Arm_types.Opcode.t -> (int64 * int64) option
val v3_selector : Arm_types.V3.op -> int64 * int64 * int64 * int64
val v3_of_selector : int64 * int64 * int64 * int64 -> Arm_encode__Arm_types.V3.op option
val v3_name : Arm_types.V3.op -> string
val requirement : Arm_types.Opcode.t -> Target_component.Req.t option
val of_enc : Enc.t -> Arm_types.Opcode.t option
val s_form : int -> Kind.t list -> Arm_types.Opcode.t option
