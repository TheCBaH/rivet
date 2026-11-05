(* The arm (A32) target's instruction types: registers, operands, conditions, opcodes, instructions
   and lowered forms. The encoder ({!Arm_encode}) builds on them, and so does the per-opcode table
   ({!Arm_ops}), which sits between the two. *)

module C = Codec

module Reg : sig
  type t = { num : int }

  val equal : t -> t -> bool
  val aliases : (string * int) list
  val canonical_name : int -> string
  val name : t -> string
  val pp : Format.formatter -> t -> unit
  val of_num : int -> t
  val find : String.t -> t option
  val sp : t
  val lr : t
  val numbers : int list
  val count : int
  val mem : int -> bool
  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val all : t list
  val range : (int * int) option
  val r0 : t
  val r1 : t
  val r2 : t
  val r3 : t
  val r4 : t
  val r5 : t
  val r6 : t
  val r7 : t
  val r8 : t
  val r9 : t
  val r10 : t
  val r11 : t
  val r12 : t
  val r13 : t
  val r14 : t
  val r15 : t
  val pc : t
  val ip : t
  val fp : t
  val sl : t
end

module Dreg : sig
  type t = { num : int }

  val equal : t -> t -> bool
  val of_num : int -> t
  val name : t -> string
  val pp : Format.formatter -> t -> unit
  val find : string -> t option
  val hilo : t -> int64 * int64
  val of_hilo : int64 -> int64 -> t
  val numbers : int list
  val count : int
  val mem : int -> bool
  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val all : t list
  val range : (int * int) option
  val d0 : t
  val d1 : t
  val d2 : t
  val d3 : t
  val d4 : t
  val d5 : t
  val d6 : t
  val d7 : t
  val d8 : t
  val d9 : t
  val d10 : t
  val d11 : t
  val d12 : t
  val d13 : t
  val d14 : t
  val d15 : t
  val d16 : t
  val d17 : t
  val d18 : t
  val d19 : t
  val d20 : t
  val d21 : t
  val d22 : t
  val d23 : t
  val d24 : t
  val d25 : t
  val d26 : t
  val d27 : t
  val d28 : t
  val d29 : t
  val d30 : t
  val d31 : t
end

module Sreg : sig
  type t = { num : int }

  val equal : t -> t -> bool
  val of_num : int -> t
  val name : t -> string
  val pp : Format.formatter -> t -> unit
  val find : string -> t option
  val hilo : t -> int64 * int64
  val of_hilo : int64 -> int64 -> t
  val numbers : int list
  val count : int
  val mem : int -> bool
  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val all : t list
  val range : (int * int) option
  val s0 : t
  val s1 : t
  val s2 : t
  val s3 : t
  val s4 : t
  val s5 : t
  val s6 : t
  val s7 : t
  val s8 : t
  val s9 : t
  val s10 : t
  val s11 : t
  val s12 : t
  val s13 : t
  val s14 : t
  val s15 : t
  val s16 : t
  val s17 : t
  val s18 : t
  val s19 : t
  val s20 : t
  val s21 : t
  val s22 : t
  val s23 : t
  val s24 : t
  val s25 : t
  val s26 : t
  val s27 : t
  val s28 : t
  val s29 : t
  val s30 : t
  val s31 : t
end

module Lo : sig
  type t = private Reg.t

  val of_reg : Reg.t -> t option
  val to_reg : t -> Reg.t
  val name : t -> string
  val numbers : int list
  val count : int
  val mem : int -> bool
  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val all : t list
  val range : (int * int) option
  val r0 : t
  val r1 : t
  val r2 : t
  val r3 : t
  val r4 : t
  val r5 : t
  val r6 : t
  val r7 : t
end

val shift_name : int -> string
val shift_of_name : string -> int option

module Mem : sig
  type offset =
    | Imm of int64
    | Reg_offset of { reg : Reg.t; negate : bool; kind : int; amount : int }

  type t = { base : Reg.t; offset : offset; writeback : bool; pre : bool }

  val pp : Format.formatter -> t -> unit
end

val decimal_of_float : Float.t -> string
val vfp_expand : int -> float
val vfp_compress : float -> int option
val vfp_imm_domain : float C.Finite.domain

module Operand : sig
  type t =
    | Reg of Reg.t
    | Imm of Foundation.Bigint.t
    | Mem of Mem.t
    | Sym of Asm_core.Expr.t
    | Shifted of { reg : Reg.t; kind : int; amount : int }
    | Dreg of Dreg.t
    | Sreg of Sreg.t
    | FImm of float
    | Reglist of int list
    | Reg_writeback of Reg.t
    | Dreglist of int list

  val pp : Format.formatter -> t -> unit
end

module Surface : sig
  type t = { mnemonic : string; ops : Operand.t list; origin : Foundation.Origin.t }

  val pp : Format.formatter -> t -> unit
end

module Cond : sig
  type t = Eq | Ne | Cs | Cc | Mi | Pl | Vs | Vc | Hi | Ls | Ge | Lt | Gt | Le | Al | Nv

  val all : t list
  val name : t -> string
  val equal : t -> t -> bool
  val code : t -> int
  val synonyms : (string * t) list
  val of_name : String.t -> t option
  val suffix : t -> string
  val split : string -> string * t
end

module V3 : sig
  type op = Vadd | Vsub | Vmul | Vdiv | Vmla
end

module Opcode : sig
  type t =
    | Mov
    | Add
    | Sub
    | And
    | Cmp
    | Cmn
    | Eor
    | Rsb
    | Orr
    | Mvn
    | Bic
    | Adc
    | Sbc
    | Adds
    | Subs
    | Rsbs
    | Orrs
    | Adcs
    | Sbcs
    | Mul
    | Mla
    | Umull
    | Str
    | Ldr
    | Bx
    | Strb
    | Ldrb
    | Strh
    | Ldrh
    | Ldrsb
    | Ldrsh
    | Sbfx
    | Clz
    | Rev
    | Rev16
    | Udf
    | Bl
    | B
    | Movw
    | Movt
    | Ite
    | Nop
    | Shift of int
    | Shifts of int
    | Rrx
    | Rrxs
    | Vmov_d
    | Vmov_s
    | Vmov_core
    | V3d of V3.op
    | V3s of V3.op
    | Vneg_d
    | Vneg_s
    | Vcmp_d
    | Vcmp_s
    | Vmrs
    | Vcvt_f32_f64
    | Vcvt_f64_f32
    | Vcvt_f32_s32
    | Vcvt_f64_s32
    | Vcvt_f64_u32
    | Vcvt_s32_f64
    | Vldr
    | Vstr
    | Vpush
    | Push
    | Stmia
    | Ldmia
    | Pop
end

module Instruction : sig
  type t = { op : Opcode.t; cond : Cond.t; ops : Operand.t list }

  val mk : ?cond:Cond.t -> Opcode.t -> Operand.t list -> t
end

type x_offset = X_imm of int64 | X_reg of { rm : Reg.t; negate : bool }

module Lowered : sig
  type t =
    | Dp_imm of { cond : Cond.t; dp : int; s : bool; rd : Reg.t; rn : Reg.t; imm : int64 }
    | Dp_reg of {
        cond : Cond.t;
        dp : int;
        s : bool;
        rd : Reg.t;
        rn : Reg.t;
        rm : Reg.t;
        sh_kind : int;
        sh_amt : int;
      }
    | Ldst_imm of {
        cond : Cond.t;
        load : bool;
        byte : bool;
        rt : Reg.t;
        rn : Reg.t;
        offset : int64;
        writeback : bool;
        post : bool;
      }
    | Ldst_x of {
        cond : Cond.t;
        kind : int;
        rt : Reg.t;
        rn : Reg.t;
        offset : x_offset;
        post : bool;
      }
    | Rd_rm of { cond : Cond.t; op : Opcode.t; rd : Reg.t; rm : Reg.t }
    | Sbfx of { cond : Cond.t; rd : Reg.t; rn : Reg.t; lsb : int; width : int }
    | Ldst_reg of {
        cond : Cond.t;
        load : bool;
        byte : bool;
        rt : Reg.t;
        rn : Reg.t;
        rm : Reg.t;
        negate : bool;
        sh_kind : int;
        sh_amt : int;
      }
    | Bx of { cond : Cond.t; rm : Reg.t }
    | Push of { cond : Cond.t; regs : int }
    | Ldm of { cond : Cond.t; load : bool; rn : Reg.t; regs : int }
    | Udf of { imm16 : int64 }
    | Bl of { cond : Cond.t; target : Asm_core.Lowered_ast.branch }
    | B of { cond : Cond.t; target : Asm_core.Lowered_ast.branch }
    | Mul of { cond : Cond.t; s : bool; rd : Reg.t; rn : Reg.t; rm : Reg.t }
    | Mla of { cond : Cond.t; s : bool; rd : Reg.t; rn : Reg.t; rm : Reg.t; ra : Reg.t }
    | Umull of { cond : Cond.t; s : bool; rdlo : Reg.t; rdhi : Reg.t; rn : Reg.t; rm : Reg.t }
    | Movw_movt of { cond : Cond.t; top : bool; rd : Reg.t; imm : Asm_core.Lowered_ast.branch }
    | Vmov_reg_d of { cond : Cond.t; vd : Dreg.t; vm : Dreg.t }
    | Vmov_reg_s of { cond : Cond.t; vd : Sreg.t; vm : Sreg.t }
    | Vmov_imm_d of { cond : Cond.t; vd : Dreg.t; imm8 : int }
    | Vmov_imm_s of { cond : Cond.t; vd : Sreg.t; imm8 : int }
    | Vmov_to_single of { cond : Cond.t; sn : Sreg.t; rt : Reg.t }
    | Vmov_from_single of { cond : Cond.t; rt : Reg.t; sn : Sreg.t }
    | Vmov_to_double of { cond : Cond.t; dm : Dreg.t; rt : Reg.t; rt2 : Reg.t }
    | Vmov_from_double of { cond : Cond.t; rt : Reg.t; rt2 : Reg.t; dm : Dreg.t }
    | V3_d of { cond : Cond.t; op : V3.op; vd : Dreg.t; vn : Dreg.t; vm : Dreg.t }
    | V3_s of { cond : Cond.t; op : V3.op; vd : Sreg.t; vn : Sreg.t; vm : Sreg.t }
    | Vneg_d of { cond : Cond.t; vd : Dreg.t; vm : Dreg.t }
    | Vneg_s of { cond : Cond.t; vd : Sreg.t; vm : Sreg.t }
    | Vcmp_reg_d of { cond : Cond.t; vd : Dreg.t; vm : Dreg.t }
    | Vcmp_reg_s of { cond : Cond.t; vd : Sreg.t; vm : Sreg.t }
    | Vcmp_zero_d of { cond : Cond.t; vd : Dreg.t }
    | Vcmp_zero_s of { cond : Cond.t; vd : Sreg.t }
    | Vmrs of { cond : Cond.t }
    | Vcvt_f32_f64 of { cond : Cond.t; sd : Sreg.t; dm : Dreg.t }
    | Vcvt_f64_f32 of { cond : Cond.t; dd : Dreg.t; sm : Sreg.t }
    | Vcvt_f32_s32 of { cond : Cond.t; sd : Sreg.t; sm : Sreg.t }
    | Vcvt_f64_s32 of { cond : Cond.t; dd : Dreg.t; sm : Sreg.t }
    | Vcvt_f64_u32 of { cond : Cond.t; dd : Dreg.t; sm : Sreg.t }
    | Vcvt_s32_f64 of { cond : Cond.t; sd : Sreg.t; dm : Dreg.t }
    | Vmem_d of { cond : Cond.t; load : bool; vd : Dreg.t; rn : Reg.t; offset : int64 }
    | Vmem_s of { cond : Cond.t; load : bool; vd : Sreg.t; rn : Reg.t; offset : int64 }
    | Vldr_lit_d of { cond : Cond.t; vd : Dreg.t; target : Asm_core.Lowered_ast.branch }
    | Vldr_lit_s of { cond : Cond.t; vd : Sreg.t; target : Asm_core.Lowered_ast.branch }
    | Vpush of { cond : Cond.t; vd : Dreg.t; count : int }
    | Nop of { cond : Cond.t }
    | Shift_reg of { cond : Cond.t; kind : int; rd : Reg.t; rm : Reg.t; rs : Reg.t }
end
