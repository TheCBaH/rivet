(* The aarch64 (A64) target's instruction types: registers, operands, conditions, opcodes and
   instructions. The encoder ({!Aarch64_encode}) builds on them, and so does the per-opcode table
   ({!Aarch64_ops}), which sits between the two. *)

module Reg : sig
  type t = { num : int; width : int; is_sp : bool }

  val equal : t -> t -> bool
  val name : t -> string
  val pp : Format.formatter -> t -> unit
  val find : string -> t option
  val of_num : width:int -> sp:bool -> int -> t
  val x30 : t
end

module Freg : sig
  type t = { num : int; double : bool }

  val equal : t -> t -> bool
  val name : t -> string
  val pp : Format.formatter -> t -> unit
  val find : string -> t option
  val of_num : double:bool -> int -> t
end

module Extend : sig
  type t = Uxtb | Uxth | Uxtw | Uxtx | Sxtb | Sxth | Sxtw | Sxtx | Lsl_ext

  val all : t list
  val name : t -> string
  val of_name : String.t -> t option
end

module Disp : sig
  type t =
    | Const of int64
    | Sym of Asm_core.Expr.t
    | Reg of { index : Reg.t; extend : Extend.t; amount : int option }

  val zero : t
  val is_zero : t -> bool
  val equal : t -> t -> bool
  val to_string : t -> string
end

module Mem : sig
  type t = { base : Reg.t; offset : Disp.t; writeback : bool; pre : bool }

  val pp : Format.formatter -> t -> unit
end

module Shift : sig
  type kind = Lsl | Lsr | Asr | Ror | Ext of Extend.t
  type t = { kind : kind; amount : int }

  val name : kind -> string
  val of_name : String.t -> kind option
end

module Varr : sig
  type t = B8 | B16 | S2 | S4 | D2

  val name : t -> string
  val of_name : string -> t option
  val equal : t -> t -> bool
  val q : t -> int
  val sz : t -> int
end

module Lane : sig
  type t = S | D

  val name : t -> string
  val of_name : string -> t option
  val lanes : t -> int
  val imm5 : t -> int -> int
  val of_imm5 : int -> (t * int) option
end

module View : sig
  type x = [ `X ]
  type w = [ `W ]
  type gp = [ `Gp ]
  type zr = [ `Zr ]
  type sp = [ `Sp ]
  type d = [ `D ]
  type s = [ `S ]
  type (+'w, +'k) greg = private Reg.t
  type +'s freg = private Freg.t
  type qreg = private int
  type +'a vec = private int * Varr.t
  type lane = private int * Lane.t * int
  type vlist = private int * Varr.t
  type vlist_lane = private int * Lane.t * int

  module Unsafe : sig
    val greg : Reg.t -> ('w, 'k) greg
    val freg : Freg.t -> 's freg
    val qreg : int -> qreg
    val vec : int -> Varr.t -> 'a vec
    val lane : int -> Lane.t -> int -> lane
    val vlist : int -> Varr.t -> vlist
    val vlist_lane : int -> Lane.t -> int -> vlist_lane
  end
end

module Class : functor
  (P : sig
     type t

     val prefix : string
     val numbers : int list
     val make : int -> t
     val to_int : t -> int
   end)
  -> sig
  type t = P.t

  val numbers : int list
  val count : int
  val mem : int -> bool
  val of_int : int -> P.t option
  val of_int_exn : int -> P.t
  val to_int : P.t -> int
  val all : P.t list
  val range : (int * int) option
  val name : P.t -> string
  val pp : Format.formatter -> P.t -> unit
  val find : string -> P.t option
  val equal : P.t -> P.t -> bool
end

val upto : int -> int list

module Xreg : sig
  type t = (View.x, View.gp) View.greg

  val numbers : int list
  val count : int
  val mem : int -> bool
  val of_int : int -> (View.x, View.gp) View.greg option
  val of_int_exn : int -> (View.x, View.gp) View.greg
  val to_int : (View.x, View.gp) View.greg -> int
  val all : (View.x, View.gp) View.greg list
  val range : (int * int) option
  val name : (View.x, View.gp) View.greg -> string
  val pp : Format.formatter -> (View.x, View.gp) View.greg -> unit
  val find : string -> (View.x, View.gp) View.greg option
  val equal : (View.x, View.gp) View.greg -> (View.x, View.gp) View.greg -> bool
end

module Wreg : sig
  type t = (View.w, View.gp) View.greg

  val numbers : int list
  val count : int
  val mem : int -> bool
  val of_int : int -> (View.w, View.gp) View.greg option
  val of_int_exn : int -> (View.w, View.gp) View.greg
  val to_int : (View.w, View.gp) View.greg -> int
  val all : (View.w, View.gp) View.greg list
  val range : (int * int) option
  val name : (View.w, View.gp) View.greg -> string
  val pp : Format.formatter -> (View.w, View.gp) View.greg -> unit
  val find : string -> (View.w, View.gp) View.greg option
  val equal : (View.w, View.gp) View.greg -> (View.w, View.gp) View.greg -> bool
end

val sp : (View.x, View.sp) View.greg
val wsp : (View.w, View.sp) View.greg
val xzr : (View.x, View.zr) View.greg
val wzr : (View.w, View.zr) View.greg

module Dreg : sig
  type t = View.d View.freg

  val numbers : int list
  val count : int
  val mem : int -> bool
  val of_int : int -> View.d View.freg option
  val of_int_exn : int -> View.d View.freg
  val to_int : View.d View.freg -> int
  val all : View.d View.freg list
  val range : (int * int) option
  val name : View.d View.freg -> string
  val pp : Format.formatter -> View.d View.freg -> unit
  val find : string -> View.d View.freg option
  val equal : View.d View.freg -> View.d View.freg -> bool
end

module Sreg : sig
  type t = View.s View.freg

  val numbers : int list
  val count : int
  val mem : int -> bool
  val of_int : int -> View.s View.freg option
  val of_int_exn : int -> View.s View.freg
  val to_int : View.s View.freg -> int
  val all : View.s View.freg list
  val range : (int * int) option
  val name : View.s View.freg -> string
  val pp : Format.formatter -> View.s View.freg -> unit
  val find : string -> View.s View.freg option
  val equal : View.s View.freg -> View.s View.freg -> bool
end

module Qreg : sig
  type t = View.qreg

  val numbers : int list
  val count : int
  val mem : int -> bool
  val of_int : int -> View.qreg option
  val of_int_exn : int -> View.qreg
  val to_int : View.qreg -> int
  val all : View.qreg list
  val range : (int * int) option
  val name : View.qreg -> string
  val pp : Format.formatter -> View.qreg -> unit
  val find : string -> View.qreg option
  val equal : View.qreg -> View.qreg -> bool
end

module Vreg : sig
  type t = int

  val numbers : int list
  val count : int
  val mem : int -> bool
  val of_int : int -> int option
  val of_int_exn : int -> int
  val to_int : int -> int
  val all : int list
  val range : (int * int) option
  val name : int -> string
  val pp : Format.formatter -> int -> unit
  val find : string -> int option
  val equal : int -> int -> bool
end

module Operand : sig
  type t =
    | Reg of Reg.t
    | Freg of Freg.t
    | Imm of Foundation.Bigint.t
    | Fimm of float
    | Mem of Mem.t
    | Shift of Shift.t
    | Sym of Asm_core.Expr.t
    | Vec of int * Varr.t
    | Vlane of int * Lane.t * int
    | Vlist of int * Varr.t
    | Vlist_lane of int * Lane.t * int
    | Qreg of int

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
  val invert : t -> t
end

module Opcode : sig
  type t =
    | Add
    | Mov
    | Movz
    | Movn
    | Movk
    | Cbz
    | Cbnz
    | Sxtw
    | Uxtw
    | Udiv
    | Sdiv
    | Cset
    | Lsl
    | Lsr
    | Asr
    | Ror
    | Ubfx
    | Ubfiz
    | Sbfx
    | Sbfiz
    | Rbit
    | Rev16
    | Rev32
    | Rev
    | Clz
    | Cls
    | Stp
    | Ldp
    | Ldr
    | Ldrb
    | Ldrh
    | Ldrsb
    | Ldrsh
    | Ldrsw
    | Str
    | Strb
    | Strh
    | Ret
    | Br
    | Mrs
    | Msr
    | Sub
    | Cmp
    | Cmn
    | And
    | Orr
    | Eor
    | Bic
    | Madd
    | Msub
    | Mul
    | Csel
    | Adrp
    | Adr
    | B
    | Bcond of Cond.t
    | Udf
    | Bl
    | Fmov
    | Fcmp
    | Fadd
    | Fsub
    | Fmul
    | Fdiv
    | Fcsel
    | Scvtf
    | Ucvtf
    | Fneg
    | Fmax
    | Fmla
    | Fcmeq
    | Fcmge
    | Fcmgt
    | Bsl
    | Bit
    | Not
    | Fmadd
    | Dup
    | Ins
    | Ld1
    | Ld1r
    | St1
    | Fcvtl
    | Fcvtn
    | Fsqrt
    | Frintz
    | Fcvt
    | Fcvtzs
    | Fcvtzu
    | Tst
end

module Instruction : sig
  type t = { op : Opcode.t; ops : Operand.t list }

  val mk : Opcode.t -> Operand.t list -> t
end
