(* What the x86 builder libraries share: the typed register views, the register classes' numbers,
   memory and EVEX operands, and the builders that take a condition. {!Rivet_x86_32_asm} and
   {!Rivet_x86_64_asm} name the registers each mode has and its builders: one per operand shape, in AT&T
   order, named by the mnemonic and a suffix for the shape's immediates ([i]), memory ([m]),
   symbols ([s]), [%cl] ([cl]) and leading registers ([rm]: a register into memory). A builder's
   width is its general registers', or its size letter where none fixes it ([addl_im]); 32- and
   64-bit immediates are [int64], an [_e] builder takes an {!Rivet_asm_core.Expr.t}. *)

module View = Rivet_x86_family_encode.X86_ops.View

type w8 = View.w8
type w16 = View.w16
type w32 = View.w32
type w64 = View.w64
type l128 = View.l128
type l256 = View.l256
type l512 = View.l512
type 'w gpr = 'w View.gpr
type 'l vec = 'l View.vec
type xmm = View.xmm
type ymm = View.ymm
type zmm = View.zmm
type lo = View.lo
type hi = View.hi
type ('w, 'r) greg = ('w, 'r) View.greg
type ('l, 'r) vreg = ('l, 'r) View.vreg
type 'w agpr = 'w View.agpr
type 'l evec = 'l View.evec
type mmx = View.mmx
type kmask = View.kmask
type st = View.st
type tmm = View.tmm
type creg = View.creg
type dreg = View.dreg

module Cc = Rivet_x86_family_encode.Cc

val reg : ('w, 'r) greg -> Rivet_x86_family_encode.Reg.t

val mem :
  ?index:(([< `W32 | `W64 ] as 'w), [< `Hi | `Lo ]) greg ->
  ?scale:int ->
  ?disp:int64 ->
  ('w, [< `Hi | `Lo ]) greg ->
  Rivet_x86_family_encode.Mem.t

val mem_e :
  ?index:(([< `W32 | `W64 ] as 'w), [< `Hi | `Lo ]) greg ->
  ?scale:int ->
  Rivet_asm_core.Expr.t ->
  ('w, [< `Hi | `Lo ]) greg ->
  Rivet_x86_family_encode.Mem.t

val abs :
  ?index:([< `W32 | `W64 ], [< `Hi | `Lo ]) greg ->
  ?scale:int ->
  int64 ->
  Rivet_x86_family_encode.Mem.t

val abs_e :
  ?index:([< `W32 | `W64 ], [< `Hi | `Lo ]) greg ->
  ?scale:int ->
  Rivet_asm_core.Expr.t ->
  Rivet_x86_family_encode.Mem.t

module Evex : sig
  type rounding = Rn_sae | Rd_sae | Ru_sae | Rz_sae | Sae

  val code : rounding -> int
  val rounding : rounding -> Rivet_x86_family_encode.Operand.t

  val masked :
    ?zero:bool -> kmask -> Rivet_x86_family_encode.Operand.t -> Rivet_x86_family_encode.Operand.t

  val bcst : Rivet_x86_family_encode.Mem.t -> int -> Rivet_x86_family_encode.Operand.t
end

module Regs : functor
  (C : sig
     type t

     val cls : Rivet_x86_family_encode.Rclass.t
     val registers : Rivet_x86_family_encode.Reg.t list
     val make : Rivet_x86_family_encode.Reg.t -> t
     val reg : t -> Rivet_x86_family_encode.Reg.t
   end)
  -> sig
  type t = C.t

  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_int : t -> int
  val numbers : int list
  val count : int
  val mem : int -> bool
  val all : t list
  val range : (int * int) option
  val name : t -> string
end

val st_registers : Rivet_x86_family_encode.Reg.t list
val base_registers : Rivet_x86_family_encode.Reg.t list -> Rivet_x86_family_encode.Reg.t list

val setcc :
  Rivet_x86_family_encode__X86_types.Cc.t ->
  Rivet_x86_family_encode.X86_ops.View.w8 Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val setcc_m :
  Rivet_x86_family_encode__X86_types.Cc.t ->
  Rivet_x86_family_encode.Mem.t ->
  Rivet_x86_family_encode.Instruction.t

val cmovcc :
  Rivet_x86_family_encode__X86_types.Cc.t ->
  ([< `W16 | `W32 | `W64 ] as 'a) Rivet_x86_family_encode.X86_ops.View.gpr ->
  'a Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

val cmovcc_m :
  Rivet_x86_family_encode__X86_types.Cc.t ->
  Rivet_x86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] Rivet_x86_family_encode.X86_ops.View.gpr ->
  Rivet_x86_family_encode.Instruction.t

module Rowkit : sig
  val row :
    int ->
    Rivet_x86_family_encode__X86_types.Operand.t list ->
    Rivet_x86_family_encode.Instruction.t

  val g : ('w, 'r) greg -> Rivet_x86_family_encode.Operand.t
  val v : ('l, 'r) vreg -> Rivet_x86_family_encode.Operand.t
  val mm : mmx -> Rivet_x86_family_encode.Operand.t
  val k : kmask -> Rivet_x86_family_encode.Operand.t
  val st : st -> Rivet_x86_family_encode.Operand.t
  val tmm : tmm -> Rivet_x86_family_encode.Operand.t
  val cr : creg -> Rivet_x86_family_encode.Operand.t
  val dr : dreg -> Rivet_x86_family_encode.Operand.t
  val m : Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Operand.t
  val b : int -> Rivet_x86_family_encode.Mem.t -> Rivet_x86_family_encode.Operand.t
  val i : int -> Rivet_x86_family_encode.Operand.t
  val i64 : int64 -> Rivet_x86_family_encode.Operand.t
  val e : Rivet_asm_core.Expr.t -> Rivet_x86_family_encode.Operand.t
  val one : Rivet_x86_family_encode.Operand.t
  val dfv : int -> Rivet_x86_family_encode.Operand.t
  val rc : Evex.rounding -> Rivet_x86_family_encode.Operand.t
  val fixed : string -> Rivet_x86_family_encode.Operand.t

  val masked :
    ?k:kmask -> ?z:bool -> Rivet_x86_family_encode.Operand.t -> Rivet_x86_family_encode.Operand.t
end
