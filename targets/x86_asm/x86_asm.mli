(* What the x86 builder libraries share: the typed register views, the register classes' numbers,
   memory and EVEX operands, and the builders that take a condition. {!X86_32_asm} and
   {!X86_64_asm} name the registers each mode has and its builders: one per operand shape, in AT&T
   order, named by the mnemonic and a suffix for the shape's immediates ([i]), memory ([m]),
   symbols ([s]), [%cl] ([cl]) and leading registers ([rm]: a register into memory). A builder's
   width is its general registers', or its size letter where none fixes it ([addl_im]); 32- and
   64-bit immediates are [int64], an [_e] builder takes an {!Asm_core.Expr.t}. *)

module View = X86_family_encode.X86_ops.View

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

module Cc = X86_family_encode.Cc

val reg : ('w, 'r) greg -> X86_family_encode.Reg.t

val mem :
  ?index:(([< `W32 | `W64 ] as 'w), [< `Hi | `Lo ]) greg ->
  ?scale:int ->
  ?disp:int64 ->
  ('w, [< `Hi | `Lo ]) greg ->
  X86_family_encode.Mem.t

val mem_e :
  ?index:(([< `W32 | `W64 ] as 'w), [< `Hi | `Lo ]) greg ->
  ?scale:int ->
  Asm_core.Expr.t ->
  ('w, [< `Hi | `Lo ]) greg ->
  X86_family_encode.Mem.t

val abs :
  ?index:([< `W32 | `W64 ], [< `Hi | `Lo ]) greg -> ?scale:int -> int64 -> X86_family_encode.Mem.t

val abs_e :
  ?index:([< `W32 | `W64 ], [< `Hi | `Lo ]) greg ->
  ?scale:int ->
  Asm_core.Expr.t ->
  X86_family_encode.Mem.t

module Evex : sig
  type rounding = Rn_sae | Rd_sae | Ru_sae | Rz_sae | Sae

  val code : rounding -> int
  val rounding : rounding -> X86_family_encode.Operand.t
  val masked : ?zero:bool -> kmask -> X86_family_encode.Operand.t -> X86_family_encode.Operand.t
  val bcst : X86_family_encode.Mem.t -> int -> X86_family_encode.Operand.t
end

module Regs : functor
  (C : sig
     type t

     val cls : X86_family_encode.Rclass.t
     val registers : X86_family_encode.Reg.t list
     val make : X86_family_encode.Reg.t -> t
     val reg : t -> X86_family_encode.Reg.t
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

val st_registers : X86_family_encode.Reg.t list
val base_registers : X86_family_encode.Reg.t list -> X86_family_encode.Reg.t list

val setcc :
  X86_family_encode__X86_types.Cc.t ->
  X86_family_encode.X86_ops.View.w8 X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val setcc_m :
  X86_family_encode__X86_types.Cc.t -> X86_family_encode.Mem.t -> X86_family_encode.Instruction.t

val cmovcc :
  X86_family_encode__X86_types.Cc.t ->
  ([< `W16 | `W32 | `W64 ] as 'a) X86_family_encode.X86_ops.View.gpr ->
  'a X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

val cmovcc_m :
  X86_family_encode__X86_types.Cc.t ->
  X86_family_encode.Mem.t ->
  [< `W16 | `W32 | `W64 ] X86_family_encode.X86_ops.View.gpr ->
  X86_family_encode.Instruction.t

module Rowkit : sig
  val row : int -> X86_family_encode__X86_types.Operand.t list -> X86_family_encode.Instruction.t
  val g : ('w, 'r) greg -> X86_family_encode.Operand.t
  val v : ('l, 'r) vreg -> X86_family_encode.Operand.t
  val mm : mmx -> X86_family_encode.Operand.t
  val k : kmask -> X86_family_encode.Operand.t
  val st : st -> X86_family_encode.Operand.t
  val tmm : tmm -> X86_family_encode.Operand.t
  val cr : creg -> X86_family_encode.Operand.t
  val dr : dreg -> X86_family_encode.Operand.t
  val m : X86_family_encode.Mem.t -> X86_family_encode.Operand.t
  val b : int -> X86_family_encode.Mem.t -> X86_family_encode.Operand.t
  val i : int -> X86_family_encode.Operand.t
  val i64 : int64 -> X86_family_encode.Operand.t
  val e : Asm_core.Expr.t -> X86_family_encode.Operand.t
  val one : X86_family_encode.Operand.t
  val dfv : int -> X86_family_encode.Operand.t
  val rc : Evex.rounding -> X86_family_encode.Operand.t
  val fixed : string -> X86_family_encode.Operand.t
  val masked : ?k:kmask -> ?z:bool -> X86_family_encode.Operand.t -> X86_family_encode.Operand.t
end
