(* Typed builders for the hand-written AArch64 (A64) instructions, one per operand shape of each
   opcode and named by the mnemonic ([.] becomes [_], a keyword gains [_]) with a suffix for the
   shape: [_i] an immediate, [_sh] a shifted register, [_ext] an extended register, [_post] a
   post-indexed address, [_f] a floating-point register, [_q] a Q register, [_v] a vector, [_lo12]
   a [:lo12:] symbol. Registers are typed by what a position accepts: [x0]-[x30] fit anywhere,
   [xzr] only where the zero register is read and [sp] only where the stack pointer is, widths must
   agree across the operands that share one, [d] and [s] registers do not mix, and a vector's
   arrangement is shared by its operands. Immediates are [int] ([int64] for a bitmask or a 64-bit
   constant); lowering still checks ranges and alignment. *)

module Cond = Aarch64_encode.Cond
module Extend = Aarch64_encode.Extend
module Xreg = Aarch64_encode.Xreg
module Wreg = Aarch64_encode.Wreg
module Dreg = Aarch64_encode.Dreg
module Sreg = Aarch64_encode.Sreg
module Qreg = Aarch64_encode.Qreg
module Vreg = Aarch64_encode.Vreg
module View = Aarch64_encode.View

type sop = Aarch64_encode.Aarch64_ops.Sx.sop
type eop = Aarch64_encode.Aarch64_ops.Sx.eop

val by_lsl : int -> sop
val by_lsr : int -> sop
val by_asr : int -> sop
val by_ror : int -> sop
val extend : Aarch64_encode.Extend.t -> int -> eop
val ext_uxtb : int -> eop
val ext_uxth : int -> eop
val ext_uxtw : int -> eop
val ext_uxtx : int -> eop
val ext_sxtb : int -> eop
val ext_sxth : int -> eop
val ext_sxtw : int -> eop
val ext_sxtx : int -> eop
val label : string -> Asm_core.Expr.t

val mk :
  (View.x, 'a) View.greg -> Aarch64_encode__Aarch64_types.Disp.t -> bool -> Aarch64_encode.Mem.t

val off : (View.x, 'a) View.greg -> int -> Aarch64_encode.Mem.t
val pre : (View.x, 'a) View.greg -> int -> Aarch64_encode.Mem.t
val off_lo12 : (View.x, 'a) View.greg -> Asm_core.Expr.t -> Aarch64_encode.Mem.t

val roff :
  ?extend:Extend.t ->
  ?amount:int ->
  (View.x, 'a) View.greg ->
  ('b, 'c) View.greg ->
  Aarch64_encode.Mem.t

val v8b : int -> [ `B8 ] View.vec
val v16b : int -> [ `B16 ] View.vec
val v2s : int -> [ `S2 ] View.vec
val v4s : int -> [ `S4 ] View.vec
val v2d : int -> [ `D2 ] View.vec
val lane_s : int -> int -> Aarch64_encode__Aarch64_types.View.lane
val lane_d : int -> int -> Aarch64_encode__Aarch64_types.View.lane
val list_2s : int -> Aarch64_encode__Aarch64_types.View.vlist
val list_4s : int -> Aarch64_encode__Aarch64_types.View.vlist
val list_2d : int -> Aarch64_encode__Aarch64_types.View.vlist
val list_lane_s : int -> int -> Aarch64_encode__Aarch64_types.View.vlist_lane
val list_lane_d : int -> int -> Aarch64_encode__Aarch64_types.View.vlist_lane
val sp : (Aarch64_encode.View.x, Aarch64_encode.View.sp) Aarch64_encode.View.greg
val wsp : (Aarch64_encode.View.w, Aarch64_encode.View.sp) Aarch64_encode.View.greg
val xzr : (Aarch64_encode.View.x, Aarch64_encode.View.zr) Aarch64_encode.View.greg
val wzr : (Aarch64_encode.View.w, Aarch64_encode.View.zr) Aarch64_encode.View.greg

val x0 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x1 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x2 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x3 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x4 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x5 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x6 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x7 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x8 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x9 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x10 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x11 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x12 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x13 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x14 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x15 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x16 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x17 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x18 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x19 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x20 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x21 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x22 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x23 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x24 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x25 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x26 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x27 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x28 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x29 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val x30 :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val fp :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val lr :
  ( Aarch64_encode__Aarch64_types.View.x,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w0 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w1 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w2 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w3 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w4 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w5 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w6 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w7 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w8 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w9 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w10 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w11 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w12 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w13 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w14 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w15 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w16 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w17 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w18 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w19 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w20 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w21 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w22 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w23 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w24 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w25 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w26 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w27 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w28 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w29 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val w30 :
  ( Aarch64_encode__Aarch64_types.View.w,
    Aarch64_encode__Aarch64_types.View.gp )
  Aarch64_encode__Aarch64_types.View.greg

val d0 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d1 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d2 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d3 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d4 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d5 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d6 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d7 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d8 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d9 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d10 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d11 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d12 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d13 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d14 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d15 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d16 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d17 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d18 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d19 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d20 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d21 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d22 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d23 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d24 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d25 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d26 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d27 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d28 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d29 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d30 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val d31 : Aarch64_encode__Aarch64_types.View.d Aarch64_encode__Aarch64_types.View.freg
val s0 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s1 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s2 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s3 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s4 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s5 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s6 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s7 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s8 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s9 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s10 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s11 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s12 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s13 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s14 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s15 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s16 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s17 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s18 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s19 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s20 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s21 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s22 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s23 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s24 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s25 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s26 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s27 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s28 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s29 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s30 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val s31 : Aarch64_encode__Aarch64_types.View.s Aarch64_encode__Aarch64_types.View.freg
val q0 : Aarch64_encode__Aarch64_types.View.qreg
val q1 : Aarch64_encode__Aarch64_types.View.qreg
val q2 : Aarch64_encode__Aarch64_types.View.qreg
val q3 : Aarch64_encode__Aarch64_types.View.qreg
val q4 : Aarch64_encode__Aarch64_types.View.qreg
val q5 : Aarch64_encode__Aarch64_types.View.qreg
val q6 : Aarch64_encode__Aarch64_types.View.qreg
val q7 : Aarch64_encode__Aarch64_types.View.qreg
val q8 : Aarch64_encode__Aarch64_types.View.qreg
val q9 : Aarch64_encode__Aarch64_types.View.qreg
val q10 : Aarch64_encode__Aarch64_types.View.qreg
val q11 : Aarch64_encode__Aarch64_types.View.qreg
val q12 : Aarch64_encode__Aarch64_types.View.qreg
val q13 : Aarch64_encode__Aarch64_types.View.qreg
val q14 : Aarch64_encode__Aarch64_types.View.qreg
val q15 : Aarch64_encode__Aarch64_types.View.qreg
val q16 : Aarch64_encode__Aarch64_types.View.qreg
val q17 : Aarch64_encode__Aarch64_types.View.qreg
val q18 : Aarch64_encode__Aarch64_types.View.qreg
val q19 : Aarch64_encode__Aarch64_types.View.qreg
val q20 : Aarch64_encode__Aarch64_types.View.qreg
val q21 : Aarch64_encode__Aarch64_types.View.qreg
val q22 : Aarch64_encode__Aarch64_types.View.qreg
val q23 : Aarch64_encode__Aarch64_types.View.qreg
val q24 : Aarch64_encode__Aarch64_types.View.qreg
val q25 : Aarch64_encode__Aarch64_types.View.qreg
val q26 : Aarch64_encode__Aarch64_types.View.qreg
val q27 : Aarch64_encode__Aarch64_types.View.qreg
val q28 : Aarch64_encode__Aarch64_types.View.qreg
val q29 : Aarch64_encode__Aarch64_types.View.qreg
val q30 : Aarch64_encode__Aarch64_types.View.qreg
val q31 : Aarch64_encode__Aarch64_types.View.qreg
val v0 : int
val v1 : int
val v2 : int
val v3 : int
val v4 : int
val v5 : int
val v6 : int
val v7 : int
val v8 : int
val v9 : int
val v10 : int
val v11 : int
val v12 : int
val v13 : int
val v14 : int
val v15 : int
val v16 : int
val v17 : int
val v18 : int
val v19 : int
val v20 : int
val v21 : int
val v22 : int
val v23 : int
val v24 : int
val v25 : int
val v26 : int
val v27 : int
val v28 : int
val v29 : int
val v30 : int
val v31 : int

module Sh = Aarch64_encode.Aarch64_ops.Builders.Sh

val add :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val add_sh :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Aarch64_ops.Sx.sop ->
  Aarch64_encode.Instruction.t

val add_ext :
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  ('b, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Aarch64_ops.Sx.eop ->
  Aarch64_encode.Instruction.t

val add_i :
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val add_i_lsl12 :
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val add_lo12 :
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  Asm_core.Expr.t ->
  Aarch64_encode.Instruction.t

val sub :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val sub_sh :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Aarch64_ops.Sx.sop ->
  Aarch64_encode.Instruction.t

val sub_ext :
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  ('b, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Aarch64_ops.Sx.eop ->
  Aarch64_encode.Instruction.t

val sub_i :
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val sub_i_lsl12 :
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val cmp :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val cmp_i : ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg -> int -> Aarch64_encode.Instruction.t

val cmp_i_lsl12 :
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg -> int -> Aarch64_encode.Instruction.t

val cmn_i : ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg -> int -> Aarch64_encode.Instruction.t

val cmn_i_lsl12 :
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg -> int -> Aarch64_encode.Instruction.t

val mov :
  ('a, [< `Gp | `Sp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val mov_i : ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> int64 -> Aarch64_encode.Instruction.t
val movz : ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> int -> Aarch64_encode.Instruction.t

val movz_lsl :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> int -> int -> Aarch64_encode.Instruction.t

val movn : ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> int -> Aarch64_encode.Instruction.t

val movn_lsl :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> int -> int -> Aarch64_encode.Instruction.t

val movk : ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> int -> Aarch64_encode.Instruction.t

val movk_lsl :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> int -> int -> Aarch64_encode.Instruction.t

val sxtw :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  (Aarch64_encode.View.w, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val uxtw :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  (Aarch64_encode.View.w, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val udiv :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val sdiv :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val madd :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val msub :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val mul :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val cset :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Cond.t ->
  Aarch64_encode.Instruction.t

val csel :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Cond.t ->
  Aarch64_encode.Instruction.t

val lsl_ :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val lsl_i :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val lsr_ :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val lsr_i :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val asr_ :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val asr_i :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val ror_ :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val ubfx :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  int ->
  int ->
  Aarch64_encode.Instruction.t

val ubfiz :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  int ->
  int ->
  Aarch64_encode.Instruction.t

val sbfx :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  int ->
  int ->
  Aarch64_encode.Instruction.t

val sbfiz :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  int ->
  int ->
  Aarch64_encode.Instruction.t

val rbit :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val rev16 :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val rev32 :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val rev :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val clz :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val cls :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val and_ :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val and_sh :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Aarch64_ops.Sx.sop ->
  Aarch64_encode.Instruction.t

val and_i :
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  int64 ->
  Aarch64_encode.Instruction.t

val orr :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val orr_sh :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Aarch64_ops.Sx.sop ->
  Aarch64_encode.Instruction.t

val orr_i :
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  int64 ->
  Aarch64_encode.Instruction.t

val eor :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val eor_sh :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Aarch64_ops.Sx.sop ->
  Aarch64_encode.Instruction.t

val eor_i :
  ('a, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  int64 ->
  Aarch64_encode.Instruction.t

val bic :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val bic_sh :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Aarch64_ops.Sx.sop ->
  Aarch64_encode.Instruction.t

val tst_i : ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> int64 -> Aarch64_encode.Instruction.t

val ldr :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Mem.t ->
  Aarch64_encode.Instruction.t

val ldr_post :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  (Aarch64_encode.View.x, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val ldr_f : 'a Aarch64_encode.View.freg -> Aarch64_encode.Mem.t -> Aarch64_encode.Instruction.t
val ldr_q : Aarch64_encode.View.qreg -> Aarch64_encode.Mem.t -> Aarch64_encode.Instruction.t

val str :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Mem.t ->
  Aarch64_encode.Instruction.t

val str_post :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  (Aarch64_encode.View.x, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val str_f : 'a Aarch64_encode.View.freg -> Aarch64_encode.Mem.t -> Aarch64_encode.Instruction.t
val str_q : Aarch64_encode.View.qreg -> Aarch64_encode.Mem.t -> Aarch64_encode.Instruction.t

val ldrb :
  (Aarch64_encode.View.w, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Mem.t ->
  Aarch64_encode.Instruction.t

val ldrb_post :
  (Aarch64_encode.View.w, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  (Aarch64_encode.View.x, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val ldrh :
  (Aarch64_encode.View.w, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Mem.t ->
  Aarch64_encode.Instruction.t

val ldrh_post :
  (Aarch64_encode.View.w, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  (Aarch64_encode.View.x, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val strb :
  (Aarch64_encode.View.w, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Mem.t ->
  Aarch64_encode.Instruction.t

val strb_post :
  (Aarch64_encode.View.w, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  (Aarch64_encode.View.x, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val strh :
  (Aarch64_encode.View.w, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Mem.t ->
  Aarch64_encode.Instruction.t

val strh_post :
  (Aarch64_encode.View.w, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  (Aarch64_encode.View.x, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val ldrsb :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Mem.t ->
  Aarch64_encode.Instruction.t

val ldrsh :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Mem.t ->
  Aarch64_encode.Instruction.t

val ldrsw :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Mem.t ->
  Aarch64_encode.Instruction.t

val ldp :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Mem.t ->
  Aarch64_encode.Instruction.t

val ldp_post :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  (Aarch64_encode.View.x, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val stp :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Mem.t ->
  Aarch64_encode.Instruction.t

val stp_post :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  (Aarch64_encode.View.x, [< `Gp | `Sp ]) Aarch64_encode.View.greg ->
  int ->
  Aarch64_encode.Instruction.t

val b : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val bl : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_cond : Aarch64_encode__Aarch64_types.Cond.t -> Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val ret : unit -> Aarch64_encode.Instruction.t

val ret_r :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> Aarch64_encode.Instruction.t

val br :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> Aarch64_encode.Instruction.t

val cbz :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> Asm_core.Expr.t -> Aarch64_encode.Instruction.t

val cbnz :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> Asm_core.Expr.t -> Aarch64_encode.Instruction.t

val adrp :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Asm_core.Expr.t ->
  Aarch64_encode.Instruction.t

val adr :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Asm_core.Expr.t ->
  Aarch64_encode.Instruction.t

val udf : int -> Aarch64_encode.Instruction.t

val mrs_fpcr :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> Aarch64_encode.Instruction.t

val mrs_nzcv :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> Aarch64_encode.Instruction.t

val msr_fpcr :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> Aarch64_encode.Instruction.t

val msr_nzcv :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg -> Aarch64_encode.Instruction.t

val fmov :
  'a Aarch64_encode.View.freg -> 'a Aarch64_encode.View.freg -> Aarch64_encode.Instruction.t

val fmov_i : 'a Aarch64_encode.View.freg -> float -> Aarch64_encode.Instruction.t

val fmov_d_x :
  Aarch64_encode.View.d Aarch64_encode.View.freg ->
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val fmov_s_w :
  Aarch64_encode.View.s Aarch64_encode.View.freg ->
  (Aarch64_encode.View.w, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val fmov_x_d :
  (Aarch64_encode.View.x, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.View.d Aarch64_encode.View.freg ->
  Aarch64_encode.Instruction.t

val fmov_w_s :
  (Aarch64_encode.View.w, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.View.s Aarch64_encode.View.freg ->
  Aarch64_encode.Instruction.t

val fcmp :
  'a Aarch64_encode.View.freg -> 'a Aarch64_encode.View.freg -> Aarch64_encode.Instruction.t

val fcmp_z : 'a Aarch64_encode.View.freg -> Aarch64_encode.Instruction.t

val fadd :
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  Aarch64_encode.Instruction.t

val fsub :
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  Aarch64_encode.Instruction.t

val fmul :
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  Aarch64_encode.Instruction.t

val fdiv :
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  Aarch64_encode.Instruction.t

val fmax :
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  Aarch64_encode.Instruction.t

val fadd_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val fsub_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val fmul_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val fdiv_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val fmax_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val fmla_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val fcmeq_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val fcmge_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val fcmgt_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val fcsel :
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  Aarch64_encode.Cond.t ->
  Aarch64_encode.Instruction.t

val scvtf :
  'a Aarch64_encode.View.freg ->
  ('b, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val ucvtf :
  'a Aarch64_encode.View.freg ->
  ('b, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val fneg :
  'a Aarch64_encode.View.freg -> 'a Aarch64_encode.View.freg -> Aarch64_encode.Instruction.t

val fsqrt :
  'a Aarch64_encode.View.freg -> 'a Aarch64_encode.View.freg -> Aarch64_encode.Instruction.t

val frintz :
  'a Aarch64_encode.View.freg -> 'a Aarch64_encode.View.freg -> Aarch64_encode.Instruction.t

val fneg_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val fsqrt_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val frintz_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val fmadd :
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  'a Aarch64_encode.View.freg ->
  Aarch64_encode.Instruction.t

val fcvt_s_d :
  Aarch64_encode.View.s Aarch64_encode.View.freg ->
  Aarch64_encode.View.d Aarch64_encode.View.freg ->
  Aarch64_encode.Instruction.t

val fcvt_d_s :
  Aarch64_encode.View.d Aarch64_encode.View.freg ->
  Aarch64_encode.View.s Aarch64_encode.View.freg ->
  Aarch64_encode.Instruction.t

val fcvtzs :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  'b Aarch64_encode.View.freg ->
  Aarch64_encode.Instruction.t

val fcvtzu :
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  'b Aarch64_encode.View.freg ->
  Aarch64_encode.Instruction.t

val fcvtl :
  [ `D2 ] Aarch64_encode.View.vec -> [ `S2 ] Aarch64_encode.View.vec -> Aarch64_encode.Instruction.t

val fcvtn :
  [ `S2 ] Aarch64_encode.View.vec -> [ `D2 ] Aarch64_encode.View.vec -> Aarch64_encode.Instruction.t

val and_v :
  ([< `B16 | `B8 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val orr_v :
  ([< `B16 | `B8 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val eor_v :
  ([< `B16 | `B8 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val bsl_v :
  ([< `B16 | `B8 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val bit_v :
  ([< `B16 | `B8 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val not_v :
  ([< `B16 | `B8 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val mov_v :
  ([< `B16 | `B8 ] as 'a) Aarch64_encode.View.vec ->
  'a Aarch64_encode.View.vec ->
  Aarch64_encode.Instruction.t

val mov_lane :
  'a Aarch64_encode.View.freg -> Aarch64_encode.View.lane -> Aarch64_encode.Instruction.t

val mov_lane_lane :
  Aarch64_encode.View.lane -> Aarch64_encode.View.lane -> Aarch64_encode.Instruction.t

val ins : Aarch64_encode.View.lane -> Aarch64_encode.View.lane -> Aarch64_encode.Instruction.t

val dup_lane :
  [< `D2 | `S2 | `S4 ] Aarch64_encode.View.vec ->
  Aarch64_encode.View.lane ->
  Aarch64_encode.Instruction.t

val dup_gpr :
  [< `D2 | `S2 | `S4 ] Aarch64_encode.View.vec ->
  ('a, [< `Gp | `Zr ]) Aarch64_encode.View.greg ->
  Aarch64_encode.Instruction.t

val dup_scalar :
  'a Aarch64_encode.View.freg -> Aarch64_encode.View.lane -> Aarch64_encode.Instruction.t

val ld1_lane :
  Aarch64_encode.View.vlist_lane -> Aarch64_encode.Mem.t -> Aarch64_encode.Instruction.t

val st1_lane :
  Aarch64_encode.View.vlist_lane -> Aarch64_encode.Mem.t -> Aarch64_encode.Instruction.t

val ld1r : Aarch64_encode.View.vlist -> Aarch64_encode.Mem.t -> Aarch64_encode.Instruction.t
val b_eq : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_ne : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_cs : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_cc : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_mi : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_pl : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_vs : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_vc : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_hi : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_ls : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_ge : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_lt : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_gt : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_le : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_al : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
val b_nv : Asm_core.Expr.t -> Aarch64_encode.Instruction.t
