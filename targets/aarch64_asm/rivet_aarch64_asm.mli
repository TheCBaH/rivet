(* Typed builders for the hand-written AArch64 (A64) instructions, one per operand shape of each
   opcode and named by the mnemonic ([.] becomes [_], a keyword gains [_]) with a suffix for the
   shape: [_i] an immediate, [_sh] a shifted register, [_ext] an extended register, [_post] a
   post-indexed address, [_f] a floating-point register, [_q] a Q register, [_v] a vector, [_lo12]
   a [:lo12:] symbol. Registers are typed by what a position accepts: [x0]-[x30] fit anywhere,
   [xzr] only where the zero register is read and [sp] only where the stack pointer is, widths must
   agree across the operands that share one, [d] and [s] registers do not mix, and a vector's
   arrangement is shared by its operands. Immediates are [int] ([int64] for a bitmask or a 64-bit
   constant); lowering still checks ranges and alignment. *)

module Cond = Rivet_aarch64_encode.Cond
module Extend = Rivet_aarch64_encode.Extend
module Xreg = Rivet_aarch64_encode.Xreg
module Wreg = Rivet_aarch64_encode.Wreg
module Dreg = Rivet_aarch64_encode.Dreg
module Sreg = Rivet_aarch64_encode.Sreg
module Qreg = Rivet_aarch64_encode.Qreg
module Vreg = Rivet_aarch64_encode.Vreg
module View = Rivet_aarch64_encode.View

type sop = Rivet_aarch64_encode.Aarch64_ops.Sx.sop
type eop = Rivet_aarch64_encode.Aarch64_ops.Sx.eop

val by_lsl : int -> sop
val by_lsr : int -> sop
val by_asr : int -> sop
val by_ror : int -> sop
val extend : Rivet_aarch64_encode.Extend.t -> int -> eop
val ext_uxtb : int -> eop
val ext_uxth : int -> eop
val ext_uxtw : int -> eop
val ext_uxtx : int -> eop
val ext_sxtb : int -> eop
val ext_sxth : int -> eop
val ext_sxtw : int -> eop
val ext_sxtx : int -> eop
val label : string -> Rivet_asm_core.Expr.t

val mk :
  (View.x, 'a) View.greg ->
  Rivet_aarch64_encode__Aarch64_types.Disp.t ->
  bool ->
  Rivet_aarch64_encode.Mem.t

val off : (View.x, 'a) View.greg -> int -> Rivet_aarch64_encode.Mem.t
val pre : (View.x, 'a) View.greg -> int -> Rivet_aarch64_encode.Mem.t
val off_lo12 : (View.x, 'a) View.greg -> Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Mem.t

val roff :
  ?extend:Extend.t ->
  ?amount:int ->
  (View.x, 'a) View.greg ->
  ('b, 'c) View.greg ->
  Rivet_aarch64_encode.Mem.t

val v8b : int -> [ `B8 ] View.vec
val v16b : int -> [ `B16 ] View.vec
val v2s : int -> [ `S2 ] View.vec
val v4s : int -> [ `S4 ] View.vec
val v2d : int -> [ `D2 ] View.vec
val lane_s : int -> int -> Rivet_aarch64_encode__Aarch64_types.View.lane
val lane_d : int -> int -> Rivet_aarch64_encode__Aarch64_types.View.lane
val list_2s : int -> Rivet_aarch64_encode__Aarch64_types.View.vlist
val list_4s : int -> Rivet_aarch64_encode__Aarch64_types.View.vlist
val list_2d : int -> Rivet_aarch64_encode__Aarch64_types.View.vlist
val list_lane_s : int -> int -> Rivet_aarch64_encode__Aarch64_types.View.vlist_lane
val list_lane_d : int -> int -> Rivet_aarch64_encode__Aarch64_types.View.vlist_lane
val sp : (Rivet_aarch64_encode.View.x, Rivet_aarch64_encode.View.sp) Rivet_aarch64_encode.View.greg
val wsp : (Rivet_aarch64_encode.View.w, Rivet_aarch64_encode.View.sp) Rivet_aarch64_encode.View.greg
val xzr : (Rivet_aarch64_encode.View.x, Rivet_aarch64_encode.View.zr) Rivet_aarch64_encode.View.greg
val wzr : (Rivet_aarch64_encode.View.w, Rivet_aarch64_encode.View.zr) Rivet_aarch64_encode.View.greg

val x0 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x1 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x2 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x3 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x4 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x5 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x6 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x7 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x8 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x9 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x10 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x11 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x12 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x13 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x14 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x15 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x16 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x17 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x18 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x19 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x20 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x21 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x22 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x23 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x24 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x25 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x26 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x27 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x28 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x29 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val x30 :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val fp :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val lr :
  ( Rivet_aarch64_encode__Aarch64_types.View.x,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w0 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w1 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w2 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w3 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w4 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w5 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w6 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w7 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w8 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w9 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w10 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w11 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w12 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w13 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w14 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w15 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w16 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w17 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w18 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w19 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w20 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w21 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w22 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w23 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w24 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w25 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w26 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w27 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w28 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w29 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val w30 :
  ( Rivet_aarch64_encode__Aarch64_types.View.w,
    Rivet_aarch64_encode__Aarch64_types.View.gp )
  Rivet_aarch64_encode__Aarch64_types.View.greg

val d0 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d1 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d2 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d3 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d4 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d5 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d6 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d7 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d8 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d9 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d10 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d11 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d12 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d13 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d14 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d15 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d16 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d17 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d18 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d19 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d20 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d21 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d22 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d23 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d24 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d25 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d26 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d27 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d28 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d29 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d30 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val d31 : Rivet_aarch64_encode__Aarch64_types.View.d Rivet_aarch64_encode__Aarch64_types.View.freg
val s0 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s1 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s2 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s3 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s4 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s5 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s6 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s7 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s8 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s9 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s10 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s11 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s12 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s13 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s14 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s15 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s16 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s17 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s18 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s19 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s20 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s21 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s22 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s23 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s24 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s25 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s26 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s27 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s28 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s29 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s30 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val s31 : Rivet_aarch64_encode__Aarch64_types.View.s Rivet_aarch64_encode__Aarch64_types.View.freg
val q0 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q1 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q2 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q3 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q4 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q5 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q6 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q7 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q8 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q9 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q10 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q11 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q12 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q13 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q14 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q15 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q16 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q17 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q18 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q19 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q20 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q21 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q22 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q23 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q24 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q25 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q26 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q27 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q28 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q29 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q30 : Rivet_aarch64_encode__Aarch64_types.View.qreg
val q31 : Rivet_aarch64_encode__Aarch64_types.View.qreg
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

module Sh = Rivet_aarch64_encode.Aarch64_ops.Builders.Sh

val add :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val add_sh :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Aarch64_ops.Sx.sop ->
  Rivet_aarch64_encode.Instruction.t

val add_ext :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  ('b, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Aarch64_ops.Sx.eop ->
  Rivet_aarch64_encode.Instruction.t

val add_i :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val add_i_lsl12 :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val add_lo12 :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  Rivet_asm_core.Expr.t ->
  Rivet_aarch64_encode.Instruction.t

val sub :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val sub_sh :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Aarch64_ops.Sx.sop ->
  Rivet_aarch64_encode.Instruction.t

val sub_ext :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  ('b, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Aarch64_ops.Sx.eop ->
  Rivet_aarch64_encode.Instruction.t

val sub_i :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val sub_i_lsl12 :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val cmp :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val cmp_i :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg -> int -> Rivet_aarch64_encode.Instruction.t

val cmp_i_lsl12 :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg -> int -> Rivet_aarch64_encode.Instruction.t

val cmn_i :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg -> int -> Rivet_aarch64_encode.Instruction.t

val cmn_i_lsl12 :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg -> int -> Rivet_aarch64_encode.Instruction.t

val mov :
  ('a, [< `Gp | `Sp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Sp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val mov_i :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg -> int64 -> Rivet_aarch64_encode.Instruction.t

val movz :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg -> int -> Rivet_aarch64_encode.Instruction.t

val movz_lsl :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val movn :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg -> int -> Rivet_aarch64_encode.Instruction.t

val movn_lsl :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val movk :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg -> int -> Rivet_aarch64_encode.Instruction.t

val movk_lsl :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val sxtw :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  (Rivet_aarch64_encode.View.w, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val uxtw :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  (Rivet_aarch64_encode.View.w, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val udiv :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val sdiv :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val madd :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val msub :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val mul :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val cset :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Cond.t ->
  Rivet_aarch64_encode.Instruction.t

val csel :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Cond.t ->
  Rivet_aarch64_encode.Instruction.t

val lsl_ :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val lsl_i :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val lsr_ :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val lsr_i :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val asr_ :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val asr_i :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val ror_ :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val ubfx :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val ubfiz :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val sbfx :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val sbfiz :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val rbit :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val rev16 :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val rev32 :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val rev :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val clz :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val cls :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val and_ :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val and_sh :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Aarch64_ops.Sx.sop ->
  Rivet_aarch64_encode.Instruction.t

val and_i :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int64 ->
  Rivet_aarch64_encode.Instruction.t

val orr :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val orr_sh :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Aarch64_ops.Sx.sop ->
  Rivet_aarch64_encode.Instruction.t

val orr_i :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int64 ->
  Rivet_aarch64_encode.Instruction.t

val eor :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val eor_sh :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Aarch64_ops.Sx.sop ->
  Rivet_aarch64_encode.Instruction.t

val eor_i :
  ('a, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  int64 ->
  Rivet_aarch64_encode.Instruction.t

val bic :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val bic_sh :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Aarch64_ops.Sx.sop ->
  Rivet_aarch64_encode.Instruction.t

val tst_i :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg -> int64 -> Rivet_aarch64_encode.Instruction.t

val ldr :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val ldr_post :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  (Rivet_aarch64_encode.View.x, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val ldr_f :
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val ldr_q :
  Rivet_aarch64_encode.View.qreg -> Rivet_aarch64_encode.Mem.t -> Rivet_aarch64_encode.Instruction.t

val str :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val str_post :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  (Rivet_aarch64_encode.View.x, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val str_f :
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val str_q :
  Rivet_aarch64_encode.View.qreg -> Rivet_aarch64_encode.Mem.t -> Rivet_aarch64_encode.Instruction.t

val ldrb :
  (Rivet_aarch64_encode.View.w, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val ldrb_post :
  (Rivet_aarch64_encode.View.w, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  (Rivet_aarch64_encode.View.x, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val ldrh :
  (Rivet_aarch64_encode.View.w, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val ldrh_post :
  (Rivet_aarch64_encode.View.w, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  (Rivet_aarch64_encode.View.x, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val strb :
  (Rivet_aarch64_encode.View.w, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val strb_post :
  (Rivet_aarch64_encode.View.w, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  (Rivet_aarch64_encode.View.x, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val strh :
  (Rivet_aarch64_encode.View.w, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val strh_post :
  (Rivet_aarch64_encode.View.w, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  (Rivet_aarch64_encode.View.x, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val ldrsb :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val ldrsh :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val ldrsw :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val ldp :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val ldp_post :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  (Rivet_aarch64_encode.View.x, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val stp :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val stp_post :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  (Rivet_aarch64_encode.View.x, [< `Gp | `Sp ]) Rivet_aarch64_encode.View.greg ->
  int ->
  Rivet_aarch64_encode.Instruction.t

val b : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val bl : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t

val b_cond :
  Rivet_aarch64_encode__Aarch64_types.Cond.t ->
  Rivet_asm_core.Expr.t ->
  Rivet_aarch64_encode.Instruction.t

val ret : unit -> Rivet_aarch64_encode.Instruction.t

val ret_r :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val br :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val cbz :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_asm_core.Expr.t ->
  Rivet_aarch64_encode.Instruction.t

val cbnz :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_asm_core.Expr.t ->
  Rivet_aarch64_encode.Instruction.t

val adrp :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_asm_core.Expr.t ->
  Rivet_aarch64_encode.Instruction.t

val adr :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_asm_core.Expr.t ->
  Rivet_aarch64_encode.Instruction.t

val udf : int -> Rivet_aarch64_encode.Instruction.t

val mrs_fpcr :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val mrs_nzcv :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val msr_fpcr :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val msr_nzcv :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val fmov :
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fmov_i : 'a Rivet_aarch64_encode.View.freg -> float -> Rivet_aarch64_encode.Instruction.t

val fmov_d_x :
  Rivet_aarch64_encode.View.d Rivet_aarch64_encode.View.freg ->
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val fmov_s_w :
  Rivet_aarch64_encode.View.s Rivet_aarch64_encode.View.freg ->
  (Rivet_aarch64_encode.View.w, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val fmov_x_d :
  (Rivet_aarch64_encode.View.x, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.View.d Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fmov_w_s :
  (Rivet_aarch64_encode.View.w, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.View.s Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fcmp :
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fcmp_z : 'a Rivet_aarch64_encode.View.freg -> Rivet_aarch64_encode.Instruction.t

val fadd :
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fsub :
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fmul :
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fdiv :
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fmax :
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fadd_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val fsub_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val fmul_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val fdiv_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val fmax_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val fmla_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val fcmeq_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val fcmge_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val fcmgt_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val fcsel :
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Cond.t ->
  Rivet_aarch64_encode.Instruction.t

val scvtf :
  'a Rivet_aarch64_encode.View.freg ->
  ('b, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val ucvtf :
  'a Rivet_aarch64_encode.View.freg ->
  ('b, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val fneg :
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fsqrt :
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val frintz :
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fneg_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val fsqrt_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val frintz_v :
  ([< `D2 | `S2 | `S4 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val fmadd :
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fcvt_s_d :
  Rivet_aarch64_encode.View.s Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.View.d Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fcvt_d_s :
  Rivet_aarch64_encode.View.d Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.View.s Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fcvtzs :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  'b Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fcvtzu :
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  'b Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.Instruction.t

val fcvtl :
  [ `D2 ] Rivet_aarch64_encode.View.vec ->
  [ `S2 ] Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val fcvtn :
  [ `S2 ] Rivet_aarch64_encode.View.vec ->
  [ `D2 ] Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val and_v :
  ([< `B16 | `B8 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val orr_v :
  ([< `B16 | `B8 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val eor_v :
  ([< `B16 | `B8 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val bsl_v :
  ([< `B16 | `B8 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val bit_v :
  ([< `B16 | `B8 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val not_v :
  ([< `B16 | `B8 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val mov_v :
  ([< `B16 | `B8 ] as 'a) Rivet_aarch64_encode.View.vec ->
  'a Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.Instruction.t

val mov_lane :
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.View.lane ->
  Rivet_aarch64_encode.Instruction.t

val mov_lane_lane :
  Rivet_aarch64_encode.View.lane ->
  Rivet_aarch64_encode.View.lane ->
  Rivet_aarch64_encode.Instruction.t

val ins :
  Rivet_aarch64_encode.View.lane ->
  Rivet_aarch64_encode.View.lane ->
  Rivet_aarch64_encode.Instruction.t

val dup_lane :
  [< `D2 | `S2 | `S4 ] Rivet_aarch64_encode.View.vec ->
  Rivet_aarch64_encode.View.lane ->
  Rivet_aarch64_encode.Instruction.t

val dup_gpr :
  [< `D2 | `S2 | `S4 ] Rivet_aarch64_encode.View.vec ->
  ('a, [< `Gp | `Zr ]) Rivet_aarch64_encode.View.greg ->
  Rivet_aarch64_encode.Instruction.t

val dup_scalar :
  'a Rivet_aarch64_encode.View.freg ->
  Rivet_aarch64_encode.View.lane ->
  Rivet_aarch64_encode.Instruction.t

val ld1_lane :
  Rivet_aarch64_encode.View.vlist_lane ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val st1_lane :
  Rivet_aarch64_encode.View.vlist_lane ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val ld1r :
  Rivet_aarch64_encode.View.vlist ->
  Rivet_aarch64_encode.Mem.t ->
  Rivet_aarch64_encode.Instruction.t

val b_eq : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_ne : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_cs : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_cc : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_mi : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_pl : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_vs : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_vc : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_hi : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_ls : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_ge : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_lt : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_gt : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_le : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_al : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
val b_nv : Rivet_asm_core.Expr.t -> Rivet_aarch64_encode.Instruction.t
