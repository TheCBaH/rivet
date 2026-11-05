(* Typed builders for the hand-written AArch64 (A64) instructions, one per operand shape of each
   opcode and named by the mnemonic ([.] becomes [_], a keyword gains [_]) with a suffix for the
   shape: [_i] an immediate, [_sh] a shifted register, [_ext] an extended register, [_post] a
   post-indexed address, [_f] a floating-point register, [_q] a Q register, [_v] a vector, [_lo12]
   a [:lo12:] symbol. Registers are typed by what a position accepts: [x0]-[x30] fit anywhere,
   [xzr] only where the zero register is read and [sp] only where the stack pointer is, widths must
   agree across the operands that share one, [d] and [s] registers do not mix, and a vector's
   arrangement is shared by its operands. Immediates are [int] ([int64] for a bitmask or a 64-bit
   constant); lowering still checks ranges and alignment. *)

open Aarch64_encode
module Cond = Cond
module Extend = Extend
module Xreg = Xreg
module Wreg = Wreg
module Dreg = Dreg
module Sreg = Sreg
module Qreg = Qreg
module Vreg = Vreg
module View = View

(* {1 Operands} *)

include Aarch64_ops.Sx

(* The extend an extended-register operand names: [add x0, x1, w2, uxtw #2]. *)
let ext_uxtb = extend Extend.Uxtb
let ext_uxth = extend Extend.Uxth
let ext_uxtw = extend Extend.Uxtw
let ext_uxtx = extend Extend.Uxtx
let ext_sxtb = extend Extend.Sxtb
let ext_sxth = extend Extend.Sxth
let ext_sxtw = extend Extend.Sxtw
let ext_sxtx = extend Extend.Sxtx

(* A label or symbol expression. *)
let label name = Asm_core.Expr.Symbol name

let mk base offset writeback =
  { Mem.base : (View.x, _) View.greg :> Reg.t; offset; writeback; pre = true }

(* The address [\[base, #n\]]. *)
let off base n = mk base (Disp.Const (Int64.of_int n)) false

(* [\[base, #n\]!]. *)
let pre base n = mk base (Disp.Const (Int64.of_int n)) true

(* [\[base, #:lo12:sym\]]. *)
let off_lo12 base e = mk base (Disp.Sym (Asm_core.Expr.Modifier ("lo12", e))) false

(* [\[base, index\]]: an index register of either width, with the extend and shift it names
   ([lsl] by default). *)
let roff ?(extend = Extend.Lsl_ext) ?amount base index =
  mk base (Disp.Reg { index : (_, _) View.greg :> Reg.t; extend; amount }) false

(* {1 Vector operands} *)

let v8b r : [ `B8 ] View.vec = View.Unsafe.vec (Vreg.to_int r) Varr.B8
let v16b r : [ `B16 ] View.vec = View.Unsafe.vec (Vreg.to_int r) Varr.B16
let v2s r : [ `S2 ] View.vec = View.Unsafe.vec (Vreg.to_int r) Varr.S2
let v4s r : [ `S4 ] View.vec = View.Unsafe.vec (Vreg.to_int r) Varr.S4
let v2d r : [ `D2 ] View.vec = View.Unsafe.vec (Vreg.to_int r) Varr.D2

(* [v0.s[1]] and [v0.d[1]]. *)
let lane_s r i = View.Unsafe.lane (Vreg.to_int r) Lane.S i
let lane_d r i = View.Unsafe.lane (Vreg.to_int r) Lane.D i

(* [{v0.4s}] and [{v0.s}[1]]. *)
let list_2s r = View.Unsafe.vlist (Vreg.to_int r) Varr.S2
let list_4s r = View.Unsafe.vlist (Vreg.to_int r) Varr.S4
let list_2d r = View.Unsafe.vlist (Vreg.to_int r) Varr.D2
let list_lane_s r i = View.Unsafe.vlist_lane (Vreg.to_int r) Lane.S i
let list_lane_d r i = View.Unsafe.vlist_lane (Vreg.to_int r) Lane.D i

(* {1 Registers} *)

let sp = sp
let wsp = wsp
let xzr = xzr
let wzr = wzr
let x0 = Xreg.of_int_exn 0
let x1 = Xreg.of_int_exn 1
let x2 = Xreg.of_int_exn 2
let x3 = Xreg.of_int_exn 3
let x4 = Xreg.of_int_exn 4
let x5 = Xreg.of_int_exn 5
let x6 = Xreg.of_int_exn 6
let x7 = Xreg.of_int_exn 7
let x8 = Xreg.of_int_exn 8
let x9 = Xreg.of_int_exn 9
let x10 = Xreg.of_int_exn 10
let x11 = Xreg.of_int_exn 11
let x12 = Xreg.of_int_exn 12
let x13 = Xreg.of_int_exn 13
let x14 = Xreg.of_int_exn 14
let x15 = Xreg.of_int_exn 15
let x16 = Xreg.of_int_exn 16
let x17 = Xreg.of_int_exn 17
let x18 = Xreg.of_int_exn 18
let x19 = Xreg.of_int_exn 19
let x20 = Xreg.of_int_exn 20
let x21 = Xreg.of_int_exn 21
let x22 = Xreg.of_int_exn 22
let x23 = Xreg.of_int_exn 23
let x24 = Xreg.of_int_exn 24
let x25 = Xreg.of_int_exn 25
let x26 = Xreg.of_int_exn 26
let x27 = Xreg.of_int_exn 27
let x28 = Xreg.of_int_exn 28
let x29 = Xreg.of_int_exn 29
let x30 = Xreg.of_int_exn 30
let fp = x29
let lr = x30
let w0 = Wreg.of_int_exn 0
let w1 = Wreg.of_int_exn 1
let w2 = Wreg.of_int_exn 2
let w3 = Wreg.of_int_exn 3
let w4 = Wreg.of_int_exn 4
let w5 = Wreg.of_int_exn 5
let w6 = Wreg.of_int_exn 6
let w7 = Wreg.of_int_exn 7
let w8 = Wreg.of_int_exn 8
let w9 = Wreg.of_int_exn 9
let w10 = Wreg.of_int_exn 10
let w11 = Wreg.of_int_exn 11
let w12 = Wreg.of_int_exn 12
let w13 = Wreg.of_int_exn 13
let w14 = Wreg.of_int_exn 14
let w15 = Wreg.of_int_exn 15
let w16 = Wreg.of_int_exn 16
let w17 = Wreg.of_int_exn 17
let w18 = Wreg.of_int_exn 18
let w19 = Wreg.of_int_exn 19
let w20 = Wreg.of_int_exn 20
let w21 = Wreg.of_int_exn 21
let w22 = Wreg.of_int_exn 22
let w23 = Wreg.of_int_exn 23
let w24 = Wreg.of_int_exn 24
let w25 = Wreg.of_int_exn 25
let w26 = Wreg.of_int_exn 26
let w27 = Wreg.of_int_exn 27
let w28 = Wreg.of_int_exn 28
let w29 = Wreg.of_int_exn 29
let w30 = Wreg.of_int_exn 30
let d0 = Dreg.of_int_exn 0
let d1 = Dreg.of_int_exn 1
let d2 = Dreg.of_int_exn 2
let d3 = Dreg.of_int_exn 3
let d4 = Dreg.of_int_exn 4
let d5 = Dreg.of_int_exn 5
let d6 = Dreg.of_int_exn 6
let d7 = Dreg.of_int_exn 7
let d8 = Dreg.of_int_exn 8
let d9 = Dreg.of_int_exn 9
let d10 = Dreg.of_int_exn 10
let d11 = Dreg.of_int_exn 11
let d12 = Dreg.of_int_exn 12
let d13 = Dreg.of_int_exn 13
let d14 = Dreg.of_int_exn 14
let d15 = Dreg.of_int_exn 15
let d16 = Dreg.of_int_exn 16
let d17 = Dreg.of_int_exn 17
let d18 = Dreg.of_int_exn 18
let d19 = Dreg.of_int_exn 19
let d20 = Dreg.of_int_exn 20
let d21 = Dreg.of_int_exn 21
let d22 = Dreg.of_int_exn 22
let d23 = Dreg.of_int_exn 23
let d24 = Dreg.of_int_exn 24
let d25 = Dreg.of_int_exn 25
let d26 = Dreg.of_int_exn 26
let d27 = Dreg.of_int_exn 27
let d28 = Dreg.of_int_exn 28
let d29 = Dreg.of_int_exn 29
let d30 = Dreg.of_int_exn 30
let d31 = Dreg.of_int_exn 31
let s0 = Sreg.of_int_exn 0
let s1 = Sreg.of_int_exn 1
let s2 = Sreg.of_int_exn 2
let s3 = Sreg.of_int_exn 3
let s4 = Sreg.of_int_exn 4
let s5 = Sreg.of_int_exn 5
let s6 = Sreg.of_int_exn 6
let s7 = Sreg.of_int_exn 7
let s8 = Sreg.of_int_exn 8
let s9 = Sreg.of_int_exn 9
let s10 = Sreg.of_int_exn 10
let s11 = Sreg.of_int_exn 11
let s12 = Sreg.of_int_exn 12
let s13 = Sreg.of_int_exn 13
let s14 = Sreg.of_int_exn 14
let s15 = Sreg.of_int_exn 15
let s16 = Sreg.of_int_exn 16
let s17 = Sreg.of_int_exn 17
let s18 = Sreg.of_int_exn 18
let s19 = Sreg.of_int_exn 19
let s20 = Sreg.of_int_exn 20
let s21 = Sreg.of_int_exn 21
let s22 = Sreg.of_int_exn 22
let s23 = Sreg.of_int_exn 23
let s24 = Sreg.of_int_exn 24
let s25 = Sreg.of_int_exn 25
let s26 = Sreg.of_int_exn 26
let s27 = Sreg.of_int_exn 27
let s28 = Sreg.of_int_exn 28
let s29 = Sreg.of_int_exn 29
let s30 = Sreg.of_int_exn 30
let s31 = Sreg.of_int_exn 31
let q0 = Qreg.of_int_exn 0
let q1 = Qreg.of_int_exn 1
let q2 = Qreg.of_int_exn 2
let q3 = Qreg.of_int_exn 3
let q4 = Qreg.of_int_exn 4
let q5 = Qreg.of_int_exn 5
let q6 = Qreg.of_int_exn 6
let q7 = Qreg.of_int_exn 7
let q8 = Qreg.of_int_exn 8
let q9 = Qreg.of_int_exn 9
let q10 = Qreg.of_int_exn 10
let q11 = Qreg.of_int_exn 11
let q12 = Qreg.of_int_exn 12
let q13 = Qreg.of_int_exn 13
let q14 = Qreg.of_int_exn 14
let q15 = Qreg.of_int_exn 15
let q16 = Qreg.of_int_exn 16
let q17 = Qreg.of_int_exn 17
let q18 = Qreg.of_int_exn 18
let q19 = Qreg.of_int_exn 19
let q20 = Qreg.of_int_exn 20
let q21 = Qreg.of_int_exn 21
let q22 = Qreg.of_int_exn 22
let q23 = Qreg.of_int_exn 23
let q24 = Qreg.of_int_exn 24
let q25 = Qreg.of_int_exn 25
let q26 = Qreg.of_int_exn 26
let q27 = Qreg.of_int_exn 27
let q28 = Qreg.of_int_exn 28
let q29 = Qreg.of_int_exn 29
let q30 = Qreg.of_int_exn 30
let q31 = Qreg.of_int_exn 31
let v0 = Vreg.of_int_exn 0
let v1 = Vreg.of_int_exn 1
let v2 = Vreg.of_int_exn 2
let v3 = Vreg.of_int_exn 3
let v4 = Vreg.of_int_exn 4
let v5 = Vreg.of_int_exn 5
let v6 = Vreg.of_int_exn 6
let v7 = Vreg.of_int_exn 7
let v8 = Vreg.of_int_exn 8
let v9 = Vreg.of_int_exn 9
let v10 = Vreg.of_int_exn 10
let v11 = Vreg.of_int_exn 11
let v12 = Vreg.of_int_exn 12
let v13 = Vreg.of_int_exn 13
let v14 = Vreg.of_int_exn 14
let v15 = Vreg.of_int_exn 15
let v16 = Vreg.of_int_exn 16
let v17 = Vreg.of_int_exn 17
let v18 = Vreg.of_int_exn 18
let v19 = Vreg.of_int_exn 19
let v20 = Vreg.of_int_exn 20
let v21 = Vreg.of_int_exn 21
let v22 = Vreg.of_int_exn 22
let v23 = Vreg.of_int_exn 23
let v24 = Vreg.of_int_exn 24
let v25 = Vreg.of_int_exn 25
let v26 = Vreg.of_int_exn 26
let v27 = Vreg.of_int_exn 27
let v28 = Vreg.of_int_exn 28
let v29 = Vreg.of_int_exn 29
let v30 = Vreg.of_int_exn 30
let v31 = Vreg.of_int_exn 31

(* {1 Builders} *)

include Aarch64_ops.Builders
