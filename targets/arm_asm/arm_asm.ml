(* Typed builders for the hand-written ARM (A32) instructions, one per operand shape of each
   opcode and named by the mnemonic ([.] becomes [_], a keyword gains [_]) with a suffix for the
   shape: [_i] an immediate, [_sh] a shifted register, [_post] a post-indexed address, [_s] a
   label or symbol expression where the shape also takes a number, [_l] a literal label, [_z]
   zero. Every builder of an instruction with a condition takes [?cond]; a flag-setting form is an
   opcode of its own ([adds]). Immediates are [int] ([int64] for a modified immediate); lowering
   still checks ranges. Core registers are {!Arm_encode.Reg.t}, double-precision ones
   {!Arm_encode.Dreg.t} and single-precision ones {!Arm_encode.Sreg.t}, so a [d] register where an
   [s] one is expected does not compile. *)

open Arm_encode
module Cond = Cond
module Reg = Reg
module Dreg = Dreg
module Sreg = Sreg
module Lo = Lo

type shift = Arm_ops.Shift.kind = Lsl | Lsr | Asr | Ror
type shifted = Arm_ops.Shift.t

(* {1 Operands} *)

(* A register shifted by a constant: [r2, lsl #2]. *)
let shifted kind reg amount = { Arm_ops.Shift.reg; kind; amount }

(* The address [\[base, #off\]]. *)
let mem ?(off = 0) base =
  { Mem.base; offset = Mem.Imm (Int64.of_int off); writeback = false; pre = true }

(* [\[base, #off\]!]. *)
let mem_wb ?(off = 0) base = { (mem ~off base) with Mem.writeback = true }

(* [\[base, rm\]], or [\[base, -rm\]]. *)
let mem_reg ?(negate = false) base reg =
  {
    Mem.base;
    offset = Mem.Reg_offset { reg; negate; kind = 0; amount = 0 };
    writeback = false;
    pre = true;
  }

(* [\[base, rm, lsl #n\]]. *)
let mem_shifted ?(negate = false) base reg kind amount =
  {
    Mem.base;
    offset = Mem.Reg_offset { reg; negate; kind = Arm_ops.Shift.code kind; amount };
    writeback = false;
    pre = true;
  }

(* {1 Registers} *)

let r0 = Reg.r0
let r1 = Reg.r1
let r2 = Reg.r2
let r3 = Reg.r3
let r4 = Reg.r4
let r5 = Reg.r5
let r6 = Reg.r6
let r7 = Reg.r7
let r8 = Reg.r8
let r9 = Reg.r9
let r10 = Reg.r10
let r11 = Reg.r11
let r12 = Reg.r12
let r13 = Reg.r13
let r14 = Reg.r14
let r15 = Reg.r15
let ip = Reg.ip
let fp = Reg.fp
let sl = Reg.sl
let sp = Reg.sp
let lr = Reg.lr
let pc = Reg.pc
let d0 = Dreg.d0
let d1 = Dreg.d1
let d2 = Dreg.d2
let d3 = Dreg.d3
let d4 = Dreg.d4
let d5 = Dreg.d5
let d6 = Dreg.d6
let d7 = Dreg.d7
let d8 = Dreg.d8
let d9 = Dreg.d9
let d10 = Dreg.d10
let d11 = Dreg.d11
let d12 = Dreg.d12
let d13 = Dreg.d13
let d14 = Dreg.d14
let d15 = Dreg.d15
let d16 = Dreg.d16
let d17 = Dreg.d17
let d18 = Dreg.d18
let d19 = Dreg.d19
let d20 = Dreg.d20
let d21 = Dreg.d21
let d22 = Dreg.d22
let d23 = Dreg.d23
let d24 = Dreg.d24
let d25 = Dreg.d25
let d26 = Dreg.d26
let d27 = Dreg.d27
let d28 = Dreg.d28
let d29 = Dreg.d29
let d30 = Dreg.d30
let d31 = Dreg.d31
let s0 = Sreg.s0
let s1 = Sreg.s1
let s2 = Sreg.s2
let s3 = Sreg.s3
let s4 = Sreg.s4
let s5 = Sreg.s5
let s6 = Sreg.s6
let s7 = Sreg.s7
let s8 = Sreg.s8
let s9 = Sreg.s9
let s10 = Sreg.s10
let s11 = Sreg.s11
let s12 = Sreg.s12
let s13 = Sreg.s13
let s14 = Sreg.s14
let s15 = Sreg.s15
let s16 = Sreg.s16
let s17 = Sreg.s17
let s18 = Sreg.s18
let s19 = Sreg.s19
let s20 = Sreg.s20
let s21 = Sreg.s21
let s22 = Sreg.s22
let s23 = Sreg.s23
let s24 = Sreg.s24
let s25 = Sreg.s25
let s26 = Sreg.s26
let s27 = Sreg.s27
let s28 = Sreg.s28
let s29 = Sreg.s29
let s30 = Sreg.s30
let s31 = Sreg.s31

(* {1 Builders} *)

include Arm_ops.Builders
