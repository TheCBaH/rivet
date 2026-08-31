(* The arm (A32) target description (.ai/asm_plan.md §5.1).

   Not in a family functor with AArch64, per §5.1's explicit instruction: they
   share a vendor and nothing else. A32 is a fixed 32-bit word with a condition
   field on every instruction and a rotate-based immediate relation; A64 has no
   condition field, no rotation, and a different register file. Forcing them
   together would produce a functor whose parameter had to supply the entire
   encoding.

   Byte order. A form here is authored as one 32-bit word, most significant bit
   first, so [Bits.to_bytes] gives the big-endian rendering and {!encode}
   reverses it. Both directions are little-endian in memory, and every committed
   artifact is in memory order. *)

open Foundation
module C = Codec

(* {1 Registers}

   One table with aliases, rather than a table plus an alias map: [sp] and [r13]
   must produce the same encoding and the same diagnostic, and two structures
   that have to agree eventually will not. [name] holds the canonical spelling,
   which is what the disassembler prints; the alias is only an input spelling. *)

module Reg = struct
  type t = { name : string; num : int }

  let equal a b = a.num = b.num
  let pp ppf r = Fmt.string ppf r.name
  let all = List.init 16 (fun i -> { name = Printf.sprintf "r%d" i; num = i })
  let aliases = [ ("sp", 13); ("lr", 14); ("pc", 15); ("ip", 12); ("fp", 11); ("sl", 10) ]

  (* The canonical spelling of r13/r14/r15 is [sp]/[lr]/[pc]: that is what GNU
     objdump prints, and the diagnostic disassembler is compared against it. *)
  let canonical_name n =
    match n with
    | 10 -> "sl"
    | 11 -> "fp"
    | 12 -> "ip"
    | 13 -> "sp"
    | 14 -> "lr"
    | 15 -> "pc"
    | _ -> Printf.sprintf "r%d" n

  let of_num n = { name = canonical_name (n land 15); num = n land 15 }

  let find name =
    match List.assoc_opt name aliases with
    | Some n -> Some (of_num n)
    | None -> (
        match List.find_opt (fun r -> String.equal r.name name) all with
        | Some r -> Some (of_num r.num)
        | None -> None)

  (* The two registers the procedure-call standard fixes, named because an AST
     written by hand says [Reg.lr] far more often than it says [r14]. *)
  let sp = of_num 13
  let lr = of_num 14
end

(* {1 VFP registers}

   Sixteen doubleword (D0-D15) and thirty-two singleword (S0-S31) extension
   registers - VFPv2's range, which is what CompCert's ARM codegen ever
   emits (measured: the [test/c/] corpus never names D16 or above). Every VFP
   encoding splits a register's number across a non-adjacent 1-bit "extension"
   field and a 4-bit field elsewhere in the word - {!Dreg.hilo}/{!Sreg.hilo}
   are that split, verified against the real, installed
   [arm-linux-gnueabihf-as]/[objdump] for both register classes (e.g. [vmov.f32
   s0, s1] is [eeb00a60], where [s1]'s extension bit lands at bit 5 - not bit
   4 - which only real tool output caught). The two classes invert which side
   of the split carries the extension bit: doubleword is [D:Vx] (D is the
   high bit), singleword is [Vx:D] (D is the low bit) - so implementing D0-D31
   here, not just D0-D15, is nearly free once the split exists, and correct
   for any future corpus item that does need it. *)
module Dreg = struct
  type t = { num : int }

  let equal a b = a.num = b.num
  let of_num n = { num = n land 31 }
  let name r = Printf.sprintf "d%d" r.num
  let pp ppf r = Fmt.string ppf (name r)

  let find s =
    if String.length s > 1 && s.[0] = 'd' then
      match int_of_string_opt (String.sub s 1 (String.length s - 1)) with
      | Some n when n >= 0 && n <= 31 -> Some (of_num n)
      | _ -> None
    else None

  (* [hi] is the register's extension bit ("D" at bit 22 for Vd, "N" at bit 7
     for Vn, "M" at bit 5 for Vm - the caller places it), [lo] the 4-bit field
     next to the register's other operands in the word. *)
  let hilo r = (Int64.of_int (r.num lsr 4), Int64.of_int (r.num land 0xF))
  let of_hilo hi lo = of_num ((Int64.to_int hi lsl 4) lor Int64.to_int lo)
end

module Sreg = struct
  type t = { num : int }

  let equal a b = a.num = b.num
  let of_num n = { num = n land 31 }
  let name r = Printf.sprintf "s%d" r.num
  let pp ppf r = Fmt.string ppf (name r)

  let find s =
    if String.length s > 1 && s.[0] = 's' then
      match int_of_string_opt (String.sub s 1 (String.length s - 1)) with
      | Some n when n >= 0 && n <= 31 -> Some (of_num n)
      | _ -> None
    else None

  (* Reversed from {!Dreg.hilo}: the 4-bit field is the high bits here, and
     the extension bit ("D"/"N"/"M", same word positions as the doubleword
     case) is the register number's low bit. *)
  let hilo r = (Int64.of_int (r.num lsr 1), Int64.of_int (r.num land 1))
  let of_hilo hi lo = of_num ((Int64.to_int hi lsl 1) lor Int64.to_int lo)
end

(* {1 Operands} *)

let shift_name = function 0 -> "lsl" | 1 -> "lsr" | 2 -> "asr" | 3 -> "ror" | _ -> "?"

let shift_of_name = function
  | "lsl" -> Some 0
  | "lsr" -> Some 1
  | "asr" -> Some 2
  | "ror" -> Some 3
  | _ -> None

module Mem = struct
  (* [Reg_offset] is its own constructor rather than folding the index into
     [Imm]'s type, because A32's register-offset addressing ([ldr r2, [r1,
     r2, lsl #2]]) is a genuinely different encoding (bit 25 set, an imm5 +
     shift-kind + Rm field where the immediate form has a 12-bit magnitude),
     not a different spelling of the same one. Named [Reg_offset], not [Reg],
     so it cannot be confused with the [Reg] module or {!Operand.Reg} once
     this file's [include Arm_encode] brings both into arm.ml's scope
     unqualified. *)
  type offset =
    | Imm of int64
    | Reg_offset of { reg : Reg.t; negate : bool; kind : int; amount : int }

  type t = { base : Reg.t; offset : offset; writeback : bool; pre : bool }

  let pp ppf m =
    let off =
      match m.offset with
      | Imm v -> if Int64.equal v 0L then "" else Printf.sprintf ", #%Ld" v
      | Reg_offset { reg; negate; kind = 0; amount = 0 } ->
          Printf.sprintf ", %s%s" (if negate then "-" else "") reg.Reg.name
      | Reg_offset { reg; negate; kind; amount } ->
          Printf.sprintf ", %s%s, %s #%d"
            (if negate then "-" else "")
            reg.Reg.name (shift_name kind) amount
    in
    if m.pre then Fmt.pf ppf "[%a%s]%s" Reg.pp m.base off (if m.writeback then "!" else "")
    else Fmt.pf ppf "[%a]%s" Reg.pp m.base off
end

(* Exact decimal spelling of a finite double - GAS's own accepted syntax for a
   VFP immediate ([vmov.f64 d0, #1.5]), and what {!decode} reconstructs so a
   disassembled operand re-parses. Every double is an exact dyadic rational
   (mantissa * 2^exp), so this never rounds: the algorithm scales the mantissa
   by 5^k (k = the negated binary exponent, once trailing zero mantissa bits
   are stripped) to turn it into an exact decimal integer, then places the
   point k digits from the right - the standard technique for printing a
   binary fraction as an exact decimal. Matches GAS's own convention of a
   trailing bare dot with no fraction digits for a whole number ([1.], not
   [1.0]) because trailing zeros are stripped before the dot is placed. *)
let decimal_of_float v =
  if Float.equal v 0.0 then "0."
  else
    let bits = Int64.bits_of_float v in
    let sign_bit = Int64.logand (Int64.shift_right_logical bits 63) 1L in
    let biased_exp = Int64.to_int (Int64.logand (Int64.shift_right_logical bits 52) 0x7FFL) in
    let frac = Int64.logand bits 0xFFFFFFFFFFFFFL in
    let mantissa, exp2 =
      if biased_exp = 0 then (frac, -1074)
      else (Int64.logor frac 0x10000000000000L, biased_exp - 1075)
    in
    let rec strip m e =
      if Int64.equal (Int64.logand m 1L) 0L && not (Int64.equal m 0L) then
        strip (Int64.shift_right_logical m 1) (e + 1)
      else (m, e)
    in
    let m, e = strip mantissa exp2 in
    let digits =
      if e >= 0 then Bigint.to_string (Bigint.shift_left_exn (Bigint.of_int64 m) e) ^ "."
      else
        let k = -e in
        let rec pow5 n acc =
          if n = 0 then acc else pow5 (n - 1) (Bigint.mul acc (Bigint.of_int 5))
        in
        let scaled = Bigint.mul (Bigint.of_int64 m) (pow5 k Bigint.one) in
        let s = Bigint.to_string scaled in
        let s = if String.length s <= k then String.make (k - String.length s + 1) '0' ^ s else s in
        let cut = String.length s - k in
        let int_part = String.sub s 0 cut and frac_part = String.sub s cut k in
        let n = ref (String.length frac_part) in
        while !n > 0 && frac_part.[!n - 1] = '0' do
          decr n
        done;
        int_part ^ "." ^ String.sub frac_part 0 !n
    in
    (if Int64.equal sign_bit 1L then "-" else "") ^ digits

(* {1 The VFP modified-immediate relation}

   [vmov.f64 d0, #1.5]'s 8-bit field is [sign:b:exp_lo(2):frac(4)], expanding
   per the ARM ARM's VFPExpandImm to [sign : NOT(b) : Repeat(b, n) : exp_lo :
   frac : Zeros] - the same shape as A32's rotate-based integer modified
   immediate above, just a different expansion. Precision-independent by
   design: the *value* an imm8 names does not depend on whether the
   instruction is [.f64] or [.f32], only the width of the packed
   exponent/fraction the CPU expands it into at execution time does - so
   there is one relation here, expressed at double width, reused by both
   {!Lowered.Vmov_imm_d} and {!Lowered.Vmov_imm_s} alike. Verified against
   the real, installed arm-linux-gnueabihf-as/objdump: [vmov.f64 d0, #1.] is
   imm8 0x70 (expands to the double bit pattern for 1.0), [#1.5] is 0x78,
   [#0.5] is 0x60, [#-5.] is 0x94. *)
let vfp_expand imm8 =
  let sign = (imm8 lsr 7) land 1 in
  let b = (imm8 lsr 6) land 1 in
  let lo2 = (imm8 lsr 4) land 0x3 in
  let frac4 = imm8 land 0xF in
  let notb = 1 - b in
  let mid8 = if b = 1 then 0xFF else 0x00 in
  let exp11 = (notb lsl 10) lor (mid8 lsl 2) lor lo2 in
  let bits =
    Int64.logor
      (Int64.shift_left (Int64.of_int sign) 63)
      (Int64.logor
         (Int64.shift_left (Int64.of_int exp11) 52)
         (Int64.shift_left (Int64.of_int frac4) 48))
  in
  Int64.float_of_bits bits

(* The inverse: [None] unless [v] is one of the 256 values the expansion
   above can produce - checked by requiring the fraction's low 48 bits to be
   zero and the exponent's middle 8 bits to all equal [b], both of which the
   expansion always establishes and nothing else can. *)
let vfp_compress v =
  let bits = Int64.bits_of_float v in
  let sign = Int64.to_int (Int64.logand (Int64.shift_right_logical bits 63) 1L) in
  let exp11 = Int64.to_int (Int64.logand (Int64.shift_right_logical bits 52) 0x7FFL) in
  let frac52 = Int64.logand bits 0xFFFFFFFFFFFFFL in
  if not (Int64.equal (Int64.logand frac52 0xFFFFFFFFFFFFL) 0L) then None
  else
    let frac4 = Int64.to_int (Int64.shift_right_logical frac52 48) in
    let lo2 = exp11 land 0x3 in
    let mid8 = (exp11 lsr 2) land 0xFF in
    let notb = (exp11 lsr 10) land 1 in
    let b = 1 - notb in
    let expect_mid8 = if b = 1 then 0xFF else 0x00 in
    if mid8 <> expect_mid8 then None else Some ((sign lsl 7) lor (b lsl 6) lor (lo2 lsl 4) lor frac4)

(* Every imm8 value, 0 to 255: small enough to enumerate outright, the same
   justification {!modimm_domain} gives for A32's integer modified
   immediate. *)
let vfp_imm_domain =
  C.Finite.exhaustive ~name:"arm.vfp-imm"
    ~why:
      "the field is a sign bit, a repeat bit, a 2-bit exponent tail and a 4-bit fraction, so the \
       domain is exactly 256 values and enumerating it is a table scan rather than a sample"
    (List.init 256 vfp_expand)

module Operand = struct
  (* [Shifted] is one operand, not two: [r1, lsl #1] arrives as two
     comma-separated slices from the common parser and is rejoined here, the
     same way x86 rejoins the pieces of a scaled address. Keeping it one operand
     is what makes [add r0, r0, r1, lsl #1] a three-operand instruction, which
     is what it is. *)
  type t =
    | Reg of Reg.t
    | Imm of Bigint.t
    | Mem of Mem.t
    | Sym of Asm_core.Expr.t
    | Shifted of { reg : Reg.t; kind : int; amount : int }
    | Dreg of Dreg.t
    | Sreg of Sreg.t
    | FImm of float
        (** a VFP modified-immediate literal, e.g. [#1.5] - {!Dreg.t}/{!Sreg.t}'s dot. *)
    | Reglist of int list
        (** [{r0, r1, r2, r3}] - [push]'s operand (M5, asm/docs/corpus.md). Register *numbers*, not
            {!Reg.t}s: the encoding is one bit per number and the surface order carries no
            information the bitmask does not already have, so keeping the list unsorted-as-written
            here would only invite a caller to rely on an order the instruction itself discards. *)
    | Reg_writeback of Reg.t
        (** [Rn!] with no brackets - [stmia]/[ldmia]'s own writeback base register (M5,
            asm/docs/corpus.md classify-c-gcc: [stmia r3!, {r0, r1}]), a bare-register grammar
            distinct from {!Mem.t}'s bracketed [\[Rn, #imm\]!] since STM/LDM's writeback amount is
            implicit rather than an explicit offset. *)
    | Dreglist of int list
        (** [{d8, d9}] - [vpush.64]'s D-register list (M5, asm/docs/corpus.md classify-c-gcc:
            [vpush.64 {d8, d9}]), structurally the same shape as {!Reglist} but over {!Dreg.t}'s
            number space rather than {!Reg.t}'s - the two are never a single list because a
            bitmask position means a different register in each. {!Opcode.Vpush} consumes it,
            requiring a non-empty, ascending, contiguous run (VSTM-class encodings have a base
            register and a count, not an arbitrary bitmask). [vpop.64] itself is still
            unevidenced and out of scope. *)

  let pp ppf = function
    | Reg r -> Reg.pp ppf r
    | Imm v -> Fmt.pf ppf "#%a" Bigint.pp v
    | Mem m -> Mem.pp ppf m
    | Sym e -> Fmt.string ppf (Asm_core.Expr.to_string e)
    | Shifted { reg; kind; amount } -> Fmt.pf ppf "%a, %s #%d" Reg.pp reg (shift_name kind) amount
    | Dreg r -> Dreg.pp ppf r
    | Sreg r -> Sreg.pp ppf r
    | FImm v -> Fmt.pf ppf "#%s" (decimal_of_float v)
    | Reglist regs ->
        Fmt.pf ppf "{%a}"
          Fmt.(list ~sep:(any ", ") Reg.pp)
          (List.map Reg.of_num (List.sort compare regs))
    | Reg_writeback r -> Fmt.pf ppf "%a!" Reg.pp r
    | Dreglist regs ->
        Fmt.pf ppf "{%a}"
          Fmt.(list ~sep:(any ", ") Dreg.pp)
          (List.map Dreg.of_num (List.sort compare regs))
end

module Surface = struct
  type t = { mnemonic : string; ops : Operand.t list; origin : Origin.t }

  let pp ppf s =
    match s.ops with
    | [] -> Fmt.string ppf s.mnemonic
    | ops -> Fmt.pf ppf "%s %a" s.mnemonic Fmt.(list ~sep:(any ", ") Operand.pp) ops
end

(* {1 Condition codes}

   Every A32 instruction carries one, in the top four bits, which is the single
   biggest difference from the other three targets: on x86 a condition belongs
   to a handful of opcodes, and here it belongs to the word. M1 encoded it as
   the constant [al] because no fixture needed anything else, and said so; this
   is where that ends.

   Declared in encoding order so the code is the position. [al] is 14 and [nv]
   is 15 - deprecated since ARMv5 and never emitted - so it is named and
   accepted on decode rather than pretended not to exist. *)
module Cond = struct
  type t = Eq | Ne | Cs | Cc | Mi | Pl | Vs | Vc | Hi | Ls | Ge | Lt | Gt | Le | Al | Nv

  let all = [ Eq; Ne; Cs; Cc; Mi; Pl; Vs; Vc; Hi; Ls; Ge; Lt; Gt; Le; Al; Nv ]

  let name = function
    | Eq -> "eq"
    | Ne -> "ne"
    | Cs -> "cs"
    | Cc -> "cc"
    | Mi -> "mi"
    | Pl -> "pl"
    | Vs -> "vs"
    | Vc -> "vc"
    | Hi -> "hi"
    | Ls -> "ls"
    | Ge -> "ge"
    | Lt -> "lt"
    | Gt -> "gt"
    | Le -> "le"
    | Al -> "al"
    | Nv -> "nv"

  let equal (a : t) b = a = b

  let code c =
    let rec go i = function [] -> 14 | x :: rest -> if equal x c then i else go (i + 1) rest in
    go 0 all

  (* [hs] and [lo] are the unsigned spellings of [cs] and [cc]; GAS accepts
     both and objdump prints the first, so they are input-only here for the
     same reason x86's [jz] is. *)
  let synonyms = [ ("hs", Cs); ("lo", Cc) ]

  let of_name s =
    match List.find_opt (fun c -> String.equal (name c) s) all with
    | Some c -> Some c
    | None -> List.assoc_opt s synonyms

  (* [al] is not written: [moveq] and [mov] are the two spellings, and [moval]
     - which GAS does accept - is never what comes back out. *)
  let suffix c = if equal c Al then "" else name c

  (* Split a mnemonic into its stem and condition, longest suffix first so
     [movne] is [mov]+[ne] rather than failing on [movn]. Returns [Al] when
     there is no suffix, which is what an unconditional instruction is. *)
  let split m =
    let try_one acc (spelling, c) =
      match acc with
      | Some _ -> acc
      | None ->
          let n = String.length m and k = String.length spelling in
          if n > k && String.sub m (n - k) k = spelling then Some (String.sub m 0 (n - k), c)
          else None
    in
    let candidates = List.map (fun c -> (name c, c)) all @ synonyms in
    match List.fold_left try_one None candidates with Some r -> r | None -> (m, Al)
end

(* Add/sub/mul/div share one three-operand VFP encoding, distinguished only by
   a 3-bit selector spread across bits 23/21/20 plus one more at bit 6 -
   {!V3.selector} below. A shared [Lowered] payload parametrized by this type,
   rather than four near-identical constructors, is what keeps that one codec
   alt from being four near-identical alts. *)
module V3 = struct
  type op = Vadd | Vsub | Vmul | Vdiv

  let name = function Vadd -> "vadd" | Vsub -> "vsub" | Vmul -> "vmul" | Vdiv -> "vdiv"

  (* (b23, b21, b20, b6), verified against arm-linux-gnueabihf-as/objdump:
     vadd.f64 is [ee310b02], vsub.f64 (same b23/b21/b20, b6 flips) is
     [ee310b42], vmul.f64 is [ee210b02], vdiv.f64 is [ee810b02]. *)
  let selector = function
    | Vadd -> (0L, 1L, 1L, 0L)
    | Vsub -> (0L, 1L, 1L, 1L)
    | Vmul -> (0L, 1L, 0L, 0L)
    | Vdiv -> (1L, 0L, 0L, 0L)

  let of_selector = function
    | 0L, 1L, 1L, 0L -> Some Vadd
    | 0L, 1L, 1L, 1L -> Some Vsub
    | 0L, 1L, 0L, 0L -> Some Vmul
    | 1L, 0L, 0L, 0L -> Some Vdiv
    | _ -> None
end

module Opcode = struct
  type t =
    | Mov
    | Add
    | Sub
    | And
    | Cmp
    | Eor  (** [eor rd, rn, rm[, shift]] - bitwise XOR, the same data-processing family as {!And} *)
    | Rsb
        (** [rsb rd, rn, rm[, shift]] / [rsb rd, rn, #imm] - reverse subtract (computes [op2 - rn]),
            structurally identical to {!Sub} in every operand shape this corpus evidences. *)
    | Orr  (** [orr rd, rn, rm[, shift]] - bitwise OR, the same data-processing family as {!And}. *)
    | Mvn
        (** [mvn rd, rm] / [mvn rd, #imm] - bitwise NOT, {!Mov}'s own two-operand shape (no [rn]) in
            the same data-processing family. *)
    | Bic
        (** [bic rd, rn, rm[, shift]] / [bic rd, rn, #imm] - bit clear ([rn AND NOT op2]), the same
            data-processing family as {!And}/{!Orr}. *)
    | Adc
        (** [adc rd, rn, rm[, shift]] - add with carry, the same data-processing family as
            {!And}/{!Orr}/{!Bic} (evidenced only in the plain register-register-register shape:
            asm/docs/corpus.md's [siphash24.c], the upper half of a 64-bit add {!Adds} already
            covers the lower half of). *)
    | Adds
        (** [adds rd, rn, rm] - {!Add}'s own flag-setting form (dp = 4, [s] = 1), evidenced only in
            this one register-register-register shape (asm/docs/corpus.md: gcc's own carry-out
            idiom, [siphash24.c]'s 64-bit add built from two 32-bit halves). A [dp] value alone
            cannot tell [add] and [adds] apart - only the already-present [s] field can - so this is
            its own opcode with its own dedicated lowering/printing case, the same relationship
            {!Cmp} already has to being "[sub] with [s] = 1 and no destination": {!to_dp} still maps
            it to 4, but it is deliberately excluded from the generic [Add]/[Sub]/[And]/... lowering
            arm, which always produces [s] = false. *)
    | Mul
    | Mla
    | Str
    | Ldr
    | Bx
    | Strb
    | Ldrb
    | Udf
    | Bl
    | B
    | Movw
    | Movt
    | Ite
    | Nop
        (** the dedicated ARMv7 hint encoding ([e320f000], the same bytes {!nop_bytes} already pads
            with) - not a data-processing alias the way {!Mvn}'s [mov]-family siblings are. *)
    | Shift of int
        (** [lsl]/[lsr]/[asr]/[ror rd, rm, rs] - GNU's own mnemonics for {!Mov} with a
            register-specified shift amount, a different word shape from the already-supported
            immediate-shift-amount one ({!Lowered.Dp_reg}'s [sh_amt]: bit 4 is the immediate/register
            discriminator, and CompCert's own codegen only ever emits the register form under these
            names, never [mov rd, rm, TYPE rs]). [int] is the shared shift-kind encoding
            {!shift_of_name}/{!shift_name} already use. *)
    | Vmov_d  (** [vmov.f64 Dd, Dm] or [vmov.f64 Dd, #imm] - operand-shape dispatched *)
    | Vmov_s  (** [vmov.f32 Sd, Sm] or [vmov.f32 Sd, #imm] *)
    | Vmov_core  (** [vmov Rt, Sn] / [vmov Sn, Rt] / [vmov Rt, Rt2, Dm] / [vmov Dm, Rt, Rt2] *)
    | V3d of V3.op  (** [vadd.f64]/[vsub.f64]/[vmul.f64]/[vdiv.f64], double-precision *)
    | V3s of V3.op  (** [vadd.f32]/[vsub.f32]/[vmul.f32]/[vdiv.f32], single-precision *)
    | Vneg_d
    | Vneg_s
    | Vcmp_d  (** covers both [vcmp Dd, Dm] and [vcmp Dd, #0] - operand-shape dispatched *)
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
        (** [vpush.64 {dN, ...}], GNU's alias for [vstmdb sp!, {dN, ...}] - the D-register
            reglist push (M5, asm/docs/corpus.md: almabench.c/fft.c/fftsp.c/perlin.c's own
            varargs-spill prologue). Unlike {!Push}'s STMDB encoding, there is no separate
            one-register alias to exclude: VPUSH's own encoding covers a single D register
            exactly as it covers several (verified against real
            arm-linux-gnueabihf-as/objdump), so {!Operand.Dreglist} is accepted from one
            member up, provided it is contiguous and ascending. *)
    | Push
        (** [push {reglist}], GNU's alias for [stmdb sp!, {reglist}] - not a general STM/LDM (M5,
            asm/docs/corpus.md: CompCert's own ARM codegen only ever emits this one multi-register
            alias, always as its varargs-spill prologue). Scoped to two or more registers: GNU's own
            canonical printer spells a *one*-register [push {rN}] as [str rN, [sp, #-4]!] instead
            (verified against real [as]/[objdump]), a different encoding this corpus does not
            evidence and this opcode does not attempt. *)
    | Stmia
        (** [stmia Rn!, {reglist}] - increment-after STM, a different encoding class from
                 [push]'s STMDB one (asm/docs/corpus.md: gcc's [aes.c]). Always carries the bare
                 [Rn!] writeback base as an explicit operand, the only shape evidenced. *)
    | Ldmia
        (** [ldmia Rn!, {reglist}] - [Stmia]'s load counterpart, same encoding class with the
                 L bit set. *)
    | Pop
        (** [pop {reglist}], GNU's alias for [ldmia sp!, {reglist}] - the LDM-class counterpart of
            [Push]'s STMDB alias (asm/docs/corpus.md: [gas_frontier.t]'s [i64_udivmod]/[i64_umod]
            epilogues). Scoped to two or more registers for the same reason as [Push]: GNU spells a
            one-register [pop {rN}] as [ldr rN, [sp], #4] instead (verified against real
            [as]/[objdump]), a different encoding not attempted here. *)

  let name = function
    | Mov -> "mov"
    | Add -> "add"
    | Sub -> "sub"
    | And -> "and"
    | Cmp -> "cmp"
    | Eor -> "eor"
    | Rsb -> "rsb"
    | Orr -> "orr"
    | Mvn -> "mvn"
    | Bic -> "bic"
    | Adc -> "adc"
    | Adds -> "adds"
    | Nop -> "nop"
    | Shift k -> shift_name k
    | Mul -> "mul"
    | Mla -> "mla"
    | B -> "b"
    | Movw -> "movw"
    | Movt -> "movt"
    | Ite -> "ite"
    | Str -> "str"
    | Ldr -> "ldr"
    | Bx -> "bx"
    | Strb -> "strb"
    | Ldrb -> "ldrb"
    | Udf -> "udf"
    | Bl -> "bl"
    | Vmov_d -> "vmov.f64"
    | Vmov_s -> "vmov.f32"
    | Vmov_core -> "vmov"
    | V3d op -> V3.name op ^ ".f64"
    | V3s op -> V3.name op ^ ".f32"
    | Vneg_d -> "vneg.f64"
    | Vneg_s -> "vneg.f32"
    | Vcmp_d -> "vcmp.f64"
    | Vcmp_s -> "vcmp.f32"
    | Vmrs -> "vmrs"
    | Vcvt_f32_f64 -> "vcvt.f32.f64"
    | Vcvt_f64_f32 -> "vcvt.f64.f32"
    | Vcvt_f32_s32 -> "vcvt.f32.s32"
    | Vcvt_f64_s32 -> "vcvt.f64.s32"
    | Vcvt_f64_u32 -> "vcvt.f64.u32"
    | Vcvt_s32_f64 -> "vcvt.s32.f64"
    | Vldr -> "vldr"
    | Vstr -> "vstr"
    | Vpush -> "vpush.64"
    | Push -> "push"
    | Stmia -> "stmia"
    | Ldmia -> "ldmia"
    | Pop -> "pop"

  (* The surface spellings this target accepts. [simplify_instruction] consults
     this rather than repeating the list. VFP mnemonics are not here - they
     carry a [.f64]/[.f32] (or, for [vcvt], a two-part) type suffix that this
     whole-string table cannot express, and their condition, when present,
     sits *before* that suffix rather than at the very end - see
     {!split_vfp_mnemonic}/{!vfp_opcode_of} below. *)
  let of_mnemonic = function
    | "mov" -> Some Mov
    | "add" -> Some Add
    | "sub" -> Some Sub
    | "and" -> Some And
    | "cmp" -> Some Cmp
    | "eor" -> Some Eor
    | "rsb" -> Some Rsb
    | "orr" -> Some Orr
    | "mvn" -> Some Mvn
    | "bic" -> Some Bic
    | "adc" -> Some Adc
    | "adds" -> Some Adds
    | "nop" -> Some Nop
    | "mul" -> Some Mul
    | "mla" -> Some Mla
    | "b" -> Some B
    | "movw" -> Some Movw
    | "movt" -> Some Movt
    | "ite" -> Some Ite
    | "str" -> Some Str
    | "ldr" -> Some Ldr
    | "bx" -> Some Bx
    | "strb" -> Some Strb
    | "ldrb" -> Some Ldrb
    | "udf" -> Some Udf
    | "bl" -> Some Bl
    | "push" -> Some Push
    | "stmia" -> Some Stmia
    | "ldmia" -> Some Ldmia
    | "pop" -> Some Pop
    | m -> ( match shift_of_name m with Some k -> Some (Shift k) | None -> None)

  (* VFP's own mnemonic bases - never a substring of a GPR mnemonic above, so
     there is no ambiguity between the two tables. *)
  let vfp_bases =
    [
      "vmov";
      "vadd";
      "vsub";
      "vmul";
      "vdiv";
      "vneg";
      "vcmp";
      "vmrs";
      "vldr";
      "vstr";
      "vcvt";
      "vpush";
    ]

  (* [vmoveq.f64] splits as base [vmov], condition [eq], type suffix [f64] -
     the condition sits between the base and the first dot, not at the very
     end the way [Cond.split] (used by every other opcode) expects. Tried
     after the plain GPR table above, so this never shadows it. *)
  let split_vfp_mnemonic m =
    match String.split_on_char '.' m with
    | [] -> None
    | base_cond :: rest ->
        if List.mem base_cond vfp_bases then Some (base_cond, Cond.Al, rest)
        else
          let stem, cond = Cond.split base_cond in
          if List.mem stem vfp_bases then Some (stem, cond, rest) else None

  (* Operand shapes (register class, operand count) disambiguate everything
     [vcvt] cannot: [vmov]'s reg-move vs. immediate-move (an [Imm] vs an
     [FImm] operand), [vcmp]'s register vs. zero-compare, [vmov_core]'s four
     directions. Only [vcvt] needs the type suffix itself to pick an opcode,
     since e.g. [vcvt.f64.f32 d0, s0] and [vcvt.f64.s32 d0, s0] share an
     operand shape but mean different instructions. *)
  let vfp_opcode_of stem rest =
    match (stem, rest) with
    | "vmov", [ "f64" ] -> Some Vmov_d
    | "vmov", [ "f32" ] -> Some Vmov_s
    | "vmov", [] -> Some Vmov_core
    | "vadd", [ "f64" ] -> Some (V3d V3.Vadd)
    | "vsub", [ "f64" ] -> Some (V3d V3.Vsub)
    | "vmul", [ "f64" ] -> Some (V3d V3.Vmul)
    | "vdiv", [ "f64" ] -> Some (V3d V3.Vdiv)
    | "vadd", [ "f32" ] -> Some (V3s V3.Vadd)
    | "vsub", [ "f32" ] -> Some (V3s V3.Vsub)
    | "vmul", [ "f32" ] -> Some (V3s V3.Vmul)
    | "vdiv", [ "f32" ] -> Some (V3s V3.Vdiv)
    | "vneg", [ "f64" ] -> Some Vneg_d
    | "vneg", [ "f32" ] -> Some Vneg_s
    | "vcmp", [ "f64" ] -> Some Vcmp_d
    | "vcmp", [ "f32" ] -> Some Vcmp_s
    | "vmrs", [] -> Some Vmrs
    | "vldr", [] -> Some Vldr
    | "vstr", [] -> Some Vstr
    | "vpush", [ "64" ] -> Some Vpush
    | "vcvt", [ "f32"; "f64" ] -> Some Vcvt_f32_f64
    | "vcvt", [ "f64"; "f32" ] -> Some Vcvt_f64_f32
    | "vcvt", [ "f32"; "s32" ] -> Some Vcvt_f32_s32
    | "vcvt", [ "f64"; "s32" ] -> Some Vcvt_f64_s32
    | "vcvt", [ "f64"; "u32" ] -> Some Vcvt_f64_u32
    | "vcvt", [ "s32"; "f64" ] -> Some Vcvt_s32_f64
    | _ -> None

  (* A32 encodes add, sub and mov as one data-processing format distinguished
     by a 4-bit opcode field. The two directions live together so they cannot
     drift; [-1] is "this opcode is not a data-processing one". *)
  let to_dp = function
    | And -> 0
    | Eor -> 1
    | Sub -> 2
    | Rsb -> 3
    | Add | Adds -> 4
    | Adc -> 5
    | Cmp -> 10
    | Orr -> 12
    | Mov -> 13
    | Bic -> 14
    | Mvn -> 15
    | _ -> -1

  let of_dp = function
    | 0 -> Some And
    | 1 -> Some Eor
    | 2 -> Some Sub
    | 3 -> Some Rsb
    | 4 -> Some Add
    | 5 -> Some Adc
    | 10 -> Some Cmp
    | 12 -> Some Orr
    | 13 -> Some Mov
    | 14 -> Some Bic
    | 15 -> Some Mvn
    | _ -> None

  (* The three that write no destination register and set the flags instead, so
     their [Rd] field is zero and their [S] bit is one. Reading that off a table
     rather than off the opcode number is what keeps [Dp_reg] one form. *)
  let is_compare = function Cmp -> true | _ -> false
end

module Instruction = struct
  type t = { op : Opcode.t; cond : Cond.t; ops : Operand.t list }

  let mk ?(cond = Cond.Al) op ops = { op; cond; ops }

  (* The condition is a mnemonic suffix on the way out as well as in, and [al]
     is spelled by its absence. For a VFP opcode, [Opcode.name] already
     carries its own [.f64]/[.f32]/[.f32.f64]-shaped type suffix, and the
     condition has to land *before* that first dot ([vmoveq.f64], not
     [vmov.f64eq] - the latter is not an instruction GAS accepts), so it is
     spliced in there rather than appended at the end. *)
  let pp ppf i =
    let name = Opcode.name i.op and c = Cond.suffix i.cond in
    let m =
      match String.index_opt name '.' with
      | Some dot -> String.sub name 0 dot ^ c ^ String.sub name dot (String.length name - dot)
      | None -> name ^ c
    in
    match i.ops with
    | [] -> Fmt.string ppf m
    | ops -> Fmt.pf ppf "%s %a" m Fmt.(list ~sep:(any ", ") Operand.pp) ops
end

(* {1 Lowered forms}

   Named after the *encoding class* rather than the mnemonic, because A32
   encodes add, sub and mov as one data-processing format distinguished by a
   4-bit opcode field. Three constructors named add/sub/mov would each have to
   carry that field anyway, and could then disagree with their own name. *)

module Lowered = struct
  (* Every variant carries a condition, because every A32 word does. It is a
     field on each rather than a wrapper around the sum so that a form which
     structurally cannot be conditional would be visible by not having one -
     none is, today, which is itself worth being able to see. *)
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
      }
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
        (** [regs] is the 16-bit register-list bitmask directly, bit [n] set meaning [rn] is in the
            list - what the word itself stores, and what {!Opcode.Push}'s own doc explains the scope
            of. *)
    | Ldm of { cond : Cond.t; load : bool; rn : Reg.t; regs : int }
        (** [stmia]/[ldmia]'s own increment-after encoding class, distinct from {!Push}'s STMDB one
            (asm/docs/corpus.md). Always carries [rn] and forces W=1: the only evidenced shape is
            the bare [Rn!] writeback base, so there is no separate writeback flag to get wrong.
            {!Opcode.Pop} lowers into this too, with [rn] fixed to [Reg.sp] - the same relationship
            {!Push} has to [stmdb sp!] - and {!instruction_of_lowered} collapses it back to [pop]
            on the way out, mirroring how real [objdump] never prints [ldmia sp!, {reglist}] for
            two or more registers. *)
    | Udf of { imm16 : int64 }  (** permanently undefined, and unconditional by definition *)
    | Bl of { cond : Cond.t; target : Asm_core.Lowered_ast.branch }
    | B of { cond : Cond.t; target : Asm_core.Lowered_ast.branch }
    | Mul of { cond : Cond.t; s : bool; rd : Reg.t; rn : Reg.t; rm : Reg.t }
    | Mla of { cond : Cond.t; s : bool; rd : Reg.t; rn : Reg.t; rm : Reg.t; ra : Reg.t }
    | Movw_movt of { cond : Cond.t; top : bool; rd : Reg.t; imm : Asm_core.Lowered_ast.branch }
        (** [movw]/[movt] with a 16-bit immediate that is usually [:lower16:] or [:upper16:] of a
            symbol - hence a [branch], which is this codebase's name for "an operand that is either
            an expression awaiting the linker or a value a decoder recovered". *)
    (* {2 VFP}

           Every VFP data-processing form below shares the same skeleton (cond
           1110 1 D b21 b20 <4-bit> Vd 101 sz N op M 0 Vm - {!codec}'s comments
           give each instruction's exact bits, all verified against the real,
           installed arm-linux-gnueabihf-as/objdump) but no two share enough of
           it to be one constructor without a payload wide enough to defeat the
           point - so, as with the GPR forms above, each is named after what it
           does. *)
    | Vmov_reg_d of { cond : Cond.t; vd : Dreg.t; vm : Dreg.t }
    | Vmov_reg_s of { cond : Cond.t; vd : Sreg.t; vm : Sreg.t }
    | Vmov_imm_d of { cond : Cond.t; vd : Dreg.t; imm8 : int }
        (** [imm8] is the raw 8-bit VFP modified-immediate field ({!vfp_expand}/{!vfp_compress}
            are its relation to the actual float value), not the value itself - the same split
            {!modimm_codec} already keeps for A32's integer modified immediate. *)
    | Vmov_imm_s of { cond : Cond.t; vd : Sreg.t; imm8 : int }
    | Vmov_to_single of { cond : Cond.t; sn : Sreg.t; rt : Reg.t }  (** [vmov Sn, Rt] *)
    | Vmov_from_single of { cond : Cond.t; rt : Reg.t; sn : Sreg.t }  (** [vmov Rt, Sn] *)
    | Vmov_to_double of { cond : Cond.t; dm : Dreg.t; rt : Reg.t; rt2 : Reg.t }
        (** [vmov Dm, Rt, Rt2] *)
    | Vmov_from_double of { cond : Cond.t; rt : Reg.t; rt2 : Reg.t; dm : Dreg.t }
        (** [vmov Rt, Rt2, Dm] *)
    | V3_d of { cond : Cond.t; op : V3.op; vd : Dreg.t; vn : Dreg.t; vm : Dreg.t }
    | V3_s of { cond : Cond.t; op : V3.op; vd : Sreg.t; vn : Sreg.t; vm : Sreg.t }
    | Vneg_d of { cond : Cond.t; vd : Dreg.t; vm : Dreg.t }
    | Vneg_s of { cond : Cond.t; vd : Sreg.t; vm : Sreg.t }
    | Vcmp_reg_d of { cond : Cond.t; vd : Dreg.t; vm : Dreg.t }
    | Vcmp_reg_s of { cond : Cond.t; vd : Sreg.t; vm : Sreg.t }
    | Vcmp_zero_d of { cond : Cond.t; vd : Dreg.t }
    | Vcmp_zero_s of { cond : Cond.t; vd : Sreg.t }
    | Vmrs of { cond : Cond.t }
        (** always [vmrs APSR_nzcv, FPSCR] - the only form this corpus needs *)
    | Vcvt_f32_f64 of { cond : Cond.t; sd : Sreg.t; dm : Dreg.t }
    | Vcvt_f64_f32 of { cond : Cond.t; dd : Dreg.t; sm : Sreg.t }
    | Vcvt_f32_s32 of { cond : Cond.t; sd : Sreg.t; sm : Sreg.t }
    | Vcvt_f64_s32 of { cond : Cond.t; dd : Dreg.t; sm : Sreg.t }
    | Vcvt_f64_u32 of { cond : Cond.t; dd : Dreg.t; sm : Sreg.t }
    | Vcvt_s32_f64 of { cond : Cond.t; sd : Sreg.t; dm : Dreg.t }
    | Vmem_d of { cond : Cond.t; load : bool; vd : Dreg.t; rn : Reg.t; offset : int64 }
    | Vmem_s of { cond : Cond.t; load : bool; vd : Sreg.t; rn : Reg.t; offset : int64 }
    | Vldr_lit_d of { cond : Cond.t; vd : Dreg.t; target : Asm_core.Lowered_ast.branch }
        (** [vldr Dd, label] - a PC-relative literal-pool load, the same word shape as
            {!Vmem_d} with the base register fixed to [pc] (Rn = 0b1111) instead of a runtime
            register. Load-only: real hardware defines the store form's [Rn = 1111] encoding as
            UNPREDICTABLE, and no fixture ever spells [vstr] this way (GAS accepts it anyway, with
            a deprecation warning). Forward references only ([U] fixed to 1) - every corpus
            occurrence is a same-function trailing literal pool, and {!evaluate_fixup} rejects a
            negative distance rather than silently mis-encoding one. *)
    | Vldr_lit_s of { cond : Cond.t; vd : Sreg.t; target : Asm_core.Lowered_ast.branch }
    | Vpush of { cond : Cond.t; vd : Dreg.t; count : int }
        (** [vd] is the reglist's lowest-numbered D register and [count] how many consecutive
            ones follow it - VSTM-class encodings have no bitmask, only a base and a count
            ({!Opcode.Vpush}'s own doc explains why a single register needs no separate case
            the way {!Push}'s does). [imm8 = count * 2]: each doubleword register is two
            32-bit transfers. *)
    | Nop of { cond : Cond.t }
        (** the dedicated ARMv7 hint-NOP word, [e320f000] for [cond = Al] - the same bytes
            {!nop_bytes} already pads with, now reachable from a source-level [nop] mnemonic too. *)
    | Shift_reg of { cond : Cond.t; kind : int; rd : Reg.t; rm : Reg.t; rs : Reg.t }
        (** [lsl]/[lsr]/[asr]/[ror rd, rm, rs] - {!Opcode.Shift}'s register-shift-amount form, a
            [mov]-family word (dp = 13, [s] = 0, [rn] = 0 always: the only evidenced shape) with a
            genuinely different bit 4/11:8 layout from {!Dp_reg}'s immediate-shift one, so it is its
            own constructor rather than a variant field on {!Dp_reg}. *)

  let pp ppf = function
    | Dp_imm { cond; dp; rd; rn; imm; _ } -> (
        let c = Cond.suffix cond in
        match Opcode.of_dp dp with
        | Some ((Opcode.Mov | Opcode.Mvn) as o) ->
            Fmt.pf ppf "%s%s %a, #%Ld" (Opcode.name o) c Reg.pp rd imm
        (* A compare writes no destination, so printing [Rd] would print the
           zero the encoding requires as though it were an operand. *)
        | Some o when Opcode.is_compare o ->
            Fmt.pf ppf "%s%s %a, #%Ld" (Opcode.name o) c Reg.pp rn imm
        | Some o -> Fmt.pf ppf "%s%s %a, %a, #%Ld" (Opcode.name o) c Reg.pp rd Reg.pp rn imm
        | None -> Fmt.pf ppf "dp%d%s %a, %a, #%Ld" dp c Reg.pp rd Reg.pp rn imm)
    | Dp_reg { cond; dp; s; rd; rn; rm; sh_kind; sh_amt } -> (
        let sh =
          if sh_amt = 0 && sh_kind = 0 then ""
          else Printf.sprintf ", %s #%d" (shift_name sh_kind) sh_amt
        in
        let c = Cond.suffix cond in
        match Opcode.of_dp dp with
        | Some ((Opcode.Mov | Opcode.Mvn) as o) ->
            Fmt.pf ppf "%s%s %a, %a%s" (Opcode.name o) c Reg.pp rd Reg.pp rm sh
        | Some o when Opcode.is_compare o ->
            Fmt.pf ppf "%s%s %a, %a%s" (Opcode.name o) c Reg.pp rn Reg.pp rm sh
        (* [adds] - {!Add}'s own flag-setting form. [dp] alone cannot distinguish it from plain
           [add]; only [s] can, exactly as {!Opcode.Adds}'s own doc explains. *)
        | Some Opcode.Add when s ->
            Fmt.pf ppf "adds%s %a, %a, %a%s" c Reg.pp rd Reg.pp rn Reg.pp rm sh
        | Some o ->
            Fmt.pf ppf "%s%s %a, %a, %a%s" (Opcode.name o) c Reg.pp rd Reg.pp rn Reg.pp rm sh
        | None -> Fmt.pf ppf "dp%d%s %a, %a, %a%s" dp c Reg.pp rd Reg.pp rn Reg.pp rm sh)
    | Nop { cond } -> Fmt.pf ppf "nop%s" (Cond.suffix cond)
    | Shift_reg { cond; kind; rd; rm; rs } ->
        Fmt.pf ppf "%s%s %a, %a, %a" (shift_name kind) (Cond.suffix cond) Reg.pp rd Reg.pp rm Reg.pp
          rs
    | Ldst_imm { cond; load; byte; rt; rn; offset; writeback } ->
        Fmt.pf ppf "%s%s%s %a, [%a, #%Ld]%s"
          (if load then "ldr" else "str")
          (if byte then "b" else "")
          (Cond.suffix cond) Reg.pp rt Reg.pp rn offset
          (if writeback then "!" else "")
    | Ldst_reg { cond; load; byte; rt; rn; rm; negate; sh_kind; sh_amt } ->
        let sh =
          if sh_amt = 0 && sh_kind = 0 then ""
          else Printf.sprintf ", %s #%d" (shift_name sh_kind) sh_amt
        in
        Fmt.pf ppf "%s%s%s %a, [%a, %s%a%s]"
          (if load then "ldr" else "str")
          (if byte then "b" else "")
          (Cond.suffix cond) Reg.pp rt Reg.pp rn
          (if negate then "-" else "")
          Reg.pp rm sh
    | Bx { cond; rm } -> Fmt.pf ppf "bx%s %a" (Cond.suffix cond) Reg.pp rm
    | Push { cond; regs } ->
        let members =
          List.filter_map
            (fun n -> if regs land (1 lsl n) <> 0 then Some (Reg.of_num n) else None)
            (List.init 16 Fun.id)
        in
        Fmt.pf ppf "push%s {%a}" (Cond.suffix cond) Fmt.(list ~sep:(any ", ") Reg.pp) members
    | Ldm { cond; load; rn; regs } ->
        let members =
          List.filter_map
            (fun n -> if regs land (1 lsl n) <> 0 then Some (Reg.of_num n) else None)
            (List.init 16 Fun.id)
        in
        if load && Reg.equal rn Reg.sp && List.length members >= 2 then
          Fmt.pf ppf "pop%s {%a}" (Cond.suffix cond) Fmt.(list ~sep:(any ", ") Reg.pp) members
        else
          Fmt.pf ppf "%s%s %a!, {%a}"
            (if load then "ldmia" else "stmia")
            (Cond.suffix cond) Reg.pp rn
            Fmt.(list ~sep:(any ", ") Reg.pp)
            members
    | Udf { imm16 } -> Fmt.pf ppf "udf #%Ld" imm16
    | Bl { cond; target } ->
        Fmt.pf ppf "bl%s %a" (Cond.suffix cond) Asm_core.Lowered_ast.pp_branch target
    | B { cond; target } ->
        Fmt.pf ppf "b%s %a" (Cond.suffix cond) Asm_core.Lowered_ast.pp_branch target
    | Mul { cond; rd; rn; rm; _ } ->
        Fmt.pf ppf "mul%s %a, %a, %a" (Cond.suffix cond) Reg.pp rd Reg.pp rn Reg.pp rm
    | Mla { cond; rd; rn; rm; ra; _ } ->
        Fmt.pf ppf "mla%s %a, %a, %a, %a" (Cond.suffix cond) Reg.pp rd Reg.pp rn Reg.pp rm Reg.pp ra
    | Movw_movt { cond; top; rd; imm } ->
        Fmt.pf ppf "%s%s %a, #%a"
          (if top then "movt" else "movw")
          (Cond.suffix cond) Reg.pp rd Asm_core.Lowered_ast.pp_branch imm
    | Vmov_reg_d { cond; vd; vm } ->
        Fmt.pf ppf "vmov%s.f64 %a, %a" (Cond.suffix cond) Dreg.pp vd Dreg.pp vm
    | Vmov_reg_s { cond; vd; vm } ->
        Fmt.pf ppf "vmov%s.f32 %a, %a" (Cond.suffix cond) Sreg.pp vd Sreg.pp vm
    | Vmov_imm_d { cond; vd; imm8 } ->
        Fmt.pf ppf "vmov%s.f64 %a, #%s" (Cond.suffix cond) Dreg.pp vd
          (decimal_of_float (vfp_expand imm8))
    | Vmov_imm_s { cond; vd; imm8 } ->
        Fmt.pf ppf "vmov%s.f32 %a, #%s" (Cond.suffix cond) Sreg.pp vd
          (decimal_of_float (vfp_expand imm8))
    | Vmov_to_single { cond; sn; rt } ->
        Fmt.pf ppf "vmov%s %a, %a" (Cond.suffix cond) Sreg.pp sn Reg.pp rt
    | Vmov_from_single { cond; rt; sn } ->
        Fmt.pf ppf "vmov%s %a, %a" (Cond.suffix cond) Reg.pp rt Sreg.pp sn
    | Vmov_to_double { cond; dm; rt; rt2 } ->
        Fmt.pf ppf "vmov%s %a, %a, %a" (Cond.suffix cond) Dreg.pp dm Reg.pp rt Reg.pp rt2
    | Vmov_from_double { cond; rt; rt2; dm } ->
        Fmt.pf ppf "vmov%s %a, %a, %a" (Cond.suffix cond) Reg.pp rt Reg.pp rt2 Dreg.pp dm
    | V3_d { cond; op; vd; vn; vm } ->
        Fmt.pf ppf "%s%s.f64 %a, %a, %a" (V3.name op) (Cond.suffix cond) Dreg.pp vd Dreg.pp vn
          Dreg.pp vm
    | V3_s { cond; op; vd; vn; vm } ->
        Fmt.pf ppf "%s%s.f32 %a, %a, %a" (V3.name op) (Cond.suffix cond) Sreg.pp vd Sreg.pp vn
          Sreg.pp vm
    | Vneg_d { cond; vd; vm } ->
        Fmt.pf ppf "vneg%s.f64 %a, %a" (Cond.suffix cond) Dreg.pp vd Dreg.pp vm
    | Vneg_s { cond; vd; vm } ->
        Fmt.pf ppf "vneg%s.f32 %a, %a" (Cond.suffix cond) Sreg.pp vd Sreg.pp vm
    | Vcmp_reg_d { cond; vd; vm } ->
        Fmt.pf ppf "vcmp%s.f64 %a, %a" (Cond.suffix cond) Dreg.pp vd Dreg.pp vm
    | Vcmp_reg_s { cond; vd; vm } ->
        Fmt.pf ppf "vcmp%s.f32 %a, %a" (Cond.suffix cond) Sreg.pp vd Sreg.pp vm
    | Vcmp_zero_d { cond; vd } -> Fmt.pf ppf "vcmp%s.f64 %a, #0" (Cond.suffix cond) Dreg.pp vd
    | Vcmp_zero_s { cond; vd } -> Fmt.pf ppf "vcmp%s.f32 %a, #0" (Cond.suffix cond) Sreg.pp vd
    | Vmrs { cond } -> Fmt.pf ppf "vmrs%s APSR_nzcv, FPSCR" (Cond.suffix cond)
    | Vcvt_f32_f64 { cond; sd; dm } ->
        Fmt.pf ppf "vcvt%s.f32.f64 %a, %a" (Cond.suffix cond) Sreg.pp sd Dreg.pp dm
    | Vcvt_f64_f32 { cond; dd; sm } ->
        Fmt.pf ppf "vcvt%s.f64.f32 %a, %a" (Cond.suffix cond) Dreg.pp dd Sreg.pp sm
    | Vcvt_f32_s32 { cond; sd; sm } ->
        Fmt.pf ppf "vcvt%s.f32.s32 %a, %a" (Cond.suffix cond) Sreg.pp sd Sreg.pp sm
    | Vcvt_f64_s32 { cond; dd; sm } ->
        Fmt.pf ppf "vcvt%s.f64.s32 %a, %a" (Cond.suffix cond) Dreg.pp dd Sreg.pp sm
    | Vcvt_f64_u32 { cond; dd; sm } ->
        Fmt.pf ppf "vcvt%s.f64.u32 %a, %a" (Cond.suffix cond) Dreg.pp dd Sreg.pp sm
    | Vcvt_s32_f64 { cond; sd; dm } ->
        Fmt.pf ppf "vcvt%s.s32.f64 %a, %a" (Cond.suffix cond) Sreg.pp sd Dreg.pp dm
    | Vmem_d { cond; load; vd; rn; offset } ->
        Fmt.pf ppf "%s%s %a, [%a, #%Ld]"
          (if load then "vldr" else "vstr")
          (Cond.suffix cond) Dreg.pp vd Reg.pp rn offset
    | Vmem_s { cond; load; vd; rn; offset } ->
        Fmt.pf ppf "%s%s %a, [%a, #%Ld]"
          (if load then "vldr" else "vstr")
          (Cond.suffix cond) Sreg.pp vd Reg.pp rn offset
    | Vldr_lit_d { cond; vd; target } ->
        Fmt.pf ppf "vldr%s %a, %a" (Cond.suffix cond) Dreg.pp vd Asm_core.Lowered_ast.pp_branch
          target
    | Vldr_lit_s { cond; vd; target } ->
        Fmt.pf ppf "vldr%s %a, %a" (Cond.suffix cond) Sreg.pp vd Asm_core.Lowered_ast.pp_branch
          target
    | Vpush { cond; vd; count } ->
        let members = List.init count (fun i -> Dreg.of_num (vd.Dreg.num + i)) in
        Fmt.pf ppf "vpush%s.64 {%a}" (Cond.suffix cond) Fmt.(list ~sep:(any ", ") Dreg.pp) members

  let equal a b =
    match (a, b) with
    | Dp_imm x, Dp_imm y ->
        Cond.equal x.cond y.cond && x.dp = y.dp && x.s = y.s && Reg.equal x.rd y.rd
        && Reg.equal x.rn y.rn && Int64.equal x.imm y.imm
    | Dp_reg x, Dp_reg y ->
        Cond.equal x.cond y.cond && x.dp = y.dp && x.s = y.s && Reg.equal x.rd y.rd
        && Reg.equal x.rn y.rn && Reg.equal x.rm y.rm && x.sh_kind = y.sh_kind
        && x.sh_amt = y.sh_amt
    | Ldst_imm x, Ldst_imm y ->
        Cond.equal x.cond y.cond && x.load = y.load && x.byte = y.byte && Reg.equal x.rt y.rt
        && Reg.equal x.rn y.rn && Int64.equal x.offset y.offset && x.writeback = y.writeback
    | Ldst_reg x, Ldst_reg y ->
        Cond.equal x.cond y.cond && x.load = y.load && x.byte = y.byte && Reg.equal x.rt y.rt
        && Reg.equal x.rn y.rn && Reg.equal x.rm y.rm && x.negate = y.negate
        && x.sh_kind = y.sh_kind && x.sh_amt = y.sh_amt
    | Bx x, Bx y -> Cond.equal x.cond y.cond && Reg.equal x.rm y.rm
    | Push x, Push y -> Cond.equal x.cond y.cond && x.regs = y.regs
    | Ldm x, Ldm y ->
        Cond.equal x.cond y.cond && x.load = y.load && Reg.equal x.rn y.rn && x.regs = y.regs
    | Udf x, Udf y -> Int64.equal x.imm16 y.imm16
    | Bl x, Bl y -> Cond.equal x.cond y.cond && Asm_core.Lowered_ast.equal_branch x.target y.target
    | B x, B y -> Cond.equal x.cond y.cond && Asm_core.Lowered_ast.equal_branch x.target y.target
    | Mul x, Mul y ->
        Cond.equal x.cond y.cond && x.s = y.s && Reg.equal x.rd y.rd && Reg.equal x.rn y.rn
        && Reg.equal x.rm y.rm
    | Mla x, Mla y ->
        Cond.equal x.cond y.cond && x.s = y.s && Reg.equal x.rd y.rd && Reg.equal x.rn y.rn
        && Reg.equal x.rm y.rm && Reg.equal x.ra y.ra
    | Movw_movt x, Movw_movt y ->
        Cond.equal x.cond y.cond && x.top = y.top && Reg.equal x.rd y.rd
        && Asm_core.Lowered_ast.equal_branch x.imm y.imm
    | Vmov_reg_d x, Vmov_reg_d y ->
        Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd && Dreg.equal x.vm y.vm
    | Vmov_reg_s x, Vmov_reg_s y ->
        Cond.equal x.cond y.cond && Sreg.equal x.vd y.vd && Sreg.equal x.vm y.vm
    | Vmov_imm_d x, Vmov_imm_d y ->
        Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd && x.imm8 = y.imm8
    | Vmov_imm_s x, Vmov_imm_s y ->
        Cond.equal x.cond y.cond && Sreg.equal x.vd y.vd && x.imm8 = y.imm8
    | Vmov_to_single x, Vmov_to_single y ->
        Cond.equal x.cond y.cond && Sreg.equal x.sn y.sn && Reg.equal x.rt y.rt
    | Vmov_from_single x, Vmov_from_single y ->
        Cond.equal x.cond y.cond && Reg.equal x.rt y.rt && Sreg.equal x.sn y.sn
    | Vmov_to_double x, Vmov_to_double y ->
        Cond.equal x.cond y.cond && Dreg.equal x.dm y.dm && Reg.equal x.rt y.rt
        && Reg.equal x.rt2 y.rt2
    | Vmov_from_double x, Vmov_from_double y ->
        Cond.equal x.cond y.cond && Reg.equal x.rt y.rt && Reg.equal x.rt2 y.rt2
        && Dreg.equal x.dm y.dm
    | V3_d x, V3_d y ->
        Cond.equal x.cond y.cond && x.op = y.op && Dreg.equal x.vd y.vd && Dreg.equal x.vn y.vn
        && Dreg.equal x.vm y.vm
    | V3_s x, V3_s y ->
        Cond.equal x.cond y.cond && x.op = y.op && Sreg.equal x.vd y.vd && Sreg.equal x.vn y.vn
        && Sreg.equal x.vm y.vm
    | Vneg_d x, Vneg_d y -> Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd && Dreg.equal x.vm y.vm
    | Vneg_s x, Vneg_s y -> Cond.equal x.cond y.cond && Sreg.equal x.vd y.vd && Sreg.equal x.vm y.vm
    | Vcmp_reg_d x, Vcmp_reg_d y ->
        Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd && Dreg.equal x.vm y.vm
    | Vcmp_reg_s x, Vcmp_reg_s y ->
        Cond.equal x.cond y.cond && Sreg.equal x.vd y.vd && Sreg.equal x.vm y.vm
    | Vcmp_zero_d x, Vcmp_zero_d y -> Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd
    | Vcmp_zero_s x, Vcmp_zero_s y -> Cond.equal x.cond y.cond && Sreg.equal x.vd y.vd
    | Vmrs x, Vmrs y -> Cond.equal x.cond y.cond
    | Vcvt_f32_f64 x, Vcvt_f32_f64 y ->
        Cond.equal x.cond y.cond && Sreg.equal x.sd y.sd && Dreg.equal x.dm y.dm
    | Vcvt_f64_f32 x, Vcvt_f64_f32 y ->
        Cond.equal x.cond y.cond && Dreg.equal x.dd y.dd && Sreg.equal x.sm y.sm
    | Vcvt_f32_s32 x, Vcvt_f32_s32 y ->
        Cond.equal x.cond y.cond && Sreg.equal x.sd y.sd && Sreg.equal x.sm y.sm
    | Vcvt_f64_s32 x, Vcvt_f64_s32 y ->
        Cond.equal x.cond y.cond && Dreg.equal x.dd y.dd && Sreg.equal x.sm y.sm
    | Vcvt_f64_u32 x, Vcvt_f64_u32 y ->
        Cond.equal x.cond y.cond && Dreg.equal x.dd y.dd && Sreg.equal x.sm y.sm
    | Vcvt_s32_f64 x, Vcvt_s32_f64 y ->
        Cond.equal x.cond y.cond && Sreg.equal x.sd y.sd && Dreg.equal x.dm y.dm
    | Vmem_d x, Vmem_d y ->
        Cond.equal x.cond y.cond && x.load = y.load && Dreg.equal x.vd y.vd && Reg.equal x.rn y.rn
        && Int64.equal x.offset y.offset
    | Vmem_s x, Vmem_s y ->
        Cond.equal x.cond y.cond && x.load = y.load && Sreg.equal x.vd y.vd && Reg.equal x.rn y.rn
        && Int64.equal x.offset y.offset
    | Vldr_lit_d x, Vldr_lit_d y ->
        Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd
        && Asm_core.Lowered_ast.equal_branch x.target y.target
    | Vldr_lit_s x, Vldr_lit_s y ->
        Cond.equal x.cond y.cond && Sreg.equal x.vd y.vd
        && Asm_core.Lowered_ast.equal_branch x.target y.target
    | Vpush x, Vpush y -> Cond.equal x.cond y.cond && Dreg.equal x.vd y.vd && x.count = y.count
    | Nop x, Nop y -> Cond.equal x.cond y.cond
    | Shift_reg x, Shift_reg y ->
        Cond.equal x.cond y.cond && x.kind = y.kind && Reg.equal x.rd y.rd && Reg.equal x.rm y.rm
        && Reg.equal x.rs y.rs
    | _ -> false
end

(* Measured on the M2 fixtures: a same-file call is R_ARM_CALL, a global address
   arrives as the R_ARM_MOVW_ABS_NC / R_ARM_MOVT_ABS pair - one relocation per
   instruction, each patching a field split across two non-adjacent runs - and a
   branch to a local label keeps no record at all. A32 is fixed-width, so there
   is no ladder and no short rung. *)
type fixup_kind = Abs32 | Pcrel_b26 | Pcrel_call | Movw_abs_nc | Movt_abs | Pcrel_vldr8

let fixup_kind_name = function
  | Abs32 -> "abs32"
  | Pcrel_b26 -> "pcrel-b26"
  | Pcrel_call -> "pcrel-call"
  | Movw_abs_nc -> "movw-abs-nc"
  | Movt_abs -> "movt-abs"
  | Pcrel_vldr8 -> "pcrel-vldr8"

let equal_fixup_kind a b = a = b

let fixup_family = function
  | Abs32 -> "abs"
  | Pcrel_b26 -> "pcrel-branch"
  | Pcrel_call -> "pcrel-call"
  | Movw_abs_nc -> "abs-lo16"
  | Movt_abs -> "abs-hi16"
  | Pcrel_vldr8 -> "pcrel-vldr-literal"

let fixup_role = function
  | Abs32 | Movw_abs_nc | Movt_abs | Pcrel_vldr8 -> Asm_core.Lowered_ast.Data_address
  | Pcrel_b26 -> Asm_core.Lowered_ast.Branch
  | Pcrel_call -> Asm_core.Lowered_ast.Call

type feature = No_features
type target_state = { syntax : string; arch : string; fpu : string; thumb : bool }

let default_state = { syntax = "divided"; arch = ""; fpu = ""; thumb = false }
let default_features = []

(* {1 The modified-immediate relation (§ M1.3)}

   A32's immediate field is 4 bits of rotation and 8 bits of value, denoting
   [ror32(imm8, 2 * rot)]. This is the project's motivating [Iso_fun]: the
   relation is neither injective (0x2a has two representations) nor total
   (0x12345678 has none), so it is not an [Iso_table] and no table of 2^32
   entries would make it one. What proves it correct is the exhaustive
   round-trip over all 4096 (rot, imm8) pairs in the test suite.

   Canonical selection is *the smallest rotation*, which is what GNU as picks
   and what the three committed cases pin: [#42] -> rot 0, [#0x3f000000] ->
   rot 4, [#0x100] -> rot 12. Choosing "the first representation found" without
   fixing the scan direction would be a coin flip that happened to agree on the
   one value in the fixture. *)

let rotr32 v n =
  let v = Int64.logand v 0xFFFFFFFFL in
  let n = n land 31 in
  if n = 0 then v
  else
    Int64.logand
      (Int64.logor (Int64.shift_right_logical v n) (Int64.shift_left v (32 - n)))
      0xFFFFFFFFL

let rotl32 v n = rotr32 v (32 - (n land 31))

let encode_modimm v =
  let v = Int64.logand v 0xFFFFFFFFL in
  let rec go rot =
    if rot > 15 then None
    else
      let cand = rotl32 v (2 * rot) in
      if Int64.compare cand 256L < 0 then Some (rot, Int64.to_int cand) else go (rot + 1)
  in
  go 0

let decode_modimm ~rot ~imm8 = rotr32 (Int64.of_int imm8) (2 * rot)

(* Every (rot, imm8) pair. 4096 is small enough to enumerate outright, which is
   the whole justification the domain has to record. *)
let modimm_domain =
  C.Finite.exhaustive ~name:"arm.modimm"
    ~why:
      "the field is 4 bits of rotation and 8 bits of value, so the domain is exactly 4096 pairs \
       and enumerating it is a table scan rather than a sample"
    (List.concat_map
       (fun rot -> List.init 256 (fun imm8 -> decode_modimm ~rot ~imm8))
       (List.init 16 (fun r -> r)))

let modimm_codec : (int64, fixup_kind) C.t =
  C.iso_fun ~name:"modimm"
    ~encode:(fun v ->
      match encode_modimm v with
      | None -> None
      | Some (rot, imm8) -> Some (Int64.of_int rot, Int64.of_int imm8))
    ~decode:(fun (rot, imm8) ->
      Some (decode_modimm ~rot:(Int64.to_int rot) ~imm8:(Int64.to_int imm8)))
    C.(field ~width:4 "rot" ** field ~width:8 "imm8")

(* {1 The codec}

   Every M1 instruction is unconditional. The condition field is therefore a
   constant rather than a field: making it a field would mean the codec accepted
   conditional forms that lowering cannot produce and that no test covers, and
   [Finite.unreachable_alts] would not catch it because the condition is not an
   alternative. Conditional execution arrives with M2, as a field and a
   suffix-parsing rule together. *)

(* The condition is a real field now, on every form. M1 encoded it as the
   constant [al] and said in this comment that conditional execution would
   arrive with M2; it has.

   A declared finite relation rather than a bare four-bit field, so [check]
   decides that all sixteen codes decode and that no two share one. *)
let cond_codec =
  C.iso_table ~name:"cond" ~equal:Cond.equal ~show:Cond.name
    ~entries:(List.map (fun c -> (c, Int64.of_int (Cond.code c))) Cond.all)
    (C.field ~width:4 "cond")

let reg_field name =
  C.iso_fun ~name
    ~encode:(fun (r : Reg.t) -> Some (Int64.of_int r.num))
    ~decode:(fun v -> Some (Reg.of_num (Int64.to_int v)))
    (C.field ~width:4 name)

(* {1 VFP register field splitting}

   Every VFP encoding splits a D/S register's number across a 1-bit
   "extension" field (D/N/M in the ARM ARM's own names, depending which
   operand slot it belongs to) and an adjacent 4-bit field - never the same
   two word positions twice, so the split itself, not its placement, is what
   belongs here. [Dreg.hilo]/[Sreg.hilo] already compute the two components;
   what differs between the register classes is *which* component goes in
   the 1-bit slot and which in the 4-bit slot - {!Dreg.hilo}'s own doc
   comment spells out the inversion. These wrappers normalize both classes
   to one shape, [(ext, field4)], so every VFP [Alt] below can place the two
   without having to remember which class inverts. *)
let dreg_parts (r : Dreg.t) : int64 * int64 = Dreg.hilo r
let dreg_unparts ext field4 = Dreg.of_hilo ext field4

let sreg_parts (r : Sreg.t) : int64 * int64 =
  let field4, ext = Sreg.hilo r in
  (ext, field4)

let sreg_unparts ext field4 = Sreg.of_hilo field4 ext

(* [movw]/[movt]. The 16-bit immediate is split across two non-adjacent fields -
   [imm4] at bits 19:16 and [imm12] at bits 11:0 - which is exactly the case
   §B2 of the contract exists for: two [fixup] nodes carrying the same name and
   kind, each tagged with the bit of the *logical* value it starts at, merged by
   [encode] into one placement with two slices. The split lives here, at the
   [iso_fun] boundary, so the sequential bit walk never has to interleave one
   node across two positions.

   [top] selects which half of the address the field holds, and with it the
   relocation kind, so it is a parameter of the form rather than a field in it:
   [Movw_abs_nc] and [Movt_abs] are different relocations, not one relocation
   with a bit. *)
let movw_alt ~top =
  let kind = if top then Movt_abs else Movw_abs_nc in
  let name = if top then "movt" else "movw" in
  let op = if top then 0b00110100L else 0b00110000L in
  C.iso_fun ~name
    ~encode:(function
      | Lowered.Movw_movt { cond; top = t; rd; imm } when t = top ->
          let v =
            match imm with
            | Asm_core.Lowered_ast.Symbolic _ -> 0L
            | Asm_core.Lowered_ast.Resolved { value; _ } -> value
          in
          Some
            ( cond,
              ((), (Int64.logand (Int64.shift_right_logical v 12) 0xFL, (rd, Int64.logand v 0xFFFL)))
            )
      | _ -> None)
    ~decode:(fun (cond, ((), (imm4, (rd, imm12)))) ->
      let value = Int64.logor (Int64.shift_left imm4 12) imm12 in
      Some
        (Lowered.Movw_movt
           { cond; top; rd; imm = Asm_core.Lowered_ast.Resolved { value; rung = name } }))
    C.(
      cond_codec ** const ~width:8 op
      ** fixup ~width:4 ~value_lsb:12 ~kind "imm"
      ** reg_field "rd"
      ** fixup ~width:12 ~value_lsb:0 ~kind "imm")

let codec : (Lowered.t, fixup_kind) C.t =
  C.choice ~name:"arm"
    [
      (* [bx] first. Its pattern [0001 0010 1111 1111 1111 0001 Rm] reads like a
         specialization of the data-processing register format, and putting it
         first is what keeps a [bx] from decoding as a plausible [teq]. It is
         only a near miss, though: [dp-reg] fixes bit 4 to zero and [bx] sets
         it, so [check] reports no overlap between the two. *)
      C.alt ~label:"bx" ~priority:0
        (C.iso_fun ~name:"bx"
           ~encode:(function Lowered.Bx { cond; rm } -> Some (cond, ((), rm)) | _ -> None)
           ~decode:(fun (cond, ((), rm)) -> Some (Lowered.Bx { cond; rm }))
           C.(cond_codec ** const ~width:24 0x12FFF1L ** reg_field "rm"));
      (* [push {reglist}], GNU's [stmdb sp!, {reglist}] alias (M5,
         asm/docs/corpus.md). [100 1 0 0 1 0] is STM's P=1/U=0/S=0/W=1/L=0
         (pre-indexed, decrement, no S-bit, writeback, store) - verified
         against real [arm-linux-gnueabihf-as]/[objdump]:
         [e92d000f] for [push {r0, r1, r2, r3}]. *)
      C.alt ~label:"push" ~priority:37
        (C.iso_fun ~name:"push"
           ~encode:(function
             | Lowered.Push { cond; regs } -> Some (cond, ((), Int64.of_int regs)) | _ -> None)
           ~decode:(fun (cond, ((), regs)) ->
             Some (Lowered.Push { cond; regs = Int64.to_int regs }))
           C.(cond_codec ** const ~width:12 0b100100101101L ** field ~width:16 "reglist"));
      (* [stmia]/[ldmia Rn!, {reglist}], and [pop]'s own alias for the load
         direction with [Rn = sp] (M5, asm/docs/corpus.md). Increment-after
         STM/LDM: [100 0 1 0 1] is P=0/U=1/S=0/W=1 (post-indexed, increment,
         no S-bit, writeback forced - the only evidenced shape), leaving L
         (store/load) and Rn variable, unlike [push] above which bakes both
         P/U *and* Rn=sp into its fixed field. Verified against real
         arm-linux-gnueabihf-as/objdump: [e8a30003] for [stmia r3!, {r0,
         r1}], [e8b30003] for [ldmia r3!, {r0, r1}], and [e8bd0003] for
         [pop {r0, r1}] (identical to [ldmia sp!, {r0, r1}] - {!Opcode.Pop}
         is a pure printing alias, not a separate bit pattern). *)
      C.alt ~label:"ldm" ~priority:38
        (C.iso_fun ~name:"ldm"
           ~encode:(function
             | Lowered.Ldm { cond; load; rn; regs } ->
                 Some (cond, ((), ((if load then 1L else 0L), (rn, Int64.of_int regs))))
             | _ -> None)
           ~decode:(fun (cond, ((), (l, (rn, regs)))) ->
             Some (Lowered.Ldm { cond; load = Int64.equal l 1L; rn; regs = Int64.to_int regs }))
           C.(
             cond_codec ** const ~width:7 0b1000101L ** field ~width:1 "l" ** reg_field "rn"
             ** field ~width:16 "reglist"));
      (* [vpush.64 {dN, ...}], GNU's [vstmdb sp!, {dN, ...}] alias (M5, asm/docs/corpus.md) -
         the D-register cousin of [push] above, but VSTM-class rather than STMDB-class: no
         bitmask, only a base register and a count. [1101 0] is P=1/U=0 (pre-indexed,
         decrement), [D] is the base D-register's extension bit, [10] is W=1/L=0 (writeback,
         store), [1101] fixes Rn=sp, and [1011] is VSTM's own coprocessor tag ([101]) with
         the doubleword size bit ([1]) set - a single D register needs no separate
         one-register alias the way GPR [push] does, since this encoding already covers it
         (imm8 = 2). Verified against real arm-linux-gnueabihf-as/objdump (-march=armv7-a
         -mfpu=vfpv3): [ed2d8b04] for [vpush.64 {d8, d9}] (2 registers), [ed2d8b06] for
         [vpush.64 {d8-d10}] (3), [ed2d8b02] for [vpush.64 {d8}] (1), and [ed6d0b04] for
         [vpush.64 {d16-d17}] (base >= D16, exercising the D:Vd split's high half). *)
      C.alt ~label:"vpush" ~priority:39
        (C.iso_fun ~name:"vpush"
           ~encode:(function
             | Lowered.Vpush { cond; vd; count } ->
                 let d, vdf = dreg_parts vd in
                 let imm8 = Int64.of_int (count * 2) in
                 if Int64.compare imm8 256L >= 0 then None
                 else Some (cond, ((), (d, ((), ((), (vdf, ((), imm8)))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), imm8))))))) ->
             Some (Lowered.Vpush { cond; vd = dreg_unparts d vdf; count = Int64.to_int imm8 / 2 }))
           C.(
             cond_codec ** const ~width:5 0b11010L ** field ~width:1 "D" ** const ~width:2 0b10L
             ** const ~width:4 0b1101L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** field ~width:8 "imm8"));
      (* [movw]/[movt] must be tried *before* [dp-imm], not after. Their
         encodings sit inside the data-processing immediate space - [movw] is
         [dp = 8, S = 0] and [movt] is [dp = 10, S = 0], the [tst] and [cmp]
         slots with the flag-setting bit clear - so a [movw] word decodes
         perfectly well as a nonsensical [tst] if [dp-imm] is asked first.
         Lowering can never *produce* those two combinations (a comparison
         always sets S, and there is no [tst]), so the overlap costs nothing on
         the encode side; it is only decode that has to be told which reading is
         the real one. [check] reports both pairs, naming the priorities that
         decide them - the report is the record that this was chosen rather than
         missed. *)
      (* [nop] - the dedicated ARMv7 hint-NOP encoding, [e320f000] for [cond = Al] (the same
         bytes {!nop_bytes} already pads with). Its [dp] reading is 9 (the [teq] slot, S clear -
         unmapped, since {!Opcode.of_dp} names no [Teq]), but [dp-imm]'s own fields are all
         variable, not fixed, so it would still decode - wrongly, as a nonsensical [dp9] word -
         unless tried first, the identical overlap [movw]/[movt] already document just below. *)
      C.alt ~label:"nop" ~priority:(-1)
        (C.iso_fun ~name:"nop"
           ~encode:(function Lowered.Nop { cond } -> Some (cond, ()) | _ -> None)
           ~decode:(fun (cond, ()) -> Some (Lowered.Nop { cond }))
           C.(cond_codec ** const ~width:28 0x320F000L));
      C.alt ~label:"movw" ~priority:1 (movw_alt ~top:false);
      C.alt ~label:"movt" ~priority:2 (movw_alt ~top:true);
      C.alt ~label:"dp-imm" ~priority:3
        (C.iso_fun ~name:"dp-imm"
           ~encode:(function
             | Lowered.Dp_imm { cond; dp; s; rd; rn; imm } ->
                 Some (cond, ((), (Int64.of_int dp, ((if s then 1L else 0L), (rn, (rd, imm))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (dp, (s, (rn, (rd, imm)))))) ->
             Some (Lowered.Dp_imm { cond; dp = Int64.to_int dp; s = Int64.equal s 1L; rd; rn; imm }))
           C.(
             cond_codec ** const ~width:3 1L ** field ~width:4 "dp" ** field ~width:1 "s"
             ** reg_field "rn" ** reg_field "rd" ** modimm_codec));
      C.alt ~label:"dp-reg" ~priority:4
        (C.iso_fun ~name:"dp-reg"
           ~encode:(function
             | Lowered.Dp_reg { cond; dp; s; rd; rn; rm; sh_kind; sh_amt } ->
                 Some
                   ( cond,
                     ( (),
                       ( Int64.of_int dp,
                         ( (if s then 1L else 0L),
                           (rn, (rd, (Int64.of_int sh_amt, (Int64.of_int sh_kind, ((), rm))))) ) )
                     ) )
             | _ -> None)
           ~decode:(fun (cond, ((), (dp, (s, (rn, (rd, (sh_amt, (sh_kind, ((), rm))))))))) ->
             Some
               (Lowered.Dp_reg
                  {
                    cond;
                    dp = Int64.to_int dp;
                    s = Int64.equal s 1L;
                    rd;
                    rn;
                    rm;
                    sh_kind = Int64.to_int sh_kind;
                    sh_amt = Int64.to_int sh_amt;
                  }))
           C.(
             cond_codec ** const ~width:3 0L ** field ~width:4 "dp" ** field ~width:1 "s"
             ** reg_field "rn" ** reg_field "rd" ** field ~width:5 "shift-amount"
             ** field ~width:2 "shift-kind" ** const ~width:1 0L ** reg_field "rm"));
      (* Load and store, immediate offset. [U] is the sign of the offset and the
         12-bit field is its magnitude - A32 has no negative displacement field,
         so a signed field here would encode -4 as 0xFFC and address the wrong
         word. *)
      C.alt ~label:"ldst-imm" ~priority:5
        (C.iso_fun ~name:"ldst-imm"
           ~encode:(function
             | Lowered.Ldst_imm { cond; load; byte; rt; rn; offset; writeback } ->
                 let up = Int64.compare offset 0L >= 0 in
                 let mag = Int64.abs offset in
                 if Int64.compare mag 4096L >= 0 then None
                 else
                   Some
                     ( cond,
                       ( (),
                         ( (),
                           ( (if up then 1L else 0L),
                             ( (if byte then 1L else 0L),
                               ( (if writeback then 1L else 0L),
                                 ((if load then 1L else 0L), (rn, (rt, mag))) ) ) ) ) ) )
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (up, (byte, (w, (load, (rn, (rt, mag))))))))) ->
             Some
               (Lowered.Ldst_imm
                  {
                    cond;
                    load = Int64.equal load 1L;
                    byte = Int64.equal byte 1L;
                    rt;
                    rn;
                    offset = (if Int64.equal up 1L then mag else Int64.neg mag);
                    writeback = Int64.equal w 1L;
                  }))
           C.(
             cond_codec ** const ~width:3 2L ** const ~width:1 1L
             (* P: pre-indexed - the only addressing this project parses
                (asm/docs/corpus.md's classify-c-gcc: str/ldr writeback), so
                P is still fixed; only W now varies. *)
             ** field ~width:1 "u"
             ** field ~width:1 "b" ** field ~width:1 "w" ** field ~width:1 "l" ** reg_field "rn"
             ** reg_field "rt" ** field ~width:12 "imm12"));
      (* Load and store, register offset: [ldr r2, [r1, r2, lsl #2]] / [ldrb
         r2, [r7, r0]]. Same word shape as ldst-imm one level up, but bit 25
         is set (the [I] bit ARM's own manual uses for this distinction) and
         the 12-bit magnitude is replaced by imm5 + shift-kind + a fixed 0 +
         Rm. [U] carries the index register's sign here exactly as it carries
         the immediate's sign above - there is no separate "negative register"
         field. Verified byte-for-byte against arm-linux-gnueabihf-as/objdump:
         [ldr r2, [r1, r2, lsl #2]] is [e7912102], [ldr r2, [r1, -r2]] is
         [e7112002], [ldr r2, [r1, r2, ror #7]] is [e79123e2]. *)
      C.alt ~label:"ldst-reg" ~priority:6
        (C.iso_fun ~name:"ldst-reg"
           ~encode:(function
             | Lowered.Ldst_reg { cond; load; byte; rt; rn; rm; negate; sh_kind; sh_amt } ->
                 Some
                   ( cond,
                     ( (),
                       ( (),
                         ( (if negate then 0L else 1L),
                           ( (if byte then 1L else 0L),
                             ( (),
                               ( (if load then 1L else 0L),
                                 (rn, (rt, (Int64.of_int sh_amt, (Int64.of_int sh_kind, ((), rm)))))
                               ) ) ) ) ) ) )
             | _ -> None)
           ~decode:(fun
               ( cond,
                 ((), ((), (u, (byte, ((), (load, (rn, (rt, (sh_amt, (sh_kind, ((), rm))))))))))) )
             ->
             Some
               (Lowered.Ldst_reg
                  {
                    cond;
                    load = Int64.equal load 1L;
                    byte = Int64.equal byte 1L;
                    rt;
                    rn;
                    rm;
                    negate = Int64.equal u 0L;
                    sh_kind = Int64.to_int sh_kind;
                    sh_amt = Int64.to_int sh_amt;
                  }))
           C.(
             cond_codec ** const ~width:3 3L
             ** const ~width:1 1L (* P: offset addressing, no writeback *)
             ** field ~width:1 "u" ** field ~width:1 "b" ** const ~width:1 0L (* W *)
             ** field ~width:1 "l" ** reg_field "rn" ** reg_field "rt"
             ** field ~width:5 "shift-amount" ** field ~width:2 "shift-kind" ** const ~width:1 0L
             ** reg_field "rm"));
      (* [udf], the permanently-undefined encoding. Its fixed top nibble is
         literally [cond_codec]'s bit pattern - not a coincidence to route around,
         but where ARM parked this encoding. *)
      C.alt ~label:"udf" ~priority:7
        (C.iso_fun ~name:"udf"
           ~encode:(function
             | Lowered.Udf { imm16 } ->
                 if Int64.compare imm16 0L < 0 || Int64.compare imm16 65536L >= 0 then None
                 else
                   Some
                     ((), ((), (Int64.shift_right_logical imm16 4, ((), Int64.logand imm16 0xFL))))
             | _ -> None)
           ~decode:(fun ((), ((), (imm12, ((), imm4)))) ->
             Some (Lowered.Udf { imm16 = Int64.logor (Int64.shift_left imm12 4) imm4 }))
           C.(
             (* Not a condition field: UDF is unconditional, and 0xE is simply
                where ARM parked this encoding. Reading it as [al] would let a
                decoder accept fifteen more words that are not [udf]. *)
             const ~width:4 0xEL ** const ~width:8 0b01111111L ** field ~width:12 "imm12"
             ** const ~width:4 0b1111L ** field ~width:4 "imm4"));
      (* [bl] - R_ARM_CALL. The 24-bit field is a word count measured from the
         instruction plus eight, the A32 pipeline offset; both the division and
         the bias live outside this node - the division in [evaluate_fixup] and
         the bias in the fixup, so the evaluator receives a PC rather than
         something it has to correct. *)
      C.alt ~label:"bl" ~priority:8
        (C.iso_fun ~name:"bl"
           ~encode:(function
             | Lowered.Bl { cond; target } -> (
                 match target with
                 | Asm_core.Lowered_ast.Symbolic _ -> Some (cond, ((), 0L))
                 | Asm_core.Lowered_ast.Resolved { value; _ } -> Some (cond, ((), value)))
             | _ -> None)
           ~decode:(fun (cond, ((), d)) ->
             let v = Int64.shift_right (Int64.shift_left d 40) 40 in
             Some
               (Lowered.Bl
                  { cond; target = Asm_core.Lowered_ast.Resolved { value = v; rung = "bl" } }))
           C.(cond_codec ** const ~width:4 0b1011L ** fixup ~width:24 ~kind:Pcrel_call "target"));
      (* [b] - the same word-counted 24-bit field as [bl], one opcode bit apart,
         and the reason A32 needs no relaxation ladder: 24 words of signed reach
         is +/-32 MiB from a single fixed-width form. *)
      C.alt ~label:"b" ~priority:9
        (C.iso_fun ~name:"b"
           ~encode:(function
             | Lowered.B { cond; target } -> (
                 match target with
                 | Asm_core.Lowered_ast.Symbolic _ -> Some (cond, ((), 0L))
                 | Asm_core.Lowered_ast.Resolved { value; _ } -> Some (cond, ((), value)))
             | _ -> None)
           ~decode:(fun (cond, ((), d)) ->
             let v = Int64.shift_right (Int64.shift_left d 40) 40 in
             Some
               (Lowered.B { cond; target = Asm_core.Lowered_ast.Resolved { value = v; rung = "b" } }))
           C.(cond_codec ** const ~width:4 0b1010L ** fixup ~width:24 ~kind:Pcrel_b26 "target"));
      (* [mul] and [mla]. Bits 7:4 are [1001], and the data-processing register
         format requires bit 4 to be zero, so these do not overlap [dp-reg] and
         their position in this list is free. The operand order is the trap:
         [mul Rd, Rn, Rm] puts Rd at 19:16 where [dp-reg] keeps Rn, and Rn at
         3:0 where [dp-reg] keeps Rm. *)
      C.alt ~label:"mul" ~priority:10
        (C.iso_fun ~name:"mul"
           ~encode:(function
             | Lowered.Mul { cond; s; rd; rn; rm } ->
                 Some (cond, ((), ((if s then 1L else 0L), (rd, ((), (rm, ((), rn)))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (s, (rd, ((), (rm, ((), rn))))))) ->
             Some (Lowered.Mul { cond; s = Int64.equal s 1L; rd; rn; rm }))
           C.(
             cond_codec ** const ~width:7 0L ** field ~width:1 "s" ** reg_field "rd"
             ** const ~width:4 0L ** reg_field "rm" ** const ~width:4 0b1001L ** reg_field "rn"));
      C.alt ~label:"mla" ~priority:11
        (C.iso_fun ~name:"mla"
           ~encode:(function
             | Lowered.Mla { cond; s; rd; rn; rm; ra } ->
                 Some (cond, ((), ((if s then 1L else 0L), (rd, (ra, (rm, ((), rn)))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (s, (rd, (ra, (rm, ((), rn))))))) ->
             Some (Lowered.Mla { cond; s = Int64.equal s 1L; rd; rn; rm; ra }))
           C.(
             cond_codec ** const ~width:7 1L ** field ~width:1 "s" ** reg_field "rd"
             ** reg_field "ra" ** reg_field "rm" ** const ~width:4 0b1001L ** reg_field "rn"));
      (* {2 VFP}

         Every form below was measured byte-for-byte against the real,
         installed arm-linux-gnueabihf-as/objdump (vfpv3-d16): the register
         moves, the immediate move (including the negative case, since VFP's
         modified immediate has a sign bit like A32's integer one), the
         three-register arithmetic family, negate, compare, the system
         register transfer, every VCVT direction this corpus needs, and
         VLDR/VSTR. *)
      C.alt ~label:"vmov-reg-d" ~priority:12
        (C.iso_fun ~name:"vmov-reg-d"
           ~encode:(function
             | Lowered.Vmov_reg_d { cond; vd; vm } ->
                 let d, vdf = dreg_parts vd in
                 let m, vmf = dreg_parts vm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vmov_reg_d { cond; vd = dreg_unparts d vdf; vm = dreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:2 0b01L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vmov-reg-s" ~priority:13
        (C.iso_fun ~name:"vmov-reg-s"
           ~encode:(function
             | Lowered.Vmov_reg_s { cond; vd; vm } ->
                 let d, vdf = sreg_parts vd in
                 let m, vmf = sreg_parts vm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vmov_reg_s { cond; vd = sreg_unparts d vdf; vm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0L ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:2 0b01L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vmov-imm-d" ~priority:14
        (C.iso_fun ~name:"vmov-imm-d"
           ~encode:(function
             | Lowered.Vmov_imm_d { cond; vd; imm8 } ->
                 let d, vdf = dreg_parts vd in
                 let imm4h = Int64.of_int (imm8 lsr 4) and imm4l = Int64.of_int (imm8 land 0xF) in
                 Some (cond, ((), (d, ((), (imm4h, (vdf, ((), ((), imm4l))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), (imm4h, (vdf, ((), ((), imm4l)))))))) ->
             let imm8 = (Int64.to_int imm4h lsl 4) lor Int64.to_int imm4l in
             Some (Lowered.Vmov_imm_d { cond; vd = dreg_unparts d vdf; imm8 }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** field ~width:4 "imm4H" ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:4 0L ** field ~width:4 "imm4L"));
      C.alt ~label:"vmov-imm-s" ~priority:15
        (C.iso_fun ~name:"vmov-imm-s"
           ~encode:(function
             | Lowered.Vmov_imm_s { cond; vd; imm8 } ->
                 let d, vdf = sreg_parts vd in
                 let imm4h = Int64.of_int (imm8 lsr 4) and imm4l = Int64.of_int (imm8 land 0xF) in
                 Some (cond, ((), (d, ((), (imm4h, (vdf, ((), ((), imm4l))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), (imm4h, (vdf, ((), ((), imm4l)))))))) ->
             let imm8 = (Int64.to_int imm4h lsl 4) lor Int64.to_int imm4l in
             Some (Lowered.Vmov_imm_s { cond; vd = sreg_unparts d vdf; imm8 }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** field ~width:4 "imm4H" ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:4 0L ** field ~width:4 "imm4L"));
      C.alt ~label:"vmov-to-single" ~priority:16
        (C.iso_fun ~name:"vmov-to-single"
           ~encode:(function
             | Lowered.Vmov_to_single { cond; sn; rt } ->
                 let n, vnf = sreg_parts sn in
                 Some (cond, ((), ((), (vnf, (rt, ((), (n, ())))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (vnf, (rt, ((), (n, ()))))))) ->
             Some (Lowered.Vmov_to_single { cond; sn = sreg_unparts n vnf; rt }))
           C.(
             cond_codec ** const ~width:7 0b1110000L ** const ~width:1 0L ** field ~width:4 "Vn"
             ** reg_field "rt" ** const ~width:4 0b1010L ** field ~width:1 "N"
             ** const ~width:7 0b0010000L));
      C.alt ~label:"vmov-from-single" ~priority:17
        (C.iso_fun ~name:"vmov-from-single"
           ~encode:(function
             | Lowered.Vmov_from_single { cond; rt; sn } ->
                 let n, vnf = sreg_parts sn in
                 Some (cond, ((), ((), (vnf, (rt, ((), (n, ())))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (vnf, (rt, ((), (n, ()))))))) ->
             Some (Lowered.Vmov_from_single { cond; rt; sn = sreg_unparts n vnf }))
           C.(
             cond_codec ** const ~width:7 0b1110000L ** const ~width:1 1L ** field ~width:4 "Vn"
             ** reg_field "rt" ** const ~width:4 0b1010L ** field ~width:1 "N"
             ** const ~width:7 0b0010000L));
      C.alt ~label:"vmov-to-double" ~priority:18
        (C.iso_fun ~name:"vmov-to-double"
           ~encode:(function
             | Lowered.Vmov_to_double { cond; dm; rt; rt2 } ->
                 let m, vmf = dreg_parts dm in
                 Some (cond, ((), ((), (rt2, (rt, ((), ((), (m, ((), vmf)))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (rt2, (rt, ((), ((), (m, ((), vmf))))))))) ->
             Some (Lowered.Vmov_to_double { cond; dm = dreg_unparts m vmf; rt; rt2 }))
           C.(
             cond_codec ** const ~width:7 0b1100010L ** const ~width:1 0L ** reg_field "rt2"
             ** reg_field "rt" ** const ~width:4 0b1011L ** const ~width:2 0b00L
             ** field ~width:1 "D" ** const ~width:1 1L ** field ~width:4 "Vm"));
      C.alt ~label:"vmov-from-double" ~priority:19
        (C.iso_fun ~name:"vmov-from-double"
           ~encode:(function
             | Lowered.Vmov_from_double { cond; rt; rt2; dm } ->
                 let m, vmf = dreg_parts dm in
                 Some (cond, ((), ((), (rt2, (rt, ((), ((), (m, ((), vmf)))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (rt2, (rt, ((), ((), (m, ((), vmf))))))))) ->
             Some (Lowered.Vmov_from_double { cond; rt; rt2; dm = dreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:7 0b1100010L ** const ~width:1 1L ** reg_field "rt2"
             ** reg_field "rt" ** const ~width:4 0b1011L ** const ~width:2 0b00L
             ** field ~width:1 "D" ** const ~width:1 1L ** field ~width:4 "Vm"));
      (* [v3-d]/[v3-s] declare [b23]/[b21]/[b20]/[b6] as plain fields rather
         than four separate consts per op, because {!V3.selector} is the one
         place that relation lives and a const per alternative would restate
         it. The cost is exactly the situation {!movw_alt} already documents:
         [check] sees a field where the two-register "extension" family
         (vneg/vcmp/vcvt/vmov, priorities 12-34) sees a literal, so it reports
         overlap wherever a wildcarded field could reach one of those literal
         patterns. It is harmless on decode: {!V3.of_selector} accepts only
         the four combinations lowering ever produces, declines everything
         else, and [Alt]'s decoder falls through to the next priority on a
         decline - which is exactly why [v3-d]/[v3-s] are given the *lower*
         priority number of every pair [check] names, so a real three-register
         word is claimed here first and everything else falls through to its
         own, correct alternative. *)
      C.alt ~label:"v3-d" ~priority:20
        (C.iso_fun ~name:"v3-d"
           ~encode:(function
             | Lowered.V3_d { cond; op; vd; vn; vm } ->
                 let b23, b21, b20, b6 = V3.selector op in
                 let d, vdf = dreg_parts vd in
                 let n, vnf = dreg_parts vn in
                 let m, vmf = dreg_parts vm in
                 Some
                   ( cond,
                     ((), (b23, (d, (b21, (b20, (vnf, (vdf, ((), (n, (b6, (m, ((), vmf))))))))))))
                   )
             | _ -> None)
           ~decode:(fun
               (cond, ((), (b23, (d, (b21, (b20, (vnf, (vdf, ((), (n, (b6, (m, ((), vmf)))))))))))))
             ->
             match V3.of_selector (b23, b21, b20, b6) with
             | None -> None
             | Some op ->
                 Some
                   (Lowered.V3_d
                      {
                        cond;
                        op;
                        vd = dreg_unparts d vdf;
                        vn = dreg_unparts n vnf;
                        vm = dreg_unparts m vmf;
                      }))
           C.(
             cond_codec ** const ~width:4 0b1110L ** field ~width:1 "b23" ** field ~width:1 "D"
             ** field ~width:1 "b21" ** field ~width:1 "b20" ** field ~width:4 "Vn"
             ** field ~width:4 "Vd" ** const ~width:4 0b1011L ** field ~width:1 "N"
             ** field ~width:1 "b6" ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"v3-s" ~priority:21
        (C.iso_fun ~name:"v3-s"
           ~encode:(function
             | Lowered.V3_s { cond; op; vd; vn; vm } ->
                 let b23, b21, b20, b6 = V3.selector op in
                 let d, vdf = sreg_parts vd in
                 let n, vnf = sreg_parts vn in
                 let m, vmf = sreg_parts vm in
                 Some
                   ( cond,
                     ((), (b23, (d, (b21, (b20, (vnf, (vdf, ((), (n, (b6, (m, ((), vmf))))))))))))
                   )
             | _ -> None)
           ~decode:(fun
               (cond, ((), (b23, (d, (b21, (b20, (vnf, (vdf, ((), (n, (b6, (m, ((), vmf)))))))))))))
             ->
             match V3.of_selector (b23, b21, b20, b6) with
             | None -> None
             | Some op ->
                 Some
                   (Lowered.V3_s
                      {
                        cond;
                        op;
                        vd = sreg_unparts d vdf;
                        vn = sreg_unparts n vnf;
                        vm = sreg_unparts m vmf;
                      }))
           C.(
             cond_codec ** const ~width:4 0b1110L ** field ~width:1 "b23" ** field ~width:1 "D"
             ** field ~width:1 "b21" ** field ~width:1 "b20" ** field ~width:4 "Vn"
             ** field ~width:4 "Vd" ** const ~width:4 0b1010L ** field ~width:1 "N"
             ** field ~width:1 "b6" ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vneg-d" ~priority:22
        (C.iso_fun ~name:"vneg-d"
           ~encode:(function
             | Lowered.Vneg_d { cond; vd; vm } ->
                 let d, vdf = dreg_parts vd in
                 let m, vmf = dreg_parts vm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vneg_d { cond; vd = dreg_unparts d vdf; vm = dreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0001L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:2 0b01L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vneg-s" ~priority:23
        (C.iso_fun ~name:"vneg-s"
           ~encode:(function
             | Lowered.Vneg_s { cond; vd; vm } ->
                 let d, vdf = sreg_parts vd in
                 let m, vmf = sreg_parts vm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vneg_s { cond; vd = sreg_unparts d vdf; vm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0001L ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:2 0b01L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcmp-reg-d" ~priority:24
        (C.iso_fun ~name:"vcmp-reg-d"
           ~encode:(function
             | Lowered.Vcmp_reg_d { cond; vd; vm } ->
                 let d, vdf = dreg_parts vd in
                 let m, vmf = dreg_parts vm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vcmp_reg_d { cond; vd = dreg_unparts d vdf; vm = dreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0100L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:2 0b01L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcmp-reg-s" ~priority:25
        (C.iso_fun ~name:"vcmp-reg-s"
           ~encode:(function
             | Lowered.Vcmp_reg_s { cond; vd; vm } ->
                 let d, vdf = sreg_parts vd in
                 let m, vmf = sreg_parts vm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vcmp_reg_s { cond; vd = sreg_unparts d vdf; vm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0100L ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:2 0b01L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcmp-zero-d" ~priority:26
        (C.iso_fun ~name:"vcmp-zero-d"
           ~encode:(function
             | Lowered.Vcmp_zero_d { cond; vd } ->
                 let d, vdf = dreg_parts vd in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), ()))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), ())))))))) ->
             Some (Lowered.Vcmp_zero_d { cond; vd = dreg_unparts d vdf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0101L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:2 0b01L ** const ~width:6 0L));
      C.alt ~label:"vcmp-zero-s" ~priority:27
        (C.iso_fun ~name:"vcmp-zero-s"
           ~encode:(function
             | Lowered.Vcmp_zero_s { cond; vd } ->
                 let d, vdf = sreg_parts vd in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), ()))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), ())))))))) ->
             Some (Lowered.Vcmp_zero_s { cond; vd = sreg_unparts d vdf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0101L ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:2 0b01L ** const ~width:6 0L));
      (* [vmrs APSR_nzcv, FPSCR] - the only form this corpus needs, so the
         entire word past [cond] is one constant. *)
      C.alt ~label:"vmrs" ~priority:28
        (C.iso_fun ~name:"vmrs"
           ~encode:(function Lowered.Vmrs { cond } -> Some (cond, ()) | _ -> None)
           ~decode:(fun (cond, ()) -> Some (Lowered.Vmrs { cond }))
           C.(cond_codec ** const ~width:28 0xEF1FA10L));
      C.alt ~label:"vcvt-f32-f64" ~priority:29
        (C.iso_fun ~name:"vcvt-f32-f64"
           ~encode:(function
             | Lowered.Vcvt_f32_f64 { cond; sd; dm } ->
                 let d, vdf = sreg_parts sd in
                 let m, vmf = dreg_parts dm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vcvt_f32_f64 { cond; sd = sreg_unparts d vdf; dm = dreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0111L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:2 0b11L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcvt-f64-f32" ~priority:30
        (C.iso_fun ~name:"vcvt-f64-f32"
           ~encode:(function
             | Lowered.Vcvt_f64_f32 { cond; dd; sm } ->
                 let d, vdf = dreg_parts dd in
                 let m, vmf = sreg_parts sm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vcvt_f64_f32 { cond; dd = dreg_unparts d vdf; sm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b0111L ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:2 0b11L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcvt-f32-s32" ~priority:31
        (C.iso_fun ~name:"vcvt-f32-s32"
           ~encode:(function
             | Lowered.Vcvt_f32_s32 { cond; sd; sm } ->
                 let d, vdf = sreg_parts sd in
                 let m, vmf = sreg_parts sm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), ((), (m, ((), vmf)))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), ((), (m, ((), vmf))))))))))) ->
             Some (Lowered.Vcvt_f32_s32 { cond; sd = sreg_unparts d vdf; sm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b1000L ** field ~width:4 "Vd" ** const ~width:4 0b1010L
             ** const ~width:1 1L ** const ~width:1 1L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcvt-f64-s32" ~priority:32
        (C.iso_fun ~name:"vcvt-f64-s32"
           ~encode:(function
             | Lowered.Vcvt_f64_s32 { cond; dd; sm } ->
                 let d, vdf = dreg_parts dd in
                 let m, vmf = sreg_parts sm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), ((), (m, ((), vmf)))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), ((), (m, ((), vmf))))))))))) ->
             Some (Lowered.Vcvt_f64_s32 { cond; dd = dreg_unparts d vdf; sm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b1000L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:1 1L ** const ~width:1 1L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcvt-f64-u32" ~priority:33
        (C.iso_fun ~name:"vcvt-f64-u32"
           ~encode:(function
             | Lowered.Vcvt_f64_u32 { cond; dd; sm } ->
                 let d, vdf = dreg_parts dd in
                 let m, vmf = sreg_parts sm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), ((), (m, ((), vmf)))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), ((), (m, ((), vmf))))))))))) ->
             Some (Lowered.Vcvt_f64_u32 { cond; dd = dreg_unparts d vdf; sm = sreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b1000L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:1 0L ** const ~width:1 1L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vcvt-s32-f64" ~priority:34
        (C.iso_fun ~name:"vcvt-s32-f64"
           ~encode:(function
             | Lowered.Vcvt_s32_f64 { cond; sd; dm } ->
                 let d, vdf = sreg_parts sd in
                 let m, vmf = dreg_parts dm in
                 Some (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), (d, ((), ((), (vdf, ((), ((), (m, ((), vmf)))))))))) ->
             Some (Lowered.Vcvt_s32_f64 { cond; sd = sreg_unparts d vdf; dm = dreg_unparts m vmf }))
           C.(
             cond_codec ** const ~width:5 0x1DL ** field ~width:1 "D" ** const ~width:2 0b11L
             ** const ~width:4 0b1101L ** field ~width:4 "Vd" ** const ~width:4 0b1011L
             ** const ~width:2 0b11L ** field ~width:1 "M" ** const ~width:1 0L
             ** field ~width:4 "Vm"));
      C.alt ~label:"vmem-d" ~priority:35
        (C.iso_fun ~name:"vmem-d"
           ~encode:(function
             | Lowered.Vmem_d { cond; load; vd; rn; offset } ->
                 let up = Int64.compare offset 0L >= 0 in
                 let mag = Int64.abs offset in
                 if not (Int64.equal (Int64.rem mag 4L) 0L) then None
                 else
                   let imm8 = Int64.div mag 4L in
                   if Int64.compare imm8 256L >= 0 then None
                   else
                     let d, vdf = dreg_parts vd in
                     Some
                       ( cond,
                         ( (),
                           ( (if up then 1L else 0L),
                             (d, ((), ((if load then 1L else 0L), (rn, (vdf, ((), imm8)))))) ) ) )
             | _ -> None)
           ~decode:(fun (cond, ((), (up, (d, ((), (load, (rn, (vdf, ((), imm8))))))))) ->
             let mag = Int64.mul imm8 4L in
             let offset = if Int64.equal up 1L then mag else Int64.neg mag in
             Some
               (Lowered.Vmem_d
                  { cond; load = Int64.equal load 1L; vd = dreg_unparts d vdf; rn; offset }))
           C.(
             cond_codec ** const ~width:4 0b1101L ** field ~width:1 "U" ** field ~width:1 "D"
             ** const ~width:1 0L ** field ~width:1 "L" ** reg_field "rn" ** field ~width:4 "Vd"
             ** const ~width:4 0b1011L ** field ~width:8 "imm8"));
      C.alt ~label:"vmem-s" ~priority:36
        (C.iso_fun ~name:"vmem-s"
           ~encode:(function
             | Lowered.Vmem_s { cond; load; vd; rn; offset } ->
                 let up = Int64.compare offset 0L >= 0 in
                 let mag = Int64.abs offset in
                 if not (Int64.equal (Int64.rem mag 4L) 0L) then None
                 else
                   let imm8 = Int64.div mag 4L in
                   if Int64.compare imm8 256L >= 0 then None
                   else
                     let d, vdf = sreg_parts vd in
                     Some
                       ( cond,
                         ( (),
                           ( (if up then 1L else 0L),
                             (d, ((), ((if load then 1L else 0L), (rn, (vdf, ((), imm8)))))) ) ) )
             | _ -> None)
           ~decode:(fun (cond, ((), (up, (d, ((), (load, (rn, (vdf, ((), imm8))))))))) ->
             let mag = Int64.mul imm8 4L in
             let offset = if Int64.equal up 1L then mag else Int64.neg mag in
             Some
               (Lowered.Vmem_s
                  { cond; load = Int64.equal load 1L; vd = sreg_unparts d vdf; rn; offset }))
           C.(
             cond_codec ** const ~width:4 0b1101L ** field ~width:1 "U" ** field ~width:1 "D"
             ** const ~width:1 0L ** field ~width:1 "L" ** reg_field "rn" ** field ~width:4 "Vd"
             ** const ~width:4 0b1010L ** field ~width:8 "imm8"));
      (* [vldr Dd/Sd, label] - the literal-pool form of {!Vmem_d}/{!Vmem_s}'s own
         word, with [Rn] fixed to [pc] (0b1111) rather than a runtime register,
         [L] fixed to 1 (load-only, asm/docs/corpus.md), and [U] fixed to 1
         (forward-only: every corpus occurrence is a trailing same-function
         literal pool, and {!evaluate_fixup} rejects a negative distance rather
         than silently mis-encoding one). [imm8] is a fixup, not a plain field -
         the only difference from {!Vmem_d}/{!Vmem_s}'s own [imm8] is *where its
         value comes from* (a resolved link-time distance, not an operand
         already known at lowering time). Necessarily overlaps [vmem-d]/[vmem-s]
         on fixed bits (their [Rn]/[U]/[L] are wildcards, so an all-1111/1/1
         instance of either already matches them) - given a lower priority so
         they win on decode, reproducing real objdump's own [pc, #imm]
         disassembly rather than a bare-label spelling nothing re-parses back
         to the identical word (Codec.check's own tolerated-overlap list, the
         same shape [nop]/[movw]/[movt] already appear in). *)
      C.alt ~label:"vldr-lit-d" ~priority:41
        (C.iso_fun ~name:"vldr-lit-d"
           ~encode:(function
             | Lowered.Vldr_lit_d { cond; vd; target } ->
                 let imm8 =
                   match target with
                   | Asm_core.Lowered_ast.Symbolic _ -> 0L
                   | Asm_core.Lowered_ast.Resolved { value; _ } -> value
                 in
                 let d, vdf = dreg_parts vd in
                 Some (cond, ((), ((), (d, ((), ((), ((), (vdf, ((), imm8)))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (d, ((), ((), ((), (vdf, ((), imm8))))))))) ->
             Some
               (Lowered.Vldr_lit_d
                  {
                    cond;
                    vd = dreg_unparts d vdf;
                    target = Asm_core.Lowered_ast.Resolved { value = imm8; rung = "vldr-lit-d" };
                  }))
           C.(
             cond_codec ** const ~width:4 0b1101L ** const ~width:1 1L (* U *) ** field ~width:1 "D"
             ** const ~width:1 0L ** const ~width:1 1L (* L *) ** const ~width:4 0b1111L
             (* Rn = pc *) ** field ~width:4 "Vd"
             ** const ~width:4 0b1011L
             ** fixup ~width:8 ~kind:Pcrel_vldr8 "target"));
      C.alt ~label:"vldr-lit-s" ~priority:42
        (C.iso_fun ~name:"vldr-lit-s"
           ~encode:(function
             | Lowered.Vldr_lit_s { cond; vd; target } ->
                 let imm8 =
                   match target with
                   | Asm_core.Lowered_ast.Symbolic _ -> 0L
                   | Asm_core.Lowered_ast.Resolved { value; _ } -> value
                 in
                 let d, vdf = sreg_parts vd in
                 Some (cond, ((), ((), (d, ((), ((), ((), (vdf, ((), imm8)))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (d, ((), ((), ((), (vdf, ((), imm8))))))))) ->
             Some
               (Lowered.Vldr_lit_s
                  {
                    cond;
                    vd = sreg_unparts d vdf;
                    target = Asm_core.Lowered_ast.Resolved { value = imm8; rung = "vldr-lit-s" };
                  }))
           C.(
             cond_codec ** const ~width:4 0b1101L ** const ~width:1 1L (* U *) ** field ~width:1 "D"
             ** const ~width:1 0L ** const ~width:1 1L (* L *) ** const ~width:4 0b1111L
             (* Rn = pc *) ** field ~width:4 "Vd"
             ** const ~width:4 0b1010L
             ** fixup ~width:8 ~kind:Pcrel_vldr8 "target"));
      (* [lsl]/[lsr]/[asr]/[ror rd, rm, rs] - {!Opcode.Shift}'s register-shift-amount form
         (asm/docs/corpus.md: gcc's own [nsieve.c]/[nsievebits.c]). A [mov]-family word (dp = 13,
         S = 0, Rn = 0 - the only evidenced shape) with Rs at bits 11:8 in place of {!Dp_reg}'s
         5-bit immediate shift amount, and bit 4 set rather than clear - the ARM manual's own
         register/immediate-shift discriminator, so this never overlaps {!Dp_reg}'s own fixed
         bit 4 = 0. Verified against real arm-linux-gnueabihf-as/objdump: [e1a00211] for
         [lsl r0, r1, r2], [e1a00231]/[e1a00251]/[e1a00271] for [lsr]/[asr]/[ror] with the same
         operands (only the 2-bit shift-kind field differs). *)
      C.alt ~label:"shift-reg" ~priority:40
        (C.iso_fun ~name:"shift-reg"
           ~encode:(function
             | Lowered.Shift_reg { cond; kind; rd; rm; rs } ->
                 Some (cond, ((), ((), ((), ((), (rd, (rs, ((), (Int64.of_int kind, ((), rm))))))))))
             | _ -> None)
           ~decode:(fun (cond, ((), ((), ((), ((), (rd, (rs, ((), (kind, ((), rm)))))))))) ->
             Some (Lowered.Shift_reg { cond; kind = Int64.to_int kind; rd; rm; rs }))
           C.(
             cond_codec ** const ~width:3 0L ** const ~width:4 13L ** const ~width:1 0L
             ** const ~width:4 0L ** reg_field "rd" ** reg_field "rs" ** const ~width:1 0L
             ** field ~width:2 "shift-kind" ** const ~width:1 1L ** reg_field "rm"));
    ]

let name = "arm"
let triple = "arm-linux-gnueabihf"

(* The A32 encoder's error domain (asm/docs/errors.md). Below source text, so it
   names no token; the front end's operand failures are a separate domain in
   arm.ml, for the reason {!Target_intf.Target.TARGET} gives.

   [`No_modified_immediate] carries the value that has no representation, which
   is the whole content of the failure: an A32 immediate is a rotation of an
   eight-bit field, so *which constant* is what a caller needs. *)
type error_kind =
  [ Target_error.shared
  | `Thumb_out_of_scope
  | `Expected_register_or_shifted
  | `No_modified_immediate of int64
  | `No_modified_immediate_2op of no_modified_immediate_2op
  | `Writeback_out_of_scope
  | `Offset_not_12bit
  | `Udf_imm16_overflow
  | `Wrong_relocation_modifier of wrong_relocation_modifier
  | `Missing_relocation_modifier of missing_relocation_modifier
  | `Imm16_overflow of string
  | `Codec of Codec.error
  | `Decode_short
  | `Decode_no_match
  | `Branch_not_word_aligned
  | `Branch_out_of_range
  | `Vldr_literal_backward
  | `Vldr_literal_not_word_aligned
  | `Vldr_literal_out_of_range
  | `No_data_relocation of int
  | `Padding_not_word_multiple
  | `No_vfp_immediate of float
  | `Vfp_offset_out_of_range of int64
  | `Vmrs_unsupported_operands
  | `Push_needs_two_or_more_registers
  | `Pop_needs_two_or_more_registers
  | `Vpush_needs_contiguous_registers ]

and wrong_relocation_modifier = { opcode : string; want : string; got : string }
and missing_relocation_modifier = { opcode : string; want : string }
and no_modified_immediate_2op = { opcode : string; value : int64 }

type error = error_kind Target_error.t

let pp_error_kind ppf : error_kind -> unit = function
  | #Target_error.shared as e -> Target_error.pp_shared ppf e
  | `Thumb_out_of_scope -> Fmt.string ppf "Thumb is not in M1 scope"
  | `Expected_register_or_shifted -> Fmt.string ppf "expected a register or a shifted register"
  | `No_modified_immediate v -> Fmt.pf ppf "#%Ld has no A32 modified-immediate representation" v
  | `No_modified_immediate_2op { opcode; value } ->
      Fmt.pf ppf "%s #%Ld has no A32 modified-immediate representation; movw is M2" opcode value
  | `Writeback_out_of_scope -> Fmt.string ppf "writeback addressing is not in M1 scope"
  | `Offset_not_12bit -> Fmt.string ppf "offset does not fit the 12-bit immediate"
  | `Udf_imm16_overflow -> Fmt.string ppf "udf immediate does not fit 16 bits"
  | `Wrong_relocation_modifier { opcode; want; got } ->
      Fmt.pf ppf "%s takes #:%s:, not #:%s:" opcode want got
  | `Missing_relocation_modifier { opcode; want } ->
      Fmt.pf ppf "%s needs a #:%s: operand" opcode want
  | `Imm16_overflow op -> Fmt.pf ppf "%s immediate does not fit 16 bits" op
  | `Codec e -> Codec.pp_error ppf e
  | `Decode_short -> Fmt.string ppf "fewer than four bytes remain"
  | `Decode_no_match -> Fmt.string ppf "no form matches this word"
  | `Branch_not_word_aligned -> Fmt.string ppf "branch target is not word-aligned"
  | `Branch_out_of_range -> Fmt.string ppf "branch target is out of range"
  | `Vldr_literal_backward ->
      Fmt.string ppf
        "vldr literal-pool target must follow the instruction; a backward one is not in scope"
  | `Vldr_literal_not_word_aligned -> Fmt.string ppf "vldr literal-pool target is not word-aligned"
  | `Vldr_literal_out_of_range -> Fmt.string ppf "vldr literal-pool target is out of range"
  | `No_data_relocation w -> Fmt.pf ppf "no absolute relocation for a %d-byte data initializer" w
  | `Padding_not_word_multiple ->
      Fmt.string ppf "A32 padding must be a whole number of four-byte instructions"
  | `No_vfp_immediate v ->
      Fmt.pf ppf "#%s has no VFP modified-immediate representation" (decimal_of_float v)
  | `Vfp_offset_out_of_range off ->
      Fmt.pf ppf "vldr/vstr offset %Ld does not fit a word-aligned 10-bit range" off
  | `Vmrs_unsupported_operands -> Fmt.string ppf "vmrs only supports APSR_nzcv, FPSCR in M1 scope"
  | `Push_needs_two_or_more_registers ->
      Fmt.string ppf
        "push needs two or more registers; a single register is str rN, [sp, #-4]!, not in scope"
  | `Pop_needs_two_or_more_registers ->
      Fmt.string ppf
        "pop needs two or more registers; a single register is ldr rN, [sp], #4, not in scope"
  | `Vpush_needs_contiguous_registers ->
      Fmt.string ppf
        "vpush.64 needs a non-empty, ascending, contiguous D-register list, e.g. {d8, d9}"

let error_kind_code : error_kind -> string = function
  | `Unknown_instruction _ -> "arm.simplify"
  | `Thumb_out_of_scope | `Immediate_too_wide | `Expected_register_or_shifted
  | `No_modified_immediate _ | `No_modified_immediate_2op _ | `Writeback_out_of_scope
  | `Offset_not_12bit | `Udf_imm16_overflow | `Wrong_relocation_modifier _
  | `Missing_relocation_modifier _ | `Imm16_overflow _ | `No_form _ | `No_vfp_immediate _
  | `Vfp_offset_out_of_range _ | `Vmrs_unsupported_operands | `Push_needs_two_or_more_registers
  | `Pop_needs_two_or_more_registers | `Vpush_needs_contiguous_registers ->
      "arm.lower"
  | `Codec e -> Option.value (Codec.code e) ~default:"arm.encode"
  | `Decode_short | `Decode_no_match | `Decode_no_normalized -> "arm.decode"
  | `Branch_not_word_aligned | `Branch_out_of_range | `Vldr_literal_backward
  | `Vldr_literal_not_word_aligned | `Vldr_literal_out_of_range ->
      "arm.fixup"
  | `No_data_relocation _ -> "arm.data-fixup"
  | `Padding_not_word_multiple -> "arm.nop"

let pp_error ppf e = pp_error_kind ppf (Target_error.kind e)
let error_code e = error_kind_code (Target_error.kind e)
let error_diagnostic e = Target_error.to_diagnostic ~code:error_kind_code ~pp:pp_error_kind e

(* Returns the wrapped error rather than a bare one, so every [Error (diag ...)]
   site keeps its shape: [Err.t] is a [result] whose error branch is an
   [Err.Error.t]. *)
let diag ?pos ?origin kind =
  Err.Error.make ?pos ~pp_error
    (Target_error.make
       ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:"arm" ())
       kind)

let make_surface_instruction ~mnemonic ~origin ops = Ok { Surface.mnemonic; ops; origin }

(* {1 Simplify} *)

let simplify_instruction ~features s =
  ignore features;
  let bad kind = Error (diag ~pos:__POS__ ~origin:s.Surface.origin kind) in
  let m = s.Surface.mnemonic in
  match Opcode.of_mnemonic m with
  (* The unsuffixed spelling first, so [b] is the branch rather than [b] with a
     stripped condition, and [bl] is not read as [b] plus [l]. Only when no
     opcode claims the whole mnemonic is a condition suffix looked for. *)
  | Some op -> Ok (Instruction.mk op s.Surface.ops)
  | None -> (
      match Opcode.split_vfp_mnemonic m with
      | Some (stem, cond, rest) -> (
          match Opcode.vfp_opcode_of stem rest with
          | Some op -> Ok (Instruction.mk ~cond op s.Surface.ops)
          | None -> bad (`Unknown_instruction m))
      | None -> (
          let stem, cond = Cond.split m in
          match Opcode.of_mnemonic stem with
          | Some op -> Ok (Instruction.mk ~cond op s.Surface.ops)
          | None -> bad (`Unknown_instruction m)))

(* {1 Lower} *)

(* Shared by [ldr]/[str]/[strb]: the mnemonic and byte-width vary, but which
   [Lowered] form a memory operand becomes - [Ldst_imm] or [Ldst_reg] - and
   the 12-bit magnitude check that only the immediate form needs, do not. *)
let lower_ldst ~bad ~cond ~load ~byte ~rt ~(m : Mem.t) =
  match m.offset with
  | Mem.Imm offset ->
      if Int64.compare (Int64.abs offset) 4096L >= 0 then bad `Offset_not_12bit
      else
        Ok
          [
            Lowered.Ldst_imm { cond; load; byte; rt; rn = m.base; offset; writeback = m.writeback };
          ]
  | Mem.Reg_offset { reg = rm; negate; kind = sh_kind; amount = sh_amt } ->
      (* Register-offset writeback ([str r0, [r1, r2]!]) is real ARM syntax
         but corpus-unevidenced (M5 classify-c-gcc's one finding, [sp, #-N]!,
         is always an immediate offset - asm/docs/corpus.md) - stays out of
         scope here even though the immediate sibling just above is now in. *)
      if m.writeback then bad `Writeback_out_of_scope
      else
        Ok [ Lowered.Ldst_reg { cond; load; byte; rt; rn = m.base; rm; negate; sh_kind; sh_amt } ]

let lower_instruction state i =
  let bad kind = Error (diag ~pos:__POS__ kind) in
  if state.thumb then bad `Thumb_out_of_scope
  else
    let imm_of v =
      match Bigint.to_int64_opt v with
      | Some x -> Ok x
      | None -> Error (diag ~pos:__POS__ `Immediate_too_wide)
    in
    let cond = i.Instruction.cond in
    match (i.Instruction.op, i.Instruction.ops) with
    | ((Opcode.Mov | Opcode.Mvn) as op), [ Operand.Reg rd; Operand.Imm v ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm ->
            if encode_modimm imm = None then
              bad (`No_modified_immediate_2op { opcode = Opcode.name op; value = imm })
            else
              Ok
                [
                  Lowered.Dp_imm
                    { cond; dp = Opcode.to_dp op; s = false; rd; rn = Reg.of_num 0; imm };
                ])
    (* The register forms, with or without a shift. [Shifted] and a bare [Reg]
       differ only in the two fields the encoding already has, so one case
       covers both rather than duplicating the lowering. *)
    | ( (( Opcode.Add | Opcode.Sub | Opcode.And | Opcode.Mov | Opcode.Eor | Opcode.Rsb | Opcode.Orr
         | Opcode.Mvn | Opcode.Bic | Opcode.Adc ) as op),
        (Operand.Reg rd :: rest as ops) )
      when match ops with
           | [ _; Operand.Reg _ ]
           | [ _; Operand.Shifted _ ]
           | [ _; Operand.Reg _; Operand.Reg _ ]
           | [ _; Operand.Reg _; Operand.Shifted _ ] ->
               true
           | _ -> false -> (
        let rn, shifted =
          match rest with
          | [ x ] -> (Reg.of_num 0, x)
          | [ Operand.Reg n; x ] -> (n, x)
          | _ -> (Reg.of_num 0, Operand.Reg (Reg.of_num 0))
        in
        match shifted with
        | Operand.Reg rm ->
            Ok
              [
                Lowered.Dp_reg
                  { cond; dp = Opcode.to_dp op; s = false; rd; rn; rm; sh_kind = 0; sh_amt = 0 };
              ]
        | Operand.Shifted { reg = rm; kind; amount } ->
            Ok
              [
                Lowered.Dp_reg
                  {
                    cond;
                    dp = Opcode.to_dp op;
                    s = false;
                    rd;
                    rn;
                    rm;
                    sh_kind = kind;
                    sh_amt = amount;
                  };
              ]
        | _ -> bad `Expected_register_or_shifted)
    | Opcode.Cmp, [ Operand.Reg rn; Operand.Shifted { reg = rm; kind; amount } ] ->
        Ok
          [
            Lowered.Dp_reg
              {
                cond;
                dp = Opcode.to_dp Opcode.Cmp;
                s = true;
                rd = Reg.of_num 0;
                rn;
                rm;
                sh_kind = kind;
                sh_amt = amount;
              };
          ]
    | ( ((Opcode.Add | Opcode.Sub | Opcode.And | Opcode.Rsb | Opcode.Orr | Opcode.Bic | Opcode.Eor)
         as op),
        [ Operand.Reg rd; Operand.Reg rn; Operand.Imm v ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm ->
            if encode_modimm imm = None then bad (`No_modified_immediate imm)
            else Ok [ Lowered.Dp_imm { cond; dp = Opcode.to_dp op; s = false; rd; rn; imm } ])
    (* str/ldr writeback ([str fp, [sp, #-4]!]) is M5 classify-c-gcc's one
       corpus-evidenced writeback form (asm/docs/corpus.md: gcc's own
       frame-pointer prologue idiom) - lower_ldst now honors m.writeback for
       an immediate offset instead of rejecting it outright. strb/ldrb below
       keep the blanket rejection: no byte-access writeback is evidenced, and
       nothing about the word-vs-byte bit makes it a safe default to widen
       for free. *)
    | ((Opcode.Str | Opcode.Ldr) as op), [ Operand.Reg rt; Operand.Mem m ] ->
        lower_ldst ~bad ~cond ~load:(op = Opcode.Ldr) ~byte:false ~rt ~m
    | Opcode.Strb, [ Operand.Reg rt; Operand.Mem m ] ->
        if m.writeback then bad `Writeback_out_of_scope
        else lower_ldst ~bad ~cond ~load:false ~byte:true ~rt ~m
    | Opcode.Ldrb, [ Operand.Reg rt; Operand.Mem m ] ->
        if m.writeback then bad `Writeback_out_of_scope
        else lower_ldst ~bad ~cond ~load:true ~byte:true ~rt ~m
    | Opcode.Bx, [ Operand.Reg rm ] -> Ok [ Lowered.Bx { cond; rm } ]
    | Opcode.Push, [ Operand.Reglist regs ] ->
        if List.length regs < 2 then bad `Push_needs_two_or_more_registers
        else
          Ok
            [ Lowered.Push { cond; regs = List.fold_left (fun acc n -> acc lor (1 lsl n)) 0 regs } ]
    | (Opcode.Stmia | Opcode.Ldmia), [ Operand.Reg_writeback rn; Operand.Reglist regs ] ->
        Ok
          [
            Lowered.Ldm
              {
                cond;
                load = i.Instruction.op = Opcode.Ldmia;
                rn;
                regs = List.fold_left (fun acc n -> acc lor (1 lsl n)) 0 regs;
              };
          ]
    | Opcode.Pop, [ Operand.Reglist regs ] ->
        if List.length regs < 2 then bad `Pop_needs_two_or_more_registers
        else
          Ok
            [
              Lowered.Ldm
                {
                  cond;
                  load = true;
                  rn = Reg.sp;
                  regs = List.fold_left (fun acc n -> acc lor (1 lsl n)) 0 regs;
                };
            ]
    | Opcode.Vpush, [ Operand.Dreglist regs ] -> (
        let sorted = List.sort compare regs in
        match sorted with
        | [] -> bad `Vpush_needs_contiguous_registers
        | base :: _ as members ->
            let count = List.length members in
            if List.for_all2 (fun n i -> n = base + i) members (List.init count Fun.id) then
              Ok [ Lowered.Vpush { cond; vd = Dreg.of_num base; count } ]
            else bad `Vpush_needs_contiguous_registers)
    | Opcode.Adds, [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm ] ->
        Ok
          [
            Lowered.Dp_reg
              { cond; dp = Opcode.to_dp Opcode.Adds; s = true; rd; rn; rm; sh_kind = 0; sh_amt = 0 };
          ]
    | Opcode.Nop, [] -> Ok [ Lowered.Nop { cond } ]
    | Opcode.Shift kind, [ Operand.Reg rd; Operand.Reg rm; Operand.Reg rs ] ->
        Ok [ Lowered.Shift_reg { cond; kind; rd; rm; rs } ]
    | Opcode.Udf, [ Operand.Imm v ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm ->
            if Int64.compare imm 0L < 0 || Int64.compare imm 65536L >= 0 then
              bad `Udf_imm16_overflow
            else Ok [ Lowered.Udf { imm16 = imm } ])
    | Opcode.Bl, [ Operand.Sym e ] ->
        Ok
          [ Lowered.Bl { cond; target = Asm_core.Lowered_ast.Symbolic { value = e; rung = None } } ]
    (* A32 branches are fixed-width: one form, no ladder, so no rung to pin.
       The 24-bit field reaches 32 MiB either way, which every fixture is far
       inside. *)
    | Opcode.B, [ Operand.Sym e ] ->
        Ok [ Lowered.B { cond; target = Asm_core.Lowered_ast.Symbolic { value = e; rung = None } } ]
    (* A comparison writes no destination: the encoding requires Rd = 0 and
       S = 1, and reading that off [is_compare] rather than off the operand
       list is what keeps one lowering for the whole data-processing family. *)
    | Opcode.Cmp, [ Operand.Reg rn; Operand.Reg rm ] ->
        Ok
          [
            Lowered.Dp_reg
              {
                cond;
                dp = Opcode.to_dp Opcode.Cmp;
                s = true;
                rd = Reg.of_num 0;
                rn;
                rm;
                sh_kind = 0;
                sh_amt = 0;
              };
          ]
    | Opcode.Cmp, [ Operand.Reg rn; Operand.Imm v ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm ->
            if encode_modimm imm = None then bad (`No_modified_immediate imm)
            else
              Ok
                [
                  Lowered.Dp_imm
                    { cond; dp = Opcode.to_dp Opcode.Cmp; s = true; rd = Reg.of_num 0; rn; imm };
                ])
    (* [movw]/[movt]. Per the contract's §B7 the modifier is *consumed* here -
       it selects the form, and what is stored is the expression underneath -
       because [Expr.fold] deliberately preserves a [Modifier], so one left in
       place would reach [bind_image] as a residual and be reported there
       instead of being understood here.
       The pairing is checked rather than assumed: [movw] takes [:lower16:] and
       [movt] takes [:upper16:], and the two are not interchangeable - a [movt]
       given the low half would silently write the wrong sixteen bits of every
       address. *)
    | ((Opcode.Movw | Opcode.Movt) as op), [ Operand.Reg rd; Operand.Sym e ] -> (
        let top = op = Opcode.Movt in
        let want = if top then "upper16" else "lower16" in
        match e with
        | Asm_core.Expr.Modifier (m, inner) when String.equal m want ->
            Ok
              [
                Lowered.Movw_movt
                  {
                    cond;
                    top;
                    rd;
                    imm = Asm_core.Lowered_ast.Symbolic { value = inner; rung = None };
                  };
              ]
        | Asm_core.Expr.Modifier (m, _) ->
            bad (`Wrong_relocation_modifier { opcode = Opcode.name op; want; got = m })
        | _ -> bad (`Missing_relocation_modifier { opcode = Opcode.name op; want }))
    | ((Opcode.Movw | Opcode.Movt) as op), [ Operand.Reg rd; Operand.Imm v ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm ->
            if Int64.compare imm 0L < 0 || Int64.compare imm 65536L >= 0 then
              bad (`Imm16_overflow (Opcode.name op))
            else
              Ok
                [
                  Lowered.Movw_movt
                    {
                      cond;
                      top = op = Opcode.Movt;
                      rd;
                      imm = Asm_core.Lowered_ast.Resolved { value = imm; rung = Opcode.name op };
                    };
                ])
    | Opcode.Mul, [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm ] ->
        Ok [ Lowered.Mul { cond; s = false; rd; rn; rm } ]
    | Opcode.Mla, [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm; Operand.Reg ra ] ->
        Ok [ Lowered.Mla { cond; s = false; rd; rn; rm; ra } ]
    (* {2 VFP} *)
    | Opcode.Vmov_d, [ Operand.Dreg vd; Operand.Dreg vm ] ->
        Ok [ Lowered.Vmov_reg_d { cond; vd; vm } ]
    | Opcode.Vmov_d, [ Operand.Dreg vd; Operand.FImm v ] -> (
        match vfp_compress v with
        | Some imm8 -> Ok [ Lowered.Vmov_imm_d { cond; vd; imm8 } ]
        | None -> bad (`No_vfp_immediate v))
    | Opcode.Vmov_s, [ Operand.Sreg vd; Operand.Sreg vm ] ->
        Ok [ Lowered.Vmov_reg_s { cond; vd; vm } ]
    | Opcode.Vmov_s, [ Operand.Sreg vd; Operand.FImm v ] -> (
        match vfp_compress v with
        | Some imm8 -> Ok [ Lowered.Vmov_imm_s { cond; vd; imm8 } ]
        | None -> bad (`No_vfp_immediate v))
    | Opcode.Vmov_core, [ Operand.Sreg sn; Operand.Reg rt ] ->
        Ok [ Lowered.Vmov_to_single { cond; sn; rt } ]
    | Opcode.Vmov_core, [ Operand.Reg rt; Operand.Sreg sn ] ->
        Ok [ Lowered.Vmov_from_single { cond; rt; sn } ]
    | Opcode.Vmov_core, [ Operand.Dreg dm; Operand.Reg rt; Operand.Reg rt2 ] ->
        Ok [ Lowered.Vmov_to_double { cond; dm; rt; rt2 } ]
    | Opcode.Vmov_core, [ Operand.Reg rt; Operand.Reg rt2; Operand.Dreg dm ] ->
        Ok [ Lowered.Vmov_from_double { cond; rt; rt2; dm } ]
    | Opcode.V3d op, [ Operand.Dreg vd; Operand.Dreg vn; Operand.Dreg vm ] ->
        Ok [ Lowered.V3_d { cond; op; vd; vn; vm } ]
    | Opcode.V3s op, [ Operand.Sreg vd; Operand.Sreg vn; Operand.Sreg vm ] ->
        Ok [ Lowered.V3_s { cond; op; vd; vn; vm } ]
    | Opcode.Vneg_d, [ Operand.Dreg vd; Operand.Dreg vm ] -> Ok [ Lowered.Vneg_d { cond; vd; vm } ]
    | Opcode.Vneg_s, [ Operand.Sreg vd; Operand.Sreg vm ] -> Ok [ Lowered.Vneg_s { cond; vd; vm } ]
    | Opcode.Vcmp_d, [ Operand.Dreg vd; Operand.Dreg vm ] ->
        Ok [ Lowered.Vcmp_reg_d { cond; vd; vm } ]
    | Opcode.Vcmp_d, [ Operand.Dreg vd; Operand.Imm v ] when Bigint.is_zero v ->
        Ok [ Lowered.Vcmp_zero_d { cond; vd } ]
    | Opcode.Vcmp_s, [ Operand.Sreg vd; Operand.Sreg vm ] ->
        Ok [ Lowered.Vcmp_reg_s { cond; vd; vm } ]
    | Opcode.Vcmp_s, [ Operand.Sreg vd; Operand.Imm v ] when Bigint.is_zero v ->
        Ok [ Lowered.Vcmp_zero_s { cond; vd } ]
    | Opcode.Vmrs, [ Operand.Sym (Asm_core.Expr.Symbol a); Operand.Sym (Asm_core.Expr.Symbol f) ]
      when String.equal a "APSR_nzcv" && String.equal f "FPSCR" ->
        Ok [ Lowered.Vmrs { cond } ]
    | Opcode.Vmrs, _ -> bad `Vmrs_unsupported_operands
    | Opcode.Vcvt_f32_f64, [ Operand.Sreg sd; Operand.Dreg dm ] ->
        Ok [ Lowered.Vcvt_f32_f64 { cond; sd; dm } ]
    | Opcode.Vcvt_f64_f32, [ Operand.Dreg dd; Operand.Sreg sm ] ->
        Ok [ Lowered.Vcvt_f64_f32 { cond; dd; sm } ]
    | Opcode.Vcvt_f32_s32, [ Operand.Sreg sd; Operand.Sreg sm ] ->
        Ok [ Lowered.Vcvt_f32_s32 { cond; sd; sm } ]
    | Opcode.Vcvt_f64_s32, [ Operand.Dreg dd; Operand.Sreg sm ] ->
        Ok [ Lowered.Vcvt_f64_s32 { cond; dd; sm } ]
    | Opcode.Vcvt_f64_u32, [ Operand.Dreg dd; Operand.Sreg sm ] ->
        Ok [ Lowered.Vcvt_f64_u32 { cond; dd; sm } ]
    | Opcode.Vcvt_s32_f64, [ Operand.Sreg sd; Operand.Dreg dm ] ->
        Ok [ Lowered.Vcvt_s32_f64 { cond; sd; dm } ]
    (* [vldr]/[vstr] offset-only addressing: the 8-bit field is scaled by 4,
       so the reach is a word-aligned +/-1020 - {!Mem.Reg_offset} (register
       offset) has no VFP encoding at all and falls to [No_form]. *)
    | ( (Opcode.Vldr | Opcode.Vstr),
        [ Operand.Dreg vd; Operand.Mem { offset = Mem.Imm offset; base; _ } ] ) ->
        if Int64.rem offset 4L <> 0L || Int64.compare (Int64.abs offset) 1024L >= 0 then
          bad (`Vfp_offset_out_of_range offset)
        else
          Ok
            [
              Lowered.Vmem_d { cond; load = i.Instruction.op = Opcode.Vldr; vd; rn = base; offset };
            ]
    | ( (Opcode.Vldr | Opcode.Vstr),
        [ Operand.Sreg vd; Operand.Mem { offset = Mem.Imm offset; base; _ } ] ) ->
        if Int64.rem offset 4L <> 0L || Int64.compare (Int64.abs offset) 1024L >= 0 then
          bad (`Vfp_offset_out_of_range offset)
        else
          Ok
            [
              Lowered.Vmem_s { cond; load = i.Instruction.op = Opcode.Vldr; vd; rn = base; offset };
            ]
    (* [vldr Dd/Sd, label] - a PC-relative literal-pool load (asm/docs/corpus.md:
       fftsp.c's/knucleotide.c's own [.f32] float constants). Load-only:
       {!Opcode.Vstr} never reaches here, since real hardware defines a
       store's [Rn = 1111] as UNPREDICTABLE and no fixture spells it. *)
    | Opcode.Vldr, [ Operand.Dreg vd; Operand.Sym e ] ->
        Ok
          [
            Lowered.Vldr_lit_d
              { cond; vd; target = Asm_core.Lowered_ast.Symbolic { value = e; rung = None } };
          ]
    | Opcode.Vldr, [ Operand.Sreg vd; Operand.Sym e ] ->
        Ok
          [
            Lowered.Vldr_lit_s
              { cond; vd; target = Asm_core.Lowered_ast.Symbolic { value = e; rung = None } };
          ]
    (* [ite] is a Thumb construct. GAS accepts it in A32 source and emits
       nothing, because conditional execution there is native to every
       instruction - the suffixed [moveq]/[movne] that follow already say what
       the [ite] would have said. Measured: the fixture's offsets step straight
       from the [cmp] to the [moveq]. Accepting and dropping it is therefore
       matching GAS, not ignoring an instruction. *)
    | Opcode.Ite, _ -> Ok []
    | _ -> bad (`No_form (Opcode.name i.Instruction.op))

(* {1 Encode and decode}

   A32 words are stored little-endian, and the codec produced them most
   significant bit first, so the four bytes are reversed exactly once, here. *)

let word_to_memory s = if String.length s <> 4 then s else String.init 4 (fun i -> s.[3 - i])

(* The same reversal reaches the fixups: a codec bit at position [p] of the
   most-significant-first word lands at bit [32 - p - w] of the little-endian
   container. movw/movt's imm4 and imm12 are two slices of one value under this,
   with no special case. [pc_bias] is 8 - the A32 pipeline offset - and it lives
   here rather than inside [evaluate_fixup], so that the evaluator receives a PC
   and not a thing it has to correct. *)
let fixup_of_placement (p : fixup_kind C.placement) =
  let kind = p.C.kind in
  {
    Asm_core.Lowered_ast.kind;
    kind_name = fixup_kind_name kind;
    family = fixup_family kind;
    role = fixup_role kind;
    name = p.C.name;
    slices =
      List.map
        (fun (s : C.slice) ->
          {
            Asm_core.Lowered_ast.bit_offset = 32 - s.C.bit_offset - s.C.bit_width;
            bit_width = s.C.bit_width;
            value_lsb = s.C.value_lsb;
          })
        p.C.slices;
    byte_offset = 0;
    container = 4;
    pc_bias = (match kind with Pcrel_b26 | Pcrel_call | Pcrel_vldr8 -> 8 | _ -> 0);
    range =
      (match kind with
      | Abs32 -> Asm_core.Lowered_ast.Bitpattern 32
      | Pcrel_b26 | Pcrel_call -> Asm_core.Lowered_ast.Signed 24
      | Movw_abs_nc | Movt_abs -> Asm_core.Lowered_ast.Unsigned 16
      (* Already reduced to the final imm8 field's own value by
         [evaluate_fixup] (word-shifted, forward-only) - this is a defensive
         re-check, the same role [Unsigned 16] plays for [Movw_abs_nc]/
         [Movt_abs] above, not a second computation. *)
      | Pcrel_vldr8 -> Asm_core.Lowered_ast.Unsigned 8);
    value = Asm_core.Expr.Const (Bigint.of_int 0);
    pairing = Asm_core.Lowered_ast.Unpaired;
    origin = Origin.synthesized ~pass:"arm.encode" ();
  }

(* The expression lives in the lowered value - Codec sits below asm_core and
   cannot mention Expr - and is paired with the placement by name. A placement
   with no expression came from a Resolved operand whose bits are already
   final, so it yields no linker fixup. *)
let expr_of_lowered : Lowered.t -> (string * Asm_core.Expr.t) list = function
  | Lowered.Bl { target = Asm_core.Lowered_ast.Symbolic { value; _ }; _ }
  | Lowered.B { target = Asm_core.Lowered_ast.Symbolic { value; _ }; _ }
  | Lowered.Vldr_lit_d { target = Asm_core.Lowered_ast.Symbolic { value; _ }; _ }
  | Lowered.Vldr_lit_s { target = Asm_core.Lowered_ast.Symbolic { value; _ }; _ } ->
      [ ("target", value) ]
  (* The name is the codec placement's, not a description: [form_of] pairs the
     two by name, and a mismatch here does not fail - it silently produces an
     encoding with no fixup, which then binds to the placeholder. *)
  | Lowered.Movw_movt { imm = Asm_core.Lowered_ast.Symbolic { value; _ }; _ } -> [ ("imm", value) ]
  | _ -> []

let form_of l enc =
  let exprs = expr_of_lowered l in
  {
    Asm_core.Lowered_ast.bytes = word_to_memory (C.Bits.to_bytes enc.C.bits);
    form = C.form_id enc;
    fixups =
      List.filter_map
        (fun (p : fixup_kind C.placement) ->
          match List.assoc_opt p.C.name exprs with
          | None -> None
          | Some value -> Some { (fixup_of_placement p) with Asm_core.Lowered_ast.value })
        enc.C.placements;
  }

(* A32 is fixed-width: no ladder, every form `Fixed. *)
let encode l =
  match C.encode codec l with
  (* A32 is fixed-width and declares no ladder, so [e.code] is always [None]
     here; reading it anyway keeps one rule across the three targets rather than
     a special case waiting to be forgotten. *)
  | Error (e : C.error) -> Error (diag ~pos:__POS__ (`Codec e))
  | Ok enc -> Ok (`Fixed (form_of l enc))

type decode_context = { state : target_state; address : int64 }

let instruction_of_lowered ?(at = 0L) =
  (* A branch field is a word count measured from the instruction plus eight,
     the A32 pipeline offset, so the address it names is [at + 8 + 4 * imm24].
     Printing the absolute target rather than the count is what objdump does and
     the only spelling re-parsing can mean. *)
  let absolute_target = function
    | Asm_core.Lowered_ast.Resolved { value; _ } -> Int64.add at (Int64.add 8L (Int64.mul value 4L))
    | Asm_core.Lowered_ast.Symbolic _ -> at
  in
  function
  | Lowered.Dp_imm { cond; dp; rd; rn; imm; _ } -> (
      match Opcode.of_dp dp with
      | None -> None
      | Some ((Opcode.Mov | Opcode.Mvn) as o) ->
          Some (Instruction.mk ~cond o [ Operand.Reg rd; Operand.Imm (Bigint.of_int64 imm) ])
      | Some o when Opcode.is_compare o ->
          Some (Instruction.mk ~cond o [ Operand.Reg rn; Operand.Imm (Bigint.of_int64 imm) ])
      | Some o ->
          Some
            (Instruction.mk ~cond o
               [ Operand.Reg rd; Operand.Reg rn; Operand.Imm (Bigint.of_int64 imm) ]))
  | Lowered.Dp_reg { cond; dp; s; rd; rn; rm; sh_kind; sh_amt } -> (
      let shifted =
        if sh_kind = 0 && sh_amt = 0 then Operand.Reg rm
        else Operand.Shifted { reg = rm; kind = sh_kind; amount = sh_amt }
      in
      match Opcode.of_dp dp with
      | None -> None
      | Some ((Opcode.Mov | Opcode.Mvn) as o) ->
          Some (Instruction.mk ~cond o [ Operand.Reg rd; shifted ])
      | Some o when Opcode.is_compare o -> Some (Instruction.mk ~cond o [ Operand.Reg rn; shifted ])
      | Some Opcode.Add when s ->
          Some (Instruction.mk ~cond Opcode.Adds [ Operand.Reg rd; Operand.Reg rn; shifted ])
      | Some o -> Some (Instruction.mk ~cond o [ Operand.Reg rd; Operand.Reg rn; shifted ]))
  | Lowered.Nop { cond } -> Some (Instruction.mk ~cond Opcode.Nop [])
  | Lowered.Shift_reg { cond; kind; rd; rm; rs } ->
      Some
        (Instruction.mk ~cond (Opcode.Shift kind)
           [ Operand.Reg rd; Operand.Reg rm; Operand.Reg rs ])
  | Lowered.Ldst_imm { cond; load; byte; rt; rn; offset; writeback } -> (
      let mem_op = Operand.Mem { base = rn; offset = Mem.Imm offset; writeback; pre = true } in
      match (load, byte) with
      | false, false -> Some (Instruction.mk ~cond Opcode.Str [ Operand.Reg rt; mem_op ])
      | true, false -> Some (Instruction.mk ~cond Opcode.Ldr [ Operand.Reg rt; mem_op ])
      | false, true -> Some (Instruction.mk ~cond Opcode.Strb [ Operand.Reg rt; mem_op ])
      | true, true -> Some (Instruction.mk ~cond Opcode.Ldrb [ Operand.Reg rt; mem_op ]))
  | Lowered.Ldst_reg { cond; load; byte; rt; rn; rm; negate; sh_kind; sh_amt } -> (
      let mem_op =
        Operand.Mem
          {
            base = rn;
            offset = Mem.Reg_offset { reg = rm; negate; kind = sh_kind; amount = sh_amt };
            writeback = false;
            pre = true;
          }
      in
      match (load, byte) with
      | false, false -> Some (Instruction.mk ~cond Opcode.Str [ Operand.Reg rt; mem_op ])
      | true, false -> Some (Instruction.mk ~cond Opcode.Ldr [ Operand.Reg rt; mem_op ])
      | false, true -> Some (Instruction.mk ~cond Opcode.Strb [ Operand.Reg rt; mem_op ])
      | true, true -> Some (Instruction.mk ~cond Opcode.Ldrb [ Operand.Reg rt; mem_op ]))
  | Lowered.Bx { cond; rm } -> Some (Instruction.mk ~cond Opcode.Bx [ Operand.Reg rm ])
  | Lowered.Push { cond; regs } ->
      let members = List.filter (fun n -> regs land (1 lsl n) <> 0) (List.init 16 Fun.id) in
      Some (Instruction.mk ~cond Opcode.Push [ Operand.Reglist members ])
  | Lowered.Ldm { cond; load; rn; regs } ->
      let members = List.filter (fun n -> regs land (1 lsl n) <> 0) (List.init 16 Fun.id) in
      if load && Reg.equal rn Reg.sp && List.length members >= 2 then
        Some (Instruction.mk ~cond Opcode.Pop [ Operand.Reglist members ])
      else
        Some
          (Instruction.mk ~cond
             (if load then Opcode.Ldmia else Opcode.Stmia)
             [ Operand.Reg_writeback rn; Operand.Reglist members ])
  | Lowered.Udf { imm16 } ->
      Some (Instruction.mk Opcode.Udf [ Operand.Imm (Bigint.of_int64 imm16) ])
  | Lowered.Bl { cond; target } ->
      Some
        (Instruction.mk ~cond Opcode.Bl
           [ Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 (absolute_target target))) ])
  | Lowered.B { cond; target } ->
      Some
        (Instruction.mk ~cond Opcode.B
           [ Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 (absolute_target target))) ])
  | Lowered.Mul { cond; rd; rn; rm; _ } ->
      Some (Instruction.mk ~cond Opcode.Mul [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm ])
  | Lowered.Mla { cond; rd; rn; rm; ra; _ } ->
      Some
        (Instruction.mk ~cond Opcode.Mla
           [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm; Operand.Reg ra ])
  | Lowered.Movw_movt { cond; top; rd; imm } ->
      (* Decoded, the immediate is the raw sixteen bits; symbolic, it is still
         the modifier expression. Either way the operand is an immediate, which
         is what re-parsing needs to see. *)
      let v =
        match imm with
        | Asm_core.Lowered_ast.Resolved { value; _ } -> Operand.Imm (Bigint.of_int64 value)
        | Asm_core.Lowered_ast.Symbolic { value; _ } -> Operand.Sym value
      in
      Some (Instruction.mk ~cond (if top then Opcode.Movt else Opcode.Movw) [ Operand.Reg rd; v ])
  | Lowered.Vmov_reg_d { cond; vd; vm } ->
      Some (Instruction.mk ~cond Opcode.Vmov_d [ Operand.Dreg vd; Operand.Dreg vm ])
  | Lowered.Vmov_reg_s { cond; vd; vm } ->
      Some (Instruction.mk ~cond Opcode.Vmov_s [ Operand.Sreg vd; Operand.Sreg vm ])
  | Lowered.Vmov_imm_d { cond; vd; imm8 } ->
      Some (Instruction.mk ~cond Opcode.Vmov_d [ Operand.Dreg vd; Operand.FImm (vfp_expand imm8) ])
  | Lowered.Vmov_imm_s { cond; vd; imm8 } ->
      Some (Instruction.mk ~cond Opcode.Vmov_s [ Operand.Sreg vd; Operand.FImm (vfp_expand imm8) ])
  | Lowered.Vmov_to_single { cond; sn; rt } ->
      Some (Instruction.mk ~cond Opcode.Vmov_core [ Operand.Sreg sn; Operand.Reg rt ])
  | Lowered.Vmov_from_single { cond; rt; sn } ->
      Some (Instruction.mk ~cond Opcode.Vmov_core [ Operand.Reg rt; Operand.Sreg sn ])
  | Lowered.Vmov_to_double { cond; dm; rt; rt2 } ->
      Some
        (Instruction.mk ~cond Opcode.Vmov_core [ Operand.Dreg dm; Operand.Reg rt; Operand.Reg rt2 ])
  | Lowered.Vmov_from_double { cond; rt; rt2; dm } ->
      Some
        (Instruction.mk ~cond Opcode.Vmov_core [ Operand.Reg rt; Operand.Reg rt2; Operand.Dreg dm ])
  | Lowered.V3_d { cond; op; vd; vn; vm } ->
      Some
        (Instruction.mk ~cond (Opcode.V3d op) [ Operand.Dreg vd; Operand.Dreg vn; Operand.Dreg vm ])
  | Lowered.V3_s { cond; op; vd; vn; vm } ->
      Some
        (Instruction.mk ~cond (Opcode.V3s op) [ Operand.Sreg vd; Operand.Sreg vn; Operand.Sreg vm ])
  | Lowered.Vneg_d { cond; vd; vm } ->
      Some (Instruction.mk ~cond Opcode.Vneg_d [ Operand.Dreg vd; Operand.Dreg vm ])
  | Lowered.Vneg_s { cond; vd; vm } ->
      Some (Instruction.mk ~cond Opcode.Vneg_s [ Operand.Sreg vd; Operand.Sreg vm ])
  | Lowered.Vcmp_reg_d { cond; vd; vm } ->
      Some (Instruction.mk ~cond Opcode.Vcmp_d [ Operand.Dreg vd; Operand.Dreg vm ])
  | Lowered.Vcmp_reg_s { cond; vd; vm } ->
      Some (Instruction.mk ~cond Opcode.Vcmp_s [ Operand.Sreg vd; Operand.Sreg vm ])
  | Lowered.Vcmp_zero_d { cond; vd } ->
      Some (Instruction.mk ~cond Opcode.Vcmp_d [ Operand.Dreg vd; Operand.Imm Bigint.zero ])
  | Lowered.Vcmp_zero_s { cond; vd } ->
      Some (Instruction.mk ~cond Opcode.Vcmp_s [ Operand.Sreg vd; Operand.Imm Bigint.zero ])
  | Lowered.Vmrs { cond } ->
      Some
        (Instruction.mk ~cond Opcode.Vmrs
           [
             Operand.Sym (Asm_core.Expr.Symbol "APSR_nzcv");
             Operand.Sym (Asm_core.Expr.Symbol "FPSCR");
           ])
  | Lowered.Vcvt_f32_f64 { cond; sd; dm } ->
      Some (Instruction.mk ~cond Opcode.Vcvt_f32_f64 [ Operand.Sreg sd; Operand.Dreg dm ])
  | Lowered.Vcvt_f64_f32 { cond; dd; sm } ->
      Some (Instruction.mk ~cond Opcode.Vcvt_f64_f32 [ Operand.Dreg dd; Operand.Sreg sm ])
  | Lowered.Vcvt_f32_s32 { cond; sd; sm } ->
      Some (Instruction.mk ~cond Opcode.Vcvt_f32_s32 [ Operand.Sreg sd; Operand.Sreg sm ])
  | Lowered.Vcvt_f64_s32 { cond; dd; sm } ->
      Some (Instruction.mk ~cond Opcode.Vcvt_f64_s32 [ Operand.Dreg dd; Operand.Sreg sm ])
  | Lowered.Vcvt_f64_u32 { cond; dd; sm } ->
      Some (Instruction.mk ~cond Opcode.Vcvt_f64_u32 [ Operand.Dreg dd; Operand.Sreg sm ])
  | Lowered.Vcvt_s32_f64 { cond; sd; dm } ->
      Some (Instruction.mk ~cond Opcode.Vcvt_s32_f64 [ Operand.Sreg sd; Operand.Dreg dm ])
  | Lowered.Vmem_d { cond; load; vd; rn; offset } ->
      Some
        (Instruction.mk ~cond
           (if load then Opcode.Vldr else Opcode.Vstr)
           [
             Operand.Dreg vd;
             Operand.Mem { base = rn; offset = Mem.Imm offset; writeback = false; pre = true };
           ])
  | Lowered.Vmem_s { cond; load; vd; rn; offset } ->
      Some
        (Instruction.mk ~cond
           (if load then Opcode.Vldr else Opcode.Vstr)
           [
             Operand.Sreg vd;
             Operand.Mem { base = rn; offset = Mem.Imm offset; writeback = false; pre = true };
           ])
  | Lowered.Vldr_lit_d { cond; vd; target } ->
      Some
        (Instruction.mk ~cond Opcode.Vldr
           [
             Operand.Dreg vd;
             Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 (absolute_target target)));
           ])
  | Lowered.Vldr_lit_s { cond; vd; target } ->
      Some
        (Instruction.mk ~cond Opcode.Vldr
           [
             Operand.Sreg vd;
             Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 (absolute_target target)));
           ])
  | Lowered.Vpush { cond; vd; count } ->
      Some
        (Instruction.mk ~cond Opcode.Vpush
           [ Operand.Dreglist (List.init count (fun i -> vd.Dreg.num + i)) ])

let decode ctx bytes ~pos =
  if String.length bytes - pos < 4 then Error (diag ~pos:__POS__ `Decode_short)
  else
    let word = word_to_memory (String.sub bytes pos 4) in
    match C.decode_bits codec (C.Bits.of_bytes word) with
    | None -> Error (diag ~pos:__POS__ `Decode_no_match)
    | Some d -> (
        match instruction_of_lowered ~at:ctx.address d.C.value with
        | None -> Error (diag ~pos:__POS__ `Decode_no_normalized)
        | Some i -> Ok (i, String.concat "." d.C.dform, 4))

(* {1 Fixups, padding, directives} *)

let evaluate_fixup kind ~place ~target =
  (* [place] already carries the A32 pipeline offset: the caller adds the
     fixup's [pc_bias], which is 8 here. Adding it again - as this did while it
     was the only caller's convention - would double it now that x86 needs a
     per-form bias and the bias therefore lives in the fixup. *)
  match kind with
  | Abs32 -> Ok (Int64.logand target 0xFFFFFFFFL)
  | Pcrel_b26 | Pcrel_call ->
      let d = Int64.sub target place in
      if Int64.rem d 4L <> 0L then Error (diag ~pos:__POS__ `Branch_not_word_aligned)
      else
        let words = Int64.div d 4L in
        (* +-32MB. Checked here rather than left to field truncation: a branch
           that does not reach is a diagnostic, not a wrong address. *)
        if Int64.compare words (-0x800000L) >= 0 && Int64.compare words 0x800000L < 0 then Ok words
        else Error (diag ~pos:__POS__ `Branch_out_of_range)
  | Movw_abs_nc -> Ok (Int64.logand target 0xFFFFL)
  | Movt_abs -> Ok (Int64.logand (Int64.shift_right_logical target 16) 0xFFFFL)
  (* [vldr]'s literal form always adds ([U] is fixed to 1 in the codec, not a
     field this fixup carries) - every corpus occurrence is a trailing
     same-function literal pool, ahead of the instruction, so a backward
     target is a scope decision, not a truncation. *)
  | Pcrel_vldr8 ->
      let d = Int64.sub target place in
      if Int64.compare d 0L < 0 then Error (diag ~pos:__POS__ `Vldr_literal_backward)
      else if Int64.rem d 4L <> 0L then Error (diag ~pos:__POS__ `Vldr_literal_not_word_aligned)
      else
        let imm8 = Int64.div d 4L in
        if Int64.compare imm8 256L < 0 then Ok imm8
        else Error (diag ~pos:__POS__ `Vldr_literal_out_of_range)

(* A32 has an architectural no-op, so padding an executable section needs no
   table and no policy. A boundary that is not a multiple of four is rejected
   rather than padded with bytes, because a partial instruction is not a no-op. *)
(* On ARM [.word] is four bytes, not two, and CompCert writes [.4byte] for a
   32-bit address - both spellings mean the same width here. *)
let data_widths =
  [
    (".byte", 1);
    (".short", 2);
    (".hword", 2);
    (".word", 4);
    (".4byte", 4);
    (".long", 4);
    (".quad", 8);
  ]

let data_fixup ~width =
  match width with 4 -> Ok Abs32 | _ -> Error (diag ~pos:__POS__ (`No_data_relocation width))

let nop_bytes ~length =
  if length mod 4 <> 0 then Error (diag ~pos:__POS__ `Padding_not_word_multiple)
  else Ok (String.concat "" (List.init (length / 4) (fun _ -> "\x00\xf0\x20\xe3")))

(* Measured (M3 §3/§5, .ai/asm_plan.md §12): a linker-inserted merge gap in an
   executable section is plain zero fill on ARM, not NOP fill. *)
let merge_fill = None
