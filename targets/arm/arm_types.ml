(* The arm (A32) target's instruction types: registers, operands, conditions, opcodes, instructions
   and lowered forms. The encoder ({!Arm_encode}) builds on them, and so does the per-opcode table
   ({!Arm_ops}), which sits between the two. *)

open Foundation
module C = Codec

(* {1 Registers}

   One table with aliases, rather than a table plus an alias map: [sp] and [r13]
   must produce the same encoding and the same diagnostic, and two structures
   that have to agree eventually will not. A register is its number; {!name} is the
   canonical spelling, which is what the disassembler prints; an alias is only an input
   spelling. *)

module Reg = struct
  type t = { num : int }

  let equal a b = a.num = b.num
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

  let name r = canonical_name r.num
  let pp ppf r = Fmt.string ppf (name r)
  let of_num n = { num = n land 15 }

  let find spelling =
    match List.assoc_opt spelling aliases with
    | Some n -> Some (of_num n)
    | None ->
        List.find_map
          (fun n ->
            if String.equal (Printf.sprintf "r%d" n) spelling then Some (of_num n) else None)
          (List.init 16 Fun.id)

  (* The two registers the procedure-call standard fixes, named because an AST
     written by hand says [Reg.lr] far more often than it says [r14]. *)
  let sp = of_num 13
  let lr = of_num 14

  (* The register class as numbers (REGS), and a value per register. *)
  let numbers = List.init 16 Fun.id
  let count = 16
  let mem n = n >= 0 && n < 16
  let of_int n = if mem n then Some (of_num n) else None

  let of_int_exn n =
    match of_int n with Some r -> r | None -> invalid_arg "Reg.of_int_exn: no such register"

  let to_int r = r.num
  let all = List.map of_num numbers
  let range = Some (0, 16 - 1)
  let r0 = of_num 0
  let r1 = of_num 1
  let r2 = of_num 2
  let r3 = of_num 3
  let r4 = of_num 4
  let r5 = of_num 5
  let r6 = of_num 6
  let r7 = of_num 7
  let r8 = of_num 8
  let r9 = of_num 9
  let r10 = of_num 10
  let r11 = of_num 11
  let r12 = of_num 12
  let r13 = of_num 13
  let r14 = of_num 14
  let r15 = of_num 15
  let pc = of_num 15
  let ip = of_num 12
  let fp = of_num 11
  let sl = of_num 10
end

(* {1 VFP registers}

   Sixteen doubleword (D0-D15) and thirty-two singleword (S0-S31) extension
   registers - VFPv2's range, which is what the compiler's ARM codegen ever
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

  (* The register class as numbers (REGS), and a value per register. *)
  let numbers = List.init 32 Fun.id
  let count = 32
  let mem n = n >= 0 && n < 32
  let of_int n = if mem n then Some (of_num n) else None

  let of_int_exn n =
    match of_int n with Some r -> r | None -> invalid_arg "Dreg.of_int_exn: no such register"

  let to_int r = r.num
  let all = List.map of_num numbers
  let range = Some (0, 32 - 1)
  let d0 = of_num 0
  let d1 = of_num 1
  let d2 = of_num 2
  let d3 = of_num 3
  let d4 = of_num 4
  let d5 = of_num 5
  let d6 = of_num 6
  let d7 = of_num 7
  let d8 = of_num 8
  let d9 = of_num 9
  let d10 = of_num 10
  let d11 = of_num 11
  let d12 = of_num 12
  let d13 = of_num 13
  let d14 = of_num 14
  let d15 = of_num 15
  let d16 = of_num 16
  let d17 = of_num 17
  let d18 = of_num 18
  let d19 = of_num 19
  let d20 = of_num 20
  let d21 = of_num 21
  let d22 = of_num 22
  let d23 = of_num 23
  let d24 = of_num 24
  let d25 = of_num 25
  let d26 = of_num 26
  let d27 = of_num 27
  let d28 = of_num 28
  let d29 = of_num 29
  let d30 = of_num 30
  let d31 = of_num 31
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

  (* The register class as numbers (REGS), and a value per register. *)
  let numbers = List.init 32 Fun.id
  let count = 32
  let mem n = n >= 0 && n < 32
  let of_int n = if mem n then Some (of_num n) else None

  let of_int_exn n =
    match of_int n with Some r -> r | None -> invalid_arg "Sreg.of_int_exn: no such register"

  let to_int r = r.num
  let all = List.map of_num numbers
  let range = Some (0, 32 - 1)
  let s0 = of_num 0
  let s1 = of_num 1
  let s2 = of_num 2
  let s3 = of_num 3
  let s4 = of_num 4
  let s5 = of_num 5
  let s6 = of_num 6
  let s7 = of_num 7
  let s8 = of_num 8
  let s9 = of_num 9
  let s10 = of_num 10
  let s11 = of_num 11
  let s12 = of_num 12
  let s13 = of_num 13
  let s14 = of_num 14
  let s15 = of_num 15
  let s16 = of_num 16
  let s17 = of_num 17
  let s18 = of_num 18
  let s19 = of_num 19
  let s20 = of_num 20
  let s21 = of_num 21
  let s22 = of_num 22
  let s23 = of_num 23
  let s24 = of_num 24
  let s25 = of_num 25
  let s26 = of_num 26
  let s27 = of_num 27
  let s28 = of_num 28
  let s29 = of_num 29
  let s30 = of_num 30
  let s31 = of_num 31
end

(* The low registers r0-r7: the ones most 16-bit Thumb encodings reach. Reserved: the target does
   not lower Thumb, so no instruction takes one yet. *)
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
end = struct
  type t = Reg.t

  let numbers = List.init 8 Fun.id
  let count = 8
  let mem n = n >= 0 && n < 8
  let of_reg (r : Reg.t) = if mem r.Reg.num then Some r else None
  let to_reg r = r
  let name = Reg.name
  let of_int n = if mem n then Some (Reg.of_num n) else None

  let of_int_exn n =
    match of_int n with Some r -> r | None -> invalid_arg "Lo.of_int_exn: no such register"

  let to_int (r : Reg.t) = r.Reg.num
  let all = List.map Reg.of_num numbers
  let range = Some (0, 7)
  let r0 = Reg.r0
  let r1 = Reg.r1
  let r2 = Reg.r2
  let r3 = Reg.r3
  let r4 = Reg.r4
  let r5 = Reg.r5
  let r6 = Reg.r6
  let r7 = Reg.r7
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
          Printf.sprintf ", %s%s" (if negate then "-" else "") (Reg.name reg)
      | Reg_offset { reg; negate; kind; amount } ->
          Printf.sprintf ", %s%s, %s #%d"
            (if negate then "-" else "")
            (Reg.name reg) (shift_name kind) amount
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
        (** [{r0, r1, r2, r3}] - [push]'s operand (M5). Register *numbers*, not
            {!Reg.t}s: the encoding is one bit per number and the surface order carries no
            information the bitmask does not already have, so keeping the list unsorted-as-written
            here would only invite a caller to rely on an order the instruction itself discards. *)
    | Reg_writeback of Reg.t
        (** [Rn!] with no brackets - [stmia]/[ldmia]'s own writeback base register (M5,
            the corpus notes classify-c-gcc: [stmia r3!, {r0, r1}]), a bare-register grammar
            distinct from {!Mem.t}'s bracketed [\[Rn, #imm\]!] since STM/LDM's writeback amount is
            implicit rather than an explicit offset. *)
    | Dreglist of int list
        (** [{d8, d9}] - [vpush.64]'s D-register list (M5, the corpus notes classify-c-gcc:
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
  type op = Vadd | Vsub | Vmul | Vdiv | Vmla
end

module Opcode = struct
  type t =
    | Mov
    | Add
    | Sub
    | And
    | Cmp
    | Cmn
        (** [cmn rn, #imm] - compare negative ([rn + op2], flags only, dp = 11), {!Cmp}'s own sibling
            test instruction: same [Rd = 0]/[S = 1]-forced shape {!is_compare} already generalizes,
            just a different [dp] value (M5 - gas_frontier.t's
            runtime-i64_dtos.S/i64_dtou.S). Evidenced only as the immediate form. *)
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
        (** [adc rd, rn, rm[, shift]] / [adc rd, rn, #imm] - add with carry, the same
            data-processing family as {!And}/{!Orr}/{!Bic}. The register form is evidenced in
            [siphash24.c], the upper half of a 64-bit add {!Adds} already
            covers the lower half of; the immediate form ([adc rd, rn, #0], propagating a carry
            with no other addend) in gas_frontier.t's runtime-i64_smulh.S/i64_umulh.S's own
            128-bit-product carry-chain idiom. *)
    | Sbc
        (** [sbc rd, rn, rm] - subtract with carry, {!Adc}'s exact mirror (dp = 6), evidenced only in
            the same plain register-register-register shape (M5, the corpus notes -
            gas_frontier.t's runtime-i64_dtos.S/i64_sdiv.S/i64_smod.S, the upper half of a 64-bit
            subtract {!Subs} covers the lower half of). *)
    | Adds
        (** [adds rd, rn, rm] / [adds rd, rn, #imm] - {!Add}'s own flag-setting form (dp = 4,
            [s] = 1). The register form is evidenced as gcc's own carry-out idiom
            ([siphash24.c]'s 64-bit add built from two 32-bit halves); the immediate form as
            gas_frontier.t's runtime-i64_stof.S's own round-to-nearest carry-increment idiom
            ([adds r2, r2, #1], M5, the corpus notes). A [dp] value alone cannot tell [add] and
            [adds] apart - only the already-present [s] field can - so this is its own opcode with
            its own dedicated lowering/printing case, the same relationship {!Cmp} already has to
            being "[sub] with [s] = 1 and no destination": {!to_dp} still maps it to 4, but it is
            deliberately excluded from the generic [Add]/[Sub]/[And]/... lowering arm, which always
            produces [s] = false. *)
    | Subs
        (** [subs rd, rn, rm] - {!Sub}'s own flag-setting form (dp = 2, [s] = 1), {!Adds}'s exact
            counterpart on the other side of the family (M5 - gas_frontier.t's
            runtime-i64_sdiv.S/i64_smod.S own absolute-value idiom). Register-register-register only,
            the one shape evidenced. *)
    | Rsbs
        (** [rsbs rd, rn, #imm] - {!Rsb}'s own flag-setting form (dp = 3, [s] = 1), evidenced only as
            the immediate form (M5 - gas_frontier.t's runtime-i64_sar.S's own
            "32 - amount" idiom). *)
    | Orrs
        (** [orrs rd, rn, rm] - {!Orr}'s own flag-setting form (dp = 12, [s] = 1), evidenced only as
            the register form, its result used only as a zero test (M5, the corpus notes -
            gas_frontier.t's runtime-i64_udivmod.S). *)
    | Adcs
        (** [adcs rd, rn, rm] - {!Adc}'s own flag-setting form (dp = 5, [s] = 1), {!Adds}'s own
            pattern applied to add-with-carry: evidenced only in this one register-register-
            register shape, the upper half of a 64-bit product's carry-propagating accumulation
            (M5 - gas_frontier.t's runtime-i64_smulh.S/i64_umulh.S). *)
    | Sbcs
        (** [sbcs rd, rn, rm] - {!Sbc}'s own flag-setting form (dp = 6, [s] = 1), {!Adcs}'s exact
            mirror on the subtract side, evidenced only in the same register-register-register
            shape (M5 - gas_frontier.t's runtime-i64_smulh.S's own signed
            128-bit-product borrow-chain idiom). *)
    | Mul
    | Mla
    | Umull
        (** [umull RdLo, RdHi, Rn, Rm] - unsigned multiply long, a genuinely different word shape
            from {!Mul}/{!Mla} (two destination registers, RdLo:RdHi, rather than one): bits
            27:23 are a fixed [00001], not the [dp]/multiply-short prefixes any other opcode uses,
            so it cannot overlap them (M5 - gas_frontier.t's
            runtime-i64_smulh.S/i64_umulh.S's own 64-bit product idiom, cross-multiplying each
            operand's low/high 32-bit halves). Only the plain, unsigned, non-accumulating,
            non-flag-setting form is evidenced; {!Lowered.Umull}'s [s] field exists for the same
            reason {!Mul}/{!Mla}'s own [s] does (decode must be able to represent it) even though
            nothing here ever lowers a [true] value, and the signed ([smull]) / accumulate
            ([umlal]/[smlal]) siblings are not implemented at all. *)
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
        (** [it]/[ite] - the Thumb IT-block prefix. GAS accepts both spellings (and every other
            IT-block letter combination) in A32 source and emits nothing for any of them, so this
            one constructor covers all of them; see {!simplify_instruction}'s own comment (M5,
            the corpus notes - gas_frontier.t's runtime-i64_udivmod.S's own bare [it eq]). *)
    | Nop
        (** the dedicated ARMv7 hint encoding ([e320f000], the same bytes {!nop_bytes} already pads
            with) - not a data-processing alias the way {!Mvn}'s [mov]-family siblings are. *)
    | Shift of int
        (** [lsl]/[lsr]/[asr]/[ror rd, rm, rs] - GNU's own mnemonics for {!Mov} with a
            register-specified shift amount, a different word shape from the already-supported
            immediate-shift-amount one ({!Lowered.Dp_reg}'s [sh_amt]: bit 4 is the immediate/register
            discriminator, and the compiler's own codegen only ever emits the register form under these
            names, never [mov rd, rm, TYPE rs]). [int] is the shared shift-kind encoding
            {!shift_of_name}/{!shift_name} already use. *)
    | Shifts of int
        (** [lsls]/[lsrs]/[asrs]/[rors rd, rm, #imm] - {!Shift}'s own flag-setting sibling on the
            immediate-shift-amount form only ([s] = 1 on the identical [mov rd, rm, <shift> #imm]
            shape; evidenced only as [lsrs], M5, the corpus notes - gas_frontier.t's
            runtime-i64_utof.S). A [dp]/[sh_kind] pair alone cannot tell [lsr #imm] from [lsrs #imm]
            apart - only [s] can, the same relationship {!Adds} already has to {!Add} - so this is
            its own opcode with its own dedicated lowering/decoding case rather than a field on
            {!Shift}. No register-shift-amount ([lsrs rd, rm, rs]) form is evidenced or attempted. *)
    | Rrx
        (** [rrx rd, rm] - GAS's dedicated two-operand mnemonic for rotate-right-with-extend
            (rotate through the carry flag by exactly one bit), encoded as [mov rd, rm, ror #0]
            - a ROR immediate shift of 0 means RRX rather than "no shift" the way an LSL 0 would
            (M5 - gas_frontier.t's runtime-i64_udivmod.S's own bare [rrx r2,
            r2]). Kept separate from {!Shift}/{!Shifts} rather than reusing [Shift 3] with an
            implicit zero, since [ror rd, rm] (no immediate at all) is not evidenced and this
            keeps {!Shift}'s own three-operand-only scope untouched. Byte-identical to
            [mov rd, rm, ror #0] either way (verified against real arm-linux-gnueabihf-as); no
            dedicated printer is added, the same judgment call {!Shift}'s own comment explains -
            correctness here is byte-level, not spelling-level. *)
    | Rrxs  (** [rrxs rd, rm] - {!Rrx}'s own flag-setting sibling, [movs rd, rm, ror #0]. *)
    | Vmov_d  (** [vmov.f64 Dd, Dm] or [vmov.f64 Dd, #imm] - operand-shape dispatched *)
    | Vmov_s  (** [vmov.f32 Sd, Sm] or [vmov.f32 Sd, #imm] *)
    | Vmov_core  (** [vmov Rt, Sn] / [vmov Sn, Rt] / [vmov Rt, Rt2, Dm] / [vmov Dm, Rt, Rt2] *)
    | V3d of V3.op  (** [vadd.f64]/[vsub.f64]/[vmul.f64]/[vdiv.f64]/[vmla.f64], double-precision *)
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
            reglist push (M5: almabench.c/fft.c/fftsp.c/perlin.c's own
            varargs-spill prologue). Unlike {!Push}'s STMDB encoding, there is no separate
            one-register alias to exclude: VPUSH's own encoding covers a single D register
            exactly as it covers several (verified against real
            arm-linux-gnueabihf-as/objdump), so {!Operand.Dreglist} is accepted from one
            member up, provided it is contiguous and ascending. *)
    | Push
        (** [push {reglist}], GNU's alias for [stmdb sp!, {reglist}] - not a general STM/LDM (M5,
            the corpus notes: the compiler's own ARM codegen only ever emits this one multi-register
            alias, always as its varargs-spill prologue). Scoped to two or more registers: GNU's own
            canonical printer spells a *one*-register [push {rN}] as [str rN, [sp, #-4]!] instead
            (verified against real [as]/[objdump]), a different encoding this corpus does not
            evidence and this opcode does not attempt. *)
    | Stmia
        (** [stmia Rn!, {reglist}] - increment-after STM, a different encoding class from
                 [push]'s STMDB one (gcc's [aes.c]). Always carries the bare
                 [Rn!] writeback base as an explicit operand, the only shape evidenced. *)
    | Ldmia
        (** [ldmia Rn!, {reglist}] - [Stmia]'s load counterpart, same encoding class with the
                 L bit set. *)
    | Pop
        (** [pop {reglist}], GNU's alias for [ldmia sp!, {reglist}] - the LDM-class counterpart of
            [Push]'s STMDB alias ([gas_frontier.t]'s [i64_udivmod]/[i64_umod]
            epilogues). Scoped to two or more registers for the same reason as [Push]: GNU spells a
            one-register [pop {rN}] as [ldr rN, [sp], #4] instead (verified against real
            [as]/[objdump]), a different encoding not attempted here. *)
end

module Instruction = struct
  type t = { op : Opcode.t; cond : Cond.t; ops : Operand.t list }

  let mk ?(cond = Cond.Al) op ops = { op; cond; ops }
end

(* {1 Lowered forms}

   Named after the *encoding class* rather than the mnemonic, because A32
   encodes add, sub and mov as one data-processing format distinguished by a
   4-bit opcode field. Three constructors named add/sub/mov would each have to
   carry that field anyway, and could then disagree with their own name. *)

(* The offset of an extra load/store ({!Lowered.Ldst_x}): an 8-bit magnitude with its sign,
   or a register, added or subtracted. *)
type x_offset = X_imm of int64 | X_reg of { rm : Reg.t; negate : bool }

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
        post : bool;
            (** [\[rn\], #offset]: access at [rn], then add [offset] to it (P = 0, W = 0).
                the compiler's inline [memcpy] loop copies that way. *)
      }
    | Ldst_x of {
        cond : Cond.t;
        kind : int;
        rt : Reg.t;
        rn : Reg.t;
        offset : x_offset;
        post : bool;
      }
        (** The "extra" loads and stores - [strh]/[ldrh]/[ldrsb]/[ldrsh] ([kind] 0-3, see
            {!x_kinds}) - with an 8-bit immediate or a plain register offset, at the offset
            ([\[rn, off\]]) or post-indexed ([\[rn\], off]). Embedded-corpus evidence:
            the compiler's [short] and [signed char] accesses, and its halfword [memcpy] loop. *)
    | Rd_rm of { cond : Cond.t; op : Opcode.t; rd : Reg.t; rm : Reg.t }
        (** [clz]/[rev]/[rev16 rd, rm] ({!rd_rm_ops}; compiler-output evidence: the compiler's
            [__builtin_clz*] and [__builtin_bswap*]). *)
    | Sbfx of { cond : Cond.t; rd : Reg.t; rn : Reg.t; lsb : int; width : int }
        (** [sbfx rd, rn, #lsb, #width] - signed bitfield extract (compiler-output evidence:
            the compiler's sign extension of a narrow value). *)
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
            . Always carries [rn] and forces W=1: the only evidenced shape is
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
    | Umull of { cond : Cond.t; s : bool; rdlo : Reg.t; rdhi : Reg.t; rn : Reg.t; rm : Reg.t }
        (** [umull RdLo, RdHi, Rn, Rm] - unsigned multiply long, {!Opcode.Umull}'s own doc comment
            has the full word-shape detail. *)
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
end
