(* Shared x86 encoding machinery: registers, addressing, REX, ModR/M, SIB,
   displacements and immediates (.ai/asm_plan.md §5.1).

   The only family package. Everything here is built from the generic
   Seq/Iso/Alt combinators, and the x86 vocabulary lives here rather than in
   lib/codec - which is what keeps the codec EDSL target-agnostic and what
   tools/asm-check-layers.sh enforces.

   The family parameter [MODE] exposes real differences, per §5.1: register
   availability, default operand size, and whether a REX byte may exist at all.
   It is not a name: x86_32 and x86_64 differ in what they can encode, and a
   mode that only supplied a string would let a 32-bit assembly acquire a REX
   prefix from a shared code path.

   Byte order. The codec lays bits down most-significant first, so an x86 form
   is authored as a sequence of 8-bit chunks in memory order and the packing is
   memory order. Multi-byte immediates and displacements are little-endian,
   which is a real part of the encoding rather than an output-stage detail, so
   it appears in the tree as an [Iso_fun] ({!le}) - [inspect] then shows it and
   [decode] inverts it for free. *)

open Foundation
module C = Codec
module X86_table_row = X86_table_row
module X86_table_rows = X86_table_rows

(* {1 Registers}

   [num] is the architectural number 0-15, so [num land 7] is the ModR/M or
   SIB field and [num >= 8] is the REX extension bit. Keeping one number
   rather than a pair is what makes "did this operand need REX.B" a property of
   the register instead of a fact the caller has to remember to carry. *)

module Reg = struct
  type t = { name : string; num : int; width : int }

  let equal a b = a.num = b.num && a.width = b.width
  let pp ppf r = Fmt.pf ppf "%%%s" r.name
  let names_32 = [| "eax"; "ecx"; "edx"; "ebx"; "esp"; "ebp"; "esi"; "edi" |]
  let names_64 = [| "rax"; "rcx"; "rdx"; "rbx"; "rsp"; "rbp"; "rsi"; "rdi" |]
  let names_16 = [| "ax"; "cx"; "dx"; "bx"; "sp"; "bp"; "si"; "di" |]
  let names_8l = [| "al"; "cl"; "dl"; "bl"; "spl"; "bpl"; "sil"; "dil" |]

  (* [xmm0]-[xmm15], one flat array rather than an 8-wide base plus a
     REX-extended tail the way the GPR names above are split: xmm's naming
     doesn't have a legacy/extended-spelling distinction the way [al] vs.
     [r8b] does, so all sixteen go through [base_regs] at once. Width 128 is
     not a real operand width any encoding ever selects by - SSE forms fix
     their own width dimension by opcode, not by suffix - it exists here only
     so a register's [.width] keeps doubling as its class marker
     ([reg_at]/[retype] already key purely on [(width, num)]), the same way
     8/16/32/64 already mark the four GPR widths. *)
  let names_xmm = Array.init 16 (fun i -> Printf.sprintf "xmm%d" i)

  (* [ymm0]-[ymm15] (AVX): {!names_xmm}'s own 256-bit sibling, width 256 serving as the
     class marker for the same reason 128 does for xmm - a VEX instruction's [L] bit is read off
     its operand registers' width, not carried separately. *)
  let names_ymm = Array.init 16 (fun i -> Printf.sprintf "ymm%d" i)

  (* [zmm0]-[zmm15] (AVX-512): {!names_ymm}'s own 512-bit sibling, width 512 as the class
     marker for EVEX.L'L = 2. [zmm16]-[zmm31] need the EVEX [R'], [V'] and [X] extension bits and
     are not modelled yet. *)
  let names_zmm = Array.init 16 (fun i -> Printf.sprintf "zmm%d" i)

  (* [mm0]-[mm7] (MMX) and [k0]-[k7] (AVX-512 opmasks), used only by generated table rows; their
     widths are {!X86_table_row.class_width}'s class markers. *)
  (* [xmm16]-[zmm31]: EVEX's upper sixteen, x86-64 only and used only by generated EVEX rows *)
  let evex_upper =
    List.concat_map
      (fun (prefix, width) ->
        List.init 16 (fun i ->
            { name = Printf.sprintf "%s%d" prefix (i + 16); num = i + 16; width }))
      [ ("xmm", 128); ("ymm", 256); ("zmm", 512) ]

  let mm_and_k =
    List.init 8 (fun i ->
        { name = Printf.sprintf "mm%d" i; num = i; width = X86_table_row.class_width Mmx })
    @ List.init 8 (fun i ->
        { name = Printf.sprintf "k%d" i; num = i; width = X86_table_row.class_width Kmask })
    @ List.init 8 (fun i ->
        { name = Printf.sprintf "tmm%d" i; num = i; width = X86_table_row.class_width Tmm })

  let base_regs width names =
    Array.to_list (Array.mapi (fun i n -> { name = n; num = i; width }) names)

  let extended_regs width suffix =
    List.init 8 (fun i -> { name = Printf.sprintf "r%d%s" (i + 8) suffix; num = i + 8; width })

  (* Lookup by spelling over a mode's own register set. The set is a [MODE]
     field rather than a constant here, because [%rax] exists in 64-bit mode
     and does not exist in 32-bit mode, and a shared table would have to be
     filtered at every use. *)
  let find_in registers name = List.find_opt (fun r -> String.equal r.name name) registers
end

(* {1 Memory operands}

   [scale] is the multiplier (1, 2, 4 or 8), not its logarithm, because that is
   what the surface syntax writes and converting once at the encoder is one
   place to be wrong instead of every place that builds a [mem]. *)

(* A memory operand's displacement is either a number or an expression the
   linker will resolve. It has to be a sum rather than a number beside an
   optional symbol: the two fields could disagree, and "displacement 5 and also
   the symbol g" has no meaning. An addend rides inside the expression, which is
   where GAS puts it too - [g+4(%%rip)] is one expression. *)
module Disp = struct
  type t = Const of int64 | Sym of Asm_core.Expr.t

  let equal a b =
    match (a, b) with
    | Const x, Const y -> Int64.equal x y
    (* Structural: [Expr.t] is a first-order tree of constants and names, so
       this is the same equality the ladder validator uses on a fixup value. *)
    | Sym x, Sym y -> x = y
    | _ -> false

  let zero = Const 0L
  let is_zero = function Const v -> Int64.equal v 0L | Sym _ -> false

  (* What goes in the bits before the linker gets there: a placeholder for
     anything symbolic, exactly as a branch target does. *)
  let placeholder = function Const v -> v | Sym _ -> 0L
  let to_string = function Const v -> Int64.to_string v | Sym e -> Asm_core.Expr.to_string e
end

module Mem = struct
  type t = { base : Reg.t option; index : Reg.t option; scale : int; disp : Disp.t }

  let equal a b =
    Option.equal Reg.equal a.base b.base
    && Option.equal Reg.equal a.index b.index
    && a.scale = b.scale && Disp.equal a.disp b.disp

  let pp ppf m =
    let disp = if Disp.is_zero m.disp then "" else Disp.to_string m.disp in
    match (m.base, m.index) with
    | None, None -> Fmt.string ppf (Disp.to_string m.disp)
    | Some b, None -> Fmt.pf ppf "%s(%a)" disp Reg.pp b
    | Some b, Some i -> Fmt.pf ppf "%s(%a,%a,%d)" disp Reg.pp b Reg.pp i m.scale
    | None, Some i -> Fmt.pf ppf "%s(,%a,%d)" disp Reg.pp i m.scale

  (* [disp(%base)] with no index, which is every memory operand the M1 fixtures
     contain and the shape a hand-written AST reaches for first. *)
  let of_base ?(disp = Disp.zero) base = { base = Some base; index = None; scale = 1; disp }
end

module Operand = struct
  (* [Sym] is a bare symbolic operand: a branch or call target. It is separate
     from [Imm] rather than an [Imm] holding an expression because the two are
     encoded differently - an immediate is a value in the instruction, a target
     becomes a fixup - and because AT&T spells the first with a [$] and the
     second without.

     [Imm_sym] (M5 classify-c-gcc corpus evidence: `movl $.LC0, %edi`,
     `addq $bodies+24, %rax`, `pushl $sym`, ...) is the [$]-spelled sibling
     [Sym] is not: a genuine immediate whose value is a symbol's address
     rather than a literal number - gcc's own idiom for materializing a
     string/array address into a register or the stack, which ccomp's codegen
     (this project's only source of fixtures until now) never emitted. Kept
     as its own constructor rather than widening [Imm] to
     [Bigint.t Asm_core.Expr.t]-like union, so every existing [Imm v] site
     stays exactly as narrow as it was. All three evidenced forms now lower
     it: [mov]'s register-destination form ({!Lowered.Mov_r_imm}), the
     ALU-immediate form ({!Lowered.Alu_rm_imm}), and [push]'s immediate form
     ({!Lowered.Push_imm}) - see asm/docs/corpus.md's classify-c-gcc
     section. *)
  type t =
    | Reg of Reg.t
    | Mem of Mem.t
    | Imm of Bigint.t
    | Imm_sym of Asm_core.Expr.t
    | Sym of Asm_core.Expr.t
    | Dfv of int  (** APX [{dfv=...}]: OF 8, SF 4, ZF 2, CF 1 *)
    | Masked of { op : t; k : int; zero : bool }
        (** an EVEX destination with an opmask: [%zmm0{%k1}], [{z}] for zeroing *)
    | Rc of int
        (** EVEX embedded rounding: [{rn-sae}] 0, [{rd-sae}] 1, [{ru-sae}] 2, [{rz-sae}] 3; or
            [{sae}] 4, exceptions suppressed with no rounding override *)

  let rc_name = function
    | 0 -> "rn-sae"
    | 1 -> "rd-sae"
    | 2 -> "ru-sae"
    | 3 -> "rz-sae"
    | _ -> "sae"

  let rec pp ppf = function
    | Reg r -> Reg.pp ppf r
    | Mem m -> Mem.pp ppf m
    | Imm v -> Fmt.pf ppf "$%a" Bigint.pp v
    | Imm_sym e -> Fmt.pf ppf "$%s" (Asm_core.Expr.to_string e)
    | Sym e -> Fmt.string ppf (Asm_core.Expr.to_string e)
    | Rc n -> Fmt.pf ppf "{%s}" (rc_name n)
    | Dfv v ->
        Fmt.pf ppf "{dfv=%s}"
          (String.concat ","
             (List.filter_map
                (fun (bit, n) -> if v land bit <> 0 then Some n else None)
                [ (8, "of"); (4, "sf"); (2, "zf"); (1, "cf") ]))
    | Masked { op; k; zero } -> Fmt.pf ppf "%a{%%k%d}%s" pp op k (if zero then "{z}" else "")
end

(* {1 The three staged instruction types} *)

module Surface = struct
  type t = { mnemonic : string; ops : Operand.t list; origin : Origin.t }

  (* AT&T operand order throughout: source first, destination last.
     Canonicalizing to Intel order here would make every diagnostic disagree
     with the source the user wrote. *)
  let pp ppf s =
    match s.ops with
    | [] -> Fmt.string ppf s.mnemonic
    | ops -> Fmt.pf ppf "%s %a" s.mnemonic Fmt.(list ~sep:(any ", ") Operand.pp) ops
end

(* {1 Condition codes}

   The 4-bit [tttn] field, declared in its encoding order so the code *is* the
   position and the two cannot disagree. All sixteen are named although the M2
   fixtures use four: the field is a complete finite domain, which is what lets
   {!Codec.check} decide the table's injectivity and totality instead of the
   author asserting them.

   One condition has several accepted spellings - [jc] and [jnae] are [jb], [jz]
   is [je], [jnge] is [jl] - and they are input-only. A canonical dump has one
   spelling per form, so {!name} is the single spelling that comes back out, and
   it is objdump's, which is what the differential gate compares against. *)
module Cc = struct
  type t = O | No | B | Ae | E | Ne | Be | A | S | Ns | P | Np | L | Ge | Le | G

  let all = [ O; No; B; Ae; E; Ne; Be; A; S; Ns; P; Np; L; Ge; Le; G ]

  let name = function
    | O -> "o"
    | No -> "no"
    | B -> "b"
    | Ae -> "ae"
    | E -> "e"
    | Ne -> "ne"
    | Be -> "be"
    | A -> "a"
    | S -> "s"
    | Ns -> "ns"
    | P -> "p"
    | Np -> "np"
    | L -> "l"
    | Ge -> "ge"
    | Le -> "le"
    | G -> "g"

  let equal (a : t) b = a = b

  let code c =
    let rec go i = function [] -> -1 | x :: rest -> if equal x c then i else go (i + 1) rest in
    go 0 all

  let of_code n = List.nth_opt all n

  let synonyms =
    [
      ("c", B);
      ("nae", B);
      ("nb", Ae);
      ("nc", Ae);
      ("z", E);
      ("nz", Ne);
      ("na", Be);
      ("nbe", A);
      ("pe", P);
      ("po", Np);
      ("nge", L);
      ("nl", Ge);
      ("ng", Le);
      ("nle", G);
    ]

  let of_name s =
    match List.find_opt (fun c -> String.equal (name c) s) all with
    | Some c -> Some c
    | None -> List.assoc_opt s synonyms

  (* Longest first, so [jnle] is read as [nle] rather than as [n] followed by
     junk: several condition names are prefixes of others. *)
  let split_after prefix m =
    let p = String.length prefix and n = String.length m in
    if n <= p || String.sub m 0 p <> prefix then None
    else
      let tail = String.sub m p (n - p) in
      Option.map (fun c -> c) (of_name tail)
end

module Opcode = struct
  (** [sahf] - load [%ah] into the flags register ([0x9E]), bare, no operand,
            {!Fucomp}/{!Fnstsw}'s exact fixed-opcode shape (M5, asm/docs/corpus.md -
            same fixture as {!Fnstsw}). *)
  type t =
    | Add
    | Sub
    | Mov
    | Lea
    | Ret
    | Xor
    | And
    | Cmp
    | Imul
    | Cmov of Cc.t
    | Jcc of Cc.t
    | Ud2
    | Pop
    | Jmp
    | Call
    | Push
    | Neg
    | Test
    | Adc
    | Sbb
    | Mul
    | Div
    | Dec
    | Rcr
    | Shr
    | Or
    | Not
    | Ror
    | Shl
    | Sar
    | Shld
    | Setcc of Cc.t
    | Movzx of { src_width : int }
    | Movsx of { src_width : int }
    | Addsd
    | Subsd
    | Mulsd
    | Divsd
    | Addss
    | Subss
    | Mulss
    | Divss
    | Comisd
    | Ucomisd
        (** [ucomisd rm, reg] - unordered scalar double compare ([66 0F 2E /r]), {!Comisd}'s exact
            sibling in the same [sse_binop_66_codec] table, one more opcode byte at the same
            mandatory-prefix group (M5, asm/docs/corpus.md - gas_frontier.t's
            runtime-i64_dtou.S's own float-to-unsigned range test). *)
    | Comiss
    | Ucomiss
        (** [ucomiss rm, reg] - unordered scalar single compare ([0F 2E /r], no mandatory
            prefix), {!Comiss}'s exact sibling in the same [sse_binop_none_codec] table, one
            more opcode byte at the same mandatory-prefix-free group - overlooked alongside
            {!Ucomisd}'s own group when the comparison family was first admitted. *)
    | Xorpd
    | Pxor
        (** [pxor rm, reg] - packed bitwise XOR ([66 0F EF /r]), {!Xorpd}'s own mandatory-prefix
            group at a different opcode byte, evidenced only as the register-register
            self-zeroing idiom [pxor %xmmN, %xmmN] (M5, asm/docs/corpus.md - gas_frontier.t's
            runtime-i64_utod.S/i64_utof.S, priming an accumulator ahead of an integer-to-float
            conversion). *)
    | Movapd
    | Cvtsd2ss
    | Cvtss2sd
    | Cvtps2pd
        (** [cvtps2pd rm, reg] - packed single-to-double conversion ([0F 5A /r], no mandatory
            prefix), {!Cvtsd2ss}/{!Cvtss2sd}'s own opcode byte at the two remaining
            mandatory-prefix groups (none/66 instead of F2/F3). Both register-register and
            register<-memory confirmed unambiguous against real GNU as at this legacy encoding
            (no VEX.L concept here at all) - see {!Vcvtpd2ps}'s own comment for the real
            ambiguity its VEX sibling hits. *)
    | Cvtpd2ps
        (** [cvtpd2ps rm, reg] - packed double-to-single conversion ([66 0F 5A /r]),
            {!Cvtps2pd}'s mandatory-66 counterpart at the same opcode byte. Also unambiguous
            both ways at this legacy encoding, confirmed against real GNU as. *)
    | Cvtdq2ps
        (** [cvtdq2ps rm, reg] - packed doubleword-integer-to-single-precision conversion
            ([0F 5B /r], no mandatory prefix), {!Cvtps2pd}/{!Cvtpd2ps}'s own opcode-0x5B sibling
            (a three-way none/66/F3 prefix split, no F2 member). Both directions confirmed
            unambiguous against real GNU as. *)
    | Cvtps2dq
        (** [cvtps2dq rm, reg] - single-precision-to-packed-doubleword-integer conversion
            ([66 0F 5B /r]), {!Cvtdq2ps}'s mandatory-66 counterpart. *)
    | Cvttps2dq
        (** [cvttps2dq rm, reg] - the truncating variant ([F3 0F 5B /r]), {!Cvtdq2ps}'s
            mandatory-F3 counterpart. *)
    | Movsd
    | Movss
    | Cvtsi2sd
    | Cvtsi2ss
    | Cvttsd2si
    | Unpcklps
        (** [unpcklps rm, reg] - packed interleave, low half, single precision ([0F 14 /r], no
            mandatory prefix), the first genuine two-source-operand packed binop in
            {!sse_binop_none_codec}'s own mandatory-prefix-free group ({!Andps}'s and
            {!Comiss}'s group), a different opcode byte from either. *)
    | Unpckhps
        (** [unpckhps rm, reg] - packed interleave, high half, single precision ([0F 15 /r]),
            {!Unpcklps}'s sibling. *)
    | Unpcklpd
        (** [unpcklpd rm, reg] - packed interleave, low half, double precision ([66 0F 14 /r]),
            {!Unpcklps}'s mandatory-66-prefix counterpart at the same opcode byte. *)
    | Unpckhpd
        (** [unpckhpd rm, reg] - packed interleave, high half, double precision ([66 0F 15 /r]),
            {!Unpcklpd}'s sibling. *)
    | Punpcklqdq
        (** [punpcklqdq rm, reg] - packed integer interleave, low half, quadword ([66 0F 6C /r]),
            {!Unpcklps}'s own shape at a different opcode byte, 66-mandatory-prefix only (no
            non-66 sibling - this is an integer SIMD op, not a float one). *)
    | Punpckhqdq
        (** [punpckhqdq rm, reg] - packed integer interleave, high half, quadword
            ([66 0F 6D /r]), {!Punpcklqdq}'s sibling. *)
    | Punpcklbw
        (** [punpcklbw rm, reg] - packed integer interleave, low half, byte lanes
            ([66 0F 60 /r]), {!Punpcklqdq}'s own group at narrower lane widths. Confirmed against
            real GNU as: [punpcklbw %xmm2,%xmm1] -> [66 0f 60 ca]. *)
    | Punpckhbw
        (** [punpckhbw rm, reg] - packed integer interleave, high half, byte lanes
            ([66 0F 68 /r]), {!Punpcklbw}'s sibling. *)
    | Punpcklwd
        (** [punpcklwd rm, reg] - packed integer interleave, low half, word lanes
            ([66 0F 61 /r]), {!Punpcklbw}'s sibling at a wider lane width. *)
    | Punpckhwd
        (** [punpckhwd rm, reg] - packed integer interleave, high half, word lanes
            ([66 0F 69 /r]), {!Punpcklwd}'s sibling. *)
    | Punpckldq
        (** [punpckldq rm, reg] - packed integer interleave, low half, doubleword lanes
            ([66 0F 62 /r]), {!Punpcklwd}'s sibling at a wider lane width. *)
    | Punpckhdq
        (** [punpckhdq rm, reg] - packed integer interleave, high half, doubleword lanes
            ([66 0F 6A /r]), {!Punpckldq}'s sibling. *)
    | Paddb
        (** [paddb rm, reg] - packed integer add, byte lanes ([66 0F FC /r]),
            {!Punpcklqdq}'s own 66-mandatory-prefix-only integer-SIMD group at a different opcode
            byte (no non-66 sibling - not a float op). Confirmed against real GNU as:
            [paddb %xmm2,%xmm1] -> [66 0f fc ca]. *)
    | Paddw
        (** [paddw rm, reg] - packed integer add, word lanes ([66 0F FD /r]), {!Paddb}'s sibling. *)
    | Paddd
        (** [paddd rm, reg] - packed integer add, doubleword lanes ([66 0F FE /r]), {!Paddb}'s
            sibling. *)
    | Paddq
        (** [paddq rm, reg] - packed integer add, quadword lanes ([66 0F D4 /r]), {!Paddb}'s
            sibling. *)
    | Psubb
        (** [psubb rm, reg] - packed integer subtract, byte lanes ([66 0F F8 /r]), {!Paddb}'s own
            group at a different opcode byte. *)
    | Psubw
        (** [psubw rm, reg] - packed integer subtract, word lanes ([66 0F F9 /r]), {!Psubb}'s sibling. *)
    | Psubd
        (** [psubd rm, reg] - packed integer subtract, doubleword lanes ([66 0F FA /r]),
            {!Psubb}'s sibling. *)
    | Psubq
        (** [psubq rm, reg] - packed integer subtract, quadword lanes ([66 0F FB /r]), {!Psubb}'s
            sibling. *)
    | Pcmpeqb
        (** [pcmpeqb rm, reg] - packed compare-equal, byte lanes ([66 0F 74 /r]),
            {!Paddb}'s own 66-mandatory-prefix-only integer-SIMD group at a different opcode byte.
            Confirmed against real GNU as: [pcmpeqb %xmm2,%xmm1] -> [66 0f 74 ca]. *)
    | Pcmpeqw
        (** [pcmpeqw rm, reg] - packed compare-equal, word lanes ([66 0F 75 /r]), {!Pcmpeqb}'s
            sibling. *)
    | Pcmpeqd
        (** [pcmpeqd rm, reg] - packed compare-equal, doubleword lanes ([66 0F 76 /r]),
            {!Pcmpeqb}'s sibling. *)
    | Pcmpgtb
        (** [pcmpgtb rm, reg] - packed compare-greater-than, byte lanes ([66 0F 64 /r]),
            {!Pcmpeqb}'s own group at a different opcode byte. *)
    | Pcmpgtw
        (** [pcmpgtw rm, reg] - packed compare-greater-than, word lanes ([66 0F 65 /r]),
            {!Pcmpgtb}'s sibling. *)
    | Pcmpgtd
        (** [pcmpgtd rm, reg] - packed compare-greater-than, doubleword lanes ([66 0F 66 /r]),
            {!Pcmpgtb}'s sibling. *)
    | Packsswb
        (** [packsswb rm, reg] - pack words into bytes with signed saturation ([66 0F 63 /r]), {!Paddb}'s own 66-mandatory-prefix-only integer-SIMD group at a different
            opcode byte. Confirmed against real GNU as: [packsswb %xmm2,%xmm1] -> [66 0f 63 ca]. *)
    | Packssdw
        (** [packssdw rm, reg] - pack doublewords into words with signed saturation ([66 0F 6B
            /r]), {!Packsswb}'s sibling. *)
    | Packuswb
        (** [packuswb rm, reg] - pack words into bytes with unsigned saturation ([66 0F 67 /r]),
            {!Packsswb}'s sibling. *)
    | Pand
        (** [pand rm, reg] - packed bitwise AND, integer ([66 0F DB /r]), {!Pxor}'s own
            66-mandatory-prefix-only integer-SIMD group at a different opcode byte. Confirmed
            against real GNU as: [pand %xmm2,%xmm1] -> [66 0f db ca]. *)
    | Pandn
        (** [pandn rm, reg] - packed bitwise AND-NOT, integer ([66 0F DF /r]), {!Pand}'s
            sibling. *)
    | Por  (** [por rm, reg] - packed bitwise OR, integer ([66 0F EB /r]), {!Pand}'s sibling. *)
    | Pminub
        (** [pminub rm, reg] - packed integer minimum, unsigned byte lanes ([66 0F DA /r]), {!Paddb}'s own 66-mandatory-prefix-only integer-SIMD group at a different
            opcode byte. Confirmed against real GNU as: [pminub %xmm2,%xmm1] -> [66 0f da ca]. *)
    | Pmaxub
        (** [pmaxub rm, reg] - packed integer maximum, unsigned byte lanes ([66 0F DE /r]),
            {!Pminub}'s sibling. *)
    | Pminsw
        (** [pminsw rm, reg] - packed integer minimum, signed word lanes ([66 0F EA /r]),
            {!Pminub}'s sibling. *)
    | Pmaxsw
        (** [pmaxsw rm, reg] - packed integer maximum, signed word lanes ([66 0F EE /r]),
            {!Pminub}'s sibling. *)
    | Pmullw
        (** [pmullw rm, reg] - packed integer multiply, low word result ([66 0F D5 /r]),
            {!Paddb}'s own 66-mandatory-prefix-only integer-SIMD group at a different opcode
            byte. Confirmed against real GNU as: [pmullw %xmm2,%xmm1] -> [66 0f d5 ca]. *)
    | Pmulhw
        (** [pmulhw rm, reg] - packed integer multiply, high word result, signed ([66 0F E5 /r]),
            {!Pmullw}'s sibling. *)
    | Pmulhuw
        (** [pmulhuw rm, reg] - packed integer multiply, high word result, unsigned
            ([66 0F E4 /r]), {!Pmullw}'s sibling. *)
    | Pavgb
        (** [pavgb rm, reg] - packed integer average, byte lanes ([66 0F E0 /r]), {!Pmullw}'s own
            group at a different opcode byte. *)
    | Pavgw
        (** [pavgw rm, reg] - packed integer average, word lanes ([66 0F E3 /r]), {!Pavgb}'s
            sibling. *)
    | Psadbw
        (** [psadbw rm, reg] - packed sum of absolute differences, byte lanes into a qword
            accumulator ([66 0F F6 /r]), {!Pavgb}'s own group at a different opcode byte. *)
    | Psllw
        (** [psllw rm, reg] - packed shift left logical, word lanes, register/memory shift count
            ([66 0F F1 /r]), {!Paddb}'s own 66-mandatory-prefix-only integer-SIMD group at
            a different opcode byte. The separate immediate-count form ([66 0F 71 /6 ib], a
            ModR/M.reg-as-opcode-extension "group" shape unlike every immediate-carrying form
            admitted before it) is {!Lowered.Xmm_shift_imm_rm} instead - see
            {!Opcode.xmm_shift_ext_opcode}. Confirmed against real GNU as: [psllw %xmm2,%xmm1] ->
            [66 0f f1 ca]. *)
    | Pslld
        (** [pslld rm, reg] - packed shift left logical, doubleword lanes ([66 0F F2 /r]),
            {!Psllw}'s sibling. *)
    | Psllq
        (** [psllq rm, reg] - packed shift left logical, quadword lanes ([66 0F F3 /r]),
            {!Psllw}'s sibling. *)
    | Psrlw
        (** [psrlw rm, reg] - packed shift right logical, word lanes ([66 0F D1 /r]), {!Psllw}'s
            own group at a different opcode byte. *)
    | Psrld
        (** [psrld rm, reg] - packed shift right logical, doubleword lanes ([66 0F D2 /r]),
            {!Psrlw}'s sibling. *)
    | Psrlq
        (** [psrlq rm, reg] - packed shift right logical, quadword lanes ([66 0F D3 /r]),
            {!Psrlw}'s sibling. *)
    | Psraw
        (** [psraw rm, reg] - packed shift right arithmetic, word lanes ([66 0F E1 /r]), {!Psllw}'s
            own group at a different opcode byte. *)
    | Psrad
        (** [psrad rm, reg] - packed shift right arithmetic, doubleword lanes ([66 0F E2 /r]),
            {!Psraw}'s sibling. There is no quadword [psraq] in legacy SSE2 - confirmed against
            real GNU as ([no such instruction]); the quadword arithmetic shift only exists under
            AVX-512 (EVEX-encoded), out of scope for this project. *)
    | Pslldq
        (** [pslldq $imm8, xmm] - byte-granularity shift-left of the whole 128-bit register
            ([66 0F 73 /7 ib]), {!Xmm_shift_imm_rm}'s own "group" table at ext 7 -
            {!Psllw}'s own opcode byte (0x73), unlike {!Psllq}/{!Psrlq}'s ext 6/2 at that same
            byte. No register/memory-count sibling exists for this mnemonic - confirmed against
            real GNU as: [pslldq %xmm2,%xmm1] is rejected, only the imm8 form assembles. Confirmed
            against real GNU as: [pslldq $5,%xmm1] -> [66 0f 73 f9 05]. *)
    | Psrldq
        (** [psrldq $imm8, xmm] - {!Pslldq}'s shift-right sibling ([66 0F 73 /3 ib]), ext 3 at the
            same opcode byte. Confirmed against real GNU as: [psrldq $5,%xmm1] ->
            [66 0f 73 d9 05]. *)
    | Pshufb
        (** [pshufb rm, reg] - packed byte shuffle/permute using [rm] as a per-byte control mask
            ([66 0F 38 00 /r], SSSE3): {!Paddb}'s own mandatory-66 [reg, rm] binop shape
            ([Lowered.Sse_binop_r_rm}), but at opcode map 2 ([0F 38 xx]) rather than map 1
            ([0F xx]) - this project's first legacy map-2 mnemonic, so it gets its own codec alt
            ({!sse_binop_0f38_alt}) with one extra fixed [0x38] byte rather than reusing
            {!sse_binop_alt}'s table. The VEX sibling ([vpshufb], also map 2) needs the three-byte
            VEX prefix this project has not built (the same gap {!Vmovq}'s own comment already
            names) - out of scope here. Confirmed against real GNU as: [pshufb %xmm2,%xmm1] ->
            [66 0f 38 00 ca], [pshufb 0x10(%esp),%xmm1] -> [66 0f 38 00 4c 24 10]. *)
    | Phaddw
        (** [phaddw rm, reg] - packed horizontal add, word lanes ([66 0F 38 01 /r], SSSE3), {!Pshufb}'s own map-2 group at a different opcode byte. Confirmed against
            real GNU as: [phaddw %xmm2,%xmm1] -> [66 0f 38 01 ca]. *)
    | Phaddd
        (** [phaddd rm, reg] - packed horizontal add, doubleword lanes ([66 0F 38 02 /r]),
            {!Phaddw}'s sibling. *)
    | Phsubw
        (** [phsubw rm, reg] - packed horizontal subtract, word lanes ([66 0F 38 05 /r]),
            {!Phaddw}'s own group at a different opcode byte. *)
    | Phsubd
        (** [phsubd rm, reg] - packed horizontal subtract, doubleword lanes ([66 0F 38 06 /r]),
            {!Phsubw}'s sibling. *)
    | Psignb
        (** [psignb rm, reg] - packed conditional sign-flip, byte lanes ([66 0F 38 08 /r]),
            {!Phaddw}'s own group at a different opcode byte. *)
    | Psignw
        (** [psignw rm, reg] - packed conditional sign-flip, word lanes ([66 0F 38 09 /r]),
            {!Psignb}'s sibling. *)
    | Psignd
        (** [psignd rm, reg] - packed conditional sign-flip, doubleword lanes ([66 0F 38 0A /r]),
            {!Psignb}'s sibling. *)
    | Pmaddubsw
        (** [pmaddubsw rm, reg] - packed multiply unsigned-signed bytes, horizontal add into word
            lanes ([66 0F 38 04 /r]), {!Phaddw}'s own group at a different opcode byte. *)
    | Pmulhrsw
        (** [pmulhrsw rm, reg] - packed multiply high, round and scale, word lanes
            ([66 0F 38 0B /r]), {!Phaddw}'s own group at a different opcode byte. Confirmed
            against real GNU as: [pmulhrsw %xmm2,%xmm1] -> [66 0f 38 0b ca],
            [pmulhrsw 0x10(%esp),%xmm1] -> [66 0f 38 0b 4c 24 10]. *)
    | Phaddsw
        (** [phaddsw rm, reg] - packed horizontal add, word lanes, saturating
            ([66 0F 38 03 /r]), {!Phaddw}'s own group at a different opcode byte - {!Phaddw}'s
            saturating sibling, matching real GNU as's own naming. Confirmed against real GNU as:
            [phaddsw %xmm2,%xmm1] -> [66 0f 38 03 ca], [phaddsw 0x10(%esp),%xmm1] ->
            [66 0f 38 03 4c 24 10]. *)
    | Phsubsw
        (** [phsubsw rm, reg] - packed horizontal subtract, word lanes, saturating
            ([66 0F 38 07 /r]), {!Phaddsw}'s sibling. Confirmed against real GNU as:
            [phsubsw %xmm2,%xmm1] -> [66 0f 38 07 ca]. *)
    | Pabsb
        (** [pabsb rm, reg] - packed absolute value, byte lanes ([66 0F 38 1C /r]): REG0's own XED
            [rw="w"] (write-only, unlike every other map-2 member's [rw="rw"]) still fits
            {!Lowered.Sse_binop_r_rm}/{!sse_binop_0f38_alt} unchanged - dataflow direction is a
            normalization-layer fact, not an encoder-layer one, the same way {!Movdqa}'s own
            register-register form reuses {!xmm_binop_rr_form}'s generic [role_of_rw] handling.
            Confirmed against real GNU as: [pabsb %xmm2,%xmm1] -> [66 0f 38 1c ca],
            [pabsb 0x10(%esp),%xmm1] -> [66 0f 38 1c 4c 24 10]. *)
    | Pabsw
        (** [pabsw rm, reg] - packed absolute value, word lanes ([66 0F 38 1D /r]), {!Pabsb}'s
            sibling. *)
    | Pabsd
        (** [pabsd rm, reg] - packed absolute value, doubleword lanes ([66 0F 38 1E /r]),
            {!Pabsb}'s sibling. *)
    | Palignr
        (** [palignr $imm8, rm, reg] - packed right-align concatenation of [reg:rm] by [imm8]
            bytes ([66 0F 3A 0F /r ib], SSSE3): {!Shld}'s own [imm, rm, reg] operand order,
            the exact same {!Lowered.Sse_binop_imm_r_rm} shape {!Pshufd}/{!Shufps} already use -
            but opcode map 3 ([0F 3A xx]) rather than {!Pshufd}'s map 1, so it gets its own codec
            alt ({!sse_binop_imm_0f3a_alt}) with one extra fixed [0x3A] byte, the exact map-3
            counterpart of {!Pshufb}'s own map-2 {!sse_binop_0f38_alt}. Confirmed against real GNU
            as: [palignr $5,%xmm2,%xmm1] -> [66 0f 3a 0f ca 05],
            [palignr $5,0x10(%esp),%xmm1] -> [66 0f 3a 0f 4c 24 10 05]. *)
    | Roundps
        (** [roundps $imm8, rm, reg] - packed round to integer, single precision, with an
            explicit rounding-mode/exception-suppression control ([66 0F 3A 08 /r ib], SSE4.1): {!Palignr}'s own map-3 group at a different opcode byte, same
            {!Lowered.Sse_binop_imm_r_rm} shape unchanged. Confirmed against real GNU as:
            [roundps $5,%xmm2,%xmm1] -> [66 0f 3a 08 ca 05],
            [roundps $5,0x10(%esp),%xmm1] -> [66 0f 3a 08 4c 24 10 05]. *)
    | Roundpd
        (** [roundpd $imm8, rm, reg] - packed round to integer, double precision
            ([66 0F 3A 09 /r ib]), {!Roundps}'s sibling. *)
    | Roundss
        (** [roundss $imm8, rm, reg] - scalar round to integer, single precision
            ([66 0F 3A 0A /r ib]); REG0's XED [rw="rw"] (unlike {!Roundps}'s [rw="w"], since the
            scalar form leaves the destination's upper lanes untouched) still fits the same
            unchanged shape - dataflow direction is a normalization-layer fact, not an
            encoder-layer one, the same fact {!Pabsb}'s own doc comment already established. *)
    | Roundsd
        (** [roundsd $imm8, rm, reg] - scalar round to integer, double precision
            ([66 0F 3A 0B /r ib]), {!Roundss}'s sibling. *)
    | Pcmpeqq
        (** [pcmpeqq rm, reg] - packed compare equal, qword lanes ([66 0F 38 29 /r], SSE4.1): {!Pshufb}'s own map-2 group at a different opcode byte, same
            {!Lowered.Sse_binop_r_rm}/{!sse_binop_0f38_alt} shape unchanged. Confirmed against
            real GNU as: [pcmpeqq %xmm2,%xmm1] -> [66 0f 38 29 ca],
            [pcmpeqq 0x10(%esp),%xmm1] -> [66 0f 38 29 4c 24 10]. *)
    | Pcmpgtq
        (** [pcmpgtq rm, reg] - packed compare greater-than (signed), qword lanes
            ([66 0F 38 37 /r]), {!Pcmpeqq}'s sibling. *)
    | Packusdw
        (** [packusdw rm, reg] - pack doubleword to word with unsigned saturation
            ([66 0F 38 2B /r]), {!Pcmpeqq}'s own group at a different opcode byte. *)
    | Pmaxsb
        (** [pmaxsb rm, reg] - packed maximum, signed byte lanes ([66 0F 38 3C /r]),
            {!Pcmpeqq}'s own group at a different opcode byte. *)
    | Pmaxsd
        (** [pmaxsd rm, reg] - packed maximum, signed doubleword lanes ([66 0F 38 3D /r]),
            {!Pmaxsb}'s sibling. *)
    | Pmaxud
        (** [pmaxud rm, reg] - packed maximum, unsigned doubleword lanes ([66 0F 38 3F /r]),
            {!Pmaxsb}'s sibling. *)
    | Pmaxuw
        (** [pmaxuw rm, reg] - packed maximum, unsigned word lanes ([66 0F 38 3E /r]),
            {!Pmaxsb}'s sibling. *)
    | Pminsb
        (** [pminsb rm, reg] - packed minimum, signed byte lanes ([66 0F 38 38 /r]),
            {!Pmaxsb}'s minimum-direction sibling. *)
    | Pminsd
        (** [pminsd rm, reg] - packed minimum, signed doubleword lanes ([66 0F 38 39 /r]),
            {!Pminsb}'s sibling. *)
    | Pminud
        (** [pminud rm, reg] - packed minimum, unsigned doubleword lanes ([66 0F 38 3B /r]),
            {!Pminsb}'s sibling. *)
    | Pminuw
        (** [pminuw rm, reg] - packed minimum, unsigned word lanes ([66 0F 38 3A /r]),
            {!Pminsb}'s sibling. *)
    | Pmuldq
        (** [pmuldq rm, reg] - packed signed multiply, low 32 bits of each even qword lane widened
            to 64 bits ([66 0F 38 28 /r]), {!Pcmpeqq}'s own group at a different opcode byte. *)
    | Pmulld
        (** [pmulld rm, reg] - packed signed multiply, doubleword lanes, low 32 bits of the
            product ([66 0F 38 40 /r]), {!Pcmpeqq}'s own group at a different opcode byte. *)
    | Phminposuw
        (** [phminposuw rm, reg] - packed horizontal minimum of unsigned word lanes plus its
            index ([66 0F 38 41 /r]): REG0's XED [rw="w"] (write-only, unlike every other member
            here's [rw="rw"]) still fits {!Lowered.Sse_binop_r_rm}/{!sse_binop_0f38_alt} unchanged,
            the same fact {!Pabsb}'s own doc comment already established. Confirmed against real
            GNU as: [phminposuw %xmm2,%xmm1] -> [66 0f 38 41 ca],
            [phminposuw 0x10(%esp),%xmm1] -> [66 0f 38 41 4c 24 10]. *)
    | Ptest
        (** [ptest rm, reg] - logical compare setting ZF/CF, no register written ([66 0F 38 17 /r],
            SSE4.1): {!Pshufb}'s own map-2 group at a different opcode byte, same
            {!Lowered.Sse_binop_r_rm}/{!sse_binop_0f38_alt} shape unchanged; REG0's XED [rw="r"] is a
            normalization-layer fact, the same one {!Pabsb}'s doc comment already established.
            Confirmed against real GNU as: [ptest %xmm2,%xmm1] -> [66 0f 38 17 ca],
            [ptest 0x10(%esp),%xmm1] -> [66 0f 38 17 4c 24 10]. *)
    | Pmovsxbw
        (** [pmovsx{bw,bd,bq,wd,wq,dq} rm, reg] / [pmovzx...] - sign-/zero-extending packed move
            ([66 0F 38 20..25] / [66 0F 38 30..35], SSE4.1), {!Ptest}'s own group. XED's
            source width varies per form ([XMMq]/[XMMd]/[XMMw], [MEMq]/[MEMd]/[MEMw]) but AT&T
            spelling carries no size, so the encoder shape is unchanged. Confirmed against real GNU
            as: [pmovsxbw %xmm2,%xmm1] -> [66 0f 38 20 ca], [pmovzxdq 0x10(%esp),%xmm1] ->
            [66 0f 38 35 4c 24 10]. *)
    | Pmovsxbd  (** See {!Pmovsxbw}. *)
    | Pmovsxbq  (** See {!Pmovsxbw}. *)
    | Pmovsxwd  (** See {!Pmovsxbw}. *)
    | Pmovsxwq  (** See {!Pmovsxbw}. *)
    | Pmovsxdq  (** See {!Pmovsxbw}. *)
    | Pmovzxbw  (** See {!Pmovsxbw}. *)
    | Pmovzxbd  (** See {!Pmovsxbw}. *)
    | Pmovzxbq  (** See {!Pmovsxbw}. *)
    | Pmovzxwd  (** See {!Pmovsxbw}. *)
    | Pmovzxwq  (** See {!Pmovsxbw}. *)
    | Pmovzxdq  (** See {!Pmovsxbw}. *)
    | Movntdqa
        (** [movntdqa mem, reg] - non-temporal aligned load ([66 0F 38 2A /r], SSE4.1),
            {!Ptest}'s own group, memory source only. Confirmed against real GNU as:
            [movntdqa 0x10(%esp),%xmm1] -> [66 0f 38 2a 4c 24 10]. *)
    | Blendvps
        (** [blendvps %xmm0, rm, reg] - variable blend, single precision, per-dword mask taken from
            the implicit [%xmm0] ([66 0F 38 14 /r], SSE4.1): {!Ptest}'s own map-2 group,
            same {!Lowered.Sse_binop_r_rm}/{!sse_binop_0f38_alt} shape - the mask is not encoded.
            Real GNU as accepts both spellings, the canonical three-operand one with an explicit
            [%xmm0] first and the two-operand one without it, and emits identical bytes; only an
            explicit [%xmm0] mask (register number 0, 128-bit) lowers, any other register falls to
            the [No_form] catch-all. Confirmed against real GNU as: [blendvps %xmm0,%xmm2,%xmm1]
            and [blendvps %xmm2,%xmm1] -> [66 0f 38 14 ca], [blendvpd %xmm0,0x10(%esp),%xmm1] ->
            [66 0f 38 15 4c 24 10]. *)
    | Blendvpd
        (** [blendvpd %xmm0, rm, reg] - {!Blendvps}'s double-precision sibling ([66 0F 38 15 /r]). *)
    | Pblendvb
        (** [pblendvb %xmm0, rm, reg] - {!Blendvps}'s byte-lane sibling ([66 0F 38 10 /r]). *)
    | Blendps
        (** [blendps $imm8, rm, reg] - packed blend, single precision, per-dword mask selected by
            [imm8] ([66 0F 3A 0C /r ib], SSE4.1): {!Palignr}'s own map-3 group at a
            different opcode byte, same {!Lowered.Sse_binop_imm_r_rm}/{!sse_binop_imm_0f3a_alt}
            shape unchanged. Confirmed against real GNU as: [blendps $5,%xmm2,%xmm1] ->
            [66 0f 3a 0c ca 05], [blendps $5,0x10(%esp),%xmm1] -> [66 0f 3a 0c 4c 24 10 05]. *)
    | Blendpd
        (** [blendpd $imm8, rm, reg] - packed blend, double precision ([66 0F 3A 0D /r ib]),
            {!Blendps}'s sibling. *)
    | Dpps
        (** [dpps $imm8, rm, reg] - packed dot product, single precision, with a broadcast/write
            mask selected by [imm8] ([66 0F 3A 40 /r ib]), {!Blendps}'s own group at a different
            opcode byte. *)
    | Dppd
        (** [dppd $imm8, rm, reg] - packed dot product, double precision ([66 0F 3A 41 /r ib]),
            {!Dpps}'s sibling. *)
    | Mpsadbw
        (** [mpsadbw $imm8, rm, reg] - multiple packed sums of absolute differences, byte lanes,
            with [imm8] selecting the comparison offsets ([66 0F 3A 42 /r ib]), {!Blendps}'s own
            group at a different opcode byte. *)
    | Pblendw
        (** [pblendw $imm8, rm, reg] - packed blend, word lanes ([66 0F 3A 0E /r ib]),
            {!Blendps}'s sibling. *)
    | Insertps
        (** [insertps $imm8, rm, reg] - insert a single-precision lane selected/zeroed by [imm8]
            ([66 0F 3A 21 /r ib], SSE4.1): {!Blendps}'s own map-3 group at a different opcode
            byte, same {!Lowered.Sse_binop_imm_r_rm}/{!sse_binop_imm_0f3a_alt} shape unchanged.
            Confirmed against real GNU as: [insertps $0x10,%xmm2,%xmm1] -> [66 0f 3a 21 ca 10],
            [insertps $0x10,0x10(%esp),%xmm1] -> [66 0f 3a 21 4c 24 10 10]. *)
    | Pinsrb
        (** [pinsrb $imm8, gpr32/m8, xmm] - packed insert byte ([66 0F 3A 20 /r ib], SSE4.1): {!Pinsrw}'s own cross-register-class field roles ([reg] the xmm destination,
            [rm] a GPR32 or memory source) at opcode map 3, where the memory spelling exists
            (unlike {!Pextrw}). Confirmed against real GNU as: [pinsrb $1,%eax,%xmm1] ->
            [66 0f 3a 20 c8 01]. *)
    | Pinsrd
        (** [pinsrd $imm8, gpr32/m32, xmm] - packed insert dword ([66 0F 3A 22 /r ib]),
            {!Pinsrb}'s sibling. The REX.W-promoted [pinsrq] sibling is not yet built. Confirmed
            against real GNU as: [pinsrd $1,0x10(%esp),%xmm1] -> [66 0f 3a 22 4c 24 10 01]. *)
    | Pextrb
        (** [pextrb $imm8, xmm, gpr32/m8] - packed extract byte ([66 0F 3A 14 /r ib]), {!Pinsrb}'s
            store-direction mirror with the xmm in the ModR/M [reg] field and the GPR32 or memory
            destination in [rm] - the opposite field roles from {!Pextrw}'s own two-byte-opcode
            layout, hence the same {!Lowered.Sse_binop_imm_r_rm} node as {!Pinsrb} with the AT&T
            operand order reversed at lowering. Confirmed against real GNU as:
            [pextrb $1,%xmm1,%eax] -> [66 0f 3a 14 c8 01], [pextrb $1,%xmm1,0x10(%esp)] ->
            [66 0f 3a 14 4c 24 10 01]. *)
    | Pextrd
        (** [pextrd $imm8, xmm, gpr32/m32] - packed extract dword ([66 0F 3A 16 /r ib]),
            {!Pextrb}'s sibling. The REX.W-promoted [pextrq] sibling is not yet built. *)
    | Extractps
        (** [extractps $imm8, xmm, gpr32/m32] - extract a single-precision lane ([66 0F 3A 17 /r
            ib]), {!Pextrb}'s sibling. Confirmed against real GNU as: [extractps $1,%xmm1,%eax]
            -> [66 0f 3a 17 c8 01]. *)
    | Vpshufb
        (** [vpshufb src2, src1, dst] - the VEX sibling of the legacy {!Pshufb} and the first
            mnemonic needing the three-byte VEX prefix ([0xC4]): [VEX.128.66.0F38.WIG 00
            /r], opcode map 2 selected by the prefix's own [mmmmm] field rather than a fixed [0x0F
            0x38] escape pair. Same {!Lowered.Vex_binop_rr_rm} shape as {!Vpaddb}, so the same
            two-byte-VEX register restriction on [src2] still applies here for now, and the whole
            [VEX.128.66.0F38] group below ({!Vphaddw}..{!Vpmulld}) mirrors the legacy map-2 binop
            table entry for entry. Confirmed against real GNU as: [vpmulld %xmm7,%xmm3,%xmm5] ->
            [c4 e2 61 40 ef], [vpshufb 0x10(%esp),%xmm1,%xmm0] -> [c4 e2 71 00 44 24 10]. *)
    | Vphaddw  (** See {!Vpshufb}. *)
    | Vphaddd  (** See {!Vpshufb}. *)
    | Vphaddsw  (** See {!Vpshufb}. *)
    | Vpmaddubsw  (** See {!Vpshufb}. *)
    | Vphsubw  (** See {!Vpshufb}. *)
    | Vphsubd  (** See {!Vpshufb}. *)
    | Vphsubsw  (** See {!Vpshufb}. *)
    | Vpsignb  (** See {!Vpshufb}. *)
    | Vpsignw  (** See {!Vpshufb}. *)
    | Vpsignd  (** See {!Vpshufb}. *)
    | Vpmulhrsw  (** See {!Vpshufb}. *)
    | Vpmuldq  (** See {!Vpshufb}. *)
    | Vpcmpeqq  (** See {!Vpshufb}. *)
    | Vpackusdw  (** See {!Vpshufb}. *)
    | Vpcmpgtq  (** See {!Vpshufb}. *)
    | Vpminsb  (** See {!Vpshufb}. *)
    | Vpminsd  (** See {!Vpshufb}. *)
    | Vpminuw  (** See {!Vpshufb}. *)
    | Vpminud  (** See {!Vpshufb}. *)
    | Vpmaxsb  (** See {!Vpshufb}. *)
    | Vpmaxsd  (** See {!Vpshufb}. *)
    | Vpmaxuw  (** See {!Vpshufb}. *)
    | Vpmaxud  (** See {!Vpshufb}. *)
    | Vpmulld  (** See {!Vpshufb}. *)
    | Vpalignr
        (** [vpalignr $imm8, src2, src1, dst] - the VEX sibling of the legacy {!Palignr}, and the
            first mnemonic in the [VEX.128.66.0F3A] map ([mmmmm = 3]) - {!Vpshufb}'s own
            three-byte-prefix layout, with the trailing imm8 {!Lowered.Vex_binop_imm_rr_rm} adds.
            The rest of the group ({!Vblendps}..{!Vinsertps}) mirrors the legacy map-3 binop table;
            [vroundss]/[vroundsd] take a merge [src1] the way [roundss]/[roundsd] merge [dst].
            Confirmed against real GNU as: [vpalignr $5,%xmm2,%xmm1,%xmm0] -> [c4 e3 71 0f c2
            05]. *)
    | Vblendps  (** See {!Vpalignr}. *)
    | Vblendpd  (** See {!Vpalignr}. *)
    | Vpblendw  (** See {!Vpalignr}. *)
    | Vroundss  (** See {!Vpalignr}. *)
    | Vroundsd  (** See {!Vpalignr}. *)
    | Vdpps  (** See {!Vpalignr}. *)
    | Vdppd  (** See {!Vpalignr}. *)
    | Vmpsadbw  (** See {!Vpalignr}. *)
    | Vinsertps  (** See {!Vpalignr}. *)
    | Vpabsb
        (** [vpabsb src, dst] - the VEX sibling of the legacy {!Pabsb}, the first two-operand
            mnemonic in the [VEX.128.66.0F38] map: {!Vpshufb}'s three-byte prefix over
            {!Lowered.Vex_unop_r_rm}'s architecturally-unused [vvvv = 1111]. The whole group
            ({!Vpabsw}..{!Vmovntdqa}: [vpabs*], [vphminposuw], [vptest], [vpmovsx*]/[vpmovzx*])
            mirrors the legacy map-2 unary table; [vmovntdqa] is memory-source only, like its
            legacy sibling. Confirmed against real GNU as: [vpabsb %xmm2,%xmm1] -> [c4 e2 79 1c
            ca], [vptest 0x10(%esp),%xmm1] -> [c4 e2 79 17 4c 24 10]. *)
    | Vpabsw  (** See {!Vpabsb}. *)
    | Vpabsd  (** See {!Vpabsb}. *)
    | Vphminposuw  (** See {!Vpabsb}. *)
    | Vptest  (** See {!Vpabsb}. *)
    | Vpmovsxbw  (** See {!Vpabsb}. *)
    | Vpmovsxbd  (** See {!Vpabsb}. *)
    | Vpmovsxbq  (** See {!Vpabsb}. *)
    | Vpmovsxwd  (** See {!Vpabsb}. *)
    | Vpmovsxwq  (** See {!Vpabsb}. *)
    | Vpmovsxdq  (** See {!Vpabsb}. *)
    | Vpmovzxbw  (** See {!Vpabsb}. *)
    | Vpmovzxbd  (** See {!Vpabsb}. *)
    | Vpmovzxbq  (** See {!Vpabsb}. *)
    | Vpmovzxwd  (** See {!Vpabsb}. *)
    | Vpmovzxwq  (** See {!Vpabsb}. *)
    | Vpmovzxdq  (** See {!Vpabsb}. *)
    | Vmovntdqa  (** See {!Vpabsb}. *)
    | Movdqa
        (** [movdqa rm, reg] / [movdqa reg, rm] - integer/general XMM register move, aligned
            ([66 0F 6F /r] load, [66 0F 7F /r] store), {!Movaps}'s integer-classified
            sibling: unlike {!Movaps} (which only ever admits the load direction, since its own
            single opcode byte makes register-register and register<-memory share one shape),
            [movdqa]'s load and store directions are genuinely distinct opcode bytes, so this
            reuses {!Lowered.Sse_mov_r_rm}/{!Sse_mov_rm_r} - {!Movsd}/{!Movss}'s own two-direction
            shape - instead, extended here to also admit register-register (mapped to the
            load-direction opcode, the "low-numbered iform" convention this project already
            applies elsewhere: real GNU as's own [MOVDQA_XMMdq_XMMdq_0F7F] redundant
            register-register encoding via the store opcode is deliberately left unadmitted).
            Confirmed against real GNU as: [movdqa %xmm2,%xmm1] -> [66 0f 6f ca],
            [movdqa (%eax),%xmm1] -> [66 0f 6f 08], [movdqa %xmm1,(%eax)] -> [66 0f 7f 08]. *)
    | Movdqu
        (** [movdqu rm, reg] / [movdqu reg, rm] - {!Movdqa}'s mandatory-[F3] (unaligned)
            counterpart at the same opcode bytes ([F3 0F 6F /r] load, [F3 0F 7F /r] store).
            Confirmed against real GNU as: [movdqu %xmm2,%xmm1] -> [f3 0f 6f ca]. *)
    | Andps
        (** [andps rm, reg] - packed bitwise AND, single precision ([0F 54 /r], no mandatory
            prefix - {!Comiss}'s own mandatory-prefix-free group at a different opcode byte). *)
    | Andnps
        (** [andnps rm, reg] - packed bitwise ANDN ([0F 55 /r]), {!Andps}'s exact sibling at the
            next opcode byte. *)
    | Orps  (** [orps rm, reg] - packed bitwise OR ([0F 56 /r]), {!Andps}'s exact sibling. *)
    | Xorps
        (** [xorps rm, reg] - packed bitwise XOR ([0F 57 /r]), {!Andps}'s exact sibling and
            {!Xorpd}'s mandatory-prefix-free counterpart. *)
    | Andpd
        (** [andpd rm, reg] - packed bitwise AND, double precision ([66 0F 54 /r]), {!Xorpd}'s
            own mandatory-prefix group at a different opcode byte. *)
    | Andnpd
        (** [andnpd rm, reg] - packed bitwise ANDN ([66 0F 55 /r]), {!Andpd}'s exact sibling. *)
    | Orpd  (** [orpd rm, reg] - packed bitwise OR ([66 0F 56 /r]), {!Andpd}'s exact sibling. *)
    | Movaps
        (** [movaps rm, reg] - aligned packed move, single precision ([0F 28 /r], no mandatory
            prefix), {!Movapd}'s mandatory-prefix-free counterpart at the same opcode byte and
            {!Andps}'s own mandatory-prefix-free group at a different opcode byte. Only the
            reg-dest load direction is admitted here, matching {!Movapd}'s own precedent: the
            reverse store direction ([0F 29]) is a distinct, real form this project does not yet
            build. *)
    | Movups
        (** [movups rm, reg] - unaligned packed move, single precision ([0F 10 /r], no mandatory
            prefix), {!Movaps}'s own mandatory-prefix-free group at a different opcode byte.
            Load direction only, matching {!Movaps}'s precedent. *)
    | Movupd
        (** [movupd rm, reg] - unaligned packed move, double precision ([66 0F 10 /r]),
            {!Movups}'s mandatory-66-prefix counterpart at the same opcode byte. Load direction
            only, matching {!Movaps}'s precedent. *)
    | Addps
        (** [addps rm, reg] - packed add, single precision ([0F 58 /r], no mandatory prefix),
            {!Addsd}'s mandatory-prefix-free counterpart at the same opcode byte and {!Andps}'s
            own mandatory-prefix-free group at a different opcode byte. *)
    | Subps
        (** [subps rm, reg] - packed subtract, single precision ([0F 5C /r]), {!Addps}'s sibling. *)
    | Mulps
        (** [mulps rm, reg] - packed multiply, single precision ([0F 59 /r]), {!Addps}'s sibling. *)
    | Divps
        (** [divps rm, reg] - packed divide, single precision ([0F 5E /r]), {!Addps}'s sibling. *)
    | Addpd
        (** [addpd rm, reg] - packed add, double precision ([66 0F 58 /r]), {!Addps}'s
            mandatory-66-prefix counterpart at the same opcode byte and {!Andpd}'s own
            mandatory-66-prefix group at a different opcode byte. *)
    | Subpd
        (** [subpd rm, reg] - packed subtract, double precision ([66 0F 5C /r]), {!Addpd}'s sibling. *)
    | Mulpd
        (** [mulpd rm, reg] - packed multiply, double precision ([66 0F 59 /r]), {!Addpd}'s sibling. *)
    | Divpd
        (** [divpd rm, reg] - packed divide, double precision ([66 0F 5E /r]), {!Addpd}'s sibling. *)
    | Maxss
        (** [maxss rm, reg] - scalar maximum, single precision ([F3 0F 5F /r]), {!Addss}'s own
            mandatory-prefix group at a different opcode byte. *)
    | Minss
        (** [minss rm, reg] - scalar minimum, single precision ([F3 0F 5D /r]), {!Maxss}'s
            sibling. *)
    | Maxsd
        (** [maxsd rm, reg] - scalar maximum, double precision ([F2 0F 5F /r]), {!Maxss}'s
            mandatory-[F2] counterpart at the same opcode byte and {!Addsd}'s own mandatory-prefix
            group at a different opcode byte. *)
    | Minsd
        (** [minsd rm, reg] - scalar minimum, double precision ([F2 0F 5D /r]), {!Maxsd}'s sibling. *)
    | Maxps
        (** [maxps rm, reg] - packed maximum, single precision ([0F 5F /r], no mandatory prefix),
            {!Maxss}'s mandatory-prefix-free counterpart at the same opcode byte and {!Addps}'s
            own mandatory-prefix-free group at a different opcode byte. *)
    | Minps
        (** [minps rm, reg] - packed minimum, single precision ([0F 5D /r]), {!Maxps}'s sibling. *)
    | Maxpd
        (** [maxpd rm, reg] - packed maximum, double precision ([66 0F 5F /r]), {!Maxps}'s
            mandatory-66-prefix counterpart at the same opcode byte and {!Addpd}'s own
            mandatory-66-prefix group at a different opcode byte. *)
    | Minpd
        (** [minpd rm, reg] - packed minimum, double precision ([66 0F 5D /r]), {!Maxpd}'s sibling. *)
    | Sqrtss
        (** [sqrtss rm, reg] - scalar square root, single precision ([F3 0F 51 /r]), {!Addss}'s
            own mandatory-prefix group at a different opcode byte. The first genuinely unary
            member of this family - [reg] is only ever a destination architecturally, but XED
            still marks it [rw] (a scalar op leaves the destination's upper 96 bits untouched),
            the same convention {!Addsd}'s own [REG0] already has, so {!Lowered.Sse_binop_r_rm}
            and its [xmm_binop_rr_form]/[xmm_binop_rm_form] normalizers need no change at all. *)
    | Sqrtsd
        (** [sqrtsd rm, reg] - scalar square root, double precision ([F2 0F 51 /r]), {!Sqrtss}'s
            mandatory-[F2] counterpart at the same opcode byte. *)
    | Sqrtps
        (** [sqrtps rm, reg] - packed square root, single precision ([0F 51 /r], no mandatory
            prefix), {!Sqrtss}'s mandatory-prefix-free counterpart at the same opcode byte. *)
    | Sqrtpd
        (** [sqrtpd rm, reg] - packed square root, double precision ([66 0F 51 /r]), {!Sqrtps}'s
            mandatory-66-prefix counterpart at the same opcode byte. *)
    | Shufps
        (** [shufps $imm8, rm, reg] - packed shuffle, single precision ([0F C6 /r ib], no
            mandatory prefix): the first XMM-immediate-carrying legacy shape - every SSE
            mnemonic above has at most two real operands, but this one takes a genuine trailing
            imm8 selector alongside its [reg]/[rm] pair, {!Lowered.Sse_binop_imm_r_rm} rather
            than {!Lowered.Sse_binop_r_rm}. Confirmed against real GNU as: [shufps $0x1b,
            %xmm2,%xmm1] -> [0f c6 ca 1b], both register-register and register<-memory
            unambiguous at this legacy encoding. *)
    | Shufpd
        (** [shufpd $imm8, rm, reg] - packed shuffle, double precision ([66 0F C6 /r ib]),
            {!Shufps}'s mandatory-66-prefix counterpart at the same opcode byte. *)
    | Cmpss
        (** [cmpss $imm8, rm, reg] - scalar compare, single precision ([F3 0F C2 /r ib]),
            {!Addss}'s own four-mandatory-prefix-group shape at opcode 0xC2, but - like
            {!Shufps} - with a trailing imm8 predicate selector: {!Lowered.Sse_binop_imm_r_rm}.
            Confirmed against real GNU as: [cmpss $0x0,%xmm2,%xmm1] -> [f3 0f c2 ca 00]
            (GNU as prints this back as the [cmpeqss] pseudo-mnemonic alias for imm8=0; only the
            canonical [cmpss $imm, ...] spelling is admitted here, not the [cmpeq]/[cmplt]/etc.
            mnemonic-suffix aliases). *)
    | Cmpsd
        (** [cmpsd $imm8, rm, reg] - {!Cmpss}'s mandatory-[F2] counterpart ([F2 0F C2 /r ib]). *)
    | Cmpps
        (** [cmpps $imm8, rm, reg] - packed compare, single precision ([0F C2 /r ib], no
            mandatory prefix), {!Cmpss}'s mandatory-prefix-free counterpart at the same opcode
            byte, joining {!Shufps}'s own mandatory-prefix-free imm8 group. *)
    | Cmppd
        (** [cmppd $imm8, rm, reg] - {!Cmpps}'s mandatory-66-prefix counterpart
            ([66 0F C2 /r ib]), joining {!Shufpd}'s own mandatory-66 imm8 group. *)
    | Pshufd
        (** [pshufd $imm8, rm, reg] - packed shuffle doublewords ([66 0F 70 /r ib]):
            unlike {!Shufps}/{!Cmpps}, [reg] is dest-only (XED marks it [w], not [rw]) and [rm]
            is source-only rather than a second read-write operand, but the ModR/M-reg/ModR/M-rm/
            trailing-imm8 byte layout is identical, so this reuses {!Lowered.Sse_binop_imm_r_rm}
            unchanged - {!xmm_binop_imm_rr_form}'s own [role_of_rw] already derives operand roles
            from XED's own [rw] fact rather than assuming read-write, so no normalization change
            is needed either. Confirmed against real GNU as: [pshufd $0x1b,%xmm2,%xmm1] ->
            [66 0f 70 ca 1b]. *)
    | Pshuflw
        (** [pshuflw $imm8, rm, reg] - {!Pshufd}'s mandatory-[F2] sibling at the same opcode byte
            ([F2 0F 70 /r ib]), shuffling only the low 4 words and leaving the high 64 bits
            unchanged. No mandatory-prefix-free sibling is admitted at this opcode: that slot
            belongs to the unrelated MMX instruction [pshufw] (a different register class this
            project does not model), not a fourth member of this family. *)
    | Pshufhw
        (** [pshufhw $imm8, rm, reg] - {!Pshufd}'s mandatory-[F3] sibling at the same opcode byte
            ([F3 0F 70 /r ib]), shuffling only the high 4 words and leaving the low 64 bits
            unchanged. *)
    | Movd
        (** [movd]/[movq] ([66 0F 6E /r] load, [66 0F 7E /r] store): moves between a GPR
            (or GPR-sized memory) and the low 32/64 bits of an xmm register, zero-extending on
            load. One shared opcode across both mnemonics and both directions, exactly like
            {!Cvtsi2sd}/[cvtsi2sdq]: [width] (32 for [movd], 64 for [movq]) selects REX.W, and the
            frontend spells the two widths as distinct mnemonics rather than inferring from an
            operand. Confirmed against real GNU as (both directions, both widths, register and
            memory, both x86-32/x86-64): [movd %eax,%xmm0] -> [66 0f 6e c0]; [movd %xmm0,%eax] ->
            [66 0f 7e c0]; [movd (%eax),%xmm0]/[movd %xmm0,(%eax)] -> [66 0f 6e 00]/[66 0f 7e 00]
            (byte-identical on both profiles); [movq %rax,%xmm0]/[movq %xmm0,%rax] (x86-64 only,
            REX.W) -> [66 48 0f 6e c0]/[66 48 0f 7e c0]; [movq %eax,%xmm0] is rejected outright in
            32-bit mode ("operand type mismatch"), matching XED's own per-profile record split
            (unlike {!Cvtsi2sd}/[cvtsi2sdq], whose mode64 requirement had to be derived, MOVQ's
            64-bit-only applicability is already a native XED fact - no derived requirement
            needed here). MOVQ's memory-operand forms are deliberately NOT admitted: [movq
            (%rax),%xmm0] assembles to [f3 0f 7e 00] and [movq %xmm0,(%rax)] to [66 0f d6 00] -
            real GNU as always prefers the unrelated scalar-XMM [MOVQ xmm1, xmm2/m64] instruction
            (a different, genuinely distinct opcode sharing the "movq" mnemonic) whenever the
            other operand is memory rather than a specific-width GPR register, so the GPR64<->mem
            form of *this* instruction is unreachable via that spelling - a real GAS ambiguity,
            not an oversight, mirroring {!Vcvtpd2ps}'s own memory-operand-ambiguity precedent.
            The XMM<->XMM/mem64 [movq] forms XED's own resolved export separately names
            (MOVQ_XMMdq_XMMq_0F7E/0FD6, MOVQ_MEMq_XMMq_0FD6) are a completely different
            instruction this project does not admit at all yet - a named follow-up, not scoped
            here. *)
    | Vaddsd
        (** [vaddsd src2, src1, dst] - VEX-encoded scalar-double add ([VEX.LIG.F2.0F.WIG 58 /r]),
            the first x86 vector-extension (AVX) form this project admits: unlike every opcode
            above, which shares {!Addsd}'s legacy destructive two-operand [dst := dst op src]
            shape, VEX's non-destructive three-operand form reads [dst := src1 op src2] with
            [src1] carried in the VEX prefix's own [vvvv] field rather than the ModR/M byte. Only
            the two-byte VEX prefix ([0xC5]) is built - [src2] (the ModR/M r/m operand) is
            therefore restricted to xmm0-7, since encoding xmm8-15 there needs REX.B's VEX
            counterpart, which only the three-byte VEX prefix ([0xC4]) carries; [dst] and [src1]
            are unrestricted since the two-byte prefix's own R and vvvv bits already reach all of
            xmm0-15. Register-register only in this slice - no memory [src2], no YMM (VEX.L),
            no three-byte VEX, no EVEX; each is future work, not built here. *)
    | Vsubsd  (** [vsubsd src2, src1, dst] - {!Vaddsd}'s sibling ([VEX.LIG.F2.0F.WIG 5C /r]). *)
    | Vmulsd  (** [vmulsd src2, src1, dst] - {!Vaddsd}'s sibling ([VEX.LIG.F2.0F.WIG 59 /r]). *)
    | Vdivsd  (** [vdivsd src2, src1, dst] - {!Vaddsd}'s sibling ([VEX.LIG.F2.0F.WIG 5E /r]). *)
    | Vaddss
        (** [vaddss src2, src1, dst] - {!Vaddsd}'s scalar-single sibling
            ([VEX.LIG.F3.0F.WIG 58 /r]): same two-byte-VEX shape and the same xmm0-7 restriction
            on [src2], only [pp] (2, not 3) differs. *)
    | Vsubss  (** [vsubss src2, src1, dst] - {!Vaddss}'s sibling ([VEX.LIG.F3.0F.WIG 5C /r]). *)
    | Vmulss  (** [vmulss src2, src1, dst] - {!Vaddss}'s sibling ([VEX.LIG.F3.0F.WIG 59 /r]). *)
    | Vdivss  (** [vdivss src2, src1, dst] - {!Vaddss}'s sibling ([VEX.LIG.F3.0F.WIG 5E /r]). *)
    | Vaddps
        (** [vaddps src2, src1, dst] - {!Vaddsd}'s packed-single sibling ([VEX.128.0F.WIG 58 /r],
            [pp = 0], no mandatory prefix): completes the [pp] square (F2/F3/none/66) for opcode
            [0x58] the legacy {!Addps}/{!Addpd} slice already completed for the non-VEX encoding. *)
    | Vsubps  (** [vsubps src2, src1, dst] - {!Vaddps}'s sibling ([VEX.128.0F.WIG 5C /r]). *)
    | Vmulps  (** [vmulps src2, src1, dst] - {!Vaddps}'s sibling ([VEX.128.0F.WIG 59 /r]). *)
    | Vdivps  (** [vdivps src2, src1, dst] - {!Vaddps}'s sibling ([VEX.128.0F.WIG 5E /r]). *)
    | Vaddpd
        (** [vaddpd src2, src1, dst] - {!Vaddps}'s packed-double sibling ([VEX.128.66.0F.WIG 58 /r],
            [pp = 1], mandatory [66]). *)
    | Vsubpd  (** [vsubpd src2, src1, dst] - {!Vaddpd}'s sibling ([VEX.128.66.0F.WIG 5C /r]). *)
    | Vmulpd  (** [vmulpd src2, src1, dst] - {!Vaddpd}'s sibling ([VEX.128.66.0F.WIG 59 /r]). *)
    | Vdivpd  (** [vdivpd src2, src1, dst] - {!Vaddpd}'s sibling ([VEX.128.66.0F.WIG 5E /r]). *)
    | Vandps
        (** [vandps src2, src1, dst] - the VEX packed-single sibling of the legacy
            {!Andps}/{!Andnps}/{!Orps}/{!Xorps} bitwise-logical family
            ([VEX.128.0F.WIG 54 /r], [pp = 0]): same [vex_scalar_none_codec] group
            {!Vaddps} already uses, a disjoint opcode byte. *)
    | Vandnps  (** [vandnps src2, src1, dst] - {!Vandps}'s sibling ([VEX.128.0F.WIG 55 /r]). *)
    | Vorps  (** [vorps src2, src1, dst] - {!Vandps}'s sibling ([VEX.128.0F.WIG 56 /r]). *)
    | Vxorps  (** [vxorps src2, src1, dst] - {!Vandps}'s sibling ([VEX.128.0F.WIG 57 /r]). *)
    | Vandpd
        (** [vandpd src2, src1, dst] - {!Vandps}'s packed-double sibling
            ([VEX.128.66.0F.WIG 54 /r], [pp = 1], mandatory [66]). *)
    | Vandnpd  (** [vandnpd src2, src1, dst] - {!Vandpd}'s sibling ([VEX.128.66.0F.WIG 55 /r]). *)
    | Vorpd  (** [vorpd src2, src1, dst] - {!Vandpd}'s sibling ([VEX.128.66.0F.WIG 56 /r]). *)
    | Vxorpd  (** [vxorpd src2, src1, dst] - {!Vandpd}'s sibling ([VEX.128.66.0F.WIG 57 /r]). *)
    | Vunpcklps
        (** [vunpcklps src2, src1, dst] - the VEX sibling of the legacy {!Unpcklps}/{!Unpckhps}/
            {!Unpcklpd}/{!Unpckhpd} family ([VEX.128.0F.WIG 14 /r], [pp = 0]): same
            [vex_scalar_none_codec] group {!Vaddps}/{!Vandps} already use, a disjoint opcode
            byte. Confirmed against real GNU as (both [i686-linux-gnu-as] and
            [x86_64-linux-gnu-as] 2.44): a genuine two-source-operand binop like {!Vandps}, not a
            merge-only unop like {!Vsqrtps}, so it reuses {!Vex_binop_rr_rm} unchanged. *)
    | Vunpckhps
        (** [vunpckhps src2, src1, dst] - {!Vunpcklps}'s sibling ([VEX.128.0F.WIG 15 /r]). *)
    | Vunpcklpd
        (** [vunpcklpd src2, src1, dst] - {!Vunpcklps}'s packed-double sibling
            ([VEX.128.66.0F.WIG 14 /r], [pp = 1], mandatory [66]). *)
    | Vunpckhpd
        (** [vunpckhpd src2, src1, dst] - {!Vunpcklpd}'s sibling ([VEX.128.66.0F.WIG 15 /r]). *)
    | Vpunpcklqdq
        (** [vpunpcklqdq src2, src1, dst] - the VEX sibling of the legacy {!Punpcklqdq}/
            {!Punpckhqdq} family ([VEX.128.66.0F.WIG 6C /r], [pp = 1], mandatory [66] only - no
            non-66 sibling, matching the legacy integer-SIMD-only shape). Confirmed against real
            GNU as (both [i686-linux-gnu-as] and [x86_64-linux-gnu-as] 2.44): a genuine
            two-source-operand binop like {!Vunpcklps}, reusing {!Vex_binop_rr_rm} unchanged. *)
    | Vpunpckhqdq
        (** [vpunpckhqdq src2, src1, dst] - {!Vpunpcklqdq}'s sibling ([VEX.128.66.0F.WIG 6D /r]). *)
    | Vpunpcklbw
        (** [vpunpcklbw src2, src1, dst] - the VEX sibling of the legacy {!Punpcklbw}/
            {!Punpckhbw}/{!Punpcklwd}/{!Punpckhwd}/{!Punpckldq}/{!Punpckhdq} family
            ([VEX.128.66.0F.WIG 60 /r]). *)
    | Vpunpckhbw
        (** [vpunpckhbw src2, src1, dst] - {!Vpunpcklbw}'s sibling ([VEX.128.66.0F.WIG 68 /r]). *)
    | Vpunpcklwd
        (** [vpunpcklwd src2, src1, dst] - {!Vpunpcklbw}'s sibling ([VEX.128.66.0F.WIG 61 /r]). *)
    | Vpunpckhwd
        (** [vpunpckhwd src2, src1, dst] - {!Vpunpcklbw}'s sibling ([VEX.128.66.0F.WIG 69 /r]). *)
    | Vpunpckldq
        (** [vpunpckldq src2, src1, dst] - {!Vpunpcklbw}'s sibling ([VEX.128.66.0F.WIG 62 /r]). *)
    | Vpunpckhdq
        (** [vpunpckhdq src2, src1, dst] - {!Vpunpcklbw}'s sibling ([VEX.128.66.0F.WIG 6A /r]). *)
    | Vpaddb
        (** [vpaddb src2, src1, dst] - the VEX sibling of the legacy {!Paddb}/{!Paddw}/{!Paddd}/
            {!Paddq}/{!Psubb}/{!Psubw}/{!Psubd}/{!Psubq} family ([VEX.128.66.0F.WIG FC /r]).
            Confirmed against real GNU as: [vpaddb %xmm3,%xmm2,%xmm1] -> [c5 e9 fc cb]. *)
    | Vpaddw  (** [vpaddw src2, src1, dst] - {!Vpaddb}'s sibling ([VEX.128.66.0F.WIG FD /r]). *)
    | Vpaddd  (** [vpaddd src2, src1, dst] - {!Vpaddb}'s sibling ([VEX.128.66.0F.WIG FE /r]). *)
    | Vpaddq  (** [vpaddq src2, src1, dst] - {!Vpaddb}'s sibling ([VEX.128.66.0F.WIG D4 /r]). *)
    | Vpsubb
        (** [vpsubb src2, src1, dst] - {!Vpaddb}'s own group at a different opcode byte ([VEX.128.66.0F.WIG F8 /r]). *)
    | Vpsubw  (** [vpsubw src2, src1, dst] - {!Vpsubb}'s sibling ([VEX.128.66.0F.WIG F9 /r]). *)
    | Vpsubd  (** [vpsubd src2, src1, dst] - {!Vpsubb}'s sibling ([VEX.128.66.0F.WIG FA /r]). *)
    | Vpsubq  (** [vpsubq src2, src1, dst] - {!Vpsubb}'s sibling ([VEX.128.66.0F.WIG FB /r]). *)
    | Vpcmpeqb
        (** [vpcmpeqb src2, src1, dst] - the VEX sibling of the legacy {!Pcmpeqb}/{!Pcmpeqw}/
            {!Pcmpeqd}/{!Pcmpgtb}/{!Pcmpgtw}/{!Pcmpgtd} family ([VEX.128.66.0F.WIG 74 /r]).
            Confirmed against real GNU as: [vpcmpeqb %xmm3,%xmm2,%xmm1] -> [c5 e9 74 cb]. *)
    | Vpcmpeqw
        (** [vpcmpeqw src2, src1, dst] - {!Vpcmpeqb}'s sibling ([VEX.128.66.0F.WIG 75 /r]). *)
    | Vpcmpeqd
        (** [vpcmpeqd src2, src1, dst] - {!Vpcmpeqb}'s sibling ([VEX.128.66.0F.WIG 76 /r]). *)
    | Vpcmpgtb
        (** [vpcmpgtb src2, src1, dst] - {!Vpcmpeqb}'s own group at a different opcode byte
            ([VEX.128.66.0F.WIG 64 /r]). *)
    | Vpcmpgtw
        (** [vpcmpgtw src2, src1, dst] - {!Vpcmpgtb}'s sibling ([VEX.128.66.0F.WIG 65 /r]). *)
    | Vpcmpgtd
        (** [vpcmpgtd src2, src1, dst] - {!Vpcmpgtb}'s sibling ([VEX.128.66.0F.WIG 66 /r]). *)
    | Vpacksswb
        (** [vpacksswb src2, src1, dst] - the VEX sibling of the legacy {!Packsswb}/{!Packssdw}/
            {!Packuswb} family ([VEX.128.66.0F.WIG 63 /r]). Confirmed against real GNU as:
            [vpacksswb %xmm3,%xmm2,%xmm1] -> [c5 e9 63 cb]. *)
    | Vpackssdw
        (** [vpackssdw src2, src1, dst] - {!Vpacksswb}'s sibling ([VEX.128.66.0F.WIG 6B /r]). *)
    | Vpackuswb
        (** [vpackuswb src2, src1, dst] - {!Vpacksswb}'s sibling ([VEX.128.66.0F.WIG 67 /r]). *)
    | Vpand
        (** [vpand src2, src1, dst] - the VEX sibling of the legacy {!Pand}/{!Pandn}/{!Por}
            family ([VEX.128.66.0F.WIG DB /r]). Confirmed against real GNU as:
            [vpand %xmm3,%xmm2,%xmm1] -> [c5 e9 db cb]. *)
    | Vpandn  (** [vpandn src2, src1, dst] - {!Vpand}'s sibling ([VEX.128.66.0F.WIG DF /r]). *)
    | Vpor  (** [vpor src2, src1, dst] - {!Vpand}'s sibling ([VEX.128.66.0F.WIG EB /r]). *)
    | Vpminub
        (** [vpminub src2, src1, dst] - the VEX sibling of the legacy {!Pminub}/{!Pmaxub}/
            {!Pminsw}/{!Pmaxsw} family ([VEX.128.66.0F.WIG DA /r]). *)
    | Vpmaxub  (** [vpmaxub src2, src1, dst] - {!Vpminub}'s sibling ([VEX.128.66.0F.WIG DE /r]). *)
    | Vpminsw  (** [vpminsw src2, src1, dst] - {!Vpminub}'s sibling ([VEX.128.66.0F.WIG EA /r]). *)
    | Vpmaxsw  (** [vpmaxsw src2, src1, dst] - {!Vpminub}'s sibling ([VEX.128.66.0F.WIG EE /r]). *)
    | Vmaxsd
        (** [vmaxsd src2, src1, dst] - the VEX sibling of the legacy {!Maxsd}/{!Minsd}/{!Maxss}/
            {!Minss}/{!Maxps}/{!Minps}/{!Maxpd}/{!Minpd} family ([VEX.LIG.F2.0F.WIG 5F /r],
            [pp = 3]): same [vex_scalar_f2_codec] group {!Vaddsd} already uses, a disjoint
            opcode byte. *)
    | Vminsd  (** [vminsd src2, src1, dst] - {!Vmaxsd}'s sibling ([VEX.LIG.F2.0F.WIG 5D /r]). *)
    | Vmaxss
        (** [vmaxss src2, src1, dst] - {!Vmaxsd}'s scalar-single sibling ([VEX.LIG.F3.0F.WIG 5F /r], [pp = 2]). *)
    | Vminss  (** [vminss src2, src1, dst] - {!Vmaxss}'s sibling ([VEX.LIG.F3.0F.WIG 5D /r]). *)
    | Vmaxps
        (** [vmaxps src2, src1, dst] - {!Vmaxsd}'s packed-single sibling ([VEX.128.0F.WIG 5F /r], [pp = 0]). *)
    | Vminps  (** [vminps src2, src1, dst] - {!Vmaxps}'s sibling ([VEX.128.0F.WIG 5D /r]). *)
    | Vmaxpd
        (** [vmaxpd src2, src1, dst] - {!Vmaxps}'s packed-double sibling ([VEX.128.66.0F.WIG 5F /r], [pp = 1]). *)
    | Vminpd  (** [vminpd src2, src1, dst] - {!Vmaxpd}'s sibling ([VEX.128.66.0F.WIG 5D /r]). *)
    | Vsqrtsd
        (** [vsqrtsd src2, src1, dst] - the VEX sibling of the legacy {!Sqrtsd}/{!Sqrtss}/
            {!Sqrtps}/{!Sqrtpd} family ([VEX.LIG.F2.0F.WIG 51 /r], [pp = 3]): same
            [vex_scalar_f2_codec] group {!Vaddsd} already uses. Confirmed against real GNU as
            that [src1] ([vvvv]) is real here too, even though the CPU only uses it to merge the
            destination's upper bits rather than as a second arithmetic input - the byte-level
            operand-to-field mapping is identical to {!Vmaxsd}'s, which is all this project's
            encoder needs to reuse {!Vex_binop_rr_rm} unchanged. *)
    | Vsqrtss
        (** [vsqrtss src2, src1, dst] - {!Vsqrtsd}'s scalar-single sibling ([VEX.LIG.F3.0F.WIG 51 /r], [pp = 2]). *)
    | Vsqrtps
        (** [vsqrtps src, dst] - {!Vsqrtsd}'s packed-single sibling ([VEX.128.0F.WIG 51 /r],
            [pp = 0]), genuinely two-operand: confirmed against real GNU as, which rejects a
            third operand ("number of operands mismatch") since there is no scalar
            upper-bits-preservation concept for a fully-packed op - see {!Vex_unop_r_rm}. *)
    | Vsqrtpd
        (** [vsqrtpd src, dst] - {!Vsqrtps}'s packed-double sibling ([VEX.128.66.0F.WIG 51 /r], [pp = 1]). *)
    | Vmovaps
        (** [vmovaps src, dst] - the VEX sibling of the legacy {!Movaps}/{!Movups}/{!Movapd}/
            {!Movupd} family ([VEX.128.0F.WIG 28 /r], [pp = 0]), reusing {!Vex_unop_r_rm} the
            same way {!Vsqrtps} does: confirmed against real GNU as that register-register and
            register<-memory both use this opcode, with register-register also reachable through
            the redundant [0x29] iform GAS never selects (the same "low-numbered iform" precedent
            {!Movapd}'s own comment already established) - left unadmitted here too, along with
            the real [MEMdq<-XMMdq] store direction, which is a separately admittable, genuinely
            distinct opcode ([0x29]) rather than a redundancy. *)
    | Vmovups
        (** [vmovups src, dst] - {!Vmovaps}'s unaligned sibling ([VEX.128.0F.WIG 10 /r], [pp = 0]). *)
    | Vmovapd
        (** [vmovapd src, dst] - {!Vmovaps}'s packed-double sibling ([VEX.128.66.0F.WIG 28 /r], [pp = 1]). *)
    | Vmovupd
        (** [vmovupd src, dst] - {!Vmovups}'s packed-double sibling ([VEX.128.66.0F.WIG 10 /r], [pp = 1]). *)
    | Vcomisd
        (** [vcomisd src, dst] - the VEX sibling of the legacy {!Comisd}/{!Ucomisd}/{!Comiss}/
            {!Ucomiss} family ([VEX.LIG.66.0F.WIG 2F /r], [pp = 1]), reusing {!Vex_unop_r_rm}
            the same way {!Vsqrtps}/{!Vmovaps} do: confirmed against real GNU as that both
            operands are read-only (no destination register is actually written; the real
            result goes to EFLAGS) with [VEX.vvvv] the same literal [1111] "unused" pattern as
            every other genuinely-two-operand VEX form - this project's encoder only assembles
            bytes, so the ModRM/vvvv shape being identical to {!Vsqrtps}'s is what matters, not
            which operand the CPU treats as writable. *)
    | Vucomisd  (** [vucomisd src, dst] - {!Vcomisd}'s sibling ([VEX.LIG.66.0F.WIG 2E /r]). *)
    | Vcomiss
        (** [vcomiss src, dst] - {!Vcomisd}'s mandatory-prefix-free sibling
            ([VEX.LIG.0F.WIG 2F /r], [pp = 0]). *)
    | Vucomiss  (** [vucomiss src, dst] - {!Vcomiss}'s sibling ([VEX.LIG.0F.WIG 2E /r]). *)
    | Vcvtps2pd
        (** [vcvtps2pd src, dst] - the VEX sibling of the legacy {!Cvtps2pd}/{!Cvtpd2ps} family
            ([VEX.128.0F.WIG 5A /r], [pp = 0]), reusing {!Vex_unop_r_rm} the same way
            {!Vsqrtps}/{!Vmovaps}/{!Vcomisd} do. Confirmed against real GNU as: both
            register-register and register<-memory are unambiguous, since the destination
            register class ([xmm] here, never [ymm]) already pins which of VEX.128's [m64]
            source or VEX.256's [m128] source is meant - unlike {!Vcvtpd2ps}. *)
    | Vcvtpd2ps
        (** [vcvtpd2ps src, dst] - {!Vcvtps2pd}'s mandatory-66 sibling
            ([VEX.128.66.0F.WIG 5A /r], [pp = 1]). Register-register only: confirmed against
            real GNU as that the register<-memory spelling is genuinely ambiguous here (unlike
            every other {!Vex_unop_r_rm} mnemonic) - VEX.128's [xmm/m128] source and VEX.256's
            [ymm/m256] source narrow to the *same* xmm destination class, so a bare memory
            operand cannot disambiguate the way a register operand's own class does; real GNU as
            rejects [vcvtpd2ps mem, %xmmN] outright ("operand size mismatch") and requires the
            separate [vcvtpd2psx]/[vcvtpd2psy] disambiguating spellings this project's parser
            does not implement. Left as a named follow-up rather than admitted. *)
    | Vcvtdq2ps
        (** [vcvtdq2ps src, dst] - the VEX sibling of the legacy {!Cvtdq2ps}/{!Cvtps2dq}/
            {!Cvttps2dq} family ([VEX.128.0F.WIG 5B /r], [pp = 0]), reusing {!Vex_unop_r_rm}.
            Confirmed against real GNU as: both directions unambiguous for all three mnemonics
            at VEX.128 (no VEX.256 same-destination-class collision the way {!Vcvtpd2ps} hits,
            since none of these three narrow two different source widths onto one destination
            class). *)
    | Vcvtps2dq
        (** [vcvtps2dq src, dst] - {!Vcvtdq2ps}'s mandatory-66 sibling
            ([VEX.128.66.0F.WIG 5B /r], [pp = 1]). *)
    | Vcvttps2dq
        (** [vcvttps2dq src, dst] - the truncating variant's VEX sibling
            ([VEX.128.F3.0F.WIG 5B /r], [pp = 2]). *)
    | Vshufps
        (** [vshufps $imm8, src2, src1, dst] - VEX-encoded packed shuffle, single precision
            ([VEX.128.0F.WIG C6 /r ib], [pp = 0]), {!Shufps}'s non-destructive three-operand
            VEX sibling: {!Lowered.Vex_binop_imm_rr_rm} rather than {!Lowered.Vex_binop_rr_rm},
            since real [src1]/[src2] plus a trailing imm8 selector need a field {!Vex_binop_rr_rm}
            has no room for. Confirmed against real GNU as: [vshufps $0x1b,%xmm3,%xmm2,%xmm1]
            -> [c5 e8 c6 cb 1b]. *)
    | Vshufpd
        (** [vshufpd $imm8, src2, src1, dst] - {!Vshufps}'s mandatory-66 ([pp = 1]) sibling at
            the same opcode byte ([VEX.128.66.0F.WIG C6 /r ib]). Confirmed against real GNU as:
            [vshufpd $0x1,%xmm3,%xmm2,%xmm1] -> [c5 e9 c6 cb 01]. *)
    | Vcmpss
        (** [vcmpss $imm8, src2, src1, dst] - the VEX sibling of the legacy {!Cmpss}/{!Cmpsd}/
            {!Cmpps}/{!Cmppd} family ([VEX.LIG.F3.0F.WIG C2 /r ib], [pp = 2]), reusing
            {!Lowered.Vex_binop_imm_rr_rm} the same way {!Vshufps} does. Confirmed against real
            GNU as: [vcmpss $0x0,%xmm3,%xmm2,%xmm1] -> [c5 ea c2 cb 00]. *)
    | Vcmpsd
        (** [vcmpsd $imm8, src2, src1, dst] - {!Vcmpss}'s mandatory-[F2] ([pp = 3]) counterpart
            ([VEX.LIG.F2.0F.WIG C2 /r ib]). *)
    | Vcmpps
        (** [vcmpps $imm8, src2, src1, dst] - {!Vcmpss}'s mandatory-prefix-free ([pp = 0])
            counterpart ([VEX.128.0F.WIG C2 /r ib]), joining {!Vshufps}'s own [pp = 0] imm8
            group. *)
    | Vcmppd
        (** [vcmppd $imm8, src2, src1, dst] - {!Vcmpps}'s mandatory-66 ([pp = 1]) counterpart
            ([VEX.128.66.0F.WIG C2 /r ib]), joining {!Vshufpd}'s own [pp = 1] imm8 group. *)
    | Vpshufd
        (** [vpshufd $imm8, src, dst] - the VEX sibling of the legacy {!Pshufd}/{!Pshuflw}/
            {!Pshufhw} family ([VEX.128.66.0F.WIG 70 /r ib], [pp = 1]): genuinely two-operand-
            plus-immediate, no real [vvvv] operand at all (confirmed against real GNU as, which
            rejects a third operand outright, the same way {!Vsqrtps}'s own comment explains for
            the non-immediate packed unary forms) - {!Lowered.Vex_unop_imm_r_rm} rather than
            {!Lowered.Vex_binop_imm_rr_rm}. Confirmed against real GNU as:
            [vpshufd $0x1b,%xmm2,%xmm1] -> [c5 f9 70 ca 1b]. *)
    | Vpshuflw
        (** [vpshuflw $imm8, src, dst] - {!Vpshufd}'s mandatory-[F2] ([pp = 3]) sibling at the
            same opcode byte ([VEX.128.F2.0F.WIG 70 /r ib]). *)
    | Vpshufhw
        (** [vpshufhw $imm8, src, dst] - {!Vpshufd}'s mandatory-[F3] ([pp = 2]) sibling at the
            same opcode byte ([VEX.128.F3.0F.WIG 70 /r ib]). *)
    | Vmovd
        (** [vmovd rm, dst] / [vmovd reg, rm] - the VEX sibling of the legacy {!Movd}, GPR32<->xmm
            data move only ([VEX.128.66.0F.W0 6E|7E /r]): unlike {!Movd}, which also spells the
            GPR64<->xmm form as [movq] via REX.W, no [vmovq] sibling exists here, because the
            two-byte VEX prefix ([0xC5]) has no W bit at all - a GPR64<->xmm VEX move needs the
            three-byte VEX prefix ([0xC4]), which this project has not built (out of scope).
            Reusing {!Lowered.Cvtsi2f_r_rm}/{!Movd_rm_r}'s [width = i.Instruction.width] dispatch
            fixes this naturally: {!Vmovd} is always constructed with [width = 32], so a 64-bit
            GPR operand is rejected by the ordinary [width_ok] register-width mismatch check, not
            by anything VEX-specific. Confirmed against real GNU as: [vmovd %eax,%xmm0] ->
            [c5 f9 6e c0], [vmovd %xmm0,%eax] -> [c5 f9 7e c0], both register-register and
            register<->memory. Unlike the legacy [movd]/[movq] pair, real GNU as does NOT reject
            [vmovd %rax,%xmm0] - it silently reassembles it as [vmovq] instead ([c4 e1 f9 6e c0],
            three-byte VEX, [VEX.W1]), the same width-driven mnemonic substitution {!Movd}'s own
            [movd]/[movq] dispatch already does at the text-parsing level. This project does not
            do that substitution (no [vmovq]/three-byte-VEX support exists), so a 64-bit GPR
            operand here is a genuine, deliberate rejection - not a divergence from GNU as's
            accepted-forms set, since the intended [vmovq] spelling is simply not implemented
            yet, the same "explicit rejection of an unimplemented but real form" every other
            three-byte-VEX gap in this project already has. *)
    | Vpmullw
        (** [vpmullw src2, src1, dst] - the VEX sibling of the legacy {!Pmullw}/{!Pmulhw}/
            {!Pmulhuw}/{!Pavgb}/{!Pavgw}/{!Psadbw} family ([VEX.128.66.0F.WIG D5 /r]). Confirmed
            against real GNU as: [vpmullw %xmm3,%xmm2,%xmm1] -> [c5 e9 d5 cb]. *)
    | Vpmulhw  (** [vpmulhw src2, src1, dst] - {!Vpmullw}'s sibling ([VEX.128.66.0F.WIG E5 /r]). *)
    | Vpmulhuw
        (** [vpmulhuw src2, src1, dst] - {!Vpmullw}'s sibling ([VEX.128.66.0F.WIG E4 /r]). *)
    | Vpavgb  (** [vpavgb src2, src1, dst] - {!Vpmullw}'s own group ([VEX.128.66.0F.WIG E0 /r]). *)
    | Vpavgw  (** [vpavgw src2, src1, dst] - {!Vpavgb}'s sibling ([VEX.128.66.0F.WIG E3 /r]). *)
    | Vpsadbw  (** [vpsadbw src2, src1, dst] - {!Vpavgb}'s own group ([VEX.128.66.0F.WIG F6 /r]). *)
    | Vpsllw
        (** [vpsllw src2, src1, dst] - the VEX sibling of the legacy {!Psllw}/{!Pslld}/{!Psllq}/
            {!Psrlw}/{!Psrld}/{!Psrlq}/{!Psraw}/{!Psrad} register/memory-count shift family
            ([VEX.128.66.0F.WIG F1 /r]); the separate immediate-count group-opcode form is
            {!Lowered.Vex_shift_imm_rm} instead. Confirmed against real GNU as:
            [vpsllw %xmm2,%xmm1,%xmm0] -> [c5 f1 f1 c2]. *)
    | Vpslld  (** [vpslld src2, src1, dst] - {!Vpsllw}'s sibling ([VEX.128.66.0F.WIG F2 /r]). *)
    | Vpsllq  (** [vpsllq src2, src1, dst] - {!Vpsllw}'s sibling ([VEX.128.66.0F.WIG F3 /r]). *)
    | Vpsrlw
        (** [vpsrlw src2, src1, dst] - {!Vpsllw}'s own group at a different opcode byte
            ([VEX.128.66.0F.WIG D1 /r]). *)
    | Vpsrld  (** [vpsrld src2, src1, dst] - {!Vpsrlw}'s sibling ([VEX.128.66.0F.WIG D2 /r]). *)
    | Vpsrlq  (** [vpsrlq src2, src1, dst] - {!Vpsrlw}'s sibling ([VEX.128.66.0F.WIG D3 /r]). *)
    | Vpsraw
        (** [vpsraw src2, src1, dst] - {!Vpsllw}'s own group at a different opcode byte
            ([VEX.128.66.0F.WIG E1 /r]). *)
    | Vpsrad
        (** [vpsrad src2, src1, dst] - {!Vpsraw}'s sibling ([VEX.128.66.0F.WIG E2 /r]). There is
            no 2-byte-VEX [vpsraq]: confirmed against real GNU as that spelling assembles to an
            EVEX-encoded ([62 ..]) AVX-512VL form instead, not the plain-VEX ([c5]/[c4]) shape
            every other mnemonic here uses - out of scope, the same "no EVEX/3-byte-VEX
            infrastructure" gap [vmovq] (VEX.W1) was already found to need. *)
    | Vpslldq
        (** [vpslldq $imm8, src, dst] - {!Pslldq}'s VEX sibling ([VEX.128.66.0F.WIG 73 /7 ib]),
            {!Vex_shift_imm_rm}'s own real-[vvvv] shape. Confirmed against real GNU as:
            [vpslldq $5,%xmm2,%xmm1] -> [c5 f1 73 fa 05]. *)
    | Vpsrldq
        (** [vpsrldq $imm8, src, dst] - {!Vpslldq}'s shift-right sibling
            ([VEX.128.66.0F.WIG 73 /3 ib]). Confirmed against real GNU as:
            [vpsrldq $5,%xmm2,%xmm1] -> [c5 f1 73 da 05]. *)
    | Vmovdqa
        (** [vmovdqa rm, dst] - the VEX sibling of the legacy {!Movdqa}/{!Movdqu} family
            ([VEX.128.66.0F.WIG 6F /r], [pp = 1]), reusing {!Lowered.Vex_unop_r_rm} the same way
            {!Vmovaps} does - load direction only (register-register and register<-memory); the
            store direction ([VEX.128.66.0F.WIG 7F /r]) is a distinct, real form this project
            does not yet build, matching {!Vmovaps}'s own precedent. Confirmed against real GNU
            as: [vmovdqa %xmm2,%xmm1] -> [c5 f9 6f ca]. *)
    | Vmovdqu
        (** [vmovdqu rm, dst] - {!Vmovdqa}'s mandatory-[F3] ([pp = 2]) counterpart at the same
            opcode byte ([VEX.128.F3.0F.WIG 6F /r]), the first {!Lowered.Vex_unop_r_rm} mnemonic
            needing a mandatory-[F3] VEX codec table (every prior {!Vex_unop_r_rm} member used
            [pp = 0] or [pp = 1] only). Load direction only, matching {!Vmovdqa}. Confirmed against
            real GNU as: [vmovdqu %xmm2,%xmm1] -> [c5 fa 6f ca]. *)
    | Pinsrw
        (** [pinsrw $imm8, gpr32/m16, xmm] - packed insert word ([66 0F C4 /r ib]):
            {!Lowered.Sse_binop_imm_r_rm}'s first cross-register-class member - [reg] is the xmm
            destination, [rm] is a GPR32 or 16-bit-memory source rather than xmm, the same class
            split {!Cvtsi2sd}'s own [rm] uses. No REX.W-equivalent variant exists (always a 32-bit
            GPR source): confirmed against real GNU as, [pinsrw $1,%rax,%xmm0] assembles
            identically to the [%eax] spelling ([66 0f c4 c0 01], no REX.W emitted either way), so
            this project's own [width_ok] naturally rejects the 64-bit-named spelling as an
            unimplemented-but-real substitution, the same deliberate boundary {!Vmovd}'s own doc
            comment describes for [vmovq]. Confirmed against real GNU as: [pinsrw $1,%eax,%xmm0]
            -> [66 0f c4 c0 01], [pinsrw $1,(%eax),%xmm0] -> [66 0f c4 00 01]. *)
    | Pextrw
        (** [pextrw $imm8, xmm, gpr32] - packed extract word ([66 0F C5 /r ib]), {!Pinsrw}'s
            store-direction mirror: [reg] is the GPR32 destination, [rm] is an xmm source
            restricted to a register - no memory form exists for this two-byte opcode. Confirmed
            against real GNU as: [pextrw $1,%xmm0,(%eax)] silently reassembles as the unrelated,
            three-byte-opcode SSE4.1 [PEXTRW r32,xmm,imm8] instruction instead
            ([66 0f 3a 15 00 01]) - a genuine, deliberate scope boundary this project does not
            build (no [0F38]/[0F3A] opcode-map infrastructure exists, confirmed absent when
            {!Packsswb} was admitted), not a divergence from GNU as's accepted forms. Confirmed
            against real GNU as: [pextrw $1,%xmm0,%eax] -> [66 0f c5 c0 01]. *)
    | Vpinsrw
        (** [vpinsrw $imm8, gpr32/m16, src1, dst] - the VEX sibling of the legacy {!Pinsrw}
            ([VEX.128.66.0F.WIG C4 /r ib]): {!Lowered.Vex_binop_imm_rr_rm}'s own cross-register-
            class member, [src2] a GPR32 (or memory) source rather than xmm - the VEX-and-
            immediate-carrying sibling of {!Vmovd}'s own GPR-crossing shape. A single mandatory-66
            ([pp = 1]), fixed-opcode entry: {!Vpinsrw} has no other mandatory-prefix sibling at
            this opcode byte the way {!Vshufps} does, mirroring {!Vmovd}'s own unparametrized
            singleton shape. Confirmed against real GNU as: [vpinsrw $1,%eax,%xmm2,%xmm1] ->
            [c5 e9 c4 c8 01], [vpinsrw $1,(%eax),%xmm2,%xmm1] -> [c5 e9 c4 08 01]. *)
    | Vpextrw
        (** [vpextrw $imm8, xmm, gpr32] - the VEX sibling of the legacy {!Pextrw}
            ([VEX.128.66.0F.WIG C5 /r ib]): {!Lowered.Vex_unop_imm_r_rm}'s own cross-register-
            class member, [dst] a GPR32 rather than xmm. Register-only [src] (no memory form
            exists at this opcode - confirmed against real GNU as, [vpextrw $1,%xmm1,(%eax)]
            silently reassembles as the unrelated three-byte-opcode [c4 e3 79 15 08 01] instead,
            out of scope, matching {!Pextrw}'s own legacy precedent exactly). Confirmed against
            real GNU as: [vpextrw $1,%xmm1,%eax] -> [c5 f9 c5 c1 01]. *)
    | Movmskps
        (** [movmskps xmm, gpr32] - extract each packed-single lane's sign bit into a GPR
            ([0F 50 /r], no mandatory prefix): {!Sse_binop_r_rm}'s cross-register-class member,
            [reg] the GPR32 destination (width 32, {!Pextrw}'s own [reg] convention) and [rm] an
            xmm source (width 128) restricted to a register by construction - confirmed against
            real GNU as, [movmskps (%eax),%eax] is rejected outright ("operand size mismatch"; no
            memory form exists at all, unlike {!Pextrw} where the memory spelling is merely
            routed elsewhere). No REX.W-equivalent 64-bit-GPR variant exists either: real GNU as
            silently accepts the [%rax]-named spelling as identical to [%eax] (no REX.W emitted),
            which this project's own [width_ok] naturally rejects as an unimplemented-but-real
            substitution, the same deliberate boundary {!Vmovd}'s own doc comment describes for
            [vmovq]. Confirmed against real GNU as: [movmskps %xmm0,%eax] -> [0f 50 c0]. *)
    | Movmskpd
        (** [movmskpd xmm, gpr32] - {!Movmskps}'s mandatory-66 sibling ([66 0F 50 /r], packed
            double). Confirmed against real GNU as: [movmskpd %xmm1,%ecx] -> [66 0f 50 c9]. *)
    | Pmovmskb
        (** [pmovmskb xmm, gpr32] - {!Movmskps}'s integer sibling at a different opcode byte
            ([66 0F D7 /r], packed byte). Confirmed against real GNU as: [pmovmskb %xmm2,%edx] ->
            [66 0f d7 d2]. *)
    | Vmovmskps
        (** [vmovmskps xmm, gpr32] - the VEX sibling of the legacy {!Movmskps}
            ([VEX.128.0F.WIG 50 /r]): {!Lowered.Vex_unop_r_rm}'s own cross-register-class member,
            [dst] a GPR32 rather than xmm - the mandatory-prefix-free counterpart of {!Vmovd}'s
            own GPR-crossing shape. Confirmed against real GNU as: [vmovmskps %xmm0,%eax] ->
            [c5 f8 50 c0]. *)
    | Vmovmskpd
        (** [vmovmskpd xmm, gpr32] - {!Vmovmskps}'s mandatory-66 sibling ([VEX.128.66.0F.WIG
            50 /r]). Confirmed against real GNU as: [vmovmskpd %xmm1,%ecx] -> [c5 f9 50 c9]. *)
    | Vpmovmskb
        (** [vpmovmskb xmm, gpr32] - {!Vmovmskps}'s integer sibling at a different opcode byte
            ([VEX.128.66.0F.WIG D7 /r]). Confirmed against real GNU as: [vpmovmskb %xmm2,%edx] ->
            [c5 f9 d7 d2]. *)
    | Fldl
    | Fstpl
    | Fstps
    | Flds
    | Fildll
        (** [fildll mem] - x87 64-bit integer load ([0xDF /5]), GAS's own spelling of FILD m64int -
            distinct from the 32-bit [fildl]/[0xDB /0] no fixture evidences. Shares {!Lowered.Fpu_mem}
            with {!Fldl}/{!Fstpl}/{!Fstps}/{!Flds}: same opcode-plus-ModR/M-extension shape, a
            disjoint opcode byte and extension. *)
    | Fadds
        (** [fadds mem] - x87 single-precision add ([st(0) := st(0) + mem], [0xD8 /0]), a memory-
            source-only arithmetic sibling of {!Fldl}/{!Fstpl}/{!Fstps}/{!Flds}/{!Fildll}'s pure
            load/store family - same {!Lowered.Fpu_mem} shape, one more disjoint opcode/extension
            pair. Evidenced only against a bare-symbol source (M5, asm/docs/corpus.md -
            gas_frontier.t's runtime-i64_utod.S/i64_utof.S), so it shares {!Flds}'s
            [mem_of_symbol] duality rather than {!Fldl}/{!Fstpl}/{!Fstps}'s memory-operand-only
            scope. *)
    | Fadd
        (** [fadd %st(i), %st] ([0xD8 0xC0+i]): register-stack add with ST0
            as its implicit read-write destination.  This is deliberately not
            the reverse-direction FADD_X87_ST0 or FADDP form. *)
    | Fucomp
    | Fnstcw
        (** [fnstcw mem] - x87 store-control-word ([0xD9 /7], M5, asm/docs/corpus.md -
            gas_frontier.t's runtime-i64_dtos.S/i64_dtou.S own round-to-nearest-then-
            truncate idiom around an integer conversion). Shares {!Lowered.Fpu_mem}
            with {!Fldl}/.../{!Fadds}: same opcode-plus-ModR/M-extension shape, one
            more disjoint opcode/extension pair. *)
    | Fldcw
        (** [fldcw mem] - x87 load-control-word ([0xD9 /5]), {!Fnstcw}'s load-back
            counterpart restoring the saved rounding mode (M5, asm/docs/corpus.md -
            same fixtures as {!Fnstcw}). *)
    | Fistpll
        (** [fistpll mem] - x87 64-bit integer store-and-pop ([0xDF /7]), {!Fildll}'s
            store direction (M5, asm/docs/corpus.md - same fixtures as {!Fnstcw}). *)
    | Fsubs
        (** [fsubs mem] - x87 single-precision subtract ([st(0) := st(0) - mem],
            [0xD8 /4]), {!Fadds}'s exact sibling at a different ModR/M extension,
            including the same bare-symbol [mem_of_symbol] duality (M5,
            asm/docs/corpus.md - gas_frontier.t's runtime-i64_dtou.S). *)
    | Fnstsw
        (** [fnstsw %ax] - x87 store-status-word into [%ax] ([0xDF 0xE0]), a fixed
            two-byte word with no ModR/M, the same "no operand" shape {!Fucomp}
            already uses - the [%ax] destination is implicit in the opcode, not an
            encoded operand (M5, asm/docs/corpus.md - gas_frontier.t's
            runtime-i64_dtou.S own status-word-into-[sahf] idiom). *)
    | Sahf
    | Table of int  (** a generated {!X86_table_rows} row, by index (DEC-X86-TABLE) *)

  let name = function
    | Add -> "add"
    | Sub -> "sub"
    | Mov -> "mov"
    | Lea -> "lea"
    | Ret -> "ret"
    | Xor -> "xor"
    | And -> "and"
    | Cmp -> "cmp"
    | Imul -> "imul"
    | Cmov c -> "cmov" ^ Cc.name c
    | Jcc c -> "j" ^ Cc.name c
    | Ud2 -> "ud2"
    | Pop -> "pop"
    | Jmp -> "jmp"
    | Call -> "call"
    | Push -> "push"
    | Neg -> "neg"
    | Test -> "test"
    | Adc -> "adc"
    | Sbb -> "sbb"
    | Mul -> "mul"
    | Div -> "div"
    | Dec -> "dec"
    | Rcr -> "rcr"
    | Shr -> "shr"
    | Or -> "or"
    | Not -> "not"
    | Ror -> "ror"
    | Shl -> "shl"
    | Sar -> "sar"
    | Shld -> "shld"
    | Setcc c -> "set" ^ Cc.name c
    | Movzx { src_width } -> ( "movz" ^ match src_width with 8 -> "b" | 16 -> "w" | _ -> "?")
    | Movsx { src_width } -> (
        "movs" ^ match src_width with 8 -> "b" | 16 -> "w" | 32 -> "l" | _ -> "?")
    | Addsd -> "addsd"
    | Subsd -> "subsd"
    | Mulsd -> "mulsd"
    | Divsd -> "divsd"
    | Addss -> "addss"
    | Subss -> "subss"
    | Mulss -> "mulss"
    | Divss -> "divss"
    | Comisd -> "comisd"
    | Ucomisd -> "ucomisd"
    | Comiss -> "comiss"
    | Ucomiss -> "ucomiss"
    | Xorpd -> "xorpd"
    | Pxor -> "pxor"
    | Movapd -> "movapd"
    | Cvtsd2ss -> "cvtsd2ss"
    | Cvtss2sd -> "cvtss2sd"
    | Cvtps2pd -> "cvtps2pd"
    | Cvtpd2ps -> "cvtpd2ps"
    | Cvtdq2ps -> "cvtdq2ps"
    | Cvtps2dq -> "cvtps2dq"
    | Cvttps2dq -> "cvttps2dq"
    | Movsd -> "movsd"
    | Movss -> "movss"
    | Cvtsi2sd -> "cvtsi2sd"
    | Cvtsi2ss -> "cvtsi2ss"
    | Cvttsd2si -> "cvttsd2si"
    | Unpcklps -> "unpcklps"
    | Unpckhps -> "unpckhps"
    | Unpcklpd -> "unpcklpd"
    | Unpckhpd -> "unpckhpd"
    | Punpcklqdq -> "punpcklqdq"
    | Punpckhqdq -> "punpckhqdq"
    | Punpcklbw -> "punpcklbw"
    | Punpckhbw -> "punpckhbw"
    | Punpcklwd -> "punpcklwd"
    | Punpckhwd -> "punpckhwd"
    | Punpckldq -> "punpckldq"
    | Punpckhdq -> "punpckhdq"
    | Paddb -> "paddb"
    | Paddw -> "paddw"
    | Paddd -> "paddd"
    | Paddq -> "paddq"
    | Psubb -> "psubb"
    | Psubw -> "psubw"
    | Psubd -> "psubd"
    | Psubq -> "psubq"
    | Pcmpeqb -> "pcmpeqb"
    | Pcmpeqw -> "pcmpeqw"
    | Pcmpeqd -> "pcmpeqd"
    | Pcmpgtb -> "pcmpgtb"
    | Pcmpgtw -> "pcmpgtw"
    | Pcmpgtd -> "pcmpgtd"
    | Packsswb -> "packsswb"
    | Packssdw -> "packssdw"
    | Packuswb -> "packuswb"
    | Pand -> "pand"
    | Pandn -> "pandn"
    | Por -> "por"
    | Pminub -> "pminub"
    | Pmaxub -> "pmaxub"
    | Pminsw -> "pminsw"
    | Pmaxsw -> "pmaxsw"
    | Pmullw -> "pmullw"
    | Pmulhw -> "pmulhw"
    | Pmulhuw -> "pmulhuw"
    | Pavgb -> "pavgb"
    | Pavgw -> "pavgw"
    | Psadbw -> "psadbw"
    | Psllw -> "psllw"
    | Pslld -> "pslld"
    | Psllq -> "psllq"
    | Psrlw -> "psrlw"
    | Psrld -> "psrld"
    | Psrlq -> "psrlq"
    | Psraw -> "psraw"
    | Psrad -> "psrad"
    | Pslldq -> "pslldq"
    | Psrldq -> "psrldq"
    | Pshufb -> "pshufb"
    | Phaddw -> "phaddw"
    | Phaddd -> "phaddd"
    | Phsubw -> "phsubw"
    | Phsubd -> "phsubd"
    | Psignb -> "psignb"
    | Psignw -> "psignw"
    | Psignd -> "psignd"
    | Pmaddubsw -> "pmaddubsw"
    | Pmulhrsw -> "pmulhrsw"
    | Phaddsw -> "phaddsw"
    | Phsubsw -> "phsubsw"
    | Pabsb -> "pabsb"
    | Pabsw -> "pabsw"
    | Pabsd -> "pabsd"
    | Palignr -> "palignr"
    | Roundps -> "roundps"
    | Roundpd -> "roundpd"
    | Roundss -> "roundss"
    | Roundsd -> "roundsd"
    | Pcmpeqq -> "pcmpeqq"
    | Pcmpgtq -> "pcmpgtq"
    | Packusdw -> "packusdw"
    | Pmaxsb -> "pmaxsb"
    | Pmaxsd -> "pmaxsd"
    | Pmaxud -> "pmaxud"
    | Pmaxuw -> "pmaxuw"
    | Pminsb -> "pminsb"
    | Pminsd -> "pminsd"
    | Pminud -> "pminud"
    | Pminuw -> "pminuw"
    | Pmuldq -> "pmuldq"
    | Pmulld -> "pmulld"
    | Phminposuw -> "phminposuw"
    | Ptest -> "ptest"
    | Pmovsxbw -> "pmovsxbw"
    | Pmovsxbd -> "pmovsxbd"
    | Pmovsxbq -> "pmovsxbq"
    | Pmovsxwd -> "pmovsxwd"
    | Pmovsxwq -> "pmovsxwq"
    | Pmovsxdq -> "pmovsxdq"
    | Pmovzxbw -> "pmovzxbw"
    | Pmovzxbd -> "pmovzxbd"
    | Pmovzxbq -> "pmovzxbq"
    | Pmovzxwd -> "pmovzxwd"
    | Pmovzxwq -> "pmovzxwq"
    | Pmovzxdq -> "pmovzxdq"
    | Movntdqa -> "movntdqa"
    | Blendvps -> "blendvps"
    | Blendvpd -> "blendvpd"
    | Pblendvb -> "pblendvb"
    | Blendps -> "blendps"
    | Blendpd -> "blendpd"
    | Dpps -> "dpps"
    | Dppd -> "dppd"
    | Mpsadbw -> "mpsadbw"
    | Pblendw -> "pblendw"
    | Insertps -> "insertps"
    | Pinsrb -> "pinsrb"
    | Pinsrd -> "pinsrd"
    | Pextrb -> "pextrb"
    | Pextrd -> "pextrd"
    | Extractps -> "extractps"
    | Vpshufb -> "vpshufb"
    | Vphaddw -> "vphaddw"
    | Vphaddd -> "vphaddd"
    | Vphaddsw -> "vphaddsw"
    | Vpmaddubsw -> "vpmaddubsw"
    | Vphsubw -> "vphsubw"
    | Vphsubd -> "vphsubd"
    | Vphsubsw -> "vphsubsw"
    | Vpsignb -> "vpsignb"
    | Vpsignw -> "vpsignw"
    | Vpsignd -> "vpsignd"
    | Vpmulhrsw -> "vpmulhrsw"
    | Vpmuldq -> "vpmuldq"
    | Vpcmpeqq -> "vpcmpeqq"
    | Vpackusdw -> "vpackusdw"
    | Vpcmpgtq -> "vpcmpgtq"
    | Vpminsb -> "vpminsb"
    | Vpminsd -> "vpminsd"
    | Vpminuw -> "vpminuw"
    | Vpminud -> "vpminud"
    | Vpmaxsb -> "vpmaxsb"
    | Vpmaxsd -> "vpmaxsd"
    | Vpmaxuw -> "vpmaxuw"
    | Vpmaxud -> "vpmaxud"
    | Vpmulld -> "vpmulld"
    | Vpalignr -> "vpalignr"
    | Vblendps -> "vblendps"
    | Vblendpd -> "vblendpd"
    | Vpblendw -> "vpblendw"
    | Vroundss -> "vroundss"
    | Vroundsd -> "vroundsd"
    | Vdpps -> "vdpps"
    | Vdppd -> "vdppd"
    | Vmpsadbw -> "vmpsadbw"
    | Vinsertps -> "vinsertps"
    | Vpabsb -> "vpabsb"
    | Vpabsw -> "vpabsw"
    | Vpabsd -> "vpabsd"
    | Vphminposuw -> "vphminposuw"
    | Vptest -> "vptest"
    | Vpmovsxbw -> "vpmovsxbw"
    | Vpmovsxbd -> "vpmovsxbd"
    | Vpmovsxbq -> "vpmovsxbq"
    | Vpmovsxwd -> "vpmovsxwd"
    | Vpmovsxwq -> "vpmovsxwq"
    | Vpmovsxdq -> "vpmovsxdq"
    | Vpmovzxbw -> "vpmovzxbw"
    | Vpmovzxbd -> "vpmovzxbd"
    | Vpmovzxbq -> "vpmovzxbq"
    | Vpmovzxwd -> "vpmovzxwd"
    | Vpmovzxwq -> "vpmovzxwq"
    | Vpmovzxdq -> "vpmovzxdq"
    | Vmovntdqa -> "vmovntdqa"
    | Movdqa -> "movdqa"
    | Movdqu -> "movdqu"
    | Pinsrw -> "pinsrw"
    | Pextrw -> "pextrw"
    | Movmskps -> "movmskps"
    | Movmskpd -> "movmskpd"
    | Pmovmskb -> "pmovmskb"
    | Andps -> "andps"
    | Andnps -> "andnps"
    | Orps -> "orps"
    | Xorps -> "xorps"
    | Andpd -> "andpd"
    | Andnpd -> "andnpd"
    | Orpd -> "orpd"
    | Movaps -> "movaps"
    | Movups -> "movups"
    | Movupd -> "movupd"
    | Addps -> "addps"
    | Subps -> "subps"
    | Mulps -> "mulps"
    | Divps -> "divps"
    | Addpd -> "addpd"
    | Subpd -> "subpd"
    | Mulpd -> "mulpd"
    | Divpd -> "divpd"
    | Maxss -> "maxss"
    | Minss -> "minss"
    | Maxsd -> "maxsd"
    | Minsd -> "minsd"
    | Maxps -> "maxps"
    | Minps -> "minps"
    | Maxpd -> "maxpd"
    | Minpd -> "minpd"
    | Sqrtss -> "sqrtss"
    | Sqrtsd -> "sqrtsd"
    | Sqrtps -> "sqrtps"
    | Sqrtpd -> "sqrtpd"
    | Shufps -> "shufps"
    | Shufpd -> "shufpd"
    | Cmpss -> "cmpss"
    | Cmpsd -> "cmpsd"
    | Cmpps -> "cmpps"
    | Cmppd -> "cmppd"
    | Pshufd -> "pshufd"
    | Pshuflw -> "pshuflw"
    | Pshufhw -> "pshufhw"
    | Movd -> "movd"
    | Vaddsd -> "vaddsd"
    | Vsubsd -> "vsubsd"
    | Vmulsd -> "vmulsd"
    | Vdivsd -> "vdivsd"
    | Vaddss -> "vaddss"
    | Vsubss -> "vsubss"
    | Vmulss -> "vmulss"
    | Vdivss -> "vdivss"
    | Vaddps -> "vaddps"
    | Vsubps -> "vsubps"
    | Vmulps -> "vmulps"
    | Vdivps -> "vdivps"
    | Vaddpd -> "vaddpd"
    | Vsubpd -> "vsubpd"
    | Vmulpd -> "vmulpd"
    | Vdivpd -> "vdivpd"
    | Vandps -> "vandps"
    | Vandnps -> "vandnps"
    | Vorps -> "vorps"
    | Vxorps -> "vxorps"
    | Vandpd -> "vandpd"
    | Vandnpd -> "vandnpd"
    | Vorpd -> "vorpd"
    | Vxorpd -> "vxorpd"
    | Vunpcklps -> "vunpcklps"
    | Vunpckhps -> "vunpckhps"
    | Vunpcklpd -> "vunpcklpd"
    | Vunpckhpd -> "vunpckhpd"
    | Vpunpcklqdq -> "vpunpcklqdq"
    | Vpunpckhqdq -> "vpunpckhqdq"
    | Vpunpcklbw -> "vpunpcklbw"
    | Vpunpckhbw -> "vpunpckhbw"
    | Vpunpcklwd -> "vpunpcklwd"
    | Vpunpckhwd -> "vpunpckhwd"
    | Vpunpckldq -> "vpunpckldq"
    | Vpunpckhdq -> "vpunpckhdq"
    | Vpaddb -> "vpaddb"
    | Vpaddw -> "vpaddw"
    | Vpaddd -> "vpaddd"
    | Vpaddq -> "vpaddq"
    | Vpsubb -> "vpsubb"
    | Vpsubw -> "vpsubw"
    | Vpsubd -> "vpsubd"
    | Vpsubq -> "vpsubq"
    | Vpcmpeqb -> "vpcmpeqb"
    | Vpcmpeqw -> "vpcmpeqw"
    | Vpcmpeqd -> "vpcmpeqd"
    | Vpcmpgtb -> "vpcmpgtb"
    | Vpcmpgtw -> "vpcmpgtw"
    | Vpcmpgtd -> "vpcmpgtd"
    | Vpacksswb -> "vpacksswb"
    | Vpackssdw -> "vpackssdw"
    | Vpackuswb -> "vpackuswb"
    | Vpand -> "vpand"
    | Vpandn -> "vpandn"
    | Vpor -> "vpor"
    | Vpminub -> "vpminub"
    | Vpmaxub -> "vpmaxub"
    | Vpminsw -> "vpminsw"
    | Vpmaxsw -> "vpmaxsw"
    | Vmaxsd -> "vmaxsd"
    | Vminsd -> "vminsd"
    | Vmaxss -> "vmaxss"
    | Vminss -> "vminss"
    | Vmaxps -> "vmaxps"
    | Vminps -> "vminps"
    | Vmaxpd -> "vmaxpd"
    | Vminpd -> "vminpd"
    | Vsqrtsd -> "vsqrtsd"
    | Vsqrtss -> "vsqrtss"
    | Vsqrtps -> "vsqrtps"
    | Vsqrtpd -> "vsqrtpd"
    | Vmovaps -> "vmovaps"
    | Vmovups -> "vmovups"
    | Vmovapd -> "vmovapd"
    | Vmovupd -> "vmovupd"
    | Vcomisd -> "vcomisd"
    | Vucomisd -> "vucomisd"
    | Vcomiss -> "vcomiss"
    | Vucomiss -> "vucomiss"
    | Vcvtps2pd -> "vcvtps2pd"
    | Vcvtpd2ps -> "vcvtpd2ps"
    | Vcvtdq2ps -> "vcvtdq2ps"
    | Vcvtps2dq -> "vcvtps2dq"
    | Vcvttps2dq -> "vcvttps2dq"
    | Vshufps -> "vshufps"
    | Vshufpd -> "vshufpd"
    | Vcmpss -> "vcmpss"
    | Vcmpsd -> "vcmpsd"
    | Vcmpps -> "vcmpps"
    | Vcmppd -> "vcmppd"
    | Vpshufd -> "vpshufd"
    | Vpshuflw -> "vpshuflw"
    | Vpshufhw -> "vpshufhw"
    | Vmovd -> "vmovd"
    | Vpmullw -> "vpmullw"
    | Vpmulhw -> "vpmulhw"
    | Vpmulhuw -> "vpmulhuw"
    | Vpavgb -> "vpavgb"
    | Vpavgw -> "vpavgw"
    | Vpsadbw -> "vpsadbw"
    | Vpsllw -> "vpsllw"
    | Vpslld -> "vpslld"
    | Vpsllq -> "vpsllq"
    | Vpsrlw -> "vpsrlw"
    | Vpsrld -> "vpsrld"
    | Vpsrlq -> "vpsrlq"
    | Vpsraw -> "vpsraw"
    | Vpsrad -> "vpsrad"
    | Vpslldq -> "vpslldq"
    | Vpsrldq -> "vpsrldq"
    | Vmovdqa -> "vmovdqa"
    | Vmovdqu -> "vmovdqu"
    | Vpinsrw -> "vpinsrw"
    | Vpextrw -> "vpextrw"
    | Vmovmskps -> "vmovmskps"
    | Vmovmskpd -> "vmovmskpd"
    | Vpmovmskb -> "vpmovmskb"
    | Fldl -> "fldl"
    | Fstpl -> "fstpl"
    | Fstps -> "fstps"
    | Flds -> "flds"
    | Fildll -> "fildll"
    | Fadds -> "fadds"
    | Fadd -> "fadd"
    | Fucomp -> "fucomp"
    | Fnstcw -> "fnstcw"
    | Fldcw -> "fldcw"
    | Fistpll -> "fistpll"
    | Fsubs -> "fsubs"
    | Fnstsw -> "fnstsw"
    | Sahf -> "sahf"
    | Table i -> X86_table_rows.rows.(i).X86_table_row.mnemonic

  (* The opcodes the {!X86_x87} component owns. Its tables key forms by mnemonic; this list is
     the one place a mnemonic is turned back into a constructor, and the family checks at
     instantiation that it and the component agree. *)
  let x87 =
    [ Fldl; Fstpl; Fstps; Flds; Fildll; Fadds; Fadd; Fucomp; Fnstcw; Fldcw; Fistpll; Fsubs; Fnstsw ]

  let of_x87_mnemonic m = List.find_opt (fun o -> String.equal (name o) m) x87

  (* The machine encodes add and sub as one opcode with the operation in the
     ModR/M reg field, so the opcode and its extension are two spellings of one
     fact and live together. [-1] is "not an ALU-immediate operation". [Adc]
     (M4, .ai/asm_plan.md §12: the CompCert-runtime-helper fixture) needs only
     this immediate form - [i64_sdiv.S]/[i64_smod.S] never add it to a memory
     destination. [Sbb] fills ext 3, the one gap in
     this table, confirmed against real GNU as: [sbbl $1000000,%ecx] -> [81
     d9 40 42 0f 00]. *)
  let to_ext = function
    | Add -> 0
    | Or -> 1
    | Adc -> 2
    | Sbb -> 3
    | And -> 4
    | Sub -> 5
    | Xor -> 6
    | Cmp -> 7
    | _ -> -1

  let of_ext = function
    | 0 -> Some Add
    | 1 -> Some Or
    | 2 -> Some Adc
    | 3 -> Some Sbb
    | 4 -> Some And
    | 5 -> Some Sub
    | 6 -> Some Xor
    | 7 -> Some Cmp
    | _ -> None

  (* The other ALU direction: one byte per operation, r/m written from reg.
     [Add]/[Sbb]/[Test] (M4) join [Xor]/[Cmp]/[Sub] for the same reason those
     three were added - real bytes the i64_divmod runtime-helper fixture
     measurably selects (0x01/0x19/0x85 respectively), not speculative
     coverage. [Adc] (M5, asm/docs/corpus.md - siphash24.c's [adcl %ecx,%edx],
     the carry-propagation half of its 64-bit-add idiom) joins them the same
     way, checked against real i686-linux-gnu-as: [adcl %ecx,%edx] -> [11
     ca]. *)
  let to_rm_r = function
    | Xor -> Some 0x31L
    | Cmp -> Some 0x39L
    | Sub -> Some 0x29L
    | Add -> Some 0x01L
    | Adc -> Some 0x11L
    | Sbb -> Some 0x19L
    | Test -> Some 0x85L
    | Or -> Some 0x09L
    | And -> Some 0x21L
    | _ -> None

  (* The reg<-rm ALU direction ([Alu_r_rm]): one byte per operation, the
     register field is the DESTINATION. M4's own [adcl 0x20(%esp),%edx] and
     [add 0x1c(%esp),%eax] were the first two measured uses; [Xor] joined
     them without its own row-specific comment. [Sub]/[And]/[Or]/[Sbb]/[Cmp]
     join the rest of {!to_rm_r}'s own opcode set
     into this direction too, confirmed against real GNU as: [subl
     16(%esp),%ecx] -> [2b 4c 24 10], [andl 16(%esp),%ecx] -> [23 4c 24 10],
     [orl 16(%esp),%ecx] -> [0b 4c 24 10], [sbbl 16(%esp),%ecx] -> [1b 4c 24
     10], [cmpl 16(%esp),%ecx] -> [3b 4c 24 10]. [Test] is not added here:
     real GNU as does accept [testl 16(%esp),%ecx] (encoding it with the
     same opcode [0x85] as the register-register form), but the checked-in
     XED export has no [TEST_GPRv_MEMv]-named record to admit, unlike the other five. *)
  let to_r_rm = function
    | Adc -> Some 0x13L
    | Add -> Some 0x03L
    | Xor -> Some 0x33L
    | Sub -> Some 0x2bL
    | And -> Some 0x23L
    | Or -> Some 0x0bL
    | Sbb -> Some 0x1bL
    | Cmp -> Some 0x3bL
    | _ -> None

  (* Group-3 unary forms (opcode 0xF7): the ModR/M reg field selects the
     operation, exactly [to_ext]'s idea but a different opcode and a disjoint
     extension namespace - group-3's /3 is NEG, not SBB. *)
  let to_unary_ext = function Neg -> 3 | Not -> 2 | Mul -> 4 | Div -> 6 | _ -> -1

  let of_unary_ext = function
    | 2 -> Some Not
    | 3 -> Some Neg
    | 4 -> Some Mul
    | 6 -> Some Div
    | _ -> None

  (* Group-2 shift/rotate forms, shared by three opcodes with the same
     ext-in-ModR/M-reg layout: [0xD1] (shift/rotate-by-exactly-1, no
     immediate byte), [0xC1 ib] (general immediate count, M5 corpus
     evidence - [rorl $27,%eax], [sarl $2,%eax], [shrq $63,%rax]) and [0xD3]
     (count in %cl - [sall %cl,%eax]). One ext table serves all three forms,
     since the reg-field encoding of the operation is identical across them;
     only the opcode byte and the trailing count representation differ. *)
  let to_shift1_ext = function Ror -> 1 | Rcr -> 3 | Shl -> 4 | Shr -> 5 | Sar -> 7 | _ -> -1

  let of_shift1_ext = function
    | 1 -> Some Ror
    | 3 -> Some Rcr
    | 4 -> Some Shl
    | 5 -> Some Shr
    | 7 -> Some Sar
    | _ -> None

  (* {!Xmm_shift_imm_rm}/{!Vex_shift_imm_rm}'s own "group" table: the ModR/M reg
     extension is the same across all three lane widths ([/6]=shift-left, [/2]=shift-right-
     logical, [/4]=shift-right-arithmetic - {!to_shift1_ext}'s exact idea again, just three
     opcode bytes rather than one), so [ext]/[opcode] come as a pair, not two independent
     lookups. There is no quadword arithmetic-right entry: {!Psrad}'s own doc comment already
     established that [psraq]/[vpsraq] do not exist at this (non-EVEX) encoding. *)
  let xmm_shift_ext_opcode = function
    | Psllw | Vpsllw -> Some (6, 0x71)
    | Pslld | Vpslld -> Some (6, 0x72)
    | Psllq | Vpsllq -> Some (6, 0x73)
    | Psrlw | Vpsrlw -> Some (2, 0x71)
    | Psrld | Vpsrld -> Some (2, 0x72)
    | Psrlq | Vpsrlq -> Some (2, 0x73)
    | Psraw | Vpsraw -> Some (4, 0x71)
    | Psrad | Vpsrad -> Some (4, 0x72)
    | Pslldq | Vpslldq -> Some (7, 0x73)
    | Psrldq | Vpsrldq -> Some (3, 0x73)
    | _ -> None

  let of_xmm_shift_ext_opcode ~opcode ext =
    match (opcode, ext) with
    | 0x71, 6 -> Some Psllw
    | 0x71, 2 -> Some Psrlw
    | 0x71, 4 -> Some Psraw
    | 0x72, 6 -> Some Pslld
    | 0x72, 2 -> Some Psrld
    | 0x72, 4 -> Some Psrad
    | 0x73, 6 -> Some Psllq
    | 0x73, 2 -> Some Psrlq
    | 0x73, 7 -> Some Pslldq
    | 0x73, 3 -> Some Psrldq
    | _ -> None

  let of_vex_shift_ext_opcode ~opcode ext =
    match (opcode, ext) with
    | 0x71, 6 -> Some Vpsllw
    | 0x71, 2 -> Some Vpsrlw
    | 0x71, 4 -> Some Vpsraw
    | 0x72, 6 -> Some Vpslld
    | 0x72, 2 -> Some Vpsrld
    | 0x72, 4 -> Some Vpsrad
    | 0x73, 6 -> Some Vpsllq
    | 0x73, 2 -> Some Vpsrlq
    | 0x73, 7 -> Some Vpslldq
    | 0x73, 3 -> Some Vpsrldq
    | _ -> None
end

let suffix_of_width = function 8 -> "b" | 16 -> "w" | 32 -> "l" | 64 -> "q" | _ -> "?"

module Instruction = struct
  (* The normalized instruction carries an explicit operand *width in bits*:
     the suffix that spelled it (`l`, `q`) is surface syntax, and by this stage
     the only thing that matters is 32 or 64. *)
  type t = { op : Opcode.t; width : int; ops : Operand.t list; form : string option }
  (** [form] is B10's form preference: which rung of a relaxation ladder this instruction insists
      on, or [None] for "layout decides".

      It is [None] from ordinary parsing, so CompCert's and a user's branches relax normally. It is
      [Some] from a decoded branch and from a size-suffixed mnemonic, and the reason it has to
      survive as far as here rather than being consumed at the text boundary is that canonical
      disassembly must reassemble byte-exactly: a near branch whose final displacement also fits
      [rel8] would otherwise come back short. Lowering copies it into the operand; [encode] is what
      acts on it. *)

  let mk ?form op width ops = { op; width; ops; form }

  (* GAS's own x86 pseudo-suffix, confirmed accepted by the installed binutils
     in both modes, so the form-forcing spelling is a real dialect feature
     rather than an invention. Ordinary source carries none and relaxes
     normally; only canonical output is pinned, which is exactly right, because
     its contract is byte-exact reproduction and not minimal spelling. *)
  let form_suffix i = match i.form with Some r -> "." ^ r | None -> ""

  (* A generated row that is not the first of its spelling and operand shape is reached only
     through a pseudo-prefix ([{evex} vaddps], [{load} addl]); printing it keeps the text
     re-assembling to the same bytes. *)
  let table_spelling i =
    let rows = X86_table_rows.rows in
    let r = rows.(i) in
    let shape (r : X86_table_row.row) =
      List.map
        (function
          | X86_table_row.Reg { cls; _ } -> `Reg cls
          | Mem _ -> `Mem
          | Imm { bytes } -> `Imm bytes
          | Fixed_reg n -> `Fixed n
          | Rounding _ -> `Rounding
          | One -> `One
          | Dfv -> `Dfv
          | Vsib _ -> `Vsib)
        r.operands
    in
    let dest (r : X86_table_row.row) =
      match List.rev r.operands with X86_table_row.Reg { field; _ } :: _ -> Some field | _ -> None
    in
    let rec first j =
      if j >= i then None
      else
        let q = rows.(j) in
        if String.equal q.mnemonic r.mnemonic && q.mode = r.mode && shape q = shape r then Some q
        else first (j + 1)
    in
    if r.pseudo <> "" then "{" ^ r.pseudo ^ "} " ^ r.mnemonic
    else
      match first 0 with
      | None -> r.mnemonic
      | Some q -> (
          if q.space <> r.space then
            match r.space with
            | X86_table_row.Evex -> "{evex} " ^ r.mnemonic
            | Vex -> "{vex} " ^ r.mnemonic
            | Legacy | Xop -> r.mnemonic
          else
            match (dest r, dest q) with
            | Some X86_table_row.Modrm_reg, Some X86_table_row.Modrm_rm -> "{load} " ^ r.mnemonic
            | Some X86_table_row.Modrm_rm, Some X86_table_row.Modrm_reg -> "{store} " ^ r.mnemonic
            | _ -> r.mnemonic)

  let pp ppf i =
    match i.ops with
    | [] -> Fmt.string ppf (Opcode.name i.op)
    | ops -> (
        match i.op with
        (* a generated row's spelling is whole: no width suffix to add *)
        | Opcode.Table row -> (
            match ops with
            (* {dfv=...} takes no comma after it *)
            | (Operand.Dfv _ as dfv) :: rest ->
                Fmt.pf ppf "%s %a %a" (table_spelling row) Operand.pp dfv
                  Fmt.(list ~sep:(any ", ") Operand.pp)
                  rest
            | _ -> Fmt.pf ppf "%s %a" (table_spelling row) Fmt.(list ~sep:(any ", ") Operand.pp) ops
            )
        (* [pop]/[jmp] take no AT&T size suffix in M1 - their one operand's own
           width is what disambiguates, and [simplify_instruction] only
           recognizes the bare mnemonic. [jmp]'s indirect-target sigil is
           equally part of this spelling: it is what [parse_one_operand]'s
           [Token.Star] case, and GNU as, both expect. *)
        | Opcode.Pop -> Fmt.pf ppf "pop %a" Fmt.(list ~sep:(any ", ") Operand.pp) ops
        | Opcode.Jmp -> (
            match ops with
            (* A relative jump and an indirect one share a mnemonic and differ in
               their operand, which is exactly how GAS tells them apart: the
               sigil marks the indirect one. *)
            | [ Operand.Sym _ ] ->
                Fmt.pf ppf "jmp%s %a" (form_suffix i) Fmt.(list ~sep:(any ", ") Operand.pp) ops
            | _ -> Fmt.pf ppf "jmp *%a" Fmt.(list ~sep:(any ", ") Operand.pp) ops)
        | Opcode.Jcc c ->
            Fmt.pf ppf "j%s%s %a" (Cc.name c) (form_suffix i)
              Fmt.(list ~sep:(any ", ") Operand.pp)
              ops
        (* No size suffix: a near call is rel32 in both modes, so there is
           nothing for one to select, and GNU as writes none either. *)
        | Opcode.Call -> Fmt.pf ppf "call %a" Fmt.(list ~sep:(any ", ") Operand.pp) ops
        (* Nor does a conditional move: both its operands are registers, so the
           width is on the line already, and GAS accepts no suffix on it. *)
        | Opcode.Cmov c ->
            Fmt.pf ppf "cmov%s %a" (Cc.name c) Fmt.(list ~sep:(any ", ") Operand.pp) ops
        (* No AT&T size suffix: [sete]/[setl] are always 8-bit, so there is no
           width for a suffix to disambiguate - GAS accepts none. *)
        | Opcode.Setcc c ->
            Fmt.pf ppf "set%s %a" (Cc.name c) Fmt.(list ~sep:(any ", ") Operand.pp) ops
        (* [movzbl]/[movsbl]/[movslq] (M5, asm/docs/corpus.md): [Opcode.name]
           already carries the *source* width as its own trailing letter
           ([movzb], [movsl], ...); appending [suffix_of_width i.width] here
           supplies the destination one, together reconstructing GAS's own
           two-suffix spelling. *)
        | (Opcode.Movzx _ | Opcode.Movsx _) as op ->
            Fmt.pf ppf "%s%s %a" (Opcode.name op) (suffix_of_width i.width)
              Fmt.(list ~sep:(any ", ") Operand.pp)
              ops
        (* M4 (.ai/asm_plan.md §12): [push]/[dec] are single-register-only here
           (like [pop]), so their own operand always disambiguates - measured
           against the real i64_divmod runtime-helper oracle, GNU's objdump
           prints both with no suffix. *)
        | Opcode.Push -> Fmt.pf ppf "push %a" Fmt.(list ~sep:(any ", ") Operand.pp) ops
        | Opcode.Dec -> Fmt.pf ppf "dec %a" Fmt.(list ~sep:(any ", ") Operand.pp) ops
        (* Unlike [push]/[dec], these can target memory with no register
           operand anywhere on the line ([negl 0x10(%esp)]), where nothing
           else carries a width - so the suffix is conditional on whether a
           register operand is present, not absent outright. Measured
           against the same oracle: any register operand present gets none
           (["neg %esi"], ["adc $0x0,%esi"], ["adc 0x20(%esp),%edx"], ["rcr
           $1,%ecx"]); an all-memory(+immediate) operand list keeps it
           (["negl 0x10(%esp)"]). Scoped to exactly the M4 opcodes added
           alongside this rule, plus M5's [Ror]/[Shl]/[Sar] (measured the
           same way against the same oracle: ["ror $0x1b,%eax"], ["shl
           $0x10,%eax"], ["sar $0x2,%eax"], but ["rorl $0x3,(%rax)"]) - and
           M5's [Shld] (asm/docs/corpus.md - CompCert's own [shldl
           $6,%ecx,%eax], measured against real objdump as suffixless
           ["shld $0x6,%ecx,%eax"], its two register operands disambiguating
           the same way) - existing opcodes below keep their already-
           measured, unconditional suffix. *)
        | ( Opcode.Neg | Opcode.Mul | Opcode.Div | Opcode.Test | Opcode.Adc | Opcode.Sbb
          | Opcode.Rcr | Opcode.Shr | Opcode.Ror | Opcode.Shl | Opcode.Sar | Opcode.Shld ) as op ->
            if List.exists (function Operand.Reg _ -> true | _ -> false) ops then
              Fmt.pf ppf "%s %a" (Opcode.name op) Fmt.(list ~sep:(any ", ") Operand.pp) ops
            else
              Fmt.pf ppf "%s%s %a" (Opcode.name op) (suffix_of_width i.width)
                Fmt.(list ~sep:(any ", ") Operand.pp)
                ops
        (* No AT&T size suffix on any of these, ever: they are fixed-name SSE
           mnemonics (M5, asm/docs/corpus.md), not a stem plus a width letter
           - GAS never writes [addsdl] or [movsdq]. [i.width] here is not the
           128-bit xmm operand's width in the first place (see
           {!instruction_of_lowered}'s comment) so it must never reach
           [suffix_of_width] the way the fallback case below does. *)
        | ( Opcode.Addsd | Opcode.Subsd | Opcode.Mulsd | Opcode.Divsd | Opcode.Addss | Opcode.Subss
          | Opcode.Mulss | Opcode.Divss | Opcode.Comisd | Opcode.Ucomisd | Opcode.Comiss
          | Opcode.Ucomiss | Opcode.Xorpd | Opcode.Pxor | Opcode.Movapd | Opcode.Cvtsd2ss
          | Opcode.Cvtss2sd | Opcode.Cvtps2pd | Opcode.Cvtpd2ps | Opcode.Cvtdq2ps | Opcode.Cvtps2dq
          | Opcode.Cvttps2dq | Opcode.Movsd | Opcode.Movss | Opcode.Cvtsi2sd | Opcode.Cvtsi2ss
          | Opcode.Cvttsd2si | Opcode.Unpcklps | Opcode.Unpckhps | Opcode.Unpcklpd | Opcode.Unpckhpd
          | Opcode.Punpcklqdq | Opcode.Punpckhqdq | Opcode.Punpcklbw | Opcode.Punpckhbw
          | Opcode.Punpcklwd | Opcode.Punpckhwd | Opcode.Punpckldq | Opcode.Punpckhdq | Opcode.Paddb
          | Opcode.Paddw | Opcode.Paddd | Opcode.Paddq | Opcode.Psubb | Opcode.Psubw | Opcode.Psubd
          | Opcode.Psubq | Opcode.Pcmpeqb | Opcode.Pcmpeqw | Opcode.Pcmpeqd | Opcode.Pcmpgtb
          | Opcode.Pcmpgtw | Opcode.Pcmpgtd | Opcode.Packsswb | Opcode.Packssdw | Opcode.Packuswb
          | Opcode.Pand | Opcode.Pandn | Opcode.Por | Opcode.Pminub | Opcode.Pmaxub | Opcode.Pminsw
          | Opcode.Pmaxsw | Opcode.Pmullw | Opcode.Pmulhw | Opcode.Pmulhuw | Opcode.Pavgb
          | Opcode.Pavgw | Opcode.Psadbw | Opcode.Andps | Opcode.Andnps | Opcode.Orps | Opcode.Xorps
          | Opcode.Andpd | Opcode.Andnpd | Opcode.Orpd | Opcode.Movaps | Opcode.Movups
          | Opcode.Movupd | Opcode.Addps | Opcode.Subps | Opcode.Mulps | Opcode.Divps | Opcode.Addpd
          | Opcode.Subpd | Opcode.Mulpd | Opcode.Divpd | Opcode.Maxss | Opcode.Minss | Opcode.Maxsd
          | Opcode.Minsd | Opcode.Maxps | Opcode.Minps | Opcode.Maxpd | Opcode.Minpd | Opcode.Sqrtss
          | Opcode.Sqrtsd | Opcode.Sqrtps | Opcode.Sqrtpd | Opcode.Vaddsd | Opcode.Vsubsd
          | Opcode.Vmulsd | Opcode.Vdivsd | Opcode.Vaddss | Opcode.Vsubss | Opcode.Vmulss
          | Opcode.Vdivss | Opcode.Vaddps | Opcode.Vsubps | Opcode.Vmulps | Opcode.Vdivps
          | Opcode.Vaddpd | Opcode.Vsubpd | Opcode.Vmulpd | Opcode.Vdivpd | Opcode.Vandps
          | Opcode.Vandnps | Opcode.Vorps | Opcode.Vxorps | Opcode.Vandpd | Opcode.Vandnpd
          | Opcode.Vorpd | Opcode.Vxorpd | Opcode.Vmaxsd | Opcode.Vminsd | Opcode.Vmaxss
          | Opcode.Vminss | Opcode.Vmaxps | Opcode.Vminps | Opcode.Vmaxpd | Opcode.Vminpd
          | Opcode.Vsqrtsd | Opcode.Vsqrtss | Opcode.Vsqrtps | Opcode.Vsqrtpd | Opcode.Vmovaps
          | Opcode.Vmovups | Opcode.Vmovapd | Opcode.Vmovupd | Opcode.Vcomisd | Opcode.Vucomisd
          | Opcode.Vcomiss | Opcode.Vucomiss | Opcode.Vcvtps2pd | Opcode.Vcvtpd2ps
          | Opcode.Vcvtdq2ps | Opcode.Vcvtps2dq | Opcode.Vcvttps2dq | Opcode.Vunpcklps
          | Opcode.Vunpckhps | Opcode.Vunpcklpd | Opcode.Vunpckhpd | Opcode.Vpunpcklqdq
          | Opcode.Vpunpckhqdq | Opcode.Vpunpcklbw | Opcode.Vpunpckhbw | Opcode.Vpunpcklwd
          | Opcode.Vpunpckhwd | Opcode.Vpunpckldq | Opcode.Vpunpckhdq | Opcode.Vpaddb
          | Opcode.Vpaddw | Opcode.Vpaddd | Opcode.Vpaddq | Opcode.Vpsubb | Opcode.Vpsubw
          | Opcode.Vpsubd | Opcode.Vpsubq | Opcode.Vpcmpeqb | Opcode.Vpcmpeqw | Opcode.Vpcmpeqd
          | Opcode.Vpcmpgtb | Opcode.Vpcmpgtw | Opcode.Vpcmpgtd | Opcode.Vpacksswb
          | Opcode.Vpackssdw | Opcode.Vpackuswb | Opcode.Vpand | Opcode.Vpandn | Opcode.Vpor
          | Opcode.Vpminub | Opcode.Vpmaxub | Opcode.Vpminsw | Opcode.Vpmaxsw | Opcode.Vmovd
          | Opcode.Vpmullw | Opcode.Vpmulhw | Opcode.Vpmulhuw | Opcode.Vpavgb | Opcode.Vpavgw
          | Opcode.Vpsadbw | Opcode.Psllw | Opcode.Pslld | Opcode.Psllq | Opcode.Psrlw
          | Opcode.Psrld | Opcode.Psrlq | Opcode.Psraw | Opcode.Psrad | Opcode.Vpsllw
          | Opcode.Vpslld | Opcode.Vpsllq | Opcode.Vpsrlw | Opcode.Vpsrld | Opcode.Vpsrlq
          | Opcode.Vpsraw | Opcode.Vpsrad | Opcode.Movdqa | Opcode.Movdqu | Opcode.Vmovdqa
          | Opcode.Vmovdqu | Opcode.Vpabsb | Opcode.Vpabsw | Opcode.Vpabsd | Opcode.Vphminposuw
          | Opcode.Vptest | Opcode.Vpmovsxbw | Opcode.Vpmovsxbd | Opcode.Vpmovsxbq
          | Opcode.Vpmovsxwd | Opcode.Vpmovsxwq | Opcode.Vpmovsxdq | Opcode.Vpmovzxbw
          | Opcode.Vpmovzxbd | Opcode.Vpmovzxbq | Opcode.Vpmovzxwd | Opcode.Vpmovzxwq
          | Opcode.Vpmovzxdq | Opcode.Vmovntdqa | Opcode.Vpalignr | Opcode.Vblendps
          | Opcode.Vblendpd | Opcode.Vpblendw | Opcode.Vroundss | Opcode.Vroundsd | Opcode.Vdpps
          | Opcode.Vdppd | Opcode.Vmpsadbw | Opcode.Vinsertps | Opcode.Vpshufb | Opcode.Vphaddw
          | Opcode.Vphaddd | Opcode.Vphaddsw | Opcode.Vpmaddubsw | Opcode.Vphsubw | Opcode.Vphsubd
          | Opcode.Vphsubsw | Opcode.Vpsignb | Opcode.Vpsignw | Opcode.Vpsignd | Opcode.Vpmulhrsw
          | Opcode.Vpmuldq | Opcode.Vpcmpeqq | Opcode.Vpackusdw | Opcode.Vpcmpgtq | Opcode.Vpminsb
          | Opcode.Vpminsd | Opcode.Vpminuw | Opcode.Vpminud | Opcode.Vpmaxsb | Opcode.Vpmaxsd
          | Opcode.Vpmaxuw | Opcode.Vpmaxud | Opcode.Vpmulld | Opcode.Fldl | Opcode.Fstpl
          | Opcode.Fstps | Opcode.Flds | Opcode.Fildll | Opcode.Fadds | Opcode.Fadd | Opcode.Fnstcw
          | Opcode.Fldcw | Opcode.Fistpll | Opcode.Fsubs | Opcode.Fnstsw | Opcode.Movd ) as op ->
            Fmt.pf ppf "%s %a" (Opcode.name op) Fmt.(list ~sep:(any ", ") Operand.pp) ops
        | _ ->
            Fmt.pf ppf "%s%s %a" (Opcode.name i.op) (suffix_of_width i.width)
              Fmt.(list ~sep:(any ", ") Operand.pp)
              ops)
end

module Rm = struct
  type t = Reg of Reg.t | Mem of Mem.t

  let equal a b =
    match (a, b) with Reg x, Reg y -> Reg.equal x y | Mem x, Mem y -> Mem.equal x y | _ -> false

  let pp ppf = function Reg r -> Reg.pp ppf r | Mem m -> Mem.pp ppf m
end

type rm = Rm.t

(* The lowered instruction names an *encoding shape*, not a mnemonic:
   [Lowered.Alu_rm_imm] with [ext = 5] is `sub` and with [ext = 0] is `add`,
   because the machine encodes both as opcode 0x83 with the operation in the
   ModR/M reg field. A lowered type that still said "sub" would have to say it
   twice - once as the constructor and once as the extension - and the two
   could disagree. *)
module Lowered = struct
  (** [0x9E] (M5, asm/docs/corpus.md - same fixture as {!Fnstsw}): load [%ah] into the
            flags register, bare, no operand, {!Fucomp}/{!Fnstsw}'s exact fixed-opcode shape. *)
  type t =
    | Alu_rm_imm of { ext : int; width : int; rm : Rm.t; imm : Disp.t }
        (** [imm] is a [Disp.t] rather than a bare [int64], for the same reason
            as {!Mov_r_imm}'s: gcc's `addq $bodies+24, %rax` (M5,
            asm/docs/corpus.md) writes a symbol's address as an ALU
            immediate. *)
    | Mov_r_imm of { width : int; reg : Reg.t; imm : Disp.t }
        (** [imm] is a [Disp.t] rather than a bare [int64] - unlike every other
            immediate-carrying form here - because this is the one place a
            gcc-only idiom (M5, asm/docs/corpus.md - [movl $.LC0,%edi]) writes
            a symbol's address as an immediate rather than through a memory
            operand; see {!sym_imm32}. *)
    | Mov_rm_r of { width : int; rm : Rm.t; reg : Reg.t }
    | Mov_r_rm of { width : int; reg : Reg.t; rm : Rm.t }
    | Lea of { width : int; reg : Reg.t; mem : Mem.t }
    | Ret
    | Mov_rm_imm of { width : int; rm : Rm.t; imm : int64 }
        (** imm-to-memory move; M1 only ever builds [width = 8]. *)
    | Alu_rm_r of { op : Opcode.t; width : int; rm : Rm.t; reg : Reg.t }
        (** the r/m-written direction of a two-operand ALU op: one opcode byte selects which. *)
    | Imul_r_rm of { width : int; reg : Reg.t; rm : Rm.t }
        (** [0f af /r], and the other direction: the register operand is the destination. *)
    | Imul_r_rm_imm of { width : int; reg : Reg.t; rm : Rm.t; imm : int64 }
        (** [0x69 /r id] / [0x6B /r ib] (M5, asm/docs/corpus.md - [imull $10000,%ebx], [imulq
            $56,%rax]): the two-operand AT&T form, where GAS writes the same register as both
            source and destination - [reg] and [rm] are therefore always equal at construction,
            never independently chosen the way the real three-operand instruction otherwise
            allows, since nothing here selects a genuine [imul $imm,src,dst]. *)
    | Test_rm_imm of { width : int; rm : Rm.t; imm : int64 }
        (** [0xF7 /0 id] (M5, asm/docs/corpus.md - [testl $1,%edi]): TEST's own immediate form,
            structurally like {!Unary_rm}'s opcode and ext-in-ModR/M-reg layout but with a
            trailing immediate {!Unary_rm}'s forms never carry - so it is its own constructor
            rather than a field added to that one. Always a full-width immediate: unlike
            {!Alu_rm_imm}, TEST has no imm8/imm32 dual encoding to choose between. *)
    | Cmov_r_rm of { cc : Cc.t; width : int; reg : Reg.t; rm : Rm.t }  (** [0f 4x /r] *)
    | Ud2
    | Pop of { reg : Reg.t }
    | Jmp_rm of { rm : Rm.t }
    | Jmp_rel of { target : Asm_core.Lowered_ast.branch }
    | Jcc_rel of { cc : Cc.t; target : Asm_core.Lowered_ast.branch }
        (** The two relaxing forms: [eb/e9] and [7x/0f 8x]. Unlike [call] these really do have two
            rungs, which is the whole reason {!Codec.Relax} exists. *)
    | Call_rel of { target : Asm_core.Lowered_ast.branch }
        (** [e8 rel32]. The target is [Symbolic] from lowering and [Resolved] only from decode; the
            encoder dispatches on which, so a resolved displacement never rebuilds a ladder and a
            symbolic one never pretends to have a value. *)
    | Push of { reg : Reg.t }  (** [0x50+r], mirroring {!Pop}'s [0x58+r]. *)
    | Push_imm of { imm : Disp.t }
        (** [0x6a ib] / [0x68 id] (M5, asm/docs/corpus.md - [pushl $sym]): push's own immediate
            form, disjoint from {!Push}'s register-only [0x50+r]. [imm] is a {!Disp.t} for the
            same reason {!Mov_r_imm}'s and {!Alu_rm_imm}'s are - GAS lets [$sym] stand in for a
            numeric literal here too - and like {!Alu_rm_imm} it sign-extends, confirmed by real
            [as] emitting [R_X86_64_32S] for the symbolic case the same way it does there. *)
    | Dec of { reg : Reg.t }
        (** [0x48+r] - the single-byte 32-bit-only form; the general [0xFF /1] group is
            unimplemented, no fixture selects it. *)
    | Unary_rm of { ext : int; width : int; rm : Rm.t }
        (** Group-3 (opcode [0xF7]): [Neg]/[Mul]/[Div], one r/m operand, no immediate - the same
            "opcode plus ModR/M-reg extension" idea as {!Alu_rm_imm}, a disjoint table. *)
    | Alu_r_rm of { op : Opcode.t; width : int; reg : Reg.t; rm : Rm.t }
        (** The reg<-rm ALU direction: {!Alu_rm_r}'s mirror image, needed only because [Adc]/[Add]
            (M4, .ai/asm_plan.md §12) are measured writing the register operand rather than the
            r/m one - [adcl 0x20(%esp),%edx]. *)
    | Shift1_rm of { ext : int; width : int; rm : Rm.t }
        (** Group-2 shift/rotate-by-1 (opcode [0xD1]): a literal count of 1, always chosen over
            {!Shift_imm_rm} when the count is exactly 1 (GAS's own canonical choice - one byte
            shorter). *)
    | Shift_imm_rm of { ext : int; width : int; rm : Rm.t; imm : int64 }
        (** Group-2 general immediate count (opcode [0xC1 ib], M5 corpus evidence -
            [rorl $27,%eax], [sall $16,%eax], [sarl $2,%eax], [shrq $63,%rax]). Register
            destination only - unlike {!Alu_rm_imm}, no fixture here selects a memory destination,
            so it is not built (the same "measured forms only" scoping as everywhere else in this
            module). *)
    | Shift_cl_rm of { ext : int; width : int; rm : Rm.t }
        (** Group-2 count-in-%cl (opcode [0xD3], M5 corpus evidence - [sall %cl,%eax]). Register
            destination only, for the same reason as {!Shift_imm_rm}. *)
    | Shld_imm_rm of { width : int; reg : Reg.t; rm : Rm.t; imm : int64 }
        (** [0F A4 /r ib] (M5, asm/docs/corpus.md - [shldl $6,%ecx,%eax]): SHLD's own
            immediate-count double-precision shift, structurally {!imul_imm_form}'s two-byte-
            opcode-plus-trailing-imm shape rather than {!Shift_imm_rm}'s: the ModR/M reg field is
            a genuine register operand here (the bit-supplying source), not an extension code, so
            [reg] and [rm] mirror {!Alu_r_rm}'s pair rather than {!Shift_imm_rm}'s [ext]. Register
            destination only - no fixture in this corpus selects a memory destination. *)
    | Sse_binop_r_rm of { op : Opcode.t; reg : Reg.t; rm : Rm.t }
        (** [\[66/F2/F3/none\] 0F opcode /r], reg<-rm (M5 corpus evidence, asm/docs/corpus.md):
            the SSE2 scalar-float arithmetic/compare/move family - [addsd subsd mulsd divsd addss
            subss mulss divss comisd comiss xorpd movapd cvtsd2ss cvtss2sd]. [reg] and a register
            [rm] are always xmm (width 128); a memory [rm] is unconstrained, as for any other ALU
            form. One constructor covers all fourteen mnemonics, encoded as four [C.alt]
            alternatives grouped by mandatory prefix - not fourteen, and not one, since the
            mandatory-prefix byte is fixed per alternative rather than a table value (see the
            encoder's own comment on why one alt per mnemonic-family, not one per mnemonic, was
            rejected). *)
    | Sse_mov_r_rm of { op : Opcode.t; reg : Reg.t; rm : Rm.t }
        (** [F2/F3 0F 10 /r], the load direction of [movsd]/[movss]: xmm<-(xmm or mem). Split from
            {!Sse_binop_r_rm} because these two mnemonics are the only ones here with a real second
            (store) direction. *)
    | Sse_mov_rm_r of { op : Opcode.t; rm : Rm.t; reg : Reg.t }
        (** [F2/F3 0F 11 /r], the store direction: (xmm or mem)<-xmm. *)
    | Sse_binop_imm_r_rm of { op : Opcode.t; reg : Reg.t; rm : Rm.t; imm : int64 }
        (** [\[66/none\] 0F C6 /r ib] - {!Sse_binop_r_rm}'s trailing-immediate sibling
            ([shufps]/[shufpd] only): the first XMM-immediate-carrying legacy shape, a plain
            [Disp.t]-free [int64] since a shuffle selector is never a symbol the way
            {!Alu_rm_imm}'s immediate can be. *)
    | Cvtsi2f_r_rm of { op : Opcode.t; width : int; reg : Reg.t; rm : Rm.t }
        (** [F2/F3 0F 2A /r], xmm<-(r/m32 or r/m64) - [cvtsi2sd]/[cvtsi2ss]. [reg] is xmm; [rm] is
            a GPR (or memory) at [width], which is also what selects REX.W - CompCert always
            spells the 64-bit source explicitly ([cvtsi2sdq]), so [width] comes from the mnemonic,
            not inferred from an operand. *)
    | Cvtf2i_r_rm of { width : int; reg : Reg.t; rm : Rm.t }
        (** [F2 0F 2C /r], (r32 or r64)<-(xmm or m64) - [cvttsd2si] only ([cvttss2si] is unevidenced
            by this corpus). [reg] is a GPR at [width]; [rm] is xmm (or memory). *)
    | Movd_rm_r of { op : Opcode.t; width : int; rm : Rm.t; reg : Reg.t }
        (** [66 0F 7E /r] - {!Cvtsi2f_r_rm}'s store-direction sibling for [movd]/[movq]:
            (gpr-or-mem)<-xmm. Byte-for-byte the same ModR/M-reg=xmm/ModR/M-rm=gpr-or-mem field
            layout and REX.W-selecting [width] as {!Cvtsi2f_r_rm} (confirmed against real GNU as:
            [movd %xmm0,%eax] -> [66 0f 7e c0], identical ModR/M byte to [movd %eax,%xmm0]'s own
            [66 0f 6e c0], only the opcode byte differs) - but [reg] (xmm) is the source here
            rather than the destination, so this is printed reg-first ([Sse_mov_rm_r]'s own AT&T
            order) rather than {!Cvtsi2f_r_rm}'s rm-first order, the reason this is a distinct
            constructor rather than a reuse. *)
    | Vex_binop_rr_rm of { op : Opcode.t; dst : Reg.t; src1 : Reg.t; src2 : Rm.t }
        (** [VEX.LIG.F2.0F.WIG opcode /r] - {!Opcode.Vaddsd}'s non-destructive three-operand
            shape: [dst := src1 op src2]. [src2] is the ModR/M r/m field, register or memory;
            a register [src2] is restricted to xmm0-7 and a memory [src2]'s base/index (if
            present) to the low 8 GPRs - the two-byte-VEX prefix carries only a [vvvv] field
            and one extension bit (for [dst]'s ModR/M reg field, in the [R] bit), with no
            REX.X/B equivalent to extend a ModR/M r/m register or a SIB base/index past 7
            ({!Opcode.Vaddsd}'s own comment has the byte-level detail). [src1] is the VEX
            prefix's own [vvvv] field, which reaches all of xmm0-15 directly with no separate
            extension bit; [dst] is the ModR/M reg field, extended by the VEX prefix's own R
            bit exactly as REX.R would. *)
    | Vex_unop_r_rm of { op : Opcode.t; dst : Reg.t; src : Rm.t }
        (** [VEX.128.pp.0F.WIG opcode /r] - {!Vex_binop_rr_rm}'s two-operand sibling for a VEX
            mnemonic with no real [vvvv] operand ({!Opcode.Vsqrtps}'s own comment): [dst :=
            f(src)]. The VEX prefix's [vvvv] field is architecturally unused (must be [1111])
            for these forms - confirmed against real GNU as, which rejects a third operand
            outright - so unlike {!Vex_binop_rr_rm} this shape has no [src1] field at all, and
            the codec always emits the literal [1111] bit pattern rather than deriving it from
            an operand. [src] is the ModR/M r/m field, subject to the same xmm0-7/low-8-GPR
            two-byte-VEX restriction as {!Vex_binop_rr_rm}'s [src2]. *)
    | Vex_binop_imm_rr_rm of { op : Opcode.t; dst : Reg.t; src1 : Reg.t; src2 : Rm.t; imm : int64 }
        (** [VEX.128.pp.0F.WIG opcode /r ib] - {!Vex_binop_rr_rm}'s trailing-immediate sibling
            ([vshufps]/[vshufpd] only): identical operand roles and two-byte-VEX restrictions,
            plus a genuine imm8 selector {!Vex_binop_rr_rm} has no field for. *)
    | Vex_unop_imm_r_rm of { op : Opcode.t; dst : Reg.t; src : Rm.t; imm : int64 }
        (** [VEX.128.pp.0F.WIG opcode /r ib] - {!Vex_unop_r_rm}'s trailing-immediate sibling
            ([vpshufd]/[vpshuflw]/[vpshufhw] only): identical two-operand shape and two-byte-VEX
            restriction, plus a genuine imm8 selector {!Vex_unop_r_rm} has no field for - unlike
            {!Vex_binop_imm_rr_rm} there is no [src1]/[vvvv] operand at all, architecturally
            unused (the literal [1111] pattern) the same way {!Vex_unop_r_rm}'s own [vvvv] is. *)
    | Vex_movd_r_rm of { op : Opcode.t; dst : Reg.t; rm : Rm.t }
        (** [VEX.128.66.0F.W0 6E /r] - {!Opcode.Vmovd}'s load direction, (gpr-or-mem)->xmm: the
            VEX, GPR-crossing sibling of {!Vex_unop_r_rm}, built because {!Vex_unop_r_rm}'s own
            [dst] is always constructed at width 128 (xmm), which does not fit [movd]'s GPR
            destination-or-source pairing - mirrors {!Cvtsi2f_r_rm}'s load-direction role instead,
            minus its [width] field, since {!Opcode.Vmovd}'s own comment explains why no REX.W-
            equivalent variant exists in the two-byte VEX prefix at all (always GPR32). [dst] is
            the ModR/M reg field (VEX.R-extended, reaching xmm0-15); [rm] is the ModR/M r/m field,
            register or memory, subject to the same xmm0-7-or-low-8-GPR/low-8-base-index
            two-byte-VEX restriction as {!Vex_unop_r_rm}'s own [src]. *)
    | Vex_movd_rm_r of { op : Opcode.t; rm : Rm.t; reg : Reg.t }
        (** [VEX.128.66.0F.W0 7E /r] - {!Vex_movd_r_rm}'s store-direction sibling: (gpr-or-mem)<-
            xmm. Byte-for-byte the same field layout, only the opcode byte differs - [reg] (xmm)
            is the source here rather than the destination, so this is printed reg-first
            ({!Movd_rm_r}'s own AT&T order) rather than {!Vex_movd_r_rm}'s rm-first order, the
            reason this is a distinct constructor rather than a reuse, exactly as {!Movd_rm_r}
            is distinct from {!Cvtsi2f_r_rm}. *)
    | Xmm_shift_imm_rm of { op : Opcode.t; rm : Rm.t; imm : int64 }
        (** [66 0F 71/72/73 /ext ib] ({!Opcode.Psllw}'s own doc comment): the
            immediate-count sibling of {!Sse_binop_r_rm}'s register/memory-count shift family
            ([psllw]/[pslld]/[psllq]/[psrlw]/[psrld]/[psrlq]/[psraw]/[psrad]). The ModR/M reg
            field is a fixed per-mnemonic opcode extension, the same "group" idea as
            {!Shift_imm_rm}'s own [ext] - but unlike {!Shift_imm_rm}, [rm] is register-only:
            confirmed against real GNU as, this iform has no memory alternative at all (XED's own
            pattern fixes MOD=3). *)
    | Vex_shift_imm_rm of { op : Opcode.t; dst : Reg.t; rm : Rm.t; imm : int64 }
        (** [VEX.128.66.0F.WIG 71/72/73 /ext ib] - {!Xmm_shift_imm_rm}'s VEX sibling
            ([vpsllw]/etc.): [dst] is the VEX prefix's own [vvvv] field (write), [rm] the ModR/M
            r/m field (read, register-only for the same reason as {!Xmm_shift_imm_rm}'s own
            [rm]) - unlike every other VEX immediate-carrying shape here, the ModR/M reg field
            carries the same fixed opcode extension as the legacy form rather than a third
            register operand, so there is no genuine third-register role despite [vvvv] being
            real (unlike {!Vex_unop_imm_r_rm}'s architecturally-unused [vvvv]). *)
    | Setcc_rm of { cc : Cc.t; rm : Rm.t }
        (** [0F 90+cc /0] (M5 corpus evidence - [sete %al], [setl %r8b]). Always 8-bit; the ModR/M
            reg field is a fixed 0, not an operand or an extension table lookup - the condition is
            already fully carried by the opcode byte, the same way {!Cmov_r_rm}'s is. *)
    | Movx_r_rm of { zero_extend : bool; src_width : int; width : int; reg : Reg.t; rm : Rm.t }
        (** [0F B6/B7/BE/BF /r] (M5 corpus evidence - [movzbl], [movsbl]): zero- or sign-extending
            move, [src_width] (8 or 16, the ModR/M-side width) always less than [width] (the
            ModR/M-reg-side, destination width - REX.W's source, exactly as {!Cvtsi2f_r_rm}'s own
            comment explains for a different width-mismatched pair). The 32-bit-source sign-extend
            ([movslq], opcode [0x63]) is a structurally different encoding and is
            {!Movsxd_r_rm} instead, not a third [src_width] here. *)
    | Movsxd_r_rm of { reg : Reg.t; rm : Rm.t }
        (** [0x63 /r] (M5 corpus evidence - [movslq]), REX.W mandatory: sign-extend r/m32 to r64.
            Not a [Movx_r_rm] with [src_width = 32] - unlike the B6/B7/BE/BF family this is a
            single fixed opcode byte with no zero-extending counterpart ([movzlq] is not a real
            instruction; a 32-bit write already zero-extends). *)
    | Fpu_mem of { op : Opcode.t; mem : Mem.t }
        (** [0xD9|0xDD /ext] (M5 corpus evidence - [fldl]/[fstpl]/[fstps]): the x87 load/store-
            and-pop family, evidenced only against a memory operand - the mod=11 ModR/M shape
            this opcode family also permits means an [%st(n)] register operand instead, which no
            fixture here selects, so [mem] is a bare {!Mem.t} rather than the general {!Rm.t}
            every GPR/xmm form above uses. [op] picks the opcode byte and ModR/M-reg extension
            the same way {!Sse_binop_r_rm}'s does. *)
    | Fadd_st0_x87 of { src : Reg.t }
        (** [0xD8 0xC0+i], with an implicit ST0 destination and explicit ST(i)
            source.  Separate from {!Fpu_mem}: MOD=11 means a stack register,
            not a memory address. *)
    | Fucomp
        (** [0xDD 0xE9] (M5, asm/docs/corpus.md - i64_dtou.S's own bare [fucomp]): x87
            compare-and-pop against the fixed stack slot [%st(1)] - GAS's bare, no-operand
            spelling of [fucomp %st(1)], the only form this corpus evidences, so unlike
            {!Fpu_mem} this carries no operand at all rather than a general [%st(n)]. *)
    | Fnstsw
        (** [0xDF 0xE0] (M5, asm/docs/corpus.md - i64_dtou.S's own [fnstsw %ax]): x87
            store-status-word, {!Fucomp}'s exact "fixed two-byte word, no operand" shape - the
            [%ax] destination is implicit in the opcode and checked away in
            {!simplify_instruction} rather than carried here. *)
    | Sahf
    | Table of { row : int; ops : Operand.t list }
        (** a generated {!X86_table_rows} row (DEC-X86-TABLE); [row] names the mnemonic, and
            encoding tries every row spelled the same way *)

  let pp ppf = function
    | Alu_rm_imm { ext; width; rm; imm } ->
        Fmt.pf ppf "%s%s $%s, %a"
          (match Opcode.of_ext ext with Some o -> Opcode.name o | None -> "alu?")
          (suffix_of_width width) (Disp.to_string imm) Rm.pp rm
    | Mov_r_imm { width; reg; imm } ->
        Fmt.pf ppf "mov%s $%s, %a" (suffix_of_width width) (Disp.to_string imm) Reg.pp reg
    | Mov_rm_r { width; rm; reg } ->
        Fmt.pf ppf "mov%s %a, %a" (suffix_of_width width) Reg.pp reg Rm.pp rm
    | Mov_r_rm { width; reg; rm } ->
        Fmt.pf ppf "mov%s %a, %a" (suffix_of_width width) Rm.pp rm Reg.pp reg
    | Lea { width; reg; mem } ->
        Fmt.pf ppf "lea%s %a, %a" (suffix_of_width width) Mem.pp mem Reg.pp reg
    | Ret -> Fmt.string ppf "ret"
    | Mov_rm_imm { width; rm; imm } ->
        Fmt.pf ppf "mov%s $%Ld, %a" (suffix_of_width width) imm Rm.pp rm
    | Alu_rm_r { op; width; rm; reg } ->
        Fmt.pf ppf "%s%s %a, %a" (Opcode.name op) (suffix_of_width width) Reg.pp reg Rm.pp rm
    | Imul_r_rm { width; reg; rm } ->
        Fmt.pf ppf "imul%s %a, %a" (suffix_of_width width) Rm.pp rm Reg.pp reg
    | Imul_r_rm_imm { width; imm; rm; _ } ->
        Fmt.pf ppf "imul%s $%Ld, %a" (suffix_of_width width) imm Rm.pp rm
    | Test_rm_imm { width; rm; imm } ->
        Fmt.pf ppf "test%s $%Ld, %a" (suffix_of_width width) imm Rm.pp rm
    (* No size suffix: both operands are registers, so the width is already on
       the line, and GAS writes none either. *)
    | Cmov_r_rm { cc; reg; rm; _ } -> Fmt.pf ppf "cmov%s %a, %a" (Cc.name cc) Rm.pp rm Reg.pp reg
    | Ud2 -> Fmt.string ppf "ud2"
    | Pop { reg } -> Fmt.pf ppf "pop %a" Reg.pp reg
    | Jmp_rm { rm } -> Fmt.pf ppf "jmp *%a" Rm.pp rm
    | Jmp_rel { target } -> Fmt.pf ppf "jmp %a" Asm_core.Lowered_ast.pp_branch target
    | Jcc_rel { cc; target } ->
        Fmt.pf ppf "j%s %a" (Cc.name cc) Asm_core.Lowered_ast.pp_branch target
    | Call_rel { target } -> Fmt.pf ppf "call %a" Asm_core.Lowered_ast.pp_branch target
    | Push { reg } -> Fmt.pf ppf "push %a" Reg.pp reg
    | Push_imm { imm } -> Fmt.pf ppf "push $%s" (Disp.to_string imm)
    | Dec { reg } -> Fmt.pf ppf "dec %a" Reg.pp reg
    | Unary_rm { ext; width; rm } -> (
        let name =
          match Opcode.of_unary_ext ext with Some o -> Opcode.name o | None -> "unary?"
        in
        match rm with
        | Rm.Reg _ -> Fmt.pf ppf "%s %a" name Rm.pp rm
        | Rm.Mem _ -> Fmt.pf ppf "%s%s %a" name (suffix_of_width width) Rm.pp rm)
    | Alu_r_rm { op; reg; rm; _ } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Rm.pp rm Reg.pp reg
    | Shift1_rm { ext; width; rm } -> (
        let name =
          match Opcode.of_shift1_ext ext with Some o -> Opcode.name o | None -> "shift1?"
        in
        match rm with
        | Rm.Reg _ -> Fmt.pf ppf "%s $1, %a" name Rm.pp rm
        | Rm.Mem _ -> Fmt.pf ppf "%s%s $1, %a" name (suffix_of_width width) Rm.pp rm)
    | Shift_imm_rm { ext; imm; rm; _ } ->
        let name =
          match Opcode.of_shift1_ext ext with Some o -> Opcode.name o | None -> "shift?"
        in
        Fmt.pf ppf "%s $%Ld, %a" name imm Rm.pp rm
    | Shift_cl_rm { ext; rm; _ } ->
        let name =
          match Opcode.of_shift1_ext ext with Some o -> Opcode.name o | None -> "shift?"
        in
        Fmt.pf ppf "%s %%cl, %a" name Rm.pp rm
    | Shld_imm_rm { imm; reg; rm; _ } -> Fmt.pf ppf "shld $%Ld, %a, %a" imm Reg.pp reg Rm.pp rm
    | Setcc_rm { cc; rm } -> Fmt.pf ppf "set%s %a" (Cc.name cc) Rm.pp rm
    | Movx_r_rm { zero_extend; reg; rm; _ } ->
        Fmt.pf ppf "%s %a, %a" (if zero_extend then "movzx" else "movsx") Rm.pp rm Reg.pp reg
    | Movsxd_r_rm { reg; rm } -> Fmt.pf ppf "movslq %a, %a" Rm.pp rm Reg.pp reg
    | Sse_binop_r_rm { op; reg; rm } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Rm.pp rm Reg.pp reg
    | Sse_mov_r_rm { op; reg; rm } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Rm.pp rm Reg.pp reg
    | Sse_mov_rm_r { op; rm; reg } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Reg.pp reg Rm.pp rm
    | Sse_binop_imm_r_rm { op; reg; rm; imm } ->
        Fmt.pf ppf "%s $%Ld, %a, %a" (Opcode.name op) imm Rm.pp rm Reg.pp reg
    | Cvtsi2f_r_rm { op; reg; rm; _ } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Rm.pp rm Reg.pp reg
    | Cvtf2i_r_rm { reg; rm; _ } -> Fmt.pf ppf "cvttsd2si %a, %a" Rm.pp rm Reg.pp reg
    | Movd_rm_r { op; reg; rm; _ } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Reg.pp reg Rm.pp rm
    | Vex_binop_rr_rm { op; dst; src1; src2 } ->
        Fmt.pf ppf "%s %a, %a, %a" (Opcode.name op) Rm.pp src2 Reg.pp src1 Reg.pp dst
    | Vex_unop_r_rm { op; dst; src } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Rm.pp src Reg.pp dst
    | Vex_binop_imm_rr_rm { op; dst; src1; src2; imm } ->
        Fmt.pf ppf "%s $%Ld, %a, %a, %a" (Opcode.name op) imm Rm.pp src2 Reg.pp src1 Reg.pp dst
    | Vex_unop_imm_r_rm { op; dst; src; imm } ->
        Fmt.pf ppf "%s $%Ld, %a, %a" (Opcode.name op) imm Rm.pp src Reg.pp dst
    | Vex_movd_r_rm { op; dst; rm } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Rm.pp rm Reg.pp dst
    | Vex_movd_rm_r { op; rm; reg } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Reg.pp reg Rm.pp rm
    | Xmm_shift_imm_rm { op; rm; imm } -> Fmt.pf ppf "%s $%Ld, %a" (Opcode.name op) imm Rm.pp rm
    | Vex_shift_imm_rm { op; dst; rm; imm } ->
        Fmt.pf ppf "%s $%Ld, %a, %a" (Opcode.name op) imm Rm.pp rm Reg.pp dst
    | Fpu_mem { op; mem } -> Fmt.pf ppf "%s %a" (Opcode.name op) Mem.pp mem
    | Fadd_st0_x87 { src } -> Fmt.pf ppf "fadd %a, %%st" Reg.pp src
    | Fucomp -> Fmt.string ppf "fucomp %st(1)"
    | Fnstsw -> Fmt.string ppf "fnstsw %ax"
    | Sahf -> Fmt.string ppf "sahf"
    | Table x -> Fmt.string ppf X86_table_rows.rows.(x.row).X86_table_row.mnemonic

  let equal a b =
    match (a, b) with
    | Alu_rm_imm x, Alu_rm_imm y ->
        x.ext = y.ext && x.width = y.width && Rm.equal x.rm y.rm && Disp.equal x.imm y.imm
    | Mov_r_imm x, Mov_r_imm y ->
        x.width = y.width && Reg.equal x.reg y.reg && Disp.equal x.imm y.imm
    | Mov_rm_r x, Mov_rm_r y -> x.width = y.width && Rm.equal x.rm y.rm && Reg.equal x.reg y.reg
    | Mov_r_rm x, Mov_r_rm y -> x.width = y.width && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Lea x, Lea y -> x.width = y.width && Reg.equal x.reg y.reg && Mem.equal x.mem y.mem
    | Ret, Ret -> true
    | Mov_rm_imm x, Mov_rm_imm y ->
        x.width = y.width && Rm.equal x.rm y.rm && Int64.equal x.imm y.imm
    | Alu_rm_r x, Alu_rm_r y ->
        x.op = y.op && x.width = y.width && Rm.equal x.rm y.rm && Reg.equal x.reg y.reg
    | Imul_r_rm x, Imul_r_rm y -> x.width = y.width && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Imul_r_rm_imm x, Imul_r_rm_imm y ->
        x.width = y.width && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm && Int64.equal x.imm y.imm
    | Test_rm_imm x, Test_rm_imm y ->
        x.width = y.width && Rm.equal x.rm y.rm && Int64.equal x.imm y.imm
    | Cmov_r_rm x, Cmov_r_rm y ->
        Cc.equal x.cc y.cc && x.width = y.width && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Ud2, Ud2 -> true
    | Pop x, Pop y -> Reg.equal x.reg y.reg
    | Jmp_rm x, Jmp_rm y -> Rm.equal x.rm y.rm
    | Jmp_rel x, Jmp_rel y -> Asm_core.Lowered_ast.equal_branch x.target y.target
    | Jcc_rel x, Jcc_rel y ->
        Cc.equal x.cc y.cc && Asm_core.Lowered_ast.equal_branch x.target y.target
    | Call_rel x, Call_rel y -> Asm_core.Lowered_ast.equal_branch x.target y.target
    | Push x, Push y -> Reg.equal x.reg y.reg
    | Push_imm x, Push_imm y -> Disp.equal x.imm y.imm
    | Dec x, Dec y -> Reg.equal x.reg y.reg
    | Unary_rm x, Unary_rm y -> x.ext = y.ext && x.width = y.width && Rm.equal x.rm y.rm
    | Alu_r_rm x, Alu_r_rm y ->
        x.op = y.op && x.width = y.width && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Shift1_rm x, Shift1_rm y -> x.ext = y.ext && x.width = y.width && Rm.equal x.rm y.rm
    | Shift_imm_rm x, Shift_imm_rm y ->
        x.ext = y.ext && x.width = y.width && Rm.equal x.rm y.rm && Int64.equal x.imm y.imm
    | Shift_cl_rm x, Shift_cl_rm y -> x.ext = y.ext && x.width = y.width && Rm.equal x.rm y.rm
    | Shld_imm_rm x, Shld_imm_rm y ->
        x.width = y.width && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm && Int64.equal x.imm y.imm
    | Setcc_rm x, Setcc_rm y -> Cc.equal x.cc y.cc && Rm.equal x.rm y.rm
    | Movx_r_rm x, Movx_r_rm y ->
        x.zero_extend = y.zero_extend && x.src_width = y.src_width && x.width = y.width
        && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Movsxd_r_rm x, Movsxd_r_rm y -> Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Sse_binop_r_rm x, Sse_binop_r_rm y ->
        x.op = y.op && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Sse_mov_r_rm x, Sse_mov_r_rm y -> x.op = y.op && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Sse_mov_rm_r x, Sse_mov_rm_r y -> x.op = y.op && Rm.equal x.rm y.rm && Reg.equal x.reg y.reg
    | Sse_binop_imm_r_rm x, Sse_binop_imm_r_rm y ->
        x.op = y.op && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm && x.imm = y.imm
    | Cvtsi2f_r_rm x, Cvtsi2f_r_rm y ->
        x.op = y.op && x.width = y.width && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Cvtf2i_r_rm x, Cvtf2i_r_rm y ->
        x.width = y.width && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Movd_rm_r x, Movd_rm_r y ->
        x.op = y.op && x.width = y.width && Rm.equal x.rm y.rm && Reg.equal x.reg y.reg
    | Vex_binop_rr_rm x, Vex_binop_rr_rm y ->
        x.op = y.op && Reg.equal x.dst y.dst && Reg.equal x.src1 y.src1 && Rm.equal x.src2 y.src2
    | Vex_unop_r_rm x, Vex_unop_r_rm y ->
        x.op = y.op && Reg.equal x.dst y.dst && Rm.equal x.src y.src
    | Vex_binop_imm_rr_rm x, Vex_binop_imm_rr_rm y ->
        x.op = y.op && Reg.equal x.dst y.dst && Reg.equal x.src1 y.src1 && Rm.equal x.src2 y.src2
        && x.imm = y.imm
    | Vex_unop_imm_r_rm x, Vex_unop_imm_r_rm y ->
        x.op = y.op && Reg.equal x.dst y.dst && Rm.equal x.src y.src && x.imm = y.imm
    | Vex_movd_r_rm x, Vex_movd_r_rm y -> x.op = y.op && Reg.equal x.dst y.dst && Rm.equal x.rm y.rm
    | Vex_movd_rm_r x, Vex_movd_rm_r y -> x.op = y.op && Rm.equal x.rm y.rm && Reg.equal x.reg y.reg
    | Xmm_shift_imm_rm x, Xmm_shift_imm_rm y ->
        x.op = y.op && Rm.equal x.rm y.rm && Int64.equal x.imm y.imm
    | Vex_shift_imm_rm x, Vex_shift_imm_rm y ->
        x.op = y.op && Reg.equal x.dst y.dst && Rm.equal x.rm y.rm && Int64.equal x.imm y.imm
    | Fpu_mem x, Fpu_mem y -> x.op = y.op && Mem.equal x.mem y.mem
    | Fadd_st0_x87 x, Fadd_st0_x87 y -> Reg.equal x.src y.src
    | Fucomp, Fucomp -> true
    | Fnstsw, Fnstsw -> true
    | Sahf, Sahf -> true
    | _ -> false
end

(* {1 Fixups}

   The PC-relative kinds are split by *role* rather than merged into one
   [Pcrel32], because GNU does not treat them alike and the differential gate
   compares relocation intent. Measured on the M2 fixtures: a same-file global
   call is R_X86_64_PLT32 on x86-64 but R_386_PC32 on x86-32, a global data
   reference is PC-relative on x86-64 and *absolute* (R_386_32) on x86-32, and a
   branch to a local label keeps no record on either. One kind spanning all
   three would make those distinctions unrepresentable.

   [Pcrel8] is the short rung of the branch ladder; it exists because GNU picks
   the two-byte encodings for the loop fixture and byte equality is the gate. *)
type fixup_kind = Abs32 | Abs64 | Pcrel8_branch | Pcrel32_branch | Pcrel32_call | Pcrel32_data

let fixup_kind_name = function
  | Abs32 -> "abs32"
  | Abs64 -> "abs64"
  | Pcrel8_branch -> "pcrel8-branch"
  | Pcrel32_branch -> "pcrel32-branch"
  | Pcrel32_call -> "pcrel32-call"
  | Pcrel32_data -> "pcrel32-data"

let equal_fixup_kind a b = a = b

(* Ladder compatibility is checked on the family, so the short and near rungs of
   one branch must agree here even though they are different kinds of different
   widths - which is exactly what a ladder is. [Abs64] gets its own family
   rather than sharing [Abs32]'s "abs": they are different widths, and nothing
   should ever treat them as interchangeable rungs of one ladder. *)
let fixup_family = function
  | Abs32 -> "abs"
  | Abs64 -> "abs64"
  | Pcrel8_branch | Pcrel32_branch -> "pcrel-branch"
  | Pcrel32_call -> "pcrel-call"
  | Pcrel32_data -> "pcrel-data"

let fixup_role = function
  | Abs32 | Abs64 | Pcrel32_data -> Asm_core.Lowered_ast.Data_address
  | Pcrel8_branch | Pcrel32_branch -> Asm_core.Lowered_ast.Branch
  | Pcrel32_call -> Asm_core.Lowered_ast.Call

(* {1 Little-endian fields} *)

let mask ~width v =
  if width >= 64 then v else Int64.logand v (Int64.sub (Int64.shift_left 1L width) 1L)

let sign_extend ~width v =
  if width >= 64 then v
  else
    let s = 64 - width in
    Int64.shift_right (Int64.shift_left v s) s

(* Reversing the bytes of a [width]-bit value. An involution at each width, so
   the same function serves both directions of the isomorphism. *)
let bswap ~width v =
  let n = width / 8 in
  let v = mask ~width v in
  let r = ref 0L in
  for i = 0 to n - 1 do
    r :=
      Int64.logor (Int64.shift_left !r 8) (Int64.logand (Int64.shift_right_logical v (8 * i)) 0xFFL)
  done;
  !r

let le ~signedness ~width name =
  if width = 8 then C.field ~signedness ~width name
  else
    C.iso_fun ~name:(Printf.sprintf "le%d" width)
      ~encode:(fun v -> Some (bswap ~width v))
      ~decode:(fun v ->
        let raw = bswap ~width v in
        Some (match signedness with C.Signed -> sign_extend ~width raw | C.Unsigned -> raw))
      (C.field ~width name)

(* The same little-endian treatment for a *fixup* field.

   It is not optional. The codec lays bits down most-significant first, so a
   32-bit field spelled directly reads the four memory bytes as a big-endian
   number - while the linker patches the container little-endian, as every x86
   displacement is. A symbolic placeholder is all zeroes and hides the
   disagreement completely; it surfaces the moment anything decodes patched
   bytes, as [fa ff ff ff] read back as 0xfaffffff rather than -6.

   Wrapping does not disturb the placement: the [Iso_fun] transforms the value,
   not the bit positions, so the fixup still names the same four bytes. *)
let le_fixup ~width ~kind name =
  if width = 8 then C.fixup ~width ~kind name
  else
    C.iso_fun ~name:(Printf.sprintf "le%d" width)
      ~encode:(fun v -> Some (bswap ~width v))
      ~decode:(fun v -> Some (bswap ~width v))
      (C.fixup ~width ~kind name)

(* The absent displacement, typed as a displacement rather than as [unit], so
   that all six memory alternatives have the same tuple shape and are built by
   the same two functions. Reading it as "the displacement is not stored, which
   means zero" is exactly what mod=00 says. *)
let no_disp : (int64, fixup_kind) C.t =
  C.iso_fun ~name:"no-disp"
    ~encode:(fun v -> if Int64.equal v 0L then Some () else None)
    ~decode:(fun () -> Some 0L)
    C.empty

(* Lift a numeric displacement codec to the [Disp.t] an operand carries. The
   plain ones *decline* a symbolic displacement rather than writing its
   placeholder: there is no fixup node under them, so the zero would go out with
   nothing to patch it and the reference would silently vanish. Declining sends
   the encoder to the alternative that does have one, or to a diagnostic. *)
let const_disp ~name inner =
  C.iso_fun ~name
    ~encode:(function Disp.Const v -> Some v | Disp.Sym _ -> None)
    ~decode:(fun v -> Some (Disp.Const v))
    inner

(* The displacement of the mod=00 rm=101 form, and of the base-less SIB form
   below, both of which can carry a symbol. Symbolic values write a
   placeholder and emit a placement; constants write themselves and their
   placement is dropped for want of an expression, so one node serves both
   without a second alternative.

   [Fixup] (unlike [Field]) carries no signedness of its own - decode hands
   back the raw 32-bit pattern reconstructed as an unsigned magnitude, e.g.
   0xffffffff decodes to 4294967295L, not -1L. [Codec.sign_extend] is the
   same correction {!Field}'s own decode already applies for a signed field
   (codec.ml:535); without it here, a negative disp32 round-trips to the
   correct BYTES (encode truncates to the low 32 bits either way) but
   redisplays as a huge positive magnitude instead of the negative value the
   source wrote, which is what a reader needs to see to tell the two apart. *)
let sym_disp ~kind =
  C.iso_fun ~name:"disp-sym"
    ~encode:(fun d -> Some (Disp.placeholder d))
    ~decode:(fun v -> Some (Disp.Const (C.sign_extend ~width:32 v)))
    (le_fixup ~width:32 ~kind "disp")

(* The same idea as [sym_disp], for an immediate rather than a displacement
   (M5, asm/docs/corpus.md - [movl $.LC0,%edi], gcc's idiom for materializing
   a string/array address into a register; also [addq $bodies+24,%rax], the
   same idiom as an ALU operand). [signedness] is not hard-coded the way
   [sym_disp]'s always-sign-extending decode is, because the two callers
   disagree: {!Lowered.Mov_r_imm}'s [imm] has always round-tripped as the raw
   unsigned 32-bit magnitude, while {!Lowered.Alu_rm_imm}'s imm32 form
   already sign-extended before this constructor carried a symbol at all -
   each keeps its own prior decode exactly, so this stays purely additive for
   every value the non-symbolic forms already handled. *)
let sym_imm32 ~kind ~signedness =
  C.iso_fun ~name:"imm-sym32"
    ~encode:(fun d -> Some (Disp.placeholder d))
    ~decode:(fun v ->
      Some
        (Disp.Const
           (match signedness with
           | C.Signed -> sign_extend ~width:32 v
           | C.Unsigned -> mask ~width:32 v)))
    (le_fixup ~width:32 ~kind "imm")

(* [%rip] is not a general-purpose register and is admitted in exactly one
   place: as the base of a 64-bit RIP-relative operand. Numbering it outside the
   sixteen encodable registers is what stops it being written into a ModR/M or
   SIB field by accident - there is no bit pattern it could become. *)
(* Numbered outside the encodable range and *negative*, so it can never be
   mistaken for a register number: [num >= 8] is what sets REX.B, and RIP
   contributes no REX bit because it is not one of the sixteen. It reached this
   file as 16 first, which set REX.B and produced a byte GNU does not emit. *)
let rip_reg = { Reg.name = "rip"; num = -1; width = 64 }
let is_rip (r : Reg.t) = r.Reg.num = rip_reg.Reg.num

let base_is_pc ~rip_relative (m : Mem.t) =
  match m.Mem.base with
  | None -> not rip_relative
  | Some b -> rip_relative && b.Reg.num = rip_reg.Reg.num

let fits_s8 v = Int64.compare v (-128L) >= 0 && Int64.compare v 127L <= 0
let fits_s32 v = Int64.compare v (-2147483648L) >= 0 && Int64.compare v 2147483647L <= 0

(* GAS's own reading of a width-full immediate literal before any rung's
   [fits_s8]/[fits_s32] range check runs (M5, asm/docs/corpus.md -
   [vararg.S]'s real `andl $0xfffffffc,%edx`, GAS's own idiom for a 4-byte-
   alignment mask): the parsed literal is reduced modulo the *destination
   operand's* width and reinterpreted as two's-complement signed, not taken
   at its raw magnitude. Without this, the unsigned-looking hex spelling of
   a negative value at its own width (4294967292, i.e. 0xfffffffc) fits no
   signed rung at all, even though real i686-linux-gnu-as reduces it to -4
   and picks the short imm8 form ([83 e2 fc], byte-identical to the decimal
   spelling [andl $-4,%edx]). A no-op for width>=64: an OCaml [int64] is
   already its own 64-bit two's-complement representation, so reducing
   modulo 2^64 changes nothing. Truncating the *original* value to a
   shorter rung's field width afterwards still produces the same bytes
   either way - the two values are congruent modulo every power of two up
   to [width] - so only the fits-check inputs need this, not what gets
   written to the wire. *)
let to_width_signed ~width v =
  if width >= 64 then v
  else
    let modulus = Int64.shift_left 1L width in
    let residue = Int64.rem (Int64.add (Int64.rem v modulus) modulus) modulus in
    let half = Int64.shift_left 1L (width - 1) in
    if Int64.compare residue half >= 0 then Int64.sub residue modulus else residue

(* {1 ModR/M, SIB and displacement}

   One codec over [rm_enc], covering the whole group of bytes that the ModR/M
   byte governs. Splitting it - a ModR/M codec, then an optional SIB codec, then
   an optional displacement codec - is not possible without a shared decision,
   because [mod] and [rm] jointly decide whether the other two bytes exist. *)

type rm_enc = { re_reg : int; re_rm : rm }
(** [re_reg] is the ModR/M reg field, which for the group-1 opcodes is the
    *operation* rather than a register - the /digit in Intel's tables. *)

type sib = { sc : int; ix : int; bs : int }

let sib_codec : (sib, fixup_kind) C.t =
  C.iso_fun ~name:"sib"
    ~encode:(fun s -> Some (Int64.of_int s.sc, (Int64.of_int s.ix, Int64.of_int s.bs)))
    ~decode:(fun (sc, (ix, bs)) ->
      Some { sc = Int64.to_int sc; ix = Int64.to_int ix; bs = Int64.to_int bs })
    C.(field ~width:2 "scale" ** field ~width:3 "index" ** field ~width:3 "base")

let log2_scale = function 1 -> Some 0 | 2 -> Some 1 | 4 -> Some 2 | 8 -> Some 3 | _ -> None
let scale_of_log2 = function 0 -> 1 | 1 -> 2 | 2 -> 4 | 3 -> 8 | _ -> 1

(* SIB is required when there is an index, and also when the base's low three
   bits are 100 - the value that in the rm field *means* "a SIB byte follows",
   so rsp and r12 have no non-SIB encoding. *)
let needs_sib (m : Mem.t) =
  m.index <> None
  || match m.base with Some b -> (not (is_rip b)) && b.num land 7 = 4 | None -> true

(* Which displacement form GAS picks, and the one rule that is not "the smallest
   that fits": a base whose low three bits are 101 (rbp, r13) cannot use mod=00,
   because that encoding means "no base, disp32 follows". Such an operand gets
   an explicit zero disp8. *)
type disp_form = D_none | D_8 | D_32

let disp_form_of (m : Mem.t) =
  match m.base with
  (* No base is the SIB "base=101" escape (base register field is not used
     for a real register at all), same family as the rbp/r13 rule below but
     total rather than conditional: there is no mod=00-with-zero-bytes or
     mod=01-disp8 encoding for "no base", only mod=00 with a mandatory
     disp32, regardless of the displacement's actual value. *)
  | None -> D_32
  | Some b -> (
      let ebp_like = b.num land 7 = 5 in
      match m.disp with
      (* A symbolic displacement is always the wide form: its value is not
         known here, and a byte could not hold an address even if it were. *)
      | Disp.Sym _ -> D_32
      | Disp.Const v ->
          if Int64.equal v 0L && not ebp_like then D_none else if fits_s8 v then D_8 else D_32)

let sib_of (m : Mem.t) =
  match log2_scale (if m.index = None then 1 else m.scale) with
  | None -> None
  | Some sc ->
      Some
        {
          sc;
          ix = (match m.index with Some i -> i.num land 7 | None -> 4);
          bs = (match m.base with Some b -> b.num land 7 | None -> 5);
        }

(* Decoding a memory operand needs the register table back, so the two
   directions are parameterized over it. [reg_of_num] is the mode's 32- or
   64-bit address register set. *)
let make_rm_codec ~(reg_of_num : int -> Reg.t) ~rip_relative ~disp_kind : (rm_enc, fixup_kind) C.t =
  let mem_of_sib ~sib ~disp =
    {
      Mem.base = Some (reg_of_num sib.bs);
      (* Raw: whether index 4 is the "no index" escape or r12 depends on
         REX.X, which this codec cannot see. [extend_rex] decides. *)
      index = Some (reg_of_num sib.ix);
      scale = scale_of_log2 sib.sc;
      disp;
    }
  in
  let sib_alt ~label ~priority ~modbits ~form ~disp_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:("modrm-" ^ label)
         ~encode:(fun e ->
           match e.re_rm with
           | Rm.Mem m when needs_sib m && disp_form_of m = form -> (
               match sib_of m with
               | None -> None
               | Some s -> Some ((), (Int64.of_int (e.re_reg land 7), ((), (s, m.disp)))))
           | _ -> None)
         ~decode:(fun ((), (reg, ((), (s, disp)))) ->
           Some { re_reg = Int64.to_int reg; re_rm = Rm.Mem (mem_of_sib ~sib:s ~disp) })
         C.(
           const ~width:2 (Int64.of_int modbits)
           ** field ~width:3 "reg" ** const ~width:3 4L ** sib_codec ** disp_codec))
  in
  (* mod=00, SIB present, SIB.base=101: the SIB-side counterpart of
     [disp32-norm] below - the one SIB encoding whose *meaning* differs from
     the generic [sib_alt] forms rather than only its displacement width.
     SIB.base=101 with mod=00 is reserved to mean "no base register, disp32
     always follows", never "base=rbp/r13, no displacement" - so it cannot be
     built from [sib_alt] (whose SIB base subfield is a free, decoded field)
     no matter what [~form] it is given. The base field here is a CONSTANT,
     not a decoded value - there is nothing to store, since [Mem.base] is
     [None] whenever this alternative applies. *)
  let sib_nobase_disp32 ~priority =
    C.alt ~label:"sib-nobase-disp32" ~priority
      (C.iso_fun ~name:"modrm-sib-nobase-disp32"
         ~encode:(fun e ->
           match e.re_rm with
           | Rm.Mem m when m.Mem.index <> None && m.Mem.base = None -> (
               match (m.Mem.index, log2_scale m.Mem.scale) with
               | Some i, Some sc ->
                   Some
                     ( (),
                       ( Int64.of_int (e.re_reg land 7),
                         ((), ((Int64.of_int sc, (Int64.of_int (i.num land 7), ())), m.Mem.disp)) )
                     )
               | _ -> None)
           | _ -> None)
         ~decode:(fun ((), (reg, ((), ((sc, (ix, ())), disp)))) ->
           Some
             {
               re_reg = Int64.to_int reg;
               re_rm =
                 Rm.Mem
                   {
                     Mem.base = None;
                     index = Some (reg_of_num (Int64.to_int ix));
                     scale = scale_of_log2 (Int64.to_int sc);
                     disp;
                   };
             })
         C.(
           const ~width:2 0L ** field ~width:3 "reg" ** const ~width:3 4L
           ** (field ~width:2 "scale" ** field ~width:3 "index" ** const ~width:3 5L)
           ** sym_disp ~kind:disp_kind))
  in
  let base_alt ~label ~priority ~modbits ~form ~disp_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:("modrm-" ^ label)
         ~encode:(fun e ->
           match e.re_rm with
           (* A RIP base is excluded here and handled by [disp32-norm]: its bits
              are mod=00 rm=101, not a register field, and writing [-1 land 7]
              into one would encode a different address entirely. *)
           | Rm.Mem m when (not (needs_sib m)) && disp_form_of m = form -> (
               match m.base with
               | Some b when is_rip b -> None
               | None -> None
               | Some b ->
                   Some ((), (Int64.of_int (e.re_reg land 7), (Int64.of_int (b.num land 7), m.disp)))
               )
           | _ -> None)
         ~decode:(fun ((), (reg, (rm, disp))) ->
           Some
             {
               re_reg = Int64.to_int reg;
               re_rm = Rm.Mem (Mem.of_base ~disp (reg_of_num (Int64.to_int rm)));
             })
         C.(
           const ~width:2 (Int64.of_int modbits)
           ** field ~width:3 "reg" ** field ~width:3 "rm" ** disp_codec))
  in
  C.choice ~name:"modrm"
    [
      (* Priority is decode order, and it has to put the SIB forms first: their
         rm field is the constant 100 while the non-SIB forms have a free rm
         field that can also hold 100, so the two overlap and only order
         separates them. [check] reports the overlap, correctly - it is real,
         and priority is what resolves it. *)
      C.alt ~label:"reg" ~priority:0
        (C.iso_fun ~name:"modrm-reg"
           ~encode:(fun e ->
             match e.re_rm with
             | Rm.Reg r -> Some ((), (Int64.of_int (e.re_reg land 7), Int64.of_int (r.num land 7)))
             | Rm.Mem _ -> None)
           ~decode:(fun ((), (reg, rm)) ->
             Some { re_reg = Int64.to_int reg; re_rm = Rm.Reg (reg_of_num (Int64.to_int rm)) })
           C.(const ~width:2 3L ** field ~width:3 "reg" ** field ~width:3 "rm"));
      (* Ahead of sib-disp0: sib-disp0's SIB base subfield is free and its
         displacement is [no_disp] (zero bytes), so without this ordering it
         would match the first 3 bytes of a real base-less-SIB instruction
         and stop, leaving the mandatory disp32 bytes to be misread as the
         start of the next instruction. Bit-disjoint from [disp32-norm]
         below (rm=100 with a SIB byte here, vs. rm=101 with none there), so
         no other reordering is required. *)
      sib_nobase_disp32 ~priority:1;
      sib_alt ~label:"sib-disp0" ~priority:2 ~modbits:0 ~form:D_none
        ~disp_codec:(const_disp ~name:"disp-none" no_disp);
      sib_alt ~label:"sib-disp8" ~priority:3 ~modbits:1 ~form:D_8
        ~disp_codec:(const_disp ~name:"disp-c8" (le ~signedness:C.Signed ~width:8 "disp8"));
      sib_alt ~label:"sib-disp32" ~priority:4 ~modbits:2 ~form:D_32
        ~disp_codec:(const_disp ~name:"disp-c32" (le ~signedness:C.Signed ~width:32 "disp32"));
      base_alt ~label:"base-disp0" ~priority:6 ~modbits:0 ~form:D_none
        ~disp_codec:(const_disp ~name:"disp-none" no_disp);
      base_alt ~label:"base-disp8" ~priority:7 ~modbits:1 ~form:D_8
        ~disp_codec:(const_disp ~name:"disp-c8" (le ~signedness:C.Signed ~width:8 "disp8"));
      (* Unlike the disp0/disp8 base forms above, disp32 is also what a
         symbolic base displacement takes ([disp_form_of] always classifies
         [Disp.Sym] as [D_32]), so this alternative reuses [sym_disp] rather
         than [const_disp] - the same base/SIB split [disp32-norm] and
         [sib_nobase_disp32] already draw between a fixup-carrying disp32 and
         a plain one. A numeric operand is unaffected: [sym_disp] already
         round-trips [Disp.Const] identically to [const_disp] (see its
         comment above). *)
      base_alt ~label:"base-disp32" ~priority:8 ~modbits:2 ~form:D_32
        ~disp_codec:(sym_disp ~kind:disp_kind);
      (* mod=00, rm=101: the one ModR/M encoding whose *meaning* differs between
         the two modes rather than only its operand width. In 32-bit it is an
         absolute disp32 with no base; in 64-bit the same bits are RIP-relative,
         which is why [%%rip] exists as a register admitted nowhere else. The
         difference is a parameter rather than two alternatives because the bits
         are identical and a decoder must not have to choose.

         The displacement is a fixup node, not a field, because this is the form
         a data reference takes: [movl asm_test_global(%%rip), %%eax]. A constant
         disp reaching it emits a placement with no expression, which [form_of]
         drops - so nothing changes for a numeric operand. *)
      (* Ahead of the [base-*] forms, because its rm field is the constant 101
         while theirs is free and can hold 101 too - and mod=00 rm=101 is never
         a base register on either mode. Decoding it as one would read four
         displacement bytes as the next instruction. *)
      C.alt ~label:"disp32-norm" ~priority:5
        (C.iso_fun ~name:"modrm-disp32-norm"
           ~encode:(fun e ->
             match e.re_rm with
             | Rm.Mem m when m.Mem.index = None && base_is_pc ~rip_relative m ->
                 Some ((), (Int64.of_int (e.re_reg land 7), ((), m.Mem.disp)))
             | _ -> None)
           ~decode:(fun ((), (reg, ((), disp))) ->
             Some
               {
                 re_reg = Int64.to_int reg;
                 re_rm =
                   Rm.Mem
                     {
                       Mem.base = (if rip_relative then Some rip_reg else None);
                       index = None;
                       scale = 1;
                       disp;
                     };
               })
           C.(
             const ~width:2 0L ** field ~width:3 "reg" ** const ~width:3 5L
             ** sym_disp ~kind:disp_kind));
    ]

(* An [Empty] displacement makes the disp0 alternatives' tuple shape
   [(unit * ...) ] rather than a special case, so all six memory alternatives
   are built by the same two functions above. *)

(* {1 The mode parameter} *)

module type MODE = sig
  val name : string
  val triple : string

  val address_width : int
  (** the width of a register usable as a base or index: 32 or 64. Not the same as the default
      operand size, which is 32 in both modes - [movl $42, %eax] is the identical encoding on
      x86-64. *)

  val rex_allowed : bool
  (** whether a REX byte may exist at all. In 32-bit mode the encodings 0x40-0x4f are inc/dec, so a
      REX-shaped alternative in the codec would not merely be unused - it would decode those
      instructions as a prefix on whatever followed. *)

  val registers : Reg.t list

  val nop_table : string array
  (** the padding this mode's GNU as emits, indexed by length minus one. Per mode, not shared, and
      the difference is not cosmetic: 64-bit GAS pads with the long-NOP family ([0f 1f ...]) while
      32-bit GAS pads with [lea] forms ([8d 76 00], [2e 8d b4 26 ...]), because the long NOP is P6+
      and the 32-bit default target does not assume it. A shared table would be wrong in one mode. *)

  val merge_nop_table : string array
  (** M3 §5 (.ai/asm_plan.md §12): the padding GNU's LINKER (not [as]) emits for a gap it inserts
      between two modules' contributions to one executable output section. Measured separately from
      {!nop_table}, and not always the same array: [x86_64]'s [ld] agrees with its [as] and reuses the
      long-NOP table, but [x86_32]'s [ld] fills with repeated 2-byte [66 90] alone, never the wider
      LEA forms {!nop_table} uses for [.align] - a genuine difference between what the assembler and
      the linker each do on 32-bit, not a simplification. *)
end

module Make (M : MODE) = struct
  let name = M.name
  let triple = M.triple

  (* The constructor modules are re-exported rather than re-declared, so
     [X86_64.Operand.Reg] and [X86_32.Operand.Reg] are the *same* constructor
     of the same type. Two modes that had their own copies would make an AST
     written for one mode fail to typecheck against the other for a reason that
     has nothing to do with the instruction set. *)
  module Reg = Reg
  module Mem = Mem
  module Operand = Operand
  module Opcode = Opcode
  module Rm = Rm
  module Surface = Surface
  module Instruction = Instruction
  module Lowered = Lowered

  type nonrec fixup_kind = fixup_kind

  (* Shared with the family, not per mode: what a kind *means* is an ELF
     property of the architecture, and x86-32 and x86-64 differ only in which
     kinds their forms select, not in what a kind is. *)
  let fixup_kind_name = fixup_kind_name
  let equal_fixup_kind = equal_fixup_kind
  let fixup_family = fixup_family
  let fixup_role = fixup_role

  (* The separable instruction components this family publishes. The rest is the base. *)
  let components = [ X86_x87.component ]

  (* [config] is the validated feature set; see the RISC-V family's [target_state] for why it
     lives in the state. No directive changes it - [.arch] is not supported - so it is fixed
     for a unit by the caller's initial state. *)
  type target_state = { config : Target_config.t }

  let default_config = Target_config.default components
  let default_state = { config = default_config }
  let initial_state config = { config }
  let state_config s = s.config
  let address_regs = List.filter (fun (r : Reg.t) -> r.width = M.address_width) M.registers

  let reg_at ~width n =
    match List.find_opt (fun (r : Reg.t) -> r.width = width && r.num = n) M.registers with
    | Some r -> r
    | None when width = 80 && n >= 0 && n <= 7 ->
        { Reg.name = Printf.sprintf "st(%d)" n; num = n; width }
    | None -> { Reg.name = Printf.sprintf "r%d?" n; num = n; width }

  (* The register a ModR/M or SIB field denotes when it is being used to form an
     *address*. Always the mode's address width, whatever the operand size is:
     the base register in [movl %eax, 0(%rsp)] is 64-bit on x86-64 even though
     the operand moved is 32-bit. *)
  let reg_of_num n = reg_at ~width:M.address_width (n land 7)

  (* And the register the same field denotes when it is an *operand*. Decoding
     cannot know which until the form says so, which is why this is applied by
     each form's decode rather than inside [rm_codec]: with no REX the mov below
     writes [%eax], and printing [%rax] there would be a disassembly that does
     not re-assemble to the bytes it came from. *)
  let retype ~width (r : Reg.t) = reg_at ~width r.num
  let retype_rm ~width = function Rm.Reg r -> Rm.Reg (retype ~width r) | Rm.Mem _ as m -> m
  let find_reg n = List.find_opt (fun (r : Reg.t) -> String.equal r.name n) M.registers

  (* The encoder's error domain (asm/docs/errors.md). Below source text, so it
     names no token; the front end's operand failures are a separate domain in
     x86_family.ml, for the reason {!Target_intf.Target.TARGET} gives. Inside
     [Make], so x86_32 and x86_64 share one domain the way they share one
     encoder.

     The out-of-scope tags are the interesting ones. Eight messages here said
     "is not in M1 scope" or "is only supported as ... in M1", which made the
     milestone boundary a fact about English. Each is a constructor now, so a
     caller - or a test asking what this milestone refuses - matches on it. *)
  type error_kind =
    [ Target_error.shared
    | `Missing_size_suffix of string
    | `Prefix66_out_of_scope
    | `Operand8_out_of_scope
    | `No_64bit_size of string
    | `Ret_takes_operands
    | `Ud2_takes_operands
    | `Fucomp_takes_operands
    | `Fnstsw_operand
    | `Sahf_takes_operands
    | `Cmov_operands
    | `Bad_branch_suffix of bad_branch_suffix
    | `Immediate_destination
    | `Imm_to_mem_only_movb
    | `Mov8_only_movb
    | `Codec of Codec.error
    | `Decode_no_match
    | `Decode_partial_bytes
    | `Displacement_not_8bit
    | `Displacement_not_32bit
    | `No_data_relocation of int
    | `Negative_padding of int
    | `Register_width_mismatch of register_width_mismatch
    | `Shift_count_not_one of int64
    | `Setcc_operands
    | `Sse_operand_class of sse_operand_class_mismatch
    | `Vex_rm_extended_register of string ]

  and bad_branch_suffix = { mnemonic : string; rungs : string list }
  and register_width_mismatch = { reg : string; reg_width : int; insn_width : int }

  and sse_operand_class_mismatch = { sse_reg : string; sse_reg_width : int }
  (** Always "expected xmm, found something else" (M5, asm/docs/corpus.md):
          reusing {!Reg.t}/{!Rm.t} for both GPR and xmm operands buys width- and
          number-generic ModR/M machinery for free, but it also means nothing
          else in this domain stops [addsd %eax, %xmm0] from lowering as if
          [%eax] were [%xmm0] - the codec sees only a register number, never a
          class. This is that check's diagnostic. The opposite direction (an
          xmm register where a GPR is required, e.g. [cvtsi2sd]'s [rm] operand)
          reuses {!Register_width_mismatch} instead of a second case here,
          since xmm's width (128) already can never equal a GPR instruction's
          declared width - the existing check is already exactly the right
          shape for it. *)

  type error = error_kind Target_error.t

  let pp_error_kind ppf : error_kind -> unit = function
    | #Target_error.shared as e -> Target_error.pp_shared ppf e
    | `Missing_size_suffix m -> Fmt.pf ppf "%s needs an operand-size suffix (b, w, l or q)" m
    | `Prefix66_out_of_scope ->
        Fmt.string ppf "16-bit operands need the 0x66 prefix, which is not in M1 scope"
    | `Operand8_out_of_scope -> Fmt.string ppf "8-bit operands are not in M1 scope"
    | `No_64bit_size mode -> Fmt.pf ppf "%s has no 64-bit operand size" mode
    | `Ret_takes_operands -> Fmt.string ppf "ret takes no operands in M1"
    | `Ud2_takes_operands -> Fmt.string ppf "ud2 takes no operands"
    | `Fucomp_takes_operands -> Fmt.string ppf "fucomp takes no operands in M5"
    | `Fnstsw_operand -> Fmt.string ppf "fnstsw is only supported as fnstsw %%ax in M5"
    | `Sahf_takes_operands -> Fmt.string ppf "sahf takes no operands"
    | `Cmov_operands -> Fmt.string ppf "cmov takes two register operands in M2"
    | `Setcc_operands -> Fmt.string ppf "setcc takes exactly one 8-bit register or memory operand"
    | `Bad_branch_suffix { mnemonic; rungs } ->
        Fmt.pf ppf "%s: the branch form suffixes are %s" mnemonic
          (String.concat ", " (List.map (fun r -> "." ^ r) rungs))
    | `Immediate_destination -> Fmt.string ppf "an immediate cannot be a destination"
    | `Imm_to_mem_only_movb ->
        Fmt.string ppf "movb $imm,(mem) is the only imm-to-memory mov form in M1"
    | `Mov8_only_movb -> Fmt.string ppf "8-bit mov is only supported as movb $imm,(mem) in M1"
    | `Codec e -> Codec.pp_error ppf e
    | `Decode_no_match -> Fmt.string ppf "no form matches these bytes"
    | `Decode_partial_bytes -> Fmt.string ppf "a form consumed a non-whole number of bytes"
    | `Displacement_not_8bit -> Fmt.string ppf "displacement does not fit 8 bits"
    | `Displacement_not_32bit -> Fmt.string ppf "displacement does not fit 32 bits"
    | `No_data_relocation w -> Fmt.pf ppf "no absolute relocation for a %d-byte data initializer" w
    | `Negative_padding _ -> Fmt.string ppf "negative padding length"
    | `Register_width_mismatch { reg; reg_width; insn_width } ->
        Fmt.pf ppf "%s is %d-bit but the instruction is %d-bit" reg reg_width insn_width
    | `Shift_count_not_one n ->
        Fmt.pf ppf
          "shift/rotate-by-1 count must be exactly 1, got %Ld (the general immediate-count form is \
           not implemented)"
          n
    | `Sse_operand_class { sse_reg; sse_reg_width } ->
        Fmt.pf ppf "%s is %d-bit, expected an xmm register" sse_reg sse_reg_width
    | `Vex_rm_extended_register reg ->
        Fmt.pf ppf
          "%s cannot be part of the r/m operand of a two-byte-VEX-encoded instruction (a register \
           numbered 8 and above there needs the three-byte VEX prefix, not yet supported)"
          reg

  (* The phase that detected it, which is what the code has always named. The
     codec arm delegates: [Codec.code] is [Some] only where that layer is the
     only one that could have seen the mistake (asm/docs/errors.md §2). *)
  let error_kind_code : error_kind -> string = function
    | `Unknown_instruction _ | `Missing_size_suffix _ | `Prefix66_out_of_scope
    | `Operand8_out_of_scope | `No_64bit_size _ | `Ret_takes_operands | `Ud2_takes_operands
    | `Fucomp_takes_operands | `Fnstsw_operand | `Sahf_takes_operands | `Cmov_operands
    | `Setcc_operands ->
        "x86.simplify"
    | `Bad_branch_suffix _ -> "x86.branch-suffix"
    | `Immediate_too_wide | `No_form _ | `Immediate_destination | `Imm_to_mem_only_movb
    | `Mov8_only_movb | `Register_width_mismatch _ | `Shift_count_not_one _ | `Sse_operand_class _
    | `Vex_rm_extended_register _ ->
        "x86.lower"
    | `Codec e -> Option.value (Codec.code e) ~default:"x86.encode"
    | `Decode_no_match | `Decode_partial_bytes | `Decode_no_normalized -> "x86.decode"
    | `Displacement_not_8bit | `Displacement_not_32bit -> "x86.fixup"
    | `No_data_relocation _ -> "x86.data-fixup"
    | `Negative_padding _ -> "x86.nop"
    | `Feature_disabled _ -> "x86.feature"

  let pp_error ppf e = pp_error_kind ppf (Target_error.kind e)
  let error_code e = error_kind_code (Target_error.kind e)
  let error_diagnostic e = Target_error.to_diagnostic ~code:error_kind_code ~pp:pp_error_kind e

  let diag ?pos ?origin kind =
    Err.Error.make ?pos ~pp_error
      (Target_error.make
         ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:M.name ())
         kind)

  let make_surface_instruction ~mnemonic ~origin ops = Ok { Surface.mnemonic; ops; origin }

  (* {2 Simplify: surface -> normalized}

     The AT&T suffix decides the operand width, and dropping it is the whole of
     x86 normalization at M1. An unsuffixed mnemonic that needs a width is
     rejected rather than defaulted: GAS infers one from the register operand,
     and inferring differently from GAS is worse than refusing. *)

  let split_suffix m =
    let n = String.length m in
    if n < 2 then (m, None)
    else
      let stem = String.sub m 0 (n - 1) in
      match m.[n - 1] with
      | 'b' -> (stem, Some 8)
      | 'w' -> (stem, Some 16)
      | 'l' -> (stem, Some 32)
      | 'q' -> (stem, Some 64)
      | _ -> (m, None)

  (* [movzbl]/[movsbl]/[movslq] and the rest of the zero-/sign-extending move
     family (M5, asm/docs/corpus.md): GAS spells these as [prefix] followed by
     *two* one-letter widths (source, then destination), not a stem plus one
     trailing suffix - [split_suffix] cannot express this shape at all, so it
     gets its own small parser. [sw < dw] is what makes [movslq] (l then q)
     valid and rejects a nonsensical [movzlb] (l then b) the same table would
     otherwise accept. *)
  let width_suffix_char = function
    | 'b' -> Some 8
    | 'w' -> Some 16
    | 'l' -> Some 32
    | 'q' -> Some 64
    | _ -> None

  let movx_suffixes ~prefix m =
    let plen = String.length prefix and n = String.length m in
    if n <> plen + 2 || String.sub m 0 plen <> prefix then None
    else
      match (width_suffix_char m.[plen], width_suffix_char m.[plen + 1]) with
      | Some sw, Some dw when sw < dw -> Some (sw, dw)
      | _ -> None

  (* The rungs this target's branch ladders declare, and therefore exactly the
     suffixes its mnemonics accept. Sharing one list with the codec is what
     keeps a spelling and a form id from drifting apart. *)
  let branch_rungs = [ "d8"; "d32" ]

  (* [jmp], [jne], and either with a [.d8]/[.d32] pin. Returns the opcode and
     the pin, or [None] if the mnemonic is not a branch at all - which is why
     the caller can use it as a guard without a second parse. *)
  let branch_of m =
    let base m =
      if String.equal m "jmp" then Some Opcode.Jmp
      else Option.map (fun c -> Opcode.Jcc c) (Cc.split_after "j" m)
    in
    match base m with
    | Some op -> Some (op, None)
    | None ->
        List.fold_left
          (fun acc rung ->
            match acc with
            | Some _ -> acc
            | None ->
                let suffix = "." ^ rung in
                let n = String.length m and k = String.length suffix in
                if n > k && String.sub m (n - k) k = suffix then
                  Option.map (fun op -> (op, Some rung)) (base (String.sub m 0 (n - k)))
                else None)
          None branch_rungs

  (* [jmp.d16] - a branch mnemonic with a suffix this target has no rung for.
     Distinguished from an unknown instruction because it is a different
     mistake: the writer meant a branch and named a form that does not exist,
     and being told "unknown instruction" would send them looking for the wrong
     thing. *)
  let bad_branch_suffix m =
    match String.rindex_opt m '.' with
    | None -> false
    | Some i ->
        branch_of m = None
        && (String.equal (String.sub m 0 i) "jmp" || Cc.split_after "j" (String.sub m 0 i) <> None)

  let simplify_hand_written s =
    let bad kind = Error (diag ~pos:__POS__ ~origin:s.Surface.origin kind) in
    let stem, suffix = split_suffix s.Surface.mnemonic in
    (* [~allow16] is narrowly scoped to [mov] (M5, asm/docs/corpus.md:
       [movw %r8w, 58(%rsp)]), the one 16-bit form this corpus evidences -
       every other ALU mnemonic keeps the blanket [Prefix66_out_of_scope]
       rejection with its own clear diagnostic, rather than silently
       reaching [lower_instruction] and failing there with a generic
       "no form takes these operands" once the encoder gained the general
       0x66 machinery [mov] needed. [~allow8] started the same way (scoped to
       [mov] alone) and is now also set for the eight GRP1 ALU mnemonics
       ([add]/[sub]/
       [and]/[or]/[xor]/[cmp]/[adc]/[sbb]); this only opens the *suffix*, not
       every operand shape at that width - a shape [lower_instruction]/the
       codec do not yet build for width 8 (register-register, either memory
       direction) still fails there with its own generic diagnostic, exactly
       as an unimplemented shape at any other width already does. *)
    let widthed ?(allow8 = false) ?(allow16 = false) op =
      match suffix with
      | None -> bad (`Missing_size_suffix s.Surface.mnemonic)
      | Some 16 when not allow16 -> bad `Prefix66_out_of_scope
      | Some 8 when not allow8 -> bad `Operand8_out_of_scope
      | Some w when w = 64 && not M.rex_allowed -> bad (`No_64bit_size M.name)
      | Some w -> Ok (Instruction.mk op w s.Surface.ops)
    in
    match (s.Surface.mnemonic, stem) with
    | "ret", _ ->
        if s.Surface.ops = [] then Ok (Instruction.mk Opcode.Ret M.address_width [])
        else bad `Ret_takes_operands
    | "ud2", _ ->
        if s.Surface.ops = [] then Ok (Instruction.mk Opcode.Ud2 M.address_width [])
        else bad `Ud2_takes_operands
    (* Matched on [stem], not the full mnemonic: real INRIA/GNU source spells
       these WITH the operand-size suffix (M4, .ai/asm_plan.md §12 - the
       i64_divmod runtime-helper fixture's own [popl]/[pushl]/[decl]), and
       [split_suffix] already reduces both spellings to the same stem. The
       one operand's own width disambiguates either way, so the suffix (if
       any) is accepted and ignored rather than validated against
       [M.address_width]. *)
    (* only the address width: [decb]/[pushw] are generated rows, not this form *)
    | _, "pop" when suffix = None || suffix = Some M.address_width ->
        Ok (Instruction.mk Opcode.Pop M.address_width s.Surface.ops)
    | _, "push" when suffix = None || suffix = Some M.address_width ->
        Ok (Instruction.mk Opcode.Push M.address_width s.Surface.ops)
    (* the codec builds only x86-32's 0x48+r dec; x86-64's FF /1 is a generated row *)
    | _, "dec" when (not M.rex_allowed) && (suffix = None || suffix = Some M.address_width) ->
        Ok (Instruction.mk Opcode.Dec M.address_width s.Surface.ops)
    | "jmp", _ -> Ok (Instruction.mk Opcode.Jmp M.address_width s.Surface.ops)
    (* No size suffix, in either mode: a near call is rel32 on x86-32 and on
       x86-64 alike, so there is nothing for a suffix to select. *)
    | "call", _ -> Ok (Instruction.mk Opcode.Call M.address_width s.Surface.ops)
    (* {3 SSE2 scalar float (M5, asm/docs/corpus.md)}

       Fixed mnemonics, matched on the mnemonic directly rather than through
       [stem]/[widthed]: none of these carry an AT&T size suffix - GAS never
       writes [addsdl] - so there is no suffix to split off. [Instruction.width]
       does not mean "this instruction's operand width" for these the way it
       does elsewhere in this function; see {!instruction_of_lowered}'s and
       {!Instruction.pp}'s comments for what it means here instead. *)
    | "addsd", _ -> Ok (Instruction.mk Opcode.Addsd 32 s.Surface.ops)
    | "subsd", _ -> Ok (Instruction.mk Opcode.Subsd 32 s.Surface.ops)
    | "mulsd", _ -> Ok (Instruction.mk Opcode.Mulsd 32 s.Surface.ops)
    | "divsd", _ -> Ok (Instruction.mk Opcode.Divsd 32 s.Surface.ops)
    | "addss", _ -> Ok (Instruction.mk Opcode.Addss 32 s.Surface.ops)
    | "subss", _ -> Ok (Instruction.mk Opcode.Subss 32 s.Surface.ops)
    | "mulss", _ -> Ok (Instruction.mk Opcode.Mulss 32 s.Surface.ops)
    | "divss", _ -> Ok (Instruction.mk Opcode.Divss 32 s.Surface.ops)
    | "comisd", _ -> Ok (Instruction.mk Opcode.Comisd 32 s.Surface.ops)
    | "ucomisd", _ -> Ok (Instruction.mk Opcode.Ucomisd 32 s.Surface.ops)
    | "comiss", _ -> Ok (Instruction.mk Opcode.Comiss 32 s.Surface.ops)
    | "xorpd", _ -> Ok (Instruction.mk Opcode.Xorpd 32 s.Surface.ops)
    | "pxor", _ -> Ok (Instruction.mk Opcode.Pxor 32 s.Surface.ops)
    | "movapd", _ -> Ok (Instruction.mk Opcode.Movapd 32 s.Surface.ops)
    | "cvtsd2ss", _ -> Ok (Instruction.mk Opcode.Cvtsd2ss 32 s.Surface.ops)
    | "cvtss2sd", _ -> Ok (Instruction.mk Opcode.Cvtss2sd 32 s.Surface.ops)
    (* {!Opcode.Cvtsd2ss}/{!Opcode.Cvtss2sd}'s own opcode byte, none/66 prefix instead of
       F2/F3 - {!Opcode.Cvtps2pd}'s own doc comment. *)
    | "cvtps2pd", _ -> Ok (Instruction.mk Opcode.Cvtps2pd 32 s.Surface.ops)
    | "cvtpd2ps", _ -> Ok (Instruction.mk Opcode.Cvtpd2ps 32 s.Surface.ops)
    | "cvtdq2ps", _ -> Ok (Instruction.mk Opcode.Cvtdq2ps 32 s.Surface.ops)
    | "cvtps2dq", _ -> Ok (Instruction.mk Opcode.Cvtps2dq 32 s.Surface.ops)
    | "cvttps2dq", _ -> Ok (Instruction.mk Opcode.Cvttps2dq 32 s.Surface.ops)
    | "movsd", _ -> Ok (Instruction.mk Opcode.Movsd 32 s.Surface.ops)
    | "movss", _ -> Ok (Instruction.mk Opcode.Movss 32 s.Surface.ops)
    (* Packed interleave family: the first genuine two-source-operand packed binop,
       {!Opcode.Andps}'s own mandatory-prefix-free/66 group at a different opcode byte. *)
    | "unpcklps", _ -> Ok (Instruction.mk Opcode.Unpcklps 32 s.Surface.ops)
    | "unpckhps", _ -> Ok (Instruction.mk Opcode.Unpckhps 32 s.Surface.ops)
    | "unpcklpd", _ -> Ok (Instruction.mk Opcode.Unpcklpd 32 s.Surface.ops)
    | "unpckhpd", _ -> Ok (Instruction.mk Opcode.Unpckhpd 32 s.Surface.ops)
    (* {!Opcode.Unpcklps}'s integer-SIMD sibling: 66-mandatory-prefix only, same
       opcode byte at {!Opcode.Punpcklqdq}'s own doc comment. *)
    | "punpcklqdq", _ -> Ok (Instruction.mk Opcode.Punpcklqdq 32 s.Surface.ops)
    | "punpckhqdq", _ -> Ok (Instruction.mk Opcode.Punpckhqdq 32 s.Surface.ops)
    | "punpcklbw", _ -> Ok (Instruction.mk Opcode.Punpcklbw 32 s.Surface.ops)
    | "punpckhbw", _ -> Ok (Instruction.mk Opcode.Punpckhbw 32 s.Surface.ops)
    | "punpcklwd", _ -> Ok (Instruction.mk Opcode.Punpcklwd 32 s.Surface.ops)
    | "punpckhwd", _ -> Ok (Instruction.mk Opcode.Punpckhwd 32 s.Surface.ops)
    | "punpckldq", _ -> Ok (Instruction.mk Opcode.Punpckldq 32 s.Surface.ops)
    | "punpckhdq", _ -> Ok (Instruction.mk Opcode.Punpckhdq 32 s.Surface.ops)
    (* {!Opcode.Paddb}'s own doc comment: packed integer add/subtract, 66-mandatory-
       prefix only, {!Opcode.Punpcklqdq}'s own group at different opcode bytes. *)
    | "paddb", _ -> Ok (Instruction.mk Opcode.Paddb 32 s.Surface.ops)
    | "paddw", _ -> Ok (Instruction.mk Opcode.Paddw 32 s.Surface.ops)
    | "paddd", _ -> Ok (Instruction.mk Opcode.Paddd 32 s.Surface.ops)
    | "paddq", _ -> Ok (Instruction.mk Opcode.Paddq 32 s.Surface.ops)
    | "psubb", _ -> Ok (Instruction.mk Opcode.Psubb 32 s.Surface.ops)
    | "psubw", _ -> Ok (Instruction.mk Opcode.Psubw 32 s.Surface.ops)
    | "psubd", _ -> Ok (Instruction.mk Opcode.Psubd 32 s.Surface.ops)
    | "psubq", _ -> Ok (Instruction.mk Opcode.Psubq 32 s.Surface.ops)
    (* {!Opcode.Pcmpeqb}'s own doc comment: packed compare-equal/greater-than,
       66-mandatory-prefix only, {!Opcode.Paddb}'s own group at different opcode bytes. *)
    | "pcmpeqb", _ -> Ok (Instruction.mk Opcode.Pcmpeqb 32 s.Surface.ops)
    | "pcmpeqw", _ -> Ok (Instruction.mk Opcode.Pcmpeqw 32 s.Surface.ops)
    | "pcmpeqd", _ -> Ok (Instruction.mk Opcode.Pcmpeqd 32 s.Surface.ops)
    | "pcmpgtb", _ -> Ok (Instruction.mk Opcode.Pcmpgtb 32 s.Surface.ops)
    | "pcmpgtw", _ -> Ok (Instruction.mk Opcode.Pcmpgtw 32 s.Surface.ops)
    | "pcmpgtd", _ -> Ok (Instruction.mk Opcode.Pcmpgtd 32 s.Surface.ops)
    (* {!Opcode.Packsswb}'s own doc comment: pack-with-saturation, {!Opcode.Paddb}'s own
       group at different opcode bytes. *)
    | "packsswb", _ -> Ok (Instruction.mk Opcode.Packsswb 32 s.Surface.ops)
    | "packssdw", _ -> Ok (Instruction.mk Opcode.Packssdw 32 s.Surface.ops)
    | "packuswb", _ -> Ok (Instruction.mk Opcode.Packuswb 32 s.Surface.ops)
    (* {!Opcode.Pand}'s own doc comment: packed integer bitwise AND/AND-NOT/OR,
       {!Opcode.Pxor}'s own group at different opcode bytes. *)
    | "pand", _ -> Ok (Instruction.mk Opcode.Pand 32 s.Surface.ops)
    | "pandn", _ -> Ok (Instruction.mk Opcode.Pandn 32 s.Surface.ops)
    | "por", _ -> Ok (Instruction.mk Opcode.Por 32 s.Surface.ops)
    (* {!Opcode.Pminub}'s own doc comment: packed integer min/max, {!Opcode.Paddb}'s own
       group at different opcode bytes. *)
    | "pminub", _ -> Ok (Instruction.mk Opcode.Pminub 32 s.Surface.ops)
    | "pmaxub", _ -> Ok (Instruction.mk Opcode.Pmaxub 32 s.Surface.ops)
    | "pminsw", _ -> Ok (Instruction.mk Opcode.Pminsw 32 s.Surface.ops)
    | "pmaxsw", _ -> Ok (Instruction.mk Opcode.Pmaxsw 32 s.Surface.ops)
    (* {!Opcode.Pmullw}'s own doc comment: packed integer multiply/average/sum-of-
       absolute-differences, {!Opcode.Paddb}'s own group at different opcode bytes. *)
    | "pmullw", _ -> Ok (Instruction.mk Opcode.Pmullw 32 s.Surface.ops)
    | "pmulhw", _ -> Ok (Instruction.mk Opcode.Pmulhw 32 s.Surface.ops)
    | "pmulhuw", _ -> Ok (Instruction.mk Opcode.Pmulhuw 32 s.Surface.ops)
    | "pavgb", _ -> Ok (Instruction.mk Opcode.Pavgb 32 s.Surface.ops)
    | "pavgw", _ -> Ok (Instruction.mk Opcode.Pavgw 32 s.Surface.ops)
    | "psadbw", _ -> Ok (Instruction.mk Opcode.Psadbw 32 s.Surface.ops)
    | "psllw", _ -> Ok (Instruction.mk Opcode.Psllw 32 s.Surface.ops)
    | "pslld", _ -> Ok (Instruction.mk Opcode.Pslld 32 s.Surface.ops)
    | "psllq", _ -> Ok (Instruction.mk Opcode.Psllq 32 s.Surface.ops)
    | "psrlw", _ -> Ok (Instruction.mk Opcode.Psrlw 32 s.Surface.ops)
    | "psrld", _ -> Ok (Instruction.mk Opcode.Psrld 32 s.Surface.ops)
    | "psrlq", _ -> Ok (Instruction.mk Opcode.Psrlq 32 s.Surface.ops)
    | "psraw", _ -> Ok (Instruction.mk Opcode.Psraw 32 s.Surface.ops)
    | "psrad", _ -> Ok (Instruction.mk Opcode.Psrad 32 s.Surface.ops)
    | "pslldq", _ -> Ok (Instruction.mk Opcode.Pslldq 32 s.Surface.ops)
    | "psrldq", _ -> Ok (Instruction.mk Opcode.Psrldq 32 s.Surface.ops)
    | "pshufb", _ -> Ok (Instruction.mk Opcode.Pshufb 32 s.Surface.ops)
    | "phaddw", _ -> Ok (Instruction.mk Opcode.Phaddw 32 s.Surface.ops)
    | "phaddd", _ -> Ok (Instruction.mk Opcode.Phaddd 32 s.Surface.ops)
    | "phsubw", _ -> Ok (Instruction.mk Opcode.Phsubw 32 s.Surface.ops)
    | "phsubd", _ -> Ok (Instruction.mk Opcode.Phsubd 32 s.Surface.ops)
    | "psignb", _ -> Ok (Instruction.mk Opcode.Psignb 32 s.Surface.ops)
    | "psignw", _ -> Ok (Instruction.mk Opcode.Psignw 32 s.Surface.ops)
    | "psignd", _ -> Ok (Instruction.mk Opcode.Psignd 32 s.Surface.ops)
    | "pmaddubsw", _ -> Ok (Instruction.mk Opcode.Pmaddubsw 32 s.Surface.ops)
    | "pmulhrsw", _ -> Ok (Instruction.mk Opcode.Pmulhrsw 32 s.Surface.ops)
    | "phaddsw", _ -> Ok (Instruction.mk Opcode.Phaddsw 32 s.Surface.ops)
    | "phsubsw", _ -> Ok (Instruction.mk Opcode.Phsubsw 32 s.Surface.ops)
    | "pabsb", _ -> Ok (Instruction.mk Opcode.Pabsb 32 s.Surface.ops)
    | "pabsw", _ -> Ok (Instruction.mk Opcode.Pabsw 32 s.Surface.ops)
    | "pabsd", _ -> Ok (Instruction.mk Opcode.Pabsd 32 s.Surface.ops)
    | "palignr", _ -> Ok (Instruction.mk Opcode.Palignr 32 s.Surface.ops)
    | "roundps", _ -> Ok (Instruction.mk Opcode.Roundps 32 s.Surface.ops)
    | "roundpd", _ -> Ok (Instruction.mk Opcode.Roundpd 32 s.Surface.ops)
    | "roundss", _ -> Ok (Instruction.mk Opcode.Roundss 32 s.Surface.ops)
    | "roundsd", _ -> Ok (Instruction.mk Opcode.Roundsd 32 s.Surface.ops)
    | "pcmpeqq", _ -> Ok (Instruction.mk Opcode.Pcmpeqq 32 s.Surface.ops)
    | "pcmpgtq", _ -> Ok (Instruction.mk Opcode.Pcmpgtq 32 s.Surface.ops)
    | "packusdw", _ -> Ok (Instruction.mk Opcode.Packusdw 32 s.Surface.ops)
    | "pmaxsb", _ -> Ok (Instruction.mk Opcode.Pmaxsb 32 s.Surface.ops)
    | "pmaxsd", _ -> Ok (Instruction.mk Opcode.Pmaxsd 32 s.Surface.ops)
    | "pmaxud", _ -> Ok (Instruction.mk Opcode.Pmaxud 32 s.Surface.ops)
    | "pmaxuw", _ -> Ok (Instruction.mk Opcode.Pmaxuw 32 s.Surface.ops)
    | "pminsb", _ -> Ok (Instruction.mk Opcode.Pminsb 32 s.Surface.ops)
    | "pminsd", _ -> Ok (Instruction.mk Opcode.Pminsd 32 s.Surface.ops)
    | "pminud", _ -> Ok (Instruction.mk Opcode.Pminud 32 s.Surface.ops)
    | "pminuw", _ -> Ok (Instruction.mk Opcode.Pminuw 32 s.Surface.ops)
    | "pmuldq", _ -> Ok (Instruction.mk Opcode.Pmuldq 32 s.Surface.ops)
    | "pmulld", _ -> Ok (Instruction.mk Opcode.Pmulld 32 s.Surface.ops)
    | "phminposuw", _ -> Ok (Instruction.mk Opcode.Phminposuw 32 s.Surface.ops)
    | "ptest", _ -> Ok (Instruction.mk Opcode.Ptest 32 s.Surface.ops)
    | "pmovsxbw", _ -> Ok (Instruction.mk Opcode.Pmovsxbw 32 s.Surface.ops)
    | "pmovsxbd", _ -> Ok (Instruction.mk Opcode.Pmovsxbd 32 s.Surface.ops)
    | "pmovsxbq", _ -> Ok (Instruction.mk Opcode.Pmovsxbq 32 s.Surface.ops)
    | "pmovsxwd", _ -> Ok (Instruction.mk Opcode.Pmovsxwd 32 s.Surface.ops)
    | "pmovsxwq", _ -> Ok (Instruction.mk Opcode.Pmovsxwq 32 s.Surface.ops)
    | "pmovsxdq", _ -> Ok (Instruction.mk Opcode.Pmovsxdq 32 s.Surface.ops)
    | "pmovzxbw", _ -> Ok (Instruction.mk Opcode.Pmovzxbw 32 s.Surface.ops)
    | "pmovzxbd", _ -> Ok (Instruction.mk Opcode.Pmovzxbd 32 s.Surface.ops)
    | "pmovzxbq", _ -> Ok (Instruction.mk Opcode.Pmovzxbq 32 s.Surface.ops)
    | "pmovzxwd", _ -> Ok (Instruction.mk Opcode.Pmovzxwd 32 s.Surface.ops)
    | "pmovzxwq", _ -> Ok (Instruction.mk Opcode.Pmovzxwq 32 s.Surface.ops)
    | "pmovzxdq", _ -> Ok (Instruction.mk Opcode.Pmovzxdq 32 s.Surface.ops)
    | "movntdqa", _ -> Ok (Instruction.mk Opcode.Movntdqa 32 s.Surface.ops)
    | "blendvps", _ -> Ok (Instruction.mk Opcode.Blendvps 32 s.Surface.ops)
    | "blendvpd", _ -> Ok (Instruction.mk Opcode.Blendvpd 32 s.Surface.ops)
    | "pblendvb", _ -> Ok (Instruction.mk Opcode.Pblendvb 32 s.Surface.ops)
    | "blendps", _ -> Ok (Instruction.mk Opcode.Blendps 32 s.Surface.ops)
    | "blendpd", _ -> Ok (Instruction.mk Opcode.Blendpd 32 s.Surface.ops)
    | "dpps", _ -> Ok (Instruction.mk Opcode.Dpps 32 s.Surface.ops)
    | "dppd", _ -> Ok (Instruction.mk Opcode.Dppd 32 s.Surface.ops)
    | "mpsadbw", _ -> Ok (Instruction.mk Opcode.Mpsadbw 32 s.Surface.ops)
    | "pblendw", _ -> Ok (Instruction.mk Opcode.Pblendw 32 s.Surface.ops)
    | "insertps", _ -> Ok (Instruction.mk Opcode.Insertps 32 s.Surface.ops)
    | "pinsrb", _ -> Ok (Instruction.mk Opcode.Pinsrb 32 s.Surface.ops)
    | "pinsrd", _ -> Ok (Instruction.mk Opcode.Pinsrd 32 s.Surface.ops)
    | "pextrb", _ -> Ok (Instruction.mk Opcode.Pextrb 32 s.Surface.ops)
    | "pextrd", _ -> Ok (Instruction.mk Opcode.Pextrd 32 s.Surface.ops)
    | "extractps", _ -> Ok (Instruction.mk Opcode.Extractps 32 s.Surface.ops)
    | "vpshufb", _ -> Ok (Instruction.mk Opcode.Vpshufb 32 s.Surface.ops)
    | "vphaddw", _ -> Ok (Instruction.mk Opcode.Vphaddw 32 s.Surface.ops)
    | "vphaddd", _ -> Ok (Instruction.mk Opcode.Vphaddd 32 s.Surface.ops)
    | "vphaddsw", _ -> Ok (Instruction.mk Opcode.Vphaddsw 32 s.Surface.ops)
    | "vpmaddubsw", _ -> Ok (Instruction.mk Opcode.Vpmaddubsw 32 s.Surface.ops)
    | "vphsubw", _ -> Ok (Instruction.mk Opcode.Vphsubw 32 s.Surface.ops)
    | "vphsubd", _ -> Ok (Instruction.mk Opcode.Vphsubd 32 s.Surface.ops)
    | "vphsubsw", _ -> Ok (Instruction.mk Opcode.Vphsubsw 32 s.Surface.ops)
    | "vpsignb", _ -> Ok (Instruction.mk Opcode.Vpsignb 32 s.Surface.ops)
    | "vpsignw", _ -> Ok (Instruction.mk Opcode.Vpsignw 32 s.Surface.ops)
    | "vpsignd", _ -> Ok (Instruction.mk Opcode.Vpsignd 32 s.Surface.ops)
    | "vpmulhrsw", _ -> Ok (Instruction.mk Opcode.Vpmulhrsw 32 s.Surface.ops)
    | "vpmuldq", _ -> Ok (Instruction.mk Opcode.Vpmuldq 32 s.Surface.ops)
    | "vpcmpeqq", _ -> Ok (Instruction.mk Opcode.Vpcmpeqq 32 s.Surface.ops)
    | "vpackusdw", _ -> Ok (Instruction.mk Opcode.Vpackusdw 32 s.Surface.ops)
    | "vpcmpgtq", _ -> Ok (Instruction.mk Opcode.Vpcmpgtq 32 s.Surface.ops)
    | "vpminsb", _ -> Ok (Instruction.mk Opcode.Vpminsb 32 s.Surface.ops)
    | "vpminsd", _ -> Ok (Instruction.mk Opcode.Vpminsd 32 s.Surface.ops)
    | "vpminuw", _ -> Ok (Instruction.mk Opcode.Vpminuw 32 s.Surface.ops)
    | "vpminud", _ -> Ok (Instruction.mk Opcode.Vpminud 32 s.Surface.ops)
    | "vpmaxsb", _ -> Ok (Instruction.mk Opcode.Vpmaxsb 32 s.Surface.ops)
    | "vpmaxsd", _ -> Ok (Instruction.mk Opcode.Vpmaxsd 32 s.Surface.ops)
    | "vpmaxuw", _ -> Ok (Instruction.mk Opcode.Vpmaxuw 32 s.Surface.ops)
    | "vpmaxud", _ -> Ok (Instruction.mk Opcode.Vpmaxud 32 s.Surface.ops)
    | "vpmulld", _ -> Ok (Instruction.mk Opcode.Vpmulld 32 s.Surface.ops)
    | "vpalignr", _ -> Ok (Instruction.mk Opcode.Vpalignr 32 s.Surface.ops)
    | "vblendps", _ -> Ok (Instruction.mk Opcode.Vblendps 32 s.Surface.ops)
    | "vblendpd", _ -> Ok (Instruction.mk Opcode.Vblendpd 32 s.Surface.ops)
    | "vpblendw", _ -> Ok (Instruction.mk Opcode.Vpblendw 32 s.Surface.ops)
    | "vroundss", _ -> Ok (Instruction.mk Opcode.Vroundss 32 s.Surface.ops)
    | "vroundsd", _ -> Ok (Instruction.mk Opcode.Vroundsd 32 s.Surface.ops)
    | "vdpps", _ -> Ok (Instruction.mk Opcode.Vdpps 32 s.Surface.ops)
    | "vdppd", _ -> Ok (Instruction.mk Opcode.Vdppd 32 s.Surface.ops)
    | "vmpsadbw", _ -> Ok (Instruction.mk Opcode.Vmpsadbw 32 s.Surface.ops)
    | "vinsertps", _ -> Ok (Instruction.mk Opcode.Vinsertps 32 s.Surface.ops)
    | "vpabsb", _ -> Ok (Instruction.mk Opcode.Vpabsb 32 s.Surface.ops)
    | "vpabsw", _ -> Ok (Instruction.mk Opcode.Vpabsw 32 s.Surface.ops)
    | "vpabsd", _ -> Ok (Instruction.mk Opcode.Vpabsd 32 s.Surface.ops)
    | "vphminposuw", _ -> Ok (Instruction.mk Opcode.Vphminposuw 32 s.Surface.ops)
    | "vptest", _ -> Ok (Instruction.mk Opcode.Vptest 32 s.Surface.ops)
    | "vpmovsxbw", _ -> Ok (Instruction.mk Opcode.Vpmovsxbw 32 s.Surface.ops)
    | "vpmovsxbd", _ -> Ok (Instruction.mk Opcode.Vpmovsxbd 32 s.Surface.ops)
    | "vpmovsxbq", _ -> Ok (Instruction.mk Opcode.Vpmovsxbq 32 s.Surface.ops)
    | "vpmovsxwd", _ -> Ok (Instruction.mk Opcode.Vpmovsxwd 32 s.Surface.ops)
    | "vpmovsxwq", _ -> Ok (Instruction.mk Opcode.Vpmovsxwq 32 s.Surface.ops)
    | "vpmovsxdq", _ -> Ok (Instruction.mk Opcode.Vpmovsxdq 32 s.Surface.ops)
    | "vpmovzxbw", _ -> Ok (Instruction.mk Opcode.Vpmovzxbw 32 s.Surface.ops)
    | "vpmovzxbd", _ -> Ok (Instruction.mk Opcode.Vpmovzxbd 32 s.Surface.ops)
    | "vpmovzxbq", _ -> Ok (Instruction.mk Opcode.Vpmovzxbq 32 s.Surface.ops)
    | "vpmovzxwd", _ -> Ok (Instruction.mk Opcode.Vpmovzxwd 32 s.Surface.ops)
    | "vpmovzxwq", _ -> Ok (Instruction.mk Opcode.Vpmovzxwq 32 s.Surface.ops)
    | "vpmovzxdq", _ -> Ok (Instruction.mk Opcode.Vpmovzxdq 32 s.Surface.ops)
    | "vmovntdqa", _ -> Ok (Instruction.mk Opcode.Vmovntdqa 32 s.Surface.ops)
    | "movdqa", _ -> Ok (Instruction.mk Opcode.Movdqa 32 s.Surface.ops)
    | "movdqu", _ -> Ok (Instruction.mk Opcode.Movdqu 32 s.Surface.ops)
    | "pinsrw", _ -> Ok (Instruction.mk Opcode.Pinsrw 32 s.Surface.ops)
    | "pextrw", _ -> Ok (Instruction.mk Opcode.Pextrw 32 s.Surface.ops)
    | "movmskps", _ -> Ok (Instruction.mk Opcode.Movmskps 32 s.Surface.ops)
    | "movmskpd", _ -> Ok (Instruction.mk Opcode.Movmskpd 32 s.Surface.ops)
    | "pmovmskb", _ -> Ok (Instruction.mk Opcode.Pmovmskb 32 s.Surface.ops)
    (* Packed bitwise-logical family: {!Opcode.Xorpd}'s siblings, all matched the same
       fixed-mnemonic way. *)
    | "andps", _ -> Ok (Instruction.mk Opcode.Andps 32 s.Surface.ops)
    | "andnps", _ -> Ok (Instruction.mk Opcode.Andnps 32 s.Surface.ops)
    | "orps", _ -> Ok (Instruction.mk Opcode.Orps 32 s.Surface.ops)
    | "xorps", _ -> Ok (Instruction.mk Opcode.Xorps 32 s.Surface.ops)
    | "andpd", _ -> Ok (Instruction.mk Opcode.Andpd 32 s.Surface.ops)
    | "andnpd", _ -> Ok (Instruction.mk Opcode.Andnpd 32 s.Surface.ops)
    | "orpd", _ -> Ok (Instruction.mk Opcode.Orpd 32 s.Surface.ops)
    (* {!Opcode.Movapd}'s data-movement siblings: aligned/unaligned packed move, single/double
       precision, load direction only (matching {!Opcode.Movapd}'s own precedent). *)
    | "movaps", _ -> Ok (Instruction.mk Opcode.Movaps 32 s.Surface.ops)
    | "movups", _ -> Ok (Instruction.mk Opcode.Movups 32 s.Surface.ops)
    | "movupd", _ -> Ok (Instruction.mk Opcode.Movupd 32 s.Surface.ops)
    (* {!Opcode.Addsd}'s packed-arithmetic siblings: the prefix square for opcodes
       0x58/0x59/0x5C/0x5E, no-prefix (ps) and 66-prefix (pd), the same way ANDPS/MOVAPS
       completed it for their own opcode groups. *)
    | "addps", _ -> Ok (Instruction.mk Opcode.Addps 32 s.Surface.ops)
    | "subps", _ -> Ok (Instruction.mk Opcode.Subps 32 s.Surface.ops)
    | "mulps", _ -> Ok (Instruction.mk Opcode.Mulps 32 s.Surface.ops)
    | "divps", _ -> Ok (Instruction.mk Opcode.Divps 32 s.Surface.ops)
    | "addpd", _ -> Ok (Instruction.mk Opcode.Addpd 32 s.Surface.ops)
    | "subpd", _ -> Ok (Instruction.mk Opcode.Subpd 32 s.Surface.ops)
    | "mulpd", _ -> Ok (Instruction.mk Opcode.Mulpd 32 s.Surface.ops)
    | "divpd", _ -> Ok (Instruction.mk Opcode.Divpd 32 s.Surface.ops)
    (* {!Opcode.Addsd}/{!Opcode.Addss}/{!Opcode.Addps}/{!Opcode.Addpd}'s min/max siblings
      : the same four-prefix-group shape at opcodes 0x5D (min)/0x5F (max), overlooked in
       the earlier arithmetic-family survey passes. *)
    | "maxss", _ -> Ok (Instruction.mk Opcode.Maxss 32 s.Surface.ops)
    | "minss", _ -> Ok (Instruction.mk Opcode.Minss 32 s.Surface.ops)
    | "maxsd", _ -> Ok (Instruction.mk Opcode.Maxsd 32 s.Surface.ops)
    | "minsd", _ -> Ok (Instruction.mk Opcode.Minsd 32 s.Surface.ops)
    | "maxps", _ -> Ok (Instruction.mk Opcode.Maxps 32 s.Surface.ops)
    | "minps", _ -> Ok (Instruction.mk Opcode.Minps 32 s.Surface.ops)
    | "maxpd", _ -> Ok (Instruction.mk Opcode.Maxpd 32 s.Surface.ops)
    | "minpd", _ -> Ok (Instruction.mk Opcode.Minpd 32 s.Surface.ops)
    | "sqrtss", _ -> Ok (Instruction.mk Opcode.Sqrtss 32 s.Surface.ops)
    | "sqrtsd", _ -> Ok (Instruction.mk Opcode.Sqrtsd 32 s.Surface.ops)
    | "sqrtps", _ -> Ok (Instruction.mk Opcode.Sqrtps 32 s.Surface.ops)
    | "sqrtpd", _ -> Ok (Instruction.mk Opcode.Sqrtpd 32 s.Surface.ops)
    (* [shufps]/[shufpd]: the first XMM-immediate-carrying legacy shape, [imm, rm, reg]
       in AT&T order matching [shld]'s own [imm, src, dst] operand order. *)
    | "shufps", _ -> Ok (Instruction.mk Opcode.Shufps 32 s.Surface.ops)
    | "shufpd", _ -> Ok (Instruction.mk Opcode.Shufpd 32 s.Surface.ops)
    (* [cmpss]/[cmpsd]/[cmpps]/[cmppd]: {!Opcode.Addsd}'s own four-prefix-group shape
       at opcode 0xC2, with {!Shufps}'s trailing imm8 - only the canonical [cmp{ss,sd,ps,pd}
       $imm, ...] spelling, not the [cmpeq]/[cmplt]/etc. mnemonic-suffix pseudo-aliases. *)
    | "cmpss", _ -> Ok (Instruction.mk Opcode.Cmpss 32 s.Surface.ops)
    | "cmpsd", _ -> Ok (Instruction.mk Opcode.Cmpsd 32 s.Surface.ops)
    | "cmpps", _ -> Ok (Instruction.mk Opcode.Cmpps 32 s.Surface.ops)
    | "cmppd", _ -> Ok (Instruction.mk Opcode.Cmppd 32 s.Surface.ops)
    (* [pshufd]/[pshuflw]/[pshufhw]: {!Opcode.Pshufd}'s own doc comment - the unary
       sibling of {!Opcode.Shufps}'s imm8-carrying shape, same [imm, rm, reg] AT&T order. *)
    | "pshufd", _ -> Ok (Instruction.mk Opcode.Pshufd 32 s.Surface.ops)
    | "pshuflw", _ -> Ok (Instruction.mk Opcode.Pshuflw 32 s.Surface.ops)
    | "pshufhw", _ -> Ok (Instruction.mk Opcode.Pshufhw 32 s.Surface.ops)
    (* {!Opcode.Comiss}'s own mandatory-prefix-free sibling ({!Opcode.Ucomiss}'s own doc
       comment), overlooked alongside {!Opcode.Ucomisd} when this family was first admitted. *)
    | "ucomiss", _ -> Ok (Instruction.mk Opcode.Ucomiss 32 s.Surface.ops)
    (* {!Opcode.Vaddsd}'s VEX-encoded family (x86 vector extensions): three real
       operands, [src2, src1, dst], not a suffix-bearing GPR mnemonic - matched the same
       fixed-mnemonic way as the rest of this SSE block. *)
    | "vaddsd", _ -> Ok (Instruction.mk Opcode.Vaddsd 32 s.Surface.ops)
    | "vsubsd", _ -> Ok (Instruction.mk Opcode.Vsubsd 32 s.Surface.ops)
    | "vmulsd", _ -> Ok (Instruction.mk Opcode.Vmulsd 32 s.Surface.ops)
    | "vdivsd", _ -> Ok (Instruction.mk Opcode.Vdivsd 32 s.Surface.ops)
    | "vaddss", _ -> Ok (Instruction.mk Opcode.Vaddss 32 s.Surface.ops)
    | "vsubss", _ -> Ok (Instruction.mk Opcode.Vsubss 32 s.Surface.ops)
    | "vmulss", _ -> Ok (Instruction.mk Opcode.Vmulss 32 s.Surface.ops)
    | "vdivss", _ -> Ok (Instruction.mk Opcode.Vdivss 32 s.Surface.ops)
    | "vaddps", _ -> Ok (Instruction.mk Opcode.Vaddps 32 s.Surface.ops)
    | "vsubps", _ -> Ok (Instruction.mk Opcode.Vsubps 32 s.Surface.ops)
    | "vmulps", _ -> Ok (Instruction.mk Opcode.Vmulps 32 s.Surface.ops)
    | "vdivps", _ -> Ok (Instruction.mk Opcode.Vdivps 32 s.Surface.ops)
    | "vaddpd", _ -> Ok (Instruction.mk Opcode.Vaddpd 32 s.Surface.ops)
    | "vsubpd", _ -> Ok (Instruction.mk Opcode.Vsubpd 32 s.Surface.ops)
    | "vmulpd", _ -> Ok (Instruction.mk Opcode.Vmulpd 32 s.Surface.ops)
    | "vdivpd", _ -> Ok (Instruction.mk Opcode.Vdivpd 32 s.Surface.ops)
    | "vandps", _ -> Ok (Instruction.mk Opcode.Vandps 32 s.Surface.ops)
    | "vandnps", _ -> Ok (Instruction.mk Opcode.Vandnps 32 s.Surface.ops)
    | "vorps", _ -> Ok (Instruction.mk Opcode.Vorps 32 s.Surface.ops)
    | "vxorps", _ -> Ok (Instruction.mk Opcode.Vxorps 32 s.Surface.ops)
    | "vandpd", _ -> Ok (Instruction.mk Opcode.Vandpd 32 s.Surface.ops)
    | "vandnpd", _ -> Ok (Instruction.mk Opcode.Vandnpd 32 s.Surface.ops)
    | "vorpd", _ -> Ok (Instruction.mk Opcode.Vorpd 32 s.Surface.ops)
    | "vxorpd", _ -> Ok (Instruction.mk Opcode.Vxorpd 32 s.Surface.ops)
    (* {!Opcode.Vunpcklps}'s own doc comment: the VEX sibling of the legacy
       UNPCKLPS/UNPCKHPS/UNPCKLPD/UNPCKHPD family, opcodes 0x14/0x15. *)
    | "vunpcklps", _ -> Ok (Instruction.mk Opcode.Vunpcklps 32 s.Surface.ops)
    | "vunpckhps", _ -> Ok (Instruction.mk Opcode.Vunpckhps 32 s.Surface.ops)
    | "vunpcklpd", _ -> Ok (Instruction.mk Opcode.Vunpcklpd 32 s.Surface.ops)
    | "vunpckhpd", _ -> Ok (Instruction.mk Opcode.Vunpckhpd 32 s.Surface.ops)
    (* {!Opcode.Vpunpcklqdq}'s own doc comment: the VEX sibling of the legacy
       PUNPCKLQDQ/PUNPCKHQDQ family, opcode 0x6C/0x6D, 66-mandatory-prefix only. *)
    | "vpunpcklqdq", _ -> Ok (Instruction.mk Opcode.Vpunpcklqdq 32 s.Surface.ops)
    | "vpunpckhqdq", _ -> Ok (Instruction.mk Opcode.Vpunpckhqdq 32 s.Surface.ops)
    | "vpunpcklbw", _ -> Ok (Instruction.mk Opcode.Vpunpcklbw 32 s.Surface.ops)
    | "vpunpckhbw", _ -> Ok (Instruction.mk Opcode.Vpunpckhbw 32 s.Surface.ops)
    | "vpunpcklwd", _ -> Ok (Instruction.mk Opcode.Vpunpcklwd 32 s.Surface.ops)
    | "vpunpckhwd", _ -> Ok (Instruction.mk Opcode.Vpunpckhwd 32 s.Surface.ops)
    | "vpunpckldq", _ -> Ok (Instruction.mk Opcode.Vpunpckldq 32 s.Surface.ops)
    | "vpunpckhdq", _ -> Ok (Instruction.mk Opcode.Vpunpckhdq 32 s.Surface.ops)
    (* {!Opcode.Vpaddb}'s own doc comment: the VEX sibling of the legacy
       PADDB/PADDW/PADDD/PADDQ/PSUBB/PSUBW/PSUBD/PSUBQ family. *)
    | "vpaddb", _ -> Ok (Instruction.mk Opcode.Vpaddb 32 s.Surface.ops)
    | "vpaddw", _ -> Ok (Instruction.mk Opcode.Vpaddw 32 s.Surface.ops)
    | "vpaddd", _ -> Ok (Instruction.mk Opcode.Vpaddd 32 s.Surface.ops)
    | "vpaddq", _ -> Ok (Instruction.mk Opcode.Vpaddq 32 s.Surface.ops)
    | "vpsubb", _ -> Ok (Instruction.mk Opcode.Vpsubb 32 s.Surface.ops)
    | "vpsubw", _ -> Ok (Instruction.mk Opcode.Vpsubw 32 s.Surface.ops)
    | "vpsubd", _ -> Ok (Instruction.mk Opcode.Vpsubd 32 s.Surface.ops)
    | "vpsubq", _ -> Ok (Instruction.mk Opcode.Vpsubq 32 s.Surface.ops)
    (* {!Opcode.Vpaddb}'s own doc comment: the VEX sibling of the legacy
       PCMPEQB/PCMPEQW/PCMPEQD/PCMPGTB/PCMPGTW/PCMPGTD family. *)
    | "vpcmpeqb", _ -> Ok (Instruction.mk Opcode.Vpcmpeqb 32 s.Surface.ops)
    | "vpcmpeqw", _ -> Ok (Instruction.mk Opcode.Vpcmpeqw 32 s.Surface.ops)
    | "vpcmpeqd", _ -> Ok (Instruction.mk Opcode.Vpcmpeqd 32 s.Surface.ops)
    | "vpcmpgtb", _ -> Ok (Instruction.mk Opcode.Vpcmpgtb 32 s.Surface.ops)
    | "vpcmpgtw", _ -> Ok (Instruction.mk Opcode.Vpcmpgtw 32 s.Surface.ops)
    | "vpcmpgtd", _ -> Ok (Instruction.mk Opcode.Vpcmpgtd 32 s.Surface.ops)
    (* {!Opcode.Vpaddb}'s own doc comment: the VEX sibling of the legacy
       PACKSSWB/PACKSSDW/PACKUSWB family. *)
    | "vpacksswb", _ -> Ok (Instruction.mk Opcode.Vpacksswb 32 s.Surface.ops)
    | "vpackssdw", _ -> Ok (Instruction.mk Opcode.Vpackssdw 32 s.Surface.ops)
    | "vpackuswb", _ -> Ok (Instruction.mk Opcode.Vpackuswb 32 s.Surface.ops)
    (* {!Opcode.Vpand}'s own doc comment: the VEX sibling of the legacy
       PAND/PANDN/POR family. *)
    | "vpand", _ -> Ok (Instruction.mk Opcode.Vpand 32 s.Surface.ops)
    | "vpandn", _ -> Ok (Instruction.mk Opcode.Vpandn 32 s.Surface.ops)
    | "vpor", _ -> Ok (Instruction.mk Opcode.Vpor 32 s.Surface.ops)
    (* {!Opcode.Vpminub}'s own doc comment: the VEX sibling of the legacy
       PMINUB/PMAXUB/PMINSW/PMAXSW family. *)
    | "vpminub", _ -> Ok (Instruction.mk Opcode.Vpminub 32 s.Surface.ops)
    | "vpmaxub", _ -> Ok (Instruction.mk Opcode.Vpmaxub 32 s.Surface.ops)
    | "vpminsw", _ -> Ok (Instruction.mk Opcode.Vpminsw 32 s.Surface.ops)
    | "vpmaxsw", _ -> Ok (Instruction.mk Opcode.Vpmaxsw 32 s.Surface.ops)
    (* {!Opcode.Vaddsd}/{!Opcode.Vandps}'s min/max siblings: the VEX counterpart of the
       legacy MAXSD/MINSD/MAXSS/MINSS/MAXPS/MINPS/MAXPD/MINPD family, opcodes 0x5F (max)/0x5D
       (min) instead of 0x54-0x57. *)
    | "vmaxsd", _ -> Ok (Instruction.mk Opcode.Vmaxsd 32 s.Surface.ops)
    | "vminsd", _ -> Ok (Instruction.mk Opcode.Vminsd 32 s.Surface.ops)
    | "vmaxss", _ -> Ok (Instruction.mk Opcode.Vmaxss 32 s.Surface.ops)
    | "vminss", _ -> Ok (Instruction.mk Opcode.Vminss 32 s.Surface.ops)
    | "vmaxps", _ -> Ok (Instruction.mk Opcode.Vmaxps 32 s.Surface.ops)
    | "vminps", _ -> Ok (Instruction.mk Opcode.Vminps 32 s.Surface.ops)
    | "vmaxpd", _ -> Ok (Instruction.mk Opcode.Vmaxpd 32 s.Surface.ops)
    | "vminpd", _ -> Ok (Instruction.mk Opcode.Vminpd 32 s.Surface.ops)
    (* {!Opcode.Vsqrtsd}'s own doc comment: the VEX sibling of the legacy
       SQRTSD/SQRTSS/SQRTPS/SQRTPD family, opcode 0x51. *)
    | "vsqrtsd", _ -> Ok (Instruction.mk Opcode.Vsqrtsd 32 s.Surface.ops)
    | "vsqrtss", _ -> Ok (Instruction.mk Opcode.Vsqrtss 32 s.Surface.ops)
    | "vsqrtps", _ -> Ok (Instruction.mk Opcode.Vsqrtps 32 s.Surface.ops)
    | "vsqrtpd", _ -> Ok (Instruction.mk Opcode.Vsqrtpd 32 s.Surface.ops)
    (* {!Opcode.Vmovaps}'s own doc comment: the VEX sibling of the legacy
       MOVAPS/MOVUPS/MOVAPD/MOVUPD family, opcodes 0x28/0x10. *)
    | "vmovaps", _ -> Ok (Instruction.mk Opcode.Vmovaps 32 s.Surface.ops)
    | "vmovups", _ -> Ok (Instruction.mk Opcode.Vmovups 32 s.Surface.ops)
    | "vmovapd", _ -> Ok (Instruction.mk Opcode.Vmovapd 32 s.Surface.ops)
    | "vmovupd", _ -> Ok (Instruction.mk Opcode.Vmovupd 32 s.Surface.ops)
    (* {!Opcode.Vcomisd}'s own doc comment: the VEX sibling of the legacy
       COMISD/UCOMISD/COMISS/UCOMISS family, opcodes 0x2F/0x2E. *)
    | "vcomisd", _ -> Ok (Instruction.mk Opcode.Vcomisd 32 s.Surface.ops)
    | "vucomisd", _ -> Ok (Instruction.mk Opcode.Vucomisd 32 s.Surface.ops)
    | "vcomiss", _ -> Ok (Instruction.mk Opcode.Vcomiss 32 s.Surface.ops)
    | "vucomiss", _ -> Ok (Instruction.mk Opcode.Vucomiss 32 s.Surface.ops)
    (* {!Opcode.Vcvtps2pd}'s own doc comment: the VEX sibling of the legacy
       CVTPS2PD/CVTPD2PS family, opcode 0x5A. *)
    | "vcvtps2pd", _ -> Ok (Instruction.mk Opcode.Vcvtps2pd 32 s.Surface.ops)
    | "vcvtpd2ps", _ -> Ok (Instruction.mk Opcode.Vcvtpd2ps 32 s.Surface.ops)
    | "vcvtdq2ps", _ -> Ok (Instruction.mk Opcode.Vcvtdq2ps 32 s.Surface.ops)
    | "vcvtps2dq", _ -> Ok (Instruction.mk Opcode.Vcvtps2dq 32 s.Surface.ops)
    | "vcvttps2dq", _ -> Ok (Instruction.mk Opcode.Vcvttps2dq 32 s.Surface.ops)
    (* {!Opcode.Vshufps}'s own doc comment: the VEX sibling of the legacy
       SHUFPS/SHUFPD family, opcode 0xC6, [imm, src2, src1, dst] in AT&T order. *)
    | "vshufps", _ -> Ok (Instruction.mk Opcode.Vshufps 32 s.Surface.ops)
    | "vshufpd", _ -> Ok (Instruction.mk Opcode.Vshufpd 32 s.Surface.ops)
    (* {!Opcode.Vcmpss}'s own doc comment: the VEX sibling of the legacy
       CMPSS/CMPSD/CMPPS/CMPPD family, opcode 0xC2. *)
    | "vcmpss", _ -> Ok (Instruction.mk Opcode.Vcmpss 32 s.Surface.ops)
    | "vcmpsd", _ -> Ok (Instruction.mk Opcode.Vcmpsd 32 s.Surface.ops)
    | "vcmpps", _ -> Ok (Instruction.mk Opcode.Vcmpps 32 s.Surface.ops)
    | "vcmppd", _ -> Ok (Instruction.mk Opcode.Vcmppd 32 s.Surface.ops)
    (* {!Opcode.Vpshufd}'s own doc comment: the VEX sibling of the legacy
       PSHUFD/PSHUFLW/PSHUFHW family, opcode 0x70, [imm, src, dst] - no [src1]/[vvvv] operand. *)
    | "vpshufd", _ -> Ok (Instruction.mk Opcode.Vpshufd 32 s.Surface.ops)
    | "vpshuflw", _ -> Ok (Instruction.mk Opcode.Vpshuflw 32 s.Surface.ops)
    | "vpshufhw", _ -> Ok (Instruction.mk Opcode.Vpshufhw 32 s.Surface.ops)
    (* {!Opcode.Vmovd}'s own doc comment: the VEX sibling of the legacy [movd] GPR32<->xmm
       move only - no [vmovq] ambiguity to resolve here (unlike [movq]/[mov]), since the two-byte
       VEX prefix has no REX.W-equivalent bit, so [vmovd] dispatches unconditionally. *)
    | "vmovd", _ -> Ok (Instruction.mk Opcode.Vmovd 32 s.Surface.ops)
    (* {!Opcode.Vpmullw}'s own doc comment: the VEX sibling of the legacy
       PMULLW/PMULHW/PMULHUW/PAVGB/PAVGW/PSADBW family. *)
    | "vpmullw", _ -> Ok (Instruction.mk Opcode.Vpmullw 32 s.Surface.ops)
    | "vpmulhw", _ -> Ok (Instruction.mk Opcode.Vpmulhw 32 s.Surface.ops)
    | "vpmulhuw", _ -> Ok (Instruction.mk Opcode.Vpmulhuw 32 s.Surface.ops)
    | "vpavgb", _ -> Ok (Instruction.mk Opcode.Vpavgb 32 s.Surface.ops)
    | "vpavgw", _ -> Ok (Instruction.mk Opcode.Vpavgw 32 s.Surface.ops)
    | "vpsadbw", _ -> Ok (Instruction.mk Opcode.Vpsadbw 32 s.Surface.ops)
    | "vpsllw", _ -> Ok (Instruction.mk Opcode.Vpsllw 32 s.Surface.ops)
    | "vpslld", _ -> Ok (Instruction.mk Opcode.Vpslld 32 s.Surface.ops)
    | "vpsllq", _ -> Ok (Instruction.mk Opcode.Vpsllq 32 s.Surface.ops)
    | "vpsrlw", _ -> Ok (Instruction.mk Opcode.Vpsrlw 32 s.Surface.ops)
    | "vpsrld", _ -> Ok (Instruction.mk Opcode.Vpsrld 32 s.Surface.ops)
    | "vpsrlq", _ -> Ok (Instruction.mk Opcode.Vpsrlq 32 s.Surface.ops)
    | "vpsraw", _ -> Ok (Instruction.mk Opcode.Vpsraw 32 s.Surface.ops)
    | "vpsrad", _ -> Ok (Instruction.mk Opcode.Vpsrad 32 s.Surface.ops)
    | "vpslldq", _ -> Ok (Instruction.mk Opcode.Vpslldq 32 s.Surface.ops)
    | "vpsrldq", _ -> Ok (Instruction.mk Opcode.Vpsrldq 32 s.Surface.ops)
    | "vmovdqa", _ -> Ok (Instruction.mk Opcode.Vmovdqa 32 s.Surface.ops)
    | "vmovdqu", _ -> Ok (Instruction.mk Opcode.Vmovdqu 32 s.Surface.ops)
    | "vpinsrw", _ -> Ok (Instruction.mk Opcode.Vpinsrw 32 s.Surface.ops)
    | "vpextrw", _ -> Ok (Instruction.mk Opcode.Vpextrw 32 s.Surface.ops)
    | "vmovmskps", _ -> Ok (Instruction.mk Opcode.Vmovmskps 32 s.Surface.ops)
    | "vmovmskpd", _ -> Ok (Instruction.mk Opcode.Vmovmskpd 32 s.Surface.ops)
    | "vpmovmskb", _ -> Ok (Instruction.mk Opcode.Vpmovmskb 32 s.Surface.ops)
    (* {3 x87 (M5, asm/docs/corpus.md)}

       [fldl]/[fstpl]/[fstps]: ccomp's own double/single-precision spill and
       reload around a `%st(0)` return value. Like the SSE mnemonics above,
       matched on the full name rather than [stem]/[widthed] - the `l`/`s`
       here is GAS's fixed x87 spelling for the operand's memory width, not
       a general AT&T size suffix a second one could be appended to. *)
    | m, _ when X86_x87.owns m -> (
        match Opcode.of_x87_mnemonic m with
        | Some Opcode.Fucomp when s.Surface.ops <> [] -> bad `Fucomp_takes_operands
        | Some op -> Ok (Instruction.mk op 32 (if op = Opcode.Fucomp then [] else s.Surface.ops))
        | None -> bad (`Unknown_instruction s.Surface.mnemonic))
    (* [sahf] - bare, no operand, {!Fucomp}'s exact fixed-opcode shape (M5,
       asm/docs/corpus.md - same fixture as [fnstsw]). *)
    | "sahf", _ ->
        if s.Surface.ops = [] then Ok (Instruction.mk Opcode.Sahf 32 [])
        else bad `Sahf_takes_operands
    (* M5 (asm/docs/corpus.md): zero-/sign-extending move. Matched on the full
       mnemonic via {!movx_suffixes}, not on [stem] - see its own comment for
       why [split_suffix] cannot express this shape. [movslq] (src 32, dst 64)
       is included here at the [Instruction]/[Opcode] level like any other
       [Movsx]; only {!lower_instruction} treats it differently, since [0x63]
       is a structurally distinct encoding from the [0F BE/BF] family. *)
    | m, _ when movx_suffixes ~prefix:"movz" m <> None -> (
        match movx_suffixes ~prefix:"movz" m with
        | Some (src_width, dw) -> Ok (Instruction.mk (Opcode.Movzx { src_width }) dw s.Surface.ops)
        | None -> bad (`Unknown_instruction s.Surface.mnemonic))
    | m, _ when movx_suffixes ~prefix:"movs" m <> None -> (
        match movx_suffixes ~prefix:"movs" m with
        | Some (src_width, dw) -> Ok (Instruction.mk (Opcode.Movsx { src_width }) dw s.Surface.ops)
        | None -> bad (`Unknown_instruction s.Surface.mnemonic))
    (* [cvtsi2sd]/[cvtsi2ss] take their REX.W directly from the mnemonic:
       CompCert always spells the 64-bit-source form explicitly ([cvtsi2sdq]/
       [cvtsi2ssq]), never infers it, so there is no operand to inspect here
       the way the bare-[add] case above inspects one. *)
    | "cvtsi2sd", _ -> Ok (Instruction.mk Opcode.Cvtsi2sd 32 s.Surface.ops)
    | "cvtsi2sdq", _ -> Ok (Instruction.mk Opcode.Cvtsi2sd 64 s.Surface.ops)
    | "cvtsi2ss", _ -> Ok (Instruction.mk Opcode.Cvtsi2ss 32 s.Surface.ops)
    | "cvtsi2ssq", _ -> Ok (Instruction.mk Opcode.Cvtsi2ss 64 s.Surface.ops)
    (* [cvttsd2si] has one spelling for both GPR destination widths in this
       corpus (unlike [cvtsi2sd]/[cvtsi2sdq]) - GAS disambiguates from the
       destination register alone, so this reads it the same way the bare
       [add] case above reads its register operand. A malformed operand list
       falls through to [lower_instruction]'s exhaustive match and its
       [`No_form] catch-all rather than being rejected twice; the placeholder
       width here is never used for anything besides that later, definitive
       check. *)
    | "cvttsd2si", _ ->
        let width =
          match List.rev s.Surface.ops with
          | Operand.Reg r :: _ -> r.Reg.width
          | _ -> M.address_width
        in
        Ok (Instruction.mk Opcode.Cvttsd2si width s.Surface.ops)
    (* [cvttsd2siq]: the explicit-width spelling of the same instruction, {!Cvtsi2ss}'s own
       [cvtsi2ssq] precedent just above - always paired with a 64-bit destination register in
       this corpus, so byte-identical to the bare mnemonic reading its width off that register
       (M5, asm/docs/corpus.md - gas_frontier.t's runtime-i64_dtou.S). *)
    | "cvttsd2siq", _ -> Ok (Instruction.mk Opcode.Cvttsd2si 64 s.Surface.ops)
    (* [movd]/[movq]: [movd] never collides with the generic width-suffix [mov] case
       below - 'd' is not a stripped suffix character - so it dispatches unconditionally, the
       same way [cvtsi2sd] does just above. [movq], though, IS the standard 64-bit [mov] suffix
       spelling, genuinely ambiguous between a plain 64-bit GPR move and this GPR64<->xmm move;
       real GNU as resolves it purely from the operand register classes (both accept identical
       syntax), so this project does the same - dispatch here only when an xmm operand ([width =
       128]) is actually present, and let every other [movq] fall through unchanged to [_, "mov"]
       below. {!Movd}'s own doc comment has the confirmed bytes and the deliberately-excluded
       memory-operand ambiguity. *)
    | "movd", _ -> Ok (Instruction.mk Opcode.Movd 32 s.Surface.ops)
    | "movq", _
      when match s.Surface.ops with
           | [ Operand.Reg a; Operand.Reg b ] -> a.Reg.width = 128 || b.Reg.width = 128
           | _ -> false ->
        Ok (Instruction.mk Opcode.Movd 64 s.Surface.ops)
    (* M4 (.ai/asm_plan.md §12): the real i64_udivmod.S source spells its
       one register-register [add] with no suffix at all ([add %ecx,
       %edx]) - valid GNU as, since the register operand disambiguates the
       same way it does for [cmov] below, and this is the only ALU form
       here that needs the inference: every other reg-reg use in this
       fixture (xor/cmp/sub/test/sbb) keeps an explicit suffix. Scoped to
       exactly this shape rather than broadened to every ALU op, so
       [xor]/[cmp]/[sub]/[and] keep requiring a suffix exactly as before. *)
    | _, "add" -> (
        match (suffix, s.Surface.ops) with
        | None, [ Operand.Reg a; Operand.Reg _ ] ->
            Ok (Instruction.mk Opcode.Add a.Reg.width s.Surface.ops)
        | _ -> widthed ~allow8:true Opcode.Add)
    | _, "sub" -> widthed ~allow8:true Opcode.Sub
    | _, "mov" -> widthed ~allow8:true ~allow16:true Opcode.Mov
    | _, "lea" -> widthed Opcode.Lea
    | _, "xor" -> widthed ~allow8:true Opcode.Xor
    | _, "and" -> widthed ~allow8:true Opcode.And
    | _, "cmp" -> widthed ~allow8:true Opcode.Cmp
    | _, "imul" -> widthed Opcode.Imul
    (* M4 (.ai/asm_plan.md §12): the CompCert-runtime-helper fixture's own
       measured instruction set. *)
    | _, "neg" -> widthed Opcode.Neg
    | _, "test" -> widthed Opcode.Test
    | _, "adc" -> widthed ~allow8:true Opcode.Adc
    | _, "sbb" -> widthed ~allow8:true Opcode.Sbb
    | _, "mul" -> widthed Opcode.Mul
    | _, "div" -> widthed Opcode.Div
    | _, "rcr" -> widthed Opcode.Rcr
    | _, "shr" -> widthed Opcode.Shr
    (* M5 (asm/docs/corpus.md): the x86_64 [test/c/] corpus's own measured
       instruction set. *)
    | _, "or" -> widthed ~allow8:true Opcode.Or
    | _, "not" -> widthed Opcode.Not
    | _, "ror" -> widthed Opcode.Ror
    (* GAS accepts both [shl] and [sal] for the same opcode; only [sal] is
       evidenced (CompCert's own spelling), so only that stem is recognized -
       consistent with this function's general practice of not building an
       unevidenced mnemonic alias. *)
    | _, "sal" -> widthed Opcode.Shl
    | _, "sar" -> widthed Opcode.Sar
    (* M5 (asm/docs/corpus.md): [shldl $6,%ecx,%eax], sha3.c/siphash24.c's
       64-bit-rotate idiom built from two 32-bit halves. *)
    | _, "shld" -> widthed Opcode.Shld
    (* The width comes from the operands rather than from a suffix, because
       there is no suffix to come from: GAS spells this [cmovne %%r8, %%rax] and
       rejects [cmovnel]. Both operands are registers here, so asking the first
       is asking the instruction. *)
    (* A branch mnemonic may carry GAS's form-forcing suffix - [je.d32] - which
       is a pin, not a size: it names the rung, and B10 requires it because
       canonical output has to reassemble byte-exactly. Only the two rungs this
       target has are accepted; a [jmp.d16] is a diagnostic here, in the phase
       that knows the mnemonic and has a source span, and never reaches the
       codec. *)
    | m, _ when branch_of m <> None -> (
        match branch_of m with
        | Some (op, form) -> Ok (Instruction.mk ?form op M.address_width s.Surface.ops)
        | None -> bad (`Unknown_instruction s.Surface.mnemonic))
    (* A suffix on a branch that is not one of this target's rungs is rejected
       *here*, in the phase that knows the mnemonic and has a source span, and
       never reaches the codec - which is why [codec.unknown-rung] is reserved
       for a pin that arrived through the direct-lowered path instead. Freezing
       which phase answers keeps the two diagnostics from drifting into each
       other. *)
    | m, _ when bad_branch_suffix m ->
        Error
          (diag ~pos:__POS__ ~origin:s.Surface.origin
             (`Bad_branch_suffix { mnemonic = m; rungs = branch_rungs }))
    | m, _ when Cc.split_after "cmov" m <> None -> (
        match (Cc.split_after "cmov" m, s.Surface.ops) with
        (* The width has to come from the destination (the second, always-a-register
           operand), not the first: a memory source (M5, asm/docs/corpus.md -
           gas_frontier.t's i64_smulh.S's own `cmovl 20(%esp), %eax`) carries no width
           of its own the way a register source does. *)
        | Some c, ([ Operand.Reg _; Operand.Reg r ] | [ Operand.Mem _; Operand.Reg r ]) ->
            Ok (Instruction.mk (Opcode.Cmov c) r.Reg.width s.Surface.ops)
        | Some _, _ -> bad `Cmov_operands
        | None, _ -> bad (`Unknown_instruction s.Surface.mnemonic))
    (* M5 (asm/docs/corpus.md): [sete %al], [setl %r8b] - always 8-bit, so
       unlike [cmov] there is no operand width to read; the single operand's
       own class ([`Setcc_operands] otherwise) is all that is checked here. *)
    | m, _ when Cc.split_after "set" m <> None -> (
        match (Cc.split_after "set" m, s.Surface.ops) with
        | Some c, [ ((Operand.Reg _ | Operand.Mem _) as dst) ] ->
            Ok (Instruction.mk (Opcode.Setcc c) 8 [ dst ])
        | Some _, _ -> bad `Setcc_operands
        | None, _ -> bad (`Unknown_instruction s.Surface.mnemonic))
    | _ -> bad (`Unknown_instruction s.Surface.mnemonic)

  (* {2 Lower: normalized -> lowered}

     One to one for every M1 form - x86 has no pseudo-instruction in the
     fixtures - but the signature returns a list because [lower_instruction] is
     where a pseudo would expand and a function that returned a single value
     could not be extended without changing every caller. *)

  (* An absolute memory reference by name. On x86-32 that is a bare disp32 with
     no base; on x86-64 the encodable form of the same reference is
     RIP-relative, and CompCert writes it that way, so a bare symbol there would
     not encode - loudly, at [encode], rather than as a wrong address. *)
  let mem_of_symbol e =
    {
      Mem.base = (if M.rex_allowed then Some rip_reg else None);
      index = None;
      scale = 1;
      disp = Disp.Sym e;
    }

  (* Which operand kinds an x87 memory-form mnemonic takes, from {!X86_x87}. [fadds]/[fsubs]
     currently accept only a bare symbol, not a register-indirect operand: that is a recorded
     gap in the implemented subset, kept as-is so the extraction changes no behavior. *)
  let x87_shape op =
    Option.map (fun (f : X86_x87.memory_form) -> f.shape) (X86_x87.find_memory (Opcode.name op))

  let x87_memory_op op =
    match x87_shape op with Some (X86_x87.Memory | Memory_or_symbol) -> true | _ -> false

  let x87_symbol_op op =
    match x87_shape op with Some (X86_x87.Symbol | Memory_or_symbol) -> true | _ -> false

  let lower_hand_written i =
    let bad kind = Error (diag ~pos:__POS__ kind) in
    let imm_of v =
      match Bigint.to_int64_opt v with
      | Some x -> Ok x
      | None -> Error (diag ~pos:__POS__ `Immediate_too_wide)
    in
    let width_ok (r : Reg.t) =
      if r.width = i.Instruction.width then Ok ()
      else
        bad
          (`Register_width_mismatch
             { reg = r.name; reg_width = r.width; insn_width = i.Instruction.width })
    in
    (* xmm-side of the SSE operand-class check (M5, asm/docs/corpus.md); see
       {!sse_operand_class_mismatch}'s comment for why the GPR side reuses
       [width_ok] instead of a mirrored helper here. *)
    let xmm_ok (r : Reg.t) =
      if r.width = 128 then Ok ()
      else bad (`Sse_operand_class { sse_reg = r.name; sse_reg_width = r.width })
    in
    (* The 256-bit ([ymm], VEX.L = 1) counterpart of {!xmm_ok}, and the per-mnemonic gate on which
       VEX instructions have a 256-bit form at all: only packed-float binops and the packed
       moves/square roots are admitted here, so a [ymm] operand on any other mnemonic (scalar
       [vaddsd], the integer group that needs AVX2, ...) still falls through to {!xmm_ok}'s error
       rather than mis-encoding. *)
    let ymm_ok (r : Reg.t) =
      if r.width = 256 then Ok ()
      else bad (`Sse_operand_class { sse_reg = r.name; sse_reg_width = r.width })
    in
    let zmm_ok (r : Reg.t) =
      if r.width = 512 then Ok ()
      else bad (`Sse_operand_class { sse_reg = r.name; sse_reg_width = r.width })
    in
    (* EVEX (AVX-512F, {!Opcode.Vaddps}'s own doc comment): only the register-register spelling of
       the packed-float binops with an EVEX form is admitted at 512 bits, unmasked. *)
    let evex512_binop = function
      | Opcode.Vaddps | Opcode.Vsubps | Opcode.Vmulps | Opcode.Vdivps | Opcode.Vmaxps
      | Opcode.Vminps | Opcode.Vunpcklps | Opcode.Vunpckhps | Opcode.Vaddpd | Opcode.Vsubpd
      | Opcode.Vmulpd | Opcode.Vdivpd | Opcode.Vmaxpd | Opcode.Vminpd | Opcode.Vunpcklpd
      | Opcode.Vunpckhpd ->
          true
      | _ -> false
    in

    let vex256_binop = function
      | Opcode.Vaddps | Opcode.Vsubps | Opcode.Vmulps | Opcode.Vdivps | Opcode.Vandps
      | Opcode.Vandnps | Opcode.Vorps | Opcode.Vxorps | Opcode.Vmaxps | Opcode.Vminps
      | Opcode.Vunpcklps | Opcode.Vunpckhps | Opcode.Vaddpd | Opcode.Vsubpd | Opcode.Vmulpd
      | Opcode.Vdivpd | Opcode.Vandpd | Opcode.Vandnpd | Opcode.Vorpd | Opcode.Vxorpd
      | Opcode.Vmaxpd | Opcode.Vminpd | Opcode.Vunpcklpd | Opcode.Vunpckhpd | Opcode.Vpunpcklqdq
      | Opcode.Vpunpckhqdq | Opcode.Vpunpcklbw | Opcode.Vpunpckhbw | Opcode.Vpunpcklwd
      | Opcode.Vpunpckhwd | Opcode.Vpunpckldq | Opcode.Vpunpckhdq | Opcode.Vpaddb | Opcode.Vpaddw
      | Opcode.Vpaddd | Opcode.Vpaddq | Opcode.Vpsubb | Opcode.Vpsubw | Opcode.Vpsubd
      | Opcode.Vpsubq | Opcode.Vpcmpeqb | Opcode.Vpcmpeqw | Opcode.Vpcmpeqd | Opcode.Vpcmpgtb
      | Opcode.Vpcmpgtw | Opcode.Vpcmpgtd | Opcode.Vpacksswb | Opcode.Vpackssdw | Opcode.Vpackuswb
      | Opcode.Vpand | Opcode.Vpandn | Opcode.Vpor | Opcode.Vpminub | Opcode.Vpmaxub
      | Opcode.Vpminsw | Opcode.Vpmaxsw | Opcode.Vpmullw | Opcode.Vpmulhw | Opcode.Vpmulhuw
      | Opcode.Vpavgb | Opcode.Vpavgw | Opcode.Vpsadbw | Opcode.Vpshufb | Opcode.Vphaddw
      | Opcode.Vphaddd | Opcode.Vphaddsw | Opcode.Vpmaddubsw | Opcode.Vphsubw | Opcode.Vphsubd
      | Opcode.Vphsubsw | Opcode.Vpsignb | Opcode.Vpsignw | Opcode.Vpsignd | Opcode.Vpmulhrsw
      | Opcode.Vpmuldq | Opcode.Vpcmpeqq | Opcode.Vpackusdw | Opcode.Vpcmpgtq | Opcode.Vpminsb
      | Opcode.Vpminsd | Opcode.Vpminuw | Opcode.Vpminud | Opcode.Vpmaxsb | Opcode.Vpmaxsd
      | Opcode.Vpmaxuw | Opcode.Vpmaxud | Opcode.Vpmulld ->
          true
      | _ -> false
    in
    let vex256_unop = function
      | Opcode.Vsqrtps | Opcode.Vsqrtpd | Opcode.Vmovaps | Opcode.Vmovups | Opcode.Vmovapd
      | Opcode.Vmovupd | Opcode.Vmovdqa | Opcode.Vmovdqu | Opcode.Vpabsb | Opcode.Vpabsw
      | Opcode.Vpabsd | Opcode.Vptest | Opcode.Vmovntdqa ->
          true
      | _ -> false
    in
    (* The two-byte-VEX memory-operand counterpart of {!Vex_binop_rr_rm}'s
       own register-side [src2.num >= 8] check: a RIP base ([num = -1]) never
       consumes the restricted 3-bit field, so it always passes here without
       a special case. *)
    let vex_mem_ok (m : Mem.t) =
      let reg_ok (r : Reg.t) =
        if r.num < 8 then Ok () else bad (`Vex_rm_extended_register r.name)
      in
      match (m.Mem.base, m.Mem.index) with
      | Some b, Some idx -> ( match reg_ok b with Ok () -> reg_ok idx | Error e -> Error e)
      | Some b, None -> reg_ok b
      | None, Some idx -> reg_ok idx
      | None, None -> Ok ()
    in
    match (i.Instruction.op, i.Instruction.ops) with
    | ( ( Opcode.Add | Opcode.Adc | Opcode.And | Opcode.Sub | Opcode.Cmp | Opcode.Or | Opcode.Xor
        | Opcode.Sbb ),
        [ Operand.Imm v; dst ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            let ext = Opcode.to_ext i.Instruction.op in
            match dst with
            | Operand.Reg r -> (
                match width_ok r with
                | Error e -> Error e
                | Ok () ->
                    Ok
                      [
                        Lowered.Alu_rm_imm
                          { ext; width = i.Instruction.width; rm = Rm.Reg r; imm = Disp.Const imm };
                      ])
            | Operand.Mem m ->
                Ok
                  [
                    Lowered.Alu_rm_imm
                      { ext; width = i.Instruction.width; rm = Rm.Mem m; imm = Disp.Const imm };
                  ]
            | Operand.Imm _ | Operand.Imm_sym _ | Operand.Sym _ | Operand.Rc _ | Operand.Masked _
            | Operand.Dfv _ ->
                bad `Immediate_destination))
    (* [addq $bodies+24, %rax] - gcc's idiom for address arithmetic against a
       symbol's own address rather than through [lea] (M5, asm/docs/corpus.md).
       Register destination only - no fixture evidences a symbolic-immediate
       memory destination for an ALU op. *)
    | ( (Opcode.Add | Opcode.Adc | Opcode.And | Opcode.Sub | Opcode.Cmp | Opcode.Or | Opcode.Xor),
        [ Operand.Imm_sym e; Operand.Reg r ] ) -> (
        match width_ok r with
        | Error e -> Error e
        | Ok () ->
            let ext = Opcode.to_ext i.Instruction.op in
            Ok
              [
                Lowered.Alu_rm_imm
                  { ext; width = i.Instruction.width; rm = Rm.Reg r; imm = Disp.Sym e };
              ])
    | Opcode.Mov, [ Operand.Imm v; Operand.Mem m ] -> (
        if i.Instruction.width <> 8 then bad `Imm_to_mem_only_movb
        else
          match imm_of v with
          | Error e -> Error e
          | Ok imm -> Ok [ Lowered.Mov_rm_imm { width = 8; rm = Rm.Mem m; imm } ])
    | Opcode.Mov, [ Operand.Imm v; Operand.Reg r ] -> (
        match (imm_of v, width_ok r) with
        | Ok imm, Ok () ->
            Ok [ Lowered.Mov_r_imm { width = i.Instruction.width; reg = r; imm = Disp.Const imm } ]
        | Error e, _ | _, Error e -> Error e)
    (* [movl $.LC0, %edi] - gcc's idiom for materializing a string/array
       address into a register (M5, asm/docs/corpus.md), rather than through
       [lea] against a memory operand the way ccomp's own codegen always did. *)
    | Opcode.Mov, [ Operand.Imm_sym e; Operand.Reg r ] -> (
        match width_ok r with
        | Error e -> Error e
        | Ok () ->
            Ok [ Lowered.Mov_r_imm { width = i.Instruction.width; reg = r; imm = Disp.Sym e } ])
    | Opcode.Mov, [ Operand.Reg r; Operand.Mem m ] -> (
        match width_ok r with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Mov_rm_r { width = i.Instruction.width; rm = Rm.Mem m; reg = r } ])
    | Opcode.Mov, [ Operand.Reg a; Operand.Reg b ] -> (
        match (width_ok a, width_ok b) with
        | Ok (), Ok () ->
            Ok [ Lowered.Mov_rm_r { width = i.Instruction.width; rm = Rm.Reg b; reg = a } ]
        | Error e, _ | _, Error e -> Error e)
    (* [movl asm_test_global, %eax] loads *from* that address: in AT&T a bare
       symbol operand is memory and [$sym] is the address. The opcode is what
       decides - the same [Operand.Sym] after a [call] is a branch target - so
       the reading belongs here and not in the parser. *)
    | Opcode.Mov, [ Operand.Sym e; Operand.Reg r ] -> (
        match width_ok r with
        | Error e2 -> Error e2
        | Ok () ->
            Ok
              [
                Lowered.Mov_r_rm
                  { width = i.Instruction.width; reg = r; rm = Rm.Mem (mem_of_symbol e) };
              ])
    | Opcode.Mov, [ Operand.Reg r; Operand.Sym e ] -> (
        match width_ok r with
        | Error e2 -> Error e2
        | Ok () ->
            Ok
              [
                Lowered.Mov_rm_r
                  { width = i.Instruction.width; rm = Rm.Mem (mem_of_symbol e); reg = r };
              ])
    | Opcode.Mov, [ Operand.Mem m; Operand.Reg r ] -> (
        match width_ok r with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Mov_r_rm { width = i.Instruction.width; reg = r; rm = Rm.Mem m } ])
    | Opcode.Lea, [ Operand.Mem m; Operand.Reg r ] -> (
        match width_ok r with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Lea { width = i.Instruction.width; reg = r; mem = m } ])
    (* [leal sym, %reg]: a bare symbol operand, same duality as [Mov] above -
       reuse [mem_of_symbol] rather than a second [Lea] constructor. *)
    | Opcode.Lea, [ Operand.Sym e; Operand.Reg r ] -> (
        match width_ok r with
        | Error e2 -> Error e2
        | Ok () ->
            Ok [ Lowered.Lea { width = i.Instruction.width; reg = r; mem = mem_of_symbol e } ])
    (* M4 (.ai/asm_plan.md §12): [Add]/[Test]/[Sbb] join the pre-existing three
       here for the same reason they joined {!Opcode.to_rm_r} - real bytes
       the i64_divmod runtime-helper fixture measurably selects
       ([addl %ecx,%edx], [testl %esi,%esi], [sbbl %ecx,%edx]). [Adc] (M5,
       asm/docs/corpus.md - siphash24.c's [adcl %ecx,%edx], the carry half of
       its 64-bit add) joins them too, for the same reason it joined
       {!Opcode.to_rm_r} above. *)
    | ( ( Opcode.Xor | Opcode.Cmp | Opcode.Sub | Opcode.Add | Opcode.Adc | Opcode.Test | Opcode.Sbb
        | Opcode.Or | Opcode.And ),
        [ Operand.Reg a; Operand.Reg b ] )
    (* no byte-width opcode is built here: a [b]-suffixed form is a generated row's *)
      when i.Instruction.width <> 8 -> (
        match (width_ok a, width_ok b) with
        | Ok (), Ok () ->
            Ok
              [
                Lowered.Alu_rm_r
                  { op = i.Instruction.op; width = i.Instruction.width; rm = Rm.Reg b; reg = a };
              ]
        | Error e, _ | _, Error e -> Error e)
    (* The rm<-reg ALU direction with a genuine memory destination
       ([addl %eax, 0x10(%esp)]): {!Opcode.to_rm_r}'s
       own opcode table already covers this - a memory r/m and a register
       r/m share one opcode per operation, distinguished only by ModR/M's
       mod field, not by a second table - so this reuses the arm above's own
       [Lowered.Alu_rm_r] constructor with [rm = Rm.Mem m] rather than
       needing a new one. [Test] joins the seven read-modify-write ops here
       even though it never writes its destination, matching real GNU as:
       [testl %eax, 0x10(%esp)] -> [85 44 24 10], the same opcode
       [TEST_GPRv_GPRv] already uses. *)
    | ( ( Opcode.Xor | Opcode.Cmp | Opcode.Sub | Opcode.Add | Opcode.Adc | Opcode.Test | Opcode.Sbb
        | Opcode.Or | Opcode.And ),
        [ Operand.Reg r; Operand.Mem m ] )
      when i.Instruction.width <> 8 -> (
        match width_ok r with
        | Error e -> Error e
        | Ok () ->
            Ok
              [
                Lowered.Alu_rm_r
                  { op = i.Instruction.op; width = i.Instruction.width; rm = Rm.Mem m; reg = r };
              ])
    (* The reg<-rm ALU direction: {!Opcode.to_r_rm}'s mirror image of the form
       above. Only measured with a memory source ([adcl 0x20(%esp),%edx],
       [add 0x1c(%esp),%eax]) - a register-register source would also be
       valid x86, but nothing here selects it, so it is not built. [Sub]/
       [And]/[Or]/[Sbb]/[Cmp] join [Adc]/[Add]/
       [Xor] here for the same reason they joined {!Opcode.to_r_rm} above. *)
    | ( ( Opcode.Adc | Opcode.Add | Opcode.Xor | Opcode.Sub | Opcode.And | Opcode.Or | Opcode.Sbb
        | Opcode.Cmp ),
        [ Operand.Mem m; Operand.Reg r ] )
      when i.Instruction.width <> 8 -> (
        match width_ok r with
        | Error e -> Error e
        | Ok () ->
            Ok
              [
                Lowered.Alu_r_rm
                  { op = i.Instruction.op; width = i.Instruction.width; reg = r; rm = Rm.Mem m };
              ])
    (* Group-3 unary forms: one r/m operand, register or memory
       ([negl 0x10(%esp)], [neg %esi], [mull %esi], [divl %ecx]). *)
    | (Opcode.Neg | Opcode.Mul | Opcode.Div | Opcode.Not), [ dst ] -> (
        let ext = Opcode.to_unary_ext i.Instruction.op in
        match dst with
        | Operand.Reg r -> (
            match width_ok r with
            | Error e -> Error e
            | Ok () -> Ok [ Lowered.Unary_rm { ext; width = i.Instruction.width; rm = Rm.Reg r } ])
        | Operand.Mem m ->
            Ok [ Lowered.Unary_rm { ext; width = i.Instruction.width; rm = Rm.Mem m } ]
        | Operand.Imm _ | Operand.Imm_sym _ | Operand.Sym _ | Operand.Rc _ | Operand.Masked _
        | Operand.Dfv _ ->
            bad `Immediate_destination)
    (* Group-2 shift/rotate, bare-mnemonic implicit-1 form ([shrq %rax]) - GAS's
       own shorter surface spelling of the explicit [$1, dst] one just below,
       byte-identical either way (M5, asm/docs/corpus.md - gas_frontier.t's
       runtime-i64_utod.S/i64_utof.S). Lowers straight into the same
       {!Lowered.Shift1_rm} the explicit-count-1 case builds. *)
    | (Opcode.Rcr | Opcode.Shr | Opcode.Ror | Opcode.Shl | Opcode.Sar), [ dst ] -> (
        let ext = Opcode.to_shift1_ext i.Instruction.op in
        match dst with
        | Operand.Reg r -> (
            match width_ok r with
            | Error e -> Error e
            | Ok () -> Ok [ Lowered.Shift1_rm { ext; width = i.Instruction.width; rm = Rm.Reg r } ])
        | Operand.Mem m ->
            Ok [ Lowered.Shift1_rm { ext; width = i.Instruction.width; rm = Rm.Mem m } ]
        | Operand.Imm _ | Operand.Imm_sym _ | Operand.Sym _ | Operand.Rc _ | Operand.Masked _
        | Operand.Dfv _ ->
            bad `Immediate_destination)
    (* Group-2 shift/rotate, explicit-count form. A literal count of exactly 1
       still picks {!Lowered.Shift1_rm} - GAS's own shorter, canonical
       encoding (M4's original scope here) - and any other count is
       {!Lowered.Shift_imm_rm} (M5, asm/docs/corpus.md: [rorl $27,%eax],
       [sall $16,%eax], [sarl $2,%eax], [shrq $63,%rax]). Register
       destination only for a non-1 count: unlike {!Alu_rm_imm}, no fixture
       selects a memory destination at a count other than 1, so that shape
       is not built. *)
    | (Opcode.Rcr | Opcode.Shr | Opcode.Ror | Opcode.Shl | Opcode.Sar), [ Operand.Imm v; dst ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            let ext = Opcode.to_shift1_ext i.Instruction.op in
            match (imm, dst) with
            | 1L, Operand.Reg r -> (
                match width_ok r with
                | Error e -> Error e
                | Ok () ->
                    Ok [ Lowered.Shift1_rm { ext; width = i.Instruction.width; rm = Rm.Reg r } ])
            | 1L, Operand.Mem m ->
                Ok [ Lowered.Shift1_rm { ext; width = i.Instruction.width; rm = Rm.Mem m } ]
            | _, Operand.Reg r -> (
                match width_ok r with
                | Error e -> Error e
                | Ok () ->
                    Ok
                      [
                        Lowered.Shift_imm_rm
                          { ext; width = i.Instruction.width; rm = Rm.Reg r; imm };
                      ])
            | _, Operand.Mem _ -> bad (`No_form (Opcode.name i.Instruction.op))
            | ( _,
                ( Operand.Imm _ | Operand.Imm_sym _ | Operand.Sym _ | Operand.Rc _
                | Operand.Masked _ | Operand.Dfv _ ) ) ->
                bad `Immediate_destination))
    (* Group-2 shift/rotate, count-in-%cl (M5, asm/docs/corpus.md: [sall
       %cl,%eax]). [cl]'s width and number pin it to exactly %cl, not any
       other byte register - GAS accepts no other register here, and this
       target's parser has no separate "the count register" operand class to
       enforce it earlier. Register destination only, for the same reason as
       the immediate-count form above. *)
    | (Opcode.Rcr | Opcode.Shr | Opcode.Ror | Opcode.Shl | Opcode.Sar), [ Operand.Reg cl; dst ]
      when cl.Reg.width = 8 && cl.Reg.num = 1 -> (
        let ext = Opcode.to_shift1_ext i.Instruction.op in
        match dst with
        | Operand.Reg r -> (
            match width_ok r with
            | Error e -> Error e
            | Ok () ->
                Ok [ Lowered.Shift_cl_rm { ext; width = i.Instruction.width; rm = Rm.Reg r } ])
        | Operand.Mem _ -> bad (`No_form (Opcode.name i.Instruction.op))
        | Operand.Imm _ | Operand.Imm_sym _ | Operand.Sym _ | Operand.Rc _ | Operand.Masked _
        | Operand.Dfv _ ->
            bad `Immediate_destination)
    (* [shldl $6,%ecx,%eax] (M5, asm/docs/corpus.md): SHLD's own three-operand
       AT&T form - GAS reverses Intel's [SHLD r/m32, r32, imm8] to put the
       count first and the r/m destination last, exactly the order
       [parse_one_operand] already builds. Register destination only; no
       fixture selects a memory one. *)
    | Opcode.Shld, [ Operand.Imm v; Operand.Reg src; Operand.Reg dst ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (width_ok src, width_ok dst) with
            | Ok (), Ok () ->
                Ok
                  [
                    Lowered.Shld_imm_rm
                      { width = i.Instruction.width; reg = src; rm = Rm.Reg dst; imm };
                  ]
            | Error e, _ | _, Error e -> Error e))
    (* [shufps $imm8, rm, reg]/[shufpd $imm8, rm, reg]: {!Opcode.Shld}'s own
       [imm, src, dst] operand order just above, reused for [Sse_binop_imm_r_rm]'s
       [imm, rm, reg] - GAS's [parse_one_operand] builds the same order for any instruction
       whose immediate comes first. *)
    | ( ( Opcode.Shufps | Opcode.Shufpd | Opcode.Cmpss | Opcode.Cmpsd | Opcode.Cmpps | Opcode.Cmppd
        | Opcode.Pshufd | Opcode.Pshuflw | Opcode.Pshufhw | Opcode.Palignr | Opcode.Roundps
        | Opcode.Roundpd | Opcode.Roundss | Opcode.Roundsd | Opcode.Blendps | Opcode.Blendpd
        | Opcode.Dpps | Opcode.Dppd | Opcode.Mpsadbw | Opcode.Pblendw | Opcode.Insertps ),
        [ Operand.Imm v; Operand.Reg src; Operand.Reg reg ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (xmm_ok src, xmm_ok reg) with
            | Ok (), Ok () ->
                Ok
                  [
                    Lowered.Sse_binop_imm_r_rm { op = i.Instruction.op; reg; rm = Rm.Reg src; imm };
                  ]
            | Error e, _ | _, Error e -> Error e))
    | ( ( Opcode.Shufps | Opcode.Shufpd | Opcode.Cmpss | Opcode.Cmpsd | Opcode.Cmpps | Opcode.Cmppd
        | Opcode.Pshufd | Opcode.Pshuflw | Opcode.Pshufhw | Opcode.Palignr | Opcode.Roundps
        | Opcode.Roundpd | Opcode.Roundss | Opcode.Roundsd | Opcode.Blendps | Opcode.Blendpd
        | Opcode.Dpps | Opcode.Dppd | Opcode.Mpsadbw | Opcode.Pblendw | Opcode.Insertps ),
        [ Operand.Imm v; Operand.Mem m; Operand.Reg reg ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match xmm_ok reg with
            | Error e -> Error e
            | Ok () ->
                Ok [ Lowered.Sse_binop_imm_r_rm { op = i.Instruction.op; reg; rm = Rm.Mem m; imm } ]
            ))
    | Opcode.Push, [ Operand.Reg r ] -> Ok [ Lowered.Push { reg = r } ]
    (* [pushl $sym] (M5, asm/docs/corpus.md), gcc's own idiom for materializing
       a symbol's address on the stack: {!Push_imm}'s [imm] carries it as a
       {!Disp.t} the same way {!Mov_r_imm}'s and {!Alu_rm_imm}'s already do. *)
    | Opcode.Push, [ Operand.Imm v ] -> (
        match imm_of v with
        | Ok imm -> Ok [ Lowered.Push_imm { imm = Disp.Const imm } ]
        | Error e -> Error e)
    | Opcode.Push, [ Operand.Imm_sym e ] -> Ok [ Lowered.Push_imm { imm = Disp.Sym e } ]
    | Opcode.Dec, [ Operand.Reg r ] -> Ok [ Lowered.Dec { reg = r } ]
    (* The operands swap sides relative to the ALU forms above: AT&T [imull
       %%esi, %%ecx] writes %%ecx, and the register field of an [0f af] is the
       destination rather than the source. *)
    | Opcode.Imul, [ Operand.Reg a; Operand.Reg b ] -> (
        match (width_ok a, width_ok b) with
        | Ok (), Ok () ->
            Ok [ Lowered.Imul_r_rm { width = i.Instruction.width; reg = b; rm = Rm.Reg a } ]
        | Error e, _ | _, Error e -> Error e)
    (* [imull $10000,%ebx] / [imulq $56,%rax]: GAS's two-operand form, same
       register read as [rm] and written as [reg] - not a genuine
       [imul $imm,src,dst] with independent operands, since nothing here
       selects that three-operand shape. *)
    | Opcode.Imul, [ Operand.Imm v; Operand.Reg r ] -> (
        match (imm_of v, width_ok r) with
        | Ok imm, Ok () ->
            Ok
              [ Lowered.Imul_r_rm_imm { width = i.Instruction.width; reg = r; rm = Rm.Reg r; imm } ]
        | Error e, _ | _, Error e -> Error e)
    (* [testl $1,%edi] (M5, asm/docs/corpus.md): TEST's own immediate form,
       disjoint from the [reg,reg] pattern above. *)
    | Opcode.Test, [ Operand.Imm v; dst ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match dst with
            (* GNU as encodes an accumulator destination with the short 0xA8/0xA9 form, which
               is a generated row's, not this one's *)
            | Operand.Reg r when r.num = 0 -> bad (`No_form "test")
            | Operand.Reg r -> (
                match width_ok r with
                | Error e -> Error e
                | Ok () ->
                    Ok [ Lowered.Test_rm_imm { width = i.Instruction.width; rm = Rm.Reg r; imm } ])
            | Operand.Mem m ->
                Ok [ Lowered.Test_rm_imm { width = i.Instruction.width; rm = Rm.Mem m; imm } ]
            | Operand.Imm _ | Operand.Imm_sym _ | Operand.Sym _ | Operand.Rc _ | Operand.Masked _
            | Operand.Dfv _ ->
                bad `Immediate_destination))
    | Opcode.Cmov cc, [ Operand.Reg a; Operand.Reg b ] -> (
        match (width_ok a, width_ok b) with
        | Ok (), Ok () ->
            Ok [ Lowered.Cmov_r_rm { cc; width = i.Instruction.width; reg = b; rm = Rm.Reg a } ]
        | Error e, _ | _, Error e -> Error e)
    (* [cmovl 20(%esp), %eax] (M5, asm/docs/corpus.md - gas_frontier.t's i64_smulh.S):
       {!Lowered.Cmov_r_rm}'s [rm] is already a general {!Rm.t} - this is the same
       shape as the register-register form above, just [Rm.Mem] instead of [Rm.Reg]. *)
    | Opcode.Cmov cc, [ Operand.Mem m; Operand.Reg b ] -> (
        match width_ok b with
        | Error e -> Error e
        | Ok () ->
            Ok [ Lowered.Cmov_r_rm { cc; width = i.Instruction.width; reg = b; rm = Rm.Mem m } ])
    | Opcode.Ret, [] -> Ok [ Lowered.Ret ]
    | Opcode.Ud2, [] -> Ok [ Lowered.Ud2 ]
    | Opcode.Pop, [ Operand.Reg r ] -> Ok [ Lowered.Pop { reg = r } ]
    | Opcode.Jmp, [ Operand.Reg r ] -> Ok [ Lowered.Jmp_rm { rm = Rm.Reg r } ]
    (* [jmp *sym(,%reg,scale)] - an indirect jump through a jump-table entry
       (M5 corpus, asm/docs/corpus.md: siphash24.c/vmach.c's [switch] dispatch).
       [Jmp_rm] is already generic over [Rm.t] - the [jmp-rm] codec alt encodes
       whatever ModR/M+SIB [rm] carries - so this needs no new lowered form or
       codec, only the match arm the [Reg] case above never needed a [Mem]
       sibling for until this corpus evidence. *)
    | Opcode.Jmp, [ Operand.Mem m ] -> Ok [ Lowered.Jmp_rm { rm = Rm.Mem m } ]
    (* Symbolic and unpinned: the distance is not known here, so the encoder
       offers what it can and layout decides. A [call] has only one rung today,
       which is why it still reaches the byte gate as a [`Fixed] form. *)
    | Opcode.Call, [ Operand.Sym e ] ->
        Ok
          [ Lowered.Call_rel { target = Asm_core.Lowered_ast.Symbolic { value = e; rung = None } } ]
    (* The pin travels from the mnemonic into the operand and stops there.
       Lowering does not decide fixed-versus-relaxable - [encode] does, on
       exactly this field - which keeps the layering the same as everywhere
       else: this stage says what the instruction is, not how long it will be. *)
    | Opcode.Jmp, [ Operand.Sym e ] ->
        Ok
          [
            Lowered.Jmp_rel
              { target = Asm_core.Lowered_ast.Symbolic { value = e; rung = i.Instruction.form } };
          ]
    | Opcode.Jcc cc, [ Operand.Sym e ] ->
        Ok
          [
            Lowered.Jcc_rel
              {
                cc;
                target = Asm_core.Lowered_ast.Symbolic { value = e; rung = i.Instruction.form };
              };
          ]
    (* {3 SSE2 scalar float (M5, asm/docs/corpus.md)}

       [reg] is always the destination and always xmm for the binop/mov-load
       family, matching every shape this corpus evidences; a register [rm]
       must be xmm too ({!xmm_ok}), and a memory [rm] needs no check here -
       {!x86_family.ml}'s memory-operand parser already refuses an xmm
       register as a [Mem.t] base or index, so an [Operand.Mem] arriving here
       is already guaranteed clean. Register-register [movsd]/[movss] is not
       built: unevidenced by this corpus, and unlike plain [mov] (which has a
       real fixture pinning its reg-reg direction to the store opcode) there
       is nothing here to check that choice against. *)
    | ( ( Opcode.Addsd | Opcode.Subsd | Opcode.Mulsd | Opcode.Divsd | Opcode.Addss | Opcode.Subss
        | Opcode.Mulss | Opcode.Divss | Opcode.Comisd | Opcode.Ucomisd | Opcode.Comiss
        | Opcode.Ucomiss | Opcode.Xorpd | Opcode.Pxor | Opcode.Movapd | Opcode.Cvtsd2ss
        | Opcode.Cvtss2sd | Opcode.Cvtps2pd | Opcode.Cvtpd2ps | Opcode.Cvtdq2ps | Opcode.Cvtps2dq
        | Opcode.Cvttps2dq | Opcode.Andps | Opcode.Andnps | Opcode.Orps | Opcode.Xorps
        | Opcode.Andpd | Opcode.Andnpd | Opcode.Orpd | Opcode.Movaps | Opcode.Movups | Opcode.Movupd
        | Opcode.Addps | Opcode.Subps | Opcode.Mulps | Opcode.Divps | Opcode.Addpd | Opcode.Subpd
        | Opcode.Mulpd | Opcode.Divpd | Opcode.Maxss | Opcode.Minss | Opcode.Maxsd | Opcode.Minsd
        | Opcode.Maxps | Opcode.Minps | Opcode.Maxpd | Opcode.Minpd | Opcode.Sqrtss | Opcode.Sqrtsd
        | Opcode.Sqrtps | Opcode.Sqrtpd | Opcode.Unpcklps | Opcode.Unpckhps | Opcode.Unpcklpd
        | Opcode.Unpckhpd | Opcode.Punpcklqdq | Opcode.Punpckhqdq | Opcode.Punpcklbw
        | Opcode.Punpckhbw | Opcode.Punpcklwd | Opcode.Punpckhwd | Opcode.Punpckldq
        | Opcode.Punpckhdq | Opcode.Paddb | Opcode.Paddw | Opcode.Paddd | Opcode.Paddq
        | Opcode.Psubb | Opcode.Psubw | Opcode.Psubd | Opcode.Psubq | Opcode.Pcmpeqb
        | Opcode.Pcmpeqw | Opcode.Pcmpeqd | Opcode.Pcmpgtb | Opcode.Pcmpgtw | Opcode.Pcmpgtd
        | Opcode.Packsswb | Opcode.Packssdw | Opcode.Packuswb | Opcode.Pand | Opcode.Pandn
        | Opcode.Por | Opcode.Pminub | Opcode.Pmaxub | Opcode.Pminsw | Opcode.Pmaxsw | Opcode.Pmullw
        | Opcode.Pmulhw | Opcode.Pmulhuw | Opcode.Pavgb | Opcode.Pavgw | Opcode.Psadbw
        | Opcode.Psllw | Opcode.Pslld | Opcode.Psllq | Opcode.Psrlw | Opcode.Psrld | Opcode.Psrlq
        | Opcode.Psraw | Opcode.Psrad | Opcode.Pshufb | Opcode.Phaddw | Opcode.Phaddd
        | Opcode.Phsubw | Opcode.Phsubd | Opcode.Psignb | Opcode.Psignw | Opcode.Psignd
        | Opcode.Pmaddubsw | Opcode.Pmulhrsw | Opcode.Phaddsw | Opcode.Phsubsw | Opcode.Pabsb
        | Opcode.Pabsw | Opcode.Pabsd | Opcode.Pcmpeqq | Opcode.Pcmpgtq | Opcode.Packusdw
        | Opcode.Pmaxsb | Opcode.Pmaxsd | Opcode.Pmaxud | Opcode.Pmaxuw | Opcode.Pminsb
        | Opcode.Pminsd | Opcode.Pminud | Opcode.Pminuw | Opcode.Pmuldq | Opcode.Pmulld
        | Opcode.Phminposuw | Opcode.Blendvps | Opcode.Blendvpd | Opcode.Pblendvb | Opcode.Ptest
        | Opcode.Pmovsxbw | Opcode.Pmovsxbd | Opcode.Pmovsxbq | Opcode.Pmovsxwd | Opcode.Pmovsxwq
        | Opcode.Pmovsxdq | Opcode.Pmovzxbw | Opcode.Pmovzxbd | Opcode.Pmovzxbq | Opcode.Pmovzxwd
        | Opcode.Pmovzxwq | Opcode.Pmovzxdq ),
        [ Operand.Reg src; Operand.Reg reg ] ) -> (
        match (xmm_ok src, xmm_ok reg) with
        | Ok (), Ok () ->
            Ok [ Lowered.Sse_binop_r_rm { op = i.Instruction.op; reg; rm = Rm.Reg src } ]
        | Error e, _ | _, Error e -> Error e)
    | ( ( Opcode.Addsd | Opcode.Subsd | Opcode.Mulsd | Opcode.Divsd | Opcode.Addss | Opcode.Subss
        | Opcode.Mulss | Opcode.Divss | Opcode.Comisd | Opcode.Ucomisd | Opcode.Comiss
        | Opcode.Ucomiss | Opcode.Xorpd | Opcode.Pxor | Opcode.Movapd | Opcode.Cvtsd2ss
        | Opcode.Cvtss2sd | Opcode.Cvtps2pd | Opcode.Cvtpd2ps | Opcode.Cvtdq2ps | Opcode.Cvtps2dq
        | Opcode.Cvttps2dq | Opcode.Andps | Opcode.Andnps | Opcode.Orps | Opcode.Xorps
        | Opcode.Andpd | Opcode.Andnpd | Opcode.Orpd | Opcode.Movaps | Opcode.Movups | Opcode.Movupd
        | Opcode.Addps | Opcode.Subps | Opcode.Mulps | Opcode.Divps | Opcode.Addpd | Opcode.Subpd
        | Opcode.Mulpd | Opcode.Divpd | Opcode.Maxss | Opcode.Minss | Opcode.Maxsd | Opcode.Minsd
        | Opcode.Maxps | Opcode.Minps | Opcode.Maxpd | Opcode.Minpd | Opcode.Sqrtss | Opcode.Sqrtsd
        | Opcode.Sqrtps | Opcode.Sqrtpd | Opcode.Unpcklps | Opcode.Unpckhps | Opcode.Unpcklpd
        | Opcode.Unpckhpd | Opcode.Punpcklqdq | Opcode.Punpckhqdq | Opcode.Punpcklbw
        | Opcode.Punpckhbw | Opcode.Punpcklwd | Opcode.Punpckhwd | Opcode.Punpckldq
        | Opcode.Punpckhdq | Opcode.Paddb | Opcode.Paddw | Opcode.Paddd | Opcode.Paddq
        | Opcode.Psubb | Opcode.Psubw | Opcode.Psubd | Opcode.Psubq | Opcode.Pcmpeqb
        | Opcode.Pcmpeqw | Opcode.Pcmpeqd | Opcode.Pcmpgtb | Opcode.Pcmpgtw | Opcode.Pcmpgtd
        | Opcode.Packsswb | Opcode.Packssdw | Opcode.Packuswb | Opcode.Pand | Opcode.Pandn
        | Opcode.Por | Opcode.Pminub | Opcode.Pmaxub | Opcode.Pminsw | Opcode.Pmaxsw | Opcode.Pmullw
        | Opcode.Pmulhw | Opcode.Pmulhuw | Opcode.Pavgb | Opcode.Pavgw | Opcode.Psadbw
        | Opcode.Psllw | Opcode.Pslld | Opcode.Psllq | Opcode.Psrlw | Opcode.Psrld | Opcode.Psrlq
        | Opcode.Psraw | Opcode.Psrad | Opcode.Pshufb | Opcode.Phaddw | Opcode.Phaddd
        | Opcode.Phsubw | Opcode.Phsubd | Opcode.Psignb | Opcode.Psignw | Opcode.Psignd
        | Opcode.Pmaddubsw | Opcode.Pmulhrsw | Opcode.Phaddsw | Opcode.Phsubsw | Opcode.Pabsb
        | Opcode.Pabsw | Opcode.Pabsd | Opcode.Pcmpeqq | Opcode.Pcmpgtq | Opcode.Packusdw
        | Opcode.Pmaxsb | Opcode.Pmaxsd | Opcode.Pmaxud | Opcode.Pmaxuw | Opcode.Pminsb
        | Opcode.Pminsd | Opcode.Pminud | Opcode.Pminuw | Opcode.Pmuldq | Opcode.Pmulld
        | Opcode.Phminposuw | Opcode.Blendvps | Opcode.Blendvpd | Opcode.Pblendvb | Opcode.Ptest
        | Opcode.Pmovsxbw | Opcode.Pmovsxbd | Opcode.Pmovsxbq | Opcode.Pmovsxwd | Opcode.Pmovsxwq
        | Opcode.Pmovsxdq | Opcode.Pmovzxbw | Opcode.Pmovzxbd | Opcode.Pmovzxbq | Opcode.Pmovzxwd
        | Opcode.Pmovzxwq | Opcode.Pmovzxdq | Opcode.Movntdqa ),
        [ Operand.Mem m; Operand.Reg reg ] ) -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Sse_binop_r_rm { op = i.Instruction.op; reg; rm = Rm.Mem m } ])
    (* [psllw $5, %xmm1] ({!Opcode.Psllw}'s own doc comment): the immediate-count sibling
       of the register/memory-count shift family just above - register-only, per
       {!Lowered.Xmm_shift_imm_rm}'s own comment. *)
    | ( ( Opcode.Psllw | Opcode.Pslld | Opcode.Psllq | Opcode.Psrlw | Opcode.Psrld | Opcode.Psrlq
        | Opcode.Psraw | Opcode.Psrad | Opcode.Pslldq | Opcode.Psrldq ),
        [ Operand.Imm v; Operand.Reg dst ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match xmm_ok dst with
            | Error e -> Error e
            | Ok () ->
                Ok [ Lowered.Xmm_shift_imm_rm { op = i.Instruction.op; rm = Rm.Reg dst; imm } ]))
    | (Opcode.Movsd | Opcode.Movss), [ Operand.Mem m; Operand.Reg reg ] -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Sse_mov_r_rm { op = i.Instruction.op; reg; rm = Rm.Mem m } ])
    | (Opcode.Movsd | Opcode.Movss), [ Operand.Reg reg; Operand.Mem m ] -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Sse_mov_rm_r { op = i.Instruction.op; rm = Rm.Mem m; reg } ])
    (* [movsd/movss .Lxx, %xmmN]: a bare-symbol load source, GAS's spelling for
       a rip-relative/absolute float constant. Same duality as [Lea] above -
       reuse [mem_of_symbol] rather than a third [Sse_mov_r_rm] shape. *)
    | (Opcode.Movsd | Opcode.Movss), [ Operand.Sym e; Operand.Reg reg ] -> (
        match xmm_ok reg with
        | Error e2 -> Error e2
        | Ok () ->
            Ok
              [ Lowered.Sse_mov_r_rm { op = i.Instruction.op; reg; rm = Rm.Mem (mem_of_symbol e) } ]
        )
    (* [movdqa]/[movdqu]: {!Opcode.Movdqa}'s own doc comment - unlike [movsd]/[movss],
       register-register is real and unambiguous here (confirmed against real GNU as: the
       low-numbered [0x6F] load opcode is what GAS emits for [movdqa %xmmN, %xmmM]), so this adds
       the [Reg; Reg] arm {!Sse_mov_r_rm} otherwise never receives. *)
    | (Opcode.Movdqa | Opcode.Movdqu), [ Operand.Reg src; Operand.Reg reg ] -> (
        match (xmm_ok src, xmm_ok reg) with
        | Ok (), Ok () ->
            Ok [ Lowered.Sse_mov_r_rm { op = i.Instruction.op; reg; rm = Rm.Reg src } ]
        | Error e, _ | _, Error e -> Error e)
    | (Opcode.Movdqa | Opcode.Movdqu), [ Operand.Mem m; Operand.Reg reg ] -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Sse_mov_r_rm { op = i.Instruction.op; reg; rm = Rm.Mem m } ])
    | (Opcode.Movdqa | Opcode.Movdqu), [ Operand.Reg reg; Operand.Mem m ] -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Sse_mov_rm_r { op = i.Instruction.op; rm = Rm.Mem m; reg } ])
    (* [xorpd __negd_mask, %xmmN] (M5, asm/docs/corpus.md): ccomp's own
       sign-flip idiom for float negation/`fabs`, reading a sign-mask
       constant from a bare symbol - the identical bare-symbol-source
       duality as [movsd]/[movss] just above, on the one binop mnemonic this
       corpus evidences it for (register-register `xorpd %xmmN, %xmmN`, the
       zeroing idiom, is the only other shape measured; no other binop here
       is evidenced with a symbolic source). *)
    | Opcode.Xorpd, [ Operand.Sym e; Operand.Reg reg ] -> (
        match xmm_ok reg with
        | Error e2 -> Error e2
        | Ok () ->
            Ok
              [
                Lowered.Sse_binop_r_rm { op = i.Instruction.op; reg; rm = Rm.Mem (mem_of_symbol e) };
              ])
    (* [cvtsi2sd]/[cvtsi2ss]: [rm]'s width must match the mnemonic's own
       ([width_ok], not [xmm_ok] - reused exactly as documented on
       {!sse_operand_class_mismatch}, since xmm's width 128 can never equal a
       GPR instruction's declared 32/64 anyway). *)
    | (Opcode.Cvtsi2sd | Opcode.Cvtsi2ss), [ Operand.Reg src; Operand.Reg reg ] -> (
        match (width_ok src, xmm_ok reg) with
        | Ok (), Ok () ->
            Ok
              [
                Lowered.Cvtsi2f_r_rm
                  { op = i.Instruction.op; width = i.Instruction.width; reg; rm = Rm.Reg src };
              ]
        | Error e, _ | _, Error e -> Error e)
    | (Opcode.Cvtsi2sd | Opcode.Cvtsi2ss), [ Operand.Mem m; Operand.Reg reg ] -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () ->
            Ok
              [
                Lowered.Cvtsi2f_r_rm
                  { op = i.Instruction.op; width = i.Instruction.width; reg; rm = Rm.Mem m };
              ])
    | Opcode.Cvttsd2si, [ Operand.Reg src; Operand.Reg reg ] -> (
        match (xmm_ok src, width_ok reg) with
        | Ok (), Ok () ->
            Ok [ Lowered.Cvtf2i_r_rm { width = i.Instruction.width; reg; rm = Rm.Reg src } ]
        | Error e, _ | _, Error e -> Error e)
    | Opcode.Cvttsd2si, [ Operand.Mem m; Operand.Reg reg ] -> (
        match width_ok reg with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Cvtf2i_r_rm { width = i.Instruction.width; reg; rm = Rm.Mem m } ])
    (* [movd]/[movq] register-register: the same mnemonic and operand shape serves both
       directions ({!Movsd}/{!Movss}'s own precedent), disambiguated here by which operand is
       actually xmm rather than by opcode - {!Cvtsi2f_r_rm} (load: xmm dest, gpr src) reused
       verbatim for one direction, {!Movd_rm_r} (store: gpr dest, xmm src) built for the other. *)
    | Opcode.Movd, [ Operand.Reg a; Operand.Reg b ] -> (
        match (xmm_ok a, xmm_ok b) with
        | Error _, Ok () -> (
            match width_ok a with
            | Ok () ->
                Ok
                  [
                    Lowered.Cvtsi2f_r_rm
                      { op = i.Instruction.op; width = i.Instruction.width; reg = b; rm = Rm.Reg a };
                  ]
            | Error e -> Error e)
        | Ok (), Error _ -> (
            match width_ok b with
            | Ok () ->
                Ok
                  [
                    Lowered.Movd_rm_r
                      { op = i.Instruction.op; width = i.Instruction.width; reg = a; rm = Rm.Reg b };
                  ]
            | Error e -> Error e)
        | Ok (), Ok () | Error _, Error _ -> bad (`No_form (Opcode.name i.Instruction.op)))
    (* [movd rm, %xmmN] (load from memory): {!Movd}'s own doc comment - only [movd]'s memory
       forms are admitted, never [movq]'s (real GNU as routes those to the unrelated scalar-xmm
       [movq] instruction instead), and the frontend only ever constructs [Opcode.Movd] with
       [width = 64] for a register-register pair, so [width] is always 32 here regardless. *)
    | Opcode.Movd, [ Operand.Mem m; Operand.Reg reg ] -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () ->
            Ok
              [
                Lowered.Cvtsi2f_r_rm
                  { op = i.Instruction.op; width = i.Instruction.width; reg; rm = Rm.Mem m };
              ])
    (* [movd %xmmN, rm] (store to memory): {!Movd_rm_r}'s own store direction, memory-destination
       sibling of the register-register case above. *)
    | Opcode.Movd, [ Operand.Reg reg; Operand.Mem m ] -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () ->
            Ok
              [
                Lowered.Movd_rm_r
                  { op = i.Instruction.op; width = i.Instruction.width; reg; rm = Rm.Mem m };
              ])
    (* [blendvps %xmm0, rm, reg] ({!Opcode.Blendvps}'s own doc comment): the canonical spelling
       with the implicit mask written out, lowered exactly like the two-operand one; a mask other
       than [%xmm0] matches no arm. *)
    | ( (Opcode.Blendvps | Opcode.Blendvpd | Opcode.Pblendvb),
        [ Operand.Reg mask; Operand.Reg src; Operand.Reg reg ] )
      when mask.Reg.num = 0 && mask.Reg.width = 128 -> (
        match (xmm_ok src, xmm_ok reg) with
        | Ok (), Ok () ->
            Ok [ Lowered.Sse_binop_r_rm { op = i.Instruction.op; reg; rm = Rm.Reg src } ]
        | Error e, _ | _, Error e -> Error e)
    | ( (Opcode.Blendvps | Opcode.Blendvpd | Opcode.Pblendvb),
        [ Operand.Reg mask; Operand.Mem m; Operand.Reg reg ] )
      when mask.Reg.num = 0 && mask.Reg.width = 128 -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Sse_binop_r_rm { op = i.Instruction.op; reg; rm = Rm.Mem m } ])
    (* [pinsrb]/[pinsrd] ({!Opcode.Pinsrb}'s own doc comment): [rm] is a GPR32 or memory source,
       [reg] the xmm destination - {!Pinsrw}'s own arms below at opcode map 3. *)
    | (Opcode.Pinsrb | Opcode.Pinsrd), [ Operand.Imm v; Operand.Reg src; Operand.Reg reg ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (width_ok src, xmm_ok reg) with
            | Ok (), Ok () ->
                Ok
                  [
                    Lowered.Sse_binop_imm_r_rm { op = i.Instruction.op; reg; rm = Rm.Reg src; imm };
                  ]
            | Error e, _ | _, Error e -> Error e))
    | (Opcode.Pinsrb | Opcode.Pinsrd), [ Operand.Imm v; Operand.Mem m; Operand.Reg reg ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match xmm_ok reg with
            | Error e -> Error e
            | Ok () ->
                Ok [ Lowered.Sse_binop_imm_r_rm { op = i.Instruction.op; reg; rm = Rm.Mem m; imm } ]
            ))
    (* [pextrb]/[pextrd]/[extractps] ({!Opcode.Pextrb}'s own doc comment): the AT&T destination
       (GPR32 or memory) is the ModR/M [rm], the xmm source the [reg] - the reverse of the
       AT&T operand order, unlike {!Pextrw}. *)
    | ( (Opcode.Pextrb | Opcode.Pextrd | Opcode.Extractps),
        [ Operand.Imm v; Operand.Reg src; Operand.Reg dst ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (xmm_ok src, width_ok dst) with
            | Ok (), Ok () ->
                Ok
                  [
                    Lowered.Sse_binop_imm_r_rm
                      { op = i.Instruction.op; reg = src; rm = Rm.Reg dst; imm };
                  ]
            | Error e, _ | _, Error e -> Error e))
    | ( (Opcode.Pextrb | Opcode.Pextrd | Opcode.Extractps),
        [ Operand.Imm v; Operand.Reg src; Operand.Mem m ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match xmm_ok src with
            | Error e -> Error e
            | Ok () ->
                Ok
                  [
                    Lowered.Sse_binop_imm_r_rm
                      { op = i.Instruction.op; reg = src; rm = Rm.Mem m; imm };
                  ]))
    (* [pinsrw $imm8, gpr32/m16, xmm] ({!Opcode.Pinsrw}'s own doc comment): {!Lowered.Sse_binop_imm_r_rm}'s
       cross-class member - [rm] is a GPR ([width_ok], not [xmm_ok] - {!Cvtsi2sd}'s own class split
       above) or memory, [reg] is xmm. *)
    | Opcode.Pinsrw, [ Operand.Imm v; Operand.Reg src; Operand.Reg reg ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (width_ok src, xmm_ok reg) with
            | Ok (), Ok () ->
                Ok
                  [
                    Lowered.Sse_binop_imm_r_rm { op = i.Instruction.op; reg; rm = Rm.Reg src; imm };
                  ]
            | Error e, _ | _, Error e -> Error e))
    | Opcode.Pinsrw, [ Operand.Imm v; Operand.Mem m; Operand.Reg reg ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match xmm_ok reg with
            | Error e -> Error e
            | Ok () ->
                Ok [ Lowered.Sse_binop_imm_r_rm { op = i.Instruction.op; reg; rm = Rm.Mem m; imm } ]
            ))
    (* [pextrw $imm8, xmm, gpr32] ({!Opcode.Pextrw}'s own doc comment): {!Pinsrw}'s store-direction
       mirror - [rm] (source) is xmm, [reg] (dest) is a GPR; no memory-destination arm exists,
       matching real GNU as's own routing of that spelling to an unimplemented three-byte-opcode
       instruction instead ({!Opcode.Pextrw}'s own doc comment). *)
    | Opcode.Pextrw, [ Operand.Imm v; Operand.Reg src; Operand.Reg reg ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (xmm_ok src, width_ok reg) with
            | Ok (), Ok () ->
                Ok
                  [
                    Lowered.Sse_binop_imm_r_rm { op = i.Instruction.op; reg; rm = Rm.Reg src; imm };
                  ]
            | Error e, _ | _, Error e -> Error e))
    (* [movmskps]/[movmskpd]/[pmovmskb] ({!Opcode.Movmskps}'s own doc comment): [rm] (source) is
       xmm, [reg] (dest) is a GPR32 - the same field roles as {!Pextrw} minus the immediate; no
       memory-source arm exists, matching real GNU as's own outright rejection of that spelling. *)
    | (Opcode.Movmskps | Opcode.Movmskpd | Opcode.Pmovmskb), [ Operand.Reg src; Operand.Reg reg ]
      -> (
        match (xmm_ok src, width_ok reg) with
        | Ok (), Ok () ->
            Ok [ Lowered.Sse_binop_r_rm { op = i.Instruction.op; reg; rm = Rm.Reg src } ]
        | Error e, _ | _, Error e -> Error e)
    (* {3 x86 VEX (x86 vector extensions)}

       [vaddsd src2, src1, dst]: real GNU as's own AT&T operand order for the
       non-destructive three-operand form, [src2] first as for every other
       binop above, [src1] (the [vvvv] operand) in the middle, [dst] last.
       [src2] is checked against the two-byte-VEX ModR/M restriction here, at
       lowering, rather than left to the codec to reject silently -
       {!Vex_rm_extended_register} names exactly which operand and why. A
       memory [src2] needs the same check on its base/index (if present)
       instead of on a register number directly - {!vex_mem_ok}. *)
    (* 512-bit EVEX ([zmm]): every operand must be [zmm]; register source only. *)
    | op, [ Operand.Reg src2; Operand.Reg src1; Operand.Reg dst ]
      when dst.Reg.width = 512 && evex512_binop op -> (
        match (zmm_ok src2, zmm_ok src1, zmm_ok dst) with
        | Ok (), Ok (), Ok () ->
            if src2.num >= 8 then bad (`Vex_rm_extended_register src2.name)
            else Ok [ Lowered.Vex_binop_rr_rm { op; dst; src1; src2 = Rm.Reg src2 } ]
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
    (* 256-bit VEX ([ymm], {!Opcode.Vaddps}'s own doc comment): every operand must be [ymm], the
       same [src2.num >= 8] / {!vex_mem_ok} restriction applies. *)
    | op, [ Operand.Reg src2; Operand.Reg src1; Operand.Reg dst ]
      when dst.Reg.width = 256 && vex256_binop op -> (
        match (ymm_ok src2, ymm_ok src1, ymm_ok dst) with
        | Ok (), Ok (), Ok () ->
            if src2.num >= 8 then bad (`Vex_rm_extended_register src2.name)
            else Ok [ Lowered.Vex_binop_rr_rm { op; dst; src1; src2 = Rm.Reg src2 } ]
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
    | op, [ Operand.Mem m; Operand.Reg src1; Operand.Reg dst ]
      when dst.Reg.width = 256 && vex256_binop op -> (
        match (ymm_ok src1, ymm_ok dst, vex_mem_ok m) with
        | Ok (), Ok (), Ok () -> Ok [ Lowered.Vex_binop_rr_rm { op; dst; src1; src2 = Rm.Mem m } ]
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
    | op, [ Operand.Reg src; Operand.Reg dst ]
      when dst.Reg.width = 256 && vex256_unop op && op <> Opcode.Vmovntdqa -> (
        match (ymm_ok src, ymm_ok dst) with
        | Ok (), Ok () ->
            if src.num >= 8 then bad (`Vex_rm_extended_register src.name)
            else Ok [ Lowered.Vex_unop_r_rm { op; dst; src = Rm.Reg src } ]
        | Error e, _ | _, Error e -> Error e)
    | op, [ Operand.Mem m; Operand.Reg dst ] when dst.Reg.width = 256 && vex256_unop op -> (
        match (ymm_ok dst, vex_mem_ok m) with
        | Ok (), Ok () -> Ok [ Lowered.Vex_unop_r_rm { op; dst; src = Rm.Mem m } ]
        | Error e, _ | _, Error e -> Error e)
    | ( ( Opcode.Vaddsd | Opcode.Vsubsd | Opcode.Vmulsd | Opcode.Vdivsd | Opcode.Vaddss
        | Opcode.Vsubss | Opcode.Vmulss | Opcode.Vdivss | Opcode.Vaddps | Opcode.Vsubps
        | Opcode.Vmulps | Opcode.Vdivps | Opcode.Vaddpd | Opcode.Vsubpd | Opcode.Vmulpd
        | Opcode.Vdivpd | Opcode.Vandps | Opcode.Vandnps | Opcode.Vorps | Opcode.Vxorps
        | Opcode.Vandpd | Opcode.Vandnpd | Opcode.Vorpd | Opcode.Vxorpd | Opcode.Vunpcklps
        | Opcode.Vunpckhps | Opcode.Vunpcklpd | Opcode.Vunpckhpd | Opcode.Vpunpcklqdq
        | Opcode.Vpunpckhqdq | Opcode.Vpunpcklbw | Opcode.Vpunpckhbw | Opcode.Vpunpcklwd
        | Opcode.Vpunpckhwd | Opcode.Vpunpckldq | Opcode.Vpunpckhdq | Opcode.Vpaddb | Opcode.Vpaddw
        | Opcode.Vpaddd | Opcode.Vpaddq | Opcode.Vpsubb | Opcode.Vpsubw | Opcode.Vpsubd
        | Opcode.Vpsubq | Opcode.Vpcmpeqb | Opcode.Vpcmpeqw | Opcode.Vpcmpeqd | Opcode.Vpcmpgtb
        | Opcode.Vpcmpgtw | Opcode.Vpcmpgtd | Opcode.Vpacksswb | Opcode.Vpackssdw | Opcode.Vpackuswb
        | Opcode.Vpand | Opcode.Vpandn | Opcode.Vpor | Opcode.Vpminub | Opcode.Vpmaxub
        | Opcode.Vpminsw | Opcode.Vpmaxsw | Opcode.Vpmullw | Opcode.Vpmulhw | Opcode.Vpmulhuw
        | Opcode.Vpavgb | Opcode.Vpavgw | Opcode.Vpsadbw | Opcode.Vpsllw | Opcode.Vpslld
        | Opcode.Vpsllq | Opcode.Vpsrlw | Opcode.Vpsrld | Opcode.Vpsrlq | Opcode.Vpsraw
        | Opcode.Vpsrad | Opcode.Vmaxsd | Opcode.Vminsd | Opcode.Vmaxss | Opcode.Vminss
        | Opcode.Vmaxps | Opcode.Vminps | Opcode.Vmaxpd | Opcode.Vminpd | Opcode.Vsqrtsd
        | Opcode.Vsqrtss | Opcode.Vpshufb | Opcode.Vphaddw | Opcode.Vphaddd | Opcode.Vphaddsw
        | Opcode.Vpmaddubsw | Opcode.Vphsubw | Opcode.Vphsubd | Opcode.Vphsubsw | Opcode.Vpsignb
        | Opcode.Vpsignw | Opcode.Vpsignd | Opcode.Vpmulhrsw | Opcode.Vpmuldq | Opcode.Vpcmpeqq
        | Opcode.Vpackusdw | Opcode.Vpcmpgtq | Opcode.Vpminsb | Opcode.Vpminsd | Opcode.Vpminuw
        | Opcode.Vpminud | Opcode.Vpmaxsb | Opcode.Vpmaxsd | Opcode.Vpmaxuw | Opcode.Vpmaxud
        | Opcode.Vpmulld ),
        [ Operand.Reg src2; Operand.Reg src1; Operand.Reg dst ] ) -> (
        match (xmm_ok src2, xmm_ok src1, xmm_ok dst) with
        | Ok (), Ok (), Ok () ->
            if src2.num >= 8 then bad (`Vex_rm_extended_register src2.name)
            else
              Ok
                [ Lowered.Vex_binop_rr_rm { op = i.Instruction.op; dst; src1; src2 = Rm.Reg src2 } ]
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
    | ( ( Opcode.Vaddsd | Opcode.Vsubsd | Opcode.Vmulsd | Opcode.Vdivsd | Opcode.Vaddss
        | Opcode.Vsubss | Opcode.Vmulss | Opcode.Vdivss | Opcode.Vaddps | Opcode.Vsubps
        | Opcode.Vmulps | Opcode.Vdivps | Opcode.Vaddpd | Opcode.Vsubpd | Opcode.Vmulpd
        | Opcode.Vdivpd | Opcode.Vandps | Opcode.Vandnps | Opcode.Vorps | Opcode.Vxorps
        | Opcode.Vandpd | Opcode.Vandnpd | Opcode.Vorpd | Opcode.Vxorpd | Opcode.Vunpcklps
        | Opcode.Vunpckhps | Opcode.Vunpcklpd | Opcode.Vunpckhpd | Opcode.Vpunpcklqdq
        | Opcode.Vpunpckhqdq | Opcode.Vpunpcklbw | Opcode.Vpunpckhbw | Opcode.Vpunpcklwd
        | Opcode.Vpunpckhwd | Opcode.Vpunpckldq | Opcode.Vpunpckhdq | Opcode.Vpaddb | Opcode.Vpaddw
        | Opcode.Vpaddd | Opcode.Vpaddq | Opcode.Vpsubb | Opcode.Vpsubw | Opcode.Vpsubd
        | Opcode.Vpsubq | Opcode.Vpcmpeqb | Opcode.Vpcmpeqw | Opcode.Vpcmpeqd | Opcode.Vpcmpgtb
        | Opcode.Vpcmpgtw | Opcode.Vpcmpgtd | Opcode.Vpacksswb | Opcode.Vpackssdw | Opcode.Vpackuswb
        | Opcode.Vpand | Opcode.Vpandn | Opcode.Vpor | Opcode.Vpminub | Opcode.Vpmaxub
        | Opcode.Vpminsw | Opcode.Vpmaxsw | Opcode.Vpmullw | Opcode.Vpmulhw | Opcode.Vpmulhuw
        | Opcode.Vpavgb | Opcode.Vpavgw | Opcode.Vpsadbw | Opcode.Vpsllw | Opcode.Vpslld
        | Opcode.Vpsllq | Opcode.Vpsrlw | Opcode.Vpsrld | Opcode.Vpsrlq | Opcode.Vpsraw
        | Opcode.Vpsrad | Opcode.Vmaxsd | Opcode.Vminsd | Opcode.Vmaxss | Opcode.Vminss
        | Opcode.Vmaxps | Opcode.Vminps | Opcode.Vmaxpd | Opcode.Vminpd | Opcode.Vsqrtsd
        | Opcode.Vsqrtss | Opcode.Vpshufb | Opcode.Vphaddw | Opcode.Vphaddd | Opcode.Vphaddsw
        | Opcode.Vpmaddubsw | Opcode.Vphsubw | Opcode.Vphsubd | Opcode.Vphsubsw | Opcode.Vpsignb
        | Opcode.Vpsignw | Opcode.Vpsignd | Opcode.Vpmulhrsw | Opcode.Vpmuldq | Opcode.Vpcmpeqq
        | Opcode.Vpackusdw | Opcode.Vpcmpgtq | Opcode.Vpminsb | Opcode.Vpminsd | Opcode.Vpminuw
        | Opcode.Vpminud | Opcode.Vpmaxsb | Opcode.Vpmaxsd | Opcode.Vpmaxuw | Opcode.Vpmaxud
        | Opcode.Vpmulld ),
        [ Operand.Mem m; Operand.Reg src1; Operand.Reg dst ] ) -> (
        match (xmm_ok src1, xmm_ok dst) with
        | Ok (), Ok () -> (
            match vex_mem_ok m with
            | Error e -> Error e
            | Ok () ->
                Ok [ Lowered.Vex_binop_rr_rm { op = i.Instruction.op; dst; src1; src2 = Rm.Mem m } ]
            )
        | Error e, _ | _, Error e -> Error e)
    (* [vaddsd sym, %xmm1, %xmm0]: a bare-symbol source, the same
       [mem_of_symbol] duality [movsd]/[movss]/[xorpd] already read through -
       a synthesized [rip_reg] base on x86-64, an absolute disp32 on x86-32,
       neither of which ever names a real base/index register, so
       {!vex_mem_ok} is unneeded here (it always accepts [None]/[None]). *)
    | ( ( Opcode.Vaddsd | Opcode.Vsubsd | Opcode.Vmulsd | Opcode.Vdivsd | Opcode.Vaddss
        | Opcode.Vsubss | Opcode.Vmulss | Opcode.Vdivss | Opcode.Vaddps | Opcode.Vsubps
        | Opcode.Vmulps | Opcode.Vdivps | Opcode.Vaddpd | Opcode.Vsubpd | Opcode.Vmulpd
        | Opcode.Vdivpd | Opcode.Vandps | Opcode.Vandnps | Opcode.Vorps | Opcode.Vxorps
        | Opcode.Vandpd | Opcode.Vandnpd | Opcode.Vorpd | Opcode.Vxorpd | Opcode.Vunpcklps
        | Opcode.Vunpckhps | Opcode.Vunpcklpd | Opcode.Vunpckhpd | Opcode.Vpunpcklqdq
        | Opcode.Vpunpckhqdq | Opcode.Vpunpcklbw | Opcode.Vpunpckhbw | Opcode.Vpunpcklwd
        | Opcode.Vpunpckhwd | Opcode.Vpunpckldq | Opcode.Vpunpckhdq | Opcode.Vpaddb | Opcode.Vpaddw
        | Opcode.Vpaddd | Opcode.Vpaddq | Opcode.Vpsubb | Opcode.Vpsubw | Opcode.Vpsubd
        | Opcode.Vpsubq | Opcode.Vpcmpeqb | Opcode.Vpcmpeqw | Opcode.Vpcmpeqd | Opcode.Vpcmpgtb
        | Opcode.Vpcmpgtw | Opcode.Vpcmpgtd | Opcode.Vpacksswb | Opcode.Vpackssdw | Opcode.Vpackuswb
        | Opcode.Vpand | Opcode.Vpandn | Opcode.Vpor | Opcode.Vpminub | Opcode.Vpmaxub
        | Opcode.Vpminsw | Opcode.Vpmaxsw | Opcode.Vpmullw | Opcode.Vpmulhw | Opcode.Vpmulhuw
        | Opcode.Vpavgb | Opcode.Vpavgw | Opcode.Vpsadbw | Opcode.Vpsllw | Opcode.Vpslld
        | Opcode.Vpsllq | Opcode.Vpsrlw | Opcode.Vpsrld | Opcode.Vpsrlq | Opcode.Vpsraw
        | Opcode.Vpsrad | Opcode.Vmaxsd | Opcode.Vminsd | Opcode.Vmaxss | Opcode.Vminss
        | Opcode.Vmaxps | Opcode.Vminps | Opcode.Vmaxpd | Opcode.Vminpd | Opcode.Vsqrtsd
        | Opcode.Vsqrtss | Opcode.Vpshufb | Opcode.Vphaddw | Opcode.Vphaddd | Opcode.Vphaddsw
        | Opcode.Vpmaddubsw | Opcode.Vphsubw | Opcode.Vphsubd | Opcode.Vphsubsw | Opcode.Vpsignb
        | Opcode.Vpsignw | Opcode.Vpsignd | Opcode.Vpmulhrsw | Opcode.Vpmuldq | Opcode.Vpcmpeqq
        | Opcode.Vpackusdw | Opcode.Vpcmpgtq | Opcode.Vpminsb | Opcode.Vpminsd | Opcode.Vpminuw
        | Opcode.Vpminud | Opcode.Vpmaxsb | Opcode.Vpmaxsd | Opcode.Vpmaxuw | Opcode.Vpmaxud
        | Opcode.Vpmulld ),
        [ Operand.Sym e; Operand.Reg src1; Operand.Reg dst ] ) -> (
        match (xmm_ok src1, xmm_ok dst) with
        | Ok (), Ok () ->
            Ok
              [
                Lowered.Vex_binop_rr_rm
                  { op = i.Instruction.op; dst; src1; src2 = Rm.Mem (mem_of_symbol e) };
              ]
        | Error e2, _ | _, Error e2 -> Error e2)
    (* [vpsllw $5, %xmm2, %xmm1] ({!Opcode.Vpsllw}'s own doc comment): the VEX sibling of
       the legacy immediate-count shift arm above - [imm, src, dst], {!Opcode.Vpshufd}'s own
       operand order, into {!Lowered.Vex_shift_imm_rm} instead of {!Vex_unop_imm_r_rm} since
       [dst] is real vvvv here, not architecturally-unused. *)
    | ( ( Opcode.Vpsllw | Opcode.Vpslld | Opcode.Vpsllq | Opcode.Vpsrlw | Opcode.Vpsrld
        | Opcode.Vpsrlq | Opcode.Vpsraw | Opcode.Vpsrad | Opcode.Vpslldq | Opcode.Vpsrldq ),
        [ Operand.Imm v; Operand.Reg src; Operand.Reg dst ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (xmm_ok src, xmm_ok dst) with
            | Ok (), Ok () ->
                if src.num >= 8 then bad (`Vex_rm_extended_register src.name)
                else
                  Ok
                    [
                      Lowered.Vex_shift_imm_rm { op = i.Instruction.op; dst; rm = Rm.Reg src; imm };
                    ]
            | Error e, _ | _, Error e -> Error e))
    (* [vsqrtps]/[vsqrtpd]/[vmovaps]/[vmovups]/[vmovapd]/[vmovupd]/[vcomisd]/[vucomisd]/
       [vcomiss]/[vucomiss] ({!Opcode.Vsqrtps}'s own doc comment): genuinely two-operand, no
       [vvvv]-carried [src1] at all - {!Lowered.Vex_unop_r_rm} rather than {!Vex_binop_rr_rm}. *)
    | ( ( Opcode.Vsqrtps | Opcode.Vsqrtpd | Opcode.Vmovaps | Opcode.Vmovups | Opcode.Vmovapd
        | Opcode.Vmovupd | Opcode.Vcomisd | Opcode.Vucomisd | Opcode.Vcomiss | Opcode.Vucomiss
        | Opcode.Vcvtps2pd | Opcode.Vcvtpd2ps | Opcode.Vcvtdq2ps | Opcode.Vcvtps2dq
        | Opcode.Vcvttps2dq | Opcode.Vmovdqa | Opcode.Vmovdqu | Opcode.Vpabsb | Opcode.Vpabsw
        | Opcode.Vpabsd | Opcode.Vphminposuw | Opcode.Vptest | Opcode.Vpmovsxbw | Opcode.Vpmovsxbd
        | Opcode.Vpmovsxbq | Opcode.Vpmovsxwd | Opcode.Vpmovsxwq | Opcode.Vpmovsxdq
        | Opcode.Vpmovzxbw | Opcode.Vpmovzxbd | Opcode.Vpmovzxbq | Opcode.Vpmovzxwd
        | Opcode.Vpmovzxwq | Opcode.Vpmovzxdq ),
        [ Operand.Reg src; Operand.Reg dst ] ) -> (
        match (xmm_ok src, xmm_ok dst) with
        | Ok (), Ok () ->
            if src.num >= 8 then bad (`Vex_rm_extended_register src.name)
            else Ok [ Lowered.Vex_unop_r_rm { op = i.Instruction.op; dst; src = Rm.Reg src } ]
        | Error e, _ | _, Error e -> Error e)
    (* {!Opcode.Vcvtpd2ps}'s own doc comment: its register<-memory spelling is genuinely
       ambiguous in real GAS (needs the [x]/[y]-suffix disambiguation this project's parser
       does not implement), so it is deliberately excluded from this arm and the [Sym] one
       below - {!Opcode.Vcvtps2pd} has no such ambiguity and is included in both. *)
    | ( ( Opcode.Vsqrtps | Opcode.Vsqrtpd | Opcode.Vmovaps | Opcode.Vmovups | Opcode.Vmovapd
        | Opcode.Vmovupd | Opcode.Vcomisd | Opcode.Vucomisd | Opcode.Vcomiss | Opcode.Vucomiss
        | Opcode.Vcvtps2pd | Opcode.Vcvtdq2ps | Opcode.Vcvtps2dq | Opcode.Vcvttps2dq
        | Opcode.Vmovdqa | Opcode.Vmovdqu | Opcode.Vpabsb | Opcode.Vpabsw | Opcode.Vpabsd
        | Opcode.Vphminposuw | Opcode.Vptest | Opcode.Vpmovsxbw | Opcode.Vpmovsxbd
        | Opcode.Vpmovsxbq | Opcode.Vpmovsxwd | Opcode.Vpmovsxwq | Opcode.Vpmovsxdq
        | Opcode.Vpmovzxbw | Opcode.Vpmovzxbd | Opcode.Vpmovzxbq | Opcode.Vpmovzxwd
        | Opcode.Vpmovzxwq | Opcode.Vpmovzxdq | Opcode.Vmovntdqa ),
        [ Operand.Mem m; Operand.Reg dst ] ) -> (
        match xmm_ok dst with
        | Ok () -> (
            match vex_mem_ok m with
            | Error e -> Error e
            | Ok () -> Ok [ Lowered.Vex_unop_r_rm { op = i.Instruction.op; dst; src = Rm.Mem m } ])
        | Error e -> Error e)
    | ( ( Opcode.Vsqrtps | Opcode.Vsqrtpd | Opcode.Vmovaps | Opcode.Vmovups | Opcode.Vmovapd
        | Opcode.Vmovupd | Opcode.Vcomisd | Opcode.Vucomisd | Opcode.Vcomiss | Opcode.Vucomiss
        | Opcode.Vcvtps2pd | Opcode.Vcvtdq2ps | Opcode.Vcvtps2dq | Opcode.Vcvttps2dq ),
        [ Operand.Sym e; Operand.Reg dst ] ) -> (
        match xmm_ok dst with
        | Ok () ->
            Ok
              [
                Lowered.Vex_unop_r_rm { op = i.Instruction.op; dst; src = Rm.Mem (mem_of_symbol e) };
              ]
        | Error e2 -> Error e2)
    (* [vshufps $imm8, src2, src1, dst]/[vshufpd $imm8, src2, src1, dst]:
       {!Vex_binop_rr_rm}'s own [src2, src1, dst] operand order, with a leading imm8 -
       {!Lowered.Vex_binop_imm_rr_rm} rather than {!Vex_binop_rr_rm}. *)
    | ( ( Opcode.Vshufps | Opcode.Vshufpd | Opcode.Vcmpss | Opcode.Vcmpsd | Opcode.Vcmpps
        | Opcode.Vcmppd | Opcode.Vpalignr | Opcode.Vblendps | Opcode.Vblendpd | Opcode.Vpblendw
        | Opcode.Vroundss | Opcode.Vroundsd | Opcode.Vdpps | Opcode.Vdppd | Opcode.Vmpsadbw
        | Opcode.Vinsertps ),
        [ Operand.Imm v; Operand.Reg src2; Operand.Reg src1; Operand.Reg dst ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (xmm_ok src2, xmm_ok src1, xmm_ok dst) with
            | Ok (), Ok (), Ok () ->
                if src2.num >= 8 then bad (`Vex_rm_extended_register src2.name)
                else
                  Ok
                    [
                      Lowered.Vex_binop_imm_rr_rm
                        { op = i.Instruction.op; dst; src1; src2 = Rm.Reg src2; imm };
                    ]
            | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e))
    | ( ( Opcode.Vshufps | Opcode.Vshufpd | Opcode.Vcmpss | Opcode.Vcmpsd | Opcode.Vcmpps
        | Opcode.Vcmppd | Opcode.Vpalignr | Opcode.Vblendps | Opcode.Vblendpd | Opcode.Vpblendw
        | Opcode.Vroundss | Opcode.Vroundsd | Opcode.Vdpps | Opcode.Vdppd | Opcode.Vmpsadbw
        | Opcode.Vinsertps ),
        [ Operand.Imm v; Operand.Mem m; Operand.Reg src1; Operand.Reg dst ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (xmm_ok src1, xmm_ok dst) with
            | Ok (), Ok () -> (
                match vex_mem_ok m with
                | Error e -> Error e
                | Ok () ->
                    Ok
                      [
                        Lowered.Vex_binop_imm_rr_rm
                          { op = i.Instruction.op; dst; src1; src2 = Rm.Mem m; imm };
                      ])
            | Error e, _ | _, Error e -> Error e))
    (* [vpshufd $imm8, src, dst]/[vpshuflw $imm8, src, dst]/[vpshufhw $imm8, src, dst] ({!Opcode.Vpshufd}'s own doc comment): {!Vex_unop_r_rm}'s own [src, dst] operand order,
       with a leading imm8 - {!Lowered.Vex_unop_imm_r_rm} rather than {!Vex_unop_r_rm}. *)
    | ( (Opcode.Vpshufd | Opcode.Vpshuflw | Opcode.Vpshufhw),
        [ Operand.Imm v; Operand.Reg src; Operand.Reg dst ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (xmm_ok src, xmm_ok dst) with
            | Ok (), Ok () ->
                if src.num >= 8 then bad (`Vex_rm_extended_register src.name)
                else
                  Ok
                    [
                      Lowered.Vex_unop_imm_r_rm
                        { op = i.Instruction.op; dst; src = Rm.Reg src; imm };
                    ]
            | Error e, _ | _, Error e -> Error e))
    | ( (Opcode.Vpshufd | Opcode.Vpshuflw | Opcode.Vpshufhw),
        [ Operand.Imm v; Operand.Mem m; Operand.Reg dst ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match xmm_ok dst with
            | Error e -> Error e
            | Ok () -> (
                match vex_mem_ok m with
                | Error e -> Error e
                | Ok () ->
                    Ok
                      [
                        Lowered.Vex_unop_imm_r_rm
                          { op = i.Instruction.op; dst; src = Rm.Mem m; imm };
                      ])))
    (* [vmovd] register-register ({!Opcode.Vmovd}'s own doc comment): the same mnemonic and
       operand shape serves both directions, disambiguated by which operand is actually xmm -
       {!Lowered.Vex_movd_r_rm}'s load direction (rm=gpr, dst=xmm) or {!Lowered.Vex_movd_rm_r}'s
       store direction (reg=xmm, rm=gpr), exactly mirroring the legacy [movd] dispatch just above
       but with the two-byte-VEX extended-register check on the GPR side (which always occupies
       the ModR/M r/m field here, in both directions). *)
    | Opcode.Vmovd, [ Operand.Reg a; Operand.Reg b ] -> (
        match (xmm_ok a, xmm_ok b) with
        | Error _, Ok () -> (
            match width_ok a with
            | Ok () ->
                if a.num >= 8 then bad (`Vex_rm_extended_register a.name)
                else Ok [ Lowered.Vex_movd_r_rm { op = i.Instruction.op; dst = b; rm = Rm.Reg a } ]
            | Error e -> Error e)
        | Ok (), Error _ -> (
            match width_ok b with
            | Ok () ->
                if b.num >= 8 then bad (`Vex_rm_extended_register b.name)
                else Ok [ Lowered.Vex_movd_rm_r { op = i.Instruction.op; reg = a; rm = Rm.Reg b } ]
            | Error e -> Error e)
        | Ok (), Ok () | Error _, Error _ -> bad (`No_form (Opcode.name i.Instruction.op)))
    (* [vmovd rm, %xmmN] (load from memory): {!Vex_movd_r_rm}'s memory-source sibling of the
       register-register case above. *)
    | Opcode.Vmovd, [ Operand.Mem m; Operand.Reg reg ] -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () -> (
            match vex_mem_ok m with
            | Error e -> Error e
            | Ok () ->
                Ok [ Lowered.Vex_movd_r_rm { op = i.Instruction.op; dst = reg; rm = Rm.Mem m } ]))
    (* [vmovd %xmmN, rm] (store to memory): {!Vex_movd_rm_r}'s memory-destination sibling. *)
    | Opcode.Vmovd, [ Operand.Reg reg; Operand.Mem m ] -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () -> (
            match vex_mem_ok m with
            | Error e -> Error e
            | Ok () -> Ok [ Lowered.Vex_movd_rm_r { op = i.Instruction.op; reg; rm = Rm.Mem m } ]))
    (* [vpinsrw $imm8, gpr32/m16, src1, dst] ({!Opcode.Vpinsrw}'s own doc comment):
       {!Vex_binop_imm_rr_rm}'s own [src2, src1, dst] operand order, [src2] a GPR ([width_ok], not
       [xmm_ok] - {!Vmovd}'s own GPR-crossing precedent) or memory rather than xmm. *)
    | Opcode.Vpinsrw, [ Operand.Imm v; Operand.Reg src2; Operand.Reg src1; Operand.Reg dst ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (width_ok src2, xmm_ok src1, xmm_ok dst) with
            | Ok (), Ok (), Ok () ->
                if src2.num >= 8 then bad (`Vex_rm_extended_register src2.name)
                else
                  Ok
                    [
                      Lowered.Vex_binop_imm_rr_rm
                        { op = i.Instruction.op; dst; src1; src2 = Rm.Reg src2; imm };
                    ]
            | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e))
    | Opcode.Vpinsrw, [ Operand.Imm v; Operand.Mem m; Operand.Reg src1; Operand.Reg dst ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (xmm_ok src1, xmm_ok dst) with
            | Ok (), Ok () -> (
                match vex_mem_ok m with
                | Error e -> Error e
                | Ok () ->
                    Ok
                      [
                        Lowered.Vex_binop_imm_rr_rm
                          { op = i.Instruction.op; dst; src1; src2 = Rm.Mem m; imm };
                      ])
            | Error e, _ | _, Error e -> Error e))
    (* [vpextrw $imm8, xmm, gpr32] ({!Opcode.Vpextrw}'s own doc comment): {!Vex_unop_imm_r_rm}'s
       own [src, dst] operand order, [dst] a GPR ([width_ok]) rather than xmm; no memory-source arm
       exists at this opcode, mirroring legacy [pextrw]'s own scope boundary. *)
    | Opcode.Vpextrw, [ Operand.Imm v; Operand.Reg src; Operand.Reg dst ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm -> (
            match (xmm_ok src, width_ok dst) with
            | Ok (), Ok () ->
                if src.num >= 8 then bad (`Vex_rm_extended_register src.name)
                else
                  Ok
                    [
                      Lowered.Vex_unop_imm_r_rm
                        { op = i.Instruction.op; dst; src = Rm.Reg src; imm };
                    ]
            | Error e, _ | _, Error e -> Error e))
    (* [vmovmskps]/[vmovmskpd]/[vpmovmskb] ({!Opcode.Vmovmskps}'s own doc comment): {!Vpextrw}'s
       own field roles and two-byte-VEX [src.num >= 8] restriction, minus the immediate. *)
    | (Opcode.Vmovmskps | Opcode.Vmovmskpd | Opcode.Vpmovmskb), [ Operand.Reg src; Operand.Reg dst ]
      -> (
        match (xmm_ok src, width_ok dst) with
        | Ok (), Ok () ->
            if src.num >= 8 then bad (`Vex_rm_extended_register src.name)
            else Ok [ Lowered.Vex_unop_r_rm { op = i.Instruction.op; dst; src = Rm.Reg src } ]
        | Error e, _ | _, Error e -> Error e)
    (* [fldl]/[fstpl]/[fstps]/[flds] (M5, asm/docs/corpus.md): ccomp's own x87
       double/single-precision spill-and-reload sequence around a `%st(0)`
       return value - always to/from a stack memory operand in this corpus,
       never a register, so {!Lowered.Fpu_mem} takes a bare {!Mem.t} rather
       than the general {!Rm.t} every GPR/xmm form above uses. *)
    | op, [ Operand.Mem m ] when x87_memory_op op ->
        Ok [ Lowered.Fpu_mem { op = i.Instruction.op; mem = m } ]
    (* [flds sym] / [fadds sym] / [fsubs sym] - a bare-symbol source, the same duality
       [lea]/[movsd]/[xorpd] already read through [mem_of_symbol] (M5, asm/docs/corpus.md -
       i64_dtou.S's `flds LC1`/`fsubs LC1`; gas_frontier.t's i64_utod.S/i64_utof.S own
       `fadds LC1`). Scoped to [Flds]/[Fadds]/[Fsubs]: no fixture evidences a
       bare-symbol [fldl]/[fstpl]/[fstps] (every recurrence of those three
       is `disp(%esp)`). *)
    | op, [ Operand.Sym e ] when x87_symbol_op op ->
        Ok [ Lowered.Fpu_mem { op = i.Instruction.op; mem = mem_of_symbol e } ]
    (* FADD_ST0_X87: the source is a stack register and the destination is the
       implicit top of stack.  Keep the reverse direction out of this case:
       it is a different XED IFORM and opcode family. *)
    | Opcode.Fadd, [ Operand.Reg src; Operand.Reg dest ]
      when src.Reg.width = 80 && dest.Reg.width = 80 && dest.Reg.num = 0 ->
        Ok [ Lowered.Fadd_st0_x87 { src } ]
    | Opcode.Fucomp, [] -> Ok [ Lowered.Fucomp ]
    (* [fnstsw %ax] (M5, asm/docs/corpus.md - i64_dtou.S): [%ax]'s width and number pin it to
       exactly that register, {!Shift_cl_rm}'s own count-in-%cl precedent for a fixed implicit
       operand - any other register, or none, is rejected rather than silently accepted. *)
    | Opcode.Fnstsw, [ Operand.Reg ax ] when ax.Reg.width = 16 && ax.Reg.num = 0 ->
        Ok [ Lowered.Fnstsw ]
    | Opcode.Fnstsw, _ -> bad `Fnstsw_operand
    | Opcode.Sahf, [] -> Ok [ Lowered.Sahf ]
    (* [sete %al]/[setl %r8b] (M5, asm/docs/corpus.md): [Instruction.width] is
       always 8 here ({!simplify_instruction} pins it), so [width_ok] on the
       destination register is exactly the right check with no extra
       plumbing. *)
    | Opcode.Setcc cc, [ Operand.Reg r ] -> (
        match width_ok r with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Setcc_rm { cc; rm = Rm.Reg r } ])
    | Opcode.Setcc cc, [ Operand.Mem m ] -> Ok [ Lowered.Setcc_rm { cc; rm = Rm.Mem m } ]
    (* Zero-/sign-extending move (M5, asm/docs/corpus.md). [src_width = 32]
       is [movslq] - a structurally different opcode ([0x63], no ModR/M-reg
       extension table, mandatory REX.W) from the [0F B6/B7/BE/BF] family the
       other widths share, so it gets its own {!Lowered.Movsxd_r_rm} rather
       than a third [Movx_r_rm] case; matched first since it is the more
       specific pattern (a variable [src_width] binding below would
       otherwise shadow it). The [when src.Reg.width = src_width] guard on
       the register-source forms rejects a mnemonic/operand mismatch
       ([movsbl %eax, ...]) by simply not matching, the same way
       {!width_ok}'s failures elsewhere fall through to the catch-all. *)
    | Opcode.Movsx { src_width = 32 }, [ Operand.Reg src; Operand.Reg reg ] when src.Reg.width = 32
      -> (
        match width_ok reg with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Movsxd_r_rm { reg; rm = Rm.Reg src } ])
    | Opcode.Movsx { src_width = 32 }, [ Operand.Mem m; Operand.Reg reg ] -> (
        match width_ok reg with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Movsxd_r_rm { reg; rm = Rm.Mem m } ])
    | Opcode.Movzx { src_width }, [ Operand.Reg src; Operand.Reg reg ]
      when src.Reg.width = src_width -> (
        match width_ok reg with
        | Error e -> Error e
        | Ok () ->
            Ok
              [
                Lowered.Movx_r_rm
                  {
                    zero_extend = true;
                    src_width;
                    width = i.Instruction.width;
                    reg;
                    rm = Rm.Reg src;
                  };
              ])
    | Opcode.Movzx { src_width }, [ Operand.Mem m; Operand.Reg reg ] -> (
        match width_ok reg with
        | Error e -> Error e
        | Ok () ->
            Ok
              [
                Lowered.Movx_r_rm
                  { zero_extend = true; src_width; width = i.Instruction.width; reg; rm = Rm.Mem m };
              ])
    | Opcode.Movsx { src_width }, [ Operand.Reg src; Operand.Reg reg ]
      when src.Reg.width = src_width -> (
        match width_ok reg with
        | Error e -> Error e
        | Ok () ->
            Ok
              [
                Lowered.Movx_r_rm
                  {
                    zero_extend = false;
                    src_width;
                    width = i.Instruction.width;
                    reg;
                    rm = Rm.Reg src;
                  };
              ])
    | Opcode.Movsx { src_width }, [ Operand.Mem m; Operand.Reg reg ] -> (
        match width_ok reg with
        | Error e -> Error e
        | Ok () ->
            Ok
              [
                Lowered.Movx_r_rm
                  {
                    zero_extend = false;
                    src_width;
                    width = i.Instruction.width;
                    reg;
                    rm = Rm.Mem m;
                  };
              ])
    | _ -> bad (`No_form (Opcode.name i.Instruction.op))

  (* {2 The codec}

     [fixup_kind] must be declared before the first codec value, or the weak
     type variable in [rm_codec] is created before the type exists and OCaml
     reports it escaping its scope. *)

  (* The one place the two modes differ in what a ModR/M pattern *means*: in
     64-bit, mod=00 rm=101 is RIP-relative and its displacement is a
     PC-relative data reference; in 32-bit the same bits are an absolute
     address. Measured, not assumed - the committed relocations are
     R_X86_64_PC32 and R_386_32 respectively. *)
  (* An operand this pair of opcodes can express: the accumulator, 32 bits, and
     an address with no base and no index. Anything else falls through to the
     general ModR/M forms. *)
  let moffs_applies ~width ~(reg : Reg.t) ~(rm : Rm.t) =
    (not M.rex_allowed) && width = 32 && reg.Reg.num = 0
    && match rm with Rm.Mem m -> m.Mem.base = None && m.Mem.index = None | Rm.Reg _ -> false

  let moffs_disp = function Rm.Mem m -> m.Mem.disp | Rm.Reg _ -> Disp.zero
  let moffs_mem d = { Mem.base = None; index = None; scale = 1; disp = d }

  let rm_codec =
    make_rm_codec ~reg_of_num ~rip_relative:M.rex_allowed
      ~disp_kind:(if M.rex_allowed then Pcrel32_data else Abs32)

  (* Two declared finite relations. Writing them as tables rather than as an
     [Iso_fun] over an integer is what makes [check] able to say anything: it
     decides injectivity in both directions and totality against the field
     width, which for the condition code means all sixteen values decode. *)
  let alu_rm_r_codec =
    C.iso_table ~name:"alu-rm-r-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        (List.filter_map
           (fun op -> Option.map (fun b -> (op, b)) (Opcode.to_rm_r op))
           [
             Opcode.Xor;
             Opcode.Cmp;
             Opcode.Sub;
             Opcode.Add;
             Opcode.Adc;
             Opcode.Sbb;
             Opcode.Test;
             Opcode.Or;
             Opcode.And;
           ])
      (C.field ~width:8 "opcode")

  (* The reg<-rm mirror of {!alu_rm_r_codec}, {!Opcode.to_r_rm}'s table. *)
  let alu_r_rm_codec =
    C.iso_table ~name:"alu-r-rm-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        (List.filter_map
           (fun op -> Option.map (fun b -> (op, b)) (Opcode.to_r_rm op))
           [
             Opcode.Adc;
             Opcode.Add;
             Opcode.Xor;
             Opcode.Sub;
             Opcode.And;
             Opcode.Or;
             Opcode.Sbb;
             Opcode.Cmp;
           ])
      (C.field ~width:8 "opcode")

  let cc_codec =
    C.iso_table ~name:"cc" ~equal:Cc.equal ~show:Cc.name
      ~entries:(List.map (fun c -> (c, Int64.of_int (Cc.code c))) Cc.all)
      (C.field ~width:4 "cc")

  (* One rung of a branch ladder. The displacement it carries is the lowered
     operand's, and which of the three shapes that operand is in decides what
     goes in the bits: a placeholder for anything still symbolic, the real value
     for a decoded one. [encode] never sees the difference - it asks for a
     rung by name or for all of them - because the dispatch happened before the
     codec was reached. *)
  let disp_of = function
    | Asm_core.Lowered_ast.Symbolic _ -> 0L
    | Asm_core.Lowered_ast.Resolved { value; _ } -> value

  let resolved ~rung ~width d =
    (* A fixup node is unsigned - it is one slice of a value whose signedness
       belongs to the whole - so applying it is this Iso_fun's job. Skipping it
       makes a backward branch decode as a forward one at the far end of the
       address space. *)
    Asm_core.Lowered_ast.Resolved { value = sign_extend ~width d; rung }

  let jmp_rung ~opcode ~width ~kind ~rung =
    C.iso_fun ~name:("jmp." ^ rung)
      ~encode:(function Lowered.Jmp_rel { target } -> Some ((), disp_of target) | _ -> None)
      ~decode:(fun ((), d) -> Some (Lowered.Jmp_rel { target = resolved ~rung ~width d }))
      C.(const ~width:8 opcode ** le_fixup ~width ~kind "target")

  (* The two [jcc] rungs cannot share a builder the way the [jmp] ones do: the
     condition sits in the low nibble of a one-byte opcode in the short form and
     of the *second* byte of a two-byte one in the near form, so the bit layout
     differs by more than a constant. *)
  let jcc_rung_short () =
    C.iso_fun ~name:"jcc.d8"
      ~encode:(function
        | Lowered.Jcc_rel { cc; target } -> Some ((), (cc, disp_of target)) | _ -> None)
      ~decode:(fun ((), (cc, d)) ->
        Some (Lowered.Jcc_rel { cc; target = resolved ~rung:"d8" ~width:8 d }))
      C.(const ~width:4 0b0111L ** cc_codec ** le_fixup ~width:8 ~kind:Pcrel8_branch "target")

  let jcc_rung_near () =
    C.iso_fun ~name:"jcc.d32"
      ~encode:(function
        | Lowered.Jcc_rel { cc; target } -> Some ((), (cc, disp_of target)) | _ -> None)
      ~decode:(fun ((), (cc, d)) ->
        Some (Lowered.Jcc_rel { cc; target = resolved ~rung:"d32" ~width:32 d }))
      C.(const ~width:12 0x0F8L ** cc_codec ** le_fixup ~width:32 ~kind:Pcrel32_branch "target")

  (* REX is *derived*, never chosen: W comes from the operand width and R/X/B
     from whether a register number needs a fourth bit. So the alternative is
     between a byte and nothing, and which one applies is settled before the
     codec runs. What the codec still owns is the byte's layout and the decode
     direction. *)
  (* {2 Prefixes}

     Two derived bytes in one slot, in the order the machine requires: the
     legacy address-size prefix precedes REX.

     [asz] is 0x67, and in 64-bit mode it means "the address registers in this
     memory operand are 32-bit". CompCert emits exactly that -
     [leal 0(%edi,%edi,1), %eax] and [leal 2(%eax), %eax] - and omitting it does
     not produce a different-but-equal encoding: it addresses %rdi where the
     program said %edi. So it is derived from the operand rather than offered as
     a choice, for the same reason REX is.

     They share a slot so that adding one did not have to reshape all thirteen
     alternatives' tuples. *)
  type prefixes = { asz : bool; opsz : bool; rex : int option }

  let asz_of ~rm =
    (* Only meaningful where the address width and the register width can
       differ. In 32-bit mode 0x67 would select 16-bit addressing, which no
       fixture uses and nothing here emits. *)
    M.rex_allowed
    &&
    match rm with
    | Rm.Reg _ -> false
    | Rm.Mem m ->
        let is32 (r : Reg.t option) = match r with Some r -> r.width = 32 | None -> false in
        is32 m.base || is32 m.index

  let prefixes_of ~width ~reg ~rm =
    if not M.rex_allowed then None
    else
      let r = reg >= 8 in
      let x, b =
        match rm with
        | Rm.Reg g -> (false, g.num >= 8)
        | Rm.Mem m ->
            ( (match m.index with Some i -> i.num >= 8 | None -> false),
              match m.base with Some bb -> bb.num >= 8 | None -> false )
      in
      let w = width = 64 in
      (* {!Reg.names_8l} has no legacy AH/CH/DH/BH spelling at all - num 4-7
         at width 8 is always SPL/BPL/SIL/DIL (M5, asm/docs/corpus.md:
         [movb %sil, 7(%rdi)]) - so an *empty* REX byte (0x40, every bit
         clear) has to be present whenever one of those four is named, purely
         to select that reading over the legacy one; [w]/[r]/[x]/[b] above
         would all otherwise stay false and this prefix would vanish
         entirely. Scoped to real register operands - a [reg]/[rm] that is
         actually a ModR/M-reg extension code (group-1/2/3's [ext], or
         [Setcc_rm]'s fixed 0) rather than a register number happens to share
         this parameter, but no fixture reaches an 8-bit extension-coded form
         with a raw value in 4-7 today, so the two are not distinguished
         here. *)
      let byte_legacy_num n = width = 8 && n >= 4 && n < 8 in
      let needs_empty_rex =
        byte_legacy_num reg || match rm with Rm.Reg g -> byte_legacy_num g.num | Rm.Mem _ -> false
      in
      if w || r || x || b || needs_empty_rex then
        Some
          ((if w then 8 else 0)
          lor (if r then 4 else 0)
          lor (if x then 2 else 0)
          lor if b then 1 else 0)
      else None

  let rex_codec : (int option, fixup_kind) C.t =
    if M.rex_allowed then
      C.choice ~name:"rex"
        [
          (* Present is tried first on decode, where its 0100 constant is what
             discriminates; on encode the two projections are disjoint, so order
             does not matter there. *)
          C.alt ~label:"rex-present" ~priority:0
            (C.iso_fun ~name:"rex-present"
               ~encode:(function Some v -> Some ((), Int64.of_int (v land 0xf)) | None -> None)
               ~decode:(fun ((), v) -> Some (Some (0x40 lor Int64.to_int v)))
               C.(const ~width:4 4L ** field ~width:4 "wrxb"));
          C.alt ~label:"rex-absent" ~priority:1
            (C.iso_fun ~name:"rex-absent"
               ~encode:(function None -> Some () | Some _ -> None)
               ~decode:(fun () -> Some None)
               C.empty);
        ]
    else
      (* Not an [Alt] with one branch: in 32-bit mode there is no choice to
         record, and a one-branch alternative would put a meaningless component
         in every form id. *)
      C.iso_fun ~name:"no-rex"
        ~encode:(function None -> Some () | Some _ -> None)
        ~decode:(fun () -> Some None)
        C.empty

  let asz_codec : (bool, fixup_kind) C.t =
    if M.rex_allowed then
      C.choice ~name:"asz"
        [
          C.alt ~label:"asz-present" ~priority:0
            (C.iso_fun ~name:"asz-present"
               ~encode:(function true -> Some () | false -> None)
               ~decode:(fun () -> Some true)
               C.(const ~width:8 0x67L));
          C.alt ~label:"asz-absent" ~priority:1
            (C.iso_fun ~name:"asz-absent"
               ~encode:(function false -> Some () | true -> None)
               ~decode:(fun () -> Some false)
               C.empty);
        ]
    else
      (* As for REX: a one-branch alternative would put a meaningless component
         in every 32-bit form id. *)
      C.iso_fun ~name:"no-asz"
        ~encode:(function false -> Some () | true -> None)
        ~decode:(fun () -> Some false)
        C.empty

  (* The operand-size override (M5, asm/docs/corpus.md: [movw %r8w,
     58(%rsp)]) - unlike [asz_codec]/[rex_codec] this exists in both modes
     (x86_32 has 16-bit operands too), so it is never the trivial
     [M.rex_allowed]-gated single-branch form the other two have. Ordering
     matters: real [as] always places [0x66] before REX (verified against
     [x86_64-linux-gnu-as]/[objdump]: [66 44 89 44 24 3a] for
     [movw %r8w, 0x3a(%rsp)]), which is why it sits between [asz_codec] and
     [rex_codec] in {!prefixes_codec} rather than after. *)
  let opsz_codec : (bool, fixup_kind) C.t =
    C.choice ~name:"opsz"
      [
        C.alt ~label:"opsz-present" ~priority:0
          (C.iso_fun ~name:"opsz-present"
             ~encode:(function true -> Some () | false -> None)
             ~decode:(fun () -> Some true)
             C.(const ~width:8 0x66L));
        C.alt ~label:"opsz-absent" ~priority:1
          (C.iso_fun ~name:"opsz-absent"
             ~encode:(function false -> Some () | true -> None)
             ~decode:(fun () -> Some false)
             C.empty);
      ]

  let prefixes_of ~width ~reg ~rm =
    { asz = asz_of ~rm; opsz = width = 16; rex = prefixes_of ~width ~reg ~rm }

  (* {!prefixes_of} for a ModR/M-reg field that holds an opcode extension
     (group-1/2/3's [/ext]) rather than a register number. The extension is
     always below 8, so it never needs REX.R, and it must not be read as a
     register: at [width = 8], values 4-7 would otherwise look like
     SPL/BPL/SIL/DIL and force an empty REX byte, giving [andb $5, %cl] a
     spurious [0x40] on x86-64. *)
  let prefixes_of_ext ~width ~rm = prefixes_of ~width ~reg:0 ~rm

  let prefixes_codec : (prefixes, fixup_kind) C.t =
    C.iso_fun ~name:"prefixes"
      ~encode:(fun p -> Some (p.asz, (p.opsz, p.rex)))
      ~decode:(fun (asz, (opsz, rex)) -> Some { asz; opsz; rex })
      C.(asz_codec ** opsz_codec ** rex_codec)

  let width_of_prefixes p =
    if p.opsz then 16 else match p.rex with Some v when v land 8 <> 0 -> 64 | _ -> 32

  (* Decoding cannot learn the address width from the ModR/M bits - only the
     prefix says it - so the registers the rm codec built at the default address
     width are retyped once the prefix is known. Without this a decoded
     [0(%edi,%edi,1)] would re-encode without its 0x67 and stop round-tripping. *)
  let retype_addr ~(p : prefixes) rm =
    if not p.asz then rm
    else
      match rm with
      | Rm.Reg _ -> rm
      | Rm.Mem m ->
          Rm.Mem
            {
              m with
              Mem.base =
                Option.map
                  (fun (r : Reg.t) -> if is_rip r then r else reg_at ~width:32 r.num)
                  m.Mem.base;
              index = Option.map (fun (r : Reg.t) -> reg_at ~width:32 r.num) m.Mem.index;
            }

  (* REX.R, REX.X and REX.B are the fourth bit of a register number, and they
     live in the prefix rather than in the ModR/M or SIB byte - so decode has to
     put them back, and nothing did. The encoder derives them from the register
     number, which is why the *bytes* were right all along and only the
     disassembly was wrong: no fixture reached r8-r15 until cond_select.

     [ix = 4] is the one place this interacts with the SIB escape. With REX.X
     clear it means "no index"; with REX.X set it means r12, because the escape
     is on the four-bit number and not on the three-bit field. The rm codec
     cannot see the prefix, so it hands the raw register up and the decision is
     made here, where the prefix is known. *)
  let rex_bit (p : prefixes) mask = match p.rex with Some v when v land mask <> 0 -> 8 | _ -> 0

  let extend_rex ~(p : prefixes) rm =
    match rm with
    | Rm.Reg r -> Rm.Reg (reg_at ~width:r.width (r.num + rex_bit p 1))
    | Rm.Mem m ->
        Rm.Mem
          {
            m with
            Mem.base =
              Option.map
                (fun (r : Reg.t) ->
                  (* RIP is not one of the sixteen, so no prefix bit extends it. *)
                  if is_rip r then r else reg_at ~width:r.width (r.num + rex_bit p 1))
                m.Mem.base;
            index =
              (match m.Mem.index with
              | Some r when r.num = 4 && rex_bit p 2 = 0 -> None
              | other ->
                  Option.map (fun (r : Reg.t) -> reg_at ~width:r.width (r.num + rex_bit p 2)) other);
          }

  (* The three fixes a decoded rm needs, and they are different: [extend_rex]
     restores a register *number* from the prefix, [retype_rm] gives a register
     operand the instruction's operand width, and [retype_addr] gives an address
     register the width the address-size prefix selected. *)
  let rm_of ~p ~width rm = retype_addr ~p (retype_rm ~width (extend_rex ~p rm))

  (* The ModR/M reg field, with REX.R put back the same way. *)
  let reg_field ~p ~width n = reg_at ~width (n + rex_bit p 4)

  (* {3 SSE2 scalar float (M5, asm/docs/corpus.md)}

     [asz, mandatory-prefix, REX, 0F, opcode, rm] - one fixed shape per
     mandatory-prefix group, not one alt per mnemonic and not one alt
     spanning every mnemonic. {!Lowered.Sse_binop_r_rm}'s own comment says
     why the latter doesn't work: REX has to sit between the mandatory prefix
     and 0F, so a table whose *selected prefix* changes per entry cannot be
     expressed as one contiguous field - only a prefix fixed per [C.alt] can.
     [prefixes_codec]/[prefixes_of] (above) are untouched; every non-SSE
     alternative still uses them exactly as before. Where a mandatory prefix
     exists, this reimplements [prefixes_codec]'s two components as three
     separate pieces (with the mandatory byte spliced between them) rather
     than extending the shared record type. REX.W still comes from
     [prefixes_of]'s [~width] exactly as everywhere else - [~width:32] for
     the pure-xmm forms (REX.W always clear; the 128-bit xmm operand's real
     width is never what selects it) and the real GPR width for the two
     conversion families, which is also why those two builders take [~width]
     from the [Lowered] value rather than hard-coding 32. *)

  let sse_binop_f2_codec =
    C.iso_table ~name:"sse-binop-f2-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Addsd, 0x58L);
          (Opcode.Subsd, 0x5CL);
          (Opcode.Mulsd, 0x59L);
          (Opcode.Divsd, 0x5EL);
          (Opcode.Cvtsd2ss, 0x5AL);
          (Opcode.Maxsd, 0x5FL);
          (Opcode.Minsd, 0x5DL);
          (Opcode.Sqrtsd, 0x51L);
        ]
      (C.field ~width:8 "opcode")

  let sse_binop_f3_codec =
    C.iso_table ~name:"sse-binop-f3-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Addss, 0x58L);
          (Opcode.Subss, 0x5CL);
          (Opcode.Mulss, 0x59L);
          (Opcode.Divss, 0x5EL);
          (Opcode.Cvtss2sd, 0x5AL);
          (Opcode.Maxss, 0x5FL);
          (Opcode.Minss, 0x5DL);
          (Opcode.Sqrtss, 0x51L);
          (Opcode.Cvttps2dq, 0x5BL);
        ]
      (C.field ~width:8 "opcode")

  let sse_binop_66_codec =
    C.iso_table ~name:"sse-binop-66-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Comisd, 0x2FL);
          (Opcode.Ucomisd, 0x2EL);
          (Opcode.Xorpd, 0x57L);
          (Opcode.Pxor, 0xEFL);
          (Opcode.Movapd, 0x28L);
          (Opcode.Andpd, 0x54L);
          (Opcode.Andnpd, 0x55L);
          (Opcode.Orpd, 0x56L);
          (Opcode.Movupd, 0x10L);
          (Opcode.Addpd, 0x58L);
          (Opcode.Subpd, 0x5CL);
          (Opcode.Mulpd, 0x59L);
          (Opcode.Divpd, 0x5EL);
          (Opcode.Maxpd, 0x5FL);
          (Opcode.Minpd, 0x5DL);
          (Opcode.Sqrtpd, 0x51L);
          (Opcode.Cvtpd2ps, 0x5AL);
          (Opcode.Cvtps2dq, 0x5BL);
          (Opcode.Unpcklpd, 0x14L);
          (Opcode.Unpckhpd, 0x15L);
          (Opcode.Punpcklqdq, 0x6CL);
          (Opcode.Punpckhqdq, 0x6DL);
          (Opcode.Punpcklbw, 0x60L);
          (Opcode.Punpckhbw, 0x68L);
          (Opcode.Punpcklwd, 0x61L);
          (Opcode.Punpckhwd, 0x69L);
          (Opcode.Punpckldq, 0x62L);
          (Opcode.Punpckhdq, 0x6AL);
          (Opcode.Paddb, 0xFCL);
          (Opcode.Paddw, 0xFDL);
          (Opcode.Paddd, 0xFEL);
          (Opcode.Paddq, 0xD4L);
          (Opcode.Psubb, 0xF8L);
          (Opcode.Psubw, 0xF9L);
          (Opcode.Psubd, 0xFAL);
          (Opcode.Psubq, 0xFBL);
          (Opcode.Pcmpeqb, 0x74L);
          (Opcode.Pcmpeqw, 0x75L);
          (Opcode.Pcmpeqd, 0x76L);
          (Opcode.Pcmpgtb, 0x64L);
          (Opcode.Pcmpgtw, 0x65L);
          (Opcode.Pcmpgtd, 0x66L);
          (Opcode.Packsswb, 0x63L);
          (Opcode.Packssdw, 0x6BL);
          (Opcode.Packuswb, 0x67L);
          (Opcode.Pand, 0xDBL);
          (Opcode.Pandn, 0xDFL);
          (Opcode.Por, 0xEBL);
          (Opcode.Pminub, 0xDAL);
          (Opcode.Pmaxub, 0xDEL);
          (Opcode.Pminsw, 0xEAL);
          (Opcode.Pmaxsw, 0xEEL);
          (Opcode.Pmullw, 0xD5L);
          (Opcode.Pmulhw, 0xE5L);
          (Opcode.Pmulhuw, 0xE4L);
          (Opcode.Pavgb, 0xE0L);
          (Opcode.Pavgw, 0xE3L);
          (Opcode.Psadbw, 0xF6L);
          (Opcode.Psllw, 0xF1L);
          (Opcode.Pslld, 0xF2L);
          (Opcode.Psllq, 0xF3L);
          (Opcode.Psrlw, 0xD1L);
          (Opcode.Psrld, 0xD2L);
          (Opcode.Psrlq, 0xD3L);
          (Opcode.Psraw, 0xE1L);
          (Opcode.Psrad, 0xE2L);
        ]
      (C.field ~width:8 "opcode")

  let sse_binop_alt ~label ~priority ~mandatory ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_binop_r_rm { op; reg; rm } ->
               let p = prefixes_of ~width:32 ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), (op, { re_reg = reg.num; re_rm = rm })))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), (op, e))))) ->
           let p = { asz; opsz = false; rex } in
           Some
             (Lowered.Sse_binop_r_rm
                { op; reg = reg_field ~p ~width:128 e.re_reg; rm = rm_of ~p ~width:128 e.re_rm }))
         C.(
           asz_codec
           ** const ~width:8 (Int64.of_int mandatory)
           ** rex_codec ** const ~width:8 0x0FL ** opcode_codec ** rm_codec))

  (* {!Opcode.Pshufb}'s own doc comment: {!sse_binop_alt}'s exact field layout, one extra
     fixed [0x38] byte spliced in between the [0x0F] escape and the opcode byte - opcode map 2
     rather than map 1. A one-entry table since {!Pshufb} is this map's only admitted mnemonic. *)
  let sse_binop_0f38_codec =
    C.iso_table ~name:"sse-binop-0f38-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Pshufb, 0x00L);
          (Opcode.Phaddw, 0x01L);
          (Opcode.Phaddd, 0x02L);
          (Opcode.Pmaddubsw, 0x04L);
          (Opcode.Phsubw, 0x05L);
          (Opcode.Phsubd, 0x06L);
          (Opcode.Psignb, 0x08L);
          (Opcode.Psignw, 0x09L);
          (Opcode.Psignd, 0x0AL);
          (Opcode.Pmulhrsw, 0x0BL);
          (Opcode.Phaddsw, 0x03L);
          (Opcode.Phsubsw, 0x07L);
          (Opcode.Pabsb, 0x1CL);
          (Opcode.Pabsw, 0x1DL);
          (Opcode.Pabsd, 0x1EL);
          (Opcode.Pmuldq, 0x28L);
          (Opcode.Pcmpeqq, 0x29L);
          (Opcode.Packusdw, 0x2BL);
          (Opcode.Pminsb, 0x38L);
          (Opcode.Pminsd, 0x39L);
          (Opcode.Pminuw, 0x3AL);
          (Opcode.Pminud, 0x3BL);
          (Opcode.Pmaxsb, 0x3CL);
          (Opcode.Pmaxsd, 0x3DL);
          (Opcode.Pmaxuw, 0x3EL);
          (Opcode.Pmaxud, 0x3FL);
          (Opcode.Pmulld, 0x40L);
          (Opcode.Phminposuw, 0x41L);
          (Opcode.Pcmpgtq, 0x37L);
          (Opcode.Pblendvb, 0x10L);
          (Opcode.Blendvps, 0x14L);
          (Opcode.Blendvpd, 0x15L);
          (Opcode.Ptest, 0x17L);
          (Opcode.Pmovsxbw, 0x20L);
          (Opcode.Pmovsxbd, 0x21L);
          (Opcode.Pmovsxbq, 0x22L);
          (Opcode.Pmovsxwd, 0x23L);
          (Opcode.Pmovsxwq, 0x24L);
          (Opcode.Pmovsxdq, 0x25L);
          (Opcode.Pmovzxbw, 0x30L);
          (Opcode.Pmovzxbd, 0x31L);
          (Opcode.Pmovzxbq, 0x32L);
          (Opcode.Pmovzxwd, 0x33L);
          (Opcode.Pmovzxwq, 0x34L);
          (Opcode.Pmovzxdq, 0x35L);
          (Opcode.Movntdqa, 0x2AL);
        ]
      (C.field ~width:8 "opcode")

  let sse_binop_0f38_alt ~label ~priority ~mandatory ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_binop_r_rm { op; reg; rm } ->
               let p = prefixes_of ~width:32 ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), ((), (op, { re_reg = reg.num; re_rm = rm }))))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), ((), (op, e)))))) ->
           let p = { asz; opsz = false; rex } in
           Some
             (Lowered.Sse_binop_r_rm
                { op; reg = reg_field ~p ~width:128 e.re_reg; rm = rm_of ~p ~width:128 e.re_rm }))
         C.(
           asz_codec
           ** const ~width:8 (Int64.of_int mandatory)
           ** rex_codec ** const ~width:8 0x0FL ** const ~width:8 0x38L ** opcode_codec ** rm_codec))

  (* {!Lowered.Xmm_shift_imm_rm} ({!Opcode.xmm_shift_ext_opcode}'s own doc comment):
     {!sse_binop_alt}'s mandatory-66 field layout, generalized to a fixed [~opcode] byte (one
     alt per lane width, since unlike every {!sse_binop_alt} member the operation itself lives
     in the ModR/M reg field, not the opcode byte) plus a trailing imm8. [rm]'s ModR/M reg field
     is the fixed extension rather than a real register, so passing it as [prefixes_of]'s [reg]
     never sets REX.R (every extension value here is 0-7), exactly the way {!Shift_imm_rm}'s own
     [ext] does for the GPR group. Register-only: decode rejects a memory [re_rm], matching
     {!Xmm_shift_imm_rm}'s own comment that this iform has no memory alternative at all. *)
  let xmm_shift_imm_alt ~label ~priority ~opcode =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Xmm_shift_imm_rm { op; rm; imm } -> (
               match Opcode.xmm_shift_ext_opcode op with
               | Some (ext, op_byte) when op_byte = opcode ->
                   let p = prefixes_of ~width:32 ~reg:ext ~rm in
                   Some (p.asz, ((), (p.rex, ((), ((), ({ re_reg = ext; re_rm = rm }, imm))))))
               | Some _ | None -> None)
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), ((), (e, imm)))))) ->
           match e.re_rm with
           | Rm.Mem _ -> None
           | Rm.Reg _ -> (
               match Opcode.of_xmm_shift_ext_opcode ~opcode e.re_reg with
               | None -> None
               | Some op ->
                   let p = { asz; opsz = false; rex } in
                   Some (Lowered.Xmm_shift_imm_rm { op; rm = rm_of ~p ~width:128 e.re_rm; imm })))
         C.(
           asz_codec ** const ~width:8 0x66L ** rex_codec ** const ~width:8 0x0FL
           ** const ~width:8 (Int64.of_int opcode)
           ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  (* The mandatory-prefix-free members of the binop family - {!Comiss}, and
     the packed-single logical siblings {!Andps}/{!Andnps}/{!Orps}/
     {!Xorps} - reuse [prefixes_codec] directly (an [asz]-then-REX composite
     with nothing spliced between them is exactly what [prefixes_codec]
     already is), the same shape [imul-r-rm]/[cmov-r-rm] already use for a
     mandatory-prefix-free two-byte opcode, generalized over an opcode table
     the same way {!sse_binop_alt} is generalized over its mandatory-prefix
     groups. *)
  let sse_binop_none_codec =
    C.iso_table ~name:"sse-binop-none-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Comiss, 0x2FL);
          (Opcode.Ucomiss, 0x2EL);
          (Opcode.Andps, 0x54L);
          (Opcode.Andnps, 0x55L);
          (Opcode.Orps, 0x56L);
          (Opcode.Xorps, 0x57L);
          (Opcode.Movaps, 0x28L);
          (Opcode.Movups, 0x10L);
          (Opcode.Addps, 0x58L);
          (Opcode.Subps, 0x5CL);
          (Opcode.Mulps, 0x59L);
          (Opcode.Divps, 0x5EL);
          (Opcode.Maxps, 0x5FL);
          (Opcode.Minps, 0x5DL);
          (Opcode.Sqrtps, 0x51L);
          (Opcode.Cvtps2pd, 0x5AL);
          (Opcode.Cvtdq2ps, 0x5BL);
          (Opcode.Unpcklps, 0x14L);
          (Opcode.Unpckhps, 0x15L);
        ]
      (C.field ~width:8 "opcode")

  let sse_binop_none_alt ~label ~priority ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_binop_r_rm { op; reg; rm } ->
               Some
                 ( prefixes_of ~width:32 ~reg:reg.num ~rm,
                   ((), (op, { re_reg = reg.num; re_rm = rm })) )
           | _ -> None)
         ~decode:(fun (p, ((), (op, e))) ->
           Some
             (Lowered.Sse_binop_r_rm
                { op; reg = reg_field ~p ~width:128 e.re_reg; rm = rm_of ~p ~width:128 e.re_rm }))
         C.(prefixes_codec ** const ~width:8 0x0FL ** opcode_codec ** rm_codec))

  (* [movsd]/[movss]'s two directions: same ModR/M shape as {!sse_binop_alt}
     ([reg] is always xmm, one register or memory [rm]), but each direction
     has its own opcode (load [0F 10], store [0F 11]) and there is exactly
     one mnemonic per mandatory-prefix group in each direction, so a fixed
     16-bit [0F]+opcode constant replaces {!sse_binop_alt}'s opcode table -
     there is nothing left to vary once [~mandatory] has picked the
     mnemonic. *)
  let sse_mov_r_rm_alt ~label ~priority ~mandatory ~op ~opcode16 =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_mov_r_rm { op = op'; reg; rm } when op' = op ->
               let p = prefixes_of ~width:32 ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), { re_reg = reg.num; re_rm = rm }))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), e)))) ->
           let p = { asz; opsz = false; rex } in
           Some
             (Lowered.Sse_mov_r_rm
                { op; reg = reg_field ~p ~width:128 e.re_reg; rm = rm_of ~p ~width:128 e.re_rm }))
         C.(
           asz_codec
           ** const ~width:8 (Int64.of_int mandatory)
           ** rex_codec ** const ~width:16 opcode16 ** rm_codec))

  let sse_mov_rm_r_alt ~label ~priority ~mandatory ~op ~opcode16 =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_mov_rm_r { op = op'; rm; reg } when op' = op ->
               let p = prefixes_of ~width:32 ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), { re_reg = reg.num; re_rm = rm }))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), e)))) ->
           let p = { asz; opsz = false; rex } in
           Some
             (Lowered.Sse_mov_rm_r
                { op; rm = rm_of ~p ~width:128 e.re_rm; reg = reg_field ~p ~width:128 e.re_reg }))
         C.(
           asz_codec
           ** const ~width:8 (Int64.of_int mandatory)
           ** rex_codec ** const ~width:16 opcode16 ** rm_codec))

  (* [shufpd]/[cmpsd]/[cmpss]/[cmppd]: {!shld_imm_form}'s two-byte-opcode-plus-trailing-
     imm8 shape, spliced with {!sse_mov_rm_r_alt}'s mandatory-prefix layout and always-xmm
     reg/rm - the first XMM-immediate-carrying legacy shape. Generalized over
     [~mandatory]/[~opcode_codec] the same way {!sse_binop_alt} is: {!Cmpsd}/{!Cmpss} each need
     their own one-entry mandatory-[F2]/[F3] group (opcode 0xC2 has no packed-only-vs-scalar
     split the way {!Shufps}'s single opcode byte does), while {!Cmppd} joins {!Shufpd}'s own
     mandatory-66 group and {!Cmpps} joins {!Shufps}'s own mandatory-prefix-free group below. *)
  let sse_binop_imm_alt ~label ~priority ~mandatory ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_binop_imm_r_rm { op; reg; rm; imm } ->
               let p = prefixes_of ~width:32 ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), (op, ({ re_reg = reg.num; re_rm = rm }, imm))))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), (op, (e, imm)))))) ->
           let p = { asz; opsz = false; rex } in
           Some
             (Lowered.Sse_binop_imm_r_rm
                {
                  op;
                  reg = reg_field ~p ~width:128 e.re_reg;
                  rm = rm_of ~p ~width:128 e.re_rm;
                  imm;
                }))
         C.(
           asz_codec
           ** const ~width:8 (Int64.of_int mandatory)
           ** rex_codec ** const ~width:8 0x0FL ** opcode_codec ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  let sse_binop_imm_66_codec =
    C.iso_table ~name:"sse-binop-imm-66-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Shufpd, 0xC6L); (Opcode.Cmppd, 0xC2L); (Opcode.Pshufd, 0x70L) ]
      (C.field ~width:8 "opcode")

  let sse_binop_imm_f2_codec =
    C.iso_table ~name:"sse-binop-imm-f2-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Cmpsd, 0xC2L); (Opcode.Pshuflw, 0x70L) ]
      (C.field ~width:8 "opcode")

  let sse_binop_imm_f3_codec =
    C.iso_table ~name:"sse-binop-imm-f3-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Cmpss, 0xC2L); (Opcode.Pshufhw, 0x70L) ]
      (C.field ~width:8 "opcode")

  (* [shufps]/[cmpps] ([0F C6|C2 /r ib], no mandatory prefix): {!sse_binop_imm_alt}'s
     mandatory-prefix-free sibling, {!sse_binop_none_alt}'s [prefixes_codec] layout with the
     same trailing imm8. *)
  let sse_binop_imm_none_alt ~label ~priority ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_binop_imm_r_rm { op; reg; rm; imm } ->
               Some
                 ( prefixes_of ~width:32 ~reg:reg.num ~rm,
                   ((), (op, ({ re_reg = reg.num; re_rm = rm }, imm))) )
           | _ -> None)
         ~decode:(fun (p, ((), (op, (e, imm)))) ->
           Some
             (Lowered.Sse_binop_imm_r_rm
                {
                  op;
                  reg = reg_field ~p ~width:128 e.re_reg;
                  rm = rm_of ~p ~width:128 e.re_rm;
                  imm;
                }))
         C.(
           prefixes_codec ** const ~width:8 0x0FL ** opcode_codec ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  let sse_binop_imm_none_codec =
    C.iso_table ~name:"sse-binop-imm-none-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Shufps, 0xC6L); (Opcode.Cmpps, 0xC2L) ]
      (C.field ~width:8 "opcode")

  (* {!Opcode.Palignr}'s own doc comment: {!sse_binop_imm_alt}'s exact field layout, one
     extra fixed [0x3A] byte spliced in between the [0x0F] escape and the opcode byte - opcode map
     3 rather than map 1, {!sse_binop_0f38_alt}'s own map-2 trick repeated for map 3. A one-entry
     table since {!Palignr} is this map's only admitted mnemonic. *)
  let sse_binop_imm_0f3a_codec =
    C.iso_table ~name:"sse-binop-imm-0f3a-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Palignr, 0x0FL);
          (Opcode.Roundps, 0x08L);
          (Opcode.Roundpd, 0x09L);
          (Opcode.Roundss, 0x0AL);
          (Opcode.Roundsd, 0x0BL);
          (Opcode.Blendps, 0x0CL);
          (Opcode.Blendpd, 0x0DL);
          (Opcode.Insertps, 0x21L);
          (Opcode.Pblendw, 0x0EL);
          (Opcode.Dpps, 0x40L);
          (Opcode.Dppd, 0x41L);
          (Opcode.Mpsadbw, 0x42L);
        ]
      (C.field ~width:8 "opcode")

  let sse_binop_imm_0f3a_alt ~label ~priority ~mandatory ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_binop_imm_r_rm { op; reg; rm; imm } ->
               let p = prefixes_of ~width:32 ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), ((), (op, ({ re_reg = reg.num; re_rm = rm }, imm)))))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), ((), (op, (e, imm))))))) ->
           let p = { asz; opsz = false; rex } in
           Some
             (Lowered.Sse_binop_imm_r_rm
                {
                  op;
                  reg = reg_field ~p ~width:128 e.re_reg;
                  rm = rm_of ~p ~width:128 e.re_rm;
                  imm;
                }))
         C.(
           asz_codec
           ** const ~width:8 (Int64.of_int mandatory)
           ** rex_codec ** const ~width:8 0x0FL ** const ~width:8 0x3AL ** opcode_codec ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  (* [pinsrb]/[pinsrd]/[pextrb]/[pextrd]/[extractps] ({!Opcode.Pinsrb}'s own doc comment):
     {!sse_binop_imm_0f3a_alt}'s exact layout with [rm] decoded at width 32 (a GPR32 or memory
     operand) rather than 128, {!sse_pinsrw_alt}'s own class split. Insert and extract share it:
     both keep the xmm operand in the ModR/M [reg] field. *)
  let sse_gpr_imm_0f3a_codec =
    C.iso_table ~name:"sse-gpr-imm-0f3a-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Pextrb, 0x14L);
          (Opcode.Pextrd, 0x16L);
          (Opcode.Extractps, 0x17L);
          (Opcode.Pinsrb, 0x20L);
          (Opcode.Pinsrd, 0x22L);
        ]
      (C.field ~width:8 "opcode")

  let sse_gpr_imm_0f3a_alt ~label ~priority ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_binop_imm_r_rm { op; reg; rm; imm } ->
               let p = prefixes_of ~width:32 ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), ((), (op, ({ re_reg = reg.num; re_rm = rm }, imm)))))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), ((), (op, (e, imm))))))) ->
           let p = { asz; opsz = false; rex } in
           Some
             (Lowered.Sse_binop_imm_r_rm
                { op; reg = reg_field ~p ~width:128 e.re_reg; rm = rm_of ~p ~width:32 e.re_rm; imm }))
         C.(
           asz_codec ** const ~width:8 0x66L ** rex_codec ** const ~width:8 0x0FL
           ** const ~width:8 0x3AL ** opcode_codec ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  (* [pinsrw $imm8, gpr32/m16, xmm] ([66 0F C4 /r ib], {!Opcode.Pinsrw}'s own doc comment):
     {!Lowered.Sse_binop_imm_r_rm}'s first cross-register-class user - [reg] is the xmm
     destination (decoded at width 128, as with every other member of this shape), but [rm] is a
     GPR32 or 16-bit memory operand rather than xmm (decoded at width 32), the same way
     {!Lowered.Cvtsi2f_r_rm}'s own [rm] crosses classes. A single mandatory-66, fixed-opcode entry
     (no table): {!Pinsrw} has no other mandatory-prefix or opcode-byte sibling at this shape. *)
  let sse_pinsrw_alt ~label ~priority =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_binop_imm_r_rm { op = Opcode.Pinsrw; reg; rm; imm } ->
               let p = prefixes_of ~width:32 ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), ({ re_reg = reg.num; re_rm = rm }, imm)))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), (e, imm))))) ->
           let p = { asz; opsz = false; rex } in
           Some
             (Lowered.Sse_binop_imm_r_rm
                {
                  op = Opcode.Pinsrw;
                  reg = reg_field ~p ~width:128 e.re_reg;
                  rm = rm_of ~p ~width:32 e.re_rm;
                  imm;
                }))
         C.(
           asz_codec ** const ~width:8 0x66L ** rex_codec ** const ~width:16 0x0FC4L ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  (* [pextrw $imm8, xmm, gpr32] ([66 0F C5 /r ib], {!Opcode.Pextrw}'s own doc comment):
     {!Sse_pinsrw_alt}'s store-direction mirror - [reg] is the GPR32 destination (width 32), [rm]
     is an xmm source (width 128) restricted to a register by construction (no lowering arm ever
     builds a [Rm.Mem] here, matching real GNU as's own routing of that spelling elsewhere). *)
  let sse_pextrw_alt ~label ~priority =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_binop_imm_r_rm { op = Opcode.Pextrw; reg; rm; imm } ->
               let p = prefixes_of ~width:32 ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), ({ re_reg = reg.num; re_rm = rm }, imm)))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), (e, imm))))) ->
           let p = { asz; opsz = false; rex } in
           Some
             (Lowered.Sse_binop_imm_r_rm
                {
                  op = Opcode.Pextrw;
                  reg = reg_field ~p ~width:32 e.re_reg;
                  rm = rm_of ~p ~width:128 e.re_rm;
                  imm;
                }))
         C.(
           asz_codec ** const ~width:8 0x66L ** rex_codec ** const ~width:16 0x0FC5L ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  (* [movmskpd]/[pmovmskb] ([66 0F 50 /r] / [66 0F D7 /r], {!Opcode.Movmskps}'s own doc comment):
     {!Sse_pextrw_alt}'s own field roles ([reg] a GPR32 at width 32, [rm] an xmm register at
     width 128) minus the trailing immediate, generalized over an opcode table the way
     {!sse_binop_alt} is - both members share the mandatory-66 prefix. *)
  let sse_movmsk_66_codec =
    C.iso_table ~name:"sse-movmsk-66-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Movmskpd, 0x50L); (Opcode.Pmovmskb, 0xD7L) ]
      (C.field ~width:8 "opcode")

  let sse_movmsk_alt ~label ~priority ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_binop_r_rm { op; reg; rm } ->
               let p = prefixes_of ~width:32 ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), (op, { re_reg = reg.num; re_rm = rm })))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), (op, e))))) ->
           let p = { asz; opsz = false; rex } in
           Some
             (Lowered.Sse_binop_r_rm
                { op; reg = reg_field ~p ~width:32 e.re_reg; rm = rm_of ~p ~width:128 e.re_rm }))
         C.(
           asz_codec ** const ~width:8 0x66L ** rex_codec ** const ~width:8 0x0FL ** opcode_codec
           ** rm_codec))

  (* [movmskps] ([0F 50 /r], no mandatory prefix): {!Sse_movmsk_alt}'s mandatory-prefix-free
     sibling, {!sse_binop_none_alt}'s own [prefixes_codec] shape with the same width fix. A
     single-entry table since {!Movmskps} has no other opcode-byte sibling at this shape. *)
  let sse_movmsk_none_codec =
    C.iso_table ~name:"sse-movmsk-none-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Movmskps, 0x50L) ]
      (C.field ~width:8 "opcode")

  let sse_movmsk_none_alt ~label ~priority ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_binop_r_rm { op; reg; rm } ->
               Some
                 ( prefixes_of ~width:32 ~reg:reg.num ~rm,
                   ((), (op, { re_reg = reg.num; re_rm = rm })) )
           | _ -> None)
         ~decode:(fun (p, ((), (op, e))) ->
           Some
             (Lowered.Sse_binop_r_rm
                { op; reg = reg_field ~p ~width:32 e.re_reg; rm = rm_of ~p ~width:128 e.re_rm }))
         C.(prefixes_codec ** const ~width:8 0x0FL ** opcode_codec ** rm_codec))

  (* [cvtsi2sd]/[cvtsi2ss] ([0F 2A]), and - via [~opcode16] - {!Movd}'s own load direction
     ([0F 6E]): the one place [~width] threaded into {!prefixes_of} is a real GPR width
     rather than the [32] REX.W-clear sentinel the rest of this section uses - [rm] is the
     GPR/memory operand here, and its width is exactly what CompCert's [q] suffix already
     pinned down in {!simplify_instruction} (or, for [movd]/[movq], what the frontend's own
     mnemonic dispatch already fixed - {!Movd}'s own doc comment). Generalized over [~opcode16]
     the same way {!sse_binop_alt} is generalized over [~opcode_codec]: [cvtsi2sd]/[cvtsi2ss]
     share one fixed opcode across both mandatory-prefix groups, so a per-mnemonic table would be
     one entry each - not worth building until a second [0F 2A]-family opcode exists. *)
  let sse_cvtsi2f_alt ~label ~priority ~mandatory ~op ~opcode16 =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Cvtsi2f_r_rm { op = op'; width; reg; rm } when op' = op ->
               let p = prefixes_of ~width ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), { re_reg = reg.num; re_rm = rm }))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), e)))) ->
           let p = { asz; opsz = false; rex } in
           let width = width_of_prefixes p in
           Some
             (Lowered.Cvtsi2f_r_rm
                { op; width; reg = reg_field ~p ~width:128 e.re_reg; rm = rm_of ~p ~width e.re_rm }))
         C.(
           asz_codec
           ** const ~width:8 (Int64.of_int mandatory)
           ** rex_codec ** const ~width:16 opcode16 ** rm_codec))

  (* [cvttsd2si] ([0F 2C], [F2] only - [cvttss2si]/[F3] is unevidenced by
     this corpus and not built). Mirrors {!sse_cvtsi2f_alt} with [reg]/[rm]'s
     roles swapped: [reg] is the GPR (its width drives REX.W here), [rm] is
     xmm or memory. *)
  let sse_cvtf2i_alt ~label ~priority =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Cvtf2i_r_rm { width; reg; rm } ->
               let p = prefixes_of ~width ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), { re_reg = reg.num; re_rm = rm }))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), e)))) ->
           let p = { asz; opsz = false; rex } in
           let width = width_of_prefixes p in
           Some
             (Lowered.Cvtf2i_r_rm
                { width; reg = reg_field ~p ~width e.re_reg; rm = rm_of ~p ~width:128 e.re_rm }))
         C.(asz_codec ** const ~width:8 0xF2L ** rex_codec ** const ~width:16 0x0F2CL ** rm_codec))

  (* [movd]/[movq] store direction ([66 0F 7E /r]): {!sse_cvtsi2f_alt}'s own field layout
     (ModR/M-reg=xmm at [width:128], ModR/M-rm=gpr-or-mem at the real GPR [width] that drives
     REX.W) with [reg]/[rm]'s construction order swapped to match {!Lowered.Movd_rm_r} instead of
     {!Lowered.Cvtsi2f_r_rm} - the encode/decode byte layout is otherwise identical, confirmed
     against real GNU as ({!Movd}'s own doc comment has the exact bytes). A single mandatory-66,
     fixed-opcode entry, mirroring {!sse_cvtf2i_alt}'s own unparametrized singleton shape rather
     than {!sse_cvtsi2f_alt}'s [~mandatory]/[~op]/[~opcode16] generality, since [movd]/[movq] have
     no other mandatory-prefix or opcode-byte sibling at this shape the way [cvtsi2sd]/[cvtsi2ss]
     do. *)
  let sse_movd_store_alt ~label ~priority =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Movd_rm_r { op = _; width; rm; reg } ->
               let p = prefixes_of ~width ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), { re_reg = reg.num; re_rm = rm }))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), e)))) ->
           let p = { asz; opsz = false; rex } in
           let width = width_of_prefixes p in
           Some
             (Lowered.Movd_rm_r
                {
                  op = Opcode.Movd;
                  width;
                  reg = reg_field ~p ~width:128 e.re_reg;
                  rm = rm_of ~p ~width e.re_rm;
                }))
         C.(asz_codec ** const ~width:8 0x66L ** rex_codec ** const ~width:16 0x0F7EL ** rm_codec))

  (* {3 x86 VEX (x86 vector extensions)}

     The two-byte VEX prefix ([0xC5]), built for {!Opcode.Vaddsd}'s
     scalar-double [VEX.LIG.F2.0F.WIG] group and {!Opcode.Vaddss}'s
     scalar-single [VEX.LIG.F3.0F.WIG] sibling group: no [asz_codec]/
     [rex_codec] at all - VEX and REX never coexist, VEX's second byte
     carries what REX would have (here, only R) plus [vvvv]/[L]/[pp], which
     is why this is not built as one more [sse_binop_alt]-style
     mandatory-prefix group. The second byte is a single opaque 8-bit field
     rather than a record of sub-fields the way [rm_enc] is: nothing else in
     this codec needs to name its pieces, since every mnemonic in a given
     [pp] group shares one fixed [L]/[pp] (confirmed against real GNU as:
     [c5 f3 58 c2] for [vaddsd %xmm2, %xmm1, %xmm0], [c5 e3 58 ef] for
     [vaddsd %xmm7, %xmm3, %xmm5], [c5 f3 58 00] for [vaddsd (%rax), %xmm1,
     %xmm0], [c5 e3 58 6c 8b 08] for [vaddsd 8(%rbx,%rcx,4), %xmm3, %xmm5]
     ([pp = 3] throughout - identical ModR/M/SIB bytes to legacy SSE's own
     memory encoding, only the leading prefix bytes differ), and
     [c5 f2 58 c2]/[c5 e2 5c ef] for [vaddss %xmm2, %xmm1, %xmm0]/
     [vsubss %xmm7, %xmm3, %xmm5] ([pp = 2] - only [pp] itself differs from
     the [F2] group, same [R]/[vvvv] bit positions) - on both
     [x86_64-linux-gnu-as] and [i686-linux-gnu-as] 2.44).

     [dst] (the ModR/M reg field) and [src1] ([vvvv]) reuse [rm_codec]'s and
     [reg_at]'s existing machinery; [src2] (the ModR/M r/m field, register or
     memory) reuses [rm_codec] directly for the same reason - VEX's ModR/M
     and SIB bytes are byte-for-byte identical to legacy's, only the prefix
     bytes preceding them differ - but with no [rm_of]/[extend_rex] pass,
     since those assume a REX byte that does not exist here: a register
     [src2] or a memory [src2]'s base/index must already fit in 3 bits
     unextended, the [vex_rm_ok] guard below rejects anything that would
     need a REX.X/B-equivalent bit this two-byte-prefix slice does not have
     (confirmed against real GNU as: an r8-r15 memory base there is
     automatically re-encoded with the three-byte VEX prefix instead,
     e.g. [vaddsd (%r8), %xmm1, %xmm0] -> [c4 c1 73 58 00], which this slice
     does not build). *)
  let vex_scalar_f2_codec =
    C.iso_table ~name:"vex-scalar-f2-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Vaddsd, 0x58L);
          (Opcode.Vsubsd, 0x5CL);
          (Opcode.Vmulsd, 0x59L);
          (Opcode.Vdivsd, 0x5EL);
          (Opcode.Vmaxsd, 0x5FL);
          (Opcode.Vminsd, 0x5DL);
          (Opcode.Vsqrtsd, 0x51L);
        ]
      (C.field ~width:8 "opcode")

  let vex_scalar_f3_codec =
    C.iso_table ~name:"vex-scalar-f3-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Vaddss, 0x58L);
          (Opcode.Vsubss, 0x5CL);
          (Opcode.Vmulss, 0x59L);
          (Opcode.Vdivss, 0x5EL);
          (Opcode.Vmaxss, 0x5FL);
          (Opcode.Vminss, 0x5DL);
          (Opcode.Vsqrtss, 0x51L);
        ]
      (C.field ~width:8 "opcode")

  (* [pp = 0] (no mandatory prefix) - {!Vaddps}'s packed-single group, the same
     four opcode bytes as {!vex_scalar_f2_codec}/{!vex_scalar_f3_codec}. *)
  let vex_scalar_none_codec =
    C.iso_table ~name:"vex-scalar-none-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Vaddps, 0x58L);
          (Opcode.Vsubps, 0x5CL);
          (Opcode.Vmulps, 0x59L);
          (Opcode.Vdivps, 0x5EL);
          (Opcode.Vandps, 0x54L);
          (Opcode.Vandnps, 0x55L);
          (Opcode.Vorps, 0x56L);
          (Opcode.Vxorps, 0x57L);
          (Opcode.Vmaxps, 0x5FL);
          (Opcode.Vminps, 0x5DL);
          (Opcode.Vunpcklps, 0x14L);
          (Opcode.Vunpckhps, 0x15L);
        ]
      (C.field ~width:8 "opcode")

  (* [pp = 1] (mandatory [66]) - {!Vaddpd}'s packed-double group. *)
  let vex_scalar_66_codec =
    C.iso_table ~name:"vex-scalar-66-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Vaddpd, 0x58L);
          (Opcode.Vsubpd, 0x5CL);
          (Opcode.Vmulpd, 0x59L);
          (Opcode.Vdivpd, 0x5EL);
          (Opcode.Vandpd, 0x54L);
          (Opcode.Vandnpd, 0x55L);
          (Opcode.Vorpd, 0x56L);
          (Opcode.Vxorpd, 0x57L);
          (Opcode.Vmaxpd, 0x5FL);
          (Opcode.Vminpd, 0x5DL);
          (Opcode.Vunpcklpd, 0x14L);
          (Opcode.Vunpckhpd, 0x15L);
          (Opcode.Vpunpcklqdq, 0x6CL);
          (Opcode.Vpunpckhqdq, 0x6DL);
          (Opcode.Vpunpcklbw, 0x60L);
          (Opcode.Vpunpckhbw, 0x68L);
          (Opcode.Vpunpcklwd, 0x61L);
          (Opcode.Vpunpckhwd, 0x69L);
          (Opcode.Vpunpckldq, 0x62L);
          (Opcode.Vpunpckhdq, 0x6AL);
          (Opcode.Vpaddb, 0xFCL);
          (Opcode.Vpaddw, 0xFDL);
          (Opcode.Vpaddd, 0xFEL);
          (Opcode.Vpaddq, 0xD4L);
          (Opcode.Vpsubb, 0xF8L);
          (Opcode.Vpsubw, 0xF9L);
          (Opcode.Vpsubd, 0xFAL);
          (Opcode.Vpsubq, 0xFBL);
          (Opcode.Vpcmpeqb, 0x74L);
          (Opcode.Vpcmpeqw, 0x75L);
          (Opcode.Vpcmpeqd, 0x76L);
          (Opcode.Vpcmpgtb, 0x64L);
          (Opcode.Vpcmpgtw, 0x65L);
          (Opcode.Vpcmpgtd, 0x66L);
          (Opcode.Vpacksswb, 0x63L);
          (Opcode.Vpackssdw, 0x6BL);
          (Opcode.Vpackuswb, 0x67L);
          (Opcode.Vpand, 0xDBL);
          (Opcode.Vpandn, 0xDFL);
          (Opcode.Vpor, 0xEBL);
          (Opcode.Vpminub, 0xDAL);
          (Opcode.Vpmaxub, 0xDEL);
          (Opcode.Vpminsw, 0xEAL);
          (Opcode.Vpmaxsw, 0xEEL);
          (Opcode.Vpmullw, 0xD5L);
          (Opcode.Vpmulhw, 0xE5L);
          (Opcode.Vpmulhuw, 0xE4L);
          (Opcode.Vpavgb, 0xE0L);
          (Opcode.Vpavgw, 0xE3L);
          (Opcode.Vpsadbw, 0xF6L);
          (Opcode.Vpsllw, 0xF1L);
          (Opcode.Vpslld, 0xF2L);
          (Opcode.Vpsllq, 0xF3L);
          (Opcode.Vpsrlw, 0xD1L);
          (Opcode.Vpsrld, 0xD2L);
          (Opcode.Vpsrlq, 0xD3L);
          (Opcode.Vpsraw, 0xE1L);
          (Opcode.Vpsrad, 0xE2L);
        ]
      (C.field ~width:8 "opcode")

  (* Whether [rm] fits this two-byte-prefix slice's ModR/M/SIB, which has no
     REX.X/B-equivalent extension bit: a register [rm] must be xmm0-7, and a
     memory [rm]'s base/index (if present) must be among the low 8 GPRs. A
     RIP base ([num = -1]) never consumes the restricted 3-bit field, so it
     always passes. *)
  let vex_rm_ok = function
    | Rm.Reg (r : Reg.t) -> r.num < 8
    | Rm.Mem (m : Mem.t) -> (
        let reg_ok (r : Reg.t) = r.num < 8 in
        (match m.Mem.base with Some b -> reg_ok b | None -> true)
        && match m.Mem.index with Some idx -> reg_ok idx | None -> true)

  (* [pp] is the raw 2-bit VEX [pp] field (3 for the [F2] group, 2 for [F3]);
     [L] is always 0 (scalar-only, no YMM in this slice), matching every
     mnemonic [~opcode_codec] selects. Generalized over [~pp]/[~opcode_codec]
     the same way {!sse_binop_alt} is generalized over [~mandatory]/
     [~opcode_codec]. *)
  let vex_scalar_rrr_alt ~label ~priority ~pp ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_binop_rr_rm { op; dst; src1; src2 } when vex_rm_ok src2 && dst.width <> 512
             ->
               let r_bit = if dst.num >= 8 then 0 else 1 in
               let vvvv = lnot src1.num land 0xF in
               let l = if dst.width = 256 then 1 else 0 in
               let byte2 = Int64.of_int ((r_bit lsl 7) lor (vvvv lsl 3) lor (l lsl 2) lor pp) in
               Some ((), (byte2, (op, { re_reg = dst.num; re_rm = src2 })))
           | _ -> None)
         ~decode:(fun ((), (byte2, (op, e))) ->
           let b = Int64.to_int byte2 in
           let r_bit = (b lsr 7) land 1 in
           let vvvv = (b lsr 3) land 0xF in
           let width = if (b lsr 2) land 1 = 1 then 256 else 128 in
           let observed_pp = b land 3 in
           if observed_pp <> pp then None
           else
             let dst_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let src1_num = lnot vvvv land 0xF in
             let src2 =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width r) | Rm.Mem _ as m -> m
             in
             Some
               (Lowered.Vex_binop_rr_rm
                  { op; dst = reg_at ~width dst_num; src1 = reg_at ~width src1_num; src2 }))
         C.(const ~width:8 0xC5L ** field ~width:8 "vex-byte2" ** opcode_codec ** rm_codec))

  (* The three-byte VEX prefix ([0xC4], {!Opcode.Vpshufb}'s own doc comment): [0xC4], then [R X B
     mmmmm] (all three extension bits inverted; [mmmmm = 2] selects opcode map 0F38), then [W vvvv
     L pp]. {!vex_scalar_rrr_alt}'s own two-byte layout with one extra byte: [R] moves out of the
     last byte into the middle one and [W = 0] takes its place. [X] and [B] stay 1 (unextended)
     because [src2] is still restricted to registers below 8 ({!vex_rm_ok}). *)
  let vex3_map2_66_codec =
    C.iso_table ~name:"vex3-map2-66-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Vpshufb, 0x00L);
          (Opcode.Vphaddw, 0x01L);
          (Opcode.Vphaddd, 0x02L);
          (Opcode.Vphaddsw, 0x03L);
          (Opcode.Vpmaddubsw, 0x04L);
          (Opcode.Vphsubw, 0x05L);
          (Opcode.Vphsubd, 0x06L);
          (Opcode.Vphsubsw, 0x07L);
          (Opcode.Vpsignb, 0x08L);
          (Opcode.Vpsignw, 0x09L);
          (Opcode.Vpsignd, 0x0AL);
          (Opcode.Vpmulhrsw, 0x0BL);
          (Opcode.Vpmuldq, 0x28L);
          (Opcode.Vpcmpeqq, 0x29L);
          (Opcode.Vpackusdw, 0x2BL);
          (Opcode.Vpcmpgtq, 0x37L);
          (Opcode.Vpminsb, 0x38L);
          (Opcode.Vpminsd, 0x39L);
          (Opcode.Vpminuw, 0x3AL);
          (Opcode.Vpminud, 0x3BL);
          (Opcode.Vpmaxsb, 0x3CL);
          (Opcode.Vpmaxsd, 0x3DL);
          (Opcode.Vpmaxuw, 0x3EL);
          (Opcode.Vpmaxud, 0x3FL);
          (Opcode.Vpmulld, 0x40L);
        ]
      (C.field ~width:8 "opcode")

  let vex3_map2_rrr_alt ~label ~priority ~pp ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_binop_rr_rm { op; dst; src1; src2 } when vex_rm_ok src2 && dst.width <> 512
             ->
               let r_bit = if dst.num >= 8 then 0 else 1 in
               let vvvv = lnot src1.num land 0xF in
               let l = if dst.width = 256 then 1 else 0 in
               let byte1 = Int64.of_int ((r_bit lsl 7) lor 0x40 lor 0x20 lor 2) in
               let byte2 = Int64.of_int ((vvvv lsl 3) lor (l lsl 2) lor pp) in
               Some ((), (byte1, (byte2, (op, { re_reg = dst.num; re_rm = src2 }))))
           | _ -> None)
         ~decode:(fun ((), (byte1, (byte2, (op, e)))) ->
           let b1 = Int64.to_int byte1 and b2 = Int64.to_int byte2 in
           let r_bit = (b1 lsr 7) land 1 in
           let vvvv = (b2 lsr 3) land 0xF in
           let w = (b2 lsr 7) land 1 in
           let width = if (b2 lsr 2) land 1 = 1 then 256 else 128 in
           if b1 land 0x7F <> 0x62 || w <> 0 || b2 land 3 <> pp then None
           else
             let dst_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let src1_num = lnot vvvv land 0xF in
             let src2 =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width r) | Rm.Mem _ as m -> m
             in
             Some
               (Lowered.Vex_binop_rr_rm
                  { op; dst = reg_at ~width dst_num; src1 = reg_at ~width src1_num; src2 }))
         C.(
           const ~width:8 0xC4L ** field ~width:8 "vex3-byte1" ** field ~width:8 "vex3-byte2"
           ** opcode_codec ** rm_codec))

  (* {!Opcode.Vpalignr}'s own doc comment: {!vex3_map2_rrr_alt}'s layout for the [VEX.128.66.0F3A]
     map ([mmmmm = 3]), with {!vex_binop_imm_rrr_alt}'s trailing imm8. *)
  let vex3_map3_66_codec =
    C.iso_table ~name:"vex3-map3-66-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Vpalignr, 0x0FL);
          (Opcode.Vblendps, 0x0CL);
          (Opcode.Vblendpd, 0x0DL);
          (Opcode.Vpblendw, 0x0EL);
          (Opcode.Vroundss, 0x0AL);
          (Opcode.Vroundsd, 0x0BL);
          (Opcode.Vdpps, 0x40L);
          (Opcode.Vdppd, 0x41L);
          (Opcode.Vmpsadbw, 0x42L);
          (Opcode.Vinsertps, 0x21L);
        ]
      (C.field ~width:8 "opcode")

  let vex3_map3_imm_rrr_alt ~label ~priority ~pp ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_binop_imm_rr_rm { op; dst; src1; src2; imm } when vex_rm_ok src2 ->
               let r_bit = if dst.num >= 8 then 0 else 1 in
               let vvvv = lnot src1.num land 0xF in
               let byte1 = Int64.of_int ((r_bit lsl 7) lor 0x40 lor 0x20 lor 3) in
               let byte2 = Int64.of_int ((vvvv lsl 3) lor pp) in
               Some ((), (byte1, (byte2, (op, ({ re_reg = dst.num; re_rm = src2 }, imm)))))
           | _ -> None)
         ~decode:(fun ((), (byte1, (byte2, (op, (e, imm))))) ->
           let b1 = Int64.to_int byte1 and b2 = Int64.to_int byte2 in
           let r_bit = (b1 lsr 7) land 1 in
           let vvvv = (b2 lsr 3) land 0xF in
           let w = (b2 lsr 7) land 1 in
           let l = (b2 lsr 2) land 1 in
           if b1 land 0x7F <> 0x63 || w <> 0 || l <> 0 || b2 land 3 <> pp then None
           else
             let dst_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let src1_num = lnot vvvv land 0xF in
             let src2 =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width:128 r) | Rm.Mem _ as m -> m
             in
             Some
               (Lowered.Vex_binop_imm_rr_rm
                  {
                    op;
                    dst = reg_at ~width:128 dst_num;
                    src1 = reg_at ~width:128 src1_num;
                    src2;
                    imm;
                  }))
         C.(
           const ~width:8 0xC4L ** field ~width:8 "vex3-byte1" ** field ~width:8 "vex3-byte2"
           ** opcode_codec ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  (* {!Opcode.Vpabsb}'s own doc comment: {!vex_unop_alt}'s layout ([vvvv] the literal [1111]) with
     {!vex3_map2_rrr_alt}'s three-byte prefix. *)
  let vex3_map2_unop_codec =
    C.iso_table ~name:"vex3-map2-unop-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Vpabsb, 0x1CL);
          (Opcode.Vpabsw, 0x1DL);
          (Opcode.Vpabsd, 0x1EL);
          (Opcode.Vphminposuw, 0x41L);
          (Opcode.Vptest, 0x17L);
          (Opcode.Vpmovsxbw, 0x20L);
          (Opcode.Vpmovsxbd, 0x21L);
          (Opcode.Vpmovsxbq, 0x22L);
          (Opcode.Vpmovsxwd, 0x23L);
          (Opcode.Vpmovsxwq, 0x24L);
          (Opcode.Vpmovsxdq, 0x25L);
          (Opcode.Vpmovzxbw, 0x30L);
          (Opcode.Vpmovzxbd, 0x31L);
          (Opcode.Vpmovzxbq, 0x32L);
          (Opcode.Vpmovzxwd, 0x33L);
          (Opcode.Vpmovzxwq, 0x34L);
          (Opcode.Vpmovzxdq, 0x35L);
          (Opcode.Vmovntdqa, 0x2AL);
        ]
      (C.field ~width:8 "opcode")

  let vex3_map2_unop_alt ~label ~priority ~pp ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_unop_r_rm { op; dst; src } when vex_rm_ok src ->
               let r_bit = if dst.num >= 8 then 0 else 1 in
               let l = if dst.width = 256 then 1 else 0 in
               let byte1 = Int64.of_int ((r_bit lsl 7) lor 0x40 lor 0x20 lor 2) in
               let byte2 = Int64.of_int ((0xF lsl 3) lor (l lsl 2) lor pp) in
               Some ((), (byte1, (byte2, (op, { re_reg = dst.num; re_rm = src }))))
           | _ -> None)
         ~decode:(fun ((), (byte1, (byte2, (op, e)))) ->
           let b1 = Int64.to_int byte1 and b2 = Int64.to_int byte2 in
           let r_bit = (b1 lsr 7) land 1 in
           let vvvv = (b2 lsr 3) land 0xF in
           let w = (b2 lsr 7) land 1 in
           let width = if (b2 lsr 2) land 1 = 1 then 256 else 128 in
           if b1 land 0x7F <> 0x62 || w <> 0 || vvvv <> 0xF || b2 land 3 <> pp then None
           else
             let dst_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let src =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width r) | Rm.Mem _ as m -> m
             in
             Some (Lowered.Vex_unop_r_rm { op; dst = reg_at ~width dst_num; src }))
         C.(
           const ~width:8 0xC4L ** field ~width:8 "vex3-byte1" ** field ~width:8 "vex3-byte2"
           ** opcode_codec ** rm_codec))

  (* The EVEX prefix ([0x62], AVX-512, {!Opcode.Vaddps}'s own doc comment): [0x62], then [P0 = R X B
     R' 0 mmm] (extension bits inverted; [mmm = 1] selects map 0F), [P1 = W vvvv 1 pp], [P2 = z L'L
     b V' aaa] - here always unmasked ([z = 0], [aaa = 0]), no broadcast/rounding ([b = 0]), 512
     bits ([L'L = 10]), and [R'] = [V'] = 1 (registers below 16). [W] is fixed per group: 0 for the
     [ps] mnemonics, 1 for [pd] (unlike VEX, where the same opcodes are WIG). *)
  let evex_ps_codec =
    C.iso_table ~name:"evex-ps-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Vaddps, 0x58L);
          (Opcode.Vsubps, 0x5CL);
          (Opcode.Vmulps, 0x59L);
          (Opcode.Vdivps, 0x5EL);
          (Opcode.Vmaxps, 0x5FL);
          (Opcode.Vminps, 0x5DL);
          (Opcode.Vunpcklps, 0x14L);
          (Opcode.Vunpckhps, 0x15L);
        ]
      (C.field ~width:8 "opcode")

  let evex_pd_codec =
    C.iso_table ~name:"evex-pd-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Vaddpd, 0x58L);
          (Opcode.Vsubpd, 0x5CL);
          (Opcode.Vmulpd, 0x59L);
          (Opcode.Vdivpd, 0x5EL);
          (Opcode.Vmaxpd, 0x5FL);
          (Opcode.Vminpd, 0x5DL);
          (Opcode.Vunpcklpd, 0x14L);
          (Opcode.Vunpckhpd, 0x15L);
        ]
      (C.field ~width:8 "opcode")

  let evex_binop_rrr_alt ~label ~priority ~pp ~w ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_binop_rr_rm { op; dst; src1; src2 } when dst.width = 512 && vex_rm_ok src2
             ->
               let r_bit = if dst.num >= 8 then 0 else 1 in
               let vvvv = lnot src1.num land 0xF in
               let p0 = Int64.of_int ((r_bit lsl 7) lor 0x40 lor 0x20 lor 0x10 lor 1) in
               let p1 = Int64.of_int ((w lsl 7) lor (vvvv lsl 3) lor 4 lor pp) in
               Some ((), (p0, (p1, (0x48L, (op, { re_reg = dst.num; re_rm = src2 })))))
           | _ -> None)
         ~decode:(fun ((), (p0, (p1, (p2, (op, e))))) ->
           let b0 = Int64.to_int p0 and b1 = Int64.to_int p1 in
           if
             b0 land 0x7F <> 0x71
             || (b1 lsr 7) land 1 <> w
             || b1 land 4 = 0
             || b1 land 3 <> pp
             || p2 <> 0x48L
           then None
           else
             let r_bit = (b0 lsr 7) land 1 in
             let vvvv = (b1 lsr 3) land 0xF in
             let dst_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let src1_num = lnot vvvv land 0xF in
             let src2 =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width:512 r) | Rm.Mem _ as m -> m
             in
             Some
               (Lowered.Vex_binop_rr_rm
                  { op; dst = reg_at ~width:512 dst_num; src1 = reg_at ~width:512 src1_num; src2 }))
         C.(
           const ~width:8 0x62L ** field ~width:8 "evex-p0" ** field ~width:8 "evex-p1"
           ** field ~width:8 "evex-p2" ** opcode_codec ** rm_codec))

  (* [pp = 0] - {!Opcode.Vsqrtps}'s own group: opcode 0x51 does not collide with
     {!vex_scalar_none_codec}'s entries (0x54-0x59/0x5C-0x5F), so this could have been added
     there, but a dedicated table keeps it paired with {!vex_unop_alt} the same way every other
     [Lowered] constructor here has its own codec table(s), not a shared one keyed only by
     opcode-byte disjointness. *)
  let vex_unop_none_codec =
    C.iso_table ~name:"vex-unop-none-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Vsqrtps, 0x51L);
          (Opcode.Vmovaps, 0x28L);
          (Opcode.Vmovups, 0x10L);
          (Opcode.Vcomiss, 0x2FL);
          (Opcode.Vucomiss, 0x2EL);
          (Opcode.Vcvtps2pd, 0x5AL);
          (Opcode.Vcvtdq2ps, 0x5BL);
        ]
      (C.field ~width:8 "opcode")

  (* [pp = 1] (mandatory [66]) - {!Vsqrtps}'s packed-double sibling. *)
  let vex_unop_66_codec =
    C.iso_table ~name:"vex-unop-66-op" ~equal:( = ) ~show:Opcode.name
      ~entries:
        [
          (Opcode.Vsqrtpd, 0x51L);
          (Opcode.Vmovapd, 0x28L);
          (Opcode.Vmovupd, 0x10L);
          (Opcode.Vcomisd, 0x2FL);
          (Opcode.Vucomisd, 0x2EL);
          (Opcode.Vcvtpd2ps, 0x5AL);
          (Opcode.Vmovdqa, 0x6FL);
          (Opcode.Vcvtps2dq, 0x5BL);
        ]
      (C.field ~width:8 "opcode")

  (* [pp = 2] (mandatory [F3]) - {!Vmovdqu}'s own group, the first {!Lowered.Vex_unop_r_rm}
     mnemonic needing a mandatory-[F3] table. *)
  let vex_unop_f3_codec =
    C.iso_table ~name:"vex-unop-f3-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Vmovdqu, 0x6FL); (Opcode.Vcvttps2dq, 0x5BL) ]
      (C.field ~width:8 "opcode")

  (* {!vex_scalar_rrr_alt}'s two-operand sibling for {!Lowered.Vex_unop_r_rm}: no [vvvv]
     operand to derive a field from, so [byte2]'s [vvvv] bits are always the literal [1111]
     ("unused") pattern rather than [lnot src1.num land 0xF]. *)
  let vex_unop_alt ~label ~priority ~pp ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_unop_r_rm { op; dst; src } when vex_rm_ok src ->
               let r_bit = if dst.num >= 8 then 0 else 1 in
               let l = if dst.width = 256 then 1 else 0 in
               let byte2 = Int64.of_int ((r_bit lsl 7) lor (0xF lsl 3) lor (l lsl 2) lor pp) in
               Some ((), (byte2, (op, { re_reg = dst.num; re_rm = src })))
           | _ -> None)
         ~decode:(fun ((), (byte2, (op, e))) ->
           let b = Int64.to_int byte2 in
           let r_bit = (b lsr 7) land 1 in
           let width = if (b lsr 2) land 1 = 1 then 256 else 128 in
           let observed_pp = b land 3 in
           if observed_pp <> pp then None
           else
             let dst_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let src =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width r) | Rm.Mem _ as m -> m
             in
             Some (Lowered.Vex_unop_r_rm { op; dst = reg_at ~width dst_num; src }))
         C.(const ~width:8 0xC5L ** field ~width:8 "vex-byte2" ** opcode_codec ** rm_codec))

  (* [vmovd] load direction ([VEX.128.66.0F.W0 6E /r]): {!vex_unop_alt}'s own two-byte-VEX
     field layout and restriction, generalized to {!Lowered.Vex_movd_r_rm} instead of
     {!Vex_unop_r_rm} - [rm] is a GPR (or memory) at width 32, not xmm ({!Opcode.Vmovd}'s own
     comment explains why no wider variant exists in this project). A single mandatory-66
     ([pp = 1]), fixed-opcode entry, mirroring {!sse_movd_store_alt}'s own unparametrized
     singleton shape, since [vmovd] has no other mandatory-prefix or opcode-byte sibling at this
     shape the way [vsqrtps]/etc. do. *)
  let vex_movd_r_alt ~label ~priority =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_movd_r_rm { op = _; dst; rm } when vex_rm_ok rm ->
               let r_bit = if dst.num >= 8 then 0 else 1 in
               let byte2 = Int64.of_int ((r_bit lsl 7) lor (0xF lsl 3) lor 1) in
               Some ((), (byte2, ((), { re_reg = dst.num; re_rm = rm })))
           | _ -> None)
         ~decode:(fun ((), (byte2, ((), e))) ->
           let b = Int64.to_int byte2 in
           let r_bit = (b lsr 7) land 1 in
           let l = (b lsr 2) land 1 in
           let observed_pp = b land 3 in
           if l <> 0 || observed_pp <> 1 then None
           else
             let dst_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let rm =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width:32 r) | Rm.Mem _ as m -> m
             in
             Some (Lowered.Vex_movd_r_rm { op = Opcode.Vmovd; dst = reg_at ~width:128 dst_num; rm }))
         C.(const ~width:8 0xC5L ** field ~width:8 "vex-byte2" ** const ~width:8 0x6EL ** rm_codec))

  (* {!vex_movd_r_alt}'s store-direction sibling for {!Lowered.Vex_movd_rm_r}: identical field
     layout and restriction, only the opcode byte ([0x7E]) and reg/rm role assignment differ,
     mirroring {!Movd_rm_r}/{!sse_movd_store_alt}'s own load/store split. *)
  let vex_movd_rm_alt ~label ~priority =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_movd_rm_r { op = _; rm; reg } when vex_rm_ok rm ->
               let r_bit = if reg.num >= 8 then 0 else 1 in
               let byte2 = Int64.of_int ((r_bit lsl 7) lor (0xF lsl 3) lor 1) in
               Some ((), (byte2, ((), { re_reg = reg.num; re_rm = rm })))
           | _ -> None)
         ~decode:(fun ((), (byte2, ((), e))) ->
           let b = Int64.to_int byte2 in
           let r_bit = (b lsr 7) land 1 in
           let l = (b lsr 2) land 1 in
           let observed_pp = b land 3 in
           if l <> 0 || observed_pp <> 1 then None
           else
             let reg_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let rm =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width:32 r) | Rm.Mem _ as m -> m
             in
             Some (Lowered.Vex_movd_rm_r { op = Opcode.Vmovd; rm; reg = reg_at ~width:128 reg_num }))
         C.(const ~width:8 0xC5L ** field ~width:8 "vex-byte2" ** const ~width:8 0x7EL ** rm_codec))

  (* [vshufps]/[vshufpd]/[vcmpss]/[vcmpsd]/[vcmpps]/[vcmppd] ([VEX.128.pp.0F.WIG C6|C2 /r ib]): {!vex_scalar_rrr_alt}'s trailing-immediate sibling for
     {!Lowered.Vex_binop_imm_rr_rm} - same [vvvv]-carried [src1]/two-byte-VEX-restricted [src2]
     as {!vex_scalar_rrr_alt}, plus the trailing imm8 {!shld_imm_form}'s legacy shape already
     established the pattern for. Generalized over [~pp]/[~opcode_codec] the same way
     {!vex_scalar_rrr_alt} is: {!Vcmpsd}/{!Vcmpss} each need their own one-entry [pp = 3]/[pp = 2]
     group (opcode 0xC2 has no packed-only-vs-scalar split the way {!Vshufps}'s opcode 0xC6
     does), while {!Vcmppd}/{!Vcmpps} join {!Vshufpd}/{!Vshufps}'s own [pp = 1]/[pp = 0]
     groups. *)
  let vex_binop_imm_rrr_alt ~label ~priority ~pp ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_binop_imm_rr_rm { op; dst; src1; src2; imm } when vex_rm_ok src2 ->
               let r_bit = if dst.num >= 8 then 0 else 1 in
               let vvvv = lnot src1.num land 0xF in
               let byte2 = Int64.of_int ((r_bit lsl 7) lor (vvvv lsl 3) lor pp) in
               Some ((), (byte2, (op, ({ re_reg = dst.num; re_rm = src2 }, imm))))
           | _ -> None)
         ~decode:(fun ((), (byte2, (op, (e, imm)))) ->
           let b = Int64.to_int byte2 in
           let r_bit = (b lsr 7) land 1 in
           let vvvv = (b lsr 3) land 0xF in
           let l = (b lsr 2) land 1 in
           let observed_pp = b land 3 in
           if l <> 0 || observed_pp <> pp then None
           else
             let dst_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let src1_num = lnot vvvv land 0xF in
             let src2 =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width:128 r) | Rm.Mem _ as m -> m
             in
             Some
               (Lowered.Vex_binop_imm_rr_rm
                  {
                    op;
                    dst = reg_at ~width:128 dst_num;
                    src1 = reg_at ~width:128 src1_num;
                    src2;
                    imm;
                  }))
         C.(
           const ~width:8 0xC5L ** field ~width:8 "vex-byte2" ** opcode_codec ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  (* {!Lowered.Vex_shift_imm_rm} ({!Opcode.xmm_shift_ext_opcode}'s own doc comment):
     {!vex_binop_imm_rrr_alt}'s own [vvvv]/imm8 field layout, but [dst] alone supplies [vvvv] -
     there is no [src1] register at all, since the ModR/M reg field is the fixed per-mnemonic
     extension {!xmm_shift_imm_alt}'s legacy sibling uses, not a third register operand, so [R]
     is always the literal "not extended" bit the same way {!vex_unop_alt}'s own [vvvv] is
     always literal. One alt per lane width for the same reason as {!xmm_shift_imm_alt}: the
     operation lives in the ModR/M reg field, not the opcode byte, so [~opcode] cannot be a
     table. Register-only [rm], restricted to xmm0-7 by the two-byte VEX prefix exactly as
     every other [vex_*_alt] here restricts its own r/m operand. *)
  let vex_shift_imm_alt ~label ~priority ~opcode =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_shift_imm_rm { op; dst; rm; imm } when vex_rm_ok rm -> (
               match Opcode.xmm_shift_ext_opcode op with
               | Some (ext, op_byte) when op_byte = opcode ->
                   let vvvv = lnot dst.num land 0xF in
                   let byte2 = Int64.of_int ((1 lsl 7) lor (vvvv lsl 3) lor 1) in
                   Some ((), (byte2, ((), ({ re_reg = ext; re_rm = rm }, imm))))
               | Some _ | None -> None)
           | _ -> None)
         ~decode:(fun ((), (byte2, ((), (e, imm)))) ->
           match e.re_rm with
           | Rm.Mem _ -> None
           | Rm.Reg _ -> (
               let b = Int64.to_int byte2 in
               let r_bit = (b lsr 7) land 1 in
               let vvvv = (b lsr 3) land 0xF in
               let l = (b lsr 2) land 1 in
               let observed_pp = b land 3 in
               if l <> 0 || observed_pp <> 1 || r_bit <> 1 then None
               else
                 match Opcode.of_vex_shift_ext_opcode ~opcode e.re_reg with
                 | None -> None
                 | Some op ->
                     let dst_num = lnot vvvv land 0xF in
                     let rm =
                       match e.re_rm with
                       | Rm.Reg r -> retype ~width:128 r
                       | Rm.Mem _ -> assert false
                     in
                     Some
                       (Lowered.Vex_shift_imm_rm
                          { op; dst = reg_at ~width:128 dst_num; rm = Rm.Reg rm; imm })))
         C.(
           const ~width:8 0xC5L ** field ~width:8 "vex-byte2"
           ** const ~width:8 (Int64.of_int opcode)
           ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  let vex_binop_imm_none_codec =
    C.iso_table ~name:"vex-binop-imm-none-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Vshufps, 0xC6L); (Opcode.Vcmpps, 0xC2L) ]
      (C.field ~width:8 "opcode")

  let vex_binop_imm_66_codec =
    C.iso_table ~name:"vex-binop-imm-66-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Vshufpd, 0xC6L); (Opcode.Vcmppd, 0xC2L) ]
      (C.field ~width:8 "opcode")

  let vex_binop_imm_f2_codec =
    C.iso_table ~name:"vex-binop-imm-f2-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Vcmpsd, 0xC2L) ]
      (C.field ~width:8 "opcode")

  let vex_binop_imm_f3_codec =
    C.iso_table ~name:"vex-binop-imm-f3-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Vcmpss, 0xC2L) ]
      (C.field ~width:8 "opcode")

  (* {!vex_unop_alt}'s trailing-immediate sibling for {!Lowered.Vex_unop_imm_r_rm}
     ([vpshufd]/[vpshuflw]/[vpshufhw] only): no [vvvv] operand, like {!vex_unop_alt},
     plus the trailing imm8 {!vex_binop_imm_rrr_alt} already established the pattern for. *)
  let vex_unop_imm_alt ~label ~priority ~pp ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_unop_imm_r_rm { op; dst; src; imm } when vex_rm_ok src ->
               let r_bit = if dst.num >= 8 then 0 else 1 in
               let byte2 = Int64.of_int ((r_bit lsl 7) lor (0xF lsl 3) lor pp) in
               Some ((), (byte2, (op, ({ re_reg = dst.num; re_rm = src }, imm))))
           | _ -> None)
         ~decode:(fun ((), (byte2, (op, (e, imm)))) ->
           let b = Int64.to_int byte2 in
           let r_bit = (b lsr 7) land 1 in
           let l = (b lsr 2) land 1 in
           let observed_pp = b land 3 in
           if l <> 0 || observed_pp <> pp then None
           else
             let dst_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let src =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width:128 r) | Rm.Mem _ as m -> m
             in
             Some (Lowered.Vex_unop_imm_r_rm { op; dst = reg_at ~width:128 dst_num; src; imm }))
         C.(
           const ~width:8 0xC5L ** field ~width:8 "vex-byte2" ** opcode_codec ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  let vex_unop_imm_66_codec =
    C.iso_table ~name:"vex-unop-imm-66-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Vpshufd, 0x70L) ]
      (C.field ~width:8 "opcode")

  let vex_unop_imm_f2_codec =
    C.iso_table ~name:"vex-unop-imm-f2-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Vpshuflw, 0x70L) ]
      (C.field ~width:8 "opcode")

  let vex_unop_imm_f3_codec =
    C.iso_table ~name:"vex-unop-imm-f3-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Vpshufhw, 0x70L) ]
      (C.field ~width:8 "opcode")

  (* [vpinsrw $imm8, gpr32/m16, src1, dst] ([VEX.128.66.0F.WIG C4 /r ib], {!Opcode.Vpinsrw}'s own
     doc comment): {!Lowered.Vex_binop_imm_rr_rm}'s own cross-register-class member - [src2] is a
     GPR32 (or memory) source rather than xmm (decoded at width 32), the VEX-and-immediate-carrying
     sibling of {!Vex_movd_r_rm}'s own GPR-crossing shape. A single mandatory-66 ([pp = 1]),
     fixed-opcode entry, mirroring {!Vex_movd_r_alt}'s own unparametrized singleton shape. *)
  let vex_pinsrw_alt ~label ~priority =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_binop_imm_rr_rm { op = Opcode.Vpinsrw; dst; src1; src2; imm }
             when vex_rm_ok src2 ->
               let r_bit = if dst.num >= 8 then 0 else 1 in
               let vvvv = lnot src1.num land 0xF in
               let byte2 = Int64.of_int ((r_bit lsl 7) lor (vvvv lsl 3) lor 1) in
               Some ((), (byte2, ((), ({ re_reg = dst.num; re_rm = src2 }, imm))))
           | _ -> None)
         ~decode:(fun ((), (byte2, ((), (e, imm)))) ->
           let b = Int64.to_int byte2 in
           let r_bit = (b lsr 7) land 1 in
           let vvvv = (b lsr 3) land 0xF in
           let l = (b lsr 2) land 1 in
           let observed_pp = b land 3 in
           if l <> 0 || observed_pp <> 1 then None
           else
             let dst_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let src1_num = lnot vvvv land 0xF in
             let src2 =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width:32 r) | Rm.Mem _ as m -> m
             in
             Some
               (Lowered.Vex_binop_imm_rr_rm
                  {
                    op = Opcode.Vpinsrw;
                    dst = reg_at ~width:128 dst_num;
                    src1 = reg_at ~width:128 src1_num;
                    src2;
                    imm;
                  }))
         C.(
           const ~width:8 0xC5L ** field ~width:8 "vex-byte2" ** const ~width:8 0xC4L ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  (* [vpextrw $imm8, xmm, gpr32] ([VEX.128.66.0F.WIG C5 /r ib], {!Opcode.Vpextrw}'s own doc
     comment): {!Lowered.Vex_unop_imm_r_rm}'s own cross-register-class member - [dst] is a GPR32
     rather than xmm (decoded at width 32), the VEX-and-immediate-carrying sibling of
     {!Vex_movd_rm_r}'s own GPR-crossing shape. Register-only [src] by construction (no lowering
     arm ever builds a [Rm.Mem] here). *)
  let vex_pextrw_alt ~label ~priority =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_unop_imm_r_rm { op = Opcode.Vpextrw; dst; src; imm } when vex_rm_ok src ->
               let r_bit = if dst.num >= 8 then 0 else 1 in
               let byte2 = Int64.of_int ((r_bit lsl 7) lor (0xF lsl 3) lor 1) in
               Some ((), (byte2, ((), ({ re_reg = dst.num; re_rm = src }, imm))))
           | _ -> None)
         ~decode:(fun ((), (byte2, ((), (e, imm)))) ->
           let b = Int64.to_int byte2 in
           let r_bit = (b lsr 7) land 1 in
           let l = (b lsr 2) land 1 in
           let observed_pp = b land 3 in
           if l <> 0 || observed_pp <> 1 then None
           else
             let dst_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let src =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width:128 r) | Rm.Mem _ as m -> m
             in
             Some
               (Lowered.Vex_unop_imm_r_rm
                  { op = Opcode.Vpextrw; dst = reg_at ~width:32 dst_num; src; imm }))
         C.(
           const ~width:8 0xC5L ** field ~width:8 "vex-byte2" ** const ~width:8 0xC5L ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  (* [vmovmskps]/[vmovmskpd]/[vpmovmskb] ({!Opcode.Vmovmskps}'s own doc comment): {!vex_unop_alt}'s
     own two-byte-VEX field layout and restriction, with [dst] decoded at width 32 (a GPR) rather
     than 128 - the mandatory-prefix-table-generalized VEX sibling of {!Vex_pextrw_alt}'s own
     fixed-opcode cross-register-class shape, minus the immediate. *)
  let vex_movmsk_alt ~label ~priority ~pp ~opcode_codec =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Vex_unop_r_rm { op; dst; src } when vex_rm_ok src ->
               let r_bit = if dst.num >= 8 then 0 else 1 in
               let byte2 = Int64.of_int ((r_bit lsl 7) lor (0xF lsl 3) lor pp) in
               Some ((), (byte2, (op, { re_reg = dst.num; re_rm = src })))
           | _ -> None)
         ~decode:(fun ((), (byte2, (op, e))) ->
           let b = Int64.to_int byte2 in
           let r_bit = (b lsr 7) land 1 in
           let l = (b lsr 2) land 1 in
           let observed_pp = b land 3 in
           if l <> 0 || observed_pp <> pp then None
           else
             let dst_num = (e.re_reg land 7) + if r_bit = 0 then 8 else 0 in
             let src =
               match e.re_rm with Rm.Reg r -> Rm.Reg (retype ~width:128 r) | Rm.Mem _ as m -> m
             in
             Some (Lowered.Vex_unop_r_rm { op; dst = reg_at ~width:32 dst_num; src }))
         C.(const ~width:8 0xC5L ** field ~width:8 "vex-byte2" ** opcode_codec ** rm_codec))

  let vex_movmsk_66_codec =
    C.iso_table ~name:"vex-movmsk-66-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Vmovmskpd, 0x50L); (Opcode.Vpmovmskb, 0xD7L) ]
      (C.field ~width:8 "opcode")

  let vex_movmsk_none_codec =
    C.iso_table ~name:"vex-movmsk-none-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Vmovmskps, 0x50L) ]
      (C.field ~width:8 "opcode")

  (* Zero-/sign-extending move (M5, asm/docs/corpus.md): [0F B6/B7/BE/BF /r].
     No mandatory prefix, so this reuses [prefixes_codec] directly rather than
     the SSE alts' split [asz_codec]/[rex_codec] - there is no third byte to
     splice between them. [reg] (destination, [width]) and [rm] (source,
     [src_width]) are independently widthed exactly as {!sse_cvtsi2f_alt}
     already does for a different mismatched pair; see {!Lowered.Movx_r_rm}'s
     own comment for why. *)
  let movx_alt ~label ~priority ~zero_extend ~src_width ~opcode16 =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Movx_r_rm { zero_extend = ze; src_width = sw; width; reg; rm }
             when Bool.equal ze zero_extend && sw = src_width ->
               Some (prefixes_of ~width ~reg:reg.num ~rm, ((), { re_reg = reg.num; re_rm = rm }))
           | _ -> None)
         ~decode:(fun (rex, ((), e)) ->
           let width = width_of_prefixes rex in
           Some
             (Lowered.Movx_r_rm
                {
                  zero_extend;
                  src_width;
                  width;
                  reg = reg_field ~p:rex ~width e.re_reg;
                  rm = rm_of ~p:rex ~width:src_width e.re_rm;
                }))
         C.(prefixes_codec ** const ~width:16 opcode16 ** rm_codec))

  (* [movslq] ([0x63 /r], M5, asm/docs/corpus.md): REX.W mandatory (a 32-bit
     write already zero-extends, so a REX-less encoding would mean something
     else entirely), hence the hard-coded [~width:64] rather than a value
     threaded from the lowered form the way {!movx_alt} threads [width]. *)
  let movsxd_alt ~label ~priority =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Movsxd_r_rm { reg; rm } ->
               Some (prefixes_of ~width:64 ~reg:reg.num ~rm, ((), { re_reg = reg.num; re_rm = rm }))
           | _ -> None)
         ~decode:(fun (rex, ((), e)) ->
           Some
             (Lowered.Movsxd_r_rm
                { reg = reg_field ~p:rex ~width:64 e.re_reg; rm = rm_of ~p:rex ~width:32 e.re_rm }))
         C.(prefixes_codec ** const ~width:8 0x63L ** rm_codec))

  (* [0xD9|0xDD /ext] ({!Lowered.Fpu_mem} - [fldl]/[fstpl]/[fstps]): opcode
     plus ModR/M-reg extension, structurally identical to {!Unary_rm}'s
     Group-3 shape (a fixed [prefixes_codec ** opcode ** rm_codec], no
     immediate) - just a disjoint opcode byte and extension table, and one
     instance per mnemonic rather than an [Opcode.of_ext]-style lookup
     shared across every ALU op, since [ext = 3] means two different
     mnemonics here depending on the opcode byte ([fstpl] vs [fstps]). *)
  let fpu_mem_form ~label ~priority ~opcode_byte ~ext ~op =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Fpu_mem { op = o; mem } when o = op ->
               let rm = Rm.Mem mem in
               Some (prefixes_of ~width:32 ~reg:ext ~rm, ((), { re_reg = ext; re_rm = rm }))
           | _ -> None)
         ~decode:(fun (p, ((), e)) ->
           if e.re_reg <> ext then None
           else
             match rm_of ~p ~width:32 e.re_rm with
             | Rm.Mem mem -> Some (Lowered.Fpu_mem { op; mem })
             | Rm.Reg _ -> None)
         C.(prefixes_codec ** const ~width:8 (Int64.of_int opcode_byte) ** rm_codec))

  let fadd_st0_x87_form =
    C.alt ~label:X86_x87.fadd_st0.label ~priority:X86_x87.fadd_st0.priority
      (C.iso_fun ~name:X86_x87.fadd_st0.label
         ~encode:(function
           | Lowered.Fadd_st0_x87 { src }
             when src.Reg.width = 80 && src.Reg.num >= 0 && src.Reg.num <= 7 ->
               Some ((), ((), ((), Int64.of_int src.Reg.num)))
           | _ -> None)
         ~decode:(fun ((), ((), ((), src))) ->
           Some (Lowered.Fadd_st0_x87 { src = reg_at ~width:80 (Int64.to_int src) }))
         C.(const ~width:8 0xD8L ** const ~width:2 3L ** const ~width:3 0L ** field ~width:3 "st"))

  (* [imull $10000,%ebx] / [imulq $56,%rax] ([0x69 id] / [0x6B ib], M5,
     asm/docs/corpus.md): the same short-immediate-first priority discipline
     as {!alu_form} - GAS picks [6B] whenever the immediate fits a sign-
     extended byte, and getting the order wrong would differ from every
     other assembler by three bytes per instruction, exactly as that
     comment explains. *)
  let imul_imm_form ~label ~priority ~opcode_byte ~imm_width =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Imul_r_rm_imm { width; reg; rm; imm } ->
               (* [imm] itself, not just the fits-check, must use the
                  width-reduced value: the trailing [le ~signedness:Signed]
                  field below independently re-validates whatever value
                  reaches it, so passing the raw magnitude through would
                  still fail that field's own range check even once this
                  guard says the rung applies. *)
               let imm = to_width_signed ~width imm in
               let fits = if imm_width = 8 then fits_s8 imm else fits_s32 imm in
               if not fits then None
               else
                 Some
                   ( prefixes_of ~width ~reg:reg.num ~rm,
                     ((), ({ re_reg = reg.num; re_rm = rm }, imm)) )
           | _ -> None)
         ~decode:(fun (rex, ((), (e, imm))) ->
           let width = width_of_prefixes rex in
           Some
             (Lowered.Imul_r_rm_imm
                {
                  width;
                  reg = reg_field ~p:rex ~width e.re_reg;
                  rm = rm_of ~p:rex ~width e.re_rm;
                  imm;
                }))
         C.(
           prefixes_codec
           ** const ~width:8 (Int64.of_int opcode_byte)
           ** rm_codec
           ** le ~signedness:C.Signed ~width:imm_width "imm"))

  (* [0F A4 /r ib] ({!Lowered.Shld_imm_rm} - [shldl $6,%ecx,%eax], M5,
     asm/docs/corpus.md): a two-byte opcode plus trailing imm8 like
     {!imul_imm_form}'s, but the ModR/M reg field here is a genuine register
     operand (the bit-supplying source), so it is threaded through
     [reg_field] on decode exactly as {!movx_alt}'s is, not looked up in an
     extension table. The count is unsigned (0-31/0-63), not sign-extended -
     the same convention {!shift_imm_rm}'s trailing imm8 already uses. *)
  let shld_imm_form ~label ~priority =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Shld_imm_rm { width; reg; rm; imm } ->
               Some
                 (prefixes_of ~width ~reg:reg.num ~rm, ((), ({ re_reg = reg.num; re_rm = rm }, imm)))
           | _ -> None)
         ~decode:(fun (rex, ((), (e, imm))) ->
           let width = width_of_prefixes rex in
           Some
             (Lowered.Shld_imm_rm
                {
                  width;
                  reg = reg_field ~p:rex ~width e.re_reg;
                  rm = rm_of ~p:rex ~width e.re_rm;
                  imm;
                }))
         C.(
           prefixes_codec ** const ~width:16 0x0FA4L ** rm_codec
           ** le ~signedness:C.Unsigned ~width:8 "imm8"))

  (* Unlike {!imul_imm_form}'s [imm] above, this one is a [Disp.t]: the imm8
     rung can never carry a symbol (gcc's `addq $sym,%rax` never fits a
     sign-extended byte anyway), so it is wrapped in [const_disp] purely for
     the tuple shape both rungs of the ladder must share; the imm32 rung
     reuses {!sym_imm32}, {!Mov_r_imm}'s own fixup field, keeping this
     alt's prior sign-extending decode exactly (unlike [Mov_r_imm]'s
     unsigned one). [width <> 8]: neither rung's
     opcode has an 8-bit reading, so without the guard a width-8
     {!Lowered.Alu_rm_imm} would silently encode through the [0x83] rung as
     if its register number named a 32/64-bit register instead - see
     {!alu_form_byte}'s own comment for the real byte-operand opcode. *)
  let alu_form ~label ~priority ~opcode_byte ~imm_width =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Alu_rm_imm { ext; width; rm; imm } when width <> 8 ->
               (* [imm] itself, not just the fits-check, must carry the
                  width-reduced value onward: the imm8 rung's [const_disp]
                  ([le ~signedness:Signed ~width:8]) independently
                  re-validates whatever [Disp.Const] reaches it, so passing
                  the raw magnitude through would still fail that field's own
                  range check even once this guard says the rung applies -
                  the imm32 rung's [Fixup]-based [sym_imm32] never checks
                  range at all, so reducing here changes nothing for it. *)
               let imm =
                 match imm with
                 | Disp.Const v -> Disp.Const (to_width_signed ~width v)
                 | Disp.Sym _ as s -> s
               in
               let fits =
                 match imm with
                 | Disp.Const v -> if imm_width = 8 then fits_s8 v else fits_s32 v
                 | Disp.Sym _ -> imm_width = 32
               in
               if not fits then None
               else Some (prefixes_of_ext ~width ~rm, ((), ({ re_reg = ext; re_rm = rm }, imm)))
           | _ -> None)
         ~decode:(fun (rex, ((), (e, imm))) ->
           match Opcode.of_ext e.re_reg with
           | None -> None
           | Some _ ->
               let width = width_of_prefixes rex in
               Some
                 (Lowered.Alu_rm_imm
                    { ext = e.re_reg; width; rm = rm_of ~p:rex ~width e.re_rm; imm }))
         C.(
           prefixes_codec
           ** const ~width:8 (Int64.of_int opcode_byte)
           ** rm_codec
           **
           if imm_width = 8 then const_disp ~name:"imm" (le ~signedness:C.Signed ~width:8 "imm")
           else sym_imm32 ~kind:Abs32 ~signedness:C.Signed))

  (* [ext<<3 | 5] ib/id ({!Lowered.Alu_rm_imm} with the r/m register being
     exactly the accumulator, %al/%ax/%eax/%rax - register number 0): a
     shorter accumulator-only encoding real [as] always prefers over the
     general ModR/M form whenever the immediate does not fit the imm8 rung
     (M5, asm/docs/corpus.md - the corpus's own [addq $sym, %rax], a
     previously documented, deliberately-left-open byte mismatch: this
     encoder had no accumulator-specific alternative at all, so it always
     fell through to the longer ModR/M form; now fixed). No ModR/M or SIB
     byte at all - the destination is implied by the opcode, not encoded -
     so like {!push_imm_form} this is "opcode plus bare immediate", except
     the opcode's own low 3 bits are the fixed pattern [101] and its next 3
     bits carry the same [ext] {!alu_form}'s ModR/M reg field otherwise
     would ([add]=0/[or]=1/[adc]=2/[and]=4/[sub]=5/[xor]=6/[cmp]=7 - the same
     domain {!Opcode.of_ext} already recognizes, [sbb]=3 excluded since it
     never reaches this form either). Always the full-width immediate, never
     imm8 - when the value *does* fit imm8 the ModR/M imm8 rung above is one
     byte shorter and wins instead (checked against real
     i686-linux-gnu-as: [addl $5, %eax] -> [83 c0 05], not this form).
     Scoped to width 32/64: narrower ALU-immediate forms are not lowered at
     all yet (M2 scope), so [width = 16]/[width = 8] never reach this rung
     in practice, matching {!alu_form}'s own lack of an explicit guard.
     Byte-checked against real i686-linux-gnu-as/x86_64-linux-gnu-as:
     [addl $1000, %eax] -> [05 e8 03 00 00], [addq $1000, %rax] ->
     [48 05 e8 03 00 00], [subl $1000, %ecx] still picks the ModR/M form
     unchanged ([ecx] is not the accumulator). *)
  let alu_acc_form ~label ~priority =
    let opcode_codec =
      C.iso_fun ~name:(label ^ "-opcode")
        ~encode:(fun ext ->
          if ext < 0 || ext > 7 then None else Some (Int64.of_int ((ext lsl 3) lor 5)))
        ~decode:(fun b ->
          let b = Int64.to_int b in
          if b land 7 = 5 then Some (b lsr 3) else None)
        (C.field ~width:8 "opcode")
    in
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Alu_rm_imm { ext; width; rm = Rm.Reg r; imm } when width <> 8 && r.num = 0 ->
               let imm =
                 match imm with
                 | Disp.Const v -> Disp.Const (to_width_signed ~width v)
                 | Disp.Sym _ as s -> s
               in
               let fits8 = match imm with Disp.Const v -> fits_s8 v | Disp.Sym _ -> false in
               if fits8 then None else Some (prefixes_of ~width ~reg:0 ~rm:(Rm.Reg r), (ext, imm))
           | _ -> None)
         ~decode:(fun (rex, (ext, imm)) ->
           match Opcode.of_ext ext with
           | None -> None
           | Some _ ->
               let width = width_of_prefixes rex in
               Some (Lowered.Alu_rm_imm { ext; width; rm = Rm.Reg (reg_at ~width 0); imm }))
         C.(prefixes_codec ** opcode_codec ** sym_imm32 ~kind:Abs32 ~signedness:C.Signed))

  (* [0x80 /ext ib] ({!Lowered.Alu_rm_imm} at [width = 8]): GRP1's own
     byte-operand rung, a genuinely different opcode from {!alu_form}'s
     [0x83]/[0x81] rungs rather than a narrower reading of either - real x86
     has no operand-size encoding that turns a ModR/M ALU opcode into an
     8-bit one, so a byte register number reaching [0x83] would just select
     the 32/64-bit register of that same number instead (confirmed against real i686-linux-gnu-as/x86_64-linux-gnu-as
     before writing this: [addb $5, %cl] -> [80 c1 05], [addb $5,
     16(%esp)]/[16(%rsp)] -> [80 44 24 10 05], [addb $5, %sil] -> [40 80 c6
     05] - the same bare-REX byte-register rule {!prefixes_of}'s own comment
     documents, reused unchanged here). Register or memory destination both
     go through this one rung: unlike {!alu_form}'s imm8-vs-imm32 ladder,
     there is only ever one immediate width at this operand size, so no
     [fits] check or priority race between rungs is needed. XED's [0x82]
     alias (an undocumented, 64-bit-invalid duplicate of [0x80] -
     `ADD_GPR8_IMMb_82r0` etc.) is never GAS-selected, matching the
     reverse-iform precedent elsewhere in this file, so it is not admitted. *)
  let alu_form_byte ~label ~priority =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Alu_rm_imm { ext; width = 8; rm; imm = Disp.Const v } ->
               (* [to_width_signed]: a literal like [$200] parses as the
                  plain int64 200, which does not fit the trailing field's
                  own signed-8 range check even though [0xc8] is exactly the
                  byte real [as] emits for it - the same reduce-before-
                  threading discipline {!alu_form}'s own comment explains. *)
               let v = to_width_signed ~width:8 v in
               Some (prefixes_of_ext ~width:8 ~rm, ((), ({ re_reg = ext; re_rm = rm }, v)))
           | _ -> None)
         ~decode:(fun (rex, ((), (e, v))) ->
           match Opcode.of_ext e.re_reg with
           | None -> None
           | Some _ ->
               Some
                 (Lowered.Alu_rm_imm
                    {
                      ext = e.re_reg;
                      width = 8;
                      rm = rm_of ~p:rex ~width:8 e.re_rm;
                      imm = Disp.Const v;
                    }))
         C.(
           prefixes_codec ** const ~width:8 0x80L ** rm_codec
           ** le ~signedness:C.Signed ~width:8 "imm"))

  (* [ext<<3 | 4] ib ({!Lowered.Alu_rm_imm} at [width = 8] with the r/m
     register being exactly [%al] - {!alu_acc_form}'s own byte-width sibling:
     [0x04]/[0x0C]/[0x14]/[0x1C]/[0x24]/[0x2C]/[0x34]/[0x3C]). Unlike
     {!alu_acc_form}, there is no competing shorter ModR/M encoding at this
     width for GAS to prefer instead - {!alu_form_byte}'s own [0x80] rung is
     always one byte longer here (a ModR/M byte plus imm8, versus this form's
     bare opcode plus imm8) - so real [as] always selects this form when the
     destination is [%al], not only when the immediate fails an imm8 fits
     check the way {!alu_acc_form} itself does. Byte-checked against real
     i686-linux-gnu-as/x86_64-linux-gnu-as: [addb $5, %al] -> [04 05], [cmpb
     $5, %al] -> [3c 05], and [addb $200, %al] still picks this form ([04
     c8]), not the 3-byte ModR/M one. No REX byte is ever needed: [%al] is
     register 0, never one of the four legacy/REX-ambiguous byte names
     {!prefixes_of}'s own comment covers - [prefixes_of ~reg:0 ~rm:(Rm.Reg
     r)] with [r.num = 0] always returns [None] (REX absent), so threading
     {!prefixes_codec} through here changes no byte ever emitted. It is
     threaded anyway, for the same reason {!alu_acc_form} threads it despite
     x86-32 never setting REX.W: {!Codec.check}'s [pattern] function gives up
     (returns [None], not "all wildcards") on {!prefixes_codec}'s own
     variable-length rex-present-or-absent [Alt], and only an indeterminate
     pattern is safe here - the opcode field alone has no fixed bits [Codec.check]
     can see (its true fixed low 3 bits, [ext<<3 | 4], live inside the
     [Iso_fun] the checker treats as opaque), so a bare [opcode_codec ** imm]
     pattern would come out as 16 wildcard bits and register a false
     "overlapping fixed bits" conflict against every other exactly-16-bit
     alternative in this table ([ud2], [push-imm8], [fucomp], [fnstsw],
     [fadd-st0-x87]). *)
  let alu_acc_form_byte ~label ~priority =
    let opcode_codec =
      C.iso_fun ~name:(label ^ "-opcode")
        ~encode:(fun ext ->
          if ext < 0 || ext > 7 then None else Some (Int64.of_int ((ext lsl 3) lor 4)))
        ~decode:(fun b ->
          let b = Int64.to_int b in
          if b land 7 = 4 then Some (b lsr 3) else None)
        (C.field ~width:8 "opcode")
    in
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Alu_rm_imm { ext; width = 8; rm = Rm.Reg r; imm = Disp.Const v } when r.num = 0
             ->
               Some (prefixes_of ~width:8 ~reg:0 ~rm:(Rm.Reg r), (ext, to_width_signed ~width:8 v))
           | _ -> None)
         ~decode:(fun (_rex, (ext, v)) ->
           match Opcode.of_ext ext with
           | None -> None
           | Some _ ->
               Some
                 (Lowered.Alu_rm_imm
                    { ext; width = 8; rm = Rm.Reg (reg_at ~width:8 0); imm = Disp.Const v }))
         C.(prefixes_codec ** opcode_codec ** le ~signedness:C.Signed ~width:8 "imm"))

  (* [0x6a ib] / [0x68 id] ({!Lowered.Push_imm}, [pushl $sym]): the same
     short-immediate-first priority discipline and [Disp.t]/[sym_imm32]
     symbol-carrying as {!alu_form}, minus a ModR/M byte - push's immediate
     forms take no r/m or extension field, just the opcode and the value. *)
  let push_imm_form ~label ~priority ~opcode_byte ~imm_width =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Push_imm { imm } ->
               (* Same reduce-before-threading discipline as {!alu_form}'s:
                  the imm8 rung's [const_disp] re-validates whatever
                  [Disp.Const] reaches it, so the raw magnitude must not
                  survive past the fits-check. *)
               let imm =
                 match imm with
                 | Disp.Const v -> Disp.Const (to_width_signed ~width:M.address_width v)
                 | Disp.Sym _ as s -> s
               in
               let fits =
                 match imm with
                 | Disp.Const v -> if imm_width = 8 then fits_s8 v else fits_s32 v
                 | Disp.Sym _ -> imm_width = 32
               in
               if not fits then None else Some ((), imm)
           | _ -> None)
         ~decode:(fun ((), imm) -> Some (Lowered.Push_imm { imm }))
         C.(
           const ~width:8 (Int64.of_int opcode_byte)
           **
           if imm_width = 8 then const_disp ~name:"imm" (le ~signedness:C.Signed ~width:8 "imm")
           else sym_imm32 ~kind:Abs32 ~signedness:C.Signed))

  let general_alts =
    [
      (* The short immediate first: GAS encodes [subl $12, %esp] as [83 ec 0c]
           rather than [81 ec 0c 00 00 00], and the selection rule is priority
           plus the sign-extended-imm8 range check inside the form. Getting the
           order wrong here produces working code that differs from every other
           assembler by three bytes per instruction. *)
      (* Tried first, ahead of even the imm8 ModR/M rung right below: its own
         encode already declines whenever the value fits imm8 (that rung is
         one byte shorter and wins instead), so the ordering only matters
         when both this and the imm32 rung below would otherwise apply - see
         {!alu_acc_form}'s own comment. [priority] is unique across this
         whole target's opcode table, not just this cluster, so this sits at
         -1 rather than crowding the 0..64 range already in use below. *)
      (* Tried even before {!alu_acc_form}'s own imm32/imm16 accumulator rung:
         disjoint by [width] ([alu_acc_form]'s own encode already excludes
         [width = 8]), so the relative order only matters for uniqueness of
         [priority] across this whole table, not for any real ambiguity. *)
      alu_acc_form_byte ~label:"alu-acc-imm8" ~priority:(-2);
      alu_acc_form ~label:"alu-acc-imm" ~priority:(-1);
      alu_form ~label:"alu-rm-imm8" ~priority:0 ~opcode_byte:0x83 ~imm_width:8;
      alu_form ~label:"alu-rm-imm32" ~priority:1 ~opcode_byte:0x81 ~imm_width:32;
      (* [66], not adjacent to the two rungs above: distinct opcode ([0x80],
         not a rung of [0x83]/[0x81]'s own ladder), see {!alu_form_byte}'s own
         comment for why [width = 8] needs a dedicated alternative rather than
         a third rung there. *)
      alu_form_byte ~label:"alu-rm-imm8-byte" ~priority:66;
      C.alt ~label:"mov-r-imm" ~priority:2
        (C.iso_fun ~name:"mov-r-imm"
           ~encode:(function
             | Lowered.Mov_r_imm { width; reg; imm } when width <> 8 ->
                 Some
                   ( prefixes_of ~width ~reg:0 ~rm:(Rm.Reg reg),
                     (((), Int64.of_int (reg.num land 7)), imm) )
             | _ -> None)
           ~decode:(fun (rex, (((), r), imm)) ->
             let width = width_of_prefixes rex in
             (* The register lives in the opcode's own low 3 bits, not a ModR/M
                reg field, so it is REX.B (mask 1) that extends it to r8-r15/
                r8d-r15d, not {!reg_field}'s REX.R (mask 4) - confirmed against
                real x86_64-linux-gnu-as: [movl $5, %r8d] -> [41 b8 05 00 00
                00]. A pre-existing decode bug, found while fixing
                {!Mov_r_imm}'s 8-bit form below: the bytes were always
                correct, but canonical disassembly mislabelled the
                destination as %eax/%rax/... (register 0), which does not
                round-trip through real as back to the original bytes. *)
             Some
               (Lowered.Mov_r_imm
                  { width; reg = reg_at ~width (Int64.to_int r + rex_bit rex 1); imm }))
           C.(
             prefixes_codec
             ** (const ~width:5 0b10111L ** field ~width:3 "reg")
             ** sym_imm32 ~kind:Abs32 ~signedness:C.Unsigned));
      (* [movb $12, %ah] ([0xB0+reg ib], M5, asm/docs/corpus.md - gas_frontier.t's
         i64_dtos.S/i64_dtou.S own rounding-mode-byte idiom): {!Mov_r_imm}'s own
         8-bit sibling - a genuinely different opcode range ([0xB0]-[0xB7], not
         [0xB8]-[0xBF]) and a 1-byte immediate rather than {!sym_imm32}'s fixed
         4 bytes, not just [width]-parameterized the way the alt above is.
         Discovered as a real bug, not a missing feature: before this alt
         existed, {!Lowered.Mov_r_imm} with [width = 8] still matched the alt
         above unconditionally, encoding [%ah] (register number 4) as if it
         were the 32-bit register number 4 ([%esp]) instead - [movb $12, %ah]
         came out as [mov $0xc, %esp]. Only a plain constant is evidenced (no
         fixture spells [movb $sym, %reg]), so unlike the width<>8 alt this
         one does not thread a symbolic {!Disp.t} through {!sym_imm32}. *)
      C.alt ~label:"mov-r-imm8" ~priority:64
        (C.iso_fun ~name:"mov-r-imm8"
           ~encode:(function
             | Lowered.Mov_r_imm { width = 8; reg; imm = Disp.Const v } ->
                 Some
                   ( prefixes_of ~width:8 ~reg:0 ~rm:(Rm.Reg reg),
                     (((), Int64.of_int (reg.num land 7)), v) )
             | _ -> None)
           ~decode:(fun (rex, (((), r), v)) ->
             (* Same REX.B (not REX.R) extension the alt above needs, and for
                the same reason - see its own comment. *)
             Some
               (Lowered.Mov_r_imm
                  {
                    width = 8;
                    reg = reg_at ~width:8 (Int64.to_int r + rex_bit rex 1);
                    imm = Disp.Const v;
                  }))
           C.(
             prefixes_codec
             ** (const ~width:5 0b10110L ** field ~width:3 "reg")
             ** le ~signedness:C.Unsigned ~width:8 "imm"));
    ]

  (* Present only in 32-bit mode, not merely unselected there: an alternative no
     value can reach is dead code that reads like a working optimisation, and
     [Finite.unreachable_alts] exists to catch exactly that. *)
  let moffs_alts =
    if M.rex_allowed then []
    else
      [
        (* The accumulator-and-absolute-address special encodings, [a1] and
           [a3]. They are one byte shorter than the general ModR/M forms and GNU
           picks them whenever they apply, so an assembler that did not have
           them would be correct and would still fail the byte gate.

           32-bit only, deliberately: in 64-bit mode [a1]/[a3] take a *64-bit*
           moffs - the [movabs] forms - and CompCert reaches a global through
           RIP-relative addressing there anyway, so admitting them would add an
           encoding no fixture selects and a second reading of the same opcode.

           Priority below the general forms, because the shorter encoding has to
           be *tried* first; the opcodes are distinct, so decode is unambiguous
           whichever order it walks them in. *)
        C.alt ~label:"mov-eax-moffs" ~priority:3
          (C.iso_fun ~name:"mov-eax-moffs"
             ~encode:(function
               | Lowered.Mov_r_rm { width; reg; rm } when moffs_applies ~width ~reg ~rm ->
                   Some ((), moffs_disp rm)
               | _ -> None)
             ~decode:(fun ((), d) ->
               Some
                 (Lowered.Mov_r_rm
                    { width = 32; reg = reg_at ~width:32 0; rm = Rm.Mem (moffs_mem d) }))
             C.(const ~width:8 0xa1L ** sym_disp ~kind:Abs32));
        C.alt ~label:"mov-moffs-eax" ~priority:4
          (C.iso_fun ~name:"mov-moffs-eax"
             ~encode:(function
               | Lowered.Mov_rm_r { width; reg; rm } when moffs_applies ~width ~reg ~rm ->
                   Some ((), moffs_disp rm)
               | _ -> None)
             ~decode:(fun ((), d) ->
               Some
                 (Lowered.Mov_rm_r
                    { width = 32; reg = reg_at ~width:32 0; rm = Rm.Mem (moffs_mem d) }))
             C.(const ~width:8 0xa3L ** sym_disp ~kind:Abs32));
      ]

  (* The x87 component's alternatives, built from the {!X86_x87} tables. A form whose mnemonic
     has no opcode here is a component/family mismatch, caught when the family is instantiated
     rather than as a silently missing alternative. *)
  let x87_alts =
    let opcode_of mnemonic =
      match Opcode.of_x87_mnemonic mnemonic with
      | Some op -> op
      | None -> invalid_arg ("x86 x87 component: no opcode for " ^ mnemonic)
    in
    let fixed (f : X86_x87.fixed_form) =
      let value = if String.equal f.mnemonic "fucomp" then Lowered.Fucomp else Lowered.Fnstsw in
      C.alt ~label:f.label ~priority:f.priority
        (C.iso_fun ~name:f.label
           ~encode:(fun v -> if Lowered.equal v value then Some () else None)
           ~decode:(fun () -> Some value)
           (C.const ~width:f.bits f.word))
    in
    List.map
      (fun (f : X86_x87.memory_form) ->
        fpu_mem_form ~label:f.label ~priority:f.priority ~opcode_byte:f.opcode_byte ~ext:f.ext
          ~op:(opcode_of f.mnemonic))
      X86_x87.memory_forms
    @ List.map fixed X86_x87.fixed_forms
    @ [ fadd_st0_x87_form ]

  let () =
    (* Every opcode the family lists as x87 must be one the component describes, and back. *)
    let described = List.sort String.compare (Target_component.mnemonics X86_x87.component) in
    let listed = List.sort String.compare (List.map Opcode.name Opcode.x87) in
    if described <> listed then invalid_arg "x86 x87 component and Opcode.x87 disagree"

  let codec : (Lowered.t, fixup_kind) C.t =
    C.choice ~name:M.name
      (general_alts @ moffs_alts @ x87_alts
      @ [
          (* 8-bit MOV is not the same opcode with a narrower width like every
             other case here - real x86 has no operand-size prefix or REX.W
             reading that means "8 bits" for [0x89]/[0x8B], so a width-8
             [Mov_rm_r]/[Mov_r_rm] needs a genuinely different opcode byte
             ([0x88]/[0x8A]) rather than [prefixes_of]'s usual REX.W
             selection - {!width_of_prefixes} can only ever produce 32 or 64.
             M5, asm/docs/corpus.md: [movb %sil, 7(%rdi)]. *)
          C.alt ~label:"mov-rm-r8" ~priority:46
            (C.iso_fun ~name:"mov-rm-r8"
               ~encode:(function
                 | Lowered.Mov_rm_r { width = 8; rm; reg } ->
                     Some
                       ( prefixes_of ~width:8 ~reg:reg.num ~rm,
                         ((), { re_reg = reg.num; re_rm = rm }) )
                 | _ -> None)
               ~decode:(fun (rex, ((), e)) ->
                 Some
                   (Lowered.Mov_rm_r
                      {
                        width = 8;
                        rm = rm_of ~p:rex ~width:8 e.re_rm;
                        reg = reg_field ~p:rex ~width:8 e.re_reg;
                      }))
               C.(prefixes_codec ** const ~width:8 0x88L ** rm_codec));
          C.alt ~label:"mov-rm-r" ~priority:5
            (C.iso_fun ~name:"mov-rm-r"
               ~encode:(function
                 | Lowered.Mov_rm_r { width; rm; reg } when width <> 8 ->
                     Some
                       (prefixes_of ~width ~reg:reg.num ~rm, ((), { re_reg = reg.num; re_rm = rm }))
                 | _ -> None)
               ~decode:(fun (rex, ((), e)) ->
                 let width = width_of_prefixes rex in
                 Some
                   (Lowered.Mov_rm_r
                      {
                        width;
                        rm = rm_of ~p:rex ~width e.re_rm;
                        reg = reg_field ~p:rex ~width e.re_reg;
                      }))
               C.(prefixes_codec ** const ~width:8 0x89L ** rm_codec));
          C.alt ~label:"mov-r-rm8" ~priority:47
            (C.iso_fun ~name:"mov-r-rm8"
               ~encode:(function
                 | Lowered.Mov_r_rm { width = 8; reg; rm } ->
                     Some
                       ( prefixes_of ~width:8 ~reg:reg.num ~rm,
                         ((), { re_reg = reg.num; re_rm = rm }) )
                 | _ -> None)
               ~decode:(fun (rex, ((), e)) ->
                 Some
                   (Lowered.Mov_r_rm
                      {
                        width = 8;
                        reg = reg_field ~p:rex ~width:8 e.re_reg;
                        rm = rm_of ~p:rex ~width:8 e.re_rm;
                      }))
               C.(prefixes_codec ** const ~width:8 0x8aL ** rm_codec));
          C.alt ~label:"mov-r-rm" ~priority:6
            (C.iso_fun ~name:"mov-r-rm"
               ~encode:(function
                 | Lowered.Mov_r_rm { width; reg; rm } when width <> 8 ->
                     Some
                       (prefixes_of ~width ~reg:reg.num ~rm, ((), { re_reg = reg.num; re_rm = rm }))
                 | _ -> None)
               ~decode:(fun (rex, ((), e)) ->
                 let width = width_of_prefixes rex in
                 Some
                   (Lowered.Mov_r_rm
                      {
                        width;
                        reg = reg_field ~p:rex ~width e.re_reg;
                        rm = rm_of ~p:rex ~width e.re_rm;
                      }))
               C.(prefixes_codec ** const ~width:8 0x8bL ** rm_codec));
          C.alt ~label:"lea" ~priority:7
            (C.iso_fun ~name:"lea"
               ~encode:(function
                 | Lowered.Lea { width; reg; mem } ->
                     Some
                       ( prefixes_of ~width ~reg:reg.num ~rm:(Rm.Mem mem),
                         ((), { re_reg = reg.num; re_rm = Rm.Mem mem }) )
                 | _ -> None)
               ~decode:(fun (rex, ((), e)) ->
                 match e.re_rm with
                 | Rm.Mem m ->
                     let width = width_of_prefixes rex in
                     let m =
                       match rm_of ~p:rex ~width (Rm.Mem m) with Rm.Mem m -> m | Rm.Reg _ -> m
                     in
                     Some (Lowered.Lea { width; reg = reg_field ~p:rex ~width e.re_reg; mem = m })
                 | Rm.Reg _ -> None)
               C.(prefixes_codec ** const ~width:8 0x8dL ** rm_codec));
          C.alt ~label:"ret" ~priority:8
            (C.iso_fun ~name:"ret"
               ~encode:(function Lowered.Ret -> Some () | _ -> None)
               ~decode:(fun () -> Some Lowered.Ret)
               C.(const ~width:8 0xc3L));
          C.alt ~label:"mov-rm-imm8" ~priority:9
            (C.iso_fun ~name:"mov-rm-imm8"
               ~encode:(function
                 | Lowered.Mov_rm_imm { width; rm; imm } ->
                     if width <> 8 then None
                     else
                       Some
                         (prefixes_of ~width:8 ~reg:0 ~rm, ((), ({ re_reg = 0; re_rm = rm }, imm)))
                 | _ -> None)
               ~decode:(fun (rex, ((), (e, imm))) ->
                 if e.re_reg <> 0 then None
                 else
                   Some (Lowered.Mov_rm_imm { width = 8; rm = rm_of ~p:rex ~width:8 e.re_rm; imm }))
               C.(
                 prefixes_codec ** const ~width:8 0xC6L ** rm_codec
                 ** le ~signedness:C.Signed ~width:8 "imm8"));
          (* One alternative for the whole r/m-written ALU direction: the opcode
           byte is a declared finite relation rather than a constant per form,
           so a second operation is a table row instead of a copy of this
           branch, and [check] decides the table's injectivity. *)
          C.alt ~label:"alu-rm-r" ~priority:10
            (C.iso_fun ~name:"alu-rm-r"
               ~encode:(function
                 | Lowered.Alu_rm_r { op; width; rm; reg } ->
                     Some
                       (prefixes_of ~width ~reg:reg.num ~rm, (op, { re_reg = reg.num; re_rm = rm }))
                 | _ -> None)
               ~decode:(fun (rex, (op, e)) ->
                 let width = width_of_prefixes rex in
                 Some
                   (Lowered.Alu_rm_r
                      {
                        op;
                        width;
                        rm = rm_of ~p:rex ~width e.re_rm;
                        reg = reg_field ~p:rex ~width e.re_reg;
                      }))
               C.(prefixes_codec ** alu_rm_r_codec ** rm_codec));
          (* [0f af] and [0f 4x] both write the *register* operand, which is why
           they are separate alternatives from the one above rather than more
           rows in its table: the direction is part of the form, not of the
           opcode byte. *)
          C.alt ~label:"imul-r-rm" ~priority:15
            (C.iso_fun ~name:"imul-r-rm"
               ~encode:(function
                 | Lowered.Imul_r_rm { width; reg; rm } ->
                     Some
                       (prefixes_of ~width ~reg:reg.num ~rm, ((), { re_reg = reg.num; re_rm = rm }))
                 | _ -> None)
               ~decode:(fun (rex, ((), e)) ->
                 let width = width_of_prefixes rex in
                 Some
                   (Lowered.Imul_r_rm
                      {
                        width;
                        reg = reg_field ~p:rex ~width e.re_reg;
                        rm = rm_of ~p:rex ~width e.re_rm;
                      }))
               C.(prefixes_codec ** const ~width:16 0x0FAFL ** rm_codec));
          C.alt ~label:"cmov-r-rm" ~priority:16
            (C.iso_fun ~name:"cmov-r-rm"
               ~encode:(function
                 | Lowered.Cmov_r_rm { cc; width; reg; rm } ->
                     Some
                       ( prefixes_of ~width ~reg:reg.num ~rm,
                         ((), (cc, { re_reg = reg.num; re_rm = rm })) )
                 | _ -> None)
               ~decode:(fun (rex, ((), (cc, e))) ->
                 let width = width_of_prefixes rex in
                 Some
                   (Lowered.Cmov_r_rm
                      {
                        cc;
                        width;
                        reg = reg_field ~p:rex ~width e.re_reg;
                        rm = rm_of ~p:rex ~width e.re_rm;
                      }))
               C.(prefixes_codec ** const ~width:12 0x0F4L ** cc_codec ** rm_codec));
          C.alt ~label:"pop-r" ~priority:11
            (C.iso_fun ~name:"pop-r"
               ~encode:(function
                 | Lowered.Pop { reg } ->
                     Some
                       ( prefixes_of ~width:32 ~reg:0 ~rm:(Rm.Reg reg),
                         ((), Int64.of_int (reg.num land 7)) )
                 | _ -> None)
               ~decode:(fun (_rex, ((), r)) ->
                 Some
                   (Lowered.Pop
                      { reg = reg_at ~width:M.address_width (Int64.to_int r + rex_bit _rex 1) }))
               C.(prefixes_codec ** const ~width:5 0b01011L ** field ~width:3 "reg"));
          C.alt ~label:"jmp-rm" ~priority:12
            (C.iso_fun ~name:"jmp-rm"
               ~encode:(function
                 | Lowered.Jmp_rm { rm } ->
                     Some (prefixes_of ~width:32 ~reg:4 ~rm, ((), { re_reg = 4; re_rm = rm }))
                 | _ -> None)
               ~decode:(fun (rex, ((), e)) ->
                 if e.re_reg <> 4 then None
                 else Some (Lowered.Jmp_rm { rm = rm_of ~p:rex ~width:M.address_width e.re_rm }))
               C.(prefixes_codec ** const ~width:8 0xFFL ** rm_codec));
          C.alt ~label:"ud2" ~priority:13
            (C.iso_fun ~name:"ud2"
               ~encode:(function Lowered.Ud2 -> Some () | _ -> None)
               ~decode:(fun () -> Some Lowered.Ud2)
               C.(const ~width:16 0x0f0bL));
          (* [e8 rel32]. The displacement is a [fixup] node rather than a [field]
           wrapped in [le]: the linker writes the container little-endian
           itself, so passing a placeholder through a byte-swapping [Iso_fun]
           would swap zeroes to no purpose and put the slices in the wrong
           place. Decoding yields the raw displacement, which
           [instruction_of_lowered] renders against the instruction's address. *)
          C.alt ~label:"call-rel32" ~priority:14
            (C.iso_fun ~name:"call-rel32"
               ~encode:(function
                 | Lowered.Call_rel { target } -> (
                     match target with
                     | Asm_core.Lowered_ast.Symbolic _ -> Some ((), 0L)
                     | Asm_core.Lowered_ast.Resolved { value; _ } -> Some ((), value))
                 | _ -> None)
               ~decode:(fun ((), d) ->
                 (* A fixup node is unsigned - it is one slice of a value whose
                  signedness belongs to the whole - so applying it is this
                  Iso_fun's job. Skipping it makes a backward call decode as a
                  four-gigabyte forward one. *)
                 Some
                   (Lowered.Call_rel
                      {
                        target =
                          Asm_core.Lowered_ast.Resolved
                            { value = sign_extend ~width:32 d; rung = "call-rel32" };
                      }))
               C.(const ~width:8 0xe8L ** le_fixup ~width:32 ~kind:Pcrel32_call "target"));
          (* The two real ladders, and the reason {!Codec.Relax} exists: GNU
           encodes a nearby branch in two bytes, so an assembler that always
           emitted the near form would be correct and would still fail the
           byte-for-byte gate.

           The projection sits *inside* each rung rather than around the ladder,
           which is not a style choice: a decoded branch has to say which rung
           produced it - that is what stops canonical disassembly re-shortening
           a near branch - and an [Iso_fun] wrapped around the whole [Relax] has
           no way to know. Inside, each rung's decode names its own label.

           The rung labels are [d8] and [d32] because that is what GAS's
           form-forcing mnemonic suffix spells, so a form id and the canonical
           spelling that reproduces it use one vocabulary. *)
          C.alt ~label:"jmp-rel" ~priority:17
            (C.relax ~name:"jmp"
               [
                 C.rung ~label:"d8" (jmp_rung ~opcode:0xebL ~width:8 ~kind:Pcrel8_branch ~rung:"d8");
                 C.rung ~label:"d32"
                   (jmp_rung ~opcode:0xe9L ~width:32 ~kind:Pcrel32_branch ~rung:"d32");
               ]);
          C.alt ~label:"jcc-rel" ~priority:18
            (C.relax ~name:"jcc"
               [ C.rung ~label:"d8" (jcc_rung_short ()); C.rung ~label:"d32" (jcc_rung_near ()) ]);
          (* M4 (.ai/asm_plan.md §12): the CompCert-runtime-helper fixture's
             own measured forms, added alongside the opcode/lowering work
             above rather than folded into an existing alt - each is a
             distinct opcode byte or byte family. *)
          (* [0x50+r], {!Pop}'s [pop-r] mirrored at the opposite base byte. *)
          C.alt ~label:"push-r" ~priority:19
            (C.iso_fun ~name:"push-r"
               ~encode:(function
                 | Lowered.Push { reg } ->
                     Some
                       ( prefixes_of ~width:32 ~reg:0 ~rm:(Rm.Reg reg),
                         ((), Int64.of_int (reg.num land 7)) )
                 | _ -> None)
               ~decode:(fun (_rex, ((), r)) ->
                 Some
                   (Lowered.Push
                      { reg = reg_at ~width:M.address_width (Int64.to_int r + rex_bit _rex 1) }))
               C.(prefixes_codec ** const ~width:5 0b01010L ** field ~width:3 "reg"));
          (* Group-3 (opcode [0xF7]): {!Neg}/{!Mul}/{!Div}, the ModR/M reg
             field selecting the operation exactly as {!alu_form} does for
             group-1, just a different opcode and extension table. *)
          C.alt ~label:"unary-rm" ~priority:20
            (C.iso_fun ~name:"unary-rm"
               ~encode:(function
                 | Lowered.Unary_rm { ext; width; rm } ->
                     Some (prefixes_of_ext ~width ~rm, ((), { re_reg = ext; re_rm = rm }))
                 | _ -> None)
               ~decode:(fun (rex, ((), e)) ->
                 match Opcode.of_unary_ext e.re_reg with
                 | None -> None
                 | Some _ ->
                     let width = width_of_prefixes rex in
                     Some
                       (Lowered.Unary_rm { ext = e.re_reg; width; rm = rm_of ~p:rex ~width e.re_rm }))
               C.(prefixes_codec ** const ~width:8 0xF7L ** rm_codec));
          (* The reg<-rm ALU direction: {!alu-rm-r}'s mirror image. *)
          C.alt ~label:"alu-r-rm" ~priority:21
            (C.iso_fun ~name:"alu-r-rm"
               ~encode:(function
                 | Lowered.Alu_r_rm { op; width; reg; rm } ->
                     Some
                       (prefixes_of ~width ~reg:reg.num ~rm, (op, { re_reg = reg.num; re_rm = rm }))
                 | _ -> None)
               ~decode:(fun (rex, (op, e)) ->
                 let width = width_of_prefixes rex in
                 Some
                   (Lowered.Alu_r_rm
                      {
                        op;
                        width;
                        reg = reg_field ~p:rex ~width e.re_reg;
                        rm = rm_of ~p:rex ~width e.re_rm;
                      }))
               C.(prefixes_codec ** alu_r_rm_codec ** rm_codec));
          (* Group-2 shift/rotate-by-1 (opcode [0xD1], no immediate byte):
             {!Rcr}/{!Shr}, the same reg-field-as-extension idea again. *)
          C.alt ~label:"shift1-rm" ~priority:22
            (C.iso_fun ~name:"shift1-rm"
               ~encode:(function
                 | Lowered.Shift1_rm { ext; width; rm } ->
                     Some (prefixes_of_ext ~width ~rm, ((), { re_reg = ext; re_rm = rm }))
                 | _ -> None)
               ~decode:(fun (rex, ((), e)) ->
                 match Opcode.of_shift1_ext e.re_reg with
                 | None -> None
                 | Some _ ->
                     let width = width_of_prefixes rex in
                     Some
                       (Lowered.Shift1_rm
                          { ext = e.re_reg; width; rm = rm_of ~p:rex ~width e.re_rm }))
               C.(prefixes_codec ** const ~width:8 0xD1L ** rm_codec));
        ]
      (* [0x48+r]: the single-byte DEC form exists only in 32-bit mode - in
         64-bit mode 0x40-0x4F are REX prefixes instead, so an unguarded alt
         here would be dead code [Finite.unreachable_alts] exists to catch
         (the same reasoning {!moffs_alts} already documents for [a1]/[a3]).
         The general [0xFF /1] group encoding that DOES exist in 64-bit mode
         is unimplemented; no fixture selects it. *)
      @ (if M.rex_allowed then []
         else
           [
             C.alt ~label:"dec-r" ~priority:23
               (C.iso_fun ~name:"dec-r"
                  ~encode:(function
                    | Lowered.Dec { reg } -> Some ((), Int64.of_int (reg.num land 7)) | _ -> None)
                  ~decode:(fun ((), r) ->
                    Some (Lowered.Dec { reg = reg_at ~width:M.address_width (Int64.to_int r) }))
                  C.(const ~width:5 0b01001L ** field ~width:3 "reg"));
           ])
      (* SSE2 scalar float (M5, asm/docs/corpus.md), unconditional - unlike
         [dec-r] just above, nothing here is bit-pattern-dead in 32-bit mode
         (every mandatory-prefix byte and opcode used here is free in both
         modes), so this is not a second [if M.rex_allowed] split. x86_32
         simply never constructs a [Sse_*] value in the first place, because
         its own register list (x86_32_encode.ml) has no xmm entries - the
         same "unreachable via values, not via bit patterns" situation
         {!Codec.check} does not and need not flag. *)
      @ [
          sse_binop_alt ~label:"sse-binop-f2" ~priority:24 ~mandatory:0xF2
            ~opcode_codec:sse_binop_f2_codec;
          sse_binop_alt ~label:"sse-binop-f3" ~priority:25 ~mandatory:0xF3
            ~opcode_codec:sse_binop_f3_codec;
          sse_binop_alt ~label:"sse-binop-66" ~priority:26 ~mandatory:0x66
            ~opcode_codec:sse_binop_66_codec;
          (* [pshufb]: {!Opcode.Pshufb}'s own doc comment - opcode map 2, priority
             placement here doesn't matter for correctness (the extra [0x38] byte makes this
             pattern disjoint from every map-1 [66 0F <op>] alt above), kept near its closest
             structural sibling for readability. *)
          sse_binop_0f38_alt ~label:"sse-binop-0f38-66" ~priority:107 ~mandatory:0x66
            ~opcode_codec:sse_binop_0f38_codec;
          sse_binop_none_alt ~label:"sse-binop-none" ~priority:27 ~opcode_codec:sse_binop_none_codec;
          sse_mov_r_rm_alt ~label:"sse-movsd-load" ~priority:28 ~mandatory:0xF2 ~op:Opcode.Movsd
            ~opcode16:0x0F10L;
          sse_mov_r_rm_alt ~label:"sse-movss-load" ~priority:29 ~mandatory:0xF3 ~op:Opcode.Movss
            ~opcode16:0x0F10L;
          sse_mov_rm_r_alt ~label:"sse-movsd-store" ~priority:30 ~mandatory:0xF2 ~op:Opcode.Movsd
            ~opcode16:0x0F11L;
          sse_mov_rm_r_alt ~label:"sse-movss-store" ~priority:31 ~mandatory:0xF3 ~op:Opcode.Movss
            ~opcode16:0x0F11L;
          sse_cvtsi2f_alt ~label:"cvtsi2sd-r-rm" ~priority:32 ~mandatory:0xF2 ~op:Opcode.Cvtsi2sd
            ~opcode16:0x0F2AL;
          sse_cvtsi2f_alt ~label:"cvtsi2ss-r-rm" ~priority:33 ~mandatory:0xF3 ~op:Opcode.Cvtsi2ss
            ~opcode16:0x0F2AL;
          sse_cvtf2i_alt ~label:"cvttsd2si-r-rm" ~priority:34;
          sse_cvtsi2f_alt ~label:"movd-load-r-rm" ~priority:84 ~mandatory:0x66 ~op:Opcode.Movd
            ~opcode16:0x0F6EL;
          sse_movd_store_alt ~label:"movd-store-r-rm" ~priority:85;
          vex_scalar_rrr_alt ~label:"vex-scalar-f2-rrr" ~priority:67 ~pp:3
            ~opcode_codec:vex_scalar_f2_codec;
          vex_scalar_rrr_alt ~label:"vex-scalar-f3-rrr" ~priority:68 ~pp:2
            ~opcode_codec:vex_scalar_f3_codec;
          vex_scalar_rrr_alt ~label:"vex-scalar-none-rrr" ~priority:69 ~pp:0
            ~opcode_codec:vex_scalar_none_codec;
          vex_scalar_rrr_alt ~label:"vex-scalar-66-rrr" ~priority:70 ~pp:1
            ~opcode_codec:vex_scalar_66_codec;
          vex3_map2_rrr_alt ~label:"vex3-map2-66-rrr" ~priority:110 ~pp:1
            ~opcode_codec:vex3_map2_66_codec;
          vex3_map2_unop_alt ~label:"vex3-map2-66-unop" ~priority:112 ~pp:1
            ~opcode_codec:vex3_map2_unop_codec;
          evex_binop_rrr_alt ~label:"evex-ps-rrr" ~priority:113 ~pp:0 ~w:0
            ~opcode_codec:evex_ps_codec;
          evex_binop_rrr_alt ~label:"evex-pd-rrr" ~priority:114 ~pp:1 ~w:1
            ~opcode_codec:evex_pd_codec;
          vex3_map3_imm_rrr_alt ~label:"vex3-map3-66-imm-rrr" ~priority:111 ~pp:1
            ~opcode_codec:vex3_map3_66_codec;
          vex_unop_alt ~label:"vex-unop-none" ~priority:71 ~pp:0 ~opcode_codec:vex_unop_none_codec;
          vex_unop_alt ~label:"vex-unop-66" ~priority:72 ~pp:1 ~opcode_codec:vex_unop_66_codec;
          (* [cmpsd]/[cmpss]: {!Cmpsd}/{!Cmpss}'s own one-entry mandatory-[F2]/[F3]
             groups, same ordering convention as {!sse_binop_alt}'s own F2/F3 groups above. *)
          sse_binop_imm_alt ~label:"sse-binop-imm-f2" ~priority:73 ~mandatory:0xF2
            ~opcode_codec:sse_binop_imm_f2_codec;
          sse_binop_imm_alt ~label:"sse-binop-imm-f3" ~priority:74 ~mandatory:0xF3
            ~opcode_codec:sse_binop_imm_f3_codec;
          (* [sse-binop-imm-66] must be tried before [sse-binop-imm-none]: the mandatory-
             prefix-free alt reuses [prefixes_codec] directly ({!sse_binop_none_alt}'s own
             comment), whose [opsz_codec] field can ambiguously consume a genuine leading
             mandatory [0x66] byte - the same reason {!sse_binop_alt}'s [66] group (priority 26)
             is tried before {!sse_binop_none_alt} (priority 27) above. *)
          sse_binop_imm_alt ~label:"sse-binop-imm-66" ~priority:75 ~mandatory:0x66
            ~opcode_codec:sse_binop_imm_66_codec;
          sse_binop_imm_none_alt ~label:"sse-binop-imm-none" ~priority:76
            ~opcode_codec:sse_binop_imm_none_codec;
          (* [palignr]: {!Opcode.Palignr}'s own doc comment - opcode map 3, disjoint
             from every map-1 alt above by construction (the extra [0x3A] byte). *)
          sse_binop_imm_0f3a_alt ~label:"sse-binop-imm-0f3a-66" ~priority:108 ~mandatory:0x66
            ~opcode_codec:sse_binop_imm_0f3a_codec;
          sse_gpr_imm_0f3a_alt ~label:"sse-gpr-imm-0f3a-66" ~priority:109
            ~opcode_codec:sse_gpr_imm_0f3a_codec;
          vex_binop_imm_rrr_alt ~label:"vex-binop-imm-f2" ~priority:77 ~pp:3
            ~opcode_codec:vex_binop_imm_f2_codec;
          vex_binop_imm_rrr_alt ~label:"vex-binop-imm-f3" ~priority:78 ~pp:2
            ~opcode_codec:vex_binop_imm_f3_codec;
          vex_binop_imm_rrr_alt ~label:"vex-binop-imm-none" ~priority:79 ~pp:0
            ~opcode_codec:vex_binop_imm_none_codec;
          vex_binop_imm_rrr_alt ~label:"vex-binop-imm-66" ~priority:80 ~pp:1
            ~opcode_codec:vex_binop_imm_66_codec;
          (* [vpshuflw]/[vpshufhw]/[vpshufd]: {!Vpshuflw}/{!Vpshufhw}'s own one-entry
             mandatory-[F2]/[F3] groups, {!Vpshufd}'s own mandatory-66 group - same ordering
             convention as {!vex_binop_imm_rrr_alt}'s own F2/F3/66 groups above. *)
          vex_unop_imm_alt ~label:"vex-unop-imm-f2" ~priority:81 ~pp:3
            ~opcode_codec:vex_unop_imm_f2_codec;
          vex_unop_imm_alt ~label:"vex-unop-imm-f3" ~priority:82 ~pp:2
            ~opcode_codec:vex_unop_imm_f3_codec;
          vex_unop_imm_alt ~label:"vex-unop-imm-66" ~priority:83 ~pp:1
            ~opcode_codec:vex_unop_imm_66_codec;
          vex_movd_r_alt ~label:"vex-movd-load-r-rm" ~priority:86;
          vex_movd_rm_alt ~label:"vex-movd-store-r-rm" ~priority:87;
          (* [movdqa]/[movdqu]: {!Opcode.Movdqa}'s own doc comment - reusing
             {!sse_mov_r_rm_alt}/{!sse_mov_rm_r_alt} unchanged, just a new mandatory-prefix/
             opcode16 pair each, the same way [movsd]/[movss] load/store above do. *)
          sse_mov_r_rm_alt ~label:"movdqa-load" ~priority:88 ~mandatory:0x66 ~op:Opcode.Movdqa
            ~opcode16:0x0F6FL;
          sse_mov_r_rm_alt ~label:"movdqu-load" ~priority:89 ~mandatory:0xF3 ~op:Opcode.Movdqu
            ~opcode16:0x0F6FL;
          sse_mov_rm_r_alt ~label:"movdqa-store" ~priority:90 ~mandatory:0x66 ~op:Opcode.Movdqa
            ~opcode16:0x0F7FL;
          sse_mov_rm_r_alt ~label:"movdqu-store" ~priority:91 ~mandatory:0xF3 ~op:Opcode.Movdqu
            ~opcode16:0x0F7FL;
          (* [vmovdqu]: {!Opcode.Vmovdqu}'s own doc comment - the first mandatory-[F3]
             {!vex_unop_alt} group. *)
          vex_unop_alt ~label:"vex-unop-f3" ~priority:92 ~pp:2 ~opcode_codec:vex_unop_f3_codec;
          sse_pinsrw_alt ~label:"sse-pinsrw" ~priority:93;
          sse_pextrw_alt ~label:"sse-pextrw" ~priority:94;
          vex_pinsrw_alt ~label:"vex-pinsrw" ~priority:95;
          vex_pextrw_alt ~label:"vex-pextrw" ~priority:96;
          (* [movmskps]/[movmskpd]/[pmovmskb]: the mandatory-66 group must be tried
             before the mandatory-prefix-free group, the same [prefixes_codec] opsz-consuming
             ambiguity reason as {!sse_binop_alt}'s own 66-before-none ordering. *)
          sse_movmsk_alt ~label:"sse-movmsk-66" ~priority:97 ~opcode_codec:sse_movmsk_66_codec;
          sse_movmsk_none_alt ~label:"sse-movmsk-none" ~priority:98
            ~opcode_codec:sse_movmsk_none_codec;
          vex_movmsk_alt ~label:"vex-movmsk-66" ~priority:99 ~pp:1 ~opcode_codec:vex_movmsk_66_codec;
          vex_movmsk_alt ~label:"vex-movmsk-none" ~priority:100 ~pp:0
            ~opcode_codec:vex_movmsk_none_codec;
          (* [psllw]/[psrlw]/[psraw] etc.'s immediate-count group-opcode form: one alt
             per lane-width opcode byte - see {!xmm_shift_imm_alt}'s own doc comment. *)
          xmm_shift_imm_alt ~label:"xmm-shift-imm-w" ~priority:101 ~opcode:0x71;
          xmm_shift_imm_alt ~label:"xmm-shift-imm-d" ~priority:102 ~opcode:0x72;
          xmm_shift_imm_alt ~label:"xmm-shift-imm-q" ~priority:103 ~opcode:0x73;
          vex_shift_imm_alt ~label:"vex-shift-imm-w" ~priority:104 ~opcode:0x71;
          vex_shift_imm_alt ~label:"vex-shift-imm-d" ~priority:105 ~opcode:0x72;
          vex_shift_imm_alt ~label:"vex-shift-imm-q" ~priority:106 ~opcode:0x73;
        ]
      (* M5 (asm/docs/corpus.md), unconditional for the same reason as the
         SSE block above: nothing here is bit-pattern-dead in either mode. *)
      @ [
          (* Group-2 shift/rotate, general immediate count ([0xC1 ib]) - the
             non-1-count sibling of [shift1-rm] above. *)
          C.alt ~label:"shift-imm-rm" ~priority:35
            (C.iso_fun ~name:"shift-imm-rm"
               ~encode:(function
                 | Lowered.Shift_imm_rm { ext; width; rm; imm } ->
                     Some (prefixes_of_ext ~width ~rm, ((), ({ re_reg = ext; re_rm = rm }, imm)))
                 | _ -> None)
               ~decode:(fun (rex, ((), (e, imm))) ->
                 match Opcode.of_shift1_ext e.re_reg with
                 | None -> None
                 | Some _ ->
                     let width = width_of_prefixes rex in
                     Some
                       (Lowered.Shift_imm_rm
                          { ext = e.re_reg; width; rm = rm_of ~p:rex ~width e.re_rm; imm }))
               C.(
                 prefixes_codec ** const ~width:8 0xC1L ** rm_codec
                 ** le ~signedness:C.Unsigned ~width:8 "imm8"));
          (* Group-2 shift/rotate, count-in-%cl ([0xD3]). *)
          C.alt ~label:"shift-cl-rm" ~priority:36
            (C.iso_fun ~name:"shift-cl-rm"
               ~encode:(function
                 | Lowered.Shift_cl_rm { ext; width; rm } ->
                     Some (prefixes_of_ext ~width ~rm, ((), { re_reg = ext; re_rm = rm }))
                 | _ -> None)
               ~decode:(fun (rex, ((), e)) ->
                 match Opcode.of_shift1_ext e.re_reg with
                 | None -> None
                 | Some _ ->
                     let width = width_of_prefixes rex in
                     Some
                       (Lowered.Shift_cl_rm
                          { ext = e.re_reg; width; rm = rm_of ~p:rex ~width e.re_rm }))
               C.(prefixes_codec ** const ~width:8 0xD3L ** rm_codec));
          (* [sete %al]/[setl %r8b] ([0F 90+cc /0]): the ModR/M reg field is
             fixed at 0 rather than an operand or an extension lookup, unlike
             every other reg-field use in this codec - the condition is
             already fully carried by the low nibble of the opcode's second
             byte, mirroring {!Cmov_r_rm}'s own [const ~width:12 0x0F4L]. *)
          C.alt ~label:"setcc-rm" ~priority:37
            (C.iso_fun ~name:"setcc-rm"
               ~encode:(function
                 | Lowered.Setcc_rm { cc; rm } ->
                     Some (prefixes_of ~width:8 ~reg:0 ~rm, ((), (cc, { re_reg = 0; re_rm = rm })))
                 | _ -> None)
               ~decode:(fun (rex, ((), (cc, e))) ->
                 if e.re_reg <> 0 then None
                 else Some (Lowered.Setcc_rm { cc; rm = rm_of ~p:rex ~width:8 e.re_rm }))
               C.(prefixes_codec ** const ~width:12 0x0F9L ** cc_codec ** rm_codec));
          movx_alt ~label:"movzx-b-r-rm" ~priority:38 ~zero_extend:true ~src_width:8
            ~opcode16:0x0FB6L;
          movx_alt ~label:"movzx-w-r-rm" ~priority:39 ~zero_extend:true ~src_width:16
            ~opcode16:0x0FB7L;
          movx_alt ~label:"movsx-b-r-rm" ~priority:40 ~zero_extend:false ~src_width:8
            ~opcode16:0x0FBEL;
          movx_alt ~label:"movsx-w-r-rm" ~priority:41 ~zero_extend:false ~src_width:16
            ~opcode16:0x0FBFL;
          movsxd_alt ~label:"movsxd-r-rm" ~priority:42;
          imul_imm_form ~label:"imul-r-rm-imm8" ~priority:43 ~opcode_byte:0x6B ~imm_width:8;
          imul_imm_form ~label:"imul-r-rm-imm32" ~priority:44 ~opcode_byte:0x69 ~imm_width:32;
          (* TEST's own immediate form ([0xF7 /0 id], M5, asm/docs/corpus.md
             - [testl $1,%edi]): the ModR/M reg field is fixed at 0, the same
             "extension code, not an operand" shape as {!setcc-rm} above, not
             a lookup into {!Opcode.to_ext} - TEST is not part of that table
             (it is not opcode [0x80]/[0x81]/[0x83] at all). Always a
             32-bit-or-truncated immediate, never the imm8 short form
             {!alu_form} picks for group-1 - TEST has no such alternative
             encoding to choose between. *)
          C.alt ~label:"test-rm-imm" ~priority:45
            (C.iso_fun ~name:"test-rm-imm"
               ~encode:(function
                 | Lowered.Test_rm_imm { width; rm; imm } ->
                     Some (prefixes_of ~width ~reg:0 ~rm, ((), ({ re_reg = 0; re_rm = rm }, imm)))
                 | _ -> None)
               ~decode:(fun (rex, ((), (e, imm))) ->
                 if e.re_reg <> 0 then None
                 else
                   let width = width_of_prefixes rex in
                   Some (Lowered.Test_rm_imm { width; rm = rm_of ~p:rex ~width e.re_rm; imm }))
               C.(
                 prefixes_codec ** const ~width:8 0xF7L ** rm_codec
                 ** le ~signedness:C.Signed ~width:32 "imm"));
          push_imm_form ~label:"push-imm8" ~priority:48 ~opcode_byte:0x6a ~imm_width:8;
          push_imm_form ~label:"push-imm32" ~priority:49 ~opcode_byte:0x68 ~imm_width:32;
          shld_imm_form ~label:"shld-imm-rm" ~priority:53;
          (* [0x9E] ({!Lowered.Sahf} - bare [sahf], M5, asm/docs/corpus.md): a fixed
             single-byte word, no ModR/M - {!Fucomp}/{!Fnstsw}'s exact shape at a
             one-byte-shorter opcode. *)
          C.alt ~label:"sahf" ~priority:63
            (C.iso_fun ~name:"sahf"
               ~encode:(function Lowered.Sahf -> Some () | _ -> None)
               ~decode:(fun () -> Some Lowered.Sahf)
               C.(const ~width:8 0x9EL));
        ])

  (* {2 Encode and decode} *)

  (* {2 Placements into fixups}

     x86 authors bits a byte at a time in memory order, so a placement's bit
     offset divided by eight *is* its byte offset, and a rel32/disp32 fixup is
     the four little-endian bytes starting there. The fixup node is deliberately
     not wrapped in [le]: the linker writes the container little-endian itself,
     and passing a placeholder through a byte-swapping [Iso_fun] would swap
     zeroes to no purpose and put the slices in the wrong place.

     [pc_bias] is the *realized* instruction length, taken from the encoding
     rather than from a per-mnemonic table: an x86 rel32 is relative to the end
     of the instruction, and that end moves with a REX prefix - a RIP-relative
     load is six bytes without one and seven with. *)
  let fixup_of_placement ~total_bytes (p : fixup_kind C.placement) =
    let kind = p.C.kind in
    let bits = List.map (fun (s : C.slice) -> s.C.bit_offset) p.C.slices in
    let first = List.fold_left min max_int bits in
    let width = List.fold_left (fun a (s : C.slice) -> a + s.C.bit_width) 0 p.C.slices in
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
              Asm_core.Lowered_ast.bit_offset = s.C.bit_offset - first;
              bit_width = s.C.bit_width;
              value_lsb = s.C.value_lsb;
            })
          p.C.slices;
      byte_offset = first / 8;
      container = (width + 7) / 8;
      pc_bias = (match kind with Abs32 | Abs64 -> 0 | _ -> total_bytes);
      range =
        (match kind with
        (* An absolute address is a bit pattern: 0x80000000 upwards (or, for
           [Abs64], 0x8000000000000000 upwards) is a perfectly good address,
           and a signed range would reject half of them. *)
        | Abs32 -> Asm_core.Lowered_ast.Bitpattern 32
        | Abs64 -> Asm_core.Lowered_ast.Bitpattern 64
        | Pcrel8_branch -> Asm_core.Lowered_ast.Signed 8
        | Pcrel32_branch | Pcrel32_call | Pcrel32_data -> Asm_core.Lowered_ast.Signed 32);
      value = Asm_core.Expr.Const (Bigint.of_int 0);
      pairing = Asm_core.Lowered_ast.Unpaired;
      origin = Origin.synthesized ~pass:"x86.encode" ();
    }

  (* The codec produces a placement; the expression to evaluate lives in the
     lowered value, because [Codec] sits below [asm_core] and cannot mention
     [Expr]. Pairing them is by name, which is what a placement's name is for. *)
  (* A memory operand's symbolic displacement, under the name its placement
     carries. Every lowered form that can hold a [Rm.Mem] can hold one, so this
     asks the operand rather than enumerating the forms. *)
  let disp_expr (rm : Rm.t) =
    match rm with Rm.Mem { Mem.disp = Disp.Sym e; _ } -> [ ("disp", e) ] | _ -> []

  let expr_of_lowered : Lowered.t -> (string * Asm_core.Expr.t) list = function
    | Lowered.Call_rel { target = Asm_core.Lowered_ast.Symbolic { value; _ } }
    | Lowered.Jmp_rel { target = Asm_core.Lowered_ast.Symbolic { value; _ } }
    | Lowered.Jcc_rel { target = Asm_core.Lowered_ast.Symbolic { value; _ }; _ } ->
        [ ("target", value) ]
    | Lowered.Alu_rm_imm { rm; imm; _ } -> (
        disp_expr rm @ match imm with Disp.Sym e -> [ ("imm", e) ] | Disp.Const _ -> [])
    | Lowered.Mov_rm_r { rm; _ }
    | Lowered.Mov_r_rm { rm; _ }
    | Lowered.Mov_rm_imm { rm; _ }
    | Lowered.Alu_rm_r { rm; _ }
    | Lowered.Imul_r_rm { rm; _ }
    | Lowered.Cmov_r_rm { rm; _ }
    | Lowered.Sse_binop_r_rm { rm; _ }
    | Lowered.Sse_mov_r_rm { rm; _ }
    | Lowered.Sse_mov_rm_r { rm; _ }
    | Lowered.Sse_binop_imm_r_rm { rm; _ }
    | Lowered.Jmp_rm { rm } ->
        disp_expr rm
    | Lowered.Lea { mem; _ } | Lowered.Fpu_mem { mem; _ } -> disp_expr (Rm.Mem mem)
    | Lowered.Mov_r_imm { imm = Disp.Sym e; _ } | Lowered.Push_imm { imm = Disp.Sym e } ->
        [ ("imm", e) ]
    | _ -> []

  let form_of l enc =
    let bytes = C.Bits.to_bytes enc.C.bits in
    let total_bytes = String.length bytes in
    let exprs = expr_of_lowered l in
    {
      Asm_core.Lowered_ast.bytes;
      form = C.form_id enc;
      fixups =
        List.filter_map
          (fun (p : fixup_kind C.placement) ->
            (* A placement with no expression belongs to a [Resolved] operand:
               its bits are already the real displacement, so there is nothing
               for the linker to do and no expression to give it. Dropping it
               here is what keeps [`Fixed] with an empty fixup list honest. *)
            match List.assoc_opt p.C.name exprs with
            | None -> None
            | Some value ->
                Some { (fixup_of_placement ~total_bytes p) with Asm_core.Lowered_ast.value })
          enc.C.placements;
    }

  (* Which interpreter to use is decided by the *lowered operand*, never by the
     codec: a resolved displacement that fits a short form also fits a long one,
     so "several rungs succeeded" cannot tell the two apart. *)
  let pinned_rung : Lowered.t -> string option = function
    | Lowered.Call_rel { target = Asm_core.Lowered_ast.Symbolic { rung; _ } }
    | Lowered.Jmp_rel { target = Asm_core.Lowered_ast.Symbolic { rung; _ } }
    | Lowered.Jcc_rel { target = Asm_core.Lowered_ast.Symbolic { rung; _ }; _ } ->
        rung
    | Lowered.Call_rel { target = Asm_core.Lowered_ast.Resolved { rung; _ } }
    | Lowered.Jmp_rel { target = Asm_core.Lowered_ast.Resolved { rung; _ } }
    | Lowered.Jcc_rel { target = Asm_core.Lowered_ast.Resolved { rung; _ }; _ } ->
        Some rung
    | _ -> None

  (* {2 Generated form rows (DEC-X86-TABLE)}

     A byte-level encoder and decoder for X86_table_rows, beside the codec: rows are tried only
     for mnemonics the hand-written forms do not know, and decoded only after the codec
     declines. Addressing follows the same rules as the codec ({!needs_sib}, {!disp_form_of}). *)

  module T = X86_table_row

  let table_rows = X86_table_rows.rows

  let table_applies (r : T.row) =
    r.mode = 0 || (r.mode = 64 && M.rex_allowed) || (r.mode = 32 && not M.rex_allowed)

  let table_reg_ok (cls : T.rclass) (r : Reg.t) =
    r.width = T.class_width cls && r.num >= 0 && (M.rex_allowed || r.num < 8)

  (* spl/bpl/sil/dil exist only with a REX prefix *)
  let byte_reg_needs_rex (r : Reg.t) = r.width = 8 && r.num >= 4 && r.num < 8

  let le_bytes n v =
    String.init n (fun i ->
        Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical v (8 * i)) 0xffL)))

  let fits_bytes n v =
    let bits = 8 * n in
    Int64.compare v (Int64.neg (Int64.shift_left 1L (bits - 1))) >= 0
    && (bits >= 64 || Int64.compare v (Int64.shift_left 1L bits) < 0)

  (* ModR/M, SIB and displacement for ModR/M.reg [reg] and an rm operand, with the REX.X and
     REX.B bits they need. Memory must be a constant displacement off address-width registers. *)
  let table_modrm ?(n = 1) ?vsib ~reg rm =
    let byte v = String.make 1 (Char.chr v) in
    match rm with
    (* EVEX.X extends a register rm to 16-31; below 16 it is 0 *)
    | `Reg n ->
        Some
          (byte (0xc0 lor ((reg land 7) lsl 3) lor (n land 7)), (n lsr 4) land 1, (n lsr 3) land 1)
    | `Mem (m : Mem.t) -> (
        let addr_ok = function
          | None -> true
          | Some (r : Reg.t) -> r.width = M.address_width && r.num >= 0 && not (is_rip r)
        in
        (* a VSIB index is a vector register of the row's class, and required *)
        let index_ok =
          match (vsib, m.index) with
          | None, index -> addr_ok index
          | Some w, Some (r : Reg.t) -> r.width = w && r.num >= 0 && r.num < 32
          | Some _, None -> false
        in
        match m.disp with
        | Disp.Sym _ -> None
        | Disp.Const disp when addr_ok m.base && index_ok && m.base <> None -> (
            (* EVEX stores disp8 scaled by N (disp8*N); a displacement that is not a multiple
               of N, or whose quotient does not fit a byte, is a full disp32 *)
            let form, disp =
              match disp_form_of m with
              | D_none -> (D_none, disp)
              | (D_8 | D_32) as form when n <= 1 -> (form, disp)
              | _ ->
                  let q = Int64.div disp (Int64.of_int n) in
                  if Int64.rem disp (Int64.of_int n) = 0L && fits_s8 q then (D_8, q)
                  else (D_32, disp)
            in
            let md = match form with D_none -> 0 | D_8 -> 1 | D_32 -> 2 in
            let disp_bytes =
              match form with D_none -> "" | D_8 -> le_bytes 1 disp | D_32 -> le_bytes 4 disp
            in
            let x = match m.index with Some i -> (i.num lsr 3) land 1 | None -> 0 in
            let b = match m.base with Some b -> (b.num lsr 3) land 1 | None -> 0 in
            if needs_sib m then
              match sib_of m with
              | None -> None
              | Some sb ->
                  Some
                    ( byte ((md lsl 6) lor ((reg land 7) lsl 3) lor 4)
                      ^ byte ((sb.sc lsl 6) lor (sb.ix lsl 3) lor sb.bs)
                      ^ disp_bytes,
                      x,
                      b )
            else
              match m.base with
              | Some base ->
                  Some
                    ( byte ((md lsl 6) lor ((reg land 7) lsl 3) lor (base.num land 7)) ^ disp_bytes,
                      x,
                      b )
              | None -> None)
        | Disp.Const _ -> None)

  let table_encode_row (r : T.row) ops =
    (* an opmask decorates the destination, AT&T's last operand *)
    let ops, opmask =
      match List.rev ops with
      | Operand.Masked { op; k; zero } :: rest -> (List.rev (op :: rest), Some (k, zero))
      | _ -> (ops, None)
    in
    let mask_ok =
      match (r.mask, opmask) with
      | 3, None -> false
      | _, None -> true
      | 0, Some _ -> false
      | 1, Some (k, _) -> k >= 1 && k <= 7
      | _, Some (k, zero) -> k >= 1 && k <= 7 && not zero
    in
    let accumulator k =
      match List.nth_opt ops k with
      | Some (Operand.Reg (reg : Reg.t)) -> reg.num = 0 && List.mem reg.width [ 8; 16; 32; 64 ]
      | _ -> false
    in
    if (not (table_applies r)) || (not mask_ok) || List.length ops <> List.length r.operands then
      None
    else if List.exists accumulator r.no_acc then None
    else
      let reg_field = ref (if r.digit >= 0 then Some r.digit else None) in
      let rm = ref None and vvvv = ref None and is4 = ref None and imms = ref [] in
      let opcode_low = ref 0 in
      let rounding = ref None and vsib = ref None in
      let rex_byte = ref false and ok = ref true in
      List.iter2
        (fun (o : T.operand) op ->
          match (o, op) with
          (* registers 16-31 exist only in EVEX's reg, rm and vvvv fields *)
          | T.Reg { cls; field }, Operand.Reg reg
            when table_reg_ok cls reg
                 && (reg.num < 16
                    || r.space = T.Evex
                       && (field = T.Modrm_reg || field = T.Modrm_rm || field = T.Vvvv)) -> (
              if byte_reg_needs_rex reg then rex_byte := true;
              match field with
              | T.Modrm_reg -> reg_field := Some reg.num
              | T.Modrm_rm -> rm := Some (`Reg reg.num)
              | T.Vvvv -> vvvv := Some reg.num
              | T.Is4 -> is4 := Some reg.num
              | T.Opcode_low -> opcode_low := reg.num)
          | T.Fixed_reg name, Operand.Reg reg when String.equal reg.name name -> ()
          | T.One, Operand.Imm v when Bigint.to_int_opt v = Some 1 -> ()
          (* vvvv holds the flags as they are, which the encoder below inverts *)
          | T.Dfv, Operand.Dfv v -> vvvv := Some (lnot v land 15)
          | T.Rounding { sae_only = true }, Operand.Rc 4 -> rounding := Some 0
          | T.Rounding { sae_only = false }, Operand.Rc n when n >= 0 && n <= 3 ->
              rounding := Some n
          | T.Mem _, Operand.Mem m -> rm := Some (`Mem m)
          | T.Vsib { cls }, Operand.Mem m ->
              vsib := Some (T.class_width cls);
              rm := Some (`Mem m)
          | T.Imm { bytes }, Operand.Imm v -> (
              match Bigint.to_int64_opt v with
              | Some v when fits_bytes bytes v -> imms := !imms @ [ le_bytes bytes v ]
              | _ -> ok := false)
          | _ -> ok := false)
        r.operands ops;
      if not !ok then None
      else
        let modrm =
          match (!reg_field, !rm) with
          | Some reg, Some rm ->
              Option.map (fun m -> (reg, m)) (table_modrm ~n:r.disp8n ?vsib:!vsib ~reg rm)
          | None, None -> Some (0, ("", 0, (!opcode_low lsr 3) land 1))
          (* a fixed ModR/M.reg or rm and no rm operand: register form, rm fixed or 0 ([lfence] is
             0F AE E8, [tilezero %tmm1] is ... 49 C8) *)
          | Some reg, None when r.digit >= 0 || r.rm >= 0 ->
              Some
                ( reg,
                  (String.make 1 (Char.chr (0xc0 lor ((reg land 7) lsl 3) lor max 0 r.rm)), 0, 0) )
          | _ -> None
        in
        let opcode = r.opcode lor (!opcode_low land 7) in
        match modrm with
        | None -> None
        | Some (reg, (modrm_bytes, x, b)) -> (
            let rr = (reg lsr 3) land 1 in
            let w = max 0 r.w and l = max 0 r.l in
            (* an is4 register shares its byte with a 4-bit immediate (vpermil2ps) *)
            let tail =
              match (!is4, !imms) with
              | Some n, [] -> Some (modrm_bytes ^ String.make 1 (Char.chr ((n land 15) lsl 4)))
              | Some n, [ imm ] when Char.code imm.[0] < 16 ->
                  Some
                    (modrm_bytes
                    ^ String.make 1 (Char.chr (((n land 15) lsl 4) lor Char.code imm.[0])))
              | Some _, _ -> None
              | None, imms -> Some (modrm_bytes ^ String.concat "" imms)
            in
            let byte v = String.make 1 (Char.chr v) in
            match tail with
            | None -> None
            | Some tail -> (
                match r.space with
                | T.Legacy ->
                    let need_rex = w = 1 || rr = 1 || x = 1 || b = 1 || !rex_byte in
                    if need_rex && not M.rex_allowed then None
                    else
                      let rex =
                        if need_rex then byte (0x40 lor (w lsl 3) lor (rr lsl 2) lor (x lsl 1) lor b)
                        else ""
                      in
                      let escape =
                        match r.map with
                        | 1 -> "\x0f"
                        | 2 -> "\x0f\x38"
                        | 3 -> "\x0f\x3a"
                        | 4 -> "\x0f\x0f"
                        | _ -> ""
                      in
                      (* 3DNow! (0F 0F) puts its opcode byte last, after ModR/M and displacement *)
                      let body = if r.map = 4 then tail ^ byte opcode else byte opcode ^ tail in
                      Some
                        ((if r.osz then "\x66" else "")
                        ^ (if r.prefix <> 0 then byte r.prefix else "")
                        ^ rex ^ escape ^ body)
                | T.Xop ->
                    (* 8F RXB.mmmmm W.vvvv.L.pp, always the three-byte form *)
                    if (not M.rex_allowed) && (rr = 1 || x = 1 || b = 1) then None
                    else
                      let v = lnot (Option.value !vvvv ~default:0) land 15 in
                      Some
                        ("\x8f"
                        ^ byte (((1 - rr) lsl 7) lor ((1 - x) lsl 6) lor ((1 - b) lsl 5) lor r.map)
                        ^ byte ((w lsl 7) lor (v lsl 3) lor (l lsl 2))
                        ^ byte opcode ^ tail)
                | T.Vex ->
                    if (not M.rex_allowed) && (rr = 1 || x = 1 || b = 1) then None
                    else
                      let pp = match r.prefix with 0x66 -> 1 | 0xf3 -> 2 | 0xf2 -> 3 | _ -> 0 in
                      let v = lnot (Option.value !vvvv ~default:0) land 15 in
                      let prefix =
                        if w = 0 && r.map = 1 && x = 0 && b = 0 then
                          "\xc5" ^ byte (((1 - rr) lsl 7) lor (v lsl 3) lor (l lsl 2) lor pp)
                        else
                          "\xc4"
                          ^ byte (((1 - rr) lsl 7) lor ((1 - x) lsl 6) lor ((1 - b) lsl 5) lor r.map)
                          ^ byte ((w lsl 7) lor (v lsl 3) lor (l lsl 2) lor pp)
                      in
                      Some (prefix ^ byte opcode ^ tail)
                | T.Evex ->
                    (* 62 P0 P1 P2 with k0 (no masking), no zeroing; registers 0-15. EVEX.b with a
                       register operand is embedded rounding, whose mode replaces L'L. *)
                    let v = Option.value !vvvv ~default:0 in
                    (* bit 4 of ModR/M.reg in R', of vvvv or a VSIB index in V' *)
                    let r' = (reg lsr 4) land 1 in
                    let v' =
                      match (!vsib, !rm) with
                      | Some _, Some (`Mem ({ Mem.index = Some i; _ } : Mem.t)) ->
                          (i.num lsr 4) land 1
                      | _ -> (v lsr 4) land 1
                    in
                    let v = v land 15 in
                    let b_bit, l = match !rounding with Some rc -> (1, rc) | None -> (0, l) in
                    let pp = match r.prefix with 0x66 -> 1 | 0xf3 -> 2 | 0xf2 -> 3 | _ -> 0 in
                    let p0 =
                      ((1 - rr) lsl 7)
                      lor ((1 - x) lsl 6)
                      lor ((1 - b) lsl 5)
                      lor ((1 - r') lsl 4)
                      lor r.map
                    in
                    let p1 = (w lsl 7) lor ((lnot v land 15) lsl 3) lor (1 lsl 2) lor pp in
                    let aaa, z =
                      match opmask with
                      | Some (k, zero) -> (k, if zero then 1 else 0)
                      | None -> (0, 0)
                    in
                    (* CCMP/CTEST's condition takes P2's low four bits, V' included *)
                    let scc = List.mem T.Dfv r.operands in
                    let p2 =
                      if scc then r.evex_p2
                      else
                        (z lsl 7) lor (l lsl 5) lor (b_bit lsl 4)
                        lor ((1 - v') lsl 3)
                        lor aaa lor r.evex_p2
                    in
                    if (not M.rex_allowed) && (rr = 1 || x = 1 || b = 1 || r' = 1 || v' = 1) then
                      None
                    else Some ("\x62" ^ byte p0 ^ byte p1 ^ byte p2 ^ byte opcode ^ tail)))

  (* Row [i] itself if its operands fit (a pseudo-prefix or the decoder chose it), else every
     applicable row spelled like it, in table order; the first whose operands fit. *)
  let table_encode (x : [ `Row of int ]) ops =
    let (`Row i) = x in
    let mnemonic = table_rows.(i).T.mnemonic in
    match table_encode_row table_rows.(i) ops with
    | Some _ as found -> found
    | None ->
        Array.fold_left
          (fun acc (r : T.row) ->
            match acc with
            | Some _ -> acc
            | None ->
                (* a pseudo-prefix-only row is reached only through it *)
                if String.equal r.mnemonic mnemonic && r.pseudo = "" then table_encode_row r ops
                else None)
          None table_rows

  (* GNU as's pseudo-prefixes, each a condition on the row: [{evex}] and [{vex}] the encoding
     space, [{load}] and [{store}] whether the destination is ModR/M.reg or ModR/M.rm. *)
  let pseudo_prefix_allows prefix (r : T.row) =
    let dest_field () =
      match List.rev r.operands with T.Reg { field; _ } :: _ -> Some field | _ -> None
    in
    if r.pseudo <> "" then String.equal r.pseudo prefix
    else
      match prefix with
      | "evex" -> r.space = T.Evex
      | "vex" -> r.space = T.Vex
      | "load" -> dest_field () = Some T.Modrm_reg
      | "store" -> dest_field () = Some T.Modrm_rm
      | _ -> false

  (* [{evex} vaddps] -> [Some ("evex", "vaddps")] *)
  let split_pseudo_prefix m =
    if String.length m > 0 && m.[0] = '{' then
      match String.index_opt m '}' with
      | Some k when k + 1 < String.length m && m.[k + 1] = ' ' ->
          Some (String.sub m 1 (k - 1), String.sub m (k + 2) (String.length m - k - 2))
      | _ -> None
    else None

  let table_index mnemonic =
    let rec go i =
      if i >= Array.length table_rows then None
      else if String.equal table_rows.(i).T.mnemonic mnemonic && table_applies table_rows.(i) then
        Some i
      else go (i + 1)
    in
    go 0

  (* Decoding: prefixes (0x66, F2/F3, REX), then VEX or a legacy escape, then the opcode; every
     row with that space, map and opcode is tried in table order. *)
  let table_decode bytes pos =
    let n = String.length bytes in
    let at k = if k < n then Some (Char.code bytes.[k]) else None in
    let rec prefixes k osz rep =
      match at k with
      | Some 0x66 -> prefixes (k + 1) true rep
      (* F0 (lock) sits in the same slot: a row names at most one of them *)
      | Some ((0xf0 | 0xf2 | 0xf3) as p) -> prefixes (k + 1) osz p
      | _ -> (k, osz, rep)
    in
    let k, osz, rep = prefixes pos false 0 in
    (* EVEX.b: embedded rounding for a register form *)
    let evex_b = ref 0 and evex_aaa = ref 0 and evex_z = ref 0 in
    (* EVEX.R' and EVEX.V' (register bit 4), as 1 when set *)
    let evex_r4 = ref 0 and evex_v4 = ref 0 in
    let k, rex =
      match at k with Some b when M.rex_allowed && b land 0xf0 = 0x40 -> (k + 1, b) | _ -> (k, 0)
    in
    let vex =
      match (at k, at (k + 1), at (k + 2)) with
      | Some 0x62, Some p0, Some p1
        when rex = 0 && (M.rex_allowed || p0 land 0xc0 = 0xc0) && p1 land 4 = 4 && k + 3 < n ->
          (* EVEX with registers 0-15 *)
          let p2 = Char.code bytes.[k + 3] in
          if (not M.rex_allowed) && (p0 land 0x10 = 0 || p2 land 0x08 = 0) then None
          else (
            evex_r4 := 1 - ((p0 lsr 4) land 1);
            evex_v4 := 1 - ((p2 lsr 3) land 1);
            evex_b := (p2 lsr 4) land 1;
            evex_aaa := p2 land 7;
            evex_z := (p2 lsr 7) land 1;
            Some
              ( k + 4,
                `Evex
                  ( 1 - ((p0 lsr 7) land 1),
                    1 - ((p0 lsr 6) land 1),
                    1 - ((p0 lsr 5) land 1),
                    p0 land 7,
                    (p1 lsr 7) land 1,
                    lnot (p1 lsr 3) land 15,
                    (p2 lsr 5) land 3,
                    p1 land 3 ) ))
      | Some 0xc5, Some b1, _ when rex = 0 && (M.rex_allowed || b1 land 0xc0 = 0xc0) ->
          Some
            ( k + 2,
              `Vex
                ( 1 - ((b1 lsr 7) land 1),
                  0,
                  0,
                  1,
                  0,
                  lnot (b1 lsr 3) land 15,
                  (b1 lsr 2) land 1,
                  b1 land 3 ) )
      (* XOP: 8F with a map of 8 or more (POP's ModR/M.reg is 0, so its low five bits are below
         8) *)
      | Some 0x8f, Some b1, Some b2
        when rex = 0 && b1 land 31 >= 8 && (M.rex_allowed || b1 land 0xc0 = 0xc0) ->
          Some
            ( k + 3,
              `Xop
                ( 1 - ((b1 lsr 7) land 1),
                  1 - ((b1 lsr 6) land 1),
                  1 - ((b1 lsr 5) land 1),
                  b1 land 31,
                  (b2 lsr 7) land 1,
                  lnot (b2 lsr 3) land 15,
                  (b2 lsr 2) land 1,
                  b2 land 3 ) )
      | Some 0xc4, Some b1, Some b2 when rex = 0 && (M.rex_allowed || b1 land 0xc0 = 0xc0) ->
          Some
            ( k + 3,
              `Vex
                ( 1 - ((b1 lsr 7) land 1),
                  1 - ((b1 lsr 6) land 1),
                  1 - ((b1 lsr 5) land 1),
                  b1 land 31,
                  (b2 lsr 7) land 1,
                  lnot (b2 lsr 3) land 15,
                  (b2 lsr 2) land 1,
                  b2 land 3 ) )
      | _ -> None
    in
    let space, k =
      match vex with
      | Some (k, v) -> (v, k)
      | None -> (
          match (at k, at (k + 1)) with
          | Some 0x0f, Some 0x38 -> (`Legacy 2, k + 2)
          | Some 0x0f, Some 0x3a -> (`Legacy 3, k + 2)
          | Some 0x0f, Some 0x0f -> (`Legacy 4, k + 2)
          | Some 0x0f, _ -> (`Legacy 1, k + 1)
          | _ -> (`Legacy 0, k))
    in
    (* 3DNow!'s opcode byte comes last: read it per row, after the operands *)
    match match space with `Legacy 4 -> Some (-1) | _ -> at k with
    | None -> None
    | Some opcode ->
        let k = if opcode < 0 then k else k + 1 in
        let try_row i (r : T.row) =
          let header_ok, rr, xx, bb, w, vvvv, l =
            match (space, r.space) with
            | `Legacy map, T.Legacy ->
                let prefix_ok =
                  match r.prefix with
                  | 0x66 -> osz && rep = 0 && not r.osz
                  | 0 -> rep = 0 && osz = r.osz
                  | p -> rep = p && osz = r.osz
                in
                let w = (rex lsr 3) land 1 in
                ( map = r.map && prefix_ok && (r.w < 0 || r.w = w),
                  (rex lsr 2) land 1,
                  (rex lsr 1) land 1,
                  rex land 1,
                  w,
                  0,
                  0 )
            | `Vex (rr, xx, bb, map, w, vvvv, l, pp), T.Vex
            | `Xop (rr, xx, bb, map, w, vvvv, l, pp), T.Xop
            | `Evex (rr, xx, bb, map, w, vvvv, l, pp), T.Evex ->
                let want = match r.prefix with 0x66 -> 1 | 0xf3 -> 2 | 0xf2 -> 3 | _ -> 0 in
                ( map = r.map && pp = want && (not osz) && rep = 0
                  && (r.w < 0 || r.w = w)
                  && (r.l < 0 || r.l = l)
                  && (List.exists
                        (function T.Reg { field = T.Vvvv; _ } | T.Dfv -> true | _ -> false)
                        r.operands
                     || vvvv = 0
                        && (!evex_v4 = 0
                           || List.exists (function T.Vsib _ -> true | _ -> false) r.operands)),
                  rr,
                  xx,
                  bb,
                  w,
                  vvvv,
                  l )
            | _ -> (false, 0, 0, 0, 0, 0, 0)
          in
          ignore w;
          (* a rounding row is exactly the EVEX.b register form *)
          let rounding_row =
            List.exists (function T.Rounding _ -> true | _ -> false) r.operands
          in
          let header_ok =
            header_ok
            && (match r.space with
              | T.Evex when r.map = 4 || r.evex_p2 <> 0 -> true
              | T.Evex -> !evex_b = 1 = rounding_row
              | _ -> true)
            (* the opmask the row allows; an APX row's ND and NF bits sit where EVEX.b and aaa do *)
            && (match r.space with
              | T.Evex when List.mem T.Dfv r.operands ->
                  (!evex_b lsl 4) lor ((1 - !evex_v4) lsl 3) lor !evex_aaa = r.evex_p2
                  && !evex_z = 0
              | T.Evex when r.map = 4 || r.evex_p2 <> 0 ->
                  (!evex_b lsl 4) lor !evex_aaa = r.evex_p2 && !evex_z = 0
              | T.Evex -> (
                  match r.mask with
                  | 0 -> !evex_aaa = 0 && !evex_z = 0
                  | 1 -> !evex_z = 0 || !evex_aaa <> 0
                  | 2 -> !evex_z = 0
                  | _ -> !evex_z = 0 && !evex_aaa <> 0)
              | _ -> true)
            && List.for_all
                 (function T.Rounding { sae_only = true } -> l = 0 | _ -> true)
                 r.operands
          in
          let low =
            List.exists
              (function T.Reg { field = T.Opcode_low; _ } -> true | _ -> false)
              r.operands
          in
          let opcode_ok =
            if opcode < 0 then true
            else if low then opcode land 0xf8 = r.opcode
            else r.opcode = opcode
          in
          if (not header_ok) || (not opcode_ok) || not (table_applies r) then None
          else
            let uses_modrm =
              r.digit >= 0
              || List.exists
                   (function
                     | T.Reg { field = T.Modrm_reg | T.Modrm_rm; _ } | T.Mem _ | T.Vsib _ -> true
                     | _ -> false)
                   r.operands
            in
            (* a VSIB row's index register class *)
            let vsib =
              List.find_map (function T.Vsib { cls } -> Some cls | _ -> None) r.operands
            in
            let has_mem =
              List.exists (function T.Mem _ | T.Vsib _ -> true | _ -> false) r.operands
            in
            let rm_reg =
              List.exists
                (function T.Reg { field = T.Modrm_rm; _ } -> true | _ -> false)
                r.operands
            in
            let modrm = if uses_modrm then at k else Some 0 in
            match modrm with
            | None -> None
            | Some mb -> (
                let evex = r.space = T.Evex in
                let md = mb lsr 6
                and reg = (mb lsr 3) land 7 lor (rr lsl 3) lor if evex then !evex_r4 lsl 4 else 0
                and rmf = mb land 7 in
                let k = if uses_modrm then k + 1 else k in
                if uses_modrm && r.digit >= 0 && (mb lsr 3) land 7 <> r.digit then None
                else if uses_modrm && has_mem && md = 3 then None
                else if uses_modrm && rm_reg && md <> 3 then None
                else if
                  uses_modrm && (not has_mem) && (not rm_reg) && mb land 0xc7 <> 0xc0 lor max 0 r.rm
                then None
                else
                  (* the memory operand, if any: SIB and displacement *)
                  let mem, k =
                    if not (has_mem && md <> 3) then (Some None, k)
                    else
                      let regat num = reg_at ~width:M.address_width num in
                      let disp k md =
                        match md with
                        | 1 ->
                            Option.map
                              (fun b ->
                                ( Int64.mul (Int64.of_int r.disp8n)
                                    (Int64.of_int (if b >= 128 then b - 256 else b)),
                                  k + 1 ))
                              (at k)
                        | 2 ->
                            if k + 4 > n then None
                            else
                              let v = ref 0L in
                              for j = 3 downto 0 do
                                v :=
                                  Int64.logor (Int64.shift_left !v 8)
                                    (Int64.of_int (Char.code bytes.[k + j]))
                              done;
                              Some (Int64.of_int32 (Int64.to_int32 !v), k + 4)
                        | _ -> Some (0L, k)
                      in
                      if rmf = 4 then
                        match at k with
                        | None -> (None, k)
                        | Some sb -> (
                            let sc = sb lsr 6
                            and ix = (sb lsr 3) land 7 lor (xx lsl 3)
                            and bs = sb land 7 lor (bb lsl 3) in
                            if bs land 7 = 5 && md = 0 then (None, k)
                            else
                              match disp (k + 1) md with
                              | None -> (None, k)
                              | Some (d, k) ->
                                  let index =
                                    match vsib with
                                    | Some cls ->
                                        Some
                                          (reg_at ~width:(T.class_width cls)
                                             (ix lor if evex then !evex_v4 lsl 4 else 0))
                                    | None -> if ix = 4 then None else Some (regat ix)
                                  in
                                  ( Some
                                      (Some
                                         {
                                           Mem.base = Some (regat bs);
                                           index;
                                           scale = scale_of_log2 sc;
                                           disp = Disp.Const d;
                                         }),
                                    k ))
                      else if rmf = 5 && md = 0 then (None, k)
                      else if vsib <> None then (None, k)
                      else
                        match disp k md with
                        | None -> (None, k)
                        | Some (d, k) ->
                            ( Some
                                (Some
                                   {
                                     Mem.base = Some (regat (rmf lor (bb lsl 3)));
                                     index = None;
                                     scale = 1;
                                     disp = Disp.Const d;
                                   }),
                              k )
                  in
                  match mem with
                  | None -> None
                  | Some mem ->
                      let k = ref k and failed = ref false in
                      let ops =
                        List.map
                          (fun (o : T.operand) ->
                            match o with
                            | T.Reg { cls; field } ->
                                let num =
                                  match field with
                                  | T.Modrm_reg -> reg
                                  | T.Modrm_rm ->
                                      rmf lor (bb lsl 3) lor if evex then xx lsl 4 else 0
                                  | T.Vvvv -> vvvv lor if evex then !evex_v4 lsl 4 else 0
                                  | T.Is4 -> ( match at (n - 1) with _ -> 0)
                                  | T.Opcode_low -> opcode land 7 lor (bb lsl 3)
                                in
                                if cls = T.Gpr8 && num >= 4 && num < 8 && rex = 0 then
                                  failed := true;
                                if (cls = T.Mmx || cls = T.Kmask || cls = T.Tmm) && num >= 8 then
                                  failed := true;
                                Operand.Reg (reg_at ~width:(T.class_width cls) num)
                            | T.Rounding { sae_only } -> Operand.Rc (if sae_only then 4 else l)
                            | T.One -> Operand.Imm Bigint.one
                            | T.Dfv -> Operand.Dfv (lnot vvvv land 15)
                            | T.Fixed_reg name -> (
                                match find_reg name with
                                | Some reg -> Operand.Reg reg
                                | None ->
                                    failed := true;
                                    Operand.Imm Bigint.zero)
                            | T.Mem _ | T.Vsib _ -> (
                                match mem with
                                | Some m -> Operand.Mem m
                                | None ->
                                    failed := true;
                                    Operand.Imm Bigint.zero)
                            | T.Imm _
                              when List.exists
                                     (function T.Reg { field = T.Is4; _ } -> true | _ -> false)
                                     r.operands ->
                                (* filled from the is4 byte below *)
                                Operand.Imm Bigint.zero
                            | T.Imm { bytes = nb } ->
                                if !k + nb > n then (
                                  failed := true;
                                  Operand.Imm Bigint.zero)
                                else
                                  let v = ref 0L in
                                  for j = nb - 1 downto 0 do
                                    v :=
                                      Int64.logor (Int64.shift_left !v 8)
                                        (Int64.of_int (Char.code bytes.[!k + j]))
                                  done;
                                  k := !k + nb;
                                  Operand.Imm (Bigint.of_int64 !v))
                          r.operands
                      in
                      (* an is4 register sits in the byte after everything else *)
                      let ops, k =
                        if
                          List.exists
                            (function T.Reg { field = T.Is4; _ } -> true | _ -> false)
                            r.operands
                        then
                          match at !k with
                          | None ->
                              failed := true;
                              (ops, !k)
                          | Some b ->
                              ( List.map2
                                  (fun (o : T.operand) op ->
                                    match o with
                                    | T.Reg { cls; field = T.Is4 } ->
                                        Operand.Reg (reg_at ~width:(T.class_width cls) (b lsr 4))
                                    | T.Imm _ -> Operand.Imm (Bigint.of_int (b land 15))
                                    | _ -> op)
                                  r.operands ops,
                                !k + 1 )
                        else (ops, !k)
                      in
                      (* an opmask decorates the destination *)
                      let ops =
                        if r.space = T.Evex && r.map <> 4 && r.evex_p2 = 0 && !evex_aaa <> 0 then
                          match List.rev ops with
                          | last :: rest ->
                              List.rev
                                (Operand.Masked { op = last; k = !evex_aaa; zero = !evex_z = 1 }
                                :: rest)
                          | [] -> ops
                        else ops
                      in
                      let k, failed =
                        if opcode < 0 then
                          if at k = Some r.opcode then (k + 1, !failed) else (k, true)
                        else (k, !failed)
                      in
                      if failed then None
                      else Some (Instruction.mk (Opcode.Table i) 0 ops, r.mnemonic, k - pos))
        in
        let rec go i =
          if i >= Array.length table_rows then None
          else match try_row i table_rows.(i) with Some _ as found -> found | None -> go (i + 1)
        in
        go 0

  (* A mnemonic the hand-written forms do not know may be a generated row's. *)
  let simplify_instruction_ungated (s : Surface.t) =
    (* [rep movsb] reaches here as the mnemonic [rep] with a symbol operand [movsb]: a repeat
       prefix names the string-op row it is spelled with *)
    let s =
      match (s.Surface.mnemonic, s.Surface.ops) with
      | ( (("rep" | "repe" | "repz" | "repne" | "repnz") as p),
          [ Operand.Sym (Asm_core.Expr.Symbol op) ] ) ->
          let p = match p with "repz" -> "repe" | "repnz" -> "repne" | p -> p in
          { s with mnemonic = p ^ " " ^ op; ops = [] }
      (* [lock addl $1, (%rax)]: the parser hands the instruction over as a leading symbol *)
      | "lock", Operand.Sym (Asm_core.Expr.Symbol op) :: ops ->
          { s with mnemonic = "lock " ^ op; ops }
      | _ -> s
    in
    match split_pseudo_prefix s.Surface.mnemonic with
    | Some (prefix, m) -> (
        (* only a generated row can honour a pseudo-prefix: the first of the spelling's rows
           the prefix allows whose operands fit *)
        let rec go i =
          if i >= Array.length table_rows then None
          else
            let r = table_rows.(i) in
            if
              String.equal r.T.mnemonic m && table_applies r && pseudo_prefix_allows prefix r
              && table_encode_row r s.Surface.ops <> None
            then Some i
            else go (i + 1)
        in
        match go 0 with
        | Some i -> Ok (Instruction.mk (Opcode.Table i) 0 s.Surface.ops)
        | None -> Error (diag ~pos:__POS__ (`Unknown_instruction s.Surface.mnemonic)))
    | None -> (
        (* GNU as lets a register operand fix the operand size, and rejects a suffix on most newer
       integer instructions ([rdrand %eax]), so for a mnemonic the hand-written forms do not know
       the register also names the sized row. A hand-written mnemonic keeps requiring its suffix
       (see DEC-X86-SUFFIX). *)
        let inferred =
          List.filter_map
            (function
              | Operand.Reg (r : Reg.t) when List.mem r.width [ 8; 16; 32; 64 ] ->
                  Some (s.Surface.mnemonic ^ suffix_of_width r.width)
              | _ -> None)
            s.Surface.ops
        in
        let row spellings =
          List.find_map
            (fun m ->
              match table_index m with
              | Some i when table_encode (`Row i) s.Surface.ops <> None ->
                  Some (Instruction.mk (Opcode.Table i) 0 s.Surface.ops)
              | _ -> None)
            spellings
        in
        (* a vector index register is a VSIB address, which only a generated row takes: the
           hand-written forms would read its number as a GPR's *)
        (* so is a register numbered 16-31 (EVEX only) *)
        let rec upper = function
          | Operand.Reg (r : Reg.t) -> r.num >= 16
          | Operand.Masked { op; _ } -> upper op
          | Operand.Mem { Mem.index = Some (i : Reg.t); _ } -> i.num >= 16
          | _ -> false
        in
        let vector_index =
          List.exists
            (function
              | Operand.Mem { Mem.index = Some (i : Reg.t); _ } ->
                  not (List.mem i.width [ 16; 32; 64 ])
              | Operand.Masked { op = Operand.Mem { Mem.index = Some (i : Reg.t); _ }; _ } ->
                  not (List.mem i.width [ 16; 32; 64 ])
              | op -> upper op)
            s.Surface.ops
        in
        if vector_index then
          match row [ s.Surface.mnemonic ] with
          | Some i -> Ok i
          | None -> Error (diag ~pos:__POS__ (`No_form s.Surface.mnemonic))
        else
          (* a spelling the hand-written forms reject (unknown, or [movq] with an xmm operand in 32-bit
       mode), or accept but cannot lower for these operands (a high VEX register), may be a
       generated row's *)
          match simplify_hand_written s with
          | Error e as err -> (
              let spellings =
                match Target_error.kind (Err.Error.kind e) with
                | `Unknown_instruction _ -> s.Surface.mnemonic :: inferred
                | _ -> [ s.Surface.mnemonic ]
              in
              match row spellings with Some i -> Ok i | None -> err)
          | Ok i as ok -> (
              match lower_hand_written i with
              | Ok _ -> ok
              | Error _ -> ( match row [ s.Surface.mnemonic ] with Some t -> Ok t | None -> ok)))

  (* A hand-written mnemonic can have generated rows for shapes its own forms do not take (a
     ymm [vpslldq]); those are tried when the hand-written lowering declines. *)
  let lower_instruction_ungated (i : Instruction.t) =
    match i.op with
    | Opcode.Table row -> Ok [ Lowered.Table { row; ops = i.ops } ]
    | _ -> (
        match lower_hand_written i with
        | Ok _ as ok -> ok
        | Error _ as declined -> (
            let spellings = [ Opcode.name i.op; Opcode.name i.op ^ suffix_of_width i.width ] in
            match List.find_map table_index spellings with
            | Some row when table_encode (`Row row) i.ops <> None ->
                Ok [ Lowered.Table { row; ops = i.ops } ]
            | _ -> declined))

  let encode_ungated l =
    (* The codec names the phase when it is the only layer that could have seen
       the mistake - a pin for a rung that does not exist - and otherwise this
       one does. A bad suffix in *source* never reaches here: [simplify] rejects
       it with a span, which is why [codec.unknown-rung] means a direct-lowered
       producer and nothing else. *)
    let fail (e : C.error) = Error (diag ~pos:__POS__ (`Codec e)) in
    match l with
    | Lowered.Table x -> (
        match table_encode (`Row x.row) x.ops with
        | Some bytes ->
            Ok
              (`Fixed
                 { Asm_core.Lowered_ast.bytes; form = table_rows.(x.row).T.mnemonic; fixups = [] })
        | None -> Error (diag ~pos:__POS__ (`No_form table_rows.(x.row).T.mnemonic)))
    | _ -> (
        match pinned_rung l with
        | Some rung -> (
            match C.encode_rung codec ~rung l with
            | Error e -> fail e
            | Ok enc -> Ok (`Fixed (form_of l enc)))
        | None -> (
            match C.encode_ladder codec l with
            | Error e -> fail e
            | Ok [ one ] -> Ok (`Fixed (form_of l one))
            | Ok forms -> Ok (`Relax (List.map (form_of l) forms))))

  type decode_context = { state : target_state; address : int64 }

  (* The normalized instruction a lowered one came from. For M1 this is total
     and information-preserving, because no x86 form in the fixtures is a
     pseudo; the moment one is, this stops being an inverse and the disassembler
     will need the lowered form directly. *)
  let instruction_of_lowered ?(at = 0L) ?(len = 0) =
    (* A PC-relative operand prints as where it goes, which needs the address it
       is at and how long it turned out to be - both of which only the decode
       context knows. A still-symbolic one has no value yet and prints as its
       own address, which no test relies on: lowering never round-trips through
       text. *)
    let absolute_target = function
      | Asm_core.Lowered_ast.Resolved { value; _ } ->
          Int64.add at (Int64.add (Int64.of_int len) value)
      | Asm_core.Lowered_ast.Symbolic _ -> at
    in
    let rung_of = function
      | Asm_core.Lowered_ast.Resolved { rung; _ } -> Some rung
      | Asm_core.Lowered_ast.Symbolic { rung; _ } -> rung
    in
    function
    | Lowered.Alu_rm_imm { ext; width; rm; imm } -> (
        match Opcode.of_ext ext with
        | None -> None
        | Some op ->
            Some
              {
                Instruction.op;
                width;
                ops =
                  [
                    (match imm with
                    | Disp.Const v -> Operand.Imm (Bigint.of_int64 v)
                    | Disp.Sym e -> Operand.Imm_sym e);
                    (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                  ];
                form = None;
              })
    | Lowered.Mov_r_imm { width; reg; imm } ->
        Some
          {
            Instruction.op = Opcode.Mov;
            width;
            ops =
              [
                (match imm with
                | Disp.Const v -> Operand.Imm (Bigint.of_int64 v)
                | Disp.Sym e -> Operand.Imm_sym e);
                Operand.Reg reg;
              ];
            form = None;
          }
    | Lowered.Mov_rm_r { width; rm; reg } ->
        Some
          {
            Instruction.op = Opcode.Mov;
            width;
            ops =
              [
                Operand.Reg reg;
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
              ];
            form = None;
          }
    | Lowered.Mov_r_rm { width; reg; rm } ->
        Some
          {
            Instruction.op = Opcode.Mov;
            width;
            ops =
              [
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                Operand.Reg reg;
              ];
            form = None;
          }
    | Lowered.Lea { width; reg; mem } ->
        Some (Instruction.mk Opcode.Lea width [ Operand.Mem mem; Operand.Reg reg ])
    | Lowered.Ret -> Some (Instruction.mk Opcode.Ret M.address_width [])
    | Lowered.Mov_rm_imm { width; rm; imm } ->
        Some
          {
            Instruction.op = Opcode.Mov;
            width;
            ops =
              [
                Operand.Imm (Bigint.of_int64 imm);
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
              ];
            form = None;
          }
    | Lowered.Alu_rm_r { op; width; rm; reg } ->
        Some
          {
            Instruction.op;
            width;
            ops =
              [
                Operand.Reg reg;
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
              ];
            form = None;
          }
    (* Source first, destination last, as everywhere else here - and for these
       two that puts the r/m operand first, because the register is what they
       write. *)
    | Lowered.Imul_r_rm { width; reg; rm } ->
        Some
          {
            Instruction.op = Opcode.Imul;
            width;
            ops =
              [
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                Operand.Reg reg;
              ];
            form = None;
          }
    (* [reg] is dropped: {!Lowered.Imul_r_rm_imm}'s own comment says why it
       always equals [rm] as constructed here, so only [rm] needs to survive
       into the two-operand AT&T spelling. *)
    | Lowered.Imul_r_rm_imm { width; imm; rm; _ } ->
        Some
          (Instruction.mk Opcode.Imul width
             [
               Operand.Imm (Bigint.of_int64 imm);
               (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
             ])
    | Lowered.Test_rm_imm { width; imm; rm } ->
        Some
          (Instruction.mk Opcode.Test width
             [
               Operand.Imm (Bigint.of_int64 imm);
               (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
             ])
    | Lowered.Cmov_r_rm { cc; width; reg; rm } ->
        Some
          {
            Instruction.op = Opcode.Cmov cc;
            width;
            ops =
              [
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                Operand.Reg reg;
              ];
            form = None;
          }
    | Lowered.Ud2 -> Some (Instruction.mk Opcode.Ud2 M.address_width [])
    | Lowered.Pop { reg } -> Some (Instruction.mk Opcode.Pop M.address_width [ Operand.Reg reg ])
    | Lowered.Jmp_rm { rm } ->
        Some
          {
            Instruction.op = Opcode.Jmp;
            width = M.address_width;
            ops = [ (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m) ];
            form = None;
          }
    (* Rendered as the absolute target, not the displacement: that is what
       objdump prints, and it is the only spelling re-parsing can mean - text
       saying [call -6] would reassemble against a different address. [at] and
       [len] come from the decode context, which exists because a PC-relative
       operand cannot be printed without them. *)
    | Lowered.Call_rel { target } ->
        Some
          (Instruction.mk Opcode.Call M.address_width
             [ Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 (absolute_target target))) ])
    (* The two ladder forms carry their rung out to the text, which is B10: the
       image legitimately contains a near branch whose final displacement would
       also have fitted a short one - alignment padding and a cross-section
       target both produce them - and reassembling that as short would change
       the bytes. So the pin is written down for *every* decoded branch, short
       ones included. There is no address-independent correct unpinned choice to
       fall back to: re-parsed text gives back a bare constant with no defining
       symbol and no section, which layout cannot decide either way. *)
    | Lowered.Jmp_rel { target } ->
        Some
          (Instruction.mk ?form:(rung_of target) Opcode.Jmp M.address_width
             [ Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 (absolute_target target))) ])
    | Lowered.Jcc_rel { cc; target } ->
        Some
          (Instruction.mk ?form:(rung_of target) (Opcode.Jcc cc) M.address_width
             [ Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 (absolute_target target))) ])
    | Lowered.Push { reg } -> Some (Instruction.mk Opcode.Push M.address_width [ Operand.Reg reg ])
    | Lowered.Push_imm { imm } ->
        Some
          (Instruction.mk Opcode.Push M.address_width
             [
               (match imm with
               | Disp.Const v -> Operand.Imm (Bigint.of_int64 v)
               | Disp.Sym e -> Operand.Imm_sym e);
             ])
    | Lowered.Dec { reg } -> Some (Instruction.mk Opcode.Dec M.address_width [ Operand.Reg reg ])
    | Lowered.Unary_rm { ext; width; rm } -> (
        match Opcode.of_unary_ext ext with
        | None -> None
        | Some op ->
            Some
              (Instruction.mk op width
                 [ (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m) ]))
    | Lowered.Alu_r_rm { op; width; reg; rm } ->
        Some
          {
            Instruction.op;
            width;
            ops =
              [
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                Operand.Reg reg;
              ];
            form = None;
          }
    | Lowered.Shift1_rm { ext; width; rm } -> (
        match Opcode.of_shift1_ext ext with
        | None -> None
        | Some op ->
            Some
              (Instruction.mk op width
                 [
                   Operand.Imm (Bigint.of_int64 1L);
                   (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                 ]))
    | Lowered.Shift_imm_rm { ext; width; rm; imm } -> (
        match Opcode.of_shift1_ext ext with
        | None -> None
        | Some op ->
            Some
              (Instruction.mk op width
                 [
                   Operand.Imm (Bigint.of_int64 imm);
                   (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                 ]))
    | Lowered.Shift_cl_rm { ext; width; rm } -> (
        match Opcode.of_shift1_ext ext with
        | None -> None
        | Some op ->
            Some
              (Instruction.mk op width
                 [
                   Operand.Reg (reg_at ~width:8 1);
                   (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                 ]))
    | Lowered.Shld_imm_rm { width; reg; rm; imm } ->
        Some
          (Instruction.mk Opcode.Shld width
             [
               Operand.Imm (Bigint.of_int64 imm);
               Operand.Reg reg;
               (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
             ])
    | Lowered.Setcc_rm { cc; rm } ->
        Some
          (Instruction.mk (Opcode.Setcc cc) 8
             [ (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m) ])
    | Lowered.Movx_r_rm { zero_extend; src_width; width; reg; rm } ->
        Some
          (Instruction.mk
             (if zero_extend then Opcode.Movzx { src_width } else Opcode.Movsx { src_width })
             width
             [
               (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
               Operand.Reg reg;
             ])
    | Lowered.Movsxd_r_rm { reg; rm } ->
        Some
          (Instruction.mk
             (Opcode.Movsx { src_width = 32 })
             64
             [
               (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
               Operand.Reg reg;
             ])
    (* The SSE forms' [Instruction.width] is not the 128-bit xmm operand's
       width - nothing here ever prints a size suffix for these opcodes (see
       {!Instruction.pp}'s no-suffix group), so [width] only has to be
       whatever value keeps REX.W correct if this instruction were
       re-encoded, which for a pure xmm/xmm or xmm/mem form is "cleared" -
       hence the [32] sentinel, exactly as the REX-suppression cases
       elsewhere use. [Cvtsi2f_r_rm]/[Cvtf2i_r_rm] carry their own real GPR
       width and use it here instead. *)
    | Lowered.Sse_binop_r_rm { op; reg; rm } ->
        Some
          {
            Instruction.op;
            width = 32;
            ops =
              [
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                Operand.Reg reg;
              ];
            form = None;
          }
    | Lowered.Sse_binop_imm_r_rm { op; reg; rm; imm } ->
        Some
          {
            Instruction.op;
            width = 32;
            ops =
              [
                Operand.Imm (Bigint.of_int64 imm);
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                Operand.Reg reg;
              ];
            form = None;
          }
    | Lowered.Sse_mov_r_rm { op; reg; rm } ->
        Some
          {
            Instruction.op;
            width = 32;
            ops =
              [
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                Operand.Reg reg;
              ];
            form = None;
          }
    | Lowered.Sse_mov_rm_r { op; rm; reg } ->
        Some
          {
            Instruction.op;
            width = 32;
            ops =
              [
                Operand.Reg reg;
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
              ];
            form = None;
          }
    | Lowered.Cvtsi2f_r_rm { op; width; reg; rm } ->
        Some
          {
            Instruction.op;
            width;
            ops =
              [
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                Operand.Reg reg;
              ];
            form = None;
          }
    | Lowered.Cvtf2i_r_rm { width; reg; rm } ->
        Some
          {
            Instruction.op = Opcode.Cvttsd2si;
            width;
            ops =
              [
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                Operand.Reg reg;
              ];
            form = None;
          }
    | Lowered.Movd_rm_r { op; width; rm; reg } ->
        Some
          {
            Instruction.op;
            width;
            ops =
              [
                Operand.Reg reg;
                (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
              ];
            form = None;
          }
    | Lowered.Vex_binop_rr_rm { op; dst; src1; src2 } ->
        Some
          (Instruction.mk op 32
             [
               (match src2 with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
               Operand.Reg src1;
               Operand.Reg dst;
             ])
    | Lowered.Vex_unop_r_rm { op; dst; src } ->
        Some
          (Instruction.mk op 32
             [
               (match src with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
               Operand.Reg dst;
             ])
    | Lowered.Vex_binop_imm_rr_rm { op; dst; src1; src2; imm } ->
        Some
          (Instruction.mk op 32
             [
               Operand.Imm (Bigint.of_int64 imm);
               (match src2 with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
               Operand.Reg src1;
               Operand.Reg dst;
             ])
    | Lowered.Vex_unop_imm_r_rm { op; dst; src; imm } ->
        Some
          (Instruction.mk op 32
             [
               Operand.Imm (Bigint.of_int64 imm);
               (match src with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
               Operand.Reg dst;
             ])
    | Lowered.Xmm_shift_imm_rm { op; rm; imm } ->
        Some
          (Instruction.mk op 32
             [
               Operand.Imm (Bigint.of_int64 imm);
               (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
             ])
    | Lowered.Vex_shift_imm_rm { op; dst; rm; imm } ->
        Some
          (Instruction.mk op 32
             [
               Operand.Imm (Bigint.of_int64 imm);
               (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
               Operand.Reg dst;
             ])
    | Lowered.Vex_movd_r_rm { op; dst; rm } ->
        Some
          (Instruction.mk op 32
             [
               (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
               Operand.Reg dst;
             ])
    | Lowered.Vex_movd_rm_r { op; rm; reg } ->
        Some
          (Instruction.mk op 32
             [
               Operand.Reg reg;
               (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
             ])
    | Lowered.Fpu_mem { op; mem } -> Some (Instruction.mk op 32 [ Operand.Mem mem ])
    | Lowered.Fadd_st0_x87 { src } ->
        Some (Instruction.mk Opcode.Fadd 32 [ Operand.Reg src; Operand.Reg (reg_at ~width:80 0) ])
    | Lowered.Fucomp -> Some (Instruction.mk Opcode.Fucomp 32 [])
    | Lowered.Fnstsw -> Some (Instruction.mk Opcode.Fnstsw 32 [ Operand.Reg (reg_at ~width:16 0) ])
    | Lowered.Sahf -> Some (Instruction.mk Opcode.Sahf 32 [])
    | Lowered.Table x -> Some (Instruction.mk (Opcode.Table x.row) 0 x.ops)

  let decode_ungated ctx bytes ~pos =
    let bits = C.Bits.of_bytes (String.sub bytes pos (String.length bytes - pos)) in
    match C.decode_bits codec bits with
    | None -> (
        match table_decode bytes pos with
        | Some (i, name, len) -> Ok (i, name, len)
        | None -> Error (diag ~pos:__POS__ `Decode_no_match))
    | Some d -> (
        if d.C.consumed mod 8 <> 0 then Error (diag ~pos:__POS__ `Decode_partial_bytes)
        else
          let len = d.C.consumed / 8 in
          match instruction_of_lowered ~at:ctx.address ~len d.C.value with
          | None -> Error (diag ~pos:__POS__ `Decode_no_normalized)
          | Some i -> Ok (i, String.concat "." d.C.dform, len))

  (* {2 Configuration}

     A form's component is checked against the configuration wherever the form can be seen: on
     the surface instruction (best diagnostics), on a normalized instruction handed in directly,
     on a lowered form handed to the encoder directly, and on decode. Each public entry point
     below is the [_ungated] one behind a single gate, so a disabled form is refused whichever
     path reached it. *)

  (* A generated row's component follows from its ISA set: the x87 sets belong to x87 *)
  let row_feature row =
    match table_rows.(row).T.feature with
    | "x87" | "fcmov" | "fcomi" | "sse3x87" -> Some X86_x87.component.feature
    | _ -> None

  let gate ?origin state mnemonic = function
    | Some f when not (Target_config.enabled state.config f) ->
        Error (diag ~pos:__POS__ ?origin (`Feature_disabled (mnemonic, f)))
    | _ -> Ok ()

  let feature_gate ?origin state mnemonic =
    gate ?origin state mnemonic (Target_component.feature_of_mnemonic components mnemonic)

  let required_feature (i : Instruction.t) =
    match i.op with
    | Opcode.Table row -> row_feature row
    | op -> Target_component.feature_of_mnemonic components (Opcode.name op)

  let instruction_gate ?origin state (i : Instruction.t) =
    gate ?origin state (Opcode.name i.op) (required_feature i)

  let simplify_instruction state s =
    match simplify_instruction_ungated s with
    | Error _ as e -> e
    | Ok i -> (
        match instruction_gate ~origin:s.Surface.origin state i with
        | Ok () -> Ok i
        | Error _ as e -> e)

  let lower_instruction state i =
    match instruction_gate state i with Error _ as e -> e | Ok () -> lower_instruction_ungated i

  (* The mnemonic of a lowered form that belongs to a component. *)
  let lowered_mnemonic = function
    | Lowered.Fpu_mem { op; _ } -> Some (Opcode.name op)
    | Lowered.Fadd_st0_x87 _ -> Some X86_x87.fadd_st0.mnemonic
    | Lowered.Fucomp -> Some "fucomp"
    | Lowered.Fnstsw -> Some "fnstsw"
    | _ -> None

  let encode_in state l =
    match (l, lowered_mnemonic l) with
    | Lowered.Table x, _ -> (
        match gate state table_rows.(x.row).T.mnemonic (row_feature x.row) with
        | Error _ as e -> e
        | Ok () -> encode_ungated l)
    | _, Some m -> (
        match feature_gate state m with Error _ as e -> e | Ok () -> encode_ungated l)
    | _, None -> encode_ungated l

  let encode l = encode_in default_state l

  let decode ctx bytes ~pos =
    match decode_ungated ctx bytes ~pos with
    | Ok (i, _, _) as ok -> (
        match instruction_gate ctx.state i with Ok () -> ok | Error _ as e -> e)
    | Error _ as e -> e

  (* {2 Fixups, padding, directives} *)

  (* [place] arrives already biased by the fixup's [pc_bias], which on x86 is
     the realized instruction length - 2 for a short branch, 5 for call/jmp
     rel32, 6 for a near jcc or a RIP-relative load, 7 with a REX prefix. That
     is why nothing here adds a constant: only [encode] knows how long the form
     it produced turned out to be. *)
  let evaluate_fixup kind ~place ~target =
    match kind with
    | Abs32 ->
        (* An absolute 32-bit address is a bit pattern, not a signed number:
           0x80000000 upwards is a perfectly good address and [fits_s32] would
           reject it. The range check belongs to the fixup's declared range. *)
        Ok (Int64.logand target 0xFFFFFFFFL)
    | Abs64 ->
        (* The full 64-bit bit pattern - no masking needed, [target] already
           is one. *)
        Ok target
    | Pcrel8_branch ->
        let d = Int64.sub target place in
        if Int64.compare d (-128L) >= 0 && Int64.compare d 128L < 0 then Ok d
        else Error (diag ~pos:__POS__ `Displacement_not_8bit)
    | Pcrel32_branch | Pcrel32_call | Pcrel32_data ->
        let d = Int64.sub target place in
        if fits_s32 d then Ok d else Error (diag ~pos:__POS__ `Displacement_not_32bit)

  (* Each mode's own, measured against its own GNU as rather than taken from a
     manual. M1 never reached this - both x86 fixtures had their [.align] at
     offset 0, where the padding is empty - and the first fixture with two
     functions showed the published Intel table was the wrong thing to have
     assumed. See {!MODE.nop_table}. *)
  (* Measured from the committed fixtures: CompCert emits [.long] for a 32-bit
     initializer in GNU x86 syntax. [.word] is *two* bytes here, unlike on the
     two fixed-width targets, which is the whole reason this table is per
     dialect rather than shared. *)
  let data_widths = [ (".byte", 1); (".short", 2); (".word", 2); (".long", 4); (".quad", 8) ]

  let data_fixup ~width =
    match width with
    | 4 -> Ok Abs32
    (* [.quad symbol] (M5, asm/docs/corpus.md - [testvec: ... .quad __stringlit_4]):
       a full 64-bit absolute address, only representable in 64-bit mode
       ([M.rex_allowed] - x86-32 has no 8-byte general-purpose register or
       address width for this to mean anything, and no fixture needs it
       there). *)
    | 8 when M.rex_allowed -> Ok Abs64
    | _ -> Error (diag ~pos:__POS__ (`No_data_relocation width))

  let nop_table = M.nop_table

  (* Greedy, longest entry first: both {!nop_table} (as's own [.align] table)
     and {!M.merge_nop_table} (ld's merge-gap table, M3 §5) are indexed the
     same way - entry [n-1] is exactly [n] bytes - so one fill loop serves
     both, over whichever table its caller names. *)
  let fill_from table length =
    let buf = Buffer.create length in
    let rec go n =
      if n = 0 then ()
      else
        let take = min n (Array.length table) in
        Buffer.add_string buf table.(take - 1);
        go (n - take)
    in
    go length;
    Buffer.contents buf

  let nop_bytes ~length =
    if length < 0 then Error (diag ~pos:__POS__ (`Negative_padding length))
    else Ok (fill_from nop_table length)

  (* Measured (M3 §3/§5, .ai/asm_plan.md §12): a linker-inserted merge gap in an
     executable section is real NOP fill on both x86_32 and x86_64, from
     {!M.merge_nop_table} - NOT necessarily {!nop_table} again, since x86_32's
     ld and as disagree (see {!MODE.merge_nop_table}). Every other target's
     merge gap, and every NON-executable gap here too, is plain zero fill. *)
  let merge_fill = Some (fun ~length -> fill_from M.merge_nop_table length)

  (* Measured: x86 GAS records a section's alignment without rounding its
     size up to it. *)
  let pad_section_to_alignment = false
end
