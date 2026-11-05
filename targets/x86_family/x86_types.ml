(* The x86 vocabulary both modes share: register classes and registers, memory operands,
   operands, condition codes, opcodes and instructions. The per-opcode table
   ({!X86_ops}) is written over these, and the family encoder includes them. *)

open Foundation
module X86_table_row = X86_table_row
module X86_table_rows = X86_table_rows

(* {1 Registers}

   [num] is the architectural number 0-15, so [num land 7] is the ModR/M or
   SIB field and [num >= 8] is the REX extension bit. Keeping one number
   rather than a pair is what makes "did this operand need REX.B" a property of
   the register instead of a fact the caller has to remember to carry. *)

(* A register's class: which register file, and for the GPRs and vectors, its width. [Gpr8_high]
   is [ah]-[bh], the byte registers 4-7 of a mode without REX. *)
module Rclass = struct
  type t =
    | Gpr8
    | Gpr8_high
    | Gpr16
    | Gpr32
    | Gpr64
    | Xmm
    | Ymm
    | Zmm
    | Mmx
    | Kmask
    | St
    | Tmm
    | Cr
    | Dr

  (* The operand width a register of this class holds, for the classes an instruction's
     operand size selects among. *)
  let width = function
    | Gpr8 | Gpr8_high -> Some 8
    | Gpr16 -> Some 16
    | Gpr32 -> Some 32
    | Gpr64 -> Some 64
    | Xmm -> Some 128
    | Ymm -> Some 256
    | Zmm -> Some 512
    | Mmx | Kmask | St | Tmm | Cr | Dr -> None

  let of_width = function
    | 8 -> Some Gpr8
    | 16 -> Some Gpr16
    | 32 -> Some Gpr32
    | 64 -> Some Gpr64
    | 128 -> Some Xmm
    | 256 -> Some Ymm
    | 512 -> Some Zmm
    | _ -> None

  let is_vector = function Xmm | Ymm | Zmm -> true | _ -> false

  let of_row : X86_table_row.rclass -> t = function
    | Gpr8 -> Gpr8
    | Gpr16 -> Gpr16
    | Gpr32 -> Gpr32
    | Gpr64 -> Gpr64
    | Xmm -> Xmm
    | Ymm -> Ymm
    | Zmm -> Zmm
    | Mmx -> Mmx
    | Kmask -> Kmask
    | St -> St
    | Tmm -> Tmm
    | Cr -> Cr
    | Dr -> Dr

  (* A generated row's register operand of class [row] takes a register of class [cls]. *)
  let accepts row cls = of_row row = cls || (of_row row = Gpr8 && cls = Gpr8_high)
end

module Reg = struct
  type t = { cls : Rclass.t; num : int }

  let equal a b = a.cls = b.cls && a.num = b.num

  (* The operand width of a GPR or vector register; 0 for the classes no operand size names. *)
  let width r = Option.value (Rclass.width r.cls) ~default:0
  let names_32 = [| "eax"; "ecx"; "edx"; "ebx"; "esp"; "ebp"; "esi"; "edi" |]
  let names_64 = [| "rax"; "rcx"; "rdx"; "rbx"; "rsp"; "rbp"; "rsi"; "rdi" |]
  let names_16 = [| "ax"; "cx"; "dx"; "bx"; "sp"; "bp"; "si"; "di" |]
  let names_8l = [| "al"; "cl"; "dl"; "bl"; "spl"; "bpl"; "sil"; "dil" |]

  (* [num = -1] is the instruction pointer, a 64-bit GPR no field encodes. *)
  let rip = { cls = Gpr64; num = -1 }

  let name r =
    let gpr names suffix =
      if r.num < 0 then "rip"
      else if r.num < 8 then names.(r.num)
      else Printf.sprintf "r%d%s" r.num suffix
    in
    match r.cls with
    | Gpr64 -> gpr names_64 ""
    | Gpr32 -> gpr names_32 "d"
    | Gpr16 -> gpr names_16 "w"
    | Gpr8 -> gpr names_8l "b"
    | Gpr8_high -> [| "ah"; "ch"; "dh"; "bh" |].(r.num - 4)
    | Xmm -> Printf.sprintf "xmm%d" r.num
    | Ymm -> Printf.sprintf "ymm%d" r.num
    | Zmm -> Printf.sprintf "zmm%d" r.num
    | Mmx -> Printf.sprintf "mm%d" r.num
    | Kmask -> Printf.sprintf "k%d" r.num
    | St -> if r.num = 0 then "st" else Printf.sprintf "st(%d)" r.num
    | Tmm -> Printf.sprintf "tmm%d" r.num
    | Cr -> Printf.sprintf "cr%d" r.num
    | Dr -> Printf.sprintf "dr%d" r.num

  let pp ppf r = Fmt.pf ppf "%%%s" (name r)
  let in_class cls nums = List.map (fun num -> { cls; num }) nums
  let range lo n = List.init n (fun i -> lo + i)

  (* [xmm16]-[zmm31]: EVEX's upper sixteen, x86-64 only and used only by generated EVEX rows *)
  let evex_upper = List.concat_map (fun cls -> in_class cls (range 16 16)) [ Rclass.Xmm; Ymm; Zmm ]

  (* [mm0]-[mm7], [k0]-[k7], [tmm0]-[tmm7], [cr0]-[cr15] and [dr0]-[dr7], used only by generated
     table rows *)
  let mm_and_k =
    in_class Mmx (range 0 8)
    @ in_class Kmask (range 0 8)
    @ in_class Tmm (range 0 8)
    @ in_class Cr (range 0 16)
    @ in_class Dr (range 0 8)

  (* The registers 0-7 (or 0-15 for the vectors) of a class. *)
  let base_regs cls n = in_class cls (range 0 n)
  let extended_regs cls = in_class cls (range 8 8)

  (* APX's r16-r31 at every width (r16, r16d, r16w, r16b): x86-64 only, reached only by
     generated rows (REX2 for legacy maps 0 and 1, EVEX elsewhere) *)
  let apx_gprs =
    List.concat_map (fun cls -> in_class cls (range 16 16)) [ Rclass.Gpr64; Gpr32; Gpr16; Gpr8 ]

  (* Lookup by spelling over a mode's own register set. The set is a [MODE]
     field rather than a constant here, because [%rax] exists in 64-bit mode
     and does not exist in 32-bit mode, and a shared table would have to be
     filtered at every use. *)
  let find_in registers spelling = List.find_opt (fun r -> String.equal (name r) spelling) registers
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
     string/array address into a register or the stack, which the compiler's codegen
     (this project's only source of fixtures until now) never emitted. Kept
     as its own constructor rather than widening [Imm] to
     [Bigint.t Asm_core.Expr.t]-like union, so every existing [Imm v] site
     stays exactly as narrow as it was. All three evidenced forms now lower
     it: [mov]'s register-destination form ({!Lowered.Mov_r_imm}), the
     ALU-immediate form ({!Lowered.Alu_rm_imm}), and [push]'s immediate form
     ({!Lowered.Push_imm}) - see classify-c-gcc
     section. *)
  type t =
    | Reg of Reg.t
    | Mem of Mem.t
    | Imm of Bigint.t
    | Imm_sym of Asm_core.Expr.t
    | Sym of Asm_core.Expr.t
    | Dfv of int  (** APX [{dfv=...}]: OF 8, SF 4, ZF 2, CF 1 *)
    | Bcst of { mem : Mem.t; n : int }  (** an EVEX broadcast source, [(mem){1toN}] *)
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
    | Bcst { mem; n } -> Fmt.pf ppf "%a{1to%d}" Mem.pp mem n
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

(* A generated row, as an opcode names it: its place in {!X86_table_rows.rows}. Only an index
   into that table makes one. *)
module Row : sig
  type t

  val of_index : int -> t
  val index : t -> int
  val row : t -> X86_table_row.row
  val mnemonic : t -> string
  val equal : t -> t -> bool
end = struct
  type t = int

  let of_index i =
    if i >= 0 && i < Array.length X86_table_rows.rows then i
    else invalid_arg (Printf.sprintf "X86 Row.of_index %d" i)

  let index i = i
  let row i = X86_table_rows.rows.(i)
  let mnemonic i = X86_table_rows.rows.(i).X86_table_row.mnemonic
  let equal = Int.equal
end

(* The rel8-only branches: the loops and the counter-register jumps. *)
module Short = struct
  type t = Loop | Loope | Loopne | Jcxz | Jecxz | Jrcxz

  let name = function
    | Loop -> "loop"
    | Loope -> "loope"
    | Loopne -> "loopne"
    | Jcxz -> "jcxz"
    | Jecxz -> "jecxz"
    | Jrcxz -> "jrcxz"
end

module Opcode = struct
  (** [sahf] - load [%ah] into the flags register ([0x9E]), bare, no operand,
            {!Fucomp}/{!Fnstsw}'s exact fixed-opcode shape (M5, the corpus notes -
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
    | Short_branch of Short.t  (** a rel8-only branch *)
    | Ud2
    | Pop
    | Bswap
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
            mandatory-prefix group (M5 - gas_frontier.t's
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
            self-zeroing idiom [pxor %xmmN, %xmmN] (M5 - gas_frontier.t's
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
            pair. Evidenced only against a bare-symbol source (M5, the corpus notes -
            gas_frontier.t's runtime-i64_utod.S/i64_utof.S), so it shares {!Flds}'s
            [mem_of_symbol] duality rather than {!Fldl}/{!Fstpl}/{!Fstps}'s memory-operand-only
            scope. *)
    | Fadd
        (** [fadd %st(i), %st] ([0xD8 0xC0+i]): register-stack add with ST0
            as its implicit read-write destination.  This is deliberately not
            the reverse-direction FADD_X87_ST0 or FADDP form. *)
    | Fucomp
    | Fnstcw
        (** [fnstcw mem] - x87 store-control-word ([0xD9 /7], M5, the corpus notes -
            gas_frontier.t's runtime-i64_dtos.S/i64_dtou.S own round-to-nearest-then-
            truncate idiom around an integer conversion). Shares {!Lowered.Fpu_mem}
            with {!Fldl}/.../{!Fadds}: same opcode-plus-ModR/M-extension shape, one
            more disjoint opcode/extension pair. *)
    | Fldcw
        (** [fldcw mem] - x87 load-control-word ([0xD9 /5]), {!Fnstcw}'s load-back
            counterpart restoring the saved rounding mode (M5, the corpus notes -
            same fixtures as {!Fnstcw}). *)
    | Fistpll
        (** [fistpll mem] - x87 64-bit integer store-and-pop ([0xDF /7]), {!Fildll}'s
            store direction (M5 - same fixtures as {!Fnstcw}). *)
    | Fsubs
        (** [fsubs mem] - x87 single-precision subtract ([st(0) := st(0) - mem],
            [0xD8 /4]), {!Fadds}'s exact sibling at a different ModR/M extension,
            including the same bare-symbol [mem_of_symbol] duality (M5,
            the corpus notes - gas_frontier.t's runtime-i64_dtou.S). *)
    | Fnstsw
        (** [fnstsw %ax] - x87 store-status-word into [%ax] ([0xDF 0xE0]), a fixed
            two-byte word with no ModR/M, the same "no operand" shape {!Fucomp}
            already uses - the [%ax] destination is implicit in the opcode, not an
            encoded operand (M5 - gas_frontier.t's
            runtime-i64_dtou.S own status-word-into-[sahf] idiom). *)
    | Sahf
    | Table of Row.t  (** a generated {!X86_table_rows} row (DEC-X86-TABLE) *)

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
    | Short_branch s -> Short.name s
    | Ud2 -> "ud2"
    | Pop -> "pop"
    | Bswap -> "bswap"
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
    | Table r -> Row.mnemonic r

  (* Every hand-written opcode, rows aside. *)
  let all =
    Array.concat
      [
        [|
          Add;
          Sub;
          Mov;
          Lea;
          Ret;
          Xor;
          And;
          Cmp;
          Imul;
          Ud2;
          Pop;
          Bswap;
          Jmp;
          Call;
          Push;
          Neg;
          Test;
          Adc;
          Sbb;
          Mul;
          Div;
          Dec;
          Rcr;
          Shr;
          Or;
          Not;
          Ror;
          Shl;
          Sar;
          Shld;
          Addsd;
          Subsd;
          Mulsd;
          Divsd;
          Addss;
          Subss;
          Mulss;
          Divss;
          Comisd;
          Ucomisd;
          Comiss;
          Ucomiss;
          Xorpd;
          Pxor;
          Movapd;
          Cvtsd2ss;
          Cvtss2sd;
          Cvtps2pd;
          Cvtpd2ps;
          Cvtdq2ps;
          Cvtps2dq;
          Cvttps2dq;
          Movsd;
          Movss;
          Cvtsi2sd;
          Cvtsi2ss;
          Cvttsd2si;
          Unpcklps;
          Unpckhps;
          Unpcklpd;
          Unpckhpd;
          Punpcklqdq;
          Punpckhqdq;
          Punpcklbw;
          Punpckhbw;
          Punpcklwd;
          Punpckhwd;
          Punpckldq;
          Punpckhdq;
          Paddb;
          Paddw;
          Paddd;
          Paddq;
          Psubb;
          Psubw;
          Psubd;
          Psubq;
          Pcmpeqb;
          Pcmpeqw;
          Pcmpeqd;
          Pcmpgtb;
          Pcmpgtw;
          Pcmpgtd;
          Packsswb;
          Packssdw;
          Packuswb;
          Pand;
          Pandn;
          Por;
          Pminub;
          Pmaxub;
          Pminsw;
          Pmaxsw;
          Pmullw;
          Pmulhw;
          Pmulhuw;
          Pavgb;
          Pavgw;
          Psadbw;
          Psllw;
          Pslld;
          Psllq;
          Psrlw;
          Psrld;
          Psrlq;
          Psraw;
          Psrad;
          Pslldq;
          Psrldq;
          Pshufb;
          Phaddw;
          Phaddd;
          Phsubw;
          Phsubd;
          Psignb;
          Psignw;
          Psignd;
          Pmaddubsw;
          Pmulhrsw;
          Phaddsw;
          Phsubsw;
          Pabsb;
          Pabsw;
          Pabsd;
          Palignr;
          Roundps;
          Roundpd;
          Roundss;
          Roundsd;
          Pcmpeqq;
          Pcmpgtq;
          Packusdw;
          Pmaxsb;
          Pmaxsd;
          Pmaxud;
          Pmaxuw;
          Pminsb;
          Pminsd;
          Pminud;
          Pminuw;
          Pmuldq;
          Pmulld;
          Phminposuw;
          Ptest;
          Pmovsxbw;
          Pmovsxbd;
          Pmovsxbq;
          Pmovsxwd;
          Pmovsxwq;
          Pmovsxdq;
          Pmovzxbw;
          Pmovzxbd;
          Pmovzxbq;
          Pmovzxwd;
          Pmovzxwq;
          Pmovzxdq;
          Movntdqa;
          Blendvps;
          Blendvpd;
          Pblendvb;
          Blendps;
          Blendpd;
          Dpps;
          Dppd;
          Mpsadbw;
          Pblendw;
          Insertps;
          Pinsrb;
          Pinsrd;
          Pextrb;
          Pextrd;
          Extractps;
          Vpshufb;
          Vphaddw;
          Vphaddd;
          Vphaddsw;
          Vpmaddubsw;
          Vphsubw;
          Vphsubd;
          Vphsubsw;
          Vpsignb;
          Vpsignw;
          Vpsignd;
          Vpmulhrsw;
          Vpmuldq;
          Vpcmpeqq;
          Vpackusdw;
          Vpcmpgtq;
          Vpminsb;
          Vpminsd;
          Vpminuw;
          Vpminud;
          Vpmaxsb;
          Vpmaxsd;
          Vpmaxuw;
          Vpmaxud;
          Vpmulld;
          Vpalignr;
          Vblendps;
          Vblendpd;
          Vpblendw;
          Vroundss;
          Vroundsd;
          Vdpps;
          Vdppd;
          Vmpsadbw;
          Vinsertps;
          Vpabsb;
          Vpabsw;
          Vpabsd;
          Vphminposuw;
          Vptest;
          Vpmovsxbw;
          Vpmovsxbd;
          Vpmovsxbq;
          Vpmovsxwd;
          Vpmovsxwq;
          Vpmovsxdq;
          Vpmovzxbw;
          Vpmovzxbd;
          Vpmovzxbq;
          Vpmovzxwd;
          Vpmovzxwq;
          Vpmovzxdq;
          Vmovntdqa;
          Movdqa;
          Movdqu;
          Andps;
          Andnps;
          Orps;
          Xorps;
          Andpd;
          Andnpd;
          Orpd;
          Movaps;
          Movups;
          Movupd;
          Addps;
          Subps;
          Mulps;
          Divps;
          Addpd;
          Subpd;
          Mulpd;
          Divpd;
          Maxss;
          Minss;
          Maxsd;
          Minsd;
          Maxps;
          Minps;
          Maxpd;
          Minpd;
          Sqrtss;
          Sqrtsd;
          Sqrtps;
          Sqrtpd;
          Shufps;
          Shufpd;
          Cmpss;
          Cmpsd;
          Cmpps;
          Cmppd;
          Pshufd;
          Pshuflw;
          Pshufhw;
          Movd;
          Vaddsd;
          Vsubsd;
          Vmulsd;
          Vdivsd;
          Vaddss;
          Vsubss;
          Vmulss;
          Vdivss;
          Vaddps;
          Vsubps;
          Vmulps;
          Vdivps;
          Vaddpd;
          Vsubpd;
          Vmulpd;
          Vdivpd;
          Vandps;
          Vandnps;
          Vorps;
          Vxorps;
          Vandpd;
          Vandnpd;
          Vorpd;
          Vxorpd;
          Vunpcklps;
          Vunpckhps;
          Vunpcklpd;
          Vunpckhpd;
          Vpunpcklqdq;
          Vpunpckhqdq;
          Vpunpcklbw;
          Vpunpckhbw;
          Vpunpcklwd;
          Vpunpckhwd;
          Vpunpckldq;
          Vpunpckhdq;
          Vpaddb;
          Vpaddw;
          Vpaddd;
          Vpaddq;
          Vpsubb;
          Vpsubw;
          Vpsubd;
          Vpsubq;
          Vpcmpeqb;
          Vpcmpeqw;
          Vpcmpeqd;
          Vpcmpgtb;
          Vpcmpgtw;
          Vpcmpgtd;
          Vpacksswb;
          Vpackssdw;
          Vpackuswb;
          Vpand;
          Vpandn;
          Vpor;
          Vpminub;
          Vpmaxub;
          Vpminsw;
          Vpmaxsw;
          Vmaxsd;
          Vminsd;
          Vmaxss;
          Vminss;
          Vmaxps;
          Vminps;
          Vmaxpd;
          Vminpd;
          Vsqrtsd;
          Vsqrtss;
          Vsqrtps;
          Vsqrtpd;
          Vmovaps;
          Vmovups;
          Vmovapd;
          Vmovupd;
          Vcomisd;
          Vucomisd;
          Vcomiss;
          Vucomiss;
          Vcvtps2pd;
          Vcvtpd2ps;
          Vcvtdq2ps;
          Vcvtps2dq;
          Vcvttps2dq;
          Vshufps;
          Vshufpd;
          Vcmpss;
          Vcmpsd;
          Vcmpps;
          Vcmppd;
          Vpshufd;
          Vpshuflw;
          Vpshufhw;
          Vmovd;
          Vpmullw;
          Vpmulhw;
          Vpmulhuw;
          Vpavgb;
          Vpavgw;
          Vpsadbw;
          Vpsllw;
          Vpslld;
          Vpsllq;
          Vpsrlw;
          Vpsrld;
          Vpsrlq;
          Vpsraw;
          Vpsrad;
          Vpslldq;
          Vpsrldq;
          Vmovdqa;
          Vmovdqu;
          Pinsrw;
          Pextrw;
          Vpinsrw;
          Vpextrw;
          Movmskps;
          Movmskpd;
          Pmovmskb;
          Vmovmskps;
          Vmovmskpd;
          Vpmovmskb;
          Fldl;
          Fstpl;
          Fstps;
          Flds;
          Fildll;
          Fadds;
          Fadd;
          Fucomp;
          Fnstcw;
          Fldcw;
          Fistpll;
          Fsubs;
          Fnstsw;
          Sahf;
        |];
        Array.of_list (List.concat_map (fun c -> [ Cmov c; Jcc c; Setcc c ]) Cc.all);
        Array.map (fun s -> Short_branch s) Short.[| Loop; Loope; Loopne; Jcxz; Jecxz; Jrcxz |];
        [|
          Movzx { src_width = 8 };
          Movzx { src_width = 16 };
          Movsx { src_width = 8 };
          Movsx { src_width = 16 };
          Movsx { src_width = 32 };
        |];
      ]
end

let suffix_of_width = function 8 -> "b" | 16 -> "w" | 32 -> "l" | 64 -> "q" | _ -> "?"

module Instruction = struct
  (* The normalized instruction carries an explicit operand *width in bits*:
     the suffix that spelled it (`l`, `q`) is surface syntax, and by this stage
     the only thing that matters is 32 or 64. *)
  type t = { op : Opcode.t; width : int; ops : Operand.t list; form : string option }
  (** [form] is B10's form preference: which rung of a relaxation ladder this instruction insists
      on, or [None] for "layout decides".

      It is [None] from ordinary parsing, so the compiler's and a user's branches relax normally. It is
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
      List.find_map
        (function
          | X86_table_row.Reg { field = (X86_table_row.Modrm_reg | Modrm_rm) as field; _ } ->
              Some field
          | _ -> None)
        (List.rev r.operands)
    in
    let rec first j =
      if j >= i then None
      else
        let q = rows.(j) in
        if
          String.equal q.mnemonic r.mnemonic
          && q.mode = r.mode
          && shape q = shape r
          && String.equal q.pseudo r.pseudo
        then Some q
        else first (j + 1)
    in
    (* the row's own pseudo-prefix, then what tells it from the first same-shaped row reached
       the same way: the encoding space, or the destination's ModR/M field *)
    let own = if r.pseudo <> "" then [ r.pseudo ] else [] in
    let apart =
      match first 0 with
      | None -> []
      | Some q -> (
          if q.space <> r.space && r.pseudo = "" then
            match r.space with
            | X86_table_row.Evex -> [ "evex" ]
            | Vex -> [ "vex" ]
            | Legacy | Xop -> []
          else
            match (dest r, dest q) with
            | Some X86_table_row.Modrm_reg, Some X86_table_row.Modrm_rm -> [ "load" ]
            | Some X86_table_row.Modrm_rm, Some X86_table_row.Modrm_reg -> [ "store" ]
            | _ -> [])
    in
    String.concat "" (List.map (fun p -> "{" ^ p ^ "} ") (own @ apart)) ^ r.mnemonic
end

module Rm = struct
  type t = Reg of Reg.t | Mem of Mem.t

  let equal a b =
    match (a, b) with Reg x, Reg y -> Reg.equal x y | Mem x, Mem y -> Mem.equal x y | _ -> false

  let pp ppf = function Reg r -> Reg.pp ppf r | Mem m -> Mem.pp ppf m
end

type rm = Rm.t
