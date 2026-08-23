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
     second without. *)
  type t = Reg of Reg.t | Mem of Mem.t | Imm of Bigint.t | Sym of Asm_core.Expr.t

  let pp ppf = function
    | Reg r -> Reg.pp ppf r
    | Mem m -> Mem.pp ppf m
    | Imm v -> Fmt.pf ppf "$%a" Bigint.pp v
    | Sym e -> Fmt.string ppf (Asm_core.Expr.to_string e)
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
    | Comiss
    | Xorpd
    | Movapd
    | Cvtsd2ss
    | Cvtss2sd
    | Movsd
    | Movss
    | Cvtsi2sd
    | Cvtsi2ss
    | Cvttsd2si

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
    | Comiss -> "comiss"
    | Xorpd -> "xorpd"
    | Movapd -> "movapd"
    | Cvtsd2ss -> "cvtsd2ss"
    | Cvtss2sd -> "cvtss2sd"
    | Movsd -> "movsd"
    | Movss -> "movss"
    | Cvtsi2sd -> "cvtsi2sd"
    | Cvtsi2ss -> "cvtsi2ss"
    | Cvttsd2si -> "cvttsd2si"

  (* The machine encodes add and sub as one opcode with the operation in the
     ModR/M reg field, so the opcode and its extension are two spellings of one
     fact and live together. [-1] is "not an ALU-immediate operation". [Adc]
     (M4, .ai/asm_plan.md §12: the CompCert-runtime-helper fixture) needs only
     this immediate form - [i64_sdiv.S]/[i64_smod.S] never add it to a memory
     destination. *)
  let to_ext = function
    | Add -> 0
    | Or -> 1
    | Adc -> 2
    | And -> 4
    | Sub -> 5
    | Xor -> 6
    | Cmp -> 7
    | _ -> -1

  let of_ext = function
    | 0 -> Some Add
    | 1 -> Some Or
    | 2 -> Some Adc
    | 4 -> Some And
    | 5 -> Some Sub
    | 6 -> Some Xor
    | 7 -> Some Cmp
    | _ -> None

  (* The other ALU direction: one byte per operation, r/m written from reg.
     [Add]/[Sbb]/[Test] (M4) join [Xor]/[Cmp]/[Sub] for the same reason those
     three were added - real bytes the i64_divmod runtime-helper fixture
     measurably selects (0x01/0x19/0x85 respectively), not speculative
     coverage. *)
  let to_rm_r = function
    | Xor -> Some 0x31L
    | Cmp -> Some 0x39L
    | Sub -> Some 0x29L
    | Add -> Some 0x01L
    | Sbb -> Some 0x19L
    | Test -> Some 0x85L
    | Or -> Some 0x09L
    | And -> Some 0x21L
    | _ -> None

  (* The reg<-rm ALU direction ([Alu_r_rm]): one byte per operation, the
     register field is the DESTINATION. M4's own [adcl 0x20(%esp),%edx] and
     [add 0x1c(%esp),%eax] are the only two measured uses; a row nothing
     selects is a row nothing checks. *)
  let to_r_rm = function Adc -> Some 0x13L | Add -> Some 0x03L | Xor -> Some 0x33L | _ -> None

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

  let pp ppf i =
    match i.ops with
    | [] -> Fmt.string ppf (Opcode.name i.op)
    | ops -> (
        match i.op with
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
           $0x10,%eax"], ["sar $0x2,%eax"], but ["rorl $0x3,(%rax)"]) -
           existing opcodes below keep their already-measured, unconditional
           suffix. *)
        | ( Opcode.Neg | Opcode.Mul | Opcode.Div | Opcode.Test | Opcode.Adc | Opcode.Sbb
          | Opcode.Rcr | Opcode.Shr | Opcode.Ror | Opcode.Shl | Opcode.Sar ) as op ->
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
          | Opcode.Mulss | Opcode.Divss | Opcode.Comisd | Opcode.Comiss | Opcode.Xorpd
          | Opcode.Movapd | Opcode.Cvtsd2ss | Opcode.Cvtss2sd | Opcode.Movsd | Opcode.Movss
          | Opcode.Cvtsi2sd | Opcode.Cvtsi2ss | Opcode.Cvttsd2si ) as op ->
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
  type t =
    | Alu_rm_imm of { ext : int; width : int; rm : Rm.t; imm : int64 }
    | Mov_r_imm of { width : int; reg : Reg.t; imm : int64 }
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
    | Cvtsi2f_r_rm of { op : Opcode.t; width : int; reg : Reg.t; rm : Rm.t }
        (** [F2/F3 0F 2A /r], xmm<-(r/m32 or r/m64) - [cvtsi2sd]/[cvtsi2ss]. [reg] is xmm; [rm] is
            a GPR (or memory) at [width], which is also what selects REX.W - CompCert always
            spells the 64-bit source explicitly ([cvtsi2sdq]), so [width] comes from the mnemonic,
            not inferred from an operand. *)
    | Cvtf2i_r_rm of { width : int; reg : Reg.t; rm : Rm.t }
        (** [F2 0F 2C /r], (r32 or r64)<-(xmm or m64) - [cvttsd2si] only ([cvttss2si] is unevidenced
            by this corpus). [reg] is a GPR at [width]; [rm] is xmm (or memory). *)
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

  let pp ppf = function
    | Alu_rm_imm { ext; width; rm; imm } ->
        Fmt.pf ppf "%s%s $%Ld, %a"
          (match Opcode.of_ext ext with Some o -> Opcode.name o | None -> "alu?")
          (suffix_of_width width) imm Rm.pp rm
    | Mov_r_imm { width; reg; imm } ->
        Fmt.pf ppf "mov%s $%Ld, %a" (suffix_of_width width) imm Reg.pp reg
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
    | Setcc_rm { cc; rm } -> Fmt.pf ppf "set%s %a" (Cc.name cc) Rm.pp rm
    | Movx_r_rm { zero_extend; reg; rm; _ } ->
        Fmt.pf ppf "%s %a, %a" (if zero_extend then "movzx" else "movsx") Rm.pp rm Reg.pp reg
    | Movsxd_r_rm { reg; rm } -> Fmt.pf ppf "movslq %a, %a" Rm.pp rm Reg.pp reg
    | Sse_binop_r_rm { op; reg; rm } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Rm.pp rm Reg.pp reg
    | Sse_mov_r_rm { op; reg; rm } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Rm.pp rm Reg.pp reg
    | Sse_mov_rm_r { op; rm; reg } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Reg.pp reg Rm.pp rm
    | Cvtsi2f_r_rm { op; reg; rm; _ } -> Fmt.pf ppf "%s %a, %a" (Opcode.name op) Rm.pp rm Reg.pp reg
    | Cvtf2i_r_rm { reg; rm; _ } -> Fmt.pf ppf "cvttsd2si %a, %a" Rm.pp rm Reg.pp reg

  let equal a b =
    match (a, b) with
    | Alu_rm_imm x, Alu_rm_imm y ->
        x.ext = y.ext && x.width = y.width && Rm.equal x.rm y.rm && Int64.equal x.imm y.imm
    | Mov_r_imm x, Mov_r_imm y ->
        x.width = y.width && Reg.equal x.reg y.reg && Int64.equal x.imm y.imm
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
    | Dec x, Dec y -> Reg.equal x.reg y.reg
    | Unary_rm x, Unary_rm y -> x.ext = y.ext && x.width = y.width && Rm.equal x.rm y.rm
    | Alu_r_rm x, Alu_r_rm y ->
        x.op = y.op && x.width = y.width && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Shift1_rm x, Shift1_rm y -> x.ext = y.ext && x.width = y.width && Rm.equal x.rm y.rm
    | Shift_imm_rm x, Shift_imm_rm y ->
        x.ext = y.ext && x.width = y.width && Rm.equal x.rm y.rm && Int64.equal x.imm y.imm
    | Shift_cl_rm x, Shift_cl_rm y -> x.ext = y.ext && x.width = y.width && Rm.equal x.rm y.rm
    | Setcc_rm x, Setcc_rm y -> Cc.equal x.cc y.cc && Rm.equal x.rm y.rm
    | Movx_r_rm x, Movx_r_rm y ->
        x.zero_extend = y.zero_extend && x.src_width = y.src_width && x.width = y.width
        && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Movsxd_r_rm x, Movsxd_r_rm y -> Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Sse_binop_r_rm x, Sse_binop_r_rm y ->
        x.op = y.op && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Sse_mov_r_rm x, Sse_mov_r_rm y -> x.op = y.op && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Sse_mov_rm_r x, Sse_mov_rm_r y -> x.op = y.op && Rm.equal x.rm y.rm && Reg.equal x.reg y.reg
    | Cvtsi2f_r_rm x, Cvtsi2f_r_rm y ->
        x.op = y.op && x.width = y.width && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Cvtf2i_r_rm x, Cvtf2i_r_rm y ->
        x.width = y.width && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
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
      base_alt ~label:"base-disp32" ~priority:8 ~modbits:2 ~form:D_32
        ~disp_codec:(const_disp ~name:"disp-c32" (le ~signedness:C.Signed ~width:32 "disp32"));
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

  type feature = No_features
  type target_state = { unused : unit }

  let default_state = { unused = () }
  let default_features = []
  let address_regs = List.filter (fun (r : Reg.t) -> r.width = M.address_width) M.registers

  let reg_at ~width n =
    match List.find_opt (fun (r : Reg.t) -> r.width = width && r.num = n) M.registers with
    | Some r -> r
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
    | `Sse_operand_class of sse_operand_class_mismatch ]

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

  (* The phase that detected it, which is what the code has always named. The
     codec arm delegates: [Codec.code] is [Some] only where that layer is the
     only one that could have seen the mistake (asm/docs/errors.md §2). *)
  let error_kind_code : error_kind -> string = function
    | `Unknown_instruction _ | `Missing_size_suffix _ | `Prefix66_out_of_scope
    | `Operand8_out_of_scope | `No_64bit_size _ | `Ret_takes_operands | `Ud2_takes_operands
    | `Cmov_operands | `Setcc_operands ->
        "x86.simplify"
    | `Bad_branch_suffix _ -> "x86.branch-suffix"
    | `Immediate_too_wide | `No_form _ | `Immediate_destination | `Imm_to_mem_only_movb
    | `Mov8_only_movb | `Register_width_mismatch _ | `Shift_count_not_one _ | `Sse_operand_class _
      ->
        "x86.lower"
    | `Codec e -> Option.value (Codec.code e) ~default:"x86.encode"
    | `Decode_no_match | `Decode_partial_bytes | `Decode_no_normalized -> "x86.decode"
    | `Displacement_not_8bit | `Displacement_not_32bit -> "x86.fixup"
    | `No_data_relocation _ -> "x86.data-fixup"
    | `Negative_padding _ -> "x86.nop"

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

  let simplify_instruction ~features s =
    ignore features;
    let bad kind = Error (diag ~pos:__POS__ ~origin:s.Surface.origin kind) in
    let stem, suffix = split_suffix s.Surface.mnemonic in
    let widthed ?(allow8 = false) op =
      match suffix with
      | None -> bad (`Missing_size_suffix s.Surface.mnemonic)
      | Some 16 -> bad `Prefix66_out_of_scope
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
    | _, "pop" -> Ok (Instruction.mk Opcode.Pop M.address_width s.Surface.ops)
    | _, "push" -> Ok (Instruction.mk Opcode.Push M.address_width s.Surface.ops)
    | _, "dec" -> Ok (Instruction.mk Opcode.Dec M.address_width s.Surface.ops)
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
    | "comiss", _ -> Ok (Instruction.mk Opcode.Comiss 32 s.Surface.ops)
    | "xorpd", _ -> Ok (Instruction.mk Opcode.Xorpd 32 s.Surface.ops)
    | "movapd", _ -> Ok (Instruction.mk Opcode.Movapd 32 s.Surface.ops)
    | "cvtsd2ss", _ -> Ok (Instruction.mk Opcode.Cvtsd2ss 32 s.Surface.ops)
    | "cvtss2sd", _ -> Ok (Instruction.mk Opcode.Cvtss2sd 32 s.Surface.ops)
    | "movsd", _ -> Ok (Instruction.mk Opcode.Movsd 32 s.Surface.ops)
    | "movss", _ -> Ok (Instruction.mk Opcode.Movss 32 s.Surface.ops)
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
        | _ -> widthed Opcode.Add)
    | _, "sub" -> widthed Opcode.Sub
    | _, "mov" -> widthed ~allow8:true Opcode.Mov
    | _, "lea" -> widthed Opcode.Lea
    | _, "xor" -> widthed Opcode.Xor
    | _, "and" -> widthed Opcode.And
    | _, "cmp" -> widthed Opcode.Cmp
    | _, "imul" -> widthed Opcode.Imul
    (* M4 (.ai/asm_plan.md §12): the CompCert-runtime-helper fixture's own
       measured instruction set. *)
    | _, "neg" -> widthed Opcode.Neg
    | _, "test" -> widthed Opcode.Test
    | _, "adc" -> widthed Opcode.Adc
    | _, "sbb" -> widthed Opcode.Sbb
    | _, "mul" -> widthed Opcode.Mul
    | _, "div" -> widthed Opcode.Div
    | _, "rcr" -> widthed Opcode.Rcr
    | _, "shr" -> widthed Opcode.Shr
    (* M5 (asm/docs/corpus.md): the x86_64 [test/c/] corpus's own measured
       instruction set. *)
    | _, "or" -> widthed Opcode.Or
    | _, "not" -> widthed Opcode.Not
    | _, "ror" -> widthed Opcode.Ror
    (* GAS accepts both [shl] and [sal] for the same opcode; only [sal] is
       evidenced (CompCert's own spelling), so only that stem is recognized -
       consistent with this function's general practice of not building an
       unevidenced mnemonic alias. *)
    | _, "sal" -> widthed Opcode.Shl
    | _, "sar" -> widthed Opcode.Sar
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
        | Some c, (Operand.Reg r :: _ as ops) -> Ok (Instruction.mk (Opcode.Cmov c) r.Reg.width ops)
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

  let lower_instruction state i =
    ignore state;
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
    match (i.Instruction.op, i.Instruction.ops) with
    | ( (Opcode.Add | Opcode.Adc | Opcode.And | Opcode.Sub | Opcode.Cmp | Opcode.Or | Opcode.Xor),
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
                        Lowered.Alu_rm_imm { ext; width = i.Instruction.width; rm = Rm.Reg r; imm };
                      ])
            | Operand.Mem m ->
                Ok [ Lowered.Alu_rm_imm { ext; width = i.Instruction.width; rm = Rm.Mem m; imm } ]
            | Operand.Imm _ | Operand.Sym _ -> bad `Immediate_destination))
    | Opcode.Mov, [ Operand.Imm v; Operand.Mem m ] -> (
        if i.Instruction.width <> 8 then bad `Imm_to_mem_only_movb
        else
          match imm_of v with
          | Error e -> Error e
          | Ok imm -> Ok [ Lowered.Mov_rm_imm { width = 8; rm = Rm.Mem m; imm } ])
    | Opcode.Mov, [ Operand.Imm v; Operand.Reg r ] -> (
        match (imm_of v, width_ok r) with
        | Ok imm, Ok () -> Ok [ Lowered.Mov_r_imm { width = i.Instruction.width; reg = r; imm } ]
        | Error e, _ | _, Error e -> Error e)
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
    (* M4 (.ai/asm_plan.md §12): [Add]/[Test]/[Sbb] join the pre-existing three
       here for the same reason they joined {!Opcode.to_rm_r} - real bytes
       the i64_divmod runtime-helper fixture measurably selects
       ([addl %ecx,%edx], [testl %esi,%esi], [sbbl %ecx,%edx]). *)
    | ( ( Opcode.Xor | Opcode.Cmp | Opcode.Sub | Opcode.Add | Opcode.Test | Opcode.Sbb | Opcode.Or
        | Opcode.And ),
        [ Operand.Reg a; Operand.Reg b ] ) -> (
        match (width_ok a, width_ok b) with
        | Ok (), Ok () ->
            Ok
              [
                Lowered.Alu_rm_r
                  { op = i.Instruction.op; width = i.Instruction.width; rm = Rm.Reg b; reg = a };
              ]
        | Error e, _ | _, Error e -> Error e)
    (* The reg<-rm ALU direction: {!Opcode.to_r_rm}'s mirror image of the form
       above. Only measured with a memory source ([adcl 0x20(%esp),%edx],
       [add 0x1c(%esp),%eax]) - a register-register source would also be
       valid x86, but nothing here selects it, so it is not built. *)
    | (Opcode.Adc | Opcode.Add | Opcode.Xor), [ Operand.Mem m; Operand.Reg r ] -> (
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
        | Operand.Imm _ | Operand.Sym _ -> bad `Immediate_destination)
    (* Group-2 shift/rotate, explicit-count form. The fixture always spells
       the count explicitly ([rcrl $1, %ecx], [rorl $27, %eax]), never the
       bare-mnemonic implicit-1 form GAS also accepts. A literal count of
       exactly 1 still picks {!Lowered.Shift1_rm} - GAS's own shorter,
       canonical encoding (M4's original scope here) - and any other count
       is {!Lowered.Shift_imm_rm} (M5, asm/docs/corpus.md: [rorl $27,%eax],
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
            | _, (Operand.Imm _ | Operand.Sym _) -> bad `Immediate_destination))
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
        | Operand.Imm _ | Operand.Sym _ -> bad `Immediate_destination)
    | Opcode.Push, [ Operand.Reg r ] -> Ok [ Lowered.Push { reg = r } ]
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
            | Operand.Reg r -> (
                match width_ok r with
                | Error e -> Error e
                | Ok () ->
                    Ok [ Lowered.Test_rm_imm { width = i.Instruction.width; rm = Rm.Reg r; imm } ])
            | Operand.Mem m ->
                Ok [ Lowered.Test_rm_imm { width = i.Instruction.width; rm = Rm.Mem m; imm } ]
            | Operand.Imm _ | Operand.Sym _ -> bad `Immediate_destination))
    | Opcode.Cmov cc, [ Operand.Reg a; Operand.Reg b ] -> (
        match (width_ok a, width_ok b) with
        | Ok (), Ok () ->
            Ok [ Lowered.Cmov_r_rm { cc; width = i.Instruction.width; reg = b; rm = Rm.Reg a } ]
        | Error e, _ | _, Error e -> Error e)
    | Opcode.Ret, [] -> Ok [ Lowered.Ret ]
    | Opcode.Ud2, [] -> Ok [ Lowered.Ud2 ]
    | Opcode.Pop, [ Operand.Reg r ] -> Ok [ Lowered.Pop { reg = r } ]
    | Opcode.Jmp, [ Operand.Reg r ] -> Ok [ Lowered.Jmp_rm { rm = Rm.Reg r } ]
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
        | Opcode.Mulss | Opcode.Divss | Opcode.Comisd | Opcode.Comiss | Opcode.Xorpd | Opcode.Movapd
        | Opcode.Cvtsd2ss | Opcode.Cvtss2sd ),
        [ Operand.Reg src; Operand.Reg reg ] ) -> (
        match (xmm_ok src, xmm_ok reg) with
        | Ok (), Ok () ->
            Ok [ Lowered.Sse_binop_r_rm { op = i.Instruction.op; reg; rm = Rm.Reg src } ]
        | Error e, _ | _, Error e -> Error e)
    | ( ( Opcode.Addsd | Opcode.Subsd | Opcode.Mulsd | Opcode.Divsd | Opcode.Addss | Opcode.Subss
        | Opcode.Mulss | Opcode.Divss | Opcode.Comisd | Opcode.Comiss | Opcode.Xorpd | Opcode.Movapd
        | Opcode.Cvtsd2ss | Opcode.Cvtss2sd ),
        [ Operand.Mem m; Operand.Reg reg ] ) -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Sse_binop_r_rm { op = i.Instruction.op; reg; rm = Rm.Mem m } ])
    | (Opcode.Movsd | Opcode.Movss), [ Operand.Mem m; Operand.Reg reg ] -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Sse_mov_r_rm { op = i.Instruction.op; reg; rm = Rm.Mem m } ])
    | (Opcode.Movsd | Opcode.Movss), [ Operand.Reg reg; Operand.Mem m ] -> (
        match xmm_ok reg with
        | Error e -> Error e
        | Ok () -> Ok [ Lowered.Sse_mov_rm_r { op = i.Instruction.op; rm = Rm.Mem m; reg } ])
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
           [ Opcode.Adc; Opcode.Add; Opcode.Xor ])
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
  type prefixes = { asz : bool; rex : int option }

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

  let prefixes_of ~width ~reg ~rm = { asz = asz_of ~rm; rex = prefixes_of ~width ~reg ~rm }

  let prefixes_codec : (prefixes, fixup_kind) C.t =
    C.iso_fun ~name:"prefixes"
      ~encode:(fun p -> Some (p.asz, p.rex))
      ~decode:(fun (asz, rex) -> Some { asz; rex })
      C.(asz_codec ** rex_codec)

  let width_of_prefixes p = match p.rex with Some v when v land 8 <> 0 -> 64 | _ -> 32

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
        ]
      (C.field ~width:8 "opcode")

  let sse_binop_66_codec =
    C.iso_table ~name:"sse-binop-66-op" ~equal:( = ) ~show:Opcode.name
      ~entries:[ (Opcode.Comisd, 0x2FL); (Opcode.Xorpd, 0x57L); (Opcode.Movapd, 0x28L) ]
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
           let p = { asz; rex } in
           Some
             (Lowered.Sse_binop_r_rm
                { op; reg = reg_field ~p ~width:128 e.re_reg; rm = rm_of ~p ~width:128 e.re_rm }))
         C.(
           asz_codec
           ** const ~width:8 (Int64.of_int mandatory)
           ** rex_codec ** const ~width:8 0x0FL ** opcode_codec ** rm_codec))

  (* [comiss] alone has no mandatory prefix - the one member of the binop
     family this corpus evidences with none - so it reuses [prefixes_codec]
     directly (an [asz]-then-REX composite with nothing spliced between them
     is exactly what [prefixes_codec] already is) and a single fixed 16-bit
     [0F 2F], the same shape [imul-r-rm] and [cmov-r-rm] already use for a
     mandatory-prefix-free two-byte opcode. *)
  let sse_binop_none_alt ~label ~priority =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Sse_binop_r_rm { op = Opcode.Comiss; reg; rm } ->
               Some (prefixes_of ~width:32 ~reg:reg.num ~rm, ((), { re_reg = reg.num; re_rm = rm }))
           | _ -> None)
         ~decode:(fun (p, ((), e)) ->
           Some
             (Lowered.Sse_binop_r_rm
                {
                  op = Opcode.Comiss;
                  reg = reg_field ~p ~width:128 e.re_reg;
                  rm = rm_of ~p ~width:128 e.re_rm;
                }))
         C.(prefixes_codec ** const ~width:16 0x0F2FL ** rm_codec))

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
           let p = { asz; rex } in
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
           let p = { asz; rex } in
           Some
             (Lowered.Sse_mov_rm_r
                { op; rm = rm_of ~p ~width:128 e.re_rm; reg = reg_field ~p ~width:128 e.re_reg }))
         C.(
           asz_codec
           ** const ~width:8 (Int64.of_int mandatory)
           ** rex_codec ** const ~width:16 opcode16 ** rm_codec))

  (* [cvtsi2sd]/[cvtsi2ss] ([0F 2A]): the one place [~width] threaded into
     {!prefixes_of} is a real GPR width rather than the [32] REX.W-clear
     sentinel the rest of this section uses - [rm] is the GPR/memory operand
     here, and its width is exactly what CompCert's [q] suffix already
     pinned down in {!simplify_instruction}. *)
  let sse_cvtsi2f_alt ~label ~priority ~mandatory ~op =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Cvtsi2f_r_rm { op = op'; width; reg; rm } when op' = op ->
               let p = prefixes_of ~width ~reg:reg.num ~rm in
               Some (p.asz, ((), (p.rex, ((), { re_reg = reg.num; re_rm = rm }))))
           | _ -> None)
         ~decode:(fun (asz, ((), (rex, ((), e)))) ->
           let p = { asz; rex } in
           let width = width_of_prefixes p in
           Some
             (Lowered.Cvtsi2f_r_rm
                { op; width; reg = reg_field ~p ~width:128 e.re_reg; rm = rm_of ~p ~width e.re_rm }))
         C.(
           asz_codec
           ** const ~width:8 (Int64.of_int mandatory)
           ** rex_codec ** const ~width:16 0x0F2AL ** rm_codec))

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
           let p = { asz; rex } in
           let width = width_of_prefixes p in
           Some
             (Lowered.Cvtf2i_r_rm
                { width; reg = reg_field ~p ~width e.re_reg; rm = rm_of ~p ~width:128 e.re_rm }))
         C.(asz_codec ** const ~width:8 0xF2L ** rex_codec ** const ~width:16 0x0F2CL ** rm_codec))

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

  let alu_form ~label ~priority ~opcode_byte ~imm_width =
    C.alt ~label ~priority
      (C.iso_fun ~name:label
         ~encode:(function
           | Lowered.Alu_rm_imm { ext; width; rm; imm } ->
               let fits = if imm_width = 8 then fits_s8 imm else fits_s32 imm in
               if not fits then None
               else Some (prefixes_of ~width ~reg:ext ~rm, ((), ({ re_reg = ext; re_rm = rm }, imm)))
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
           ** le ~signedness:C.Signed ~width:imm_width "imm"))

  let general_alts =
    [
      (* The short immediate first: GAS encodes [subl $12, %esp] as [83 ec 0c]
           rather than [81 ec 0c 00 00 00], and the selection rule is priority
           plus the sign-extended-imm8 range check inside the form. Getting the
           order wrong here produces working code that differs from every other
           assembler by three bytes per instruction. *)
      alu_form ~label:"alu-rm-imm8" ~priority:0 ~opcode_byte:0x83 ~imm_width:8;
      alu_form ~label:"alu-rm-imm32" ~priority:1 ~opcode_byte:0x81 ~imm_width:32;
      C.alt ~label:"mov-r-imm" ~priority:2
        (C.iso_fun ~name:"mov-r-imm"
           ~encode:(function
             | Lowered.Mov_r_imm { width; reg; imm } ->
                 Some
                   ( prefixes_of ~width ~reg:0 ~rm:(Rm.Reg reg),
                     (((), Int64.of_int (reg.num land 7)), imm) )
             | _ -> None)
           ~decode:(fun (rex, (((), r), imm)) ->
             let width = width_of_prefixes rex in
             Some
               (Lowered.Mov_r_imm
                  { width; reg = reg_at ~width (Int64.to_int r); imm = mask ~width:32 imm }))
           C.(
             prefixes_codec
             ** (const ~width:5 0b10111L ** field ~width:3 "reg")
             ** le ~signedness:C.Unsigned ~width:32 "imm32"));
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

  let codec : (Lowered.t, fixup_kind) C.t =
    C.choice ~name:M.name
      (general_alts @ moffs_alts
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
               ~decode:(fun (_rex, ((), (e, imm))) ->
                 if e.re_reg <> 0 then None
                 else Some (Lowered.Mov_rm_imm { width = 8; rm = retype_rm ~width:8 e.re_rm; imm }))
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
               ~decode:(fun (_rex, ((), e)) ->
                 if e.re_reg <> 4 then None
                 else Some (Lowered.Jmp_rm { rm = retype_rm ~width:M.address_width e.re_rm }))
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
                     Some (prefixes_of ~width ~reg:ext ~rm, ((), { re_reg = ext; re_rm = rm }))
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
                     Some (prefixes_of ~width ~reg:ext ~rm, ((), { re_reg = ext; re_rm = rm }))
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
          sse_binop_none_alt ~label:"sse-binop-none" ~priority:27;
          sse_mov_r_rm_alt ~label:"sse-movsd-load" ~priority:28 ~mandatory:0xF2 ~op:Opcode.Movsd
            ~opcode16:0x0F10L;
          sse_mov_r_rm_alt ~label:"sse-movss-load" ~priority:29 ~mandatory:0xF3 ~op:Opcode.Movss
            ~opcode16:0x0F10L;
          sse_mov_rm_r_alt ~label:"sse-movsd-store" ~priority:30 ~mandatory:0xF2 ~op:Opcode.Movsd
            ~opcode16:0x0F11L;
          sse_mov_rm_r_alt ~label:"sse-movss-store" ~priority:31 ~mandatory:0xF3 ~op:Opcode.Movss
            ~opcode16:0x0F11L;
          sse_cvtsi2f_alt ~label:"cvtsi2sd-r-rm" ~priority:32 ~mandatory:0xF2 ~op:Opcode.Cvtsi2sd;
          sse_cvtsi2f_alt ~label:"cvtsi2ss-r-rm" ~priority:33 ~mandatory:0xF3 ~op:Opcode.Cvtsi2ss;
          sse_cvtf2i_alt ~label:"cvttsd2si-r-rm" ~priority:34;
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
                     Some
                       (prefixes_of ~width ~reg:ext ~rm, ((), ({ re_reg = ext; re_rm = rm }, imm)))
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
                     Some (prefixes_of ~width ~reg:ext ~rm, ((), { re_reg = ext; re_rm = rm }))
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
    | Lowered.Alu_rm_imm { rm; _ }
    | Lowered.Mov_rm_r { rm; _ }
    | Lowered.Mov_r_rm { rm; _ }
    | Lowered.Mov_rm_imm { rm; _ }
    | Lowered.Alu_rm_r { rm; _ }
    | Lowered.Imul_r_rm { rm; _ }
    | Lowered.Cmov_r_rm { rm; _ }
    | Lowered.Jmp_rm { rm } ->
        disp_expr rm
    | Lowered.Lea { mem; _ } -> disp_expr (Rm.Mem mem)
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

  let encode l =
    (* The codec names the phase when it is the only layer that could have seen
       the mistake - a pin for a rung that does not exist - and otherwise this
       one does. A bad suffix in *source* never reaches here: [simplify] rejects
       it with a span, which is why [codec.unknown-rung] means a direct-lowered
       producer and nothing else. *)
    let fail (e : C.error) = Error (diag ~pos:__POS__ (`Codec e)) in
    match pinned_rung l with
    | Some rung -> (
        match C.encode_rung codec ~rung l with
        | Error e -> fail e
        | Ok enc -> Ok (`Fixed (form_of l enc)))
    | None -> (
        match C.encode_ladder codec l with
        | Error e -> fail e
        | Ok [ one ] -> Ok (`Fixed (form_of l one))
        | Ok forms -> Ok (`Relax (List.map (form_of l) forms)))

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
                    Operand.Imm (Bigint.of_int64 imm);
                    (match rm with Rm.Reg r -> Operand.Reg r | Rm.Mem m -> Operand.Mem m);
                  ];
                form = None;
              })
    | Lowered.Mov_r_imm { width; reg; imm } ->
        Some
          {
            Instruction.op = Opcode.Mov;
            width;
            ops = [ Operand.Imm (Bigint.of_int64 imm); Operand.Reg reg ];
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

  let decode ctx bytes ~pos =
    let bits = C.Bits.of_bytes (String.sub bytes pos (String.length bytes - pos)) in
    match C.decode_bits codec bits with
    | None -> Error (diag ~pos:__POS__ `Decode_no_match)
    | Some d -> (
        if d.C.consumed mod 8 <> 0 then Error (diag ~pos:__POS__ `Decode_partial_bytes)
        else
          let len = d.C.consumed / 8 in
          match instruction_of_lowered ~at:ctx.address ~len d.C.value with
          | None -> Error (diag ~pos:__POS__ `Decode_no_normalized)
          | Some i -> Ok (i, String.concat "." d.C.dform, len))

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
end
