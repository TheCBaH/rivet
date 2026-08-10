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

  (* The machine encodes add and sub as one opcode with the operation in the
     ModR/M reg field, so the opcode and its extension are two spellings of one
     fact and live together. [-1] is "not an ALU-immediate operation". *)
  let to_ext = function Add -> 0 | And -> 4 | Sub -> 5 | Cmp -> 7 | _ -> -1
  let of_ext = function 0 -> Some Add | 4 -> Some And | 5 -> Some Sub | 7 -> Some Cmp | _ -> None

  (* The other ALU direction: one byte per operation, r/m written from reg.
     Three rows because three are what the fixtures select - [xorl] to zero a
     register, [cmpl] before a branch, [subl] between two registers - and a row
     nothing selects is a row nothing checks. [add] is absent for that reason:
     it appears only with an immediate, which is the other form entirely. *)
  let to_rm_r = function Xor -> Some 0x31L | Cmp -> Some 0x39L | Sub -> Some 0x29L | _ -> None
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
    | Cmov_r_rm x, Cmov_r_rm y ->
        Cc.equal x.cc y.cc && x.width = y.width && Reg.equal x.reg y.reg && Rm.equal x.rm y.rm
    | Ud2, Ud2 -> true
    | Pop x, Pop y -> Reg.equal x.reg y.reg
    | Jmp_rm x, Jmp_rm y -> Rm.equal x.rm y.rm
    | Jmp_rel x, Jmp_rel y -> Asm_core.Lowered_ast.equal_branch x.target y.target
    | Jcc_rel x, Jcc_rel y ->
        Cc.equal x.cc y.cc && Asm_core.Lowered_ast.equal_branch x.target y.target
    | Call_rel x, Call_rel y -> Asm_core.Lowered_ast.equal_branch x.target y.target
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
type fixup_kind = Abs32 | Pcrel8_branch | Pcrel32_branch | Pcrel32_call | Pcrel32_data

let fixup_kind_name = function
  | Abs32 -> "abs32"
  | Pcrel8_branch -> "pcrel8-branch"
  | Pcrel32_branch -> "pcrel32-branch"
  | Pcrel32_call -> "pcrel32-call"
  | Pcrel32_data -> "pcrel32-data"

let equal_fixup_kind a b = a = b

(* Ladder compatibility is checked on the family, so the short and near rungs of
   one branch must agree here even though they are different kinds of different
   widths - which is exactly what a ladder is. *)
let fixup_family = function
  | Abs32 -> "abs"
  | Pcrel8_branch | Pcrel32_branch -> "pcrel-branch"
  | Pcrel32_call -> "pcrel-call"
  | Pcrel32_data -> "pcrel-data"

let fixup_role = function
  | Abs32 | Pcrel32_data -> Asm_core.Lowered_ast.Data_address
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

(* The displacement of the mod=00 rm=101 form, which is the one that can carry a
   symbol. Symbolic values write a placeholder and emit a placement; constants
   write themselves and their placement is dropped for want of an expression, so
   one node serves both without a second alternative. *)
let sym_disp ~kind =
  C.iso_fun ~name:"disp-sym"
    ~encode:(fun d -> Some (Disp.placeholder d))
    ~decode:(fun v -> Some (Disp.Const v))
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
  let ebp_like = match m.base with Some b -> b.num land 7 = 5 | None -> false in
  match m.disp with
  (* A symbolic displacement is always the wide form: its value is not known
     here, and a byte could not hold an address even if it were. *)
  | Disp.Sym _ -> D_32
  | Disp.Const v ->
      if Int64.equal v 0L && not ebp_like then D_none else if fits_s8 v then D_8 else D_32

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
      sib_alt ~label:"sib-disp0" ~priority:1 ~modbits:0 ~form:D_none
        ~disp_codec:(const_disp ~name:"disp-none" no_disp);
      sib_alt ~label:"sib-disp8" ~priority:2 ~modbits:1 ~form:D_8
        ~disp_codec:(const_disp ~name:"disp-c8" (le ~signedness:C.Signed ~width:8 "disp8"));
      sib_alt ~label:"sib-disp32" ~priority:3 ~modbits:2 ~form:D_32
        ~disp_codec:(const_disp ~name:"disp-c32" (le ~signedness:C.Signed ~width:32 "disp32"));
      base_alt ~label:"base-disp0" ~priority:5 ~modbits:0 ~form:D_none
        ~disp_codec:(const_disp ~name:"disp-none" no_disp);
      base_alt ~label:"base-disp8" ~priority:6 ~modbits:1 ~form:D_8
        ~disp_codec:(const_disp ~name:"disp-c8" (le ~signedness:C.Signed ~width:8 "disp8"));
      base_alt ~label:"base-disp32" ~priority:7 ~modbits:2 ~form:D_32
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
      C.alt ~label:"disp32-norm" ~priority:4
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

  let diag ?origin code message =
    Diagnostic.error ~code ~message
      ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:M.name ())
      ()

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
    let bad msg = Error (diag ~origin:s.Surface.origin "x86.simplify" msg) in
    let stem, suffix = split_suffix s.Surface.mnemonic in
    let widthed ?(allow8 = false) op =
      match suffix with
      | None ->
          bad (Printf.sprintf "%s needs an operand-size suffix (b, w, l or q)" s.Surface.mnemonic)
      | Some 16 -> bad "16-bit operands need the 0x66 prefix, which is not in M1 scope"
      | Some 8 when not allow8 -> bad "8-bit operands are not in M1 scope"
      | Some w when w = 64 && not M.rex_allowed ->
          bad (Printf.sprintf "%s has no 64-bit operand size" M.name)
      | Some w -> Ok (Instruction.mk op w s.Surface.ops)
    in
    match (s.Surface.mnemonic, stem) with
    | "ret", _ ->
        if s.Surface.ops = [] then Ok (Instruction.mk Opcode.Ret M.address_width [])
        else bad "ret takes no operands in M1"
    | "ud2", _ ->
        if s.Surface.ops = [] then Ok (Instruction.mk Opcode.Ud2 M.address_width [])
        else bad "ud2 takes no operands"
    | "pop", _ -> Ok (Instruction.mk Opcode.Pop M.address_width s.Surface.ops)
    | "jmp", _ -> Ok (Instruction.mk Opcode.Jmp M.address_width s.Surface.ops)
    (* No size suffix, in either mode: a near call is rel32 on x86-32 and on
       x86-64 alike, so there is nothing for a suffix to select. *)
    | "call", _ -> Ok (Instruction.mk Opcode.Call M.address_width s.Surface.ops)
    | _, "add" -> widthed Opcode.Add
    | _, "sub" -> widthed Opcode.Sub
    | _, "mov" -> widthed ~allow8:true Opcode.Mov
    | _, "lea" -> widthed Opcode.Lea
    | _, "xor" -> widthed Opcode.Xor
    | _, "and" -> widthed Opcode.And
    | _, "cmp" -> widthed Opcode.Cmp
    | _, "imul" -> widthed Opcode.Imul
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
        | None -> bad (Printf.sprintf "unknown instruction %s" s.Surface.mnemonic))
    (* A suffix on a branch that is not one of this target's rungs is rejected
       *here*, in the phase that knows the mnemonic and has a source span, and
       never reaches the codec - which is why [codec.unknown-rung] is reserved
       for a pin that arrived through the direct-lowered path instead. Freezing
       which phase answers keeps the two diagnostics from drifting into each
       other. *)
    | m, _ when bad_branch_suffix m ->
        Error
          (Diagnostic.error ~code:"x86.branch-suffix"
             ~message:
               (Printf.sprintf "%s: the branch form suffixes are %s" m
                  (String.concat ", " (List.map (fun r -> "." ^ r) branch_rungs)))
             ~origin:s.Surface.origin ())
    | m, _ when Cc.split_after "cmov" m <> None -> (
        match (Cc.split_after "cmov" m, s.Surface.ops) with
        | Some c, (Operand.Reg r :: _ as ops) -> Ok (Instruction.mk (Opcode.Cmov c) r.Reg.width ops)
        | Some _, _ -> bad "cmov takes two register operands in M2"
        | None, _ -> bad (Printf.sprintf "unknown instruction %s" s.Surface.mnemonic))
    | _ -> bad (Printf.sprintf "unknown instruction %s" s.Surface.mnemonic)

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
    let bad msg = Error (diag "x86.lower" msg) in
    let imm_of v =
      match Bigint.to_int64_opt v with
      | Some x -> Ok x
      | None -> Error (diag "x86.lower" "immediate does not fit 64 bits")
    in
    let width_ok (r : Reg.t) =
      if r.width = i.Instruction.width then Ok ()
      else
        bad
          (Printf.sprintf "%s is %d-bit but the instruction is %d-bit" r.name r.width
             i.Instruction.width)
    in
    match (i.Instruction.op, i.Instruction.ops) with
    | (Opcode.Add | Opcode.And | Opcode.Sub | Opcode.Cmp), [ Operand.Imm v; dst ] -> (
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
            | Operand.Imm _ | Operand.Sym _ -> bad "an immediate cannot be a destination"))
    | Opcode.Mov, [ Operand.Imm v; Operand.Mem m ] -> (
        if i.Instruction.width <> 8 then
          bad "movb $imm,(mem) is the only imm-to-memory mov form in M1"
        else
          match imm_of v with
          | Error e -> Error e
          | Ok imm -> Ok [ Lowered.Mov_rm_imm { width = 8; rm = Rm.Mem m; imm } ])
    | Opcode.Mov, _ when i.Instruction.width = 8 ->
        bad "8-bit mov is only supported as movb $imm,(mem) in M1"
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
    | (Opcode.Xor | Opcode.Cmp | Opcode.Sub), [ Operand.Reg a; Operand.Reg b ] -> (
        match (width_ok a, width_ok b) with
        | Ok (), Ok () ->
            Ok
              [
                Lowered.Alu_rm_r
                  { op = i.Instruction.op; width = i.Instruction.width; rm = Rm.Reg b; reg = a };
              ]
        | Error e, _ | _, Error e -> Error e)
    (* The operands swap sides relative to the ALU forms above: AT&T [imull
       %%esi, %%ecx] writes %%ecx, and the register field of an [0f af] is the
       destination rather than the source. *)
    | Opcode.Imul, [ Operand.Reg a; Operand.Reg b ] -> (
        match (width_ok a, width_ok b) with
        | Ok (), Ok () ->
            Ok [ Lowered.Imul_r_rm { width = i.Instruction.width; reg = b; rm = Rm.Reg a } ]
        | Error e, _ | _, Error e -> Error e)
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
    | _ -> bad (Printf.sprintf "no %s form takes these operands" (Opcode.name i.Instruction.op))

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
           [ Opcode.Xor; Opcode.Cmp; Opcode.Sub ])
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
      if w || r || x || b then
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
          C.alt ~label:"mov-rm-r" ~priority:5
            (C.iso_fun ~name:"mov-rm-r"
               ~encode:(function
                 | Lowered.Mov_rm_r { width; rm; reg } ->
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
          C.alt ~label:"mov-r-rm" ~priority:6
            (C.iso_fun ~name:"mov-r-rm"
               ~encode:(function
                 | Lowered.Mov_r_rm { width; reg; rm } ->
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
      pc_bias = (match kind with Abs32 -> 0 | _ -> total_bytes);
      range =
        (match kind with
        (* An absolute 32-bit address is a bit pattern: 0x80000000 upwards is a
           valid address, and a signed range would reject half of them. *)
        | Abs32 -> Asm_core.Lowered_ast.Bitpattern 32
        | Pcrel8_branch -> Asm_core.Lowered_ast.Signed 8
        | Pcrel32_branch | Pcrel32_call | Pcrel32_data -> Asm_core.Lowered_ast.Signed 32);
      value = Asm_core.Expr.Const (Bigint.of_int 0);
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
    let fail (e : C.error) =
      Error (diag (Option.value e.C.code ~default:"x86.encode") (Fmt.to_to_string C.pp_error e))
    in
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

  let decode ctx bytes ~pos =
    let bits = C.Bits.of_bytes (String.sub bytes pos (String.length bytes - pos)) in
    match C.decode_bits codec bits with
    | None -> Error (diag "x86.decode" "no form matches these bytes")
    | Some d -> (
        if d.C.consumed mod 8 <> 0 then
          Error (diag "x86.decode" "a form consumed a non-whole number of bytes")
        else
          let len = d.C.consumed / 8 in
          match instruction_of_lowered ~at:ctx.address ~len d.C.value with
          | None -> Error (diag "x86.decode" "decoded a form with no normalized instruction")
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
    | Pcrel8_branch ->
        let d = Int64.sub target place in
        if Int64.compare d (-128L) >= 0 && Int64.compare d 128L < 0 then Ok d
        else Error (diag "x86.fixup" "displacement does not fit 8 bits")
    | Pcrel32_branch | Pcrel32_call | Pcrel32_data ->
        let d = Int64.sub target place in
        if fits_s32 d then Ok d else Error (diag "x86.fixup" "displacement does not fit 32 bits")

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
    | _ ->
        Error
          (diag "x86.data-fixup"
             (Printf.sprintf "no absolute relocation for a %d-byte data initializer" width))

  let nop_table = M.nop_table

  let nop_bytes ~length =
    if length < 0 then Error (diag "x86.nop" "negative padding length")
    else
      let buf = Buffer.create length in
      let rec go n =
        if n = 0 then ()
        else
          let take = min n (Array.length nop_table) in
          Buffer.add_string buf nop_table.(take - 1);
          go (n - take)
      in
      go length;
      Ok (Buffer.contents buf)
end
