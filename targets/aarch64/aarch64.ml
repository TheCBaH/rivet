(* The aarch64 (A64) target description (.ai/asm_plan.md §5.1).

   Deliberately not in a family functor with ARM. A64 is a different
   instruction set that happens to share a vendor: no condition field, no
   rotated immediates, a 31st register whose meaning depends on the instruction,
   and scaled offsets rather than magnitude-plus-sign.

   Byte order, as for A32: one 32-bit word authored most significant bit first,
   reversed once in {!encode}.

   Every constructor family lives in its own module - [Reg], [Mem], [Shift],
   [Opcode], [Operand], [Surface], [Instruction], [Lowered] - rather than in
   one flat namespace with disambiguating prefixes. The prefixes were
   load-bearing: [Op_add] was an opcode and [Op_reg] an operand, and the two
   families could not be told apart by name. Under modules the family is the
   qualifier, [Opcode.Add] and [Operand.Reg] compose without collision, the
   record fields lose their [i_]/[s_]/[m_] disambiguators, and a hand-written
   AST reads as the assembly it stands for. {!Target_intf.Target.TARGET} names
   the same modules, so a target satisfies it by exposing what it already has
   rather than by restating each type and printer as a loose alias. *)

open Foundation
module C = Codec

(* {1 Registers}

   Register 31 is [sp] in some instruction positions and [xzr] in others, and
   nothing in the encoding says which - the *instruction* says. So the flag is
   part of the register value and is checked at lowering, where the diagnostic
   can name the operand, rather than being inferred at encode time where it
   could only be guessed. *)

module Reg = struct
  type t = { num : int; width : int; is_sp : bool }

  let equal a b = a.num = b.num && a.width = b.width && a.is_sp = b.is_sp

  let name r =
    let p = if r.width = 64 then "x" else "w" in
    if r.num = 31 then if r.is_sp then if r.width = 64 then "sp" else "wsp" else p ^ "zr"
    else Printf.sprintf "%s%d" p r.num

  let pp ppf r = Fmt.string ppf (name r)

  let find name =
    let numbered prefix width =
      let n = String.length prefix in
      if String.length name > n && String.sub name 0 n = prefix then
        match int_of_string_opt (String.sub name n (String.length name - n)) with
        | Some i when i >= 0 && i <= 30 -> Some { num = i; width; is_sp = false }
        | _ -> None
      else None
    in
    match name with
    | "sp" -> Some { num = 31; width = 64; is_sp = true }
    | "wsp" -> Some { num = 31; width = 32; is_sp = true }
    | "xzr" -> Some { num = 31; width = 64; is_sp = false }
    | "wzr" -> Some { num = 31; width = 32; is_sp = false }
    | _ -> ( match numbered "x" 64 with Some r -> Some r | None -> numbered "w" 32)

  (* Decoding cannot recover [is_sp] from the bits, so it is supplied by the
     position: the caller says whether this field is an SP-accepting one. *)
  let of_num ~width ~sp n = { num = n land 31; width; is_sp = sp && n land 31 = 31 }

  (* The link register, which [ret] defaults to and which the ABI helpers name
     often enough that spelling the record out each time obscured the intent. *)
  let x30 = { num = 30; width = 64; is_sp = false }
end

(* {1 Operands} *)

(* A memory offset is a number or an expression (§D2). [ldr w0, [x16, #:lo12:g]]
   is the reason: the offset is not known until the linker places [g], so a
   numeric field cannot hold it and a separate "symbolic load" form would be a
   second spelling of the same addressing mode. *)
module Disp = struct
  type t = Const of int64 | Sym of Asm_core.Expr.t

  let zero = Const 0L
  let is_zero = function Const v -> Int64.equal v 0L | Sym _ -> false
  let equal a b = match (a, b) with Const x, Const y -> Int64.equal x y | _ -> a = b
  let to_string = function Const v -> Printf.sprintf "#%Ld" v | Sym e -> Asm_core.Expr.to_string e
end

module Mem = struct
  type t = { base : Reg.t; offset : Disp.t; writeback : bool; pre : bool }

  let pp ppf m =
    let off = if Disp.is_zero m.offset then "" else ", " ^ Disp.to_string m.offset in
    Fmt.pf ppf "[%a%s]%s" Reg.pp m.base off (if m.writeback then "!" else "")
end

module Shift = struct
  type t = { kind : string; amount : int }
end

module Operand = struct
  type t =
    | Reg of Reg.t
    | Imm of Bigint.t
    | Mem of Mem.t
    | Shift of Shift.t
        (** [lsl #0] in [movz w0, #42, lsl #0]. Not an operand in any real sense - it modifies the
            immediate before it - but the common parser hands over comma-separated slices and this
            is one of them. Simplify folds it into the operand it belongs to; a surface AST that
            had already folded it could not report a shift applied to nothing. *)
    | Sym of Asm_core.Expr.t
        (** a branch or call target. Written without a [#], unlike an immediate: it is an address,
            not a value, and the two are spelled differently for that reason. *)

  let pp ppf = function
    | Reg r -> Reg.pp ppf r
    | Imm v -> Fmt.pf ppf "#%a" Bigint.pp v
    | Mem m -> Mem.pp ppf m
    | Shift s -> Fmt.pf ppf "%s #%d" s.Shift.kind s.Shift.amount
    | Sym e -> Fmt.string ppf (Asm_core.Expr.to_string e)
end

module Surface = struct
  type t = { mnemonic : string; ops : Operand.t list; origin : Origin.t }

  let pp ppf s =
    match s.ops with
    | [] -> Fmt.string ppf s.mnemonic
    | ops -> Fmt.pf ppf "%s %a" s.mnemonic Fmt.(list ~sep:(any ", ") Operand.pp) ops
end

(* {1 Condition codes}

   Unlike A32 a condition is not on every instruction: A64 has three consumers -
   [b.<cc>], [csel] and the [cset] family - and this file needs the first two.
   Declared in encoding order, so the code is the position, and all sixteen are
   named because [decode] must be total over the field. *)
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

  (* Input-only spellings, as on A32: objdump prints [cs] and [cc]. *)
  let synonyms = [ ("hs", Cs); ("lo", Cc) ]

  let of_name s =
    match List.find_opt (fun c -> String.equal (name c) s) all with
    | Some c -> Some c
    | None -> List.assoc_opt s synonyms
end

module Opcode = struct
  type t =
    | Add
    | Mov
    | Movz
    | Stp
    | Ldr
    | Ldrb
    | Ldrh
    | Str
    | Strb
    | Strh
    | Ret
    | Sub
    | Cmp
    | And
    | Orr
    | Madd
    | Mul
    | Csel
    | Adrp
    | B
    | Bcond of Cond.t
    | Udf
    | Bl

  let name = function
    | Add -> "add"
    | Mov -> "mov"
    | Movz -> "movz"
    | Stp -> "stp"
    | Ldr -> "ldr"
    | Ldrb -> "ldrb"
    | Ldrh -> "ldrh"
    | Str -> "str"
    | Strb -> "strb"
    | Strh -> "strh"
    | Ret -> "ret"
    | Sub -> "sub"
    | Cmp -> "cmp"
    | And -> "and"
    | Orr -> "orr"
    | Madd -> "madd"
    | Mul -> "mul"
    | Csel -> "csel"
    | Adrp -> "adrp"
    | B -> "b"
    | Bcond c -> "b." ^ Cond.name c
    | Udf -> "udf"
    | Bl -> "bl"

  (* The surface spellings this target accepts, and the only place the mapping
     lives. [simplify_instruction] consults it rather than repeating the list,
     so a mnemonic that is added here is accepted there by construction.

     [b.<cc>] is the one spelling that is not a fixed string. It lexes as a
     single identifier because [.] is an identifier character in this dialect -
     the same reason [.L100] is one token - so the condition is split off here
     rather than by the lexer. *)
  let of_mnemonic = function
    | "add" -> Some Add
    | "mov" -> Some Mov
    | "movz" -> Some Movz
    | "stp" -> Some Stp
    | "ldr" -> Some Ldr
    | "ldrb" -> Some Ldrb
    | "ldrh" -> Some Ldrh
    | "str" -> Some Str
    | "strb" -> Some Strb
    | "strh" -> Some Strh
    | "ret" -> Some Ret
    | "sub" -> Some Sub
    | "cmp" -> Some Cmp
    | "and" -> Some And
    | "orr" -> Some Orr
    | "madd" -> Some Madd
    | "mul" -> Some Mul
    | "csel" -> Some Csel
    | "adrp" -> Some Adrp
    | "b" -> Some B
    | "udf" -> Some Udf
    | "bl" -> Some Bl
    | m ->
        if String.length m > 2 && String.sub m 0 2 = "b." then
          Option.map (fun c -> Bcond c) (Cond.of_name (String.sub m 2 (String.length m - 2)))
        else None
end

module Instruction = struct
  type t = { op : Opcode.t; ops : Operand.t list }

  let mk op ops = { op; ops }

  let pp ppf i =
    match i.ops with
    | [] -> Fmt.string ppf (Opcode.name i.op)
    | ops -> Fmt.pf ppf "%s %a" (Opcode.name i.op) Fmt.(list ~sep:(any ", ") Operand.pp) ops
end

(* The low-12 relocation is *scaled by the access width*, which is why it is a
   four-case variant and not one kind: R_AARCH64_LDST32_ABS_LO12_NC divides the
   offset by 4 where the 8-bit form does not divide at all, and neither is the
   same transform as [add]'s unscaled ADD_ABS_LO12_NC. A finite variant rather
   than an [int] also keeps the kind domain genuinely finite, which is what lets
   the name-consistency test enumerate it.

   It doubles as the load/store size field, which is not a coincidence to route
   around: the field, the scale and the relocation are the same fact. *)
type access_size = B | H | W | X

let access_bytes = function B -> 1 | H -> 2 | W -> 4 | X -> 8
let access_shift = function B -> 0 | H -> 1 | W -> 2 | X -> 3
let access_code = function B -> 0 | H -> 1 | W -> 2 | X -> 3
let access_all = [ B; H; W; X ]

(* The register a load or store names is 32-bit for every size but the last:
   [ldrb w0] reads one byte into a [w] register. *)
let access_reg_width = function X -> 64 | B | H | W -> 32

let ldst_mnemonic ~size ~load =
  (if load then "ldr" else "str") ^ match size with B -> "b" | H -> "h" | W | X -> ""

let shift_name = function 0 -> "lsl" | 1 -> "lsr" | 2 -> "asr" | 3 -> "ror" | _ -> "?"

let shift_of_name = function
  | "lsl" -> Some 0
  | "lsr" -> Some 1
  | "asr" -> Some 2
  | "ror" -> Some 3
  | _ -> None

let logical_name = function 0 -> "and" | 1 -> "orr" | 2 -> "eor" | 3 -> "ands" | _ -> "?"

module Lowered = struct
  type t =
    | Add_imm of { rd : Reg.t; rn : Reg.t; imm : int64; shift12 : bool }
    | Stp_pre of { rt : Reg.t; rt2 : Reg.t; rn : Reg.t; offset : int64 }
    | Movz of { rd : Reg.t; imm16 : int64; hw : int }
    | Ldst_uoff of { size : access_size; load : bool; rt : Reg.t; rn : Reg.t; offset : Disp.t }
        (** One addressing mode, eight forms. The size decides the register width, the field scale
            and the relocation kind at once, so they cannot disagree. *)
    | Ret of { rn : Reg.t }
    | Sub_imm of { s : bool; rd : Reg.t; rn : Reg.t; imm : int64; shift12 : bool }
        (** [s] is the flag-setting bit, and it changes what [rd = 31] means: SP without it and the
            zero register with it. That is why the two are separate codec alternatives rather than
            one form with a field - the register decoding depends on a bit the sequential
            interpreter has already walked past. *)
    | Addsub_shift of {
        sub : bool;
        s : bool;
        rd : Reg.t;
        rn : Reg.t;
        rm : Reg.t;
        shift : int;
        amount : int;
      }
        (** ADD/SUB (shifted register). [cmp] is this form with [sub], [s] and [rd = zr], which is
            what the machine means by a comparison - there is no compare instruction. *)
    | Logical_imm of { opc : int; rd : Reg.t; rn : Reg.t; imm : int64 }
        (** AND/ORR/EOR/ANDS with a bitmask immediate. *)
    | Madd of { rd : Reg.t; rn : Reg.t; rm : Reg.t; ra : Reg.t }
    | Csel of { cond : Cond.t; rd : Reg.t; rn : Reg.t; rm : Reg.t }
    | Adrp of { rd : Reg.t; page : Asm_core.Lowered_ast.branch }
    | B of { target : Asm_core.Lowered_ast.branch }
    | Bcond of { cond : Cond.t; target : Asm_core.Lowered_ast.branch }
    | Udf of { imm16 : int64 }
    | Bl of { target : Asm_core.Lowered_ast.branch }

  let pp ppf = function
    (* [add xD, sp, #0] is spelled [mov xD, sp] by every A64 disassembler, and
       the canonical dump has to agree with objdump for the differential gate to
       mean anything. The alias lives here, in printing, and not in the lowered
       type - there is exactly one encoding and inventing a second constructor
       for it would let the two drift. *)
    | Add_imm { rd; rn; imm; shift12 } ->
        if Int64.equal imm 0L && (not shift12) && (rd.Reg.is_sp || rn.Reg.is_sp) then
          Fmt.pf ppf "mov %a, %a" Reg.pp rd Reg.pp rn
        else
          Fmt.pf ppf "add %a, %a, #%Ld%s" Reg.pp rd Reg.pp rn imm
            (if shift12 then ", lsl #12" else "")
    | Stp_pre { rt; rt2; rn; offset } ->
        Fmt.pf ppf "stp %a, %a, [%a, #%Ld]!" Reg.pp rt Reg.pp rt2 Reg.pp rn offset
    | Movz { rd; imm16; hw } ->
        Fmt.pf ppf "movz %a, #%Ld%s" Reg.pp rd imm16
          (if hw = 0 then "" else Printf.sprintf ", lsl #%d" (hw * 16))
    | Ldst_uoff { size; load; rt; rn; offset } ->
        Fmt.pf ppf "%s %a, %a" (ldst_mnemonic ~size ~load) Reg.pp rt Mem.pp
          { Mem.base = rn; offset; writeback = false; pre = true }
    | Ret { rn } ->
        (* [ret x30] is spelled [ret]: x30 is the architectural default and every
           A64 disassembler elides it. Printing it would make the canonical dump
           disagree with objdump on a line where nothing differs. *)
        if rn.Reg.num = 30 && not rn.Reg.is_sp then Fmt.string ppf "ret"
        else Fmt.pf ppf "ret %a" Reg.pp rn
    | Sub_imm { s; rd; rn; imm; shift12 } ->
        let tail = if shift12 then ", lsl #12" else "" in
        if s && rd.Reg.num = 31 && not rd.Reg.is_sp then
          Fmt.pf ppf "cmp %a, #%Ld%s" Reg.pp rn imm tail
        else
          Fmt.pf ppf "%s %a, %a, #%Ld%s" (if s then "subs" else "sub") Reg.pp rd Reg.pp rn imm tail
    (* [cmp] and [mul] are printed rather than encoded: each is one bit pattern
       of a wider form - a [subs] whose destination is discarded, a [madd] whose
       accumulator is the zero register - and objdump prints both aliases. A
       lowered constructor per alias would be a second name for one encoding. *)
    | Addsub_shift { sub; s; rd; rn; rm; shift; amount } ->
        let tail = if amount = 0 then "" else Printf.sprintf ", %s #%d" (shift_name shift) amount in
        if sub && s && rd.Reg.num = 31 && not rd.Reg.is_sp then
          Fmt.pf ppf "cmp %a, %a%s" Reg.pp rn Reg.pp rm tail
        else
          Fmt.pf ppf "%s%s %a, %a, %a%s"
            (if sub then "sub" else "add")
            (if s then "s" else "")
            Reg.pp rd Reg.pp rn Reg.pp rm tail
    | Logical_imm { opc; rd; rn; imm } ->
        Fmt.pf ppf "%s %a, %a, #%Ld" (logical_name opc) Reg.pp rd Reg.pp rn imm
    | Madd { rd; rn; rm; ra } ->
        if ra.Reg.num = 31 && not ra.Reg.is_sp then
          Fmt.pf ppf "mul %a, %a, %a" Reg.pp rd Reg.pp rn Reg.pp rm
        else Fmt.pf ppf "madd %a, %a, %a, %a" Reg.pp rd Reg.pp rn Reg.pp rm Reg.pp ra
    | Csel { cond; rd; rn; rm } ->
        Fmt.pf ppf "csel %a, %a, %a, %s" Reg.pp rd Reg.pp rn Reg.pp rm (Cond.name cond)
    | Adrp { rd; page } -> Fmt.pf ppf "adrp %a, %a" Reg.pp rd Asm_core.Lowered_ast.pp_branch page
    | B { target } -> Fmt.pf ppf "b %a" Asm_core.Lowered_ast.pp_branch target
    | Bcond { cond; target } ->
        Fmt.pf ppf "b.%s %a" (Cond.name cond) Asm_core.Lowered_ast.pp_branch target
    | Udf { imm16 } -> Fmt.pf ppf "udf #%Ld" imm16
    | Bl { target } -> Fmt.pf ppf "bl %a" Asm_core.Lowered_ast.pp_branch target

  let equal a b =
    match (a, b) with
    | Add_imm x, Add_imm y ->
        Reg.equal x.rd y.rd && Reg.equal x.rn y.rn && Int64.equal x.imm y.imm
        && x.shift12 = y.shift12
    | Stp_pre x, Stp_pre y ->
        Reg.equal x.rt y.rt && Reg.equal x.rt2 y.rt2 && Reg.equal x.rn y.rn
        && Int64.equal x.offset y.offset
    | Movz x, Movz y -> Reg.equal x.rd y.rd && Int64.equal x.imm16 y.imm16 && x.hw = y.hw
    | Ldst_uoff x, Ldst_uoff y ->
        x.size = y.size && x.load = y.load && Reg.equal x.rt y.rt && Reg.equal x.rn y.rn
        && Disp.equal x.offset y.offset
    | Ret x, Ret y -> Reg.equal x.rn y.rn
    | Sub_imm x, Sub_imm y ->
        x.s = y.s && Reg.equal x.rd y.rd && Reg.equal x.rn y.rn && Int64.equal x.imm y.imm
        && x.shift12 = y.shift12
    | Addsub_shift x, Addsub_shift y ->
        x.sub = y.sub && x.s = y.s && Reg.equal x.rd y.rd && Reg.equal x.rn y.rn
        && Reg.equal x.rm y.rm && x.shift = y.shift && x.amount = y.amount
    | Logical_imm x, Logical_imm y ->
        x.opc = y.opc && Reg.equal x.rd y.rd && Reg.equal x.rn y.rn && Int64.equal x.imm y.imm
    | Madd x, Madd y ->
        Reg.equal x.rd y.rd && Reg.equal x.rn y.rn && Reg.equal x.rm y.rm && Reg.equal x.ra y.ra
    | Csel x, Csel y ->
        Cond.equal x.cond y.cond && Reg.equal x.rd y.rd && Reg.equal x.rn y.rn
        && Reg.equal x.rm y.rm
    | Adrp x, Adrp y -> Reg.equal x.rd y.rd && Asm_core.Lowered_ast.equal_branch x.page y.page
    | B x, B y -> Asm_core.Lowered_ast.equal_branch x.target y.target
    | Bcond x, Bcond y ->
        Cond.equal x.cond y.cond && Asm_core.Lowered_ast.equal_branch x.target y.target
    | Udf x, Udf y -> Int64.equal x.imm16 y.imm16
    | Bl x, Bl y -> Asm_core.Lowered_ast.equal_branch x.target y.target
    | _ -> false
end

type fixup_kind =
  | Abs32
  | Abs64
  | Pcrel_b26
  | Pcrel_b19  (** the conditional branch's shorter field - R_AARCH64_CONDBR19 *)
  | Pcrel_call26
  | Adrp_page
  | Add_lo12
  | Ldst_lo12 of access_size

let fixup_kind_name = function
  | Abs32 -> "abs32"
  | Abs64 -> "abs64"
  | Pcrel_b26 -> "pcrel-b26"
  | Pcrel_b19 -> "pcrel-b19"
  | Pcrel_call26 -> "pcrel-call26"
  | Adrp_page -> "adrp-page"
  | Add_lo12 -> "add-lo12"
  | Ldst_lo12 sz -> Printf.sprintf "ldst%d-lo12" (access_bytes sz * 8)

let equal_fixup_kind a b = a = b

let fixup_family = function
  | Abs32 -> "abs32"
  | Abs64 -> "abs64"
  | Pcrel_b26 | Pcrel_b19 -> "pcrel-branch"
  | Pcrel_call26 -> "pcrel-call"
  | Adrp_page -> "adrp-page"
  | Add_lo12 -> "add-lo12"
  | Ldst_lo12 _ -> "ldst-lo12"

let fixup_role = function
  | Abs32 | Abs64 | Adrp_page | Add_lo12 | Ldst_lo12 _ -> Asm_core.Lowered_ast.Data_address
  | Pcrel_b26 | Pcrel_b19 -> Asm_core.Lowered_ast.Branch
  | Pcrel_call26 -> Asm_core.Lowered_ast.Call

type feature = No_features
type target_state = { unused : unit }

let default_state = { unused = () }
let default_features = []

(* {1 Scaled immediates}

   A64 stores byte offsets divided by the access size, so [#8] in
   [ldr x30, [sp, #8]] is the field value 1. The scaling is part of the
   encoding, so it is an [Iso_fun] in the tree - which also means [decode]
   multiplies back without a second copy of the rule.

   Unlike ARM's modified immediates these domains are not exhaustible (a 12-bit
   scaled field admits 4096 values, a signed 7-bit one 128), so what they get is
   a declared boundary sample: zero, both extremes, one past each extreme, and
   one misaligned value. That is a weaker claim than exhaustiveness and is
   recorded as one. *)

let scaled ~shift ~width ~signedness name =
  let step = Int64.shift_left 1L shift in
  C.iso_fun
    ~name:(Printf.sprintf "%s-scaled%d" name (1 lsl shift))
    ~encode:(fun v -> if Int64.equal (Int64.rem v step) 0L then Some (Int64.div v step) else None)
    ~decode:(fun f -> Some (Int64.mul f step))
    (C.field ~signedness ~width name)

let uoff12_domain =
  C.Finite.sample ~name:"aarch64.ldr-uoff12"
    ~why:
      "the field is 12 unsigned bits scaled by eight, so the domain is 4096 values; enumerating it \
       would test the same arithmetic 4096 times, so this is the declared boundary set - zero, \
       both extremes, one past each, and one misaligned value"
    [ 0L; 8L; 16L; 32752L; 32760L ]

let stp_imm7_domain =
  C.Finite.sample ~name:"aarch64.stp-imm7"
    ~why:
      "the field is 7 signed bits scaled by eight, so the domain is 128 values; the declared \
       boundary set is zero, both extremes and the two steps adjacent to them"
    [ -512L; -504L; -16L; 0L; 8L; 496L; 504L ]

(* {1 Bitmask immediates}

   A64's logical immediates are not values but *patterns*: a run of ones,
   rotated, replicated to fill the register. [orr w0, wzr, #7] - which is how
   CompCert materializes a small constant - encodes 7 as "three ones, no
   rotation, element size 32".

   The relation is the architecture's own [DecodeBitMasks], transcribed rather
   than re-derived, and the encode direction is a search over all 8192 (N, immr,
   imms) triples. That is the same choice ARM's modified immediates made and for
   the same reason: the domain is small enough to enumerate, so a table scan is
   both the simplest implementation and an exhaustive test.

   The relation depends on the register width, and not by scaling - a pattern
   that means 0x7 in 32 bits means 0x700000007 in 64 - so the two widths are
   separate relations rather than one with a parameter. *)

let highest_set_bit v =
  let rec go i = if i < 0 then -1 else if v land (1 lsl i) <> 0 then i else go (i - 1) in
  go 6

let ror64 v r size =
  if size >= 64 then
    Int64.logor (Int64.shift_right_logical v r) (if r = 0 then 0L else Int64.shift_left v (64 - r))
  else
    let mask = Int64.sub (Int64.shift_left 1L size) 1L in
    let v = Int64.logand v mask in
    Int64.logand
      (Int64.logor (Int64.shift_right_logical v r)
         (if r = 0 then 0L else Int64.shift_left v (size - r)))
      mask

let decode_bitmask ~datasize ~n ~immr ~imms =
  let len = highest_set_bit ((n lsl 6) lor (lnot imms land 0x3f)) in
  if len < 1 || 1 lsl len > datasize then None
  else
    let levels = (1 lsl len) - 1 in
    let s = imms land levels and r = immr land levels in
    (* [S = levels] would be a run as long as the element, which is the all-ones
       value every element size can already express - so the architecture
       reserves it rather than allowing four spellings of -1. *)
    if s = levels then None
    else
      let esize = 1 lsl len in
      let welem = Int64.sub (Int64.shift_left 1L (s + 1)) 1L in
      let e = ror64 welem r esize in
      let rec replicate acc bits =
        if bits >= datasize then acc
        else replicate (Int64.logor acc (Int64.shift_left e bits)) (bits + esize)
      in
      let v = replicate 0L 0 in
      Some (if datasize = 64 then v else Int64.logand v 0xFFFFFFFFL)

(* Every (N, immr, imms) triple that decodes, in field order, so the first match
   is the canonical spelling - which is the one GNU as picks. *)
let bitmask_triples ~datasize =
  List.concat_map
    (fun n ->
      List.concat_map
        (fun immr -> List.map (fun imms -> (n, immr, imms)) (List.init 64 (fun i -> i)))
        (List.init 64 (fun i -> i)))
    (if datasize = 64 then [ 0; 1 ] else [ 0 ])

let encode_bitmask ~datasize v =
  List.find_opt
    (fun (n, immr, imms) ->
      match decode_bitmask ~datasize ~n ~immr ~imms with Some d -> Int64.equal d v | None -> false)
    (bitmask_triples ~datasize)

let bitmask_domain ~datasize =
  C.Finite.exhaustive
    ~name:(Printf.sprintf "aarch64.bitmask%d" datasize)
    ~why:
      "the field is N:immr:imms, so the domain is 8192 triples for 64-bit and 4096 for 32-bit; \
       enumerating the values they decode to is a table scan rather than a sample"
    (List.sort_uniq Int64.compare
       (List.filter_map
          (fun (n, immr, imms) -> decode_bitmask ~datasize ~n ~immr ~imms)
          (bitmask_triples ~datasize)))

let bitmask_codec ~datasize : (int64, fixup_kind) C.t =
  C.iso_fun
    ~name:(Printf.sprintf "bitmask%d" datasize)
    ~encode:(fun v ->
      match encode_bitmask ~datasize v with
      | None -> None
      | Some (n, immr, imms) -> Some (Int64.of_int n, (Int64.of_int immr, Int64.of_int imms)))
    ~decode:(fun (n, (immr, imms)) ->
      decode_bitmask ~datasize ~n:(Int64.to_int n) ~immr:(Int64.to_int immr)
        ~imms:(Int64.to_int imms))
    C.(field ~width:1 "n" ** field ~width:6 "immr" ** field ~width:6 "imms")

(* {1 The codec} *)

let reg_field ~width ~sp name =
  C.iso_fun ~name
    ~encode:(fun (r : Reg.t) -> Some (Int64.of_int r.Reg.num))
    ~decode:(fun v -> Some (Reg.of_num ~width ~sp (Int64.to_int v)))
    (C.field ~width:5 name)

(* The condition, as a declared finite relation rather than a bare four-bit
   field, so [check] decides that all sixteen codes decode and that no two share
   one. *)
let cond_codec =
  C.iso_table ~name:"cond" ~equal:Cond.equal ~show:Cond.name
    ~entries:(List.map (fun c -> (c, Int64.of_int (Cond.code c))) Cond.all)
    (C.field ~width:4 "cond")

(* The unsigned-offset field of a load or store, which is a *fixup* rather than
   a plain field even when the offset is a number. A fixup node encodes the
   value it is handed exactly as a field would; what it adds is a placement, and
   [form_of] keeps only the placements a symbolic operand supplied an expression
   for. So a numeric [ldr w0, [sp, #16]] emits no fixup, a
   [ldr w0, [x16, #:lo12:g]] emits one, and there is a single addressing form
   rather than two that must be kept in step. *)
let ldst_offset ~size name =
  let step = Int64.of_int (access_bytes size) in
  C.iso_fun
    ~name:(Printf.sprintf "%s-scaled%d" name (access_bytes size))
    ~encode:(fun v ->
      if Int64.compare v 0L >= 0 && Int64.equal (Int64.rem v step) 0L then Some (Int64.div v step)
      else None)
    ~decode:(fun f -> Some (Int64.mul f step))
    (C.fixup ~width:12 ~kind:(Ldst_lo12 size) name)

let ldst_alt ~size ~load =
  let rt_width = access_reg_width size in
  C.iso_fun
    ~name:(Printf.sprintf "%s%d" (if load then "ldr" else "str") (access_bytes size * 8))
    ~encode:(function
      | Lowered.Ldst_uoff { size = sz; load = l; rt; rn; offset } when sz = size && l = load ->
          let v = match offset with Disp.Const v -> v | Disp.Sym _ -> 0L in
          Some ((), ((), ((), (v, (rn, rt)))))
      | _ -> None)
    ~decode:(fun ((), ((), ((), (v, (rn, rt))))) ->
      Some (Lowered.Ldst_uoff { size; load; rt; rn; offset = Disp.Const v }))
    C.(
      const ~width:2 (Int64.of_int (access_code size))
      ** const ~width:6 0b111001L
      ** const ~width:2 (if load then 1L else 0L)
      ** ldst_offset ~size "offset" ** reg_field ~width:64 ~sp:true "rn"
      ** reg_field ~width:rt_width ~sp:false "rt")

(* ADD/SUB (immediate) with the [op] bit set, once per value of [S]. The two
   differ in exactly two places - the constant bit and whether [rd = 31] reads
   as SP or as the zero register - so they share a generator rather than being
   written twice and drifting in the twelve fields they agree on. *)
let sub_imm_alt ~s =
  C.iso_fun
    ~name:(if s then "subs-imm" else "sub-imm")
    ~encode:(function
      | Lowered.Sub_imm { s = s'; rd; rn; imm; shift12 } when s' = s ->
          if Int64.compare imm 0L < 0 || Int64.compare imm 4096L >= 0 then None
          else
            Some
              ( (if rd.Reg.width = 64 then 1L else 0L),
                ((), ((if shift12 then 1L else 0L), (imm, (rn, rd)))) )
      | _ -> None)
    ~decode:(fun (sf, ((), (sh, (imm, (rn, rd))))) ->
      let width = if Int64.equal sf 1L then 64 else 32 in
      Some
        (Lowered.Sub_imm
           {
             s;
             rd = { rd with Reg.width };
             rn = { rn with Reg.width };
             imm;
             shift12 = Int64.equal sh 1L;
           }))
    C.(
      field ~width:1 "sf"
      ** const ~width:8 (if s then 0b11100010L else 0b10100010L)
      ** field ~width:1 "sh" ** field ~width:12 "imm12" ** reg_field ~width:64 ~sp:true "rn"
      ** reg_field ~width:64 ~sp:(not s) "rd")

let branch_value = function
  | Asm_core.Lowered_ast.Symbolic _ -> 0L
  | Asm_core.Lowered_ast.Resolved { value; _ } -> value

(* The fixup node is unsigned; the sign belongs to the whole value and is
   applied once, here, where the field width is known. *)
let sign_extend ~width v = Int64.shift_right (Int64.shift_left v (64 - width)) (64 - width)

let codec : (Lowered.t, fixup_kind) C.t =
  C.choice ~name:"aarch64"
    (List.concat
       [
         [
           (* Every A64 form has a distinct fixed-bit prefix, so unlike A32 and
              x86 there is no overlap here for priority to resolve and [check]
              reports none. The priorities are still declared, because "no two
              forms overlap today" is a fact about these forms and not a
              property of A64. *)
           C.alt ~label:"add-imm" ~priority:0
             (C.iso_fun ~name:"add-imm"
                ~encode:(function
                  | Lowered.Add_imm { rd; rn; imm; shift12 } ->
                      if Int64.compare imm 0L < 0 || Int64.compare imm 4096L >= 0 then None
                      else
                        Some
                          ( (if rd.Reg.width = 64 then 1L else 0L),
                            ((), ((if shift12 then 1L else 0L), (imm, (rn, rd)))) )
                  | _ -> None)
                ~decode:(fun (sf, ((), (sh, (imm, (rn, rd))))) ->
                  let width = if Int64.equal sf 1L then 64 else 32 in
                  Some
                    (Lowered.Add_imm
                       {
                         rd = { rd with Reg.width };
                         rn = { rn with Reg.width };
                         imm;
                         shift12 = Int64.equal sh 1L;
                       }))
                C.(
                  field ~width:1 "sf" ** const ~width:8 0b00100010L ** field ~width:1 "sh"
                  ** field ~width:12 "imm12" ** reg_field ~width:64 ~sp:true "rn"
                  ** reg_field ~width:64 ~sp:true "rd"));
           (* [sub], A64's ADD/SUB(immediate) class with the [op] bit set: the
              same shape as [add-imm] with one constant bit flipped, not a
              separate instruction family - which is why A64 encodes it
              separately from [add-imm] rather than folding it into a negated
              addend. *)
           C.alt ~label:"sub-imm" ~priority:1 (sub_imm_alt ~s:false);
           (* [subs], which is [sub] with the flag-setting bit and therefore a
              different meaning for [rd = 31]: the zero register rather than SP.
              A separate alternative rather than a field, because the [S] bit
              would have to be read before [rd] is decoded and the sequential
              interpreter cannot look back. This is the form [cmp] prints as. *)
           C.alt ~label:"subs-imm" ~priority:23 (sub_imm_alt ~s:true);
           (* ADD/SUB (shifted register), the form a comparison is. The shift is
              part of the operand rather than a separate instruction, which is
              why [add w0, w0, w1, lsl #1] is one word. *)
           C.alt ~label:"addsub-shift" ~priority:2
             (C.iso_fun ~name:"addsub-shift"
                ~encode:(function
                  | Lowered.Addsub_shift { sub; s; rd; rn; rm; shift; amount } ->
                      if amount < 0 || amount >= rd.Reg.width || shift < 0 || shift > 3 then None
                      else
                        Some
                          ( (if rd.Reg.width = 64 then 1L else 0L),
                            ( (if sub then 1L else 0L),
                              ( (if s then 1L else 0L),
                                ( (),
                                  (Int64.of_int shift, ((), (rm, (Int64.of_int amount, (rn, rd)))))
                                ) ) ) )
                  | _ -> None)
                ~decode:(fun (sf, (op, (s, ((), (shift, ((), (rm, (amount, (rn, rd))))))))) ->
                  let width = if Int64.equal sf 1L then 64 else 32 in
                  Some
                    (Lowered.Addsub_shift
                       {
                         sub = Int64.equal op 1L;
                         s = Int64.equal s 1L;
                         rd = { rd with Reg.width };
                         rn = { rn with Reg.width };
                         rm = { rm with Reg.width };
                         shift = Int64.to_int shift;
                         amount = Int64.to_int amount;
                       }))
                C.(
                  field ~width:1 "sf" ** field ~width:1 "op" ** field ~width:1 "s"
                  ** const ~width:5 0b01011L ** field ~width:2 "shift" ** const ~width:1 0L
                  ** reg_field ~width:64 ~sp:false "rm"
                  ** field ~width:6 "imm6"
                  ** reg_field ~width:64 ~sp:false "rn"
                  ** reg_field ~width:64 ~sp:false "rd"));
           C.alt ~label:"logical-imm-32" ~priority:3
             (C.iso_fun ~name:"logical-imm-32"
                ~encode:(function
                  | Lowered.Logical_imm { opc; rd; rn; imm } when rd.Reg.width = 32 ->
                      Some ((), (Int64.of_int opc, ((), (imm, (rn, rd)))))
                  | _ -> None)
                ~decode:(fun ((), (opc, ((), (imm, (rn, rd))))) ->
                  Some
                    (Lowered.Logical_imm
                       {
                         opc = Int64.to_int opc;
                         rd = { rd with Reg.width = 32 };
                         rn = { rn with Reg.width = 32 };
                         imm;
                       }))
                C.(
                  const ~width:1 0L ** field ~width:2 "opc" ** const ~width:6 0b100100L
                  ** bitmask_codec ~datasize:32
                  ** reg_field ~width:32 ~sp:false "rn"
                  ** reg_field ~width:32 ~sp:true "rd"));
           C.alt ~label:"logical-imm-64" ~priority:4
             (C.iso_fun ~name:"logical-imm-64"
                ~encode:(function
                  | Lowered.Logical_imm { opc; rd; rn; imm } when rd.Reg.width = 64 ->
                      Some ((), (Int64.of_int opc, ((), (imm, (rn, rd)))))
                  | _ -> None)
                ~decode:(fun ((), (opc, ((), (imm, (rn, rd))))) ->
                  Some (Lowered.Logical_imm { opc = Int64.to_int opc; rd; rn; imm }))
                C.(
                  const ~width:1 1L ** field ~width:2 "opc" ** const ~width:6 0b100100L
                  ** bitmask_codec ~datasize:64
                  ** reg_field ~width:64 ~sp:false "rn"
                  ** reg_field ~width:64 ~sp:true "rd"));
           C.alt ~label:"madd" ~priority:5
             (C.iso_fun ~name:"madd"
                ~encode:(function
                  | Lowered.Madd { rd; rn; rm; ra } ->
                      Some ((if rd.Reg.width = 64 then 1L else 0L), ((), (rm, ((), (ra, (rn, rd))))))
                  | _ -> None)
                ~decode:(fun (sf, ((), (rm, ((), (ra, (rn, rd)))))) ->
                  let width = if Int64.equal sf 1L then 64 else 32 in
                  Some
                    (Lowered.Madd
                       {
                         rd = { rd with Reg.width };
                         rn = { rn with Reg.width };
                         rm = { rm with Reg.width };
                         ra = { ra with Reg.width };
                       }))
                C.(
                  field ~width:1 "sf" ** const ~width:10 0b0011011000L
                  ** reg_field ~width:64 ~sp:false "rm"
                  ** const ~width:1 0L
                  ** reg_field ~width:64 ~sp:false "ra"
                  ** reg_field ~width:64 ~sp:false "rn"
                  ** reg_field ~width:64 ~sp:false "rd"));
           C.alt ~label:"csel" ~priority:6
             (C.iso_fun ~name:"csel"
                ~encode:(function
                  | Lowered.Csel { cond; rd; rn; rm } ->
                      Some
                        ((if rd.Reg.width = 64 then 1L else 0L), ((), (rm, (cond, ((), (rn, rd))))))
                  | _ -> None)
                ~decode:(fun (sf, ((), (rm, (cond, ((), (rn, rd)))))) ->
                  let width = if Int64.equal sf 1L then 64 else 32 in
                  Some
                    (Lowered.Csel
                       {
                         cond;
                         rd = { rd with Reg.width };
                         rn = { rn with Reg.width };
                         rm = { rm with Reg.width };
                       }))
                C.(
                  field ~width:1 "sf" ** const ~width:10 0b0011010100L
                  ** reg_field ~width:64 ~sp:false "rm"
                  ** cond_codec ** const ~width:2 0L
                  ** reg_field ~width:64 ~sp:false "rn"
                  ** reg_field ~width:64 ~sp:false "rd"));
           C.alt ~label:"stp-pre" ~priority:7
             (C.iso_fun ~name:"stp-pre"
                ~encode:(function
                  | Lowered.Stp_pre { rt; rt2; rn; offset } -> Some ((), (offset, (rt2, (rn, rt))))
                  | _ -> None)
                ~decode:(fun ((), (offset, (rt2, (rn, rt)))) ->
                  Some (Lowered.Stp_pre { rt; rt2; rn; offset }))
                C.(
                  const ~width:10 0b1010100110L
                  ** scaled ~shift:3 ~width:7 ~signedness:C.Signed "imm7"
                  ** reg_field ~width:64 ~sp:false "rt2"
                  ** reg_field ~width:64 ~sp:true "rn"
                  ** reg_field ~width:64 ~sp:false "rt"));
           C.alt ~label:"movz" ~priority:8
             (C.iso_fun ~name:"movz"
                ~encode:(function
                  | Lowered.Movz { rd; imm16; hw } ->
                      if Int64.compare imm16 0L < 0 || Int64.compare imm16 65536L >= 0 then None
                      else
                        Some
                          ( (if rd.Reg.width = 64 then 1L else 0L),
                            ((), (Int64.of_int hw, (imm16, rd))) )
                  | _ -> None)
                ~decode:(fun (sf, ((), (hw, (imm16, rd)))) ->
                  let width = if Int64.equal sf 1L then 64 else 32 in
                  Some (Lowered.Movz { rd = { rd with Reg.width }; imm16; hw = Int64.to_int hw }))
                C.(
                  field ~width:1 "sf" ** const ~width:8 0b10100101L ** field ~width:2 "hw"
                  ** field ~width:16 "imm16"
                  ** reg_field ~width:64 ~sp:false "rd"));
         ];
         (* The eight unsigned-offset loads and stores, generated rather than
            listed: the size decides the field scale, the register width and the
            relocation kind together, so writing them out would be writing the
            same relation four times and inviting one copy to disagree. *)
         List.concat_map
           (fun size ->
             [
               C.alt
                 ~label:(Printf.sprintf "ldr%d" (access_bytes size * 8))
                 ~priority:(9 + (2 * access_code size))
                 (ldst_alt ~size ~load:true);
               C.alt
                 ~label:(Printf.sprintf "str%d" (access_bytes size * 8))
                 ~priority:(10 + (2 * access_code size))
                 (ldst_alt ~size ~load:false);
             ])
           access_all;
         [
           C.alt ~label:"ret" ~priority:17
             (C.iso_fun ~name:"ret"
                ~encode:(function Lowered.Ret { rn } -> Some ((), (rn, ())) | _ -> None)
                ~decode:(fun ((), (rn, ())) -> Some (Lowered.Ret { rn }))
                C.(
                  const ~width:22 0b1101011001011111000000L
                  ** reg_field ~width:64 ~sp:false "rn"
                  ** const ~width:5 0L));
           C.alt ~label:"udf" ~priority:18
             (C.iso_fun ~name:"udf"
                ~encode:(function Lowered.Udf { imm16 } -> Some ((), imm16) | _ -> None)
                ~decode:(fun ((), imm16) -> Some (Lowered.Udf { imm16 }))
                C.(const ~width:16 0x0000L ** field ~width:16 ~signedness:C.Unsigned "imm16"));
           (* [adrp] - R_AARCH64_ADR_PREL_PG_HI21. The 21-bit page number is
              split across two non-adjacent fields, [immlo] at bits 30:29 and
              [immhi] at 23:5, so it is two [fixup] nodes of one value tagged
              with the bit each starts at; [encode] merges them into a single
              placement with two slices. *)
           C.alt ~label:"adrp" ~priority:19
             (C.iso_fun ~name:"adrp"
                ~encode:(function
                  | Lowered.Adrp { rd; page } ->
                      let v = branch_value page in
                      Some
                        ( (),
                          ( Int64.logand v 3L,
                            ((), (Int64.logand (Int64.shift_right v 2) 0x7FFFFL, rd)) ) )
                  | _ -> None)
                ~decode:(fun ((), (immlo, ((), (immhi, rd)))) ->
                  let v = sign_extend ~width:21 (Int64.logor (Int64.shift_left immhi 2) immlo) in
                  Some
                    (Lowered.Adrp
                       { rd; page = Asm_core.Lowered_ast.Resolved { value = v; rung = "adrp" } }))
                C.(
                  const ~width:1 1L
                  ** fixup ~width:2 ~value_lsb:0 ~kind:Adrp_page "page"
                  ** const ~width:5 0b10000L
                  ** fixup ~width:19 ~value_lsb:2 ~kind:Adrp_page "page"
                  ** reg_field ~width:64 ~sp:false "rd"));
           (* [b] and [b.<cc>]: the same measurement as [bl] over two different
              field widths, which is why the conditional form has a relocation
              kind of its own. Neither needs a relaxation ladder - 26 word-counted
              bits reach 128 MiB and 19 reach 1 MiB, both far past any fixture -
              so A64 produces no [Relax] node anywhere. *)
           C.alt ~label:"b" ~priority:20
             (C.iso_fun ~name:"b"
                ~encode:(function
                  | Lowered.B { target } -> Some ((), branch_value target) | _ -> None)
                ~decode:(fun ((), d) ->
                  Some
                    (Lowered.B
                       {
                         target =
                           Asm_core.Lowered_ast.Resolved
                             { value = sign_extend ~width:26 d; rung = "b" };
                       }))
                C.(const ~width:6 0b000101L ** fixup ~width:26 ~kind:Pcrel_b26 "target"));
           C.alt ~label:"b-cond" ~priority:21
             (C.iso_fun ~name:"b-cond"
                ~encode:(function
                  | Lowered.Bcond { cond; target } -> Some ((), (branch_value target, ((), cond)))
                  | _ -> None)
                ~decode:(fun ((), (d, ((), cond))) ->
                  Some
                    (Lowered.Bcond
                       {
                         cond;
                         target =
                           Asm_core.Lowered_ast.Resolved
                             { value = sign_extend ~width:19 d; rung = "b-cond" };
                       }))
                C.(
                  const ~width:8 0b01010100L
                  ** fixup ~width:19 ~kind:Pcrel_b19 "target"
                  ** const ~width:1 0L ** cond_codec));
           (* [bl] - R_AARCH64_CALL26. The field is a *word count*, so the fixup
              value is the byte displacement divided by four; [evaluate_fixup]
              does that division and the range check, which is why the node here
              is a plain 26-bit fixup and not a scaled one. No little-endian
              wrapper: a fixed-width target authors one word and reverses it
              whole, so the container and the codec agree on bit positions. *)
           C.alt ~label:"bl" ~priority:22
             (C.iso_fun ~name:"bl"
                ~encode:(function
                  | Lowered.Bl { target } -> Some ((), branch_value target) | _ -> None)
                ~decode:(fun ((), d) ->
                  Some
                    (Lowered.Bl
                       {
                         target =
                           Asm_core.Lowered_ast.Resolved
                             { value = sign_extend ~width:26 d; rung = "bl" };
                       }))
                C.(const ~width:6 0b100101L ** fixup ~width:26 ~kind:Pcrel_call26 "target"));
         ];
       ])

(* {1 Lexical profile}

   [//] introduces a comment, so the profile's longest-first matching matters:
   a dialect that also used [/] would otherwise take the first character and
   leave the second as a divide. *)
let lexical_profile =
  Asm_core.Lexical_profile.make ~name:"aarch64" ~comment_introducers:[ "//" ]
    ~statement_separator:';' ~immediate_sigil:'#' ~modifier_syntax:[ "lo12" ] ()

let name = "aarch64"
let triple = "aarch64-linux-gnu"

let diag ?origin code message =
  Diagnostic.error ~code ~message
    ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:"aarch64" ())
    ()

(* {1 Operand parsing}

   Two things the common parser cannot do, and the reason §5.1's one-operand-
   at-a-time signature was replaced. First, [stp x15, x30, [sp, #-16]!] has a
   comma inside the bracketed operand, so the slices arrive split and are
   rejoined here by bracket depth. Second, the [!] belongs to the whole memory
   operand rather than to the token before it. *)

let regroup (slices : Asm_core.Token.slice list) =
  let depth_of slice =
    List.fold_left
      (fun d t ->
        match Asm_core.Token.kind t with
        | Asm_core.Token.Lbracket | Asm_core.Token.Lbrace -> d + 1
        | Asm_core.Token.Rbracket | Asm_core.Token.Rbrace -> d - 1
        | _ -> d)
      0 slice
  in
  (* The separator itself is gone: the grammar consumed every top-level comma
     as a slice boundary, so rejoining is concatenation and the rejoined operand
     reads [[ sp # 0 ]]. Re-inserting a comma token would be inventing source
     text, and the spans would then not be the lexer's. *)
  let rec go acc pending depth = function
    | [] -> if pending = [] then Ok (List.rev acc) else Error "unbalanced brackets in operand"
    | s :: rest ->
        let d = depth + depth_of s in
        let pending = pending @ s in
        if d = 0 then go (pending :: acc) [] 0 rest
        else if d < 0 then Error "unbalanced brackets in operand"
        else go acc pending d rest
  in
  go [] [] 0 slices

let slice_origin (s : Asm_core.Token.slice) =
  match Asm_core.Token.slice_span s with
  | Some sp -> Origin.text sp
  | None -> Origin.synthesized ~pass:"aarch64" ()

let parse_one (slice : Asm_core.Token.slice) =
  let origin = slice_origin slice in
  let bad msg = Error (diag ~origin "aarch64.operand" msg) in
  let open Asm_core in
  let mem ~base ~offset ~writeback =
    match Reg.find base with
    | Some r -> Ok (Operand.Mem { Mem.base = r; offset; writeback; pre = true })
    | None -> bad ("unknown register " ^ base)
  in
  let const_mem ~base ~offset ~writeback = mem ~base ~offset:(Disp.Const offset) ~writeback in
  let int64_of v = match Bigint.to_int64_opt v with Some x -> Some x | None -> None in
  match List.map Token.kind slice with
  | [ Token.Ident n ] -> (
      (* A bare identifier is a register if the table knows it and a symbol
         otherwise. There is no sigil to tell them apart on this dialect, so
         the table is the only thing that can. *)
      match Reg.find n with
      | Some r -> Ok (Operand.Reg r)
      | None -> Ok (Operand.Sym (Expr.Symbol n)))
  | [ Token.Immediate_sigil; Token.Int v ] -> Ok (Operand.Imm v)
  | [ Token.Immediate_sigil; Token.Minus; Token.Int v ] -> Ok (Operand.Imm (Bigint.neg v))
  | [
   Token.Ident (("lsl" | "lsr" | "asr" | "ror" | "uxtw" | "sxtw") as k);
   Token.Immediate_sigil;
   Token.Int v;
  ] -> (
      match Bigint.to_int_opt v with
      | Some n -> Ok (Operand.Shift { Shift.kind = k; amount = n })
      | None -> bad "shift amount does not fit")
  | [ Token.Lbracket; Token.Ident b; Token.Rbracket ] ->
      const_mem ~base:b ~offset:0L ~writeback:false
  | [ Token.Lbracket; Token.Ident b; Token.Rbracket; Token.Bang ] ->
      const_mem ~base:b ~offset:0L ~writeback:true
  | [ Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket ] -> (
      match int64_of v with
      | Some o -> const_mem ~base:b ~offset:o ~writeback:false
      | None -> bad "offset does not fit")
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket; Token.Bang;
  ] -> (
      match int64_of v with
      | Some o -> const_mem ~base:b ~offset:o ~writeback:true
      | None -> bad "offset does not fit")
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Minus; Token.Int v; Token.Rbracket;
  ] -> (
      match int64_of v with
      | Some o -> const_mem ~base:b ~offset:(Int64.neg o) ~writeback:false
      | None -> bad "offset does not fit")
  | [
   Token.Lbracket;
   Token.Ident b;
   Token.Immediate_sigil;
   Token.Minus;
   Token.Int v;
   Token.Rbracket;
   Token.Bang;
  ] -> (
      match int64_of v with
      | Some o -> const_mem ~base:b ~offset:(Int64.neg o) ~writeback:true
      | None -> bad "offset does not fit")
  (* [[x16, #:lo12:g]] - a memory operand whose offset is an expression rather
     than a number (§D2). The bracket and sigil are stripped here and the middle
     handed to the common expression parser, which is what knows [:lo12:g+4]
     means the same thing in an operand as in a directive argument. *)
  | Token.Lbracket :: Token.Ident b :: Token.Immediate_sigil :: Token.Modifier _ :: _
    when match List.rev (List.map Token.kind slice) with Token.Rbracket :: _ -> true | _ -> false
    -> (
      let inner =
        match slice with
        | _ :: _ :: _ :: rest -> List.filteri (fun i _ -> i < List.length rest - 1) rest
        | _ -> []
      in
      match Asm_core.Parse_lines.parse_expression inner with
      | Ok e -> mem ~base:b ~offset:(Disp.Sym e) ~writeback:false
      | Error _ -> bad ("cannot parse operand " ^ Asm_core.Token.slice_text slice))
  | _ -> (
      (* A branch or call target: not a register, an immediate or an address, but
         still an expression. The common parser decides what it means. *)
      match Asm_core.Parse_lines.parse_expression slice with
      | Ok e -> Ok (Operand.Sym e)
      | Error _ -> bad ("cannot parse operand " ^ Asm_core.Token.slice_text slice))

let parse_operands ~mnemonic slices =
  ignore mnemonic;
  match regroup slices with
  | Error m -> Error (diag "aarch64.operand" m)
  | Ok groups ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | s :: rest -> ( match parse_one s with Ok o -> go (o :: acc) rest | Error e -> Error e)
      in
      go [] groups

let make_surface_instruction ~mnemonic ~origin ops = Ok { Surface.mnemonic; ops; origin }

(* {1 Simplify}

   Where the A64 aliases are resolved (§ M1.2), and where a trailing [lsl #0]
   stops being a separate operand. Both are surface facts: the machine has one
   [add] and one [movz], and a normalized AST that still carried the alias would
   make every consumer resolve it again. *)

let simplify_instruction ~features s =
  ignore features;
  let bad msg = Error (diag ~origin:s.Surface.origin "aarch64.simplify" msg) in
  match Opcode.of_mnemonic s.Surface.mnemonic with
  | None -> bad (Printf.sprintf "unknown instruction %s" s.Surface.mnemonic)
  | Some op -> (
      (* A [lsl #0] modifier is dropped; a nonzero one is kept, because [movz]
         encodes it in [hw] and dropping it would silently change the value. *)
      match List.rev s.Surface.ops with
      | Operand.Shift { Shift.kind = "lsl"; amount = 0 } :: rest ->
          Ok { Instruction.op; ops = List.rev rest }
      | _ -> Ok { Instruction.op; ops = s.Surface.ops })

(* {1 Lower} *)

let lower_instruction state i =
  ignore state;
  let bad msg = Error (diag "aarch64.lower" msg) in
  let imm_of v =
    match Bigint.to_int64_opt v with
    | Some x -> Ok x
    | None -> Error (diag "aarch64.lower" "immediate does not fit 64 bits")
  in
  match (i.Instruction.op, i.Instruction.ops) with
  (* [mov xD, sp] and [mov sp, xS] are [add ..., #0]; [mov] between two general
     registers is [orr] with xzr, which no M1 fixture uses and which is
     therefore rejected rather than guessed. *)
  | Opcode.Mov, [ Operand.Reg rd; Operand.Reg rn ] ->
      if rd.Reg.is_sp || rn.Reg.is_sp then
        Ok [ Lowered.Add_imm { rd; rn; imm = 0L; shift12 = false } ]
      else bad "mov between two general registers lowers to orr, which is not in M1 scope"
  | Opcode.Add, [ Operand.Reg rd; Operand.Reg rn; Operand.Imm v ] -> (
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if Int64.compare imm 0L < 0 || Int64.compare imm 4096L >= 0 then
            bad "immediate does not fit the unshifted 12-bit field; lsl #12 is M2"
          else Ok [ Lowered.Add_imm { rd; rn; imm; shift12 = false } ])
  | Opcode.Movz, [ Operand.Reg rd; Operand.Imm v ] -> (
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if Int64.compare imm 0L < 0 || Int64.compare imm 65536L >= 0 then
            bad "movz immediate does not fit 16 bits"
          else Ok [ Lowered.Movz { rd; imm16 = imm; hw = 0 } ])
  | Opcode.Movz, [ Operand.Reg rd; Operand.Imm v; Operand.Shift { Shift.kind = "lsl"; amount } ]
    -> (
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if amount mod 16 <> 0 || amount / 16 > 3 then bad "movz shift must be 0, 16, 32 or 48"
          else if Int64.compare imm 0L < 0 || Int64.compare imm 65536L >= 0 then
            bad "movz immediate does not fit 16 bits"
          else Ok [ Lowered.Movz { rd; imm16 = imm; hw = amount / 16 } ])
  | Opcode.Stp, [ Operand.Reg rt; Operand.Reg rt2; Operand.Mem m ] -> (
      match m.Mem.offset with
      | Disp.Sym _ -> bad "stp takes a numeric offset"
      | Disp.Const off ->
          if not m.Mem.writeback then bad "M1 supports only the pre-index writeback form of stp"
          else if not (Int64.equal (Int64.rem off 8L) 0L) then
            bad "stp offset must be a multiple of eight"
          else Ok [ Lowered.Stp_pre { rt; rt2; rn = m.Mem.base; offset = off } ])
  (* One lowering for all eight loads and stores. The mnemonic fixes the access
     size for the sub-word forms and the register width fixes it for the rest,
     so [ldr w0] and [ldr x0] are the same rule read twice rather than two. *)
  | ( ((Opcode.Ldr | Opcode.Ldrb | Opcode.Ldrh | Opcode.Str | Opcode.Strb | Opcode.Strh) as op),
      [ Operand.Reg rt; Operand.Mem m ] ) -> (
      let load = match op with Opcode.Ldr | Opcode.Ldrb | Opcode.Ldrh -> true | _ -> false in
      let size =
        match op with
        | Opcode.Ldrb | Opcode.Strb -> B
        | Opcode.Ldrh | Opcode.Strh -> H
        | _ -> if rt.Reg.width = 64 then X else W
      in
      let expected = access_reg_width size in
      if rt.Reg.width <> expected then
        bad (Printf.sprintf "%s needs a %d-bit register" (Opcode.name op) expected)
      else if m.Mem.writeback then bad "writeback loads and stores are not in M2 scope"
      else
        match m.Mem.offset with
        | Disp.Sym e -> (
            (* §B7: the modifier chose the form, so what is stored is the
               expression underneath. A [:lo12:] left in place would reach
               [bind_image] as a residual. *)
            let inner =
              match e with
              | Asm_core.Expr.Modifier ("lo12", inner) -> Ok inner
              | Asm_core.Expr.Modifier (m, _) ->
                  Error (Printf.sprintf "a memory offset takes #:lo12:, not #:%s:" m)
              | _ -> Error "a symbolic memory offset needs a #:lo12: modifier"
            in
            match inner with
            | Error msg -> bad msg
            | Ok value ->
                Ok
                  [ Lowered.Ldst_uoff { size; load; rt; rn = m.Mem.base; offset = Disp.Sym value } ]
            )
        | Disp.Const off ->
            let scale = Int64.of_int (access_bytes size) in
            if Int64.compare off 0L < 0 then
              bad "M2 supports only the unsigned-offset form of a load or store"
            else if not (Int64.equal (Int64.rem off scale) 0L) then
              bad (Printf.sprintf "offset must be a multiple of %Ld for this access width" scale)
            else if Int64.compare (Int64.div off scale) 4096L >= 0 then
              bad "offset does not fit the scaled 12-bit field"
            else
              Ok [ Lowered.Ldst_uoff { size; load; rt; rn = m.Mem.base; offset = Disp.Const off } ])
  | Opcode.Ret, [ Operand.Reg rn ] -> Ok [ Lowered.Ret { rn } ]
  | Opcode.Ret, [] -> Ok [ Lowered.Ret { rn = Reg.x30 } ]
  | Opcode.Sub, [ Operand.Reg rd; Operand.Reg rn; Operand.Imm v ] -> (
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if Int64.compare imm 0L < 0 || Int64.compare imm 4096L >= 0 then
            bad
              "immediate does not fit the unshifted 12-bit field; use the four-operand lsl #12 form"
          else Ok [ Lowered.Sub_imm { s = false; rd; rn; imm; shift12 = false } ])
  | ( Opcode.Sub,
      [
        Operand.Reg rd;
        Operand.Reg rn;
        Operand.Imm v;
        Operand.Shift { Shift.kind = "lsl"; amount = 12 };
      ] ) -> (
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if Int64.compare imm 0L < 0 || Int64.compare imm 4096L >= 0 then
            bad "immediate does not fit the 12-bit field"
          else Ok [ Lowered.Sub_imm { s = false; rd; rn; imm; shift12 = true } ])
  (* The register forms of ADD/SUB, and the comparison that is one of them.
     A shift is a fourth operand at the surface and part of the third here. *)
  | ( ((Opcode.Add | Opcode.Sub) as op),
      (Operand.Reg rd :: Operand.Reg rn :: Operand.Reg rm :: rest as ops) ) -> (
      ignore ops;
      match rest with
      | [] ->
          Ok
            [
              Lowered.Addsub_shift
                { sub = op = Opcode.Sub; s = false; rd; rn; rm; shift = 0; amount = 0 };
            ]
      | [ Operand.Shift { Shift.kind; amount } ] -> (
          match shift_of_name kind with
          | None -> bad (Printf.sprintf "%s is not a shift of a register operand" kind)
          | Some shift ->
              if amount < 0 || amount >= rd.Reg.width then
                bad "shift amount does not fit the register width"
              else
                Ok
                  [
                    Lowered.Addsub_shift
                      { sub = op = Opcode.Sub; s = false; rd; rn; rm; shift; amount };
                  ])
      | _ -> bad "too many operands")
  | Opcode.Cmp, [ Operand.Reg rn; Operand.Imm v ] -> (
      (* The immediate half of the same aliasing: a [subs] into the zero
         register. *)
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if Int64.compare imm 0L < 0 || Int64.compare imm 4096L >= 0 then
            bad "immediate does not fit the unshifted 12-bit field"
          else
            Ok
              [
                Lowered.Sub_imm
                  {
                    s = true;
                    rd = { Reg.num = 31; width = rn.Reg.width; is_sp = false };
                    rn;
                    imm;
                    shift12 = false;
                  };
              ])
  | Opcode.Cmp, [ Operand.Reg rn; Operand.Reg rm ] ->
      (* There is no compare instruction: a comparison is a subtraction that
         sets the flags and discards its result into the zero register, and
         reading it off [Opcode.Cmp] here is what keeps one encoding. *)
      Ok
        [
          Lowered.Addsub_shift
            {
              sub = true;
              s = true;
              rd = { Reg.num = 31; width = rn.Reg.width; is_sp = false };
              rn;
              rm;
              shift = 0;
              amount = 0;
            };
        ]
  | ((Opcode.And | Opcode.Orr) as op), [ Operand.Reg rd; Operand.Reg rn; Operand.Imm v ] -> (
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if encode_bitmask ~datasize:rd.Reg.width imm = None then
            bad (Printf.sprintf "#%Ld is not an A64 bitmask immediate" imm)
          else Ok [ Lowered.Logical_imm { opc = (if op = Opcode.And then 0 else 1); rd; rn; imm } ])
  | Opcode.Madd, [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm; Operand.Reg ra ] ->
      Ok [ Lowered.Madd { rd; rn; rm; ra } ]
  | Opcode.Mul, [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm ] ->
      Ok [ Lowered.Madd { rd; rn; rm; ra = { Reg.num = 31; width = rd.Reg.width; is_sp = false } } ]
  | Opcode.Csel, [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm; Operand.Sym cond ] -> (
      (* The condition is spelled as a bare name, so it arrives from the common
         parser as a symbol - there is nothing else it could be until this
         target says otherwise. *)
      match cond with
      | Asm_core.Expr.Symbol s -> (
          match Cond.of_name s with
          | Some c -> Ok [ Lowered.Csel { cond = c; rd; rn; rm } ]
          | None -> bad (Printf.sprintf "unknown condition %s" s))
      | _ -> bad "csel needs a condition name")
  | Opcode.Adrp, [ Operand.Reg rd; Operand.Sym e ] ->
      Ok [ Lowered.Adrp { rd; page = Asm_core.Lowered_ast.Symbolic { value = e; rung = None } } ]
  | Opcode.B, [ Operand.Sym e ] ->
      Ok [ Lowered.B { target = Asm_core.Lowered_ast.Symbolic { value = e; rung = None } } ]
  | Opcode.Bcond c, [ Operand.Sym e ] ->
      Ok
        [
          Lowered.Bcond
            { cond = c; target = Asm_core.Lowered_ast.Symbolic { value = e; rung = None } };
        ]
  | Opcode.Udf, [ Operand.Imm v ] -> (
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if Int64.compare imm 0L < 0 || Int64.compare imm 65536L >= 0 then
            bad "udf immediate does not fit 16 bits"
          else Ok [ Lowered.Udf { imm16 = imm } ])
  | Opcode.Bl, [ Operand.Sym e ] ->
      Ok [ Lowered.Bl { target = Asm_core.Lowered_ast.Symbolic { value = e; rung = None } } ]
  | _ -> bad (Printf.sprintf "no %s form takes these operands" (Opcode.name i.Instruction.op))

(* {1 Encode and decode} *)

let word_to_memory s = if String.length s <> 4 then s else String.init 4 (fun i -> s.[3 - i])

(* {2 Placements into fixups}

   The codec authors one 32-bit word most-significant bit first and
   [word_to_memory] then reverses it, so a placement's bit offset is *not* a
   memory position. Converting is this target's job precisely because only it
   knows that reversal happened.

   The container is the whole four-byte word, read little-endian - which
   reconstructs the word value the codec described - so a codec bit at position
   [p] with width [w] lands at container bit [32 - p - w]. adrp's split
   immediate falls out of this: immlo and immhi are two slices of one value at
   two positions, and nothing has to special-case them.

   [pc_bias] is 0: A64 measures a branch from the instruction itself. *)
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
    pc_bias = 0;
    range =
      (match kind with
      | Abs32 -> Asm_core.Lowered_ast.Bitpattern 32
      | Abs64 -> Asm_core.Lowered_ast.Bitpattern 64
      | Pcrel_b26 | Pcrel_call26 -> Asm_core.Lowered_ast.Signed 26
      | Pcrel_b19 -> Asm_core.Lowered_ast.Signed 19
      | Adrp_page -> Asm_core.Lowered_ast.Signed 21
      | Add_lo12 -> Asm_core.Lowered_ast.Unsigned 12
      | Ldst_lo12 _ -> Asm_core.Lowered_ast.Unsigned 12);
    value = Asm_core.Expr.Const (Bigint.of_int 0);
    origin = Origin.synthesized ~pass:"aarch64.encode" ();
  }

(* The expression lives in the lowered value, because Codec sits below asm_core
   and cannot mention Expr; pairing is by placement name. A placement with no
   expression belongs to a Resolved operand - its bits are already the real
   displacement - so it yields no linker fixup. *)
let expr_of_lowered : Lowered.t -> (string * Asm_core.Expr.t) list = function
  | Lowered.Bl { target = Asm_core.Lowered_ast.Symbolic { value; _ } }
  | Lowered.B { target = Asm_core.Lowered_ast.Symbolic { value; _ } }
  | Lowered.Bcond { target = Asm_core.Lowered_ast.Symbolic { value; _ }; _ } ->
      [ ("target", value) ]
  | Lowered.Adrp { page = Asm_core.Lowered_ast.Symbolic { value; _ }; _ } -> [ ("page", value) ]
  (* The names are the codec placements', not descriptions: [form_of] pairs the
     two by name, and a mismatch does not fail - it silently produces an
     encoding with no fixup, which then binds to the placeholder. *)
  | Lowered.Ldst_uoff { offset = Disp.Sym value; _ } -> [ ("offset", value) ]
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

(* A64 is fixed-width, so there is never a ladder: every form is `Fixed. The
   dispatch still reads the operand rather than the codec, so the rule is the
   same one x86 follows. *)
let encode l =
  match C.encode codec l with
  (* Same rule as the other two targets. A64 declares no ladder either, so this
     is always [aarch64.encode] today. *)
  | Error (e : C.error) ->
      Error (diag (Option.value e.C.code ~default:"aarch64.encode") (Fmt.to_to_string C.pp_error e))
  | Ok enc -> Ok (`Fixed (form_of l enc))

type decode_context = { state : target_state; address : int64 }

let branch_operand ~at target =
  let abs =
    match target with
    | Asm_core.Lowered_ast.Resolved { value; _ } -> Int64.add at (Int64.mul value 4L)
    | Asm_core.Lowered_ast.Symbolic _ -> at
  in
  Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 abs))

let instruction_of_lowered ?(at = 0L) = function
  | Lowered.Add_imm { rd; rn; imm; shift12 } ->
      if Int64.equal imm 0L && (not shift12) && (rd.Reg.is_sp || rn.Reg.is_sp) then
        Some { Instruction.op = Opcode.Mov; ops = [ Operand.Reg rd; Operand.Reg rn ] }
      else
        Some
          {
            Instruction.op = Opcode.Add;
            ops = [ Operand.Reg rd; Operand.Reg rn; Operand.Imm (Bigint.of_int64 imm) ];
          }
  | Lowered.Stp_pre { rt; rt2; rn; offset } ->
      Some
        {
          Instruction.op = Opcode.Stp;
          ops =
            [
              Operand.Reg rt;
              Operand.Reg rt2;
              Operand.Mem
                { Mem.base = rn; offset = Disp.Const offset; writeback = true; pre = true };
            ];
        }
  | Lowered.Movz { rd; imm16; hw } ->
      Some
        {
          Instruction.op = Opcode.Movz;
          ops =
            Operand.Reg rd
            :: Operand.Imm (Bigint.of_int64 imm16)
            :: (if hw = 0 then [] else [ Operand.Shift { Shift.kind = "lsl"; amount = hw * 16 } ]);
        }
  | Lowered.Ldst_uoff { size; load; rt; rn; offset } ->
      let op =
        match (size, load) with
        | B, true -> Opcode.Ldrb
        | B, false -> Opcode.Strb
        | H, true -> Opcode.Ldrh
        | H, false -> Opcode.Strh
        | (W | X), true -> Opcode.Ldr
        | (W | X), false -> Opcode.Str
      in
      Some
        {
          Instruction.op;
          ops =
            [ Operand.Reg rt; Operand.Mem { Mem.base = rn; offset; writeback = false; pre = true } ];
        }
  | Lowered.Ret { rn } ->
      (* Same elision as [Lowered.pp]: x30 is the architectural default, so
         [ret x30] denormalizes to the operandless [ret] that both objdump and
         the assembler's own [lower] accept. *)
      Some
        {
          Instruction.op = Opcode.Ret;
          ops = (if rn.Reg.num = 30 && not rn.Reg.is_sp then [] else [ Operand.Reg rn ]);
        }
  | Lowered.Sub_imm { s; rd; rn; imm; shift12 } ->
      let tail = if shift12 then [ Operand.Shift { Shift.kind = "lsl"; amount = 12 } ] else [] in
      if s then
        (* [subs xzr, rn, #imm] is [cmp], the same aliasing [Addsub_shift]
           applies to the register form. A [subs] that keeps its result has no
           surface spelling in this dialect yet, so it denormalizes to nothing
           rather than to a mnemonic the parser would reject. *)
        if rd.Reg.num = 31 && not rd.Reg.is_sp then
          Some
            {
              Instruction.op = Opcode.Cmp;
              ops = Operand.Reg rn :: Operand.Imm (Bigint.of_int64 imm) :: tail;
            }
        else None
      else
        Some
          {
            Instruction.op = Opcode.Sub;
            ops = Operand.Reg rd :: Operand.Reg rn :: Operand.Imm (Bigint.of_int64 imm) :: tail;
          }
  (* Same aliasing as [Lowered.pp], and for the same reason: objdump prints
     [cmp] and [mul], so a canonical dump that printed the underlying form would
     disagree on a line where nothing differs, and would not re-parse to itself
     either. *)
  | Lowered.Addsub_shift { sub; s; rd; rn; rm; shift; amount } ->
      let tail =
        if amount = 0 then [] else [ Operand.Shift { Shift.kind = shift_name shift; amount } ]
      in
      if sub && s && rd.Reg.num = 31 && not rd.Reg.is_sp then
        Some { Instruction.op = Opcode.Cmp; ops = Operand.Reg rn :: Operand.Reg rm :: tail }
      else if s then None
      else
        Some
          {
            Instruction.op = (if sub then Opcode.Sub else Opcode.Add);
            ops = Operand.Reg rd :: Operand.Reg rn :: Operand.Reg rm :: tail;
          }
  | Lowered.Logical_imm { opc; rd; rn; imm } -> (
      (* [eor] and [ands] are opc 2 and 3; neither has a surface spelling here
         yet, so they denormalize to nothing rather than to a mnemonic the
         parser would reject. *)
      match opc with
      | 0 | 1 ->
          Some
            {
              Instruction.op = (if opc = 0 then Opcode.And else Opcode.Orr);
              ops = [ Operand.Reg rd; Operand.Reg rn; Operand.Imm (Bigint.of_int64 imm) ];
            }
      | _ -> None)
  | Lowered.Madd { rd; rn; rm; ra } ->
      if ra.Reg.num = 31 && not ra.Reg.is_sp then
        Some
          { Instruction.op = Opcode.Mul; ops = [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm ] }
      else
        Some
          {
            Instruction.op = Opcode.Madd;
            ops = [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm; Operand.Reg ra ];
          }
  | Lowered.Csel { cond; rd; rn; rm } ->
      Some
        {
          Instruction.op = Opcode.Csel;
          ops =
            [
              Operand.Reg rd;
              Operand.Reg rn;
              Operand.Reg rm;
              Operand.Sym (Asm_core.Expr.Symbol (Cond.name cond));
            ];
        }
  | Lowered.Adrp { rd; page } ->
      (* The operand objdump prints is the *page* the instruction computes, not
         the field: [adrp x16, 0] at address 8 names page 0, not offset 0. *)
      let abs =
        match page with
        | Asm_core.Lowered_ast.Resolved { value; _ } ->
            Int64.add (Int64.logand at (Int64.lognot 0xFFFL)) (Int64.shift_left value 12)
        | Asm_core.Lowered_ast.Symbolic _ -> Int64.logand at (Int64.lognot 0xFFFL)
      in
      Some
        {
          Instruction.op = Opcode.Adrp;
          ops = [ Operand.Reg rd; Operand.Sym (Asm_core.Expr.Const (Bigint.of_int64 abs)) ];
        }
  | Lowered.Udf { imm16 } ->
      Some { Instruction.op = Opcode.Udf; ops = [ Operand.Imm (Bigint.of_int64 imm16) ] }
  (* Printed as the absolute target. A64 measures a branch from the instruction
     itself, so there is no length to add - which is why the bias is 0 and this
     needs only the address. The field is a word count, so the displacement is
     four times it. *)
  | Lowered.Bl { target } ->
      Some { Instruction.op = Opcode.Bl; ops = [ branch_operand ~at target ] }
  | Lowered.B { target } -> Some { Instruction.op = Opcode.B; ops = [ branch_operand ~at target ] }
  | Lowered.Bcond { cond; target } ->
      Some { Instruction.op = Opcode.Bcond cond; ops = [ branch_operand ~at target ] }

let decode ctx bytes ~pos =
  if String.length bytes - pos < 4 then Error (diag "aarch64.decode" "fewer than four bytes remain")
  else
    let word = word_to_memory (String.sub bytes pos 4) in
    match C.decode_bits codec (C.Bits.of_bytes word) with
    | None -> Error (diag "aarch64.decode" "no form matches this word")
    | Some d -> (
        match instruction_of_lowered ~at:ctx.address d.C.value with
        | None -> Error (diag "aarch64.decode" "decoded a form with no normalized instruction")
        | Some i -> Ok (i, String.concat "." d.C.dform, 4))

(* {1 Fixups, padding, directives} *)

let evaluate_fixup kind ~place ~target =
  match kind with
  | Abs64 -> Ok target
  | Abs32 -> Ok (Int64.logand target 0xFFFFFFFFL)
  (* Both branch fields are word counts measured from the instruction; they
     differ only in width, so the reach is the one thing computed from the kind
     rather than shared. *)
  | (Pcrel_b26 | Pcrel_call26 | Pcrel_b19) as k ->
      let bits = match k with Pcrel_b19 -> 19 | _ -> 26 in
      let d = Int64.sub target place in
      if Int64.rem d 4L <> 0L then Error (diag "aarch64.fixup" "branch target is not word-aligned")
      else
        let words = Int64.div d 4L in
        let limit = Int64.shift_left 1L (bits - 1) in
        if Int64.compare words (Int64.neg limit) >= 0 && Int64.compare words limit < 0 then Ok words
        else Error (diag "aarch64.fixup" "branch target is out of range")
  | Adrp_page ->
      let page a = Int64.logand a (Int64.lognot 0xFFFL) in
      Ok (Int64.shift_right (Int64.sub (page target) (page place)) 12)
  | Add_lo12 -> Ok (Int64.logand target 0xFFFL)
  | Ldst_lo12 sz ->
      (* The field is the low 12 bits divided by the access width, so an
         unaligned target is an error rather than a truncation: the hardware
         would address a different object. *)
      let low = Int64.logand target 0xFFFL in
      let scale = Int64.of_int (access_bytes sz) in
      if Int64.rem low scale <> 0L then
        Error (diag "aarch64.fixup" "low-12 target is not aligned to the access width")
      else Ok (Int64.div low scale)

(* As on ARM: [.word] is four bytes. A64 additionally needs an eight-byte
   absolute for a pointer-sized initializer, which is why Abs64 exists. *)
let data_widths =
  [
    (".byte", 1);
    (".short", 2);
    (".hword", 2);
    (".word", 4);
    (".4byte", 4);
    (".long", 4);
    (".quad", 8);
    (".xword", 8);
  ]

let data_fixup ~width =
  match width with
  | 4 -> Ok Abs32
  | 8 -> Ok Abs64
  | _ ->
      Error
        (diag "aarch64.data-fixup"
           (Printf.sprintf "no absolute relocation for a %d-byte data initializer" width))

let nop_bytes ~length =
  if length mod 4 <> 0 then
    Error (diag "aarch64.nop" "A64 padding must be a whole number of four-byte instructions")
  else Ok (String.concat "" (List.init (length / 4) (fun _ -> "\x1f\x20\x03\xd5")))

let handle_directive ~name ~argument state =
  ignore name;
  ignore argument;
  ignore state;
  (* A64 needs no target-state directive for the M1 fixtures: unlike ARM there
     is no [.syntax], no [.arch] and no [.fpu] in them. *)
  Target_intf.Target.Unhandled
