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

(* {1 Operands} *)

module Mem = struct
  type t = { base : Reg.t; offset : int64; writeback : bool; pre : bool }

  let pp ppf m =
    let off = if Int64.equal m.offset 0L then "" else Printf.sprintf ", #%Ld" m.offset in
    if m.pre then Fmt.pf ppf "[%a%s]%s" Reg.pp m.base off (if m.writeback then "!" else "")
    else Fmt.pf ppf "[%a]%s" Reg.pp m.base off
end

let shift_name = function 0 -> "lsl" | 1 -> "lsr" | 2 -> "asr" | 3 -> "ror" | _ -> "?"

let shift_of_name = function
  | "lsl" -> Some 0
  | "lsr" -> Some 1
  | "asr" -> Some 2
  | "ror" -> Some 3
  | _ -> None

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

  let pp ppf = function
    | Reg r -> Reg.pp ppf r
    | Imm v -> Fmt.pf ppf "#%a" Bigint.pp v
    | Mem m -> Mem.pp ppf m
    | Sym e -> Fmt.string ppf (Asm_core.Expr.to_string e)
    | Shifted { reg; kind; amount } -> Fmt.pf ppf "%a, %s #%d" Reg.pp reg (shift_name kind) amount
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

module Opcode = struct
  type t =
    | Mov
    | Add
    | Sub
    | And
    | Cmp
    | Mul
    | Mla
    | Str
    | Ldr
    | Bx
    | Strb
    | Udf
    | Bl
    | B
    | Movw
    | Movt
    | Ite

  let name = function
    | Mov -> "mov"
    | Add -> "add"
    | Sub -> "sub"
    | And -> "and"
    | Cmp -> "cmp"
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
    | Udf -> "udf"
    | Bl -> "bl"

  (* The surface spellings this target accepts. [simplify_instruction] consults
     this rather than repeating the list. *)
  let of_mnemonic = function
    | "mov" -> Some Mov
    | "add" -> Some Add
    | "sub" -> Some Sub
    | "and" -> Some And
    | "cmp" -> Some Cmp
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
    | "udf" -> Some Udf
    | "bl" -> Some Bl
    | _ -> None

  (* A32 encodes add, sub and mov as one data-processing format distinguished
     by a 4-bit opcode field. The two directions live together so they cannot
     drift; [-1] is "this opcode is not a data-processing one". *)
  let to_dp = function And -> 0 | Sub -> 2 | Add -> 4 | Cmp -> 10 | Mov -> 13 | _ -> -1

  let of_dp = function
    | 0 -> Some And
    | 2 -> Some Sub
    | 4 -> Some Add
    | 10 -> Some Cmp
    | 13 -> Some Mov
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
     is spelled by its absence. *)
  let pp ppf i =
    let m = Opcode.name i.op ^ Cond.suffix i.cond in
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
      }
    | Bx of { cond : Cond.t; rm : Reg.t }
    | Udf of { imm16 : int64 }  (** permanently undefined, and unconditional by definition *)
    | Bl of { cond : Cond.t; target : Asm_core.Lowered_ast.branch }
    | B of { cond : Cond.t; target : Asm_core.Lowered_ast.branch }
    | Mul of { cond : Cond.t; s : bool; rd : Reg.t; rn : Reg.t; rm : Reg.t }
    | Mla of { cond : Cond.t; s : bool; rd : Reg.t; rn : Reg.t; rm : Reg.t; ra : Reg.t }
    | Movw_movt of { cond : Cond.t; top : bool; rd : Reg.t; imm : Asm_core.Lowered_ast.branch }
        (** [movw]/[movt] with a 16-bit immediate that is usually [:lower16:] or [:upper16:] of a
            symbol - hence a [branch], which is this codebase's name for "an operand that is either
            an expression awaiting the linker or a value a decoder recovered". *)

  let pp ppf = function
    | Dp_imm { cond; dp; rd; rn; imm; _ } -> (
        let c = Cond.suffix cond in
        match Opcode.of_dp dp with
        | Some Opcode.Mov -> Fmt.pf ppf "mov%s %a, #%Ld" c Reg.pp rd imm
        (* A compare writes no destination, so printing [Rd] would print the
           zero the encoding requires as though it were an operand. *)
        | Some o when Opcode.is_compare o ->
            Fmt.pf ppf "%s%s %a, #%Ld" (Opcode.name o) c Reg.pp rn imm
        | Some o -> Fmt.pf ppf "%s%s %a, %a, #%Ld" (Opcode.name o) c Reg.pp rd Reg.pp rn imm
        | None -> Fmt.pf ppf "dp%d%s %a, %a, #%Ld" dp c Reg.pp rd Reg.pp rn imm)
    | Dp_reg { cond; dp; rd; rn; rm; sh_kind; sh_amt; _ } -> (
        let sh =
          if sh_amt = 0 && sh_kind = 0 then ""
          else Printf.sprintf ", %s #%d" (shift_name sh_kind) sh_amt
        in
        let c = Cond.suffix cond in
        match Opcode.of_dp dp with
        | Some Opcode.Mov -> Fmt.pf ppf "mov%s %a, %a%s" c Reg.pp rd Reg.pp rm sh
        | Some o when Opcode.is_compare o ->
            Fmt.pf ppf "%s%s %a, %a%s" (Opcode.name o) c Reg.pp rn Reg.pp rm sh
        | Some o ->
            Fmt.pf ppf "%s%s %a, %a, %a%s" (Opcode.name o) c Reg.pp rd Reg.pp rn Reg.pp rm sh
        | None -> Fmt.pf ppf "dp%d%s %a, %a, %a%s" dp c Reg.pp rd Reg.pp rn Reg.pp rm sh)
    | Ldst_imm { cond; load; byte; rt; rn; offset } ->
        Fmt.pf ppf "%s%s%s %a, [%a, #%Ld]"
          (if load then "ldr" else "str")
          (if byte then "b" else "")
          (Cond.suffix cond) Reg.pp rt Reg.pp rn offset
    | Bx { cond; rm } -> Fmt.pf ppf "bx%s %a" (Cond.suffix cond) Reg.pp rm
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
        && Reg.equal x.rn y.rn && Int64.equal x.offset y.offset
    | Bx x, Bx y -> Cond.equal x.cond y.cond && Reg.equal x.rm y.rm
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
    | _ -> false
end

(* Measured on the M2 fixtures: a same-file call is R_ARM_CALL, a global address
   arrives as the R_ARM_MOVW_ABS_NC / R_ARM_MOVT_ABS pair - one relocation per
   instruction, each patching a field split across two non-adjacent runs - and a
   branch to a local label keeps no record at all. A32 is fixed-width, so there
   is no ladder and no short rung. *)
type fixup_kind = Abs32 | Pcrel_b26 | Pcrel_call | Movw_abs_nc | Movt_abs

let fixup_kind_name = function
  | Abs32 -> "abs32"
  | Pcrel_b26 -> "pcrel-b26"
  | Pcrel_call -> "pcrel-call"
  | Movw_abs_nc -> "movw-abs-nc"
  | Movt_abs -> "movt-abs"

let equal_fixup_kind a b = a = b

let fixup_family = function
  | Abs32 -> "abs"
  | Pcrel_b26 -> "pcrel-branch"
  | Pcrel_call -> "pcrel-call"
  | Movw_abs_nc -> "abs-lo16"
  | Movt_abs -> "abs-hi16"

let fixup_role = function
  | Abs32 | Movw_abs_nc | Movt_abs -> Asm_core.Lowered_ast.Data_address
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
             | Lowered.Ldst_imm { cond; load; byte; rt; rn; offset } ->
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
                               ((), ((if load then 1L else 0L), (rn, (rt, mag)))) ) ) ) ) )
             | _ -> None)
           ~decode:(fun (cond, ((), ((), (up, (byte, ((), (load, (rn, (rt, mag))))))))) ->
             Some
               (Lowered.Ldst_imm
                  {
                    cond;
                    load = Int64.equal load 1L;
                    byte = Int64.equal byte 1L;
                    rt;
                    rn;
                    offset = (if Int64.equal up 1L then mag else Int64.neg mag);
                  }))
           C.(
             cond_codec ** const ~width:3 2L
             ** const ~width:1 1L (* P: offset addressing, no writeback *)
             ** field ~width:1 "u" ** field ~width:1 "b" ** const ~width:1 0L (* W *)
             ** field ~width:1 "l" ** reg_field "rn" ** reg_field "rt" ** field ~width:12 "imm12"));
      (* [udf], the permanently-undefined encoding. Its fixed top nibble is
         literally [cond_codec]'s bit pattern - not a coincidence to route around,
         but where ARM parked this encoding. *)
      C.alt ~label:"udf" ~priority:6
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
      C.alt ~label:"bl" ~priority:7
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
      C.alt ~label:"b" ~priority:8
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
      C.alt ~label:"mul" ~priority:9
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
      C.alt ~label:"mla" ~priority:10
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
  | `No_modified_immediate_mov of int64
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
  | `No_data_relocation of int
  | `Padding_not_word_multiple ]

and wrong_relocation_modifier = { opcode : string; want : string; got : string }
and missing_relocation_modifier = { opcode : string; want : string }

type error = error_kind Target_error.t

let pp_error_kind ppf : error_kind -> unit = function
  | #Target_error.shared as e -> Target_error.pp_shared ppf e
  | `Thumb_out_of_scope -> Fmt.string ppf "Thumb is not in M1 scope"
  | `Expected_register_or_shifted -> Fmt.string ppf "expected a register or a shifted register"
  | `No_modified_immediate v -> Fmt.pf ppf "#%Ld has no A32 modified-immediate representation" v
  | `No_modified_immediate_mov v ->
      Fmt.pf ppf "#%Ld has no A32 modified-immediate representation; movw and mvn are M2" v
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
  | `No_data_relocation w -> Fmt.pf ppf "no absolute relocation for a %d-byte data initializer" w
  | `Padding_not_word_multiple ->
      Fmt.string ppf "A32 padding must be a whole number of four-byte instructions"

let error_kind_code : error_kind -> string = function
  | `Unknown_instruction _ -> "arm.simplify"
  | `Thumb_out_of_scope | `Immediate_too_wide | `Expected_register_or_shifted
  | `No_modified_immediate _ | `No_modified_immediate_mov _ | `Writeback_out_of_scope
  | `Offset_not_12bit | `Udf_imm16_overflow | `Wrong_relocation_modifier _
  | `Missing_relocation_modifier _ | `Imm16_overflow _ | `No_form _ ->
      "arm.lower"
  | `Codec e -> Option.value (Codec.code e) ~default:"arm.encode"
  | `Decode_short | `Decode_no_match | `Decode_no_normalized -> "arm.decode"
  | `Branch_not_word_aligned | `Branch_out_of_range -> "arm.fixup"
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
      let stem, cond = Cond.split m in
      match Opcode.of_mnemonic stem with
      | Some op -> Ok (Instruction.mk ~cond op s.Surface.ops)
      | None -> bad (`Unknown_instruction m))

(* {1 Lower} *)

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
    | Opcode.Mov, [ Operand.Reg rd; Operand.Imm v ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm ->
            if encode_modimm imm = None then bad (`No_modified_immediate_mov imm)
            else
              Ok
                [
                  Lowered.Dp_imm
                    { cond; dp = Opcode.to_dp Opcode.Mov; s = false; rd; rn = Reg.of_num 0; imm };
                ])
    (* The register forms, with or without a shift. [Shifted] and a bare [Reg]
       differ only in the two fields the encoding already has, so one case
       covers both rather than duplicating the lowering. *)
    | ((Opcode.Add | Opcode.Sub | Opcode.And | Opcode.Mov) as op), (Operand.Reg rd :: rest as ops)
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
    | ( ((Opcode.Add | Opcode.Sub | Opcode.And) as op),
        [ Operand.Reg rd; Operand.Reg rn; Operand.Imm v ] ) -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm ->
            if encode_modimm imm = None then bad (`No_modified_immediate imm)
            else Ok [ Lowered.Dp_imm { cond; dp = Opcode.to_dp op; s = false; rd; rn; imm } ])
    | ((Opcode.Str | Opcode.Ldr) as op), [ Operand.Reg rt; Operand.Mem m ] ->
        if m.writeback then bad `Writeback_out_of_scope
        else if Int64.compare (Int64.abs m.offset) 4096L >= 0 then bad `Offset_not_12bit
        else
          Ok
            [
              Lowered.Ldst_imm
                { cond; load = op = Opcode.Ldr; byte = false; rt; rn = m.base; offset = m.offset };
            ]
    | Opcode.Strb, [ Operand.Reg rt; Operand.Mem m ] ->
        if m.writeback then bad `Writeback_out_of_scope
        else if Int64.compare (Int64.abs m.offset) 4096L >= 0 then bad `Offset_not_12bit
        else
          Ok
            [
              Lowered.Ldst_imm
                { cond; load = false; byte = true; rt; rn = m.base; offset = m.offset };
            ]
    | Opcode.Bx, [ Operand.Reg rm ] -> Ok [ Lowered.Bx { cond; rm } ]
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
    pc_bias = (match kind with Pcrel_b26 | Pcrel_call -> 8 | _ -> 0);
    range =
      (match kind with
      | Abs32 -> Asm_core.Lowered_ast.Bitpattern 32
      | Pcrel_b26 | Pcrel_call -> Asm_core.Lowered_ast.Signed 24
      | Movw_abs_nc | Movt_abs -> Asm_core.Lowered_ast.Unsigned 16);
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
  | Lowered.B { target = Asm_core.Lowered_ast.Symbolic { value; _ }; _ } ->
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
      | Some Opcode.Mov ->
          Some
            (Instruction.mk ~cond Opcode.Mov [ Operand.Reg rd; Operand.Imm (Bigint.of_int64 imm) ])
      | Some o when Opcode.is_compare o ->
          Some (Instruction.mk ~cond o [ Operand.Reg rn; Operand.Imm (Bigint.of_int64 imm) ])
      | Some o ->
          Some
            (Instruction.mk ~cond o
               [ Operand.Reg rd; Operand.Reg rn; Operand.Imm (Bigint.of_int64 imm) ]))
  | Lowered.Dp_reg { cond; dp; rd; rn; rm; sh_kind; sh_amt; _ } -> (
      let shifted =
        if sh_kind = 0 && sh_amt = 0 then Operand.Reg rm
        else Operand.Shifted { reg = rm; kind = sh_kind; amount = sh_amt }
      in
      match Opcode.of_dp dp with
      | None -> None
      | Some Opcode.Mov -> Some (Instruction.mk ~cond Opcode.Mov [ Operand.Reg rd; shifted ])
      | Some o when Opcode.is_compare o -> Some (Instruction.mk ~cond o [ Operand.Reg rn; shifted ])
      | Some o -> Some (Instruction.mk ~cond o [ Operand.Reg rd; Operand.Reg rn; shifted ]))
  | Lowered.Ldst_imm { cond; load; byte; rt; rn; offset } -> (
      let mem_op = Operand.Mem { base = rn; offset; writeback = false; pre = true } in
      match (load, byte) with
      | false, false -> Some (Instruction.mk ~cond Opcode.Str [ Operand.Reg rt; mem_op ])
      | true, false -> Some (Instruction.mk ~cond Opcode.Ldr [ Operand.Reg rt; mem_op ])
      | false, true -> Some (Instruction.mk ~cond Opcode.Strb [ Operand.Reg rt; mem_op ])
      | true, true -> None
      (* ldrb decodes correctly but has no opcode to normalize into. *))
  | Lowered.Bx { cond; rm } -> Some (Instruction.mk ~cond Opcode.Bx [ Operand.Reg rm ])
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
