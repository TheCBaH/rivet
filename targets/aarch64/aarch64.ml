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

module Mem = struct
  type t = { base : Reg.t; offset : int64; writeback : bool; pre : bool }

  let pp ppf m =
    let off = if Int64.equal m.offset 0L then "" else Printf.sprintf ", #%Ld" m.offset in
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

  let pp ppf = function
    | Reg r -> Reg.pp ppf r
    | Imm v -> Fmt.pf ppf "#%a" Bigint.pp v
    | Mem m -> Mem.pp ppf m
    | Shift s -> Fmt.pf ppf "%s #%d" s.Shift.kind s.Shift.amount
end

module Surface = struct
  type t = { mnemonic : string; ops : Operand.t list; origin : Origin.t }

  let pp ppf s =
    match s.ops with
    | [] -> Fmt.string ppf s.mnemonic
    | ops -> Fmt.pf ppf "%s %a" s.mnemonic Fmt.(list ~sep:(any ", ") Operand.pp) ops
end

module Opcode = struct
  type t = Add | Mov | Movz | Stp | Ldr | Ret | Sub | Strb | Udf

  let name = function
    | Add -> "add"
    | Mov -> "mov"
    | Movz -> "movz"
    | Stp -> "stp"
    | Ldr -> "ldr"
    | Ret -> "ret"
    | Sub -> "sub"
    | Strb -> "strb"
    | Udf -> "udf"

  (* The surface spellings this target accepts, and the only place the mapping
     lives. [simplify_instruction] consults it rather than repeating the list,
     so a mnemonic that is added here is accepted there by construction. *)
  let of_mnemonic = function
    | "add" -> Some Add
    | "mov" -> Some Mov
    | "movz" -> Some Movz
    | "stp" -> Some Stp
    | "ldr" -> Some Ldr
    | "ret" -> Some Ret
    | "sub" -> Some Sub
    | "strb" -> Some Strb
    | "udf" -> Some Udf
    | _ -> None
end

module Instruction = struct
  type t = { op : Opcode.t; ops : Operand.t list }

  let pp ppf i =
    match i.ops with
    | [] -> Fmt.string ppf (Opcode.name i.op)
    | ops -> Fmt.pf ppf "%s %a" (Opcode.name i.op) Fmt.(list ~sep:(any ", ") Operand.pp) ops
end

module Lowered = struct
  type t =
    | Add_imm of { rd : Reg.t; rn : Reg.t; imm : int64; shift12 : bool }
    | Stp_pre of { rt : Reg.t; rt2 : Reg.t; rn : Reg.t; offset : int64 }
    | Movz of { rd : Reg.t; imm16 : int64; hw : int }
    | Ldr_uoff of { rt : Reg.t; rn : Reg.t; offset : int64 }
    | Ret of { rn : Reg.t }
    | Sub_imm of { rd : Reg.t; rn : Reg.t; imm : int64; shift12 : bool }
    | Strb_uoff of { rt : Reg.t; rn : Reg.t; offset : int64 }
    | Udf of { imm16 : int64 }

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
    | Ldr_uoff { rt; rn; offset } -> Fmt.pf ppf "ldr %a, [%a, #%Ld]" Reg.pp rt Reg.pp rn offset
    | Ret { rn } ->
        (* [ret x30] is spelled [ret]: x30 is the architectural default and every
           A64 disassembler elides it. Printing it would make the canonical dump
           disagree with objdump on a line where nothing differs. *)
        if rn.Reg.num = 30 && not rn.Reg.is_sp then Fmt.string ppf "ret"
        else Fmt.pf ppf "ret %a" Reg.pp rn
    | Sub_imm { rd; rn; imm; shift12 } ->
        Fmt.pf ppf "sub %a, %a, #%Ld%s" Reg.pp rd Reg.pp rn imm
          (if shift12 then ", lsl #12" else "")
    | Strb_uoff { rt; rn; offset } -> Fmt.pf ppf "strb %a, [%a, #%Ld]" Reg.pp rt Reg.pp rn offset
    | Udf { imm16 } -> Fmt.pf ppf "udf #%Ld" imm16

  let equal a b =
    match (a, b) with
    | Add_imm x, Add_imm y ->
        Reg.equal x.rd y.rd && Reg.equal x.rn y.rn && Int64.equal x.imm y.imm
        && x.shift12 = y.shift12
    | Stp_pre x, Stp_pre y ->
        Reg.equal x.rt y.rt && Reg.equal x.rt2 y.rt2 && Reg.equal x.rn y.rn
        && Int64.equal x.offset y.offset
    | Movz x, Movz y -> Reg.equal x.rd y.rd && Int64.equal x.imm16 y.imm16 && x.hw = y.hw
    | Ldr_uoff x, Ldr_uoff y ->
        Reg.equal x.rt y.rt && Reg.equal x.rn y.rn && Int64.equal x.offset y.offset
    | Ret x, Ret y -> Reg.equal x.rn y.rn
    | Sub_imm x, Sub_imm y ->
        Reg.equal x.rd y.rd && Reg.equal x.rn y.rn && Int64.equal x.imm y.imm
        && x.shift12 = y.shift12
    | Strb_uoff x, Strb_uoff y ->
        Reg.equal x.rt y.rt && Reg.equal x.rn y.rn && Int64.equal x.offset y.offset
    | Udf x, Udf y -> Int64.equal x.imm16 y.imm16
    | _ -> false
end

(* The low-12 relocation is *scaled by the access width*, which is why it is a
   four-case variant and not one kind: R_AARCH64_LDST32_ABS_LO12_NC divides the
   offset by 4 where the 8-bit form does not divide at all, and neither is the
   same transform as [add]'s unscaled ADD_ABS_LO12_NC. A finite variant rather
   than an [int] also keeps the kind domain genuinely finite, which is what lets
   the name-consistency test enumerate it. *)
type access_size = B | H | W | X

let access_bytes = function B -> 1 | H -> 2 | W -> 4 | X -> 8

type fixup_kind =
  | Abs32
  | Abs64
  | Pcrel_b26
  | Pcrel_call26
  | Adrp_page
  | Add_lo12
  | Ldst_lo12 of access_size

let fixup_kind_name = function
  | Abs32 -> "abs32"
  | Abs64 -> "abs64"
  | Pcrel_b26 -> "pcrel-b26"
  | Pcrel_call26 -> "pcrel-call26"
  | Adrp_page -> "adrp-page"
  | Add_lo12 -> "add-lo12"
  | Ldst_lo12 sz -> Printf.sprintf "ldst%d-lo12" (access_bytes sz * 8)

let equal_fixup_kind a b = a = b

let fixup_family = function
  | Abs32 -> "abs32"
  | Abs64 -> "abs64"
  | Pcrel_b26 -> "pcrel-branch"
  | Pcrel_call26 -> "pcrel-call"
  | Adrp_page -> "adrp-page"
  | Add_lo12 -> "add-lo12"
  | Ldst_lo12 _ -> "ldst-lo12"

let fixup_role = function
  | Abs32 | Abs64 | Adrp_page | Add_lo12 | Ldst_lo12 _ -> Asm_core.Lowered_ast.Data_address
  | Pcrel_b26 -> Asm_core.Lowered_ast.Branch
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

(* {1 The codec} *)

let reg_field ~width ~sp name =
  C.iso_fun ~name
    ~encode:(fun (r : Reg.t) -> Some (Int64.of_int r.Reg.num))
    ~decode:(fun v -> Some (Reg.of_num ~width ~sp (Int64.to_int v)))
    (C.field ~width:5 name)

let codec : (Lowered.t, fixup_kind) C.t =
  C.choice ~name:"aarch64"
    [
      (* Every A64 form has a distinct fixed-bit prefix, so unlike A32 and x86
         there is no overlap here for priority to resolve and [check] reports
         none. The priorities are still declared, because "no two forms overlap
         today" is a fact about these five forms and not a property of A64. *)
      C.alt ~label:"add-imm" ~priority:0
        (C.iso_fun ~name:"add-imm"
           ~encode:(function
             | Lowered.Add_imm { rd; rn; imm; shift12 } ->
                 if Int64.compare imm 0L < 0 || Int64.compare imm 4096L >= 0 then None
                 else Some ((), ((if shift12 then 1L else 0L), (imm, (rn, rd))))
             | _ -> None)
           ~decode:(fun ((), (sh, (imm, (rn, rd)))) ->
             Some (Lowered.Add_imm { rd; rn; imm; shift12 = Int64.equal sh 1L }))
           C.(
             const ~width:9 0b100100010L ** field ~width:1 "sh" ** field ~width:12 "imm12"
             ** reg_field ~width:64 ~sp:true "rn" ** reg_field ~width:64 ~sp:true "rd"));
      C.alt ~label:"stp-pre" ~priority:1
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
      C.alt ~label:"movz" ~priority:2
        (C.iso_fun ~name:"movz"
           ~encode:(function
             | Lowered.Movz { rd; imm16; hw } ->
                 if Int64.compare imm16 0L < 0 || Int64.compare imm16 65536L >= 0 then None
                 else
                   Some
                     ((if rd.Reg.width = 64 then 1L else 0L), ((), (Int64.of_int hw, (imm16, rd))))
             | _ -> None)
           ~decode:(fun (sf, ((), (hw, (imm16, rd)))) ->
             let width = if Int64.equal sf 1L then 64 else 32 in
             Some (Lowered.Movz { rd = { rd with Reg.width }; imm16; hw = Int64.to_int hw }))
           C.(
             field ~width:1 "sf" ** const ~width:8 0b10100101L ** field ~width:2 "hw"
             ** field ~width:16 "imm16"
             ** reg_field ~width:64 ~sp:false "rd"));
      C.alt ~label:"ldr-uoff" ~priority:3
        (C.iso_fun ~name:"ldr-uoff"
           ~encode:(function
             | Lowered.Ldr_uoff { rt; rn; offset } -> Some ((), (offset, (rn, rt))) | _ -> None)
           ~decode:(fun ((), (offset, (rn, rt))) -> Some (Lowered.Ldr_uoff { rt; rn; offset }))
           C.(
             const ~width:10 0b1111100101L
             ** scaled ~shift:3 ~width:12 ~signedness:C.Unsigned "imm12"
             ** reg_field ~width:64 ~sp:true "rn"
             ** reg_field ~width:64 ~sp:false "rt"));
      C.alt ~label:"ret" ~priority:4
        (C.iso_fun ~name:"ret"
           ~encode:(function Lowered.Ret { rn } -> Some ((), (rn, ())) | _ -> None)
           ~decode:(fun ((), (rn, ())) -> Some (Lowered.Ret { rn }))
           C.(
             const ~width:22 0b1101011001011111000000L
             ** reg_field ~width:64 ~sp:false "rn"
             ** const ~width:5 0L));
      (* [sub], A64's ADD/SUB(immediate) class with the [op] bit set: the same
         shape as [add-imm] with one constant bit flipped, not a separate
         instruction family - which is why A64 encodes it separately from
         [add-imm] rather than folding it into a negated addend. *)
      C.alt ~label:"sub-imm" ~priority:5
        (C.iso_fun ~name:"sub-imm"
           ~encode:(function
             | Lowered.Sub_imm { rd; rn; imm; shift12 } ->
                 if Int64.compare imm 0L < 0 || Int64.compare imm 4096L >= 0 then None
                 else Some ((), ((if shift12 then 1L else 0L), (imm, (rn, rd))))
             | _ -> None)
           ~decode:(fun ((), (sh, (imm, (rn, rd)))) ->
             Some (Lowered.Sub_imm { rd; rn; imm; shift12 = Int64.equal sh 1L }))
           C.(
             const ~width:9 0b110100010L ** field ~width:1 "sh" ** field ~width:12 "imm12"
             ** reg_field ~width:64 ~sp:true "rn" ** reg_field ~width:64 ~sp:true "rd"));
      (* [strb], the LDR/STR-unsigned-immediate class with [size=00,opc=00].
         Unlike [ldr-uoff]'s field the offset is not scaled: a byte access has
         nothing to divide by eight, so this is a plain field rather than
         [scaled ~shift:0]. *)
      C.alt ~label:"strb-uoff" ~priority:6
        (C.iso_fun ~name:"strb-uoff"
           ~encode:(function
             | Lowered.Strb_uoff { rt; rn; offset } -> Some ((), (offset, (rn, rt))) | _ -> None)
           ~decode:(fun ((), (offset, (rn, rt))) -> Some (Lowered.Strb_uoff { rt; rn; offset }))
           C.(
             const ~width:10 0b0011100100L
             ** field ~width:12 ~signedness:C.Unsigned "imm12"
             ** reg_field ~width:64 ~sp:true "rn"
             ** reg_field ~width:32 ~sp:false "rt"));
      C.alt ~label:"udf" ~priority:7
        (C.iso_fun ~name:"udf"
           ~encode:(function Lowered.Udf { imm16 } -> Some ((), imm16) | _ -> None)
           ~decode:(fun ((), imm16) -> Some (Lowered.Udf { imm16 }))
           C.(const ~width:16 0x0000L ** field ~width:16 ~signedness:C.Unsigned "imm16"));
    ]

(* {1 Lexical profile}

   [//] introduces a comment, so the profile's longest-first matching matters:
   a dialect that also used [/] would otherwise take the first character and
   leave the second as a divide. *)
let lexical_profile =
  Asm_core.Lexical_profile.make ~name:"aarch64" ~comment_introducers:[ "//" ]
    ~statement_separator:';' ~immediate_sigil:'#' ()

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
  let int64_of v = match Bigint.to_int64_opt v with Some x -> Some x | None -> None in
  match List.map Token.kind slice with
  | [ Token.Ident n ] -> (
      match Reg.find n with Some r -> Ok (Operand.Reg r) | None -> bad ("unknown register " ^ n))
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
  | [ Token.Lbracket; Token.Ident b; Token.Rbracket ] -> mem ~base:b ~offset:0L ~writeback:false
  | [ Token.Lbracket; Token.Ident b; Token.Rbracket; Token.Bang ] ->
      mem ~base:b ~offset:0L ~writeback:true
  | [ Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket ] -> (
      match int64_of v with
      | Some o -> mem ~base:b ~offset:o ~writeback:false
      | None -> bad "offset does not fit")
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket; Token.Bang;
  ] -> (
      match int64_of v with
      | Some o -> mem ~base:b ~offset:o ~writeback:true
      | None -> bad "offset does not fit")
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Minus; Token.Int v; Token.Rbracket;
  ] -> (
      match int64_of v with
      | Some o -> mem ~base:b ~offset:(Int64.neg o) ~writeback:false
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
      | Some o -> mem ~base:b ~offset:(Int64.neg o) ~writeback:true
      | None -> bad "offset does not fit")
  | _ -> bad ("cannot parse operand " ^ Asm_core.Token.slice_text slice)

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
  | Opcode.Stp, [ Operand.Reg rt; Operand.Reg rt2; Operand.Mem m ] ->
      if not m.Mem.writeback then bad "M1 supports only the pre-index writeback form of stp"
      else if not (Int64.equal (Int64.rem m.Mem.offset 8L) 0L) then
        bad "stp offset must be a multiple of eight"
      else Ok [ Lowered.Stp_pre { rt; rt2; rn = m.Mem.base; offset = m.Mem.offset } ]
  | Opcode.Ldr, [ Operand.Reg rt; Operand.Mem m ] ->
      if m.Mem.writeback then bad "writeback ldr is not in M1 scope"
      else if Int64.compare m.Mem.offset 0L < 0 then
        bad "M1 supports only the unsigned-offset form of ldr"
      else if not (Int64.equal (Int64.rem m.Mem.offset 8L) 0L) then
        bad "ldr offset must be a multiple of eight for a 64-bit access"
      else Ok [ Lowered.Ldr_uoff { rt; rn = m.Mem.base; offset = m.Mem.offset } ]
  | Opcode.Ret, [ Operand.Reg rn ] -> Ok [ Lowered.Ret { rn } ]
  | Opcode.Ret, [] -> Ok [ Lowered.Ret { rn = Reg.x30 } ]
  | Opcode.Sub, [ Operand.Reg rd; Operand.Reg rn; Operand.Imm v ] -> (
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if Int64.compare imm 0L < 0 || Int64.compare imm 4096L >= 0 then
            bad
              "immediate does not fit the unshifted 12-bit field; use the four-operand lsl #12 form"
          else Ok [ Lowered.Sub_imm { rd; rn; imm; shift12 = false } ])
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
          else Ok [ Lowered.Sub_imm { rd; rn; imm; shift12 = true } ])
  | Opcode.Strb, [ Operand.Reg rt; Operand.Mem m ] ->
      if m.Mem.writeback then bad "writeback strb is not in M1 scope"
      else if Int64.compare m.Mem.offset 0L < 0 || Int64.compare m.Mem.offset 4096L >= 0 then
        bad "strb offset must fit the unsigned 12-bit field"
      else Ok [ Lowered.Strb_uoff { rt; rn = m.Mem.base; offset = m.Mem.offset } ]
  | Opcode.Udf, [ Operand.Imm v ] -> (
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if Int64.compare imm 0L < 0 || Int64.compare imm 65536L >= 0 then
            bad "udf immediate does not fit 16 bits"
          else Ok [ Lowered.Udf { imm16 = imm } ])
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
      | Adrp_page -> Asm_core.Lowered_ast.Signed 21
      | Add_lo12 -> Asm_core.Lowered_ast.Unsigned 12
      | Ldst_lo12 _ -> Asm_core.Lowered_ast.Unsigned 12);
    value = Asm_core.Expr.Const (Bigint.of_int 0);
    origin = Origin.synthesized ~pass:"aarch64.encode" ();
  }

let form_of enc =
  {
    Asm_core.Lowered_ast.bytes = word_to_memory (C.Bits.to_bytes enc.C.bits);
    form = C.form_id enc;
    fixups = List.map fixup_of_placement enc.C.placements;
  }

let encode l =
  match C.encode codec l with
  | Error e -> Error (diag "aarch64.encode" (Fmt.to_to_string C.pp_error e))
  | Ok enc -> Ok (`Fixed (form_of enc))

type decode_context = { state : target_state; address : int64 }

let instruction_of_lowered = function
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
              Operand.Mem { Mem.base = rn; offset; writeback = true; pre = true };
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
  | Lowered.Ldr_uoff { rt; rn; offset } ->
      Some
        {
          Instruction.op = Opcode.Ldr;
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
  | Lowered.Sub_imm { rd; rn; imm; shift12 } ->
      Some
        {
          Instruction.op = Opcode.Sub;
          ops =
            Operand.Reg rd :: Operand.Reg rn
            :: Operand.Imm (Bigint.of_int64 imm)
            :: (if shift12 then [ Operand.Shift { Shift.kind = "lsl"; amount = 12 } ] else []);
        }
  | Lowered.Strb_uoff { rt; rn; offset } ->
      Some
        {
          Instruction.op = Opcode.Strb;
          ops =
            [ Operand.Reg rt; Operand.Mem { Mem.base = rn; offset; writeback = false; pre = true } ];
        }
  | Lowered.Udf { imm16 } ->
      Some { Instruction.op = Opcode.Udf; ops = [ Operand.Imm (Bigint.of_int64 imm16) ] }

let decode ctx bytes ~pos =
  ignore ctx;
  if String.length bytes - pos < 4 then Error (diag "aarch64.decode" "fewer than four bytes remain")
  else
    let word = word_to_memory (String.sub bytes pos 4) in
    match C.decode_bits codec (C.Bits.of_bytes word) with
    | None -> Error (diag "aarch64.decode" "no form matches this word")
    | Some d -> (
        match instruction_of_lowered d.C.value with
        | None -> Error (diag "aarch64.decode" "decoded a form with no normalized instruction")
        | Some i -> Ok (i, String.concat "." d.C.dform, 4))

(* {1 Fixups, padding, directives} *)

let evaluate_fixup kind ~place ~target =
  match kind with
  | Abs64 -> Ok target
  | Abs32 -> Ok (Int64.logand target 0xFFFFFFFFL)
  | Pcrel_b26 | Pcrel_call26 ->
      let d = Int64.sub target place in
      if Int64.rem d 4L <> 0L then Error (diag "aarch64.fixup" "branch target is not word-aligned")
      else
        let words = Int64.div d 4L in
        (* +-128MB, the 26-bit word-count field. *)
        if Int64.compare words (-0x2000000L) >= 0 && Int64.compare words 0x2000000L < 0 then
          Ok words
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
