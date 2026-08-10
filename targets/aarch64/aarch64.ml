(* The aarch64 (A64) target description (.ai/asm_plan.md §5.1).

   Deliberately not in a family functor with ARM. A64 is a different
   instruction set that happens to share a vendor: no condition field, no
   rotated immediates, a 31st register whose meaning depends on the instruction,
   and scaled offsets rather than magnitude-plus-sign.

   Byte order, as for A32: one 32-bit word authored most significant bit first,
   reversed once in {!encode}. *)

open Foundation
module C = Codec

(* {1 Registers}

   Register 31 is [sp] in some instruction positions and [xzr] in others, and
   nothing in the encoding says which - the *instruction* says. So the flag is
   part of the register value and is checked at lowering, where the diagnostic
   can name the operand, rather than being inferred at encode time where it
   could only be guessed. *)

type reg = { rnum : int; rwidth : int; is_sp : bool }

let reg_equal a b = a.rnum = b.rnum && a.rwidth = b.rwidth && a.is_sp = b.is_sp

let reg_name r =
  let p = if r.rwidth = 64 then "x" else "w" in
  if r.rnum = 31 then if r.is_sp then if r.rwidth = 64 then "sp" else "wsp" else p ^ "zr"
  else Printf.sprintf "%s%d" p r.rnum

let pp_reg ppf r = Fmt.string ppf (reg_name r)

let find_reg name =
  let numbered prefix width =
    let n = String.length prefix in
    if String.length name > n && String.sub name 0 n = prefix then
      match int_of_string_opt (String.sub name n (String.length name - n)) with
      | Some i when i >= 0 && i <= 30 -> Some { rnum = i; rwidth = width; is_sp = false }
      | _ -> None
    else None
  in
  match name with
  | "sp" -> Some { rnum = 31; rwidth = 64; is_sp = true }
  | "wsp" -> Some { rnum = 31; rwidth = 32; is_sp = true }
  | "xzr" -> Some { rnum = 31; rwidth = 64; is_sp = false }
  | "wzr" -> Some { rnum = 31; rwidth = 32; is_sp = false }
  | _ -> ( match numbered "x" 64 with Some r -> Some r | None -> numbered "w" 32)

(* Decoding cannot recover [is_sp] from the bits, so it is supplied by the
   position: the caller says whether this field is an SP-accepting one. *)
let reg_of_num ~width ~sp n = { rnum = n land 31; rwidth = width; is_sp = sp && n land 31 = 31 }

(* {1 Operands} *)

type memory = { m_base : reg; m_offset : int64; m_writeback : bool; m_pre : bool }
type shift_op = { sh_kind : string; sh_amount : int }

type operand =
  | Op_reg of reg
  | Op_imm of Bigint.t
  | Op_mem of memory
  | Op_shift of shift_op
      (** [lsl #0] in [movz w0, #42, lsl #0]. Not an operand in any real sense - it modifies the
          immediate before it - but the common parser hands over comma-separated slices and this
          is one of them. Simplify folds it into the operand it belongs to; a surface AST that
          had already folded it could not report a shift applied to nothing. *)

let pp_mem ppf m =
  let off = if Int64.equal m.m_offset 0L then "" else Printf.sprintf ", #%Ld" m.m_offset in
  Fmt.pf ppf "[%a%s]%s" pp_reg m.m_base off (if m.m_writeback then "!" else "")

let pp_operand ppf = function
  | Op_reg r -> pp_reg ppf r
  | Op_imm v -> Fmt.pf ppf "#%a" Bigint.pp v
  | Op_mem m -> pp_mem ppf m
  | Op_shift s -> Fmt.pf ppf "%s #%d" s.sh_kind s.sh_amount

type surface_operand = operand
type surface_instruction = { s_mnemonic : string; s_ops : operand list; s_origin : Origin.t }

let pp_surface ppf s =
  match s.s_ops with
  | [] -> Fmt.string ppf s.s_mnemonic
  | ops -> Fmt.pf ppf "%s %a" s.s_mnemonic Fmt.(list ~sep:(any ", ") pp_operand) ops

type opcode = Op_add | Op_mov | Op_movz | Op_stp | Op_ldr | Op_ret

let opcode_name = function
  | Op_add -> "add"
  | Op_mov -> "mov"
  | Op_movz -> "movz"
  | Op_stp -> "stp"
  | Op_ldr -> "ldr"
  | Op_ret -> "ret"

type instruction = { i_op : opcode; i_ops : operand list }

let pp_instruction ppf i =
  match i.i_ops with
  | [] -> Fmt.string ppf (opcode_name i.i_op)
  | ops -> Fmt.pf ppf "%s %a" (opcode_name i.i_op) Fmt.(list ~sep:(any ", ") pp_operand) ops

type lowered_instruction =
  | L_add_imm of { rd : reg; rn : reg; imm : int64; shift12 : bool }
  | L_stp_pre of { rt : reg; rt2 : reg; rn : reg; offset : int64 }
  | L_movz of { rd : reg; imm16 : int64; hw : int }
  | L_ldr_uoff of { rt : reg; rn : reg; offset : int64 }
  | L_ret of { rn : reg }

let pp_lowered ppf = function
  (* [add xD, sp, #0] is spelled [mov xD, sp] by every A64 disassembler, and the
     canonical dump has to agree with objdump for the differential gate to mean
     anything. The alias lives here, in printing, and not in the lowered type -
     there is exactly one encoding and inventing a second constructor for it
     would let the two drift. *)
  | L_add_imm { rd; rn; imm; shift12 } ->
      if Int64.equal imm 0L && (not shift12) && (rd.is_sp || rn.is_sp) then
        Fmt.pf ppf "mov %a, %a" pp_reg rd pp_reg rn
      else
        Fmt.pf ppf "add %a, %a, #%Ld%s" pp_reg rd pp_reg rn imm
          (if shift12 then ", lsl #12" else "")
  | L_stp_pre { rt; rt2; rn; offset } ->
      Fmt.pf ppf "stp %a, %a, [%a, #%Ld]!" pp_reg rt pp_reg rt2 pp_reg rn offset
  | L_movz { rd; imm16; hw } ->
      Fmt.pf ppf "movz %a, #%Ld%s" pp_reg rd imm16
        (if hw = 0 then "" else Printf.sprintf ", lsl #%d" (hw * 16))
  | L_ldr_uoff { rt; rn; offset } -> Fmt.pf ppf "ldr %a, [%a, #%Ld]" pp_reg rt pp_reg rn offset
  | L_ret { rn } ->
      (* [ret x30] is spelled [ret]: x30 is the architectural default and every
         A64 disassembler elides it. Printing it would make the canonical dump
         disagree with objdump on a line where nothing differs. *)
      if rn.rnum = 30 && not rn.is_sp then Fmt.string ppf "ret" else Fmt.pf ppf "ret %a" pp_reg rn

let lowered_equal a b =
  match (a, b) with
  | L_add_imm x, L_add_imm y ->
      reg_equal x.rd y.rd && reg_equal x.rn y.rn && Int64.equal x.imm y.imm && x.shift12 = y.shift12
  | L_stp_pre x, L_stp_pre y ->
      reg_equal x.rt y.rt && reg_equal x.rt2 y.rt2 && reg_equal x.rn y.rn
      && Int64.equal x.offset y.offset
  | L_movz x, L_movz y -> reg_equal x.rd y.rd && Int64.equal x.imm16 y.imm16 && x.hw = y.hw
  | L_ldr_uoff x, L_ldr_uoff y ->
      reg_equal x.rt y.rt && reg_equal x.rn y.rn && Int64.equal x.offset y.offset
  | L_ret x, L_ret y -> reg_equal x.rn y.rn
  | _ -> false

type fixup_kind = Abs64 | Pcrel_b26 | Adrp_page

let fixup_kind_name = function
  | Abs64 -> "abs64"
  | Pcrel_b26 -> "pcrel-b26"
  | Adrp_page -> "adrp-page"

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
    ~encode:(fun (r : reg) -> Some (Int64.of_int r.rnum))
    ~decode:(fun v -> Some (reg_of_num ~width ~sp (Int64.to_int v)))
    (C.field ~width:5 name)

let codec : (lowered_instruction, fixup_kind) C.t =
  C.choice ~name:"aarch64"
    [
      (* Every A64 form has a distinct fixed-bit prefix, so unlike A32 and x86
         there is no overlap here for priority to resolve and [check] reports
         none. The priorities are still declared, because "no two forms overlap
         today" is a fact about these five forms and not a property of A64. *)
      C.alt ~label:"add-imm" ~priority:0
        (C.iso_fun ~name:"add-imm"
           ~encode:(function
             | L_add_imm { rd; rn; imm; shift12 } ->
                 if Int64.compare imm 0L < 0 || Int64.compare imm 4096L >= 0 then None
                 else Some ((), ((if shift12 then 1L else 0L), (imm, (rn, rd))))
             | _ -> None)
           ~decode:(fun ((), (sh, (imm, (rn, rd)))) ->
             Some (L_add_imm { rd; rn; imm; shift12 = Int64.equal sh 1L }))
           C.(
             const ~width:9 0b100100010L ** field ~width:1 "sh" ** field ~width:12 "imm12"
             ** reg_field ~width:64 ~sp:true "rn" ** reg_field ~width:64 ~sp:true "rd"));
      C.alt ~label:"stp-pre" ~priority:1
        (C.iso_fun ~name:"stp-pre"
           ~encode:(function
             | L_stp_pre { rt; rt2; rn; offset } -> Some ((), (offset, (rt2, (rn, rt)))) | _ -> None)
           ~decode:(fun ((), (offset, (rt2, (rn, rt)))) -> Some (L_stp_pre { rt; rt2; rn; offset }))
           C.(
             const ~width:10 0b1010100110L
             ** scaled ~shift:3 ~width:7 ~signedness:C.Signed "imm7"
             ** reg_field ~width:64 ~sp:false "rt2"
             ** reg_field ~width:64 ~sp:true "rn"
             ** reg_field ~width:64 ~sp:false "rt"));
      C.alt ~label:"movz" ~priority:2
        (C.iso_fun ~name:"movz"
           ~encode:(function
             | L_movz { rd; imm16; hw } ->
                 if Int64.compare imm16 0L < 0 || Int64.compare imm16 65536L >= 0 then None
                 else
                   Some ((if rd.rwidth = 64 then 1L else 0L), ((), (Int64.of_int hw, (imm16, rd))))
             | _ -> None)
           ~decode:(fun (sf, ((), (hw, (imm16, rd)))) ->
             let width = if Int64.equal sf 1L then 64 else 32 in
             Some (L_movz { rd = { rd with rwidth = width }; imm16; hw = Int64.to_int hw }))
           C.(
             field ~width:1 "sf" ** const ~width:8 0b10100101L ** field ~width:2 "hw"
             ** field ~width:16 "imm16"
             ** reg_field ~width:64 ~sp:false "rd"));
      C.alt ~label:"ldr-uoff" ~priority:3
        (C.iso_fun ~name:"ldr-uoff"
           ~encode:(function
             | L_ldr_uoff { rt; rn; offset } -> Some ((), (offset, (rn, rt))) | _ -> None)
           ~decode:(fun ((), (offset, (rn, rt))) -> Some (L_ldr_uoff { rt; rn; offset }))
           C.(
             const ~width:10 0b1111100101L
             ** scaled ~shift:3 ~width:12 ~signedness:C.Unsigned "imm12"
             ** reg_field ~width:64 ~sp:true "rn"
             ** reg_field ~width:64 ~sp:false "rt"));
      C.alt ~label:"ret" ~priority:4
        (C.iso_fun ~name:"ret"
           ~encode:(function L_ret { rn } -> Some ((), (rn, ())) | _ -> None)
           ~decode:(fun ((), (rn, ())) -> Some (L_ret { rn }))
           C.(
             const ~width:22 0b1101011001011111000000L
             ** reg_field ~width:64 ~sp:false "rn"
             ** const ~width:5 0L));
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
    match find_reg base with
    | Some r -> Ok (Op_mem { m_base = r; m_offset = offset; m_writeback = writeback; m_pre = true })
    | None -> bad ("unknown register " ^ base)
  in
  let int64_of v = match Bigint.to_int64_opt v with Some x -> Some x | None -> None in
  match List.map Token.kind slice with
  | [ Token.Ident n ] -> (
      match find_reg n with Some r -> Ok (Op_reg r) | None -> bad ("unknown register " ^ n))
  | [ Token.Immediate_sigil; Token.Int v ] -> Ok (Op_imm v)
  | [ Token.Immediate_sigil; Token.Minus; Token.Int v ] -> Ok (Op_imm (Bigint.neg v))
  | [
   Token.Ident (("lsl" | "lsr" | "asr" | "ror" | "uxtw" | "sxtw") as k);
   Token.Immediate_sigil;
   Token.Int v;
  ] -> (
      match Bigint.to_int_opt v with
      | Some n -> Ok (Op_shift { sh_kind = k; sh_amount = n })
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

let make_surface_instruction ~mnemonic ~origin ops =
  Ok { s_mnemonic = mnemonic; s_ops = ops; s_origin = origin }

(* {1 Simplify}

   Where the A64 aliases are resolved (§ M1.2), and where a trailing [lsl #0]
   stops being a separate operand. Both are surface facts: the machine has one
   [add] and one [movz], and a normalized AST that still carried the alias would
   make every consumer resolve it again. *)

let simplify_instruction ~features s =
  ignore features;
  let bad msg = Error (diag ~origin:s.s_origin "aarch64.simplify" msg) in
  let op =
    match s.s_mnemonic with
    | "add" -> Some Op_add
    | "mov" -> Some Op_mov
    | "movz" -> Some Op_movz
    | "stp" -> Some Op_stp
    | "ldr" -> Some Op_ldr
    | "ret" -> Some Op_ret
    | _ -> None
  in
  match op with
  | None -> bad (Printf.sprintf "unknown instruction %s" s.s_mnemonic)
  | Some op -> (
      (* A [lsl #0] modifier is dropped; a nonzero one is kept, because [movz]
         encodes it in [hw] and dropping it would silently change the value. *)
      match List.rev s.s_ops with
      | Op_shift { sh_kind = "lsl"; sh_amount = 0 } :: rest ->
          Ok { i_op = op; i_ops = List.rev rest }
      | _ -> Ok { i_op = op; i_ops = s.s_ops })

(* {1 Lower} *)

let lower_instruction state i =
  ignore state;
  let bad msg = Error (diag "aarch64.lower" msg) in
  let imm_of v =
    match Bigint.to_int64_opt v with
    | Some x -> Ok x
    | None -> Error (diag "aarch64.lower" "immediate does not fit 64 bits")
  in
  match (i.i_op, i.i_ops) with
  (* [mov xD, sp] and [mov sp, xS] are [add ..., #0]; [mov] between two general
     registers is [orr] with xzr, which no M1 fixture uses and which is
     therefore rejected rather than guessed. *)
  | Op_mov, [ Op_reg rd; Op_reg rn ] ->
      if rd.is_sp || rn.is_sp then Ok [ L_add_imm { rd; rn; imm = 0L; shift12 = false } ]
      else bad "mov between two general registers lowers to orr, which is not in M1 scope"
  | Op_add, [ Op_reg rd; Op_reg rn; Op_imm v ] -> (
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if Int64.compare imm 0L < 0 || Int64.compare imm 4096L >= 0 then
            bad "immediate does not fit the unshifted 12-bit field; lsl #12 is M2"
          else Ok [ L_add_imm { rd; rn; imm; shift12 = false } ])
  | Op_movz, [ Op_reg rd; Op_imm v ] -> (
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if Int64.compare imm 0L < 0 || Int64.compare imm 65536L >= 0 then
            bad "movz immediate does not fit 16 bits"
          else Ok [ L_movz { rd; imm16 = imm; hw = 0 } ])
  | Op_movz, [ Op_reg rd; Op_imm v; Op_shift { sh_kind = "lsl"; sh_amount } ] -> (
      match imm_of v with
      | Error e -> Error e
      | Ok imm ->
          if sh_amount mod 16 <> 0 || sh_amount / 16 > 3 then
            bad "movz shift must be 0, 16, 32 or 48"
          else if Int64.compare imm 0L < 0 || Int64.compare imm 65536L >= 0 then
            bad "movz immediate does not fit 16 bits"
          else Ok [ L_movz { rd; imm16 = imm; hw = sh_amount / 16 } ])
  | Op_stp, [ Op_reg rt; Op_reg rt2; Op_mem m ] ->
      if not m.m_writeback then bad "M1 supports only the pre-index writeback form of stp"
      else if not (Int64.equal (Int64.rem m.m_offset 8L) 0L) then
        bad "stp offset must be a multiple of eight"
      else Ok [ L_stp_pre { rt; rt2; rn = m.m_base; offset = m.m_offset } ]
  | Op_ldr, [ Op_reg rt; Op_mem m ] ->
      if m.m_writeback then bad "writeback ldr is not in M1 scope"
      else if Int64.compare m.m_offset 0L < 0 then
        bad "M1 supports only the unsigned-offset form of ldr"
      else if not (Int64.equal (Int64.rem m.m_offset 8L) 0L) then
        bad "ldr offset must be a multiple of eight for a 64-bit access"
      else Ok [ L_ldr_uoff { rt; rn = m.m_base; offset = m.m_offset } ]
  | Op_ret, [ Op_reg rn ] -> Ok [ L_ret { rn } ]
  | Op_ret, [] -> Ok [ L_ret { rn = { rnum = 30; rwidth = 64; is_sp = false } } ]
  | _ -> bad (Printf.sprintf "no %s form takes these operands" (opcode_name i.i_op))

(* {1 Encode and decode} *)

let word_to_memory s = if String.length s <> 4 then s else String.init 4 (fun i -> s.[3 - i])

let encode l =
  match C.encode codec l with
  | Error e -> Error (diag "aarch64.encode" (Fmt.to_to_string C.pp_error e))
  | Ok enc -> Ok (word_to_memory (C.Bits.to_bytes enc.C.bits), C.form_id enc, enc.C.placements)

type decode_context = { state : target_state; address : int64 }

let instruction_of_lowered = function
  | L_add_imm { rd; rn; imm; shift12 } ->
      if Int64.equal imm 0L && (not shift12) && (rd.is_sp || rn.is_sp) then
        Some { i_op = Op_mov; i_ops = [ Op_reg rd; Op_reg rn ] }
      else Some { i_op = Op_add; i_ops = [ Op_reg rd; Op_reg rn; Op_imm (Bigint.of_int64 imm) ] }
  | L_stp_pre { rt; rt2; rn; offset } ->
      Some
        {
          i_op = Op_stp;
          i_ops =
            [
              Op_reg rt;
              Op_reg rt2;
              Op_mem { m_base = rn; m_offset = offset; m_writeback = true; m_pre = true };
            ];
        }
  | L_movz { rd; imm16; hw } ->
      Some
        {
          i_op = Op_movz;
          i_ops =
            Op_reg rd
            :: Op_imm (Bigint.of_int64 imm16)
            :: (if hw = 0 then [] else [ Op_shift { sh_kind = "lsl"; sh_amount = hw * 16 } ]);
        }
  | L_ldr_uoff { rt; rn; offset } ->
      Some
        {
          i_op = Op_ldr;
          i_ops =
            [
              Op_reg rt;
              Op_mem { m_base = rn; m_offset = offset; m_writeback = false; m_pre = true };
            ];
        }
  | L_ret { rn } ->
      (* Same elision as [pp_lowered]: x30 is the architectural default, so
         [ret x30] denormalizes to the operandless [ret] that both objdump and
         the assembler's own [lower] accept. *)
      Some { i_op = Op_ret; i_ops = (if rn.rnum = 30 && not rn.is_sp then [] else [ Op_reg rn ]) }

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
  | Pcrel_b26 ->
      let d = Int64.sub target place in
      if Int64.rem d 4L <> 0L then Error (diag "aarch64.fixup" "branch target is not word-aligned")
      else Ok (Int64.div d 4L)
  | Adrp_page ->
      let page a = Int64.logand a (Int64.lognot 0xFFFL) in
      Ok (Int64.shift_right (Int64.sub (page target) (page place)) 12)

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

type register = reg
