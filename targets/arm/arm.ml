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
   that have to agree eventually will not. [rname] holds the canonical spelling,
   which is what the disassembler prints; the alias is only an input spelling. *)

type reg = { rname : string; rnum : int }

let reg_equal a b = a.rnum = b.rnum
let pp_reg ppf r = Fmt.string ppf r.rname
let registers = List.init 16 (fun i -> { rname = Printf.sprintf "r%d" i; rnum = i })
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

let reg_of_num n = { rname = canonical_name (n land 15); rnum = n land 15 }

let find_reg name =
  match List.assoc_opt name aliases with
  | Some n -> Some (reg_of_num n)
  | None -> (
      match List.find_opt (fun r -> String.equal r.rname name) registers with
      | Some r -> Some (reg_of_num r.rnum)
      | None -> None)

(* {1 Operands} *)

type memory = { m_base : reg; m_offset : int64; m_writeback : bool; m_pre : bool }
type operand = Op_reg of reg | Op_imm of Bigint.t | Op_mem of memory

let pp_mem ppf m =
  let off = if Int64.equal m.m_offset 0L then "" else Printf.sprintf ", #%Ld" m.m_offset in
  if m.m_pre then Fmt.pf ppf "[%a%s]%s" pp_reg m.m_base off (if m.m_writeback then "!" else "")
  else Fmt.pf ppf "[%a]%s" pp_reg m.m_base off

let pp_operand ppf = function
  | Op_reg r -> pp_reg ppf r
  | Op_imm v -> Fmt.pf ppf "#%a" Bigint.pp v
  | Op_mem m -> pp_mem ppf m

type surface_operand = operand
type surface_instruction = { s_mnemonic : string; s_ops : operand list; s_origin : Origin.t }

let pp_surface ppf s =
  match s.s_ops with
  | [] -> Fmt.string ppf s.s_mnemonic
  | ops -> Fmt.pf ppf "%s %a" s.s_mnemonic Fmt.(list ~sep:(any ", ") pp_operand) ops

type opcode = Op_mov | Op_add | Op_sub | Op_str | Op_ldr | Op_bx

let opcode_name = function
  | Op_mov -> "mov"
  | Op_add -> "add"
  | Op_sub -> "sub"
  | Op_str -> "str"
  | Op_ldr -> "ldr"
  | Op_bx -> "bx"

type instruction = { i_op : opcode; i_ops : operand list }

let pp_instruction ppf i =
  match i.i_ops with
  | [] -> Fmt.string ppf (opcode_name i.i_op)
  | ops -> Fmt.pf ppf "%s %a" (opcode_name i.i_op) Fmt.(list ~sep:(any ", ") pp_operand) ops

(* {1 Lowered forms}

   Named after the *encoding class* rather than the mnemonic, because A32
   encodes add, sub and mov as one data-processing format distinguished by a
   4-bit opcode field. Three constructors named add/sub/mov would each have to
   carry that field anyway, and could then disagree with their own name. *)

type lowered_instruction =
  | L_dp_imm of { dp : int; s : bool; rd : reg; rn : reg; imm : int64 }
  | L_dp_reg of { dp : int; s : bool; rd : reg; rn : reg; rm : reg; sh_kind : int; sh_amt : int }
  | L_ldst_imm of { load : bool; rt : reg; rn : reg; offset : int64 }
  | L_bx of { rm : reg }

let dp_code = function Op_sub -> 2 | Op_add -> 4 | Op_mov -> 13 | _ -> -1
let dp_opcode = function 2 -> Some Op_sub | 4 -> Some Op_add | 13 -> Some Op_mov | _ -> None
let shift_name = function 0 -> "lsl" | 1 -> "lsr" | 2 -> "asr" | 3 -> "ror" | _ -> "?"

let pp_lowered ppf = function
  | L_dp_imm { dp; rd; rn; imm; _ } -> (
      match dp_opcode dp with
      | Some Op_mov -> Fmt.pf ppf "mov %a, #%Ld" pp_reg rd imm
      | Some o -> Fmt.pf ppf "%s %a, %a, #%Ld" (opcode_name o) pp_reg rd pp_reg rn imm
      | None -> Fmt.pf ppf "dp%d %a, %a, #%Ld" dp pp_reg rd pp_reg rn imm)
  | L_dp_reg { dp; rd; rn; rm; sh_kind; sh_amt; _ } -> (
      let sh =
        if sh_amt = 0 && sh_kind = 0 then ""
        else Printf.sprintf ", %s #%d" (shift_name sh_kind) sh_amt
      in
      match dp_opcode dp with
      | Some Op_mov -> Fmt.pf ppf "mov %a, %a%s" pp_reg rd pp_reg rm sh
      | Some o -> Fmt.pf ppf "%s %a, %a, %a%s" (opcode_name o) pp_reg rd pp_reg rn pp_reg rm sh
      | None -> Fmt.pf ppf "dp%d %a, %a, %a%s" dp pp_reg rd pp_reg rn pp_reg rm sh)
  | L_ldst_imm { load; rt; rn; offset } ->
      Fmt.pf ppf "%s %a, [%a, #%Ld]" (if load then "ldr" else "str") pp_reg rt pp_reg rn offset
  | L_bx { rm } -> Fmt.pf ppf "bx %a" pp_reg rm

let lowered_equal a b =
  match (a, b) with
  | L_dp_imm x, L_dp_imm y ->
      x.dp = y.dp && x.s = y.s && reg_equal x.rd y.rd && reg_equal x.rn y.rn
      && Int64.equal x.imm y.imm
  | L_dp_reg x, L_dp_reg y ->
      x.dp = y.dp && x.s = y.s && reg_equal x.rd y.rd && reg_equal x.rn y.rn && reg_equal x.rm y.rm
      && x.sh_kind = y.sh_kind && x.sh_amt = y.sh_amt
  | L_ldst_imm x, L_ldst_imm y ->
      x.load = y.load && reg_equal x.rt y.rt && reg_equal x.rn y.rn && Int64.equal x.offset y.offset
  | L_bx x, L_bx y -> reg_equal x.rm y.rm
  | _ -> false

type fixup_kind = Abs32 | Pcrel_b26

let fixup_kind_name = function Abs32 -> "abs32" | Pcrel_b26 -> "pcrel-b26"

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

let cond_al = C.const ~width:4 0xEL

let reg_field name =
  C.iso_fun ~name
    ~encode:(fun (r : reg) -> Some (Int64.of_int r.rnum))
    ~decode:(fun v -> Some (reg_of_num (Int64.to_int v)))
    (C.field ~width:4 name)

let codec : (lowered_instruction, fixup_kind) C.t =
  C.choice ~name:"arm"
    [
      (* [bx] first. Its pattern [0001 0010 1111 1111 1111 0001 Rm] is a
         specialization of the data-processing register format, so the two
         genuinely overlap; [check] says so and priority is what decides. A
         [bx] decoded as a data-processing instruction would print as a
         plausible [teq], which is the failure this ordering prevents. *)
      C.alt ~label:"bx" ~priority:0
        (C.iso_fun ~name:"bx"
           ~encode:(function L_bx { rm } -> Some ((), ((), rm)) | _ -> None)
           ~decode:(fun ((), ((), rm)) -> Some (L_bx { rm }))
           C.(cond_al ** const ~width:24 0x12FFF1L ** reg_field "rm"));
      C.alt ~label:"dp-imm" ~priority:1
        (C.iso_fun ~name:"dp-imm"
           ~encode:(function
             | L_dp_imm { dp; s; rd; rn; imm } ->
                 Some ((), ((), (Int64.of_int dp, ((if s then 1L else 0L), (rn, (rd, imm))))))
             | _ -> None)
           ~decode:(fun ((), ((), (dp, (s, (rn, (rd, imm)))))) ->
             Some (L_dp_imm { dp = Int64.to_int dp; s = Int64.equal s 1L; rd; rn; imm }))
           C.(
             cond_al ** const ~width:3 1L ** field ~width:4 "dp" ** field ~width:1 "s"
             ** reg_field "rn" ** reg_field "rd" ** modimm_codec));
      C.alt ~label:"dp-reg" ~priority:2
        (C.iso_fun ~name:"dp-reg"
           ~encode:(function
             | L_dp_reg { dp; s; rd; rn; rm; sh_kind; sh_amt } ->
                 Some
                   ( (),
                     ( (),
                       ( Int64.of_int dp,
                         ( (if s then 1L else 0L),
                           (rn, (rd, (Int64.of_int sh_amt, (Int64.of_int sh_kind, ((), rm))))) ) )
                     ) )
             | _ -> None)
           ~decode:(fun ((), ((), (dp, (s, (rn, (rd, (sh_amt, (sh_kind, ((), rm))))))))) ->
             Some
               (L_dp_reg
                  {
                    dp = Int64.to_int dp;
                    s = Int64.equal s 1L;
                    rd;
                    rn;
                    rm;
                    sh_kind = Int64.to_int sh_kind;
                    sh_amt = Int64.to_int sh_amt;
                  }))
           C.(
             cond_al ** const ~width:3 0L ** field ~width:4 "dp" ** field ~width:1 "s"
             ** reg_field "rn" ** reg_field "rd" ** field ~width:5 "shift-amount"
             ** field ~width:2 "shift-kind" ** const ~width:1 0L ** reg_field "rm"));
      (* Load and store, immediate offset. [U] is the sign of the offset and the
         12-bit field is its magnitude - A32 has no negative displacement field,
         so a signed field here would encode -4 as 0xFFC and address the wrong
         word. *)
      C.alt ~label:"ldst-imm" ~priority:3
        (C.iso_fun ~name:"ldst-imm"
           ~encode:(function
             | L_ldst_imm { load; rt; rn; offset } ->
                 let up = Int64.compare offset 0L >= 0 in
                 let mag = Int64.abs offset in
                 if Int64.compare mag 4096L >= 0 then None
                 else
                   Some
                     ( (),
                       ( (),
                         ( (),
                           ( (if up then 1L else 0L),
                             ((), ((), ((if load then 1L else 0L), (rn, (rt, mag))))) ) ) ) )
             | _ -> None)
           ~decode:(fun ((), ((), ((), (up, ((), ((), (load, (rn, (rt, mag))))))))) ->
             Some
               (L_ldst_imm
                  {
                    load = Int64.equal load 1L;
                    rt;
                    rn;
                    offset = (if Int64.equal up 1L then mag else Int64.neg mag);
                  }))
           C.(
             cond_al ** const ~width:3 2L
             ** const ~width:1 1L (* P: offset addressing, no writeback *)
             ** field ~width:1 "u"
             ** const ~width:1 0L (* B: word, not byte *)
             ** const ~width:1 0L (* W *) ** field ~width:1 "l"
             ** reg_field "rn" ** reg_field "rt" ** field ~width:12 "imm12"));
    ]

(* {1 Lexical profile}

   [@] introduces a comment and [#] an immediate - the exact inversion of x86,
   and the reason the profile exists at all. There is no register sigil, so
   [r12] and a symbol named [r12] are the same token and only this file can
   tell them apart. *)
let lexical_profile =
  Asm_core.Lexical_profile.make ~name:"arm" ~comment_introducers:[ "@" ] ~statement_separator:';'
    ~immediate_sigil:'#' ()

let name = "arm"
let triple = "arm-linux-gnueabihf"

let diag ?origin code message =
  Diagnostic.error ~code ~message
    ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:"arm" ())
    ()

(* {1 Operand parsing}

   The regrouping step is the reason §5.1's [parse_surface_operand] could not
   survive: the common parser splits a line at every top-level comma, but
   [str r12, [sp, #0]] has a comma *inside* a bracketed operand, so the slices
   it produces are [r12], [[sp] and [#0]]. Rejoining them needs bracket depth,
   which is a target fact - AArch64 and ARM bracket memory operands, x86
   parenthesizes them - so it belongs here and not in the grammar. *)

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
  | None -> Origin.synthesized ~pass:"arm" ()

let parse_one (slice : Asm_core.Token.slice) =
  let origin = slice_origin slice in
  let bad msg = Error (diag ~origin "arm.operand" msg) in
  let open Asm_core in
  let signed_int neg v =
    match Bigint.to_int64_opt v with
    | None -> None
    | Some x -> Some (if neg then Int64.neg x else x)
  in
  match List.map Token.kind slice with
  | [ Token.Ident n ] -> (
      match find_reg n with Some r -> Ok (Op_reg r) | None -> bad ("unknown register " ^ n))
  | [ Token.Immediate_sigil; Token.Int v ] -> Ok (Op_imm v)
  | [ Token.Immediate_sigil; Token.Minus; Token.Int v ] -> Ok (Op_imm (Bigint.neg v))
  | [ Token.Lbracket; Token.Ident b; Token.Rbracket ] -> (
      match find_reg b with
      | Some r -> Ok (Op_mem { m_base = r; m_offset = 0L; m_writeback = false; m_pre = true })
      | None -> bad ("unknown register " ^ b))
  | [ Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket ] -> (
      match (find_reg b, signed_int false v) with
      | Some r, Some off ->
          Ok (Op_mem { m_base = r; m_offset = off; m_writeback = false; m_pre = true })
      | None, _ -> bad ("unknown register " ^ b)
      | _, None -> bad "offset does not fit 64 bits")
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Minus; Token.Int v; Token.Rbracket;
  ] -> (
      match (find_reg b, signed_int true v) with
      | Some r, Some off ->
          Ok (Op_mem { m_base = r; m_offset = off; m_writeback = false; m_pre = true })
      | None, _ -> bad ("unknown register " ^ b)
      | _, None -> bad "offset does not fit 64 bits")
  | _ -> bad ("cannot parse operand " ^ Asm_core.Token.slice_text slice)

let parse_operands ~mnemonic slices =
  ignore mnemonic;
  match regroup slices with
  | Error m -> Error (diag "arm.operand" m)
  | Ok groups ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | s :: rest -> ( match parse_one s with Ok o -> go (o :: acc) rest | Error e -> Error e)
      in
      go [] groups

let make_surface_instruction ~mnemonic ~origin ops =
  Ok { s_mnemonic = mnemonic; s_ops = ops; s_origin = origin }

(* {1 Simplify} *)

let simplify_instruction ~features s =
  ignore features;
  let bad msg = Error (diag ~origin:s.s_origin "arm.simplify" msg) in
  let op =
    match s.s_mnemonic with
    | "mov" -> Some Op_mov
    | "add" -> Some Op_add
    | "sub" -> Some Op_sub
    | "str" -> Some Op_str
    | "ldr" -> Some Op_ldr
    | "bx" -> Some Op_bx
    | _ -> None
  in
  match op with
  | None -> bad (Printf.sprintf "unknown instruction %s" s.s_mnemonic)
  | Some op -> Ok { i_op = op; i_ops = s.s_ops }

(* {1 Lower} *)

let lower_instruction state i =
  let bad msg = Error (diag "arm.lower" msg) in
  if state.thumb then bad "Thumb is not in M1 scope"
  else
    let imm_of v =
      match Bigint.to_int64_opt v with
      | Some x -> Ok x
      | None -> Error (diag "arm.lower" "immediate does not fit 64 bits")
    in
    match (i.i_op, i.i_ops) with
    | Op_mov, [ Op_reg rd; Op_reg rm ] ->
        Ok
          [
            L_dp_reg
              { dp = dp_code Op_mov; s = false; rd; rn = reg_of_num 0; rm; sh_kind = 0; sh_amt = 0 };
          ]
    | Op_mov, [ Op_reg rd; Op_imm v ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm ->
            if encode_modimm imm = None then
              bad
                (Printf.sprintf
                   "#%Ld has no A32 modified-immediate representation; movw and mvn are M2" imm)
            else Ok [ L_dp_imm { dp = dp_code Op_mov; s = false; rd; rn = reg_of_num 0; imm } ])
    | ((Op_add | Op_sub) as op), [ Op_reg rd; Op_reg rn; Op_imm v ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm ->
            if encode_modimm imm = None then
              bad (Printf.sprintf "#%Ld has no A32 modified-immediate representation" imm)
            else Ok [ L_dp_imm { dp = dp_code op; s = false; rd; rn; imm } ])
    | ((Op_str | Op_ldr) as op), [ Op_reg rt; Op_mem m ] ->
        if m.m_writeback then bad "writeback addressing is not in M1 scope"
        else if Int64.compare (Int64.abs m.m_offset) 4096L >= 0 then
          bad "offset does not fit the 12-bit immediate"
        else Ok [ L_ldst_imm { load = op = Op_ldr; rt; rn = m.m_base; offset = m.m_offset } ]
    | Op_bx, [ Op_reg rm ] -> Ok [ L_bx { rm } ]
    | _ -> bad (Printf.sprintf "no %s form takes these operands" (opcode_name i.i_op))

(* {1 Encode and decode}

   A32 words are stored little-endian, and the codec produced them most
   significant bit first, so the four bytes are reversed exactly once, here. *)

let word_to_memory s = if String.length s <> 4 then s else String.init 4 (fun i -> s.[3 - i])

let encode l =
  match C.encode codec l with
  | Error e -> Error (diag "arm.encode" (Fmt.to_to_string C.pp_error e))
  | Ok enc -> Ok (word_to_memory (C.Bits.to_bytes enc.C.bits), C.form_id enc, enc.C.placements)

type decode_context = { state : target_state; address : int64 }

let instruction_of_lowered = function
  | L_dp_imm { dp; rd; rn; imm; _ } -> (
      match dp_opcode dp with
      | None -> None
      | Some Op_mov -> Some { i_op = Op_mov; i_ops = [ Op_reg rd; Op_imm (Bigint.of_int64 imm) ] }
      | Some o -> Some { i_op = o; i_ops = [ Op_reg rd; Op_reg rn; Op_imm (Bigint.of_int64 imm) ] })
  | L_dp_reg { dp; rd; rn; rm; _ } -> (
      match dp_opcode dp with
      | None -> None
      | Some Op_mov -> Some { i_op = Op_mov; i_ops = [ Op_reg rd; Op_reg rm ] }
      | Some o -> Some { i_op = o; i_ops = [ Op_reg rd; Op_reg rn; Op_reg rm ] })
  | L_ldst_imm { load; rt; rn; offset } ->
      Some
        {
          i_op = (if load then Op_ldr else Op_str);
          i_ops =
            [
              Op_reg rt;
              Op_mem { m_base = rn; m_offset = offset; m_writeback = false; m_pre = true };
            ];
        }
  | L_bx { rm } -> Some { i_op = Op_bx; i_ops = [ Op_reg rm ] }

let decode ctx bytes ~pos =
  ignore ctx;
  if String.length bytes - pos < 4 then Error (diag "arm.decode" "fewer than four bytes remain")
  else
    let word = word_to_memory (String.sub bytes pos 4) in
    match C.decode_bits codec (C.Bits.of_bytes word) with
    | None -> Error (diag "arm.decode" "no form matches this word")
    | Some d -> (
        match instruction_of_lowered d.C.value with
        | None -> Error (diag "arm.decode" "decoded a form with no normalized instruction")
        | Some i -> Ok (i, String.concat "." d.C.dform, 4))

(* {1 Fixups, padding, directives} *)

let evaluate_fixup kind ~place ~target =
  match kind with
  | Abs32 -> Ok (Int64.logand target 0xFFFFFFFFL)
  | Pcrel_b26 ->
      (* The branch offset is relative to the instruction address plus eight -
         the A32 pipeline offset - and is stored as a word count. *)
      let d = Int64.sub target (Int64.add place 8L) in
      if Int64.rem d 4L <> 0L then Error (diag "arm.fixup" "branch target is not word-aligned")
      else Ok (Int64.div d 4L)

(* A32 has an architectural no-op, so padding an executable section needs no
   table and no policy. A boundary that is not a multiple of four is rejected
   rather than padded with bytes, because a partial instruction is not a no-op. *)
let nop_bytes ~length =
  if length mod 4 <> 0 then
    Error (diag "arm.nop" "A32 padding must be a whole number of four-byte instructions")
  else Ok (String.concat "" (List.init (length / 4) (fun _ -> "\x00\xf0\x20\xe3")))

let handle_directive ~name ~argument state =
  match name with
  | ".syntax" -> Target_intf.Target.Handled { state = { state with syntax = argument }; emit = [] }
  | ".arch" -> Target_intf.Target.Handled { state = { state with arch = argument }; emit = [] }
  | ".fpu" -> Target_intf.Target.Handled { state = { state with fpu = argument }; emit = [] }
  | ".arm" -> Target_intf.Target.Handled { state = { state with thumb = false }; emit = [] }
  | ".thumb" -> Target_intf.Target.Rejected (diag "arm.directive" "Thumb is not in M1 scope")
  | ".eabi_attribute" ->
      (* Recorded as handled and otherwise ignored: it is an ELF attribute, and
         M1 produces no ELF. Rejecting it would fail every CompCert ARM file;
         treating it as unknown would be worse, because then a *misspelled*
         directive and this one would be indistinguishable. *)
      Target_intf.Target.Handled { state; emit = [] }
  | _ -> Target_intf.Target.Unhandled

type register = reg
