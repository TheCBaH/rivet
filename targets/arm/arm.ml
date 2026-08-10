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

module Operand = struct
  type t = Reg of Reg.t | Imm of Bigint.t | Mem of Mem.t

  let pp ppf = function
    | Reg r -> Reg.pp ppf r
    | Imm v -> Fmt.pf ppf "#%a" Bigint.pp v
    | Mem m -> Mem.pp ppf m
end

module Surface = struct
  type t = { mnemonic : string; ops : Operand.t list; origin : Origin.t }

  let pp ppf s =
    match s.ops with
    | [] -> Fmt.string ppf s.mnemonic
    | ops -> Fmt.pf ppf "%s %a" s.mnemonic Fmt.(list ~sep:(any ", ") Operand.pp) ops
end

module Opcode = struct
  type t = Mov | Add | Sub | Str | Ldr | Bx | Strb | Udf

  let name = function
    | Mov -> "mov"
    | Add -> "add"
    | Sub -> "sub"
    | Str -> "str"
    | Ldr -> "ldr"
    | Bx -> "bx"
    | Strb -> "strb"
    | Udf -> "udf"

  (* The surface spellings this target accepts. [simplify_instruction] consults
     this rather than repeating the list. *)
  let of_mnemonic = function
    | "mov" -> Some Mov
    | "add" -> Some Add
    | "sub" -> Some Sub
    | "str" -> Some Str
    | "ldr" -> Some Ldr
    | "bx" -> Some Bx
    | "strb" -> Some Strb
    | "udf" -> Some Udf
    | _ -> None

  (* A32 encodes add, sub and mov as one data-processing format distinguished
     by a 4-bit opcode field. The two directions live together so they cannot
     drift; [-1] is "this opcode is not a data-processing one". *)
  let to_dp = function Sub -> 2 | Add -> 4 | Mov -> 13 | _ -> -1
  let of_dp = function 2 -> Some Sub | 4 -> Some Add | 13 -> Some Mov | _ -> None
end

module Instruction = struct
  type t = { op : Opcode.t; ops : Operand.t list }

  let pp ppf i =
    match i.ops with
    | [] -> Fmt.string ppf (Opcode.name i.op)
    | ops -> Fmt.pf ppf "%s %a" (Opcode.name i.op) Fmt.(list ~sep:(any ", ") Operand.pp) ops
end

(* {1 Lowered forms}

   Named after the *encoding class* rather than the mnemonic, because A32
   encodes add, sub and mov as one data-processing format distinguished by a
   4-bit opcode field. Three constructors named add/sub/mov would each have to
   carry that field anyway, and could then disagree with their own name. *)

module Lowered = struct
  type t =
    | Dp_imm of { dp : int; s : bool; rd : Reg.t; rn : Reg.t; imm : int64 }
    | Dp_reg of {
        dp : int;
        s : bool;
        rd : Reg.t;
        rn : Reg.t;
        rm : Reg.t;
        sh_kind : int;
        sh_amt : int;
      }
    | Ldst_imm of { load : bool; byte : bool; rt : Reg.t; rn : Reg.t; offset : int64 }
    | Bx of { rm : Reg.t }
    | Udf of { imm16 : int64 }

  let shift_name = function 0 -> "lsl" | 1 -> "lsr" | 2 -> "asr" | 3 -> "ror" | _ -> "?"

  let pp ppf = function
    | Dp_imm { dp; rd; rn; imm; _ } -> (
        match Opcode.of_dp dp with
        | Some Opcode.Mov -> Fmt.pf ppf "mov %a, #%Ld" Reg.pp rd imm
        | Some o -> Fmt.pf ppf "%s %a, %a, #%Ld" (Opcode.name o) Reg.pp rd Reg.pp rn imm
        | None -> Fmt.pf ppf "dp%d %a, %a, #%Ld" dp Reg.pp rd Reg.pp rn imm)
    | Dp_reg { dp; rd; rn; rm; sh_kind; sh_amt; _ } -> (
        let sh =
          if sh_amt = 0 && sh_kind = 0 then ""
          else Printf.sprintf ", %s #%d" (shift_name sh_kind) sh_amt
        in
        match Opcode.of_dp dp with
        | Some Opcode.Mov -> Fmt.pf ppf "mov %a, %a%s" Reg.pp rd Reg.pp rm sh
        | Some o -> Fmt.pf ppf "%s %a, %a, %a%s" (Opcode.name o) Reg.pp rd Reg.pp rn Reg.pp rm sh
        | None -> Fmt.pf ppf "dp%d %a, %a, %a%s" dp Reg.pp rd Reg.pp rn Reg.pp rm sh)
    | Ldst_imm { load; byte; rt; rn; offset } ->
        Fmt.pf ppf "%s%s %a, [%a, #%Ld]"
          (if load then "ldr" else "str")
          (if byte then "b" else "")
          Reg.pp rt Reg.pp rn offset
    | Bx { rm } -> Fmt.pf ppf "bx %a" Reg.pp rm
    | Udf { imm16 } -> Fmt.pf ppf "udf #%Ld" imm16

  let equal a b =
    match (a, b) with
    | Dp_imm x, Dp_imm y ->
        x.dp = y.dp && x.s = y.s && Reg.equal x.rd y.rd && Reg.equal x.rn y.rn
        && Int64.equal x.imm y.imm
    | Dp_reg x, Dp_reg y ->
        x.dp = y.dp && x.s = y.s && Reg.equal x.rd y.rd && Reg.equal x.rn y.rn
        && Reg.equal x.rm y.rm && x.sh_kind = y.sh_kind && x.sh_amt = y.sh_amt
    | Ldst_imm x, Ldst_imm y ->
        x.load = y.load && x.byte = y.byte && Reg.equal x.rt y.rt && Reg.equal x.rn y.rn
        && Int64.equal x.offset y.offset
    | Bx x, Bx y -> Reg.equal x.rm y.rm
    | Udf x, Udf y -> Int64.equal x.imm16 y.imm16
    | _ -> false
end

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
    ~encode:(fun (r : Reg.t) -> Some (Int64.of_int r.num))
    ~decode:(fun v -> Some (Reg.of_num (Int64.to_int v)))
    (C.field ~width:4 name)

let codec : (Lowered.t, fixup_kind) C.t =
  C.choice ~name:"arm"
    [
      (* [bx] first. Its pattern [0001 0010 1111 1111 1111 0001 Rm] is a
         specialization of the data-processing register format, so the two
         genuinely overlap; [check] says so and priority is what decides. A
         [bx] decoded as a data-processing instruction would print as a
         plausible [teq], which is the failure this ordering prevents. *)
      C.alt ~label:"bx" ~priority:0
        (C.iso_fun ~name:"bx"
           ~encode:(function Lowered.Bx { rm } -> Some ((), ((), rm)) | _ -> None)
           ~decode:(fun ((), ((), rm)) -> Some (Lowered.Bx { rm }))
           C.(cond_al ** const ~width:24 0x12FFF1L ** reg_field "rm"));
      C.alt ~label:"dp-imm" ~priority:1
        (C.iso_fun ~name:"dp-imm"
           ~encode:(function
             | Lowered.Dp_imm { dp; s; rd; rn; imm } ->
                 Some ((), ((), (Int64.of_int dp, ((if s then 1L else 0L), (rn, (rd, imm))))))
             | _ -> None)
           ~decode:(fun ((), ((), (dp, (s, (rn, (rd, imm)))))) ->
             Some (Lowered.Dp_imm { dp = Int64.to_int dp; s = Int64.equal s 1L; rd; rn; imm }))
           C.(
             cond_al ** const ~width:3 1L ** field ~width:4 "dp" ** field ~width:1 "s"
             ** reg_field "rn" ** reg_field "rd" ** modimm_codec));
      C.alt ~label:"dp-reg" ~priority:2
        (C.iso_fun ~name:"dp-reg"
           ~encode:(function
             | Lowered.Dp_reg { dp; s; rd; rn; rm; sh_kind; sh_amt } ->
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
               (Lowered.Dp_reg
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
             | Lowered.Ldst_imm { load; byte; rt; rn; offset } ->
                 let up = Int64.compare offset 0L >= 0 in
                 let mag = Int64.abs offset in
                 if Int64.compare mag 4096L >= 0 then None
                 else
                   Some
                     ( (),
                       ( (),
                         ( (),
                           ( (if up then 1L else 0L),
                             ( (if byte then 1L else 0L),
                               ((), ((if load then 1L else 0L), (rn, (rt, mag)))) ) ) ) ) )
             | _ -> None)
           ~decode:(fun ((), ((), ((), (up, (byte, ((), (load, (rn, (rt, mag))))))))) ->
             Some
               (Lowered.Ldst_imm
                  {
                    load = Int64.equal load 1L;
                    byte = Int64.equal byte 1L;
                    rt;
                    rn;
                    offset = (if Int64.equal up 1L then mag else Int64.neg mag);
                  }))
           C.(
             cond_al ** const ~width:3 2L
             ** const ~width:1 1L (* P: offset addressing, no writeback *)
             ** field ~width:1 "u" ** field ~width:1 "b" ** const ~width:1 0L (* W *)
             ** field ~width:1 "l" ** reg_field "rn" ** reg_field "rt" ** field ~width:12 "imm12"));
      (* [udf], the permanently-undefined encoding. Its fixed top nibble is
         literally [cond_al]'s bit pattern - not a coincidence to route around,
         but where ARM parked this encoding. *)
      C.alt ~label:"udf" ~priority:4
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
             cond_al ** const ~width:8 0b01111111L ** field ~width:12 "imm12"
             ** const ~width:4 0b1111L ** field ~width:4 "imm4"));
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
      match Reg.find n with Some r -> Ok (Operand.Reg r) | None -> bad ("unknown register " ^ n))
  | [ Token.Immediate_sigil; Token.Int v ] -> Ok (Operand.Imm v)
  | [ Token.Immediate_sigil; Token.Minus; Token.Int v ] -> Ok (Operand.Imm (Bigint.neg v))
  | [ Token.Lbracket; Token.Ident b; Token.Rbracket ] -> (
      match Reg.find b with
      | Some r -> Ok (Operand.Mem { base = r; offset = 0L; writeback = false; pre = true })
      | None -> bad ("unknown register " ^ b))
  | [ Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket ] -> (
      match (Reg.find b, signed_int false v) with
      | Some r, Some off ->
          Ok (Operand.Mem { base = r; offset = off; writeback = false; pre = true })
      | None, _ -> bad ("unknown register " ^ b)
      | _, None -> bad "offset does not fit 64 bits")
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Minus; Token.Int v; Token.Rbracket;
  ] -> (
      match (Reg.find b, signed_int true v) with
      | Some r, Some off ->
          Ok (Operand.Mem { base = r; offset = off; writeback = false; pre = true })
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

let make_surface_instruction ~mnemonic ~origin ops = Ok { Surface.mnemonic; ops; origin }

(* {1 Simplify} *)

let simplify_instruction ~features s =
  ignore features;
  let bad msg = Error (diag ~origin:s.Surface.origin "arm.simplify" msg) in
  match Opcode.of_mnemonic s.Surface.mnemonic with
  | None -> bad (Printf.sprintf "unknown instruction %s" s.Surface.mnemonic)
  | Some op -> Ok { Instruction.op; ops = s.Surface.ops }

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
    match (i.Instruction.op, i.Instruction.ops) with
    | Opcode.Mov, [ Operand.Reg rd; Operand.Reg rm ] ->
        Ok
          [
            Lowered.Dp_reg
              {
                dp = Opcode.to_dp Opcode.Mov;
                s = false;
                rd;
                rn = Reg.of_num 0;
                rm;
                sh_kind = 0;
                sh_amt = 0;
              };
          ]
    | Opcode.Mov, [ Operand.Reg rd; Operand.Imm v ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm ->
            if encode_modimm imm = None then
              bad
                (Printf.sprintf
                   "#%Ld has no A32 modified-immediate representation; movw and mvn are M2" imm)
            else
              Ok
                [
                  Lowered.Dp_imm
                    { dp = Opcode.to_dp Opcode.Mov; s = false; rd; rn = Reg.of_num 0; imm };
                ])
    | ((Opcode.Add | Opcode.Sub) as op), [ Operand.Reg rd; Operand.Reg rn; Operand.Imm v ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm ->
            if encode_modimm imm = None then
              bad (Printf.sprintf "#%Ld has no A32 modified-immediate representation" imm)
            else Ok [ Lowered.Dp_imm { dp = Opcode.to_dp op; s = false; rd; rn; imm } ])
    | ((Opcode.Str | Opcode.Ldr) as op), [ Operand.Reg rt; Operand.Mem m ] ->
        if m.writeback then bad "writeback addressing is not in M1 scope"
        else if Int64.compare (Int64.abs m.offset) 4096L >= 0 then
          bad "offset does not fit the 12-bit immediate"
        else
          Ok
            [
              Lowered.Ldst_imm
                { load = op = Opcode.Ldr; byte = false; rt; rn = m.base; offset = m.offset };
            ]
    | Opcode.Strb, [ Operand.Reg rt; Operand.Mem m ] ->
        if m.writeback then bad "writeback addressing is not in M1 scope"
        else if Int64.compare (Int64.abs m.offset) 4096L >= 0 then
          bad "offset does not fit the 12-bit immediate"
        else
          Ok [ Lowered.Ldst_imm { load = false; byte = true; rt; rn = m.base; offset = m.offset } ]
    | Opcode.Bx, [ Operand.Reg rm ] -> Ok [ Lowered.Bx { rm } ]
    | Opcode.Udf, [ Operand.Imm v ] -> (
        match imm_of v with
        | Error e -> Error e
        | Ok imm ->
            if Int64.compare imm 0L < 0 || Int64.compare imm 65536L >= 0 then
              bad "udf immediate does not fit 16 bits"
            else Ok [ Lowered.Udf { imm16 = imm } ])
    | _ -> bad (Printf.sprintf "no %s form takes these operands" (Opcode.name i.Instruction.op))

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
  | Lowered.Dp_imm { dp; rd; rn; imm; _ } -> (
      match Opcode.of_dp dp with
      | None -> None
      | Some Opcode.Mov ->
          Some
            {
              Instruction.op = Opcode.Mov;
              ops = [ Operand.Reg rd; Operand.Imm (Bigint.of_int64 imm) ];
            }
      | Some o ->
          Some
            {
              Instruction.op = o;
              ops = [ Operand.Reg rd; Operand.Reg rn; Operand.Imm (Bigint.of_int64 imm) ];
            })
  | Lowered.Dp_reg { dp; rd; rn; rm; _ } -> (
      match Opcode.of_dp dp with
      | None -> None
      | Some Opcode.Mov ->
          Some { Instruction.op = Opcode.Mov; ops = [ Operand.Reg rd; Operand.Reg rm ] }
      | Some o ->
          Some { Instruction.op = o; ops = [ Operand.Reg rd; Operand.Reg rn; Operand.Reg rm ] })
  | Lowered.Ldst_imm { load; byte; rt; rn; offset } -> (
      let mem_op = Operand.Mem { base = rn; offset; writeback = false; pre = true } in
      match (load, byte) with
      | false, false -> Some { Instruction.op = Opcode.Str; ops = [ Operand.Reg rt; mem_op ] }
      | true, false -> Some { Instruction.op = Opcode.Ldr; ops = [ Operand.Reg rt; mem_op ] }
      | false, true -> Some { Instruction.op = Opcode.Strb; ops = [ Operand.Reg rt; mem_op ] }
      | true, true -> None
      (* ldrb decodes correctly but has no M1 opcode to normalize into. *))
  | Lowered.Bx { rm } -> Some { Instruction.op = Opcode.Bx; ops = [ Operand.Reg rm ] }
  | Lowered.Udf { imm16 } ->
      Some { Instruction.op = Opcode.Udf; ops = [ Operand.Imm (Bigint.of_int64 imm16) ] }

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
