(* The AArch64 front end: how A64 source text becomes operands.

   {!Aarch64_encode} is the rest of the target - the instruction types and
   everything that turns one into bytes - and this file is what {!Target.TARGET}
   adds on top of {!Target_encode.ENCODE}: a lexical profile, an operand parser,
   and a directive handler. The two are separate libraries so that a consumer
   which builds A64 instructions as values rather than reading them links the
   encoder alone, with no lexer and no Menhir table behind it.

   [include] rather than functor application: this module *is* the aarch64
   target, so its type identities have to be the encoder's own. Everything
   {!Aarch64_encode} exports is re-exported here, which is also what lets the
   parser below use [diag], [Reg], [Operand] and the rest without qualification. *)

open Foundation
include Aarch64_encode

(* The front end's error domain (asm/docs/errors.md). Separate from the
   encoder's because it may name {!Asm_syntax} and the encoder's may not - see
   {!Target_intf.Target.TARGET} for why that split exists. *)
type parse_error_kind =
  [ `Unknown_register of string
  | `Shift_amount_too_wide
  | `Offset_too_wide
  | `Cannot_parse_operand of cannot_parse
  | `Unbalanced_brackets ]

and cannot_parse = { slice : Asm_syntax.Token.slice; reason : Asm_syntax.Parse_lines.error_kind }

type parse_error = parse_error_kind Target_error.t

let pp_parse_error_kind ppf : parse_error_kind -> unit = function
  | `Unknown_register n -> Fmt.pf ppf "unknown register %s" n
  | `Shift_amount_too_wide -> Fmt.string ppf "shift amount does not fit"
  | `Offset_too_wide -> Fmt.string ppf "offset does not fit"
  | `Cannot_parse_operand { slice; _ } ->
      Fmt.pf ppf "cannot parse operand %s" (Asm_syntax.Token.slice_text slice)
  | `Unbalanced_brackets -> Fmt.string ppf "unbalanced brackets in operand"

let parse_error_kind_code : parse_error_kind -> string = function
  | `Unknown_register _ | `Shift_amount_too_wide | `Offset_too_wide | `Cannot_parse_operand _
  | `Unbalanced_brackets ->
      "aarch64.operand"

let pp_parse_error ppf e = pp_parse_error_kind ppf (Target_error.kind e)
let parse_error_code e = parse_error_kind_code (Target_error.kind e)

let parse_error_diagnostic e =
  Target_error.to_diagnostic ~code:parse_error_kind_code ~pp:pp_parse_error_kind e

let parse_diag ?pos ?origin kind =
  Err.Error.make ?pos ~pp_error:pp_parse_error
    (Target_error.make
       ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:"aarch64" ())
       kind)

(* {1 Lexical profile}

   [//] introduces a comment, so the profile's longest-first matching matters:
   a dialect that also used [/] would otherwise take the first character and
   leave the second as a divide. *)
let lexical_profile =
  Asm_syntax.Lexical_profile.make ~name:"aarch64" ~comment_introducers:[ "//" ]
    ~statement_separator:';' ~immediate_sigil:'#' ~modifier_syntax:[ "lo12" ] ()
(* {1 Operand parsing}

   Two things the common parser cannot do, and the reason §5.1's one-operand-
   at-a-time signature was replaced. First, [stp x15, x30, [sp, #-16]!] has a
   comma inside the bracketed operand, so the slices arrive split and are
   rejoined here by bracket depth. Second, the [!] belongs to the whole memory
   operand rather than to the token before it. *)

let regroup (slices : Asm_syntax.Token.slice list) =
  let depth_of slice =
    List.fold_left
      (fun d t ->
        match Asm_syntax.Token.kind t with
        | Asm_syntax.Token.Lbracket | Asm_syntax.Token.Lbrace -> d + 1
        | Asm_syntax.Token.Rbracket | Asm_syntax.Token.Rbrace -> d - 1
        | _ -> d)
      0 slice
  in
  (* The separator itself is gone: the grammar consumed every top-level comma
     as a slice boundary, so rejoining is concatenation and the rejoined operand
     reads [[ sp # 0 ]]. Re-inserting a comma token would be inventing source
     text, and the spans would then not be the lexer's. *)
  let rec go acc pending depth = function
    | [] -> if pending = [] then Ok (List.rev acc) else Error `Unbalanced_brackets
    | s :: rest ->
        let d = depth + depth_of s in
        let pending = pending @ s in
        if d = 0 then go (pending :: acc) [] 0 rest
        else if d < 0 then Error `Unbalanced_brackets
        else go acc pending d rest
  in
  go [] [] 0 slices

let slice_origin (s : Asm_syntax.Token.slice) =
  match Asm_syntax.Token.slice_span s with
  | Some sp -> Origin.text sp
  | None -> Origin.synthesized ~pass:"aarch64" ()

let parse_one (slice : Asm_syntax.Token.slice) =
  let origin = slice_origin slice in
  let bad kind = Error (parse_diag ~pos:__POS__ ~origin kind) in
  let open Asm_core in
  let open Asm_syntax in
  let mem ~base ~offset ~writeback =
    match Reg.find base with
    | Some r -> Ok (Operand.Mem { Mem.base = r; offset; writeback; pre = true })
    | None -> bad (`Unknown_register base)
  in
  let const_mem ~base ~offset ~writeback = mem ~base ~offset:(Disp.Const offset) ~writeback in
  let int64_of v = match Bigint.to_int64_opt v with Some x -> Some x | None -> None in
  let mem_reg ~base ~index ~extend ~amount =
    match (Reg.find base, Reg.find index) with
    | Some br, Some ir ->
        Ok
          (Operand.Mem
             {
               Mem.base = br;
               offset = Disp.Reg { index = ir; extend; amount };
               writeback = false;
               pre = true;
             })
    | None, _ -> bad (`Unknown_register base)
    | _, None -> bad (`Unknown_register index)
  in
  (* [#1.0000000], [#0.5000000], [#1.] (ARM's own trailing-dot-no-fraction
     spelling also lexes the same way here, even though this corpus does not
     use it) - the FP modified-immediate FMOV/FCMP take. Tokenized as
     [Int; Directive ".NNN"] or [Int; Dot] (no digits after), never a float
     token of its own: confirmed against the real lexer, since [.] only ever
     starts an identifier/directive-shaped token on this dialect (the same
     rule that makes [.L100] one token), not a numeric-literal one. *)
  let float_literal ~neg int_part frac =
    let s = Printf.sprintf "%s%s%s" (if neg then "-" else "") (Bigint.to_string int_part) frac in
    match float_of_string_opt s with Some f -> Ok (Operand.Fimm f) | None -> bad `Offset_too_wide
  in
  match List.map Token.kind slice with
  | [ Token.Ident n ] -> (
      (* A bare identifier is a register if the table knows it and a symbol
         otherwise. There is no sigil to tell them apart on this dialect, so
         the table is the only thing that can. [Freg] is checked first: [d0]-
         [d31]/[s0]-[s31] would otherwise fall to [Sym] (harmless for
         classify-c's parse-only goal, since a bare identifier always parses
         either way, but wrong for anything that lowers an [fmov]/[fcmp]
         operand). *)
      match Freg.find n with
      | Some r -> Ok (Operand.Freg r)
      | None -> (
          match Reg.find n with
          | Some r -> Ok (Operand.Reg r)
          | None -> Ok (Operand.Sym (Expr.Symbol n))))
  | [ Token.Immediate_sigil; Token.Int v ] -> Ok (Operand.Imm v)
  | [ Token.Immediate_sigil; Token.Minus; Token.Int v ] -> Ok (Operand.Imm (Bigint.neg v))
  | [ Token.Immediate_sigil; Token.Int i; Token.Directive frac ] -> float_literal ~neg:false i frac
  | [ Token.Immediate_sigil; Token.Int i; Token.Dot ] -> float_literal ~neg:false i "."
  | [ Token.Immediate_sigil; Token.Minus; Token.Int i; Token.Directive frac ] ->
      float_literal ~neg:true i frac
  | [ Token.Immediate_sigil; Token.Minus; Token.Int i; Token.Dot ] -> float_literal ~neg:true i "."
  (* [#:lo12:sym] as a plain (non-bracketed) operand - [add x9, x9,
     #:lo12:Te4]. Unlike the bracketed memory form below, the modifier stays
     on the expression here rather than being stripped: [lower_instruction]
     is what knows which instruction gets to keep it. *)
  | Token.Immediate_sigil :: Token.Modifier _ :: _ -> (
      match Asm_syntax.Parse_lines.parse_expression (List.tl slice) with
      | Ok e -> Ok (Operand.Sym e)
      | Error e -> bad (`Cannot_parse_operand { slice; reason = Err.Error.kind e }))
  (* Register-offset addressing - [ldr w4, [x9, w10, uxtw #2]],
     [ldrb w2, [x22, w5, uxtw]], [ldr x2, [x20, x1]]. Three shapes: index
     alone (bare [lsl], unscaled - the same encoding real [as] gives
     [[x20, x1, lsl #0]]... except unscaled, since no [#0] was written -
     verified byte-for-byte, see [Aarch64_encode.Disp.Reg]'s comment), index
     plus an extend keyword with no [#amount] (S=0), and index plus extend
     plus an explicit [#amount] (S=1). The extend keyword itself is not
     validated here - [lower_instruction] rejects an unknown one with the
     opcode/width diagnostic register-offset addressing already has. *)
  | [ Token.Lbracket; Token.Ident b; Token.Ident idx; Token.Rbracket ] ->
      mem_reg ~base:b ~index:idx ~extend:"lsl" ~amount:None
  | [ Token.Lbracket; Token.Ident b; Token.Ident idx; Token.Ident ext; Token.Rbracket ] ->
      mem_reg ~base:b ~index:idx ~extend:ext ~amount:None
  | [
   Token.Lbracket;
   Token.Ident b;
   Token.Ident idx;
   Token.Ident ext;
   Token.Immediate_sigil;
   Token.Int v;
   Token.Rbracket;
  ] -> (
      match Bigint.to_int_opt v with
      | Some n -> mem_reg ~base:b ~index:idx ~extend:ext ~amount:(Some n)
      | None -> bad `Shift_amount_too_wide)
  (* The shift keywords ([lsl]/[lsr]/[asr]/[ror]) are ADD/SUB (shifted
     register)'s operand; the extend keywords are the *other* ADD/SUB
     register form - extended register, e.g. [add x0, x0, x16, uxtx #0]
     (M5 corpus evidence: test/regression/int64.c, asm/docs/corpus.md).
     Both spell as [<keyword> #<amount>] at this token shape, so one operand
     type ([Operand.Shift]) carries either; {!Aarch64_encode.lower_instruction}
     is what tells them apart, by which table ([shift_of_name] vs. the
     extended-register option table) recognizes [kind]. *)
  | [
   Token.Ident
     (( "lsl" | "lsr" | "asr" | "ror" | "uxtb" | "uxth" | "uxtw" | "uxtx" | "sxtb" | "sxth" | "sxtw"
      | "sxtx" ) as k);
   Token.Immediate_sigil;
   Token.Int v;
  ] -> (
      match Bigint.to_int_opt v with
      | Some n -> Ok (Operand.Shift { Shift.kind = k; amount = n })
      | None -> bad `Shift_amount_too_wide)
  | [ Token.Lbracket; Token.Ident b; Token.Rbracket ] ->
      const_mem ~base:b ~offset:0L ~writeback:false
  | [ Token.Lbracket; Token.Ident b; Token.Rbracket; Token.Bang ] ->
      const_mem ~base:b ~offset:0L ~writeback:true
  | [ Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket ] -> (
      match int64_of v with
      | Some o -> const_mem ~base:b ~offset:o ~writeback:false
      | None -> bad `Offset_too_wide)
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket; Token.Bang;
  ] -> (
      match int64_of v with
      | Some o -> const_mem ~base:b ~offset:o ~writeback:true
      | None -> bad `Offset_too_wide)
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Minus; Token.Int v; Token.Rbracket;
  ] -> (
      match int64_of v with
      | Some o -> const_mem ~base:b ~offset:(Int64.neg o) ~writeback:false
      | None -> bad `Offset_too_wide)
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
      | None -> bad `Offset_too_wide)
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
      match Asm_syntax.Parse_lines.parse_expression inner with
      | Ok e -> mem ~base:b ~offset:(Disp.Sym e) ~writeback:false
      | Error e -> bad (`Cannot_parse_operand { slice; reason = Err.Error.kind e }))
  | _ -> (
      (* A branch or call target: not a register, an immediate or an address, but
         still an expression. The common parser decides what it means. *)
      match Asm_syntax.Parse_lines.parse_expression slice with
      | Ok e -> Ok (Operand.Sym e)
      | Error e -> bad (`Cannot_parse_operand { slice; reason = Err.Error.kind e }))

let parse_operands ~mnemonic slices =
  ignore mnemonic;
  match regroup slices with
  | Error kind -> Error (parse_diag ~pos:__POS__ kind)
  | Ok groups ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | s :: rest -> ( match parse_one s with Ok o -> go (o :: acc) rest | Error e -> Error e)
      in
      go [] groups

let handle_directive ~name ~argument state =
  ignore name;
  ignore argument;
  ignore state;
  (* A64 needs no target-state directive for the M1 fixtures: unlike ARM there
     is no [.syntax], no [.arch] and no [.fpu] in them. *)
  Target_intf.Target.Unhandled
