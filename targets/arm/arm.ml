(* The ARM front end: how A32 source text becomes operands.

   {!Arm_encode} is the rest of the target - the instruction types and
   everything that turns one into bytes - and this file is what {!Target.TARGET}
   adds on top of {!Target_encode.ENCODE}: a lexical profile, an operand parser,
   and a directive handler. The two are separate libraries so that a consumer
   which builds A32 instructions as values rather than reading them links the
   encoder alone, with no lexer and no Menhir table behind it.

   [include] rather than functor application: this module *is* the arm target,
   so its type identities have to be the encoder's own. Everything
   {!Arm_encode} exports is re-exported here, which is also what lets the parser
   below use [diag], [shift_of_name], [Reg] and the rest without qualification. *)

open Foundation
include Arm_encode

(* The front end's error domain (asm/docs/errors.md). Separate from the
   encoder's because it may name {!Asm_syntax} and the encoder's may not - see
   {!Target_intf.Target.TARGET} for why that split exists. *)
type parse_error_kind =
  [ `Unknown_register of string
  | `Unknown_shift of string
  | `Offset_too_wide
  | `Shift_amount_out_of_range of shift_amount_out_of_range
  | `Shift_amount_too_wide
  | `Cannot_parse_operand of cannot_parse
  | `Unbalanced_brackets
  | `Bad_float_literal of string
  | `Thumb_directive_out_of_scope ]

and shift_amount_out_of_range = { amount : int; max : int }
and cannot_parse = { slice : Asm_syntax.Token.slice; reason : Asm_syntax.Parse_lines.error_kind }

type parse_error = parse_error_kind Target_error.t

let pp_parse_error_kind ppf : parse_error_kind -> unit = function
  | `Unknown_register n -> Fmt.pf ppf "unknown register %s" n
  | `Unknown_shift s -> Fmt.pf ppf "unknown shift %s" s
  | `Offset_too_wide -> Fmt.string ppf "offset does not fit 64 bits"
  | `Shift_amount_out_of_range { max; _ } -> Fmt.pf ppf "shift amount must be 0 to %d" max
  | `Shift_amount_too_wide -> Fmt.string ppf "shift amount does not fit"
  | `Cannot_parse_operand { slice; _ } ->
      Fmt.pf ppf "cannot parse operand %s" (Asm_syntax.Token.slice_text slice)
  | `Unbalanced_brackets -> Fmt.string ppf "unbalanced brackets in operand"
  | `Bad_float_literal s -> Fmt.pf ppf "cannot parse floating-point literal %s" s
  | `Thumb_directive_out_of_scope -> Fmt.string ppf "Thumb is not in M1 scope"

let parse_error_kind_code : parse_error_kind -> string = function
  | `Thumb_directive_out_of_scope -> "arm.directive"
  | `Unknown_register _ | `Unknown_shift _ | `Offset_too_wide | `Shift_amount_out_of_range _
  | `Shift_amount_too_wide | `Cannot_parse_operand _ | `Unbalanced_brackets | `Bad_float_literal _
    ->
      "arm.operand"

let pp_parse_error ppf e = pp_parse_error_kind ppf (Target_error.kind e)
let parse_error_code e = parse_error_kind_code (Target_error.kind e)

let parse_error_diagnostic e =
  Target_error.to_diagnostic ~code:parse_error_kind_code ~pp:pp_parse_error_kind e

let parse_diag ?pos ?origin kind =
  Err.Error.make ?pos ~pp_error:pp_parse_error
    (Target_error.make
       ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:"arm" ())
       kind)

(* {1 Lexical profile}

   [@] introduces a comment and [#] an immediate - the exact inversion of x86,
   and the reason the profile exists at all. There is no register sigil, so
   [r12] and a symbol named [r12] are the same token and only this file can
   tell them apart. *)
let lexical_profile =
  Asm_syntax.Lexical_profile.make ~name:"arm" ~comment_introducers:[ "@" ] ~statement_separator:';'
    ~immediate_sigil:'#' ~modifier_syntax:[ "lower16"; "upper16" ] ()

(* {1 Operand parsing}

   The regrouping step is the reason §5.1's [parse_surface_operand] could not
   survive: the common parser splits a line at every top-level comma, but
   [str r12, [sp, #0]] has a comma *inside* a bracketed operand, so the slices
   it produces are [r12], [[sp] and [#0]]. Rejoining them needs bracket depth,
   which is a target fact - AArch64 and ARM bracket memory operands, x86
   parenthesizes them - so it belongs here and not in the grammar. *)

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
  | None -> Origin.synthesized ~pass:"arm" ()

let parse_one (slice : Asm_syntax.Token.slice) =
  let origin = slice_origin slice in
  let bad kind = Error (parse_diag ~pos:__POS__ ~origin kind) in
  let open Asm_syntax in
  let signed_int neg v =
    match Bigint.to_int64_opt v with
    | None -> None
    | Some x -> Some (if neg then Int64.neg x else x)
  in
  (* [whole.frac] is one exact decimal literal, e.g. [#1.5]/[#1.]/[#-0.5]: the
     lexer has no float token, so a fractional VFP immediate arrives as an
     [Int] followed by either a bare [Dot] ([1.], no digits after the point)
     or a [Directive] token ([.5] - the lexer reads any [.]-led run of
     identifier-ish characters as a directive name, having no way to know
     this one is not [.text]). [Directive]'s payload keeps the leading dot,
     so concatenating it straight onto the whole part's digits is enough. *)
  let float_lit ~negate whole frac =
    let s = (if negate then "-" else "") ^ Bigint.to_string whole ^ frac in
    match float_of_string_opt s with
    | Some v -> Ok (Operand.FImm v)
    | None -> bad (`Bad_float_literal s)
  in
  (* [#2.0e+0]/[#5.0e-1]/... - every VFP immediate M5 classify-c-gcc corpus
     evidence actually has is scientific notation (gcc's own printer, never
     [float_lit]'s bare [#1.5]/[#1.] shape above), tokenized as
     [Int; Directive ".Ne"; (Plus|Minus); Int] exactly like AArch64's own
     [float_literal_exp] gap (aarch64.ml) - the [Directive] already ends in
     the bare [e] the lexer's directive-name scan swallowed, so appending the
     sign and exponent digits straight onto [frac] reproduces the string
     [float_of_string] expects. *)
  let float_lit_exp ~negate whole frac ~exp_negate exp =
    let s =
      (if negate then "-" else "")
      ^ Bigint.to_string whole ^ frac
      ^ (if exp_negate then "-" else "+")
      ^ Bigint.to_string exp
    in
    match float_of_string_opt s with
    | Some v -> Ok (Operand.FImm v)
    | None -> bad (`Bad_float_literal s)
  in
  match List.map Token.kind slice with
  | [ Token.Ident n ] -> (
      (* A register if a table knows the name, a symbol otherwise: A32 has no
         register sigil, so nothing else can tell them apart. GPR first, then
         the two VFP register files - none of the three namespaces overlap. *)
      match Reg.find n with
      | Some r -> Ok (Operand.Reg r)
      | None -> (
          match Dreg.find n with
          | Some r -> Ok (Operand.Dreg r)
          | None -> (
              match Sreg.find n with
              | Some r -> Ok (Operand.Sreg r)
              | None -> Ok (Operand.Sym (Asm_core.Expr.Symbol n)))))
  | [ Token.Immediate_sigil; Token.Int v ] -> Ok (Operand.Imm v)
  | [ Token.Immediate_sigil; Token.Minus; Token.Int v ] -> Ok (Operand.Imm (Bigint.neg v))
  | [ Token.Immediate_sigil; Token.Int whole; Token.Dot ] -> float_lit ~negate:false whole "."
  | [ Token.Immediate_sigil; Token.Int whole; Token.Directive frac ] ->
      float_lit ~negate:false whole frac
  | [ Token.Immediate_sigil; Token.Minus; Token.Int whole; Token.Dot ] ->
      float_lit ~negate:true whole "."
  | [ Token.Immediate_sigil; Token.Minus; Token.Int whole; Token.Directive frac ] ->
      float_lit ~negate:true whole frac
  | [
   Token.Immediate_sigil;
   Token.Int whole;
   Token.Directive frac;
   ((Token.Plus | Token.Minus) as sign);
   Token.Int exp;
  ] ->
      float_lit_exp ~negate:false whole frac ~exp_negate:(sign = Token.Minus) exp
  | [
   Token.Immediate_sigil;
   Token.Minus;
   Token.Int whole;
   Token.Directive frac;
   ((Token.Plus | Token.Minus) as sign);
   Token.Int exp;
  ] ->
      float_lit_exp ~negate:true whole frac ~exp_negate:(sign = Token.Minus) exp
  | [ Token.Lbracket; Token.Ident b; Token.Rbracket ] -> (
      match Reg.find b with
      | Some r -> Ok (Operand.Mem { base = r; offset = Mem.Imm 0L; writeback = false; pre = true })
      | None -> bad (`Unknown_register b))
  | [ Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket ] -> (
      match (Reg.find b, signed_int false v) with
      | Some r, Some off ->
          Ok (Operand.Mem { base = r; offset = Mem.Imm off; writeback = false; pre = true })
      | None, _ -> bad (`Unknown_register b)
      | _, None -> bad `Offset_too_wide)
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Minus; Token.Int v; Token.Rbracket;
  ] -> (
      match (Reg.find b, signed_int true v) with
      | Some r, Some off ->
          Ok (Operand.Mem { base = r; offset = Mem.Imm off; writeback = false; pre = true })
      | None, _ -> bad (`Unknown_register b)
      | _, None -> bad `Offset_too_wide)
  (* [Rn, #imm]! / [Rn, #-imm]! - pre-indexed with writeback (M5 corpus
     evidence: asm/docs/corpus.md's classify-c-gcc, gcc's own frame-pointer
     prologue `str fp, [sp, #-4]!`). The trailing [!] is the same [Token.Bang]
     the lexer already produces elsewhere; nothing splits it from the bracket
     group before it since no comma separates them, so it just extends this
     operand's slice by one token past the already-supported non-writeback
     patterns just above. *)
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket; Token.Bang;
  ] -> (
      match (Reg.find b, signed_int false v) with
      | Some r, Some off ->
          Ok (Operand.Mem { base = r; offset = Mem.Imm off; writeback = true; pre = true })
      | None, _ -> bad (`Unknown_register b)
      | _, None -> bad `Offset_too_wide)
  | [
   Token.Lbracket;
   Token.Ident b;
   Token.Immediate_sigil;
   Token.Minus;
   Token.Int v;
   Token.Rbracket;
   Token.Bang;
  ] -> (
      match (Reg.find b, signed_int true v) with
      | Some r, Some off ->
          Ok (Operand.Mem { base = r; offset = Mem.Imm off; writeback = true; pre = true })
      | None, _ -> bad (`Unknown_register b)
      | _, None -> bad `Offset_too_wide)
  (* Register-offset addressing: [ldr r2, [r1, r2]] / [ldrb r2, [r7, r0]], no
     shift. *)
  | [ Token.Lbracket; Token.Ident b; Token.Ident idx; Token.Rbracket ] -> (
      match (Reg.find b, Reg.find idx) with
      | Some br, Some ir ->
          Ok
            (Operand.Mem
               {
                 base = br;
                 offset = Mem.Reg_offset { reg = ir; negate = false; kind = 0; amount = 0 };
                 writeback = false;
                 pre = true;
               })
      | None, _ -> bad (`Unknown_register b)
      | _, None -> bad (`Unknown_register idx))
  (* Register-offset, negated: [ldr r2, [r1, -r2]]. *)
  | [ Token.Lbracket; Token.Ident b; Token.Minus; Token.Ident idx; Token.Rbracket ] -> (
      match (Reg.find b, Reg.find idx) with
      | Some br, Some ir ->
          Ok
            (Operand.Mem
               {
                 base = br;
                 offset = Mem.Reg_offset { reg = ir; negate = true; kind = 0; amount = 0 };
                 writeback = false;
                 pre = true;
               })
      | None, _ -> bad (`Unknown_register b)
      | _, None -> bad (`Unknown_register idx))
  (* Shifted register-offset: [ldr r2, [r1, r2, lsl #2]]. *)
  | [
   Token.Lbracket;
   Token.Ident b;
   Token.Ident idx;
   Token.Ident sh;
   Token.Immediate_sigil;
   Token.Int v;
   Token.Rbracket;
  ] -> (
      match (Reg.find b, Reg.find idx, shift_of_name sh, Bigint.to_int_opt v) with
      | Some br, Some ir, Some kind, Some amount ->
          if amount < 0 || amount > 31 then bad (`Shift_amount_out_of_range { amount; max = 31 })
          else
            Ok
              (Operand.Mem
                 {
                   base = br;
                   offset = Mem.Reg_offset { reg = ir; negate = false; kind; amount };
                   writeback = false;
                   pre = true;
                 })
      | None, _, _, _ -> bad (`Unknown_register b)
      | _, None, _, _ -> bad (`Unknown_register idx)
      | _, _, None, _ -> bad (`Unknown_shift sh)
      | _, _, _, None -> bad `Shift_amount_too_wide)
  (* Shifted register-offset, negated: [ldr r2, [r1, -r2, lsl #3]]. *)
  | [
   Token.Lbracket;
   Token.Ident b;
   Token.Minus;
   Token.Ident idx;
   Token.Ident sh;
   Token.Immediate_sigil;
   Token.Int v;
   Token.Rbracket;
  ] -> (
      match (Reg.find b, Reg.find idx, shift_of_name sh, Bigint.to_int_opt v) with
      | Some br, Some ir, Some kind, Some amount ->
          if amount < 0 || amount > 31 then bad (`Shift_amount_out_of_range { amount; max = 31 })
          else
            Ok
              (Operand.Mem
                 {
                   base = br;
                   offset = Mem.Reg_offset { reg = ir; negate = true; kind; amount };
                   writeback = false;
                   pre = true;
                 })
      | None, _, _, _ -> bad (`Unknown_register b)
      | _, None, _, _ -> bad (`Unknown_register idx)
      | _, _, None, _ -> bad (`Unknown_shift sh)
      | _, _, _, None -> bad `Shift_amount_too_wide)
  (* [rm, lsl #n] as one operand, the two halves already rejoined by
     [join_shifts]. A32 folds the shift into the data-processing word rather
     than making it a separate instruction, so it is part of the operand and not
     an operand of its own. *)
  | [ Token.Ident r; Token.Ident sh; Token.Immediate_sigil; Token.Int v ] -> (
      match (Reg.find r, shift_of_name sh, Bigint.to_int_opt v) with
      | Some reg, Some kind, Some amount ->
          if amount < 0 || amount > 31 then bad (`Shift_amount_out_of_range { amount; max = 31 })
          else Ok (Operand.Shifted { reg; kind; amount })
      | None, _, _ -> bad (`Unknown_register r)
      | _, None, _ -> bad (`Unknown_shift sh)
      | _, _, None -> bad `Shift_amount_too_wide)
  (* [#:lower16:g]. The sigil says "immediate" and the modifier says which half
     of the address; the expression parser knows the second and not the first,
     so the sigil is dropped here and the rest handed on. Dropping it loses
     nothing: an A32 modifier only ever qualifies an immediate. *)
  | Token.Immediate_sigil :: Token.Modifier _ :: _ -> (
      match Asm_syntax.Parse_lines.parse_expression (List.tl slice) with
      | Ok e -> Ok (Operand.Sym e)
      | Error e -> bad (`Cannot_parse_operand { slice; reason = Err.Error.kind e }))
  (* [{r0, r1, r2, r3}] - [push]'s operand (M5, asm/docs/corpus.md:
     test/regression/varargs1.c and friends). [regroup] has already rejoined
     the comma-split pieces by brace depth - and, per its own comment, without
     reinserting the commas themselves, which the common parser already
     consumed as slice boundaries - so this is one flat slice:
     [Lbrace; Ident; Ident; ...; Rbrace], no ranges (CompCert's own ARM
     codegen never writes [{r0-r3}], only the flat comma form). [{d8-d9}] -
     [vpush.64]/[vpop.64]'s D-register list (M5, asm/docs/corpus.md
     classify-c-gcc: [vpush.64 {d8-d9}]) - is the same flat-slice grammar
     over {!Dreg.t}'s name space instead, dispatched on the first member
     since a "dN" spelling is never also a GPR name ({!Reg.find} and
     {!Dreg.find} never both succeed on the same string), but gcc's own VFP
     printer spells it as a hyphenated range rather than the GPR form's flat
     comma list (confirmed against real [as]/[objdump]: [{d8-d9}] and
     [{d8, d9}] both assemble to the identical [ed2d8b04]) - so a range is
     expanded here into the same flat register-number list a comma form
     would produce, and the two grammars are otherwise interchangeable. *)
  | Token.Lbrace :: (_ :: _ as rest) when List.nth rest (List.length rest - 1) = Token.Rbrace -> (
      let inner = List.filteri (fun i _ -> i < List.length rest - 1) rest in
      let is_dreg_list =
        match inner with
        | Token.Ident n :: _ -> Option.is_some (Dreg.find n) && Option.is_none (Reg.find n)
        | _ -> false
      in
      if is_dreg_list then
        let rec dregs_of = function
          | [] -> Ok []
          | Token.Ident lo :: Token.Minus :: Token.Ident hi :: more -> (
              match (Dreg.find lo, Dreg.find hi) with
              | Some l, Some h -> (
                  if l.Dreg.num > h.Dreg.num then
                    bad (`Cannot_parse_operand { slice; reason = `Unexpected_token None })
                  else
                    match dregs_of more with
                    | Ok rest ->
                        Ok (List.init (h.Dreg.num - l.Dreg.num + 1) (fun i -> l.Dreg.num + i) @ rest)
                    | Error _ as e -> e)
              | None, _ -> bad (`Unknown_register lo)
              | _, None -> bad (`Unknown_register hi))
          | Token.Ident n :: more -> (
              match (Dreg.find n, dregs_of more) with
              | Some r, Ok rest -> Ok (r.Dreg.num :: rest)
              | None, _ -> bad (`Unknown_register n)
              | _, (Error _ as e) -> e)
          | _ -> bad (`Cannot_parse_operand { slice; reason = `Unexpected_token None })
        in
        match dregs_of inner with
        | Ok [] -> bad (`Cannot_parse_operand { slice; reason = `Unexpected_token None })
        | Ok regs -> Ok (Operand.Dreglist regs)
        | Error _ as e -> e
      else
        let rec regs_of = function
          | [] -> Ok []
          | Token.Ident n :: more -> (
              match (Reg.find n, regs_of more) with
              | Some r, Ok rest -> Ok (r.Reg.num :: rest)
              | None, _ -> bad (`Unknown_register n)
              | _, (Error _ as e) -> e)
          | _ -> bad (`Cannot_parse_operand { slice; reason = `Unexpected_token None })
        in
        match regs_of inner with
        | Ok [] -> bad (`Cannot_parse_operand { slice; reason = `Unexpected_token None })
        | Ok regs -> Ok (Operand.Reglist regs)
        | Error _ as e -> e)
  (* [Rn!] with no brackets - [stmia]/[ldmia]'s own writeback base register
     (M5, asm/docs/corpus.md classify-c-gcc: [stmia r3!, {r0, r1}]). Parse-
     level only, like {!Operand.Reg_writeback}'s own doc says: neither
     mnemonic is in {!Opcode.t} yet, and [dump-source-ast] never asks for
     more than a successful operand parse. *)
  | [ Token.Ident r; Token.Bang ] -> (
      match Reg.find r with
      | Some reg -> Ok (Operand.Reg_writeback reg)
      | None -> bad (`Unknown_register r))
  | _ -> (
      (* A branch or call target, or any other expression the shapes above do
         not claim. The common expression parser decides, so [.LC1] and [foo+4]
         mean the same here as in a directive argument. *)
      match Asm_syntax.Parse_lines.parse_expression slice with
      | Ok e -> Ok (Operand.Sym e)
      | Error e -> bad (`Cannot_parse_operand { slice; reason = Err.Error.kind e }))

(* [add r0, r0, r1, lsl #1] arrives as four comma-separated slices, because the
   common grammar splits at every top-level comma and a shift specifier is
   written after one. It is not a fourth operand - it qualifies the third - so
   a slice beginning with a shift name is joined to the one before it, exactly
   as x86 rejoins the pieces of a scaled address. Brackets cannot do this job
   the way [regroup] does, because there are none. *)
let join_shifts (groups : Asm_syntax.Token.slice list) =
  let starts_with_shift s =
    match s with
    | t :: _ -> (
        match Asm_syntax.Token.kind t with
        | Asm_syntax.Token.Ident n -> shift_of_name n <> None
        | _ -> false)
    | [] -> false
  in
  let rec go acc = function
    | [] -> List.rev acc
    | s :: rest when starts_with_shift s -> (
        match acc with prev :: others -> go ((prev @ s) :: others) rest | [] -> go [ s ] rest)
    | s :: rest -> go (s :: acc) rest
  in
  go [] groups

let parse_operands ~mnemonic slices =
  ignore mnemonic;
  match regroup slices with
  | Error kind -> Error (parse_diag ~pos:__POS__ kind)
  | Ok groups ->
      let groups = join_shifts groups in
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | s :: rest -> ( match parse_one s with Ok o -> go (o :: acc) rest | Error e -> Error e)
      in
      go [] groups

let handle_directive ~name ~argument state =
  match name with
  | ".syntax" -> Target_intf.Target.Handled { state = { state with syntax = argument }; emit = [] }
  | ".arch" -> Target_intf.Target.Handled { state = { state with arch = argument }; emit = [] }
  | ".fpu" -> Target_intf.Target.Handled { state = { state with fpu = argument }; emit = [] }
  | ".arm" -> Target_intf.Target.Handled { state = { state with thumb = false }; emit = [] }
  | ".thumb" -> Target_intf.Target.Rejected (parse_diag ~pos:__POS__ `Thumb_directive_out_of_scope)
  | ".eabi_attribute" ->
      (* Recorded as handled and otherwise ignored: it is an ELF attribute, and
         M1 produces no ELF. Rejecting it would fail every CompCert ARM file;
         treating it as unknown would be worse, because then a *misspelled*
         directive and this one would be indistinguishable. *)
      Target_intf.Target.Handled { state; emit = [] }
  | _ -> Target_intf.Target.Unhandled
