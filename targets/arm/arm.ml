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
  | `Thumb_directive_out_of_scope -> Fmt.string ppf "Thumb is not in M1 scope"

let parse_error_kind_code : parse_error_kind -> string = function
  | `Thumb_directive_out_of_scope -> "arm.directive"
  | `Unknown_register _ | `Unknown_shift _ | `Offset_too_wide | `Shift_amount_out_of_range _
  | `Shift_amount_too_wide | `Cannot_parse_operand _ | `Unbalanced_brackets ->
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
  match List.map Token.kind slice with
  | [ Token.Ident n ] -> (
      (* A register if the table knows the name, a symbol otherwise: A32 has no
         register sigil, so nothing else can tell them apart. *)
      match Reg.find n with
      | Some r -> Ok (Operand.Reg r)
      | None -> Ok (Operand.Sym (Asm_core.Expr.Symbol n)))
  | [ Token.Immediate_sigil; Token.Int v ] -> Ok (Operand.Imm v)
  | [ Token.Immediate_sigil; Token.Minus; Token.Int v ] -> Ok (Operand.Imm (Bigint.neg v))
  | [ Token.Lbracket; Token.Ident b; Token.Rbracket ] -> (
      match Reg.find b with
      | Some r -> Ok (Operand.Mem { base = r; offset = 0L; writeback = false; pre = true })
      | None -> bad (`Unknown_register b))
  | [ Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Int v; Token.Rbracket ] -> (
      match (Reg.find b, signed_int false v) with
      | Some r, Some off ->
          Ok (Operand.Mem { base = r; offset = off; writeback = false; pre = true })
      | None, _ -> bad (`Unknown_register b)
      | _, None -> bad `Offset_too_wide)
  | [
   Token.Lbracket; Token.Ident b; Token.Immediate_sigil; Token.Minus; Token.Int v; Token.Rbracket;
  ] -> (
      match (Reg.find b, signed_int true v) with
      | Some r, Some off ->
          Ok (Operand.Mem { base = r; offset = off; writeback = false; pre = true })
      | None, _ -> bad (`Unknown_register b)
      | _, None -> bad `Offset_too_wide)
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
