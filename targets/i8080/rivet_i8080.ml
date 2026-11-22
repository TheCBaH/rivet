(* The Intel 8080 front end: how Intel-syntax source text becomes operands.

   {!Rivet_i8080_encode} is the rest of the target; this file is what
   {!Rivet_target_intf.Target.TARGET} adds to {!Rivet_target_encode.ENCODE}: a lexical
   profile, an operand parser and a directive handler. Comments start with [;],
   registers are bare identifiers (a symbol with a register's name is a
   register), numbers are GAS [0x] or decimal. [db], [dw] and [org] are
   instruction-shaped in this dialect, so they are the encoder's pseudo
   instructions; no directive is the target's. *)

open Rivet_foundation
include Rivet_i8080_encode

type parse_error_kind = [ `Cannot_parse_operand of cannot_parse ]

and cannot_parse = {
  slice : Rivet_asm_syntax.Token.slice;
  reason : Rivet_asm_syntax.Parse_lines.error_kind;
}

type parse_error = parse_error_kind Rivet_target_error.t

let pp_parse_error_kind ppf : parse_error_kind -> unit = function
  | `Cannot_parse_operand { slice; _ } ->
      Fmt.pf ppf "cannot parse operand %s" (Rivet_asm_syntax.Token.slice_text slice)

let parse_error_kind_code : parse_error_kind -> string = function
  | `Cannot_parse_operand _ -> "i8080.operand"

let pp_parse_error ppf e = pp_parse_error_kind ppf (Rivet_target_error.kind e)
let parse_error_code e = parse_error_kind_code (Rivet_target_error.kind e)

let parse_error_diagnostic e =
  Rivet_target_error.to_diagnostic ~code:parse_error_kind_code ~pp:pp_parse_error_kind e

let parse_diag ?pos ?origin kind =
  Err.Error.make ?pos ~pp_error:pp_parse_error
    (Rivet_target_error.make
       ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:"i8080" ())
       kind)

let lexical_profile =
  Rivet_asm_syntax.Lexical_profile.make ~name:"i8080" ~comment_introducers:[ ";" ] ()

let slice_origin (s : Rivet_asm_syntax.Token.slice) =
  match Rivet_asm_syntax.Token.slice_span s with
  | Some sp -> Origin.text sp
  | None -> Origin.synthesized ~pass:"i8080" ()

let parse_one (slice : Rivet_asm_syntax.Token.slice) =
  match List.map Rivet_asm_syntax.Token.kind slice with
  | [ Rivet_asm_syntax.Token.Ident n ] -> (
      match String.lowercase_ascii n with
      | "sp" -> Ok Operand.Sp
      | "psw" -> Ok Operand.Psw
      | _ -> (
          match Reg.find n with
          | Some r -> Ok (Operand.Reg r)
          | None -> Ok (Operand.Sym (Rivet_asm_core.Expr.Symbol n))))
  | [ Rivet_asm_syntax.Token.String s ] -> Ok (Operand.Str s)
  | _ -> (
      match Rivet_asm_syntax.Parse_lines.parse_expression slice with
      | Ok e -> (
          match Rivet_asm_core.Expr.fold Rivet_asm_core.Expr.no_env e with
          | Ok (Rivet_asm_core.Expr.Const v) -> Ok (Operand.Imm v)
          | Ok e -> Ok (Operand.Sym e)
          | Error _ -> Ok (Operand.Sym e))
      | Error e ->
          Error
            (parse_diag ~pos:__POS__ ~origin:(slice_origin slice)
               (`Cannot_parse_operand { slice; reason = Err.Error.kind e })))

let parse_operands ~mnemonic:_ slices =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | s :: rest -> ( match parse_one s with Ok o -> go (o :: acc) rest | Error e -> Error e)
  in
  go [] slices

let handle_directive ~name:_ ~arguments:_ _ = Rivet_target_intf.Target.Unhandled
