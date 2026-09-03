open Foundation

module type PROFILE = Riscv_family_encode.PROFILE

module Make (P : PROFILE) = struct
  include Riscv_family_encode.Make (P)

  type parse_error_kind =
    [ `Unknown_register of string
    | `Cannot_parse_operand of Asm_syntax.Token.slice
    | `Malformed_memory_operand
    | `Option_stack_underflow
    | `Unsupported_option of string
    | `Unknown_option of string ]

  type parse_error = parse_error_kind Target_error.t

  let pp_parse_error_kind ppf : parse_error_kind -> unit = function
    | `Unknown_register r -> Fmt.pf ppf "unknown register %s" r
    | `Cannot_parse_operand s ->
        Fmt.pf ppf "cannot parse operand %s" (Asm_syntax.Token.slice_text s)
    | `Malformed_memory_operand -> Fmt.string ppf "malformed offset(base) operand"
    | `Option_stack_underflow -> Fmt.string ppf ".option pop has no matching push"
    | `Unsupported_option o -> Fmt.pf ppf ".option %s is not supported" o
    | `Unknown_option o -> Fmt.pf ppf "unknown .option %s" o

  let parse_error_kind_code : parse_error_kind -> string = function
    | `Option_stack_underflow | `Unsupported_option _ | `Unknown_option _ -> name ^ ".directive"
    | _ -> name ^ ".operand"

  let pp_parse_error ppf e = pp_parse_error_kind ppf (Target_error.kind e)
  let parse_error_code e = parse_error_kind_code (Target_error.kind e)

  let parse_error_diagnostic e =
    Target_error.to_diagnostic ~code:parse_error_kind_code ~pp:pp_parse_error_kind e

  let parse_diag ?pos ?origin kind =
    Err.Error.make ?pos ~pp_error:pp_parse_error
      (Target_error.make
         ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:name ())
         kind)

  let lexical_profile =
    Asm_syntax.Lexical_profile.make ~name ~comment_introducers:[ "#" ] ~statement_separator:';'
      ~percent_call_modifiers:[ "hi"; "lo"; "pcrel_hi"; "pcrel_lo" ]
      ()

  let slice_origin s =
    match Asm_syntax.Token.slice_span s with
    | Some span -> Origin.text span
    | None -> Origin.synthesized ~pass:name ()

  let parse_expr slice =
    match Asm_syntax.Parse_lines.parse_expression slice with
    | Ok e -> Ok e
    | Error _ ->
        Error (parse_diag ~pos:__POS__ ~origin:(slice_origin slice) (`Cannot_parse_operand slice))

  (* [sym@plt]: GNU's marker that a call target may need to route through the
     PLT, on a symbol CompCert cannot see defined in this translation unit -
     the RISC-V spelling of the same construct x86 writes [sym@PLT] for.
     Semantically inert: [call sym@plt] and plain [call sym] both lower to the
     same [call-hi20]/[call-lo12-i] pair, so stripping the suffix before the
     common expression parser ever sees [@] changes no byte. Scoped to the
     bare-symbol operand fallback below, not {!parse_expr}'s other two call
     sites, since a memory-operand offset or a [%pcrel_hi] argument never
     carries this suffix. *)
  let strip_plt_suffix (slice : Asm_syntax.Token.slice) =
    let open Asm_syntax in
    match List.rev slice with
    | { Token.kind = Token.Ident "plt"; _ } :: { Token.kind = Token.At; _ } :: (_ :: _ as rest) ->
        List.rev rest
    | _ -> slice

  let split_memory slice =
    let rev_take = function
      | rparen :: base :: lparen :: rest -> (
          match
            (Asm_syntax.Token.kind rparen, Asm_syntax.Token.kind base, Asm_syntax.Token.kind lparen)
          with
          | Asm_syntax.Token.Rparen, Asm_syntax.Token.Ident b, Asm_syntax.Token.Lparen ->
              Some (List.rev rest, b)
          | _ -> None)
      | _ -> None
    in
    rev_take (List.rev slice)

  (* [%pcrel_hi(f1)]'s parenthesized argument is the modifier's symbol, never
     a memory-operand base register - but [f1] is also a legal floating
     register spelling ([Reg.find] has no way to know which reading a bare
     identifier is "for"), and CompCert is free to name a static function or
     variable anything, register spellings included (M5 corpus evidence:
     test/regression/charlit.c's static [f1]; asm/docs/corpus.md). A single
     leading [Token.Modifier] is unambiguous - a real [offset(base)] never
     starts with one - so it decides the reading before [Reg.find] is even
     consulted. *)
  let modifier_prefix = function
    | [ tok ] -> (
        match Asm_syntax.Token.kind tok with Asm_syntax.Token.Modifier _ -> true | _ -> false)
    | _ -> false

  let parse_one slice =
    let open Asm_syntax in
    match split_memory slice with
    | Some (prefix, _) when modifier_prefix prefix -> (
        match parse_expr slice with Ok e -> Ok (Operand.Sym e) | Error _ as e -> e)
    | Some (prefix, base_name) -> (
        match Reg.find base_name with
        | None -> (
            (* [%pcrel_hi(symbol)] has the same final token shape as
               [offset(base)].  If the parenthesized name is not a register,
               let the declared modifier expression claim the operand. *)
            match parse_expr slice with
            | Ok e -> Ok (Operand.Sym e)
            | Error _ as e -> e)
        | Some base -> (
            let parsed =
              if prefix = [] then Ok (Asm_core.Expr.Const Bigint.zero) else parse_expr prefix
            in
            match parsed with
            | Ok offset -> Ok (Operand.Mem { Mem.offset; base })
            | Error _ as e -> e))
    | None -> (
        match List.map Token.kind slice with
        | [ Token.Ident n ] -> (
            match Reg.find n with
            | Some r -> Ok (Operand.Reg r)
            | None -> Ok (Operand.Sym (Asm_core.Expr.Symbol n)))
        | _ -> (
            match parse_expr (strip_plt_suffix slice) with
            | Ok (Asm_core.Expr.Const n) -> Ok (Operand.Imm n)
            | Ok e -> Ok (Operand.Sym e)
            | Error _ as e -> e))

  let parse_operands ~mnemonic:_ slices =
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | s :: rest -> ( match parse_one s with Ok o -> go (o :: acc) rest | Error _ as e -> e)
    in
    go [] slices

  let handle_directive ~name:directive ~argument state =
    if String.equal directive ".attribute" then (
      (* [.attribute arch, "rv32i2p1_..."], [.attribute unaligned_access, 0], ...
         - RISC-V's own build-attribute directive (ARM's [.eabi_attribute]
         precedent above: recorded as handled and otherwise ignored, since it is
         an ELF attribute and M1 produces no ELF; rejecting it would fail every
         gcc-compiled RISC-V file, and treating it as unknown would make it
         indistinguishable from a real misspelling). *)
      ignore argument;
      Target_intf.Target.Handled { state; emit = [] })
    else if not (String.equal directive ".option") then Target_intf.Target.Unhandled
    else
      let option = String.trim argument in
      let handled state = Target_intf.Target.Handled { state; emit = [] } in
      match option with
      | "push" ->
          handled
            { state with option_stack = (state.pic, state.relax, state.rvc) :: state.option_stack }
      | "pop" -> (
          match state.option_stack with
          | (pic, relax, rvc) :: rest -> handled { pic; relax; rvc; option_stack = rest }
          | [] -> Target_intf.Target.Rejected (parse_diag ~pos:__POS__ `Option_stack_underflow))
      | "pic" -> handled { state with pic = true }
      | "nopic" -> handled { state with pic = false }
      | "norelax" -> handled { state with relax = false }
      | "norvc" -> handled { state with rvc = false }
      | ("relax" | "rvc") as o ->
          Target_intf.Target.Rejected (parse_diag ~pos:__POS__ (`Unsupported_option o))
      | o -> Target_intf.Target.Rejected (parse_diag ~pos:__POS__ (`Unknown_option o))
end
