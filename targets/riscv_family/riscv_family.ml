open Rivet_foundation

module type PROFILE = Riscv_family_encode.PROFILE
module type EXTENSIONS = Riscv_family_encode.EXTENSIONS

module Make_with (P : PROFILE) (E : EXTENSIONS) = struct
  include Riscv_family_encode.Make_with (P) (E)

  type parse_error_kind =
    [ `Unknown_register of string
    | `Cannot_parse_operand of Rivet_asm_syntax.Token.slice
    | `Malformed_memory_operand
    | `Option_stack_underflow
    | `Unsupported_option of string
    | `Unknown_option of string
    | `Invalid_arch of string ]

  type parse_error = parse_error_kind Rivet_target_error.t

  let pp_parse_error_kind ppf : parse_error_kind -> unit = function
    | `Unknown_register r -> Fmt.pf ppf "unknown register %s" r
    | `Cannot_parse_operand s ->
        Fmt.pf ppf "cannot parse operand %s" (Rivet_asm_syntax.Token.slice_text s)
    | `Malformed_memory_operand -> Fmt.string ppf "malformed offset(base) operand"
    | `Option_stack_underflow -> Fmt.string ppf ".option pop has no matching push"
    | `Unsupported_option o -> Fmt.pf ppf ".option %s is not supported" o
    | `Unknown_option o -> Fmt.pf ppf "unknown .option %s" o
    | `Invalid_arch why -> Fmt.pf ppf ".option arch: %s" why

  let parse_error_kind_code : parse_error_kind -> string = function
    | `Option_stack_underflow | `Unsupported_option _ | `Unknown_option _ | `Invalid_arch _ ->
        name ^ ".directive"
    | _ -> name ^ ".operand"

  let pp_parse_error ppf e = pp_parse_error_kind ppf (Rivet_target_error.kind e)
  let parse_error_code e = parse_error_kind_code (Rivet_target_error.kind e)

  let parse_error_diagnostic e =
    Rivet_target_error.to_diagnostic ~code:parse_error_kind_code ~pp:pp_parse_error_kind e

  let parse_diag ?pos ?origin kind =
    Err.Error.make ?pos ~pp_error:pp_parse_error
      (Rivet_target_error.make
         ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:name ())
         kind)

  let lexical_profile =
    Rivet_asm_syntax.Lexical_profile.make ~name ~comment_introducers:[ "#" ]
      ~statement_separator:';'
      ~percent_call_modifiers:[ "hi"; "lo"; "pcrel_hi"; "pcrel_lo" ]
      ()

  let slice_origin s =
    match Rivet_asm_syntax.Token.slice_span s with
    | Some span -> Origin.text span
    | None -> Origin.synthesized ~pass:name ()

  let parse_expr slice =
    match Rivet_asm_syntax.Parse_lines.parse_expression slice with
    | Ok e -> Ok e
    | Error _ ->
        Error (parse_diag ~pos:__POS__ ~origin:(slice_origin slice) (`Cannot_parse_operand slice))

  (* [sym@plt]: GNU's marker that a call target may need to route through the
     PLT, on a symbol the compiler cannot see defined in this translation unit -
     the RISC-V spelling of the same construct x86 writes [sym@PLT] for.
     Semantically inert: [call sym@plt] and plain [call sym] both lower to the
     same [call-hi20]/[call-lo12-i] pair, so stripping the suffix before the
     common expression parser ever sees [@] changes no byte. Scoped to the
     bare-symbol operand fallback below, not {!parse_expr}'s other two call
     sites, since a memory-operand offset or a [%pcrel_hi] argument never
     carries this suffix. *)
  let strip_plt_suffix (slice : Rivet_asm_syntax.Token.slice) =
    let open Rivet_asm_syntax in
    match List.rev slice with
    | { Token.kind = Token.Ident "plt"; _ } :: { Token.kind = Token.At; _ } :: (_ :: _ as rest) ->
        List.rev rest
    | _ -> slice

  let split_memory slice =
    let rev_take = function
      | rparen :: base :: lparen :: rest -> (
          match
            ( Rivet_asm_syntax.Token.kind rparen,
              Rivet_asm_syntax.Token.kind base,
              Rivet_asm_syntax.Token.kind lparen )
          with
          | ( Rivet_asm_syntax.Token.Rparen,
              Rivet_asm_syntax.Token.Ident b,
              Rivet_asm_syntax.Token.Lparen ) ->
              Some (List.rev rest, b)
          | _ -> None)
      | _ -> None
    in
    rev_take (List.rev slice)

  (* [%pcrel_hi(f1)]'s parenthesized argument is the modifier's symbol, never
     a memory-operand base register - but [f1] is also a legal floating
     register spelling ([Reg.find] has no way to know which reading a bare
     identifier is "for"), and the compiler is free to name a static function or
     variable anything, register spellings included (M5 corpus evidence:
     test/regression/charlit.c's static [f1]; the corpus notes). A single
     leading [Token.Modifier] is unambiguous - a real [offset(base)] never
     starts with one - so it decides the reading before [Reg.find] is even
     consulted. *)
  let modifier_prefix = function
    | [ tok ] -> (
        match Rivet_asm_syntax.Token.kind tok with
        | Rivet_asm_syntax.Token.Modifier _ -> true
        | _ -> false)
    | _ -> false

  let parse_one slice =
    let open Rivet_asm_syntax in
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
              if prefix = [] then Ok (Rivet_asm_core.Expr.Const Bigint.zero) else parse_expr prefix
            in
            match parsed with
            | Ok offset -> Ok (Operand.Mem { Mem.offset; base })
            | Error _ as e -> e))
    | None -> (
        match List.map Token.kind slice with
        | [ Token.Ident n ] -> (
            match Reg.find n with
            | Some r -> Ok (Operand.Reg r)
            | None -> Ok (Operand.Sym (Rivet_asm_core.Expr.Symbol n)))
        | _ -> (
            match parse_expr (strip_plt_suffix slice) with
            | Ok (Rivet_asm_core.Expr.Const n) -> Ok (Operand.Imm n)
            | Ok e -> Ok (Operand.Sym e)
            | Error _ as e -> e))

  (* Zfa's [fli.<fmt> rd, constant]: the constant is a floating-point literal
     ([1.0], [-1.0], [0x1p-16]) or a name ([min], [inf], [nan]), not an
     expression, so it reaches the encoder as its source text. *)
  let parse_operands ~mnemonic slices =
    let fli = String.length mnemonic > 4 && String.sub mnemonic 0 4 = "fli." in
    (* Zcmp's [cm.push {ra, s0-s1}, -32]: the braced list contains commas, so
       every slice but the last is the list, passed on as its text *)
    let rlist = String.length mnemonic > 4 && String.sub mnemonic 0 4 = "cm.p" in
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | s :: rest when fli && acc <> [] ->
          go
            (Operand.Sym (Rivet_asm_core.Expr.Symbol (Rivet_asm_syntax.Token.slice_text s)) :: acc)
            rest
      | s :: rest -> ( match parse_one s with Ok o -> go (o :: acc) rest | Error _ as e -> e)
    in
    match (rlist, List.rev slices) with
    | true, last :: (_ :: _ as list) -> (
        let text = String.concat "," (List.rev_map Rivet_asm_syntax.Token.slice_text list) in
        match parse_one last with
        | Ok adj -> Ok [ Operand.Sym (Rivet_asm_core.Expr.Symbol text); adj ]
        | Error _ as e -> e)
    | _ -> go [] slices

  (* [.option arch, +zba, -c1p0]: each item is a signed extension name, optionally followed by a
     GNU version ([2p0]). Versions are accepted and ignored: the registry models one version of
     each extension. The ISA-string form [=rv64gc] is full GNU intake and stays unsupported. *)
  let strip_version name =
    let is_digit c = c >= '0' && c <= '9' in
    let rec digits_start i = if i > 0 && is_digit name.[i - 1] then digits_start (i - 1) else i in
    let n = String.length name in
    let minor = digits_start n in
    if minor < n && minor > 0 && name.[minor - 1] = 'p' then
      let major = digits_start (minor - 1) in
      if major < minor - 1 then String.sub name 0 major else name
    else name

  let parse_arch items =
    let item raw =
      let s = String.lowercase_ascii (String.trim raw) in
      let name = if s = "" then "" else strip_version (String.sub s 1 (String.length s - 1)) in
      match if s = "" then ' ' else s.[0] with
      | '+' when name <> "" -> Ok (Rivet_target_features.Config.Enable name)
      | '-' when name <> "" -> Ok (Rivet_target_features.Config.Disable name)
      | '=' -> Error (`Unsupported_option "arch =<ISA string>")
      | _ ->
          Error
            (`Invalid_arch
               (Printf.sprintf "expected +<extension> or -<extension>, got %S" (String.trim raw)))
    in
    List.fold_right
      (fun raw acc ->
        match (item raw, acc) with Ok i, Ok l -> Ok (i :: l) | (Error _ as e), _ -> e | _, e -> e)
      items (Ok [])

  let handle_directive ~name:directive ~arguments state =
    if String.equal directive ".attribute" then (
      (* [.attribute arch, "rv32i2p1_..."], [.attribute unaligned_access, 0], ...
         - RISC-V's own build-attribute directive (ARM's [.eabi_attribute]
         precedent above: recorded as handled and otherwise ignored, since it is
         an ELF attribute and M1 produces no ELF; rejecting it would fail every
         gcc-compiled RISC-V file, and treating it as unknown would make it
         indistinguishable from a real misspelling). *)
      ignore arguments;
      Rivet_target_intf.Target.Handled { state; emit = [] })
    else if not (String.equal directive ".option") then Rivet_target_intf.Target.Unhandled
    else
      let option = String.trim (String.concat ", " arguments) in
      let arch =
        match arguments with
        | first :: items when String.equal (String.trim first) "arch" ->
            Some (if items = [] then [ "" ] else items)
        | _ -> None
      in
      let handled state = Rivet_target_intf.Target.Handled { state; emit = [] } in
      match (arch, option) with
      | Some items, _ -> (
          let reject why = Rivet_target_intf.Target.Rejected (parse_diag ~pos:__POS__ why) in
          match parse_arch items with
          | Error why -> reject why
          | Ok spec -> (
              match Rivet_target_features.Config.apply components state.config spec with
              | Ok config -> handled { state with config }
              | Error why -> reject (`Invalid_arch why)))
      | None, "push" ->
          handled
            {
              state with
              option_stack = (state.pic, state.relax, state.rvc, state.config) :: state.option_stack;
            }
      | None, "pop" -> (
          match state.option_stack with
          | (pic, relax, rvc, config) :: rest ->
              handled { pic; relax; rvc; config; option_stack = rest }
          | [] ->
              Rivet_target_intf.Target.Rejected (parse_diag ~pos:__POS__ `Option_stack_underflow))
      | None, "pic" -> handled { state with pic = true }
      | None, "nopic" -> handled { state with pic = false }
      | None, "norelax" -> handled { state with relax = false }
      | None, "norvc" -> handled { state with rvc = false }
      | None, "rvc" -> handled { state with rvc = true }
      | None, "relax" ->
          Rivet_target_intf.Target.Rejected (parse_diag ~pos:__POS__ (`Unsupported_option "relax"))
      | None, o -> Rivet_target_intf.Target.Rejected (parse_diag ~pos:__POS__ (`Unknown_option o))
end

module Make (P : PROFILE) = Make_with (P) (Riscv_family_encode.Standard (P))
