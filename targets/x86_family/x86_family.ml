(* The x86 front end: how AT&T source text becomes operands.

   {!X86_family_encode} is the rest of the family - the constructor modules, the
   codec, and everything that turns an instruction into bytes - and this file is
   what {!Target.TARGET} adds on top of {!Target_encode.ENCODE}: a lexical
   profile, an operand parser, and a directive handler. The two are separate
   libraries so that a consumer which builds x86 instructions as values rather
   than reading them links the encoder alone, with no lexer and no Menhir table
   behind it.

   The mode functor is applied here as well as there, which costs a second copy
   of the encoder's closures and nothing else: every type the two applications
   produce comes from the family's *top-level* constructor modules, so
   [X86_32.Instruction.t] and [X86_32_encode.Instruction.t] are the same type,
   exactly as [X86_32.Instruction.t] and [X86_64.Instruction.t] already were. *)

open Foundation

module type MODE = X86_family_encode.MODE

(* Opened rather than aliased module by module: the parser below was written as
   part of the family and reads its constructor modules, its register helpers
   and [rip_reg] as plain names. What the split moved is where the code lives,
   not what it may see. *)
open X86_family_encode

module Make (M : MODE) = struct
  include X86_family_encode.Make (M)

  (* {2 Lexical profile}

     x86's `#` is a comment introducer and its immediates carry `$`; registers
     carry `%`. That last one is why [.section ...,%progbits] lexes as a
     register token whose name is not a register - the sigil is lexical, and
     what the name means is decided here. *)
  let lexical_profile =
    Asm_syntax.Lexical_profile.make ~name:M.name ~comment_introducers:[ "#" ]
      ~statement_separator:';' ~immediate_sigil:'$' ~register_sigil:'%' ()

  (* {2 Operand parsing (§4.7)}

     A hand-written token cursor, because AT&T addressing is not a grammar the
     common parser can own: `16(%esp)` and `(1+2)*3` start identically and only
     the target knows that the first is a memory operand. *)

  let slice_origin (s : Asm_syntax.Token.slice) =
    match Asm_syntax.Token.slice_span s with
    | Some sp -> Origin.text sp
    | None -> Origin.synthesized ~pass:M.name ()

  (* The trailing [(%rip)] of a PC-relative operand, and what precedes it. *)
  let split_rip (slice : Asm_syntax.Token.slice) =
    let open Asm_syntax in
    match List.rev slice with
    | rp :: reg :: lp :: rest
      when Token.kind rp = Token.Rparen
           && Token.kind lp = Token.Lparen
           && (match Token.kind reg with Token.Register "rip" -> true | _ -> false)
           && rest <> [] ->
        Some (List.rev rest)
    | _ -> None

  (* A displacement expression, folded first. Folding is what decides between
     the two constructors, and it has to: a decoded RIP-relative operand prints
     its raw displacement, and re-parsing [2044(%rip)] as *symbolic* would make
     the linker subtract the place from a number that is already relative to it.
     A constant is a constant however it was spelled. *)
  let disp_expression ~bad (slice : Asm_syntax.Token.slice) =
    match Asm_syntax.Parse_lines.parse_expression slice with
    | Error _ ->
        bad (Printf.sprintf "cannot parse displacement %s" (Asm_syntax.Token.slice_text slice))
    | Ok e -> (
        match Asm_core.Expr.fold Asm_core.Expr.no_env e with
        | Ok (Asm_core.Expr.Const v) -> (
            match Bigint.to_int64_opt v with
            | Some d -> Ok (Disp.Const d)
            | None -> bad "displacement does not fit 64 bits")
        | Ok folded -> Ok (Disp.Sym folded)
        | Error _ -> Ok (Disp.Sym e))

  let parse_one_operand (slice : Asm_syntax.Token.slice) =
    let origin = slice_origin slice in
    let bad msg = Error (diag ~origin "x86.operand" msg) in
    let reg_named n =
      match find_reg n with
      | Some r -> Ok r
      | None -> bad (Printf.sprintf "unknown register %%%s" n)
    in
    let open Asm_syntax in
    match List.map Token.kind slice with
    (* $imm *)
    | [ Token.Immediate_sigil; Token.Int v ] -> Ok (Operand.Imm v)
    | Token.Immediate_sigil :: Token.Minus :: [ Token.Int v ] -> Ok (Operand.Imm (Bigint.neg v))
    (* %reg *)
    | [ Token.Register n ] -> Result.map (fun r -> Operand.Reg r) (reg_named n)
    (* *%reg, the indirect-jump/call target sigil. GNU as requires the [*];
       we also accept it without one below (falls through to the case
       above), which is harmless since M1 has no direct/PC-relative jmp form
       to be confused with - that's M2. *)
    | [ Token.Star; Token.Register n ] -> Result.map (fun r -> Operand.Reg r) (reg_named n)
    (* disp(%base) and (%base) *)
    | [ Token.Lparen; Token.Register "rip"; Token.Rparen ] ->
        if not M.rex_allowed then bad "%rip-relative addressing is 64-bit only"
        else Ok (Operand.Mem { Mem.base = Some rip_reg; index = None; scale = 1; disp = Disp.zero })
    | [ Token.Lparen; Token.Register b; Token.Rparen ] ->
        Result.map (fun b -> Operand.Mem (Mem.of_base b)) (reg_named b)
    (* [%rip] is not in the register table, so these two would report it as an
       unknown register before the fallback ever saw it. *)
    | [ Token.Int _; Token.Lparen; Token.Register "rip"; Token.Rparen ]
    | Token.Minus :: Token.Int _ :: [ Token.Lparen; Token.Register "rip"; Token.Rparen ] -> (
        if not M.rex_allowed then bad "%rip-relative addressing is 64-bit only"
        else
          match split_rip slice with
          | Some prefix ->
              Result.map
                (fun d ->
                  Operand.Mem { Mem.base = Some rip_reg; index = None; scale = 1; disp = d })
                (disp_expression ~bad prefix)
          | None -> bad "malformed %rip-relative operand")
    | [ Token.Int d; Token.Lparen; Token.Register b; Token.Rparen ] -> (
        match (Bigint.to_int64_opt d, reg_named b) with
        | Some d, Ok b -> Ok (Operand.Mem (Mem.of_base ~disp:(Disp.Const d) b))
        | None, _ -> bad "displacement does not fit 64 bits"
        | _, Error e -> Error e)
    | Token.Minus :: Token.Int d :: [ Token.Lparen; Token.Register b; Token.Rparen ] -> (
        match (Bigint.to_int64_opt d, reg_named b) with
        | Some d, Ok b -> Ok (Operand.Mem (Mem.of_base ~disp:(Disp.Const (Int64.neg d)) b))
        | None, _ -> bad "displacement does not fit 64 bits"
        | _, Error e -> Error e)
    (* disp(%base,%index,scale), with the commas already consumed as slice
       separators and the pieces rejoined by [regroup] - so the pattern is the
       tokens that remain, not the ones the source wrote. *)
    | [ Token.Int d; Token.Lparen; Token.Register b; Token.Register i; Token.Int s; Token.Rparen ]
      -> (
        match (Bigint.to_int64_opt d, Bigint.to_int_opt s, reg_named b, reg_named i) with
        | Some d, Some s, Ok b, Ok i ->
            if log2_scale s = None then bad "scale must be 1, 2, 4 or 8"
            else
              Ok (Operand.Mem { Mem.base = Some b; index = Some i; scale = s; disp = Disp.Const d })
        | _ -> bad "malformed scaled memory operand")
    | [ Token.Lparen; Token.Register b; Token.Register i; Token.Int s; Token.Rparen ] -> (
        match (Bigint.to_int_opt s, reg_named b, reg_named i) with
        | Some s, Ok b, Ok i ->
            if log2_scale s = None then bad "scale must be 1, 2, 4 or 8"
            else Ok (Operand.Mem { Mem.base = Some b; index = Some i; scale = s; disp = Disp.zero })
        | _ -> bad "malformed scaled memory operand")
    | _ -> (
        (* [expr(%rip)]. Matched as a suffix rather than as a token pattern
           because the displacement is an arbitrary expression - [g+4(%rip)] is
           one operand - so what identifies the form is the three tokens at the
           end, not the shape of what precedes them. *)
        match split_rip slice with
        | Some prefix ->
            if not M.rex_allowed then bad "%rip-relative addressing is 64-bit only"
            else
              Result.map
                (fun d ->
                  Operand.Mem { Mem.base = Some rip_reg; index = None; scale = 1; disp = d })
                (disp_expression ~bad prefix)
        | None -> (
            (* Last resort: anything that is not a register, an immediate or an
               addressing form may still be an expression - a branch or call
               target. The common expression parser decides, rather than a
               second hand-written one here, so [foo+4] means the same thing in
               an operand as in a directive argument. *)
            match Asm_syntax.Parse_lines.parse_expression slice with
            | Ok e -> Ok (Operand.Sym e)
            | Error _ ->
                bad (Printf.sprintf "cannot parse operand %s" (Asm_syntax.Token.slice_text slice))))

  (* The common parser splits a line at every top-level comma, and a scaled
     address has commas *inside* its parentheses: [0(%edi,%edi,1)] arrives as
     three slices. Rejoining them needs paren depth, which is a target fact -
     x86 parenthesizes memory operands where ARM and AArch64 bracket them - so
     it belongs here, exactly as ARM's bracket version belongs there.

     M1 never needed this because none of its fixtures had an index register.
     The separator itself is gone, so rejoining is concatenation; re-inserting a
     comma token would be inventing source text whose span is not the lexer's. *)
  let regroup (slices : Asm_syntax.Token.slice list) =
    let depth_of slice =
      List.fold_left
        (fun d t ->
          match Asm_syntax.Token.kind t with
          | Asm_syntax.Token.Lparen -> d + 1
          | Asm_syntax.Token.Rparen -> d - 1
          | _ -> d)
        0 slice
    in
    let rec go acc pending depth = function
      | [] -> if pending = [] then Ok (List.rev acc) else Error "unbalanced parentheses in operand"
      | s :: rest ->
          let d = depth + depth_of s in
          let pending = pending @ s in
          if d = 0 then go (pending :: acc) [] 0 rest
          else if d < 0 then Error "unbalanced parentheses in operand"
          else go acc pending d rest
    in
    go [] [] 0 slices

  let parse_operands ~mnemonic slices =
    ignore mnemonic;
    (* x86 needs no mnemonic-directed operand parsing: unlike ARM's [ldm sp!, {r0}]
       there is no operand shape whose meaning depends on which instruction it
       belongs to. The parameter is in the signature because ARM and AArch64 do
       need it, and a signature with a per-target shape would not be one
       signature. *)
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | s :: rest -> (
          match parse_one_operand s with Ok o -> go (o :: acc) rest | Error e -> Error e)
    in
    match regroup slices with
    | Error m -> Error (diag "x86.operand" m)
    | Ok grouped -> go [] grouped

  let handle_directive ~name ~argument state =
    ignore argument;
    ignore name;
    ignore state;
    (* x86 has no target-state directive in M1: [.syntax], [.arch] and [.fpu]
       are ARM's, and the common table owns everything the x86 fixtures use. *)
    Target_intf.Target.Unhandled
end
