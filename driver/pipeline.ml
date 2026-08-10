(* The architecture-independent pipeline (.ai/asm_plan.md §5.1's
   [Make_assembler]).

   One functor, applied once per target, holding every stage that is not a
   target's business: driving the lexer and the common parser, building the
   source AST, normalizing directives, threading target state, walking the
   normalized module into fragments, and handing the result to the internal
   linker. What the target supplies is listed in [TARGET] and nothing else.

   Each stage is a public entry point (§4), not an internal step of one
   [assemble] function, so a direct AST producer joins the pipeline at the AST
   it can build rather than by generating text for the parser to read back. *)

open Foundation
open Asm_core
module T_intf = Target_intf.Target

module Make (T : T_intf.TARGET) = struct
  let name = T.name
  let triple = T.triple

  let diag ?origin code message =
    Diagnostic.error ~code ~message
      ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:T.name ())
      ()

  (* {1 Stage 1 - text to source AST (§4.2)} *)

  let parse ~unit_name ~source =
    match Parse_lines.parse_program ~profile:T.lexical_profile ~source with
    | Error ds -> Error ds
    | Ok lines ->
        let errors = ref [] in
        let items = ref [] in
        let add i = items := i :: !items in
        List.iter
          (fun (line : Statement.line) ->
            (* Labels first and in order: [foo: bar: ret] defines both at the
               same offset, and a representation that folded them into the
               statement would have to invent an order. *)
            List.iter
              (fun l ->
                match l with
                | Statement.Named (span, n) ->
                    add (Source_ast.Label { name = n; origin = Origin.text span })
                | Statement.Numeric (span, n) ->
                    (* Rejected, not renamed. A numeric local label means
                       nothing without the [1b]/[1f] references that resolve
                       against it, and that resolution is a pass over fragment
                       order which M1 does not have. Turning [1:] into a symbol
                       called "1:" would define something no reference can ever
                       reach - a file assembled without being understood, which
                       is what §2.2 exists to forbid. *)
                    errors :=
                      diag ~origin:(Origin.text span) "parse.local-label"
                        (Printf.sprintf
                           "numeric local label %d: needs 1b/1f resolution, which is not in M1 \
                            scope"
                           n)
                      :: !errors)
              line.Statement.labels;
            match line.Statement.statement with
            | Statement.Empty -> ()
            | Statement.Directive { name; arguments; span } ->
                add (Source_ast.Directive { name; arguments; origin = Origin.text span })
            | Statement.Assignment { name; value; span } ->
                add (Source_ast.Assignment { name; value; origin = Origin.text span })
            | Statement.Instruction { mnemonic; operands; span } -> (
                let origin = Origin.text span in
                match T.parse_operands ~mnemonic operands with
                | Error e -> errors := e :: !errors
                | Ok ops -> (
                    match T.make_surface_instruction ~mnemonic ~origin ops with
                    | Error e -> errors := e :: !errors
                    | Ok insn -> add (Source_ast.Instruction { insn; origin }))))
          lines;
        if !errors <> [] then Error (List.rev !errors)
        else Ok { Source_ast.unit_name; items = List.rev !items }

  (* {1 Stage 2 - simplify (§4.3)}

     Directives become values, metadata is discarded, and instructions become
     canonical semantic operations. Target state is threaded rather than
     mutated, so this function is replayable and two runs over the same module
     cannot differ. *)

  (* Constant folding (§4.3), and the rule that makes it safe: [Expr.fold]
     collapses only *fully absolute* subterms and rewrites the rest unchanged.
     So [2 + 3] becomes [5] here, while [. - asm_test_entry] stays symbolic -
     it cannot be a number until an address exists, and folding it early would
     mean inventing one.

     Folding at simplify rather than at bind is what §4.3 asks for, and it has a
     practical consequence: a division by zero or an out-of-range shift is
     reported against the source line that wrote it, not against a link map. *)
  let fold_directive ~origin d =
    let fold e =
      match Expr.fold Expr.no_env e with
      | Ok e' -> Ok e'
      | Error m -> Error (diag ~origin "simplify.expression" m)
    in
    match d with
    | Directive.Sym_size { name; size } ->
        Result.map (fun size -> Directive.Sym_size { name; size }) (fold size)
    | other -> Ok other

  let simplify (m : T.Surface.t Source_ast.module_) =
    let errors = ref [] in
    let items = ref [] in
    let add i = items := i :: !items in
    let state = ref T.default_state in
    List.iter
      (fun item ->
        match item with
        | Source_ast.Label { name; origin } -> add (Normalized_ast.Label { name; origin })
        | Source_ast.Assignment { name; origin; _ } ->
            errors :=
              diag ~origin "simplify.assignment"
                (Printf.sprintf "symbol assignment (%s = ...) is not in M1 scope" name)
              :: !errors
        | Source_ast.Instruction { insn; origin } -> (
            match T.simplify_instruction ~features:T.default_features insn with
            | Error e -> errors := e :: !errors
            | Ok i -> add (Normalized_ast.Instruction { insn = i; origin }))
        | Source_ast.Directive { name; arguments; origin } -> (
            (* The target is asked first, because a target-state directive is
               the target's to interpret and the common table has no business
               guessing what [.arch armv7-a] means. On [Unhandled] the common
               table decides; if neither claims it, it is a diagnostic. *)
            let argument = String.concat ", " (List.map Token.slice_text arguments) in
            match T.handle_directive ~name ~argument !state with
            | T_intf.Rejected e -> errors := e :: !errors
            | T_intf.Handled { state = s; emit } ->
                state := s;
                List.iter (fun insn -> add (Normalized_ast.Instruction { insn; origin })) emit
            | T_intf.Unhandled -> (
                match Directives.normalize ~name ~arguments with
                | Ok Directives.Dropped -> ()
                | Ok (Directives.Normalized d) -> (
                    match fold_directive ~origin d with
                    | Ok d -> add (Normalized_ast.Directive { directive = d; origin })
                    | Error e -> errors := e :: !errors)
                | Ok Directives.Unknown ->
                    errors :=
                      diag ~origin "simplify.directive" ("unknown directive " ^ name) :: !errors
                | Error msg -> errors := diag ~origin "simplify.directive" msg :: !errors)))
      m.Source_ast.items;
    if !errors <> [] then Error (List.rev !errors)
    else Ok ({ Normalized_ast.unit_name = m.Source_ast.unit_name; items = List.rev !items }, !state)

  (* {1 Stage 3 - lower (§4.4, §7)}

     Fragments, sections and symbols, with no addresses anywhere. The section
     bookkeeping is here rather than in the linker because "which section is
     current" is a property of the *input order*, and the linker sees sections
     already separated. *)

  type section_build = {
    sb_name : string;
    sb_perms : Perms.t;
    mutable sb_align : int;
    mutable sb_frags : T.fixup_kind Lowered_ast.fragment list;  (** reversed *)
  }

  type symbol_build = {
    sy_name : string;
    mutable sy_global : bool;
    mutable sy_kind : Directive.sym_kind;
    mutable sy_size : Expr.t option;
    mutable sy_section : string;
  }

  let lower ~state (m : T.Instruction.t Normalized_ast.module_) =
    let errors = ref [] in
    let sections : section_build list ref = ref [] in
    let declared = ref [] in
    let symbols : symbol_build list ref = ref [] in
    let state = ref state in
    let current = ref None in
    let ensure_section name perms =
      match List.find_opt (fun s -> String.equal s.sb_name name) !sections with
      | Some s ->
          current := Some s;
          s
      | None ->
          let s = { sb_name = name; sb_perms = perms; sb_align = 1; sb_frags = [] } in
          sections := !sections @ [ s ];
          current := Some s;
          s
    in
    let with_section origin f =
      match !current with
      | Some s -> f s
      | None ->
          errors :=
            diag ~origin "lower.no-section" "this appears before any section directive" :: !errors
    in
    let symbol name =
      match List.find_opt (fun s -> String.equal s.sy_name name) !symbols with
      | Some s -> s
      | None ->
          let s =
            {
              sy_name = name;
              sy_global = false;
              sy_kind = Directive.Notype;
              sy_size = None;
              sy_section = (match !current with Some c -> c.sb_name | None -> "");
            }
          in
          symbols := !symbols @ [ s ];
          s
    in
    List.iter
      (fun item ->
        match item with
        | Normalized_ast.Label { name; origin } ->
            with_section origin (fun s ->
                s.sb_frags <- Lowered_ast.Label_def { name; origin } :: s.sb_frags;
                let sy = symbol name in
                sy.sy_section <- s.sb_name)
        | Normalized_ast.Directive { directive; origin } -> (
            match directive with
            | Directive.Section { name; perms } -> ignore (ensure_section name perms)
            | Directive.Declared_section { name } ->
                declared := !declared @ [ name ];
                (* Deliberately does *not* become the current section: it is
                   never allocated, so a fragment landing in it would have
                   nowhere to go. Anything emitted after it is an error rather
                   than silently dropped. *)
                current := None
            | Directive.Align { boundary } ->
                with_section origin (fun s ->
                    s.sb_align <- max s.sb_align boundary;
                    (* The fill is the target's, and only for an executable
                       section: a zero-filled gap in .text decodes as an
                       instruction nobody wrote. *)
                    let fill =
                      if Perms.executable s.sb_perms then (
                        match T.nop_bytes ~length:boundary with
                        | Ok b -> b
                        | Error e ->
                            errors := e :: !errors;
                            "\000")
                      else "\000"
                    in
                    s.sb_frags <- Lowered_ast.Align { boundary; fill; origin } :: s.sb_frags)
            | Directive.Global { name } -> (symbol name).sy_global <- true
            | Directive.Sym_type { name; kind } -> (symbol name).sy_kind <- kind
            | Directive.Sym_size { name; size } ->
                (symbol name).sy_size <- Some size;
                with_section origin (fun s ->
                    s.sb_frags <- Lowered_ast.Set_size { name; size; origin } :: s.sb_frags)
            | Directive.Target_state { name; argument } -> (
                match T.handle_directive ~name ~argument !state with
                | T_intf.Handled { state = s; _ } -> state := s
                | T_intf.Rejected e -> errors := e :: !errors
                | T_intf.Unhandled ->
                    errors :=
                      diag ~origin "lower.directive" ("the target did not accept " ^ name)
                      :: !errors))
        | Normalized_ast.Instruction { insn; origin } ->
            with_section origin (fun s ->
                match T.lower_instruction !state insn with
                | Error e -> errors := e :: !errors
                | Ok lowered ->
                    List.iter
                      (fun l ->
                        match T.encode l with
                        | Error e -> errors := e :: !errors
                        | Ok (bytes, form, _placements) ->
                            s.sb_frags <-
                              Lowered_ast.Bytes
                                { bytes; form = Some (T.name ^ "." ^ form); fixups = []; origin }
                              :: s.sb_frags)
                      lowered))
      m.Normalized_ast.items;
    if !errors <> [] then Error (List.rev !errors)
    else
      Ok
        {
          Lowered_ast.unit_name = m.Normalized_ast.unit_name;
          sections =
            List.map
              (fun s ->
                {
                  Lowered_ast.sec =
                    { Lowered_ast.sec_name = s.sb_name; perms = s.sb_perms; alignment = s.sb_align };
                  fragments = List.rev s.sb_frags;
                })
              !sections;
          symbols =
            List.map
              (fun s ->
                {
                  Lowered_ast.name = s.sy_name;
                  global = s.sy_global;
                  kind = s.sy_kind;
                  size = s.sy_size;
                  section = s.sy_section;
                })
              !symbols;
          declared_sections = !declared;
        }

  (* {1 Stage 4 - the image (§8, §9)} *)

  let policy_for (m : T.fixup_kind Lowered_ast.module_) =
    (* The entry is the single strong exported symbol, which for M1 is the one
       global the fixtures declare. Naming it here rather than requiring the
       caller to pass it means [assemble] has a total signature; a caller that
       wants a different entry calls [plan] itself. *)
    let globals =
      List.filter_map
        (fun (s : Lowered_ast.symbol) ->
          if s.Lowered_ast.global then Some s.Lowered_ast.name else None)
        m.Lowered_ast.symbols
    in
    { Image.default_policy with entry_symbol = (match globals with [ g ] -> Some g | _ -> None) }

  let plan m = Image.plan_image (policy_for m) [ m ]

  (* {1 The whole path} *)

  let assemble ~unit_name ~source =
    match parse ~unit_name ~source with
    | Error ds -> Error ds
    | Ok src -> (
        match simplify src with
        | Error ds -> Error ds
        | Ok (norm, state) -> (
            match lower ~state norm with Error ds -> Error ds | Ok low -> plan low))

  (* {1 Dumps (asm/docs/contracts.md §1)} *)

  (* The spelling column. Every token spells itself exactly, with two
     exceptions that have to be conventions rather than raw text: an [eol]
     token's spelling is a newline, which would end the dump line it is on, and
     an [eof] token has no spelling at all. *)
  let spelling (t : Token.t) =
    match Token.kind t with Token.Eol -> "\\n" | Token.Eof -> "" | _ -> Span.text (Token.span t)

  let dump_tokens ~source =
    match Lexer.tokenize ~profile:T.lexical_profile ~source with
    | Error e -> Error [ Lexer.diagnostic_of_error e ]
    | Ok tokens ->
        Ok
          (String.concat ""
             (List.map
                (fun (t : Token.t) ->
                  let sp = Token.span t in
                  Printf.sprintf "%d %d %s %s\n" (Span.offset sp) (Span.length sp)
                    (Token.kind_tag (Token.kind t))
                    (spelling t))
                tokens))

  let dump_source_ast ~unit_name ~source =
    Result.map (Fmt.to_to_string (Source_ast.pp T.Surface.pp)) (parse ~unit_name ~source)

  let dump_normalized_ast ~unit_name ~source =
    match parse ~unit_name ~source with
    | Error ds -> Error ds
    | Ok src ->
        Result.map
          (fun (n, _) -> Fmt.to_to_string (Normalized_ast.pp T.Instruction.pp) n)
          (simplify src)

  let dump_lowered_ast ~unit_name ~source =
    match parse ~unit_name ~source with
    | Error ds -> Error ds
    | Ok src -> (
        match simplify src with
        | Error ds -> Error ds
        | Ok (n, state) -> Result.map (Fmt.to_to_string Lowered_ast.pp) (lower ~state n))

  (* {2 The diagnostic disassembler (§6)}

     Address, encoding bytes, canonical spelling and form id, in columns. Not
     re-parseable, and no test may re-parse it: that is [dump_disasm_canonical]'s
     job, and a format that had to satisfy both audiences would satisfy
     neither. *)

  let disassemble_lines ~address bytes =
    let rec go pos acc =
      if pos >= String.length bytes then Ok (List.rev acc)
      else
        let here = Int64.add address (Int64.of_int pos) in
        match T.decode { T.state = T.default_state; address = here } bytes ~pos with
        | Error e -> Error [ e ]
        | Ok (insn, form, n) ->
            let run =
              String.concat " "
                (List.init n (fun i -> Printf.sprintf "%02x" (Char.code bytes.[pos + i])))
            in
            go (pos + n) ((here, run, Fmt.to_to_string T.Instruction.pp insn, form) :: acc)
    in
    go 0 []

  let dump_disasm_diagnostic ~address bytes =
    match disassemble_lines ~address bytes with
    | Error ds -> Error ds
    | Ok rows ->
        let wr = List.fold_left (fun a (_, r, _, _) -> max a (String.length r)) 0 rows in
        let wt = List.fold_left (fun a (_, _, t, _) -> max a (String.length t)) 0 rows in
        Ok
          (String.concat ""
             (List.map
                (fun (addr, run, text, form) ->
                  Printf.sprintf "%08Lx  %-*s  %-*s  [%s.%s]\n" addr wr run wt text T.name form)
                rows))

  let dump_disasm_canonical ~address bytes =
    match disassemble_lines ~address bytes with
    | Error ds -> Error ds
    | Ok rows -> Ok (String.concat "" (List.map (fun (_, _, t, _) -> "\t" ^ t ^ "\n") rows))

  (* {2 The codec itself} *)

  let dump_codec () = Fmt.to_to_string Codec.Shape.pp (Codec.inspect T.codec) ^ "\n"
  let check_codec () = List.map (Fmt.to_to_string Codec.pp_problem) (Codec.check T.codec)
end
