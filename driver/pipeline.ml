(* The architecture-independent pipeline (docs/design.md §5.1's
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
open Asm_syntax
module T_intf = Target_intf.Target

module Make (T : T_intf.TARGET) = struct

  (* The stages below source text are {!Pipeline_direct}'s, over this target's
     encoding half; what follows adds the front end. *)
  module D = Driver_direct.Pipeline_direct.Make (T)
  include D

  (* The pipeline's own error domain (docs/errors.md): the direct stages' and
     the text front end's. [`Expression] and [`Relax_ladder] wrap their causes.
     [`Directive_rejected] carries {!Directives.rejection} whole, including the
     optional code that lets a deferral name itself - the delegation of §2. *)
  type error =
    [ D.error
    | `Local_label_survived of int
    | `Expression of Expr.error
    | `Assignment_out_of_scope of string
    | `Unknown_directive of string
    | `Directive_rejected of Directives.rejection ]

  let pp_error ppf : error -> unit = function
    | `Local_label_survived n -> Fmt.pf ppf "numeric local label %d: survived resolution" n
    | `Expression e -> Expr.pp_error ppf e
    | `Assignment_out_of_scope name ->
        Fmt.pf ppf "symbol assignment (%s = ...) is not in M1 scope" name
    | `Unknown_directive name -> Fmt.pf ppf "unknown directive %s" name
    | `Directive_rejected r -> Directives.pp_rejection ppf r
    | #D.error as e -> D.pp_error ppf e

  let error_code : error -> string = function
    | `Local_label_survived _ -> "parse.local-label"
    | `Expression _ -> "simplify.expression"
    | `Assignment_out_of_scope _ -> "simplify.assignment"
    | `Unknown_directive _ -> "simplify.directive"
    | `Directive_rejected r -> Option.value r.Directives.code ~default:"simplify.directive"
    | #D.error as e -> D.error_code e

  let diag ?origin (e : error) =
    Diagnostic.of_error ~code:error_code ~pp:pp_error
      ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:T.name ())
      e

  (* The same erasure for the target's *front-end* domain, which is a separate
     type because it may name Asm_syntax and the encoder's may not - see
     Target_intf.Target.TARGET. *)
  let parse_diagnostic e = T.parse_error_diagnostic (Err.Error.kind e)

  (* {1 Stage 1 - text to source AST (§4.2)} *)

  let parse ~unit_name ~source =
    match Parse_lines.parse_program ~profile:T.lexical_profile ~source with
    | Error ds -> Error ds
    | Ok lines ->
        let errors = ref [] in
        (* Numeric local labels are resolved here, before [T.parse_operands]:
           once an operand is inside the target's abstract [Surface.t] there is
           no expression traversal in [TARGET] to reach a [Local_ref] with, and
           the feature is target-independent. After this pass they are ordinary
           generated symbols and nothing downstream knows the difference. *)
        let lines, label_errors = Local_labels.run lines in
        (* Shared, not wrapped: the tag means the same thing here as it does in
           [Local_labels] and reports under the same code, so this crossing
           re-codes nothing (docs/errors.md §2). *)
        List.iter
          (fun (e : Local_labels.error) -> errors := Local_labels.diagnostic_of_error e :: !errors)
          label_errors;
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
                    (* Unreachable: [Local_labels.run] has already rewritten
                       every numeric definition to a generated [Named] one. It
                       stays as a diagnostic rather than an assertion because a
                       silently dropped definition would be a label nothing can
                       reach, which is a file assembled without being
                       understood. *)
                    errors := diag ~origin:(Origin.text span) (`Local_label_survived n) :: !errors)
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
                | Error e -> errors := parse_diagnostic e :: !errors
                | Ok ops -> (
                    match T.make_surface_instruction ~mnemonic ~origin ops with
                    | Error e -> errors := target_diagnostic e :: !errors
                    | Ok insn -> add (Source_ast.Instruction { insn; origin }))))
          lines;
        if !errors <> [] then Diag.fail ~pos:__POS__ (List.rev !errors)
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
      | Error m -> Error (diag ~origin (`Expression (Err.Error.kind m)))
    in
    match d with
    | Directive.Sym_size { name; size } ->
        Result.map (fun size -> Directive.Sym_size { name; size }) (fold size)
    | other -> Ok other

  let simplify ~state (m : T.Surface.t Source_ast.module_) =
    let errors = ref [] in
    let items = ref [] in
    let add i = items := i :: !items in
    let state = ref state in
    List.iter
      (fun item ->
        match item with
        | Source_ast.Label { name; origin } -> add (Normalized_ast.Label { name; origin })
        | Source_ast.Assignment { name; origin; _ } ->
            errors := diag ~origin (`Assignment_out_of_scope name) :: !errors
        | Source_ast.Instruction { insn; origin } -> (
            match T.simplify_instruction !state insn with
            | Error e -> errors := target_diagnostic e :: !errors
            | Ok i -> add (Normalized_ast.Instruction { insn = i; origin }))
        | Source_ast.Directive { name; arguments; origin } -> (
            (* The target is asked first, because a target-state directive is
               the target's to interpret and the common table has no business
               guessing what [.arch armv7-a] means. On [Unhandled] the common
               table decides; if neither claims it, it is a diagnostic. *)
            let argument = String.concat ", " (List.map Token.slice_text arguments) in
            match T.handle_directive ~name ~argument !state with
            | T_intf.Rejected e -> errors := parse_diagnostic e :: !errors
            | T_intf.Handled { state = s; emit } ->
                state := s;
                (* State transitions are part of the normalized program.  They
                   must survive so lowering can replay them at their source
                   position instead of seeing only the final file state. *)
                add
                  (Normalized_ast.Directive
                     { directive = Directive.Target_state { name; argument }; origin });
                List.iter (fun insn -> add (Normalized_ast.Instruction { insn; origin })) emit
            | T_intf.Unhandled -> (
                match Directives.normalize ~data_widths:T.data_widths ~name ~arguments with
                | Ok Directives.Dropped -> ()
                | Ok (Directives.Normalized d) -> (
                    match fold_directive ~origin d with
                    | Ok d -> add (Normalized_ast.Directive { directive = d; origin })
                    | Error e -> errors := e :: !errors)
                | Ok Directives.Unknown ->
                    errors := diag ~origin (`Unknown_directive name) :: !errors
                | Error r ->
                    errors := diag ~origin (`Directive_rejected (Err.Error.kind r)) :: !errors)))
      m.Source_ast.items;
    if !errors <> [] then Diag.fail ~pos:__POS__ (List.rev !errors)
    else Ok ({ Normalized_ast.unit_name = m.Source_ast.unit_name; items = List.rev !items }, !state)


  (* {1 Stage 3 - lower (§4.4, §7)}

     The direct stage, with this target's state directives handled as the text
     front end reads them. *)

  let lower ~state m =
    lower_with
      ~target_state:(fun ~name ~argument st ->
        match T.handle_directive ~name ~argument st with
        | T_intf.Handled { state = s; _ } -> Handled s
        | T_intf.Rejected e -> Rejected (parse_diagnostic e)
        | T_intf.Unhandled -> Unhandled)
      ~state m

  (* {1 The whole path}

     Each [Diag.stage] marks a phase boundary at the position of the crossing,
     which is what makes the event trail read parse -> simplify -> lower -> plan
     rather than leaving the phase implicit in a diagnostic code prefix
     (docs/errors.md §2). The payload is untouched: a stage mark records
     that a failure crossed here, not a new failure. *)

  let assemble ?entry ?features ~unit_name ~source () =
    let open Err.Syntax in
    let stage r = Diag.stage ~pos:__POS__ Err.Action.Map r in
    let* state = initial_state features in
    let* src = stage (parse ~unit_name ~source) in
    let* norm, _final_state = stage (simplify ~state src) in
    let* low = stage (lower ~state norm) in
    stage (plan ?entry low)

  (* M3's multi-module entry point. Every input is lowered independently
     ([lower_one]) - a failure in one input does not stop the others from
     being checked too, via [Err.Accum.map] rather than the [let*] short-
     circuit [lower_one]'s own three stages still use internally - and only
     the resulting module list crosses into [plan_many], the one place
     cross-input resolution (§2) actually happens. *)
  let assemble_many ?entry ?features (sources : (string * Span.source) list) () =
    let open Err.Syntax in
    let stage r = Diag.stage ~pos:__POS__ Err.Action.Map r in
    let* state = initial_state features in
    (* Every unit starts from the same initial state, so one unit's directives cannot leak
       into the next. *)
    let lower_one (unit_name, source) =
      let* src = stage (parse ~unit_name ~source) in
      let* norm, _final_state = stage (simplify ~state src) in
      stage (lower ~state norm)
    in
    let* modules =
      Err.Accum.map ~pos:__POS__ lower_one sources
      |> Err.Accum.fold_errors (fun errs -> List.concat_map Err.Error.kind errs)
    in
    stage (plan_many ?entry modules)

  (* {1 Dumps (docs/contracts.md §1)} *)

  (* The spelling column. Every token spells itself exactly, with two
     exceptions that have to be conventions rather than raw text: an [eol]
     token's spelling is a newline, which would end the dump line it is on, and
     an [eof] token has no spelling at all. *)
  let spelling (t : Token.t) =
    match Token.kind t with Token.Eol -> "\\n" | Token.Eof -> "" | _ -> Span.text (Token.span t)

  let dump_tokens ~source =
    match Lexer.tokenize ~profile:T.lexical_profile ~source with
    | Error e -> Diag.fail ~pos:__POS__ [ Lexer.diagnostic_of_error e ]
    | Ok tokens ->
        (* [List.iter] into a [Buffer] rather than [String.concat] over a
           [List.map]: [List.map] conses onto its recursive call rather than an
           accumulator, one native stack frame per token, which a real
           generated file's token count (M5, gcc's [floats.c]: ~600K tokens)
           overflows - confirmed by gdb, hundreds of thousands of stacked
           [List.map] frames. [List.iter]'s call to itself is a genuine tail
           call, so this is O(1) stack regardless of token count. *)
        let buf = Buffer.create (List.length tokens * 16) in
        List.iter
          (fun (t : Token.t) ->
            let sp = Token.span t in
            Buffer.add_string buf
              (Printf.sprintf "%d %d %s %s\n" (Span.offset sp) (Span.length sp)
                 (Token.kind_tag (Token.kind t))
                 (spelling t)))
          tokens;
        Ok (Buffer.contents buf)

  let dump_source_ast ~unit_name ~source =
    Result.map (Fmt.to_to_string (Source_ast.pp T.Surface.pp)) (parse ~unit_name ~source)

  let dump_normalized_ast ?features ~unit_name ~source () =
    let open Err.Syntax in
    let* state = initial_state features in
    match parse ~unit_name ~source with
    | Error ds -> Error ds
    | Ok src ->
        Result.map
          (fun (n, _) -> Fmt.to_to_string (Normalized_ast.pp T.Instruction.pp) n)
          (simplify ~state src)

  let dump_lowered_ast ?features ~unit_name ~source () =
    let open Err.Syntax in
    let* state = initial_state features in
    match parse ~unit_name ~source with
    | Error ds -> Error ds
    | Ok src -> (
        match simplify ~state src with
        | Error ds -> Error ds
        | Ok (n, _final_state) -> Result.map (Fmt.to_to_string Lowered_ast.pp) (lower ~state n))

  (* {2 The diagnostic disassembler (§6)}

     Address, encoding bytes, canonical spelling and form id, in columns. Not
     re-parseable, and no test may re-parse it: that is [dump_disasm_canonical]'s
     job, and a format that had to satisfy both audiences would satisfy
     neither. *)

  (* Alignment padding is filler, not code, and disassembling it as code is not
     merely awkward - on x86 it is impossible. GNU's padding table for 64-bit
     mode ends with [66 2e 0f 1f 84 ...] and [66 66 2e 0f 1f 84 ...], and GAS
     emits its prefixes in the order [2e 66]: no instruction source reassembles
     to those two rows. They exist only inside the assembler's own padding
     table. A canonical dump that spelled them as instructions could therefore
     never round-trip, whatever spelling it chose.

     What *does* reproduce filler is the directive that asked for it, so both
     dumps recognize a padding run and print [.balign]. The recognition is
     exact and target-driven: the bytes from here to a power-of-two boundary
     must be precisely what this target's own [nop_bytes] emits for that length.
     A byte sequence GNU pads with and we do not is then a loud decode failure
     rather than a quiet misreading, which is the right way round.

     [.balign] rather than [.align] or [.p2align] because it is a byte count on
     all four dialects and is already in the accepted table; the identity being
     preserved is the bytes, and any boundary that reproduces them reproduces
     them exactly. A lone [nop] that happens to sit one byte below a boundary is
     read as padding, which is harmless for exactly that reason.

     Layout aligns *section offsets* and this aligns *addresses*, and the two
     agree because a section's alignment is raised to the widest [Align] it
     contains and [bind_image] rejects an address that does not honour it. That
     invariant is what makes an address-based reading of the bytes correct; it
     is not an assumption about where hosts put things. *)
  let padding_at ~address bytes ~pos =
    let len = String.length bytes in
    let here = Int64.add address (Int64.of_int pos) in
    (* Two possible fill sources for the same run, not one: [T.nop_bytes] is
       what [as] emits for an [.align] gap within one module, and (M3 §5,
       docs/design.md §12) [T.merge_fill] is what [ld] emits for a gap the
       merge step inserts between two modules - and on x86_32 those are
       DIFFERENT byte sequences for the same length (nop_table's LEA forms
       vs. merge_fill's plain [66 90] chain). Neither table's bytes decode as
       ordinary instructions - {!T.nop_bytes}'s own doc comment is explicit
       that GNU's padding table exists nowhere a real encoder would produce it
       - so a merge gap recognized by neither source falls through to the
       ordinary decoder and fails outright, which is exactly the gap this
       covers. *)
    let fill_matches n candidate =
      (match T.nop_bytes ~length:n with Ok s -> String.equal s candidate | Error _ -> false)
      || match T.merge_fill with Some f -> String.equal (f ~length:n) candidate | None -> false
    in
    let matches k =
      let b = Int64.of_int (1 lsl k) in
      let stop = Int64.mul (Int64.div (Int64.add here (Int64.sub b 1L)) b) b in
      let n = Int64.to_int (Int64.sub stop here) in
      if n > 0 && pos + n <= len && fill_matches n (String.sub bytes pos n) then Some (1 lsl k, n)
      else None
    in
    (* Longest run first, so a short boundary can never claim a prefix of a
       longer pad and leave the tail to be decoded as instructions. Among the
       boundaries that produce that same run - a pad ending at 0x20 is reached
       from 0x16 by both [.balign 16] and [.balign 32] - the smallest, which is
       what the source said. Every one of them reproduces the bytes, so this is
       a fidelity choice rather than a correctness one. *)
    List.fold_left
      (fun best k ->
        match matches k with
        | Some (b, n) when match best with None -> true | Some (_, bn) -> n > bn -> Some (b, n)
        | _ -> best)
      None
      (List.init 12 (fun i -> i + 1))

  type disasm_row = { at : int64; run : string; text : string; form : string option }

  let disassemble_lines ~state ~inspect ~address bytes =
    (* Strict decoding refuses a disabled component's instruction. Inspection decodes under
       every component enabled and notes which instructions the configuration would refuse. *)
    let decode_state =
      if inspect then T.initial_state (Target_config.default T.components) else state
    in
    let unmet insn =
      match T.required_feature insn with
      | Some f when not (Target_config.enabled (T.state_config state) f) ->
          Printf.sprintf "  ; requires feature %s, which is not enabled" f
      | _ -> ""
    in
    let run_of pos n =
      String.concat " " (List.init n (fun i -> Printf.sprintf "%02x" (Char.code bytes.[pos + i])))
    in
    let rec go pos acc =
      if pos >= String.length bytes then Ok (List.rev acc)
      else
        let here = Int64.add address (Int64.of_int pos) in
        match padding_at ~address bytes ~pos with
        | Some (boundary, n) ->
            go (pos + n)
              ({
                 at = here;
                 run = run_of pos n;
                 text = Printf.sprintf ".balign %d" boundary;
                 form = None;
               }
              :: acc)
        | None -> (
            match T.decode { T.state = decode_state; address = here } bytes ~pos with
            | Error e -> Diag.fail ~pos:__POS__ [ target_diagnostic e ]
            | Ok (insn, form, n) ->
                go (pos + n)
                  ({
                     at = here;
                     run = run_of pos n;
                     text =
                       (Fmt.to_to_string T.Instruction.pp insn ^ if inspect then unmet insn else "");
                     form = Some form;
                   }
                  :: acc))
    in
    go 0 []

  let dump_disasm_diagnostic ?features ?(inspect = false) ~address bytes =
    let open Err.Syntax in
    let* state = initial_state features in
    match disassemble_lines ~state ~inspect ~address bytes with
    | Error ds -> Error ds
    | Ok rows ->
        let wr = List.fold_left (fun a r -> max a (String.length r.run)) 0 rows in
        let wt = List.fold_left (fun a r -> max a (String.length r.text)) 0 rows in
        Ok
          (String.concat ""
             (List.map
                (fun r ->
                  Printf.sprintf "%08Lx  %-*s  %-*s  [%s]\n" r.at wr r.run wt r.text
                    (match r.form with Some f -> T.name ^ "." ^ f | None -> "padding"))
                rows))

  let dump_disasm_canonical ?features ~address bytes =
    let open Err.Syntax in
    let* state = initial_state features in
    match disassemble_lines ~state ~inspect:false ~address bytes with
    | Error ds -> Error ds
    | Ok rows -> Ok (String.concat "" (List.map (fun r -> "\t" ^ r.text ^ "\n") rows))

  (* {2 The codec itself} *)

  (* The kind dictionaries are supplied here rather than stored in the codec
     nodes: the fixup kind is [T]'s abstract type, so the codec cannot name or
     compare it, and a copy carried alongside would be free to disagree with
     [T.fixup_kind_name]. *)
  let dump_codec () =
    Fmt.to_to_string Codec.Shape.pp (Codec.inspect ~kind_name:T.fixup_kind_name T.codec) ^ "\n"

  let check_codec () =
    List.map
      (Fmt.to_to_string Codec.pp_problem)
      (Codec.check ~equal_kind:T.equal_fixup_kind ~kind_name:T.fixup_kind_name T.codec)
end
