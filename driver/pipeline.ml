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
open Asm_syntax
module T_intf = Target_intf.Target

module Make (T : T_intf.TARGET) = struct
  let name = T.name
  let triple = T.triple

  (* The pipeline's own error domain (asm/docs/errors.md) - the failures that
     belong to no target and no library, but to the act of walking a module
     through the stages.

     [`Expression] and [`Relax_ladder] wrap their causes, which deletes the last
     two rendering seams the migration left. [`Directive_rejected] carries
     {!Directives.rejection} whole, including the optional code that lets a
     deferral name itself - the delegation of §2. *)
  type error =
    [ `Local_label_survived of int
    | `Expression of Expr.error
    | `Assignment_out_of_scope of string
    | `Unknown_directive of string
    | `Directive_rejected of Directives.rejection
    | `Statement_before_section
    | `Data_value of Expr.error
    | `Data_fixup of string
    | `Directive_not_accepted of string
    | `Relax_ladder of Lowered_ast.relax_error ]

  let pp_error ppf : error -> unit = function
    | `Local_label_survived n -> Fmt.pf ppf "numeric local label %d: survived resolution" n
    | `Expression e | `Data_value e -> Expr.pp_error ppf e
    | `Assignment_out_of_scope name ->
        Fmt.pf ppf "symbol assignment (%s = ...) is not in M1 scope" name
    | `Unknown_directive name -> Fmt.pf ppf "unknown directive %s" name
    | `Directive_rejected r -> Directives.pp_rejection ppf r
    | `Statement_before_section -> Fmt.string ppf "this appears before any section directive"
    (* Already rendered when it arrived: the target erased its own domain to
       hand this over, so the arm reproduces the message rather than composing
       one. *)
    | `Data_fixup m -> Fmt.string ppf m
    | `Directive_not_accepted name -> Fmt.pf ppf "the target did not accept %s" name
    | `Relax_ladder e -> Lowered_ast.pp_relax_error ppf e

  let error_code : error -> string = function
    | `Local_label_survived _ -> "parse.local-label"
    | `Expression _ -> "simplify.expression"
    | `Assignment_out_of_scope _ -> "simplify.assignment"
    | `Unknown_directive _ -> "simplify.directive"
    | `Directive_rejected r -> Option.value r.Directives.code ~default:"simplify.directive"
    | `Statement_before_section -> "lower.no-section"
    | `Data_value _ | `Data_fixup _ -> "lower.data"
    | `Directive_not_accepted _ -> "lower.directive"
    | `Relax_ladder _ -> "lower.relax-ladder"

  let diag ?origin (e : error) =
    Diagnostic.of_error ~code:error_code ~pp:pp_error
      ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:T.name ())
      e

  (* Where a target's failure becomes part of the pipeline's report. This is the
     erasure boundary {!Target_encode.ENCODE} names: [Make] is generic over [T]
     and cannot hold a [T.error], so the target renders its own domain and a
     diagnostic is what crosses (asm/docs/errors.md §2). Everything below this
     call keeps the typed payload. *)
  let target_diagnostic e = T.error_diagnostic (Err.Error.kind e)

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
           re-codes nothing (asm/docs/errors.md §2). *)
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
            errors := diag ~origin (`Assignment_out_of_scope name) :: !errors
        | Source_ast.Instruction { insn; origin } -> (
            match T.simplify_instruction ~features:T.default_features insn with
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

     Fragments, sections and symbols, with no addresses anywhere. The section
     bookkeeping is here rather than in the linker because "which section is
     current" is a property of the *input order*, and the linker sees sections
     already separated. *)

  type section_build = {
    sb_name : string;
    sb_perms : Perms.t;
    sb_kind : Lowered_ast.section_kind;
    mutable sb_align : int;
    mutable sb_frags : T.fixup_kind Lowered_ast.fragment list;  (** reversed *)
  }

  type symbol_build = {
    sy_name : string;
    mutable sy_binding : Lowered_ast.binding;
    mutable sy_visibility : Lowered_ast.visibility;
    mutable sy_kind : Directive.sym_kind;
    mutable sy_size : Expr.t option;
    mutable sy_section : string;
  }

  (* M3 §7: measured against real GNU `as` ([.weak;.local], [.local;.weak],
     [.globl;.weak] and [.weak;.globl] all produced a WEAK symbol) rather than
     assumed from the ELF spec. [.weak] is sticky against a later
     [.local]/[.globl]; those two remain plain last-wins against each other
     whenever [.weak] has not applied. *)
  let apply_binding_directive current (directive : [ `Weak | `Local | `Global ]) =
    match directive with
    | `Weak -> Lowered_ast.Weak
    | `Local -> (
        match current with Lowered_ast.Weak -> Lowered_ast.Weak | _ -> Lowered_ast.Local)
    | `Global -> (
        match current with Lowered_ast.Weak -> Lowered_ast.Weak | _ -> Lowered_ast.Global)

  let lower ~state (m : T.Instruction.t Normalized_ast.module_) =
    let errors = ref [] in
    let sections : section_build list ref = ref [] in
    let declared = ref [] in
    let symbols : symbol_build list ref = ref [] in
    let commons : Lowered_ast.common list ref = ref [] in
    (* [state] is retained in the public entry point for direct normalized-AST
       producers, but parsed programs always pass [default_state].  Target
       directives in [m] then replay in source order below. *)
    let state = ref state in
    let current = ref None in
    let ensure_section name perms kind =
      match List.find_opt (fun s -> String.equal s.sb_name name) !sections with
      | Some s ->
          current := Some s;
          s
      | None ->
          let s =
            { sb_name = name; sb_perms = perms; sb_kind = kind; sb_align = 1; sb_frags = [] }
          in
          sections := !sections @ [ s ];
          current := Some s;
          s
    in
    let with_section origin f =
      match !current with
      | Some s -> f s
      | None -> errors := diag ~origin `Statement_before_section :: !errors
    in
    let symbol name =
      match List.find_opt (fun s -> String.equal s.sy_name name) !symbols with
      | Some s -> s
      | None ->
          let s =
            {
              sy_name = name;
              sy_binding = Lowered_ast.Local;
              sy_visibility = Lowered_ast.Default;
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
            | Directive.Section { name; perms; nobits } ->
                ignore
                  (ensure_section name perms
                     (if nobits then Lowered_ast.Nobits else Lowered_ast.Progbits))
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
                    (* One fill per possible gap, not one pattern to repeat.
                       On x86 the padding is a *single* no-op sized for the
                       gap, so cycling a shorter one produces a different
                       instruction stream of the same length - which is exactly
                       what GAS does not do.

                       A gap is always smaller than the boundary, so lengths 1
                       to boundary-1 cover every case. The target may refuse
                       some of them - A32 and A64 have no one-byte no-op - and
                       an empty entry means "this target cannot pad by that
                       much", which layout reports if it ever needs one.

                       Only for an executable section: a zero-filled gap in
                       .text decodes as an instruction nobody wrote, and
                       outside .text zero is exactly right. *)
                    let fills =
                      if Perms.executable s.sb_perms then
                        Array.init
                          (max 0 (boundary - 1))
                          (fun i ->
                            match T.nop_bytes ~length:(i + 1) with Ok b -> b | Error _ -> "")
                      else Array.init (max 0 (boundary - 1)) (fun i -> String.make (i + 1) '\000')
                    in
                    s.sb_frags <- Lowered_ast.Align { boundary; fills; origin } :: s.sb_frags)
            (* One fragment per value, and a value that is not already a number
               becomes a fixup rather than being evaluated here: an initializer
               naming a symbol has no value until the linker chooses addresses,
               which is the same reason an instruction operand naming one does
               not. The bytes emitted are a placeholder of the right width, so
               layout is exact either way. *)
            | Directive.Data { width; values } ->
                List.iter
                  (fun value ->
                    match Expr.fold Expr.no_env value with
                    | Error m -> errors := diag ~origin (`Data_value (Err.Error.kind m)) :: !errors
                    | Ok folded -> (
                        let bytes =
                          match folded with
                          | Expr.Const v -> (
                              match Bigint.to_int64_opt v with
                              | Some n ->
                                  Ok
                                    (String.init width (fun i ->
                                         Char.chr
                                           (Int64.to_int
                                              (Int64.logand
                                                 (Int64.shift_right_logical n (8 * i))
                                                 0xFFL))))
                              | None -> Error "value does not fit 64 bits")
                          | _ -> Ok (String.make width '\000')
                        in
                        let fixups =
                          match folded with
                          | Expr.Const _ -> Ok []
                          | _ -> (
                              match T.data_fixup ~width with
                              | Error e -> Error (Diagnostic.message (target_diagnostic e))
                              | Ok kind ->
                                  Ok
                                    [
                                      {
                                        Lowered_ast.kind;
                                        kind_name = T.fixup_kind_name kind;
                                        family = T.fixup_family kind;
                                        role = T.fixup_role kind;
                                        name = "data";
                                        slices =
                                          [
                                            {
                                              Lowered_ast.bit_offset = 0;
                                              bit_width = width * 8;
                                              value_lsb = 0;
                                            };
                                          ];
                                        byte_offset = 0;
                                        container = width;
                                        pc_bias = 0;
                                        range = Lowered_ast.Bitpattern (width * 8);
                                        value = folded;
                                        pairing = Lowered_ast.Unpaired;
                                        origin;
                                      };
                                    ])
                        in
                        match (bytes, fixups) with
                        | Error m, _ | _, Error m ->
                            errors := diag ~origin (`Data_fixup m) :: !errors
                        | Ok bytes, Ok fixups ->
                            with_section origin (fun s ->
                                s.sb_frags <-
                                  Lowered_ast.Bytes { bytes; form = None; fixups; origin }
                                  :: s.sb_frags)))
                  values
            | Directive.Zero { length } ->
                with_section origin (fun s ->
                    s.sb_frags <- Lowered_ast.Zero { length; origin } :: s.sb_frags)
            | Directive.Global { name } ->
                let sy = symbol name in
                sy.sy_binding <- apply_binding_directive sy.sy_binding `Global
            | Directive.Weak { name } ->
                let sy = symbol name in
                sy.sy_binding <- apply_binding_directive sy.sy_binding `Weak
            | Directive.Local { name } ->
                let sy = symbol name in
                sy.sy_binding <- apply_binding_directive sy.sy_binding `Local
            | Directive.Common { name; size; align } ->
                commons :=
                  !commons
                  @ [ { Lowered_ast.comm_name = name; comm_size = size; comm_align = align } ]
            | Directive.Sym_type { name; kind } -> (symbol name).sy_kind <- kind
            | Directive.Sym_size { name; size } ->
                (symbol name).sy_size <- Some size;
                with_section origin (fun s ->
                    s.sb_frags <- Lowered_ast.Set_size { name; size; origin } :: s.sb_frags)
            | Directive.Target_state { name; argument } -> (
                match T.handle_directive ~name ~argument !state with
                | T_intf.Handled { state = s; _ } -> state := s
                | T_intf.Rejected e -> errors := parse_diagnostic e :: !errors
                | T_intf.Unhandled ->
                    errors := diag ~origin (`Directive_not_accepted name) :: !errors))
        | Normalized_ast.Instruction { insn; origin } ->
            with_section origin (fun s ->
                match T.lower_instruction !state insn with
                | Error e -> errors := target_diagnostic e :: !errors
                | Ok lowered ->
                    List.iter
                      (fun l ->
                        let qualify (a : _ Lowered_ast.encoded_form) =
                          { a with Lowered_ast.form = T.name ^ "." ^ a.Lowered_ast.form }
                        in
                        match T.encode l with
                        | Error e -> errors := target_diagnostic e :: !errors
                        | Ok (`Fixed a) ->
                            let a = qualify a in
                            s.sb_frags <-
                              Lowered_ast.Bytes
                                {
                                  bytes = a.Lowered_ast.bytes;
                                  form = Some a.Lowered_ast.form;
                                  fixups = a.Lowered_ast.fixups;
                                  origin;
                                }
                              :: s.sb_frags
                        | Ok (`Relax alts) -> (
                            let alts = List.map qualify alts in
                            (* Checked here *and* in the image, because a direct
                               Lowered_ast producer never passes through this
                               function and an ill-formed ladder would otherwise
                               reach layout unchecked. *)
                            match Lowered_ast.validate_relax alts with
                            | Error m ->
                                errors := diag ~origin (`Relax_ladder (Err.Error.kind m)) :: !errors
                            | Ok () ->
                                s.sb_frags <- Lowered_ast.Relax { alts; origin } :: s.sb_frags))
                      lowered))
      m.Normalized_ast.items;
    if !errors <> [] then Diag.fail ~pos:__POS__ (List.rev !errors)
    else
      Ok
        {
          Lowered_ast.unit_name = m.Normalized_ast.unit_name;
          sections =
            List.map
              (fun s ->
                {
                  Lowered_ast.sec =
                    {
                      Lowered_ast.sec_name = s.sb_name;
                      perms = s.sb_perms;
                      alignment = s.sb_align;
                      kind = s.sb_kind;
                    };
                  fragments = List.rev s.sb_frags;
                })
              !sections;
          symbols =
            List.map
              (fun s ->
                {
                  Lowered_ast.name = s.sy_name;
                  binding = s.sy_binding;
                  visibility = s.sy_visibility;
                  kind = s.sy_kind;
                  size = s.sy_size;
                  section = s.sy_section;
                })
              !symbols;
          commons = !commons;
          declared_sections = !declared;
        }

  (* {1 Stage 4 - the image (§8, §9)} *)

  (* An explicit entry wins. The single-global inference below was enough
     while every fixture declared exactly one global, but three of the M2
     fixtures declare two - a callee, or a data object, alongside the entry -
     and cardinality then names nothing at all. Keeping the inference as the
     fallback is what lets [assemble]/[assemble_many] stay total.

     [Weak] never counts as a candidate (M3 §8): an entry silently landing on
     a weak symbol that later loses to nothing - or to a different input's
     definition - would be a footgun, and cardinality over every input's
     [Global] declarations is the direct generalization of the single-module
     rule, not a new one. *)
  let policy_for_many ?entry (modules : T.fixup_kind Lowered_ast.module_ list) =
    let globals =
      List.sort_uniq compare
        (List.concat_map
           (fun (m : T.fixup_kind Lowered_ast.module_) ->
             List.filter_map
               (fun (s : Lowered_ast.symbol) ->
                 match s.Lowered_ast.binding with
                 | Lowered_ast.Global -> Some s.Lowered_ast.name
                 | Lowered_ast.Local | Lowered_ast.Weak -> None)
               m.Lowered_ast.symbols)
           modules)
    in
    let entry_symbol =
      match entry with
      | Some e -> Some e
      | None -> ( match globals with [ g ] -> Some g | _ -> None)
    in
    { Image.default_policy with entry_symbol }

  let policy_for ?entry (m : T.fixup_kind Lowered_ast.module_) = policy_for_many ?entry [ m ]

  (* The other erasure boundary: [Image] stores the fixup evaluator in a
     [laid_out], which the architecture-erased [DRIVER] hands around, so the
     closure cannot mention [T.error]. The target renders here, exactly as it
     does at [target_diagnostic] (asm/docs/errors.md §2). *)
  let evaluate kind ~place ~target =
    Result.map_error target_diagnostic (T.evaluate_fixup kind ~place ~target)

  let plan ?entry m = Image.plan_image ~evaluate (policy_for ?entry m) [ m ]

  let plan_many ?entry (modules : T.fixup_kind Lowered_ast.module_ list) =
    Image.plan_image ~evaluate (policy_for_many ?entry modules) modules

  let fixup_observations = Image.fixup_observations

  (* {1 The whole path}

     Each [Diag.stage] marks a phase boundary at the position of the crossing,
     which is what makes the event trail read parse -> simplify -> lower -> plan
     rather than leaving the phase implicit in a diagnostic code prefix
     (asm/docs/errors.md §2). The payload is untouched: a stage mark records
     that a failure crossed here, not a new failure. *)

  let assemble ?entry ~unit_name ~source () =
    let open Err.Syntax in
    let stage r = Diag.stage ~pos:__POS__ Err.Action.Map r in
    let* src = stage (parse ~unit_name ~source) in
    let* norm, _final_state = stage (simplify src) in
    let* low = stage (lower ~state:T.default_state norm) in
    stage (plan ?entry low)

  (* M3's multi-module entry point. Every input is lowered independently
     ([lower_one]) - a failure in one input does not stop the others from
     being checked too, via [Err.Accum.map] rather than the [let*] short-
     circuit [lower_one]'s own three stages still use internally - and only
     the resulting module list crosses into [plan_many], the one place
     cross-input resolution (§2) actually happens. *)
  let assemble_many ?entry (sources : (string * Span.source) list) () =
    let open Err.Syntax in
    let stage r = Diag.stage ~pos:__POS__ Err.Action.Map r in
    let lower_one (unit_name, source) =
      let* src = stage (parse ~unit_name ~source) in
      let* norm, _final_state = stage (simplify src) in
      stage (lower ~state:T.default_state norm)
    in
    let* modules =
      Err.Accum.map ~pos:__POS__ lower_one sources
      |> Err.Accum.fold_errors (fun errs -> List.concat_map Err.Error.kind errs)
    in
    stage (plan_many ?entry modules)

  (* {1 Dumps (asm/docs/contracts.md §1)} *)

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
        | Ok (n, _final_state) ->
            Result.map (Fmt.to_to_string Lowered_ast.pp) (lower ~state:T.default_state n))

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
    let matches k =
      let b = Int64.of_int (1 lsl k) in
      let stop = Int64.mul (Int64.div (Int64.add here (Int64.sub b 1L)) b) b in
      let n = Int64.to_int (Int64.sub stop here) in
      if
        n > 0
        && pos + n <= len
        &&
        match T.nop_bytes ~length:n with
        | Ok s -> String.equal s (String.sub bytes pos n)
        | Error _ -> false
      then Some (1 lsl k, n)
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

  let disassemble_lines ~address bytes =
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
            match T.decode { T.state = T.default_state; address = here } bytes ~pos with
            | Error e -> Diag.fail ~pos:__POS__ [ target_diagnostic e ]
            | Ok (insn, form, n) ->
                go (pos + n)
                  ({
                     at = here;
                     run = run_of pos n;
                     text = Fmt.to_to_string T.Instruction.pp insn;
                     form = Some form;
                   }
                  :: acc))
    in
    go 0 []

  let dump_disasm_diagnostic ~address bytes =
    match disassemble_lines ~address bytes with
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

  let dump_disasm_canonical ~address bytes =
    match disassemble_lines ~address bytes with
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
