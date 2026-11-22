(* The diagnostic-code taxonomy, checked rather than conventional.

   Before the typed-error migration this was ~40 string literals scattered
   across the tree, and nothing stopped two unrelated failures reporting under
   one code or a typo inventing a new one. Each is now a total function of its
   domain's variant - the compiler checks *that*, because a non-exhaustive match
   does not build - so what is left to check is the property no single domain
   can see: that two domains do not claim the same code.

   The inventory is printed as well as asserted. A code is a user-visible
   contract (docs/contracts.md), so adding or moving one should read as a
   diff in this file rather than being discovered by a fixture.

   Enumeration is by *code*, not by constructor: several constructors share a
   code on purpose - image.undefined covers both "declared and never defined"
   and "a fixup names a symbol nothing defines" - so the list below needs one
   value per distinct code, not one per tag. Totality is the compiler's job. *)

open Rivet_foundation

let claims = ref []
let claim domain code = claims := (domain, code) :: !claims
let record domain code_of values = List.iter (fun v -> claim domain (code_of v)) values

(* {1 foundation} *)

let () =
  record "Bigint" Bigint.error_code
    [
      `Division_by_zero;
      `Negative_shift { Bigint.shift_op = `Left; amount = -1 };
      `Negative_width { Bigint.width_of = `Signed; width = -1 };
      `Nonpositive_width 0;
      `Empty_literal;
      `Does_not_fit { Bigint.value = Bigint.zero; bits = 8 };
    ];
  record "Byte_cursor" Byte_cursor.error_code
    [ `Range_outside_string { Byte_cursor.start = 0; len = None; length = 0 } ]

(* {1 asm_syntax} *)

let () =
  record "Lexer" Rivet_asm_syntax.Lexer.kind_code [ `Unterminated_string ];
  record "Local_labels" Rivet_asm_syntax.Local_labels.kind_code [ `Reserved_namespace "x" ];
  record "Parse_lines" Rivet_asm_syntax.Parse_lines.error_code
    [ `Unexpected_token None; `Empty_expression ]

(* {1 asm_core and codec} *)

let () =
  record "Expr" Rivet_asm_core.Expr.error_code
    [
      `Arithmetic `Division_by_zero;
      `Shift_out_of_range Bigint.zero;
      `Not_absolute (Rivet_asm_core.Expr.Symbol "s");
      `Resource { Rivet_asm_core.Expr.limit = 1; needed = 2 };
    ];
  record "Relax" Rivet_asm_core.Lowered_ast.relax_error_code
    [
      `Empty_ladder;
      `Rung_not_shorter
        { Rivet_asm_core.Lowered_ast.rung = "a"; rung_bytes = 1; other = "b"; other_bytes = 1 };
      `Rung_fixups_differ
        { Rivet_asm_core.Lowered_ast.rung = "a"; rung_bytes = 1; other = "b"; other_bytes = 1 };
      `Fixup_target_differs "f";
    ];
  (* Codec answers [Some] only where it is the only layer that could have seen
     the mistake (docs/errors.md §2); everywhere else the caller's phase
     names it, so those arms contribute no code of their own. *)
  List.iter
    (fun e -> match Rivet_codec.code e with Some c -> claim "Codec" c | None -> ())
    [
      `Unknown_rung { Rivet_codec.wanted = "r"; declared = [] };
      `Rung_inapplicable { Rivet_codec.wanted = "r"; applicable = [] };
      `No_form;
    ]

(* {1 image} *)

let () =
  record "Image" Rivet_image.error_code
    [
      `No_modules;
      `No_section;
      `Section_kind_mismatch ".bss";
      `Invalid_module
        { Rivet_image.unit_name = "u"; reason = Rivet_asm_core.Lowered_ast.Empty_name `Unit };
      `Duplicate_unit "u";
      `Mixed_targets [ "a"; "b" ];
      `Rwx_segment ".text";
      `Duplicate_definition "s";
      `Common_conflicts_with_local "s";
      `Declared_never_defined "s";
      `Export_undefined "s";
      `Entry_undefined "s";
      `Ambiguous_symbol "s";
      `Too_large { Rivet_image.size = 2; limit = 1 };
      `Relax_ladder `Empty_ladder;
      `Relax_unreachable { Rivet_image.widest = "w"; at = 0 };
      `Align_fill { Rivet_image.pad = 1; boundary = 2 };
      `Nobits_content ".bss";
      `Overflow { Rivet_image.segment = ".text"; address = 0L; size = 0 };
      `Overlap
        {
          Rivet_image.a_name = ".text";
          a_at = 0L;
          a_size = 0;
          b_name = ".data";
          b_at = 0L;
          b_size = 0;
        };
      `No_address ".text";
      `Alignment { Rivet_image.segment = ".text"; needs = 4; address = 1L };
      `Symbol_size { Rivet_image.symbol = "s"; reason = `Arithmetic `Division_by_zero };
      `Symbol_size_too_wide "s";
      `Fixup_target_too_wide "f";
      `Fixup_wraps "f";
      `Fixup_out_of_range
        { Rivet_image.fixup = "f"; value = 0L; range = Rivet_asm_core.Lowered_ast.Signed 8; at = 0 };
    ]

(* {1 targets}

   One representative per distinct code. x86_32 and x86_64 share a domain - the
   family functor produces one - so listing x86_64 covers both. *)

let () =
  record "x86 encode" X86_64.error_kind_code
    [
      `Unknown_instruction "m";
      `Bad_branch_suffix { X86_64.mnemonic = "m"; rungs = [] };
      `No_form "op";
      `Codec `No_form;
      `Decode_no_match;
      `Displacement_not_8bit;
      `No_data_relocation 4;
      `Negative_padding 0;
    ];
  record "x86 parse" X86_64.parse_error_kind_code [ `Unknown_register "r" ];
  record "arm encode" Arm.error_kind_code
    [
      `Unknown_instruction "m";
      `Thumb_out_of_scope;
      `Codec `No_form;
      `Decode_short;
      `Branch_out_of_range;
      `No_data_relocation 4;
      `Padding_not_word_multiple;
    ];
  record "arm parse" Arm.parse_error_kind_code
    [ `Unknown_register "r"; `Thumb_directive_out_of_scope ];
  record "aarch64 encode" Aarch64.error_kind_code
    [
      `Unknown_instruction "m";
      `Too_many_operands;
      `Codec `No_form;
      `Decode_short;
      `Branch_out_of_range;
      `No_data_relocation 4;
      `Padding_not_word_multiple;
    ];
  record "aarch64 parse" Aarch64.parse_error_kind_code [ `Unknown_register "r" ]

(* {1 driver}

   Through the registry's own functor application, so this is the pipeline the
   CLI runs and not a second instantiation. *)

let () =
  record "Pipeline" Rivet_driver.Registry.X86_64.error_code
    [
      `Local_label_survived 1;
      `Expression (`Arithmetic `Division_by_zero);
      `Assignment_out_of_scope "s";
      `Unknown_directive ".d";
      `Directive_rejected { Rivet_text_pipeline.Directives.code = None; message = ""; reasons = [] };
      `Statement_before_section;
      `Data_fixup "";
      `Section_attributes_changed ".s";
      `Directive_not_accepted ".d";
      `Relax_ladder `Empty_ladder;
    ];
  record "Portable" Rivet_driver.Portable.error_code
    [
      `Unknown_target { Rivet_driver.Portable.requested = "t"; known = [] };
      `Bind_multi_segment [];
      `Diagnostics [];
    ]

let%expect_test ".size keeps its typed expression failure" =
  let source = Span.source ~name:"test.s" ~contents:"s" in
  let span = Span.of_offset ~source ~offset:0 ~length:1 in
  let symbol = Rivet_asm_syntax.Token.make (Rivet_asm_syntax.Token.Ident "s") span in
  (match
     Rivet_text_pipeline.Directives.normalize ~data_widths:[]
       ~align_unit:Rivet_asm_core.Directive.Bytes ~name:".size" ~arguments:[ [ symbol ]; [] ]
   with
  | Error error ->
      let rejection = Err.Error.kind error in
      Fmt.pr "%a; typed-cause=%b@." Rivet_text_pipeline.Directives.pp_rejection rejection
        (match rejection.Rivet_text_pipeline.Directives.reasons with
        | [ reason ] -> Err.Error.kind reason = `Empty_expression
        | _ -> false)
  | Ok _ -> Fmt.pr "unexpected success@.");
  [%expect {| .size: empty expression; typed-cause=true |}]

let%expect_test "data directives accumulate every expression failure" =
  let source = Span.source ~name:"test.s" ~contents:"+" in
  let span = Span.of_offset ~source ~offset:0 ~length:1 in
  let plus = Rivet_asm_syntax.Token.make Rivet_asm_syntax.Token.Plus span in
  (match
     Rivet_text_pipeline.Directives.normalize
       ~data_widths:[ (".long", 4) ]
       ~align_unit:Rivet_asm_core.Directive.Bytes ~name:".long" ~arguments:[ []; [ plus ] ]
   with
  | Error error ->
      let rejection = Err.Error.kind error in
      Fmt.pr "%a; typed-causes=%b@." Rivet_text_pipeline.Directives.pp_rejection rejection
        (match rejection.Rivet_text_pipeline.Directives.reasons with
        | [ first; second ] -> (
            match (Err.Error.kind first, Err.Error.kind second) with
            | `Empty_expression, `Malformed_expression _ -> true
            | _ -> false)
        | _ -> false)
  | Ok _ -> Fmt.pr "unexpected success@.");
  [%expect
    {|
    .long: every value must be an expression: empty expression; malformed expression; typed-causes=true
    |}]

(* {1 The two assertions} *)

let inventory () = List.sort_uniq compare (List.map (fun (d, c) -> (c, d)) !claims)

let%expect_test "every diagnostic code, and the domain that claims it" =
  List.iter (fun (code, domain) -> Fmt.pr "%-28s %s@." code domain) (inventory ());
  [%expect
    {|
    aarch64.data-fixup           aarch64 encode
    aarch64.decode               aarch64 encode
    aarch64.encode               aarch64 encode
    aarch64.fixup                aarch64 encode
    aarch64.lower                aarch64 encode
    aarch64.nop                  aarch64 encode
    aarch64.operand              aarch64 parse
    aarch64.simplify             aarch64 encode
    arm.data-fixup               arm encode
    arm.decode                   arm encode
    arm.directive                arm parse
    arm.encode                   arm encode
    arm.fixup                    arm encode
    arm.lower                    arm encode
    arm.nop                      arm encode
    arm.operand                  arm parse
    arm.simplify                 arm encode
    bigint.division-by-zero      Bigint
    bigint.does-not-fit          Bigint
    bigint.literal               Bigint
    bigint.negative-shift        Bigint
    bigint.negative-width        Bigint
    bigint.nonpositive-width     Bigint
    bind.address                 Image
    bind.alignment               Image
    bind.fixup                   Image
    bind.fixup-range             Image
    bind.multi-segment           Portable
    bind.overflow                Image
    bind.overlap                 Image
    bind.size                    Image
    byte-cursor.range            Byte_cursor
    codec.rung-inapplicable      Codec
    codec.unknown-rung           Codec
    expr.arithmetic              Expr
    expr.not-absolute            Expr
    expr.resource                Expr
    expr.shift                   Expr
    image.align-fill             Image
    image.ambiguous              Image
    image.common-conflicts-local Image
    image.duplicate              Image
    image.duplicate-unit         Image
    image.entry                  Image
    image.export                 Image
    image.invalid-module         Image
    image.kind-mismatch          Image
    image.mixed-targets          Image
    image.no-modules             Image
    image.no-section             Image
    image.nobits-content         Image
    image.relax-ladder           Image
    image.relax-unreachable      Image
    image.rwx-segment            Image
    image.too-large              Image
    image.undefined              Image
    lex                          Lexer
    lower.data                   Pipeline
    lower.directive              Pipeline
    lower.no-section             Pipeline
    lower.relax-ladder           Pipeline
    lower.section-attributes     Pipeline
    parse                        Parse_lines
    parse.expression             Parse_lines
    parse.local-label            Local_labels
    parse.local-label            Pipeline
    portable.diagnostics         Portable
    portable.unknown-target      Portable
    relax.empty                  Relax
    relax.fixup-differs          Relax
    relax.fixups-differ          Relax
    relax.not-shorter            Relax
    simplify.assignment          Pipeline
    simplify.directive           Pipeline
    simplify.expression          Pipeline
    x86.branch-suffix            x86 encode
    x86.data-fixup               x86 encode
    x86.decode                   x86 encode
    x86.encode                   x86 encode
    x86.fixup                    x86 encode
    x86.lower                    x86 encode
    x86.nop                      x86 encode
    x86.operand                  x86 parse
    x86.simplify                 x86 encode |}]

(* A code two domains may both claim, and why. Sharing is not automatically
   wrong - what would be wrong is two *unrelated* failures answering to one
   code, because a consumer filtering on it would get shapes it cannot handle.
   So a share is allowed when it is declared here with a reason, and an
   undeclared one is a failure.

   [parse.local-label] is the one: [Local_labels] reports a reference that
   resolves to nothing, and the pipeline reports a numeric definition that
   survived resolution - an invariant of its own that it keeps as a diagnostic
   rather than an assertion, because a silently dropped definition is a label
   nothing can reach. All three are "something is wrong with a numeric local
   label", which is what a consumer filtering on the code is asking. *)
let declared_shares =
  [ ("parse.local-label", "numeric local labels: source-side and pipeline-side") ]

let%expect_test "no undeclared code is claimed by two domains" =
  let claimed_twice code =
    List.length (List.filter (fun (c, _) -> String.equal c code) (inventory ())) > 1
  in
  let undeclared =
    List.filter
      (fun (code, _) -> claimed_twice code && not (List.mem_assoc code declared_shares))
      (inventory ())
  in
  List.iter
    (fun (code, why) ->
      if claimed_twice code then Fmt.pr "shared (declared): %-24s %s@." code why
      else Fmt.pr "STALE DECLARATION: %s is no longer shared@." code)
    declared_shares;
  (match undeclared with
  | [] -> Fmt.pr "no undeclared collisions@."
  | cs -> List.iter (fun (code, domain) -> Fmt.pr "COLLISION %s claimed by %s@." code domain) cs);
  [%expect
    {|
    shared (declared): parse.local-label        numeric local labels: source-side and pipeline-side
    no undeclared collisions
    |}]
