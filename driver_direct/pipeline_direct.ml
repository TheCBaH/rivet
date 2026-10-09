(* The stages below source text, over the encoding half of a target alone: a
   producer that builds normalized modules as values configures a target,
   lowers its module, plans the image, and links no lexer, parser or other
   target. The text pipeline includes this and adds the front end. *)

open Foundation
open Asm_core

module Make (T : Target_encode.ENCODE) = struct
  let name = T.name
  let triple = T.triple

  (* The failures that belong to no target and no library, but to walking a
     module through these stages. *)
  type error =
    [ `Statement_before_section
    | `Data_value of Expr.error
    | `Data_fixup of string
    | `Directive_not_accepted of string
    | `Relax_ladder of Lowered_ast.relax_error
    | `Invalid_configuration of string ]

  let pp_error ppf : error -> unit = function
    | `Data_value e -> Expr.pp_error ppf e
    | `Statement_before_section -> Fmt.string ppf "this appears before any section directive"
    (* Already rendered when it arrived: the target erased its own domain to
       hand this over, so the arm reproduces the message rather than composing
       one. *)
    | `Data_fixup m -> Fmt.string ppf m
    | `Directive_not_accepted name -> Fmt.pf ppf "the target did not accept %s" name
    | `Relax_ladder e -> Lowered_ast.pp_relax_error ppf e
    | `Invalid_configuration m -> Fmt.pf ppf "invalid feature configuration: %s" m

  let error_code : error -> string = function
    | `Statement_before_section -> "lower.no-section"
    | `Data_value _ | `Data_fixup _ -> "lower.data"
    | `Directive_not_accepted _ -> "lower.directive"
    | `Relax_ladder _ -> "lower.relax-ladder"
    | `Invalid_configuration _ -> "config.invalid"

  let diag ?origin (e : error) =
    Diagnostic.of_error ~code:error_code ~pp:pp_error
      ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:T.name ())
      e

  (* Where a target's failure becomes part of the pipeline's report. This is the
     erasure boundary {!Target_encode.ENCODE} names: [Make] is generic over [T]
     and cannot hold a [T.error], so the target renders its own domain and a
     diagnostic is what crosses (docs/errors.md §2). *)
  let target_diagnostic e = T.error_diagnostic (Err.Error.kind e)

  (* {1 Configuration}

     A caller's feature spec is resolved once per entry point, against this target's own
     components, into the initial state every stage starts from. An unknown or contradictory
     spec fails here with one diagnostic, before any source is read. [None] is the unconfigured
     assembler: [T.default_state], every implemented component on. *)

  let components = T.components
  let configure spec = Target_config.resolve T.components spec

  let initial_state = function
    | None -> Ok T.default_state
    | Some spec -> (
        match configure spec with
        | Ok c -> Ok (T.initial_state c)
        | Error m -> Diag.fail ~pos:__POS__ [ diag (`Invalid_configuration m) ])

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

  (* What a state directive written in a module does. A producer that builds
     instructions as values has none; the text front end supplies its target's
     handler, whose rejections are already diagnostics. *)
  type 'state state_directive = Handled of 'state | Rejected of Diagnostic.t | Unhandled

  let lower_with ~(target_state : name:string -> argument:string -> T.target_state -> T.target_state state_directive) ~state
      (m : T.Instruction.t Normalized_ast.module_) =
    let errors = ref [] in
    let sections : section_build list ref = ref [] in
    let declared = ref [] in
    let symbols : symbol_build list ref = ref [] in
    let commons : (Lowered_ast.common * Origin.t) list ref = ref [] in
    let made_local = ref [] in
    (* [state] is retained in the public entry point for direct normalized-AST
       producers, and parsed programs pass the initial state of the caller's configuration.  Target
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
                              (* [to_int64_opt] alone rejects a raw bit pattern at or above
                                 2^63 - a [.quad] holding a float's bits or an unsigned
                                 constant ([0x8000000000000000], [0xFFFFFFFFFFFFFFFF]) is
                                 ordinary, common data, not an out-of-range value, so the
                                 unsigned reading is tried second rather than the whole
                                 initializer being rejected. Both readings produce the same
                                 two's-complement bytes below; only which one succeeds at all
                                 differs. *)
                              match
                                match Bigint.to_int64_opt v with
                                | Some _ as r -> r
                                | None -> Bigint.to_uint64_opt v
                              with
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
                made_local := name :: !made_local;
                sy.sy_binding <- apply_binding_directive sy.sy_binding `Local
            | Directive.Common { name; size; align } ->
                commons :=
                  !commons
                  @ [
                      ( { Lowered_ast.comm_name = name; comm_size = size; comm_align = align },
                        origin );
                    ]
            | Directive.Sym_type { name; kind } -> (symbol name).sy_kind <- kind
            | Directive.Sym_size { name; size } ->
                (symbol name).sy_size <- Some size;
                with_section origin (fun s ->
                    s.sb_frags <- Lowered_ast.Set_size { name; size; origin } :: s.sb_frags)
            | Directive.Cfi _ -> ()
            | Directive.Target_state { name; argument } -> (
                match target_state ~name ~argument !state with
                | Handled s -> state := s
                | Rejected d -> errors := d :: !errors
                | Unhandled ->
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
                        match T.encode_in !state l with
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
    (* [.local x] then [.comm x, size, align] is how a static zero-initialized object is spelled
       (the compiler's [static long scratch[512];]): GAS then allocates [x] in this input's own
       [.bss] as a local symbol rather than as a common one (checked with real as/readelf: a
       LOCAL OBJECT in [.bss], not [COM]). A common left for the linker could never satisfy a
       local reference, which only resolves within its own input. Decided on the final binding,
       so [.globl] after [.local] still makes an ordinary common. *)
    let local_commons, global_commons =
      List.partition
        (fun ((c : Lowered_ast.common), _) ->
          List.mem c.Lowered_ast.comm_name !made_local
          &&
          match
            List.find_opt (fun s -> String.equal s.sy_name c.Lowered_ast.comm_name) !symbols
          with
          | Some { sy_binding = Lowered_ast.Local; _ } -> true
          | _ -> false)
        !commons
    in
    if local_commons <> [] then begin
      let saved = !current in
      let bss = ensure_section ".bss" Perms.rw Lowered_ast.Nobits in
      current := saved;
      List.iter
        (fun ((c : Lowered_ast.common), origin) ->
          let name = c.Lowered_ast.comm_name in
          bss.sb_align <- max bss.sb_align c.Lowered_ast.comm_align;
          bss.sb_frags <-
            Lowered_ast.Zero { length = c.Lowered_ast.comm_size; origin }
            :: Lowered_ast.Label_def { name; origin }
            :: Lowered_ast.Align
                 {
                   boundary = c.Lowered_ast.comm_align;
                   fills =
                     Array.init
                       (max 0 (c.Lowered_ast.comm_align - 1))
                       (fun i -> String.make (i + 1) '\000');
                   origin;
                 }
            :: bss.sb_frags;
          (symbol name).sy_section <- ".bss")
        local_commons
    end;
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
          commons = List.map fst global_commons;
          declared_sections = !declared;
        }

  let lower ~state m = lower_with ~target_state:(fun ~name:_ ~argument:_ _ -> Unhandled) ~state m

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
     does at [target_diagnostic] (docs/errors.md §2). *)
  let evaluate kind ~place ~target =
    Result.map_error target_diagnostic (T.evaluate_fixup kind ~place ~target)

  (* M3 §5's merge-gap fill (docs/design.md §12): [T.merge_fill], where the
     target has one, is its OWN linker's measured fill for a gap a merge
     inserts in an EXECUTABLE output section - not necessarily [T.nop_bytes]
     again (x86_32's ld and as disagree; see {!Target_encode.ENCODE.merge_fill}).
     [None], and every non-executable gap even where [Some], falls through to
     [Image.plan_image]'s own zero-fill default, which is what makes this the
     identity substitute for a single-module [plan]: no merge boundary exists
     there to fill at all. *)
  let merge_fill ~executable pad =
    match T.merge_fill with Some f when executable -> f ~length:pad | _ -> String.make pad '\000'

  (* The runtime-vararg frontier gap : [T.pad_section_to_alignment],
     where set, rounds a merged Progbits section's own final size up to its recorded
     alignment - GAS's own end-of-section behavior on RISC-V alone, distinct from
     {!merge_fill}'s between-contribution gap above. The padding bytes are
     [T.nop_bytes], the same source the assembler's own [.align] padding already
     uses within a module (not [T.merge_fill], which is [None]/zero-fill on every
     target that sets this flag) - falling back to zero fill only if [nop_bytes]
     itself rejects the length, which should not happen since section sizes here
     are always instruction-word multiples. *)
  let section_pad =
    if T.pad_section_to_alignment then
      Some
        (fun ~length ->
          match T.nop_bytes ~length with Ok b -> b | Error _ -> String.make length '\000')
    else None

  let plan ?entry m =
    Image.plan_image ~evaluate ~fill:merge_fill ?section_pad (policy_for ?entry m) [ m ]

  let plan_many ?entry (modules : T.fixup_kind Lowered_ast.module_ list) =
    Image.plan_image ~evaluate ~fill:merge_fill ?section_pad (policy_for_many ?entry modules)
      modules

  let fixup_observations = Image.fixup_observations
end
