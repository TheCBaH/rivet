(* The stages below source text, over the encoding half of a target alone: a
   producer that builds normalized modules as values configures a target,
   lowers its module, plans the image, and links no lexer, parser or other
   target. The text pipeline includes this and adds the front end. *)

open Rivet_foundation
open Rivet_asm_core
module List = Safe_list

module Make (T : Target_encode.ENCODE) = struct
  let name = T.name
  let triple = T.triple

  (* The failures that belong to no target and no library, but to walking a
     module through these stages. *)
  type error =
    [ `Statement_before_section
    | `Data_value of Expr.error
    | `Data_fixup of string
    | `Section_attributes_changed of string
    | `Section_stack_empty
    | `Directive_not_accepted of string
    | `Relax_ladder of Lowered_ast.relax_error
    | `Invalid_configuration of string
    | `Beyond_ceiling of string list
    | `Unknown_segment of string ]

  let pp_error ppf : error -> unit = function
    | `Data_value e -> Expr.pp_error ppf e
    | `Statement_before_section -> Fmt.string ppf "this appears before any section directive"
    (* Already rendered when it arrived: the target erased its own domain to
       hand this over, so the arm reproduces the message rather than composing
       one. *)
    | `Data_fixup m -> Fmt.string ppf m
    | `Section_attributes_changed n ->
        Fmt.pf ppf "section %s is re-entered with different attributes" n
    | `Section_stack_empty -> Fmt.string ppf "there is no section to return to"
    | `Directive_not_accepted name -> Fmt.pf ppf "the target did not accept %s" name
    | `Relax_ladder e -> Lowered_ast.pp_relax_error ppf e
    | `Invalid_configuration m -> Fmt.pf ppf "invalid feature configuration: %s" m
    | `Beyond_ceiling fs ->
        let shown, more =
          if List.length fs <= 8 then (fs, "")
          else
            (List.filteri (fun i _ -> i < 8) fs, Printf.sprintf " and %d more" (List.length fs - 8))
        in
        Fmt.pf ppf "enables %s%s, beyond the maximum feature configuration"
          (String.concat ", " shown) more
    | `Unknown_segment s -> Fmt.pf ppf "the image has no segment %s" s

  let error_code : error -> string = function
    | `Statement_before_section -> "lower.no-section"
    | `Data_value _ | `Data_fixup _ -> "lower.data"
    | `Directive_not_accepted _ -> "lower.directive"
    | `Section_attributes_changed _ -> "lower.section-attributes"
    | `Section_stack_empty -> "lower.section-stack"
    | `Relax_ladder _ -> "lower.relax-ladder"
    | `Invalid_configuration _ -> "config.invalid"
    | `Beyond_ceiling _ -> "config.ceiling"
    | `Unknown_segment _ -> "listing.segment"

  let diag ?origin (e : error) =
    Diagnostic.of_error ~code:error_code ~pp:pp_error
      ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:T.name ())
      e

  (* Where a target's failure becomes part of the pipeline's report. This is the
     erasure boundary {!Target_encode.ENCODE} names: [Make] is generic over [T]
     and cannot hold a [T.error], so the target renders its own domain and a
     diagnostic is what crosses (docs/errors.md §2). *)
  let target_diagnostic ?within e =
    let d = T.error_diagnostic (Err.Error.kind e) in
    match within with Some o -> Diagnostic.within o d | None -> d

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

  (* A ceiling is a maximum configuration: no state may enable a feature it leaves out, so a
     directive that would ([.option arch, +d]) is refused where it is applied, not only at the
     end of the module. [beyond] lists what [state] enables past it. *)
  let beyond ~ceiling state =
    match ceiling with
    | None -> []
    | Some c ->
        List.filter
          (fun f -> not (Target_config.enabled c f))
          (Target_config.features (T.state_config state))

  let ceiling_of = function
    | None -> Ok None
    | Some spec -> (
        match configure spec with
        | Ok c -> Ok (Some c)
        | Error m -> Diag.fail ~pos:__POS__ [ diag (`Invalid_configuration m) ])

  (* The initial state and the ceiling a request names: with a ceiling and no [features], the
     ceiling is also where assembly starts. *)
  let start ?features ?max_features () =
    let open Err.Syntax in
    let* ceiling = ceiling_of max_features in
    let* state =
      match (features, ceiling) with
      | None, Some c -> Ok (T.initial_state c)
      | _ -> initial_state features
    in
    match beyond ~ceiling state with
    | [] -> Ok (state, ceiling)
    | fs -> Diag.fail ~pos:__POS__ [ diag (`Beyond_ceiling fs) ]

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

  let lower_with ?ceiling
      ~(target_state :
         name:string -> arguments:string list -> T.target_state -> T.target_state state_directive)
      ~state (m : T.Instruction.t Normalized_ast.module_) =
    let errors = ref [] in
    (* The lists are reversed while building; the tables answer by-name lookups. *)
    let sections : section_build list ref = ref [] in
    let section_table : (string, section_build) Hashtbl.t = Hashtbl.create 16 in
    let declared = ref [] in
    let symbols : symbol_build list ref = ref [] in
    let symbol_table : (string, symbol_build) Hashtbl.t = Hashtbl.create 256 in
    let commons : (Lowered_ast.common * Origin.t) list ref = ref [] in
    let made_local : (string, unit) Hashtbl.t = Hashtbl.create 16 in
    (* [state] is retained in the public entry point for direct normalized-AST
       producers, and parsed programs pass the initial state of the caller's configuration.  Target
       directives in [m] then replay in source order below. *)
    (match beyond ~ceiling state with
    | [] -> ()
    | fs -> errors := diag (`Beyond_ceiling fs) :: !errors);
    let state = ref state in
    let current = ref None in
    let ensure_section name perms kind =
      match Hashtbl.find_opt section_table name with
      | Some s ->
          current := Some s;
          s
      | None ->
          let s =
            { sb_name = name; sb_perms = perms; sb_kind = kind; sb_align = 1; sb_frags = [] }
          in
          sections := s :: !sections;
          Hashtbl.add section_table name s;
          current := Some s;
          s
    in
    let previous = ref None in
    let stack = ref [] in
    let switch_section ~origin = function
      | Directive.Section { name; perms; nobits } ->
          let kind = if nobits then Lowered_ast.Nobits else Lowered_ast.Progbits in
          (match Hashtbl.find_opt section_table name with
          | Some s when s.sb_perms <> perms || s.sb_kind <> kind ->
              errors := diag ~origin (`Section_attributes_changed name) :: !errors
          | _ -> ());
          previous := !current;
          ignore (ensure_section name perms kind)
      | Directive.Declared_section { name } ->
          declared := name :: !declared;
          (* Deliberately does *not* become the current section: it is
             never allocated, so a fragment landing in it would have
             nowhere to go. Anything emitted after it is an error rather
             than silently dropped. *)
          previous := !current;
          current := None
      | _ -> ()
    in
    let with_section origin f =
      match !current with
      | Some s -> f s
      | None -> errors := diag ~origin `Statement_before_section :: !errors
    in
    let symbol name =
      match Hashtbl.find_opt symbol_table name with
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
          symbols := s :: !symbols;
          Hashtbl.add symbol_table name s;
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
            | Directive.Section _ | Directive.Declared_section _ -> switch_section ~origin directive
            | Directive.Push_section d ->
                stack := !current :: !stack;
                switch_section ~origin d
            | Directive.Pop_section -> (
                match !stack with
                | [] -> errors := diag ~origin `Section_stack_empty :: !errors
                | s :: rest ->
                    stack := rest;
                    previous := !current;
                    current := s)
            | Directive.Previous_section -> (
                match !previous with
                | None -> errors := diag ~origin `Section_stack_empty :: !errors
                | p ->
                    let c = !current in
                    current := p;
                    previous := c)
            | Directive.Visibility { name; visibility } -> (symbol name).sy_visibility <- visibility
            | Directive.Align { boundary; fill; max_skip } ->
                with_section origin (fun s ->
                    s.sb_align <- max s.sb_align boundary;
                    s.sb_frags <-
                      Lowered_ast.Align { boundary; fill; max_skip; origin } :: s.sb_frags)
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
                              | Some _
                                when width < 8
                                     && not
                                          (Bigint.fits_signed_exn ~width:(8 * width) v
                                          || Bigint.fits_unsigned_exn ~width:(8 * width) v) ->
                                  Error
                                    (Printf.sprintf "value %s does not fit in %d byte%s"
                                       (Bigint.to_string v) width
                                       (if width = 1 then "" else "s"))
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
            | Directive.Ascii { bytes } ->
                if bytes <> "" then
                  with_section origin (fun s ->
                      s.sb_frags <-
                        Lowered_ast.Bytes { bytes; form = None; fixups = []; origin } :: s.sb_frags)
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
                Hashtbl.replace made_local name ();
                sy.sy_binding <- apply_binding_directive sy.sy_binding `Local
            | Directive.Common { name; size; align } ->
                commons :=
                  ({ Lowered_ast.comm_name = name; comm_size = size; comm_align = align }, origin)
                  :: !commons
            | Directive.Sym_type { name; kind } -> (symbol name).sy_kind <- kind
            | Directive.Sym_size { name; size } ->
                (symbol name).sy_size <- Some size;
                with_section origin (fun s ->
                    s.sb_frags <- Lowered_ast.Set_size { name; size; origin } :: s.sb_frags)
            | Directive.Cfi _ -> ()
            | Directive.Target_state { name; arguments } -> (
                match target_state ~name ~arguments !state with
                | Handled s -> (
                    match beyond ~ceiling s with
                    | [] -> state := s
                    | fs -> errors := diag ~origin (`Beyond_ceiling fs) :: !errors)
                | Rejected d -> errors := d :: !errors
                | Unhandled -> errors := diag ~origin (`Directive_not_accepted name) :: !errors))
        | Normalized_ast.Instruction { insn; origin } ->
            with_section origin (fun s ->
                match T.lower_instruction !state insn with
                | Error e -> errors := target_diagnostic ~within:origin e :: !errors
                | Ok lowered ->
                    List.iter
                      (fun l ->
                        let qualify (a : _ Lowered_ast.encoded_form) =
                          { a with Lowered_ast.form = T.name ^ "." ^ a.Lowered_ast.form }
                        in
                        match T.encode_in !state l with
                        | Error e -> errors := target_diagnostic ~within:origin e :: !errors
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
          Hashtbl.mem made_local c.Lowered_ast.comm_name
          &&
          match Hashtbl.find_opt symbol_table c.Lowered_ast.comm_name with
          | Some { sy_binding = Lowered_ast.Local; _ } -> true
          | _ -> false)
        (List.rev !commons)
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
                 { boundary = c.Lowered_ast.comm_align; fill = None; max_skip = None; origin }
            :: bss.sb_frags;
          (symbol name).sy_section <- ".bss")
        local_commons
    end;
    if !errors <> [] then Diag.fail ~pos:__POS__ (List.rev !errors)
    else
      Ok
        {
          Lowered_ast.unit_name = m.Normalized_ast.unit_name;
          target = T.name;
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
              (List.rev !sections);
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
              (List.rev !symbols);
          commons = List.map fst global_commons;
          declared_sections = List.rev !declared;
        }

  let lower ?ceiling ~state m =
    lower_with ?ceiling ~target_state:(fun ~name:_ ~arguments:_ _ -> Unhandled) ~state m

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
  let policy_for_many ?entry ?(externs = []) ?exported ?max_size ?(allow_rwx = false)
      ?(group_sections = true) (modules : T.fixup_kind Lowered_ast.module_ list) =
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
    { Image.entry_symbol; externs; exported; max_size; allow_rwx; group_sections }

  let policy_for ?entry ?externs ?exported ?max_size ?allow_rwx ?group_sections
      (m : T.fixup_kind Lowered_ast.module_) =
    policy_for_many ?entry ?externs ?exported ?max_size ?allow_rwx ?group_sections [ m ]

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

  (* The assembler's [.align] padding: the target's no-op sized for the gap in an executable
     section, zero elsewhere. [None] where the target has no padding of that length (A32 and A64
     have no one-byte no-op), which layout reports. *)
  let align_fill ~executable n =
    if not executable then Some (String.make n '\000')
    else match T.nop_bytes ~length:n with Ok b -> Some b | Error _ -> None

  let plan ?entry ?externs ?exported ?max_size ?allow_rwx ?group_sections m =
    Image.plan_image ~evaluate ~fill:merge_fill ~align_fill ?section_pad
      (policy_for ?entry ?externs ?exported ?max_size ?allow_rwx ?group_sections m)
      [ m ]

  let plan_many ?entry ?externs ?exported ?max_size ?allow_rwx ?group_sections
      (modules : T.fixup_kind Lowered_ast.module_ list) =
    Image.plan_image ~evaluate ~fill:merge_fill ~align_fill ?section_pad
      (policy_for_many ?entry ?externs ?exported ?max_size ?allow_rwx ?group_sections modules)
      modules

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
  let fill_cache : (int, string option * string option) Hashtbl.t = Hashtbl.create 64

  let fills n =
    match Hashtbl.find_opt fill_cache n with
    | Some f -> f
    | None ->
        let f =
          ( (match T.nop_bytes ~length:n with Ok s -> Some s | Error _ -> None),
            Option.map (fun f -> f ~length:n) T.merge_fill )
        in
        Hashtbl.add fill_cache n f;
        f

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
    let same_at n candidate =
      let rec go i =
        i >= n || (String.unsafe_get bytes (pos + i) = String.unsafe_get candidate i && go (i + 1))
      in
      String.length candidate = n && go 0
    in
    let fill_matches n =
      let nop, merge = fills n in
      (match nop with Some s -> same_at n s | None -> false)
      || match merge with Some s -> same_at n s | None -> false
    in
    let matches k =
      let b = Int64.of_int (1 lsl k) in
      let stop = Int64.mul (Int64.div (Int64.add here (Int64.sub b 1L)) b) b in
      let n = Int64.to_int (Int64.sub stop here) in
      if n > 0 && pos + n <= len && fill_matches n then Some (1 lsl k, n) else None
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

  let disassemble_records ?features ?(inspect = false) ~address bytes =
    let open Err.Syntax in
    let* state = initial_state features in
    (* Strict decoding refuses a disabled component's instruction. Inspection first decodes
       strictly; a word the configuration refuses is read under every component enabled, and each
       reading is judged against the caller's configuration. *)
    let discovery_state =
      if inspect then T.initial_state (Target_config.all_enabled T.components) else state
    in
    let unmet insn =
      match T.required insn with
      | None -> None
      | Some req ->
          Option.map
            (fun u -> Fmt.str "requires feature %a, which is not enabled" Target_component.Req.pp u)
            (Target_component.Req.unmet (Target_config.enabled (T.state_config state)) req)
    in
    let shown insn = Fmt.to_to_string T.Instruction.pp insn in
    let readings here pos strict_error =
      match T.decode_candidates { T.state = discovery_state; address = here } bytes ~pos with
      | [] -> Diag.fail ~pos:__POS__ [ target_diagnostic strict_error ]
      | (insn, form, n) :: others ->
          let alternative (i, _, _) =
            shown i ^ match unmet i with Some m -> " (" ^ m ^ ")" | None -> ""
          in
          Ok (insn, form, n, unmet insn, List.map alternative others)
    in
    let record ~at ~pos ~n ~text ~form ~refusal ~alternatives : Target_encode.disasm_record =
      { address = at; bytes = String.sub bytes pos n; text; form; refusal; alternatives }
    in
    let rec go pos acc =
      if pos >= String.length bytes then Ok (List.rev acc)
      else
        let here = Int64.add address (Int64.of_int pos) in
        match padding_at ~address bytes ~pos with
        | Some (boundary, n) ->
            go (pos + n)
              (record ~at:here ~pos ~n
                 ~text:(Printf.sprintf ".balign %d" boundary)
                 ~form:None ~refusal:None ~alternatives:[]
              :: acc)
        | None -> (
            let decoded =
              match T.decode { T.state; address = here } bytes ~pos with
              | Ok (insn, form, n) -> Ok (insn, form, n, None, [])
              | Error e ->
                  if inspect then readings here pos e
                  else Diag.fail ~pos:__POS__ [ target_diagnostic e ]
            in
            match decoded with
            | Error _ as e -> e
            | Ok (insn, form, n, refusal, alternatives) ->
                go (pos + n)
                  (record ~at:here ~pos ~n ~text:(shown insn)
                     ~form:(Some (T.name ^ "." ^ form))
                     ~refusal ~alternatives
                  :: acc))
    in
    go 0 []

  let dump_disasm_diagnostic ?features ?inspect ~address bytes =
    let open Err.Syntax in
    let* records = disassemble_records ?features ?inspect ~address bytes in
    let run (r : Target_encode.disasm_record) =
      String.concat " "
        (List.init (String.length r.bytes) (fun i -> Printf.sprintf "%02x" (Char.code r.bytes.[i])))
    in
    let text (r : Target_encode.disasm_record) =
      r.text
      ^ (match r.refusal with Some m -> "  ; " ^ m | None -> "")
      ^ if r.alternatives = [] then "" else "  ; or " ^ String.concat "; or " r.alternatives
    in
    let rows = List.map (fun r -> (r, run r, text r)) records in
    let wr = List.fold_left (fun a (_, run, _) -> max a (String.length run)) 0 rows in
    let wt = List.fold_left (fun a (_, _, text) -> max a (String.length text)) 0 rows in
    Ok
      (String.concat ""
         (List.map
            (fun ((r : Target_encode.disasm_record), run, text) ->
              Printf.sprintf "%08Lx  %-*s  %-*s  [%s]\n" r.address wr run wt text
                (Option.value r.form ~default:"padding"))
            rows))

  let dump_disasm_canonical ?features ~address bytes =
    let open Err.Syntax in
    let* records = disassemble_records ?features ~address bytes in
    Ok
      (String.concat ""
         (List.map (fun (r : Target_encode.disasm_record) -> "\t" ^ r.text ^ "\n") records))

  (* {2 The symbol-labelled listing}

     A bound image's segment, disassembled, with each symbol written where it is defined and each
     fixup's symbol beside the instruction it patches. A numeric local label is spelled as the GNU
     export spells it. Like the diagnostic dump it is for reading, not for re-parsing. *)

  let listing_records ?features ?inspect (image : Image.t) ~segment =
    let open Err.Syntax in
    match List.find_opt (fun (s : Image.segment) -> String.equal s.name segment) image.segments with
    | None -> Diag.fail ~pos:__POS__ [ diag (`Unknown_segment segment) ]
    | Some s ->
        let* records = disassemble_records ?features ?inspect ~address:s.address s.bytes in
        let symbol = Rivet_asm_core.Gnu_module.symbol in
        let rec go records (symbols : Image.symbol list) (refs : Image.reference list) acc =
          match records with
          | [] -> List.rev acc
          | (r : Target_encode.disasm_record) :: rest ->
              let stop = Int64.add r.address (Int64.of_int (String.length r.bytes)) in
              let before a = Int64.unsigned_compare a stop < 0 in
              let here, symbols =
                List.partition (fun (y : Image.symbol) -> before y.sym_address) symbols
              in
              let mine, refs =
                List.partition (fun (f : Image.reference) -> before f.ref_site) refs
              in
              let line : Target_encode.listing_line =
                {
                  record = r;
                  labels = List.map (fun (y : Image.symbol) -> symbol y.sym_name) here;
                  targets =
                    List.sort_uniq compare
                      (List.map
                         (fun (f : Image.reference) -> (symbol f.ref_symbol, f.ref_addend))
                         mine);
                }
              in
              go rest symbols refs (line :: acc)
        in
        let inside a =
          Int64.unsigned_compare a s.address >= 0
          && Int64.unsigned_compare a (Int64.add s.address (Int64.of_int (String.length s.bytes)))
             < 0
        in
        Ok
          (go records
             (List.filter (fun (y : Image.symbol) -> inside y.sym_address) image.symbols)
             (List.filter (fun (f : Image.reference) -> inside f.ref_site) image.references)
             [])

  let dump_listing ?features ?inspect image ~segment =
    let open Err.Syntax in
    let* lines = listing_records ?features ?inspect image ~segment in
    let run (r : Target_encode.disasm_record) =
      String.concat " "
        (List.init (String.length r.bytes) (fun i -> Printf.sprintf "%02x" (Char.code r.bytes.[i])))
    in
    let target (name, addend) =
      if Int64.equal addend 0L then name
      else if Int64.compare addend 0L > 0 then Printf.sprintf "%s+0x%Lx" name addend
      else Printf.sprintf "%s-0x%Lx" name (Int64.neg addend)
    in
    let wr =
      List.fold_left
        (fun a (l : Target_encode.listing_line) -> max a (String.length (run l.record)))
        0 lines
    in
    let wt =
      List.fold_left
        (fun a (l : Target_encode.listing_line) -> max a (String.length l.record.text))
        0 lines
    in
    let buf = Buffer.create 256 in
    List.iter
      (fun (l : Target_encode.listing_line) ->
        List.iter (fun n -> Buffer.add_string buf (n ^ ":\n")) l.labels;
        let refs =
          match l.targets with
          | [] -> ""
          | ts -> "  <" ^ String.concat ", " (List.map target ts) ^ ">"
        in
        Buffer.add_string buf
          (Printf.sprintf "  %08Lx  %-*s  %-*s%s\n" l.record.address wr (run l.record)
             (if refs = "" then 0 else wt)
             l.record.text refs))
      lines;
    Ok (Buffer.contents buf)

  let fixup_observations = Image.fixup_observations
end
