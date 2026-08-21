let ( let* ) = Result.bind

let fatal ?path op detail =
  Command.of_error
    (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path op detail))

let usage_error detail =
  {
    Command.events =
      [
        Command.Fatal
          (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp
             (Tool_error.v Tool_error.Validate detail));
      ];
    (* Exit 1, NOT 2, and this outlives the launcher that first required it: a
       bad target is a rejected VALUE, not a malformed command line. Validating
       it inside the term rather than through a Cmdliner converter is what keeps
       it 1 - a converter failure is a parse error and exits 2. *)
    exit = `Failure;
  }

let work_root repo =
  Result.map Tool_workspace.read_path (Tool_workspace.fixture_work repo ~env:Sys.getenv_opt)

let previous_and_source = Corpus.previous_and_source

let over_cases repo ~cases ~f =
  let corpus = Repo.fixture_corpus repo in
  let names =
    match cases with
    | [] -> Result.map (List.map (fun c -> c.Corpus.name)) (Corpus.discover corpus)
    | given -> Ok given
  in
  match names with
  | Error e -> Command.of_error e
  | Ok names ->
      Command.accumulate names ~f:(fun name ->
          match Corpus.resolve corpus name with Error e -> Command.of_error e | Ok case -> f case)

(* {1 verify} *)

(* [case.name/unit/target] for a genuinely multi-source case, but the legacy
   single-source case's unit name IS the case name (Corpus.sources: [Ok [
   (case.name, source_rel) ]]), so printing it again would produce a
   confusing "alpha/alpha/x86_64" for the common single-source fixture. *)
let unit_label case unit_name =
  if String.equal unit_name case.Corpus.name then case.Corpus.name
  else case.Corpus.name ^ "/" ^ unit_name

(* M4 (.ai/asm_plan.md §12): a [Preexisting] unit's committed [<target>/
   <stem>.s] is preprocessed, not compiled, from its [origin]'s upstream
   [.S] source - reusing the exact CPP-only invocation
   [Gas_xref_cmd.runtime_defines]/[Gnu_tools.preprocess] already implements
   for this same upstream tree, rather than a second preprocessing path.
   Validates the origin is actually under the target's own runtime
   directory first, so an x86_32 origin can't silently get used while
   processing arm. *)
let preprocess_preexisting repo ~target ~origin ~out =
  let expected_dir = "modules/CompCert/runtime/" ^ Target.to_string target ^ "/" in
  let n = String.length expected_dir in
  if not (String.length origin > n && String.sub origin 0 n = expected_dir) then
    Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
      (Tool_error.v Tool_error.Validate
         (Printf.sprintf "origin %S is not under %s for target %s" origin expected_dir
            (Target.to_string target)))
  else
    let tools = Gnu_tools.for_target target in
    let src = Fpath.(Repo.path repo // v origin) in
    let include_dir = Fpath.(Repo.path repo // v (Filename.dirname origin)) in
    let* ok =
      Gnu_tools.preprocess tools ~src ~out
        ~defines:(Gas_xref_cmd.runtime_defines target)
        ~include_dir
    in
    if ok then Ok ()
    else
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v Tool_error.Spawn
           (Printf.sprintf "preprocessing %s for %s failed" origin (Target.to_string target)))

let verify_unit repo ~target ~compiler ~c case (u : Corpus.unit_source) =
  let unit = Corpus.unit_name u in
  let outcome =
    Tool_workspace.with_scratch ~label:"verify" (fun scratch ->
        let* out_rel =
          match u with
          | Corpus.Compiled { unit = _; c_path = source_rel } ->
              let* stem = Corpus.stem_of_source source_rel in
              let out_rel = Target.to_string target ^ "/" ^ stem ^ ".s" in
              let* () = Tool_fs.mkdir_p Fpath.(scratch // v (Filename.dirname source_rel)) in
              let* () = Tool_fs.mkdir_p Fpath.(scratch / Target.to_string target) in
              let* () =
                Tool_fs.copy
                  ~src:Fpath.(case.Corpus.root // v source_rel)
                  ~dst:Fpath.(scratch // v source_rel)
              in
              (* cwd = scratch and BOTH paths relative: CompCert embeds the
                 command line in a banner, so an absolute path here would put
                 a temporary directory name into the compared bytes. *)
              let* () =
                Ccomp.compile_s ~compiler ~cwd:scratch ~args:c.Target.ccomp_args ~out_rel
                  ~source_rel ~case:case.Corpus.name ~target
              in
              Ok out_rel
          | Corpus.Preexisting { unit = stem; origin } ->
              let out_rel = Target.to_string target ^ "/" ^ stem ^ ".s" in
              let* () = Tool_fs.mkdir_p Fpath.(scratch / Target.to_string target) in
              let* () =
                preprocess_preexisting repo ~target ~origin ~out:Fpath.(scratch // v out_rel)
              in
              Ok out_rel
        in
        let regenerated = Fpath.(scratch // v out_rel) in
        let committed = Fpath.(case.Corpus.root // v out_rel) in
        let* same = Tool_fs.same_bytes regenerated committed in
        if same then Ok None
        else
          (* Statuses [0;1] (D10): the shell's `diff -u ... || true` ignores
             EVERY non-zero status, so a broken diff there is silently treated
             as "no differences to show". *)
          let* r =
            Tool_process.exec
              (Tool_process.spec ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_capture
                 ~accepted:(Process_status.Statuses [ 0; 1 ])
                 ~label:"diff" "diff"
                 [ "-u"; Fpath.to_string committed; Fpath.to_string regenerated ])
          in
          Ok (Some (Option.value ~default:"" r.Tool_process.stdout)))
  in
  match outcome with
  | Error e -> Command.of_error e
  | Ok None ->
      Command.ok
        [
          Diagnostic.stdout
            (Printf.sprintf "fixtures: %s/%s regenerates byte-identically" (unit_label case unit)
               (Target.to_string target));
        ]
  | Ok (Some body) ->
      (* The diff on STDOUT, then the fatal on stderr, and the caller
         continues to the next unit. *)
      {
        Command.events =
          [
            Command.Output (Diagnostic.stdout body);
            Command.Fatal
              (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp
                 (Tool_error.v Tool_error.Validate
                    (Printf.sprintf "fixture regeneration differs for %s/%s" (unit_label case unit)
                       (Target.to_string target))));
          ];
        exit = `Failure;
      }

let verify_case repo ~target case =
  match Corpus.supports_target case target with
  | Error e -> Command.of_error e
  | Ok false ->
      Command.ok
        [
          Diagnostic.stdout
            (Printf.sprintf "fixtures: %s: skipped (does not support %s)" case.Corpus.name
               (Target.to_string target));
        ]
  | Ok true -> (
      match work_root repo with
      | Error e -> Command.of_error e
      | Ok work -> (
          let compiler = Ccomp.path target ~work_root:work in
          if Ccomp.installed target ~work_root:work = None then
            fatal Tool_error.Spawn
              (Printf.sprintf "%s: no installed ccomp at %s" case.Corpus.name
                 (Fpath.to_string compiler))
          else
            let c = Target.config target in
            match Corpus.sources case with
            | Error e -> Command.of_error e
            | Ok units -> Command.accumulate units ~f:(verify_unit repo ~target ~compiler ~c case)))

let verify repo ~target ~cases = over_cases repo ~cases ~f:(verify_case repo ~target)

(* {2 regen and rehash} *)

let report_write case = function
  | Fixture_manifest.Up_to_date ->
      Command.ok
        [ Diagnostic.stdout (Printf.sprintf "fixtures: %s: manifest up to date" case.Corpus.name) ]
  | Fixture_manifest.Changed body ->
      (* Installed AND failed. This is how a scope change is forced through
         review: the new manifest is on disk so the diff can be committed
         deliberately, and the run fails so it cannot pass unnoticed. *)
      Command.fail
        [
          Diagnostic.stderr
            "fixtures: manifest CHANGED - review the differences, they change the tested scope";
          Diagnostic.stderr body;
        ]

let write_for case ~targets ~work_root ~previous ~sources =
  let* records = Fixture_manifest.records ~case ~targets ~work_root ~previous ~sources in
  Fixture_manifest.write ~final:Fpath.(case.Corpus.root / "manifest.txt") ~previous records

(* Every [Compiled] unit's own source must exist before any compiler runs, so
   a missing file in a multi-source case is reported once, up front, rather
   than as a compiler failure on whichever unit happened to be compiled
   first. Pairs each unit with its stem - a [Preexisting] unit's stem is
   always its own name (Corpus.unit_stems's reasoning), so this needs no
   filesystem check for those; a missing/unreadable origin surfaces as a
   preprocessing failure instead, at the point that actually reads it. *)
let stems_of case units =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | (Corpus.Compiled { unit = _; c_path } as u) :: rest ->
        let source = Fpath.(case.Corpus.root // v c_path) in
        if not (Sys.file_exists (Fpath.to_string source)) then
          Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
            (Tool_error.v Tool_error.Read_file
               (Printf.sprintf "missing fixture source %s" (Fpath.to_string source)))
        else
          let* stem = Corpus.stem_of_source c_path in
          go ((u, stem) :: acc) rest
    | (Corpus.Preexisting { unit; origin = _ } as u) :: rest -> go ((u, unit) :: acc) rest
  in
  go [] units

(* M4 (.ai/asm_plan.md §12): per-case, not batch-wide - a partial toolchain
   only blocks the targets THIS case actually declares (via
   [Corpus.supported_targets]), so authoring a single-target-restricted
   fixture like [i64_divmod] needs only that one cross toolchain rather than
   all six up front. Still checked before any compile/preprocess runs, for
   the same "no silently partial provenance" reason the old batch-wide check
   had. *)
let regen_case repo case =
  match work_root repo with
  | Error e -> Command.of_error e
  | Ok work -> (
      let step =
        let* previous, _ = previous_and_source case in
        let* targets = Corpus.supported_targets case in
        let* () = Ccomp.require_all targets ~work_root:work in
        let* units = Corpus.sources case in
        let* stems = stems_of case units in
        (* Each invocation is CHECKED, and the loop stops at the first
           failure: the shell ran all six and then reported a manifest over a
           partial tree. Every unit of one target is compiled before moving to
           the next target, so a partial-tree failure never leaves some
           targets fully written and others not attempted at all. *)
        let rec compile_all = function
          | [] -> Ok ()
          | t :: rest ->
              let c = Target.config t in
              let compiler = Ccomp.path t ~work_root:work in
              let* () = Tool_fs.mkdir_p Fpath.(case.Corpus.root / Target.to_string t) in
              let rec compile_units = function
                | [] -> Ok ()
                | (Corpus.Compiled { unit = _; c_path = source_rel }, stem) :: units_rest ->
                    let out_rel = Target.to_string t ^ "/" ^ stem ^ ".s" in
                    let* () =
                      Ccomp.compile_s ~compiler ~cwd:case.Corpus.root ~args:c.Target.ccomp_args
                        ~out_rel ~source_rel ~case:case.Corpus.name ~target:t
                    in
                    compile_units units_rest
                | (Corpus.Preexisting { unit = _; origin }, stem) :: units_rest ->
                    let out_rel = Target.to_string t ^ "/" ^ stem ^ ".s" in
                    let* () =
                      preprocess_preexisting repo ~target:t ~origin
                        ~out:Fpath.(case.Corpus.root // v out_rel)
                    in
                    compile_units units_rest
              in
              let* () = compile_units stems in
              compile_all rest
        in
        let* () = compile_all targets in
        Ok (previous, units, stems, targets)
      in
      match step with
      | Error e -> Command.of_error e
      | Ok (previous, units, stems, targets) -> (
          (* The gate runs BEFORE the manifest is written, and its status is
             honored. The Phase 0A defect was that this status was discarded, so
             a rejected fixture had its manifest installed and the case reported
             success.

             Comm_or_local/Bss_or_nobits/Tail_converted_call are each one
             unit's own property, checked per unit. Missing_entry_label is a
             whole-CASE property per target instead, checked once over the
             UNION of all of a case's units - a callee-only or data-only unit
             legitimately carries no entry label of its own, and requiring
             every unit to carry one would reject every multi-source case by
             construction.

             M4: a [Preexisting] unit is hand-authored INRIA runtime
             assembly, not ccomp output, so none of this gate's checks apply
             to it - it is excluded from [asms] entirely rather than merely
             exempted from one check, mirroring the case-level carve-out M3
             gave [cross_bss] but at unit granularity. *)
          let gate_for_target t =
            let* asms =
              let rec go acc = function
                | [] -> Ok (List.rev acc)
                | (Corpus.Preexisting _, _) :: rest -> go acc rest
                | (Corpus.Compiled _, stem) :: rest ->
                    let* asm =
                      Tool_fs.read Fpath.(case.Corpus.root / Target.to_string t / (stem ^ ".s"))
                    in
                    go (asm :: acc) rest
              in
              go [] stems
            in
            let per_unit =
              List.concat_map
                (fun asm ->
                  List.map
                    (Fixture_gate.message ~case:case.Corpus.name ~target:t)
                    (Fixture_gate.check ~case:case.Corpus.name ~asm))
                asms
            in
            let entry =
              if List.exists Fixture_gate.has_entry_label asms then []
              else
                [
                  Fixture_gate.message ~case:case.Corpus.name ~target:t
                    Fixture_gate.Missing_entry_label;
                ]
            in
            Ok (per_unit @ entry)
          in
          let rec gate_all acc = function
            | [] -> Ok (List.concat (List.rev acc))
            | t :: rest ->
                let* g = gate_for_target t in
                gate_all (g :: acc) rest
          in
          match gate_all [] targets with
          | Error e -> Command.of_error e
          | Ok gate -> (
              if gate <> [] then Command.fail (List.map Diagnostic.stderr gate)
              else
                match write_for case ~targets ~work_root:work ~previous ~sources:units with
                | Error e -> Command.of_error e
                | Ok outcome -> report_write case outcome)))

let regen repo ~cases = over_cases repo ~cases ~f:(regen_case repo)

(* M4: sourced from the case's OWN declared [supported-targets], not the
   hardcoded six - rehashing a target-restricted fixture must not add
   ccomp-*/sha256 provenance for targets it never claimed, which would
   report a spurious CHANGED against its committed manifest. No compiler is
   required here, matching rehash's existing no-compiler contract
   ([Fixture_manifest.records]'s per-target loop only ever reads a
   compiler when one is installed). *)
let rehash_case repo case =
  match work_root repo with
  | Error e -> Command.of_error e
  | Ok work -> (
      let step =
        let* previous, _ = previous_and_source case in
        let* targets = Corpus.supported_targets case in
        let* units = Corpus.sources case in
        write_for case ~targets ~work_root:work ~previous ~sources:units
      in
      match step with Error e -> Command.of_error e | Ok outcome -> report_write case outcome)

let rehash repo ~cases = over_cases repo ~cases ~f:(rehash_case repo)
