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

let verify_case repo ~target case =
  match work_root repo with
  | Error e -> Command.of_error e
  | Ok work -> (
      let compiler = Ccomp.path target ~work_root:work in
      if Ccomp.installed target ~work_root:work = None then
        fatal Tool_error.Spawn
          (Printf.sprintf "%s: no installed ccomp at %s" case.Corpus.name (Fpath.to_string compiler))
      else
        let c = Target.config target in
        let outcome =
          Tool_workspace.with_scratch ~label:"verify" (fun scratch ->
              let* _prev, source_rel = previous_and_source case in
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
                 command line in a banner, so an absolute path here would put a
                 temporary directory name into the compared bytes. *)
              let* () =
                Ccomp.compile_s ~compiler ~cwd:scratch ~args:c.Target.ccomp_args ~out_rel
                  ~source_rel ~case:case.Corpus.name ~target
              in
              let regenerated = Fpath.(scratch // v out_rel) in
              let committed = Fpath.(case.Corpus.root // v out_rel) in
              let* same = Tool_fs.same_bytes regenerated committed in
              if same then Ok None
              else
                (* Statuses [0;1] (D10): the shell's `diff -u ... || true`
                   ignores EVERY non-zero status, so a broken diff there is
                   silently treated as "no differences to show". *)
                let* r =
                  Tool_process.exec
                    (Tool_process.spec ~stdout:Tool_process.Out_capture
                       ~stderr:Tool_process.Err_capture
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
                  (Printf.sprintf "fixtures: %s/%s regenerates byte-identically" case.Corpus.name
                     (Target.to_string target));
              ]
        | Ok (Some body) ->
            (* The diff on STDOUT, then the fatal on stderr, and the caller
               continues to the next case. *)
            {
              Command.events =
                [
                  Command.Output (Diagnostic.stdout body);
                  Command.Fatal
                    (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp
                       (Tool_error.v Tool_error.Validate
                          (Printf.sprintf "fixture regeneration differs for %s/%s" case.Corpus.name
                             (Target.to_string target))));
                ];
              exit = `Failure;
            })

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

let write_for case ~targets ~work_root ~previous ~source_rel =
  let* records = Fixture_manifest.records ~case ~targets ~work_root ~previous ~source_rel in
  Fixture_manifest.write ~final:Fpath.(case.Corpus.root / "manifest.txt") ~previous records

let regen_case repo ~targets case =
  match work_root repo with
  | Error e -> Command.of_error e
  | Ok work -> (
      let step =
        let* previous, source_rel = previous_and_source case in
        let source = Fpath.(case.Corpus.root // v source_rel) in
        if not (Sys.file_exists (Fpath.to_string source)) then
          Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
            (Tool_error.v Tool_error.Read_file
               (Printf.sprintf "missing fixture source %s" (Fpath.to_string source)))
        else
          let* stem = Corpus.stem_of_source source_rel in
          (* Each invocation is CHECKED, and the loop stops at the first
             failure: the shell ran all six and then reported a manifest over a
             partial tree. *)
          let rec compile_all = function
            | [] -> Ok ()
            | t :: rest ->
                let c = Target.config t in
                let compiler = Ccomp.path t ~work_root:work in
                let out_rel = Target.to_string t ^ "/" ^ stem ^ ".s" in
                let* () = Tool_fs.mkdir_p Fpath.(case.Corpus.root / Target.to_string t) in
                let* () =
                  Ccomp.compile_s ~compiler ~cwd:case.Corpus.root ~args:c.Target.ccomp_args ~out_rel
                    ~source_rel ~case:case.Corpus.name ~target:t
                in
                compile_all rest
          in
          let* () = compile_all targets in
          Ok (previous, source_rel, stem)
      in
      match step with
      | Error e -> Command.of_error e
      | Ok (previous, source_rel, stem) -> (
          (* The gate runs BEFORE the manifest is written, and its status is
             honored. The Phase 0A defect was that this status was discarded, so
             a rejected fixture had its manifest installed and the case reported
             success. *)
          let gate =
            List.concat_map
              (fun t ->
                let p = Fpath.(case.Corpus.root / Target.to_string t / (stem ^ ".s")) in
                match Tool_fs.read p with
                | Error _ ->
                    [
                      Fixture_gate.message ~case:case.Corpus.name ~target:t
                        Fixture_gate.Missing_entry_label;
                    ]
                | Ok asm ->
                    List.map
                      (Fixture_gate.message ~case:case.Corpus.name ~target:t)
                      (Fixture_gate.check ~case:case.Corpus.name ~asm))
              targets
          in
          if gate <> [] then Command.fail (List.map Diagnostic.stderr gate)
          else
            match write_for case ~targets ~work_root:work ~previous ~source_rel with
            | Error e -> Command.of_error e
            | Ok outcome -> report_write case outcome))

let regen repo ~cases =
  let targets = Target.set Target.Fixture in
  match work_root repo with
  | Error e -> Command.of_error e
  | Ok work -> (
      (* Up front, before any case runs: a partial toolchain would otherwise
         produce a manifest whose provenance silently omits targets. *)
      match Ccomp.require_all targets ~work_root:work with
      | Error e -> Command.of_error e
      | Ok () -> over_cases repo ~cases ~f:(regen_case repo ~targets))

let rehash_case repo case =
  match work_root repo with
  | Error e -> Command.of_error e
  | Ok work -> (
      let step =
        let* previous, source_rel = previous_and_source case in
        write_for case ~targets:(Target.set Target.Fixture) ~work_root:work ~previous ~source_rel
      in
      match step with Error e -> Command.of_error e | Ok outcome -> report_write case outcome)

let rehash repo ~cases = over_cases repo ~cases ~f:(rehash_case repo)
