let fatal ?path op detail =
  Command.of_error
    (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path op detail))

let of_err = function Ok v -> Ok v | Error e -> Error e

(* One case, checked against its own manifest. Findings go to stderr WITHOUT a
   FATAL: prefix, because the shell does not put one there - they are output,
   not errors, and a gate comparing bytes would notice. *)
let check_one (case : Corpus.case) =
  let manifest_path = Fpath.(case.Corpus.root / "manifest.txt") in
  if not (Sys.file_exists (Fpath.to_string manifest_path)) then
    fatal ~path:manifest_path Tool_error.Read_file
      (Printf.sprintf "no manifest at %s - run --regen first" (Fpath.to_string manifest_path))
  else
    match Tool_fs.read manifest_path with
    | Error e -> Command.of_error e
    | Ok text -> (
        match Manifest.parse text with
        | Error e -> Command.of_errors (Err.Error.kind e)
        | Ok m -> (
            match Corpus.check case.Corpus.root m with
            | Error e -> Command.of_error e
            | Ok (seen, findings) ->
                if seen = 0 then fatal Tool_error.Validate "manifest records no files"
                else if findings = [] then
                  Command.ok
                    [
                      Diagnostic.stdout
                        (Printf.sprintf "fixtures: %s: %d files match the manifest" case.Corpus.name
                           seen);
                    ]
                else
                  Command.fail
                    (List.map
                       (fun f -> Diagnostic.stderr (Format.asprintf "%a" Corpus.pp_finding f))
                       findings)))

let fixture_check repo ~cases =
  let corpus = Repo.fixture_corpus repo in
  let names =
    match cases with
    | [] -> (
        match Corpus.discover corpus with
        | Ok cs -> Ok (List.map (fun c -> c.Corpus.name) cs)
        | Error e -> Error e)
    | given -> Ok given
  in
  match of_err names with
  | Error e -> Command.of_error e
  | Ok names ->
      (* Resolve immediately before running each, never all up front: that is
         what makes a valid case's success line precede a later unknown case's
         failure (P4). Validating the whole list first would print the failure
         alone, which is a different transcript even at the same exit code. *)
      Command.accumulate names ~f:(fun name ->
          match Corpus.resolve corpus name with
          | Error e -> Command.of_error e
          | Ok case -> check_one case)

let gas_xref_check repo =
  let corpus = Repo.gas_xref_corpus repo in
  let manifest_path = Fpath.(corpus / "manifest.txt") in
  if not (Sys.file_exists (Fpath.to_string manifest_path)) then
    fatal ~path:manifest_path Tool_error.Read_file
      (Printf.sprintf "no manifest at %s - run --regen first" (Fpath.to_string manifest_path))
  else
    match Tool_fs.read manifest_path with
    | Error e -> Command.of_error e
    | Ok text -> (
        match Manifest.parse text with
        | Error e -> Command.of_errors (Err.Error.kind e)
        | Ok m -> (
            match Corpus.files corpus with
            | Error e -> Command.of_error e
            | Ok present -> (
                if
                  (* present = 0 is an error, not "no unrecorded files". The
                   distinction is the whole reason the shell counts. *)
                  present = []
                then
                  fatal Tool_error.Validate
                    (Printf.sprintf "found no files under %s" (Fpath.to_string corpus))
                else
                  match Corpus.check corpus m with
                  | Error e -> Command.of_error e
                  | Ok (seen, findings) ->
                      if seen = 0 then fatal Tool_error.Validate "manifest records no files"
                      else if findings = [] then
                        Command.ok
                          [
                            Diagnostic.stdout
                              (Printf.sprintf "gas-xref: %d files match the manifest" seen);
                          ]
                      else
                        Command.fail
                          (List.map
                             (fun f -> Diagnostic.stderr (Format.asprintf "%a" Corpus.pp_finding f))
                             findings))))

(* Replays committed cases offline without producer/toolchain dependencies,
   rejecting unexplained missing cases or tools in required tiers. Every step
   below only reads the checked-in isa-db export and the committed corpus
   file - Isa_gen_pilot.normalize_entry/Isa_gen_case_build.build never invoke
   a tool, and Isa_generated_corpus.replay (which now also recomputes the
   "ours" comparison and verdict, see its own .mli) is a pure recomputation
   over already-committed bytes, never a real `as` or `tool/asm.exe`
   invocation. *)
let isa_generated_check repo =
  let corpus_path = Fpath.(Repo.isa_generated_corpus repo / "cases.jsonl") in
  if not (Sys.file_exists (Fpath.to_string corpus_path)) then
    fatal ~path:corpus_path Tool_error.Read_file
      (Printf.sprintf "no corpus at %s - run --regen first" (Fpath.to_string corpus_path))
  else
    match Isa_generated_corpus.load corpus_path with
    | Error e -> Command.of_error e
    | Ok records ->
        let expected_ids = ref [] in
        let per_entry =
          List.map
            (fun (entry : Isa_gen_pilot.pilot_entry) ->
              let label = Printf.sprintf "%s/%s" (Target.to_string entry.target) entry.form_id in
              match Isa_gen_pilot.normalize_entry repo entry with
              | Error e -> Command.of_error e
              | Ok (form : Isa_norm_model.form) -> (
                  match Isa_gen_case_build.build entry form with
                  | Error msg ->
                      fatal Tool_error.Validate
                        (Printf.sprintf
                           "isa-generated check: %s: no canonical case can be built (%s) - every \
                            pilot manifest entry must have a committed case"
                           label msg)
                  | Ok case -> (
                      expected_ids := case.Isa_generated_case.case_id :: !expected_ids;
                      match
                        List.find_opt
                          (fun (r : Isa_generated_corpus.record) ->
                            String.equal r.case.Isa_generated_case.case_id
                              case.Isa_generated_case.case_id)
                          records
                      with
                      | None ->
                          fatal Tool_error.Validate
                            (Printf.sprintf
                               "isa-generated check: %s: no committed case %s in %s - run --regen"
                               label case.Isa_generated_case.case_id (Fpath.to_string corpus_path))
                      | Some r -> (
                          if not (Stdlib.( = ) r.Isa_generated_corpus.case case) then
                            fatal Tool_error.Validate
                              (Printf.sprintf
                                 "isa-generated check: %s: the committed case does not match what \
                                  Isa_gen_pilot/Isa_gen_case_build produce today - run --regen"
                                 label)
                          else if
                            String.length r.Isa_generated_corpus.gas.Isa_generated_case.tool_label
                            = 0
                          then
                            fatal Tool_error.Validate
                              (Printf.sprintf
                                 "isa-generated check: %s: committed artifact has no tool_label - \
                                  run --regen"
                                 label)
                          else
                            match Isa_generated_corpus.replay r form.encoding with
                            | Error msg ->
                                fatal Tool_error.Validate
                                  (Printf.sprintf "isa-generated check: %s: %s" label msg)
                            | Ok () ->
                                Command.ok
                                  [
                                    Diagnostic.stdout
                                      (Printf.sprintf "isa-generated: %s: %s ours=%s" label
                                         (Isa_generated_corpus.finding_description
                                            r.Isa_generated_corpus.finding)
                                         (Isa_generated_case.verdict_description
                                            r.Isa_generated_corpus.verdict));
                                  ]))))
            Isa_gen_pilot.all
        in
        let extras =
          List.filter_map
            (fun (r : Isa_generated_corpus.record) ->
              if List.mem r.case.Isa_generated_case.case_id !expected_ids then None
              else
                Some
                  (fatal Tool_error.Validate
                     (Printf.sprintf
                        "isa-generated check: committed case %s is not produced by any pilot \
                         manifest entry - stale corpus, run --regen"
                        r.case.Isa_generated_case.case_id)))
            records
        in
        Command.accumulate (per_entry @ extras) ~f:Fun.id

(* Isa_gen_difficult: the same toolchain-free replay discipline as
   isa_generated_check above, generalized over
   Isa_gen_difficult.all/normalize_entry/build instead of
   Isa_gen_pilot.all/normalize_entry/Isa_gen_case_build.build, and reading/
   writing its own corpus file so growing this manifest never touches the
   frozen isa-generated corpus. *)
let isa_difficult_check repo =
  let corpus_path = Fpath.(Repo.isa_difficult_corpus repo / "cases.jsonl") in
  if not (Sys.file_exists (Fpath.to_string corpus_path)) then
    fatal ~path:corpus_path Tool_error.Read_file
      (Printf.sprintf "no corpus at %s - run --regen first" (Fpath.to_string corpus_path))
  else
    match Isa_generated_corpus.load corpus_path with
    | Error e -> Command.of_error e
    | Ok records ->
        let expected_ids = ref [] in
        let per_entry =
          List.map
            (fun (entry : Isa_gen_difficult.entry) ->
              let label = Printf.sprintf "%s/%s" (Target.to_string entry.target) entry.case_id in
              match Isa_gen_difficult.normalize_entry repo entry with
              | Error e -> Command.of_error e
              | Ok (form : Isa_norm_model.form) -> (
                  match Isa_gen_difficult.build entry form with
                  | Error msg ->
                      fatal Tool_error.Validate
                        (Printf.sprintf
                           "isa-difficult check: %s: no case can be built (%s) - every difficult \
                            manifest entry must have a committed case"
                           label msg)
                  | Ok case -> (
                      expected_ids := case.Isa_generated_case.case_id :: !expected_ids;
                      match
                        List.find_opt
                          (fun (r : Isa_generated_corpus.record) ->
                            String.equal r.case.Isa_generated_case.case_id
                              case.Isa_generated_case.case_id)
                          records
                      with
                      | None ->
                          fatal Tool_error.Validate
                            (Printf.sprintf
                               "isa-difficult check: %s: no committed case %s in %s - run --regen"
                               label case.Isa_generated_case.case_id (Fpath.to_string corpus_path))
                      | Some r -> (
                          if not (Stdlib.( = ) r.Isa_generated_corpus.case case) then
                            fatal Tool_error.Validate
                              (Printf.sprintf
                                 "isa-difficult check: %s: the committed case does not match what \
                                  Isa_gen_difficult produces today - run --regen"
                                 label)
                          else if
                            String.length r.Isa_generated_corpus.gas.Isa_generated_case.tool_label
                            = 0
                          then
                            fatal Tool_error.Validate
                              (Printf.sprintf
                                 "isa-difficult check: %s: committed artifact has no tool_label - \
                                  run --regen"
                                 label)
                          else
                            match Isa_generated_corpus.replay r form.encoding with
                            | Error msg ->
                                fatal Tool_error.Validate
                                  (Printf.sprintf "isa-difficult check: %s: %s" label msg)
                            | Ok () ->
                                Command.ok
                                  [
                                    Diagnostic.stdout
                                      (Printf.sprintf "isa-difficult: %s: %s ours=%s" label
                                         (Isa_generated_corpus.finding_description
                                            r.Isa_generated_corpus.finding)
                                         (Isa_generated_case.verdict_description
                                            r.Isa_generated_corpus.verdict));
                                  ]))))
            Isa_gen_difficult.all
        in
        let extras =
          List.filter_map
            (fun (r : Isa_generated_corpus.record) ->
              if List.mem r.case.Isa_generated_case.case_id !expected_ids then None
              else
                Some
                  (fatal Tool_error.Validate
                     (Printf.sprintf
                        "isa-difficult check: committed case %s is not produced by any difficult \
                         manifest entry - stale corpus, run --regen"
                        r.case.Isa_generated_case.case_id)))
            records
        in
        Command.accumulate (per_entry @ extras) ~f:Fun.id

let targets capability =
  Command.ok (List.map (fun t -> Diagnostic.stdout (Target.to_string t)) (Target.set capability))
