let ( let* ) = Result.bind

let run_one repo (entry : Isa_gen_pilot.pilot_entry) =
  let label = Printf.sprintf "%s/%s" (Target.to_string entry.target) entry.form_id in
  match Isa_gen_pilot.normalize_entry repo entry with
  | Error e -> { Isa_gen_drive.command = Command.of_error e; record = None }
  | Ok (form : Isa_norm_model.form) -> (
      match Isa_gen_case_build.build entry form with
      | Error msg ->
          {
            Isa_gen_drive.record = None;
            command =
              Command.ok
                [ Diagnostic.stdout (Printf.sprintf "isa-generated: %s: SKIP (%s)" label msg) ];
          }
      | Ok case ->
          Isa_gen_drive.run_case ~prefix:Isa_generated_case.cli_group_name ~label repo case
            form.encoding)

(* Every per-case report line streams exactly as before (the report
   vocabulary is unchanged); this additionally persists every
   successfully-built case's record to the checked-in corpus
   (asm/fixtures/isa-generated/cases.jsonl), which `isa-generated check`
   (Check_cmd.isa_generated_check) replays offline. A SKIPped or errored entry
   simply contributes no record - `check` rebuilding that same entry hits the
   same SKIP/error path and reports it directly, rather than this command
   needing to explain an absence. *)
let regen repo =
  let results = List.map (run_one repo) Isa_gen_pilot.all in
  let records = List.filter_map (fun (r : Isa_gen_drive.result) -> r.record) results in
  let write_command =
    match
      let* () = Tool_fs.mkdir_p (Repo.isa_generated_corpus repo) in
      Isa_generated_corpus.write Fpath.(Repo.isa_generated_corpus repo / "cases.jsonl") records
    with
    | Ok () -> Command.ok []
    | Error e -> Command.of_error e
  in
  Command.accumulate
    (List.map (fun (r : Isa_gen_drive.result) -> r.command) results @ [ write_command ])
    ~f:Fun.id
