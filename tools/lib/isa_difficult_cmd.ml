let ( let* ) = Result.bind

let run_one repo (entry : Isa_gen_difficult.entry) =
  let label = Printf.sprintf "%s/%s" (Target.to_string entry.target) entry.case_id in
  match Isa_gen_difficult.normalize_entry repo entry with
  | Error e -> { Isa_gen_drive.command = Command.of_error e; record = None }
  | Ok (form : Isa_norm_model.form) -> (
      match Isa_gen_difficult.build entry form with
      | Error msg ->
          {
            Isa_gen_drive.record = None;
            command =
              Command.ok
                [ Diagnostic.stdout (Printf.sprintf "isa-difficult: %s: SKIP (%s)" label msg) ];
          }
      | Ok case ->
          Isa_gen_drive.run_case ~prefix:Isa_gen_difficult.cli_group_name ~label repo case
            form.encoding)

let regen repo =
  let results = List.map (run_one repo) Isa_gen_difficult.all in
  let records = List.filter_map (fun (r : Isa_gen_drive.result) -> r.record) results in
  let write_command =
    match
      let* () = Tool_fs.mkdir_p (Repo.isa_difficult_corpus repo) in
      Isa_generated_corpus.write Fpath.(Repo.isa_difficult_corpus repo / "cases.jsonl") records
    with
    | Ok () -> Command.ok []
    | Error e -> Command.of_error e
  in
  Command.accumulate
    (List.map (fun (r : Isa_gen_drive.result) -> r.command) results @ [ write_command ])
    ~f:Fun.id
