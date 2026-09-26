let ( let* ) = Result.bind

let run_one ~previous repo (entry : Isa_gen_difficult.entry) =
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
          Isa_gen_drive.run_case ?previous:(previous case.case_id)
            ~prefix:Isa_gen_difficult.cli_group_name ~label repo case form.encoding)

let run_negative ~previous repo (entry : Isa_gen_negative.entry) =
  let case = Isa_gen_negative.case_of entry in
  Isa_gen_drive.run_negative ?previous:(previous case.case_id)
    ~prefix:Isa_gen_difficult.cli_group_name
    ~label:(Printf.sprintf "%s/%s" (Target.to_string entry.target) case.case_id)
    repo case Isa_gen_negative.dummy_encoding

type job = Positive of Isa_gen_difficult.entry | Negative of Isa_gen_negative.entry

let run_job ~previous repo = function
  | Positive entry -> run_one ~previous repo entry
  | Negative entry -> run_negative ~previous repo entry

(* Incremental by default: the committed corpus supplies previous GAS
   artifacts. [COMPCERT_TOOLS_REGEN=full] ignores it and re-runs every tool. *)
let previous_records repo =
  let path = Fpath.(Repo.isa_difficult_corpus repo / "cases.jsonl") in
  let full = Sys.getenv_opt "COMPCERT_TOOLS_REGEN" = Some "full" in
  let tbl = Hashtbl.create 4096 in
  (if (not full) && Sys.file_exists (Fpath.to_string path) then
     match Isa_generated_corpus.load path with
     | Ok records ->
         List.iter
           (fun (r : Isa_generated_corpus.record) -> Hashtbl.replace tbl r.case.case_id r)
           records
     | Error _ -> ());
  Hashtbl.find_opt tbl

(* Cases are independent, so they are sharded over forked workers
   ({!Tool_parallel}); each still runs its own GAS and assembler processes,
   so every artifact is exactly what a sequential run records and a failing
   case is reported under its own id. Report lines keep the manifest order. *)
let regen repo =
  let jobs =
    List.map (fun e -> Positive e) Isa_gen_difficult.all
    @ List.map (fun e -> Negative e) Isa_gen_negative.all
  in
  let previous = previous_records repo in
  match Tool_parallel.map ~jobs:(Tool_parallel.default_jobs ()) (run_job ~previous repo) jobs with
  | Error e -> Command.of_error e
  | Ok results ->
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
