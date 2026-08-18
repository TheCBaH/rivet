type outcome = Up_to_date | Changed of string

let ( let* ) = Result.bind
let record key value = { Manifest.key; value }

let source_records sources =
  match sources with
  (* Exactly one unit keeps the legacy [source] key, byte-identical to every
     already-committed single-source manifest - the [source-unit:] key family
     is what a genuinely multi-source case gets instead, never a length-1
     rewrite of a case that never needed it. *)
  | [ (_, source_rel) ] -> [ record Manifest.Source (Some source_rel) ]
  | many -> List.map (fun (name, rel) -> record (Manifest.Source_unit name) (Some rel)) many

let records ~case ~targets ~work_root ~previous ~sources =
  let per_target t =
    let c = Target.config t in
    let* version_records =
      match Ccomp.installed t ~work_root with
      | Some compiler ->
          let* v = Ccomp.version compiler in
          Ok [ record (Manifest.Ccomp_version t) (Some v) ]
      | None -> (
          (* Carried forward verbatim: --rehash runs with no compiler, and
             dropping the record would silently lose the provenance the manifest
             exists to carry. *)
          match previous with
          | None -> Ok []
          | Some prev -> (
              match Manifest.find prev (Manifest.Ccomp_version t) with
              | None -> Ok []
              | Some v -> Ok [ record (Manifest.Ccomp_version t) v ]))
    in
    (* A BARE KEY when the list is empty - `printf 'ccomp-args:%s\n' "$t"` emits
       no tab, and `ccomp-args:arm` is a different manifest from
       `ccomp-args:arm<TAB>`. *)
    let args_value = function [] -> None | l -> Some (String.concat " " l) in
    Ok
      (version_records
      @ [
          record (Manifest.Ccomp_target t) (Some c.Target.configure_target);
          record (Manifest.Ccomp_args t) (args_value c.Target.ccomp_args);
          record (Manifest.Ccomp_configure_args t) (args_value c.Target.compcert_configure_args);
        ])
  in
  let rec targets_records acc = function
    | [] -> Ok (List.rev acc)
    | t :: rest ->
        let* rs = per_target t in
        targets_records (List.rev_append rs acc) rest
  in
  let* target_records = targets_records [] targets in
  let* files = Corpus.files case.Corpus.root in
  let rec hash_records acc = function
    | [] -> Ok (List.rev acc)
    | f :: rest ->
        (* CHECKED, unlike the shell's `printf ... "$(hash_of ...)"`, where
           printf's success masked the substitution's failure and the record was
           written with an empty hash. *)
        let* h = Tool_fs.sha256 Fpath.(case.Corpus.root // v f) in
        hash_records (record (Manifest.Sha256 f) (Some h) :: acc) rest
  in
  let* hashes = hash_records [] files in
  Ok
    ((record Manifest.Generator (Some "compcert-tools fixture regen") :: source_records sources)
    @ target_records @ hashes)

let run_diff ~old_path ~new_path =
  (* Statuses [0;1] only: 0 is identical, 1 is differs, and anything else means
     diff itself failed. The shell's `! diff` is true for 1 AND 2, so a broken
     diff there installs the new manifest and reports a changed scope (D9). *)
  Tool_process.exec
    (Tool_process.spec ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_capture
       ~accepted:(Process_status.Statuses [ 0; 1 ])
       ~label:"diff" "diff"
       [ "-u"; Fpath.to_string old_path; Fpath.to_string new_path ])

let write ~final ~previous records =
  let text = Manifest.serialize (Manifest.of_records records) in
  Tool_fs.with_staging ~final ~install_suffix:".new" ~auxiliary_suffixes:[ ".diff" ]
    (fun ~install ~auxiliary:_ ~write_install ~write_auxiliary:_ ~commit ->
      let* () = write_install text in
      match previous with
      (* P6: on a first generation the shell's `[ -f "$MANIFEST" ] && ! diff ...`
         short-circuits, so Changed is unreachable and the run reports "manifest
         up to date" with exit 0. *)
      | None ->
          let* () = commit () in
          Ok Up_to_date
      | Some _ ->
          let* r = run_diff ~old_path:final ~new_path:(Tool_fs.install_path install) in
          let body = Option.value ~default:"" r.Tool_process.stdout in
          let* () = commit () in
          if r.Tool_process.status = Process_status.Exited 0 then Ok Up_to_date
          else Ok (Changed body))
