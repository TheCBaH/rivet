let ( let* ) = Result.bind

let submodule_commit dir =
  match
    Tool_process.exec
      (Tool_process.spec ~cwd:dir ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_capture
         ~accepted:Process_status.Zero_only ~label:"isa-inventory git rev-parse" "git"
         [ "rev-parse"; "HEAD" ])
  with
  | Ok { Tool_process.stdout; _ } -> Ok (String.trim (Option.value stdout ~default:""))
  | Error e -> Error e

(* One vendored source (riscv-opcodes or xed), applied to its own target
   list - the two are otherwise unrelated modules (see Isa_inventory_riscv
   and Isa_inventory_xed's own headers for why they are not unified), so
   this record is only the small write/regen plumbing they share, not their
   parsing. *)
type 'entry source = {
  name : string;
  submodule_dir : Repo.t -> Fpath.t;
  targets : Target.t list;
  entries_for_target : Repo.t -> Target.t -> ('entry list, Tool_error.t) Err.t;
  render_manifest : source_commit:string -> Target.t -> 'entry list -> string;
  render_summary : Target.t -> 'entry list -> string;
}

let riscv_source =
  {
    name = "riscv-opcodes";
    submodule_dir = Repo.isa_data_riscv_opcodes;
    targets = [ Target.Riscv32; Target.Riscv64 ];
    entries_for_target = Isa_inventory_riscv.entries_for_target;
    render_manifest = Isa_inventory_riscv.render_manifest;
    render_summary = Isa_inventory_riscv.render_summary;
  }

let xed_source =
  {
    name = "xed";
    submodule_dir = Repo.isa_data_xed_upstream;
    targets = [ Target.X86_32; Target.X86_64 ];
    entries_for_target = Isa_inventory_xed.entries_for_target;
    render_manifest = Isa_inventory_xed.render_manifest;
    render_summary = Isa_inventory_xed.render_summary;
  }

let err op detail = Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v op detail)

let write_target repo src ~source_commit target =
  let* entries = src.entries_for_target repo target in
  if entries = [] then
    err Tool_error.Validate
      (Printf.sprintf "%s produced no entries for %s" src.name (Target.to_string target))
  else
    let dest = Repo.isa_inventory repo target in
    let* () = Tool_fs.mkdir_p dest in
    let* () =
      Tool_fs.write
        Fpath.(dest / "manifest.txt")
        (src.render_manifest ~source_commit target entries)
    in
    let* () = Tool_fs.write Fpath.(dest / "summary.txt") (src.render_summary target entries) in
    Ok (List.length entries)

let regen_source repo src =
  let* source_commit = submodule_commit (src.submodule_dir repo) in
  let* reversed =
    List.fold_left
      (fun acc target ->
        let* lines = acc in
        let* n = write_target repo src ~source_commit target in
        Ok ((Target.to_string target, n) :: lines))
      (Ok []) src.targets
  in
  Ok (List.rev reversed)

let regen repo =
  let step =
    let* riscv_counts = regen_source repo riscv_source in
    let* xed_counts = regen_source repo xed_source in
    Ok (riscv_counts @ xed_counts)
  in
  match step with
  | Error e -> Command.of_error e
  | Ok counts ->
      let summary =
        counts |> List.map (fun (t, n) -> Printf.sprintf "%s: %d entries" t n) |> String.concat ", "
      in
      Command.ok [ Diagnostic.stdout (Printf.sprintf "isa-inventory: %s" summary) ]
