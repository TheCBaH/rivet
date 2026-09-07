module SMap = Map.Make (String)

type mutable_tally = {
  mutable normalized : int;
  mutable by_rule : int SMap.t;
  mutable by_name : int SMap.t;
}

type summary = {
  total : int;
  normalized : int;
  by_rule : (string * int) list;
  by_name : (string * int) list;
}

let empty_tally () = { normalized = 0; by_rule = SMap.empty; by_name = SMap.empty }
let bump map key = SMap.update key (function None -> Some 1 | Some n -> Some (n + 1)) map

let record_outcome (tally : mutable_tally) ~name = function
  | Ok (_ : Isa_norm_model.form) -> tally.normalized <- tally.normalized + 1
  | Error (d : Isa_norm_model.diagnostic) ->
      tally.by_rule <- bump tally.by_rule d.rule;
      tally.by_name <- bump tally.by_name name

let normalize_one source (rec_ : Isa_source_record.t) =
  match source with
  | "riscv_opcodes" -> Isa_norm_riscv.normalize rec_
  | "xed_resolved" -> Isa_norm_xed.normalize rec_
  | other -> Error { Isa_norm_model.rule = "unhandled-source"; message = other }

(* Sorted by descending count, then name, so the report is deterministic and
   the biggest actionable gaps (the most-common unhandled native_name/iform)
   sort to the top. *)
let sorted_desc map =
  SMap.bindings map |> List.sort (fun (n1, c1) (n2, c2) -> compare (c2, n1) (c1, n2))

let summarize repo ~source target =
  let path = Repo.isa_db_export repo ~source target in
  match Isa_source_record.read_file path with
  | Error _ as e -> e
  | Ok records ->
      let tally = empty_tally () in
      List.iter
        (fun (rec_ : Isa_source_record.t) ->
          record_outcome tally ~name:rec_.native_name (normalize_one source rec_))
        records;
      Ok
        {
          total = List.length records;
          normalized = tally.normalized;
          by_rule = sorted_desc tally.by_rule;
          by_name = sorted_desc tally.by_name;
        }

let report_lines ~label (s : summary) =
  let header =
    Printf.sprintf "isa-norm-accounting: %s: %d records, %d normalized, %d diagnosed" label s.total
      s.normalized (s.total - s.normalized)
  in
  let rule_lines =
    List.map (fun (rule, count) -> Printf.sprintf "  by rule: %-24s %d" rule count) s.by_rule
  in
  let name_lines =
    s.by_name
    |> List.filteri (fun i _ -> i < 10)
    |> List.map (fun (name, count) -> Printf.sprintf "  top unhandled name: %-16s %d" name count)
  in
  (header :: rule_lines) @ name_lines

let targets_and_sources =
  [
    ("riscv_opcodes", Target.Riscv32);
    ("riscv_opcodes", Target.Riscv64);
    ("xed_resolved", Target.X86_32);
    ("xed_resolved", Target.X86_64);
  ]

let run repo =
  Command.accumulate targets_and_sources ~f:(fun (source, target) ->
      match summarize repo ~source target with
      | Error e -> Command.of_error e
      | Ok summary ->
          let label = Printf.sprintf "%s/%s" source (Target.to_string target) in
          Command.ok (List.map Diagnostic.stdout (report_lines ~label summary)))
