module SMap = Map.Make (String)

type status = Added | Removed | Unchanged | Changed

type entry = {
  record_id : string;
  status : status;
  old_fingerprint : string option;
  new_fingerprint : string option;
}

type report = { entries : entry list; added : int; removed : int; unchanged : int; changed : int }

let fingerprint line = String.sub Digestif.SHA256.(to_hex (digest_string line)) 0 12

let entry_of ~record_id old_line new_line =
  match (old_line, new_line) with
  | None, None -> assert false
  | Some o, None ->
      {
        record_id;
        status = Removed;
        old_fingerprint = Some (fingerprint o);
        new_fingerprint = None;
      }
  | None, Some n ->
      { record_id; status = Added; old_fingerprint = None; new_fingerprint = Some (fingerprint n) }
  | Some o, Some n ->
      let status = if String.equal o n then Unchanged else Changed in
      {
        record_id;
        status;
        old_fingerprint = Some (fingerprint o);
        new_fingerprint = Some (fingerprint n);
      }

let diff ~old_ ~new_ =
  (* Later bindings win on a duplicate key, matching List.assoc-style
     shadowing - {!load} is what actually rejects duplicates. *)
  let map_of pairs = List.fold_left (fun m (id, line) -> SMap.add id line m) SMap.empty pairs in
  let old_map = map_of old_ and new_map = map_of new_ in
  let ids =
    SMap.fold (fun id _ acc -> id :: acc) old_map (SMap.fold (fun id _ acc -> id :: acc) new_map [])
    |> List.sort_uniq String.compare
  in
  let entries =
    List.map
      (fun id -> entry_of ~record_id:id (SMap.find_opt id old_map) (SMap.find_opt id new_map))
      ids
  in
  let count st = List.length (List.filter (fun e -> e.status = st) entries) in
  {
    entries;
    added = count Added;
    removed = count Removed;
    unchanged = count Unchanged;
    changed = count Changed;
  }

let fail ?path detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path Tool_error.Parse detail)

(* Same trailing-newline handling as Isa_db_jsonl: export/writer.py always
   ends the file with a newline after its last line, so splitting on '\n'
   leaves one trailing empty element to drop, not a record to decode. *)
let lines_of_text text =
  match String.split_on_char '\n' text with
  | parts -> ( match List.rev parts with "" :: rest -> List.rev rest | _ -> parts)

let load path =
  let ( let* ) = Result.bind in
  let* text = Tool_fs.read path in
  let seen = Hashtbl.create 4096 in
  let rec go acc lineno = function
    | [] -> Ok (List.rev acc)
    | line :: rest -> (
        match Isa_source_record.of_line line with
        | Error msg -> fail ~path (Printf.sprintf "line %d: %s" lineno msg)
        | Ok (r : Isa_source_record.t) ->
            if Hashtbl.mem seen r.record_id then
              fail ~path (Printf.sprintf "line %d: duplicate record_id %s" lineno r.record_id)
            else (
              Hashtbl.add seen r.record_id ();
              go ((r.record_id, line) :: acc) (lineno + 1) rest))
  in
  go [] 1 (lines_of_text text)

let diff_files old_path new_path =
  let ( let* ) = Result.bind in
  let* old_ = load old_path in
  let* new_ = load new_path in
  Ok (diff ~old_ ~new_)

let string_of_status = function
  | Added -> "added"
  | Removed -> "removed"
  | Unchanged -> "unchanged"
  | Changed -> "changed"

let report_lines ~label (r : report) =
  let header =
    Printf.sprintf "isa-snapshot-diff: %s: %d ids (%d added, %d removed, %d unchanged, %d changed)"
      label (List.length r.entries) r.added r.removed r.unchanged r.changed
  in
  let fp = function None -> "-" | Some f -> f in
  let detail_lines =
    r.entries
    |> List.filter (fun (e : entry) -> e.status <> Unchanged)
    |> List.map (fun (e : entry) ->
        Printf.sprintf "  %s: %s (%s -> %s)" (string_of_status e.status) e.record_id
          (fp e.old_fingerprint) (fp e.new_fingerprint))
  in
  header :: detail_lines
