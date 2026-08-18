type case = { name : string; root : Fpath.t }

let err ?path op detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path op detail)

let discover root =
  match Sys.readdir (Fpath.to_string root) with
  | exception Sys_error m -> err ~path:root Tool_error.Traverse m
  | entries ->
      let names =
        Array.to_list entries
        |> List.filter (fun e ->
            let s = Fpath.(to_string (root / e / "source")) in
            Sys.file_exists s && Sys.is_directory s)
        |> List.sort String.compare
      in
      if names = [] then
        err ~path:root Tool_error.Traverse
          (Printf.sprintf "no cases under %s" (Fpath.to_string root))
      else Ok (List.map (fun name -> { name; root = Fpath.(root / name) }) names)

let resolve corpus name =
  match Identifier.parse_case_name name with
  | Error e -> Error e
  | Ok id ->
      let name = Identifier.to_string id in
      let root = Fpath.(corpus / name) in
      if Sys.file_exists (Fpath.to_string root) && Sys.is_directory (Fpath.to_string root) then
        Ok { name; root }
      else
        err ~path:root Tool_error.Validate
          (Printf.sprintf "no such case '%s' under %s" name (Fpath.to_string corpus))

(* manifest.txt*, not manifest.txt: --regen writes manifest.txt.new and
   manifest.txt.diff beside the fixtures, and a plain exclusion would record the
   scratch file in the very manifest it is about to become.

   Matched on the BASENAME AT ANY DEPTH, because that is what the shell's
   `find . -type f ! -name 'manifest.txt*'` does - -name tests the basename
   wherever it appears. This is not a detail: the corpus contains nested
   oracle/linked/manifest.txt files, which are a DIFFERENT format written by
   asm-fixture-oracle.sh, and anchoring the exclusion at the case root would
   record every one of them as an unrecorded file. *)
let is_manifest_scratch rel =
  let base = Filename.basename rel in
  String.length base >= 12 && String.sub base 0 12 = "manifest.txt"

let files root = Tool_fs.files ~root ~exclude:is_manifest_scratch

type finding = Missing of string | Changed of string | Unrecorded of string

let pp_finding ppf = function
  | Missing p -> Format.fprintf ppf "MISSING %s" p
  | Changed p -> Format.fprintf ppf "CHANGED %s" p
  | Unrecorded p -> Format.fprintf ppf "UNRECORDED %s" p

let check root manifest =
  let ( let* ) = Result.bind in
  let recorded = Manifest.hashes manifest in
  (* Exact set membership (D1), not a regex match: the shell interpolates the
     path into `grep -q "^sha256:$f\t"`, so a name containing a metacharacter
     matches loosely. Building the set once also makes this linear rather than
     one grep per file over the whole manifest. *)
  let recorded_set = Hashtbl.create 128 in
  List.iter (fun (p, h) -> Hashtbl.replace recorded_set p h) recorded;
  let* present = files root in
  let findings =
    List.filter_map
      (fun (rel, expected) ->
        let p = Fpath.(root // v rel) in
        if not (Sys.file_exists (Fpath.to_string p)) then Some (Missing rel)
        else
          match Tool_fs.sha256 p with
          | Ok got when String.equal got expected -> None
          | Ok _ -> Some (Changed rel)
          (* An unreadable file is CHANGED rather than an abort: the shell's
             hash_of would yield an empty string and fail the comparison the
             same way, and a check that stopped at the first unreadable file
             would hide every finding after it. *)
          | Error _ -> Some (Changed rel))
      recorded
  in
  let unrecorded =
    List.filter_map
      (fun rel -> if Hashtbl.mem recorded_set rel then None else Some (Unrecorded rel))
      present
  in
  Ok (List.length recorded, findings @ unrecorded)

let stem_of_source rel =
  let ( let* ) = Result.bind in
  let* rel = Identifier.relative_path rel in
  let base = Filename.basename rel in
  if Filename.check_suffix base ".c" then Ok (Filename.chop_suffix base ".c")
  else err Tool_error.Validate (Printf.sprintf "source %S does not end in .c" rel)

let previous_and_source case =
  let manifest = Fpath.(case.root / "manifest.txt") in
  if not (Sys.file_exists (Fpath.to_string manifest)) then Ok (None, "source/" ^ case.name ^ ".c")
  else
    let ( let* ) = Result.bind in
    let* text = Tool_fs.read manifest in
    match Manifest.parse text with
    | Error e -> Error (List.hd (Err.Error.kind e))
    | Ok m -> (
        let* s = Manifest.source_rel m in
        match s with Some s -> Ok (Some m, s) | None -> Ok (Some m, "source/" ^ case.name ^ ".c"))

let source_units case =
  let manifest = Fpath.(case.root / "manifest.txt") in
  if not (Sys.file_exists (Fpath.to_string manifest)) then Ok []
  else
    let ( let* ) = Result.bind in
    let* text = Tool_fs.read manifest in
    match Manifest.parse text with
    | Error e -> Error (List.hd (Err.Error.kind e))
    | Ok m -> Manifest.source_units m

(* Top-level only (not recursive): a case's own [source/] directory is where
   its compilation units live, and going deeper would risk picking up a
   helper header or a nested fixture's own tree. *)
let discover_c_files case =
  let dir = Fpath.(case.root / "source") in
  match Sys.readdir (Fpath.to_string dir) with
  | exception Sys_error m -> err ~path:dir Tool_error.Traverse m
  | entries ->
      Array.to_list entries
      |> List.filter (fun e -> Filename.check_suffix e ".c")
      |> List.sort String.compare
      |> fun names -> Ok (List.map (fun n -> "source/" ^ n) names)

let sources case =
  let ( let* ) = Result.bind in
  let* units = source_units case in
  match units with
  | _ :: _ -> Ok units
  | [] -> (
      (* No [source-unit] records: either a legacy single-source manifest
         already pins ONE source, which is the case's whole scope regardless
         of what else happens to sit in [source/] - or there is no manifest
         yet and this is a case being authored for the first time, where the
         directory contents are the only source of truth. *)
      let manifest = Fpath.(case.root / "manifest.txt") in
      if Sys.file_exists (Fpath.to_string manifest) then
        let* _prev, source_rel = previous_and_source case in
        Ok [ (case.name, source_rel) ]
      else
        let* found = discover_c_files case in
        match found with
        | [] ->
            let* _prev, source_rel = previous_and_source case in
            Ok [ (case.name, source_rel) ]
        | [ rel ] -> Ok [ (case.name, rel) ]
        | many ->
            let rec go acc = function
              | [] -> Ok (List.rev acc)
              | rel :: rest ->
                  let* stem = stem_of_source rel in
                  go ((stem, rel) :: acc) rest
            in
            go [] many)

let unit_stems case =
  let ( let* ) = Result.bind in
  let* units = sources case in
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | (unit_name, source_rel) :: rest ->
        let* stem = stem_of_source source_rel in
        go ((unit_name, stem) :: acc) rest
  in
  go [] units
