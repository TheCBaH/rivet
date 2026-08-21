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

(* M4 (.ai/asm_plan.md §12): a case's [origin:] records, or [[]] for a case
   with no manifest yet or no such records - the same shape and fallback as
   {!source_units}. *)
let origins case =
  let manifest = Fpath.(case.root / "manifest.txt") in
  if not (Sys.file_exists (Fpath.to_string manifest)) then Ok []
  else
    let ( let* ) = Result.bind in
    let* text = Tool_fs.read manifest in
    match Manifest.parse text with
    | Error e -> Error (List.hd (Err.Error.kind e))
    | Ok m -> Manifest.origins m

(* M4: [Supported_targets], decoded and defaulted to all six - the single
   source of truth every target/case traversal consults before any compiler
   requirement or target-directory filesystem access. *)
let supported_targets case =
  let manifest = Fpath.(case.root / "manifest.txt") in
  if not (Sys.file_exists (Fpath.to_string manifest)) then Ok Target.all
  else
    let ( let* ) = Result.bind in
    let* text = Tool_fs.read manifest in
    match Manifest.parse text with
    | Error e -> Error (List.hd (Err.Error.kind e))
    | Ok m -> (
        let* ts = Manifest.supported_targets m in
        match ts with Some ts -> Ok ts | None -> Ok Target.all)

let supports_target case target =
  let ( let* ) = Result.bind in
  let* ts = supported_targets case in
  Ok (List.mem target ts)

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

type unit_source =
  | Compiled of { unit : string; c_path : string }
  | Preexisting of { unit : string; origin : string }

let unit_name = function Compiled { unit; _ } | Preexisting { unit; _ } -> unit

(* Exactly the pre-M4 [sources] logic, restricted to the [Compiled] units a
   case has - a [source-unit]/legacy-[source]/first-authoring case ladder
   independent of whatever [origin:] records the case also carries. *)
let compiled_units case =
  let ( let* ) = Result.bind in
  let* su = source_units case in
  match su with
  | _ :: _ -> Ok (List.map (fun (unit, c_path) -> Compiled { unit; c_path }) su)
  | [] -> (
      (* No [source-unit] records: either a legacy single-source manifest
         already pins ONE source, which is the case's whole scope regardless
         of what else happens to sit in [source/] - or there is no manifest
         yet and this is a case being authored for the first time, where the
         directory contents are the only source of truth. *)
      let manifest = Fpath.(case.root / "manifest.txt") in
      if Sys.file_exists (Fpath.to_string manifest) then
        let* _prev, source_rel = previous_and_source case in
        Ok [ Compiled { unit = case.name; c_path = source_rel } ]
      else
        let* found = discover_c_files case in
        match found with
        | [] ->
            let* _prev, source_rel = previous_and_source case in
            Ok [ Compiled { unit = case.name; c_path = source_rel } ]
        | [ rel ] -> Ok [ Compiled { unit = case.name; c_path = rel } ]
        | many ->
            let rec go acc = function
              | [] -> Ok (List.rev acc)
              | rel :: rest ->
                  let* stem = stem_of_source rel in
                  go (Compiled { unit = stem; c_path = rel } :: acc) rest
            in
            go [] many)

let preexisting_units case =
  let ( let* ) = Result.bind in
  let* og = origins case in
  Ok (List.map (fun (unit, origin) -> Preexisting { unit; origin }) og)

(* M4 (.ai/asm_plan.md §12): every compilation unit a case has, [Compiled]
   (from a project [.c] file) or [Preexisting] (an upstream [.S] source,
   e.g. a CompCert runtime helper, preprocessed rather than compiled) -
   merged and sorted by unit name, the SAME comparison rule
   [asm/test/oracle/exec.ml]'s [units_of] and
   [asm/test/differential/test_differential.ml]'s [unit_paths] already apply
   to committed [.s] filenames, so once a unit is materialized as an
   ordinary [<target>/<stem>.s] file every consumer agrees on one order by
   construction. Rejects two units (of either kind, including the implicit
   case-name unit a legacy single-[source] case has) sharing one name -
   [Manifest.parse] cannot check this itself, since it has no case name to
   compare an [Origin] stem against. *)
let sources case =
  let ( let* ) = Result.bind in
  let* compiled = compiled_units case in
  let* preexisting = preexisting_units case in
  let merged = compiled @ preexisting in
  let has_dup =
    let seen = Hashtbl.create 8 in
    List.exists
      (fun u ->
        let n = unit_name u in
        if Hashtbl.mem seen n then true
        else (
          Hashtbl.add seen n ();
          false))
      merged
  in
  if has_dup then
    err Tool_error.Validate
      (Printf.sprintf
         "case %S has two units sharing one name (an origin unit colliding with a compiled unit, \
          or with the case's own implicit unit name)"
         case.name)
  else Ok (List.sort (fun a b -> String.compare (unit_name a) (unit_name b)) merged)

let unit_stems case =
  let ( let* ) = Result.bind in
  let* units = sources case in
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | Compiled { unit; c_path } :: rest ->
        let* stem = stem_of_source c_path in
        go ((unit, stem) :: acc) rest
    (* A [Preexisting] unit's declared stem IS its name - [origin:<stem>]
       names the unit by the committed [<target>/<stem>.s] it will be
       preprocessed into, unlike a [Compiled] unit's name, which need not
       equal its [.c] file's stem. *)
    | Preexisting { unit; origin = _ } :: rest -> go ((unit, unit) :: acc) rest
  in
  go [] units
