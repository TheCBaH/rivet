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

let targets capability =
  Command.ok (List.map (fun t -> Diagnostic.stdout (Target.to_string t)) (Target.set capability))
