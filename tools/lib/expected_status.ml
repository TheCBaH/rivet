let err case detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
    (Tool_error.v Tool_error.Parse (Printf.sprintf "%s: %s" case detail))

let parse ~case text =
  match String.index_opt text '\n' with
  | None -> err case "expected-status.txt must end with a newline"
  | Some i -> (
      let first = String.sub text 0 i in
      (* The round-trip IS the validation: any byte the canonical form would
         not produce - a second line, a stray \r, trailing spaces, a leading
         zero - makes this comparison fail. *)
      if first ^ "\n" <> text then
        err case "expected-status.txt must contain exactly one canonical line"
      else
        let canonical =
          first <> ""
          && String.for_all (fun c -> c >= '0' && c <= '9') first
          && (first = "0" || first.[0] <> '0')
        in
        if not canonical then
          err case
            (Printf.sprintf "expected-status.txt must be a canonical decimal integer, got '%s'"
               first)
        else
          match int_of_string_opt first with
          | Some n when n <= 123 -> Ok n
          | Some n ->
              err case
                (Printf.sprintf
                   "expected status %d is outside 0-123 (124+ is reserved for the host runner)" n)
          | None -> err case (Printf.sprintf "expected status '%s' is not representable" first))

let read ~case path =
  if not (Sys.file_exists (Fpath.to_string path)) then
    Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
      (Tool_error.v ~path Tool_error.Read_file
         (Printf.sprintf "%s: missing expected-status.txt" case))
  else Result.bind (Tool_fs.read path) (parse ~case)
