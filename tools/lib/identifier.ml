type t = string

let to_string t = t
let pp = Format.pp_print_string

let reject ?path detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path Tool_error.Validate detail)

let has_forbidden_char s = String.exists (function '\t' | '\n' | '\000' -> true | _ -> false) s

let check_component ~what s =
  if s = "" then reject (Printf.sprintf "empty %s" what)
  else if s = "." || s = ".." then reject (Printf.sprintf "%s %S is a directory reference" what s)
  else if String.contains s '/' then
    reject (Printf.sprintf "%s %S contains a path separator" what s)
  else if has_forbidden_char s then
    reject (Printf.sprintf "%s %S contains a tab, newline or NUL" what s)
  else if s.[0] = '-' then reject (Printf.sprintf "%s %S begins with '-'" what s)
  else Ok s

let parse s = check_component ~what:"name" s
let is_case_char = function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true | _ -> false

let parse_case_name s =
  match check_component ~what:"case name" s with
  | Error _ as e -> e
  | Ok s ->
      if String.for_all is_case_char s then Ok s
      else reject (Printf.sprintf "case name %S is not [A-Za-z0-9_-]+" s)

let relative_path s =
  if s = "" then reject "empty relative path"
  else if has_forbidden_char s then
    reject (Printf.sprintf "path %S contains a tab, newline or NUL" s)
  else if String.length s > 0 && s.[0] = '/' then
    reject (Printf.sprintf "path %S is absolute; manifest paths are relative to the case root" s)
  else
    (* Every component is checked, not just the first: "source/../../x" is the
       escape that matters and its first component is innocuous. *)
    let components = String.split_on_char '/' s in
    let rec go = function
      | [] -> Ok s
      | c :: rest -> (
          match check_component ~what:"path component" c with Ok _ -> go rest | Error _ as e -> e)
    in
    go components
