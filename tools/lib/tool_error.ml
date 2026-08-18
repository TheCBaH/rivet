type op = Read_file | Write_file | Traverse | Parse | Hash | Spawn | Exec | Validate | Usage

type t = {
  op : op;
  context : string list;
  path : Fpath.t option;
  detail : string;
  status : Process_status.t option;
  stderr : string option;
  cause : string option;
}

let v ?(context = []) ?path ?status ?stderr ?cause op detail =
  { op; context; path; detail; status; stderr; cause }

let string_of_op = function
  | Read_file -> "read_file"
  | Write_file -> "write_file"
  | Traverse -> "traverse"
  | Parse -> "parse"
  | Hash -> "hash"
  | Spawn -> "spawn"
  | Exec -> "exec"
  | Validate -> "validate"
  | Usage -> "usage"

let pp ppf t =
  Format.fprintf ppf "%s: %s" (string_of_op t.op) t.detail;
  if t.context <> [] then Format.fprintf ppf " (in %s)" (String.concat "/" t.context);
  Option.iter (fun p -> Format.fprintf ppf " [path %a]" Fpath.pp p) t.path;
  Option.iter (fun s -> Format.fprintf ppf " [%a]" Process_status.pp s) t.status;
  Option.iter (fun s -> Format.fprintf ppf " [stderr %S]" s) t.stderr;
  Option.iter (fun c -> Format.fprintf ppf " [cause %s]" c) t.cause

let to_fatal_line t = "FATAL: " ^ t.detail
let exit_code t = match t.op with Usage -> 2 | _ -> 1
