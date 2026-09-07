type t = Fpath.t

(* All three are required. Makefile alone matches half the trees on a machine,
   and asm/dune-project alone would happily accept a checkout of asm/ lifted out
   of its repository - which is exactly the %{workspace_root} confusion this
   module exists to prevent. *)
let sentinels = [ "Makefile"; "tools/target-matrix.sh"; "asm/dune-project" ]

let fail detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v Tool_error.Validate detail)

let missing_sentinel dir =
  List.find_opt (fun s -> not (Sys.file_exists Fpath.(to_string (dir // v s)))) sentinels

let validate dir =
  match missing_sentinel dir with
  | None -> Ok dir
  | Some s ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v ~path:dir Tool_error.Validate
           (Printf.sprintf "%s is not a repository root: %s is missing" (Fpath.to_string dir) s))

(* Canonicalize before validating, so a root reached through a symlink is the
   same root as the one reached directly. *)
let canonical dir =
  match Unix.realpath (Fpath.to_string dir) with
  | exception Unix.Unix_error (e, _, _) ->
      fail (Printf.sprintf "cannot resolve %s: %s" (Fpath.to_string dir) (Unix.error_message e))
  | s -> Ok (Fpath.v s)

let rec search_upward dir =
  match missing_sentinel dir with
  | None -> Ok dir
  | Some _ ->
      let parent = Fpath.parent dir in
      if Fpath.equal parent dir then
        fail
          ("no repository root above the current directory: expected one containing "
         ^ String.concat ", " sentinels)
      else search_upward (Fpath.normalize parent)

let resolve ~cli ~env ~cwd =
  let ( let* ) = Result.bind in
  match cli with
  | Some p ->
      let* p = canonical p in
      validate p
  | None -> (
      match env "COMPCERT_REPO_ROOT" with
      | Some s when s <> "" ->
          let* p = canonical (Fpath.v s) in
          validate p
      | Some _ | None ->
          let* cwd = canonical cwd in
          search_upward cwd)

let path t = t
let fixture_corpus t = Fpath.(t / "asm" / "fixtures" / "compcert-3.17")
let gas_xref_corpus t = Fpath.(t / "asm" / "fixtures" / "gas-xref")
let corpus_work t = Fpath.(t / ".corpus-work")
let corpus_c t target = Fpath.(t / "asm" / "fixtures" / "corpus" / "c" / Target.to_string target)

let corpus_c_assemble t target =
  Fpath.(t / "asm" / "fixtures" / "corpus" / "c-assemble" / Target.to_string target)

let corpus_regression t target =
  Fpath.(t / "asm" / "fixtures" / "corpus" / "regression" / Target.to_string target)

let corpus_compression t target =
  Fpath.(t / "asm" / "fixtures" / "corpus" / "compression" / Target.to_string target)

let corpus_c_gcc t target =
  Fpath.(t / "asm" / "fixtures" / "corpus" / "c-gcc" / Target.to_string target)

let isa_data_riscv_opcodes t =
  Fpath.(t / "asm" / "vendor" / "isa-data" / "riscv-opcodes" / "upstream")

let isa_data_xed_upstream t = Fpath.(t / "asm" / "vendor" / "isa-data" / "xed" / "upstream")
let isa_data_xed t = Fpath.(isa_data_xed_upstream t / "datafiles")

let isa_inventory t target =
  Fpath.(t / "asm" / "fixtures" / "isa-inventory" / Target.to_string target)

let isa_db_export t ~source target =
  Fpath.(t / "isa-db" / "export" / source / (Target.to_string target ^ ".jsonl"))
