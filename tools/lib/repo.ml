type t = Fpath.t

(* All three are required. Makefile alone matches half the trees on a machine,
   and dune-project alone would match any dune project - including the nested
   tools/ one, which is not the repository root. *)
let sentinels = [ "Makefile"; "scripts/target-matrix.sh"; "dune-project" ]

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
      match env "RIVET_ROOT" with
      | Some s when s <> "" ->
          let* p = canonical (Fpath.v s) in
          validate p
      | Some _ | None ->
          let* cwd = canonical cwd in
          search_upward cwd)

let path t = t

(* The C sources and per-case expected status, and the generated assembly and
   oracle artifacts for them. The compiler and its major version name the
   second: a different compiler's output is a different corpus. *)
let fixture_sources t = Fpath.(t / "fixtures" / "c")
let fixture_compiler_dir = "gcc-14"

let fixture_corpus t =
  { Corpus.sources = fixture_sources t; outputs = Fpath.(t / "fixtures" / fixture_compiler_dir) }

let gas_xref_corpus t = Fpath.(t / "fixtures" / "gas-xref")
let isa_generated_corpus t = Fpath.(t / "fixtures" / Isa_generated_case.fixture_dir_name)

(* Isa_gen_difficult itself depends on Repo (its case/normalize builders take
   a Repo.t), so - unlike Isa_generated_case, a dependency-free schema module
   - it cannot be referenced from here without a module cycle; "isa-difficult"
   is duplicated as a literal and must be kept equal to
   Isa_gen_difficult.fixture_dir_name (checked by test_isa_gen_difficult.ml). *)
let isa_difficult_corpus t = Fpath.(t / "fixtures" / "isa-difficult")
let isa_data_riscv_opcodes t = Fpath.(t / "vendor" / "isa-data" / "riscv-opcodes" / "upstream")
let isa_data_xed_upstream t = Fpath.(t / "vendor" / "isa-data" / "xed" / "upstream")
let isa_data_xed t = Fpath.(isa_data_xed_upstream t / "datafiles")
let isa_inventory t target = Fpath.(t / "fixtures" / "isa-inventory" / Target.to_string target)

let isa_db_export t ~source target =
  Fpath.(t / "isa-db" / "export" / source / (Target.to_string target ^ ".jsonl"))
