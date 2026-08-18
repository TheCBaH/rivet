type read_root = Fpath.t
type child_owned = Fpath.t
type recreatable_root = Fpath.t

let read_path p = p
let recreatable_path p = p
let child_path p = p

let fail ?path detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path Tool_error.Validate detail)

let fixture_corpus repo = Repo.fixture_corpus repo
let gas_xref_corpus repo = Repo.gas_xref_corpus repo

(* Walk the prefixes that EXIST and reject a symlinked one. The root is
   recreated, and rm -rf through a symlink deletes something the caller did not
   name - so this is a safety property, not a canonicalization convenience.
   Prefixes that do not exist yet are fine: they cannot be symlinks. *)
let rec no_symlinked_component p =
  (* rem_empty_seg is load-bearing, not tidying. Fpath.parent yields a path with
     a TRAILING SLASH, and lstat("/a/link/") follows the link - POSIX makes a
     trailing slash force directory resolution - so the symlink would be
     reported as the directory it points at and never rejected. *)
  let p = Fpath.rem_empty_seg p in
  if Fpath.is_root p then Ok ()
  else
    match Unix.lstat (Fpath.to_string p) with
    | { Unix.st_kind = Unix.S_LNK; _ } ->
        fail ~path:p (Printf.sprintf "work root %s has a symlinked component" (Fpath.to_string p))
    | _ -> no_symlinked_component (Fpath.parent p)
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> no_symlinked_component (Fpath.parent p)
    | exception Unix.Unix_error (e, _, _) ->
        fail ~path:p
          (Printf.sprintf "cannot inspect %s: %s" (Fpath.to_string p) (Unix.error_message e))

let forbidden repo p =
  let s = Fpath.to_string (Fpath.normalize p) in
  let eq q = String.equal s (Fpath.to_string (Fpath.normalize q)) in
  if s = "/" then Some "the filesystem root"
  else if eq (Repo.path repo) then Some "the repository root"
  else if eq (Repo.fixture_corpus repo) then Some "the fixture corpus"
  else if eq (Repo.gas_xref_corpus repo) then Some "the gas-xref corpus"
  else
    match Sys.getenv_opt "HOME" with
    | Some h when h <> "" && eq (Fpath.v h) -> Some "the home directory"
    | _ -> None

(* P5: an external root is allowed. D11: it must be absolute, free of "..", and
   free of symlinked components - none of which the shell checks today. *)
let validate_work_root repo p =
  let ( let* ) = Result.bind in
  if not (Fpath.is_abs p) then
    fail ~path:p (Printf.sprintf "work root %s must be absolute" (Fpath.to_string p))
  else if List.exists (fun seg -> seg = "..") (Fpath.segs p) then
    fail ~path:p (Printf.sprintf "work root %s contains '..'" (Fpath.to_string p))
  else
    match forbidden repo p with
    | Some what -> fail ~path:p (Printf.sprintf "work root %s is %s" (Fpath.to_string p) what)
    | None ->
        let* () = no_symlinked_component (Fpath.normalize p) in
        Ok (Fpath.normalize p)

let from_env repo ~env ~var ~default =
  let p =
    match env var with Some s when s <> "" -> Fpath.v s | _ -> Fpath.(Repo.path repo / default)
  in
  validate_work_root repo p

let fixture_work repo ~env = from_env repo ~env ~var:"FIXTURE_WORK" ~default:".fixture-work"

let exec_artifacts repo ~env =
  from_env repo ~env ~var:"FIXTURE_EXEC_ARTIFACTS" ~default:".fixture-exec-artifacts"

(* ASM_TOOL_GATE_WORK, not TOOL_GATE_WORK: the tool gate is the one script whose
   work-root variable carries the ASM_ prefix. Phase 1 declared this constructor
   before anything consumed it and got the name wrong, and nothing could notice
   until Phase 6 gave it a caller - a redirect that silently did nothing. *)
let tool_gate_work repo ~env = from_env repo ~env ~var:"ASM_TOOL_GATE_WORK" ~default:".tool-gate"

let child root components =
  let ( let* ) = Result.bind in
  let rec go acc = function
    | [] -> Ok acc
    | c :: rest ->
        let* c = Identifier.parse c in
        go Fpath.(acc / Identifier.to_string c) rest
  in
  let* p = go root components in
  let p = Fpath.normalize p in
  (* Belt and braces: Identifier already rejects "..", but the containment
     property is what actually matters, so it is asserted rather than inferred. *)
  if Fpath.is_prefix (Fpath.to_dir_path root) p then Ok p
  else fail ~path:p (Printf.sprintf "%s escapes its root" (Fpath.to_string p))

let mkdir_p p =
  match Bos.OS.Dir.create ~path:true p with
  | Ok _ -> Ok ()
  | Error (`Msg cause) ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v ~path:p ~cause Tool_error.Write_file
           (Printf.sprintf "cannot create %s" (Fpath.to_string p)))

let rm_rf p =
  match Bos.OS.Dir.delete ~recurse:true p with
  | Ok () -> Ok ()
  | Error (`Msg cause) ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v ~path:p ~cause Tool_error.Write_file
           (Printf.sprintf "cannot remove %s" (Fpath.to_string p)))

let ensure p = mkdir_p p

let recreate p =
  let ( let* ) = Result.bind in
  let* () = if Sys.file_exists (Fpath.to_string p) then rm_rf p else Ok () in
  mkdir_p p

let recreate_child = recreate
let recreate_root = recreate

let with_scratch ~label f =
  (* The pattern must be a literal format string, so the label goes into the
     directory's contents-free name via a fixed prefix rather than by
     interpolation. *)
  ignore label;
  match Bos.OS.Dir.tmp "compcert-tools-%s" with
  | Error (`Msg cause) ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v ~cause Tool_error.Write_file "cannot create a scratch directory")
  | Ok dir ->
      Fun.protect
        ~finally:(fun () -> ignore (Bos.OS.Dir.delete ~recurse:true dir))
        (fun () -> f dir)
