let err ?path op detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?path op detail)

let unix_err ?path op detail e =
  err ?path op (Printf.sprintf "%s: %s" detail (Unix.error_message e))

(* lstat, never stat: `find -type f` does not follow symlinks, so a symlink to a
   regular file is not a file here either. Getting this wrong would silently add
   the link's TARGET bytes to a manifest under the link's name. *)
let files ~root ~exclude =
  let out = ref [] in
  let rec walk rel =
    let dir = if rel = "" then root else Fpath.(root // v rel) in
    match Sys.readdir (Fpath.to_string dir) with
    | exception Sys_error m -> Error m
    | entries ->
        let entries = Array.to_list entries in
        List.sort String.compare entries
        |> List.fold_left
             (fun acc name ->
               match acc with
               | Error _ as e -> e
               | Ok () -> (
                   let child_rel = if rel = "" then name else rel ^ "/" ^ name in
                   let child = Fpath.(root // v child_rel) in
                   match Unix.lstat (Fpath.to_string child) with
                   | exception Unix.Unix_error (e, _, _) ->
                       Error (Printf.sprintf "cannot stat %s: %s" child_rel (Unix.error_message e))
                   | { Unix.st_kind = Unix.S_REG; _ } ->
                       if not (exclude child_rel) then out := child_rel :: !out;
                       Ok ()
                   | { Unix.st_kind = Unix.S_DIR; _ } -> walk child_rel
                   | _ -> Ok ()))
             (Ok ())
  in
  match walk "" with
  | Error m -> err ~path:root Tool_error.Traverse m
  | Ok () -> Ok (List.sort String.compare !out)

(* Bos's own message is kept as [~cause] rather than folded into [detail]: the
   detail is what the compatibility renderer prints, and it has to stay the
   repository's wording. The foreign message survives for the trace. *)
let read path =
  match Bos.OS.File.read path with
  | Ok s -> Ok s
  | Error (`Msg cause) ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v ~path ~cause Tool_error.Read_file
           (Printf.sprintf "cannot read %s" (Fpath.to_string path)))

let write path contents =
  match Bos.OS.File.write path contents with
  | Ok () -> Ok ()
  | Error (`Msg cause) ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v ~path ~cause Tool_error.Write_file
           (Printf.sprintf "cannot write %s" (Fpath.to_string path)))

let mkdir_p path =
  match Bos.OS.Dir.create ~path:true path with
  | Ok _ -> Ok ()
  | Error (`Msg cause) ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v ~path ~cause Tool_error.Write_file
           (Printf.sprintf "cannot create %s" (Fpath.to_string path)))

let copy ~src ~dst =
  let ( let* ) = Result.bind in
  let* contents = read src in
  write dst contents

let same_bytes a b =
  let ( let* ) = Result.bind in
  let* x = read a in
  let* y = read b in
  Ok (String.equal x y)

let sha256 path =
  match read path with Error _ as e -> e | Ok s -> Ok Digestif.SHA256.(to_hex (digest_string s))

type install_stage = Fpath.t
type auxiliary_stage = Fpath.t

let install_path p = p
let auxiliary_path p = p

let validate_suffixes ~install_suffix ~auxiliary_suffixes =
  let all = install_suffix :: auxiliary_suffixes in
  let bad s =
    s = "" || String.contains s '/' || String.contains s '\000'
    || (Filename.dir_sep <> "/" && String.contains s Filename.dir_sep.[0])
  in
  match List.find_opt bad all with
  | Some s -> err Tool_error.Validate (Printf.sprintf "invalid staging suffix %S" s)
  | None -> (
      let sorted = List.sort String.compare all in
      let rec dup = function
        | a :: (b :: _ as rest) -> if a = b then Some a else dup rest
        | _ -> None
      in
      match dup sorted with
      | Some s -> err Tool_error.Validate (Printf.sprintf "duplicate staging suffix %S" s)
      | None -> Ok ())

let write_file path contents =
  match Bos.OS.File.write path contents with
  | Ok () -> Ok ()
  | Error (`Msg m) ->
      err ~path Tool_error.Write_file
        (Printf.sprintf "cannot write %s: %s" (Fpath.to_string path) m)

let remove_if_present path =
  match Unix.unlink (Fpath.to_string path) with
  | () -> ()
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | exception Unix.Unix_error _ -> ()

let with_staging ~final ~install_suffix ~auxiliary_suffixes f =
  match validate_suffixes ~install_suffix ~auxiliary_suffixes with
  | Error _ as e -> e
  | Ok () ->
      let stage_path suffix = Fpath.v (Fpath.to_string final ^ suffix) in
      let install = stage_path install_suffix in
      let auxiliary name =
        if List.mem name auxiliary_suffixes then Ok (stage_path name)
        else err Tool_error.Validate (Printf.sprintf "undeclared auxiliary stage %S" name)
      in
      let write_install contents = write_file install contents in
      let write_auxiliary stage contents = write_file stage contents in
      let commit () =
        (* D13: preserve the existing file's mode. The shell's `mv` installs the
           .new file, whose mode came from the umask, so it effectively RESETS
           mode on every write. Identical for the tracked 0644 manifests, and
           different only for an artificially-moded one. *)
        let mode =
          match Unix.stat (Fpath.to_string final) with
          | { Unix.st_perm; _ } -> st_perm
          | exception Unix.Unix_error _ -> 0o644
        in
        match Unix.rename (Fpath.to_string install) (Fpath.to_string final) with
        | () -> (
            match Unix.chmod (Fpath.to_string final) mode with
            | () -> Ok ()
            | exception Unix.Unix_error (e, _, _) ->
                unix_err ~path:final Tool_error.Write_file "cannot set manifest mode" e)
        | exception Unix.Unix_error (e, _, _) ->
            unix_err ~path:final Tool_error.Write_file
              (Printf.sprintf "cannot install %s" (Fpath.to_string final))
              e
      in
      let cleanup () =
        (* Tolerates the install stage already being absent: on the success path
           commit has renamed it away, and treating that as an error would turn
           every successful write into a failure. *)
        remove_if_present install;
        List.iter (fun s -> remove_if_present (stage_path s)) auxiliary_suffixes
      in
      Fun.protect ~finally:cleanup (fun () ->
          f ~install ~auxiliary ~write_install ~write_auxiliary ~commit)

let remove_matching dir ~suffix =
  let d = Fpath.to_string dir in
  match Sys.readdir d with
  | exception Sys_error _ -> Ok ()
  | entries ->
      let ends s =
        String.length s >= String.length suffix
        && String.sub s (String.length s - String.length suffix) (String.length suffix) = suffix
      in
      Array.sort String.compare entries;
      Array.fold_left
        (fun acc e ->
          match acc with
          | Error _ -> acc
          | Ok () -> (
              if not (ends e) then Ok ()
              else
                try
                  Sys.remove (Filename.concat d e);
                  Ok ()
                with Sys_error m ->
                  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
                    (Tool_error.v ~path:Fpath.(dir / e) Tool_error.Write_file m)))
        (Ok ()) entries

let rec remove_tree path =
  let p = Fpath.to_string path in
  match Unix.lstat p with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
  | exception Unix.Unix_error (e, _, _) ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v ~path Tool_error.Traverse (Unix.error_message e))
  | st ->
      (* S_LNK is unlinked, never descended: descending would let a symlink
         planted in a work root delete outside it. *)
      if st.Unix.st_kind <> Unix.S_DIR then
        try
          Unix.unlink p;
          Ok ()
        with Unix.Unix_error (e, _, _) ->
          Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
            (Tool_error.v ~path Tool_error.Write_file (Unix.error_message e))
      else
        let entries = Sys.readdir p in
        Array.sort String.compare entries;
        let rec go i =
          if i >= Array.length entries then
            try
              Unix.rmdir p;
              Ok ()
            with Unix.Unix_error (e, _, _) ->
              Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
                (Tool_error.v ~path Tool_error.Write_file (Unix.error_message e))
          else
            match remove_tree Fpath.(path / entries.(i)) with
            | Ok () -> go (i + 1)
            | Error _ as e -> e
        in
        go 0
