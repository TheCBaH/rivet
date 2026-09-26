let err detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v Tool_error.Spawn detail)

let positive s =
  match int_of_string_opt (String.trim s) with Some n when n > 0 -> Some n | _ -> None

let default_jobs () =
  match Option.bind (Sys.getenv_opt "COMPCERT_TOOLS_JOBS") positive with
  | Some n -> n
  | None -> (
      match
        Tool_process.exec
          (Tool_process.spec ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_null
             ~accepted:Process_status.Zero_only ~label:"getconf" "getconf" [ "_NPROCESSORS_ONLN" ])
      with
      | Ok { Tool_process.stdout = Some out; _ } -> Option.value (positive out) ~default:1
      | _ -> 1)

(* One worker: evaluate its share and marshal [(index, result)] pairs, or the
   exception text, into its own file. [_exit], never [exit]: the parent's
   at_exit handlers and unflushed buffers belong to the parent. *)
let worker f share path =
  let payload =
    try Ok (List.map (fun (i, x) -> (i, f x)) share) with e -> Error (Printexc.to_string e)
  in
  (try
     let oc = open_out_bin path in
     Marshal.to_channel oc payload [ Marshal.Closures ];
     close_out oc
   with _ -> Unix._exit 2);
  Unix._exit 0

let map ~jobs f xs =
  let n = List.length xs in
  let jobs = min jobs n in
  if jobs <= 1 then Ok (List.map f xs)
  else
    let indexed = List.mapi (fun i x -> (i, x)) xs in
    let shares = Array.make jobs [] in
    List.iter (fun (i, x) -> shares.(i mod jobs) <- (i, x) :: shares.(i mod jobs)) indexed;
    Tool_workspace.with_scratch ~label:"parallel" (fun dir ->
        flush_all ();
        let workers =
          Array.to_list
            (Array.mapi
               (fun k share ->
                 let path = Fpath.(to_string (dir / Printf.sprintf "worker-%d" k)) in
                 match Unix.fork () with
                 | 0 -> worker f (List.rev share) path
                 | pid -> (k, pid, path))
               shares)
        in
        let results = Array.make n None in
        let ( let* ) = Result.bind in
        let collect acc (k, pid, path) =
          let _, status = Unix.waitpid [] pid in
          let* () = acc in
          match status with
          | Unix.WEXITED 0 -> (
              let ic = open_in_bin path in
              let payload : ((int * 'b) list, string) result =
                Fun.protect ~finally:(fun () -> close_in ic) (fun () -> Marshal.from_channel ic)
              in
              match payload with
              | Ok pairs ->
                  List.iter (fun (i, r) -> results.(i) <- Some r) pairs;
                  Ok ()
              | Error e -> err (Printf.sprintf "parallel worker %d raised %s" k e))
          | Unix.WEXITED c -> err (Printf.sprintf "parallel worker %d exited with %d" k c)
          | Unix.WSIGNALED s | Unix.WSTOPPED s ->
              err (Printf.sprintf "parallel worker %d stopped by signal %d" k s)
        in
        (* Every worker is reaped even after one fails. *)
        let* () = List.fold_left collect (Ok ()) workers in
        Ok (Array.to_list (Array.map Option.get results)))
