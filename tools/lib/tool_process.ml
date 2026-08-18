type stdout_sink = Out_capture | Out_file of Fpath.t | Out_null | Out_inherit
type stderr_sink = Err_capture | Err_file of Fpath.t | Err_null | Err_inherit | Err_to_stdout
type input = From_string of string | From_file of Fpath.t | From_dev_null | In_inherit
type result = { status : Process_status.t; stdout : string option; stderr : string option }

type spec = {
  prog : string;
  args : string list;
  cwd : Fpath.t option;
  env : (string * string option) list;
  stdin : input;
  stdout : stdout_sink;
  stderr : stderr_sink;
  accepted : Process_status.accepted;
  parsed_output : bool;
  label : string;
}

let spec ?cwd ?(env = []) ?(stdin = From_dev_null) ?(stdout = Out_null) ?(stderr = Err_inherit)
    ?(parsed_output = false) ~accepted ~label prog args =
  { prog; args; cwd; env; stdin; stdout; stderr; accepted; parsed_output; label }

let display s = String.concat " " (s.prog :: s.args)

let err ?status ?stderr ?cause op detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?status ?stderr ?cause op detail)

(* {1 Environment}

   The base is the PARENT environment, then the caller's ordered overrides, then
   LC_ALL last so it beats a caller's own setting. That precedence is tested: a
   caller asking for parsed output and a caller setting LC_ALL must not race. *)
let child_env s =
  let tbl = Hashtbl.create 64 in
  Array.iter
    (fun kv ->
      match String.index_opt kv '=' with
      | Some i ->
          Hashtbl.replace tbl (String.sub kv 0 i) (String.sub kv (i + 1) (String.length kv - i - 1))
      | None -> ())
    (Unix.environment ());
  List.iter
    (fun (k, v) -> match v with Some v -> Hashtbl.replace tbl k v | None -> Hashtbl.remove tbl k)
    s.env;
  if s.parsed_output then Hashtbl.replace tbl "LC_ALL" "C";
  Hashtbl.fold (fun k v acc -> (k ^ "=" ^ v) :: acc) tbl []
  |> List.sort String.compare |> Array.of_list

let path_of env =
  Array.to_list env
  |> List.find_map (fun kv ->
      match String.index_opt kv '=' with
      | Some i when String.sub kv 0 i = "PATH" ->
          Some (String.sub kv (i + 1) (String.length kv - i - 1))
      | _ -> None)
  |> Option.value ~default:""

(* {2 Program resolution}

   execvp-style, resolved against the EFFECTIVE CHILD PATH rather than the
   parent's - which is what makes a fake tool on a temporary PATH work.

   EACCES on a match is remembered and the search continues, then reported if
   nothing else matches: a non-executable file earlier on PATH must not mask a
   working one later, but neither should it vanish from the diagnostic. *)
let resolve_program s env =
  if String.contains s.prog '/' then
    match Unix.access s.prog [ Unix.X_OK ] with
    | () -> Ok s.prog
    | exception Unix.Unix_error (e, _, _) ->
        err Tool_error.Spawn (Printf.sprintf "cannot execute %s: %s" s.prog (Unix.error_message e))
  else
    let dirs =
      String.split_on_char ':' (path_of env) |> List.map (fun d -> if d = "" then "." else d)
    in
    let denied = ref None in
    let rec go = function
      | [] -> (
          match !denied with
          | Some d ->
              err Tool_error.Spawn
                (Printf.sprintf "cannot execute %s: %s is not executable" s.prog d)
          | None ->
              err Tool_error.Spawn (Printf.sprintf "cannot execute %s: not found on PATH" s.prog))
      | dir :: rest -> (
          let candidate = Filename.concat dir s.prog in
          match Unix.access candidate [ Unix.X_OK ] with
          | () -> Ok candidate
          | exception Unix.Unix_error (Unix.EACCES, _, _) ->
              if !denied = None then denied := Some candidate;
              go rest
          | exception Unix.Unix_error _ -> go rest)
    in
    go dirs

(* {3 Capture}

   Captured output goes to a temporary FILE, not a pipe. A pipe would need the
   parent to drain it while the child runs, and a parent that waits first
   deadlocks as soon as the child writes more than one pipe buffer - which the
   >1 MiB cases do. A file cannot fill. *)
let with_tmp_file f =
  let path = Filename.temp_file "compcert-tools-cap" "" in
  Fun.protect ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ()) (fun () -> f path)

let read_all path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let exec s =
  let ( let* ) = Result.bind in
  let env = child_env s in
  let* program = resolve_program s env in
  let argv = Array.of_list (s.prog :: s.args) in
  let want_out = s.stdout = Out_capture in
  let want_err = s.stderr = Err_capture in

  let run out_cap err_cap =
    let stdin_path = ref None in
    (* From_string is materialized BEFORE the fork: writing it in the child
       would make the child depend on data the parent had not finished
       producing. *)
    let open_stdin () =
      match s.stdin with
      | In_inherit -> Unix.stdin
      | From_dev_null -> Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0
      | From_file p -> Unix.openfile (Fpath.to_string p) [ Unix.O_RDONLY ] 0
      | From_string str ->
          let path = Filename.temp_file "compcert-tools-in" "" in
          stdin_path := Some path;
          let oc = open_out_bin path in
          output_string oc str;
          close_out oc;
          Unix.openfile path [ Unix.O_RDONLY ] 0
    in
    let open_out_fd () =
      match s.stdout with
      | Out_inherit -> Unix.stdout
      | Out_null -> Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0
      | Out_file p ->
          Unix.openfile (Fpath.to_string p) [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
      | Out_capture -> Unix.openfile (Option.get out_cap) [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
    in
    let open_err_fd out_fd =
      match s.stderr with
      | Err_inherit -> Unix.stderr
      | Err_null -> Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0
      | Err_file p ->
          Unix.openfile (Fpath.to_string p) [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
      | Err_capture -> Unix.openfile (Option.get err_cap) [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
      (* 2>&1 is the SAME descriptor, deliberately - two descriptors onto one
         file would each carry their own offset and overwrite each other, losing
         exactly the interleaving `ccomp -version 2>&1 | head -1` depends on. *)
      | Err_to_stdout -> out_fd
    in
    let fd_in = open_stdin () in
    let fd_out = open_out_fd () in
    let fd_err = open_err_fd fd_out in
    (* Both ends close-on-exec: in the child the write end vanishes on a
       SUCCESSFUL exec, so the parent reads EOF and knows the exec worked. No
       timing assumption and no sentinel value. *)
    let errno_r, errno_w = Unix.pipe ~cloexec:true () in
    let std fd = fd = Unix.stdin || fd = Unix.stdout || fd = Unix.stderr in
    let close_parent_fds () =
      let maybe fd = if not (std fd) then try Unix.close fd with Unix.Unix_error _ -> () in
      maybe fd_in;
      maybe fd_out;
      if s.stderr <> Err_to_stdout then maybe fd_err;
      match !stdin_path with Some p -> ( try Sys.remove p with Sys_error _ -> ()) | None -> ()
    in
    match Unix.fork () with
    | 0 -> (
        (* Child. A failure here goes through the pipe and then _exit, never
           exit: running the parent's at_exit handlers in a forked child would
           flush the parent's buffers a second time. *)
        try
          Unix.close errno_r;
          Unix.dup2 fd_in Unix.stdin;
          Unix.dup2 fd_out Unix.stdout;
          Unix.dup2 fd_err Unix.stderr;
          (match s.cwd with Some d -> Unix.chdir (Fpath.to_string d) | None -> ());
          Unix.execve program argv env
        with e ->
          let msg =
            match e with
            | Unix.Unix_error (code, _, _) -> Unix.error_message code
            | e -> Printexc.to_string e
          in
          let b = Bytes.of_string msg in
          (try ignore (Unix.write errno_w b 0 (Bytes.length b)) with _ -> ());
          Unix._exit 127)
    | pid -> (
        Unix.close errno_w;
        let exec_error =
          let buf = Bytes.create 256 in
          match Unix.read errno_r buf 0 256 with
          | 0 -> None
          | n -> Some (Bytes.sub_string buf 0 n)
          | exception Unix.Unix_error _ -> None
        in
        (try Unix.close errno_r with Unix.Unix_error _ -> ());
        (* Reaped unconditionally, including when exec failed: the child called
           _exit and is a zombie until waited for. Closing descriptors would
           leak it. *)
        let rec wait () =
          match Unix.waitpid [] pid with
          | _, st -> st
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
        in
        let wait_status = wait () in
        close_parent_fds ();
        match exec_error with
        | Some msg -> err Tool_error.Spawn (Printf.sprintf "cannot execute %s: %s" s.prog msg)
        | None ->
            let status =
              match wait_status with
              | Unix.WEXITED n -> Process_status.Exited n
              | Unix.WSIGNALED n -> Process_status.signaled n
              | Unix.WSTOPPED n -> Process_status.signaled n
            in
            let captured_out = if want_out then Some (read_all (Option.get out_cap)) else None in
            let captured_err = if want_err then Some (read_all (Option.get err_cap)) else None in
            if Process_status.accepts s.accepted status then
              Ok { status; stdout = captured_out; stderr = captured_err }
            else
              err ~status ?stderr:captured_err Tool_error.Exec
                (Format.asprintf "%s %a" s.label Process_status.pp status))
  in
  (* Nested rather than unconditional, so a run that captures nothing creates
     no temporary files at all. *)
  match (want_out, want_err) with
  | false, false -> run None None
  | true, false -> with_tmp_file (fun o -> run (Some o) None)
  | false, true -> with_tmp_file (fun e -> run None (Some e))
  | true, true -> with_tmp_file (fun o -> with_tmp_file (fun e -> run (Some o) (Some e)))

let resolve s =
  let env = child_env s in
  Result.map (fun program -> (program, env)) (resolve_program s env)
