let ( let* ) = Result.bind

type t = { pid : int; label : string; mutable status : Process_status.t option }

let pid t = t.pid
let term_grace = 2.0
let kill_grace = 2.0

let err ?cause op detail =
  Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v ?cause op detail)

let spawn ?cwd ?(env = []) ~stdout ~label prog args =
  (* Resolution and the environment come from the same code the synchronous
     runner uses, so a supervised process and an ordinary one disagree about
     nothing - not PATH, not LC_ALL, not the override order. *)
  let spec = Tool_process.spec ?cwd ~env ~accepted:Process_status.Any_exit ~label prog args in
  let* program, child_env = Tool_process.resolve spec in
  let argv = Array.of_list (prog :: args) in
  let fd_out =
    Unix.openfile (Fpath.to_string stdout) [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let fd_null = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let errno_r, errno_w = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 -> (
      try
        Unix.close errno_r;
        (* setsid, not setpgid: OCaml's Unix exposes only the former, and it
           is the stronger of the two - a new SESSION and a new process group
           with this child as leader, so its pgid equals its pid and
           kill(-pid) reaches the whole tree. A freshly forked child is never
           already a group leader, so this cannot fail with EPERM.
           In the CHILD, before exec: doing it in the parent races with a fast
           exec, and losing that race is precisely the case where cleanup would
           later fail to reach a grandchild. *)
        ignore (Unix.setsid ());
        Unix.dup2 fd_null Unix.stdin;
        Unix.dup2 fd_out Unix.stdout;
        (* Merged, deliberately: the same descriptor, so the two streams share
           one file offset and their interleaving survives. *)
        Unix.dup2 fd_out Unix.stderr;
        (match cwd with Some d -> Unix.chdir (Fpath.to_string d) | None -> ());
        Unix.execve program argv child_env
      with e ->
        let msg =
          match e with
          | Unix.Unix_error (code, _, _) -> Unix.error_message code
          | e -> Printexc.to_string e
        in
        let b = Bytes.of_string msg in
        (try ignore (Unix.write errno_w b 0 (Bytes.length b)) with _ -> ());
        Unix._exit 127)
  | child -> (
      Unix.close errno_w;
      let exec_error =
        let buf = Bytes.create 256 in
        match Unix.read errno_r buf 0 256 with
        | 0 -> None
        | n -> Some (Bytes.sub_string buf 0 n)
        | exception Unix.Unix_error _ -> None
      in
      (try Unix.close errno_r with Unix.Unix_error _ -> ());
      (try Unix.close fd_out with Unix.Unix_error _ -> ());
      (try Unix.close fd_null with Unix.Unix_error _ -> ());
      match exec_error with
      | Some msg ->
          (* Reaped even here: the child called _exit and is a zombie until
             waited for. *)
          (try ignore (Unix.waitpid [] child) with Unix.Unix_error _ -> ());
          err Tool_error.Spawn (Printf.sprintf "cannot execute %s: %s" prog msg)
      | None -> Ok { pid = child; label; status = None })

(* Non-blocking reap. Records the status the first time it is observed, so a
   later terminate does not block on an already-dead child. *)
let poll_status t =
  match t.status with
  | Some _ as s -> s
  | None -> (
      match Unix.waitpid [ Unix.WNOHANG ] t.pid with
      | 0, _ -> None
      | _, Unix.WEXITED n ->
          t.status <- Some (Process_status.Exited n);
          t.status
      | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) ->
          t.status <- Some (Process_status.signaled n);
          t.status
      | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
          (* Already reaped by someone else. Recorded as an unknown exit rather
             than left as None, so terminate cannot spin. *)
          t.status <- Some (Process_status.Exited 0);
          t.status
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> None)

let exit_status t = Ok (poll_status t)
let elapsed_since start = Mtime.Span.to_float_ns (Mtime.span start (Mtime_clock.now ())) /. 1e9

let await t ~ready ~timeout ~poll =
  let start = Mtime_clock.now () in
  let rec go () =
    if ready () then Ok true
    else if poll_status t <> None then
      (* The child is gone. It can never become ready, so waiting out the
         timeout would report a hang where there was a crash. *)
      Ok false
    else if elapsed_since start >= timeout then Ok false
    else (
      (try Unix.sleepf poll with Unix.Unix_error (Unix.EINTR, _, _) -> ());
      go ())
  in
  go ()

(* Signals the GROUP (negative pid). ESRCH is success: it means nothing is left
   to signal, which is the state being asked for. *)
let signal_group t signal =
  let send target =
    try Unix.kill target signal with
    | Unix.Unix_error (Unix.ESRCH, _, _) -> ()
    | Unix.Unix_error (Unix.EPERM, _, _) -> ()
  in
  (* The GROUP first, then the pid itself. Both, because setsid runs in the
     child and the parent cannot observe when: if we signalled only the group
     and the child had not yet called setsid, no group with that id would exist
     and ESRCH would be indistinguishable from success - leaking exactly the
     process this module promises to remove. Signalling the pid directly always
     reaches it whatever the grouping, and a duplicate signal to an already
     dying process is harmless. *)
  send (-t.pid);
  send t.pid

let wait_for_exit t ~timeout =
  let start = Mtime_clock.now () in
  let rec go () =
    if poll_status t <> None then true
    else if elapsed_since start >= timeout then false
    else (
      (try Unix.sleepf 0.02 with Unix.Unix_error (Unix.EINTR, _, _) -> ());
      go ())
  in
  go ()

let terminate t =
  match t.status with
  | Some _ -> Ok None (* already reaped by an earlier call *)
  | None ->
      signal_group t Sys.sigterm;
      if wait_for_exit t ~timeout:term_grace then Ok t.status
      else (
        signal_group t Sys.sigkill;
        if wait_for_exit t ~timeout:kill_grace then Ok t.status
        else
          (* KILL is uncatchable, so this is an uninterruptible-sleep kernel
             state rather than a process ignoring us. Reported rather than
             looped on forever: a silent spin here would look exactly like the
             hang it is meant to prevent. *)
          err Tool_error.Exec
            (Printf.sprintf "%s (pid %d) survived SIGKILL for %.1fs" t.label t.pid kill_grace))

let with_process ?cwd ?env ~stdout ~label prog args f =
  let* t = spawn ?cwd ?env ~stdout ~label prog args in
  Fun.protect ~finally:(fun () -> ignore (terminate t)) (fun () -> f t)
