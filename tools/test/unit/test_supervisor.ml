(* Supervisor, tested on the properties it exists to guarantee.

   Every check here is about a process being GONE, or about a wait ending when
   it should. Those are the failures that do not announce themselves: a leaked
   qemu holds a TCP port and the next run fails to bind with no indication why,
   and a deadline that never fires looks exactly like a hang. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let scratch = ref (Fpath.v "/tmp")
let out name = Fpath.(!scratch / name)

let unwrap = function
  | Ok v -> v
  | Error e ->
      prerr_endline (Tool_error.to_fatal_line (Err.Error.kind e));
      exit 1

(* Live-ness is asked of the KERNEL, not of our own bookkeeping: signal 0 to the
   pid reports whether it still exists. A test that consulted the handle's
   recorded status would agree with the implementation by construction. *)
let alive pid =
  try
    Unix.kill pid 0;
    true
  with Unix.Unix_error _ -> false

let rec waitgone pid n =
  if (not (alive pid)) || n = 0 then not (alive pid)
  else (
    Unix.sleepf 0.05;
    waitgone pid (n - 1))

let test_terminate_kills () =
  let t = unwrap (Supervisor.spawn ~stdout:(out "sleep.log") ~label:"sleep" "sleep" [ "300" ]) in
  check "spawn: the process is running" (alive (Supervisor.pid t));
  ignore (unwrap (Supervisor.terminate t));
  check "terminate: the process is gone" (waitgone (Supervisor.pid t) 40);
  (* Idempotent: the second call must not block, signal a recycled pid, or
     fail. *)
  match Supervisor.terminate t with
  | Ok None -> check "terminate: second call is a no-op" true
  | Ok (Some _) -> check "terminate: second call reported a fresh reap" false
  | Error _ -> check "terminate: second call errored" false

(* SIGTERM ignored, so only the KILL escalation can end it. Without escalation
   this hangs, which is the failure the grace interval exists to bound. *)
let test_escalation () =
  let script = Fpath.(!scratch / "stubborn.sh") in
  unwrap (Tool_fs.write script "#!/bin/sh\ntrap '' TERM\nwhile :; do sleep 0.1; done\n");
  Unix.chmod (Fpath.to_string script) 0o755;
  let t =
    unwrap
      (Supervisor.spawn ~stdout:(out "stubborn.log") ~label:"stubborn" (Fpath.to_string script) [])
  in
  Unix.sleepf 0.3;
  check "escalation: the stubborn process is running" (alive (Supervisor.pid t));
  let started = Unix.gettimeofday () in
  ignore (unwrap (Supervisor.terminate t));
  let took = Unix.gettimeofday () -. started in
  check "escalation: SIGKILL ended it" (waitgone (Supervisor.pid t) 60);
  (* It must have WAITED for the grace period rather than killing at once, and
     must not have waited much beyond it. *)
  check "escalation: waited out the TERM grace" (took >= Supervisor.term_grace);
  check "escalation: did not wait far beyond it" (took < Supervisor.term_grace +. 3.0)

(* The whole reason with_process exists: the shell's `kill "$qpid" || true` at
   the end of gdb_gate is never reached when anything above it exits non-zero
   under errexit. *)
let test_with_process_cleans_up_on_error () =
  let leaked = ref 0 in
  let r =
    Supervisor.with_process ~stdout:(out "err.log") ~label:"sleep" "sleep" [ "300" ] (fun t ->
        leaked := Supervisor.pid t;
        Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
          (Tool_error.v Tool_error.Validate "deliberate"))
  in
  check "with_process: the error propagates" (Result.is_error r);
  check "with_process: cleaned up on the error path" (waitgone !leaked 40)

let test_with_process_cleans_up_on_exception () =
  let leaked = ref 0 in
  let raised =
    try
      ignore
        (Supervisor.with_process ~stdout:(out "exn.log") ~label:"sleep" "sleep" [ "300" ] (fun t ->
             leaked := Supervisor.pid t;
             failwith "deliberate"));
      false
    with Failure _ -> true
  in
  check "with_process: the exception escapes" raised;
  check "with_process: cleaned up on the exception path" (waitgone !leaked 40)

(* A grandchild is why termination signals the GROUP. The child exits
   immediately, leaving its own child holding the log file open; killing only
   the direct pid would leave that one running. *)
let test_group_kill_reaches_grandchild () =
  let script = Fpath.(!scratch / "parent.sh") in
  let marker = Fpath.(!scratch / "grandchild.pid") in
  unwrap
    (Tool_fs.write script
       (Printf.sprintf "#!/bin/sh\nsleep 300 &\necho $! > %s\nwait\n" (Fpath.to_string marker)));
  Unix.chmod (Fpath.to_string script) 0o755;
  let t =
    unwrap (Supervisor.spawn ~stdout:(out "parent.log") ~label:"parent" (Fpath.to_string script) [])
  in
  Unix.sleepf 0.5;
  let gpid = int_of_string (String.trim (unwrap (Tool_fs.read marker))) in
  check "group: the grandchild is running" (alive gpid);
  ignore (unwrap (Supervisor.terminate t));
  check "group: terminate reached the grandchild" (waitgone gpid 60)

let test_await_ready () =
  let flag = Fpath.(!scratch / "ready.flag") in
  let script = Fpath.(!scratch / "slow.sh") in
  unwrap
    (Tool_fs.write script
       (Printf.sprintf "#!/bin/sh\nsleep 0.4\ntouch %s\nsleep 300\n" (Fpath.to_string flag)));
  Unix.chmod (Fpath.to_string script) 0o755;
  Supervisor.with_process ~stdout:(out "slow.log") ~label:"slow" (Fpath.to_string script) []
    (fun t ->
      let ready () = Sys.file_exists (Fpath.to_string flag) in
      let got = unwrap (Supervisor.await t ~ready ~timeout:5.0 ~poll:0.05) in
      check "await: became ready" got;
      Ok ())
  |> unwrap

let test_await_timeout () =
  Supervisor.with_process ~stdout:(out "never.log") ~label:"sleep" "sleep" [ "300" ] (fun t ->
      let started = Unix.gettimeofday () in
      let got = unwrap (Supervisor.await t ~ready:(fun () -> false) ~timeout:0.5 ~poll:0.05) in
      let took = Unix.gettimeofday () -. started in
      check "await: timeout returns false" (not got);
      check "await: the deadline actually fired" (took >= 0.5 && took < 3.0);
      Ok ())
  |> unwrap

(* A server that dies during startup can never become ready. Waiting the full
   timeout for it would turn a crash into a timeout, and on CI the two get
   diagnosed very differently. *)
let test_await_early_exit () =
  Supervisor.with_process ~stdout:(out "die.log") ~label:"false" "false" [] (fun t ->
      let started = Unix.gettimeofday () in
      let got = unwrap (Supervisor.await t ~ready:(fun () -> false) ~timeout:30.0 ~poll:0.05) in
      let took = Unix.gettimeofday () -. started in
      check "await: early exit returns false" (not got);
      check "await: returned promptly rather than waiting out 30s" (took < 5.0);
      Ok ())
  |> unwrap

(* exit_status is what lets a caller tell "never became ready" from "died". From
   the outside those are the same observation, and they deserve very different
   diagnoses - the tool gate reports a qemu that crashed at startup as having
   crashed rather than as a socket that never accepted. *)
let test_exit_status () =
  let t = unwrap (Supervisor.spawn ~stdout:(out "es.log") ~label:"sleep" "sleep" [ "300" ]) in
  check "exit_status: None while running" (unwrap (Supervisor.exit_status t) = None);
  ignore (unwrap (Supervisor.terminate t));
  let quick = unwrap (Supervisor.spawn ~stdout:(out "es2.log") ~label:"true" "true" []) in
  (* Polled rather than slept on: the child may not have been scheduled yet, and
     asserting immediately would make this a race that usually passes. *)
  let rec settle n =
    match unwrap (Supervisor.exit_status quick) with
    | Some st -> Some st
    | None when n = 0 -> None
    | None ->
        Unix.sleepf 0.05;
        settle (n - 1)
  in
  check "exit_status: Some Exited 0 once it has ended" (settle 40 = Some (Process_status.Exited 0));
  ignore (unwrap (Supervisor.terminate quick))

let test_spawn_failure () =
  match Supervisor.spawn ~stdout:(out "nope.log") ~label:"nope" "no_such_program_xyzzy" [] with
  | Error e ->
      check "spawn: a missing program is a Spawn error"
        ((Err.Error.kind e).Tool_error.op = Tool_error.Spawn)
  | Ok _ -> check "spawn: a missing program must not succeed" false

let test_output_captured () =
  let log = out "hello.log" in
  Supervisor.with_process ~stdout:log ~label:"sh" "sh" [ "-c"; "echo out; echo err >&2" ] (fun t ->
      ignore (Supervisor.await t ~ready:(fun () -> false) ~timeout:2.0 ~poll:0.05);
      Ok ())
  |> unwrap;
  let text = unwrap (Tool_fs.read log) in
  (* Merged onto ONE descriptor, so both appear and neither truncates the
     other - two descriptors on one file would each carry their own offset. *)
  check "stdout and stderr are merged into one file"
    (String.trim text = "out\nerr" || String.trim text = "err\nout")

let () =
  Tool_workspace.with_scratch ~label:"supervisor-test" (fun dir ->
      scratch := dir;
      print_endline "supervisor:";
      test_terminate_kills ();
      test_escalation ();
      test_with_process_cleans_up_on_error ();
      test_with_process_cleans_up_on_exception ();
      test_group_kill_reaches_grandchild ();
      test_await_ready ();
      test_await_timeout ();
      test_await_early_exit ();
      test_exit_status ();
      test_spawn_failure ();
      test_output_captured ();
      Ok ())
  |> unwrap;
  if !failures > 0 then (
    Printf.printf "supervisor: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "supervisor: all %d checks passed\n" !checks
