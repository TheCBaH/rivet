(* Tool_workspace and Tool_process.

   The workspace tests all start from an ABSENT root, because "construction
   creates nothing" is the property that matters and it cannot be observed
   against a root that already exists. *)

open Compcert_tools

let failures = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let check_eq name ~expected ~actual =
  if String.equal expected actual then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n    expected: %S\n    actual:   %S\n" name expected actual;
    incr failures)

let contains hay needle =
  let n = String.length needle and m = String.length hay in
  let rec go i = i + n <= m && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let is_ok = function Ok _ -> true | Error _ -> false
let is_err = function Ok _ -> false | Error _ -> true

let with_tmp f =
  let dir = Filename.temp_file "tools_proc" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  let rec rm p =
    match Unix.lstat p with
    | { Unix.st_kind = Unix.S_DIR; _ } ->
        Array.iter (fun e -> rm (Filename.concat p e)) (Sys.readdir p);
        Unix.rmdir p
    | _ -> Unix.unlink p
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  in
  Fun.protect ~finally:(fun () -> rm dir) (fun () -> f (Fpath.v dir))

let write p s =
  let oc = open_out_bin (Fpath.to_string p) in
  output_string oc s;
  close_out oc

let make_repo root =
  write Fpath.(root / "Makefile") "";
  Unix.mkdir Fpath.(to_string (root / "tools")) 0o700;
  write Fpath.(root / "tools" / "target-matrix.sh") "";
  Unix.mkdir Fpath.(to_string (root / "asm")) 0o700;
  write Fpath.(root / "asm" / "dune-project") "";
  match Repo.resolve ~cli:(Some root) ~env:(fun _ -> None) ~cwd:root with
  | Ok r -> r
  | Error _ -> failwith "test repo did not validate"

let exists p = Sys.file_exists (Fpath.to_string p)

(* {1 Construction creates nothing} *)

let test_construction_creates_nothing () =
  with_tmp (fun root ->
      let repo = make_repo root in
      let default = Fpath.(root / ".fixture-work") in
      check "workspace: the default work root does not exist to begin with" (not (exists default));
      let r = Tool_workspace.fixture_work repo ~env:(fun _ -> None) in
      check "workspace: constructing a capability for an ABSENT default succeeds" (is_ok r);
      check "workspace: and creates nothing" (not (exists default));
      (* Same for an external root, which is the P5 case. *)
      let ext = Fpath.(root / "elsewhere" / "work") in
      let env = function "FIXTURE_WORK" -> Some (Fpath.to_string ext) | _ -> None in
      let r = Tool_workspace.fixture_work repo ~env in
      check "workspace: an ABSENT external root is accepted (P5)" (is_ok r);
      check "workspace: and still nothing is created" (not (exists ext));
      (* Only ensure creates. *)
      match r with
      | Error _ -> check "workspace: ensure creates the tree" false
      | Ok root_cap -> (
          match Tool_workspace.child root_cap [ "install"; "x86_64" ] with
          | Error _ -> check "workspace: child of a valid root" false
          | Ok c ->
              check "workspace: child of a valid root" true;
              check "workspace: naming a child creates nothing"
                (not (exists (Tool_workspace.child_path c)));
              check "workspace: ensure creates the tree" (is_ok (Tool_workspace.ensure c));
              check "workspace: ... and it now exists" (exists (Tool_workspace.child_path c))))

(* {2 Work-root validation - D11 and the forbidden set} *)

let test_work_root_validation () =
  with_tmp (fun root ->
      let repo = make_repo root in
      let with_work s =
        Tool_workspace.fixture_work repo ~env:(function "FIXTURE_WORK" -> Some s | _ -> None)
      in
      (* D11: the shell uses the value with no validation at all, so relative
         and symlinked roots work today. Both are rejected here. *)
      check "workspace: a relative root is rejected (D11)" (is_err (with_work "relative/work"));
      check "workspace: '..' anywhere is rejected"
        (is_err (with_work (Fpath.to_string root ^ "/a/../b")));
      check "workspace: the filesystem root is rejected" (is_err (with_work "/"));
      check "workspace: the repository root is rejected" (is_err (with_work (Fpath.to_string root)));
      check "workspace: the fixture corpus is rejected"
        (is_err (with_work (Fpath.to_string (Repo.fixture_corpus repo))));
      check "workspace: the gas-xref corpus is rejected"
        (is_err (with_work (Fpath.to_string (Repo.gas_xref_corpus repo))));
      (* A symlinked component is rejected rather than resolved, because the
         root gets rm -rf'd and that would delete something else. *)
      Unix.mkdir Fpath.(to_string (root / "real")) 0o700;
      Unix.symlink "real" Fpath.(to_string (root / "link"));
      check "workspace: a symlinked component is rejected (D11)"
        (is_err (with_work (Fpath.to_string root ^ "/link/work")));
      check "workspace: an absolute, symlink-free external root is accepted (P5)"
        (is_ok (with_work (Fpath.to_string root ^ "/real/work")));
      (* Children are Identifier-validated and must stay inside. *)
      match with_work (Fpath.to_string root ^ "/real/work") with
      | Error _ -> check "workspace: child validation" false
      | Ok cap ->
          check "workspace: a child component with '..' is rejected"
            (is_err (Tool_workspace.child cap [ ".."; "escape" ]));
          check "workspace: a child component with a separator is rejected"
            (is_err (Tool_workspace.child cap [ "a/b" ]));
          check "workspace: a child component with a leading dash is rejected"
            (is_err (Tool_workspace.child cap [ "-rf" ])))

let test_scratch_cleanup () =
  let seen = ref None in
  let r =
    Tool_workspace.with_scratch ~label:"unit" (fun dir ->
        seen := Some dir;
        check "scratch: the directory exists inside the body" (exists dir);
        Ok ())
  in
  check "scratch: body result propagates" (is_ok r);
  (match !seen with
  | Some dir -> check "scratch: removed on the success path" (not (exists dir))
  | None -> check "scratch: removed on the success path" false);
  (* And on the failure path, which is the one that regresses silently. *)
  let seen2 = ref None in
  let _ =
    Tool_workspace.with_scratch ~label:"unit" (fun dir ->
        seen2 := Some dir;
        Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v Tool_error.Exec "boom"))
  in
  match !seen2 with
  | Some dir -> check "scratch: removed on the failure path too" (not (exists dir))
  | None -> check "scratch: removed on the failure path too" false

(* {3 Tool_process - the owned Unix runner} *)

let fake =
  (* Built by dune into a sibling directory; located relative to this test
     executable so the suite does not depend on a cwd. *)
  let dir = Filename.dirname Sys.executable_name in
  Filename.concat (Filename.concat (Filename.dirname dir) "fake") "fake_tool.exe"

let run_fake ?cwd ?(env = []) ?(accepted = Process_status.Zero_only) ?stdout ?stderr ?parsed_output
    spec_str =
  Tool_process.exec
    (Tool_process.spec ?cwd ?stdout ?stderr ?parsed_output
       ~env:(("FAKE_TOOL_SPEC", Some spec_str) :: env)
       ~accepted ~label:"fake" fake [])

let test_spawn_vs_exit () =
  (* The distinction op = Spawn exists to preserve. A missing program is not a
     program that ran and exited 127 - and the child DOES exit 127 internally,
     so this only works because the errno pipe reports the exec failure. *)
  (match
     Tool_process.exec
       (Tool_process.spec ~accepted:Process_status.Any_exit ~label:"absent"
          "/nonexistent/definitely-not-a-program" [])
   with
  | Error e ->
      check "process: a missing ABSOLUTE program is op = Spawn, never Exited 127"
        ((Err.Error.kind e).Tool_error.op = Tool_error.Spawn)
  | Ok _ -> check "process: a missing ABSOLUTE program is op = Spawn, never Exited 127" false);
  match
    Tool_process.exec
      (Tool_process.spec ~accepted:Process_status.Any_exit ~label:"absent"
         "definitely-not-a-program-on-path" [])
  with
  | Error e ->
      check "process: a missing PATH program is op = Spawn"
        ((Err.Error.kind e).Tool_error.op = Tool_error.Spawn)
  | Ok _ -> check "process: a missing PATH program is op = Spawn" false

(* PATH resolution against the EFFECTIVE CHILD environment, which is what makes
   a fake tool on a temporary PATH work at all. *)
let test_path_search () =
  with_tmp (fun dir ->
      let bin = Fpath.(dir / "bin") in
      Unix.mkdir (Fpath.to_string bin) 0o755;
      Unix.symlink fake Fpath.(to_string (bin / "faketool"));
      (match
         Tool_process.exec
           (Tool_process.spec
              ~env:[ ("PATH", Some (Fpath.to_string bin)); ("FAKE_TOOL_SPEC", Some "out:hi") ]
              ~stdout:Tool_process.Out_capture ~accepted:Process_status.Zero_only ~label:"fake"
              "faketool" [])
       with
      | Ok { Tool_process.stdout = Some s; _ } ->
          check_eq "process: PATH search uses the CHILD's PATH" ~expected:"hi" ~actual:s
      | _ -> check "process: PATH search uses the CHILD's PATH" false);
      (* A non-executable file earlier on PATH must not mask a working one
         later, and must not vanish from the diagnostic either. *)
      let shadow = Fpath.(dir / "shadow") in
      Unix.mkdir (Fpath.to_string shadow) 0o755;
      let blocked = Fpath.(shadow / "faketool") in
      let oc = open_out (Fpath.to_string blocked) in
      close_out oc;
      Unix.chmod (Fpath.to_string blocked) 0o644;
      match
        Tool_process.exec
          (Tool_process.spec
             ~env:
               [
                 ("PATH", Some (Fpath.to_string shadow ^ ":" ^ Fpath.to_string bin));
                 ("FAKE_TOOL_SPEC", Some "out:hi");
               ]
             ~stdout:Tool_process.Out_capture ~accepted:Process_status.Zero_only ~label:"fake"
             "faketool" [])
      with
      | Ok { Tool_process.stdout = Some s; _ } ->
          check_eq "process: a non-executable earlier on PATH does not mask a later match"
            ~expected:"hi" ~actual:s
      | _ -> check "process: a non-executable earlier on PATH does not mask a later match" false)

let test_capture () =
  (match run_fake ~stdout:Tool_process.Out_capture "out:abc" with
  | Ok { Tool_process.stdout = Some s; stderr; _ } ->
      check_eq "process: stdout captured exactly, no trailing newline added" ~expected:"abc"
        ~actual:s;
      check "process: the uncaptured stream is None" (stderr = None)
  | _ -> check "process: stdout captured exactly, no trailing newline added" false);
  (match run_fake ~stderr:Tool_process.Err_capture "err:oops\n" with
  | Ok { Tool_process.stderr = Some s; stdout; _ } ->
      check_eq "process: stderr captured exactly" ~expected:"oops\n" ~actual:s;
      check "process: stdout is None when only stderr is captured" (stdout = None)
  | _ -> check "process: stderr captured exactly" false);
  (* INDEPENDENT capture of both, which the Bos backend could not express. *)
  (match run_fake ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_capture "both:x" with
  | Ok { Tool_process.stdout = Some o; stderr = Some e; _ } ->
      check_eq "process: both streams captured independently - stdout" ~expected:"out:x" ~actual:o;
      check_eq "process: both streams captured independently - stderr" ~expected:"err:x" ~actual:e
  | _ -> check "process: both streams captured independently" false);
  (* Err_to_stdout is `2>&1`: ONE descriptor, so the child's own write order is
     preserved. Two descriptors onto one file would overwrite each other. *)
  match run_fake ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_to_stdout "both:y" with
  | Ok { Tool_process.stdout = Some s; _ } ->
      check "process: Err_to_stdout merges both streams into one capture"
        (contains s "out:y" && contains s "err:y")
  | _ -> check "process: Err_to_stdout merges both streams into one capture" false

let test_cwd () =
  with_tmp (fun dir ->
      (* The property CompCert's banner depends on: the child observes the
         requested directory, so a compile can use relative paths. *)
      let real = Unix.realpath (Fpath.to_string dir) in
      match run_fake ~cwd:(Fpath.v real) ~stdout:Tool_process.Out_capture "cwd" with
      | Ok { Tool_process.stdout = Some s; _ } ->
          check_eq "process: the child observes the requested cwd" ~expected:(real ^ "\n") ~actual:s
      | _ -> check "process: the child observes the requested cwd" false)

let test_env_precedence () =
  (* parsed_output's LC_ALL=C is applied AFTER the caller's overrides, so it
     wins. A caller and the runner must not race for this variable. *)
  (match
     run_fake
       ~env:[ ("LC_ALL", Some "en_US.UTF-8") ]
       ~parsed_output:true ~stdout:Tool_process.Out_capture "env:LC_ALL"
   with
  | Ok { Tool_process.stdout = Some s; _ } ->
      check_eq "process: parsed_output's LC_ALL beats a caller override" ~expected:"C\n" ~actual:s
  | _ -> check "process: parsed_output's LC_ALL beats a caller override" false);
  (match run_fake ~env:[ ("LC_ALL", Some "xx") ] ~stdout:Tool_process.Out_capture "env:LC_ALL" with
  | Ok { Tool_process.stdout = Some s; _ } ->
      check_eq "process: without parsed_output the caller's value stands" ~expected:"xx\n" ~actual:s
  | _ -> check "process: without parsed_output the caller's value stands" false);
  (* Removal, and inheritance from the parent. *)
  Unix.putenv "TOOLS_TEST_MARKER" "inherited";
  (match run_fake ~stdout:Tool_process.Out_capture "env:TOOLS_TEST_MARKER" with
  | Ok { Tool_process.stdout = Some s; _ } ->
      check_eq "process: the parent environment is inherited" ~expected:"inherited\n" ~actual:s
  | _ -> check "process: the parent environment is inherited" false);
  match
    run_fake
      ~env:[ ("TOOLS_TEST_MARKER", None) ]
      ~stdout:Tool_process.Out_capture "env:TOOLS_TEST_MARKER"
  with
  | Ok { Tool_process.stdout = Some s; _ } ->
      check_eq "process: (k, None) removes a variable" ~expected:"\n" ~actual:s
  | _ -> check "process: (k, None) removes a variable" false

let test_status_handling () =
  (match run_fake ~accepted:Process_status.Any_exit ~stderr:Tool_process.Err_null "signal:term" with
  | Error e ->
      check "process: a signal death is rejected even by Any_exit, with the POSIX number"
        ((Err.Error.kind e).Tool_error.status = Some (Process_status.Signaled 15))
  | Ok _ -> check "process: a signal death is rejected even by Any_exit" false);
  check "process: a non-zero exit is an error under Zero_only" (is_err (run_fake "exit:1"));
  check "process: the same exit is fine under Statuses [0;1] (the diff contract)"
    (is_ok (run_fake ~accepted:(Process_status.Statuses [ 0; 1 ]) "exit:1"));
  check "process: exit 2 is still an error under Statuses [0;1] (D9)"
    (is_err (run_fake ~accepted:(Process_status.Statuses [ 0; 1 ]) "exit:2"));
  check "process: Any_exit accepts exit 42"
    (is_ok (run_fake ~accepted:Process_status.Any_exit "exit:42"));
  (* A rejected run still carries the captured stderr, which is what makes a
     compiler failure diagnosable. *)
  match run_fake ~stderr:Tool_process.Err_capture "fail_with:boom" with
  | Error e ->
      check "process: a rejected run carries the captured stderr"
        ((Err.Error.kind e).Tool_error.stderr = Some "boom\n")
  | Ok _ -> check "process: a rejected run carries the captured stderr" false

(* A capture large enough that a pipe-based implementation would deadlock: the
   parent waits before reading, so anything over one pipe buffer would wedge. *)
let test_large_capture () =
  match run_fake ~stdout:Tool_process.Out_capture "big:2000000" with
  | Ok { Tool_process.stdout = Some s; _ } ->
      check "process: 2 MB on stdout does not deadlock" (String.length s = 2000000)
  | _ -> check "process: 2 MB on stdout does not deadlock" false

let test_no_shell () =
  (* Never through a shell: an argument that would be syntax to sh reaches the
     child intact. *)
  match
    Tool_process.exec
      (Tool_process.spec
         ~env:[ ("FAKE_TOOL_SPEC", Some "argv") ]
         ~stdout:Tool_process.Out_capture ~accepted:Process_status.Zero_only ~label:"fake" fake
         [ "a b"; "$HOME"; ";rm -rf /"; "*" ])
  with
  | Ok { Tool_process.stdout = Some s; _ } ->
      check_eq "process: arguments reach the child verbatim, never through a shell"
        ~expected:"a b\n$HOME\n;rm -rf /\n*\n" ~actual:s
  | _ -> check "process: arguments reach the child verbatim, never through a shell" false

let test_display () =
  let s = Tool_process.spec ~accepted:Process_status.Zero_only ~label:"l" "prog" [ "a b"; "c" ] in
  check_eq "process: display is a rendering, never a command line" ~expected:"prog a b c"
    ~actual:(Tool_process.display s)

let test_process_status_accepts () =
  let open Process_status in
  check "status: Any_exit rejects Signaled" (not (accepts Any_exit (Signaled 9)));
  check "status: Any_exit rejects Timed_out" (not (accepts Any_exit Timed_out));
  check "status: Zero_only accepts only 0"
    (accepts Zero_only (Exited 0) && not (accepts Zero_only (Exited 1)));
  check "status: Statuses membership"
    (accepts (Statuses [ 0; 1 ]) (Exited 1) && not (accepts (Statuses [ 0; 1 ]) (Exited 2)))

let () =
  print_endline "process:";
  test_process_status_accepts ();
  test_construction_creates_nothing ();
  test_work_root_validation ();
  test_scratch_cleanup ();
  test_spawn_vs_exit ();
  test_path_search ();
  test_capture ();
  test_cwd ();
  test_env_precedence ();
  test_status_handling ();
  test_large_capture ();
  test_no_shell ();
  test_display ();
  if !failures > 0 then (
    Printf.printf "process: %d failures\n" !failures;
    exit 1)
  else print_endline "process: all checks passed"
