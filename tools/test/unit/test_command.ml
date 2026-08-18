(* The ordering properties of Command, which are the reason it is an event list
   rather than a result.

   Rendering is checked by capturing the real file descriptors rather than by
   inspecting the event list: the claim being made is about the BYTES on stdout
   and stderr and their order, and an event-list assertion would pass just as
   happily if render wrote everything to one stream. *)

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

(* Redirect both real descriptors, so what is captured is what a shell would
   see. Format.eprintf writes through the same descriptor, so the --err-trace
   path is captured too. *)
let capture f =
  let tmp_out = Filename.temp_file "tools_out" ".txt" in
  let tmp_err = Filename.temp_file "tools_err" ".txt" in
  let read p =
    let ic = open_in_bin p in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    Sys.remove p;
    s
  in
  (* Flush BEFORE redirecting, not only after: this test's own buffered output
     would otherwise be written through the redirected descriptor and captured
     as if the command under test had produced it. *)
  flush stdout;
  flush stderr;
  let saved_out = Unix.dup Unix.stdout and saved_err = Unix.dup Unix.stderr in
  let fd_out = Unix.openfile tmp_out [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let fd_err = Unix.openfile tmp_err [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  Unix.dup2 fd_out Unix.stdout;
  Unix.dup2 fd_err Unix.stderr;
  let finish () =
    flush stdout;
    flush stderr;
    Unix.dup2 saved_out Unix.stdout;
    Unix.dup2 saved_err Unix.stderr;
    Unix.close saved_out;
    Unix.close saved_err;
    Unix.close fd_out;
    Unix.close fd_err
  in
  let result =
    try f ()
    with e ->
      finish ();
      raise e
  in
  finish ();
  (result, read tmp_out, read tmp_err)

let contains hay needle =
  let n = String.length needle and m = String.length hay in
  let rec go i = i + n <= m && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let err ?(op = Tool_error.Validate) detail =
  Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp (Tool_error.v op detail)

let fatal detail = Command.of_error (err detail)

(* {1 P4: success then failure, in that order} *)

let test_success_then_failure () =
  let c =
    Command.accumulate [ `Good; `Bad ] ~f:(function
      | `Good -> Command.ok [ Diagnostic.stdout "fixtures: return42: 50 files match the manifest" ]
      | `Bad -> fatal "no such case 'does_not_exist' under /corpus")
  in
  let code, out, e = capture (fun () -> Command.render ~err_trace:false c) in
  check "p4: exit 1" (code = 1);
  check_eq "p4: stdout carries the success line"
    ~expected:"fixtures: return42: 50 files match the manifest\n" ~actual:out;
  check_eq "p4: stderr carries only the fatal"
    ~expected:"FATAL: no such case 'does_not_exist' under /corpus\n" ~actual:e

(* {2 verify: diff on stdout, fatal on stderr, and the NEXT case still runs} *)

let test_diff_then_fatal_then_next_case () =
  let diff_body = "--- a\n+++ b\n@@ -1 +1 @@\n-x\n+y\n" in
  let c =
    Command.accumulate [ `Differs; `Matches ] ~f:(function
      | `Differs ->
          {
            Command.events =
              [
                Command.Output (Diagnostic.stdout diff_body);
                Command.Fatal (err "fixture regeneration differs for return42/x86_64");
              ];
            exit = `Failure;
          }
      | `Matches ->
          Command.ok [ Diagnostic.stdout "fixtures: loop/x86_64 regenerates byte-identically" ])
  in
  let code, out, e = capture (fun () -> Command.render ~err_trace:false c) in
  check "verify: exit 1" (code = 1);
  (* The diff body already ends in a newline; the renderer must not double it,
     and the following case's line must appear after it. *)
  check_eq "verify: diff then the later success, one newline between"
    ~expected:(diff_body ^ "fixtures: loop/x86_64 regenerates byte-identically\n")
    ~actual:out;
  check_eq "verify: fatal on stderr"
    ~expected:"FATAL: fixture regeneration differs for return42/x86_64\n" ~actual:e

(* {3 usage then fatal, exiting 1 - the --verify badtarget shape} *)

let test_usage_then_fatal () =
  let c =
    {
      Command.events =
        [
          Command.Output (Diagnostic.stderr "Usage: prog --check | --regen | --rehash [case ...]");
          Command.Fatal (err "--verify supports x86_32 x86_64, got 'sparc'");
        ];
      exit = `Failure;
    }
  in
  let code, out, e = capture (fun () -> Command.render ~err_trace:false c) in
  check "usage-then-fatal: exit 1, NOT 2" (code = 1);
  check_eq "usage-then-fatal: nothing on stdout" ~expected:"" ~actual:out;
  check_eq "usage-then-fatal: usage precedes the fatal, and usage has no FATAL: prefix"
    ~expected:
      "Usage: prog --check | --regen | --rehash [case ...]\n\
       FATAL: --verify supports x86_32 x86_64, got 'sparc'\n"
    ~actual:e

(* {4 Findings are Output, so they never gain a FATAL: prefix} *)

let test_findings_are_not_fatal () =
  let c =
    Command.fail
      [
        Diagnostic.stderr "MISSING x86_64/asm_test_entry.s";
        Diagnostic.stderr "CHANGED x86_32/asm_test_entry.s";
      ]
  in
  let code, out, e = capture (fun () -> Command.render ~err_trace:false c) in
  check "findings: exit 1" (code = 1);
  check_eq "findings: stdout empty" ~expected:"" ~actual:out;
  check_eq "findings: verbatim, in order, no FATAL: prefix"
    ~expected:"MISSING x86_64/asm_test_entry.s\nCHANGED x86_32/asm_test_entry.s\n" ~actual:e

(* {5 Accumulated errors: both rendered, once each, in input order} *)

let test_accum_two_malformed_records () =
  let bad n =
    Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
      (Tool_error.v Tool_error.Parse (Printf.sprintf "malformed record %d" n))
  in
  let r = Err.Accum.map (fun n -> bad n) [ 1; 2 ] in
  let c = Command.of_accum_result r ~ok:(fun _ -> Command.ok []) in
  let code, out, e = capture (fun () -> Command.render ~err_trace:false c) in
  check "accum: exit 1" (code = 1);
  check_eq "accum: stdout empty" ~expected:"" ~actual:out;
  check_eq "accum: both diagnostics, once each, in input order"
    ~expected:"FATAL: malformed record 1\nFATAL: malformed record 2\n" ~actual:e;
  (* P7: the default rendering carries no trace metadata at all. Asserted as
     exact equality above, which is the strongest form of this claim - any
     source location, event name or origin would have to appear in those bytes. *)
  check "accum: default output mentions no source file" (not (contains e ".ml"))

(* {6 --err-trace: the two DISTINCT detection origins are both present} *)

let test_err_trace_keeps_distinct_origins () =
  let bad n =
    (* Two different call sites, so two different ~pos values. *)
    if n = 1 then
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v Tool_error.Parse "malformed record 1")
    else
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v Tool_error.Parse "malformed record 2")
  in
  let r = Err.Accum.map (fun n -> bad n) [ 1; 2 ] in
  let c = Command.of_accum_result r ~ok:(fun _ -> Command.ok []) in
  let _, _, e = capture (fun () -> Command.render ~err_trace:true c) in
  check "err-trace: both payloads present"
    (contains e "malformed record 1" && contains e "malformed record 2");
  (* Two DISTINCT detection origins: fold_errors would have kept only the
     first, which is exactly why Command.of_errors does not use it. *)
  check "err-trace: names this test file as the detection site" (contains e "test_command.ml");
  check "err-trace: output differs from the default rendering"
    (String.length e > String.length "FATAL: malformed record 1\nFATAL: malformed record 2\n")

(* {7 accumulate takes the WORST exit class, and Usage beats Failure} *)

let test_worst_exit_class () =
  let usage = Command.of_error (err ~op:Tool_error.Usage "bad usage") in
  let c = Command.accumulate [ `A; `B ] ~f:(function `A -> Command.fail [] | `B -> usage) in
  let code, _, _ = capture (fun () -> Command.render ~err_trace:false c) in
  check "worst: Usage beats Failure" (code = 2);
  let c2 =
    Command.accumulate [ `A; `B ] ~f:(function `A -> Command.ok [] | `B -> Command.ok [])
  in
  let code2, _, _ = capture (fun () -> Command.render ~err_trace:false c2) in
  check "worst: all success stays 0" (code2 = 0)

(* {8 Diagnostic newline ownership} *)

let test_newline_ownership () =
  let _, with_nl, _ =
    capture (fun () -> Command.render ~err_trace:false (Command.ok [ Diagnostic.stdout "x\n" ]))
  in
  let _, without_nl, _ =
    capture (fun () -> Command.render ~err_trace:false (Command.ok [ Diagnostic.stdout "x" ]))
  in
  check_eq "newline: a trailing newline is not doubled" ~expected:"x\n" ~actual:with_nl;
  check_eq "newline: a bare line gains exactly one" ~expected:"x\n" ~actual:without_nl;
  let _, blank, _ =
    capture (fun () -> Command.render ~err_trace:false (Command.ok [ Diagnostic.stdout "x\n\n" ]))
  in
  check_eq "newline: only ONE trailing newline is stripped, so a blank final line survives"
    ~expected:"x\n\n" ~actual:blank

let () =
  print_endline "command:";
  test_success_then_failure ();
  test_diff_then_fatal_then_next_case ();
  test_usage_then_fatal ();
  test_findings_are_not_fatal ();
  test_accum_two_malformed_records ();
  test_err_trace_keeps_distinct_origins ();
  test_worst_exit_class ();
  test_newline_ownership ();
  if !failures > 0 then (
    Printf.printf "command: %d failures\n" !failures;
    exit 1)
  else print_endline "command: all checks passed"
