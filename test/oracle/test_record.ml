(* Synthetic conformance cases for the recovery validator
   (asm/docs/exec-abi-v1.md §15.2, required by §16.4).

   These are the cases QEMU cannot produce. The user-mode helper never publishes
   a terminal fault - an unhandled guest signal kills the process, leaving
   running - and no test can reliably interrupt a helper between two adjacent
   stores. Both are nevertheless part of the wire format the optional system-mode
   backend will publish, and §6.1 is explicit that the optional backend must not
   be the first validator of a shared format. So the records are fabricated
   here, byte by byte, from the same offsets the validator reads.

   Every case is written as a canonical record plus exactly one difference, so a
   failure names the field that caused it. The expect output is the artifact: it
   shows what the host would report, which is the thing a reviewer needs to
   check against the document. *)

open Asm_oracle

let case_id = 7
let show s = Fmt.pr "%a@." Record.pp_verdict (Record.validate ~expected_case_id:case_id s)

(* A canonical bootstrap page: what step 4 of §4 writes through the control
   alias, and the starting point for the pending-pattern cases. *)
let bootstrap = Record.build ()

(* The shape §4 step 9 commits, and the two terminal records that can follow. *)
let validated status =
  Record.build ~record_state:Abi.Validated ~status ~case_id ~expected:[| 42L |] ()

let returned status ~values =
  Record.build ~record_state:Abi.Returned ~status ~case_id ~expected:[| 42L |] ~values ()

let patch = Manifest.patch_bytes
let patch_u16 = Manifest.patch_u16
let patch_u32 = Manifest.patch_u32

(* {1 Step 1 - unconditional framing} *)

let%expect_test "framing" =
  show (Manifest.truncate_to bootstrap 256);
  show (Manifest.append bootstrap "\000");
  show (patch bootstrap Abi.Result_off.magic "ASMRES\000\002");
  show (patch_u16 bootstrap Abi.Result_off.abi_version 2);
  show (patch bootstrap Abi.Result_off.reserved "\001");
  (* The guest can see the result page through the second alias, so a stray
     guest store lands in the padding and must not read back as a valid
     record. *)
  show (patch bootstrap 4095 "\001");
  [%expect
    {|
    invalid: framing: result file is 256 bytes, expected 4096
    invalid: framing: result file is 4097 bytes, expected 4096
    invalid: framing: bad result magic
    invalid: framing: abi_version 2, expected 1
    invalid: framing: reserved bytes nonzero
    invalid: framing: page tail nonzero |}]

(* {1 Step 2 - the raw pending-error patterns}

   The four rows of §15.2's zero-subcode table, in order. They differ only in
   record_state and whether the staged subcode is zero, and they have four
   different outcomes - which is why the table is exhaustive rather than a rule
   of thumb about "uncommitted errors". *)

let%expect_test "zero-subcode table" =
  show bootstrap;
  show (patch_u16 bootstrap Abi.Result_off.error_subcode 3);
  show (patch_u32 bootstrap Abi.Result_off.record_state 1L);
  show
    (patch_u16 (patch_u32 bootstrap Abi.Result_off.record_state 1L) Abi.Result_off.error_subcode 3);
  [%expect
    {|
    bootstrap/not_started case_id=0 n_expected=0 expected=[] n_values=0 values=[] diag=""
    bootstrap/not_started case_id=0 n_expected=0 expected=[] n_values=0 values=[] diag="" staged_subcode=ignored
    invalid: state: pre_run_error without a committed runner_error
    pending_pre_run_error (result unavailable) |}]

(* With runner_error = 0 the class commit has not happened, so step 2 cannot
   consult a class-scoped subcode table: it tests nonzero-ness and nothing more.
   A subcode no class defines is therefore still a pending pattern. *)
let%expect_test "unknown staged subcode" =
  show (patch_u16 bootstrap Abi.Result_off.error_subcode 60000);
  show
    (patch_u16
       (patch_u32 bootstrap Abi.Result_off.record_state 1L)
       Abi.Result_off.error_subcode 60000);
  [%expect
    {|
    bootstrap/not_started case_id=0 n_expected=0 expected=[] n_values=0 values=[] diag="" staged_subcode=ignored
    pending_pre_run_error (result unavailable) |}]

(* Step 2 returns immediately, so each pattern must enforce every step-3 and
   step-4 invariant it bypasses. One near-miss per bypassed field: all of them
   must fall through to step 3 and fail there, which is what keeps the two
   exceptions narrow enough not to become a door for arbitrary corruption.

   They converge on one message, and that is the correct answer rather than a
   weak test. Each record still carries the staged subcode that made it a
   candidate for pattern A; once the corrupt field disqualifies it from the
   pattern, the *first* rule it violates in step 3 is the committed-error schema
   - a subcode with no committed class - and precedence says the earliest
   failure is what gets reported. What the case establishes is that none of the
   eight is accepted, not which message each produces. *)
let%expect_test "pattern near-misses" =
  let staged = patch_u16 bootstrap Abi.Result_off.error_subcode 3 in
  let near name s =
    Fmt.pr "%-12s " name;
    show s
  in
  near "status" (patch_u16 staged Abi.Result_off.status (Abi.status_code Abi.Running));
  near "case_id" (patch_u32 staged Abi.Result_off.case_id 7L);
  near "n_expected" (patch_u16 staged Abi.Result_off.n_expected 1);
  near "expected[]" (patch_u32 staged Abi.Result_off.expected 1L);
  near "n_values" (patch_u16 staged Abi.Result_off.n_values 1);
  near "values[]" (patch_u32 staged Abi.Result_off.values 1L);
  near "diag_len" (patch_u32 staged Abi.Result_off.diag_len 1L);
  near "diag" (patch staged Abi.Result_off.diag "x");
  [%expect
    {|
    status       invalid: state: error_subcode 3 staged with runner_error = 0
    case_id      invalid: state: error_subcode 3 staged with runner_error = 0
    n_expected   invalid: state: error_subcode 3 staged with runner_error = 0
    expected[]   invalid: state: error_subcode 3 staged with runner_error = 0
    n_values     invalid: state: error_subcode 3 staged with runner_error = 0
    values[]     invalid: state: error_subcode 3 staged with runner_error = 0
    diag_len     invalid: state: error_subcode 3 staged with runner_error = 0
    diag         invalid: state: error_subcode 3 staged with runner_error = 0 |}]

(* The same treatment for pattern B. A corrupt field disqualifies it from the
   pending interpretation exactly as it does for pattern A, so instead of
   reporting the result unavailable the record is rejected outright - which is
   the point: corruption cannot buy itself the benefit of the doubt that an
   interrupted write gets. *)
let%expect_test "pattern B near-misses" =
  let staged =
    patch_u16 (patch_u32 bootstrap Abi.Result_off.record_state 1L) Abi.Result_off.error_subcode 3
  in
  show (patch_u16 staged Abi.Result_off.status (Abi.status_code Abi.Running));
  show (patch_u32 staged Abi.Result_off.case_id 7L);
  show (patch_u16 staged Abi.Result_off.n_values 1);
  [%expect
    {|
    invalid: state: error_subcode 3 staged with runner_error = 0
    invalid: state: error_subcode 3 staged with runner_error = 0
    invalid: state: error_subcode 3 staged with runner_error = 0 |}]

(* {1 Step 3 - canonical committed state} *)

let%expect_test "unknown enumerations" =
  show (patch_u32 bootstrap Abi.Result_off.record_state 4L);
  show (patch_u32 bootstrap Abi.Result_off.record_state 0xFFFFFFFFL);
  show (patch_u16 bootstrap Abi.Result_off.status 5);
  [%expect
    {|
    invalid: state: unknown record_state 4
    invalid: state: unknown record_state 4294967295
    invalid: state: unknown status 5 |}]

(* Class/subcode compatibility is validated here and only here - after the
   runner_error commit. Before it, step 2 has no class to check against. *)
let%expect_test "committed error schema" =
  let pre cls sub =
    Record.build ~record_state:Abi.Pre_run_error ~runner_error:(Abi.class_code cls)
      ~error_subcode:sub ()
  in
  show (pre Abi.Manifest 3);
  show (pre Abi.Environment 1);
  show (pre Abi.Manifest 200);
  show (pre Abi.Manifest 0);
  show (Record.build ~record_state:Abi.Pre_run_error ~runner_error:70 ~error_subcode:1 ());
  [%expect
    {|
    pre_run_error/not_started case_id=0 n_expected=0 expected=[] n_values=0 values=[] diag="" runner_error=manifest.bad_magic
    pre_run_error/not_started case_id=0 n_expected=0 expected=[] n_values=0 values=[] diag="" runner_error=environment.page_size_wrong
    invalid: state: subcode 200 is not defined for class manifest
    invalid: state: class manifest committed with subcode 0
    invalid: state: unknown runner_error class 70 |}]

(* Trusted fields are readable exactly when record_state >= 2 (§6). Below that
   the manifest was never trusted, so a case_id or expected[] value in the
   record is corruption, not information. *)
let%expect_test "trusted fields" =
  show
    (Record.build ~record_state:Abi.Validated ~status:Abi.Running ~case_id:8 ~expected:[| 42L |] ());
  show
    (Record.build ~record_state:Abi.Validated ~status:Abi.Running ~case_id ~expected:[| 42L; 1L |]
       ());
  show
    ( Record.build ~record_state:Abi.Validated ~status:Abi.Running ~case_id ~expected:[| 42L |]
        ~n_expected:1 ~values:[||] ()
    |> fun s -> patch_u32 s (Abi.Result_off.expected + 8) 1L );
  [%expect
    {|
    invalid: state: case_id 8, expected 7
    invalid: state: n_expected 2, expected 1
    invalid: state: expected[] tail beyond n_expected nonzero |}]

(* {1 Step 4 - the result-staging matrix}

   One case per cell. The three "required zero" cells are the reason the matrix
   is written out rather than summarized as "ignored whenever the status is not
   terminal": validated/not_started is exactly the state §15.1 uses to attribute
   a helper fault between the validated and running commits, and a summary rule
   would have excused its staging area from the zero requirement. *)

let%expect_test "matrix cells" =
  show bootstrap;
  show
    (Record.build ~record_state:Abi.Pre_run_error ~runner_error:(Abi.class_code Abi.Io)
       ~error_subcode:3 ());
  show (validated Abi.Not_started);
  show (validated Abi.Running);
  show (returned Abi.Running ~values:[||]);
  show (Record.build ~record_state:Abi.Validated ~status:Abi.Fault ~case_id ~expected:[| 42L |] ());
  show (returned Abi.Passed ~values:[| 42L |]);
  show (returned Abi.Failed ~values:[| 41L |]);
  [%expect
    {|
    bootstrap/not_started case_id=0 n_expected=0 expected=[] n_values=0 values=[] diag=""
    pre_run_error/not_started case_id=0 n_expected=0 expected=[] n_values=0 values=[] diag="" runner_error=io.input_open
    validated/not_started case_id=7 n_expected=1 expected=[42] n_values=0 values=[] diag=""
    validated/running case_id=7 n_expected=1 expected=[42] staging=ignored
    returned/running case_id=7 n_expected=1 expected=[42] staging=ignored
    validated/fault case_id=7 n_expected=1 expected=[42] n_values=0 values=[] diag=""
    returned/passed case_id=7 n_expected=1 expected=[42] n_values=1 values=[42] diag=""
    returned/failed case_id=7 n_expected=1 expected=[42] n_values=1 values=[41] diag="" |}]

let%expect_test "matrix non-cells" =
  show (Record.build ~status:Abi.Running ());
  show
    (Record.build ~record_state:Abi.Pre_run_error ~status:Abi.Running ~runner_error:65
       ~error_subcode:3 ());
  show (Record.build ~record_state:Abi.Validated ~status:Abi.Passed ~case_id ~expected:[| 42L |] ());
  show (Record.build ~record_state:Abi.Returned ~status:Abi.Fault ~case_id ~expected:[| 42L |] ());
  show
    (Record.build ~record_state:Abi.Returned ~status:Abi.Not_started ~case_id ~expected:[| 42L |] ());
  [%expect
    {|
    invalid: state: status running is not admitted by record_state bootstrap
    invalid: state: status running is not admitted by record_state pre_run_error
    invalid: state: status passed is not admitted by record_state validated
    invalid: state: status fault is not admitted by record_state returned
    invalid: state: status not_started is not admitted by record_state returned |}]

(* A fault records no observation. Requiring n_values = 1 here would let the
   initialized zero in values[0] masquerade as a real observation from a guest
   that never returned one - so the schema requires 0, and 1 is rejected. *)
let%expect_test "terminal fault schema" =
  let fault ?(n_values = 0) ?(values = [||]) ?diag () =
    Record.build ~record_state:Abi.Validated ~status:Abi.Fault ~case_id ~expected:[| 42L |] ~values
      ~n_values ?diag ()
  in
  show (fault ());
  show (fault ~diag:"guest trapped" ());
  show (fault ~n_values:1 ());
  show (fault ~values:[| 42L |] ~n_values:0 ());
  [%expect
    {|
    validated/fault case_id=7 n_expected=1 expected=[42] n_values=0 values=[] diag=""
    validated/fault case_id=7 n_expected=1 expected=[42] n_values=0 values=[] diag="guest trapped"
    invalid: staging: n_values 1, expected 0
    invalid: staging: actual slots beyond n_values nonzero |}]

let%expect_test "terminal value completeness" =
  show
    (Record.build ~record_state:Abi.Returned ~status:Abi.Passed ~case_id ~expected:[| 42L |]
       ~n_values:0 ());
  show
    (Record.build ~record_state:Abi.Returned ~status:Abi.Passed ~case_id ~expected:[| 42L |]
       ~values:[| 42L; 1L |] ~n_values:1 ());
  show (returned Abi.Passed ~values:[| 42L |] |> fun s -> patch_u32 s Abi.Result_off.diag_len 65L);
  show (returned Abi.Passed ~values:[| 42L |] |> fun s -> patch s (Abi.Result_off.diag + 10) "x");
  [%expect
    {|
    invalid: staging: n_values 0, expected 1
    invalid: staging: actual slots beyond n_values nonzero
    invalid: staging: diag_len 65 exceeds 64
    invalid: staging: diagnostic tail nonzero |}]

(* §16.4's "records interrupted on both sides of the step-14 n_values store" -
   the transition most likely to expose a staging/validation mismatch. Step 14
   writes the actual, then n_values, then diagnostics, then commits status; a
   timeout landing anywhere inside it leaves returned/running, whose staging is
   ignored. Both sides of the store must therefore validate, and neither may be
   read as a result. *)
let%expect_test "interrupted step 14" =
  show (returned Abi.Running ~values:[| 42L |] |> fun s -> patch_u16 s Abi.Result_off.n_values 0);
  show (returned Abi.Running ~values:[| 42L |] |> fun s -> patch_u16 s Abi.Result_off.n_values 1);
  show
    ( returned Abi.Running ~values:[| 42L |] |> fun s ->
      patch
        (patch_u32 (patch_u16 s Abi.Result_off.n_values 1) Abi.Result_off.diag_len 200L)
        Abi.Result_off.diag "partial" );
  [%expect
    {|
    returned/running case_id=7 n_expected=1 expected=[42] staging=ignored
    returned/running case_id=7 n_expected=1 expected=[42] staging=ignored
    returned/running case_id=7 n_expected=1 expected=[42] staging=ignored |}]

(* A late failure after the guest returned stays in returned with a committed
   runner_error and status still running (§7's last row); it does not revert to
   pre_run_error. *)
let%expect_test "late failure" =
  show
    (Record.build ~record_state:Abi.Returned ~status:Abi.Running ~case_id ~expected:[| 42L |]
       ~runner_error:(Abi.class_code Abi.Protocol) ~error_subcode:1 ~values:[| 42L |] ());
  show
    (Record.build ~record_state:Abi.Returned ~status:Abi.Passed ~case_id ~expected:[| 42L |]
       ~runner_error:(Abi.class_code Abi.Protocol) ~error_subcode:1 ~values:[| 42L |] ());
  [%expect
    {|
    returned/running case_id=7 n_expected=1 expected=[42] staging=ignored runner_error=protocol.stack_pointer_corrupt
    invalid: state: runner_error committed with a terminal status |}]

(* {1 The subcode table itself}

   §14.2 promises every subcode carries a case proving it is observable. That
   claim is about the helpers and lives in the QEMU conformance suite; what can
   be established without a helper is that the table this host validates against
   is the table the document froze - no duplicates within a class, no gaps that
   would suggest a renumbering, and a name for every code. *)

(* §6 says which statuses a record state admits; §15.2 step 4 gives each
   admissible pair a staging rule. Two separate normative tables over the same
   domain, and the validator consults both - so if they ever disagreed, one of
   the two rejections would become unreachable and the other would be doing all
   the work. This asserts they agree, which is what lets step 4's "not a cell of
   the matrix" branch stay as the enforcement point for a future divergence
   rather than being quietly redundant today. *)
let%expect_test "state tables agree" =
  let states = [ Abi.Bootstrap; Abi.Pre_run_error; Abi.Validated; Abi.Returned ] in
  let statuses = [ Abi.Not_started; Abi.Running; Abi.Passed; Abi.Failed; Abi.Fault ] in
  let cells = ref 0 in
  List.iter
    (fun st ->
      List.iter
        (fun s ->
          let admitted = Abi.status_admitted st s and staged = Record.staging_rule st s <> None in
          if admitted then incr cells;
          if admitted <> staged then
            Fmt.pr "disagree: %s/%s §6=%b step4=%b@." (Abi.record_state_name st) (Abi.status_name s)
              admitted staged)
        statuses)
    states;
  Fmt.pr "%d of %d pairs admitted; the two tables agree@." !cells
    (List.length states * List.length statuses);
  [%expect {| 8 of 20 pairs admitted; the two tables agree |}]

let%expect_test "subcode table" =
  List.iter
    (fun cls ->
      let subs = List.filter (fun s -> s.Abi.cls = cls) Abi.all_subcodes in
      let codes = List.map (fun s -> s.Abi.code) subs in
      let n = List.length codes in
      let contiguous = codes = List.init n (fun i -> i + 1) in
      Fmt.pr "%-12s %2d subcodes 1..%d contiguous=%b@." (Abi.class_name cls) n n contiguous)
    Abi.all_classes;
  [%expect
    {|
    manifest     42 subcodes 1..42 contiguous=true
    io            5 subcodes 1..5 contiguous=true
    mapping       7 subcodes 1..7 contiguous=true
    cache-sync    2 subcodes 1..2 contiguous=true
    protocol      2 subcodes 1..2 contiguous=true
    environment   2 subcodes 1..2 contiguous=true |}]
