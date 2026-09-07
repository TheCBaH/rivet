(* Isa_source_snapshot_diff: the
   pure diff against synthetic (record_id, raw_line) pairs, plus load/
   diff_files against real temporary files for the duplicate-id rejection
   and decode-error paths, which only arise from real file content. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let is_err = function Ok _ -> false | Error _ -> true

let minimal_line ~record_id ~native_name =
  Printf.sprintf
    {|{"record_id":"%s","source":"riscv_opcodes","kind":"instruction-form","native_name":"%s","snapshot":"riscv_opcodes@abc"}|}
    record_id native_name

let test_diff_pure () =
  let old_ =
    [
      ("a", minimal_line ~record_id:"a" ~native_name:"add");
      ("b", minimal_line ~record_id:"b" ~native_name:"sub");
      ("d", minimal_line ~record_id:"d" ~native_name:"div");
    ]
  in
  let new_ =
    [
      ("a", minimal_line ~record_id:"a" ~native_name:"add");
      (* unchanged *)
      ("b", minimal_line ~record_id:"b" ~native_name:"subw");
      (* changed: same id, different content *)
      ("c", minimal_line ~record_id:"c" ~native_name:"mul");
      (* added *)
    ]
  in
  let r = Isa_source_snapshot_diff.diff ~old_ ~new_ in
  check "diff: 4 total ids" (List.length r.entries = 4);
  check "diff: 1 added" (r.added = 1);
  check "diff: 1 removed" (r.removed = 1);
  check "diff: 1 unchanged" (r.unchanged = 1);
  check "diff: 1 changed" (r.changed = 1);
  let find id =
    List.find (fun (e : Isa_source_snapshot_diff.entry) -> e.record_id = id) r.entries
  in
  (match find "a" with
  | { status = Unchanged; old_fingerprint = Some o; new_fingerprint = Some n; _ } ->
      check "a: unchanged, fingerprints present and equal" (String.equal o n)
  | _ -> check "a: unchanged, fingerprints present and equal" false);
  (match find "b" with
  | { status = Changed; old_fingerprint = Some o; new_fingerprint = Some n; _ } ->
      check "b: changed, fingerprints present and different" (not (String.equal o n))
  | _ -> check "b: changed, fingerprints present and different" false);
  (match find "c" with
  | { status = Added; old_fingerprint = None; new_fingerprint = Some _; _ } ->
      check "c: added, no old fingerprint" true
  | _ -> check "c: added, no old fingerprint" false);
  match find "d" with
  | { status = Removed; old_fingerprint = Some _; new_fingerprint = None; _ } ->
      check "d: removed, no new fingerprint" true
  | _ -> check "d: removed, no new fingerprint" false

let with_tmp_file lines f =
  let path = Filename.temp_file "isa_snapshot_diff" ".jsonl" in
  let oc = open_out path in
  List.iter
    (fun line ->
      output_string oc line;
      output_char oc '\n')
    lines;
  close_out oc;
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () -> f (Fpath.v path))

let test_load_and_diff_files () =
  let lines =
    [
      minimal_line ~record_id:"a" ~native_name:"add"; minimal_line ~record_id:"b" ~native_name:"sub";
    ]
  in
  with_tmp_file lines (fun path ->
      match Isa_source_snapshot_diff.load path with
      | Error _ -> check "load: two well-formed lines" false
      | Ok pairs -> (
          check "load: two well-formed lines" (List.length pairs = 2);
          match Isa_source_snapshot_diff.diff_files path path with
          | Error _ -> check "diff_files: a file against itself" false
          | Ok r ->
              check "diff_files: a file against itself has no drift"
                (r.added = 0 && r.removed = 0 && r.changed = 0);
              check "diff_files: a file against itself: every id unchanged" (r.unchanged = 2)))

let test_load_rejects_duplicate_record_id () =
  let lines =
    [
      minimal_line ~record_id:"a" ~native_name:"add";
      minimal_line ~record_id:"a" ~native_name:"addi";
    ]
  in
  with_tmp_file lines (fun path ->
      check "load rejects a duplicate record_id" (is_err (Isa_source_snapshot_diff.load path)))

let test_load_rejects_bad_line () =
  with_tmp_file [ "not json" ] (fun path ->
      check "load rejects an undecodable line" (is_err (Isa_source_snapshot_diff.load path)))

let test_report_lines_omit_unchanged () =
  let old_ = [ ("a", minimal_line ~record_id:"a" ~native_name:"add") ] in
  let new_ =
    [
      ("a", minimal_line ~record_id:"a" ~native_name:"add");
      ("b", minimal_line ~record_id:"b" ~native_name:"sub");
    ]
  in
  let r = Isa_source_snapshot_diff.diff ~old_ ~new_ in
  let lines = Isa_source_snapshot_diff.report_lines ~label:"synthetic" r in
  check "report_lines: header plus exactly the one non-unchanged entry" (List.length lines = 2);
  check "report_lines: the added id appears"
    (List.exists
       (fun l -> String.length l > 0 && String.equal (String.sub l 0 2) "  ")
       (List.tl lines))

let () =
  print_endline "isa-source-snapshot-diff:";
  test_diff_pure ();
  test_load_and_diff_files ();
  test_load_rejects_duplicate_record_id ();
  test_load_rejects_bad_line ();
  test_report_lines_omit_unchanged ();
  if !failures > 0 then (
    Printf.printf "isa-source-snapshot-diff: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-source-snapshot-diff: all %d checks passed\n" !checks
