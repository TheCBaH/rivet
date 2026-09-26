(* Isa_generated_corpus: the toolchain-free "replay committed cases offline"
   corpus - JSONL
   round-trip for {!record} (mirroring Isa_norm_jsonl's own codec tests)
   plus {!Isa_generated_corpus.replay}'s pure recomputation, checked against
   synthetic records covering every {!gas_finding}/{!Isa_generated_case.verdict}
   combination reachable today and a deliberately broken one of each kind.
   Check_cmd.isa_generated_check's own real-corpus exercise is repo_tests.ml's
   job (needs the checked-in asm/fixtures/isa-generated/cases.jsonl), not this
   toolchain-free suite's. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let sample_case : Isa_generated_case.case =
  {
    case_id = "riscv:add:canonical:riscv32";
    target = Target.Riscv32;
    form_id = "riscv:add";
    source_record_ids = [ "riscv-opcodes:rv_i:add@L27" ];
    rule_ids = [ "canonical-spelling" ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("rs2", "a2") ];
    rendered_source = ".text\nadd a0, a1, a2\n";
    configuration = [ "-march=rv32im"; "-mabi=ilp32"; "-mno-relax" ];
    negative = false;
  }

let add_encoding : Isa_norm_model.encoding =
  Riscv_encoding { width_bits = 32; mask = "0xfe00707f"; value = "0x33" }

(* Synthetic, for frontier_gap_record's replay_gas_side check only - not a
   claim about MOV_GPRv_IMMz's real normalized encoding (which is opcode
   0xC7, per the real measured finding); this only has to
   agree with that record's own fabricated gas bytes ("b9 ..."). *)
let mov_r_imm_b9_encoding : Isa_norm_model.encoding =
  X86_encoding { space = "legacy"; opcode_map = 0; opcode = "0xb9"; pattern = "" }

let sample_artifact ~case ~exit_status ~bytes : Isa_generated_case.artifact =
  {
    tool_label = "riscv32-as-GNU assembler (crosstool-NG 1.27.0) 2.43.1";
    argv = Isa_gen_oracle.normalized_argv case;
    exit_status;
    stdout = "";
    stderr = "";
    bytes;
    relocations = [];
  }

let sample_ours_artifact ~case ~exit_status ~bytes ~stderr : Isa_generated_case.artifact =
  {
    tool_label = "ours-0000000000000000000000000000000000000000";
    argv = Isa_gen_ours.normalized_argv case;
    exit_status;
    stdout = Option.value bytes ~default:"";
    stderr;
    bytes;
    relocations = [];
  }

(* mismatched_record/rejected_record/signaled_record/frontier_gap_record/
   regression_record only borrow other pilot case_ids to keep every record's
   id distinct for write/load's uniqueness check - case/gas/finding otherwise
   stay sample_case's own "add" shape throughout, which is what the replay
   tests below check against add_encoding (Isa_gen_oracle's own observed-form
   check, orthogonal to Isa_gen_verdict's raw gas-vs-ours byte comparison -
   see Isa_gen_verdict.mli's own header on why the two are decoupled). *)
let matching_record : Isa_generated_corpus.record =
  {
    case = sample_case;
    gas =
      sample_artifact ~case:sample_case ~exit_status:(Process_status.Exited 0)
        ~bytes:(Some "33 85 c5 00\n");
    ours =
      Some
        (sample_ours_artifact ~case:sample_case ~exit_status:(Process_status.Exited 0)
           ~bytes:(Some "33 85 c5 00\n") ~stderr:"");
    finding = Matches_normalized_encoding;
    verdict = Pass;
  }

let mismatched_case = { sample_case with case_id = "riscv:sub:canonical:riscv32" }

let mismatched_record : Isa_generated_corpus.record =
  {
    case = mismatched_case;
    gas =
      sample_artifact ~case:mismatched_case ~exit_status:(Process_status.Exited 0)
        ~bytes:(Some "33 85 c5 40\n");
    (* "ours" agrees with GAS's own bytes here, even though neither matches
       add_encoding - Isa_gen_verdict.classify compares gas bytes against ours
       bytes directly, never against the normalized encoding, so this is a
       genuine Pass despite Isa_gen_oracle's own Different_observed_form
       finding on the SAME record. *)
    ours =
      Some
        (sample_ours_artifact ~case:mismatched_case ~exit_status:(Process_status.Exited 0)
           ~bytes:(Some "33 85 c5 40\n") ~stderr:"");
    finding =
      Different_observed_form
        { detail = "0x40c58533 & mask 0xfe00707f = 0x40000033, expected value 0x33" };
    verdict = Pass;
  }

let rejected_case = { sample_case with case_id = "riscv:mul:canonical:riscv32" }

let rejected_record : Isa_generated_corpus.record =
  {
    case = rejected_case;
    gas = sample_artifact ~case:rejected_case ~exit_status:(Process_status.Exited 1) ~bytes:None;
    ours = None;
    finding = Gas_rejected { diagnostic = "Error: unrecognized opcode" };
    verdict = Gas_rejected_valid_case;
  }

let signaled_record : Isa_generated_corpus.record =
  {
    rejected_record with
    case = { rejected_record.case with case_id = "riscv:addi:canonical:riscv32" };
    gas =
      {
        (sample_artifact ~case:rejected_record.case ~exit_status:(Process_status.signaled 6)
           ~bytes:None)
        with
        stdout = "Aborted";
      };
  }

(* A real measured finding: ours rejects with the recorded
   missing-size-suffix diagnostic for a case_id on Isa_gen_verdict's own
   allowlist. *)
let frontier_gap_case = { sample_case with case_id = "x86:MOV_GPRv_IMMz:canonical:x86_64" }

let frontier_gap_record : Isa_generated_corpus.record =
  {
    case = frontier_gap_case;
    gas =
      sample_artifact ~case:frontier_gap_case ~exit_status:(Process_status.Exited 0)
        ~bytes:(Some "b9 40 42 0f 00\n");
    ours =
      Some
        (sample_ours_artifact ~case:frontier_gap_case ~exit_status:(Process_status.Exited 1)
           ~bytes:None
           ~stderr:"error[x86.simplify]: mov needs an operand-size suffix (b, w, l or q)");
    finding = Matches_normalized_encoding;
    verdict = Frontier_gap;
  }

(* An ours-side rejection with NO recorded reason - never excused, always a
   Regression. Reuses "add"'s own real bytes/encoding (so replay_gas_side's
   Isa_gen_oracle.observed_form_check against add_encoding is genuinely
   consistent) under a borrowed case_id, same fiction as mismatched_record. *)
let regression_case = { sample_case with case_id = "riscv:sub:canonical:riscv64" }

let regression_record : Isa_generated_corpus.record =
  {
    case = regression_case;
    gas =
      sample_artifact ~case:regression_case ~exit_status:(Process_status.Exited 0)
        ~bytes:(Some "33 85 c5 00\n");
    ours =
      Some
        (sample_ours_artifact ~case:regression_case ~exit_status:(Process_status.Exited 1)
           ~bytes:None ~stderr:"Error: an unrelated, unreviewed failure");
    finding = Matches_normalized_encoding;
    verdict = Regression;
  }

(* A real measured control this task's own Isa_gen_oracle addition guards
   against: GAS accepted, but the object carries a .text relocation - "ours"
   is never invoked (Isa_gen_verdict.Gas_unexpected_relocation), and the
   committed bytes are an unresolved placeholder, not a real encoding. *)
let reloc_case = { sample_case with case_id = "riscv:mul:canonical:riscv64" }

let reloc_record : Isa_generated_corpus.record =
  {
    case = reloc_case;
    gas =
      sample_artifact ~case:reloc_case ~exit_status:(Process_status.Exited 0) ~bytes:(Some "00\n");
    ours = None;
    finding = Unexpected_relocation { relocations = [ "RELOCATION RECORDS FOR [.text]:" ] };
    verdict = Byte_mismatch;
  }

let all_records =
  [
    matching_record;
    mismatched_record;
    rejected_record;
    signaled_record;
    frontier_gap_record;
    regression_record;
    reloc_record;
  ]

let test_json_roundtrip () =
  List.iteri
    (fun i (r : Isa_generated_corpus.record) ->
      match Isa_generated_corpus.of_json (Isa_generated_corpus.to_json r) with
      | Ok r' -> check (Printf.sprintf "record %d: to_json/of_json round-trips" i) (r' = r)
      | Error msg ->
          check (Printf.sprintf "record %d: to_json/of_json round-trips (%s)" i msg) false)
    all_records

let test_encode_decode_line () =
  List.iteri
    (fun i (r : Isa_generated_corpus.record) ->
      match Isa_generated_corpus.encode_line r with
      | Error _ -> check (Printf.sprintf "record %d: encode_line succeeds" i) false
      | Ok line -> (
          match Isa_generated_corpus.decode_line line with
          | Error _ -> check (Printf.sprintf "record %d: decode_line succeeds" i) false
          | Ok r' ->
              check (Printf.sprintf "record %d: encode_line/decode_line round-trips" i) (r' = r)))
    all_records

let test_determinism () =
  match
    ( Isa_generated_corpus.encode_line matching_record,
      Isa_generated_corpus.encode_line matching_record )
  with
  | Ok a, Ok b -> check "encode_line: same record twice yields identical bytes" (String.equal a b)
  | _ -> check "encode_line: same record twice yields identical bytes" false

let replace_substring ~sub ~by s =
  let sub_len = String.length sub in
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i <= String.length s - sub_len do
    if String.sub s !i sub_len = sub then (
      Buffer.add_string buf by;
      i := !i + sub_len)
    else (
      Buffer.add_char buf s.[!i];
      incr i)
  done;
  Buffer.add_string buf (String.sub s !i (String.length s - !i));
  Buffer.contents buf

let test_schema_version_rejected () =
  match Isa_generated_corpus.encode_line matching_record with
  | Error _ -> check "schema_version mismatch is rejected" false
  | Ok line ->
      let bumped = replace_substring ~sub:"\"schema_version\":2" ~by:"\"schema_version\":3" line in
      check "schema_version mismatch is rejected"
        (Result.is_error (Isa_generated_corpus.decode_line bumped))

let with_tmp_path f =
  let path = Filename.temp_file "isa_generated_corpus" ".jsonl" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () -> f (Fpath.v path))

let test_write_load_roundtrip () =
  with_tmp_path (fun path ->
      match Isa_generated_corpus.write path all_records with
      | Error _ -> check "write/load: write succeeds" false
      | Ok () -> (
          match Isa_generated_corpus.load path with
          | Error _ -> check "write/load: load succeeds" false
          | Ok loaded ->
              check "write/load: same records back"
                (List.sort compare loaded = List.sort compare all_records)))

let test_write_is_sorted_by_case_id () =
  let distinct = { mismatched_record with case = { sample_case with case_id = "zzz" } } in
  with_tmp_path (fun path ->
      (* Deliberately reversed input order - write must sort by case_id
         regardless. *)
      match Isa_generated_corpus.write path [ distinct; matching_record ] with
      | Error _ -> check "write: sorts by case_id" false
      | Ok () -> (
          match Isa_generated_corpus.load path with
          | Error _ -> check "write: sorts by case_id" false
          | Ok [ first; second ] ->
              check "write: sorts by case_id"
                (String.equal first.case.case_id "riscv:add:canonical:riscv32"
                && String.equal second.case.case_id "zzz")
          | Ok _ -> check "write: sorts by case_id" false))

let test_load_rejects_duplicate_case_id () =
  with_tmp_path (fun path ->
      match Isa_generated_corpus.write path [ matching_record; matching_record ] with
      | Error _ -> check "load: rejects duplicate case_id (setup)" false
      | Ok () ->
          check "load: rejects duplicate case_id" (Result.is_error (Isa_generated_corpus.load path)))

let is_ok = function Ok () -> true | Error _ -> false
let is_err = function Ok () -> false | Error _ -> true

let test_replay_matching () =
  check "replay: a genuinely matching record replays clean"
    (is_ok (Isa_generated_corpus.replay matching_record add_encoding))

let test_replay_mismatched () =
  check "replay: a genuinely mismatched record with the exact recorded detail replays clean"
    (is_ok (Isa_generated_corpus.replay mismatched_record add_encoding))

let test_replay_rejected () =
  check "replay: a rejected record (no bytes) replays clean"
    (is_ok (Isa_generated_corpus.replay rejected_record add_encoding))

let test_replay_catches_stale_finding () =
  let stale = { matching_record with finding = Different_observed_form { detail = "stale" } } in
  check "replay: bytes now match but finding claims otherwise - caught"
    (is_err (Isa_generated_corpus.replay stale add_encoding))

let test_replay_catches_wrong_detail () =
  let wrong =
    { mismatched_record with finding = Different_observed_form { detail = "wrong detail" } }
  in
  check "replay: mismatch detail text drifted from what is recomputed - caught"
    (is_err (Isa_generated_corpus.replay wrong add_encoding))

let test_replay_catches_bytes_none_without_gas_rejected () =
  let bad = { rejected_record with finding = Matches_normalized_encoding } in
  check "replay: bytes=None but finding is not Gas_rejected - caught"
    (is_err (Isa_generated_corpus.replay bad add_encoding))

let test_replay_catches_argv_drift () =
  let drifted =
    { matching_record with gas = { matching_record.gas with argv = [ "-completely-different" ] } }
  in
  check "replay: recorded argv no longer matches Isa_gen_oracle.normalized_argv - caught"
    (is_err (Isa_generated_corpus.replay drifted add_encoding))

let test_replay_catches_undecodable_hex () =
  let bad = { matching_record with gas = { matching_record.gas with bytes = Some "not-hex" } } in
  check "replay: undecodable committed hex bytes - caught"
    (is_err (Isa_generated_corpus.replay bad add_encoding))

(* Replay also recomputes ours/verdict. *)

let test_replay_frontier_gap () =
  check "replay: a genuine Frontier_gap (known missing-suffix diagnostic) replays clean"
    (is_ok (Isa_generated_corpus.replay frontier_gap_record mov_r_imm_b9_encoding))

let test_replay_regression () =
  check "replay: a genuine Regression (unexplained ours rejection) replays clean"
    (is_ok (Isa_generated_corpus.replay regression_record add_encoding))

let test_replay_reloc () =
  check "replay: a genuine Unexpected_relocation (ours never invoked) replays clean"
    (is_ok (Isa_generated_corpus.replay reloc_record add_encoding))

let test_replay_catches_reloc_with_empty_evidence () =
  let bad = { reloc_record with finding = Unexpected_relocation { relocations = [] } } in
  check "replay: Unexpected_relocation with an empty relocations list - caught"
    (is_err (Isa_generated_corpus.replay bad add_encoding))

let test_replay_catches_reloc_with_ours_present () =
  let bad =
    {
      reloc_record with
      ours =
        Some
          (sample_ours_artifact ~case:reloc_record.case ~exit_status:(Process_status.Exited 0)
             ~bytes:(Some "00\n") ~stderr:"");
    }
  in
  check "replay: Unexpected_relocation but a committed ours artifact exists anyway - caught"
    (is_err (Isa_generated_corpus.replay bad add_encoding))

let test_replay_catches_ours_none_when_gas_accepted () =
  let bad = { matching_record with ours = None } in
  check "replay: gas.bytes is Some but ours is None - caught"
    (is_err (Isa_generated_corpus.replay bad add_encoding))

let test_replay_catches_ours_some_when_gas_rejected () =
  let bad =
    {
      rejected_record with
      ours =
        Some
          (sample_ours_artifact ~case:rejected_record.case ~exit_status:(Process_status.Exited 0)
             ~bytes:(Some "00\n") ~stderr:"");
    }
  in
  check "replay: gas.bytes is None but a committed ours artifact exists - caught"
    (is_err (Isa_generated_corpus.replay bad add_encoding))

let test_replay_catches_ours_argv_drift () =
  let drifted =
    {
      matching_record with
      ours =
        Option.map
          (fun (a : Isa_generated_case.artifact) -> { a with argv = [ "-completely-different" ] })
          matching_record.ours;
    }
  in
  check "replay: recorded ours.argv no longer matches Isa_gen_ours.normalized_argv - caught"
    (is_err (Isa_generated_corpus.replay drifted add_encoding))

let test_replay_catches_stale_verdict_pass_to_byte_mismatch () =
  let stale =
    {
      matching_record with
      ours =
        Option.map
          (fun (a : Isa_generated_case.artifact) -> { a with bytes = Some "ff ff ff ff\n" })
          matching_record.ours;
    }
  in
  check "replay: ours bytes now differ from gas bytes but verdict still claims Pass - caught"
    (is_err (Isa_generated_corpus.replay stale add_encoding))

let test_replay_catches_unexplained_rejection_marked_frontier_gap () =
  let mislabeled =
    {
      regression_record with
      ours =
        Option.map
          (fun (a : Isa_generated_case.artifact) ->
            { a with stderr = "Error: an unrelated, unreviewed failure" })
          regression_record.ours;
      verdict = Isa_generated_case.Frontier_gap;
    }
  in
  check
    "replay: an unexplained ours rejection committed as Frontier_gap instead of Regression - caught"
    (is_err (Isa_generated_corpus.replay mislabeled add_encoding))

(* Negative records: both artifacts committed, verdict and diagnostic category recomputed. *)
let negative_case : Isa_generated_case.case =
  {
    case_id = "negative:addi:imm12-out-of-range:riscv32";
    target = Target.Riscv32;
    form_id = "negative:addi";
    source_record_ids = [];
    rule_ids = [ "negative"; "category:immediate-range"; "expect-code:riscv32.fixup" ];
    operands = [];
    rendered_source = ".text\naddi a0, a1, 2048\n";
    configuration = [ "-march=rv32im"; "-mabi=ilp32"; "-mno-relax" ];
    negative = true;
  }

let negative_record ?(ours_bytes = None) ?(ours_stderr = "error[riscv32.fixup]: out of range")
    ?(gas_bytes = None) ?(verdict = Isa_generated_case.Pass) () : Isa_generated_corpus.record =
  {
    case = negative_case;
    gas =
      sample_artifact ~case:negative_case ~exit_status:(Process_status.Exited 1) ~bytes:gas_bytes;
    ours =
      Some
        (sample_ours_artifact ~case:negative_case ~exit_status:(Process_status.Exited 1)
           ~bytes:ours_bytes ~stderr:ours_stderr);
    finding =
      (match gas_bytes with
      | None -> Isa_generated_corpus.Gas_rejected { diagnostic = "illegal operands" }
      | Some _ -> Isa_generated_corpus.Matches_normalized_encoding);
    verdict;
  }

let dummy = Isa_gen_negative.dummy_encoding
let replay_ok r = Isa_generated_corpus.replay r dummy = Ok ()

let replay_error_mentions r needle =
  match Isa_generated_corpus.replay r dummy with
  | Ok () -> false
  | Error msg ->
      let n = String.length needle and h = String.length msg in
      let rec go i = i + n <= h && (String.sub msg i n = needle || go (i + 1)) in
      go 0

let test_replay_negative () =
  check "negative: both reject with the declared category replays" (replay_ok (negative_record ()));
  check "negative: an accepted negative (ours) is caught, not committed as a pass"
    (replay_error_mentions
       (negative_record ~ours_bytes:(Some "13 85 05 80\n") ())
       "does not match committed");
  check "negative: an accepted negative recorded honestly replays as its own failing verdict"
    (replay_ok
       (negative_record ~ours_bytes:(Some "13 85 05 80\n")
          ~verdict:Isa_generated_case.Negative_case_accepted ()));
  check "negative: GAS accepting the source is caught as a stale pass"
    (replay_error_mentions
       (negative_record ~gas_bytes:(Some "13 85 05 00\n") ())
       "does not match committed");
  check "negative: rejecting for the wrong reason (wrong category) is caught"
    (replay_error_mentions
       (negative_record ~ours_stderr:"error[riscv32.lower]: no addi form takes these operands" ())
       "riscv32.fixup");
  check "negative: a missing ours artifact is caught"
    (replay_error_mentions { (negative_record ()) with ours = None } "must commit an ours artifact");
  check "negative: gas argv drift is caught"
    (let r = negative_record () in
     replay_error_mentions { r with gas = { r.gas with argv = [ "x" ] } } "gas.argv");
  check "negative: a pass with no declared category is caught"
    (replay_error_mentions
       { (negative_record ()) with case = { negative_case with rule_ids = [ "negative" ] } }
       "expect-code");
  check "negative: is_hard_failure flags the accepted-negative verdict"
    (Isa_generated_case.is_hard_failure Isa_generated_case.Negative_case_accepted)

let test_ours_features_argv () =
  let with_features =
    { negative_case with rule_ids = "ours-features:none,+zmmul" :: negative_case.rule_ids }
  in
  check "ours: a plain case passes no --features"
    (not (List.mem "--features" (Isa_gen_ours.normalized_argv negative_case)));
  check "ours: an ours-features rule id becomes --features <spec>"
    (List.mem "--features" (Isa_gen_ours.normalized_argv with_features)
    && List.mem "none,+zmmul" (Isa_gen_ours.normalized_argv with_features));
  check "ours: features_of_case reads the spec"
    (Isa_gen_ours.features_of_case with_features = Some "none,+zmmul")

let test_finding_of_outcome () =
  check "finding_of_outcome: Assembled_matching"
    (Isa_generated_corpus.finding_of_outcome
       (Isa_gen_oracle.Assembled_matching { bytes_hex = "00" })
    = Matches_normalized_encoding);
  check "finding_of_outcome: Assembled_mismatched"
    (Isa_generated_corpus.finding_of_outcome
       (Isa_gen_oracle.Assembled_mismatched { bytes_hex = "00"; detail = "d" })
    = Different_observed_form { detail = "d" });
  check "finding_of_outcome: Rejected"
    (Isa_generated_corpus.finding_of_outcome (Isa_gen_oracle.Rejected "body")
    = Gas_rejected { diagnostic = "body" })

let test_finding_description_nonempty () =
  List.iter
    (fun (r : Isa_generated_corpus.record) ->
      check
        (Printf.sprintf "finding_description is non-empty for %s" r.case.case_id)
        (String.length (Isa_generated_corpus.finding_description r.finding) > 0))
    all_records

let () =
  print_endline "isa-generated-corpus:";
  test_json_roundtrip ();
  test_encode_decode_line ();
  test_determinism ();
  test_schema_version_rejected ();
  test_write_load_roundtrip ();
  test_write_is_sorted_by_case_id ();
  test_load_rejects_duplicate_case_id ();
  test_replay_matching ();
  test_replay_mismatched ();
  test_replay_rejected ();
  test_replay_catches_stale_finding ();
  test_replay_catches_wrong_detail ();
  test_replay_catches_bytes_none_without_gas_rejected ();
  test_replay_catches_argv_drift ();
  test_replay_catches_undecodable_hex ();
  test_replay_frontier_gap ();
  test_replay_regression ();
  test_replay_reloc ();
  test_replay_catches_reloc_with_empty_evidence ();
  test_replay_catches_reloc_with_ours_present ();
  test_replay_catches_ours_none_when_gas_accepted ();
  test_replay_catches_ours_some_when_gas_rejected ();
  test_replay_catches_ours_argv_drift ();
  test_replay_catches_stale_verdict_pass_to_byte_mismatch ();
  test_replay_catches_unexplained_rejection_marked_frontier_gap ();
  test_replay_negative ();
  test_ours_features_argv ();
  test_finding_of_outcome ();
  test_finding_description_nonempty ();
  if !failures > 0 then (
    Printf.printf "isa-generated-corpus: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-generated-corpus: all %d checks passed\n" !checks
