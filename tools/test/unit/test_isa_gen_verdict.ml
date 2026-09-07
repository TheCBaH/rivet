(* Isa_gen_verdict: pure, checked
   against the real bytes and diagnostics actually measured for
   all 21 pilot cases (asm-isa-generated-regen, run against the real
   cross binutils and this project's own tool/asm.exe). *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let test_gas_rejected () =
  check "Gas_only_rejected classifies as Gas_rejected_valid_case"
    (Isa_gen_verdict.classify ~known_syntax_gap:false
       (Isa_gen_verdict.Gas_only_rejected "no such instruction")
    = Isa_generated_case.Gas_rejected_valid_case)

let test_unexpected_relocation () =
  check "Gas_unexpected_relocation classifies as Byte_mismatch (the closest hard-failure row)"
    (Isa_gen_verdict.classify ~known_syntax_gap:false
       (Isa_gen_verdict.Gas_unexpected_relocation [ "RELOCATION RECORDS FOR [.text]:" ])
    = Isa_generated_case.Byte_mismatch)

let test_unexpected_relocation_ignores_known_syntax_gap () =
  check "Gas_unexpected_relocation is unaffected by known_syntax_gap either way"
    (Isa_gen_verdict.classify ~known_syntax_gap:true
       (Isa_gen_verdict.Gas_unexpected_relocation [ "RELOCATION RECORDS FOR [.text]:" ])
    = Isa_generated_case.Byte_mismatch)

(* riscv:add's real measured bytes on both profiles. *)
let test_matching_bytes_pass () =
  check "identical bytes on both sides pass"
    (Isa_gen_verdict.classify ~known_syntax_gap:false
       (Isa_gen_verdict.Both_ran
          { gas_hex = "33 85 c5 00\n"; ours = Isa_gen_verdict.Ours_assembled "33 85 c5 00\n" })
    = Isa_generated_case.Pass)

let test_case_insensitive_pass () =
  check "hex comparison folds case and trims whitespace"
    (Isa_gen_verdict.classify ~known_syntax_gap:false
       (Isa_gen_verdict.Both_ran
          { gas_hex = "01 D1\n"; ours = Isa_gen_verdict.Ours_assembled "01 d1" })
    = Isa_generated_case.Pass)

let test_mismatched_bytes_fails () =
  check "different bytes on the two sides is a hard Byte_mismatch"
    (Isa_gen_verdict.classify ~known_syntax_gap:false
       (Isa_gen_verdict.Both_ran
          { gas_hex = "33 85 c5 00\n"; ours = Isa_gen_verdict.Ours_assembled "33 85 c5 40\n" })
    = Isa_generated_case.Byte_mismatch)

let test_byte_mismatch_ignores_known_syntax_gap () =
  check "Byte_mismatch is unconditional - a byte disagreement is never excused by known_syntax_gap"
    (Isa_gen_verdict.classify ~known_syntax_gap:true
       (Isa_gen_verdict.Both_ran
          { gas_hex = "89 d1\n"; ours = Isa_gen_verdict.Ours_assembled "8b d1\n" })
    = Isa_generated_case.Byte_mismatch)

let test_ours_rejected_known_gap_is_frontier_gap () =
  check "ours rejects, known_syntax_gap true -> Frontier_gap"
    (Isa_gen_verdict.classify ~known_syntax_gap:true
       (Isa_gen_verdict.Both_ran
          {
            gas_hex = "89 d1\n";
            ours = Isa_gen_verdict.Ours_rejected "needs an operand-size suffix";
          })
    = Isa_generated_case.Frontier_gap)

let test_ours_rejected_unknown_is_regression () =
  check "ours rejects, no recorded reason -> Regression (the conservative default)"
    (Isa_gen_verdict.classify ~known_syntax_gap:false
       (Isa_gen_verdict.Both_ran
          { gas_hex = "33 85 c5 00\n"; ours = Isa_gen_verdict.Ours_rejected "unknown instruction" })
    = Isa_generated_case.Regression)

(* The eight real case_id/diagnostic pairs actually measured. *)
let measured_gap_cases =
  [
    ( "x86:ADD_GPRv_IMMz:canonical:x86_32",
      "/tmp/one.s:2:1: error[x86.simplify]: add needs an operand-size suffix (b, w, l or q)\n\
      \  add $1000000, %ecx\n\
      \  ^^^" );
    ( "x86:ADD_GPRv_IMMz:canonical:x86_64",
      "error[x86.simplify]: add needs an operand-size suffix (b, w, l or q)" );
    ( "x86:MOV_GPRv_GPRv_89:canonical:x86_32",
      "error[x86.simplify]: mov needs an operand-size suffix (b, w, l or q)" );
    ( "x86:MOV_GPRv_GPRv_89:canonical:x86_64",
      "error[x86.simplify]: mov needs an operand-size suffix (b, w, l or q)" );
    ( "x86:MOV_GPRv_GPRv_8B:canonical:x86_32",
      "error[x86.simplify]: mov needs an operand-size suffix (b, w, l or q)" );
    ( "x86:MOV_GPRv_GPRv_8B:canonical:x86_64",
      "error[x86.simplify]: mov needs an operand-size suffix (b, w, l or q)" );
    ( "x86:MOV_GPRv_IMMz:canonical:x86_32",
      "error[x86.simplify]: mov needs an operand-size suffix (b, w, l or q)" );
    ( "x86:MOV_GPRv_IMMz:canonical:x86_64",
      "error[x86.simplify]: mov needs an operand-size suffix (b, w, l or q)" );
  ]

let test_known_syntax_gap_matches_every_measured_case () =
  List.iter
    (fun (case_id, diagnostic) ->
      check
        (Printf.sprintf "known_syntax_gap recognizes the real measured %s" case_id)
        (Isa_gen_verdict.known_syntax_gap ~case_id ~diagnostic))
    measured_gap_cases

let test_known_syntax_gap_false_for_unlisted_case () =
  check "an unlisted case_id is never excused, even with the same diagnostic text"
    (not
       (Isa_gen_verdict.known_syntax_gap ~case_id:"x86:SOME_OTHER_FORM:canonical:x86_64"
          ~diagnostic:"error[x86.simplify]: mov needs an operand-size suffix (b, w, l or q)"))

let test_known_syntax_gap_false_for_different_diagnostic () =
  check
    "a listed case_id with an UNRELATED diagnostic is not silently absorbed into the same excuse"
    (not
       (Isa_gen_verdict.known_syntax_gap ~case_id:"x86:MOV_GPRv_IMMz:canonical:x86_64"
          ~diagnostic:"error[x86.simplify]: no form takes these operands"))

let () =
  print_endline "isa-gen-verdict:";
  test_gas_rejected ();
  test_unexpected_relocation ();
  test_unexpected_relocation_ignores_known_syntax_gap ();
  test_matching_bytes_pass ();
  test_case_insensitive_pass ();
  test_mismatched_bytes_fails ();
  test_byte_mismatch_ignores_known_syntax_gap ();
  test_ours_rejected_known_gap_is_frontier_gap ();
  test_ours_rejected_unknown_is_regression ();
  test_known_syntax_gap_matches_every_measured_case ();
  test_known_syntax_gap_false_for_unlisted_case ();
  test_known_syntax_gap_false_for_different_diagnostic ();
  if !failures > 0 then (
    Printf.printf "isa-gen-verdict: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-gen-verdict: all %d checks passed\n" !checks
