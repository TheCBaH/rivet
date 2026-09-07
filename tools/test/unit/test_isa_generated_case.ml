(* Isa_generated_case: the frozen
   case/artifact/verdict schema and tier names are a design freeze, not
   generation logic - these checks pin the exact frozen strings (so a rename
   is a deliberate, visible diff) and prove the type shapes actually compose
   into an observation for a representative of every verdict row. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let check_eq name ~expected ~actual =
  if String.equal expected actual then (
    incr checks;
    Printf.printf "  ok   %s\n" name)
  else (
    incr checks;
    Printf.printf "  FAIL %s\n    expected: %S\n    actual:   %S\n" name expected actual;
    incr failures)

let check_none name = function
  | None ->
      incr checks;
      Printf.printf "  ok   %s\n" name
  | Some _ ->
      incr checks;
      Printf.printf "  FAIL %s\n" name;
      incr failures

let test_frozen_names () =
  check_eq "fixture_dir_name" ~expected:"isa-generated" ~actual:Isa_generated_case.fixture_dir_name;
  check_eq "cli_group_name" ~expected:"isa-generated" ~actual:Isa_generated_case.cli_group_name;
  check_eq "make_target Offline_consumer" ~expected:"asm-isa-generated-check"
    ~actual:(Option.get (Isa_generated_case.make_target Offline_consumer));
  check_eq "make_target Gnu_regeneration" ~expected:"asm-isa-generated-regen"
    ~actual:(Option.get (Isa_generated_case.make_target Gnu_regeneration));
  check_none "make_target Producer_update" (Isa_generated_case.make_target Producer_update);
  check_eq "joins_prerequisite_of Offline_consumer" ~expected:"asm-test"
    ~actual:(Option.get (Isa_generated_case.joins_prerequisite_of Offline_consumer));
  check_none "joins_prerequisite_of Gnu_regeneration"
    (Isa_generated_case.joins_prerequisite_of Gnu_regeneration);
  check_none "joins_prerequisite_of Producer_update"
    (Isa_generated_case.joins_prerequisite_of Producer_update)

let all_verdicts : Isa_generated_case.verdict list =
  [
    Pass;
    Byte_mismatch;
    Regression;
    Frontier_gap;
    Gas_rejected_valid_case;
    Oracle_unavailable { probe = "gas-lacks-zcb" };
    Blocked_unknown_requirement { rule = "unmapped-extension" };
    Negative_case_accepted;
  ]

let test_verdict_descriptions_distinct_and_nonempty () =
  let descriptions = List.map Isa_generated_case.verdict_description all_verdicts in
  check "verdict_description: every row is non-empty"
    (List.for_all (fun s -> String.length s > 0) descriptions);
  check "verdict_description: 8 rows, all distinct"
    (List.length (List.sort_uniq String.compare descriptions) = List.length all_verdicts)

let sample_case ~negative : Isa_generated_case.case =
  {
    case_id = "riscv32:sw:imm-zero";
    target = Target.Riscv32;
    form_id = "riscv:sw:rv_i";
    source_record_ids = [ "riscv-opcodes:rv_i:sw@L20" ];
    rule_ids = [ "boundary-zero-immediate" ];
    operands = [ ("value", "a0"); ("base", "a1"); ("offset", "0") ];
    rendered_source = "sw a0, 0(a1)\n";
    configuration = [ "-march=rv32i"; "-mabi=ilp32"; "-mno-relax" ];
    negative;
  }

let sample_artifact ~tool_label ~bytes : Isa_generated_case.artifact =
  {
    tool_label;
    argv = [ "as"; "-march=rv32i"; "-mabi=ilp32"; "-mno-relax"; "-o"; "out.o"; "in.s" ];
    exit_status = Process_status.Exited 0;
    stdout = "";
    stderr = "";
    bytes;
    relocations = [];
  }

(* One representative observation per verdict row, checking that the
   gas/ours Some/None shape the .mli documents actually holds together. *)
let test_observations_per_verdict () =
  let pass : Isa_generated_case.observation =
    {
      case = sample_case ~negative:false;
      gas = Some (sample_artifact ~tool_label:"riscv32-gas-2.43.1" ~bytes:(Some "0000a023"));
      ours = Some (sample_artifact ~tool_label:"ours-dev" ~bytes:(Some "0000a023"));
      verdict = Pass;
    }
  in
  check "Pass: both artifacts present" (Option.is_some pass.gas && Option.is_some pass.ours);
  let byte_mismatch =
    {
      pass with
      verdict = Byte_mismatch;
      ours = Some (sample_artifact ~tool_label:"ours-dev" ~bytes:(Some "deadbeef"));
    }
  in
  check "Byte_mismatch: both artifacts present"
    (Option.is_some byte_mismatch.gas && Option.is_some byte_mismatch.ours);
  let regression =
    {
      pass with
      verdict = Regression;
      ours = Some (sample_artifact ~tool_label:"ours-dev" ~bytes:None);
    }
  in
  check "Regression: ours rejected (no bytes), gas still present"
    (regression.ours <> None && (Option.get regression.ours).bytes = None);
  let frontier : Isa_generated_case.observation =
    { case = sample_case ~negative:false; gas = pass.gas; ours = None; verdict = Frontier_gap }
  in
  check "Frontier_gap: ours never ran" (frontier.ours = None);
  let gas_rejected : Isa_generated_case.observation =
    {
      case = sample_case ~negative:false;
      gas = Some (sample_artifact ~tool_label:"riscv32-gas-2.43.1" ~bytes:None);
      ours = Some (sample_artifact ~tool_label:"ours-dev" ~bytes:(Some "0000a023"));
      verdict = Gas_rejected_valid_case;
    }
  in
  check "Gas_rejected_valid_case: gas rejected (no bytes)"
    ((Option.get gas_rejected.gas).bytes = None);
  let oracle_unavailable : Isa_generated_case.observation =
    {
      case = sample_case ~negative:false;
      gas = None;
      ours = None;
      verdict = Oracle_unavailable { probe = "gas-lacks-zcb" };
    }
  in
  check "Oracle_unavailable: gas never invoked" (oracle_unavailable.gas = None);
  let blocked : Isa_generated_case.observation =
    {
      case = sample_case ~negative:false;
      gas = None;
      ours = None;
      verdict = Blocked_unknown_requirement { rule = "unmapped-extension" };
    }
  in
  check "Blocked_unknown_requirement: neither tool ran" (blocked.gas = None && blocked.ours = None);
  let negative_accepted : Isa_generated_case.observation =
    {
      case = sample_case ~negative:true;
      gas = Some (sample_artifact ~tool_label:"riscv32-gas-2.43.1" ~bytes:(Some "0000a023"));
      ours = Some (sample_artifact ~tool_label:"ours-dev" ~bytes:(Some "0000a023"));
      verdict = Negative_case_accepted;
    }
  in
  check "Negative_case_accepted: the case itself is marked negative" negative_accepted.case.negative

let () =
  print_endline "isa-generated-case:";
  test_frozen_names ();
  test_verdict_descriptions_distinct_and_nonempty ();
  test_observations_per_verdict ();
  if !failures > 0 then (
    Printf.printf "isa-generated-case: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-generated-case: all %d checks passed\n" !checks
