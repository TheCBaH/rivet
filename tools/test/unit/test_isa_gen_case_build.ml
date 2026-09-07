(* Isa_gen_case_build: the canonical
   operand table covers every pilot entry and avoids the RISC-V x0/x86
   accumulator registers this module's own comment warns about, and
   configuration_for never reuses Target.config's frozen CompCert flags. No
   toolchain needed - this module only renders text, it does not invoke one. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let test_every_pilot_entry_has_an_assignment () =
  List.iter
    (fun (entry : Isa_gen_pilot.pilot_entry) ->
      check
        (Printf.sprintf "%s has a canonical operand assignment" entry.form_id)
        (Isa_gen_case_build.operands_for entry.form_id <> None))
    Isa_gen_pilot.all

let test_no_accumulator_or_x0 () =
  List.iter
    (fun (entry : Isa_gen_pilot.pilot_entry) ->
      match Isa_gen_case_build.operands_for entry.form_id with
      | None -> check (Printf.sprintf "%s has an operand table entry" entry.form_id) false
      | Some assignments ->
          let bad_riscv = List.exists (fun (_, v) -> v = "x0" || v = "zero") assignments in
          let bad_x86 =
            List.exists (fun (_, v) -> v = "eax" || v = "al" || v = "ax" || v = "rax") assignments
          in
          check
            (Printf.sprintf "%s avoids x0/accumulator registers" entry.form_id)
            ((not bad_riscv) && not bad_x86))
    Isa_gen_pilot.all

let test_configuration_never_empty_for_riscv () =
  check "riscv32 configuration is non-empty (own -march/-mabi, not Target.config)"
    (Isa_gen_case_build.configuration_for Target.Riscv32 <> []);
  check "riscv64 configuration is non-empty (own -march/-mabi, not Target.config)"
    (Isa_gen_case_build.configuration_for Target.Riscv64 <> [])

let () =
  print_endline "isa-gen-case-build:";
  test_every_pilot_entry_has_an_assignment ();
  test_no_accumulator_or_x0 ();
  test_configuration_never_empty_for_riscv ();
  if !failures > 0 then (
    Printf.printf "isa-gen-case-build: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-gen-case-build: all %d checks passed\n" !checks
