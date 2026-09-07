(* Isa_gen_pilot: the manifest's own
   shape - counts, the addw single-target XLEN restriction, and mandatory
   obligations naming real pilot form_ids. Grounding every entry against
   real normalization output needs the checked-in exports and lives in
   repo_tests.ml instead, matching every other normalization/generator suite's split. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let test_counts () =
  check
    "riscv_pilot has 9 entries (4 mnemonics x 2 profiles, minus addw's missing RV32 half, plus \
     addw)"
    (List.length Isa_gen_pilot.riscv_pilot = 9);
  check "x86_pilot has 12 entries (6 forms x 2 profiles)" (List.length Isa_gen_pilot.x86_pilot = 12);
  check "all is exactly riscv_pilot @ x86_pilot"
    (List.length Isa_gen_pilot.all
    = List.length Isa_gen_pilot.riscv_pilot + List.length Isa_gen_pilot.x86_pilot)

let test_addw_is_riscv64_only () =
  let addw_entries =
    List.filter
      (fun (e : Isa_gen_pilot.pilot_entry) -> String.equal e.lookup_key "addw")
      Isa_gen_pilot.riscv_pilot
  in
  check "addw appears exactly once" (List.length addw_entries = 1);
  match addw_entries with
  | [ { target = Target.Riscv64; _ } ] -> check "addw's one entry targets Riscv64" true
  | _ -> check "addw's one entry targets Riscv64" false

let test_form_ids_distinct_per_target () =
  let key (e : Isa_gen_pilot.pilot_entry) = (Target.to_string e.target, e.form_id) in
  let keys = List.map key Isa_gen_pilot.all in
  check "every (target, form_id) pair is unique"
    (List.length (List.sort_uniq compare keys) = List.length keys)

let test_obligations_name_real_pilot_form_ids () =
  let pilot_form_ids =
    List.sort_uniq String.compare
      (List.map (fun (e : Isa_gen_pilot.pilot_entry) -> e.form_id) Isa_gen_pilot.all)
  in
  List.iter
    (fun obligation ->
      let ids = Isa_gen_pilot.obligation_form_ids obligation in
      check "obligation names at least one form_id" (ids <> []);
      List.iter
        (fun id ->
          check
            (Printf.sprintf "obligation form_id %s is a real pilot entry" id)
            (List.mem id pilot_form_ids))
        ids)
    [
      Isa_gen_pilot.Negative_immediate_boundary;
      Isa_gen_pilot.Riscv_xlen_restriction;
      Isa_gen_pilot.X86_short_vs_full_immediate;
    ]

let () =
  print_endline "isa-gen-pilot:";
  test_counts ();
  test_addw_is_riscv64_only ();
  test_form_ids_distinct_per_target ();
  test_obligations_name_real_pilot_form_ids ();
  if !failures > 0 then (
    Printf.printf "isa-gen-pilot: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-gen-pilot: all %d checks passed\n" !checks
