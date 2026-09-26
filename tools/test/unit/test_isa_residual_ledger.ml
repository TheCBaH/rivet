(* Isa_residual_ledger: the ownership audit, on synthetic families so each failure class is
   provoked deliberately. The real ledger against the real matrix is checked by the repository
   test; this pins what the audit means. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let row id source families : Isa_residual_ledger.row =
  { id; source; families; capability = "c"; evidence = "e"; task = "T"; reopening_gate = "g" }

let cell ?(target = Target.Riscv64) ?(blocked = 1) source family : Isa_residual_ledger.cell =
  {
    source;
    family;
    target;
    total = 4;
    normalized_only = 0;
    gas_generatable = 0;
    promoted = 4 - blocked;
    oracle_unavailable = 0;
    blocked;
  }

let audit rows cells = Isa_residual_ledger.audit rows cells

let test_clean () =
  let a = audit [ row "R1" "s" [ "a"; "b" ] ] [ cell "s" "a"; cell "s" "b" ] in
  check "clean: every blocked family owned once" (Isa_residual_ledger.is_clean a);
  check "clean: no problems reported" (Isa_residual_ledger.problems a = [])

let test_unowned () =
  let a = audit [ row "R1" "s" [ "a" ] ] [ cell "s" "a"; cell "s" "orphan" ] in
  check "unowned: a blocked family with no row is reported" (a.unowned = [ ("s", "orphan") ]);
  check "unowned: not clean" (not (Isa_residual_ledger.is_clean a))

let test_source_scoping () =
  (* The same family name in another source is a different family. *)
  let a = audit [ row "R1" "s1" [ "a" ] ] [ cell "s1" "a"; cell "s2" "a" ] in
  check "scoping: a row owns only its own source's family" (a.unowned = [ ("s2", "a") ])

let test_ambiguous () =
  let a = audit [ row "R1" "s" [ "a" ]; row "R2" "s" [ "a" ] ] [ cell "s" "a" ] in
  check "ambiguous: two rows claiming one family are named"
    (a.ambiguous = [ ("s", "a", [ "R1"; "R2" ]) ])

let test_stale () =
  let rows = [ row "R1" "s" [ "a"; "closed" ]; row "R2" "s" [ "gone" ] ] in
  let a = audit rows [ cell "s" "a"; cell ~blocked:0 "s" "closed" ] in
  check "stale: a row naming a fully admitted family is reported"
    (List.mem ("R1", "closed") a.stale_names);
  check "stale: a row naming an absent family is reported" (List.mem ("R2", "gone") a.stale_names);
  check "stale: a row that owns nothing blocked is empty" (a.empty_rows = [ "R2" ])

let test_blocked_in_any_profile () =
  (* Blocked in one profile only still counts as blocked. *)
  let cells =
    [
      cell ~target:Target.Riscv32 ~blocked:0 "s" "a"; cell ~target:Target.Riscv64 ~blocked:2 "s" "a";
    ]
  in
  check "profiles: blocked in one profile needs an owner" ((audit [] cells).unowned = [ ("s", "a") ]);
  check "profiles: and is owned once a row lists it"
    (Isa_residual_ledger.is_clean (audit [ row "R1" "s" [ "a" ] ] cells))

let test_problem_text () =
  let a = audit [ row "R1" "s" [ "a" ] ] [ cell "s" "orphan" ] in
  let text = String.concat "\n" (Isa_residual_ledger.problems a) in
  let contains needle =
    let n = String.length needle and h = String.length text in
    let rec go i = i + n <= h && (String.sub text i n = needle || go (i + 1)) in
    go 0
  in
  check "problems: an unowned family is named" (contains "orphan");
  check "problems: an empty row is named" (contains "R1")

let test_real_rows_wellformed () =
  let rows = Isa_residual_ledger.rows in
  let nonempty (s : string) = String.length (String.trim s) > 0 in
  check "rows: every row states capability, evidence, task and gate"
    (List.for_all
       (fun (r : Isa_residual_ledger.row) ->
         nonempty r.capability && nonempty r.evidence && nonempty r.task
         && nonempty r.reopening_gate)
       rows);
  let ids = List.map (fun (r : Isa_residual_ledger.row) -> r.id) rows in
  check "rows: ids are unique" (List.length (List.sort_uniq String.compare ids) = List.length ids);
  check "rows: every row owns at least one family"
    (List.for_all (fun (r : Isa_residual_ledger.row) -> r.families <> []) rows)

let () =
  print_endline "isa-residual-ledger:";
  test_clean ();
  test_unowned ();
  test_source_scoping ();
  test_ambiguous ();
  test_stale ();
  test_blocked_in_any_profile ();
  test_problem_text ();
  test_real_rows_wellformed ();
  if !failures > 0 then (
    Printf.printf "isa-residual-ledger: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-residual-ledger: all %d checks passed\n" !checks
