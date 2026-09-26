(* Isa_gen_negative: the manifest's own well-formedness. Whether GAS and ours actually reject each
   case is the regenerated corpus's job (asm-isa-difficult-regen / -check); this pins what every
   entry must declare so a new one cannot be committed half-described. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let cases = List.map Isa_gen_negative.case_of Isa_gen_negative.all

let contains needle s =
  let n = String.length needle and h = String.length s in
  let rec go i = i + n <= h && (String.sub s i n = needle || go (i + 1)) in
  go 0

let () =
  print_endline "isa-gen-negative:";
  check "manifest is non-empty" (cases <> []);
  let ids = List.map (fun (c : Isa_generated_case.case) -> c.case_id) cases in
  check "case ids are unique" (List.length (List.sort_uniq String.compare ids) = List.length ids);
  check "every case is negative and starts with the negative: id prefix"
    (List.for_all
       (fun (c : Isa_generated_case.case) ->
         c.negative && String.length c.case_id > 9 && String.sub c.case_id 0 9 = "negative:")
       cases);
  check "every case declares its category and expected diagnostic code"
    (List.for_all
       (fun (c : Isa_generated_case.case) ->
         List.exists (fun r -> String.length r > 9 && String.sub r 0 9 = "category:") c.rule_ids
         && Isa_generated_corpus.expected_code c <> None)
       cases);
  check "every expected code is prefixed by its own target's diagnostic domain"
    (List.for_all
       (fun (e : Isa_gen_negative.entry) ->
         let domain =
           match e.target with
           | Target.X86_32 | Target.X86_64 -> "x86."
           | t -> Target.to_string t ^ "."
         in
         String.length e.expected_code > String.length domain
         && String.sub e.expected_code 0 (String.length domain) = domain)
       Isa_gen_negative.all);
  check "a feature-disabled case declares both GAS's -march and our --features"
    (List.for_all
       (fun (e : Isa_gen_negative.entry) ->
         (not (String.equal e.category "feature-disabled"))
         || e.ours_features <> None
            && List.exists (contains "-march=") e.gas_configuration
            && String.equal (List.nth (String.split_on_char '.' e.expected_code) 1) "feature")
       Isa_gen_negative.all);
  check "only a feature-disabled case is configured for our assembler"
    (List.for_all
       (fun (e : Isa_gen_negative.entry) ->
         e.ours_features = None || String.equal e.category "feature-disabled")
       Isa_gen_negative.all);
  check "every profile has at least one negative"
    (List.for_all
       (fun t ->
         List.exists (fun (e : Isa_gen_negative.entry) -> e.target = t) Isa_gen_negative.all)
       [ Target.Riscv32; Target.Riscv64; Target.X86_32; Target.X86_64 ]);
  if !failures > 0 then (
    Printf.printf "isa-gen-negative: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-gen-negative: all %d checks passed\n" !checks
