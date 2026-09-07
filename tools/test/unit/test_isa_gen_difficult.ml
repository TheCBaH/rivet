(* Isa_gen_difficult: the manifest's
   own shape - entry counts, legal offset/label domains, distinct case_ids,
   register choices, and render_source_lines wiring. Grounding every entry
   against real normalization output needs the checked-in exports and lives
   in repo_tests.ml instead, matching Isa_gen_pilot's own split
   (test_isa_gen_pilot.ml). *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let test_names () =
  check "fixture_dir_name is isa-difficult, distinct from isa-generated"
    (String.equal Isa_gen_difficult.fixture_dir_name "isa-difficult"
    && not (String.equal Isa_gen_difficult.fixture_dir_name Isa_generated_case.fixture_dir_name));
  check "cli_group_name is isa-difficult"
    (String.equal Isa_gen_difficult.cli_group_name "isa-difficult");
  check "check_make_target follows the asm-<thing>-check convention"
    (String.equal Isa_gen_difficult.check_make_target "asm-isa-difficult-check");
  check "regen_make_target follows the asm-<thing>-regen convention"
    (String.equal Isa_gen_difficult.regen_make_target "asm-isa-difficult-regen")

let test_counts () =
  check "sw_entries has 8 entries (4 offsets x 2 profiles)"
    (List.length Isa_gen_difficult.sw_entries = 8);
  check "beq_entries has 4 entries (2 directions x 2 profiles)"
    (List.length Isa_gen_difficult.beq_entries = 4);
  check "c_addi_entries has 8 entries (4 nzimm boundaries x 2 profiles)"
    (List.length Isa_gen_difficult.c_addi_entries = 8);
  check "x86_mov_entries has 4 entries (load/store x 2 modes)"
    (List.length Isa_gen_difficult.x86_mov_entries = 4);
  check "x86_fadd_entries has 2 entries (one x87 stack form x 2 modes)"
    (List.length Isa_gen_difficult.x86_fadd_entries = 2);
  check "all includes every difficult-form family"
    (List.length Isa_gen_difficult.all
    = List.length Isa_gen_difficult.sw_entries
      + List.length Isa_gen_difficult.beq_entries
      + List.length Isa_gen_difficult.c_addi_entries
      + List.length Isa_gen_difficult.x86_mov_entries
      + List.length Isa_gen_difficult.x86_fadd_entries)

let test_case_ids_distinct () =
  let ids = List.map (fun (e : Isa_gen_difficult.entry) -> e.case_id) Isa_gen_difficult.all in
  check "every case_id is unique" (List.length (List.sort_uniq String.compare ids) = List.length ids)

let test_sw_offsets_are_legal_s_type_domain () =
  let offset_of (e : Isa_gen_difficult.entry) = List.assoc "offset" e.operands in
  let offsets_for target =
    List.filter_map
      (fun (e : Isa_gen_difficult.entry) -> if e.target = target then Some (offset_of e) else None)
      Isa_gen_difficult.sw_entries
    |> List.sort_uniq String.compare
  in
  List.iter
    (fun target ->
      let offsets = offsets_for target in
      check
        (Printf.sprintf "%s sw offsets are exactly {-2048, -2048's boundary sibling, 0, 16, 2047}"
           (Target.to_string target))
        (List.equal String.equal offsets [ "-2048"; "0"; "16"; "2047" ]);
      List.iter
        (fun offset ->
          let n = int_of_string offset in
          check
            (Printf.sprintf "%s offset %s is within the signed 12-bit S-type domain"
               (Target.to_string target) offset)
            (n >= -2048 && n <= 2047))
        offsets)
    [ Target.Riscv32; Target.Riscv64 ]

let test_c_addi_nzimm_is_legal_nonzero_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      let imm = int_of_string (List.assoc "nzimm" e.operands) in
      check
        (Printf.sprintf "%s: nzimm %d is within the signed 6-bit domain" e.case_id imm)
        (imm >= -32 && imm <= 31);
      check (Printf.sprintf "%s: nzimm %d is nonzero" e.case_id imm) (imm <> 0);
      check
        (Printf.sprintf "%s: brackets the instruction in .option rvc/norvc" e.case_id)
        (e.lines_before = [ ".option rvc" ] && e.lines_after = [ ".option norvc" ]);
      check
        (Printf.sprintf "%s: configuration includes the c extension" e.case_id)
        (List.exists
           (fun arg ->
             String.length arg > 7
             && String.sub arg 0 7 = "-march="
             && String.ends_with ~suffix:"c" arg)
           e.configuration))
    Isa_gen_difficult.c_addi_entries

let test_beq_uses_local_labels_both_directions () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      let offset = List.assoc "offset" e.operands in
      check
        (Printf.sprintf "%s: offset is a local numeric label reference, not a raw immediate"
           e.case_id)
        (String.equal offset "1f" || String.equal offset "1b"))
    Isa_gen_difficult.beq_entries;
  check "beq_entries covers both the forward (1f) and backward (1b) direction"
    (List.exists
       (fun (e : Isa_gen_difficult.entry) -> List.assoc "offset" e.operands = "1f")
       Isa_gen_difficult.beq_entries
    && List.exists
         (fun (e : Isa_gen_difficult.entry) -> List.assoc "offset" e.operands = "1b")
         Isa_gen_difficult.beq_entries)

(* Mirrors Isa_gen_case_build's own "never x0/accumulator" check: a0/a1/a2 are
   plain GPRs with no special-cased RISC-V encoding, so a byte match actually
   exercises the general register field. *)
let test_no_entry_uses_x0 () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      List.iter
        (fun (_, v) ->
          check (Printf.sprintf "%s: operand %s is not x0" e.case_id v) (not (String.equal v "x0")))
        e.operands)
    Isa_gen_difficult.all

let test_x86_address_and_x87_domains () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      let mem = List.assoc "mem" e.operands in
      check
        (Printf.sprintf "%s: uses an explicit 32-bit width rule" e.case_id)
        (List.mem "explicit-32-bit-width" e.rule_ids);
      check
        (Printf.sprintf "%s: has AT&T memory syntax" e.case_id)
        (String.contains mem '(' && String.contains mem '%'))
    Isa_gen_difficult.x86_mov_entries;
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: binds source stack slot one" e.case_id)
        (List.assoc "src" e.operands = "1");
      check
        (Printf.sprintf "%s: identifies the implicit ST0 destination" e.case_id)
        (List.mem "implicit-st0-destination" e.rule_ids))
    Isa_gen_difficult.x86_fadd_entries

let test_render_source_lines () =
  check "render_source_lines matches render_source for a single line"
    (String.equal
       (Isa_gen_render.render_source_lines [ "add a0, a1, a2" ])
       (Isa_gen_render.render_source "add a0, a1, a2"));
  check "render_source_lines joins multiple lines with newlines under one .text"
    (String.equal
       (Isa_gen_render.render_source_lines [ "beq a0, a1, 1f"; "nop"; "1:" ])
       ".text\nbeq a0, a1, 1f\nnop\n1:\n")

(* Isa_gen_difficult.build itself - and grounding every entry's lookup_key
   against a real normalized form - needs the checked-in exports via a real
   Repo.t and lives in repo_tests.ml, matching Isa_gen_pilot's own split
   (test_isa_gen_pilot.ml's header comment above). This module only checks
   what render_source_lines produces from literal syntax_recipe/operand
   values, with no file access. *)
let test_build_wraps_lines_before_and_after () =
  let syntax =
    Isa_norm_model.
      {
        dialect = "gas-att";
        mnemonic = "beq";
        operands = [ Syn_operand "lhs"; Syn_operand "rhs"; Syn_operand "offset" ];
      }
  in
  match
    Isa_gen_render.render_line syntax ~operands:[ ("lhs", "a0"); ("rhs", "a1"); ("offset", "1f") ]
  with
  | Error msg -> check (Printf.sprintf "beq's syntax recipe renders (%s)" msg) false
  | Ok line ->
      check "a forward-branch entry's lines_before/lines_after wrap the instruction line"
        (String.equal
           (Isa_gen_render.render_source_lines ([] @ [ line ] @ [ "nop"; "1:" ]))
           ".text\nbeq a0, a1, 1f\nnop\n1:\n")

let () =
  print_endline "isa-gen-difficult:";
  test_names ();
  test_counts ();
  test_case_ids_distinct ();
  test_sw_offsets_are_legal_s_type_domain ();
  test_c_addi_nzimm_is_legal_nonzero_domain ();
  test_beq_uses_local_labels_both_directions ();
  test_no_entry_uses_x0 ();
  test_x86_address_and_x87_domains ();
  test_render_source_lines ();
  test_build_wraps_lines_before_and_after ();
  if !failures > 0 then (
    Printf.printf "isa-gen-difficult: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-gen-difficult: all %d checks passed\n" !checks
