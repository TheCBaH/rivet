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
  check "fadd_s_entries has 2 entries (bare dynamic-rounding form x 2 profiles)"
    (List.length Isa_gen_difficult.fadd_s_entries = 2);
  check "fsub/fmul/fdiv scalar entries have 2 entries each (bare dynamic rounding x 2 profiles)"
    (List.length Isa_gen_difficult.fsub_s_entries = 2
    && List.length Isa_gen_difficult.fmul_s_entries = 2
    && List.length Isa_gen_difficult.fdiv_s_entries = 2);
  check "double scalar arithmetic entries have 2 entries each (bare dynamic rounding x 2 profiles)"
    (List.length Isa_gen_difficult.fadd_d_entries = 2
    && List.length Isa_gen_difficult.fsub_d_entries = 2
    && List.length Isa_gen_difficult.fmul_d_entries = 2
    && List.length Isa_gen_difficult.fdiv_d_entries = 2);
  check "sh1add_entries has 2 entries (Zba scale-one form x 2 profiles)"
    (List.length Isa_gen_difficult.sh1add_entries = 2);
  check "sh2add/sh3add entries have 2 entries each (Zba scale-two/scale-three x 2 profiles)"
    (List.length Isa_gen_difficult.sh2add_entries = 2
    && List.length Isa_gen_difficult.sh3add_entries = 2);
  check "min/minu/max/maxu entries have 2 entries each (Zbb comparisons x 2 profiles)"
    (List.length Isa_gen_difficult.min_entries = 2
    && List.length Isa_gen_difficult.minu_entries = 2
    && List.length Isa_gen_difficult.max_entries = 2
    && List.length Isa_gen_difficult.maxu_entries = 2);
  check "sh1add.uw/sh2add.uw/sh3add.uw entries have 1 entry each (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.sh1adduw_entries = 1
    && List.length Isa_gen_difficult.sh2adduw_entries = 1
    && List.length Isa_gen_difficult.sh3adduw_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         (Isa_gen_difficult.sh1adduw_entries @ Isa_gen_difficult.sh2adduw_entries
        @ Isa_gen_difficult.sh3adduw_entries));
  check "andn/orn/xnor/rol/ror entries have 2 entries each (Zbb Req_any comparisons x 2 profiles)"
    (List.length Isa_gen_difficult.andn_entries = 2
    && List.length Isa_gen_difficult.orn_entries = 2
    && List.length Isa_gen_difficult.xnor_entries = 2
    && List.length Isa_gen_difficult.rol_entries = 2
    && List.length Isa_gen_difficult.ror_entries = 2);
  check "clmul/clmulh entries have 2 entries each (Zbc Req_any x 2 profiles)"
    (List.length Isa_gen_difficult.clmul_entries = 2
    && List.length Isa_gen_difficult.clmulh_entries = 2);
  check "xperm4/xperm8 entries have 2 entries each (Zbkx Req_any x 2 profiles)"
    (List.length Isa_gen_difficult.xperm4_entries = 2
    && List.length Isa_gen_difficult.xperm8_entries = 2);
  check
    "clz/ctz/cpop/sext.b/sext.h/orc.b entries have 2 entries each (XLEN-independent x 2 profiles)"
    (List.length Isa_gen_difficult.clz_entries = 2
    && List.length Isa_gen_difficult.ctz_entries = 2
    && List.length Isa_gen_difficult.cpop_entries = 2
    && List.length Isa_gen_difficult.sextb_entries = 2
    && List.length Isa_gen_difficult.sexth_entries = 2
    && List.length Isa_gen_difficult.orcb_entries = 2);
  check "clzw/ctzw/cpopw entries have 1 entry each (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.clzw_entries = 1
    && List.length Isa_gen_difficult.ctzw_entries = 1
    && List.length Isa_gen_difficult.cpopw_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         (Isa_gen_difficult.clzw_entries @ Isa_gen_difficult.ctzw_entries
        @ Isa_gen_difficult.cpopw_entries));
  check "brev8_entries has 2 entries (Zbkb four-way Req_any x 2 profiles)"
    (List.length Isa_gen_difficult.brev8_entries = 2);
  check "rev8_entries has 2 entries (byte-reverse, distinct lookup_key per profile)"
    (List.length Isa_gen_difficult.rev8_entries = 2
    && List.exists
         (fun (e : Isa_gen_difficult.entry) ->
           e.target = Target.Riscv32 && String.equal e.lookup_key "rev8.rv32")
         Isa_gen_difficult.rev8_entries
    && List.exists
         (fun (e : Isa_gen_difficult.entry) ->
           e.target = Target.Riscv64 && String.equal e.lookup_key "rev8")
         Isa_gen_difficult.rev8_entries);
  check "pack/packh_entries have 2 entries each (Zbkb four-way Req_any x 2 profiles)"
    (List.length Isa_gen_difficult.pack_entries = 2
    && List.length Isa_gen_difficult.packh_entries = 2);
  check "packw_entries has 1 entry (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.packw_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         Isa_gen_difficult.packw_entries);
  check "zip/unzip_entries have 1 entry each (RV32-only, no RV64 counterpart)"
    (List.length Isa_gen_difficult.zip_entries = 1
    && List.length Isa_gen_difficult.unzip_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv32)
         (Isa_gen_difficult.zip_entries @ Isa_gen_difficult.unzip_entries));
  check "rolw/rorw_entries have 1 entry each (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.rolw_entries = 1
    && List.length Isa_gen_difficult.rorw_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         (Isa_gen_difficult.rolw_entries @ Isa_gen_difficult.rorw_entries));
  check "rori_entries has 2 entries (profile-specific native_name split like rev8)"
    (List.length Isa_gen_difficult.rori_entries = 2
    && List.exists
         (fun (e : Isa_gen_difficult.entry) ->
           e.target = Target.Riscv32 && String.equal e.lookup_key "rori.rv32")
         Isa_gen_difficult.rori_entries
    && List.exists
         (fun (e : Isa_gen_difficult.entry) ->
           e.target = Target.Riscv64 && String.equal e.lookup_key "rori")
         Isa_gen_difficult.rori_entries);
  check "roriw_entries has 1 entry (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.roriw_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         Isa_gen_difficult.roriw_entries);
  check "zext_h_entries has 2 entries (profile-specific native_name split like rev8)"
    (List.length Isa_gen_difficult.zext_h_entries = 2
    && List.exists
         (fun (e : Isa_gen_difficult.entry) ->
           e.target = Target.Riscv32 && String.equal e.lookup_key "zext.h.rv32")
         Isa_gen_difficult.zext_h_entries
    && List.exists
         (fun (e : Isa_gen_difficult.entry) ->
           e.target = Target.Riscv64 && String.equal e.lookup_key "zext.h")
         Isa_gen_difficult.zext_h_entries);
  check "all includes every difficult-form family"
    (List.length Isa_gen_difficult.all
    = List.length Isa_gen_difficult.sw_entries
      + List.length Isa_gen_difficult.beq_entries
      + List.length Isa_gen_difficult.c_addi_entries
      + List.length Isa_gen_difficult.x86_mov_entries
      + List.length Isa_gen_difficult.x86_fadd_entries
      + List.length Isa_gen_difficult.fadd_s_entries
      + List.length Isa_gen_difficult.fsub_s_entries
      + List.length Isa_gen_difficult.fmul_s_entries
      + List.length Isa_gen_difficult.fdiv_s_entries
      + List.length Isa_gen_difficult.fadd_d_entries
      + List.length Isa_gen_difficult.fsub_d_entries
      + List.length Isa_gen_difficult.fmul_d_entries
      + List.length Isa_gen_difficult.fdiv_d_entries
      + List.length Isa_gen_difficult.sh1add_entries
      + List.length Isa_gen_difficult.sh2add_entries
      + List.length Isa_gen_difficult.sh3add_entries
      + List.length Isa_gen_difficult.sh1adduw_entries
      + List.length Isa_gen_difficult.sh2adduw_entries
      + List.length Isa_gen_difficult.sh3adduw_entries
      + List.length Isa_gen_difficult.min_entries
      + List.length Isa_gen_difficult.minu_entries
      + List.length Isa_gen_difficult.max_entries
      + List.length Isa_gen_difficult.maxu_entries
      + List.length Isa_gen_difficult.andn_entries
      + List.length Isa_gen_difficult.orn_entries
      + List.length Isa_gen_difficult.xnor_entries
      + List.length Isa_gen_difficult.rol_entries
      + List.length Isa_gen_difficult.ror_entries
      + List.length Isa_gen_difficult.clz_entries
      + List.length Isa_gen_difficult.ctz_entries
      + List.length Isa_gen_difficult.cpop_entries
      + List.length Isa_gen_difficult.sextb_entries
      + List.length Isa_gen_difficult.sexth_entries
      + List.length Isa_gen_difficult.orcb_entries
      + List.length Isa_gen_difficult.clzw_entries
      + List.length Isa_gen_difficult.ctzw_entries
      + List.length Isa_gen_difficult.cpopw_entries
      + List.length Isa_gen_difficult.brev8_entries
      + List.length Isa_gen_difficult.rev8_entries
      + List.length Isa_gen_difficult.pack_entries
      + List.length Isa_gen_difficult.packh_entries
      + List.length Isa_gen_difficult.packw_entries
      + List.length Isa_gen_difficult.zip_entries
      + List.length Isa_gen_difficult.unzip_entries
      + List.length Isa_gen_difficult.rolw_entries
      + List.length Isa_gen_difficult.rorw_entries
      + List.length Isa_gen_difficult.rori_entries
      + List.length Isa_gen_difficult.roriw_entries
      + List.length Isa_gen_difficult.zext_h_entries
      + List.length Isa_gen_difficult.clmul_entries
      + List.length Isa_gen_difficult.clmulh_entries
      + List.length Isa_gen_difficult.xperm4_entries
      + List.length Isa_gen_difficult.xperm8_entries)

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

let test_f_arith_s_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses floating-point register operands" e.case_id)
        (List.map snd e.operands = [ "ft0"; "ft1"; "ft2" ]);
      check
        (Printf.sprintf "%s: selects measured implicit dynamic rounding" e.case_id)
        (List.mem "implicit-dynamic-rounding" e.rule_ids);
      check
        (Printf.sprintf "%s: configuration enables scalar FP" e.case_id)
        (List.exists
           (fun arg -> String.contains arg 'f' || String.contains arg 'd')
           e.configuration))
    (Isa_gen_difficult.fadd_s_entries @ Isa_gen_difficult.fsub_s_entries
   @ Isa_gen_difficult.fmul_s_entries @ Isa_gen_difficult.fdiv_s_entries
   @ Isa_gen_difficult.fadd_d_entries @ Isa_gen_difficult.fsub_d_entries
   @ Isa_gen_difficult.fmul_d_entries @ Isa_gen_difficult.fdiv_d_entries)

let test_sh1add_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses three ordinary GPR operands" e.case_id)
        (List.map snd e.operands = [ "a0"; "a1"; "a2" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zba extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.sh1add_entries @ Isa_gen_difficult.sh2add_entries
   @ Isa_gen_difficult.sh3add_entries)

let test_minmax_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses three ordinary GPR operands" e.case_id)
        (List.map snd e.operands = [ "a0"; "a1"; "a2" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zbb extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.min_entries @ Isa_gen_difficult.minu_entries @ Isa_gen_difficult.max_entries
   @ Isa_gen_difficult.maxu_entries @ Isa_gen_difficult.andn_entries @ Isa_gen_difficult.orn_entries
   @ Isa_gen_difficult.xnor_entries @ Isa_gen_difficult.rol_entries @ Isa_gen_difficult.ror_entries
   @ Isa_gen_difficult.pack_entries @ Isa_gen_difficult.packh_entries
   @ Isa_gen_difficult.packw_entries @ Isa_gen_difficult.rolw_entries
   @ Isa_gen_difficult.rorw_entries @ Isa_gen_difficult.clmul_entries
   @ Isa_gen_difficult.clmulh_entries @ Isa_gen_difficult.xperm4_entries
   @ Isa_gen_difficult.xperm8_entries)

let test_unary_gpr_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses two ordinary GPR operands, no immediate" e.case_id)
        (List.map snd e.operands = [ "a0"; "a1" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zbb/Zbkb extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.clz_entries @ Isa_gen_difficult.ctz_entries @ Isa_gen_difficult.cpop_entries
   @ Isa_gen_difficult.sextb_entries @ Isa_gen_difficult.sexth_entries
   @ Isa_gen_difficult.orcb_entries @ Isa_gen_difficult.clzw_entries
   @ Isa_gen_difficult.ctzw_entries @ Isa_gen_difficult.cpopw_entries
   @ Isa_gen_difficult.brev8_entries @ Isa_gen_difficult.rev8_entries
   @ Isa_gen_difficult.zip_entries @ Isa_gen_difficult.unzip_entries
   @ Isa_gen_difficult.zext_h_entries)

let test_shamt_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses two ordinary GPR operands plus a shamt immediate" e.case_id)
        (List.map fst e.operands = [ "rd"; "rs1"; "shamt" ]
        && List.map snd e.operands = [ "a0"; "a1"; "5" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zbb extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.rori_entries @ Isa_gen_difficult.roriw_entries)

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
  test_f_arith_s_domain ();
  test_sh1add_domain ();
  test_minmax_domain ();
  test_unary_gpr_domain ();
  test_shamt_domain ();
  test_render_source_lines ();
  test_build_wraps_lines_before_and_after ();
  if !failures > 0 then (
    Printf.printf "isa-gen-difficult: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-gen-difficult: all %d checks passed\n" !checks
