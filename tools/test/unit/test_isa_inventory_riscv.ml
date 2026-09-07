(* Isa_inventory_riscv's pure parts: parse_extension_file, applies_to and the
   two renderers. No filesystem or submodule dependency - parse_extension_file
   takes already-read text, exactly like the real caller in Isa_inventory_cmd
   does after Tool_fs.read. entries_for_target itself (the vendored-submodule
   traversal) is exercised indirectly by `make asm-ci`'s isa-inventory regen,
   not duplicated here. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let mnemonics entries = List.map (fun (e : Isa_inventory_riscv.entry) -> e.mnemonic) entries

let test_parse_basic () =
  let text = "# rv_i\n\nadd     rd rs1 rs2 ...\naddi    rd rs1 imm12 ...\n" in
  match Isa_inventory_riscv.parse_extension_file ~name:"rv_i" text with
  | Error _ -> check "parse_basic succeeds" false
  | Ok entries ->
      check "parse_basic: two entries" (List.length entries = 2);
      check "parse_basic: mnemonics" (mnemonics entries = [ "add"; "addi" ]);
      check "parse_basic: extension tagged"
        (List.for_all (fun (e : Isa_inventory_riscv.entry) -> e.extension = "rv_i") entries)

let test_parse_pseudo_op () =
  let text = "$pseudo_op rv_zbkb::pack     zext.h.rv32 rd rs1 ...\n" in
  match Isa_inventory_riscv.parse_extension_file ~name:"rv32_zbkb" text with
  | Error _ -> check "parse_pseudo_op succeeds" false
  | Ok entries ->
      check "parse_pseudo_op: one entry" (mnemonics entries = [ "zext.h.rv32" ]);
      check "parse_pseudo_op: keyed by the defining file, not the base extension"
        (match entries with [ e ] -> e.extension = "rv32_zbkb" | _ -> false)

let test_parse_import_skipped () =
  let text = "$import rv64_zbb::rolw\nrori    rd rs1 shamtw ...\n" in
  match Isa_inventory_riscv.parse_extension_file ~name:"rv32_zk" text with
  | Error _ -> check "parse_import_skipped succeeds" false
  | Ok entries ->
      check "parse_import_skipped: only the non-$import line" (mnemonics entries = [ "rori" ])

let test_parse_malformed_pseudo_op () =
  match Isa_inventory_riscv.parse_extension_file ~name:"rv_i" "$pseudo_op only_one_field\n" with
  | Error _ -> check "malformed $pseudo_op is rejected" true
  | Ok _ -> check "malformed $pseudo_op is rejected" false

let test_parse_comments_and_blanks () =
  let text = "# a comment\n\n   \nadd rd rs1 rs2\n" in
  match Isa_inventory_riscv.parse_extension_file ~name:"rv_i" text with
  | Error _ -> check "comments_and_blanks succeeds" false
  | Ok entries -> check "comments_and_blanks: only the real line" (mnemonics entries = [ "add" ])

let test_applies_to () =
  let open Isa_inventory_riscv in
  check "rv_i applies to riscv32" (applies_to Target.Riscv32 ~extension:"rv_i");
  check "rv_i applies to riscv64" (applies_to Target.Riscv64 ~extension:"rv_i");
  check "rv32_zbb applies to riscv32 only"
    (applies_to Target.Riscv32 ~extension:"rv32_zbb"
    && not (applies_to Target.Riscv64 ~extension:"rv32_zbb"));
  check "rv64_a applies to riscv64 only"
    (applies_to Target.Riscv64 ~extension:"rv64_a"
    && not (applies_to Target.Riscv32 ~extension:"rv64_a"));
  check "no riscv extension applies to a non-riscv target"
    (not (applies_to Target.X86_64 ~extension:"rv_i"))

(* The duplicate-form collapse test_parse_* above does not reach:
   entries_for_target's own sort_uniq is what dedupes rv_i's `jal rd, offset`
   against its one-operand pseudo-op alias of the same mnemonic - see the
   .mli. That path needs the real submodule tree, so it is covered by `make
   asm-ci`'s isa-inventory regen instead of re-mocked here. *)

let test_render_manifest_and_summary () =
  let entries =
    [
      { Isa_inventory_riscv.mnemonic = "addi"; extension = "rv_i" };
      { Isa_inventory_riscv.mnemonic = "add"; extension = "rv_i" };
    ]
  in
  let manifest =
    Isa_inventory_riscv.render_manifest ~source_commit:"deadbeef" Target.Riscv32 entries
  in
  check "manifest carries the header"
    (String.length manifest > 0
    &&
    let lines = String.split_on_char '\n' manifest in
    List.nth_opt lines 0 = Some "schema-version:1"
    && List.nth_opt lines 1 = Some "target:riscv32"
    && List.nth_opt lines 3 = Some "source-commit:deadbeef");
  check "manifest carries every entry, unsorted (the caller sorts)"
    (let lines = String.split_on_char '\n' manifest in
     List.exists (fun l -> String.length l >= 13 && String.sub l 0 13 = "mnemonic:addi") lines
     && List.exists (fun l -> String.length l >= 12 && String.sub l 0 12 = "mnemonic:add") lines);
  let summary = Isa_inventory_riscv.render_summary Target.Riscv32 entries in
  check "summary total" (List.mem "total:2" (String.split_on_char '\n' summary));
  check "summary by-extension" (List.mem "by-extension:rv_i:2" (String.split_on_char '\n' summary))

let () =
  print_endline "isa-inventory-riscv:";
  test_parse_basic ();
  test_parse_pseudo_op ();
  test_parse_import_skipped ();
  test_parse_malformed_pseudo_op ();
  test_parse_comments_and_blanks ();
  test_applies_to ();
  test_render_manifest_and_summary ();
  if !failures > 0 then (
    Printf.printf "isa-inventory-riscv: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-inventory-riscv: all %d checks passed\n" !checks
