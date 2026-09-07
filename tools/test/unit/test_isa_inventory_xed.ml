(* Isa_inventory_xed's pure parts: parse_datafile, merge_entries (through
   entries_for_target's own filtering logic reimplemented in miniature
   below) and the two renderers. No filesystem or submodule dependency -
   parse_datafile takes already-read text, exactly like the real caller in
   Isa_inventory_xed.entries_in_dir does after Tool_fs.read.

   entries_for_target itself (the vendored-submodule directory traversal)
   is exercised indirectly by `make asm-ci`'s isa-inventory regen, not
   duplicated here - see asm/docs/isa-inventory.md's "XED's data shape"
   section for the real data's own quirks these cases are drawn from. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let mnemonics entries = List.map (fun (e : Isa_inventory_xed.entry) -> e.mnemonic) entries

(* The "generated" dialect: one PATTERN per block, tight ICLASS gutter. *)
let test_parse_generated_dialect () =
  let text =
    "{\n\
     ICLASS:      VPDPBUSD\n\
     CATEGORY:    VEX\n\
     EXTENSION:   AVX_VNNI\n\
     PATTERN:    VV1 0x50 V66 V0F38 MOD[0b11] MOD=3\n\
     OPERANDS:    REG0=XMM_R():rw:dq:i32\n\
     }\n"
  in
  match Isa_inventory_xed.parse_datafile ~extension:"avx-vnni" text with
  | Error _ -> check "generated_dialect succeeds" false
  | Ok entries ->
      check "generated_dialect: one entry, lowercased" (mnemonics entries = [ "vpdpbusd" ]);
      check "generated_dialect: applies to both modes (no restriction token)"
        (match entries with [ e ] -> e.applies_32 && e.applies_64 | _ -> false)

(* The hand-authored dialect: several PATTERN/OPERANDS pairs under one
   ICLASS, wide gutter, blank lines inside the block. *)
let test_parse_hand_authored_dialect () =
  let text =
    "{\n\
     ICLASS    : VADDPD\n\
     EXCEPTIONS: avx-type-2\n\
     CATEGORY  : AVX\n\
     PATTERN : VV1 0x58  V66 VL128 V0F MOD[mm] MOD!=3\n\
     OPERANDS  : REG0=XMM_R():w:dq:f64 MEM0:r:dq:f64\n\n\
     PATTERN : VV1 0x58  V66 VL256 V0F MOD[0b11] MOD=3\n\
     OPERANDS  : REG0=YMM_R():w:qq:f64 REG2=YMM_B():r:qq:f64\n\
     }\n"
  in
  match Isa_inventory_xed.parse_datafile ~extension:"avx" text with
  | Error _ -> check "hand_authored_dialect succeeds" false
  | Ok entries ->
      check "hand_authored_dialect: one entry despite two PATTERNs"
        (mnemonics entries = [ "vaddpd" ])

let test_parse_not64_only () =
  let text = "{\nICLASS: SYSRET_AMD\nEXTENSION: BASE\nPATTERN: 0x0F 0x07 not64\n}\n" in
  match Isa_inventory_xed.parse_datafile ~extension:"amd" text with
  | Error _ -> check "not64_only succeeds" false
  | Ok entries ->
      check "not64_only: excluded from x86_64, kept for x86_32"
        (match entries with [ e ] -> e.applies_32 && not e.applies_64 | _ -> false)

let test_parse_mode64_only () =
  let text = "{\nICLASS: AADD\nEXTENSION: APX_F\nPATTERN: EVV 0x0FC 0x38 mode64\n}\n" in
  match Isa_inventory_xed.parse_datafile ~extension:"apx-f" text with
  | Error _ -> check "mode64_only succeeds" false
  | Ok entries ->
      check "mode64_only: excluded from x86_32, kept for x86_64"
        (match entries with [ e ] -> (not e.applies_32) && e.applies_64 | _ -> false)

(* PUSHA's real shape: two PATTERN lines, one mode16 and one mode32, neither
   tagged not64 or mode64. MODE is a three-way field (mode16/mode32/mode64),
   so a PATTERN restricted to mode16 or mode32 is just as excluded from
   x86_64 as one tagged not64 - missing this previously left PUSHA/POPA/BOUND
   in the x86_64 manifest (asm/docs/isa-inventory.md's mode-applicability
   correction). *)
let test_parse_mode16_and_mode32_only_excludes_64 () =
  let text =
    "{\n\
     ICLASS: PUSHA\n\
     EXTENSION: BASE\n\
     PATTERN: 0x60 mode16 no66_prefix\n\
     OPERANDS: REG0=XED_REG_AX:r:SUPP\n\n\
     PATTERN: 0x60 mode32 66_prefix\n\
     OPERANDS: REG0=XED_REG_AX:r:SUPP\n\
     }\n"
  in
  match Isa_inventory_xed.parse_datafile ~extension:"base" text with
  | Error _ -> check "mode16_and_mode32_only succeeds" false
  | Ok entries ->
      check "mode16_and_mode32_only: excluded from x86_64, kept for x86_32"
        (match entries with [ e ] -> e.applies_32 && not e.applies_64 | _ -> false)

(* monitorx's own real shape: two PATTERN lines, BOTH not64, but that must
   not be confused with "one 32-only, one 64-only" - the OR is across
   PATTERNs for the SAME mode exclusion, not one exclusion per PATTERN. *)
let test_parse_mixed_restrictions_same_direction () =
  let text =
    "{\n\
     ICLASS: MONITORX\n\
     EXTENSION: MONITORX\n\
     PATTERN: 0x0F 0x01 not64 eamode32\n\
     OPERANDS: REG0=XED_REG_EAX:r:SUPP\n\n\
     PATTERN: 0x0F 0x01 not64 eamode16\n\
     OPERANDS: REG0=XED_REG_EAX:r:SUPP\n\
     }\n"
  in
  match Isa_inventory_xed.parse_datafile ~extension:"monitorx" text with
  | Error _ -> check "mixed_restrictions succeeds" false
  | Ok entries ->
      check "mixed_restrictions: still not64-excluded from x86_64"
        (match entries with [ e ] -> e.applies_32 && not e.applies_64 | _ -> false)

let test_parse_no_pattern_defaults_open () =
  let text = "{\nICLASS: FOO\nEXTENSION: BASE\n}\n" in
  match Isa_inventory_xed.parse_datafile ~extension:"base" text with
  | Error _ -> check "no_pattern_defaults_open succeeds" false
  | Ok entries ->
      check "no_pattern_defaults_open: applies to both when there is no PATTERN at all"
        (match entries with [ e ] -> e.applies_32 && e.applies_64 | _ -> false)

let test_parse_block_without_iclass_is_skipped () =
  let text = "{\nCOMMENT: not an instruction\nPATTERN: 0x90\n}\n" in
  match Isa_inventory_xed.parse_datafile ~extension:"base" text with
  | Error _ -> check "block_without_iclass succeeds" false
  | Ok entries -> check "block_without_iclass: contributes nothing" (entries = [])

let test_parse_comments_and_blank_lines_between_blocks () =
  let text =
    "# EMITTING VPDPBUSD (VPDPBUSD-128-2)\n\
     AVX_INSTRUCTIONS()::\n\n\
     {\n\
     ICLASS: ADD\n\
     PATTERN: 0x00\n\
     }\n"
  in
  match Isa_inventory_xed.parse_datafile ~extension:"base" text with
  | Error _ -> check "comments_between_blocks succeeds" false
  | Ok entries ->
      check "comments_between_blocks: only the real block" (mnemonics entries = [ "add" ])

let test_parse_unterminated_block_is_rejected () =
  match Isa_inventory_xed.parse_datafile ~extension:"base" "{\nICLASS: ADD\n" with
  | Error _ -> check "unterminated block is rejected" true
  | Ok _ -> check "unterminated block is rejected" false

let test_render_manifest_and_summary () =
  let entries =
    [
      {
        Isa_inventory_xed.mnemonic = "vaddpd";
        extension = "avx";
        applies_32 = true;
        applies_64 = true;
      };
      {
        Isa_inventory_xed.mnemonic = "add";
        extension = "base";
        applies_32 = true;
        applies_64 = true;
      };
    ]
  in
  let manifest =
    Isa_inventory_xed.render_manifest ~source_commit:"deadbeef" Target.X86_64 entries
  in
  check "manifest carries the header"
    (let lines = String.split_on_char '\n' manifest in
     List.nth_opt lines 0 = Some "schema-version:1"
     && List.nth_opt lines 1 = Some "target:x86_64"
     && List.nth_opt lines 2 = Some "source:xed"
     && List.nth_opt lines 4 = Some "source-license:Apache-2.0");
  check "manifest carries every entry"
    (let lines = String.split_on_char '\n' manifest in
     List.exists (fun l -> String.length l >= 15 && String.sub l 0 15 = "mnemonic:vaddpd") lines);
  let summary = Isa_inventory_xed.render_summary Target.X86_64 entries in
  check "summary total" (List.mem "total:2" (String.split_on_char '\n' summary));
  check "summary by-extension" (List.mem "by-extension:avx:1" (String.split_on_char '\n' summary))

let () =
  print_endline "isa-inventory-xed:";
  test_parse_generated_dialect ();
  test_parse_hand_authored_dialect ();
  test_parse_not64_only ();
  test_parse_mode64_only ();
  test_parse_mode16_and_mode32_only_excludes_64 ();
  test_parse_mixed_restrictions_same_direction ();
  test_parse_no_pattern_defaults_open ();
  test_parse_block_without_iclass_is_skipped ();
  test_parse_comments_and_blank_lines_between_blocks ();
  test_parse_unterminated_block_is_rejected ();
  test_render_manifest_and_summary ();
  if !failures > 0 then (
    Printf.printf "isa-inventory-xed: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-inventory-xed: all %d checks passed\n" !checks
