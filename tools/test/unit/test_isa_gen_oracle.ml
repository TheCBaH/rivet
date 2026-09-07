(* Isa_gen_oracle.observed_form_check:
   pure, no toolchain - the real assembler invocation is exercised only by
   repo_tests.ml's toolchain-required path (make asm-isa-generated-regen),
   never by tools-test/tools-integration. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let is_ok = function Ok () -> true | Error _ -> false
let is_err = function Ok () -> false | Error _ -> true

(* add's real mask/value (riscv-opcodes rv_i), little-endian bytes for
   0x00c58533 ("add a0, a1, a2" per the real measured
   bytes: 33 85 c5 00). *)
let add_encoding : Isa_norm_model.encoding =
  Riscv_encoding { width_bits = 32; mask = "0xfe00707f"; value = "0x33" }

let add_bytes_le = "\x33\x85\xc5\x00"

let test_riscv_match () =
  check "add's real bytes match its own mask/value"
    (is_ok (Isa_gen_oracle.observed_form_check add_encoding add_bytes_le))

let test_riscv_wrong_length () =
  check "wrong byte count is rejected"
    (is_err (Isa_gen_oracle.observed_form_check add_encoding "\x33\x85\xc5"))

let test_riscv_mismatch () =
  (* sub's real word (funct7 0x20) does not satisfy add's mask/value. *)
  check "sub's bytes do not match add's mask/value"
    (is_err (Isa_gen_oracle.observed_form_check add_encoding "\x33\x85\xc5\x40"))

let add_gprv_gprv_01 : Isa_norm_model.encoding =
  X86_encoding { space = "legacy"; opcode_map = 0; opcode = "0x01"; pattern = "" }

let test_x86_match () =
  check "0x01 leading byte matches ADD_GPRv_GPRv_01"
    (is_ok (Isa_gen_oracle.observed_form_check add_gprv_gprv_01 "\x01\xd1"))

let test_x86_mismatch () =
  check "0x03 leading byte does not match ADD_GPRv_GPRv_01 (the real DIFFERENT-FORM case)"
    (is_err (Isa_gen_oracle.observed_form_check add_gprv_gprv_01 "\x03\x0a"))

let test_x86_empty_bytes () =
  check "empty bytes never match" (is_err (Isa_gen_oracle.observed_form_check add_gprv_gprv_01 ""))

(* has_text_relocations/text_relocation_lines: real riscv32-linux-gnu-objdump
   -r output shapes, measured by hand (one with no relocations, `add a0, a1, a2`;
   one with a real R_RISCV_CALL_PLT relocation, `call foo` to a global symbol). *)

let no_relocations_output = "radd.o:     file format elf32-littleriscv\n\n"

let call_relocation_output =
  "rcall.o:     file format elf32-littleriscv\n\n\
   RELOCATION RECORDS FOR [.text]:\n\
   OFFSET   TYPE              VALUE\n\
   00000000 R_RISCV_CALL_PLT  foo\n\n\n"

let test_has_text_relocations_false_when_none () =
  check "no RELOCATION RECORDS banner means no .text relocations"
    (not (Isa_gen_oracle.has_text_relocations no_relocations_output))

let test_has_text_relocations_true_when_present () =
  check "a real RELOCATION RECORDS FOR [.text] banner is detected"
    (Isa_gen_oracle.has_text_relocations call_relocation_output)

let test_has_text_relocations_ignores_other_sections () =
  check "a relocation banner for a DIFFERENT section is not mistaken for .text"
    (not
       (Isa_gen_oracle.has_text_relocations
          "RELOCATION RECORDS FOR [.debug_info]:\nOFFSET TYPE VALUE\n00000000 R_X foo\n"))

let test_text_relocation_lines_nonempty_when_present () =
  check "text_relocation_lines captures the real entry line"
    (List.exists
       (fun l -> String.length l >= 2 && String.sub l 0 8 = "00000000")
       (Isa_gen_oracle.text_relocation_lines call_relocation_output))

let () =
  print_endline "isa-gen-oracle:";
  test_riscv_match ();
  test_riscv_wrong_length ();
  test_riscv_mismatch ();
  test_x86_match ();
  test_x86_mismatch ();
  test_x86_empty_bytes ();
  test_has_text_relocations_false_when_none ();
  test_has_text_relocations_true_when_present ();
  test_has_text_relocations_ignores_other_sections ();
  test_text_relocation_lines_nonempty_when_present ();
  if !failures > 0 then (
    Printf.printf "isa-gen-oracle: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-gen-oracle: all %d checks passed\n" !checks
