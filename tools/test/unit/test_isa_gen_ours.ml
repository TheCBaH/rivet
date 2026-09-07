(* Isa_gen_ours.normalized_argv: pure,
   no toolchain and no dune-exec subprocess - the real tool/asm.exe invocation
   is make asm-isa-generated-regen's job, never tools-test/tools-integration's. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let case : Isa_generated_case.case =
  {
    case_id = "riscv:add:canonical:riscv32";
    target = Target.Riscv32;
    form_id = "riscv:add";
    source_record_ids = [ "rv_i:1" ];
    rule_ids = [ "canonical-spelling" ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("rs2", "a2") ];
    rendered_source = ".text\nadd a0, a1, a2\n";
    configuration = [ "-march=rv32im"; "-mabi=ilp32"; "-mno-relax" ];
    negative = false;
  }

let test_normalized_argv_uses_fixed_basename () =
  check "normalized_argv names the fixed case.s basename, not a scratch path"
    (Isa_gen_ours.normalized_argv case
    = [ "--target"; "riscv32"; "--fixed-base"; "0x0"; "--dump-bytes"; "case.s" ])

let test_normalized_argv_tracks_target () =
  check "normalized_argv's --target follows case.target"
    (Isa_gen_ours.normalized_argv { case with target = Target.X86_64 }
    = [ "--target"; "x86_64"; "--fixed-base"; "0x0"; "--dump-bytes"; "case.s" ])

let () =
  print_endline "isa-gen-ours:";
  test_normalized_argv_uses_fixed_basename ();
  test_normalized_argv_tracks_target ();
  if !failures > 0 then (
    Printf.printf "isa-gen-ours: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-gen-ours: all %d checks passed\n" !checks
