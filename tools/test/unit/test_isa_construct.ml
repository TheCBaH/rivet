(* Isa_construct: the construct vocabulary on synthetic records, and the refinement of the
   catch-all blocker against a measured known set. The real exports are exercised through the
   family-admission report by the repository test. *)

open Compcert_tools
module R = Isa_source_record

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let operand ?(visibility = "DEFAULT") ?lookup ?oc2 ?bits ?(op_type = "nt_lookup_fn") op_name :
    R.x86_operand =
  { op_name; op_type; lookupfn_name = lookup; oc2; bits; rw = "r"; visibility }

let x86 ?(space = "legacy") ?(opcode_map = 0) ~pattern operands : R.t =
  {
    record_id = "xed:test:0";
    source = "xed";
    kind = "instruction-form";
    native_name = "TEST";
    snapshot = "test";
    origin = { path = "test"; line = None };
    encoding = R.X86_encoding { space; opcode_map; opcode = "0x00"; pattern; operands };
    provenance = R.Other;
    applicability = R.App_all [];
    unresolved = [];
  }

let riscv ?(width_bits = 32) fields : R.t =
  {
    (x86 ~pattern:"" []) with
    record_id = "riscv-opcodes:test";
    source = "riscv_opcodes";
    encoding = R.Fixed_bits { width_bits; mask = "0x0"; value = "0x0"; fields = [] };
    provenance =
      R.Riscv_provenance
        {
          extension = Some "rv_test";
          raw_tokens = [];
          resolved_mask = None;
          resolved_match = None;
          variable_fields = fields;
          relationships = [];
        };
  }

let names rec_ = List.map Isa_construct.to_string (Isa_construct.of_record rec_)

let gpr_rr =
  x86 ~pattern:"0x01 MOD[0b11] MOD=3 REG[rrr] RM[nnn]"
    [ operand ~lookup:"GPRv_B" "REG0"; operand ~lookup:"GPRv_R" "REG1" ]

let test_vocabulary () =
  check "legacy gpr pair: map plus one register class"
    (names gpr_rr = [ "unsupported-encoding:legacy-map0"; "unknown-operand:reg:gpr" ]);
  let masked =
    x86 ~space:"evex" ~opcode_map:1
      ~pattern:"VEXVALID=2 0x58 MOD[mm] MOD!=3 BCRC=1 VL=0 REXW=1 ZEROING=1"
      [
        operand ~lookup:"XMM_R3" "REG0";
        operand ~lookup:"MASK1" "REG1";
        operand ~op_type:"imm_const" ~oc2:"zf32" "MEM0";
        operand ~visibility:"SUPPRESSED" ~op_type:"reg" ~bits:"XED_REG_MXCSR" "REG2";
      ]
  in
  check "evex: vl, w1, zeroing, broadcast, opmask; suppressed operands ignored"
    (names masked
    = [
        "unsupported-encoding:evex-broadcast";
        "unsupported-encoding:evex-map1";
        "unsupported-encoding:evex-opmask";
        "unsupported-encoding:evex-vl128";
        "unsupported-encoding:evex-w1";
        "unsupported-encoding:evex-zeroing";
        "unknown-operand:mem:zf32";
        "unknown-operand:reg:xmm";
      ]);
  let shift =
    x86 ~pattern:"0xD3 MOD[0b11] MOD=3 REG[0b100] RM[nnn]"
      [
        operand ~lookup:"GPRv_B" "REG0";
        operand ~visibility:"IMPLICIT" ~op_type:"reg" ~bits:"XED_REG_CL" "REG1";
      ]
  in
  check "implicit visible register is its own operand construct"
    (List.mem "unknown-operand:implicit:CL" (names shift));
  check "riscv: one construct per variable field, len16 for compressed"
    (names (riscv ~width_bits:16 [ "rd"; "c_imm6lo" ])
    = [ "unsupported-encoding:len16"; "unknown-operand:field:c_imm6lo"; "unknown-operand:field:rd" ]
    )

let test_blocker () =
  let known = Isa_construct.known_of [ (gpr_rr, true); (riscv [ "rd"; "rs1" ], true) ] in
  check "no rule, every construct known" (Isa_construct.blocker known gpr_rr = Isa_construct.no_rule);
  check "first unknown construct names the blocker"
    (Isa_construct.blocker known (riscv [ "rd"; "rs1"; "csr" ]) = "unknown-operand:field:csr");
  let unknown_only = Isa_construct.known_of [ (riscv [ "csr" ], false) ] in
  check "a record that does not normalize contributes nothing"
    (Isa_construct.blocker unknown_only (riscv [ "csr" ]) = "unknown-operand:field:csr");
  check "encoding constructs precede operand constructs"
    (Isa_construct.blocker known (riscv ~width_bits:16 [ "c_x" ]) = "unsupported-encoding:len16");
  check "catch-all rules are the only refined ones"
    (Isa_construct.is_catch_all "unhandled-iform"
    && Isa_construct.is_catch_all "unhandled-native-name"
    && not (Isa_construct.is_catch_all "addi-unrecognized-operands"))

let () =
  test_vocabulary ();
  test_blocker ();
  if !failures > 0 then (
    Printf.printf "isa-construct: %d of %d checks FAILED\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-construct: all %d checks passed\n" !checks
