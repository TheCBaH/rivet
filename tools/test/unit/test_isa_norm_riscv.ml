(* Isa_norm_riscv against the five frozen pilot records,
   taken verbatim (one compact JSON line
   each, re-serialized with sorted keys but otherwise byte-identical) from
   the checked-in isa-db/export/riscv_opcodes/riscv32.jsonl at this
   revision - so a decoder/normalization regression here is a regression
   against real captured data, not a synthetic shape. No filesystem
   dependency: {!Isa_source_record.of_line} decodes inline text, matching
   this directory's existing isa-inventory tests. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let sw_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":25,"name":"imm12hi","width":7},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5},{"lsb":7,"name":"imm12lo","width":5}],"kind":"fixed_bits","mask":"0x707f","value":"0x2023","width_bits":32},"kind":"instruction-form","native_name":"sw","origin":{"line":20,"path":"extensions/rv_i"},"provenance":{"extension":"rv_i","operands":["imm12hi","rs1","rs2","imm12lo"],"raw":{"line":"sw     imm12hi rs1 rs2 imm12lo 14..12=2 6..2=0x08 1..0=3","tokens":["sw","imm12hi","rs1","rs2","imm12lo","14..12=2","6..2=0x08","1..0=3"]},"upstream-resolved":{"mask":"0x707f","match":"0x2023","variable_fields":["imm12hi","rs1","rs2","imm12lo"]}},"record_id":"riscv-opcodes:rv_i:sw@L20","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let beq_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":25,"name":"bimm12hi","width":7},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5},{"lsb":7,"name":"bimm12lo","width":5}],"kind":"fixed_bits","mask":"0x707f","value":"0x63","width_bits":32},"kind":"instruction-form","native_name":"beq","origin":{"line":7,"path":"extensions/rv_i"},"provenance":{"extension":"rv_i","operands":["bimm12hi","rs1","rs2","bimm12lo"],"raw":{"line":"beq     bimm12hi rs1 rs2 bimm12lo 14..12=0 6..2=0x18 1..0=3","tokens":["beq","bimm12hi","rs1","rs2","bimm12lo","14..12=0","6..2=0x18","1..0=3"]},"upstream-resolved":{"mask":"0x707f","match":"0x63","variable_fields":["bimm12hi","rs1","rs2","bimm12lo"]}},"record_id":"riscv-opcodes:rv_i:beq@L7","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let c_addi_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":13,"name":"bits[15:13]","width":3},{"lsb":7,"name":"rd_rs1_n0","width":5},{"lsb":2,"name":"c_nzimm6lo","width":5},{"lsb":12,"name":"c_nzimm6hi","width":1}],"kind":"fixed_bits","mask":"0xe003","value":"0x1","width_bits":16},"kind":"instruction-form","native_name":"c.addi","origin":{"line":8,"path":"extensions/rv_c"},"provenance":{"extension":"rv_c","operands":["rd_rs1_n0","c_nzimm6lo","c_nzimm6hi"],"raw":{"line":"c.addi rd_rs1_n0 c_nzimm6lo c_nzimm6hi   1..0=1 15..13=0","tokens":["c.addi","rd_rs1_n0","c_nzimm6lo","c_nzimm6hi","1..0=1","15..13=0"]},"upstream-resolved":{"mask":"0xe003","match":"0x1","variable_fields":["rd_rs1_n0","c_nzimm6lo","c_nzimm6hi"]}},"record_id":"riscv-opcodes:rv_c:c.addi@L8","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let sh1add_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x20002033","width_bits":32},"kind":"instruction-form","native_name":"sh1add","origin":{"line":1,"path":"extensions/rv_zba"},"provenance":{"extension":"rv_zba","operands":["rd","rs1","rs2"],"raw":{"line":"sh1add     rd rs1 rs2 31..25=16 14..12=2 6..2=0x0C 1..0=3","tokens":["sh1add","rd","rs1","rs2","31..25=16","14..12=2","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x20002033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zba:sh1add@L1","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let sh2add_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x20004033","width_bits":32},"kind":"instruction-form","native_name":"sh2add","origin":{"line":2,"path":"extensions/rv_zba"},"provenance":{"extension":"rv_zba","operands":["rd","rs1","rs2"],"raw":{"line":"sh2add     rd rs1 rs2 31..25=16 14..12=4 6..2=0x0C 1..0=3","tokens":["sh2add","rd","rs1","rs2","31..25=16","14..12=4","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x20004033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zba:sh2add@L2","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let sh3add_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x20006033","width_bits":32},"kind":"instruction-form","native_name":"sh3add","origin":{"line":3,"path":"extensions/rv_zba"},"provenance":{"extension":"rv_zba","operands":["rd","rs1","rs2"],"raw":{"line":"sh3add     rd rs1 rs2 31..25=16 14..12=6 6..2=0x0C 1..0=3","tokens":["sh3add","rd","rs1","rs2","31..25=16","14..12=6","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x20006033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zba:sh3add@L3","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let sh1adduw_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x2000203b","width_bits":32},"kind":"instruction-form","native_name":"sh1add.uw","origin":{"line":2,"path":"extensions/rv64_zba"},"provenance":{"extension":"rv64_zba","operands":["rd","rs1","rs2"],"raw":{"line":"sh1add.uw  rd rs1 rs2 31..25=16 14..12=2 6..2=0x0E 1..0=3","tokens":["sh1add.uw","rd","rs1","rs2","31..25=16","14..12=2","6..2=0x0E","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x2000203b","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv64_zba:sh1add.uw@L2","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let sh2adduw_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x2000403b","width_bits":32},"kind":"instruction-form","native_name":"sh2add.uw","origin":{"line":3,"path":"extensions/rv64_zba"},"provenance":{"extension":"rv64_zba","operands":["rd","rs1","rs2"],"raw":{"line":"sh2add.uw  rd rs1 rs2 31..25=16 14..12=4 6..2=0x0E 1..0=3","tokens":["sh2add.uw","rd","rs1","rs2","31..25=16","14..12=4","6..2=0x0E","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x2000403b","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv64_zba:sh2add.uw@L3","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let sh3adduw_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x2000603b","width_bits":32},"kind":"instruction-form","native_name":"sh3add.uw","origin":{"line":4,"path":"extensions/rv64_zba"},"provenance":{"extension":"rv64_zba","operands":["rd","rs1","rs2"],"raw":{"line":"sh3add.uw  rd rs1 rs2 31..25=16 14..12=6 6..2=0x0E 1..0=3","tokens":["sh3add.uw","rd","rs1","rs2","31..25=16","14..12=6","6..2=0x0E","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x2000603b","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv64_zba:sh3add.uw@L4","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let min_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0xa004033","width_bits":32},"kind":"instruction-form","native_name":"min","origin":{"line":9,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1","rs2"],"raw":{"line":"min        rd rs1 rs2 31..25=5 14..12=4 6..2=0x0C 1..0=3","tokens":["min","rd","rs1","rs2","31..25=5","14..12=4","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0xa004033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zbb:min@L9","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let minu_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0xa005033","width_bits":32},"kind":"instruction-form","native_name":"minu","origin":{"line":10,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1","rs2"],"raw":{"line":"minu       rd rs1 rs2 31..25=5 14..12=5 6..2=0x0C 1..0=3","tokens":["minu","rd","rs1","rs2","31..25=5","14..12=5","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0xa005033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zbb:minu@L10","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let max_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0xa006033","width_bits":32},"kind":"instruction-form","native_name":"max","origin":{"line":7,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1","rs2"],"raw":{"line":"max        rd rs1 rs2 31..25=5 14..12=6 6..2=0x0C 1..0=3","tokens":["max","rd","rs1","rs2","31..25=5","14..12=6","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0xa006033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zbb:max@L7","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let maxu_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0xa007033","width_bits":32},"kind":"instruction-form","native_name":"maxu","origin":{"line":8,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1","rs2"],"raw":{"line":"maxu       rd rs1 rs2 31..25=5 14..12=7 6..2=0x0C 1..0=3","tokens":["maxu","rd","rs1","rs2","31..25=5","14..12=7","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0xa007033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zbb:maxu@L8","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let fadd_s_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":27,"name":"bits[31:27]","width":5},{"lsb":25,"name":"bits[26:25]","width":2},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5},{"lsb":12,"name":"rm","width":3}],"kind":"fixed_bits","mask":"0xfe00007f","value":"0x53","width_bits":32},"kind":"instruction-form","native_name":"fadd.s","origin":{"line":7,"path":"extensions/rv_f"},"provenance":{"extension":"rv_f","operands":["rd","rs1","rs2","rm"],"raw":{"line":"fadd.s    rd rs1 rs2      31..27=0x00 rm       26..25=0 6..2=0x14 1..0=3","tokens":["fadd.s","rd","rs1","rs2","31..27=0x00","rm","26..25=0","6..2=0x14","1..0=3"]},"upstream-resolved":{"mask":"0xfe00007f","match":"0x53","variable_fields":["rd","rs1","rs2","rm"]}},"record_id":"riscv-opcodes:rv_f:fadd.s@L7","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let decode_or_fail label json =
  match Isa_source_record.of_line json with
  | Ok r -> r
  | Error msg ->
      check (Printf.sprintf "%s: decodes" label) false;
      failwith msg

let normalize_or_fail label rec_ =
  match Isa_norm_riscv.normalize rec_ with
  | Ok form -> form
  | Error d ->
      check (Printf.sprintf "%s: normalizes (%s: %s)" label d.rule d.message) false;
      failwith d.message

let test_sw () =
  let rec_ = decode_or_fail "sw" sw_json in
  check "sw: decoded native_name" (rec_.native_name = "sw");
  let form = normalize_or_fail "sw" rec_ in
  check "sw: form_id" (form.form_id = "riscv:sw");
  check "sw: three operands" (List.length form.operands = 3);
  check "sw: requirement is unconditional (rv_i)" (form.requirement = Isa_norm_model.Req_all []);
  check "sw: offset operand is a 12-bit signed immediate built from two runs"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "offset") form.operands with
    | {
     op_kind = Immediate { width_bits = 12; signed = true; runs = [ hi; lo ]; nonzero = false; _ };
     _;
    } ->
        hi.field_name = "imm12hi" && hi.dest_hi = 11 && hi.dest_lo = 5 && lo.field_name = "imm12lo"
        && lo.dest_hi = 4 && lo.dest_lo = 0
    | _ -> false);
  check "sw: renders exactly the plan's §4.2 example text"
    (Isa_norm_model.render_syntax form.syntax = "sw value, offset(base)")

let test_beq () =
  let rec_ = decode_or_fail "beq" beq_json in
  let form = normalize_or_fail "beq" rec_ in
  check "beq: form_id" (form.form_id = "riscv:beq");
  check "beq: offset is a 13-bit signed immediate with an implicit low zero bit and four runs"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "offset") form.operands with
    | {
     op_kind =
       Immediate
         {
           width_bits = 13;
           signed = true;
           implicit_low_zero_bits = 1;
           runs = [ r1; r2; r3; r4 ];
           _;
         };
     _;
    } ->
        (* imm[12], imm[10:5], imm[4:1], imm[11] - the B-type permutation, not concatenation *)
        r1.field_name = "bimm12hi" && r1.dest_hi = 12 && r1.dest_lo = 12 && r1.field_hi = 6
        && r1.field_lo = 6 && r2.field_name = "bimm12hi" && r2.dest_hi = 10 && r2.dest_lo = 5
        && r2.field_hi = 5 && r2.field_lo = 0 && r3.field_name = "bimm12lo" && r3.dest_hi = 4
        && r3.dest_lo = 1 && r3.field_hi = 4 && r3.field_lo = 1 && r4.field_name = "bimm12lo"
        && r4.dest_hi = 11 && r4.dest_lo = 11 && r4.field_hi = 0 && r4.field_lo = 0
    | _ -> false);
  check "beq: renders as three operands"
    (Isa_norm_model.render_syntax form.syntax = "beq lhs, rhs, offset")

let test_c_addi () =
  let rec_ = decode_or_fail "c.addi" c_addi_json in
  let form = normalize_or_fail "c.addi" rec_ in
  check "c.addi: requirement is the C feature"
    (form.requirement = Isa_norm_model.Req_feature "riscv:c");
  check "c.addi: acc excludes x0 and is tied in/out"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "acc") form.operands with
    | { op_kind = Register { excluded = [ "x0" ]; _ }; role = In_out; _ } -> true
    | _ -> false);
  check "c.addi: nzimm is a nonzero 6-bit split immediate"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "nzimm") form.operands with
    | { op_kind = Immediate { width_bits = 6; nonzero = true; runs = [ hi; lo ]; _ }; _ } ->
        hi.field_name = "c_nzimm6hi" && hi.dest_hi = 5 && lo.field_name = "c_nzimm6lo"
        && lo.dest_lo = 0
    | _ -> false)

let andn_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x40007033","width_bits":32},"kind":"instruction-form","native_name":"andn","origin":{"line":1,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1","rs2"],"raw":{"line":"andn       rd rs1 rs2 31..25=32 14..12=7 6..2=0x0C 1..0=3","tokens":["andn","rd","rs1","rs2","31..25=32","14..12=7","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x40007033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zbb:andn@L1","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

(* the same andn instruction, exported a second time as rv_zbkb's $import
   record - used to prove Req_any is identical regardless of which of the
   five extension-membership records is normalized, not just true of the
   primary rv_zbb one. *)
let andn_zbkb_import_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x40007033","width_bits":32},"kind":"import","native_name":"andn","origin":{"line":3,"path":"extensions/rv_zbkb"},"provenance":{"extension":"rv_zbkb","import-reference":{"extension":"rv_zbb","name":"andn"},"raw":{"line":"$import rv_zbb::andn","tokens":["$import","rv_zbb::andn"]},"relationship-resolution":[{"candidates":["riscv-opcodes:rv_zbb:andn@L1"],"kind":"imports","status":"exact"}],"upstream-resolved":{"mask":"0xfe00707f","match":"0x40007033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zbkb:andn@L3","relationships":[{"kind":"imports","target":"riscv-opcodes:rv_zbb:andn@L1"}],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let orn_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x40006033","width_bits":32},"kind":"instruction-form","native_name":"orn","origin":{"line":2,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1","rs2"],"raw":{"line":"orn        rd rs1 rs2 31..25=32 14..12=6 6..2=0x0C 1..0=3","tokens":["orn","rd","rs1","rs2","31..25=32","14..12=6","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x40006033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zbb:orn@L2","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let xnor_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x40004033","width_bits":32},"kind":"instruction-form","native_name":"xnor","origin":{"line":3,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1","rs2"],"raw":{"line":"xnor       rd rs1 rs2 31..25=32 14..12=4 6..2=0x0C 1..0=3","tokens":["xnor","rd","rs1","rs2","31..25=32","14..12=4","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x40004033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zbb:xnor@L3","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let rol_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x60001033","width_bits":32},"kind":"instruction-form","native_name":"rol","origin":{"line":13,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1","rs2"],"raw":{"line":"rol        rd rs1 rs2 31..25=0x30 14..12=1 6..2=0x0C 1..0=3","tokens":["rol","rd","rs1","rs2","31..25=0x30","14..12=1","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x60001033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zbb:rol@L13","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let ror_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x60005033","width_bits":32},"kind":"instruction-form","native_name":"ror","origin":{"line":14,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1","rs2"],"raw":{"line":"ror        rd rs1 rs2 31..25=0x30 14..12=5 6..2=0x0C 1..0=3","tokens":["ror","rd","rs1","rs2","31..25=0x30","14..12=5","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x60005033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zbb:ror@L14","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let clz_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":20,"name":"bits[31:20]","width":12},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x60001013","width_bits":32},"kind":"instruction-form","native_name":"clz","origin":{"line":4,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1"],"raw":{"line":"clz        rd rs1 31..20=0x600 14..12=1 6..2=0x04 1..0=3","tokens":["clz","rd","rs1","31..20=0x600","14..12=1","6..2=0x04","1..0=3"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x60001013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv_zbb:clz@L4","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let ctz_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":20,"name":"bits[31:20]","width":12},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x60101013","width_bits":32},"kind":"instruction-form","native_name":"ctz","origin":{"line":5,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1"],"raw":{"line":"ctz        rd rs1 31..20=0x601 14..12=1 6..2=0x04 1..0=3","tokens":["ctz","rd","rs1","31..20=0x601","14..12=1","6..2=0x04","1..0=3"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x60101013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv_zbb:ctz@L5","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let cpop_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":20,"name":"bits[31:20]","width":12},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x60201013","width_bits":32},"kind":"instruction-form","native_name":"cpop","origin":{"line":6,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1"],"raw":{"line":"cpop       rd rs1 31..20=0x602 14..12=1 6..2=0x04 1..0=3","tokens":["cpop","rd","rs1","31..20=0x602","14..12=1","6..2=0x04","1..0=3"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x60201013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv_zbb:cpop@L6","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let sextb_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":20,"name":"bits[31:20]","width":12},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x60401013","width_bits":32},"kind":"instruction-form","native_name":"sext.b","origin":{"line":11,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1"],"raw":{"line":"sext.b     rd rs1 31..20=0x604 14..12=1 6..2=0x04 1..0=3","tokens":["sext.b","rd","rs1","31..20=0x604","14..12=1","6..2=0x04","1..0=3"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x60401013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv_zbb:sext.b@L11","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let sexth_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":20,"name":"bits[31:20]","width":12},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x60501013","width_bits":32},"kind":"instruction-form","native_name":"sext.h","origin":{"line":12,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1"],"raw":{"line":"sext.h     rd rs1 31..20=0x605 14..12=1 6..2=0x04 1..0=3","tokens":["sext.h","rd","rs1","31..20=0x605","14..12=1","6..2=0x04","1..0=3"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x60501013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv_zbb:sext.h@L12","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let orcb_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":20,"name":"bits[31:20]","width":12},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":0,"name":"bits[6:0]","width":7},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x28705013","width_bits":32},"kind":"instruction-form","native_name":"orc.b","origin":{"line":15,"path":"extensions/rv_zbb"},"provenance":{"extension":"rv_zbb","operands":["rd","rs1"],"raw":{"line":"orc.b rd rs1 31..20=0x287 14..12=0x5 6..0=0x13","tokens":["orc.b","rd","rs1","31..20=0x287","14..12=0x5","6..0=0x13"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x28705013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv_zbb:orc.b@L15","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let clzw_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":20,"name":"bits[31:20]","width":12},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x6000101b","width_bits":32},"kind":"instruction-form","native_name":"clzw","origin":{"line":1,"path":"extensions/rv64_zbb"},"provenance":{"extension":"rv64_zbb","operands":["rd","rs1"],"raw":{"line":"clzw  rd rs1                                31..20=0x600 14..12=1 6..2=0x06 1..0=3","tokens":["clzw","rd","rs1","31..20=0x600","14..12=1","6..2=0x06","1..0=3"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x6000101b","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv64_zbb:clzw@L1","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let ctzw_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":20,"name":"bits[31:20]","width":12},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x6010101b","width_bits":32},"kind":"instruction-form","native_name":"ctzw","origin":{"line":2,"path":"extensions/rv64_zbb"},"provenance":{"extension":"rv64_zbb","operands":["rd","rs1"],"raw":{"line":"ctzw  rd rs1                                31..20=0x601 14..12=1 6..2=0x06 1..0=3","tokens":["ctzw","rd","rs1","31..20=0x601","14..12=1","6..2=0x06","1..0=3"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x6010101b","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv64_zbb:ctzw@L2","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let cpopw_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":20,"name":"bits[31:20]","width":12},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x6020101b","width_bits":32},"kind":"instruction-form","native_name":"cpopw","origin":{"line":3,"path":"extensions/rv64_zbb"},"provenance":{"extension":"rv64_zbb","operands":["rd","rs1"],"raw":{"line":"cpopw rd rs1                                31..20=0x602 14..12=1 6..2=0x06 1..0=3","tokens":["cpopw","rd","rs1","31..20=0x602","14..12=1","6..2=0x06","1..0=3"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x6020101b","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv64_zbb:cpopw@L3","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let brev8_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":20,"name":"bits[31:20]","width":12},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x68705013","width_bits":32},"kind":"instruction-form","native_name":"brev8","origin":{"line":8,"path":"extensions/rv_zbkb"},"provenance":{"extension":"rv_zbkb","operands":["rd","rs1"],"raw":{"line":"brev8 rd rs1 31..20=0x687 14..12=5 6..2=0x4 1..0=0x3","tokens":["brev8","rd","rs1","31..20=0x687","14..12=5","6..2=0x4","1..0=0x3"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x68705013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv_zbkb:brev8@L8","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let brev8_zk_pseudo_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x68705013","width_bits":32},"kind":"pseudo-op","native_name":"brev8","origin":{"line":9,"path":"extensions/rv_zk"},"provenance":{"extension":"rv_zk","operands":["rd","rs1"],"raw":{"line":"$pseudo_op rv_zbkb::brev8 brev8 rd rs1 31..20=0x687 14..12=5 6..2=0x4 1..0=0x3","tokens":["$pseudo_op","rv_zbkb::brev8","brev8","rd","rs1","31..20=0x687","14..12=5","6..2=0x4","1..0=0x3"]},"relationship-resolution":[{"candidates":["riscv-opcodes:rv_zbkb:brev8@L8"],"kind":"specializes","status":"exact"}],"specializes-reference":{"extension":"rv_zbkb","name":"brev8"},"upstream-resolved":{"mask":"0xfff0707f","match":"0x68705013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv_zk:brev8@L9","relationships":[{"kind":"specializes","target":"riscv-opcodes:rv_zbkb:brev8@L8"}],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let rev8_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":20,"name":"bits[31:20]","width":12},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":0,"name":"bits[6:0]","width":7},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x6b805013","width_bits":32},"kind":"instruction-form","native_name":"rev8","origin":{"line":9,"path":"extensions/rv64_zbb"},"provenance":{"extension":"rv64_zbb","operands":["rd","rs1"],"raw":{"line":"rev8 rd rs1 31..20=0x6B8 14..12=5 6..0=0x13","tokens":["rev8","rd","rs1","31..20=0x6B8","14..12=5","6..0=0x13"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x6b805013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv64_zbb:rev8@L9","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let rev8_zk_pseudo_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x6b805013","width_bits":32},"kind":"pseudo-op","native_name":"rev8","origin":{"line":2,"path":"extensions/rv64_zk"},"provenance":{"extension":"rv64_zk","operands":["rd","rs1"],"raw":{"line":"$pseudo_op rv64_zbb::rev8 rev8 rd rs1      31..20=0x6B8 14..12=5 6..0=0x13","tokens":["$pseudo_op","rv64_zbb::rev8","rev8","rd","rs1","31..20=0x6B8","14..12=5","6..0=0x13"]},"relationship-resolution":[{"candidates":["riscv-opcodes:rv64_zbb:rev8@L9"],"kind":"specializes","status":"exact"}],"specializes-reference":{"extension":"rv64_zbb","name":"rev8"},"upstream-resolved":{"mask":"0xfff0707f","match":"0x6b805013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv64_zk:rev8@L2","relationships":[{"kind":"specializes","target":"riscv-opcodes:rv64_zbb:rev8@L9"}],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let rev8_rv32_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":20,"name":"bits[31:20]","width":12},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":0,"name":"bits[6:0]","width":7},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x69805013","width_bits":32},"kind":"instruction-form","native_name":"rev8.rv32","origin":{"line":4,"path":"extensions/rv32_zbkb"},"provenance":{"extension":"rv32_zbkb","operands":["rd","rs1"],"raw":{"line":"rev8.rv32 rd rs1 31..20=0x698 14..12=5 6..0=0x13","tokens":["rev8.rv32","rd","rs1","31..20=0x698","14..12=5","6..0=0x13"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x69805013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv32_zbkb:rev8.rv32@L4","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let test_r_type_gpr ~feature ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (Printf.sprintf "%s: requirement is the %s feature" mnemonic feature)
    (form.requirement = Isa_norm_model.Req_feature (Printf.sprintf "riscv:%s" feature));
  check
    (mnemonic ^ ": three plain GPR operands, no immediate")
    (List.length form.operands = 3
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check
    (mnemonic ^ ": renders as rd, rs1, rs2")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, rs1, rs2" mnemonic)

let test_sh1add () = test_r_type_gpr ~feature:"zba" ~mnemonic:"sh1add" ~json:sh1add_json
let test_sh2add () = test_r_type_gpr ~feature:"zba" ~mnemonic:"sh2add" ~json:sh2add_json
let test_sh3add () = test_r_type_gpr ~feature:"zba" ~mnemonic:"sh3add" ~json:sh3add_json

(* sh1add.uw/sh2add.uw/sh3add.uw are RV64-only (riscv-opcodes has no RV32
   counterpart), so their requirement is Req_all [Req_xlen 64; Req_feature
   "riscv:zba"], not the plain single Req_feature test_r_type_gpr checks -
   the same rv64_m-shaped combination addiw's Req_xlen already exercises,
   now paired with a real feature name. *)
let test_r_type_gpr_rv64 ~feature ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (Printf.sprintf "%s: requirement is RV64 plus the %s feature" mnemonic feature)
    (form.requirement
    = Isa_norm_model.Req_all [ Req_xlen 64; Req_feature (Printf.sprintf "riscv:%s" feature) ]);
  check (mnemonic ^ ": carries no diagnostics") (form.diagnostics = []);
  check
    (mnemonic ^ ": three plain GPR operands, no immediate")
    (List.length form.operands = 3
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check
    (mnemonic ^ ": renders as rd, rs1, rs2")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, rs1, rs2" mnemonic)

let test_sh1adduw () = test_r_type_gpr_rv64 ~feature:"zba" ~mnemonic:"sh1add.uw" ~json:sh1adduw_json
let test_sh2adduw () = test_r_type_gpr_rv64 ~feature:"zba" ~mnemonic:"sh2add.uw" ~json:sh2adduw_json
let test_sh3adduw () = test_r_type_gpr_rv64 ~feature:"zba" ~mnemonic:"sh3add.uw" ~json:sh3adduw_json
let test_min () = test_r_type_gpr ~feature:"zbb" ~mnemonic:"min" ~json:min_json
let test_minu () = test_r_type_gpr ~feature:"zbb" ~mnemonic:"minu" ~json:minu_json
let test_max () = test_r_type_gpr ~feature:"zbb" ~mnemonic:"max" ~json:max_json
let test_maxu () = test_r_type_gpr ~feature:"zbb" ~mnemonic:"maxu" ~json:maxu_json

(* vsetvl: V's register-register configuration-setting instruction - the
   same plain three-GPR R-type shape {!test_r_type_gpr} already exercises,
   with no import duplication (a single rv_v record on both profiles), so
   its requirement is a plain Req_feature "riscv:v", not a Req_any. Taken
   verbatim from the checked-in riscv64.jsonl (identical in riscv32.jsonl). *)
let vsetvl_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 31, "name": "bits[31:31]", "width": 1}, {"lsb": 25, "name": "bits[30:25]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "rd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x80007057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsetvl", "origin": {"line": 13, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["rs2", "rs1", "rd"], "raw": {"line": "vsetvl       31=1 30..25=0x0 rs2  rs1 14..12=0x7 rd 6..0=0x57", "tokens": ["vsetvl", "31=1", "30..25=0x0", "rs2", "rs1", "14..12=0x7", "rd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x80007057", "variable_fields": ["rs2", "rs1", "rd"]}}, "record_id": "riscv-opcodes:rv_v:vsetvl@L13", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vsetvl () = test_r_type_gpr ~feature:"v" ~mnemonic:"vsetvl" ~json:vsetvl_json

(* vsetvli/vsetivli: V's immediate-vtype siblings of vsetvl. Both taken
   verbatim from the checked-in riscv64.jsonl (identical in riscv32.jsonl). *)
let vsetvli_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 31, "name": "bits[31:31]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "zimm11", "width": 11}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "rd", "width": 5}], "kind": "fixed_bits", "mask": "0x8000707f", "value": "0x7057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsetvli", "origin": {"line": 12, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["zimm11", "rs1", "rd"], "raw": {"line": "vsetvli      31=0 zimm11          rs1 14..12=0x7 rd 6..0=0x57", "tokens": ["vsetvli", "31=0", "zimm11", "rs1", "14..12=0x7", "rd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0x8000707f", "match": "0x7057", "variable_fields": ["zimm11", "rs1", "rd"]}}, "record_id": "riscv-opcodes:rv_v:vsetvli@L12", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsetivli_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 31, "name": "bits[31:31]", "width": 1}, {"lsb": 30, "name": "bits[30:30]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "zimm10", "width": 10}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "rd", "width": 5}], "kind": "fixed_bits", "mask": "0xc000707f", "value": "0xc0007057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsetivli", "origin": {"line": 11, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["zimm10", "zimm5", "rd"], "raw": {"line": "vsetivli     31=1 30=1 zimm10    zimm5 14..12=0x7 rd 6..0=0x57", "tokens": ["vsetivli", "31=1", "30=1", "zimm10", "zimm5", "14..12=0x7", "rd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xc000707f", "match": "0xc0007057", "variable_fields": ["zimm10", "zimm5", "rd"]}}, "record_id": "riscv-opcodes:rv_v:vsetivli@L11", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vsetvli () =
  let rec_ = decode_or_fail "vsetvli" vsetvli_json in
  let form = normalize_or_fail "vsetvli" rec_ in
  check "vsetvli: form_id" (form.form_id = "riscv:vsetvli");
  check "vsetvli: requirement is the v feature"
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check "vsetvli: renders as rd, rs1, vtype"
    (Isa_norm_model.render_syntax form.syntax = "vsetvli rd, rs1, vtype");
  check "vsetvli: vtype is an unsigned 11-bit immediate"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "vtype") form.operands with
    | { op_kind = Immediate { width_bits = 11; signed = false; runs = [ r ]; _ }; _ } ->
        r.field_name = "zimm11" && r.dest_hi = 10 && r.dest_lo = 0
    | _ -> false)

let test_vsetivli () =
  let rec_ = decode_or_fail "vsetivli" vsetivli_json in
  let form = normalize_or_fail "vsetivli" rec_ in
  check "vsetivli: form_id" (form.form_id = "riscv:vsetivli");
  check "vsetivli: requirement is the v feature"
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check "vsetivli: renders as rd, uimm, vtype"
    (Isa_norm_model.render_syntax form.syntax = "vsetivli rd, uimm, vtype");
  check "vsetivli: uimm is an unsigned 5-bit immediate"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "uimm") form.operands with
    | { op_kind = Immediate { width_bits = 5; signed = false; runs = [ r ]; _ }; _ } ->
        r.field_name = "zimm5" && r.dest_hi = 4 && r.dest_lo = 0
    | _ -> false);
  check "vsetivli: vtype is an unsigned 10-bit immediate"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "vtype") form.operands with
    | { op_kind = Immediate { width_bits = 10; signed = false; runs = [ r ]; _ }; _ } ->
        r.field_name = "zimm10" && r.dest_hi = 9 && r.dest_lo = 0
    | _ -> false)

(* vadd.vv/vadd.vx/vadd.vi: OP-V's plain vector-register add family, the
   entry point into the real ~373-record vector arithmetic space. Taken
   verbatim from the checked-in riscv64.jsonl (identical in riscv32.jsonl -
   V is XLEN-independent). *)
let vadd_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x57", "width_bits": 32}, "kind": "instruction-form", "native_name": "vadd.vv", "origin": {"line": 260, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vadd.vv         31..26=0x00 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vadd.vv", "31..26=0x00", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x57", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vadd.vv@L260", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vadd_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x4057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vadd.vx", "origin": {"line": 213, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vadd.vx        31..26=0x00 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vadd.vx", "31..26=0x00", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x4057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vadd.vx@L213", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vadd_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x3057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vadd.vi", "origin": {"line": 306, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vadd.vi        31..26=0x00 vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vadd.vi", "31..26=0x00", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x3057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vadd.vi@L306", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vadd_vv () =
  let rec_ = decode_or_fail "vadd.vv" vadd_vv_json in
  let form = normalize_or_fail "vadd.vv" rec_ in
  check "vadd.vv: form_id" (form.form_id = "riscv:vadd.vv");
  check "vadd.vv: requirement is the v feature"
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check "vadd.vv: renders as rd, rs2, rs1"
    (Isa_norm_model.render_syntax form.syntax = "vadd.vv rd, rs2, rs1");
  check "vadd.vv: all three operands are vector registers"
    (List.for_all
       (fun (o : Isa_norm_model.operand) ->
         match o.op_kind with Register { class_ = Riscv_vec; _ } -> true | _ -> false)
       form.operands)

let test_vadd_vx () =
  let rec_ = decode_or_fail "vadd.vx" vadd_vx_json in
  let form = normalize_or_fail "vadd.vx" rec_ in
  check "vadd.vx: form_id" (form.form_id = "riscv:vadd.vx");
  check "vadd.vx: renders as rd, rs2, rs1"
    (Isa_norm_model.render_syntax form.syntax = "vadd.vx rd, rs2, rs1");
  check "vadd.vx: rs1 is a GPR, rd/rs2 are vector registers"
    (match
       ( List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rs1") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rd") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rs2") form.operands )
     with
    | ( { op_kind = Register { class_ = Riscv_gpr; _ }; _ },
        { op_kind = Register { class_ = Riscv_vec; _ }; _ },
        { op_kind = Register { class_ = Riscv_vec; _ }; _ } ) ->
        true
    | _ -> false)

let test_vadd_vi () =
  let rec_ = decode_or_fail "vadd.vi" vadd_vi_json in
  let form = normalize_or_fail "vadd.vi" rec_ in
  check "vadd.vi: form_id" (form.form_id = "riscv:vadd.vi");
  check "vadd.vi: renders as rd, rs2, simm5"
    (Isa_norm_model.render_syntax form.syntax = "vadd.vi rd, rs2, simm5");
  check "vadd.vi: simm5 is a signed 5-bit immediate"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "simm5") form.operands with
    | { op_kind = Immediate { width_bits = 5; signed = true; runs = [ r ]; _ }; _ } ->
        r.field_name = "simm5" && r.dest_hi = 4 && r.dest_lo = 0
    | _ -> false)

(* vsub/vrsub/vand/vor/vxor: OP-V's OPIVV/OPIVX/OPIVI shapes, generalized
   by {!Isa_norm_riscv.opivv_form}/[opivx_form]/[opivi_form]. Taken verbatim
   from the checked-in riscv64.jsonl (identical in riscv32.jsonl - V is
   XLEN-independent). *)
let vsub_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x8000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsub.vv", "origin": {"line": 261, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vsub.vv         31..26=0x02 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vsub.vv", "31..26=0x02", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x8000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsub.vv@L261", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsub_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x8004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsub.vx", "origin": {"line": 214, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vsub.vx        31..26=0x02 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vsub.vx", "31..26=0x02", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x8004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsub.vx@L214", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vrsub_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xc004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vrsub.vx", "origin": {"line": 215, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vrsub.vx       31..26=0x03 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vrsub.vx", "31..26=0x03", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xc004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vrsub.vx@L215", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vrsub_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xc003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vrsub.vi", "origin": {"line": 307, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vrsub.vi       31..26=0x03 vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vrsub.vi", "31..26=0x03", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xc003057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vrsub.vi@L307", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vand_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x24000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vand.vv", "origin": {"line": 266, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vand.vv         31..26=0x09 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vand.vv", "31..26=0x09", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x24000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vand.vv@L266", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vand_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x24004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vand.vx", "origin": {"line": 220, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vand.vx        31..26=0x09 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vand.vx", "31..26=0x09", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x24004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vand.vx@L220", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vand_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x24003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vand.vi", "origin": {"line": 308, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vand.vi        31..26=0x09 vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vand.vi", "31..26=0x09", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x24003057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vand.vi@L308", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vor_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x28000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vor.vv", "origin": {"line": 267, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vor.vv          31..26=0x0a vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vor.vv", "31..26=0x0a", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x28000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vor.vv@L267", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vor_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x28004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vor.vx", "origin": {"line": 221, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vor.vx         31..26=0x0a vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vor.vx", "31..26=0x0a", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x28004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vor.vx@L221", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vor_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x28003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vor.vi", "origin": {"line": 309, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vor.vi         31..26=0x0a vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vor.vi", "31..26=0x0a", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x28003057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vor.vi@L309", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vxor_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x2c000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vxor.vv", "origin": {"line": 268, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vxor.vv         31..26=0x0b vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vxor.vv", "31..26=0x0b", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x2c000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vxor.vv@L268", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vxor_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x2c004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vxor.vx", "origin": {"line": 222, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vxor.vx        31..26=0x0b vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vxor.vx", "31..26=0x0b", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x2c004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vxor.vx@L222", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vxor_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x2c003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vxor.vi", "origin": {"line": 310, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vxor.vi        31..26=0x0b vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vxor.vi", "31..26=0x0b", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x2c003057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vxor.vi@L310", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_opivv_form ~mnemonic json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs2, rs1")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs2, rs1");
  check
    (mnemonic ^ ": all three operands are vector registers")
    (List.for_all
       (fun (o : Isa_norm_model.operand) ->
         match o.op_kind with Register { class_ = Riscv_vec; _ } -> true | _ -> false)
       form.operands)

let test_opivx_form ~mnemonic json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs2, rs1")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs2, rs1");
  check
    (mnemonic ^ ": rs1 is a GPR, rd/rs2 are vector registers")
    (match
       ( List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rs1") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rd") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rs2") form.operands )
     with
    | ( { op_kind = Register { class_ = Riscv_gpr; _ }; _ },
        { op_kind = Register { class_ = Riscv_vec; _ }; _ },
        { op_kind = Register { class_ = Riscv_vec; _ }; _ } ) ->
        true
    | _ -> false)

let test_opmacc_vv_form ~mnemonic json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs1, rs2 (reordered relative to opivv_form)")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1, rs2");
  check
    (mnemonic ^ ": all three operands are vector registers")
    (List.for_all
       (fun (o : Isa_norm_model.operand) ->
         match o.op_kind with Register { class_ = Riscv_vec; _ } -> true | _ -> false)
       form.operands)

let test_opmacc_vx_form ~mnemonic json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs1, rs2 (reordered relative to opivx_form)")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1, rs2");
  check
    (mnemonic ^ ": rs1 is a GPR, rd/rs2 are vector registers")
    (match
       ( List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rs1") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rd") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rs2") form.operands )
     with
    | ( { op_kind = Register { class_ = Riscv_gpr; _ }; _ },
        { op_kind = Register { class_ = Riscv_vec; _ }; _ },
        { op_kind = Register { class_ = Riscv_vec; _ }; _ } ) ->
        true
    | _ -> false)

let test_vext_form ~mnemonic json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs2")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs2");
  check
    (mnemonic ^ ": both operands are vector registers")
    (List.for_all
       (fun (o : Isa_norm_model.operand) ->
         match o.op_kind with Register { class_ = Riscv_vec; _ } -> true | _ -> false)
       form.operands)

let test_v_to_x_unary_form ~mnemonic json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs2")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs2");
  check
    (mnemonic ^ ": rd is a GPR, rs2 is a vector register")
    (match
       ( List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rd") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rs2") form.operands )
     with
    | ( { op_kind = Register { class_ = Riscv_gpr; _ }; _ },
        { op_kind = Register { class_ = Riscv_vec; _ }; _ } ) ->
        true
    | _ -> false)

let test_carry_m_vv_form ~mnemonic json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs2, rs1, vcarry")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs2, rs1, vcarry");
  check
    (mnemonic ^ ": all four operands are vector registers")
    (List.for_all
       (fun (o : Isa_norm_model.operand) ->
         match o.op_kind with Register { class_ = Riscv_vec; _ } -> true | _ -> false)
       form.operands)

let test_carry_m_vx_form ~mnemonic json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs2, rs1, vcarry")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs2, rs1, vcarry");
  check
    (mnemonic ^ ": rs1 is a GPR, rd/rs2/vcarry are vector registers")
    (match
       ( List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rs1") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rd") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rs2") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "vcarry") form.operands )
     with
    | ( { op_kind = Register { class_ = Riscv_gpr; _ }; _ },
        { op_kind = Register { class_ = Riscv_vec; _ }; _ },
        { op_kind = Register { class_ = Riscv_vec; _ }; _ },
        { op_kind = Register { class_ = Riscv_vec; _ }; _ } ) ->
        true
    | _ -> false)

let test_carry_m_vi_form ~mnemonic json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs2, simm5, vcarry")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs2, simm5, vcarry");
  check
    (mnemonic ^ ": simm5 is a signed 5-bit immediate")
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "simm5") form.operands with
    | { op_kind = Immediate { width_bits = 5; signed = true; runs = [ r ]; _ }; _ } ->
        r.field_name = "simm5" && r.dest_hi = 4 && r.dest_lo = 0
    | _ -> false)

let test_mv_x_s_form json =
  let mnemonic = "vmv.x.s" in
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs2")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs2");
  check
    (mnemonic ^ ": rd is a GPR, rs2 is a vector register")
    (match
       ( List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rd") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rs2") form.operands )
     with
    | ( { op_kind = Register { class_ = Riscv_gpr; _ }; _ },
        { op_kind = Register { class_ = Riscv_vec; _ }; _ } ) ->
        true
    | _ -> false)

let test_mv_s_x_form json =
  let mnemonic = "vmv.s.x" in
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs1")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1");
  check
    (mnemonic ^ ": rd is a vector register, rs1 is a GPR")
    (match
       ( List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rd") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rs1") form.operands )
     with
    | ( { op_kind = Register { class_ = Riscv_vec; _ }; _ },
        { op_kind = Register { class_ = Riscv_gpr; _ }; _ } ) ->
        true
    | _ -> false)

let test_vmv_v_v_form json =
  let mnemonic = "vmv.v.v" in
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": renders as rd, rs1")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1");
  check
    (mnemonic ^ ": both operands are vector registers")
    (List.for_all
       (fun (o : Isa_norm_model.operand) ->
         match o.op_kind with Register { class_ = Riscv_vec; _ } -> true | _ -> false)
       form.operands)

let test_vmv_v_x_form json =
  let mnemonic = "vmv.v.x" in
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": renders as rd, rs1")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1");
  check
    (mnemonic ^ ": rd is a vector register, rs1 is a GPR")
    (match
       ( List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rd") form.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rs1") form.operands )
     with
    | ( { op_kind = Register { class_ = Riscv_vec; _ }; _ },
        { op_kind = Register { class_ = Riscv_gpr; _ }; _ } ) ->
        true
    | _ -> false)

let test_vmv_v_i_form json =
  let mnemonic = "vmv.v.i" in
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": renders as rd, simm5")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, simm5");
  check
    (mnemonic ^ ": simm5 is a signed 5-bit immediate")
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "simm5") form.operands with
    | { op_kind = Immediate { width_bits = 5; signed = true; runs = [ r ]; _ }; _ } ->
        r.field_name = "simm5" && r.dest_hi = 4 && r.dest_lo = 0
    | _ -> false)

let test_whole_reg_move_form ~mnemonic json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs2")
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs2");
  check
    (mnemonic ^ ": both operands are vector registers")
    (List.for_all
       (fun (o : Isa_norm_model.operand) ->
         match o.op_kind with Register { class_ = Riscv_vec; _ } -> true | _ -> false)
       form.operands)

let test_vid_form json =
  let mnemonic = "vid.v" in
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check (mnemonic ^ ": renders as rd") (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd");
  check
    (mnemonic ^ ": has exactly one vector-register operand")
    (match form.operands with
    | [ { op_kind = Register { class_ = Riscv_vec; _ }; _ } ] -> true
    | _ -> false)

let test_opivi_form ?(imm_name = "simm5") ?(signed = true) ~mnemonic json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check (mnemonic ^ ": form_id") (form.form_id = "riscv:" ^ mnemonic);
  check
    (mnemonic ^ ": requirement is the v feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:v");
  check
    (mnemonic ^ ": renders as rd, rs2, " ^ imm_name)
    (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs2, " ^ imm_name);
  check
    (mnemonic ^ ": " ^ imm_name ^ " is a "
    ^ (if signed then "signed" else "unsigned")
    ^ " 5-bit immediate")
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = imm_name) form.operands with
    | { op_kind = Immediate { width_bits = 5; signed = s; runs = [ r ]; _ }; _ } ->
        s = signed && r.field_name = imm_name && r.dest_hi = 4 && r.dest_lo = 0
    | _ -> false)

let test_vsub_vv () = test_opivv_form ~mnemonic:"vsub.vv" vsub_vv_json
let test_vsub_vx () = test_opivx_form ~mnemonic:"vsub.vx" vsub_vx_json
let test_vrsub_vx () = test_opivx_form ~mnemonic:"vrsub.vx" vrsub_vx_json
let test_vrsub_vi () = test_opivi_form ~mnemonic:"vrsub.vi" vrsub_vi_json
let test_vand_vv () = test_opivv_form ~mnemonic:"vand.vv" vand_vv_json
let test_vand_vx () = test_opivx_form ~mnemonic:"vand.vx" vand_vx_json
let test_vand_vi () = test_opivi_form ~mnemonic:"vand.vi" vand_vi_json
let test_vor_vv () = test_opivv_form ~mnemonic:"vor.vv" vor_vv_json
let test_vor_vx () = test_opivx_form ~mnemonic:"vor.vx" vor_vx_json
let test_vor_vi () = test_opivi_form ~mnemonic:"vor.vi" vor_vi_json
let test_vxor_vv () = test_opivv_form ~mnemonic:"vxor.vv" vxor_vv_json
let test_vxor_vx () = test_opivx_form ~mnemonic:"vxor.vx" vxor_vx_json
let test_vxor_vi () = test_opivi_form ~mnemonic:"vxor.vi" vxor_vi_json

(* vsll/vsrl/vsra/vminu/vmin/vmaxu/vmax/vmul/vmulh/vmulhu/vmulhsu: OP-V's shift,
   min/max, and OPMVV/OPMVX multiply-high families, generalized across the
   same opivv_form/opivx_form/opivi_form shapes above. *)
let vsll_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x94000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsll.vv", "origin": {"line": 291, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vsll.vv        31..26=0x25 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vsll.vv", "31..26=0x25", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x94000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsll.vv@L291", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsll_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x94004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsll.vx", "origin": {"line": 248, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vsll.vx        31..26=0x25 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vsll.vx", "31..26=0x25", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x94004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsll.vx@L248", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsll_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x94003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsll.vi", "origin": {"line": 329, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "zimm5", "vd"], "raw": {"line": "vsll.vi        31..26=0x25 vm vs2 zimm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vsll.vi", "31..26=0x25", "vm", "vs2", "zimm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x94003057", "variable_fields": ["vm", "vs2", "zimm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsll.vi@L329", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsrl_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xa0000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsrl.vv", "origin": {"line": 293, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vsrl.vv        31..26=0x28 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vsrl.vv", "31..26=0x28", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xa0000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsrl.vv@L293", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsrl_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xa0004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsrl.vx", "origin": {"line": 250, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vsrl.vx        31..26=0x28 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vsrl.vx", "31..26=0x28", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xa0004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsrl.vx@L250", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsrl_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xa0003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsrl.vi", "origin": {"line": 334, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "zimm5", "vd"], "raw": {"line": "vsrl.vi        31..26=0x28 vm vs2 zimm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vsrl.vi", "31..26=0x28", "vm", "vs2", "zimm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xa0003057", "variable_fields": ["vm", "vs2", "zimm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsrl.vi@L334", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsra_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xa4000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsra.vv", "origin": {"line": 294, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vsra.vv        31..26=0x29 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vsra.vv", "31..26=0x29", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xa4000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsra.vv@L294", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsra_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xa4004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsra.vx", "origin": {"line": 251, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vsra.vx        31..26=0x29 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vsra.vx", "31..26=0x29", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xa4004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsra.vx@L251", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsra_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xa4003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsra.vi", "origin": {"line": 335, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "zimm5", "vd"], "raw": {"line": "vsra.vi        31..26=0x29 vm vs2 zimm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vsra.vi", "31..26=0x29", "vm", "vs2", "zimm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xa4003057", "variable_fields": ["vm", "vs2", "zimm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsra.vi@L335", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vminu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x10000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vminu.vv", "origin": {"line": 262, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vminu.vv        31..26=0x04 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vminu.vv", "31..26=0x04", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x10000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vminu.vv@L262", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vminu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x10004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vminu.vx", "origin": {"line": 216, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vminu.vx       31..26=0x04 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vminu.vx", "31..26=0x04", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x10004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vminu.vx@L216", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmin_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x14000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmin.vv", "origin": {"line": 263, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmin.vv         31..26=0x05 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmin.vv", "31..26=0x05", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x14000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmin.vv@L263", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmin_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x14004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmin.vx", "origin": {"line": 217, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmin.vx        31..26=0x05 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmin.vx", "31..26=0x05", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x14004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmin.vx@L217", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmaxu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x18000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmaxu.vv", "origin": {"line": 264, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmaxu.vv        31..26=0x06 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmaxu.vv", "31..26=0x06", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x18000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmaxu.vv@L264", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmaxu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x18004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmaxu.vx", "origin": {"line": 218, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmaxu.vx       31..26=0x06 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmaxu.vx", "31..26=0x06", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x18004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmaxu.vx@L218", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmax_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x1c000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmax.vv", "origin": {"line": 265, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmax.vv         31..26=0x07 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmax.vv", "31..26=0x07", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x1c000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmax.vv@L265", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmax_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x1c004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmax.vx", "origin": {"line": 219, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmax.vx        31..26=0x07 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmax.vx", "31..26=0x07", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x1c004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmax.vx@L219", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmul_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x94002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmul.vv", "origin": {"line": 391, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmul.vv        31..26=0x25 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmul.vv", "31..26=0x25", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x94002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmul.vv@L391", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmul_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x94006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmul.vx", "origin": {"line": 429, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmul.vx        31..26=0x25 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vmul.vx", "31..26=0x25", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x94006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmul.vx@L429", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmulh_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x9c002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmulh.vv", "origin": {"line": 393, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmulh.vv       31..26=0x27 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmulh.vv", "31..26=0x27", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x9c002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmulh.vv@L393", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmulh_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x9c006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmulh.vx", "origin": {"line": 431, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmulh.vx       31..26=0x27 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vmulh.vx", "31..26=0x27", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x9c006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmulh.vx@L431", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmulhu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x90002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmulhu.vv", "origin": {"line": 390, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmulhu.vv      31..26=0x24 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmulhu.vv", "31..26=0x24", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x90002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmulhu.vv@L390", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmulhu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x90006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmulhu.vx", "origin": {"line": 428, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmulhu.vx      31..26=0x24 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vmulhu.vx", "31..26=0x24", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x90006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmulhu.vx@L428", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmulhsu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x98002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmulhsu.vv", "origin": {"line": 392, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmulhsu.vv     31..26=0x26 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmulhsu.vv", "31..26=0x26", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x98002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmulhsu.vv@L392", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmulhsu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x98006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmulhsu.vx", "origin": {"line": 430, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmulhsu.vx     31..26=0x26 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vmulhsu.vx", "31..26=0x26", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x98006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmulhsu.vx@L430", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vsll_vv () = test_opivv_form ~mnemonic:"vsll.vv" vsll_vv_json
let test_vsll_vx () = test_opivx_form ~mnemonic:"vsll.vx" vsll_vx_json

let test_vsll_vi () =
  test_opivi_form ~imm_name:"zimm5" ~signed:false ~mnemonic:"vsll.vi" vsll_vi_json

let test_vsrl_vv () = test_opivv_form ~mnemonic:"vsrl.vv" vsrl_vv_json
let test_vsrl_vx () = test_opivx_form ~mnemonic:"vsrl.vx" vsrl_vx_json

let test_vsrl_vi () =
  test_opivi_form ~imm_name:"zimm5" ~signed:false ~mnemonic:"vsrl.vi" vsrl_vi_json

let test_vsra_vv () = test_opivv_form ~mnemonic:"vsra.vv" vsra_vv_json
let test_vsra_vx () = test_opivx_form ~mnemonic:"vsra.vx" vsra_vx_json

let test_vsra_vi () =
  test_opivi_form ~imm_name:"zimm5" ~signed:false ~mnemonic:"vsra.vi" vsra_vi_json

let test_vminu_vv () = test_opivv_form ~mnemonic:"vminu.vv" vminu_vv_json
let test_vminu_vx () = test_opivx_form ~mnemonic:"vminu.vx" vminu_vx_json
let test_vmin_vv () = test_opivv_form ~mnemonic:"vmin.vv" vmin_vv_json
let test_vmin_vx () = test_opivx_form ~mnemonic:"vmin.vx" vmin_vx_json
let test_vmaxu_vv () = test_opivv_form ~mnemonic:"vmaxu.vv" vmaxu_vv_json
let test_vmaxu_vx () = test_opivx_form ~mnemonic:"vmaxu.vx" vmaxu_vx_json
let test_vmax_vv () = test_opivv_form ~mnemonic:"vmax.vv" vmax_vv_json
let test_vmax_vx () = test_opivx_form ~mnemonic:"vmax.vx" vmax_vx_json
let test_vmul_vv () = test_opivv_form ~mnemonic:"vmul.vv" vmul_vv_json
let test_vmul_vx () = test_opivx_form ~mnemonic:"vmul.vx" vmul_vx_json
let test_vmulh_vv () = test_opivv_form ~mnemonic:"vmulh.vv" vmulh_vv_json
let test_vmulh_vx () = test_opivx_form ~mnemonic:"vmulh.vx" vmulh_vx_json
let test_vmulhu_vv () = test_opivv_form ~mnemonic:"vmulhu.vv" vmulhu_vv_json
let test_vmulhu_vx () = test_opivx_form ~mnemonic:"vmulhu.vx" vmulhu_vx_json
let test_vmulhsu_vv () = test_opivv_form ~mnemonic:"vmulhsu.vv" vmulhsu_vv_json
let test_vmulhsu_vx () = test_opivx_form ~mnemonic:"vmulhsu.vx" vmulhsu_vx_json

let vdivu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x80002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vdivu.vv", "origin": {"line": 386, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vdivu.vv       31..26=0x20 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vdivu.vv", "31..26=0x20", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x80002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vdivu.vv@L386", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vdivu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x80006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vdivu.vx", "origin": {"line": 424, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vdivu.vx       31..26=0x20 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vdivu.vx", "31..26=0x20", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x80006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vdivu.vx@L424", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vdiv_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x84002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vdiv.vv", "origin": {"line": 387, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vdiv.vv        31..26=0x21 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vdiv.vv", "31..26=0x21", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x84002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vdiv.vv@L387", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vdiv_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x84006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vdiv.vx", "origin": {"line": 425, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vdiv.vx        31..26=0x21 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vdiv.vx", "31..26=0x21", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x84006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vdiv.vx@L425", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vremu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x88002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vremu.vv", "origin": {"line": 388, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vremu.vv       31..26=0x22 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vremu.vv", "31..26=0x22", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x88002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vremu.vv@L388", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vremu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x88006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vremu.vx", "origin": {"line": 426, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vremu.vx       31..26=0x22 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vremu.vx", "31..26=0x22", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x88006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vremu.vx@L426", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vrem_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x8c002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vrem.vv", "origin": {"line": 389, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vrem.vv        31..26=0x23 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vrem.vv", "31..26=0x23", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x8c002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vrem.vv@L389", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vrem_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x8c006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vrem.vx", "origin": {"line": 427, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vrem.vx        31..26=0x23 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vrem.vx", "31..26=0x23", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x8c006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vrem.vx@L427", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vdivu_vv () = test_opivv_form ~mnemonic:"vdivu.vv" vdivu_vv_json
let test_vdivu_vx () = test_opivx_form ~mnemonic:"vdivu.vx" vdivu_vx_json
let test_vdiv_vv () = test_opivv_form ~mnemonic:"vdiv.vv" vdiv_vv_json
let test_vdiv_vx () = test_opivx_form ~mnemonic:"vdiv.vx" vdiv_vx_json
let test_vremu_vv () = test_opivv_form ~mnemonic:"vremu.vv" vremu_vv_json
let test_vremu_vx () = test_opivx_form ~mnemonic:"vremu.vx" vremu_vx_json
let test_vrem_vv () = test_opivv_form ~mnemonic:"vrem.vv" vrem_vv_json
let test_vrem_vx () = test_opivx_form ~mnemonic:"vrem.vx" vrem_vx_json

let vsaddu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x80000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsaddu.vv", "origin": {"line": 287, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vsaddu.vv      31..26=0x20 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vsaddu.vv", "31..26=0x20", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x80000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsaddu.vv@L287", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsaddu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x80004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsaddu.vx", "origin": {"line": 244, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vsaddu.vx      31..26=0x20 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vsaddu.vx", "31..26=0x20", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x80004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsaddu.vx@L244", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsaddu_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x80003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsaddu.vi", "origin": {"line": 327, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vsaddu.vi      31..26=0x20 vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vsaddu.vi", "31..26=0x20", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x80003057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsaddu.vi@L327", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsadd_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x84000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsadd.vv", "origin": {"line": 288, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vsadd.vv       31..26=0x21 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vsadd.vv", "31..26=0x21", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x84000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsadd.vv@L288", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsadd_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x84004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsadd.vx", "origin": {"line": 245, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vsadd.vx       31..26=0x21 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vsadd.vx", "31..26=0x21", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x84004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsadd.vx@L245", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsadd_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x84003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsadd.vi", "origin": {"line": 328, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vsadd.vi       31..26=0x21 vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vsadd.vi", "31..26=0x21", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x84003057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsadd.vi@L328", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vssubu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x88000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vssubu.vv", "origin": {"line": 289, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vssubu.vv      31..26=0x22 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vssubu.vv", "31..26=0x22", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x88000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vssubu.vv@L289", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vssubu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x88004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vssubu.vx", "origin": {"line": 246, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vssubu.vx      31..26=0x22 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vssubu.vx", "31..26=0x22", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x88004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vssubu.vx@L246", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vssub_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x8c000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vssub.vv", "origin": {"line": 290, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vssub.vv       31..26=0x23 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vssub.vv", "31..26=0x23", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x8c000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vssub.vv@L290", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vssub_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x8c004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vssub.vx", "origin": {"line": 247, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vssub.vx       31..26=0x23 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vssub.vx", "31..26=0x23", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x8c004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vssub.vx@L247", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vsaddu_vv () = test_opivv_form ~mnemonic:"vsaddu.vv" vsaddu_vv_json
let test_vsaddu_vx () = test_opivx_form ~mnemonic:"vsaddu.vx" vsaddu_vx_json
let test_vsaddu_vi () = test_opivi_form ~mnemonic:"vsaddu.vi" vsaddu_vi_json
let test_vsadd_vv () = test_opivv_form ~mnemonic:"vsadd.vv" vsadd_vv_json
let test_vsadd_vx () = test_opivx_form ~mnemonic:"vsadd.vx" vsadd_vx_json
let test_vsadd_vi () = test_opivi_form ~mnemonic:"vsadd.vi" vsadd_vi_json
let test_vssubu_vv () = test_opivv_form ~mnemonic:"vssubu.vv" vssubu_vv_json
let test_vssubu_vx () = test_opivx_form ~mnemonic:"vssubu.vx" vssubu_vx_json
let test_vssub_vv () = test_opivv_form ~mnemonic:"vssub.vv" vssub_vv_json
let test_vssub_vx () = test_opivx_form ~mnemonic:"vssub.vx" vssub_vx_json

let vaaddu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x20002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vaaddu.vv", "origin": {"line": 352, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vaaddu.vv      31..26=0x08 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vaaddu.vv", "31..26=0x08", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x20002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vaaddu.vv@L352", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vaaddu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x20006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vaaddu.vx", "origin": {"line": 415, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vaaddu.vx      31..26=0x08 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vaaddu.vx", "31..26=0x08", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x20006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vaaddu.vx@L415", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vaadd_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x24002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vaadd.vv", "origin": {"line": 353, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vaadd.vv       31..26=0x09 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vaadd.vv", "31..26=0x09", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x24002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vaadd.vv@L353", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vaadd_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x24006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vaadd.vx", "origin": {"line": 416, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vaadd.vx       31..26=0x09 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vaadd.vx", "31..26=0x09", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x24006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vaadd.vx@L416", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vasubu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x28002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vasubu.vv", "origin": {"line": 354, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vasubu.vv      31..26=0x0a vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vasubu.vv", "31..26=0x0a", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x28002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vasubu.vv@L354", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vasubu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x28006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vasubu.vx", "origin": {"line": 417, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vasubu.vx      31..26=0x0a vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vasubu.vx", "31..26=0x0a", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x28006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vasubu.vx@L417", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vasub_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x2c002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vasub.vv", "origin": {"line": 355, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vasub.vv       31..26=0x0b vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vasub.vv", "31..26=0x0b", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x2c002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vasub.vv@L355", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vasub_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x2c006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vasub.vx", "origin": {"line": 418, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vasub.vx       31..26=0x0b vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vasub.vx", "31..26=0x0b", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x2c006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vasub.vx@L418", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vaaddu_vv () = test_opivv_form ~mnemonic:"vaaddu.vv" vaaddu_vv_json
let test_vaaddu_vx () = test_opivx_form ~mnemonic:"vaaddu.vx" vaaddu_vx_json
let test_vaadd_vv () = test_opivv_form ~mnemonic:"vaadd.vv" vaadd_vv_json
let test_vaadd_vx () = test_opivx_form ~mnemonic:"vaadd.vx" vaadd_vx_json
let test_vasubu_vv () = test_opivv_form ~mnemonic:"vasubu.vv" vasubu_vv_json
let test_vasubu_vx () = test_opivx_form ~mnemonic:"vasubu.vx" vasubu_vx_json
let test_vasub_vv () = test_opivv_form ~mnemonic:"vasub.vv" vasub_vv_json
let test_vasub_vx () = test_opivx_form ~mnemonic:"vasub.vx" vasub_vx_json

let vnsrl_wv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xb0000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnsrl.wv", "origin": {"line": 297, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vnsrl.wv       31..26=0x2c vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vnsrl.wv", "31..26=0x2c", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xb0000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnsrl.wv@L297", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnsrl_wx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xb0004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnsrl.wx", "origin": {"line": 254, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vnsrl.wx       31..26=0x2c vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vnsrl.wx", "31..26=0x2c", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xb0004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnsrl.wx@L254", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnsrl_wi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xb0003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnsrl.wi", "origin": {"line": 338, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "zimm5", "vd"], "raw": {"line": "vnsrl.wi       31..26=0x2c vm vs2 zimm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vnsrl.wi", "31..26=0x2c", "vm", "vs2", "zimm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xb0003057", "variable_fields": ["vm", "vs2", "zimm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnsrl.wi@L338", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnsra_wv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xb4000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnsra.wv", "origin": {"line": 298, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vnsra.wv       31..26=0x2d vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vnsra.wv", "31..26=0x2d", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xb4000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnsra.wv@L298", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnsra_wx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xb4004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnsra.wx", "origin": {"line": 255, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vnsra.wx       31..26=0x2d vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vnsra.wx", "31..26=0x2d", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xb4004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnsra.wx@L255", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnsra_wi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xb4003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnsra.wi", "origin": {"line": 339, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "zimm5", "vd"], "raw": {"line": "vnsra.wi       31..26=0x2d vm vs2 zimm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vnsra.wi", "31..26=0x2d", "vm", "vs2", "zimm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xb4003057", "variable_fields": ["vm", "vs2", "zimm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnsra.wi@L339", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnclipu_wv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xb8000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnclipu.wv", "origin": {"line": 299, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vnclipu.wv     31..26=0x2e vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vnclipu.wv", "31..26=0x2e", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xb8000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnclipu.wv@L299", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnclipu_wx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xb8004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnclipu.wx", "origin": {"line": 256, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vnclipu.wx     31..26=0x2e vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vnclipu.wx", "31..26=0x2e", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xb8004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnclipu.wx@L256", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnclipu_wi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xb8003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnclipu.wi", "origin": {"line": 340, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "zimm5", "vd"], "raw": {"line": "vnclipu.wi     31..26=0x2e vm vs2 zimm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vnclipu.wi", "31..26=0x2e", "vm", "vs2", "zimm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xb8003057", "variable_fields": ["vm", "vs2", "zimm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnclipu.wi@L340", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnclip_wv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xbc000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnclip.wv", "origin": {"line": 300, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vnclip.wv      31..26=0x2f vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vnclip.wv", "31..26=0x2f", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xbc000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnclip.wv@L300", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnclip_wx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xbc004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnclip.wx", "origin": {"line": 257, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vnclip.wx      31..26=0x2f vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vnclip.wx", "31..26=0x2f", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xbc004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnclip.wx@L257", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnclip_wi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xbc003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnclip.wi", "origin": {"line": 341, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "zimm5", "vd"], "raw": {"line": "vnclip.wi      31..26=0x2f vm vs2 zimm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vnclip.wi", "31..26=0x2f", "vm", "vs2", "zimm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xbc003057", "variable_fields": ["vm", "vs2", "zimm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnclip.wi@L341", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vnsrl_wv () = test_opivv_form ~mnemonic:"vnsrl.wv" vnsrl_wv_json
let test_vnsrl_wx () = test_opivx_form ~mnemonic:"vnsrl.wx" vnsrl_wx_json

let test_vnsrl_wi () =
  test_opivi_form ~imm_name:"zimm5" ~signed:false ~mnemonic:"vnsrl.wi" vnsrl_wi_json

let test_vnsra_wv () = test_opivv_form ~mnemonic:"vnsra.wv" vnsra_wv_json
let test_vnsra_wx () = test_opivx_form ~mnemonic:"vnsra.wx" vnsra_wx_json

let test_vnsra_wi () =
  test_opivi_form ~imm_name:"zimm5" ~signed:false ~mnemonic:"vnsra.wi" vnsra_wi_json

let test_vnclipu_wv () = test_opivv_form ~mnemonic:"vnclipu.wv" vnclipu_wv_json
let test_vnclipu_wx () = test_opivx_form ~mnemonic:"vnclipu.wx" vnclipu_wx_json

let test_vnclipu_wi () =
  test_opivi_form ~imm_name:"zimm5" ~signed:false ~mnemonic:"vnclipu.wi" vnclipu_wi_json

let test_vnclip_wv () = test_opivv_form ~mnemonic:"vnclip.wv" vnclip_wv_json
let test_vnclip_wx () = test_opivx_form ~mnemonic:"vnclip.wx" vnclip_wx_json

let test_vnclip_wi () =
  test_opivi_form ~imm_name:"zimm5" ~signed:false ~mnemonic:"vnclip.wi" vnclip_wi_json

let vssrl_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xa8000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vssrl.vv", "origin": {"line": 295, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vssrl.vv       31..26=0x2a vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vssrl.vv", "31..26=0x2a", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xa8000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vssrl.vv@L295", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vssrl_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xa8004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vssrl.vx", "origin": {"line": 252, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vssrl.vx       31..26=0x2a vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vssrl.vx", "31..26=0x2a", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xa8004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vssrl.vx@L252", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vssrl_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xa8003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vssrl.vi", "origin": {"line": 336, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "zimm5", "vd"], "raw": {"line": "vssrl.vi       31..26=0x2a vm vs2 zimm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vssrl.vi", "31..26=0x2a", "vm", "vs2", "zimm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xa8003057", "variable_fields": ["vm", "vs2", "zimm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vssrl.vi@L336", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vssra_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xac000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vssra.vv", "origin": {"line": 296, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vssra.vv       31..26=0x2b vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vssra.vv", "31..26=0x2b", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xac000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vssra.vv@L296", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vssra_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xac004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vssra.vx", "origin": {"line": 253, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vssra.vx       31..26=0x2b vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vssra.vx", "31..26=0x2b", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xac004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vssra.vx@L253", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vssra_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xac003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vssra.vi", "origin": {"line": 337, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "zimm5", "vd"], "raw": {"line": "vssra.vi       31..26=0x2b vm vs2 zimm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vssra.vi", "31..26=0x2b", "vm", "vs2", "zimm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xac003057", "variable_fields": ["vm", "vs2", "zimm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vssra.vi@L337", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vssrl_vv () = test_opivv_form ~mnemonic:"vssrl.vv" vssrl_vv_json
let test_vssrl_vx () = test_opivx_form ~mnemonic:"vssrl.vx" vssrl_vx_json

let test_vssrl_vi () =
  test_opivi_form ~imm_name:"zimm5" ~signed:false ~mnemonic:"vssrl.vi" vssrl_vi_json

let test_vssra_vv () = test_opivv_form ~mnemonic:"vssra.vv" vssra_vv_json
let test_vssra_vx () = test_opivx_form ~mnemonic:"vssra.vx" vssra_vx_json

let test_vssra_vi () =
  test_opivi_form ~imm_name:"zimm5" ~signed:false ~mnemonic:"vssra.vi" vssra_vi_json

let vrgather_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x30000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vrgather.vv", "origin": {"line": 269, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vrgather.vv     31..26=0x0c vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vrgather.vv", "31..26=0x0c", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x30000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vrgather.vv@L269", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vrgather_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x30004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vrgather.vx", "origin": {"line": 223, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vrgather.vx    31..26=0x0c vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vrgather.vx", "31..26=0x0c", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x30004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vrgather.vx@L223", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vrgather_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x30003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vrgather.vi", "origin": {"line": 311, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "zimm5", "vd"], "raw": {"line": "vrgather.vi    31..26=0x0c vm vs2 zimm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vrgather.vi", "31..26=0x0c", "vm", "vs2", "zimm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x30003057", "variable_fields": ["vm", "vs2", "zimm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vrgather.vi@L311", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vrgatherei16_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x38000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vrgatherei16.vv", "origin": {"line": 270, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vrgatherei16.vv 31..26=0x0e vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vrgatherei16.vv", "31..26=0x0e", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x38000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vrgatherei16.vv@L270", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vrgather_vv () = test_opivv_form ~mnemonic:"vrgather.vv" vrgather_vv_json
let test_vrgather_vx () = test_opivx_form ~mnemonic:"vrgather.vx" vrgather_vx_json

let test_vrgather_vi () =
  test_opivi_form ~imm_name:"zimm5" ~signed:false ~mnemonic:"vrgather.vi" vrgather_vi_json

let test_vrgatherei16_vv () = test_opivv_form ~mnemonic:"vrgatherei16.vv" vrgatherei16_vv_json

let vwaddu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xc0002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwaddu.vv", "origin": {"line": 399, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwaddu.vv      31..26=0x30 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwaddu.vv", "31..26=0x30", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xc0002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwaddu.vv@L399", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwaddu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xc0006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwaddu.vx", "origin": {"line": 437, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwaddu.vx      31..26=0x30 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwaddu.vx", "31..26=0x30", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xc0006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwaddu.vx@L437", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwadd_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xc4002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwadd.vv", "origin": {"line": 400, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwadd.vv       31..26=0x31 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwadd.vv", "31..26=0x31", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xc4002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwadd.vv@L400", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwadd_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xc4006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwadd.vx", "origin": {"line": 438, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwadd.vx       31..26=0x31 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwadd.vx", "31..26=0x31", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xc4006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwadd.vx@L438", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwsubu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xc8002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwsubu.vv", "origin": {"line": 401, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwsubu.vv      31..26=0x32 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwsubu.vv", "31..26=0x32", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xc8002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwsubu.vv@L401", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwsubu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xc8006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwsubu.vx", "origin": {"line": 439, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwsubu.vx      31..26=0x32 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwsubu.vx", "31..26=0x32", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xc8006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwsubu.vx@L439", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwsub_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xcc002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwsub.vv", "origin": {"line": 402, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwsub.vv       31..26=0x33 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwsub.vv", "31..26=0x33", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xcc002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwsub.vv@L402", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwsub_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xcc006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwsub.vx", "origin": {"line": 440, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwsub.vx       31..26=0x33 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwsub.vx", "31..26=0x33", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xcc006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwsub.vx@L440", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwaddu_wv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xd0002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwaddu.wv", "origin": {"line": 403, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwaddu.wv      31..26=0x34 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwaddu.wv", "31..26=0x34", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xd0002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwaddu.wv@L403", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwaddu_wx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xd0006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwaddu.wx", "origin": {"line": 441, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwaddu.wx      31..26=0x34 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwaddu.wx", "31..26=0x34", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xd0006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwaddu.wx@L441", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwadd_wv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xd4002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwadd.wv", "origin": {"line": 404, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwadd.wv       31..26=0x35 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwadd.wv", "31..26=0x35", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xd4002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwadd.wv@L404", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwadd_wx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xd4006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwadd.wx", "origin": {"line": 442, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwadd.wx       31..26=0x35 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwadd.wx", "31..26=0x35", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xd4006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwadd.wx@L442", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwsubu_wv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xd8002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwsubu.wv", "origin": {"line": 405, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwsubu.wv      31..26=0x36 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwsubu.wv", "31..26=0x36", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xd8002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwsubu.wv@L405", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwsubu_wx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xd8006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwsubu.wx", "origin": {"line": 443, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwsubu.wx      31..26=0x36 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwsubu.wx", "31..26=0x36", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xd8006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwsubu.wx@L443", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwsub_wv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xdc002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwsub.wv", "origin": {"line": 406, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwsub.wv       31..26=0x37 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwsub.wv", "31..26=0x37", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xdc002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwsub.wv@L406", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwsub_wx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xdc006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwsub.wx", "origin": {"line": 444, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwsub.wx       31..26=0x37 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwsub.wx", "31..26=0x37", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xdc006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwsub.wx@L444", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vwaddu_vv () = test_opivv_form ~mnemonic:"vwaddu.vv" vwaddu_vv_json
let test_vwaddu_vx () = test_opivx_form ~mnemonic:"vwaddu.vx" vwaddu_vx_json
let test_vwadd_vv () = test_opivv_form ~mnemonic:"vwadd.vv" vwadd_vv_json
let test_vwadd_vx () = test_opivx_form ~mnemonic:"vwadd.vx" vwadd_vx_json
let test_vwsubu_vv () = test_opivv_form ~mnemonic:"vwsubu.vv" vwsubu_vv_json
let test_vwsubu_vx () = test_opivx_form ~mnemonic:"vwsubu.vx" vwsubu_vx_json
let test_vwsub_vv () = test_opivv_form ~mnemonic:"vwsub.vv" vwsub_vv_json
let test_vwsub_vx () = test_opivx_form ~mnemonic:"vwsub.vx" vwsub_vx_json
let test_vwaddu_wv () = test_opivv_form ~mnemonic:"vwaddu.wv" vwaddu_wv_json
let test_vwaddu_wx () = test_opivx_form ~mnemonic:"vwaddu.wx" vwaddu_wx_json
let test_vwadd_wv () = test_opivv_form ~mnemonic:"vwadd.wv" vwadd_wv_json
let test_vwadd_wx () = test_opivx_form ~mnemonic:"vwadd.wx" vwadd_wx_json
let test_vwsubu_wv () = test_opivv_form ~mnemonic:"vwsubu.wv" vwsubu_wv_json
let test_vwsubu_wx () = test_opivx_form ~mnemonic:"vwsubu.wx" vwsubu_wx_json
let test_vwsub_wv () = test_opivv_form ~mnemonic:"vwsub.wv" vwsub_wv_json
let test_vwsub_wx () = test_opivx_form ~mnemonic:"vwsub.wx" vwsub_wx_json

let vwmulu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xe0002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmulu.vv", "origin": {"line": 407, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwmulu.vv      31..26=0x38 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwmulu.vv", "31..26=0x38", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xe0002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmulu.vv@L407", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwmulu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xe0006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmulu.vx", "origin": {"line": 445, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwmulu.vx      31..26=0x38 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwmulu.vx", "31..26=0x38", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xe0006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmulu.vx@L445", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwmulsu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xe8002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmulsu.vv", "origin": {"line": 408, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwmulsu.vv     31..26=0x3a vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwmulsu.vv", "31..26=0x3a", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xe8002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmulsu.vv@L408", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwmulsu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xe8006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmulsu.vx", "origin": {"line": 446, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwmulsu.vx     31..26=0x3a vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwmulsu.vx", "31..26=0x3a", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xe8006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmulsu.vx@L446", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwmul_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xec002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmul.vv", "origin": {"line": 409, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwmul.vv       31..26=0x3b vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwmul.vv", "31..26=0x3b", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xec002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmul.vv@L409", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwmul_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xec006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmul.vx", "origin": {"line": 447, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwmul.vx       31..26=0x3b vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwmul.vx", "31..26=0x3b", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xec006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmul.vx@L447", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vwmulu_vv () = test_opivv_form ~mnemonic:"vwmulu.vv" vwmulu_vv_json
let test_vwmulu_vx () = test_opivx_form ~mnemonic:"vwmulu.vx" vwmulu_vx_json
let test_vwmulsu_vv () = test_opivv_form ~mnemonic:"vwmulsu.vv" vwmulsu_vv_json
let test_vwmulsu_vx () = test_opivx_form ~mnemonic:"vwmulsu.vx" vwmulsu_vx_json
let test_vwmul_vv () = test_opivv_form ~mnemonic:"vwmul.vv" vwmul_vv_json
let test_vwmul_vx () = test_opivx_form ~mnemonic:"vwmul.vx" vwmul_vx_json

let vsext_vf2_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc0ff07f", "value": "0x4803a057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsext.vf2", "origin": {"line": 366, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vd"], "raw": {"line": "vsext.vf2      31..26=0x12 vm vs2 19..15=7 14..12=0x2 vd 6..0=0x57", "tokens": ["vsext.vf2", "31..26=0x12", "vm", "vs2", "19..15=7", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc0ff07f", "match": "0x4803a057", "variable_fields": ["vm", "vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsext.vf2@L366", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsext_vf4_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc0ff07f", "value": "0x4802a057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsext.vf4", "origin": {"line": 364, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vd"], "raw": {"line": "vsext.vf4      31..26=0x12 vm vs2 19..15=5 14..12=0x2 vd 6..0=0x57", "tokens": ["vsext.vf4", "31..26=0x12", "vm", "vs2", "19..15=5", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc0ff07f", "match": "0x4802a057", "variable_fields": ["vm", "vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsext.vf4@L364", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vsext_vf8_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc0ff07f", "value": "0x4801a057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsext.vf8", "origin": {"line": 362, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vd"], "raw": {"line": "vsext.vf8      31..26=0x12 vm vs2 19..15=3 14..12=0x2 vd 6..0=0x57", "tokens": ["vsext.vf8", "31..26=0x12", "vm", "vs2", "19..15=3", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc0ff07f", "match": "0x4801a057", "variable_fields": ["vm", "vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsext.vf8@L362", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vzext_vf2_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc0ff07f", "value": "0x48032057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vzext.vf2", "origin": {"line": 365, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vd"], "raw": {"line": "vzext.vf2      31..26=0x12 vm vs2 19..15=6 14..12=0x2 vd 6..0=0x57", "tokens": ["vzext.vf2", "31..26=0x12", "vm", "vs2", "19..15=6", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc0ff07f", "match": "0x48032057", "variable_fields": ["vm", "vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vzext.vf2@L365", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vzext_vf4_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc0ff07f", "value": "0x48022057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vzext.vf4", "origin": {"line": 363, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vd"], "raw": {"line": "vzext.vf4      31..26=0x12 vm vs2 19..15=4 14..12=0x2 vd 6..0=0x57", "tokens": ["vzext.vf4", "31..26=0x12", "vm", "vs2", "19..15=4", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc0ff07f", "match": "0x48022057", "variable_fields": ["vm", "vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vzext.vf4@L363", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vzext_vf8_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc0ff07f", "value": "0x48012057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vzext.vf8", "origin": {"line": 361, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vd"], "raw": {"line": "vzext.vf8      31..26=0x12 vm vs2 19..15=2 14..12=0x2 vd 6..0=0x57", "tokens": ["vzext.vf8", "31..26=0x12", "vm", "vs2", "19..15=2", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc0ff07f", "match": "0x48012057", "variable_fields": ["vm", "vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vzext.vf8@L361", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vsext_vf2 () = test_vext_form ~mnemonic:"vsext.vf2" vsext_vf2_json
let test_vsext_vf4 () = test_vext_form ~mnemonic:"vsext.vf4" vsext_vf4_json
let test_vsext_vf8 () = test_vext_form ~mnemonic:"vsext.vf8" vsext_vf8_json
let test_vzext_vf2 () = test_vext_form ~mnemonic:"vzext.vf2" vzext_vf2_json
let test_vzext_vf4 () = test_vext_form ~mnemonic:"vzext.vf4" vzext_vf4_json
let test_vzext_vf8 () = test_vext_form ~mnemonic:"vzext.vf8" vzext_vf8_json

(* vmand/vmandn/vmor/vmxor/vmorn/vmnand/vmnor/vmxnor: OP-V's mask-register
   logical family ([.mm]), normalized by {!Isa_norm_riscv.mm_form} - the
   same [rd, rs2, rs1] all-vector-register shape [test_opivv_form] already
   covers, so it is reused unchanged (see {!Isa_norm_riscv.mm_form}'s own
   comment for why the *implementation* still needs a dedicated function:
   there is no masked [, v0.t] sibling, a fact [test_opivv_form] does not
   assert either way). Taken verbatim from the checked-in riscv64.jsonl
   (identical in riscv32.jsonl, confirmed). *)
let vmand_mm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x66002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmand.mm", "origin": {"line": 370, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmand.mm       31..26=0x19 25=1 vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmand.mm", "31..26=0x19", "25=1", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x66002057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmand.mm@L370", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmandn_mm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x62002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmandn.mm", "origin": {"line": 369, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmandn.mm      31..26=0x18 25=1 vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmandn.mm", "31..26=0x18", "25=1", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x62002057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmandn.mm@L369", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmor_mm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x6a002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmor.mm", "origin": {"line": 371, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmor.mm        31..26=0x1a 25=1 vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmor.mm", "31..26=0x1a", "25=1", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x6a002057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmor.mm@L371", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmxor_mm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x6e002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmxor.mm", "origin": {"line": 372, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmxor.mm       31..26=0x1b 25=1 vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmxor.mm", "31..26=0x1b", "25=1", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x6e002057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmxor.mm@L372", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmorn_mm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x72002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmorn.mm", "origin": {"line": 373, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmorn.mm       31..26=0x1c 25=1 vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmorn.mm", "31..26=0x1c", "25=1", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x72002057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmorn.mm@L373", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmnand_mm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x76002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmnand.mm", "origin": {"line": 374, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmnand.mm      31..26=0x1d 25=1 vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmnand.mm", "31..26=0x1d", "25=1", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x76002057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmnand.mm@L374", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmnor_mm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x7a002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmnor.mm", "origin": {"line": 375, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmnor.mm       31..26=0x1e 25=1 vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmnor.mm", "31..26=0x1e", "25=1", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x7a002057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmnor.mm@L375", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmxnor_mm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x7e002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmxnor.mm", "origin": {"line": 376, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmxnor.mm      31..26=0x1f 25=1 vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmxnor.mm", "31..26=0x1f", "25=1", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x7e002057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmxnor.mm@L376", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmand_mm () = test_opivv_form ~mnemonic:"vmand.mm" vmand_mm_json
let test_vmandn_mm () = test_opivv_form ~mnemonic:"vmandn.mm" vmandn_mm_json
let test_vmor_mm () = test_opivv_form ~mnemonic:"vmor.mm" vmor_mm_json
let test_vmxor_mm () = test_opivv_form ~mnemonic:"vmxor.mm" vmxor_mm_json
let test_vmorn_mm () = test_opivv_form ~mnemonic:"vmorn.mm" vmorn_mm_json
let test_vmnand_mm () = test_opivv_form ~mnemonic:"vmnand.mm" vmnand_mm_json
let test_vmnor_mm () = test_opivv_form ~mnemonic:"vmnor.mm" vmnor_mm_json
let test_vmxnor_mm () = test_opivv_form ~mnemonic:"vmxnor.mm" vmxnor_mm_json

let vid_v_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfdfff07f", "value": "0x5008a057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vid.v", "origin": {"line": 382, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vd"], "raw": {"line": "vid.v          31..26=0x14 vm 24..20=0 19..15=0x11 14..12=0x2 vd 6..0=0x57", "tokens": ["vid.v", "31..26=0x14", "vm", "24..20=0", "19..15=0x11", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfdfff07f", "match": "0x5008a057", "variable_fields": ["vm", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vid.v@L382", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vid_v () = test_vid_form vid_v_json

let viota_m_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc0ff07f", "value": "0x50082057", "width_bits": 32}, "kind": "instruction-form", "native_name": "viota.m", "origin": {"line": 381, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vd"], "raw": {"line": "viota.m        31..26=0x14 vm vs2 19..15=0x10 14..12=0x2 vd 6..0=0x57", "tokens": ["viota.m", "31..26=0x14", "vm", "vs2", "19..15=0x10", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc0ff07f", "match": "0x50082057", "variable_fields": ["vm", "vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:viota.m@L381", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_viota_m () = test_vext_form ~mnemonic:"viota.m" viota_m_json

let vcompress_vm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x5e002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vcompress.vm", "origin": {"line": 368, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vcompress.vm   31..26=0x17 25=1 vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vcompress.vm", "31..26=0x17", "25=1", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x5e002057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vcompress.vm@L368", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vcompress_vm () = test_opivv_form ~mnemonic:"vcompress.vm" vcompress_vm_json

let vmsbf_m_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc0ff07f", "value": "0x5000a057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsbf.m", "origin": {"line": 378, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vd"], "raw": {"line": "vmsbf.m        31..26=0x14 vm vs2 19..15=0x01 14..12=0x2 vd 6..0=0x57", "tokens": ["vmsbf.m", "31..26=0x14", "vm", "vs2", "19..15=0x01", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc0ff07f", "match": "0x5000a057", "variable_fields": ["vm", "vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsbf.m@L378", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmsbf_m () = test_vext_form ~mnemonic:"vmsbf.m" vmsbf_m_json

let vmsif_m_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc0ff07f", "value": "0x5001a057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsif.m", "origin": {"line": 380, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vd"], "raw": {"line": "vmsif.m        31..26=0x14 vm vs2 19..15=0x03 14..12=0x2 vd 6..0=0x57", "tokens": ["vmsif.m", "31..26=0x14", "vm", "vs2", "19..15=0x03", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc0ff07f", "match": "0x5001a057", "variable_fields": ["vm", "vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsif.m@L380", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmsif_m () = test_vext_form ~mnemonic:"vmsif.m" vmsif_m_json

let vmsof_m_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc0ff07f", "value": "0x50012057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsof.m", "origin": {"line": 379, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vd"], "raw": {"line": "vmsof.m        31..26=0x14 vm vs2 19..15=0x02 14..12=0x2 vd 6..0=0x57", "tokens": ["vmsof.m", "31..26=0x14", "vm", "vs2", "19..15=0x02", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc0ff07f", "match": "0x50012057", "variable_fields": ["vm", "vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsof.m@L379", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmsof_m () = test_vext_form ~mnemonic:"vmsof.m" vmsof_m_json

let vcpop_m_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "rd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc0ff07f", "value": "0x40082057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vcpop.m", "origin": {"line": 383, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rd"], "raw": {"line": "vcpop.m        31..26=0x10 vm vs2 19..15=0x10 14..12=0x2 rd 6..0=0x57", "tokens": ["vcpop.m", "31..26=0x10", "vm", "vs2", "19..15=0x10", "14..12=0x2", "rd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc0ff07f", "match": "0x40082057", "variable_fields": ["vm", "vs2", "rd"]}}, "record_id": "riscv-opcodes:rv_v:vcpop.m@L383", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vcpop_m () = test_v_to_x_unary_form ~mnemonic:"vcpop.m" vcpop_m_json

let vfirst_m_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "rd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc0ff07f", "value": "0x4008a057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vfirst.m", "origin": {"line": 384, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rd"], "raw": {"line": "vfirst.m       31..26=0x10 vm vs2 19..15=0x11 14..12=0x2 rd 6..0=0x57", "tokens": ["vfirst.m", "31..26=0x10", "vm", "vs2", "19..15=0x11", "14..12=0x2", "rd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc0ff07f", "match": "0x4008a057", "variable_fields": ["vm", "vs2", "rd"]}}, "record_id": "riscv-opcodes:rv_v:vfirst.m@L384", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vfirst_m () = test_v_to_x_unary_form ~mnemonic:"vfirst.m" vfirst_m_json

let vadc_vvm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x40000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vadc.vvm", "origin": {"line": 272, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vadc.vvm       31..26=0x10 25=0 vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vadc.vvm", "31..26=0x10", "25=0", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x40000057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vadc.vvm@L272", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vadc_vvm () = test_carry_m_vv_form ~mnemonic:"vadc.vvm" vadc_vvm_json

let vadc_vxm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x40004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vadc.vxm", "origin": {"line": 227, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "rs1", "vd"], "raw": {"line": "vadc.vxm       31..26=0x10 25=0 vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vadc.vxm", "31..26=0x10", "25=0", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x40004057", "variable_fields": ["vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vadc.vxm@L227", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vadc_vxm () = test_carry_m_vx_form ~mnemonic:"vadc.vxm" vadc_vxm_json

let vadc_vim_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x40003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vadc.vim", "origin": {"line": 315, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "simm5", "vd"], "raw": {"line": "vadc.vim       31..26=0x10 25=0 vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vadc.vim", "31..26=0x10", "25=0", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x40003057", "variable_fields": ["vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vadc.vim@L315", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vadc_vim () = test_carry_m_vi_form ~mnemonic:"vadc.vim" vadc_vim_json

let vmadc_vvm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x44000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmadc.vvm", "origin": {"line": 273, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmadc.vvm      31..26=0x11 25=0 vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmadc.vvm", "31..26=0x11", "25=0", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x44000057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmadc.vvm@L273", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmadc_vvm () = test_carry_m_vv_form ~mnemonic:"vmadc.vvm" vmadc_vvm_json

let vmadc_vxm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x44004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmadc.vxm", "origin": {"line": 228, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "rs1", "vd"], "raw": {"line": "vmadc.vxm      31..26=0x11 25=0 vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmadc.vxm", "31..26=0x11", "25=0", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x44004057", "variable_fields": ["vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmadc.vxm@L228", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmadc_vxm () = test_carry_m_vx_form ~mnemonic:"vmadc.vxm" vmadc_vxm_json

let vmadc_vim_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x44003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmadc.vim", "origin": {"line": 316, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "simm5", "vd"], "raw": {"line": "vmadc.vim      31..26=0x11 25=0 vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vmadc.vim", "31..26=0x11", "25=0", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x44003057", "variable_fields": ["vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmadc.vim@L316", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmadc_vim () = test_carry_m_vi_form ~mnemonic:"vmadc.vim" vmadc_vim_json

let vmadc_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x46000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmadc.vv", "origin": {"line": 274, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmadc.vv       31..26=0x11 25=1 vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmadc.vv", "31..26=0x11", "25=1", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x46000057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmadc.vv@L274", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmadc_vv () = test_opivv_form ~mnemonic:"vmadc.vv" vmadc_vv_json

let vmadc_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x46004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmadc.vx", "origin": {"line": 229, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "rs1", "vd"], "raw": {"line": "vmadc.vx       31..26=0x11 25=1 vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmadc.vx", "31..26=0x11", "25=1", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x46004057", "variable_fields": ["vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmadc.vx@L229", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmadc_vx () = test_opivx_form ~mnemonic:"vmadc.vx" vmadc_vx_json

let vmadc_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x46003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmadc.vi", "origin": {"line": 317, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "simm5", "vd"], "raw": {"line": "vmadc.vi       31..26=0x11 25=1 vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vmadc.vi", "31..26=0x11", "25=1", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x46003057", "variable_fields": ["vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmadc.vi@L317", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmadc_vi () = test_opivi_form ~mnemonic:"vmadc.vi" vmadc_vi_json

let vsbc_vvm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x48000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsbc.vvm", "origin": {"line": 275, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vsbc.vvm       31..26=0x12 25=0 vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vsbc.vvm", "31..26=0x12", "25=0", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x48000057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsbc.vvm@L275", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vsbc_vvm () = test_carry_m_vv_form ~mnemonic:"vsbc.vvm" vsbc_vvm_json

let vsbc_vxm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x48004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsbc.vxm", "origin": {"line": 230, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "rs1", "vd"], "raw": {"line": "vsbc.vxm       31..26=0x12 25=0 vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vsbc.vxm", "31..26=0x12", "25=0", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x48004057", "variable_fields": ["vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsbc.vxm@L230", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vsbc_vxm () = test_carry_m_vx_form ~mnemonic:"vsbc.vxm" vsbc_vxm_json

let vmsbc_vvm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x4c000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsbc.vvm", "origin": {"line": 276, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmsbc.vvm      31..26=0x13 25=0 vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmsbc.vvm", "31..26=0x13", "25=0", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x4c000057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsbc.vvm@L276", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmsbc_vvm () = test_carry_m_vv_form ~mnemonic:"vmsbc.vvm" vmsbc_vvm_json

let vmsbc_vxm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x4c004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsbc.vxm", "origin": {"line": 231, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "rs1", "vd"], "raw": {"line": "vmsbc.vxm      31..26=0x13 25=0 vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmsbc.vxm", "31..26=0x13", "25=0", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x4c004057", "variable_fields": ["vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsbc.vxm@L231", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmsbc_vxm () = test_carry_m_vx_form ~mnemonic:"vmsbc.vxm" vmsbc_vxm_json

let vmsbc_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x4e000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsbc.vv", "origin": {"line": 277, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmsbc.vv       31..26=0x13 25=1 vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmsbc.vv", "31..26=0x13", "25=1", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x4e000057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsbc.vv@L277", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmsbc_vv () = test_opivv_form ~mnemonic:"vmsbc.vv" vmsbc_vv_json

let vmsbc_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x4e004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsbc.vx", "origin": {"line": 232, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "rs1", "vd"], "raw": {"line": "vmsbc.vx       31..26=0x13 25=1 vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmsbc.vx", "31..26=0x13", "25=1", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x4e004057", "variable_fields": ["vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsbc.vx@L232", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmsbc_vx () = test_opivx_form ~mnemonic:"vmsbc.vx" vmsbc_vx_json

let vmerge_vvm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x5c000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmerge.vvm", "origin": {"line": 278, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vs1", "vd"], "raw": {"line": "vmerge.vvm     31..26=0x17 25=0 vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmerge.vvm", "31..26=0x17", "25=0", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x5c000057", "variable_fields": ["vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmerge.vvm@L278", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmerge_vvm () = test_carry_m_vv_form ~mnemonic:"vmerge.vvm" vmerge_vvm_json

let vmerge_vxm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x5c004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmerge.vxm", "origin": {"line": 233, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "rs1", "vd"], "raw": {"line": "vmerge.vxm     31..26=0x17 25=0 vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmerge.vxm", "31..26=0x17", "25=0", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x5c004057", "variable_fields": ["vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmerge.vxm@L233", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmerge_vxm () = test_carry_m_vx_form ~mnemonic:"vmerge.vxm" vmerge_vxm_json

let vmerge_vim_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x5c003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmerge.vim", "origin": {"line": 318, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "simm5", "vd"], "raw": {"line": "vmerge.vim     31..26=0x17 25=0 vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vmerge.vim", "31..26=0x17", "25=0", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x5c003057", "variable_fields": ["vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmerge.vim@L318", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmerge_vim () = test_carry_m_vi_form ~mnemonic:"vmerge.vim" vmerge_vim_json

let vmv_x_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "rd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe0ff07f", "value": "0x42002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmv.x.s", "origin": {"line": 357, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "rd"], "raw": {"line": "vmv.x.s        31..26=0x10 25=1 vs2 19..15=0 14..12=0x2 rd 6..0=0x57", "tokens": ["vmv.x.s", "31..26=0x10", "25=1", "vs2", "19..15=0", "14..12=0x2", "rd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe0ff07f", "match": "0x42002057", "variable_fields": ["vs2", "rd"]}}, "record_id": "riscv-opcodes:rv_v:vmv.x.s@L357", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmv_x_s () = test_mv_x_s_form vmv_x_s_json

let vmv_s_x_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x42006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmv.s.x", "origin": {"line": 420, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["rs1", "vd"], "raw": {"line": "vmv.s.x        31..26=0x10 25=1 24..20=0 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vmv.s.x", "31..26=0x10", "25=1", "24..20=0", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x42006057", "variable_fields": ["rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmv.s.x@L420", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmv_s_x () = test_mv_s_x_form vmv_s_x_json

let vmv_v_v_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x5e000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmv.v.v", "origin": {"line": 279, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs1", "vd"], "raw": {"line": "vmv.v.v        31..26=0x17 25=1 24..20=0 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmv.v.v", "31..26=0x17", "25=1", "24..20=0", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x5e000057", "variable_fields": ["vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmv.v.v@L279", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmv_v_v () = test_vmv_v_v_form vmv_v_v_json

let vmv_v_x_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x5e004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmv.v.x", "origin": {"line": 234, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["rs1", "vd"], "raw": {"line": "vmv.v.x        31..26=0x17 25=1 24..20=0 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmv.v.x", "31..26=0x17", "25=1", "24..20=0", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x5e004057", "variable_fields": ["rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmv.v.x@L234", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmv_v_x () = test_vmv_v_x_form vmv_v_x_json

let vmv_v_i_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x5e003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmv.v.i", "origin": {"line": 319, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["simm5", "vd"], "raw": {"line": "vmv.v.i        31..26=0x17 25=1 24..20=0 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vmv.v.i", "31..26=0x17", "25=1", "24..20=0", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x5e003057", "variable_fields": ["simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmv.v.i@L319", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmv_v_i () = test_vmv_v_i_form vmv_v_i_json

let vmv1r_v_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe0ff07f", "value": "0x9e003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmv1r.v", "origin": {"line": 330, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vd"], "raw": {"line": "vmv1r.v        31..26=0x27 25=1 vs2 19..15=0 14..12=0x3 vd 6..0=0x57", "tokens": ["vmv1r.v", "31..26=0x27", "25=1", "vs2", "19..15=0", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe0ff07f", "match": "0x9e003057", "variable_fields": ["vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmv1r.v@L330", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmv1r_v () = test_whole_reg_move_form ~mnemonic:"vmv1r.v" vmv1r_v_json

let vmv2r_v_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe0ff07f", "value": "0x9e00b057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmv2r.v", "origin": {"line": 331, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vd"], "raw": {"line": "vmv2r.v        31..26=0x27 25=1 vs2 19..15=1 14..12=0x3 vd 6..0=0x57", "tokens": ["vmv2r.v", "31..26=0x27", "25=1", "vs2", "19..15=1", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe0ff07f", "match": "0x9e00b057", "variable_fields": ["vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmv2r.v@L331", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmv2r_v () = test_whole_reg_move_form ~mnemonic:"vmv2r.v" vmv2r_v_json

let vmv4r_v_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe0ff07f", "value": "0x9e01b057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmv4r.v", "origin": {"line": 332, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vd"], "raw": {"line": "vmv4r.v        31..26=0x27 25=1 vs2 19..15=3 14..12=0x3 vd 6..0=0x57", "tokens": ["vmv4r.v", "31..26=0x27", "25=1", "vs2", "19..15=3", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe0ff07f", "match": "0x9e01b057", "variable_fields": ["vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmv4r.v@L332", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmv4r_v () = test_whole_reg_move_form ~mnemonic:"vmv4r.v" vmv4r_v_json

let vmv8r_v_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 25, "name": "bits[25:25]", "width": 1}, {"lsb": 15, "name": "bits[19:15]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfe0ff07f", "value": "0x9e03b057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmv8r.v", "origin": {"line": 333, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vs2", "vd"], "raw": {"line": "vmv8r.v        31..26=0x27 25=1 vs2 19..15=7 14..12=0x3 vd 6..0=0x57", "tokens": ["vmv8r.v", "31..26=0x27", "25=1", "vs2", "19..15=7", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfe0ff07f", "match": "0x9e03b057", "variable_fields": ["vs2", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmv8r.v@L333", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmv8r_v () = test_whole_reg_move_form ~mnemonic:"vmv8r.v" vmv8r_v_json

let vsmul_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x9c000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsmul.vv", "origin": {"line": 292, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vsmul.vv       31..26=0x27 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vsmul.vv", "31..26=0x27", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x9c000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsmul.vv@L292", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vsmul_vv () = test_opivv_form ~mnemonic:"vsmul.vv" vsmul_vv_json

let vsmul_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x9c004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vsmul.vx", "origin": {"line": 249, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vsmul.vx       31..26=0x27 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vsmul.vx", "31..26=0x27", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x9c004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vsmul.vx@L249", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vsmul_vx () = test_opivx_form ~mnemonic:"vsmul.vx" vsmul_vx_json

(* vredsum/vredand/vredor/vredxor/vredminu/vredmin/vredmaxu/vredmax and
   vwredsumu/vwredsum: OP-V's vector-reduction family, dispatched by
   {!Isa_norm_riscv.opmvv_mnemonics}/[opivv_mnemonics] straight to the same
   {!Isa_norm_riscv.opivv_form} [test_opivv_form] already covers - a real,
   selectable mask, unlike the mask-register-logical family above. Taken
   verbatim from the checked-in riscv64.jsonl (identical in riscv32.jsonl,
   confirmed). *)
let vredsum_vs_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x2057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vredsum.vs", "origin": {"line": 344, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vredsum.vs     31..26=0x00 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vredsum.vs", "31..26=0x00", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x2057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vredsum.vs@L344", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vredand_vs_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x4002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vredand.vs", "origin": {"line": 345, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vredand.vs     31..26=0x01 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vredand.vs", "31..26=0x01", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x4002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vredand.vs@L345", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vredor_vs_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x8002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vredor.vs", "origin": {"line": 346, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vredor.vs      31..26=0x02 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vredor.vs", "31..26=0x02", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x8002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vredor.vs@L346", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vredxor_vs_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xc002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vredxor.vs", "origin": {"line": 347, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vredxor.vs     31..26=0x03 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vredxor.vs", "31..26=0x03", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xc002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vredxor.vs@L347", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vredminu_vs_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x10002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vredminu.vs", "origin": {"line": 348, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vredminu.vs    31..26=0x04 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vredminu.vs", "31..26=0x04", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x10002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vredminu.vs@L348", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vredmin_vs_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x14002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vredmin.vs", "origin": {"line": 349, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vredmin.vs     31..26=0x05 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vredmin.vs", "31..26=0x05", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x14002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vredmin.vs@L349", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vredmaxu_vs_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x18002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vredmaxu.vs", "origin": {"line": 350, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vredmaxu.vs    31..26=0x06 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vredmaxu.vs", "31..26=0x06", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x18002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vredmaxu.vs@L350", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vredmax_vs_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x1c002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vredmax.vs", "origin": {"line": 351, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vredmax.vs     31..26=0x07 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vredmax.vs", "31..26=0x07", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x1c002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vredmax.vs@L351", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwredsumu_vs_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xc0000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwredsumu.vs", "origin": {"line": 302, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwredsumu.vs   31..26=0x30 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vwredsumu.vs", "31..26=0x30", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xc0000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwredsumu.vs@L302", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwredsum_vs_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xc4000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwredsum.vs", "origin": {"line": 303, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwredsum.vs    31..26=0x31 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vwredsum.vs", "31..26=0x31", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xc4000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwredsum.vs@L303", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vredsum_vs () = test_opivv_form ~mnemonic:"vredsum.vs" vredsum_vs_json
let test_vredand_vs () = test_opivv_form ~mnemonic:"vredand.vs" vredand_vs_json
let test_vredor_vs () = test_opivv_form ~mnemonic:"vredor.vs" vredor_vs_json
let test_vredxor_vs () = test_opivv_form ~mnemonic:"vredxor.vs" vredxor_vs_json
let test_vredminu_vs () = test_opivv_form ~mnemonic:"vredminu.vs" vredminu_vs_json
let test_vredmin_vs () = test_opivv_form ~mnemonic:"vredmin.vs" vredmin_vs_json
let test_vredmaxu_vs () = test_opivv_form ~mnemonic:"vredmaxu.vs" vredmaxu_vs_json
let test_vredmax_vs () = test_opivv_form ~mnemonic:"vredmax.vs" vredmax_vs_json
let test_vwredsumu_vs () = test_opivv_form ~mnemonic:"vwredsumu.vs" vwredsumu_vs_json
let test_vwredsum_vs () = test_opivv_form ~mnemonic:"vwredsum.vs" vwredsum_vs_json

(* vmseq/vmsne/vmsltu/vmslt/vmsleu/vmsle/vmsgtu/vmsgt: OP-V's mask-writing
   comparison family, the full OPIVV/OPIVX/OPIVI shape (minus [.vv] for
   vmsgtu/vmsgt and [.vi] for vmsltu/vmslt), dispatched by
   {!Isa_norm_riscv.opivv_mnemonics}/[opivx_mnemonics]/[opivi_mnemonics] to
   the same forms {!test_opivv_form}/[test_opivx_form]/[test_opivi_form]
   already cover. Taken verbatim from the checked-in riscv64.jsonl
   (identical in riscv32.jsonl, confirmed). *)
let vmseq_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x60000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmseq.vv", "origin": {"line": 280, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmseq.vv       31..26=0x18 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmseq.vv", "31..26=0x18", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x60000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmseq.vv@L280", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmseq_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x60004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmseq.vx", "origin": {"line": 235, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmseq.vx       31..26=0x18 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmseq.vx", "31..26=0x18", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x60004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmseq.vx@L235", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmseq_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x60003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmseq.vi", "origin": {"line": 320, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vmseq.vi       31..26=0x18 vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vmseq.vi", "31..26=0x18", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x60003057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmseq.vi@L320", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsne_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x64000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsne.vv", "origin": {"line": 281, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmsne.vv       31..26=0x19 vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmsne.vv", "31..26=0x19", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x64000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsne.vv@L281", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsne_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x64004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsne.vx", "origin": {"line": 236, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmsne.vx       31..26=0x19 vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmsne.vx", "31..26=0x19", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x64004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsne.vx@L236", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsne_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x64003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsne.vi", "origin": {"line": 321, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vmsne.vi       31..26=0x19 vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vmsne.vi", "31..26=0x19", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x64003057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsne.vi@L321", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsltu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x68000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsltu.vv", "origin": {"line": 282, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmsltu.vv      31..26=0x1a vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmsltu.vv", "31..26=0x1a", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x68000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsltu.vv@L282", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsltu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x68004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsltu.vx", "origin": {"line": 237, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmsltu.vx      31..26=0x1a vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmsltu.vx", "31..26=0x1a", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x68004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsltu.vx@L237", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmslt_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x6c000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmslt.vv", "origin": {"line": 283, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmslt.vv       31..26=0x1b vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmslt.vv", "31..26=0x1b", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x6c000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmslt.vv@L283", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmslt_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x6c004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmslt.vx", "origin": {"line": 238, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmslt.vx       31..26=0x1b vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmslt.vx", "31..26=0x1b", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x6c004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmslt.vx@L238", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsleu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x70000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsleu.vv", "origin": {"line": 284, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmsleu.vv      31..26=0x1c vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmsleu.vv", "31..26=0x1c", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x70000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsleu.vv@L284", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsleu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x70004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsleu.vx", "origin": {"line": 239, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmsleu.vx      31..26=0x1c vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmsleu.vx", "31..26=0x1c", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x70004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsleu.vx@L239", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsleu_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x70003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsleu.vi", "origin": {"line": 322, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vmsleu.vi      31..26=0x1c vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vmsleu.vi", "31..26=0x1c", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x70003057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsleu.vi@L322", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsle_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x74000057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsle.vv", "origin": {"line": 285, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmsle.vv       31..26=0x1d vm vs2 vs1 14..12=0x0 vd 6..0=0x57", "tokens": ["vmsle.vv", "31..26=0x1d", "vm", "vs2", "vs1", "14..12=0x0", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x74000057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsle.vv@L285", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsle_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x74004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsle.vx", "origin": {"line": 240, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmsle.vx       31..26=0x1d vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmsle.vx", "31..26=0x1d", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x74004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsle.vx@L240", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsle_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x74003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsle.vi", "origin": {"line": 323, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vmsle.vi       31..26=0x1d vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vmsle.vi", "31..26=0x1d", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x74003057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsle.vi@L323", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsgtu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x78004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsgtu.vx", "origin": {"line": 241, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmsgtu.vx      31..26=0x1e vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmsgtu.vx", "31..26=0x1e", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x78004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsgtu.vx@L241", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsgtu_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x78003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsgtu.vi", "origin": {"line": 324, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vmsgtu.vi      31..26=0x1e vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vmsgtu.vi", "31..26=0x1e", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x78003057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsgtu.vi@L324", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsgt_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x7c004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsgt.vx", "origin": {"line": 242, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmsgt.vx       31..26=0x1f vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vmsgt.vx", "31..26=0x1f", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x7c004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsgt.vx@L242", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmsgt_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "simm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x7c003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmsgt.vi", "origin": {"line": 325, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "simm5", "vd"], "raw": {"line": "vmsgt.vi       31..26=0x1f vm vs2 simm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vmsgt.vi", "31..26=0x1f", "vm", "vs2", "simm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x7c003057", "variable_fields": ["vm", "vs2", "simm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmsgt.vi@L325", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmseq_vv () = test_opivv_form ~mnemonic:"vmseq.vv" vmseq_vv_json
let test_vmseq_vx () = test_opivx_form ~mnemonic:"vmseq.vx" vmseq_vx_json
let test_vmseq_vi () = test_opivi_form ~mnemonic:"vmseq.vi" vmseq_vi_json
let test_vmsne_vv () = test_opivv_form ~mnemonic:"vmsne.vv" vmsne_vv_json
let test_vmsne_vx () = test_opivx_form ~mnemonic:"vmsne.vx" vmsne_vx_json
let test_vmsne_vi () = test_opivi_form ~mnemonic:"vmsne.vi" vmsne_vi_json
let test_vmsltu_vv () = test_opivv_form ~mnemonic:"vmsltu.vv" vmsltu_vv_json
let test_vmsltu_vx () = test_opivx_form ~mnemonic:"vmsltu.vx" vmsltu_vx_json
let test_vmslt_vv () = test_opivv_form ~mnemonic:"vmslt.vv" vmslt_vv_json
let test_vmslt_vx () = test_opivx_form ~mnemonic:"vmslt.vx" vmslt_vx_json
let test_vmsleu_vv () = test_opivv_form ~mnemonic:"vmsleu.vv" vmsleu_vv_json
let test_vmsleu_vx () = test_opivx_form ~mnemonic:"vmsleu.vx" vmsleu_vx_json
let test_vmsleu_vi () = test_opivi_form ~mnemonic:"vmsleu.vi" vmsleu_vi_json
let test_vmsle_vv () = test_opivv_form ~mnemonic:"vmsle.vv" vmsle_vv_json
let test_vmsle_vx () = test_opivx_form ~mnemonic:"vmsle.vx" vmsle_vx_json
let test_vmsle_vi () = test_opivi_form ~mnemonic:"vmsle.vi" vmsle_vi_json
let test_vmsgtu_vx () = test_opivx_form ~mnemonic:"vmsgtu.vx" vmsgtu_vx_json
let test_vmsgtu_vi () = test_opivi_form ~mnemonic:"vmsgtu.vi" vmsgtu_vi_json
let test_vmsgt_vx () = test_opivx_form ~mnemonic:"vmsgt.vx" vmsgt_vx_json
let test_vmsgt_vi () = test_opivi_form ~mnemonic:"vmsgt.vi" vmsgt_vi_json

(* vslideup/vslidedown/vslide1up/vslide1down: OP-V's slide family,
   dispatched by {!Isa_norm_riscv.opivx_mnemonics}/[opivi_zimm5_mnemonics]/
   [opmvx_mnemonics] to the same forms {!test_opivx_form}/[test_opivi_form]
   already cover. Taken verbatim from the checked-in riscv64.jsonl
   (identical in riscv32.jsonl, confirmed). *)
let vslideup_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x38003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vslideup.vi", "origin": {"line": 312, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "zimm5", "vd"], "raw": {"line": "vslideup.vi    31..26=0x0e vm vs2 zimm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vslideup.vi", "31..26=0x0e", "vm", "vs2", "zimm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x38003057", "variable_fields": ["vm", "vs2", "zimm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vslideup.vi@L312", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vslideup_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x38004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vslideup.vx", "origin": {"line": 224, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vslideup.vx    31..26=0x0e vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vslideup.vx", "31..26=0x0e", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x38004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vslideup.vx@L224", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vslidedown_vi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "zimm5", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x3c003057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vslidedown.vi", "origin": {"line": 313, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "zimm5", "vd"], "raw": {"line": "vslidedown.vi  31..26=0x0f vm vs2 zimm5 14..12=0x3 vd 6..0=0x57", "tokens": ["vslidedown.vi", "31..26=0x0f", "vm", "vs2", "zimm5", "14..12=0x3", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x3c003057", "variable_fields": ["vm", "vs2", "zimm5", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vslidedown.vi@L313", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vslidedown_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x3c004057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vslidedown.vx", "origin": {"line": 225, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vslidedown.vx  31..26=0x0f vm vs2 rs1 14..12=0x4 vd 6..0=0x57", "tokens": ["vslidedown.vx", "31..26=0x0f", "vm", "vs2", "rs1", "14..12=0x4", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x3c004057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vslidedown.vx@L225", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vslide1up_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x38006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vslide1up.vx", "origin": {"line": 421, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vslide1up.vx   31..26=0x0e vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vslide1up.vx", "31..26=0x0e", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x38006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vslide1up.vx@L421", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vslide1down_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0x3c006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vslide1down.vx", "origin": {"line": 422, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vslide1down.vx 31..26=0x0f vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vslide1down.vx", "31..26=0x0f", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0x3c006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vslide1down.vx@L422", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vslideup_vx () = test_opivx_form ~mnemonic:"vslideup.vx" vslideup_vx_json

let test_vslideup_vi () =
  test_opivi_form ~imm_name:"zimm5" ~signed:false ~mnemonic:"vslideup.vi" vslideup_vi_json

let test_vslidedown_vx () = test_opivx_form ~mnemonic:"vslidedown.vx" vslidedown_vx_json

let test_vslidedown_vi () =
  test_opivi_form ~imm_name:"zimm5" ~signed:false ~mnemonic:"vslidedown.vi" vslidedown_vi_json

let test_vslide1up_vx () = test_opivx_form ~mnemonic:"vslide1up.vx" vslide1up_vx_json
let test_vslide1down_vx () = test_opivx_form ~mnemonic:"vslide1down.vx" vslide1down_vx_json

let vmacc_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xb4002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmacc.vv", "origin": {"line": 396, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmacc.vv       31..26=0x2d vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmacc.vv", "31..26=0x2d", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xb4002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmacc.vv@L396", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmacc_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xb4006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmacc.vx", "origin": {"line": 434, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmacc.vx       31..26=0x2d vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vmacc.vx", "31..26=0x2d", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xb4006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmacc.vx@L434", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnmsac_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xbc002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnmsac.vv", "origin": {"line": 397, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vnmsac.vv      31..26=0x2f vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vnmsac.vv", "31..26=0x2f", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xbc002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnmsac.vv@L397", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnmsac_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xbc006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnmsac.vx", "origin": {"line": 435, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vnmsac.vx      31..26=0x2f vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vnmsac.vx", "31..26=0x2f", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xbc006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnmsac.vx@L435", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmadd_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xa4002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmadd.vv", "origin": {"line": 394, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vmadd.vv       31..26=0x29 vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vmadd.vv", "31..26=0x29", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xa4002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmadd.vv@L394", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vmadd_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xa4006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vmadd.vx", "origin": {"line": 432, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vmadd.vx       31..26=0x29 vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vmadd.vx", "31..26=0x29", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xa4006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vmadd.vx@L432", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnmsub_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xac002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnmsub.vv", "origin": {"line": 395, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vnmsub.vv      31..26=0x2b vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vnmsub.vv", "31..26=0x2b", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xac002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnmsub.vv@L395", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vnmsub_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xac006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vnmsub.vx", "origin": {"line": 433, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vnmsub.vx      31..26=0x2b vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vnmsub.vx", "31..26=0x2b", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xac006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vnmsub.vx@L433", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwmaccu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xf0002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmaccu.vv", "origin": {"line": 410, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwmaccu.vv     31..26=0x3c vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwmaccu.vv", "31..26=0x3c", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xf0002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmaccu.vv@L410", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwmaccu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xf0006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmaccu.vx", "origin": {"line": 448, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwmaccu.vx     31..26=0x3c vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwmaccu.vx", "31..26=0x3c", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xf0006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmaccu.vx@L448", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwmacc_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xf4002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmacc.vv", "origin": {"line": 411, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwmacc.vv      31..26=0x3d vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwmacc.vv", "31..26=0x3d", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xf4002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmacc.vv@L411", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwmacc_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xf4006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmacc.vx", "origin": {"line": 449, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwmacc.vx      31..26=0x3d vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwmacc.vx", "31..26=0x3d", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xf4006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmacc.vx@L449", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwmaccsu_vv_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "vs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xfc002057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmaccsu.vv", "origin": {"line": 412, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "vs1", "vd"], "raw": {"line": "vwmaccsu.vv    31..26=0x3f vm vs2 vs1 14..12=0x2 vd 6..0=0x57", "tokens": ["vwmaccsu.vv", "31..26=0x3f", "vm", "vs2", "vs1", "14..12=0x2", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xfc002057", "variable_fields": ["vm", "vs2", "vs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmaccsu.vv@L412", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwmaccsu_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xfc006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmaccsu.vx", "origin": {"line": 451, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwmaccsu.vx    31..26=0x3f vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwmaccsu.vx", "31..26=0x3f", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xfc006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmaccsu.vx@L451", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let vwmaccus_vx_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 26, "name": "bits[31:26]", "width": 6}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 25, "name": "vm", "width": 1}, {"lsb": 20, "name": "vs2", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 7, "name": "vd", "width": 5}], "kind": "fixed_bits", "mask": "0xfc00707f", "value": "0xf8006057", "width_bits": 32}, "kind": "instruction-form", "native_name": "vwmaccus.vx", "origin": {"line": 450, "path": "extensions/rv_v"}, "provenance": {"extension": "rv_v", "operands": ["vm", "vs2", "rs1", "vd"], "raw": {"line": "vwmaccus.vx    31..26=0x3e vm vs2 rs1 14..12=0x6 vd 6..0=0x57", "tokens": ["vwmaccus.vx", "31..26=0x3e", "vm", "vs2", "rs1", "14..12=0x6", "vd", "6..0=0x57"]}, "upstream-resolved": {"mask": "0xfc00707f", "match": "0xf8006057", "variable_fields": ["vm", "vs2", "rs1", "vd"]}}, "record_id": "riscv-opcodes:rv_v:vwmaccus.vx@L450", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_vmacc_vv () = test_opmacc_vv_form ~mnemonic:"vmacc.vv" vmacc_vv_json
let test_vmacc_vx () = test_opmacc_vx_form ~mnemonic:"vmacc.vx" vmacc_vx_json
let test_vnmsac_vv () = test_opmacc_vv_form ~mnemonic:"vnmsac.vv" vnmsac_vv_json
let test_vnmsac_vx () = test_opmacc_vx_form ~mnemonic:"vnmsac.vx" vnmsac_vx_json
let test_vmadd_vv () = test_opmacc_vv_form ~mnemonic:"vmadd.vv" vmadd_vv_json
let test_vmadd_vx () = test_opmacc_vx_form ~mnemonic:"vmadd.vx" vmadd_vx_json
let test_vnmsub_vv () = test_opmacc_vv_form ~mnemonic:"vnmsub.vv" vnmsub_vv_json
let test_vnmsub_vx () = test_opmacc_vx_form ~mnemonic:"vnmsub.vx" vnmsub_vx_json
let test_vwmaccu_vv () = test_opmacc_vv_form ~mnemonic:"vwmaccu.vv" vwmaccu_vv_json
let test_vwmaccu_vx () = test_opmacc_vx_form ~mnemonic:"vwmaccu.vx" vwmaccu_vx_json
let test_vwmacc_vv () = test_opmacc_vv_form ~mnemonic:"vwmacc.vv" vwmacc_vv_json
let test_vwmacc_vx () = test_opmacc_vx_form ~mnemonic:"vwmacc.vx" vwmacc_vx_json
let test_vwmaccsu_vv () = test_opmacc_vv_form ~mnemonic:"vwmaccsu.vv" vwmaccsu_vv_json
let test_vwmaccsu_vx () = test_opmacc_vx_form ~mnemonic:"vwmaccsu.vx" vwmaccsu_vx_json
let test_vwmaccus_vx () = test_opmacc_vx_form ~mnemonic:"vwmaccus.vx" vwmaccus_vx_json

let pack_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x8004033","width_bits":32},"kind":"instruction-form","native_name":"pack","origin":{"line":6,"path":"extensions/rv_zbkb"},"provenance":{"extension":"rv_zbkb","operands":["rd","rs1","rs2"],"raw":{"line":"pack       rd rs1 rs2 31..25=4  14..12=4 6..2=0x0C 1..0=3","tokens":["pack","rd","rs1","rs2","31..25=4","14..12=4","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x8004033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zbkb:pack@L6","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let packh_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x8007033","width_bits":32},"kind":"instruction-form","native_name":"packh","origin":{"line":7,"path":"extensions/rv_zbkb"},"provenance":{"extension":"rv_zbkb","operands":["rd","rs1","rs2"],"raw":{"line":"packh      rd rs1 rs2 31..25=4  14..12=7 6..2=0x0C 1..0=3","tokens":["packh","rd","rs1","rs2","31..25=4","14..12=7","6..2=0x0C","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x8007033","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv_zbkb:packh@L7","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let packw_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x800403b","width_bits":32},"kind":"instruction-form","native_name":"packw","origin":{"line":6,"path":"extensions/rv64_zbkb"},"provenance":{"extension":"rv64_zbkb","operands":["rd","rs1","rs2"],"raw":{"line":"packw      rd rs1 rs2 31..25=4  14..12=4 6..2=0x0E 1..0=3","tokens":["packw","rd","rs1","rs2","31..25=4","14..12=4","6..2=0x0E","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x800403b","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv64_zbkb:packw@L6","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let rolw_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x6000103b","width_bits":32},"kind":"instruction-form","native_name":"rolw","origin":{"line":4,"path":"extensions/rv64_zbb"},"provenance":{"extension":"rv64_zbb","operands":["rd","rs1","rs2"],"raw":{"line":"rolw  rd rs1 rs2                            31..25=0x30 14..12=1 6..2=0x0E 1..0=3","tokens":["rolw","rd","rs1","rs2","31..25=0x30","14..12=1","6..2=0x0E","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x6000103b","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv64_zbb:rolw@L4","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let rorw_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x6000503b","width_bits":32},"kind":"instruction-form","native_name":"rorw","origin":{"line":5,"path":"extensions/rv64_zbb"},"provenance":{"extension":"rv64_zbb","operands":["rd","rs1","rs2"],"raw":{"line":"rorw  rd rs1 rs2                            31..25=0x30 14..12=5 6..2=0x0E 1..0=3","tokens":["rorw","rd","rs1","rs2","31..25=0x30","14..12=5","6..2=0x0E","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x6000503b","variable_fields":["rd","rs1","rs2"]}},"record_id":"riscv-opcodes:rv64_zbb:rorw@L5","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let rori_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":26,"name":"bits[31:26]","width":6},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"shamtd","width":6}],"kind":"fixed_bits","mask":"0xfc00707f","value":"0x60005013","width_bits":32},"kind":"instruction-form","native_name":"rori","origin":{"line":7,"path":"extensions/rv64_zbb"},"provenance":{"extension":"rv64_zbb","operands":["rd","rs1","shamtd"],"raw":{"line":"rori  rd rs1                                31..26=0x18 shamtd 14..12=5 6..2=0x04 1..0=3","tokens":["rori","rd","rs1","31..26=0x18","shamtd","14..12=5","6..2=0x04","1..0=3"]},"upstream-resolved":{"mask":"0xfc00707f","match":"0x60005013","variable_fields":["rd","rs1","shamtd"]}},"record_id":"riscv-opcodes:rv64_zbb:rori@L7","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let rori_rv32_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"shamtw","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x60005013","width_bits":32},"kind":"pseudo-op","native_name":"rori.rv32","origin":{"line":3,"path":"extensions/rv32_zbb"},"provenance":{"extension":"rv32_zbb","operands":["rd","rs1","shamtw"],"raw":{"line":"$pseudo_op rv64_zbb::rori   rori.rv32 rd rs1   31..25=0x30 shamtw 14..12=5 6..2=0x04 1..0=3","tokens":["$pseudo_op","rv64_zbb::rori","rori.rv32","rd","rs1","31..25=0x30","shamtw","14..12=5","6..2=0x04","1..0=3"]},"relationship-resolution":[{"candidates":[],"kind":"specializes","status":"missing"}],"specializes-reference":{"extension":"rv64_zbb","name":"rori"},"upstream-resolved":{"mask":"0xfe00707f","match":"0x60005013","variable_fields":["rd","rs1","shamtw"]}},"record_id":"riscv-opcodes:rv32_zbb:rori.rv32@L3","relationships":[{"kind":"specializes","target":"riscv-opcodes:rv64_zbb:rori"}],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":["specializes-target not resolved to a concrete record_id by this adapter"]}|}

let roriw_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"shamtw","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x6000501b","width_bits":32},"kind":"instruction-form","native_name":"roriw","origin":{"line":6,"path":"extensions/rv64_zbb"},"provenance":{"extension":"rv64_zbb","operands":["rd","rs1","shamtw"],"raw":{"line":"roriw rd rs1                                31..25=0x30 shamtw 14..12=5 6..2=0x06 1..0=3","tokens":["roriw","rd","rs1","31..25=0x30","shamtw","14..12=5","6..2=0x06","1..0=3"]},"upstream-resolved":{"mask":"0xfe00707f","match":"0x6000501b","variable_fields":["rd","rs1","shamtw"]}},"record_id":"riscv-opcodes:rv64_zbb:roriw@L6","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let zext_h_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x800403b", "width_bits": 32}, "kind": "pseudo-op", "native_name": "zext.h", "origin": {"line": 8, "path": "extensions/rv64_zbb"}, "provenance": {"extension": "rv64_zbb", "operands": ["rd", "rs1"], "raw": {"line": "$pseudo_op rv64_zbkb::packw zext.h rd rs1    31..25=0x04 24..20=0 14..12=0x4 6..2=0xE 1..0=0x3", "tokens": ["$pseudo_op", "rv64_zbkb::packw", "zext.h", "rd", "rs1", "31..25=0x04", "24..20=0", "14..12=0x4", "6..2=0xE", "1..0=0x3"]}, "relationship-resolution": [{"candidates": ["riscv-opcodes:rv64_zbkb:packw@L6"], "kind": "specializes", "status": "exact"}], "specializes-reference": {"extension": "rv64_zbkb", "name": "packw"}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x800403b", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv64_zbb:zext.h@L8", "relationships": [{"kind": "specializes", "target": "riscv-opcodes:rv64_zbkb:packw@L6"}], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let zext_h_rv32_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x8004033", "width_bits": 32}, "kind": "pseudo-op", "native_name": "zext.h.rv32", "origin": {"line": 1, "path": "extensions/rv32_zbb"}, "provenance": {"extension": "rv32_zbb", "operands": ["rd", "rs1"], "raw": {"line": "$pseudo_op rv_zbkb::pack     zext.h.rv32 rd rs1 31..25=0x04 24..20=0 14..12=0x4 6..0=0x33", "tokens": ["$pseudo_op", "rv_zbkb::pack", "zext.h.rv32", "rd", "rs1", "31..25=0x04", "24..20=0", "14..12=0x4", "6..0=0x33"]}, "relationship-resolution": [{"candidates": ["riscv-opcodes:rv_zbkb:pack@L6"], "kind": "specializes", "status": "exact"}], "specializes-reference": {"extension": "rv_zbkb", "name": "pack"}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x8004033", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv32_zbb:zext.h.rv32@L1", "relationships": [{"kind": "specializes", "target": "riscv-opcodes:rv_zbkb:pack@L6"}], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let clmul_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[31:25]", "width": 7}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0xa001033", "width_bits": 32}, "kind": "instruction-form", "native_name": "clmul", "origin": {"line": 1, "path": "extensions/rv_zbc"}, "provenance": {"extension": "rv_zbc", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "clmul      rd rs1 rs2 31..25=5 14..12=1 6..2=0x0C 1..0=3", "tokens": ["clmul", "rd", "rs1", "rs2", "31..25=5", "14..12=1", "6..2=0x0C", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0xa001033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_zbc:clmul@L1", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let clmulh_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[31:25]", "width": 7}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0xa003033", "width_bits": 32}, "kind": "instruction-form", "native_name": "clmulh", "origin": {"line": 3, "path": "extensions/rv_zbc"}, "provenance": {"extension": "rv_zbc", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "clmulh     rd rs1 rs2 31..25=5 14..12=3 6..2=0x0C 1..0=3", "tokens": ["clmulh", "rd", "rs1", "rs2", "31..25=5", "14..12=3", "6..2=0x0C", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0xa003033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_zbc:clmulh@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let xperm4_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[31:25]", "width": 7}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x28002033", "width_bits": 32}, "kind": "instruction-form", "native_name": "xperm4", "origin": {"line": 1, "path": "extensions/rv_zbkx"}, "provenance": {"extension": "rv_zbkx", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "xperm4     rd rs1 rs2 31..25=20 14..12=2 6..2=0x0C 1..0=3", "tokens": ["xperm4", "rd", "rs1", "rs2", "31..25=20", "14..12=2", "6..2=0x0C", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x28002033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_zbkx:xperm4@L1", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let xperm8_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[31:25]", "width": 7}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x28004033", "width_bits": 32}, "kind": "instruction-form", "native_name": "xperm8", "origin": {"line": 2, "path": "extensions/rv_zbkx"}, "provenance": {"extension": "rv_zbkx", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "xperm8     rd rs1 rs2 31..25=20 14..12=4 6..2=0x0C 1..0=3", "tokens": ["xperm8", "rd", "rs1", "rs2", "31..25=20", "14..12=4", "6..2=0x0C", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x28004033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_zbkx:xperm8@L2", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha256sum0_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x10001013", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha256sum0", "origin": {"line": 2, "path": "extensions/rv_zknh"}, "provenance": {"extension": "rv_zknh", "operands": ["rd", "rs1"], "raw": {"line": "sha256sum0    rd rs1 31..30=0 29..25=0b01000 24..20=0b00000 14..12=1 6..0=0x13", "tokens": ["sha256sum0", "rd", "rs1", "31..30=0", "29..25=0b01000", "24..20=0b00000", "14..12=1", "6..0=0x13"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x10001013", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv_zknh:sha256sum0@L2", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha256sum1_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x10101013", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha256sum1", "origin": {"line": 3, "path": "extensions/rv_zknh"}, "provenance": {"extension": "rv_zknh", "operands": ["rd", "rs1"], "raw": {"line": "sha256sum1    rd rs1 31..30=0 29..25=0b01000 24..20=0b00001 14..12=1 6..0=0x13", "tokens": ["sha256sum1", "rd", "rs1", "31..30=0", "29..25=0b01000", "24..20=0b00001", "14..12=1", "6..0=0x13"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x10101013", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv_zknh:sha256sum1@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha256sig0_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x10201013", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha256sig0", "origin": {"line": 4, "path": "extensions/rv_zknh"}, "provenance": {"extension": "rv_zknh", "operands": ["rd", "rs1"], "raw": {"line": "sha256sig0    rd rs1 31..30=0 29..25=0b01000 24..20=0b00010 14..12=1 6..0=0x13", "tokens": ["sha256sig0", "rd", "rs1", "31..30=0", "29..25=0b01000", "24..20=0b00010", "14..12=1", "6..0=0x13"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x10201013", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv_zknh:sha256sig0@L4", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha256sig1_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x10301013", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha256sig1", "origin": {"line": 5, "path": "extensions/rv_zknh"}, "provenance": {"extension": "rv_zknh", "operands": ["rd", "rs1"], "raw": {"line": "sha256sig1    rd rs1 31..30=0 29..25=0b01000 24..20=0b00011 14..12=1 6..0=0x13", "tokens": ["sha256sig1", "rd", "rs1", "31..30=0", "29..25=0b01000", "24..20=0b00011", "14..12=1", "6..0=0x13"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x10301013", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv_zknh:sha256sig1@L5", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha512sum0_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x10401013", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha512sum0", "origin": {"line": 2, "path": "extensions/rv64_zknh"}, "provenance": {"extension": "rv64_zknh", "operands": ["rd", "rs1"], "raw": {"line": "sha512sum0 rd rs1  31..30=0 29..25=0b01000 24..20=0b00100 14..12=1 6..0=0x13", "tokens": ["sha512sum0", "rd", "rs1", "31..30=0", "29..25=0b01000", "24..20=0b00100", "14..12=1", "6..0=0x13"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x10401013", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv64_zknh:sha512sum0@L2", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha512sum1_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x10501013", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha512sum1", "origin": {"line": 3, "path": "extensions/rv64_zknh"}, "provenance": {"extension": "rv64_zknh", "operands": ["rd", "rs1"], "raw": {"line": "sha512sum1 rd rs1  31..30=0 29..25=0b01000 24..20=0b00101 14..12=1 6..0=0x13", "tokens": ["sha512sum1", "rd", "rs1", "31..30=0", "29..25=0b01000", "24..20=0b00101", "14..12=1", "6..0=0x13"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x10501013", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv64_zknh:sha512sum1@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha512sig0_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x10601013", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha512sig0", "origin": {"line": 4, "path": "extensions/rv64_zknh"}, "provenance": {"extension": "rv64_zknh", "operands": ["rd", "rs1"], "raw": {"line": "sha512sig0 rd rs1  31..30=0 29..25=0b01000 24..20=0b00110 14..12=1 6..0=0x13", "tokens": ["sha512sig0", "rd", "rs1", "31..30=0", "29..25=0b01000", "24..20=0b00110", "14..12=1", "6..0=0x13"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x10601013", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv64_zknh:sha512sig0@L4", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha512sig1_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x10701013", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha512sig1", "origin": {"line": 5, "path": "extensions/rv64_zknh"}, "provenance": {"extension": "rv64_zknh", "operands": ["rd", "rs1"], "raw": {"line": "sha512sig1 rd rs1  31..30=0 29..25=0b01000 24..20=0b00111 14..12=1 6..0=0x13", "tokens": ["sha512sig1", "rd", "rs1", "31..30=0", "29..25=0b01000", "24..20=0b00111", "14..12=1", "6..0=0x13"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x10701013", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv64_zknh:sha512sig1@L5", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha512sum0r_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x50000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha512sum0r", "origin": {"line": 2, "path": "extensions/rv32_zknh"}, "provenance": {"extension": "rv32_zknh", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "sha512sum0r   rd rs1 rs2    31..30=1 29..25=0b01000 14..12=0 6..0=0x33", "tokens": ["sha512sum0r", "rd", "rs1", "rs2", "31..30=1", "29..25=0b01000", "14..12=0", "6..0=0x33"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x50000033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv32_zknh:sha512sum0r@L2", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha512sum1r_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x52000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha512sum1r", "origin": {"line": 3, "path": "extensions/rv32_zknh"}, "provenance": {"extension": "rv32_zknh", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "sha512sum1r   rd rs1 rs2    31..30=1 29..25=0b01001 14..12=0 6..0=0x33", "tokens": ["sha512sum1r", "rd", "rs1", "rs2", "31..30=1", "29..25=0b01001", "14..12=0", "6..0=0x33"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x52000033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv32_zknh:sha512sum1r@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha512sig0l_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x54000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha512sig0l", "origin": {"line": 4, "path": "extensions/rv32_zknh"}, "provenance": {"extension": "rv32_zknh", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "sha512sig0l   rd rs1 rs2    31..30=1 29..25=0b01010 14..12=0 6..0=0x33", "tokens": ["sha512sig0l", "rd", "rs1", "rs2", "31..30=1", "29..25=0b01010", "14..12=0", "6..0=0x33"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x54000033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv32_zknh:sha512sig0l@L4", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha512sig1l_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x56000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha512sig1l", "origin": {"line": 6, "path": "extensions/rv32_zknh"}, "provenance": {"extension": "rv32_zknh", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "sha512sig1l   rd rs1 rs2    31..30=1 29..25=0b01011 14..12=0 6..0=0x33", "tokens": ["sha512sig1l", "rd", "rs1", "rs2", "31..30=1", "29..25=0b01011", "14..12=0", "6..0=0x33"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x56000033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv32_zknh:sha512sig1l@L6", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha512sig0h_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x5c000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha512sig0h", "origin": {"line": 5, "path": "extensions/rv32_zknh"}, "provenance": {"extension": "rv32_zknh", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "sha512sig0h   rd rs1 rs2    31..30=1 29..25=0b01110 14..12=0 6..0=0x33", "tokens": ["sha512sig0h", "rd", "rs1", "rs2", "31..30=1", "29..25=0b01110", "14..12=0", "6..0=0x33"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x5c000033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv32_zknh:sha512sig0h@L5", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let sha512sig1h_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x5e000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "sha512sig1h", "origin": {"line": 7, "path": "extensions/rv32_zknh"}, "provenance": {"extension": "rv32_zknh", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "sha512sig1h   rd rs1 rs2    31..30=1 29..25=0b01111 14..12=0 6..0=0x33", "tokens": ["sha512sig1h", "rd", "rs1", "rs2", "31..30=1", "29..25=0b01111", "14..12=0", "6..0=0x33"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x5e000033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv32_zknh:sha512sig1h@L7", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let aes64ds_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x3a000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "aes64ds", "origin": {"line": 3, "path": "extensions/rv64_zknd"}, "provenance": {"extension": "rv64_zknd", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "aes64ds    rd rs1 rs2  31..30=0 29..25=0b11101          14..12=0b000 6..0=0x33", "tokens": ["aes64ds", "rd", "rs1", "rs2", "31..30=0", "29..25=0b11101", "14..12=0b000", "6..0=0x33"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x3a000033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv64_zknd:aes64ds@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let aes64dsm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x3e000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "aes64dsm", "origin": {"line": 2, "path": "extensions/rv64_zknd"}, "provenance": {"extension": "rv64_zknd", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "aes64dsm   rd rs1 rs2  31..30=0 29..25=0b11111          14..12=0b000 6..0=0x33", "tokens": ["aes64dsm", "rd", "rs1", "rs2", "31..30=0", "29..25=0b11111", "14..12=0b000", "6..0=0x33"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x3e000033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv64_zknd:aes64dsm@L2", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let aes64es_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x32000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "aes64es", "origin": {"line": 3, "path": "extensions/rv64_zkne"}, "provenance": {"extension": "rv64_zkne", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "aes64es    rd rs1 rs2  31..30=0 29..25=0b11001          14..12=0b000 6..0=0x33", "tokens": ["aes64es", "rd", "rs1", "rs2", "31..30=0", "29..25=0b11001", "14..12=0b000", "6..0=0x33"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x32000033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv64_zkne:aes64es@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let aes64esm_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x36000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "aes64esm", "origin": {"line": 2, "path": "extensions/rv64_zkne"}, "provenance": {"extension": "rv64_zkne", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "aes64esm   rd rs1 rs2  31..30=0 29..25=0b11011          14..12=0b000 6..0=0x33", "tokens": ["aes64esm", "rd", "rs1", "rs2", "31..30=0", "29..25=0b11011", "14..12=0b000", "6..0=0x33"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x36000033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv64_zkne:aes64esm@L2", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let aes64ks2_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x7e000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "aes64ks2", "origin": {"line": 6, "path": "extensions/rv64_zknd"}, "provenance": {"extension": "rv64_zknd", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "aes64ks2   rd rs1 rs2  31..30=1 29..25=0b11111          14..12=0b000 6..0=0x33", "tokens": ["aes64ks2", "rd", "rs1", "rs2", "31..30=1", "29..25=0b11111", "14..12=0b000", "6..0=0x33"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x7e000033", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv64_zknd:aes64ks2@L6", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrrw_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "csr", "width": 12}], "kind": "fixed_bits", "mask": "0x707f", "value": "0x1073", "width_bits": 32}, "kind": "instruction-form", "native_name": "csrrw", "origin": {"line": 1, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["rd", "rs1", "csr"], "raw": {"line": "csrrw     rd rs1 csr        14..12=1 6..2=0x1C 1..0=3", "tokens": ["csrrw", "rd", "rs1", "csr", "14..12=1", "6..2=0x1C", "1..0=3"]}, "upstream-resolved": {"mask": "0x707f", "match": "0x1073", "variable_fields": ["rd", "rs1", "csr"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrrw@L1", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrrs_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "csr", "width": 12}], "kind": "fixed_bits", "mask": "0x707f", "value": "0x2073", "width_bits": 32}, "kind": "instruction-form", "native_name": "csrrs", "origin": {"line": 2, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["rd", "rs1", "csr"], "raw": {"line": "csrrs     rd rs1 csr        14..12=2 6..2=0x1C 1..0=3", "tokens": ["csrrs", "rd", "rs1", "csr", "14..12=2", "6..2=0x1C", "1..0=3"]}, "upstream-resolved": {"mask": "0x707f", "match": "0x2073", "variable_fields": ["rd", "rs1", "csr"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrrs@L2", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrrc_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "csr", "width": 12}], "kind": "fixed_bits", "mask": "0x707f", "value": "0x3073", "width_bits": 32}, "kind": "instruction-form", "native_name": "csrrc", "origin": {"line": 3, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["rd", "rs1", "csr"], "raw": {"line": "csrrc     rd rs1 csr        14..12=3 6..2=0x1C 1..0=3", "tokens": ["csrrc", "rd", "rs1", "csr", "14..12=3", "6..2=0x1C", "1..0=3"]}, "upstream-resolved": {"mask": "0x707f", "match": "0x3073", "variable_fields": ["rd", "rs1", "csr"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrrc@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrrwi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 20, "name": "csr", "width": 12}, {"lsb": 15, "name": "zimm5", "width": 5}], "kind": "fixed_bits", "mask": "0x707f", "value": "0x5073", "width_bits": 32}, "kind": "instruction-form", "native_name": "csrrwi", "origin": {"line": 4, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["rd", "csr", "zimm5"], "raw": {"line": "csrrwi    rd csr zimm5       14..12=5 6..2=0x1C 1..0=3", "tokens": ["csrrwi", "rd", "csr", "zimm5", "14..12=5", "6..2=0x1C", "1..0=3"]}, "upstream-resolved": {"mask": "0x707f", "match": "0x5073", "variable_fields": ["rd", "csr", "zimm5"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrrwi@L4", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrrsi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 20, "name": "csr", "width": 12}, {"lsb": 15, "name": "zimm5", "width": 5}], "kind": "fixed_bits", "mask": "0x707f", "value": "0x6073", "width_bits": 32}, "kind": "instruction-form", "native_name": "csrrsi", "origin": {"line": 5, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["rd", "csr", "zimm5"], "raw": {"line": "csrrsi    rd csr zimm5       14..12=6 6..2=0x1C 1..0=3", "tokens": ["csrrsi", "rd", "csr", "zimm5", "14..12=6", "6..2=0x1C", "1..0=3"]}, "upstream-resolved": {"mask": "0x707f", "match": "0x6073", "variable_fields": ["rd", "csr", "zimm5"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrrsi@L5", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrrci_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 20, "name": "csr", "width": 12}, {"lsb": 15, "name": "zimm5", "width": 5}], "kind": "fixed_bits", "mask": "0x707f", "value": "0x7073", "width_bits": 32}, "kind": "instruction-form", "native_name": "csrrci", "origin": {"line": 6, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["rd", "csr", "zimm5"], "raw": {"line": "csrrci    rd csr zimm5       14..12=7 6..2=0x1C 1..0=3", "tokens": ["csrrci", "rd", "csr", "zimm5", "14..12=7", "6..2=0x1C", "1..0=3"]}, "upstream-resolved": {"mask": "0x707f", "match": "0x7073", "variable_fields": ["rd", "csr", "zimm5"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrrci@L6", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrr_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 7, "name": "rd", "width": 5}, {"lsb": 20, "name": "csr", "width": 12}], "kind": "fixed_bits", "mask": "0xff07f", "value": "0x2073", "width_bits": 32}, "kind": "pseudo-op", "native_name": "csrr", "origin": {"line": 9, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["rd", "csr"], "raw": {"line": "$pseudo_op rv_zicsr::csrrs csrr     rd csr      19..15=0x0 14..12=2 6..2=0x1C 1..0=3", "tokens": ["$pseudo_op", "rv_zicsr::csrrs", "csrr", "rd", "csr", "19..15=0x0", "14..12=2", "6..2=0x1C", "1..0=3"]}, "relationship-resolution": [{"candidates": ["riscv-opcodes:rv_zicsr:csrrs@L2"], "kind": "specializes", "status": "exact"}], "specializes-reference": {"extension": "rv_zicsr", "name": "csrrs"}, "upstream-resolved": {"mask": "0xff07f", "match": "0x2073", "variable_fields": ["rd", "csr"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrr@L9", "relationships": [{"kind": "specializes", "target": "riscv-opcodes:rv_zicsr:csrrs@L2"}], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrw_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "csr", "width": 12}], "kind": "fixed_bits", "mask": "0x7fff", "value": "0x1073", "width_bits": 32}, "kind": "pseudo-op", "native_name": "csrw", "origin": {"line": 10, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["rs1", "csr"], "raw": {"line": "$pseudo_op rv_zicsr::csrrw csrw     rs1 csr     14..12=1 11..7=0x0 6..2=0x1C 1..0=3", "tokens": ["$pseudo_op", "rv_zicsr::csrrw", "csrw", "rs1", "csr", "14..12=1", "11..7=0x0", "6..2=0x1C", "1..0=3"]}, "relationship-resolution": [{"candidates": ["riscv-opcodes:rv_zicsr:csrrw@L1"], "kind": "specializes", "status": "exact"}], "specializes-reference": {"extension": "rv_zicsr", "name": "csrrw"}, "upstream-resolved": {"mask": "0x7fff", "match": "0x1073", "variable_fields": ["rs1", "csr"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrw@L10", "relationships": [{"kind": "specializes", "target": "riscv-opcodes:rv_zicsr:csrrw@L1"}], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrs_alias_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "csr", "width": 12}], "kind": "fixed_bits", "mask": "0x7fff", "value": "0x2073", "width_bits": 32}, "kind": "pseudo-op", "native_name": "csrs", "origin": {"line": 11, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["rs1", "csr"], "raw": {"line": "$pseudo_op rv_zicsr::csrrs csrs     rs1 csr     14..12=2 11..7=0x0 6..2=0x1C 1..0=3", "tokens": ["$pseudo_op", "rv_zicsr::csrrs", "csrs", "rs1", "csr", "14..12=2", "11..7=0x0", "6..2=0x1C", "1..0=3"]}, "relationship-resolution": [{"candidates": ["riscv-opcodes:rv_zicsr:csrrs@L2"], "kind": "specializes", "status": "exact"}], "specializes-reference": {"extension": "rv_zicsr", "name": "csrrs"}, "upstream-resolved": {"mask": "0x7fff", "match": "0x2073", "variable_fields": ["rs1", "csr"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrs@L11", "relationships": [{"kind": "specializes", "target": "riscv-opcodes:rv_zicsr:csrrs@L2"}], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrc_alias_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "csr", "width": 12}], "kind": "fixed_bits", "mask": "0x7fff", "value": "0x3073", "width_bits": 32}, "kind": "pseudo-op", "native_name": "csrc", "origin": {"line": 12, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["rs1", "csr"], "raw": {"line": "$pseudo_op rv_zicsr::csrrc csrc     rs1 csr     14..12=3 11..7=0x0 6..2=0x1C 1..0=3", "tokens": ["$pseudo_op", "rv_zicsr::csrrc", "csrc", "rs1", "csr", "14..12=3", "11..7=0x0", "6..2=0x1C", "1..0=3"]}, "relationship-resolution": [{"candidates": ["riscv-opcodes:rv_zicsr:csrrc@L3"], "kind": "specializes", "status": "exact"}], "specializes-reference": {"extension": "rv_zicsr", "name": "csrrc"}, "upstream-resolved": {"mask": "0x7fff", "match": "0x3073", "variable_fields": ["rs1", "csr"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrc@L12", "relationships": [{"kind": "specializes", "target": "riscv-opcodes:rv_zicsr:csrrc@L3"}], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrwi_alias_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "csr", "width": 12}, {"lsb": 15, "name": "zimm5", "width": 5}], "kind": "fixed_bits", "mask": "0x7fff", "value": "0x5073", "width_bits": 32}, "kind": "pseudo-op", "native_name": "csrwi", "origin": {"line": 13, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["csr", "zimm5"], "raw": {"line": "$pseudo_op rv_zicsr::csrrwi csrwi   csr zimm5    14..12=5 11..7=0x0 6..2=0x1C 1..0=3", "tokens": ["$pseudo_op", "rv_zicsr::csrrwi", "csrwi", "csr", "zimm5", "14..12=5", "11..7=0x0", "6..2=0x1C", "1..0=3"]}, "relationship-resolution": [{"candidates": ["riscv-opcodes:rv_zicsr:csrrwi@L4"], "kind": "specializes", "status": "exact"}], "specializes-reference": {"extension": "rv_zicsr", "name": "csrrwi"}, "upstream-resolved": {"mask": "0x7fff", "match": "0x5073", "variable_fields": ["csr", "zimm5"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrwi@L13", "relationships": [{"kind": "specializes", "target": "riscv-opcodes:rv_zicsr:csrrwi@L4"}], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrsi_alias_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "csr", "width": 12}, {"lsb": 15, "name": "zimm5", "width": 5}], "kind": "fixed_bits", "mask": "0x7fff", "value": "0x6073", "width_bits": 32}, "kind": "pseudo-op", "native_name": "csrsi", "origin": {"line": 14, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["csr", "zimm5"], "raw": {"line": "$pseudo_op rv_zicsr::csrrsi csrsi   csr zimm5    14..12=6 11..7=0x0 6..2=0x1C 1..0=3", "tokens": ["$pseudo_op", "rv_zicsr::csrrsi", "csrsi", "csr", "zimm5", "14..12=6", "11..7=0x0", "6..2=0x1C", "1..0=3"]}, "relationship-resolution": [{"candidates": ["riscv-opcodes:rv_zicsr:csrrsi@L5"], "kind": "specializes", "status": "exact"}], "specializes-reference": {"extension": "rv_zicsr", "name": "csrrsi"}, "upstream-resolved": {"mask": "0x7fff", "match": "0x6073", "variable_fields": ["csr", "zimm5"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrsi@L14", "relationships": [{"kind": "specializes", "target": "riscv-opcodes:rv_zicsr:csrrsi@L5"}], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let csrci_alias_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "csr", "width": 12}, {"lsb": 15, "name": "zimm5", "width": 5}], "kind": "fixed_bits", "mask": "0x7fff", "value": "0x7073", "width_bits": 32}, "kind": "pseudo-op", "native_name": "csrci", "origin": {"line": 15, "path": "extensions/rv_zicsr"}, "provenance": {"extension": "rv_zicsr", "operands": ["csr", "zimm5"], "raw": {"line": "$pseudo_op rv_zicsr::csrrci csrci   csr zimm5    14..12=7 11..7=0x0 6..2=0x1C 1..0=3", "tokens": ["$pseudo_op", "rv_zicsr::csrrci", "csrci", "csr", "zimm5", "14..12=7", "11..7=0x0", "6..2=0x1C", "1..0=3"]}, "relationship-resolution": [{"candidates": ["riscv-opcodes:rv_zicsr:csrrci@L6"], "kind": "specializes", "status": "exact"}], "specializes-reference": {"extension": "rv_zicsr", "name": "csrrci"}, "upstream-resolved": {"mask": "0x7fff", "match": "0x7073", "variable_fields": ["csr", "zimm5"]}}, "record_id": "riscv-opcodes:rv_zicsr:csrci@L15", "relationships": [{"kind": "specializes", "target": "riscv-opcodes:rv_zicsr:csrrci@L6"}], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let aes32dsi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 30, "name": "bs", "width": 2}], "kind": "fixed_bits", "mask": "0x3e00707f", "value": "0x2a000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "aes32dsi", "origin": {"line": 3, "path": "extensions/rv32_zknd"}, "provenance": {"extension": "rv32_zknd", "operands": ["rd", "rs1", "rs2", "bs"], "raw": {"line": "aes32dsi      rd rs1 rs2 bs          29..25=0b10101 14..12=0 6..0=0x33", "tokens": ["aes32dsi", "rd", "rs1", "rs2", "bs", "29..25=0b10101", "14..12=0", "6..0=0x33"]}, "upstream-resolved": {"mask": "0x3e00707f", "match": "0x2a000033", "variable_fields": ["rd", "rs1", "rs2", "bs"]}}, "record_id": "riscv-opcodes:rv32_zknd:aes32dsi@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let aes32dsmi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 30, "name": "bs", "width": 2}], "kind": "fixed_bits", "mask": "0x3e00707f", "value": "0x2e000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "aes32dsmi", "origin": {"line": 2, "path": "extensions/rv32_zknd"}, "provenance": {"extension": "rv32_zknd", "operands": ["rd", "rs1", "rs2", "bs"], "raw": {"line": "aes32dsmi     rd rs1 rs2 bs          29..25=0b10111 14..12=0 6..0=0x33", "tokens": ["aes32dsmi", "rd", "rs1", "rs2", "bs", "29..25=0b10111", "14..12=0", "6..0=0x33"]}, "upstream-resolved": {"mask": "0x3e00707f", "match": "0x2e000033", "variable_fields": ["rd", "rs1", "rs2", "bs"]}}, "record_id": "riscv-opcodes:rv32_zknd:aes32dsmi@L2", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let aes32esi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 30, "name": "bs", "width": 2}], "kind": "fixed_bits", "mask": "0x3e00707f", "value": "0x22000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "aes32esi", "origin": {"line": 4, "path": "extensions/rv32_zkne"}, "provenance": {"extension": "rv32_zkne", "operands": ["rd", "rs1", "rs2", "bs"], "raw": {"line": "aes32esi      rd rs1 rs2 bs          29..25=0b10001 14..12=0 6..0=0x33", "tokens": ["aes32esi", "rd", "rs1", "rs2", "bs", "29..25=0b10001", "14..12=0", "6..0=0x33"]}, "upstream-resolved": {"mask": "0x3e00707f", "match": "0x22000033", "variable_fields": ["rd", "rs1", "rs2", "bs"]}}, "record_id": "riscv-opcodes:rv32_zkne:aes32esi@L4", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let aes32esmi_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 30, "name": "bs", "width": 2}], "kind": "fixed_bits", "mask": "0x3e00707f", "value": "0x26000033", "width_bits": 32}, "kind": "instruction-form", "native_name": "aes32esmi", "origin": {"line": 3, "path": "extensions/rv32_zkne"}, "provenance": {"extension": "rv32_zkne", "operands": ["rd", "rs1", "rs2", "bs"], "raw": {"line": "aes32esmi     rd rs1 rs2 bs          29..25=0b10011 14..12=0 6..0=0x33", "tokens": ["aes32esmi", "rd", "rs1", "rs2", "bs", "29..25=0b10011", "14..12=0", "6..0=0x33"]}, "upstream-resolved": {"mask": "0x3e00707f", "match": "0x26000033", "variable_fields": ["rd", "rs1", "rs2", "bs"]}}, "record_id": "riscv-opcodes:rv32_zkne:aes32esmi@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let aes64ks1i_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 24, "name": "bits[24:24]", "width": 1}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rnum", "width": 4}], "kind": "fixed_bits", "mask": "0xff00707f", "value": "0x31001013", "width_bits": 32}, "kind": "instruction-form", "native_name": "aes64ks1i", "origin": {"line": 4, "path": "extensions/rv64_zknd"}, "provenance": {"extension": "rv64_zknd", "operands": ["rd", "rs1", "rnum"], "raw": {"line": "aes64ks1i  rd rs1 rnum 31..30=0 29..25=0b11000 24=1     14..12=0b001 6..0=0x13", "tokens": ["aes64ks1i", "rd", "rs1", "rnum", "31..30=0", "29..25=0b11000", "24=1", "14..12=0b001", "6..0=0x13"]}, "upstream-resolved": {"mask": "0xff00707f", "match": "0x31001013", "variable_fields": ["rd", "rs1", "rnum"]}}, "record_id": "riscv-opcodes:rv64_zknd:aes64ks1i@L4", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let aes64im_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 30, "name": "bits[31:30]", "width": 2}, {"lsb": 25, "name": "bits[29:25]", "width": 5}, {"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 0, "name": "bits[6:0]", "width": 7}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0x30001013", "width_bits": 32}, "kind": "instruction-form", "native_name": "aes64im", "origin": {"line": 5, "path": "extensions/rv64_zknd"}, "provenance": {"extension": "rv64_zknd", "operands": ["rd", "rs1"], "raw": {"line": "aes64im    rd rs1      31..30=0 29..25=0b11000 24..20=0b0000 14..12=0b001 6..0=0x13", "tokens": ["aes64im", "rd", "rs1", "31..30=0", "29..25=0b11000", "24..20=0b0000", "14..12=0b001", "6..0=0x13"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0x30001013", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv64_zknd:aes64im@L5", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let zip_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":20,"name":"bits[24:20]","width":5},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x8f01013","width_bits":32},"kind":"instruction-form","native_name":"zip","origin":{"line":1,"path":"extensions/rv32_zbkb"},"provenance":{"extension":"rv32_zbkb","operands":["rd","rs1"],"raw":{"line":"zip       rd rs1 31..25=4 24..20=15 14..12=1 6..2=4 1..0=3","tokens":["zip","rd","rs1","31..25=4","24..20=15","14..12=1","6..2=4","1..0=3"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x8f01013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv32_zbkb:zip@L1","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let unzip_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":25,"name":"bits[31:25]","width":7},{"lsb":20,"name":"bits[24:20]","width":5},{"lsb":12,"name":"bits[14:12]","width":3},{"lsb":2,"name":"bits[6:2]","width":5},{"lsb":0,"name":"bits[1:0]","width":2},{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5}],"kind":"fixed_bits","mask":"0xfff0707f","value":"0x8f05013","width_bits":32},"kind":"instruction-form","native_name":"unzip","origin":{"line":2,"path":"extensions/rv32_zbkb"},"provenance":{"extension":"rv32_zbkb","operands":["rd","rs1"],"raw":{"line":"unzip     rd rs1 31..25=4 24..20=15 14..12=5 6..2=4 1..0=3","tokens":["unzip","rd","rs1","31..25=4","24..20=15","14..12=5","6..2=4","1..0=3"]},"upstream-resolved":{"mask":"0xfff0707f","match":"0x8f05013","variable_fields":["rd","rs1"]}},"record_id":"riscv-opcodes:rv32_zbkb:unzip@L2","relationships":[],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

(* andn/orn/xnor/rol/ror: riscv-opcodes exports each of these identically
   under five extension files (rv_zbb, rv_zbkb, rv_zk, rv_zkn, rv_zks), so
   their requirement is Req_any over all five rather than the single
   Req_feature test_r_type_gpr checks. *)
let zbb_import_group = [ "zbb"; "zbkb"; "zk"; "zkn"; "zks" ]
let zbkb_import_group_4way = [ "zbkb"; "zk"; "zkn"; "zks" ]

let test_r_type_gpr_any ~import_group ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (mnemonic ^ ": requirement is Req_any over the import-group features")
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature -> Isa_norm_model.Req_feature (Printf.sprintf "riscv:%s" feature))
           import_group));
  check
    (mnemonic ^ ": three plain GPR operands, no immediate")
    (List.length form.operands = 3
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check
    (mnemonic ^ ": renders as rd, rs1, rs2")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, rs1, rs2" mnemonic)

(* The two-GPR-operand analogue of {!test_r_type_gpr_any} above, for
   unary_gpr_mnemonics entries (clz-shaped) that also need a Req_any
   requirement rather than a plain Req_feature/Req_all. *)
let test_unary_gpr_any ~import_group ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (mnemonic ^ ": requirement is Req_any over the import-group features")
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature -> Isa_norm_model.Req_feature (Printf.sprintf "riscv:%s" feature))
           import_group));
  check
    (mnemonic ^ ": two plain GPR operands, no immediate")
    (List.length form.operands = 2
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check
    (mnemonic ^ ": renders as rd, rs1")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, rs1" mnemonic)

let test_andn () =
  test_r_type_gpr_any ~import_group:zbb_import_group ~mnemonic:"andn" ~json:andn_json

let test_orn () = test_r_type_gpr_any ~import_group:zbb_import_group ~mnemonic:"orn" ~json:orn_json

let test_xnor () =
  test_r_type_gpr_any ~import_group:zbb_import_group ~mnemonic:"xnor" ~json:xnor_json

let test_rol () = test_r_type_gpr_any ~import_group:zbb_import_group ~mnemonic:"rol" ~json:rol_json
let test_ror () = test_r_type_gpr_any ~import_group:zbb_import_group ~mnemonic:"ror" ~json:ror_json

(* clmul/clmulh: the same five-way Req_any R-type shape as andn/orn/xnor/
   rol/ror, but rooted in Zbc (rv_zbc primary, imported by rv_zbkc/rv_zk/
   rv_zkn/rv_zks) rather than Zbb - identical mnemonic/encoding on both
   profiles, no rev8-style native_name split needed. *)
let zbc_import_group = [ "zbc"; "zbkc"; "zk"; "zkn"; "zks" ]

let test_clmul () =
  test_r_type_gpr_any ~import_group:zbc_import_group ~mnemonic:"clmul" ~json:clmul_json

let test_clmulh () =
  test_r_type_gpr_any ~import_group:zbc_import_group ~mnemonic:"clmulh" ~json:clmulh_json

(* xperm4/xperm8: the same three-GPR Req_any R-type shape, but a four-way
   zbkx-only group (rv_zbkx/rv_zk/rv_zkn/rv_zks - no separate non-K sibling
   the way clmul/clmulh have rv_zbc alongside rv_zbkc), the same shape
   {!test_pack}/{!test_packh} already exercise for zbkb_import_group_4way. *)
let zbkx_import_group = [ "zbkx"; "zk"; "zkn"; "zks" ]

let test_xperm4 () =
  test_r_type_gpr_any ~import_group:zbkx_import_group ~mnemonic:"xperm4" ~json:xperm4_json

let test_xperm8 () =
  test_r_type_gpr_any ~import_group:zbkx_import_group ~mnemonic:"xperm8" ~json:xperm8_json

(* sha256sum0/sha256sum1/sha256sig0/sha256sig1: the two-GPR unary-shape
   analogue of xperm4/xperm8 above - a three-way Req_any group (rv_zknh
   primary, imported by rv_zk/rv_zkn; no rv_zks, since SHA-256 belongs to
   the NIST crypto profile, not ShangMi). *)
let zknh_import_group = [ "zknh"; "zk"; "zkn" ]

let test_sha256sum0 () =
  test_unary_gpr_any ~import_group:zknh_import_group ~mnemonic:"sha256sum0" ~json:sha256sum0_json

let test_sha256sum1 () =
  test_unary_gpr_any ~import_group:zknh_import_group ~mnemonic:"sha256sum1" ~json:sha256sum1_json

let test_sha256sig0 () =
  test_unary_gpr_any ~import_group:zknh_import_group ~mnemonic:"sha256sig0" ~json:sha256sig0_json

let test_sha256sig1 () =
  test_unary_gpr_any ~import_group:zknh_import_group ~mnemonic:"sha256sig1" ~json:sha256sig1_json

(* sha512sum0/sum1/sig0/sig1: sha256's RV64-only siblings - the same
   two-GPR unary shape, but an XLEN-prefixed three-way Req_any group (no
   RV32 record at all; riscv-opcodes' RV32 answer is a different,
   32-bit-word-pair-split family out of this slice's scope), so this needs
   the rev8-style Req_xlen-wrapped check rather than {!test_unary_gpr_any}'s
   plain-Req_feature shape. *)
let zknh_import_group_rv64 = [ "zknh"; "zk"; "zkn" ]

let test_sha512_unary_any ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (mnemonic ^ ": requirement is Req_any over the three rv64-prefixed features")
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 64; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           zknh_import_group_rv64));
  check
    (mnemonic ^ ": two plain GPR operands, no immediate")
    (List.length form.operands = 2
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check
    (mnemonic ^ ": renders as rd, rs1")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, rs1" mnemonic)

let test_sha512sum0 () = test_sha512_unary_any ~mnemonic:"sha512sum0" ~json:sha512sum0_json
let test_sha512sum1 () = test_sha512_unary_any ~mnemonic:"sha512sum1" ~json:sha512sum1_json
let test_sha512sig0 () = test_sha512_unary_any ~mnemonic:"sha512sig0" ~json:sha512sig0_json
let test_sha512sig1 () = test_sha512_unary_any ~mnemonic:"sha512sig1" ~json:sha512sig1_json

(* sha512sum0r/sum1r/sig0l/sig1l/sig0h/sig1h: SHA-512's own RV32-only
   32-bit-word-pair-split helpers - a plain three-GPR R-type shape (not the
   two-GPR unary one every sha256/sha512 form above uses), with the same
   rv32-prefixed, Req_xlen-wrapped three-way Req_any group shape as
   {!test_sha512_unary_any} above. *)
let zknh_import_group_rv32 = [ "zknh"; "zk"; "zkn" ]

let test_r_type_gpr_any_rv32 ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (mnemonic ^ ": requirement is Req_any over the three rv32-prefixed features")
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 32; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           zknh_import_group_rv32));
  check
    (mnemonic ^ ": three plain GPR operands, no immediate")
    (List.length form.operands = 3
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check
    (mnemonic ^ ": renders as rd, rs1, rs2")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, rs1, rs2" mnemonic)

let test_sha512sum0r () = test_r_type_gpr_any_rv32 ~mnemonic:"sha512sum0r" ~json:sha512sum0r_json
let test_sha512sum1r () = test_r_type_gpr_any_rv32 ~mnemonic:"sha512sum1r" ~json:sha512sum1r_json
let test_sha512sig0l () = test_r_type_gpr_any_rv32 ~mnemonic:"sha512sig0l" ~json:sha512sig0l_json
let test_sha512sig1l () = test_r_type_gpr_any_rv32 ~mnemonic:"sha512sig1l" ~json:sha512sig1l_json
let test_sha512sig0h () = test_r_type_gpr_any_rv32 ~mnemonic:"sha512sig0h" ~json:sha512sig0h_json
let test_sha512sig1h () = test_r_type_gpr_any_rv32 ~mnemonic:"sha512sig1h" ~json:sha512sig1h_json

(* AES-64's plain three-GPR round functions and its two-GPR unary
   inverse-mix-columns sibling - the rv64-prefixed analogues of
   {!test_r_type_gpr_any_rv32}/{!test_sha512_unary_any}, parametrized over
   [import_group] since aes64ds/aes64dsm (rooted in rv64_zknd),
   aes64es/aes64esm (rooted in rv64_zkne, a disjoint primary), and
   aes64ks2 (imported by both zknd and zkne, a four-way group) each need a
   different one. *)
let aes64ds_import_group = [ "zknd"; "zk"; "zkn" ]
let aes64es_import_group = [ "zkne"; "zk"; "zkn" ]
let aes64ks2_import_group = [ "zknd"; "zk"; "zkn"; "zkne" ]

let test_r_type_gpr_any_rv64 ~import_group ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (mnemonic ^ ": requirement is Req_any over the rv64-prefixed features")
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 64; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           import_group));
  check
    (mnemonic ^ ": three plain GPR operands, no immediate")
    (List.length form.operands = 3
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check
    (mnemonic ^ ": renders as rd, rs1, rs2")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, rs1, rs2" mnemonic)

let test_aes64ds () =
  test_r_type_gpr_any_rv64 ~import_group:aes64ds_import_group ~mnemonic:"aes64ds" ~json:aes64ds_json

let test_aes64dsm () =
  test_r_type_gpr_any_rv64 ~import_group:aes64ds_import_group ~mnemonic:"aes64dsm"
    ~json:aes64dsm_json

let test_aes64es () =
  test_r_type_gpr_any_rv64 ~import_group:aes64es_import_group ~mnemonic:"aes64es" ~json:aes64es_json

let test_aes64esm () =
  test_r_type_gpr_any_rv64 ~import_group:aes64es_import_group ~mnemonic:"aes64esm"
    ~json:aes64esm_json

let test_aes64ks2 () =
  test_r_type_gpr_any_rv64 ~import_group:aes64ks2_import_group ~mnemonic:"aes64ks2"
    ~json:aes64ks2_json

let test_aes64im () =
  let rec_ = decode_or_fail "aes64im" aes64im_json in
  let form = normalize_or_fail "aes64im" rec_ in
  check "aes64im: requirement is Req_any over the three rv64-prefixed features"
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 64; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           aes64ds_import_group));
  check "aes64im: two plain GPR operands, no immediate"
    (List.length form.operands = 2
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check "aes64im: renders as rd, rs1" (Isa_norm_model.render_syntax form.syntax = "aes64im rd, rs1")

(* aes64ks1i: the same two-GPR-plus-narrow-unsigned-immediate shape as
   rori/roriw, reusing shamt_gpr_form's generalized operand_name/field_name
   overrides for riscv-opcodes' own "rnum" field, and aes64ks2's own
   four-way Req_any group (imported by both key-schedule extensions). *)
let test_aes64ks1i () =
  let rec_ = decode_or_fail "aes64ks1i" aes64ks1i_json in
  let form = normalize_or_fail "aes64ks1i" rec_ in
  check "aes64ks1i: requirement is Req_any over the four rv64-prefixed features"
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 64; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           aes64ks2_import_group));
  check "aes64ks1i: two GPR operands plus an unsigned 4-bit rnum immediate"
    (match form.operands with
    | [
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
     { op_kind = Immediate { width_bits = 4; signed = false; _ }; _ };
    ] ->
        true
    | _ -> false);
  check "aes64ks1i: renders as rd, rs1, rnum"
    (Isa_norm_model.render_syntax form.syntax = "aes64ks1i rd, rs1, rnum")

(* aes32dsi/dsmi/esi/esmi: AES-32's own round functions - a three-GPR
   analogue of aes64ks1i's own two-GPR-plus-narrow-immediate shape, via the
   new r_type_imm_gpr_form, keyed "bs" (riscv-opcodes' own field name), not
   "rnum" or "shamt". aes32dsi/dsmi share aes32d_import_group_rv32
   (rv32_zknd-rooted); aes32esi/esmi share the disjoint
   aes32e_import_group_rv32 (rv32_zkne-rooted). *)
let aes32d_import_group = [ "zknd"; "zk"; "zkn" ]
let aes32e_import_group = [ "zkne"; "zk"; "zkn" ]

let test_r_type_imm_gpr_any_rv32 ~import_group ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (mnemonic ^ ": requirement is Req_any over the three rv32-prefixed features")
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 32; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           import_group));
  check
    (mnemonic ^ ": three GPR operands plus an unsigned 2-bit bs immediate")
    (match form.operands with
    | [
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
     { op_kind = Immediate { width_bits = 2; signed = false; _ }; _ };
    ] ->
        true
    | _ -> false);
  check
    (mnemonic ^ ": renders as rd, rs1, rs2, bs")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, rs1, rs2, bs" mnemonic)

let test_aes32dsi () =
  test_r_type_imm_gpr_any_rv32 ~import_group:aes32d_import_group ~mnemonic:"aes32dsi"
    ~json:aes32dsi_json

let test_aes32dsmi () =
  test_r_type_imm_gpr_any_rv32 ~import_group:aes32d_import_group ~mnemonic:"aes32dsmi"
    ~json:aes32dsmi_json

let test_aes32esi () =
  test_r_type_imm_gpr_any_rv32 ~import_group:aes32e_import_group ~mnemonic:"aes32esi"
    ~json:aes32esi_json

let test_aes32esmi () =
  test_r_type_imm_gpr_any_rv32 ~import_group:aes32e_import_group ~mnemonic:"aes32esmi"
    ~json:aes32esmi_json

(* csrrw/csrrs/csrrc: Zicsr's register-source CSR forms - a plain
   Req_feature (no import duplication, single rv_zicsr record per
   mnemonic), operands reordered to [rd, csr, rs1] matching real GAS
   syntax rather than riscv-opcodes' own [rd, rs1, csr] field order. *)
let test_csr_reg ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (mnemonic ^ ": requirement is the Zicsr feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:zicsr");
  check
    (mnemonic ^ ": rd/rs1 GPRs plus an unsigned 12-bit csr immediate, in rd/csr/rs1 order")
    (match form.operands with
    | [
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
     { op_kind = Immediate { width_bits = 12; signed = false; _ }; _ };
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
    ] ->
        true
    | _ -> false);
  check
    (mnemonic ^ ": renders as rd, csr, rs1")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, csr, rs1" mnemonic)

let test_csrrw () = test_csr_reg ~mnemonic:"csrrw" ~json:csrrw_json
let test_csrrs () = test_csr_reg ~mnemonic:"csrrs" ~json:csrrs_json
let test_csrrc () = test_csr_reg ~mnemonic:"csrrc" ~json:csrrc_json

(* csrrwi/csrrsi/csrrci: Zicsr's immediate-source CSR forms - [rd, csr,
   zimm5], no register operand besides [rd]. *)
let test_csr_imm ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (mnemonic ^ ": requirement is the Zicsr feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:zicsr");
  check
    (mnemonic ^ ": rd GPR plus unsigned 12-bit csr and 5-bit zimm5 immediates")
    (match form.operands with
    | [
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
     { op_kind = Immediate { width_bits = 12; signed = false; _ }; _ };
     { op_kind = Immediate { width_bits = 5; signed = false; _ }; _ };
    ] ->
        true
    | _ -> false);
  check
    (mnemonic ^ ": renders as rd, csr, zimm5")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, csr, zimm5" mnemonic)

let test_csrrwi () = test_csr_imm ~mnemonic:"csrrwi" ~json:csrrwi_json
let test_csrrsi () = test_csr_imm ~mnemonic:"csrrsi" ~json:csrrsi_json
let test_csrrci () = test_csr_imm ~mnemonic:"csrrci" ~json:csrrci_json

(* csrr: GAS's read-only alias for csrrs rd, csr, x0 - [rd, csr] operands,
   no rs1 at all. *)
let test_csrr () =
  let rec_ = decode_or_fail "csrr" csrr_json in
  let form = normalize_or_fail "csrr" rec_ in
  check "csrr: requirement is the Zicsr feature"
    (form.requirement = Isa_norm_model.Req_feature "riscv:zicsr");
  check "csrr: rd GPR plus an unsigned 12-bit csr immediate, in rd/csr order"
    (match form.operands with
    | [
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
     { op_kind = Immediate { width_bits = 12; signed = false; _ }; _ };
    ] ->
        true
    | _ -> false);
  check "csrr: renders as rd, csr" (Isa_norm_model.render_syntax form.syntax = "csrr rd, csr")

(* csrw/csrs/csrc: GAS's write/set/clear-only aliases for
   csrrw/csrrs/csrrc x0, csr, rs1 - [csr, rs1] operands, no rd at all;
   csr comes first, unlike riscv-opcodes' own [rs1, csr] field order. *)
let test_csr_write ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (mnemonic ^ ": requirement is the Zicsr feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:zicsr");
  check
    (mnemonic ^ ": an unsigned 12-bit csr immediate plus rs1 GPR, in csr/rs1 order")
    (match form.operands with
    | [
     { op_kind = Immediate { width_bits = 12; signed = false; _ }; _ };
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
    ] ->
        true
    | _ -> false);
  check
    (mnemonic ^ ": renders as csr, rs1")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s csr, rs1" mnemonic)

let test_csrw () = test_csr_write ~mnemonic:"csrw" ~json:csrw_json
let test_csrs () = test_csr_write ~mnemonic:"csrs" ~json:csrs_alias_json
let test_csrc () = test_csr_write ~mnemonic:"csrc" ~json:csrc_alias_json

(* csrwi/csrsi/csrci: GAS's write/set/clear-only aliases for
   csrrwi/csrrsi/csrrci x0, csr, zimm5 - [csr, zimm5] operands, no rd. *)
let test_csr_write_imm ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (mnemonic ^ ": requirement is the Zicsr feature")
    (form.requirement = Isa_norm_model.Req_feature "riscv:zicsr");
  check
    (mnemonic ^ ": unsigned 12-bit csr and 5-bit zimm5 immediates, in csr/zimm5 order")
    (match form.operands with
    | [
     { op_kind = Immediate { width_bits = 12; signed = false; _ }; _ };
     { op_kind = Immediate { width_bits = 5; signed = false; _ }; _ };
    ] ->
        true
    | _ -> false);
  check
    (mnemonic ^ ": renders as csr, zimm5")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s csr, zimm5" mnemonic)

let test_csrwi () = test_csr_write_imm ~mnemonic:"csrwi" ~json:csrwi_alias_json
let test_csrsi () = test_csr_write_imm ~mnemonic:"csrsi" ~json:csrsi_alias_json
let test_csrci () = test_csr_write_imm ~mnemonic:"csrci" ~json:csrci_alias_json

(* amoadd.w: Zaamo's three-GPR-plus-memory shape, verbatim-extracted from
   the checked-in riscv32.jsonl - representative of every amo_form
   mnemonic (only funct5/funct3 differ between them). *)
let amoadd_w_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 29, "name": "bits[31:29]", "width": 3}, {"lsb": 27, "name": "bits[28:27]", "width": 2}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 26, "name": "aq", "width": 1}, {"lsb": 25, "name": "rl", "width": 1}], "kind": "fixed_bits", "mask": "0xf800707f", "value": "0x202f", "width_bits": 32}, "kind": "instruction-form", "native_name": "amoadd.w", "origin": {"line": 4, "path": "extensions/rv_a"}, "provenance": {"extension": "rv_a", "operands": ["rd", "rs1", "rs2", "aq", "rl"], "raw": {"line": "amoadd.w    rd rs1 rs2      aq rl 31..29=0 28..27=0 14..12=2 6..2=0x0B 1..0=3", "tokens": ["amoadd.w", "rd", "rs1", "rs2", "aq", "rl", "31..29=0", "28..27=0", "14..12=2", "6..2=0x0B", "1..0=3"]}, "upstream-resolved": {"mask": "0xf800707f", "match": "0x202f", "variable_fields": ["rd", "rs1", "rs2", "aq", "rl"]}}, "record_id": "riscv-opcodes:rv_a:amoadd.w@L4", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_amoadd_w () =
  let rec_ = decode_or_fail "amoadd.w" amoadd_w_json in
  let form = normalize_or_fail "amoadd.w" rec_ in
  check "amoadd.w: requirement is the A feature"
    (form.requirement = Isa_norm_model.Req_feature "riscv:a");
  check "amoadd.w: rd/rs2/base GPRs, in that order, no offset operand"
    (match form.operands with
    | [
     { op_name = "rd"; op_kind = Register { class_ = Riscv_gpr; excluded = [] }; role = Out; _ };
     { op_name = "rs2"; op_kind = Register { class_ = Riscv_gpr; excluded = [] }; role = In; _ };
     { op_name = "base"; op_kind = Register { class_ = Riscv_gpr; excluded = [] }; role = In; _ };
    ] ->
        true
    | _ -> false);
  check "amoadd.w: renders as rd, rs2, (base)"
    (Isa_norm_model.render_syntax form.syntax = "amoadd.w rd, rs2, (base)");
  check "amoadd.w: flags aq/rl as not modeled"
    (List.exists
       (fun (d : Isa_norm_model.diagnostic) -> String.equal d.rule "amoadd.w-aq-rl-not-modeled")
       form.diagnostics)

(* lr.w: Zaamo's own two-operand member, no rs2 at all (its field is fixed
   to 0). *)
let lr_w_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 29, "name": "bits[31:29]", "width": 3}, {"lsb": 27, "name": "bits[28:27]", "width": 2}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 26, "name": "aq", "width": 1}, {"lsb": 25, "name": "rl", "width": 1}], "kind": "fixed_bits", "mask": "0xf9f0707f", "value": "0x1000202f", "width_bits": 32}, "kind": "instruction-form", "native_name": "lr.w", "origin": {"line": 1, "path": "extensions/rv_a"}, "provenance": {"extension": "rv_a", "operands": ["rd", "rs1", "aq", "rl"], "raw": {"line": "lr.w        rd rs1 24..20=0 aq rl 31..29=0 28..27=2 14..12=2 6..2=0x0B 1..0=3", "tokens": ["lr.w", "rd", "rs1", "24..20=0", "aq", "rl", "31..29=0", "28..27=2", "14..12=2", "6..2=0x0B", "1..0=3"]}, "upstream-resolved": {"mask": "0xf9f0707f", "match": "0x1000202f", "variable_fields": ["rd", "rs1", "aq", "rl"]}}, "record_id": "riscv-opcodes:rv_a:lr.w@L1", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_lr_w () =
  let rec_ = decode_or_fail "lr.w" lr_w_json in
  let form = normalize_or_fail "lr.w" rec_ in
  check "lr.w: requirement is the A feature"
    (form.requirement = Isa_norm_model.Req_feature "riscv:a");
  check "lr.w: rd/base GPRs only, in that order, no rs2 operand"
    (match form.operands with
    | [
     { op_name = "rd"; op_kind = Register { class_ = Riscv_gpr; excluded = [] }; role = Out; _ };
     { op_name = "base"; op_kind = Register { class_ = Riscv_gpr; excluded = [] }; role = In; _ };
    ] ->
        true
    | _ -> false);
  check "lr.w: renders as rd, (base)" (Isa_norm_model.render_syntax form.syntax = "lr.w rd, (base)");
  check "lr.w: flags aq/rl as not modeled"
    (List.exists
       (fun (d : Isa_norm_model.diagnostic) -> String.equal d.rule "lr.w-aq-rl-not-modeled")
       form.diagnostics)

(* flw: F's floating-point load, verbatim-extracted from the checked-in
   riscv32.jsonl - representative of {!f_load_form} (fld shares the
   identical shape, only the requirement/width differ). *)
let flw_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "imm12", "width": 12}], "kind": "fixed_bits", "mask": "0x707f", "value": "0x2007", "width_bits": 32}, "kind": "instruction-form", "native_name": "flw", "origin": {"line": 1, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "imm12"], "raw": {"line": "flw       rd rs1 imm12 14..12=2 6..2=0x01 1..0=3", "tokens": ["flw", "rd", "rs1", "imm12", "14..12=2", "6..2=0x01", "1..0=3"]}, "upstream-resolved": {"mask": "0x707f", "match": "0x2007", "variable_fields": ["rd", "rs1", "imm12"]}}, "record_id": "riscv-opcodes:rv_f:flw@L1", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_flw () =
  let rec_ = decode_or_fail "flw" flw_json in
  let form = normalize_or_fail "flw" rec_ in
  check "flw: requirement is the F feature" (form.requirement = Isa_norm_model.Req_feature "riscv:f");
  check "flw: value is a floating-point register, base a GPR, offset a 12-bit signed immediate"
    (match form.operands with
    | [
     { op_name = "value"; op_kind = Register { class_ = Riscv_fpr; excluded = [] }; role = Out; _ };
     { op_name = "base"; op_kind = Register { class_ = Riscv_gpr; excluded = [] }; role = In; _ };
     {
       op_name = "offset";
       op_kind = Immediate { width_bits = 12; signed = true; runs = [ _ ]; _ };
       _;
     };
    ] ->
        true
    | _ -> false);
  check "flw: renders as value, offset(base)"
    (Isa_norm_model.render_syntax form.syntax = "flw value, offset(base)")

(* fsw: F's floating-point store, verbatim-extracted from the checked-in
   riscv32.jsonl - representative of {!f_store_form} (fsd shares the
   identical shape). *)
let fsw_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 25, "name": "imm12hi", "width": 7}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 7, "name": "imm12lo", "width": 5}], "kind": "fixed_bits", "mask": "0x707f", "value": "0x2027", "width_bits": 32}, "kind": "instruction-form", "native_name": "fsw", "origin": {"line": 2, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["imm12hi", "rs1", "rs2", "imm12lo"], "raw": {"line": "fsw       imm12hi rs1 rs2 imm12lo 14..12=2 6..2=0x09 1..0=3", "tokens": ["fsw", "imm12hi", "rs1", "rs2", "imm12lo", "14..12=2", "6..2=0x09", "1..0=3"]}, "upstream-resolved": {"mask": "0x707f", "match": "0x2027", "variable_fields": ["imm12hi", "rs1", "rs2", "imm12lo"]}}, "record_id": "riscv-opcodes:rv_f:fsw@L2", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_fsw () =
  let rec_ = decode_or_fail "fsw" fsw_json in
  let form = normalize_or_fail "fsw" rec_ in
  check "fsw: requirement is the F feature" (form.requirement = Isa_norm_model.Req_feature "riscv:f");
  check
    "fsw: value is a floating-point register, base a GPR, offset a 12-bit signed split immediate"
    (match form.operands with
    | [
     { op_name = "value"; op_kind = Register { class_ = Riscv_fpr; excluded = [] }; role = In; _ };
     { op_name = "base"; op_kind = Register { class_ = Riscv_gpr; excluded = [] }; role = In; _ };
     {
       op_name = "offset";
       op_kind = Immediate { width_bits = 12; signed = true; runs = [ hi; lo ]; _ };
       _;
     };
    ] ->
        hi.field_name = "imm12hi" && hi.dest_hi = 11 && hi.dest_lo = 5 && lo.field_name = "imm12lo"
        && lo.dest_hi = 4 && lo.dest_lo = 0
    | _ -> false);
  check "fsw: renders as value, offset(base)"
    (Isa_norm_model.render_syntax form.syntax = "fsw value, offset(base)")

(* pack/packh: the same Req_any R-type shape, but a four-way zbkb-only group
   (rv_zbkb/rv_zk/rv_zkn/rv_zks - no rv_zbb primary the way andn/orn/xnor/
   rol/ror have). *)
let test_pack () =
  test_r_type_gpr_any ~import_group:zbkb_import_group_4way ~mnemonic:"pack" ~json:pack_json

let test_packh () =
  test_r_type_gpr_any ~import_group:zbkb_import_group_4way ~mnemonic:"packh" ~json:packh_json

(* packw: packh/pack's RV64-only word-operand sibling - Req_any over
   rv64-prefixed features, each itself RV64-gated (Req_all [Req_xlen 64; ...]),
   the same combination {!test_rev8}'s unary-shape check already exercises. *)
let rv64_zbkb_import_group = [ "zbkb"; "zk"; "zkn"; "zks" ]

let test_packw () =
  let rec_ = decode_or_fail "packw" packw_json in
  let form = normalize_or_fail "packw" rec_ in
  check "packw: requirement is Req_any over the four rv64-prefixed features"
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 64; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           rv64_zbkb_import_group));
  check "packw: three plain GPR operands, no immediate"
    (List.length form.operands = 3
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check "packw: renders as rd, rs1, rs2"
    (Isa_norm_model.render_syntax form.syntax = "packw rd, rs1, rs2")

(* zip/unzip: the unary shape, RV32-only, with a four-way rv32-prefixed
   Req_any group (no rv32_zbb primary the way rev8.rv32 has). *)
let rv32_zbkb_import_group = [ "zbkb"; "zk"; "zkn"; "zks" ]

let test_zip_or_unzip ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (mnemonic ^ ": requirement is Req_any over the four rv32-prefixed features")
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 32; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           rv32_zbkb_import_group));
  check
    (mnemonic ^ ": two plain GPR operands, no immediate")
    (List.length form.operands = 2
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check
    (mnemonic ^ ": renders as rd, rs1")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, rs1" mnemonic)

let test_zip () = test_zip_or_unzip ~mnemonic:"zip" ~json:zip_json
let test_unzip () = test_zip_or_unzip ~mnemonic:"unzip" ~json:unzip_json

(* The point of Req_any: normalizing the SAME instruction's rv_zbkb $import
   record yields the identical requirement as the rv_zbb primary record
   above, not a Req_feature "riscv:zbkb" that would silently require only
   this one record's own extension. *)
let test_andn_import_record_matches_primary () =
  let primary = normalize_or_fail "andn" (decode_or_fail "andn" andn_json) in
  let import =
    normalize_or_fail "andn" (decode_or_fail "andn (rv_zbkb import)" andn_zbkb_import_json)
  in
  check "andn: rv_zbkb import record has the same Req_any requirement as rv_zbb"
    (import.requirement = primary.requirement);
  check "andn: rv_zbkb import record has the same form_id as rv_zbb"
    (String.equal import.form_id primary.form_id)

(* Zbb's population-count/sign-extend/byte family: two plain GPR operands
   (rd, rs1), no immediate - {!unary_gpr_form}'s shape, distinct from
   {!test_r_type_gpr}'s three-operand check above. *)
let test_unary_gpr ~feature ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (Printf.sprintf "%s: requirement is the %s feature" mnemonic feature)
    (form.requirement = Isa_norm_model.Req_feature (Printf.sprintf "riscv:%s" feature));
  check
    (mnemonic ^ ": two plain GPR operands, no immediate")
    (List.length form.operands = 2
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check
    (mnemonic ^ ": renders as rd, rs1")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, rs1" mnemonic)

let test_clz () = test_unary_gpr ~feature:"zbb" ~mnemonic:"clz" ~json:clz_json
let test_ctz () = test_unary_gpr ~feature:"zbb" ~mnemonic:"ctz" ~json:ctz_json
let test_cpop () = test_unary_gpr ~feature:"zbb" ~mnemonic:"cpop" ~json:cpop_json
let test_sextb () = test_unary_gpr ~feature:"zbb" ~mnemonic:"sext.b" ~json:sextb_json
let test_sexth () = test_unary_gpr ~feature:"zbb" ~mnemonic:"sext.h" ~json:sexth_json
let test_orcb () = test_unary_gpr ~feature:"zbb" ~mnemonic:"orc.b" ~json:orcb_json

(* clzw/ctzw/cpopw are RV64-only (rv64_zbb, no RV32 counterpart), so their
   requirement is Req_all [Req_xlen 64; Req_feature "riscv:zbb"], the same
   combination {!test_r_type_gpr_rv64} exercises for Zba's *.uw family. *)
let test_unary_gpr_rv64 ~feature ~mnemonic ~json =
  let rec_ = decode_or_fail mnemonic json in
  let form = normalize_or_fail mnemonic rec_ in
  check
    (Printf.sprintf "%s: requirement is RV64 plus the %s feature" mnemonic feature)
    (form.requirement
    = Isa_norm_model.Req_all [ Req_xlen 64; Req_feature (Printf.sprintf "riscv:%s" feature) ]);
  check (mnemonic ^ ": carries no diagnostics") (form.diagnostics = []);
  check
    (mnemonic ^ ": two plain GPR operands, no immediate")
    (List.length form.operands = 2
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check
    (mnemonic ^ ": renders as rd, rs1")
    (Isa_norm_model.render_syntax form.syntax = Printf.sprintf "%s rd, rs1" mnemonic)

let test_clzw () = test_unary_gpr_rv64 ~feature:"zbb" ~mnemonic:"clzw" ~json:clzw_json
let test_ctzw () = test_unary_gpr_rv64 ~feature:"zbb" ~mnemonic:"ctzw" ~json:ctzw_json
let test_cpopw () = test_unary_gpr_rv64 ~feature:"zbb" ~mnemonic:"cpopw" ~json:cpopw_json

(* brev8: the same two-GPR-operand unary shape as clz/etc, but with a
   four-way Req_any (rv_zbkb/rv_zk/rv_zkn/rv_zks) - {!test_r_type_gpr_any}'s
   shape carried over to the unary form. *)
let zbkb_import_group = [ "zbkb"; "zk"; "zkn"; "zks" ]

let test_brev8 () =
  let rec_ = decode_or_fail "brev8" brev8_json in
  let form = normalize_or_fail "brev8" rec_ in
  check "brev8: requirement is Req_any over the four zbkb-import-group features"
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature -> Isa_norm_model.Req_feature (Printf.sprintf "riscv:%s" feature))
           zbkb_import_group));
  check "brev8: two plain GPR operands, no immediate"
    (List.length form.operands = 2
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check "brev8: renders as rd, rs1" (Isa_norm_model.render_syntax form.syntax = "brev8 rd, rs1")

(* The point of Req_any: normalizing the SAME instruction's rv_zk pseudo-op
   record yields the identical requirement and form_id as the rv_zbkb
   primary record above, not a Req_feature "riscv:zk" that would silently
   require only this one record's own extension. *)
let test_brev8_pseudo_record_matches_primary () =
  let primary = normalize_or_fail "brev8" (decode_or_fail "brev8" brev8_json) in
  let pseudo =
    normalize_or_fail "brev8" (decode_or_fail "brev8 (rv_zk pseudo-op)" brev8_zk_pseudo_json)
  in
  check "brev8: rv_zk pseudo-op record has the same Req_any requirement as rv_zbkb"
    (pseudo.requirement = primary.requirement);
  check "brev8: rv_zk pseudo-op record has the same form_id as rv_zbkb"
    (String.equal pseudo.form_id primary.form_id)

(* rev8 (RV64): the unary shape plus a five-way Req_any over the RV64-prefixed
   extension group, native_name already equal to the rendered mnemonic. *)
let rv64_rev8_import_group = [ "zbb"; "zbkb"; "zk"; "zkn"; "zks" ]

let test_rev8 () =
  let rec_ = decode_or_fail "rev8" rev8_json in
  let form = normalize_or_fail "rev8" rec_ in
  check "rev8: requirement is Req_any over the five rv64-prefixed features"
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 64; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           rv64_rev8_import_group));
  check "rev8: two plain GPR operands, no immediate"
    (List.length form.operands = 2
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check "rev8: renders as rd, rs1" (Isa_norm_model.render_syntax form.syntax = "rev8 rd, rs1")

(* The point of Req_any again: the rv64_zk pseudo-op record normalizes to the
   identical requirement and form_id as the rv64_zbb primary above. *)
let test_rev8_pseudo_record_matches_primary () =
  let primary = normalize_or_fail "rev8" (decode_or_fail "rev8" rev8_json) in
  let pseudo =
    normalize_or_fail "rev8" (decode_or_fail "rev8 (rv64_zk pseudo-op)" rev8_zk_pseudo_json)
  in
  check "rev8: rv64_zk pseudo-op record has the same Req_any requirement as rv64_zbb"
    (pseudo.requirement = primary.requirement);
  check "rev8: rv64_zk pseudo-op record has the same form_id as rv64_zbb"
    (String.equal pseudo.form_id primary.form_id)

(* rev8.rv32: riscv-opcodes' own RV32 native_name is "rev8.rv32", but this
   must normalize to the SAME rendered mnemonic ("rev8") and form_id
   ("riscv:rev8") as the RV64 record above - real GNU as does not accept
   "rev8.rv32" as a mnemonic at all - while its Req_any uses the disjoint
   rv32-prefixed extension group, keyed by extension_lookup_key. *)
let rv32_rev8_import_group = [ "zbkb"; "zbb"; "zk"; "zkn"; "zks" ]

let test_rev8_rv32 () =
  let rec_ = decode_or_fail "rev8.rv32" rev8_rv32_json in
  let form = normalize_or_fail "rev8.rv32" rec_ in
  check "rev8.rv32: form_id matches the RV64 rev8 record's form_id"
    (String.equal form.form_id "riscv:rev8");
  check "rev8.rv32: native_name preserves the source's own RV32 spelling"
    (String.equal form.native_name "rev8.rv32");
  check "rev8.rv32: renders as the bare rev8 rd, rs1 spelling, not rev8.rv32"
    (Isa_norm_model.render_syntax form.syntax = "rev8 rd, rs1");
  check "rev8.rv32: requirement is Req_any over the five rv32-prefixed features"
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 32; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           rv32_rev8_import_group))

(* rolw/rorw: rol/ror's RV64-only word-operand siblings - the same
   three-GPR R-type shape as {!test_r_type_gpr_any}, but reusing rev8's
   five-way rv64-prefixed Req_any group above (primary rv64_zbb, imported by
   rv64_zbkb/rv64_zk/rv64_zkn/rv64_zks; riscv-opcodes has no RV32 record at
   all - confirmed real riscv32-linux-gnu-as rejects both as unrecognized
   opcodes). *)
let test_rolw () =
  let rec_ = decode_or_fail "rolw" rolw_json in
  let form = normalize_or_fail "rolw" rec_ in
  check "rolw: requirement is Req_any over the five rv64-prefixed features"
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 64; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           rv64_rev8_import_group));
  check "rolw: three plain GPR operands, no immediate"
    (List.length form.operands = 3
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check "rolw: renders as rd, rs1, rs2"
    (Isa_norm_model.render_syntax form.syntax = "rolw rd, rs1, rs2")

let test_rorw () =
  let rec_ = decode_or_fail "rorw" rorw_json in
  let form = normalize_or_fail "rorw" rec_ in
  check "rorw: requirement is Req_any over the five rv64-prefixed features"
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 64; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           rv64_rev8_import_group));
  check "rorw: three plain GPR operands, no immediate"
    (List.length form.operands = 3
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check "rorw: renders as rd, rs1, rs2"
    (Isa_norm_model.render_syntax form.syntax = "rorw rd, rs1, rs2")

(* rori/rori.rv32/roriw: the first shift-amount-immediate shape - two GPR
   operands plus a genuine, syntax-visible unsigned immediate, unlike the
   plain three-GPR R-type shape every prior Zbb test above checks. rori
   reuses rev8's own five-way rv64-prefixed Req_any group; rori.rv32 is the
   same profile-specific native_name split {!test_rev8_rv32} exercises,
   reusing rev8's rv32-prefixed group instead. *)
let test_rori () =
  let rec_ = decode_or_fail "rori" rori_json in
  let form = normalize_or_fail "rori" rec_ in
  check "rori: requirement is Req_any over the five rv64-prefixed features"
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 64; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           rv64_rev8_import_group));
  check "rori: two GPR operands plus an unsigned 6-bit shamt immediate"
    (match form.operands with
    | [
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
     { op_kind = Immediate { width_bits = 6; signed = false; _ }; _ };
    ] ->
        true
    | _ -> false);
  check "rori: renders as rd, rs1, shamt"
    (Isa_norm_model.render_syntax form.syntax = "rori rd, rs1, shamt")

let test_rori_rv32 () =
  let rec_ = decode_or_fail "rori.rv32" rori_rv32_json in
  let form = normalize_or_fail "rori.rv32" rec_ in
  check "rori.rv32: form_id matches the RV64 rori record's form_id"
    (String.equal form.form_id "riscv:rori");
  check "rori.rv32: native_name preserves the source's own RV32 pseudo-op spelling"
    (String.equal form.native_name "rori.rv32");
  check "rori.rv32: renders as the bare rori rd, rs1, shamt spelling, not rori.rv32"
    (Isa_norm_model.render_syntax form.syntax = "rori rd, rs1, shamt");
  check "rori.rv32: requirement is Req_any over the five rv32-prefixed features"
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 32; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           rv32_rev8_import_group));
  check "rori.rv32: shamt immediate is unsigned and 5 bits wide"
    (match List.rev form.operands with
    | { op_kind = Immediate { width_bits = 5; signed = false; _ }; _ } :: _ -> true
    | _ -> false)

let test_roriw () =
  let rec_ = decode_or_fail "roriw" roriw_json in
  let form = normalize_or_fail "roriw" rec_ in
  check "roriw: requirement is Req_any over the five rv64-prefixed features"
    (form.requirement
    = Isa_norm_model.Req_any
        (List.map
           (fun feature ->
             Isa_norm_model.Req_all [ Req_xlen 64; Req_feature (Printf.sprintf "riscv:%s" feature) ])
           rv64_rev8_import_group));
  check "roriw: two GPR operands plus an unsigned 5-bit shamt immediate"
    (match form.operands with
    | [
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
     { op_kind = Register { class_ = Riscv_gpr; excluded = [] }; _ };
     { op_kind = Immediate { width_bits = 5; signed = false; _ }; _ };
    ] ->
        true
    | _ -> false);
  check "roriw: renders as rd, rs1, shamt"
    (Isa_norm_model.render_syntax form.syntax = "roriw rd, rs1, shamt")

(* zext.h/zext.h.rv32: unlike every other member of this family, neither
   record is imported by a second extension file - each profile names only
   its own single extension (rv64_zbb/rv32_zbb), even though both specialize
   a pack/packw record from a different (Zbkb) extension. {!requirement_of}'s
   plain per-record lookup already gives the right answer, so no
   {!alternative_extensions_by_mnemonic} entry (Req_any) was needed - this is
   a plain Req_all, the same shape {!test_fadd_s} below checks, not a
   five-way group like every rev8/rori-family test above. *)
let test_zext_h () =
  let rec_ = decode_or_fail "zext.h" zext_h_json in
  let form = normalize_or_fail "zext.h" rec_ in
  check "zext.h: requirement is RV64 + zbb, no Req_any"
    (form.requirement = Isa_norm_model.Req_all [ Req_xlen 64; Req_feature "riscv:zbb" ]);
  check "zext.h: two plain GPR operands, no immediate"
    (List.length form.operands = 2
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check "zext.h: renders as rd, rs1" (Isa_norm_model.render_syntax form.syntax = "zext.h rd, rs1")

let test_zext_h_rv32 () =
  let rec_ = decode_or_fail "zext.h.rv32" zext_h_rv32_json in
  let form = normalize_or_fail "zext.h.rv32" rec_ in
  check "zext.h.rv32: form_id matches the RV64 zext.h record's form_id"
    (String.equal form.form_id "riscv:zext.h");
  check "zext.h.rv32: native_name preserves the source's own RV32 pseudo-op spelling"
    (String.equal form.native_name "zext.h.rv32");
  check "zext.h.rv32: renders as the bare zext.h rd, rs1 spelling, not zext.h.rv32"
    (Isa_norm_model.render_syntax form.syntax = "zext.h rd, rs1");
  check "zext.h.rv32: requirement is RV32 + zbb, no Req_any"
    (form.requirement = Isa_norm_model.Req_all [ Req_xlen 32; Req_feature "riscv:zbb" ])

let test_fadd_s () =
  let rec_ = decode_or_fail "fadd.s" fadd_s_json in
  let form = normalize_or_fail "fadd.s" rec_ in
  check "fadd.s: requirement is the F feature"
    (form.requirement = Isa_norm_model.Req_feature "riscv:f");
  check "fadd.s: rd/rs1/rs2 are floating registers, not the integer GPR class"
    (List.for_all
       (fun name ->
         match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = name) form.operands with
         | { op_kind = Register { class_ = Riscv_fpr; _ }; _ } -> true
         | _ -> false)
       [ "rd"; "rs1"; "rs2" ]);
  check "fadd.s: rm is present but implicit, with GNU's measured dyn default recorded"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rm") form.operands with
    | { op_kind = Rounding_mode; explicit = false; _ } ->
        form.diagnostics = []
        && List.exists
             (fun (f : Isa_norm_model.fact) ->
               f.label = Inferred && String.contains f.note '7' && String.contains f.note 'd')
             form.facts
    | _ -> false)

let test_other_f_arith_s () =
  (* All four source mnemonics have the same six-byte spelling, so this keeps
     the compact checked-in fadd.s fixture's decoded shape while exercising
     the normalizer's explicit scalar-arithmetic allowlist. Repo tests also
     ground each entry against its own real export record. *)
  let with_mnemonic mnemonic =
    let bytes = Bytes.of_string fadd_s_json in
    let rec replace_at i =
      if i + 6 > Bytes.length bytes then ()
      else if Bytes.sub_string bytes i 6 = "fadd.s" then (
        Bytes.blit_string mnemonic 0 bytes i 6;
        replace_at (i + 6))
      else replace_at (i + 1)
    in
    replace_at 0;
    Bytes.to_string bytes
  in
  List.iter
    (fun mnemonic ->
      let rec_ = decode_or_fail mnemonic (with_mnemonic mnemonic) in
      let form = normalize_or_fail mnemonic rec_ in
      check (mnemonic ^ ": scalar arithmetic form_id") (form.form_id = "riscv:" ^ mnemonic);
      check
        (mnemonic ^ ": renders three FPR operands")
        (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1, rs2"))
    [ "fsub.s"; "fmul.s"; "fdiv.s"; "fadd.d"; "fsub.d"; "fmul.d"; "fdiv.d" ]

(* fsgnj.s/fsgnjn.s/fsgnjx.s/fsgnj.d/fsgnjn.d/fsgnjx.d: the general
   three-distinct-FP-register sign-injection form, verbatim-extracted from the
   checked-in riscv32.jsonl. Unlike {!test_fadd_s}, no rm operand is modeled
   (funct3 here is a fixed per-mnemonic selector, not a rounding mode). *)
let fsgnj_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x20000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fsgnj.s", "origin": {"line": 12, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "fsgnj.s   rd rs1 rs2      31..27=0x04 14..12=0 26..25=0 6..2=0x14 1..0=3", "tokens": ["fsgnj.s", "rd", "rs1", "rs2", "31..27=0x04", "14..12=0", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x20000053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_f:fsgnj.s@L12", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fsgnjn_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x20001053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fsgnjn.s", "origin": {"line": 13, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "fsgnjn.s  rd rs1 rs2      31..27=0x04 14..12=1 26..25=0 6..2=0x14 1..0=3", "tokens": ["fsgnjn.s", "rd", "rs1", "rs2", "31..27=0x04", "14..12=1", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x20001053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_f:fsgnjn.s@L13", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fsgnjx_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x20002053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fsgnjx.s", "origin": {"line": 14, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "fsgnjx.s  rd rs1 rs2      31..27=0x04 14..12=2 26..25=0 6..2=0x14 1..0=3", "tokens": ["fsgnjx.s", "rd", "rs1", "rs2", "31..27=0x04", "14..12=2", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x20002053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_f:fsgnjx.s@L14", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fsgnj_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x22000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fsgnj.d", "origin": {"line": 12, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "fsgnj.d   rd rs1 rs2      31..27=0x04 14..12=0 26..25=1 6..2=0x14 1..0=3", "tokens": ["fsgnj.d", "rd", "rs1", "rs2", "31..27=0x04", "14..12=0", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x22000053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_d:fsgnj.d@L12", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fsgnjn_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x22001053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fsgnjn.d", "origin": {"line": 13, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "fsgnjn.d  rd rs1 rs2      31..27=0x04 14..12=1 26..25=1 6..2=0x14 1..0=3", "tokens": ["fsgnjn.d", "rd", "rs1", "rs2", "31..27=0x04", "14..12=1", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x22001053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_d:fsgnjn.d@L13", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fsgnjx_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x22002053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fsgnjx.d", "origin": {"line": 14, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "fsgnjx.d  rd rs1 rs2      31..27=0x04 14..12=2 26..25=1 6..2=0x14 1..0=3", "tokens": ["fsgnjx.d", "rd", "rs1", "rs2", "31..27=0x04", "14..12=2", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x22002053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_d:fsgnjx.d@L14", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_fsgnj () =
  List.iter
    (fun (mnemonic, json) ->
      let rec_ = decode_or_fail mnemonic json in
      let form = normalize_or_fail mnemonic rec_ in
      let expected_feature =
        if String.ends_with ~suffix:".d" mnemonic then "riscv:d" else "riscv:f"
      in
      check
        (mnemonic ^ ": requirement is the expected F/D feature")
        (form.requirement = Isa_norm_model.Req_feature expected_feature);
      check
        (mnemonic ^ ": rd/rs1/rs2 are floating registers, no rm operand")
        (match form.operands with
        | [
         { op_name = "rd"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
         { op_name = "rs1"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
         { op_name = "rs2"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
        ] ->
            true
        | _ -> false);
      check
        (mnemonic ^ ": renders as rd, rs1, rs2")
        (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1, rs2"))
    [
      ("fsgnj.s", fsgnj_s_json);
      ("fsgnjn.s", fsgnjn_s_json);
      ("fsgnjx.s", fsgnjx_s_json);
      ("fsgnj.d", fsgnj_d_json);
      ("fsgnjn.d", fsgnjn_d_json);
      ("fsgnjx.d", fsgnjx_d_json);
    ]

(* fmin.s/fmax.s/fmin.d/fmax.d: the same three-FP-register, no-rm shape as
   {!test_fsgnj}, verbatim-extracted from the checked-in riscv32.jsonl. *)
let fmin_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x28000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fmin.s", "origin": {"line": 15, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "fmin.s    rd rs1 rs2      31..27=0x05 14..12=0 26..25=0 6..2=0x14 1..0=3", "tokens": ["fmin.s", "rd", "rs1", "rs2", "31..27=0x05", "14..12=0", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x28000053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_f:fmin.s@L15", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fmax_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x28001053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fmax.s", "origin": {"line": 16, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "fmax.s    rd rs1 rs2      31..27=0x05 14..12=1 26..25=0 6..2=0x14 1..0=3", "tokens": ["fmax.s", "rd", "rs1", "rs2", "31..27=0x05", "14..12=1", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x28001053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_f:fmax.s@L16", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fmin_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x2a000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fmin.d", "origin": {"line": 15, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "fmin.d    rd rs1 rs2      31..27=0x05 14..12=0 26..25=1 6..2=0x14 1..0=3", "tokens": ["fmin.d", "rd", "rs1", "rs2", "31..27=0x05", "14..12=0", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x2a000053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_d:fmin.d@L15", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fmax_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0x2a001053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fmax.d", "origin": {"line": 16, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "fmax.d    rd rs1 rs2      31..27=0x05 14..12=1 26..25=1 6..2=0x14 1..0=3", "tokens": ["fmax.d", "rd", "rs1", "rs2", "31..27=0x05", "14..12=1", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0x2a001053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_d:fmax.d@L16", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_fminmax () =
  List.iter
    (fun (mnemonic, json) ->
      let rec_ = decode_or_fail mnemonic json in
      let form = normalize_or_fail mnemonic rec_ in
      let expected_feature =
        if String.ends_with ~suffix:".d" mnemonic then "riscv:d" else "riscv:f"
      in
      check
        (mnemonic ^ ": requirement is the expected F/D feature")
        (form.requirement = Isa_norm_model.Req_feature expected_feature);
      check
        (mnemonic ^ ": rd/rs1/rs2 are floating registers, no rm operand")
        (match form.operands with
        | [
         { op_name = "rd"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
         { op_name = "rs1"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
         { op_name = "rs2"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
        ] ->
            true
        | _ -> false);
      check
        (mnemonic ^ ": renders as rd, rs1, rs2")
        (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1, rs2"))
    [
      ("fmin.s", fmin_s_json);
      ("fmax.s", fmax_s_json);
      ("fmin.d", fmin_d_json);
      ("fmax.d", fmax_d_json);
    ]

(* fsqrt.s/fsqrt.d: {!test_fadd_s}'s own shape minus rs2, verbatim-extracted
   from the checked-in riscv32.jsonl. *)
let fsqrt_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0x58000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fsqrt.s", "origin": {"line": 11, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fsqrt.s   rd rs1 24..20=0 31..27=0x0B rm       26..25=0 6..2=0x14 1..0=3", "tokens": ["fsqrt.s", "rd", "rs1", "24..20=0", "31..27=0x0B", "rm", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0x58000053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv_f:fsqrt.s@L11", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fsqrt_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0x5a000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fsqrt.d", "origin": {"line": 11, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fsqrt.d   rd rs1 24..20=0 31..27=0x0B rm       26..25=1 6..2=0x14 1..0=3", "tokens": ["fsqrt.d", "rd", "rs1", "24..20=0", "31..27=0x0B", "rm", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0x5a000053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv_d:fsqrt.d@L11", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_fsqrt () =
  List.iter
    (fun (mnemonic, json) ->
      let rec_ = decode_or_fail mnemonic json in
      let form = normalize_or_fail mnemonic rec_ in
      let expected_feature =
        if String.ends_with ~suffix:".d" mnemonic then "riscv:d" else "riscv:f"
      in
      check
        (mnemonic ^ ": requirement is the expected F/D feature")
        (form.requirement = Isa_norm_model.Req_feature expected_feature);
      check
        (mnemonic ^ ": rd/rs1 are floating registers, rm is present but implicit")
        (match form.operands with
        | [
         { op_name = "rd"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
         { op_name = "rs1"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
         { op_name = "rm"; op_kind = Rounding_mode; explicit = false; _ };
        ] ->
            true
        | _ -> false);
      check
        (mnemonic ^ ": renders as rd, rs1")
        (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1"))
    [ ("fsqrt.s", fsqrt_s_json); ("fsqrt.d", fsqrt_d_json) ]

(* fclass.s/fclass.d: [rd] is a GPR, [rs1] is FP, no [rm], verbatim-extracted
   from the checked-in riscv32.jsonl. *)
let fclass_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0xe0001053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fclass.s", "origin": {"line": 23, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1"], "raw": {"line": "fclass.s  rd rs1 24..20=0 31..27=0x1C 14..12=1 26..25=0 6..2=0x14 1..0=3", "tokens": ["fclass.s", "rd", "rs1", "24..20=0", "31..27=0x1C", "14..12=1", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0xe0001053", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv_f:fclass.s@L23", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fclass_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0xe2001053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fclass.d", "origin": {"line": 22, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1"], "raw": {"line": "fclass.d  rd rs1 24..20=0 31..27=0x1C 14..12=1 26..25=1 6..2=0x14 1..0=3", "tokens": ["fclass.d", "rd", "rs1", "24..20=0", "31..27=0x1C", "14..12=1", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0xe2001053", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv_d:fclass.d@L22", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_fclass () =
  List.iter
    (fun (mnemonic, json) ->
      let rec_ = decode_or_fail mnemonic json in
      let form = normalize_or_fail mnemonic rec_ in
      let expected_feature =
        if String.ends_with ~suffix:".d" mnemonic then "riscv:d" else "riscv:f"
      in
      check
        (mnemonic ^ ": requirement is the expected F/D feature")
        (form.requirement = Isa_norm_model.Req_feature expected_feature);
      check
        (mnemonic ^ ": rd is a GPR, rs1 is a floating register")
        (match form.operands with
        | [
         { op_name = "rd"; op_kind = Register { class_ = Riscv_gpr; _ }; _ };
         { op_name = "rs1"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
        ] ->
            true
        | _ -> false);
      check
        (mnemonic ^ ": renders as rd, rs1")
        (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1"))
    [ ("fclass.s", fclass_s_json); ("fclass.d", fclass_d_json) ]

(* fmadd.s/fmsub.s/fnmsub.s/fnmadd.s/fmadd.d/fmsub.d/fnmsub.d/fnmadd.d:
   RISC-V's only R4-type mnemonics - {!f_sqrt_form}'s implicit-dynamic-
   rounding shape with two more distinct FP register operands ([rs2],
   [rs3]), verbatim-extracted from the checked-in riscv32.jsonl. *)
let fmadd_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 27, "name": "rs3", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0x600007f","value": "0x43", "width_bits": 32}, "kind": "instruction-form", "native_name": "fmadd.s", "origin": {"line": 3, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd","rs1", "rs2", "rs3", "rm"], "raw": {"line": "fmadd.s   rd rs1 rs2 rs3 rm 26..25=0 6..2=0x10 1..0=3", "tokens": ["fmadd.s", "rd", "rs1", "rs2", "rs3", "rm", "26..25=0", "6..2=0x10", "1..0=3"]}, "upstream-resolved": {"mask": "0x600007f", "match": "0x43", "variable_fields": ["rd", "rs1", "rs2", "rs3", "rm"]}}, "record_id":"riscv-opcodes:rv_f:fmadd.s@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fmsub_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 27, "name": "rs3", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0x600007f","value": "0x47", "width_bits": 32}, "kind": "instruction-form", "native_name": "fmsub.s", "origin": {"line": 4, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd","rs1", "rs2", "rs3", "rm"], "raw": {"line": "fmsub.s   rd rs1 rs2 rs3 rm 26..25=0 6..2=0x11 1..0=3", "tokens": ["fmsub.s", "rd", "rs1", "rs2", "rs3", "rm", "26..25=0", "6..2=0x11", "1..0=3"]}, "upstream-resolved": {"mask": "0x600007f", "match": "0x47", "variable_fields": ["rd", "rs1", "rs2", "rs3", "rm"]}}, "record_id":"riscv-opcodes:rv_f:fmsub.s@L4", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fnmsub_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 27, "name": "rs3", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0x600007f","value": "0x4b", "width_bits": 32}, "kind": "instruction-form", "native_name": "fnmsub.s", "origin": {"line": 5, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rs2", "rs3", "rm"], "raw": {"line": "fnmsub.s  rd rs1rs2 rs3 rm 26..25=0 6..2=0x12 1..0=3", "tokens": ["fnmsub.s", "rd", "rs1", "rs2", "rs3", "rm", "26..25=0", "6..2=0x12", "1..0=3"]}, "upstream-resolved": {"mask": "0x600007f", "match": "0x4b", "variable_fields": ["rd", "rs1", "rs2", "rs3", "rm"]}}, "record_id": "riscv-opcodes:rv_f:fnmsub.s@L5", "relationships": [], "snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fnmadd_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 27, "name": "rs3", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0x600007f","value": "0x4f", "width_bits": 32}, "kind": "instruction-form", "native_name": "fnmadd.s", "origin": {"line": 6, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rs2", "rs3", "rm"], "raw": {"line": "fnmadd.s  rd rs1rs2 rs3 rm 26..25=0 6..2=0x13 1..0=3", "tokens": ["fnmadd.s", "rd", "rs1", "rs2", "rs3", "rm", "26..25=0", "6..2=0x13", "1..0=3"]}, "upstream-resolved": {"mask": "0x600007f", "match": "0x4f", "variable_fields": ["rd", "rs1", "rs2", "rs3", "rm"]}}, "record_id": "riscv-opcodes:rv_f:fnmadd.s@L6", "relationships": [], "snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fmadd_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 27, "name": "rs3", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0x600007f","value": "0x2000043", "width_bits": 32}, "kind": "instruction-form", "native_name": "fmadd.d", "origin": {"line": 3, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rs2", "rs3", "rm"], "raw": {"line": "fmadd.d   rd rs1 rs2 rs3 rm 26..25=1 6..2=0x10 1..0=3", "tokens": ["fmadd.d", "rd", "rs1", "rs2", "rs3", "rm", "26..25=1", "6..2=0x10", "1..0=3"]}, "upstream-resolved": {"mask": "0x600007f", "match": "0x2000043", "variable_fields": ["rd", "rs1", "rs2", "rs3", "rm"]}}, "record_id": "riscv-opcodes:rv_d:fmadd.d@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fmsub_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 27, "name": "rs3", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0x600007f","value": "0x2000047", "width_bits": 32}, "kind": "instruction-form", "native_name": "fmsub.d", "origin": {"line": 4, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rs2", "rs3", "rm"], "raw": {"line": "fmsub.d   rd rs1 rs2 rs3 rm 26..25=1 6..2=0x11 1..0=3", "tokens": ["fmsub.d", "rd", "rs1", "rs2", "rs3", "rm", "26..25=1", "6..2=0x11", "1..0=3"]}, "upstream-resolved": {"mask": "0x600007f", "match": "0x2000047", "variable_fields": ["rd", "rs1", "rs2", "rs3", "rm"]}}, "record_id": "riscv-opcodes:rv_d:fmsub.d@L4", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fnmsub_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 27, "name": "rs3", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0x600007f","value": "0x200004b", "width_bits": 32}, "kind": "instruction-form", "native_name": "fnmsub.d", "origin": {"line": 5, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rs2", "rs3", "rm"], "raw": {"line": "fnmsub.d  rdrs1 rs2 rs3 rm 26..25=1 6..2=0x12 1..0=3", "tokens": ["fnmsub.d", "rd", "rs1", "rs2", "rs3", "rm", "26..25=1", "6..2=0x12", "1..0=3"]}, "upstream-resolved": {"mask": "0x600007f", "match": "0x200004b", "variable_fields": ["rd", "rs1", "rs2", "rs3", "rm"]}}, "record_id": "riscv-opcodes:rv_d:fnmsub.d@L5", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fnmadd_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}, {"lsb": 27, "name": "rs3", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0x600007f","value": "0x200004f", "width_bits": 32}, "kind": "instruction-form", "native_name": "fnmadd.d", "origin": {"line": 6, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rs2", "rs3", "rm"], "raw": {"line": "fnmadd.d  rdrs1 rs2 rs3 rm 26..25=1 6..2=0x13 1..0=3", "tokens": ["fnmadd.d", "rd", "rs1", "rs2", "rs3", "rm", "26..25=1", "6..2=0x13", "1..0=3"]}, "upstream-resolved": {"mask": "0x600007f", "match": "0x200004f", "variable_fields": ["rd", "rs1", "rs2", "rs3", "rm"]}}, "record_id": "riscv-opcodes:rv_d:fnmadd.d@L6", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_fma () =
  List.iter
    (fun (mnemonic, json) ->
      let rec_ = decode_or_fail mnemonic json in
      let form = normalize_or_fail mnemonic rec_ in
      let expected_feature =
        if String.ends_with ~suffix:".d" mnemonic then "riscv:d" else "riscv:f"
      in
      check
        (mnemonic ^ ": requirement is the expected F/D feature")
        (form.requirement = Isa_norm_model.Req_feature expected_feature);
      check
        (mnemonic ^ ": rd/rs1/rs2/rs3 are floating registers, rm is present but implicit")
        (match form.operands with
        | [
         { op_name = "rd"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
         { op_name = "rs1"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
         { op_name = "rs2"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
         { op_name = "rs3"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
         { op_name = "rm"; op_kind = Rounding_mode; explicit = false; _ };
        ] ->
            true
        | _ -> false);
      check
        (mnemonic ^ ": renders as rd, rs1, rs2, rs3")
        (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1, rs2, rs3"))
    [
      ("fmadd.s", fmadd_s_json);
      ("fmsub.s", fmsub_s_json);
      ("fnmsub.s", fnmsub_s_json);
      ("fnmadd.s", fnmadd_s_json);
      ("fmadd.d", fmadd_d_json);
      ("fmsub.d", fmsub_d_json);
      ("fnmsub.d", fnmsub_d_json);
      ("fnmadd.d", fnmadd_d_json);
    ]

(* feq.s/fle.s/flt.s/feq.d/fle.d/flt.d: rd is a GPR, rs1/rs2 are FP, verbatim-extracted from the checked-in riscv32.jsonl. *)
let feq_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0xa0002053", "width_bits": 32}, "kind": "instruction-form", "native_name": "feq.s", "origin": {"line": 20, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "feq.s     rd rs1 rs2      31..27=0x14 14..12=2 26..25=0 6..2=0x14 1..0=3", "tokens": ["feq.s", "rd", "rs1", "rs2", "31..27=0x14", "14..12=2", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0xa0002053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_f:feq.s@L20", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fle_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0xa0000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fle.s", "origin": {"line": 22, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "fle.s     rd rs1 rs2      31..27=0x14 14..12=0 26..25=0 6..2=0x14 1..0=3", "tokens": ["fle.s", "rd", "rs1", "rs2", "31..27=0x14", "14..12=0", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0xa0000053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_f:fle.s@L22", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let flt_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0xa0001053", "width_bits": 32}, "kind": "instruction-form", "native_name": "flt.s", "origin": {"line": 21, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "flt.s     rd rs1 rs2      31..27=0x14 14..12=1 26..25=0 6..2=0x14 1..0=3", "tokens": ["flt.s", "rd", "rs1", "rs2", "31..27=0x14", "14..12=1", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0xa0001053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_f:flt.s@L21", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let feq_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0xa2002053", "width_bits": 32}, "kind": "instruction-form", "native_name": "feq.d", "origin": {"line": 19, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "feq.d     rd rs1 rs2      31..27=0x14 14..12=2 26..25=1 6..2=0x14 1..0=3", "tokens": ["feq.d", "rd", "rs1", "rs2", "31..27=0x14", "14..12=2", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0xa2002053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_d:feq.d@L19", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fle_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0xa2000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fle.d", "origin": {"line": 21, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "fle.d     rd rs1 rs2      31..27=0x14 14..12=0 26..25=1 6..2=0x14 1..0=3", "tokens": ["fle.d", "rd", "rs1", "rs2", "31..27=0x14", "14..12=0", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0xa2000053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_d:fle.d@L21", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let flt_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 20, "name": "rs2", "width": 5}], "kind": "fixed_bits", "mask": "0xfe00707f", "value": "0xa2001053", "width_bits": 32}, "kind": "instruction-form", "native_name": "flt.d", "origin": {"line": 20, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rs2"], "raw": {"line": "flt.d     rd rs1 rs2      31..27=0x14 14..12=1 26..25=1 6..2=0x14 1..0=3", "tokens": ["flt.d", "rd", "rs1", "rs2", "31..27=0x14", "14..12=1", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfe00707f", "match": "0xa2001053", "variable_fields": ["rd", "rs1", "rs2"]}}, "record_id": "riscv-opcodes:rv_d:flt.d@L20", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_fcmp () =
  List.iter
    (fun (mnemonic, json) ->
      let rec_ = decode_or_fail mnemonic json in
      let form = normalize_or_fail mnemonic rec_ in
      let expected_feature =
        if String.ends_with ~suffix:".d" mnemonic then "riscv:d" else "riscv:f"
      in
      check
        (mnemonic ^ ": requirement is the expected F/D feature")
        (form.requirement = Isa_norm_model.Req_feature expected_feature);
      check
        (mnemonic ^ ": rd is a GPR, rs1/rs2 are floating registers")
        (match form.operands with
        | [
         { op_name = "rd"; op_kind = Register { class_ = Riscv_gpr; _ }; _ };
         { op_name = "rs1"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
         { op_name = "rs2"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
        ] ->
            true
        | _ -> false);
      check
        (mnemonic ^ ": renders as rd, rs1, rs2")
        (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1, rs2"))
    [
      ("feq.s", feq_s_json);
      ("fle.s", fle_s_json);
      ("flt.s", flt_s_json);
      ("feq.d", feq_d_json);
      ("fle.d", fle_d_json);
      ("flt.d", flt_d_json);
    ]

(* fmv.x.w/fmv.w.x: bit-for-bit moves, not conversions, verbatim-extracted from the checked-in riscv32.jsonl. *)
let fmv_x_w_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0xe0000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fmv.x.w", "origin": {"line": 19, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1"], "raw": {"line": "fmv.x.w   rd rs1 24..20=0 31..27=0x1C 14..12=0 26..25=0 6..2=0x14 1..0=3", "tokens": ["fmv.x.w", "rd", "rs1", "24..20=0", "31..27=0x1C", "14..12=0", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0xe0000053", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv_f:fmv.x.w@L19", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fmv_w_x_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 12, "name": "bits[14:12]", "width": 3}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}], "kind": "fixed_bits", "mask": "0xfff0707f", "value": "0xf0000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fmv.w.x", "origin": {"line": 26, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1"], "raw": {"line": "fmv.w.x   rd rs1 24..20=0 31..27=0x1E 14..12=0 26..25=0 6..2=0x14 1..0=3", "tokens": ["fmv.w.x", "rd", "rs1", "24..20=0", "31..27=0x1E", "14..12=0", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0707f", "match": "0xf0000053", "variable_fields": ["rd", "rs1"]}}, "record_id": "riscv-opcodes:rv_f:fmv.w.x@L26", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_fmv_x_w () =
  let rec_ = decode_or_fail "fmv.x.w" fmv_x_w_json in
  let form = normalize_or_fail "fmv.x.w" rec_ in
  check "fmv.x.w: requirement is the F feature"
    (form.requirement = Isa_norm_model.Req_feature "riscv:f");
  check "fmv.x.w: rd is a GPR, rs1 a floating register"
    (match form.operands with
    | [
     { op_name = "rd"; op_kind = Register { class_ = Riscv_gpr; _ }; _ };
     { op_name = "rs1"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
    ] ->
        true
    | _ -> false);
  check "fmv.x.w: renders as rd, rs1" (Isa_norm_model.render_syntax form.syntax = "fmv.x.w rd, rs1")

let test_fmv_w_x () =
  let rec_ = decode_or_fail "fmv.w.x" fmv_w_x_json in
  let form = normalize_or_fail "fmv.w.x" rec_ in
  check "fmv.w.x: requirement is the F feature"
    (form.requirement = Isa_norm_model.Req_feature "riscv:f");
  check "fmv.w.x: rd is a floating register, rs1 a GPR"
    (match form.operands with
    | [
     { op_name = "rd"; op_kind = Register { class_ = Riscv_fpr; _ }; _ };
     { op_name = "rs1"; op_kind = Register { class_ = Riscv_gpr; _ }; _ };
    ] ->
        true
    | _ -> false);
  check "fmv.w.x: renders as rd, rs1" (Isa_norm_model.render_syntax form.syntax = "fmv.w.x rd, rs1")

(* fcvt.w.s/fcvt.wu.s/fcvt.s.w/fcvt.s.wu: the only scalar FP left unclaimed by an earlier
   milestone this session - real conversions (unlike fmv.x.w/fmv.w.x's bit-for-bit moves),
   verbatim-extracted from the checked-in isa-db/export/riscv_opcodes/riscv32.jsonl. *)
let fcvt_w_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xc0000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.w.s", "origin": {"line": 17, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.w.s  rd rs1 24..20=0 31..27=0x18 rm       26..25=0 6..2=0x14 1..0=3", "tokens": ["fcvt.w.s", "rd", "rs1", "24..20=0", "31..27=0x18", "rm", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xc0000053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv_f:fcvt.w.s@L17", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_wu_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xc0100053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.wu.s", "origin": {"line": 18, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.wu.s rd rs1 24..20=1 31..27=0x18 rm       26..25=0 6..2=0x14 1..0=3", "tokens": ["fcvt.wu.s", "rd", "rs1", "24..20=1", "31..27=0x18", "rm", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xc0100053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv_f:fcvt.wu.s@L18", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_s_w_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xd0000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.s.w", "origin": {"line": 24, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.s.w  rd rs1 24..20=0 31..27=0x1A rm       26..25=0 6..2=0x14 1..0=3", "tokens": ["fcvt.s.w", "rd", "rs1", "24..20=0", "31..27=0x1A", "rm", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xd0000053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv_f:fcvt.s.w@L24", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_s_wu_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xd0100053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.s.wu", "origin": {"line": 25, "path": "extensions/rv_f"}, "provenance": {"extension": "rv_f", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.s.wu rd rs1 24..20=1 31..27=0x1A rm       26..25=0 6..2=0x14 1..0=3", "tokens": ["fcvt.s.wu", "rd", "rs1", "24..20=1", "31..27=0x1A", "rm", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xd0100053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv_f:fcvt.s.wu@L25", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_fcvt_w_s () =
  let check_one mnemonic json =
    let rec_ = decode_or_fail mnemonic json in
    let form = normalize_or_fail mnemonic rec_ in
    check
      (mnemonic ^ ": requirement is the F feature")
      (form.requirement = Isa_norm_model.Req_feature "riscv:f");
    check
      (mnemonic ^ ": rd is a GPR, rs1 a floating register, rm implicit dynamic")
      (match form.operands with
      | [
       { op_name = "rd"; op_kind = Register { class_ = Riscv_gpr; _ }; role = Out; _ };
       { op_name = "rs1"; op_kind = Register { class_ = Riscv_fpr; _ }; role = In; _ };
       { op_name = "rm"; op_kind = Rounding_mode; explicit = false; _ };
      ] ->
          true
      | _ -> false);
    check
      (mnemonic ^ ": renders as rd, rs1")
      (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1")
  in
  check_one "fcvt.w.s" fcvt_w_s_json;
  check_one "fcvt.wu.s" fcvt_wu_s_json

let test_fcvt_s_w () =
  let check_one mnemonic json =
    let rec_ = decode_or_fail mnemonic json in
    let form = normalize_or_fail mnemonic rec_ in
    check
      (mnemonic ^ ": requirement is the F feature")
      (form.requirement = Isa_norm_model.Req_feature "riscv:f");
    check
      (mnemonic ^ ": rd is a floating register, rs1 a GPR, rm implicit dynamic")
      (match form.operands with
      | [
       { op_name = "rd"; op_kind = Register { class_ = Riscv_fpr; _ }; role = Out; _ };
       { op_name = "rs1"; op_kind = Register { class_ = Riscv_gpr; _ }; role = In; _ };
       { op_name = "rm"; op_kind = Rounding_mode; explicit = false; _ };
      ] ->
          true
      | _ -> false);
    check
      (mnemonic ^ ": renders as rd, rs1")
      (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1")
  in
  check_one "fcvt.s.w" fcvt_s_w_json;
  check_one "fcvt.s.wu" fcvt_s_wu_json

(* fcvt.w.d/fcvt.wu.d/fcvt.d.w/fcvt.d.wu/fcvt.s.d/fcvt.d.s: the D-extension
   conversions {!test_fcvt_w_s}/{!test_fcvt_s_w} left open - all six already
   had encoder support from an earlier, pre-isa-consumption pass, verbatim-
   extracted from the checked-in isa-db/export/riscv_opcodes/riscv32.jsonl. *)
let fcvt_w_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xc2000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.w.d", "origin": {"line": 23, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.w.d  rd rs1 24..20=0 31..27=0x18 rm       26..25=1 6..2=0x14 1..0=3", "tokens": ["fcvt.w.d", "rd", "rs1", "24..20=0", "31..27=0x18", "rm", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xc2000053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv_d:fcvt.w.d@L23", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_wu_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xc2100053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.wu.d", "origin": {"line": 24, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.wu.d rd rs1 24..20=1 31..27=0x18 rm       26..25=1 6..2=0x14 1..0=3", "tokens": ["fcvt.wu.d", "rd", "rs1", "24..20=1", "31..27=0x18", "rm", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xc2100053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv_d:fcvt.wu.d@L24", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_d_w_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xd2000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.d.w", "origin": {"line": 25, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.d.w  rd rs1 24..20=0 31..27=0x1A rm       26..25=1 6..2=0x14 1..0=3", "tokens": ["fcvt.d.w", "rd", "rs1", "24..20=0", "31..27=0x1A", "rm", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xd2000053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv_d:fcvt.d.w@L25", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_d_wu_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xd2100053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.d.wu", "origin": {"line": 26, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.d.wu rd rs1 24..20=1 31..27=0x1A rm       26..25=1 6..2=0x14 1..0=3", "tokens": ["fcvt.d.wu", "rd", "rs1", "24..20=1", "31..27=0x1A", "rm", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xd2100053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv_d:fcvt.d.wu@L26", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_s_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0x40100053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.s.d", "origin": {"line": 17, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.s.d  rd rs1 24..20=1 31..27=0x08 rm       26..25=0 6..2=0x14 1..0=3", "tokens": ["fcvt.s.d", "rd", "rs1", "24..20=1", "31..27=0x08", "rm", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0x40100053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv_d:fcvt.s.d@L17", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_d_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0x42000053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.d.s", "origin": {"line": 18, "path": "extensions/rv_d"}, "provenance": {"extension": "rv_d", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.d.s  rd rs1 24..20=0 31..27=0x08 rm       26..25=1 6..2=0x14 1..0=3", "tokens": ["fcvt.d.s", "rd", "rs1", "24..20=0", "31..27=0x08", "rm", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0x42000053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv_d:fcvt.d.s@L18", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_fcvt_w_d () =
  let check_one mnemonic json =
    let rec_ = decode_or_fail mnemonic json in
    let form = normalize_or_fail mnemonic rec_ in
    check
      (mnemonic ^ ": requirement is the D feature")
      (form.requirement = Isa_norm_model.Req_feature "riscv:d");
    check
      (mnemonic ^ ": rd is a GPR, rs1 a floating register, rm implicit dynamic")
      (match form.operands with
      | [
       { op_name = "rd"; op_kind = Register { class_ = Riscv_gpr; _ }; role = Out; _ };
       { op_name = "rs1"; op_kind = Register { class_ = Riscv_fpr; _ }; role = In; _ };
       { op_name = "rm"; op_kind = Rounding_mode; explicit = false; _ };
      ] ->
          true
      | _ -> false);
    check
      (mnemonic ^ ": renders as rd, rs1")
      (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1")
  in
  check_one "fcvt.w.d" fcvt_w_d_json;
  check_one "fcvt.wu.d" fcvt_wu_d_json

let test_fcvt_d_w () =
  let check_one mnemonic json =
    let rec_ = decode_or_fail mnemonic json in
    let form = normalize_or_fail mnemonic rec_ in
    check
      (mnemonic ^ ": requirement is the D feature")
      (form.requirement = Isa_norm_model.Req_feature "riscv:d");
    check
      (mnemonic ^ ": rd is a floating register, rs1 a GPR, rm implicit exact")
      (match form.operands with
      | [
       { op_name = "rd"; op_kind = Register { class_ = Riscv_fpr; _ }; role = Out; _ };
       { op_name = "rs1"; op_kind = Register { class_ = Riscv_gpr; _ }; role = In; _ };
       { op_name = "rm"; op_kind = Rounding_mode; explicit = false; _ };
      ] ->
          true
      | _ -> false);
    check
      (mnemonic ^ ": renders as rd, rs1")
      (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1")
  in
  check_one "fcvt.d.w" fcvt_d_w_json;
  check_one "fcvt.d.wu" fcvt_d_wu_json

let test_fcvt_f_f () =
  let check_one mnemonic json =
    let rec_ = decode_or_fail mnemonic json in
    let form = normalize_or_fail mnemonic rec_ in
    check
      (mnemonic ^ ": requirement is the D feature")
      (form.requirement = Isa_norm_model.Req_feature "riscv:d");
    check
      (mnemonic ^ ": rd/rs1 are both floating registers, rm implicit")
      (match form.operands with
      | [
       { op_name = "rd"; op_kind = Register { class_ = Riscv_fpr; _ }; role = Out; _ };
       { op_name = "rs1"; op_kind = Register { class_ = Riscv_fpr; _ }; role = In; _ };
       { op_name = "rm"; op_kind = Rounding_mode; explicit = false; _ };
      ] ->
          true
      | _ -> false);
    check
      (mnemonic ^ ": renders as rd, rs1")
      (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1")
  in
  check_one "fcvt.s.d" fcvt_s_d_json;
  check_one "fcvt.d.s" fcvt_d_s_json

(* fcvt.l.d/fcvt.lu.d/fcvt.d.l/fcvt.d.lu/fcvt.l.s/fcvt.lu.s/fcvt.s.l/fcvt.s.lu:
   the RV64-only long conversions {!test_fcvt_w_d}/{!test_fcvt_d_w} left
   open (they exist only in riscv64.jsonl - rv64_d/rv64_f, not rv_d/rv_f -
   with no riscv32.jsonl counterpart at all), verbatim-extracted from the
   checked-in isa-db/export/riscv_opcodes/riscv64.jsonl. *)
let fcvt_l_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xc2200053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.l.d", "origin": {"line": 2, "path": "extensions/rv64_d"}, "provenance": {"extension": "rv64_d", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.l.d  rd rs1 24..20=2 31..27=0x18 rm       26..25=1 6..2=0x14 1..0=3", "tokens": ["fcvt.l.d", "rd", "rs1", "24..20=2", "31..27=0x18", "rm", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xc2200053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv64_d:fcvt.l.d@L2", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_lu_d_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xc2300053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.lu.d", "origin": {"line": 3, "path": "extensions/rv64_d"}, "provenance": {"extension": "rv64_d", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.lu.d rd rs1 24..20=3 31..27=0x18 rm       26..25=1 6..2=0x14 1..0=3", "tokens": ["fcvt.lu.d", "rd", "rs1", "24..20=3", "31..27=0x18", "rm", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xc2300053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv64_d:fcvt.lu.d@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_d_l_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xd2200053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.d.l", "origin": {"line": 5, "path": "extensions/rv64_d"}, "provenance": {"extension": "rv64_d", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.d.l  rd rs1 24..20=2 31..27=0x1A rm       26..25=1 6..2=0x14 1..0=3", "tokens": ["fcvt.d.l", "rd", "rs1", "24..20=2", "31..27=0x1A", "rm", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xd2200053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv64_d:fcvt.d.l@L5", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_d_lu_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xd2300053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.d.lu", "origin": {"line": 6, "path": "extensions/rv64_d"}, "provenance": {"extension": "rv64_d", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.d.lu rd rs1 24..20=3 31..27=0x1A rm       26..25=1 6..2=0x14 1..0=3", "tokens": ["fcvt.d.lu", "rd", "rs1", "24..20=3", "31..27=0x1A", "rm", "26..25=1", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xd2300053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv64_d:fcvt.d.lu@L6", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_l_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xc0200053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.l.s", "origin": {"line": 3, "path": "extensions/rv64_f"}, "provenance": {"extension": "rv64_f", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.l.s  rd rs1 24..20=2 31..27=0x18 rm       26..25=0 6..2=0x14 1..0=3", "tokens": ["fcvt.l.s", "rd", "rs1", "24..20=2", "31..27=0x18", "rm", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xc0200053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv64_f:fcvt.l.s@L3", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_lu_s_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xc0300053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.lu.s", "origin": {"line": 4, "path": "extensions/rv64_f"}, "provenance": {"extension": "rv64_f", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.lu.s rd rs1 24..20=3 31..27=0x18 rm       26..25=0 6..2=0x14 1..0=3", "tokens": ["fcvt.lu.s", "rd", "rs1", "24..20=3", "31..27=0x18", "rm", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xc0300053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv64_f:fcvt.lu.s@L4", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_s_l_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xd0200053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.s.l", "origin": {"line": 5, "path": "extensions/rv64_f"}, "provenance": {"extension": "rv64_f", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.s.l  rd rs1 24..20=2 31..27=0x1A rm       26..25=0 6..2=0x14 1..0=3", "tokens": ["fcvt.s.l", "rd", "rs1", "24..20=2", "31..27=0x1A", "rm", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xd0200053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv64_f:fcvt.s.l@L5", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let fcvt_s_lu_json =
  {|{"applicability": {"kind": "all", "of": []}, "encoding": {"fields": [{"lsb": 20, "name": "bits[24:20]", "width": 5}, {"lsb": 27, "name": "bits[31:27]", "width": 5}, {"lsb": 25, "name": "bits[26:25]", "width": 2}, {"lsb": 2, "name": "bits[6:2]", "width": 5}, {"lsb": 0, "name": "bits[1:0]", "width": 2}, {"lsb": 7, "name": "rd", "width": 5}, {"lsb": 15, "name": "rs1", "width": 5}, {"lsb": 12, "name": "rm", "width": 3}], "kind": "fixed_bits", "mask": "0xfff0007f", "value": "0xd0300053", "width_bits": 32}, "kind": "instruction-form", "native_name": "fcvt.s.lu", "origin": {"line": 6, "path": "extensions/rv64_f"}, "provenance": {"extension": "rv64_f", "operands": ["rd", "rs1", "rm"], "raw": {"line": "fcvt.s.lu rd rs1 24..20=3 31..27=0x1A rm       26..25=0 6..2=0x14 1..0=3", "tokens": ["fcvt.s.lu", "rd", "rs1", "24..20=3", "31..27=0x1A", "rm", "26..25=0", "6..2=0x14", "1..0=3"]}, "upstream-resolved": {"mask": "0xfff0007f", "match": "0xd0300053", "variable_fields": ["rd", "rs1", "rm"]}}, "record_id": "riscv-opcodes:rv64_f:fcvt.s.lu@L6", "relationships": [], "snapshot": "riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396", "source": "riscv_opcodes", "unresolved": []}|}

let test_fcvt_l_d () =
  let check_one mnemonic feature json =
    let rec_ = decode_or_fail mnemonic json in
    let form = normalize_or_fail mnemonic rec_ in
    check
      (mnemonic ^ ": requirement is RV64 + the expected feature")
      (form.requirement = Isa_norm_model.Req_all [ Req_xlen 64; Req_feature feature ]);
    check
      (mnemonic ^ ": rd is a GPR, rs1 a floating register, rm implicit dynamic")
      (match form.operands with
      | [
       { op_name = "rd"; op_kind = Register { class_ = Riscv_gpr; _ }; role = Out; _ };
       { op_name = "rs1"; op_kind = Register { class_ = Riscv_fpr; _ }; role = In; _ };
       { op_name = "rm"; op_kind = Rounding_mode; explicit = false; _ };
      ] ->
          true
      | _ -> false);
    check
      (mnemonic ^ ": renders as rd, rs1")
      (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1")
  in
  check_one "fcvt.l.d" "riscv:d" fcvt_l_d_json;
  check_one "fcvt.lu.d" "riscv:d" fcvt_lu_d_json;
  check_one "fcvt.l.s" "riscv:f" fcvt_l_s_json;
  check_one "fcvt.lu.s" "riscv:f" fcvt_lu_s_json

let test_fcvt_l_s () =
  let check_one mnemonic feature json =
    let rec_ = decode_or_fail mnemonic json in
    let form = normalize_or_fail mnemonic rec_ in
    check
      (mnemonic ^ ": requirement is RV64 + the expected feature")
      (form.requirement = Isa_norm_model.Req_all [ Req_xlen 64; Req_feature feature ]);
    check
      (mnemonic ^ ": rd is a floating register, rs1 a GPR, rm implicit dynamic")
      (match form.operands with
      | [
       { op_name = "rd"; op_kind = Register { class_ = Riscv_fpr; _ }; role = Out; _ };
       { op_name = "rs1"; op_kind = Register { class_ = Riscv_gpr; _ }; role = In; _ };
       { op_name = "rm"; op_kind = Rounding_mode; explicit = false; _ };
      ] ->
          true
      | _ -> false);
    check
      (mnemonic ^ ": renders as rd, rs1")
      (Isa_norm_model.render_syntax form.syntax = mnemonic ^ " rd, rs1")
  in
  check_one "fcvt.d.l" "riscv:d" fcvt_d_l_json;
  check_one "fcvt.d.lu" "riscv:d" fcvt_d_lu_json;
  check_one "fcvt.s.l" "riscv:f" fcvt_s_l_json;
  check_one "fcvt.s.lu" "riscv:f" fcvt_s_lu_json

let lui_json =
  (* A mnemonic outside both the frozen pilot set and the
     R-type/I-type allowlists (lui is U-type: a single 20-bit immediate, no
     rs1/rs2) - exercises Isa_norm_riscv.normalize's default dispatch case,
     not a different decode path. *)
  {|{"encoding":{"fields":[{"lsb":12,"name":"rd","width":5},{"lsb":0,"name":"imm20","width":20}],"kind":"fixed_bits","mask":"0x7f","value":"0x37","width_bits":32},"kind":"instruction-form","native_name":"lui","origin":{"line":1,"path":"extensions/rv_i"},"provenance":{"extension":"rv_i"},"record_id":"riscv-opcodes:rv_i:lui@L1","snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let test_unhandled_mnemonic () =
  let rec_ = decode_or_fail "lui" lui_json in
  match Isa_norm_riscv.normalize rec_ with
  | Error { rule = "unhandled-native-name"; _ } ->
      check "unhandled mnemonic reports, not fabricates" true
  | _ -> check "unhandled mnemonic reports, not fabricates" false

let add_json =
  {|{"encoding":{"fields":[{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x33","width_bits":32},"kind":"instruction-form","native_name":"add","origin":{"line":1,"path":"extensions/rv_i"},"provenance":{"extension":"rv_i"},"record_id":"riscv-opcodes:rv_i:add@L1","snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let addi_json =
  {|{"encoding":{"fields":[{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"imm12","width":12}],"kind":"fixed_bits","mask":"0x707f","value":"0x13","width_bits":32},"kind":"instruction-form","native_name":"addi","origin":{"line":1,"path":"extensions/rv_i"},"provenance":{"extension":"rv_i"},"record_id":"riscv-opcodes:rv_i:addi@L1","snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let addiw_json =
  {|{"encoding":{"fields":[{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"imm12","width":12}],"kind":"fixed_bits","mask":"0x707f","value":"0x1b","width_bits":32},"kind":"instruction-form","native_name":"addiw","origin":{"line":1,"path":"extensions/rv64_i"},"provenance":{"extension":"rv64_i"},"record_id":"riscv-opcodes:rv64_i:addiw@L1","snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let sltiu_json =
  {|{"encoding":{"fields":[{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"imm12","width":12}],"kind":"fixed_bits","mask":"0x707f","value":"0x3013","width_bits":32},"kind":"instruction-form","native_name":"sltiu","origin":{"line":1,"path":"extensions/rv_i"},"provenance":{"extension":"rv_i"},"record_id":"riscv-opcodes:rv_i:sltiu@L1","snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let mul_json =
  {|{"encoding":{"fields":[{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5}],"kind":"fixed_bits","mask":"0xfe00707f","value":"0x2000033","width_bits":32},"kind":"instruction-form","native_name":"mul","origin":{"line":1,"path":"extensions/rv_m"},"provenance":{"extension":"rv_m"},"record_id":"riscv-opcodes:rv_m:mul@L1","snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let test_r_type_gpr () =
  let rec_ = decode_or_fail "add" add_json in
  let form = normalize_or_fail "add" rec_ in
  check "add: form_id" (form.form_id = "riscv:add");
  check "add: requirement is unconditional (rv_i)" (form.requirement = Isa_norm_model.Req_all []);
  check "add: renders as rd, rs1, rs2"
    (Isa_norm_model.render_syntax form.syntax = "add rd, rs1, rs2");
  let rec_ = decode_or_fail "mul" mul_json in
  let form = normalize_or_fail "mul" rec_ in
  check "mul: form_id" (form.form_id = "riscv:mul");
  check "mul: requirement is the M feature" (form.requirement = Isa_norm_model.Req_feature "riscv:m")

let test_i_type_imm () =
  let rec_ = decode_or_fail "addi" addi_json in
  let form = normalize_or_fail "addi" rec_ in
  check "addi: form_id" (form.form_id = "riscv:addi");
  check "addi: renders as rd, rs1, imm"
    (Isa_norm_model.render_syntax form.syntax = "addi rd, rs1, imm");
  check "addi: imm is a signed 12-bit contiguous immediate"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "imm") form.operands with
    | { op_kind = Immediate { width_bits = 12; signed = true; runs = [ r ]; _ }; _ } ->
        r.field_name = "imm12" && r.dest_hi = 11 && r.dest_lo = 0
    | _ -> false);
  let rec_ = decode_or_fail "addiw" addiw_json in
  let form = normalize_or_fail "addiw" rec_ in
  check "addiw: requirement is the RV64 XLEN predicate"
    (form.requirement = Isa_norm_model.Req_xlen 64);
  check "addiw: carries no diagnostics (XLEN is a modeled requirement, not an unresolved gap)"
    (form.diagnostics = []);
  let rec_ = decode_or_fail "sltiu" sltiu_json in
  let form = normalize_or_fail "sltiu" rec_ in
  check "sltiu: carries the sign-vs-compare diagnostic"
    (List.exists
       (fun (d : Isa_norm_model.diagnostic) -> d.rule = "sltiu-imm-sign-vs-compare")
       form.diagnostics)

(* Decode relationship-resolution's exact/ambiguous/missing outcomes
   faithfully, including ambiguous's genuine one-to-many candidate list -
   taken verbatim from the checked-in isa-db/export/riscv_opcodes/riscv32.jsonl
   at this revision (c.slli's $pseudo_op has no rv64_c sibling in this
   export, aes32dsi's $import resolves to exactly one rv32_zknd record, and
   j's $pseudo_op matches both jal candidates - riscv-opcodes itself is
   ambiguous here, this is not a normalization defect). *)
let c_slli_missing_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":7,"name":"rd_rs1_n0","width":5},{"lsb":2,"name":"c_nzuimm6lo","width":5}],"kind":"fixed_bits","mask":"0xf003","value":"0x2","width_bits":16},"kind":"pseudo-op","native_name":"c.slli","origin":{"line":5,"path":"extensions/rv32_c"},"provenance":{"extension":"rv32_c","operands":["rd_rs1_n0","c_nzuimm6lo"],"raw":{"line":"$pseudo_op rv64_c::c.slli c.slli rd_rs1_n0 c_nzuimm6lo  1..0=2 15..12=0","tokens":["$pseudo_op","rv64_c::c.slli","c.slli","rd_rs1_n0","c_nzuimm6lo","1..0=2","15..12=0"]},"relationship-resolution":[{"candidates":[],"kind":"specializes","status":"missing"}],"specializes-reference":{"extension":"rv64_c","name":"c.slli"},"upstream-resolved":{"mask":"0xf003","match":"0x2","variable_fields":["rd_rs1_n0","c_nzuimm6lo"]}},"record_id":"riscv-opcodes:rv32_c:c.slli@L5","relationships":[{"kind":"specializes","target":"riscv-opcodes:rv64_c:c.slli"}],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":["specializes-target not resolved to a concrete record_id by this adapter"]}|}

let aes32dsi_exact_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":7,"name":"rd","width":5},{"lsb":15,"name":"rs1","width":5},{"lsb":20,"name":"rs2","width":5},{"lsb":30,"name":"bs","width":2}],"kind":"fixed_bits","mask":"0x3e00707f","value":"0x2a000033","width_bits":32},"kind":"import","native_name":"aes32dsi","origin":{"line":14,"path":"extensions/rv32_zk"},"provenance":{"extension":"rv32_zk","import-reference":{"extension":"rv32_zknd","name":"aes32dsi"},"raw":{"line":"$import rv32_zknd::aes32dsi","tokens":["$import","rv32_zknd::aes32dsi"]},"relationship-resolution":[{"candidates":["riscv-opcodes:rv32_zknd:aes32dsi@L3"],"kind":"imports","status":"exact"}],"upstream-resolved":{"mask":"0x3e00707f","match":"0x2a000033","variable_fields":["rd","rs1","rs2","bs"]}},"record_id":"riscv-opcodes:rv32_zk:aes32dsi@L14","relationships":[{"kind":"imports","target":"riscv-opcodes:rv32_zknd:aes32dsi@L3"}],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":[]}|}

let j_ambiguous_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"fields":[{"lsb":12,"name":"jimm20","width":20}],"kind":"fixed_bits","mask":"0xfff","value":"0x6f","width_bits":32},"kind":"pseudo-op","native_name":"j","origin":{"line":76,"path":"extensions/rv_i"},"provenance":{"extension":"rv_i","operands":["jimm20"],"raw":{"line":"$pseudo_op rv_i::jal    j    jimm20                   11..7=0x0  6..2=0x1b 1..0=3","tokens":["$pseudo_op","rv_i::jal","j","jimm20","11..7=0x0","6..2=0x1b","1..0=3"]},"relationship-resolution":[{"candidates":["riscv-opcodes:rv_i:jal@L5","riscv-opcodes:rv_i:jal@L75"],"kind":"specializes","status":"ambiguous"}],"specializes-reference":{"extension":"rv_i","name":"jal"},"upstream-resolved":{"mask":"0xfff","match":"0x6f","variable_fields":["jimm20"]}},"record_id":"riscv-opcodes:rv_i:j@L76","relationships":[{"kind":"specializes","target":"riscv-opcodes:rv_i:jal"}],"snapshot":"riscv_opcodes@7afd3dc8772909d8c94ceeb208467cff93896396","source":"riscv_opcodes","unresolved":["specializes-target not resolved to a concrete record_id by this adapter"]}|}

let relationships_of rec_ =
  match rec_.Isa_source_record.provenance with
  | Isa_source_record.Riscv_provenance { relationships; _ } -> relationships
  | _ -> []

let test_relationship_resolution () =
  let rec_ = decode_or_fail "c.slli (missing)" c_slli_missing_json in
  (match relationships_of rec_ with
  | [ { rel_kind = "specializes"; status = Missing; candidates = [] } ] ->
      check "c.slli: missing specializes relationship, no candidates" true
  | _ -> check "c.slli: missing specializes relationship, no candidates" false);
  let rec_ = decode_or_fail "aes32dsi (exact)" aes32dsi_exact_json in
  (match relationships_of rec_ with
  | [
   { rel_kind = "imports"; status = Exact; candidates = [ "riscv-opcodes:rv32_zknd:aes32dsi@L3" ] };
  ] ->
      check "aes32dsi: exact imports relationship, one candidate" true
  | _ -> check "aes32dsi: exact imports relationship, one candidate" false);
  let rec_ = decode_or_fail "j (ambiguous)" j_ambiguous_json in
  match relationships_of rec_ with
  | [
   {
     rel_kind = "specializes";
     status = Ambiguous;
     candidates = [ "riscv-opcodes:rv_i:jal@L5"; "riscv-opcodes:rv_i:jal@L75" ];
   };
  ] ->
      check "j: ambiguous specializes relationship, both candidates preserved" true
  | _ -> check "j: ambiguous specializes relationship, both candidates preserved" false

let () =
  print_endline "isa-norm-riscv:";
  test_sw ();
  test_beq ();
  test_c_addi ();
  test_sh1add ();
  test_sh2add ();
  test_sh3add ();
  test_sh1adduw ();
  test_sh2adduw ();
  test_sh3adduw ();
  test_min ();
  test_minu ();
  test_max ();
  test_maxu ();
  test_andn ();
  test_orn ();
  test_xnor ();
  test_rol ();
  test_ror ();
  test_clmul ();
  test_clmulh ();
  test_xperm4 ();
  test_xperm8 ();
  test_sha256sum0 ();
  test_sha256sum1 ();
  test_sha256sig0 ();
  test_sha256sig1 ();
  test_sha512sum0 ();
  test_sha512sum1 ();
  test_sha512sig0 ();
  test_sha512sig1 ();
  test_sha512sum0r ();
  test_sha512sum1r ();
  test_sha512sig0l ();
  test_sha512sig1l ();
  test_sha512sig0h ();
  test_sha512sig1h ();
  test_aes64ds ();
  test_aes64dsm ();
  test_aes64es ();
  test_aes64esm ();
  test_aes64ks2 ();
  test_aes64im ();
  test_aes64ks1i ();
  test_aes32dsi ();
  test_aes32dsmi ();
  test_aes32esi ();
  test_aes32esmi ();
  test_csrrw ();
  test_csrrs ();
  test_csrrc ();
  test_csrrwi ();
  test_csrrsi ();
  test_csrrci ();
  test_csrr ();
  test_csrw ();
  test_csrs ();
  test_csrc ();
  test_csrwi ();
  test_csrsi ();
  test_csrci ();
  test_amoadd_w ();
  test_lr_w ();
  test_flw ();
  test_fsw ();
  test_andn_import_record_matches_primary ();
  test_clz ();
  test_ctz ();
  test_cpop ();
  test_sextb ();
  test_sexth ();
  test_orcb ();
  test_clzw ();
  test_ctzw ();
  test_cpopw ();
  test_brev8 ();
  test_brev8_pseudo_record_matches_primary ();
  test_rev8 ();
  test_rev8_pseudo_record_matches_primary ();
  test_rev8_rv32 ();
  test_pack ();
  test_packh ();
  test_packw ();
  test_zip ();
  test_unzip ();
  test_rolw ();
  test_rorw ();
  test_rori ();
  test_rori_rv32 ();
  test_roriw ();
  test_zext_h ();
  test_zext_h_rv32 ();
  test_fadd_s ();
  test_other_f_arith_s ();
  test_fsgnj ();
  test_fminmax ();
  test_fsqrt ();
  test_fclass ();
  test_fma ();
  test_fcmp ();
  test_fmv_x_w ();
  test_fmv_w_x ();
  test_fcvt_w_s ();
  test_fcvt_s_w ();
  test_fcvt_w_d ();
  test_fcvt_d_w ();
  test_fcvt_f_f ();
  test_fcvt_l_d ();
  test_fcvt_l_s ();
  test_vsetvl ();
  test_vsetvli ();
  test_vsetivli ();
  test_vadd_vv ();
  test_vadd_vx ();
  test_vadd_vi ();
  test_vsub_vv ();
  test_vsub_vx ();
  test_vrsub_vx ();
  test_vrsub_vi ();
  test_vand_vv ();
  test_vand_vx ();
  test_vand_vi ();
  test_vor_vv ();
  test_vor_vx ();
  test_vor_vi ();
  test_vxor_vv ();
  test_vxor_vx ();
  test_vxor_vi ();
  test_vsll_vv ();
  test_vsll_vx ();
  test_vsll_vi ();
  test_vsrl_vv ();
  test_vsrl_vx ();
  test_vsrl_vi ();
  test_vsra_vv ();
  test_vsra_vx ();
  test_vsra_vi ();
  test_vminu_vv ();
  test_vminu_vx ();
  test_vmin_vv ();
  test_vmin_vx ();
  test_vmaxu_vv ();
  test_vmaxu_vx ();
  test_vmax_vv ();
  test_vmax_vx ();
  test_vmul_vv ();
  test_vmul_vx ();
  test_vmulh_vv ();
  test_vmulh_vx ();
  test_vmulhu_vv ();
  test_vmulhu_vx ();
  test_vmulhsu_vv ();
  test_vmulhsu_vx ();
  test_vdivu_vv ();
  test_vdivu_vx ();
  test_vdiv_vv ();
  test_vdiv_vx ();
  test_vremu_vv ();
  test_vremu_vx ();
  test_vrem_vv ();
  test_vrem_vx ();
  test_vsaddu_vv ();
  test_vsaddu_vx ();
  test_vsaddu_vi ();
  test_vsadd_vv ();
  test_vsadd_vx ();
  test_vsadd_vi ();
  test_vssubu_vv ();
  test_vssubu_vx ();
  test_vssub_vv ();
  test_vssub_vx ();
  test_vaaddu_vv ();
  test_vaaddu_vx ();
  test_vaadd_vv ();
  test_vaadd_vx ();
  test_vasubu_vv ();
  test_vasubu_vx ();
  test_vasub_vv ();
  test_vasub_vx ();
  test_vnsrl_wv ();
  test_vnsrl_wx ();
  test_vnsrl_wi ();
  test_vnsra_wv ();
  test_vnsra_wx ();
  test_vnsra_wi ();
  test_vnclipu_wv ();
  test_vnclipu_wx ();
  test_vnclipu_wi ();
  test_vnclip_wv ();
  test_vnclip_wx ();
  test_vnclip_wi ();
  test_vssrl_vv ();
  test_vssrl_vx ();
  test_vssrl_vi ();
  test_vssra_vv ();
  test_vssra_vx ();
  test_vssra_vi ();
  test_vrgather_vv ();
  test_vrgather_vx ();
  test_vrgather_vi ();
  test_vrgatherei16_vv ();
  test_vwaddu_vv ();
  test_vwaddu_vx ();
  test_vwadd_vv ();
  test_vwadd_vx ();
  test_vwsubu_vv ();
  test_vwsubu_vx ();
  test_vwsub_vv ();
  test_vwsub_vx ();
  test_vwaddu_wv ();
  test_vwaddu_wx ();
  test_vwadd_wv ();
  test_vwadd_wx ();
  test_vwsubu_wv ();
  test_vwsubu_wx ();
  test_vwsub_wv ();
  test_vwsub_wx ();
  test_vwmulu_vv ();
  test_vwmulu_vx ();
  test_vwmulsu_vv ();
  test_vwmulsu_vx ();
  test_vwmul_vv ();
  test_vwmul_vx ();
  test_vsext_vf2 ();
  test_vsext_vf4 ();
  test_vsext_vf8 ();
  test_vzext_vf2 ();
  test_vzext_vf4 ();
  test_vzext_vf8 ();
  test_vmand_mm ();
  test_vmandn_mm ();
  test_vmor_mm ();
  test_vmxor_mm ();
  test_vmorn_mm ();
  test_vmnand_mm ();
  test_vmnor_mm ();
  test_vmxnor_mm ();
  test_vid_v ();
  test_viota_m ();
  test_vcompress_vm ();
  test_vmsbf_m ();
  test_vmsif_m ();
  test_vmsof_m ();
  test_vcpop_m ();
  test_vfirst_m ();
  test_vadc_vvm ();
  test_vadc_vxm ();
  test_vadc_vim ();
  test_vmadc_vvm ();
  test_vmadc_vxm ();
  test_vmadc_vim ();
  test_vmadc_vv ();
  test_vmadc_vx ();
  test_vmadc_vi ();
  test_vsbc_vvm ();
  test_vsbc_vxm ();
  test_vmsbc_vvm ();
  test_vmsbc_vxm ();
  test_vmsbc_vv ();
  test_vmsbc_vx ();
  test_vmerge_vvm ();
  test_vmerge_vxm ();
  test_vmerge_vim ();
  test_vmv_x_s ();
  test_vmv_s_x ();
  test_vmv_v_v ();
  test_vmv_v_x ();
  test_vmv_v_i ();
  test_vmv1r_v ();
  test_vmv2r_v ();
  test_vmv4r_v ();
  test_vmv8r_v ();
  test_vsmul_vv ();
  test_vsmul_vx ();
  test_vredsum_vs ();
  test_vredand_vs ();
  test_vredor_vs ();
  test_vredxor_vs ();
  test_vredminu_vs ();
  test_vredmin_vs ();
  test_vredmaxu_vs ();
  test_vredmax_vs ();
  test_vwredsumu_vs ();
  test_vwredsum_vs ();
  test_vmseq_vv ();
  test_vmseq_vx ();
  test_vmseq_vi ();
  test_vmsne_vv ();
  test_vmsne_vx ();
  test_vmsne_vi ();
  test_vmsltu_vv ();
  test_vmsltu_vx ();
  test_vmslt_vv ();
  test_vmslt_vx ();
  test_vmsleu_vv ();
  test_vmsleu_vx ();
  test_vmsleu_vi ();
  test_vmsle_vv ();
  test_vmsle_vx ();
  test_vmsle_vi ();
  test_vmsgtu_vx ();
  test_vmsgtu_vi ();
  test_vmsgt_vx ();
  test_vmsgt_vi ();
  test_vslideup_vx ();
  test_vslideup_vi ();
  test_vslidedown_vx ();
  test_vslidedown_vi ();
  test_vslide1up_vx ();
  test_vslide1down_vx ();
  test_vmacc_vv ();
  test_vmacc_vx ();
  test_vnmsac_vv ();
  test_vnmsac_vx ();
  test_vmadd_vv ();
  test_vmadd_vx ();
  test_vnmsub_vv ();
  test_vnmsub_vx ();
  test_vwmaccu_vv ();
  test_vwmaccu_vx ();
  test_vwmacc_vv ();
  test_vwmacc_vx ();
  test_vwmaccsu_vv ();
  test_vwmaccsu_vx ();
  test_vwmaccus_vx ();
  test_r_type_gpr ();
  test_i_type_imm ();
  test_relationship_resolution ();
  test_unhandled_mnemonic ();
  if !failures > 0 then (
    Printf.printf "isa-norm-riscv: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-norm-riscv: all %d checks passed\n" !checks
