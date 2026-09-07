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
  test_r_type_gpr ();
  test_i_type_imm ();
  test_relationship_resolution ();
  test_unhandled_mnemonic ();
  if !failures > 0 then (
    Printf.printf "isa-norm-riscv: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-norm-riscv: all %d checks passed\n" !checks
