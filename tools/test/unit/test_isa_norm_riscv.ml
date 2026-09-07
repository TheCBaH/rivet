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
  test_r_type_gpr ();
  test_i_type_imm ();
  test_relationship_resolution ();
  test_unhandled_mnemonic ();
  if !failures > 0 then (
    Printf.printf "isa-norm-riscv: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-norm-riscv: all %d checks passed\n" !checks
