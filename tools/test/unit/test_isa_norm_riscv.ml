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

let test_sh1add () =
  let rec_ = decode_or_fail "sh1add" sh1add_json in
  let form = normalize_or_fail "sh1add" rec_ in
  check "sh1add: requirement is the Zba feature"
    (form.requirement = Isa_norm_model.Req_feature "riscv:zba");
  check "sh1add: three plain GPR operands, no immediate"
    (List.length form.operands = 3
    && List.for_all
         (fun (o : Isa_norm_model.operand) ->
           match o.op_kind with
           | Register { class_ = Riscv_gpr; excluded = [] } -> true
           | _ -> false)
         form.operands);
  check "sh1add: renders as rd, rs1, rs2"
    (Isa_norm_model.render_syntax form.syntax = "sh1add rd, rs1, rs2")

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
  check "fadd.s: rm is present but implicit, and flagged with an unresolved diagnostic"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "rm") form.operands with
    | { op_kind = Rounding_mode; explicit = false; _ } ->
        List.exists
          (fun (d : Isa_norm_model.diagnostic) -> d.rule = "fadd.s-rm-default-unverified")
          form.diagnostics
    | _ -> false)

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
  test_fadd_s ();
  test_r_type_gpr ();
  test_i_type_imm ();
  test_relationship_resolution ();
  test_unhandled_mnemonic ();
  if !failures > 0 then (
    Printf.printf "isa-norm-riscv: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-norm-riscv: all %d checks passed\n" !checks
