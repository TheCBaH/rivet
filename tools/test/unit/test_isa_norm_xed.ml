(* Isa_norm_xed against the two frozen pilot records:
   ADD_GPRv_IMMz and the x87 form
   FADD_ST0_X87. Taken verbatim (one compact JSON line each, re-serialized
   with sorted keys but otherwise byte-identical) from the checked-in
   isa-db/export/xed_resolved/*.jsonl at this revision. No filesystem
   dependency, matching test_isa_norm_riscv.ml. *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let add_gprv_immz_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"kind":"x86_encoding","opcode":"0x81","opcode_map":0,"operands":[{"bits":null,"lookupfn_name":"GPRv_B","name":"REG0","oc2":null,"rw":"rw","type":"nt_lookup_fn","visibility":"DEFAULT"},{"bits":"1","lookupfn_name":null,"name":"IMM0","oc2":"z","rw":"r","type":"imm_const","visibility":"DEFAULT"}],"pattern":"0x81 MOD[0b11] MOD=3 REG[0b000] RM[nnn] SIMMz()","space":"legacy"},"kind":"instruction-form","native_name":"ADD","origin":{"line":null,"path":"obj/dgen/all-dec-instructions.txt"},"provenance":{"category":"BINARY","extension":"BASE","iform":"ADD_GPRv_IMMz","isa_set":"I86","mode_restriction":"unspecified"},"record_id":"xed:i86:ADD_GPRv_IMMz:0","relationships":[],"snapshot":"xed@0bcb6237345c5066726dcc08b3d87928df3b5b26","source":"xed","unresolved":["no original datafiles/ file:line (see origin)","no directory group - see this module's docstring","no byte-level decode - see encoding.pattern"]}|}

let fadd_st0_x87_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"kind":"x86_encoding","opcode":"0xD8","opcode_map":0,"operands":[{"bits":"XED_REG_ST0","lookupfn_name":null,"name":"REG0","oc2":"f80","rw":"rw","type":"reg","visibility":"IMPLICIT"},{"bits":null,"lookupfn_name":"X87","name":"REG1","oc2":"f80","rw":"r","type":"nt_lookup_fn","visibility":"DEFAULT"},{"bits":"XED_REG_X87STATUS","lookupfn_name":null,"name":"REG2","oc2":null,"rw":"w","type":"reg","visibility":"SUPPRESSED"}],"pattern":"0xD8 MOD[0b11] MOD=3 REG[0b000] RM[nnn]","space":"legacy"},"kind":"instruction-form","native_name":"FADD","origin":{"line":null,"path":"obj/dgen/all-dec-instructions.txt"},"provenance":{"category":"X87_ALU","extension":"X87","iform":"FADD_ST0_X87","isa_set":"X87","mode_restriction":"unspecified"},"record_id":"xed:x87:FADD_ST0_X87:0","relationships":[],"snapshot":"xed@0bcb6237345c5066726dcc08b3d87928df3b5b26","source":"xed","unresolved":["no original datafiles/ file:line (see origin)","no directory group - see this module's docstring","no byte-level decode - see encoding.pattern"]}|}

let add_gprv_gprv_01_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"kind":"x86_encoding","opcode":"0x01","opcode_map":0,"operands":[{"bits":null,"lookupfn_name":"GPRv_B","name":"REG0","oc2":null,"rw":"rw","type":"nt_lookup_fn","visibility":"DEFAULT"},{"bits":null,"lookupfn_name":"GPRv_R","name":"REG1","oc2":null,"rw":"r","type":"nt_lookup_fn","visibility":"DEFAULT"}],"pattern":"0x01 MOD[0b11] MOD=3 REG[rrr] RM[nnn]","space":"legacy"},"kind":"instruction-form","native_name":"ADD","origin":{"line":null,"path":"obj/dgen/all-dec-instructions.txt"},"provenance":{"category":"BINARY","extension":"BASE","iform":"ADD_GPRv_GPRv_01","isa_set":"I86","mode_restriction":"unspecified"},"record_id":"xed:i86:ADD_GPRv_GPRv_01:0","relationships":[],"snapshot":"xed@0bcb6237345c5066726dcc08b3d87928df3b5b26","source":"xed","unresolved":["no original datafiles/ file:line (see origin)","no directory group - see this module's docstring","no byte-level decode - see encoding.pattern"]}|}

let add_gprv_gprv_03_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"kind":"x86_encoding","opcode":"0x03","opcode_map":0,"operands":[{"bits":null,"lookupfn_name":"GPRv_R","name":"REG0","oc2":null,"rw":"rw","type":"nt_lookup_fn","visibility":"DEFAULT"},{"bits":null,"lookupfn_name":"GPRv_B","name":"REG1","oc2":null,"rw":"r","type":"nt_lookup_fn","visibility":"DEFAULT"}],"pattern":"0x03 MOD[0b11] MOD=3 REG[rrr] RM[nnn]","space":"legacy"},"kind":"instruction-form","native_name":"ADD","origin":{"line":null,"path":"obj/dgen/all-dec-instructions.txt"},"provenance":{"category":"BINARY","extension":"BASE","iform":"ADD_GPRv_GPRv_03","isa_set":"I86","mode_restriction":"unspecified"},"record_id":"xed:i86:ADD_GPRv_GPRv_03:0","relationships":[],"snapshot":"xed@0bcb6237345c5066726dcc08b3d87928df3b5b26","source":"xed","unresolved":["no original datafiles/ file:line (see origin)","no directory group - see this module's docstring","no byte-level decode - see encoding.pattern"]}|}

let mov_gprv_gprv_89_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"kind":"x86_encoding","opcode":"0x89","opcode_map":0,"operands":[{"bits":null,"lookupfn_name":"GPRv_B","name":"REG0","oc2":null,"rw":"w","type":"nt_lookup_fn","visibility":"DEFAULT"},{"bits":null,"lookupfn_name":"GPRv_R","name":"REG1","oc2":null,"rw":"r","type":"nt_lookup_fn","visibility":"DEFAULT"}],"pattern":"0x89 MOD[0b11] MOD=3 REG[rrr] RM[nnn]","space":"legacy"},"kind":"instruction-form","native_name":"MOV","origin":{"line":null,"path":"obj/dgen/all-dec-instructions.txt"},"provenance":{"category":"DATAXFER","extension":"BASE","iform":"MOV_GPRv_GPRv_89","isa_set":"I86","mode_restriction":"unspecified"},"record_id":"xed:i86:MOV_GPRv_GPRv_89:0","relationships":[],"snapshot":"xed@0bcb6237345c5066726dcc08b3d87928df3b5b26","source":"xed","unresolved":["no original datafiles/ file:line (see origin)","no directory group - see this module's docstring","no byte-level decode - see encoding.pattern"]}|}

let mov_gprv_gprv_8b_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"kind":"x86_encoding","opcode":"0x8B","opcode_map":0,"operands":[{"bits":null,"lookupfn_name":"GPRv_R","name":"REG0","oc2":null,"rw":"w","type":"nt_lookup_fn","visibility":"DEFAULT"},{"bits":null,"lookupfn_name":"GPRv_B","name":"REG1","oc2":null,"rw":"r","type":"nt_lookup_fn","visibility":"DEFAULT"}],"pattern":"0x8B MOD[0b11] MOD=3 REG[rrr] RM[nnn]","space":"legacy"},"kind":"instruction-form","native_name":"MOV","origin":{"line":null,"path":"obj/dgen/all-dec-instructions.txt"},"provenance":{"category":"DATAXFER","extension":"BASE","iform":"MOV_GPRv_GPRv_8B","isa_set":"I86","mode_restriction":"unspecified"},"record_id":"xed:i86:MOV_GPRv_GPRv_8B:0","relationships":[],"snapshot":"xed@0bcb6237345c5066726dcc08b3d87928df3b5b26","source":"xed","unresolved":["no original datafiles/ file:line (see origin)","no directory group - see this module's docstring","no byte-level decode - see encoding.pattern"]}|}

let mov_gprv_immz_json =
  {|{"applicability":{"kind":"all","of":[]},"encoding":{"kind":"x86_encoding","opcode":"0xC7","opcode_map":0,"operands":[{"bits":null,"lookupfn_name":"GPRv_B","name":"REG0","oc2":null,"rw":"w","type":"nt_lookup_fn","visibility":"DEFAULT"},{"bits":"1","lookupfn_name":null,"name":"IMM0","oc2":"z","rw":"r","type":"imm_const","visibility":"DEFAULT"}],"pattern":"0xC7 MOD[0b11] MOD=3 REG[0b000] RM[nnn] SIMMz()","space":"legacy"},"kind":"instruction-form","native_name":"MOV","origin":{"line":null,"path":"obj/dgen/all-dec-instructions.txt"},"provenance":{"category":"DATAXFER","extension":"BASE","iform":"MOV_GPRv_IMMz","isa_set":"I86","mode_restriction":"unspecified"},"record_id":"xed:i86:MOV_GPRv_IMMz:0","relationships":[],"snapshot":"xed@0bcb6237345c5066726dcc08b3d87928df3b5b26","source":"xed","unresolved":["no original datafiles/ file:line (see origin)","no directory group - see this module's docstring","no byte-level decode - see encoding.pattern"]}|}

let decode_or_fail label json =
  match Isa_source_record.of_line json with
  | Ok r -> r
  | Error msg ->
      check (Printf.sprintf "%s: decodes" label) false;
      failwith msg

let normalize_or_fail label rec_ =
  match Isa_norm_xed.normalize rec_ with
  | Ok form -> form
  | Error d ->
      check (Printf.sprintf "%s: normalizes (%s: %s)" label d.rule d.message) false;
      failwith d.message

let test_add_gprv_immz () =
  let rec_ = decode_or_fail "ADD_GPRv_IMMz" add_gprv_immz_json in
  check "ADD_GPRv_IMMz: decoded provenance.iform"
    (match rec_.provenance with
    | Xed_provenance { iform = Some "ADD_GPRv_IMMz"; _ } -> true
    | _ -> false);
  let form = normalize_or_fail "ADD_GPRv_IMMz" rec_ in
  check "ADD_GPRv_IMMz: form_id" (form.form_id = "x86:ADD_GPRv_IMMz");
  check "ADD_GPRv_IMMz: requirement is unconditional (BASE/I86)"
    (form.requirement = Isa_norm_model.Req_all []);
  check "ADD_GPRv_IMMz: encoding carries opcode 0x81, legacy space"
    (match form.encoding with
    | X86_encoding { space = "legacy"; opcode = "0x81"; opcode_map = 0; _ } -> true
    | _ -> false);
  check "ADD_GPRv_IMMz: dest is read-write, imm's width is left unresolved rather than guessed"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "dest") form.operands with
    | { role = In_out; _ } -> (
        match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "imm") form.operands with
        | { op_kind = Immediate { width_bits = 0; _ }; _ } ->
            List.exists
              (fun (d : Isa_norm_model.diagnostic) -> d.rule = "xed-imm-width-oc2-z")
              form.diagnostics
        | _ -> false)
    | _ -> false);
  check "ADD_GPRv_IMMz: renders in AT&T operand order"
    (Isa_norm_model.render_syntax form.syntax = "add $imm, %dest")

let test_fadd_st0_x87 () =
  let rec_ = decode_or_fail "FADD_ST0_X87" fadd_st0_x87_json in
  let form = normalize_or_fail "FADD_ST0_X87" rec_ in
  check "FADD_ST0_X87: form_id" (form.form_id = "x86:FADD_ST0_X87");
  check "FADD_ST0_X87: requirement is the x87 feature"
    (form.requirement = Isa_norm_model.Req_feature "x86:x87");
  check "FADD_ST0_X87: st0 is implicit read-write"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "st0") form.operands with
    | {
     op_kind = Implicit_register { class_ = X87_st; native_name = "XED_REG_ST0" };
     role = In_out;
     explicit = false;
     _;
    } ->
        true
    | _ -> false);
  check "FADD_ST0_X87: src is an explicit ST(i) register operand"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "src") form.operands with
    | { op_kind = Register { class_ = X87_st; _ }; role = In; explicit = true; _ } -> true
    | _ -> false);
  check "FADD_ST0_X87: the suppressed status-word write is preserved but not a syntax operand"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "status") form.operands with
    | {
     op_kind = Implicit_register { native_name = "XED_REG_X87STATUS"; _ };
     role = Out;
     explicit = false;
     _;
    } ->
        not (List.mem (Isa_norm_model.Syn_operand "status") form.syntax.operands)
    | _ -> false);
  check "FADD_ST0_X87: renders the verified AT&T source-to-implicit-ST0 order"
    (Isa_norm_model.render_syntax form.syntax = "fadd %st(src), %st" && form.diagnostics = [])

let test_two_operand_gprv_reg_reg () =
  (* ADD_GPRv_GPRv_01 and _03 are the two decodable directions of the same
     "add reg, reg" text form (REG0 vs REG1 swap which XED slot is rw); both
     must render identically since AT&T order is derived from rw, not slot
     position. *)
  let rec_01 = decode_or_fail "ADD_GPRv_GPRv_01" add_gprv_gprv_01_json in
  let form_01 = normalize_or_fail "ADD_GPRv_GPRv_01" rec_01 in
  check "ADD_GPRv_GPRv_01: form_id" (form_01.form_id = "x86:ADD_GPRv_GPRv_01");
  check "ADD_GPRv_GPRv_01: renders in AT&T operand order"
    (Isa_norm_model.render_syntax form_01.syntax = "add %src, %dest");
  check "ADD_GPRv_GPRv_01: dest is read-write, src is read-only"
    (match
       ( List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "dest") form_01.operands,
         List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "src") form_01.operands )
     with
    | { role = In_out; _ }, { role = In; _ } -> true
    | _ -> false);
  let rec_03 = decode_or_fail "ADD_GPRv_GPRv_03" add_gprv_gprv_03_json in
  let form_03 = normalize_or_fail "ADD_GPRv_GPRv_03" rec_03 in
  check "ADD_GPRv_GPRv_03: form_id" (form_03.form_id = "x86:ADD_GPRv_GPRv_03");
  check "ADD_GPRv_GPRv_03: renders identically to _01 despite the REG0/REG1 swap"
    (Isa_norm_model.render_syntax form_03.syntax = "add %src, %dest");
  let rec_89 = decode_or_fail "MOV_GPRv_GPRv_89" mov_gprv_gprv_89_json in
  let form_89 = normalize_or_fail "MOV_GPRv_GPRv_89" rec_89 in
  check "MOV_GPRv_GPRv_89: renders in AT&T operand order"
    (Isa_norm_model.render_syntax form_89.syntax = "mov %src, %dest");
  check "MOV_GPRv_GPRv_89: dest is write-only (MOV, not read-modify-write)"
    (match List.find (fun (o : Isa_norm_model.operand) -> o.op_name = "dest") form_89.operands with
    | { role = Out; _ } -> true
    | _ -> false);
  let rec_8b = decode_or_fail "MOV_GPRv_GPRv_8B" mov_gprv_gprv_8b_json in
  let form_8b = normalize_or_fail "MOV_GPRv_GPRv_8B" rec_8b in
  check "MOV_GPRv_GPRv_8B: renders identically to _89 despite the REG0/REG1 swap"
    (Isa_norm_model.render_syntax form_8b.syntax = "mov %src, %dest")

let test_two_operand_gprv_reg_imm () =
  let rec_ = decode_or_fail "MOV_GPRv_IMMz" mov_gprv_immz_json in
  let form = normalize_or_fail "MOV_GPRv_IMMz" rec_ in
  check "MOV_GPRv_IMMz: form_id" (form.form_id = "x86:MOV_GPRv_IMMz");
  check "MOV_GPRv_IMMz: renders as immediate then register"
    (Isa_norm_model.render_syntax form.syntax = "mov $src, %dest");
  check "MOV_GPRv_IMMz: imm width is left unresolved, matching ADD_GPRv_IMMz"
    (List.exists
       (fun (d : Isa_norm_model.diagnostic) -> d.rule = "xed-imm-width-oc2-z")
       form.diagnostics)

(* Applicability, not the raw provenance.mode_restriction fact, now
   drives the requirement - synthetic (hand-modified from add_gprv_immz_json,
   not a verbatim captured record like the fixtures above): a mode64-gated
   BASE form must carry Req_mode, not fall through to a misleading "unmapped
   XED extension: BASE" the old mode_restriction-string heuristic produced
   for this exact shape (real x86_64 iforms like CDQ/MOVSXD hit it). *)
let add_gprv_immz_mode64_json =
  {|{"applicability":{"equals":"mode64","kind":"mode"},"encoding":{"kind":"x86_encoding","opcode":"0x81","opcode_map":0,"operands":[{"bits":null,"lookupfn_name":"GPRv_B","name":"REG0","oc2":null,"rw":"rw","type":"nt_lookup_fn","visibility":"DEFAULT"},{"bits":"1","lookupfn_name":null,"name":"IMM0","oc2":"z","rw":"r","type":"imm_const","visibility":"DEFAULT"}],"pattern":"0x81 MOD[0b11] MOD=3 REG[0b000] RM[nnn] SIMMz()","space":"legacy"},"kind":"instruction-form","native_name":"ADD","origin":{"line":null,"path":"obj/dgen/all-dec-instructions.txt"},"provenance":{"category":"BINARY","extension":"BASE","iform":"ADD_GPRv_IMMz","isa_set":"I86","mode_restriction":2},"record_id":"xed:i86:ADD_GPRv_IMMz:0","relationships":[],"snapshot":"xed@0bcb6237345c5066726dcc08b3d87928df3b5b26","source":"xed","unresolved":[]}|}

let test_mode64_requirement () =
  let rec_ = decode_or_fail "ADD_GPRv_IMMz (mode64)" add_gprv_immz_mode64_json in
  let form = normalize_or_fail "ADD_GPRv_IMMz (mode64)" rec_ in
  check "mode64-gated BASE form carries Req_mode, not Req_all []"
    (form.requirement = Isa_norm_model.Req_mode { mode = "mode64"; equals = true })

let test_unhandled_iform () =
  let json =
    {|{"encoding":{"kind":"x86_encoding","opcode":"0x00","opcode_map":0,"operands":[],"pattern":"","space":"legacy"},"kind":"instruction-form","native_name":"NOP","origin":{"path":"obj/dgen/all-dec-instructions.txt"},"provenance":{"category":"WIDENOP","extension":"BASE","iform":"NOP","isa_set":"I86","mode_restriction":"unspecified"},"record_id":"xed:i86:NOP:0","snapshot":"xed@0bcb6237345c5066726dcc08b3d87928df3b5b26","source":"xed","unresolved":[]}|}
  in
  let rec_ = decode_or_fail "NOP" json in
  match Isa_norm_xed.normalize rec_ with
  | Error { rule = "unhandled-iform"; _ } -> check "unhandled iform reports, not fabricates" true
  | _ -> check "unhandled iform reports, not fabricates" false

let () =
  print_endline "isa-norm-xed:";
  test_add_gprv_immz ();
  test_fadd_st0_x87 ();
  test_two_operand_gprv_reg_reg ();
  test_two_operand_gprv_reg_imm ();
  test_mode64_requirement ();
  test_unhandled_iform ();
  if !failures > 0 then (
    Printf.printf "isa-norm-xed: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-norm-xed: all %d checks passed\n" !checks
