type entry = {
  form_id : string;
  target : Target.t;
  lookup_key : string;
  case_id : string;
  rule_ids : string list;
  operands : (string * string) list;
  lines_before : string list;
  lines_after : string list;
  configuration : string list;
}

let fixture_dir_name = "isa-difficult"
let cli_group_name = "isa-difficult"
let check_make_target = "asm-isa-difficult-check"
let regen_make_target = "asm-isa-difficult-regen"

(* Both sw and beq avoid x0/ra/sp the way Isa_gen_case_build's pilot
   assignments avoid x86's accumulator - a0/a1/a2 are plain, unremarkable GPRs
   with no special-cased encoding either tool could opportunistically shorten
   into, so a byte match actually exercises the general register field rather
   than a special case. *)
let sw_entry ~target ~offset_label ~offset =
  {
    form_id = "riscv:sw";
    target;
    lookup_key = "sw";
    case_id = Printf.sprintf "riscv:sw:%s:%s" offset_label (Target.to_string target);
    rule_ids = [ offset_label ];
    operands = [ ("value", "a0"); ("base", "a1"); ("offset", offset) ];
    lines_before = [];
    lines_after = [];
    configuration = Isa_gen_case_build.configuration_for target;
  }

let sw_entries =
  List.concat_map
    (fun target ->
      [
        sw_entry ~target ~offset_label:"zero-immediate" ~offset:"0";
        sw_entry ~target ~offset_label:"interior-offset" ~offset:"16";
        sw_entry ~target ~offset_label:"boundary-max-positive-offset" ~offset:"2047";
        sw_entry ~target ~offset_label:"boundary-min-negative-offset" ~offset:"-2048";
      ])
    [ Target.Riscv32; Target.Riscv64 ]

let beq_entry ~target ~direction ~offset ~lines_before ~lines_after =
  {
    form_id = "riscv:beq";
    target;
    lookup_key = "beq";
    case_id = Printf.sprintf "riscv:beq:branch-%s:%s" direction (Target.to_string target);
    rule_ids = [ Printf.sprintf "branch-%s-label" direction ];
    operands = [ ("lhs", "a0"); ("rhs", "a1"); ("offset", offset) ];
    lines_before;
    lines_after;
    configuration = Isa_gen_case_build.configuration_for target;
  }

let beq_entries =
  List.concat_map
    (fun target ->
      [
        beq_entry ~target ~direction:"forward" ~offset:"1f" ~lines_before:[]
          ~lines_after:[ "nop"; "1:" ];
        beq_entry ~target ~direction:"backward" ~offset:"1b" ~lines_before:[ "1:"; "nop" ]
          ~lines_after:[];
      ])
    [ Target.Riscv32; Target.Riscv64 ]

(* c.addi needs the c extension in -march, unlike sw/beq's base-ISA
   configuration - Isa_gen_case_build.configuration_for's rv32im/rv64im does
   not include it and real GAS rejects c.addi without it. *)
let c_addi_configuration_for = function
  | Target.Riscv32 -> [ "-march=rv32imc"; "-mabi=ilp32"; "-mno-relax" ]
  | Target.Riscv64 -> [ "-march=rv64imc"; "-mabi=lp64"; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let c_addi_entry ~target ~imm_label ~imm =
  {
    form_id = "riscv:c.addi";
    target;
    lookup_key = "c.addi";
    case_id = Printf.sprintf "riscv:c.addi:%s:%s" imm_label (Target.to_string target);
    rule_ids = [ imm_label ];
    operands = [ ("acc", "a0"); ("nzimm", imm) ];
    lines_before = [ ".option rvc" ];
    lines_after = [ ".option norvc" ];
    configuration = c_addi_configuration_for target;
  }

let c_addi_entries =
  List.concat_map
    (fun target ->
      [
        c_addi_entry ~target ~imm_label:"boundary-min-positive-nzimm" ~imm:"1";
        c_addi_entry ~target ~imm_label:"boundary-min-negative-nzimm" ~imm:"-1";
        c_addi_entry ~target ~imm_label:"boundary-max-positive-nzimm" ~imm:"31";
        c_addi_entry ~target ~imm_label:"boundary-max-negative-nzimm" ~imm:"-32";
      ])
    [ Target.Riscv32; Target.Riscv64 ]

(* This x86 slice is deliberately bounded to explicit 32-bit [movl] in
   both execution modes.  Each direction has a base+disp8 SIB case and an
   indexed scale-4 disp32 case, so the source-linked forms exercise operand
   order, address-width selection, SIB construction and displacement choice
   without pretending the generic XED GPRv/MEMv forms cover every size. *)
let x86_registers = function
  | Target.X86_32 -> ("esp", "eax", "edx")
  | Target.X86_64 -> ("rsp", "rax", "rdx")
  | _ -> invalid_arg "x86_registers"

let x86_mov_entry ~target ~form_id ~lookup_key ~label ~load ~mem =
  let stack, base, index = x86_registers target in
  let mem =
    match mem with
    | `Stack -> Printf.sprintf "16(%%%s)" stack
    | `Indexed -> Printf.sprintf "1024(%%%s,%%%s,4)" base index
  in
  {
    form_id;
    target;
    lookup_key;
    case_id = Printf.sprintf "%s:%s:%s" form_id label (Target.to_string target);
    rule_ids = [ label; "explicit-32-bit-width" ];
    operands = (if load then [ ("mem", mem); ("reg", "ecx") ] else [ ("reg", "ecx"); ("mem", mem) ]);
    lines_before = [];
    lines_after = [];
    configuration = Isa_gen_case_build.configuration_for target;
  }

let x86_mov_entries =
  List.concat_map
    (fun target ->
      [
        x86_mov_entry ~target ~form_id:"x86:MOV_GPRv_MEMv" ~lookup_key:"MOV_GPRv_MEMv"
          ~label:"load-base-disp8-sib" ~load:true ~mem:`Stack;
        x86_mov_entry ~target ~form_id:"x86:MOV_MEMv_GPRv" ~lookup_key:"MOV_MEMv_GPRv"
          ~label:"store-indexed-scale4-disp32" ~load:false ~mem:`Indexed;
      ])
    [ Target.X86_32; Target.X86_64 ]

let fadd_entry target =
  {
    form_id = "x86:FADD_ST0_X87";
    target;
    lookup_key = "FADD_ST0_X87";
    case_id = Printf.sprintf "x86:FADD_ST0_X87:stack-source-1:%s" (Target.to_string target);
    rule_ids = [ "x87-st1-source"; "implicit-st0-destination" ];
    operands = [ ("src", "1") ];
    lines_before = [];
    lines_after = [];
    configuration = Isa_gen_case_build.configuration_for target;
  }

let x86_fadd_entries = List.map fadd_entry [ Target.X86_32; Target.X86_64 ]
let all = sw_entries @ beq_entries @ c_addi_entries @ x86_mov_entries @ x86_fadd_entries

let pilot_entry_of (entry : entry) =
  let evidence =
    match entry.lookup_key with
    | "sw" -> "store_desc: Sw -> Some 2 (opcode 0x23, funct3 2)"
    | "beq" -> "branch_desc: Beq -> Some 0 (opcode 0x63, funct3 0)"
    | "c.addi" ->
        "Lowered.Caddi/lower_instruction's Opcode.C_addi case (riscv_family_encode.ml); the c.addi \
         implementation slice"
    | "MOV_GPRv_MEMv" ->
        "x86_family_encode.ml's Lowered.Mov_r_rm / mov-r-rm codec alternative (opcode 0x8B)"
    | "MOV_MEMv_GPRv" ->
        "x86_family_encode.ml's Lowered.Mov_rm_r / mov-rm-r codec alternative (opcode 0x89)"
    | "FADD_ST0_X87" ->
        "x86_family_encode.ml's Lowered.Fadd_st0_x87 / fadd-st0-x87 codec alternative (0xD8 0xC0+i)"
    | k -> Printf.sprintf "no recorded store_desc/branch_desc evidence for %s" k
  in
  {
    Isa_gen_pilot.form_id = entry.form_id;
    target = entry.target;
    lookup_key = entry.lookup_key;
    implementation =
      Isa_gen_pilot.Hand_read_table
        { function_name = "source-linked difficult-form implementation table"; evidence };
  }

let normalize_entry repo (entry : entry) = Isa_gen_pilot.normalize_entry repo (pilot_entry_of entry)
let ( let* ) = Result.bind

let build (entry : entry) (form : Isa_norm_model.form) =
  let* line = Isa_gen_render.render_line form.syntax ~operands:entry.operands in
  Ok
    Isa_generated_case.
      {
        case_id = entry.case_id;
        target = entry.target;
        form_id = entry.form_id;
        source_record_ids = form.source_record_ids;
        rule_ids = entry.rule_ids;
        operands = entry.operands;
        rendered_source =
          Isa_gen_render.render_source_lines (entry.lines_before @ [ line ] @ entry.lines_after);
        configuration = entry.configuration;
        negative = false;
      }
