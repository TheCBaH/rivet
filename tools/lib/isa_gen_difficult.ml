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

(* Validates the source normalizer's intentionally bare scalar
   arithmetic spellings. GNU as selects rm=dyn (funct3=7)
   on both profiles; explicit rounding-mode spellings and the rest of F/D
   remain separate work. *)
let f_arith_configuration_for precision = function
  | Target.Riscv32 -> [ "-march=rv32im" ^ precision; "-mabi=ilp32" ^ precision; "-mno-relax" ]
  | Target.Riscv64 -> [ "-march=rv64im" ^ precision; "-mabi=lp64" ^ precision; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let f_arith_entry mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:bare-dynamic-rounding:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "implicit-dynamic-rounding" ];
    operands = [ ("rd", "ft0"); ("rs1", "ft1"); ("rs2", "ft2") ];
    lines_before = [];
    lines_after = [];
    configuration =
      f_arith_configuration_for (if String.ends_with ~suffix:".d" mnemonic then "d" else "f") target;
  }

let fadd_s_entries = List.map (f_arith_entry "fadd.s") [ Target.Riscv32; Target.Riscv64 ]
let fsub_s_entries = List.map (f_arith_entry "fsub.s") [ Target.Riscv32; Target.Riscv64 ]
let fmul_s_entries = List.map (f_arith_entry "fmul.s") [ Target.Riscv32; Target.Riscv64 ]
let fdiv_s_entries = List.map (f_arith_entry "fdiv.s") [ Target.Riscv32; Target.Riscv64 ]
let fadd_d_entries = List.map (f_arith_entry "fadd.d") [ Target.Riscv32; Target.Riscv64 ]
let fsub_d_entries = List.map (f_arith_entry "fsub.d") [ Target.Riscv32; Target.Riscv64 ]
let fmul_d_entries = List.map (f_arith_entry "fmul.d") [ Target.Riscv32; Target.Riscv64 ]
let fdiv_d_entries = List.map (f_arith_entry "fdiv.d") [ Target.Riscv32; Target.Riscv64 ]

(* flw/fld/fsw/fsd: F/D's floating-point loads and stores - already fully
   implemented by the encoder's shared f_load_desc/f_store_desc path (this
   closes only the normalization/corpus/admission side, no encoder change).
   Reuses f_arith_configuration_for's own "im"+precision march convention;
   confirmed real GNU as rejects fld/fsd under a bare "f"-only march
   ("extension `d' required") and accepts all four under "d" (D implies F). *)
let f_ldst_entry ~mnemonic ~precision target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:interior-offset:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "interior-offset" ];
    operands = [ ("value", "fa0"); ("base", "a1"); ("offset", "8") ];
    lines_before = [];
    lines_after = [];
    configuration = f_arith_configuration_for precision target;
  }

let flw_entries =
  List.map (f_ldst_entry ~mnemonic:"flw" ~precision:"f") [ Target.Riscv32; Target.Riscv64 ]

let fld_entries =
  List.map (f_ldst_entry ~mnemonic:"fld" ~precision:"d") [ Target.Riscv32; Target.Riscv64 ]

let fsw_entries =
  List.map (f_ldst_entry ~mnemonic:"fsw" ~precision:"f") [ Target.Riscv32; Target.Riscv64 ]

let fsd_entries =
  List.map (f_ldst_entry ~mnemonic:"fsd" ~precision:"d") [ Target.Riscv32; Target.Riscv64 ]

(* fsgnj.s/fsgnjn.s/fsgnjx.s/fsgnj.d/fsgnjn.d/fsgnjx.d: the general
   three-distinct-FP-register sign-injection form the encoder's own
   f_sgnj3_desc closes (its rs1=rs2 alias siblings fneg.s/fneg.d/fmv.d
   predate this pass). Distinct rs1/rs2 registers here is what
   exercises the general form rather than the pseudo-alias one. *)
let f_sgnj_entry mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id =
      Printf.sprintf "riscv:%s:distinct-source-registers:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "distinct-source-registers" ];
    operands = [ ("rd", "ft0"); ("rs1", "ft1"); ("rs2", "ft2") ];
    lines_before = [];
    lines_after = [];
    configuration =
      f_arith_configuration_for (if String.ends_with ~suffix:".d" mnemonic then "d" else "f") target;
  }

let fsgnj_s_entries = List.map (f_sgnj_entry "fsgnj.s") [ Target.Riscv32; Target.Riscv64 ]
let fsgnjn_s_entries = List.map (f_sgnj_entry "fsgnjn.s") [ Target.Riscv32; Target.Riscv64 ]
let fsgnjx_s_entries = List.map (f_sgnj_entry "fsgnjx.s") [ Target.Riscv32; Target.Riscv64 ]
let fsgnj_d_entries = List.map (f_sgnj_entry "fsgnj.d") [ Target.Riscv32; Target.Riscv64 ]
let fsgnjn_d_entries = List.map (f_sgnj_entry "fsgnjn.d") [ Target.Riscv32; Target.Riscv64 ]
let fsgnjx_d_entries = List.map (f_sgnj_entry "fsgnjx.d") [ Target.Riscv32; Target.Riscv64 ]

(* fmin.s/fmax.s/fmin.d/fmax.d: the same three-distinct-FP-register shape as
   {!f_sgnj_entry}, but no pseudo-alias sharing this word to distinguish
   from - "min-max-select" documents which of the pair the fixed funct3
   selects, not a register-identity concern. *)
let f_minmax_entry mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:min-max-select:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "min-max-select" ];
    operands = [ ("rd", "ft0"); ("rs1", "ft1"); ("rs2", "ft2") ];
    lines_before = [];
    lines_after = [];
    configuration =
      f_arith_configuration_for (if String.ends_with ~suffix:".d" mnemonic then "d" else "f") target;
  }

let fmin_s_entries = List.map (f_minmax_entry "fmin.s") [ Target.Riscv32; Target.Riscv64 ]
let fmax_s_entries = List.map (f_minmax_entry "fmax.s") [ Target.Riscv32; Target.Riscv64 ]
let fmin_d_entries = List.map (f_minmax_entry "fmin.d") [ Target.Riscv32; Target.Riscv64 ]
let fmax_d_entries = List.map (f_minmax_entry "fmax.d") [ Target.Riscv32; Target.Riscv64 ]

(* fsqrt.s/fsqrt.d: {!f_arith_entry}'s own shape minus the third operand -
   [rs2] is a fixed selector the source record's own mask fixes, not a
   real register. *)
let f_sqrt_entry mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:bare-dynamic-rounding:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "implicit-dynamic-rounding" ];
    operands = [ ("rd", "ft0"); ("rs1", "ft1") ];
    lines_before = [];
    lines_after = [];
    configuration =
      f_arith_configuration_for (if String.ends_with ~suffix:".d" mnemonic then "d" else "f") target;
  }

let fsqrt_s_entries = List.map (f_sqrt_entry "fsqrt.s") [ Target.Riscv32; Target.Riscv64 ]
let fsqrt_d_entries = List.map (f_sqrt_entry "fsqrt.d") [ Target.Riscv32; Target.Riscv64 ]

(* fclass.s/fclass.d: [rd] is a GPR (the classification bitmask), [rs1] FP -
   the mirror image of {!f_ldst_entry}'s value/base register-class split,
   here on a plain two-register R-type instead of a load/store. *)
let f_class_entry mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:gpr-result:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "gpr-result" ];
    operands = [ ("rd", "a0"); ("rs1", "ft1") ];
    lines_before = [];
    lines_after = [];
    configuration =
      f_arith_configuration_for (if String.ends_with ~suffix:".d" mnemonic then "d" else "f") target;
  }

let fclass_s_entries = List.map (f_class_entry "fclass.s") [ Target.Riscv32; Target.Riscv64 ]
let fclass_d_entries = List.map (f_class_entry "fclass.d") [ Target.Riscv32; Target.Riscv64 ]

(* fmadd.s/fmsub.s/fnmsub.s/fnmadd.s/fmadd.d/fmsub.d/fnmsub.d/fnmadd.d:
   {!f_sqrt_entry}'s own implicit-dynamic-rounding shape with two more
   distinct FP register operands ([rs2], [rs3]) - RISC-V's only R4-type
   mnemonics. *)
let f_fma_entry mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:bare-dynamic-rounding:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "implicit-dynamic-rounding" ];
    operands = [ ("rd", "ft0"); ("rs1", "ft1"); ("rs2", "ft2"); ("rs3", "ft3") ];
    lines_before = [];
    lines_after = [];
    configuration =
      f_arith_configuration_for (if String.ends_with ~suffix:".d" mnemonic then "d" else "f") target;
  }

let fmadd_s_entries = List.map (f_fma_entry "fmadd.s") [ Target.Riscv32; Target.Riscv64 ]
let fmsub_s_entries = List.map (f_fma_entry "fmsub.s") [ Target.Riscv32; Target.Riscv64 ]
let fnmsub_s_entries = List.map (f_fma_entry "fnmsub.s") [ Target.Riscv32; Target.Riscv64 ]
let fnmadd_s_entries = List.map (f_fma_entry "fnmadd.s") [ Target.Riscv32; Target.Riscv64 ]
let fmadd_d_entries = List.map (f_fma_entry "fmadd.d") [ Target.Riscv32; Target.Riscv64 ]
let fmsub_d_entries = List.map (f_fma_entry "fmsub.d") [ Target.Riscv32; Target.Riscv64 ]
let fnmsub_d_entries = List.map (f_fma_entry "fnmsub.d") [ Target.Riscv32; Target.Riscv64 ]
let fnmadd_d_entries = List.map (f_fma_entry "fnmadd.d") [ Target.Riscv32; Target.Riscv64 ]

(* feq.s/fle.s/flt.s/feq.d/fle.d/flt.d: [rd] is a GPR (the boolean result),
   [rs1]/[rs2] are FP - the mirror image of {!f_fma_entry}'s all-FPR shape. *)
let f_cmp_entry mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:gpr-result:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "gpr-result" ];
    operands = [ ("rd", "a0"); ("rs1", "ft1"); ("rs2", "ft2") ];
    lines_before = [];
    lines_after = [];
    configuration =
      f_arith_configuration_for (if String.ends_with ~suffix:".d" mnemonic then "d" else "f") target;
  }

let feq_s_entries = List.map (f_cmp_entry "feq.s") [ Target.Riscv32; Target.Riscv64 ]
let fle_s_entries = List.map (f_cmp_entry "fle.s") [ Target.Riscv32; Target.Riscv64 ]
let flt_s_entries = List.map (f_cmp_entry "flt.s") [ Target.Riscv32; Target.Riscv64 ]
let feq_d_entries = List.map (f_cmp_entry "feq.d") [ Target.Riscv32; Target.Riscv64 ]
let fle_d_entries = List.map (f_cmp_entry "fle.d") [ Target.Riscv32; Target.Riscv64 ]
let flt_d_entries = List.map (f_cmp_entry "flt.d") [ Target.Riscv32; Target.Riscv64 ]

(* fmv.x.w: bit-for-bit move (not a conversion), {!f_class_entry}'s own
   GPR-result shape but with a plain FP source instead of a classification. *)
let f_mv_x_w_entry target =
  {
    form_id = "riscv:fmv.x.w";
    target;
    lookup_key = "fmv.x.w";
    case_id = Printf.sprintf "riscv:fmv.x.w:gpr-result:%s" (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "gpr-result" ];
    operands = [ ("rd", "a0"); ("rs1", "ft1") ];
    lines_before = [];
    lines_after = [];
    configuration = f_arith_configuration_for "f" target;
  }

let fmv_x_w_entries = List.map f_mv_x_w_entry [ Target.Riscv32; Target.Riscv64 ]

(* fmv.w.x: {!f_mv_x_w_entry}'s reverse-direction sibling - [rd] FP, [rs1] a
   plain GPR. *)
let f_mv_w_x_entry target =
  {
    form_id = "riscv:fmv.w.x";
    target;
    lookup_key = "fmv.w.x";
    case_id = Printf.sprintf "riscv:fmv.w.x:fpr-result:%s" (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "fpr-result" ];
    operands = [ ("rd", "ft0"); ("rs1", "a1") ];
    lines_before = [];
    lines_after = [];
    configuration = f_arith_configuration_for "f" target;
  }

let fmv_w_x_entries = List.map f_mv_w_x_entry [ Target.Riscv32; Target.Riscv64 ]

(* fcvt.w.s/fcvt.wu.s: {!f_sqrt_entry}'s own implicit-dynamic-rounding shape
   with [rd] a GPR (the converted integer) instead of FP - {!f_class_entry}'s
   own register-class split, but with a genuine rounding-mode operand rather
   than [fclass]'s fixed one. *)
let f_cvt_w_s_entry mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:bare-dynamic-rounding:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "gpr-result"; "implicit-dynamic-rounding" ];
    operands = [ ("rd", "a0"); ("rs1", "ft1") ];
    lines_before = [];
    lines_after = [];
    configuration = f_arith_configuration_for "f" target;
  }

let fcvt_w_s_entries = List.map (f_cvt_w_s_entry "fcvt.w.s") [ Target.Riscv32; Target.Riscv64 ]
let fcvt_wu_s_entries = List.map (f_cvt_w_s_entry "fcvt.wu.s") [ Target.Riscv32; Target.Riscv64 ]

(* fcvt.s.w/fcvt.s.wu: {!f_cvt_w_s_entry}'s reverse direction - [rd] FP,
   [rs1] a plain GPR, same genuine dynamic-rounding [rm]. *)
let f_cvt_s_w_entry mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:bare-dynamic-rounding:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "fpr-result"; "implicit-dynamic-rounding" ];
    operands = [ ("rd", "ft0"); ("rs1", "a1") ];
    lines_before = [];
    lines_after = [];
    configuration = f_arith_configuration_for "f" target;
  }

let fcvt_s_w_entries = List.map (f_cvt_s_w_entry "fcvt.s.w") [ Target.Riscv32; Target.Riscv64 ]
let fcvt_s_wu_entries = List.map (f_cvt_s_w_entry "fcvt.s.wu") [ Target.Riscv32; Target.Riscv64 ]

(* fcvt.w.d/fcvt.wu.d: {!f_cvt_w_s_entry}'s own shape and dynamic-rounding
   default, but the D-extension configuration instead of F. *)
let f_cvt_w_d_entry mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:bare-dynamic-rounding:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "gpr-result"; "implicit-dynamic-rounding" ];
    operands = [ ("rd", "a0"); ("rs1", "ft1") ];
    lines_before = [];
    lines_after = [];
    configuration = f_arith_configuration_for "d" target;
  }

let fcvt_w_d_entries = List.map (f_cvt_w_d_entry "fcvt.w.d") [ Target.Riscv32; Target.Riscv64 ]
let fcvt_wu_d_entries = List.map (f_cvt_w_d_entry "fcvt.wu.d") [ Target.Riscv32; Target.Riscv64 ]

(* fcvt.l.d/fcvt.lu.d: {!f_cvt_w_d_entry}'s own shape and configuration,
   RV64-only (no RV32 counterpart at all - riscv-opcodes only exports these
   under rv64_d). *)
let fcvt_l_d_entries = List.map (f_cvt_w_d_entry "fcvt.l.d") [ Target.Riscv64 ]
let fcvt_lu_d_entries = List.map (f_cvt_w_d_entry "fcvt.lu.d") [ Target.Riscv64 ]

(* fcvt.l.s/fcvt.lu.s: {!f_cvt_w_s_entry}'s own shape and F configuration,
   RV64-only (rv64_f). *)
let fcvt_l_s_entries = List.map (f_cvt_w_s_entry "fcvt.l.s") [ Target.Riscv64 ]
let fcvt_lu_s_entries = List.map (f_cvt_w_s_entry "fcvt.lu.s") [ Target.Riscv64 ]

(* fcvt.s.l/fcvt.s.lu: {!f_cvt_s_w_entry}'s own shape and F configuration,
   RV64-only (rv64_f). *)
let fcvt_s_l_entries = List.map (f_cvt_s_w_entry "fcvt.s.l") [ Target.Riscv64 ]
let fcvt_s_lu_entries = List.map (f_cvt_s_w_entry "fcvt.s.lu") [ Target.Riscv64 ]

(* fcvt.d.w/fcvt.d.wu: {!f_cvt_s_w_entry}'s own shape ([rd] FP, [rs1] a GPR),
   but real hardware's "always exact" rne default rather than dynamic
   rounding - a 32-bit integer always converts to double exactly. *)
let f_cvt_d_w_entry mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:bare-exact-rounding:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "fpr-result"; "implicit-exact-rounding" ];
    operands = [ ("rd", "ft0"); ("rs1", "a1") ];
    lines_before = [];
    lines_after = [];
    configuration = f_arith_configuration_for "d" target;
  }

let fcvt_d_w_entries = List.map (f_cvt_d_w_entry "fcvt.d.w") [ Target.Riscv32; Target.Riscv64 ]
let fcvt_d_wu_entries = List.map (f_cvt_d_w_entry "fcvt.d.wu") [ Target.Riscv32; Target.Riscv64 ]

(* fcvt.d.l/fcvt.d.lu: {!f_cvt_d_w_entry}'s own shape ([rd] FP, [rs1] a GPR)
   and D configuration, but the family's usual dynamic-rounding default
   rather than {!f_cvt_d_w_entry}'s always-exact one - a 64-bit long does
   not always fit exactly in a double, unlike a 32-bit int. RV64-only (no
   RV32 counterpart - riscv-opcodes only exports these under rv64_d). *)
let f_cvt_d_l_entry mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:bare-dynamic-rounding:%s" mnemonic (Target.to_string target);
    rule_ids = [ "fpr-register-class"; "fpr-result"; "implicit-dynamic-rounding" ];
    operands = [ ("rd", "ft0"); ("rs1", "a1") ];
    lines_before = [];
    lines_after = [];
    configuration = f_arith_configuration_for "d" target;
  }

let fcvt_d_l_entries = List.map (f_cvt_d_l_entry "fcvt.d.l") [ Target.Riscv64 ]
let fcvt_d_lu_entries = List.map (f_cvt_d_l_entry "fcvt.d.lu") [ Target.Riscv64 ]

(* fcvt.s.d/fcvt.d.s: float-to-float precision converts, [rd]/[rs1] both FP -
   no GPR involved, so no register-class-mismatch tag - [fcvt.s.d]
   (narrowing) keeps the family's usual dynamic-rounding default,
   [fcvt.d.s] (widening, always exact) gets {!f_cvt_d_w_entry}'s own
   exact-rounding tag instead. *)
let f_cvt_f_f_entry mnemonic target =
  let exact = String.equal mnemonic "fcvt.d.s" in
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id =
      Printf.sprintf "riscv:%s:bare-%s-rounding:%s" mnemonic
        (if exact then "exact" else "dynamic")
        (Target.to_string target);
    rule_ids =
      [
        "fpr-register-class";
        (if exact then "implicit-exact-rounding" else "implicit-dynamic-rounding");
      ];
    operands = [ ("rd", "ft0"); ("rs1", "ft1") ];
    lines_before = [];
    lines_after = [];
    configuration = f_arith_configuration_for "d" target;
  }

let fcvt_s_d_entries = List.map (f_cvt_f_f_entry "fcvt.s.d") [ Target.Riscv32; Target.Riscv64 ]
let fcvt_d_s_entries = List.map (f_cvt_f_f_entry "fcvt.d.s") [ Target.Riscv32; Target.Riscv64 ]

(* Zba's SH1ADD/SH2ADD/SH3ADD are the three scale variants of the same
   R-type shape (opcode 0x33, funct7 0x10, funct3 selects the shift amount),
   already normalized by the generic riscv:r_type_gpr_form path. Their *.uw
   word-operand siblings (below) close out Zba's non-word/word split; Zbb
   remains distinct work. *)
let zba_configuration_for = function
  | Target.Riscv32 -> [ "-march=rv32im_zba"; "-mabi=ilp32"; "-mno-relax" ]
  | Target.Riscv64 -> [ "-march=rv64im_zba"; "-mabi=lp64"; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let zba_shadd_entry ~mnemonic ~scale_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic scale_name (Target.to_string target);
    rule_ids = [ "zba-enabled"; "three-gpr-operands"; scale_name ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("rs2", "a2") ];
    lines_before = [];
    lines_after = [];
    configuration = zba_configuration_for target;
  }

let sh1add_entries =
  List.map
    (zba_shadd_entry ~mnemonic:"sh1add" ~scale_name:"scale-one")
    [ Target.Riscv32; Target.Riscv64 ]

let sh2add_entries =
  List.map
    (zba_shadd_entry ~mnemonic:"sh2add" ~scale_name:"scale-two")
    [ Target.Riscv32; Target.Riscv64 ]

let sh3add_entries =
  List.map
    (zba_shadd_entry ~mnemonic:"sh3add" ~scale_name:"scale-three")
    [ Target.Riscv32; Target.Riscv64 ]

(* Zba's *.uw word-operand variants: the same three scale amounts on opcode
   0x3b (funct7 0x10, same funct3-per-scale assignment as the plain forms),
   restricted to RV64 - riscv-opcodes only exports rv64_zba:sh<n>add.uw, with
   no RV32 counterpart at all, matching addw's own single-target
   precedent rather than a new restriction mechanism. *)
let sh1adduw_entries =
  [ zba_shadd_entry ~mnemonic:"sh1add.uw" ~scale_name:"scale-one-uw" Target.Riscv64 ]

let sh2adduw_entries =
  [ zba_shadd_entry ~mnemonic:"sh2add.uw" ~scale_name:"scale-two-uw" Target.Riscv64 ]

let sh3adduw_entries =
  [ zba_shadd_entry ~mnemonic:"sh3add.uw" ~scale_name:"scale-three-uw" Target.Riscv64 ]

(* Zbb's MIN/MINU/MAX/MAXU are plain R-type comparisons (opcode 0x33, funct7
   0x05, funct3 selects signed/unsigned and min/max), each present as exactly
   one riscv-opcodes record. ANDN/ORN/XNOR/ROL/ROR (below) are the same
   three-plain-GPR-operand shape, but the same riscv-opcodes snapshot lists
   each of them identically under five different extension files (rv_zbb,
   rv_zbkb, rv_zk, rv_zkn, rv_zks) - Isa_norm_riscv.requirement_of_mnemonic
   now expresses that as Req_any over the five, and this generator picks
   rv_zbb (the primary, non-import record) as the one configuration actually
   exercised here, the same "one configuration proves promotion" discipline
   every other slice here uses; asserting GNU agreement under the other
   four extension names is not this slice's scope. *)
let zbb_configuration_for = function
  | Target.Riscv32 -> [ "-march=rv32im_zbb"; "-mabi=ilp32"; "-mno-relax" ]
  | Target.Riscv64 -> [ "-march=rv64im_zbb"; "-mabi=lp64"; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let zbb_r_type_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zbb-enabled"; "three-gpr-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("rs2", "a2") ];
    lines_before = [];
    lines_after = [];
    configuration = zbb_configuration_for target;
  }

let min_entries =
  List.map
    (zbb_r_type_entry ~mnemonic:"min" ~variant_name:"signed-min")
    [ Target.Riscv32; Target.Riscv64 ]

let minu_entries =
  List.map
    (zbb_r_type_entry ~mnemonic:"minu" ~variant_name:"unsigned-min")
    [ Target.Riscv32; Target.Riscv64 ]

let max_entries =
  List.map
    (zbb_r_type_entry ~mnemonic:"max" ~variant_name:"signed-max")
    [ Target.Riscv32; Target.Riscv64 ]

let maxu_entries =
  List.map
    (zbb_r_type_entry ~mnemonic:"maxu" ~variant_name:"unsigned-max")
    [ Target.Riscv32; Target.Riscv64 ]

let andn_entries =
  List.map
    (zbb_r_type_entry ~mnemonic:"andn" ~variant_name:"and-not")
    [ Target.Riscv32; Target.Riscv64 ]

let orn_entries =
  List.map
    (zbb_r_type_entry ~mnemonic:"orn" ~variant_name:"or-not")
    [ Target.Riscv32; Target.Riscv64 ]

let xnor_entries =
  List.map
    (zbb_r_type_entry ~mnemonic:"xnor" ~variant_name:"xor-not")
    [ Target.Riscv32; Target.Riscv64 ]

let rol_entries =
  List.map
    (zbb_r_type_entry ~mnemonic:"rol" ~variant_name:"rotate-left")
    [ Target.Riscv32; Target.Riscv64 ]

let ror_entries =
  List.map
    (zbb_r_type_entry ~mnemonic:"ror" ~variant_name:"rotate-right")
    [ Target.Riscv32; Target.Riscv64 ]

(* rolw/rorw: RV64-only word-operand siblings of rol/ror (opcode 0x3b,
   rv64_zbb, no RV32 counterpart at all - confirmed: real
   riscv32-linux-gnu-as rejects both as unrecognized opcodes), the same
   single-target restriction packw/clzw/ctzw/cpopw already use. *)
let rolw_entries =
  [ zbb_r_type_entry ~mnemonic:"rolw" ~variant_name:"rotate-left-word" Target.Riscv64 ]

let rorw_entries =
  [ zbb_r_type_entry ~mnemonic:"rorw" ~variant_name:"rotate-right-word" Target.Riscv64 ]

(* clmul/clmulh (carry-less multiply): the same plain three-GPR R-type shape
   and five-way Req_any promotion discipline as andn/orn/xnor/rol/ror above,
   but rooted in Zbc rather than Zbb - riscv-opcodes' primary record is
   rv_zbc, imported by rv_zbkc/rv_zk/rv_zkn/rv_zks, identical on both
   profiles. This generator exercises only the rv_zbc configuration, the
   same "one configuration proves promotion" discipline every other slice
   here uses. *)
let zbc_configuration_for = function
  | Target.Riscv32 -> [ "-march=rv32im_zbc"; "-mabi=ilp32"; "-mno-relax" ]
  | Target.Riscv64 -> [ "-march=rv64im_zbc"; "-mabi=lp64"; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let zbc_r_type_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zbc-enabled"; "three-gpr-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("rs2", "a2") ];
    lines_before = [];
    lines_after = [];
    configuration = zbc_configuration_for target;
  }

let clmul_entries =
  List.map
    (zbc_r_type_entry ~mnemonic:"clmul" ~variant_name:"carryless-multiply")
    [ Target.Riscv32; Target.Riscv64 ]

let clmulh_entries =
  List.map
    (zbc_r_type_entry ~mnemonic:"clmulh" ~variant_name:"carryless-multiply-high")
    [ Target.Riscv32; Target.Riscv64 ]

(* xperm4/xperm8 (crossbar permute): the same plain three-GPR R-type shape
   and Req_any promotion discipline as clmul/clmulh above, but rooted in
   Zbkx (rv_zbkx primary, imported by rv_zk/rv_zkn/rv_zks - a four-way
   group, no separate non-K sibling extension the way clmul/clmulh have
   rv_zbc alongside rv_zbkc). This generator exercises only the rv_zbkx
   configuration. *)
let zbkx_configuration_for = function
  | Target.Riscv32 -> [ "-march=rv32im_zbkx"; "-mabi=ilp32"; "-mno-relax" ]
  | Target.Riscv64 -> [ "-march=rv64im_zbkx"; "-mabi=lp64"; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let zbkx_r_type_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zbkx-enabled"; "three-gpr-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("rs2", "a2") ];
    lines_before = [];
    lines_after = [];
    configuration = zbkx_configuration_for target;
  }

let xperm4_entries =
  List.map
    (zbkx_r_type_entry ~mnemonic:"xperm4" ~variant_name:"crossbar-permute-nibble")
    [ Target.Riscv32; Target.Riscv64 ]

let xperm8_entries =
  List.map
    (zbkx_r_type_entry ~mnemonic:"xperm8" ~variant_name:"crossbar-permute-byte")
    [ Target.Riscv32; Target.Riscv64 ]

(* sha256sum0/sha256sum1/sha256sig0/sha256sig1 (Zknh's SHA-256
   message-schedule helpers): the same two-GPR-operand unary shape as
   {!zbb_unary_entry}, but needing Zknh (a three-way Req_any group - primary
   rv_zknh, imported by rv_zk/rv_zkn; no rv_zks). *)
let zknh_configuration_for = function
  | Target.Riscv32 -> [ "-march=rv32im_zknh"; "-mabi=ilp32"; "-mno-relax" ]
  | Target.Riscv64 -> [ "-march=rv64im_zknh"; "-mabi=lp64"; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let zknh_unary_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zknh-enabled"; "two-gpr-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("rs1", "a1") ];
    lines_before = [];
    lines_after = [];
    configuration = zknh_configuration_for target;
  }

let sha256sum0_entries =
  List.map
    (zknh_unary_entry ~mnemonic:"sha256sum0" ~variant_name:"message-schedule-sum0")
    [ Target.Riscv32; Target.Riscv64 ]

let sha256sum1_entries =
  List.map
    (zknh_unary_entry ~mnemonic:"sha256sum1" ~variant_name:"message-schedule-sum1")
    [ Target.Riscv32; Target.Riscv64 ]

let sha256sig0_entries =
  List.map
    (zknh_unary_entry ~mnemonic:"sha256sig0" ~variant_name:"message-schedule-sig0")
    [ Target.Riscv32; Target.Riscv64 ]

let sha256sig1_entries =
  List.map
    (zknh_unary_entry ~mnemonic:"sha256sig1" ~variant_name:"message-schedule-sig1")
    [ Target.Riscv32; Target.Riscv64 ]

(* sha512sum0/sum1/sig0/sig1: sha256's RV64-only siblings - the same
   two-GPR-operand unary shape and Zknh configuration as {!zknh_unary_entry}
   above, but riscv-opcodes has no RV32 record at all for these four
   (confirmed: real riscv32-linux-gnu-as rejects all four as unrecognized
   opcodes - its own RV32 answer is a different, 32-bit-word-pair-split
   family, out of this slice's scope), the same single-target restriction
   {!clzw_entries} etc. already use. *)
let sha512sum0_entries =
  [ zknh_unary_entry ~mnemonic:"sha512sum0" ~variant_name:"message-schedule-sum0" Target.Riscv64 ]

let sha512sum1_entries =
  [ zknh_unary_entry ~mnemonic:"sha512sum1" ~variant_name:"message-schedule-sum1" Target.Riscv64 ]

let sha512sig0_entries =
  [ zknh_unary_entry ~mnemonic:"sha512sig0" ~variant_name:"message-schedule-sig0" Target.Riscv64 ]

let sha512sig1_entries =
  [ zknh_unary_entry ~mnemonic:"sha512sig1" ~variant_name:"message-schedule-sig1" Target.Riscv64 ]

(* sha512sum0r/sum1r/sig0l/sig1l/sig0h/sig1h: SHA-512's own RV32-only
   32-bit-word-pair-split helpers - a plain three-GPR R-type shape (not the
   two-GPR unary one every entry above uses), reusing {!zknh_configuration_for}
   but restricted to Riscv32 (riscv-opcodes has no RV64 record at all -
   confirmed: real riscv64-linux-gnu-as rejects all six as unrecognized
   opcodes, the mirror image of {!sha512sum0_entries}'s own RV64-only
   restriction). *)
let zknh_r_type_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zknh-enabled"; "three-gpr-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("rs2", "a2") ];
    lines_before = [];
    lines_after = [];
    configuration = zknh_configuration_for target;
  }

let sha512sum0r_entries =
  [
    zknh_r_type_entry ~mnemonic:"sha512sum0r" ~variant_name:"message-schedule-sum0r" Target.Riscv32;
  ]

let sha512sum1r_entries =
  [
    zknh_r_type_entry ~mnemonic:"sha512sum1r" ~variant_name:"message-schedule-sum1r" Target.Riscv32;
  ]

let sha512sig0l_entries =
  [
    zknh_r_type_entry ~mnemonic:"sha512sig0l" ~variant_name:"message-schedule-sig0l" Target.Riscv32;
  ]

let sha512sig1l_entries =
  [
    zknh_r_type_entry ~mnemonic:"sha512sig1l" ~variant_name:"message-schedule-sig1l" Target.Riscv32;
  ]

let sha512sig0h_entries =
  [
    zknh_r_type_entry ~mnemonic:"sha512sig0h" ~variant_name:"message-schedule-sig0h" Target.Riscv32;
  ]

let sha512sig1h_entries =
  [
    zknh_r_type_entry ~mnemonic:"sha512sig1h" ~variant_name:"message-schedule-sig1h" Target.Riscv32;
  ]

(* AES-64's plain three-GPR round functions and its two-GPR unary
   inverse-mix-columns sibling: aes64ds/aes64dsm/aes64im are rooted in
   Zknd, aes64es/aes64esm in Zkne (a disjoint primary extension, not
   Zknd-imported), and aes64ks2 is imported by both - this generator
   exercises the Zknd configuration for aes64ks2 too, the same "one
   configuration proves promotion" discipline every other slice here
   uses. All RV64-only (riscv-opcodes has no RV32 record at all). *)
let zknd_configuration_for = function
  | Target.Riscv64 -> [ "-march=rv64im_zknd"; "-mabi=lp64"; "-mno-relax" ]
  | Target.Riscv32 -> [ "-march=rv32im_zknd"; "-mabi=ilp32"; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let zkne_configuration_for = function
  | Target.Riscv64 -> [ "-march=rv64im_zkne"; "-mabi=lp64"; "-mno-relax" ]
  | Target.Riscv32 -> [ "-march=rv32im_zkne"; "-mabi=ilp32"; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let aes64_r_type_entry ~mnemonic ~variant_name ~configuration_for target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "aes64-enabled"; "three-gpr-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("rs2", "a2") ];
    lines_before = [];
    lines_after = [];
    configuration = configuration_for target;
  }

let aes64ds_entries =
  [
    aes64_r_type_entry ~mnemonic:"aes64ds" ~variant_name:"decrypt-round"
      ~configuration_for:zknd_configuration_for Target.Riscv64;
  ]

let aes64dsm_entries =
  [
    aes64_r_type_entry ~mnemonic:"aes64dsm" ~variant_name:"decrypt-round-mixed"
      ~configuration_for:zknd_configuration_for Target.Riscv64;
  ]

let aes64es_entries =
  [
    aes64_r_type_entry ~mnemonic:"aes64es" ~variant_name:"encrypt-round"
      ~configuration_for:zkne_configuration_for Target.Riscv64;
  ]

let aes64esm_entries =
  [
    aes64_r_type_entry ~mnemonic:"aes64esm" ~variant_name:"encrypt-round-mixed"
      ~configuration_for:zkne_configuration_for Target.Riscv64;
  ]

let aes64ks2_entries =
  [
    aes64_r_type_entry ~mnemonic:"aes64ks2" ~variant_name:"key-schedule-2"
      ~configuration_for:zknd_configuration_for Target.Riscv64;
  ]

let aes64im_entries =
  [
    {
      form_id = "riscv:aes64im";
      target = Target.Riscv64;
      lookup_key = "aes64im";
      case_id = "riscv:aes64im:inverse-mix-columns:riscv64";
      rule_ids = [ "aes64-enabled"; "two-gpr-operands"; "inverse-mix-columns" ];
      operands = [ ("rd", "a0"); ("rs1", "a1") ];
      lines_before = [];
      lines_after = [];
      configuration = zknd_configuration_for Target.Riscv64;
    };
  ]

(* aes64ks1i: AES-64's first key-schedule helper - the same two-GPR-plus-
   narrow-unsigned-immediate shape as {!rori_entries}, RV64 only
   (riscv-opcodes has no RV32 record at all), reusing aes64ks2's own
   four-way Req_any group (imported by both key-schedule extensions).
   [rnum]'s real valid range is 0-10 (of the 16 the 4-bit field can
   represent) - real GNU as rejects 11-15 outright - so this uses "5", a
   round number safely inside that range, not "15" the way {!zbb_shamt_entry}'s
   own bare "5" shamt happens to also be for [rori]. *)
let aes64ks1i_entries =
  [
    {
      form_id = "riscv:aes64ks1i";
      target = Target.Riscv64;
      lookup_key = "aes64ks1i";
      case_id = "riscv:aes64ks1i:key-schedule-1:riscv64";
      rule_ids = [ "aes64-enabled"; "gpr-rnum-operands"; "key-schedule-1" ];
      operands = [ ("rd", "a0"); ("rs1", "a1"); ("rnum", "5") ];
      lines_before = [];
      lines_after = [];
      configuration = zknd_configuration_for Target.Riscv64;
    };
  ]

(* aes32dsi/aes32dsmi/aes32esi/aes32esmi: AES-32's own round functions - a
   three-GPR-plus-narrow-unsigned-immediate shape (rd/rs1/rs2 plus a real
   "bs" byte-select immediate), RV32 only (riscv-opcodes has no RV64 record
   at all). aes32dsi/aes32dsmi share aes64ds/aes64dsm's own rv64_zknd-
   rooted family lineage but at RV32 (rv32_zknd, imported by
   rv32_zk/rv32_zkn); aes32esi/aes32esmi mirror aes64es/aes64esm's
   rv32_zkne-rooted one. Unlike aes64ks1i's "rnum", "bs" uses its full 2-bit
   range (0-3) - real GNU as rejects only out-of-field values (bs=4+), not
   any narrower reserved subset - so "3" (not "5") is chosen here just to
   exercise a non-zero, non-trivial value, not because other values are
   invalid. *)
let aes32_imm_entry ~mnemonic ~variant_name ~configuration_for target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "aes32-enabled"; "gpr-bs-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("rs2", "a2"); ("bs", "3") ];
    lines_before = [];
    lines_after = [];
    configuration = configuration_for target;
  }

let aes32dsi_entries =
  [
    aes32_imm_entry ~mnemonic:"aes32dsi" ~variant_name:"decrypt-round"
      ~configuration_for:zknd_configuration_for Target.Riscv32;
  ]

let aes32dsmi_entries =
  [
    aes32_imm_entry ~mnemonic:"aes32dsmi" ~variant_name:"decrypt-round-mixed"
      ~configuration_for:zknd_configuration_for Target.Riscv32;
  ]

let aes32esi_entries =
  [
    aes32_imm_entry ~mnemonic:"aes32esi" ~variant_name:"encrypt-round"
      ~configuration_for:zkne_configuration_for Target.Riscv32;
  ]

let aes32esmi_entries =
  [
    aes32_imm_entry ~mnemonic:"aes32esmi" ~variant_name:"encrypt-round-mixed"
      ~configuration_for:zkne_configuration_for Target.Riscv32;
  ]

(* csrrw/csrrs/csrrc/csrrwi/csrrsi/csrrci: Zicsr's CSR forms, the first
   family here outside Zb/Zk - XLEN-independent (both profiles accept
   identical syntax), no Req_any (a single rv_zicsr record per mnemonic).
   The register-source forms (csrrw/csrrs/csrrc) get [rd, csr, rs1]
   operands in that GAS text order (not riscv-opcodes' own [rd, rs1, csr]
   field order); the immediate-source forms (csrrwi/csrrsi/csrrci) get
   [rd, csr, zimm5], with no register operand besides [rd]. [csr] here is
   0x300 (mstatus) for every entry - a real, canonical CSR address, not an
   arbitrary placeholder - since real GNU as accepts any 0-4095 value
   whether or not it names a defined CSR, but a recognizable one keeps the
   corpus's own disassembly readable. *)
let zicsr_configuration_for = function
  | Target.Riscv32 -> [ "-march=rv32i_zicsr"; "-mabi=ilp32"; "-mno-relax" ]
  | Target.Riscv64 -> [ "-march=rv64i_zicsr"; "-mabi=lp64"; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let csr_reg_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zicsr-enabled"; "gpr-csr-gpr-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("csr", "0x300"); ("rs1", "a1") ];
    lines_before = [];
    lines_after = [];
    configuration = zicsr_configuration_for target;
  }

let csr_imm_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zicsr-enabled"; "gpr-csr-zimm-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("csr", "0x300"); ("zimm5", "5") ];
    lines_before = [];
    lines_after = [];
    configuration = zicsr_configuration_for target;
  }

let csrrw_entries =
  List.map
    (csr_reg_entry ~mnemonic:"csrrw" ~variant_name:"read-write")
    [ Target.Riscv32; Target.Riscv64 ]

let csrrs_entries =
  List.map
    (csr_reg_entry ~mnemonic:"csrrs" ~variant_name:"read-set")
    [ Target.Riscv32; Target.Riscv64 ]

let csrrc_entries =
  List.map
    (csr_reg_entry ~mnemonic:"csrrc" ~variant_name:"read-clear")
    [ Target.Riscv32; Target.Riscv64 ]

let csrrwi_entries =
  List.map
    (csr_imm_entry ~mnemonic:"csrrwi" ~variant_name:"read-write-immediate")
    [ Target.Riscv32; Target.Riscv64 ]

let csrrsi_entries =
  List.map
    (csr_imm_entry ~mnemonic:"csrrsi" ~variant_name:"read-set-immediate")
    [ Target.Riscv32; Target.Riscv64 ]

let csrrci_entries =
  List.map
    (csr_imm_entry ~mnemonic:"csrrci" ~variant_name:"read-clear-immediate")
    [ Target.Riscv32; Target.Riscv64 ]

(* csrr/csrw/csrs/csrc/csrwi/csrsi/csrci: Zicsr's 7 pseudo-op aliases for
   the six real csrr*/csrrw* forms above, each omitting whichever of
   rd/rs1 real hardware doesn't need (rd for the write/set/clear-only
   forms, rs1 for the read-only form). Same [csr] = 0x300 (mstatus)
   convention as the base forms. *)
let csr_read_entry ~variant_name target =
  {
    form_id = "riscv:csrr";
    target;
    lookup_key = "csrr";
    case_id = Printf.sprintf "riscv:csrr:%s:%s" variant_name (Target.to_string target);
    rule_ids = [ "zicsr-enabled"; "gpr-csr-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("csr", "0x300") ];
    lines_before = [];
    lines_after = [];
    configuration = zicsr_configuration_for target;
  }

let csr_write_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zicsr-enabled"; "csr-gpr-operands"; variant_name ];
    operands = [ ("csr", "0x300"); ("rs1", "a1") ];
    lines_before = [];
    lines_after = [];
    configuration = zicsr_configuration_for target;
  }

let csr_write_imm_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zicsr-enabled"; "csr-zimm-operands"; variant_name ];
    operands = [ ("csr", "0x300"); ("zimm5", "5") ];
    lines_before = [];
    lines_after = [];
    configuration = zicsr_configuration_for target;
  }

let csrr_entries = List.map (csr_read_entry ~variant_name:"read") [ Target.Riscv32; Target.Riscv64 ]

let csrw_entries =
  List.map
    (csr_write_entry ~mnemonic:"csrw" ~variant_name:"write")
    [ Target.Riscv32; Target.Riscv64 ]

let csrs_entries =
  List.map (csr_write_entry ~mnemonic:"csrs" ~variant_name:"set") [ Target.Riscv32; Target.Riscv64 ]

let csrc_entries =
  List.map
    (csr_write_entry ~mnemonic:"csrc" ~variant_name:"clear")
    [ Target.Riscv32; Target.Riscv64 ]

let csrwi_entries =
  List.map
    (csr_write_imm_entry ~mnemonic:"csrwi" ~variant_name:"write-immediate")
    [ Target.Riscv32; Target.Riscv64 ]

let csrsi_entries =
  List.map
    (csr_write_imm_entry ~mnemonic:"csrsi" ~variant_name:"set-immediate")
    [ Target.Riscv32; Target.Riscv64 ]

let csrci_entries =
  List.map
    (csr_write_imm_entry ~mnemonic:"csrci" ~variant_name:"clear-immediate")
    [ Target.Riscv32; Target.Riscv64 ]

(* amoswap.w/amoadd.w/.../sc.w/lr.w and their RV64-only .d siblings: Zaamo's
   22-record family (self-selected after Zicsr closed - the smallest still-
   unadmitted bounded family, per the same survey discipline CSR itself
   was picked with). Unlike every prior family here, the third operand is
   a real GAS memory group ["(base)"], not a plain register or immediate -
   {!amo_form}'s own [rd, rs2, base] operand triple, verified against real
   GNU as before writing any code (`amoadd.w a0, a1, (a2)` -> `00b6252f`,
   matching every funct5 assignment in the RISC-V ISA manual; a nonzero
   offset - `amoadd.w a0, a1, 4(a2)` - is rejected as "illegal operands").
   `.w` mnemonics apply on both profiles (rv_a); `.d` mnemonics are
   RV64-only (rv64_a), reusing the identical shape. `lr.w`/`lr.d` are the
   family's only two-operand member (rs2's field is fixed to 0). The
   `.aq`/`.rl`/`.aqrl` mnemonic-suffix decorators GAS also accepts on all
   22 are out of scope here (canonical bare spelling only, per the plan's
   section 5.2 policy) - see {!amo_form}'s own diagnostic. *)
let a_configuration_for = function
  | Target.Riscv32 -> [ "-march=rv32ia"; "-mabi=ilp32"; "-mno-relax" ]
  | Target.Riscv64 -> [ "-march=rv64ia"; "-mabi=lp64"; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let amo_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s" mnemonic (Target.to_string target);
    rule_ids = [ "a-enabled"; "gpr-gpr-mem-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("rs2", "a1"); ("base", "a2") ];
    lines_before = [];
    lines_after = [];
    configuration = a_configuration_for target;
  }

let lr_entry ~mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s" mnemonic (Target.to_string target);
    rule_ids = [ "a-enabled"; "gpr-mem-operands"; "load-reserved" ];
    operands = [ ("rd", "a0"); ("base", "a2") ];
    lines_before = [];
    lines_after = [];
    configuration = a_configuration_for target;
  }

let amoswap_w_entries =
  List.map (amo_entry ~mnemonic:"amoswap.w" ~variant_name:"swap") [ Target.Riscv32; Target.Riscv64 ]

let amoadd_w_entries =
  List.map (amo_entry ~mnemonic:"amoadd.w" ~variant_name:"add") [ Target.Riscv32; Target.Riscv64 ]

let amoxor_w_entries =
  List.map (amo_entry ~mnemonic:"amoxor.w" ~variant_name:"xor") [ Target.Riscv32; Target.Riscv64 ]

let amoand_w_entries =
  List.map (amo_entry ~mnemonic:"amoand.w" ~variant_name:"and") [ Target.Riscv32; Target.Riscv64 ]

let amoor_w_entries =
  List.map (amo_entry ~mnemonic:"amoor.w" ~variant_name:"or") [ Target.Riscv32; Target.Riscv64 ]

let amomin_w_entries =
  List.map (amo_entry ~mnemonic:"amomin.w" ~variant_name:"min") [ Target.Riscv32; Target.Riscv64 ]

let amomax_w_entries =
  List.map (amo_entry ~mnemonic:"amomax.w" ~variant_name:"max") [ Target.Riscv32; Target.Riscv64 ]

let amominu_w_entries =
  List.map (amo_entry ~mnemonic:"amominu.w" ~variant_name:"minu") [ Target.Riscv32; Target.Riscv64 ]

let amomaxu_w_entries =
  List.map (amo_entry ~mnemonic:"amomaxu.w" ~variant_name:"maxu") [ Target.Riscv32; Target.Riscv64 ]

let sc_w_entries =
  List.map
    (amo_entry ~mnemonic:"sc.w" ~variant_name:"conditional-store")
    [ Target.Riscv32; Target.Riscv64 ]

let lr_w_entries = List.map (lr_entry ~mnemonic:"lr.w") [ Target.Riscv32; Target.Riscv64 ]

let amoswap_d_entries =
  List.map (amo_entry ~mnemonic:"amoswap.d" ~variant_name:"swap") [ Target.Riscv64 ]

let amoadd_d_entries =
  List.map (amo_entry ~mnemonic:"amoadd.d" ~variant_name:"add") [ Target.Riscv64 ]

let amoxor_d_entries =
  List.map (amo_entry ~mnemonic:"amoxor.d" ~variant_name:"xor") [ Target.Riscv64 ]

let amoand_d_entries =
  List.map (amo_entry ~mnemonic:"amoand.d" ~variant_name:"and") [ Target.Riscv64 ]

let amoor_d_entries = List.map (amo_entry ~mnemonic:"amoor.d" ~variant_name:"or") [ Target.Riscv64 ]

let amomin_d_entries =
  List.map (amo_entry ~mnemonic:"amomin.d" ~variant_name:"min") [ Target.Riscv64 ]

let amomax_d_entries =
  List.map (amo_entry ~mnemonic:"amomax.d" ~variant_name:"max") [ Target.Riscv64 ]

let amominu_d_entries =
  List.map (amo_entry ~mnemonic:"amominu.d" ~variant_name:"minu") [ Target.Riscv64 ]

let amomaxu_d_entries =
  List.map (amo_entry ~mnemonic:"amomaxu.d" ~variant_name:"maxu") [ Target.Riscv64 ]

let sc_d_entries =
  List.map (amo_entry ~mnemonic:"sc.d" ~variant_name:"conditional-store") [ Target.Riscv64 ]

let lr_d_entries = List.map (lr_entry ~mnemonic:"lr.d") [ Target.Riscv64 ]

(* rori/roriw: Zbb's rotate-*immediate* forms - two ordinary GPR operands
   plus a genuine shift-amount immediate (rd, rs1, shamt), unlike every
   R-type entry above. rori is the same profile-specific native_name split
   as rev8/rev8.rv32 ({!rev8_entry}): riscv64.jsonl's own native_name is
   already "rori" (6-bit shamtd), riscv32.jsonl's is the pseudo-op alias
   "rori.rv32" (5-bit shamtw), both rendering as the bare ["rori"] spelling
   real GNU as accepts on either profile (confirmed: `ror a0, a1, 5`
   assembles as "rori" on both RV32 2.43.1 and RV64 2.44). roriw is the
   plain RV64-only *w sibling (opcode 0x1b, no RV32 counterpart at all -
   confirmed: real riscv32-linux-gnu-as rejects it as an unrecognized
   opcode), needing no such split. *)
let zbb_shamt_entry ~mnemonic ~lookup_key ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zbb-enabled"; "gpr-shamt-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("shamt", "5") ];
    lines_before = [];
    lines_after = [];
    configuration = zbb_configuration_for target;
  }

let rori_entries =
  [
    zbb_shamt_entry ~mnemonic:"rori" ~lookup_key:"rori.rv32" ~variant_name:"rotate-right-immediate"
      Target.Riscv32;
    zbb_shamt_entry ~mnemonic:"rori" ~lookup_key:"rori" ~variant_name:"rotate-right-immediate"
      Target.Riscv64;
  ]

let roriw_entries =
  [
    zbb_shamt_entry ~mnemonic:"roriw" ~lookup_key:"roriw"
      ~variant_name:"rotate-right-immediate-word" Target.Riscv64;
  ]

(* Zbb's population-count/sign-extend/byte family: two plain GPR operands
   (rd, rs1), no immediate written - unlike every zbb_r_type_entry-shaped
   case above. clz/ctz/cpop/sext.b/sext.h/orc.b are XLEN-independent
   (exactly one rv_zbb record on each profile); clzw/ctzw/cpopw are their
   RV64-only word-operand siblings (rv64_zbb, no RV32 counterpart at all -
   the same single-target shape addw's own pilot entry established).
   rev8/brev8 remain out of this slice: riscv-opcodes exports rev8 once per
   importing extension (the same Req_any shape andn/orn/xnor/rol/ror needed)
   AND under an XLEN-dependent mnemonic spelling (rev8 vs rev8.rv32). *)
let zbb_unary_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zbb-enabled"; "two-gpr-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("rs1", "a1") ];
    lines_before = [];
    lines_after = [];
    configuration = zbb_configuration_for target;
  }

let clz_entries =
  List.map
    (zbb_unary_entry ~mnemonic:"clz" ~variant_name:"count-leading-zero")
    [ Target.Riscv32; Target.Riscv64 ]

let ctz_entries =
  List.map
    (zbb_unary_entry ~mnemonic:"ctz" ~variant_name:"count-trailing-zero")
    [ Target.Riscv32; Target.Riscv64 ]

let cpop_entries =
  List.map
    (zbb_unary_entry ~mnemonic:"cpop" ~variant_name:"population-count")
    [ Target.Riscv32; Target.Riscv64 ]

let sextb_entries =
  List.map
    (zbb_unary_entry ~mnemonic:"sext.b" ~variant_name:"sign-extend-byte")
    [ Target.Riscv32; Target.Riscv64 ]

let sexth_entries =
  List.map
    (zbb_unary_entry ~mnemonic:"sext.h" ~variant_name:"sign-extend-halfword")
    [ Target.Riscv32; Target.Riscv64 ]

let orcb_entries =
  List.map
    (zbb_unary_entry ~mnemonic:"orc.b" ~variant_name:"byte-wise-or-combine")
    [ Target.Riscv32; Target.Riscv64 ]

let clzw_entries =
  [ zbb_unary_entry ~mnemonic:"clzw" ~variant_name:"count-leading-zero-word" Target.Riscv64 ]

let ctzw_entries =
  [ zbb_unary_entry ~mnemonic:"ctzw" ~variant_name:"count-trailing-zero-word" Target.Riscv64 ]

let cpopw_entries =
  [ zbb_unary_entry ~mnemonic:"cpopw" ~variant_name:"population-count-word" Target.Riscv64 ]

(* brev8 (within-byte bit-reverse): the same two-GPR-operand unary shape as
   {!zbb_unary_entry}, but requiring Zbkb (its own primary riscv-opcodes
   extension) rather than Zbb, and exercising only the rv_zbkb configuration
   - the same "one configuration proves promotion" discipline the Req_any
   andn/orn/xnor/rol/ror slice used for their own primary rv_zbb record. *)
let zbkb_configuration_for = function
  | Target.Riscv32 -> [ "-march=rv32i_zbkb"; "-mabi=ilp32"; "-mno-relax" ]
  | Target.Riscv64 -> [ "-march=rv64i_zbkb"; "-mabi=lp64"; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let brev8_entry target =
  {
    form_id = "riscv:brev8";
    target;
    lookup_key = "brev8";
    case_id = Printf.sprintf "riscv:brev8:bit-reverse-byte:%s" (Target.to_string target);
    rule_ids = [ "zbkb-enabled"; "two-gpr-operands"; "bit-reverse-byte" ];
    operands = [ ("rd", "a0"); ("rs1", "a1") ];
    lines_before = [];
    lines_after = [];
    configuration = zbkb_configuration_for target;
  }

let brev8_entries = List.map brev8_entry [ Target.Riscv32; Target.Riscv64 ]

(* rev8 (byte-reverse): the same two-GPR-operand shape and rv_zbkb-family
   configuration as brev8, but its riscv-opcodes native_name genuinely
   differs by profile - "rev8" on RV64, "rev8.rv32" on RV32 - so unlike
   every other entry in this family, [lookup_key] cannot equal [form_id]'s
   own mnemonic tail on both profiles. Real GNU as accepts the bare ["rev8"]
   spelling on both profiles (confirmed; it rejects "rev8.rv32"
   outright), matching {!Isa_norm_riscv}'s own rendered mnemonic - so the
   RENDERED source is identical on both profiles even though [lookup_key]
   is not. *)
let rev8_entry ~lookup_key target =
  {
    form_id = "riscv:rev8";
    target;
    lookup_key;
    case_id = Printf.sprintf "riscv:rev8:byte-reverse:%s" (Target.to_string target);
    rule_ids = [ "zbkb-enabled"; "two-gpr-operands"; "byte-reverse" ];
    operands = [ ("rd", "a0"); ("rs1", "a1") ];
    lines_before = [];
    lines_after = [];
    configuration = zbkb_configuration_for target;
  }

let rev8_entries =
  [
    rev8_entry ~lookup_key:"rev8.rv32" Target.Riscv32; rev8_entry ~lookup_key:"rev8" Target.Riscv64;
  ]

(* pack/packh: Zbkb's plain three-GPR-operand R-type pair (opcode 0x33,
   funct7 0x04, funct3 selects pack vs packh), the same shape as
   {!zbb_r_type_entry} but requiring Zbkb; both are XLEN-independent
   (exactly one rv_zbkb record on each profile, four-way Req_any). packw is
   their RV64-only word-operand sibling (opcode 0x3b, rv64_zbkb, no RV32
   counterpart at all). *)
let zbkb_r_type_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zbkb-enabled"; "three-gpr-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("rs2", "a2") ];
    lines_before = [];
    lines_after = [];
    configuration = zbkb_configuration_for target;
  }

let pack_entries =
  List.map
    (zbkb_r_type_entry ~mnemonic:"pack" ~variant_name:"pack-low")
    [ Target.Riscv32; Target.Riscv64 ]

let packh_entries =
  List.map
    (zbkb_r_type_entry ~mnemonic:"packh" ~variant_name:"pack-byte")
    [ Target.Riscv32; Target.Riscv64 ]

let packw_entries = [ zbkb_r_type_entry ~mnemonic:"packw" ~variant_name:"pack-word" Target.Riscv64 ]

(* zip/unzip: Zbkb's RV32-only bit interleave/de-interleave, the same
   two-GPR-operand unary shape as {!brev8_entry} - riscv-opcodes has no RV64
   counterpart at all (confirmed: real riscv64-linux-gnu-as rejects "zip" as
   an unrecognized opcode), the mirror image of clzw/ctzw/cpopw's RV64-only
   restriction. *)
let zbkb_unary_entry ~mnemonic ~variant_name target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:%s:%s" mnemonic variant_name (Target.to_string target);
    rule_ids = [ "zbkb-enabled"; "two-gpr-operands"; variant_name ];
    operands = [ ("rd", "a0"); ("rs1", "a1") ];
    lines_before = [];
    lines_after = [];
    configuration = zbkb_configuration_for target;
  }

let zip_entries = [ zbkb_unary_entry ~mnemonic:"zip" ~variant_name:"bit-interleave" Target.Riscv32 ]

let unzip_entries =
  [ zbkb_unary_entry ~mnemonic:"unzip" ~variant_name:"bit-deinterleave" Target.Riscv32 ]

(* zext.h: the same two-GPR-operand unary shape as {!zbb_unary_entry}, but
   needing only Zbb (not Zbkb), and carrying the same profile-specific
   native_name split as {!rev8_entry} - riscv-opcodes' own RV32 spelling is
   "zext.h.rv32", specializing [pack], while RV64's is the bare "zext.h",
   specializing [packw]; real GNU as accepts only the bare ["zext.h"] text on
   either profile (confirmed: it rejects "zext.h.rv32" outright), matching
   {!Isa_norm_riscv}'s own rendered mnemonic - so [form_id] is identical on
   both profiles even though [lookup_key] is not. *)
let zext_h_entry ~lookup_key target =
  {
    form_id = "riscv:zext.h";
    target;
    lookup_key;
    case_id = Printf.sprintf "riscv:zext.h:zero-extend-halfword:%s" (Target.to_string target);
    rule_ids = [ "zbb-enabled"; "two-gpr-operands"; "zero-extend-halfword" ];
    operands = [ ("rd", "a0"); ("rs1", "a1") ];
    lines_before = [];
    lines_after = [];
    configuration = zbb_configuration_for target;
  }

let zext_h_entries =
  [
    zext_h_entry ~lookup_key:"zext.h.rv32" Target.Riscv32;
    zext_h_entry ~lookup_key:"zext.h" Target.Riscv64;
  ]

(* vsetvl (V's register-register vector configuration-setting instruction):
   the same plain three-GPR R-type shape as {!zbc_r_type_entry} above, but
   [rd, rs1, rs2] rather than the operation's usual roles - [rs1] the
   requested AVL, [rs2] the desired vtype, [rd] the resulting vl - riscv-
   opcodes exports a single rv_v record on both profiles with no import
   duplication. `-march=rv32iv`/`rv64iv` alone (no F/D) suffices for real
   GNU as to accept it: confirmed `vsetvl a0, a1, a2` -> `80c5f557` on both
   riscv32-linux-gnu-as 2.43.1 and riscv64-linux-gnu-as 2.44. This is the
   cheapest possible entry point into the 375-record rv_v family: no vector
   register class, no vtype-immediate text syntax (vsetvli/vsetivli's own
   "e<SEW>,m<LMUL>,ta|tu,ma|mu" operand list is a separate, larger
   normalization problem, left as a named follow-up). *)
let v_configuration_for = function
  | Target.Riscv32 -> [ "-march=rv32iv"; "-mabi=ilp32"; "-mno-relax" ]
  | Target.Riscv64 -> [ "-march=rv64iv"; "-mabi=lp64"; "-mno-relax" ]
  | (Target.X86_32 | Target.X86_64 | Target.Arm | Target.Aarch64) as t ->
      Isa_gen_case_build.configuration_for t

let vsetvl_entry target =
  {
    form_id = "riscv:vsetvl";
    target;
    lookup_key = "vsetvl";
    case_id = Printf.sprintf "riscv:vsetvl:register-register:%s" (Target.to_string target);
    rule_ids = [ "v-enabled"; "three-gpr-operands" ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("rs2", "a2") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let vsetvl_entries = List.map vsetvl_entry [ Target.Riscv32; Target.Riscv64 ]

(* vsetvli/vsetivli: V's own immediate-vtype siblings of {!vsetvl_entry}.
   {!Isa_norm_riscv.vsetvli_form}/{!vsetivli_form} model the whole
   "e<SEW>,m<LMUL>,ta|tu,ma|mu" spelling as one [vtype] operand, so its
   entry value here is the full literal spelling with its own embedded
   commas - {!Isa_gen_render.render_line} joins operand values with ", "
   regardless of whether a value itself contains one, so this renders
   exactly the canonical six-operand spelling. Confirmed against real GNU
   as: `vsetvli a0, a1, e32, m1, ta, ma` -> `0d05f557`, `vsetivli a0, 5,
   e32, m1, ta, ma` -> `cd02f557`, identical on both profiles (V is
   XLEN-independent). *)
let vsetvli_entry target =
  {
    form_id = "riscv:vsetvli";
    target;
    lookup_key = "vsetvli";
    case_id = Printf.sprintf "riscv:vsetvli:full-spelling:%s" (Target.to_string target);
    rule_ids = [ "v-enabled"; "vtype-keyword-list" ];
    operands = [ ("rd", "a0"); ("rs1", "a1"); ("vtype", "e32, m1, ta, ma") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let vsetvli_entries = List.map vsetvli_entry [ Target.Riscv32; Target.Riscv64 ]

let vsetivli_entry target =
  {
    form_id = "riscv:vsetivli";
    target;
    lookup_key = "vsetivli";
    case_id = Printf.sprintf "riscv:vsetivli:full-spelling:%s" (Target.to_string target);
    rule_ids = [ "v-enabled"; "vtype-keyword-list" ];
    operands = [ ("rd", "a0"); ("uimm", "5"); ("vtype", "e32, m1, ta, ma") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let vsetivli_entries = List.map vsetivli_entry [ Target.Riscv32; Target.Riscv64 ]

(* OP-V's OPIVV/OPIVX/OPIVI shapes, generalized across every admitted
   mnemonic (see {!Isa_norm_riscv.opivv_form}/[opivx_form]/[opivi_form] for
   the matching normalization-side generalization): the entry point into
   OP-V's real vector-register arithmetic space, as opposed to the
   configuration-setting group above. Confirmed against real GNU as,
   identical on both profiles (V is XLEN-independent): `vadd.vv v1, v2, v3`
   -> `022180d7`; `vsub.vv v1, v2, v3` -> `0a2180d7`; `vadd.vx v1, v2, a0`
   -> `022540d7`; `vsub.vx v1, v2, a0` -> `0a2540d7`; `vrsub.vx v1, v2, a0`
   -> `0e2540d7`; `vadd.vi v1, v2, -5` -> `022db0d7`; `vrsub.vi v1, v2, -5`
   -> `0e2db0d7`; `vand.vv/.vx/.vi`, `vor.vv/.vx/.vi`, `vxor.vv/.vx/.vi` ->
   `262180d7`/`262540d7`/`262db0d7`, `2a2180d7`/`2a2540d7`/`2a2db0d7`,
   `2e2180d7`/`2e2540d7`/`2e2db0d7`. *)
let opivv_entry ~mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:vector-vector:%s" mnemonic (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", "v1"); ("rs2", "v2"); ("rs1", "v3") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let opivv_entries ~mnemonic = List.map (opivv_entry ~mnemonic) [ Target.Riscv32; Target.Riscv64 ]

let opivx_entry ~mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:vector-scalar:%s" mnemonic (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", "v1"); ("rs2", "v2"); ("rs1", "a0") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let opivx_entries ~mnemonic = List.map (opivx_entry ~mnemonic) [ Target.Riscv32; Target.Riscv64 ]

(* [imm_name]/[imm_value] default to the SIGNED [simm5] shape every OPIVI
   mnemonic but the shift trio uses; {!vsll_vi_entries}/etc. below pass the
   UNSIGNED [zimm5]/["31"] pair instead, matching {!Isa_norm_riscv.opivi_form}'s
   own field-name/signedness split. *)
let opivi_entry ?(imm_name = "simm5") ?(imm_value = "-5") ~mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:vector-immediate:%s" mnemonic (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", "v1"); ("rs2", "v2"); (imm_name, imm_value) ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let opivi_entries ?imm_name ?imm_value ~mnemonic () =
  List.map (opivi_entry ?imm_name ?imm_value ~mnemonic) [ Target.Riscv32; Target.Riscv64 ]

let vadd_vv_entries = opivv_entries ~mnemonic:"vadd.vv"
let vadd_vx_entries = opivx_entries ~mnemonic:"vadd.vx"
let vadd_vi_entries = opivi_entries ~mnemonic:"vadd.vi" ()
let vsub_vv_entries = opivv_entries ~mnemonic:"vsub.vv"
let vsub_vx_entries = opivx_entries ~mnemonic:"vsub.vx"
let vrsub_vx_entries = opivx_entries ~mnemonic:"vrsub.vx"
let vrsub_vi_entries = opivi_entries ~mnemonic:"vrsub.vi" ()
let vand_vv_entries = opivv_entries ~mnemonic:"vand.vv"
let vand_vx_entries = opivx_entries ~mnemonic:"vand.vx"
let vand_vi_entries = opivi_entries ~mnemonic:"vand.vi" ()
let vor_vv_entries = opivv_entries ~mnemonic:"vor.vv"
let vor_vx_entries = opivx_entries ~mnemonic:"vor.vx"
let vor_vi_entries = opivi_entries ~mnemonic:"vor.vi" ()
let vxor_vv_entries = opivv_entries ~mnemonic:"vxor.vv"
let vxor_vx_entries = opivx_entries ~mnemonic:"vxor.vx"
let vxor_vi_entries = opivi_entries ~mnemonic:"vxor.vi" ()

(* [vsll]/[vsrl]/[vsra]: OP-V's shift family, the first extension of the
   OPIVV/OPIVX/OPIVI tables to reuse EVERY shape unchanged including the
   full [.vi] triple - only [vsll_vi_entries]/etc.'s immediate is the
   UNSIGNED [zimm5] (0..31) {!opivi_entry}'s optional arguments exist for.
   Confirmed against real GNU as (`riscv64-linux-gnu-as` 2.44,
   `-march=rv64gv`, byte-identical on RV32): `vsll.vv v1,v2,v3` ->
   `962180d7`; `vsll.vx v1,v2,a0` -> `962540d7`; `vsll.vi v1,v2,31` ->
   `962fb0d7`; `vsrl.vv/.vx/.vi 5` -> `a22180d7`/`a22540d7`/`a222b0d7`;
   `vsra.vv/.vx/.vi 5` -> `a62180d7`/`a62540d7`/`a622b0d7`; `vsll.vi
   v1,v2,32`/`,-1` both "bad value for vector immediate field, value must
   be 0...31". *)
let vsll_vv_entries = opivv_entries ~mnemonic:"vsll.vv"
let vsll_vx_entries = opivx_entries ~mnemonic:"vsll.vx"
let vsll_vi_entries = opivi_entries ~imm_name:"zimm5" ~imm_value:"31" ~mnemonic:"vsll.vi" ()
let vsrl_vv_entries = opivv_entries ~mnemonic:"vsrl.vv"
let vsrl_vx_entries = opivx_entries ~mnemonic:"vsrl.vx"
let vsrl_vi_entries = opivi_entries ~imm_name:"zimm5" ~imm_value:"5" ~mnemonic:"vsrl.vi" ()
let vsra_vv_entries = opivv_entries ~mnemonic:"vsra.vv"
let vsra_vx_entries = opivx_entries ~mnemonic:"vsra.vx"
let vsra_vi_entries = opivi_entries ~imm_name:"zimm5" ~imm_value:"5" ~mnemonic:"vsra.vi" ()

(* [vminu]/[vmin]/[vmaxu]/[vmax]: OP-V's min/max family - [.vv]/[.vx] only,
   no [.vi] sibling (riscv-opcodes exports none; real GNU as rejects
   [vminu.vi] as "unrecognized opcode"). Confirmed against real GNU as:
   `vminu.vv/.vx` -> `122180d7`/`122540d7`; `vmin.vv/.vx` ->
   `162180d7`/`162540d7`; `vmaxu.vv/.vx` -> `1a2180d7`/`1a2540d7`;
   `vmax.vv/.vx` -> `1e2180d7`/`1e2540d7`. *)
let vminu_vv_entries = opivv_entries ~mnemonic:"vminu.vv"
let vminu_vx_entries = opivx_entries ~mnemonic:"vminu.vx"
let vmin_vv_entries = opivv_entries ~mnemonic:"vmin.vv"
let vmin_vx_entries = opivx_entries ~mnemonic:"vmin.vx"
let vmaxu_vv_entries = opivv_entries ~mnemonic:"vmaxu.vv"
let vmaxu_vx_entries = opivx_entries ~mnemonic:"vmaxu.vx"
let vmax_vv_entries = opivv_entries ~mnemonic:"vmax.vv"
let vmax_vx_entries = opivx_entries ~mnemonic:"vmax.vx"

(* [vmul]/[vmulh]/[vmulhu]/[vmulhsu]: OP-V's second major functional-unit
   group, OPMVV (funct3 = 2)/OPMVX (funct3 = 6) - the identical
   all-vector-register/scalar-broadcast operand layout as OPIVV/OPIVX
   (real GNU as's operand-order/mask/rejection behavior is indistinguishable
   from that family), so {!opivv_entries}/{!opivx_entries} are reused
   unchanged; no [.vi] sibling exists for any of the four. Confirmed
   against real GNU as: `vmul.vv/.vx` -> `9621a0d7`/`962560d7`;
   `vmulh.vv/.vx` -> `9e21a0d7`/`9e2560d7`; `vmulhu.vv/.vx` ->
   `9221a0d7`/`922560d7`; `vmulhsu.vv/.vx` -> `9a21a0d7`/`9a2560d7`. *)
let vmul_vv_entries = opivv_entries ~mnemonic:"vmul.vv"
let vmul_vx_entries = opivx_entries ~mnemonic:"vmul.vx"
let vmulh_vv_entries = opivv_entries ~mnemonic:"vmulh.vv"
let vmulh_vx_entries = opivx_entries ~mnemonic:"vmulh.vx"
let vmulhu_vv_entries = opivv_entries ~mnemonic:"vmulhu.vv"
let vmulhu_vx_entries = opivx_entries ~mnemonic:"vmulhu.vx"
let vmulhsu_vv_entries = opivv_entries ~mnemonic:"vmulhsu.vv"
let vmulhsu_vx_entries = opivx_entries ~mnemonic:"vmulhsu.vx"

(* [vdivu]/[vdiv]/[vremu]/[vrem]: OP-V's divide/remainder family, the same
   OPMVV/OPMVX shape as [vmul]/etc. above with no [.vi] sibling (real GNU as
   rejects [vdiv.vi] as "unrecognized opcode"). Confirmed against real GNU
   as, byte-identical on RV32/RV64: `vdivu.vv/.vx` -> `8221a0d7`/`822560d7`,
   `vdiv.vv/.vx` -> `8621a0d7`/`862560d7`, `vremu.vv/.vx` ->
   `8a21a0d7`/`8a2560d7`, `vrem.vv/.vx` -> `8e21a0d7`/`8e2560d7`. *)
let vdivu_vv_entries = opivv_entries ~mnemonic:"vdivu.vv"
let vdivu_vx_entries = opivx_entries ~mnemonic:"vdivu.vx"
let vdiv_vv_entries = opivv_entries ~mnemonic:"vdiv.vv"
let vdiv_vx_entries = opivx_entries ~mnemonic:"vdiv.vx"
let vremu_vv_entries = opivv_entries ~mnemonic:"vremu.vv"
let vremu_vx_entries = opivx_entries ~mnemonic:"vremu.vx"
let vrem_vv_entries = opivv_entries ~mnemonic:"vrem.vv"
let vrem_vx_entries = opivx_entries ~mnemonic:"vrem.vx"

(* [vsaddu]/[vsadd]/[vssubu]/[vssub]: OP-V's saturating add/subtract
   family, the same OPIVV/OPIVX/OPIVI shape as [vadd]/etc. - the assembler
   only encodes the instruction, saturation is execution-time behavior
   invisible here. [vsadd]/[vsaddu]'s [.vi] immediate is SIGNED [simm5],
   like every [.vi] mnemonic but the shift trio; [vssub]/[vssubu] have no
   [.vi] sibling (real GNU as rejects [vssub.vi] as "unrecognized
   opcode"). Confirmed against real GNU as, byte-identical on RV32/RV64:
   `vsaddu.vv/.vx/.vi` -> `822180d7`/`822540d7`/`822db0d7`, `vsadd.vv/.vx/
   .vi` -> `862180d7`/`862540d7`/`862db0d7`, `vssubu.vv/.vx` ->
   `8a2180d7`/`8a2540d7`, `vssub.vv/.vx` -> `8e2180d7`/`8e2540d7`. *)
let vsaddu_vv_entries = opivv_entries ~mnemonic:"vsaddu.vv"
let vsaddu_vx_entries = opivx_entries ~mnemonic:"vsaddu.vx"
let vsaddu_vi_entries = opivi_entries ~mnemonic:"vsaddu.vi" ()
let vsadd_vv_entries = opivv_entries ~mnemonic:"vsadd.vv"
let vsadd_vx_entries = opivx_entries ~mnemonic:"vsadd.vx"
let vsadd_vi_entries = opivi_entries ~mnemonic:"vsadd.vi" ()
let vssubu_vv_entries = opivv_entries ~mnemonic:"vssubu.vv"
let vssubu_vx_entries = opivx_entries ~mnemonic:"vssubu.vx"
let vssub_vv_entries = opivv_entries ~mnemonic:"vssub.vv"
let vssub_vx_entries = opivx_entries ~mnemonic:"vssub.vx"

(* [vaadd]/[vaaddu]/[vasub]/[vasubu]: OP-V's averaging add/subtract
   family, the same OPMVV/OPMVX shape as [vmul]/[vdivu]/etc. above with no
   [.vi] sibling (real GNU as rejects [vaadd.vi] as "unrecognized
   opcode"). Confirmed against real GNU as, byte-identical on RV32/RV64:
   `vaaddu.vv/.vx` -> `2221a0d7`/`222560d7`, `vaadd.vv/.vx` ->
   `2621a0d7`/`262560d7`, `vasubu.vv/.vx` -> `2a21a0d7`/`2a2560d7`,
   `vasub.vv/.vx` -> `2e21a0d7`/`2e2560d7`. *)
let vaaddu_vv_entries = opivv_entries ~mnemonic:"vaaddu.vv"
let vaaddu_vx_entries = opivx_entries ~mnemonic:"vaaddu.vx"
let vaadd_vv_entries = opivv_entries ~mnemonic:"vaadd.vv"
let vaadd_vx_entries = opivx_entries ~mnemonic:"vaadd.vx"
let vasubu_vv_entries = opivv_entries ~mnemonic:"vasubu.vv"
let vasubu_vx_entries = opivx_entries ~mnemonic:"vasubu.vx"
let vasub_vv_entries = opivv_entries ~mnemonic:"vasub.vv"
let vasub_vx_entries = opivx_entries ~mnemonic:"vasub.vx"

(* [vnsrl]/[vnsra]/[vnclipu]/[vnclip]: OP-V's narrowing shift/clip family -
   the [.wv]/[.wx]/[.wi] suffix (vs2 is a "wide", 2xSEW operand
   semantically) rather than [.vv]/[.vx]/[.vi], but the assembler only
   encodes register/immediate field positions, which are identical to the
   OPIVV/OPIVX/OPIVI shape [vadd]/etc. already use - so {!opivv_entries}/
   {!opivx_entries}/{!opivi_entries} are reused unchanged. The [.wi]
   immediate is UNSIGNED [zimm5] (0..31), like the shift trio. Confirmed
   against real GNU as, byte-identical on RV32/RV64: `vnsrl.wv/.wx/.wi 31`
   -> `b22180d7`/`b22540d7`/`b22fb0d7`, `vnsra.wv/.wx/.wi 31` ->
   `b62180d7`/`b62540d7`/`b62fb0d7`, `vnclipu.wv/.wx/.wi 31` ->
   `ba2180d7`/`ba2540d7`/`ba2fb0d7`, `vnclip.wv/.wx/.wi 31` ->
   `be2180d7`/`be2540d7`/`be2fb0d7`; `vnclip.wi v1,v2,32` rejected with the
   identical "value must be 0...31" message the shift trio's own [.vi]
   siblings use. *)
let vnsrl_wv_entries = opivv_entries ~mnemonic:"vnsrl.wv"
let vnsrl_wx_entries = opivx_entries ~mnemonic:"vnsrl.wx"
let vnsrl_wi_entries = opivi_entries ~imm_name:"zimm5" ~imm_value:"31" ~mnemonic:"vnsrl.wi" ()
let vnsra_wv_entries = opivv_entries ~mnemonic:"vnsra.wv"
let vnsra_wx_entries = opivx_entries ~mnemonic:"vnsra.wx"
let vnsra_wi_entries = opivi_entries ~imm_name:"zimm5" ~imm_value:"31" ~mnemonic:"vnsra.wi" ()
let vnclipu_wv_entries = opivv_entries ~mnemonic:"vnclipu.wv"
let vnclipu_wx_entries = opivx_entries ~mnemonic:"vnclipu.wx"
let vnclipu_wi_entries = opivi_entries ~imm_name:"zimm5" ~imm_value:"31" ~mnemonic:"vnclipu.wi" ()
let vnclip_wv_entries = opivv_entries ~mnemonic:"vnclip.wv"
let vnclip_wx_entries = opivx_entries ~mnemonic:"vnclip.wx"
let vnclip_wi_entries = opivi_entries ~imm_name:"zimm5" ~imm_value:"31" ~mnemonic:"vnclip.wi" ()

(* [vssrl]/[vssra]: OP-V's scaling shift-right family (logical/arithmetic,
   rounding), the same full OPIVV/OPIVX/OPIVI shape as [vsll]/[vsrl]/
   [vsra] above including the UNSIGNED [zimm5] [.vi] immediate. Confirmed
   against real GNU as, byte-identical on RV32/RV64: `vssrl.vv/.vx/.vi 31`
   -> `aa2180d7`/`aa2540d7`/`aa2fb0d7`, `vssra.vv/.vx/.vi 31` ->
   `ae2180d7`/`ae2540d7`/`ae2fb0d7`. *)
let vssrl_vv_entries = opivv_entries ~mnemonic:"vssrl.vv"
let vssrl_vx_entries = opivx_entries ~mnemonic:"vssrl.vx"
let vssrl_vi_entries = opivi_entries ~imm_name:"zimm5" ~imm_value:"31" ~mnemonic:"vssrl.vi" ()
let vssra_vv_entries = opivv_entries ~mnemonic:"vssra.vv"
let vssra_vx_entries = opivx_entries ~mnemonic:"vssra.vx"
let vssra_vi_entries = opivi_entries ~imm_name:"zimm5" ~imm_value:"31" ~mnemonic:"vssra.vi" ()

(* [vrgather]: OP-V's full-vector-register gather/permute family, the same
   OPIVV/OPIVX/OPIVI shape as [vadd]/etc. (funct6 0x0c) plus
   [vrgatherei16.vv] (a fixed-EEW16-index sibling, funct6 0x0e, [.vv] only
   - no [.vx]/[.vi] siblings exist for it). The [.vi] index immediate is
   UNSIGNED [zimm5], like the shift trio. Confirmed against real GNU as,
   byte-identical on RV32/RV64: `vrgather.vv/.vx/.vi 31` ->
   `322180d7`/`322540d7`/`322fb0d7`, `vrgatherei16.vv` -> `3a2180d7`. *)
let vrgather_vv_entries = opivv_entries ~mnemonic:"vrgather.vv"
let vrgather_vx_entries = opivx_entries ~mnemonic:"vrgather.vx"
let vrgather_vi_entries = opivi_entries ~imm_name:"zimm5" ~imm_value:"31" ~mnemonic:"vrgather.vi" ()
let vrgatherei16_vv_entries = opivv_entries ~mnemonic:"vrgatherei16.vv"

(* [vwaddu]/[vwadd]/[vwsubu]/[vwsub]: OP-V's widening add/subtract family,
   each with a `.vv`/`.vx` (both narrow operands) and `.wv`/`.wx` (`vs2`
   wide, `vs1`/`rs1` narrow) sibling pair - the same OPMVV/OPMVX shape as
   [vmul]/[vdivu]/etc. above with no [.vi] sibling; operand *width* is an
   execution-time SEW/vtype concern the assembler does not encode.
   Confirmed against real GNU as, byte-identical on RV32/RV64:
   `vwaddu.vv/.vx` -> `c221a0d7`/`c22560d7`, `vwadd.vv/.vx` ->
   `c621a0d7`/`c62560d7`, `vwsubu.vv/.vx` -> `ca21a0d7`/`ca2560d7`,
   `vwsub.vv/.vx` -> `ce21a0d7`/`ce2560d7`, `vwaddu.wv/.wx` ->
   `d221a0d7`/`d22560d7`, `vwadd.wv/.wx` -> `d621a0d7`/`d62560d7`,
   `vwsubu.wv/.wx` -> `da21a0d7`/`da2560d7`, `vwsub.wv/.wx` ->
   `de21a0d7`/`de2560d7`. *)
let vwaddu_vv_entries = opivv_entries ~mnemonic:"vwaddu.vv"
let vwaddu_vx_entries = opivx_entries ~mnemonic:"vwaddu.vx"
let vwadd_vv_entries = opivv_entries ~mnemonic:"vwadd.vv"
let vwadd_vx_entries = opivx_entries ~mnemonic:"vwadd.vx"
let vwsubu_vv_entries = opivv_entries ~mnemonic:"vwsubu.vv"
let vwsubu_vx_entries = opivx_entries ~mnemonic:"vwsubu.vx"
let vwsub_vv_entries = opivv_entries ~mnemonic:"vwsub.vv"
let vwsub_vx_entries = opivx_entries ~mnemonic:"vwsub.vx"
let vwaddu_wv_entries = opivv_entries ~mnemonic:"vwaddu.wv"
let vwaddu_wx_entries = opivx_entries ~mnemonic:"vwaddu.wx"
let vwadd_wv_entries = opivv_entries ~mnemonic:"vwadd.wv"
let vwadd_wx_entries = opivx_entries ~mnemonic:"vwadd.wx"
let vwsubu_wv_entries = opivv_entries ~mnemonic:"vwsubu.wv"
let vwsubu_wx_entries = opivx_entries ~mnemonic:"vwsubu.wx"
let vwsub_wv_entries = opivv_entries ~mnemonic:"vwsub.wv"
let vwsub_wx_entries = opivx_entries ~mnemonic:"vwsub.wx"

(* [vwmulu]/[vwmulsu]/[vwmul]: OP-V's widening multiply family, `.vv`/`.vx`
   only, no [.vi] sibling - the same OPMVV/OPMVX shape and GAS text
   operand order as [vwadd]/etc. above (unlike the widening
   multiply-*accumulate* `vwmacc*` family, deliberately not admitted here:
   real GNU as swaps that family's last two text operands to `vd,
   vs1-or-rs1, vs2` rather than this shape's `vd, vs2, vs1-or-rs1` - a
   genuinely different shape not yet built). Confirmed against real GNU
   as, byte-identical on RV32/RV64: `vwmulu.vv/.vx` -> `e221a0d7`/
   `e22560d7`, `vwmulsu.vv/.vx` -> `ea21a0d7`/`ea2560d7`, `vwmul.vv/.vx` ->
   `ee21a0d7`/`ee2560d7`. *)
let vwmulu_vv_entries = opivv_entries ~mnemonic:"vwmulu.vv"
let vwmulu_vx_entries = opivx_entries ~mnemonic:"vwmulu.vx"
let vwmulsu_vv_entries = opivv_entries ~mnemonic:"vwmulsu.vv"
let vwmulsu_vx_entries = opivx_entries ~mnemonic:"vwmulsu.vx"
let vwmul_vv_entries = opivv_entries ~mnemonic:"vwmul.vv"
let vwmul_vx_entries = opivx_entries ~mnemonic:"vwmul.vx"

(* [vsext]/[vzext]: OP-V's integer sign-/zero-extend family - a genuinely
   new two-vector-register shape ([rd, rs2], no third operand). Confirmed
   against real GNU as, byte-identical on RV32/RV64: `vsext.vf2/.vf4/.vf8`
   -> `4a23a0d7`/`4a22a0d7`/`4a21a0d7`, `vzext.vf2/.vf4/.vf8` ->
   `4a2320d7`/`4a2220d7`/`4a2120d7`. *)
let vext_entry ~mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:vector-unary:%s" mnemonic (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", "v1"); ("rs2", "v2") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let vext_entries ~mnemonic = List.map (vext_entry ~mnemonic) [ Target.Riscv32; Target.Riscv64 ]
let vsext_vf2_entries = vext_entries ~mnemonic:"vsext.vf2"
let vsext_vf4_entries = vext_entries ~mnemonic:"vsext.vf4"
let vsext_vf8_entries = vext_entries ~mnemonic:"vsext.vf8"
let vzext_vf2_entries = vext_entries ~mnemonic:"vzext.vf2"
let vzext_vf4_entries = vext_entries ~mnemonic:"vzext.vf4"
let vzext_vf8_entries = vext_entries ~mnemonic:"vzext.vf8"

(* [vmand]/[vmandn]/[vmor]/[vmxor]/[vmorn]/[vmnand]/[vmnor]/[vmxnor]: OP-V's
   mask-register logical family ([.mm]) - the same all-vector-register
   [rd, rs2, rs1] shape {!opivv_entries} already builds for [vadd.vv]/etc.
   (see {!Isa_norm_riscv.mm_form} for why the normalization side still
   needs its own function despite this generator-side reuse: these have no
   masked [, v0.t] sibling, which this entry shape does not represent
   either way). Confirmed against real GNU as, byte-identical on RV32/RV64:
   `vmand.mm v1,v2,v3` -> `6621a0d7`, `vmandn.mm` -> `6221a0d7`, `vmor.mm`
   -> `6a21a0d7`, `vmxor.mm` -> `6e21a0d7`, `vmorn.mm` -> `7221a0d7`,
   `vmnand.mm` -> `7621a0d7`, `vmnor.mm` -> `7a21a0d7`, `vmxnor.mm` ->
   `7e21a0d7`. *)
let vmand_mm_entries = opivv_entries ~mnemonic:"vmand.mm"
let vmandn_mm_entries = opivv_entries ~mnemonic:"vmandn.mm"
let vmor_mm_entries = opivv_entries ~mnemonic:"vmor.mm"
let vmxor_mm_entries = opivv_entries ~mnemonic:"vmxor.mm"
let vmorn_mm_entries = opivv_entries ~mnemonic:"vmorn.mm"
let vmnand_mm_entries = opivv_entries ~mnemonic:"vmnand.mm"
let vmnor_mm_entries = opivv_entries ~mnemonic:"vmnor.mm"
let vmxnor_mm_entries = opivv_entries ~mnemonic:"vmxnor.mm"

(* [vredsum]/[vredand]/[vredor]/[vredxor]/[vredminu]/[vredmin]/[vredmaxu]/
   [vredmax.vs] and [vwredsumu]/[vwredsum.vs]: OP-V's vector-reduction
   family - the same all-vector-register [rd, rs2, rs1] shape as
   {!opivv_entries} already builds, with a real, selectable [vm] like every
   other reduction (unlike the mask-register-logical family above).
   Confirmed against real GNU as, byte-identical on RV32/RV64:
   `vredsum.vs v1,v2,v3` -> `022180d7`, `vredand.vs` -> `062180d7`,
   `vredor.vs` -> `0a2180d7`, `vredxor.vs` -> `0e2180d7`, `vredminu.vs` ->
   `122180d7`, `vredmin.vs` -> `162180d7`, `vredmaxu.vs` -> `1a2180d7`,
   `vredmax.vs` -> `1e2180d7`, `vwredsumu.vs` -> `c22180d7`, `vwredsum.vs`
   -> `c62180d7`. *)
let vredsum_vs_entries = opivv_entries ~mnemonic:"vredsum.vs"
let vredand_vs_entries = opivv_entries ~mnemonic:"vredand.vs"
let vredor_vs_entries = opivv_entries ~mnemonic:"vredor.vs"
let vredxor_vs_entries = opivv_entries ~mnemonic:"vredxor.vs"
let vredminu_vs_entries = opivv_entries ~mnemonic:"vredminu.vs"
let vredmin_vs_entries = opivv_entries ~mnemonic:"vredmin.vs"
let vredmaxu_vs_entries = opivv_entries ~mnemonic:"vredmaxu.vs"
let vredmax_vs_entries = opivv_entries ~mnemonic:"vredmax.vs"
let vwredsumu_vs_entries = opivv_entries ~mnemonic:"vwredsumu.vs"
let vwredsum_vs_entries = opivv_entries ~mnemonic:"vwredsum.vs"

(* [vmseq]/[vmsne]/[vmsltu]/[vmslt]/[vmsleu]/[vmsle]/[vmsgtu]/[vmsgt]: OP-V's
   mask-writing comparison family, full OPIVV/OPIVX/OPIVI shape except
   [vmsltu]/[vmslt] (no [.vi] sibling) and [vmsgtu]/[vmsgt] (no [.vv]
   sibling - real GNU as accepts `vmsgt(u).vv` only as a pseudo-instruction
   reversing `vmslt(u).vv`'s own operands, not admitted here). Confirmed
   against real GNU as, byte-identical on RV32/RV64: `vmseq.vv/.vx/.vi` ->
   `622180d7`/`622540d7`/`622db0d7`, `vmsne.vv/.vx/.vi` ->
   `662180d7`/`662540d7`/`662db0d7`, `vmsltu.vv/.vx` ->
   `6a2180d7`/`6a2540d7`, `vmslt.vv/.vx` -> `6e2180d7`/`6e2540d7`,
   `vmsleu.vv/.vx/.vi` -> `722180d7`/`722540d7`/`722db0d7`, `vmsle.vv/.vx/
   .vi` -> `762180d7`/`762540d7`/`762db0d7`, `vmsgtu.vx/.vi` ->
   `7a2540d7`/`7a2db0d7`, `vmsgt.vx/.vi` -> `7e2540d7`/`7e2db0d7`. *)
let vmseq_vv_entries = opivv_entries ~mnemonic:"vmseq.vv"
let vmseq_vx_entries = opivx_entries ~mnemonic:"vmseq.vx"
let vmseq_vi_entries = opivi_entries ~mnemonic:"vmseq.vi" ()
let vmsne_vv_entries = opivv_entries ~mnemonic:"vmsne.vv"
let vmsne_vx_entries = opivx_entries ~mnemonic:"vmsne.vx"
let vmsne_vi_entries = opivi_entries ~mnemonic:"vmsne.vi" ()
let vmsltu_vv_entries = opivv_entries ~mnemonic:"vmsltu.vv"
let vmsltu_vx_entries = opivx_entries ~mnemonic:"vmsltu.vx"
let vmslt_vv_entries = opivv_entries ~mnemonic:"vmslt.vv"
let vmslt_vx_entries = opivx_entries ~mnemonic:"vmslt.vx"
let vmsleu_vv_entries = opivv_entries ~mnemonic:"vmsleu.vv"
let vmsleu_vx_entries = opivx_entries ~mnemonic:"vmsleu.vx"
let vmsleu_vi_entries = opivi_entries ~mnemonic:"vmsleu.vi" ()
let vmsle_vv_entries = opivv_entries ~mnemonic:"vmsle.vv"
let vmsle_vx_entries = opivx_entries ~mnemonic:"vmsle.vx"
let vmsle_vi_entries = opivi_entries ~mnemonic:"vmsle.vi" ()
let vmsgtu_vx_entries = opivx_entries ~mnemonic:"vmsgtu.vx"
let vmsgtu_vi_entries = opivi_entries ~mnemonic:"vmsgtu.vi" ()
let vmsgt_vx_entries = opivx_entries ~mnemonic:"vmsgt.vx"
let vmsgt_vi_entries = opivi_entries ~mnemonic:"vmsgt.vi" ()

(* [vslideup]/[vslidedown]/[vslide1up]/[vslide1down]: OP-V's slide family -
   [.vx]/[.vi] for [vslideup]/[vslidedown] (no [.vv] sibling), [.vx] only
   for [vslide1up]/[vslide1down] (no [.vi] sibling - inserting one element
   needs a real scalar). The [.vi] immediate is UNSIGNED [zimm5] (0..31),
   like the shift-family shapes. Confirmed against real GNU as,
   byte-identical on RV32/RV64: `vslideup.vx v1,v2,a0` -> `3a2540d7`,
   `vslideup.vi v1,v2,5` -> `3a22b0d7`, `vslidedown.vx` -> `3e2540d7`,
   `vslidedown.vi` -> `3e22b0d7`, `vslide1up.vx` -> `3a2560d7`,
   `vslide1down.vx` -> `3e2560d7`. *)
let vslideup_vx_entries = opivx_entries ~mnemonic:"vslideup.vx"
let vslideup_vi_entries = opivi_entries ~imm_name:"zimm5" ~imm_value:"31" ~mnemonic:"vslideup.vi" ()
let vslidedown_vx_entries = opivx_entries ~mnemonic:"vslidedown.vx"

let vslidedown_vi_entries =
  opivi_entries ~imm_name:"zimm5" ~imm_value:"31" ~mnemonic:"vslidedown.vi" ()

let vslide1up_vx_entries = opivx_entries ~mnemonic:"vslide1up.vx"
let vslide1down_vx_entries = opivx_entries ~mnemonic:"vslide1down.vx"

(* The multiply-accumulate family - [vmacc]/[vnmsac]/[vmadd]/[vnmsub] and
   the widening siblings [vwmaccu]/[vwmacc]/[vwmaccsu]/[vwmaccus] - shares
   {!opivv_entry}/{!opivx_entry}'s operand names but real GNU as's text
   order swaps the last two operands (see
   {!Isa_norm_riscv.opmacc_vv_form}/[opmacc_vx_form]); [rs1] here always
   carries the operand real GNU as accepts second (a vector register for
   [.vv], a GPR for [.vx]), and [rs2] the one it accepts third. Confirmed
   against real GNU as, byte-identical on RV32/RV64: `vmacc.vv v1,v2,v3` ->
   `b63120d7`, `vmacc.vx v1,a0,v3` -> `b63560d7`, `vnmsac.vv/.vx` ->
   `be3120d7`/`be3560d7`, `vmadd.vv/.vx` -> `a63120d7`/`a63560d7`,
   `vnmsub.vv/.vx` -> `ae3120d7`/`ae3560d7`, `vwmaccu.vv/.vx` ->
   `f23120d7`/`f23560d7`, `vwmacc.vv/.vx` -> `f63120d7`/`f63560d7`,
   `vwmaccsu.vv/.vx` -> `fe3120d7`/`fe3560d7`, `vwmaccus.vx` ->
   `fa3560d7` (`vwmaccus` has no [.vv] sibling). *)
let opmacc_vv_entry ~mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:vector-vector:%s" mnemonic (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", "v1"); ("rs1", "v2"); ("rs2", "v3") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let opmacc_vv_entries ~mnemonic =
  List.map (opmacc_vv_entry ~mnemonic) [ Target.Riscv32; Target.Riscv64 ]

let opmacc_vx_entry ~mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:vector-scalar:%s" mnemonic (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", "v1"); ("rs1", "a0"); ("rs2", "v3") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let opmacc_vx_entries ~mnemonic =
  List.map (opmacc_vx_entry ~mnemonic) [ Target.Riscv32; Target.Riscv64 ]

let vmacc_vv_entries = opmacc_vv_entries ~mnemonic:"vmacc.vv"
let vmacc_vx_entries = opmacc_vx_entries ~mnemonic:"vmacc.vx"
let vnmsac_vv_entries = opmacc_vv_entries ~mnemonic:"vnmsac.vv"
let vnmsac_vx_entries = opmacc_vx_entries ~mnemonic:"vnmsac.vx"
let vmadd_vv_entries = opmacc_vv_entries ~mnemonic:"vmadd.vv"
let vmadd_vx_entries = opmacc_vx_entries ~mnemonic:"vmadd.vx"
let vnmsub_vv_entries = opmacc_vv_entries ~mnemonic:"vnmsub.vv"
let vnmsub_vx_entries = opmacc_vx_entries ~mnemonic:"vnmsub.vx"
let vwmaccu_vv_entries = opmacc_vv_entries ~mnemonic:"vwmaccu.vv"
let vwmaccu_vx_entries = opmacc_vx_entries ~mnemonic:"vwmaccu.vx"
let vwmacc_vv_entries = opmacc_vv_entries ~mnemonic:"vwmacc.vv"
let vwmacc_vx_entries = opmacc_vx_entries ~mnemonic:"vwmacc.vx"
let vwmaccsu_vv_entries = opmacc_vv_entries ~mnemonic:"vwmaccsu.vv"
let vwmaccsu_vx_entries = opmacc_vx_entries ~mnemonic:"vwmaccsu.vx"
let vwmaccus_vx_entries = opmacc_vx_entries ~mnemonic:"vwmaccus.vx"

(* [vid.v]: OP-V's element-index instruction - the first family surveyed
   with no [vs2]/[vs1]/[rs1] operand at all, just a destination (see
   {!Isa_norm_riscv.vid_form}). Confirmed against real GNU as,
   byte-identical on RV32/RV64: `vid.v v1` -> `5208a0d7`. *)
let vid_v_entry target =
  {
    form_id = "riscv:vid.v";
    target;
    lookup_key = "vid.v";
    case_id = Printf.sprintf "riscv:vid.v:vector-unary:%s" (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", "v1") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let vid_v_entries = List.map vid_v_entry [ Target.Riscv32; Target.Riscv64 ]

(* [viota.m]: shares {!vext_entries}'s exact [rd, rs2] shape (see
   {!Isa_norm_riscv.vext_form}'s reuse for the normalization side).
   Confirmed against real GNU as, byte-identical on RV32/RV64: `viota.m
   v1,v2` -> `522820d7`. *)
let viota_m_entries = vext_entries ~mnemonic:"viota.m"

(* [vcompress.vm]: shares {!opivv_entries}'s exact [rd, rs2, rs1] shape
   (see {!Isa_norm_riscv.mm_form}'s reuse for the normalization side - no
   masked sibling exists). Confirmed against real GNU as, byte-identical
   on RV32/RV64: `vcompress.vm v1,v2,v3` -> `5e21a0d7`. *)
let vcompress_vm_entries = opivv_entries ~mnemonic:"vcompress.vm"

(* [vmsbf.m]/[vmsif.m]/[vmsof.m]: share {!vext_entries}'s exact
   [rd, rs2] shape. Confirmed against real GNU as, byte-identical on
   RV32/RV64: `vmsbf.m v1,v2` -> `5220a0d7`, `vmsif.m v1,v2` ->
   `5221a0d7`, `vmsof.m v1,v2` -> `522120d7`. *)
let vmsbf_m_entries = vext_entries ~mnemonic:"vmsbf.m"
let vmsif_m_entries = vext_entries ~mnemonic:"vmsif.m"
let vmsof_m_entries = vext_entries ~mnemonic:"vmsof.m"

(* [vcpop.m]/[vfirst.m]: the same [rd, rs2] shape as {!vext_entries}
   above but with a GPR destination (see
   {!Isa_norm_riscv.v_to_x_unary_form}). Confirmed against real GNU as,
   byte-identical on RV32/RV64: `vcpop.m a0,v2` -> `42282557`, `vfirst.m
   a0,v2` -> `4228a557`. *)
let v_to_x_unary_entry ~mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:vector-unary:%s" mnemonic (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", "a0"); ("rs2", "v2") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let v_to_x_unary_entries ~mnemonic =
  List.map (v_to_x_unary_entry ~mnemonic) [ Target.Riscv32; Target.Riscv64 ]

let vcpop_m_entries = v_to_x_unary_entries ~mnemonic:"vcpop.m"
let vfirst_m_entries = v_to_x_unary_entries ~mnemonic:"vfirst.m"

(* The add-with-carry/subtract-with-borrow family - [vadc]/[vmadc]/[vsbc]/
   [vmsbc] - has a mandatory, literal [v0] 4th operand on its "m"-suffixed
   variants (see {!Isa_norm_riscv.carry_m_vv_form}/[carry_m_vx_form]/
   [carry_m_vi_form]); the bare (non-"m") siblings [vmadc.vv]/[.vx]/[.vi]
   and [vmsbc.vv]/[.vx] share {!opivv_entries}/{!opivx_entries}/
   {!opivi_entries}'s exact shape unchanged. Confirmed against real GNU
   as, byte-identical on RV32/RV64: `vadc.vvm v1,v2,v3,v0` -> `402180d7`,
   `vadc.vxm v1,v2,a0,v0` -> `402540d7`, `vadc.vim v1,v2,5,v0` ->
   `4022b0d7`, `vmadc.vvm v1,v2,v3,v0` -> `442180d7`, `vmadc.vv v1,v2,v3`
   -> `462180d7`, `vsbc.vvm v1,v2,v3,v0` -> `482180d7`, `vmsbc.vvm
   v1,v2,v3,v0` -> `4c2180d7`, `vmsbc.vv v1,v2,v3` -> `4e2180d7`. *)
let carry_m_vv_entry ~mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:vector-vector:%s" mnemonic (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", "v1"); ("rs2", "v2"); ("rs1", "v3"); ("vcarry", "v0") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let carry_m_vv_entries ~mnemonic =
  List.map (carry_m_vv_entry ~mnemonic) [ Target.Riscv32; Target.Riscv64 ]

let carry_m_vx_entry ~mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:vector-scalar:%s" mnemonic (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", "v1"); ("rs2", "v2"); ("rs1", "a0"); ("vcarry", "v0") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let carry_m_vx_entries ~mnemonic =
  List.map (carry_m_vx_entry ~mnemonic) [ Target.Riscv32; Target.Riscv64 ]

let carry_m_vi_entry ~mnemonic target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:vector-immediate:%s" mnemonic (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", "v1"); ("rs2", "v2"); ("simm5", "-5"); ("vcarry", "v0") ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let carry_m_vi_entries ~mnemonic =
  List.map (carry_m_vi_entry ~mnemonic) [ Target.Riscv32; Target.Riscv64 ]

let vadc_vvm_entries = carry_m_vv_entries ~mnemonic:"vadc.vvm"
let vadc_vxm_entries = carry_m_vx_entries ~mnemonic:"vadc.vxm"
let vadc_vim_entries = carry_m_vi_entries ~mnemonic:"vadc.vim"
let vmadc_vvm_entries = carry_m_vv_entries ~mnemonic:"vmadc.vvm"
let vmadc_vxm_entries = carry_m_vx_entries ~mnemonic:"vmadc.vxm"
let vmadc_vim_entries = carry_m_vi_entries ~mnemonic:"vmadc.vim"
let vmadc_vv_entries = opivv_entries ~mnemonic:"vmadc.vv"
let vmadc_vx_entries = opivx_entries ~mnemonic:"vmadc.vx"
let vmadc_vi_entries = opivi_entries ~mnemonic:"vmadc.vi" ()
let vsbc_vvm_entries = carry_m_vv_entries ~mnemonic:"vsbc.vvm"
let vsbc_vxm_entries = carry_m_vx_entries ~mnemonic:"vsbc.vxm"
let vmsbc_vvm_entries = carry_m_vv_entries ~mnemonic:"vmsbc.vvm"
let vmsbc_vxm_entries = carry_m_vx_entries ~mnemonic:"vmsbc.vxm"
let vmsbc_vv_entries = opivv_entries ~mnemonic:"vmsbc.vv"
let vmsbc_vx_entries = opivx_entries ~mnemonic:"vmsbc.vx"

(* [vmerge]: shares {!carry_m_vv_entries}/{!carry_m_vx_entries}/
   {!carry_m_vi_entries}'s exact mandatory-[v0] shape, with no bare
   (non-"m") sibling. Confirmed against real GNU as, byte-identical on
   RV32/RV64: `vmerge.vvm v1,v2,v3,v0` -> `5c2180d7`, `vmerge.vxm
   v1,v2,a0,v0` -> `5c2540d7`, `vmerge.vim v1,v2,5,v0` -> `5c22b0d7`. *)
let vmerge_vvm_entries = carry_m_vv_entries ~mnemonic:"vmerge.vvm"
let vmerge_vxm_entries = carry_m_vx_entries ~mnemonic:"vmerge.vxm"
let vmerge_vim_entries = carry_m_vi_entries ~mnemonic:"vmerge.vim"

(* [vmv.x.s]/[vmv.s.x]: OP-V's scalar-move pair - the GPR-destination/
   vector-destination two-operand shapes {!Isa_norm_riscv.mv_x_s_form}/
   [mv_s_x_form] model. Confirmed against real GNU as, byte-identical on
   RV32/RV64: `vmv.x.s a0,v2` -> `42202557`, `vmv.s.x v1,a0` ->
   `420560d7`. *)
let vmv_x_s_entries =
  List.map
    (fun target ->
      {
        form_id = "riscv:vmv.x.s";
        target;
        lookup_key = "vmv.x.s";
        case_id = Printf.sprintf "riscv:vmv.x.s:vector-unary:%s" (Target.to_string target);
        rule_ids = [ "v-enabled"; "vector-register-operands" ];
        operands = [ ("rd", "a0"); ("rs2", "v2") ];
        lines_before = [];
        lines_after = [];
        configuration = v_configuration_for target;
      })
    [ Target.Riscv32; Target.Riscv64 ]

let vmv_s_x_entries =
  List.map
    (fun target ->
      {
        form_id = "riscv:vmv.s.x";
        target;
        lookup_key = "vmv.s.x";
        case_id = Printf.sprintf "riscv:vmv.s.x:vector-scalar:%s" (Target.to_string target);
        rule_ids = [ "v-enabled"; "vector-register-operands" ];
        operands = [ ("rd", "v1"); ("rs1", "a0") ];
        lines_before = [];
        lines_after = [];
        configuration = v_configuration_for target;
      })
    [ Target.Riscv32; Target.Riscv64 ]

(* [vmv.v.v]/[.v.x]/[.v.i]: OP-V's unconditional-move family (see
   {!Isa_norm_riscv.vmv_v_form}). Confirmed against real GNU as,
   byte-identical on RV32/RV64: `vmv.v.v v1,v2` -> `5e0100d7`, `vmv.v.x
   v1,a0` -> `5e0540d7`, `vmv.v.i v1,5` -> `5e02b0d7`. *)
let vmv_v_entry ~mnemonic ~rs1_name ~rs1_value target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:vector-unary:%s" mnemonic (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", "v1"); (rs1_name, rs1_value) ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let vmv_v_entries ~mnemonic ~rs1_name ~rs1_value =
  List.map (vmv_v_entry ~mnemonic ~rs1_name ~rs1_value) [ Target.Riscv32; Target.Riscv64 ]

let vmv_v_v_entries = vmv_v_entries ~mnemonic:"vmv.v.v" ~rs1_name:"rs1" ~rs1_value:"v2"
let vmv_v_x_entries = vmv_v_entries ~mnemonic:"vmv.v.x" ~rs1_name:"rs1" ~rs1_value:"a0"
let vmv_v_i_entries = vmv_v_entries ~mnemonic:"vmv.v.i" ~rs1_name:"simm5" ~rs1_value:"5"

(* [vmv1r.v]/[vmv2r.v]/[vmv4r.v]/[vmv8r.v]: OP-V's whole-register-group
   move family (see {!Isa_norm_riscv.whole_reg_move_form}). Confirmed
   against real GNU as, byte-identical on RV32/RV64: `vmv1r.v v1,v2` ->
   `9e2030d7`, `vmv2r.v v2,v4` -> `9e40b157`, `vmv4r.v v4,v8` ->
   `9e81b257`, `vmv8r.v v8,v16` -> `9f03b457`. *)
let whole_reg_move_entry ~mnemonic ~rd ~rs2 target =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    case_id = Printf.sprintf "riscv:%s:vector-unary:%s" mnemonic (Target.to_string target);
    rule_ids = [ "v-enabled"; "vector-register-operands" ];
    operands = [ ("rd", rd); ("rs2", rs2) ];
    lines_before = [];
    lines_after = [];
    configuration = v_configuration_for target;
  }

let whole_reg_move_entries ~mnemonic ~rd ~rs2 =
  List.map (whole_reg_move_entry ~mnemonic ~rd ~rs2) [ Target.Riscv32; Target.Riscv64 ]

let vmv1r_v_entries = whole_reg_move_entries ~mnemonic:"vmv1r.v" ~rd:"v1" ~rs2:"v2"
let vmv2r_v_entries = whole_reg_move_entries ~mnemonic:"vmv2r.v" ~rd:"v2" ~rs2:"v4"
let vmv4r_v_entries = whole_reg_move_entries ~mnemonic:"vmv4r.v" ~rd:"v4" ~rs2:"v8"
let vmv8r_v_entries = whole_reg_move_entries ~mnemonic:"vmv8r.v" ~rd:"v8" ~rs2:"v16"

(* [vsmul]: the saturating fixed-point multiply pair - shares
   {!opivv_entries}/{!opivx_entries}'s exact shape unchanged (OPIVV/OPIVX,
   not OPMVV/OPMVX). Confirmed against real GNU as, byte-identical on
   RV32/RV64: `vsmul.vv v1,v2,v3` -> `9e2180d7`, `vsmul.vx v1,v2,a0` ->
   `9e2540d7`. *)
let vsmul_vv_entries = opivv_entries ~mnemonic:"vsmul.vv"
let vsmul_vx_entries = opivx_entries ~mnemonic:"vsmul.vx"

let all =
  sw_entries @ beq_entries @ c_addi_entries @ x86_mov_entries @ x86_fadd_entries @ fadd_s_entries
  @ fsub_s_entries @ fmul_s_entries @ fdiv_s_entries @ fadd_d_entries @ fsub_d_entries
  @ fmul_d_entries @ fdiv_d_entries @ flw_entries @ fld_entries @ fsw_entries @ fsd_entries
  @ sh1add_entries @ sh2add_entries @ sh3add_entries @ sh1adduw_entries @ sh2adduw_entries
  @ sh3adduw_entries @ min_entries @ minu_entries @ max_entries @ maxu_entries @ andn_entries
  @ orn_entries @ xnor_entries @ rol_entries @ ror_entries @ clz_entries @ ctz_entries
  @ cpop_entries @ sextb_entries @ sexth_entries @ orcb_entries @ clzw_entries @ ctzw_entries
  @ cpopw_entries @ brev8_entries @ rev8_entries @ pack_entries @ packh_entries @ packw_entries
  @ zip_entries @ unzip_entries @ rolw_entries @ rorw_entries @ rori_entries @ roriw_entries
  @ zext_h_entries @ clmul_entries @ clmulh_entries @ xperm4_entries @ xperm8_entries
  @ sha256sum0_entries @ sha256sum1_entries @ sha256sig0_entries @ sha256sig1_entries
  @ sha512sum0_entries @ sha512sum1_entries @ sha512sig0_entries @ sha512sig1_entries
  @ sha512sum0r_entries @ sha512sum1r_entries @ sha512sig0l_entries @ sha512sig1l_entries
  @ sha512sig0h_entries @ sha512sig1h_entries @ aes64ds_entries @ aes64dsm_entries @ aes64es_entries
  @ aes64esm_entries @ aes64ks2_entries @ aes64im_entries @ aes64ks1i_entries @ aes32dsi_entries
  @ aes32dsmi_entries @ aes32esi_entries @ aes32esmi_entries @ csrrw_entries @ csrrs_entries
  @ csrrc_entries @ csrrwi_entries @ csrrsi_entries @ csrrci_entries @ csrr_entries @ csrw_entries
  @ csrs_entries @ csrc_entries @ csrwi_entries @ csrsi_entries @ csrci_entries @ amoswap_w_entries
  @ amoadd_w_entries @ amoxor_w_entries @ amoand_w_entries @ amoor_w_entries @ amomin_w_entries
  @ amomax_w_entries @ amominu_w_entries @ amomaxu_w_entries @ sc_w_entries @ lr_w_entries
  @ amoswap_d_entries @ amoadd_d_entries @ amoxor_d_entries @ amoand_d_entries @ amoor_d_entries
  @ amomin_d_entries @ amomax_d_entries @ amominu_d_entries @ amomaxu_d_entries @ sc_d_entries
  @ lr_d_entries @ fsgnj_s_entries @ fsgnjn_s_entries @ fsgnjx_s_entries @ fsgnj_d_entries
  @ fsgnjn_d_entries @ fsgnjx_d_entries @ fmin_s_entries @ fmax_s_entries @ fmin_d_entries
  @ fmax_d_entries @ fsqrt_s_entries @ fsqrt_d_entries @ fclass_s_entries @ fclass_d_entries
  @ fmadd_s_entries @ fmsub_s_entries @ fnmsub_s_entries @ fnmadd_s_entries @ fmadd_d_entries
  @ fmsub_d_entries @ fnmsub_d_entries @ fnmadd_d_entries @ feq_s_entries @ fle_s_entries
  @ flt_s_entries @ feq_d_entries @ fle_d_entries @ flt_d_entries @ fmv_x_w_entries
  @ fmv_w_x_entries @ fcvt_w_s_entries @ fcvt_wu_s_entries @ fcvt_s_w_entries @ fcvt_s_wu_entries
  @ fcvt_w_d_entries @ fcvt_wu_d_entries @ fcvt_d_w_entries @ fcvt_d_wu_entries @ fcvt_s_d_entries
  @ fcvt_d_s_entries @ fcvt_l_d_entries @ fcvt_lu_d_entries @ fcvt_l_s_entries @ fcvt_lu_s_entries
  @ fcvt_s_l_entries @ fcvt_s_lu_entries @ fcvt_d_l_entries @ fcvt_d_lu_entries @ vsetvl_entries
  @ vsetvli_entries @ vsetivli_entries @ vadd_vv_entries @ vadd_vx_entries @ vadd_vi_entries
  @ vsub_vv_entries @ vsub_vx_entries @ vrsub_vx_entries @ vrsub_vi_entries @ vand_vv_entries
  @ vand_vx_entries @ vand_vi_entries @ vor_vv_entries @ vor_vx_entries @ vor_vi_entries
  @ vxor_vv_entries @ vxor_vx_entries @ vxor_vi_entries @ vsll_vv_entries @ vsll_vx_entries
  @ vsll_vi_entries @ vsrl_vv_entries @ vsrl_vx_entries @ vsrl_vi_entries @ vsra_vv_entries
  @ vsra_vx_entries @ vsra_vi_entries @ vminu_vv_entries @ vminu_vx_entries @ vmin_vv_entries
  @ vmin_vx_entries @ vmaxu_vv_entries @ vmaxu_vx_entries @ vmax_vv_entries @ vmax_vx_entries
  @ vmul_vv_entries @ vmul_vx_entries @ vmulh_vv_entries @ vmulh_vx_entries @ vmulhu_vv_entries
  @ vmulhu_vx_entries @ vmulhsu_vv_entries @ vmulhsu_vx_entries @ vdivu_vv_entries
  @ vdivu_vx_entries @ vdiv_vv_entries @ vdiv_vx_entries @ vremu_vv_entries @ vremu_vx_entries
  @ vrem_vv_entries @ vrem_vx_entries @ vsaddu_vv_entries @ vsaddu_vx_entries @ vsaddu_vi_entries
  @ vsadd_vv_entries @ vsadd_vx_entries @ vsadd_vi_entries @ vssubu_vv_entries @ vssubu_vx_entries
  @ vssub_vv_entries @ vssub_vx_entries @ vaaddu_vv_entries @ vaaddu_vx_entries @ vaadd_vv_entries
  @ vaadd_vx_entries @ vasubu_vv_entries @ vasubu_vx_entries @ vasub_vv_entries @ vasub_vx_entries
  @ vnsrl_wv_entries @ vnsrl_wx_entries @ vnsrl_wi_entries @ vnsra_wv_entries @ vnsra_wx_entries
  @ vnsra_wi_entries @ vnclipu_wv_entries @ vnclipu_wx_entries @ vnclipu_wi_entries
  @ vnclip_wv_entries @ vnclip_wx_entries @ vnclip_wi_entries @ vssrl_vv_entries @ vssrl_vx_entries
  @ vssrl_vi_entries @ vssra_vv_entries @ vssra_vx_entries @ vssra_vi_entries @ vrgather_vv_entries
  @ vrgather_vx_entries @ vrgather_vi_entries @ vrgatherei16_vv_entries @ vwaddu_vv_entries
  @ vwaddu_vx_entries @ vwadd_vv_entries @ vwadd_vx_entries @ vwsubu_vv_entries @ vwsubu_vx_entries
  @ vwsub_vv_entries @ vwsub_vx_entries @ vwaddu_wv_entries @ vwaddu_wx_entries @ vwadd_wv_entries
  @ vwadd_wx_entries @ vwsubu_wv_entries @ vwsubu_wx_entries @ vwsub_wv_entries @ vwsub_wx_entries
  @ vwmulu_vv_entries @ vwmulu_vx_entries @ vwmulsu_vv_entries @ vwmulsu_vx_entries
  @ vwmul_vv_entries @ vwmul_vx_entries @ vsext_vf2_entries @ vsext_vf4_entries @ vsext_vf8_entries
  @ vzext_vf2_entries @ vzext_vf4_entries @ vzext_vf8_entries @ vmand_mm_entries @ vmandn_mm_entries
  @ vmor_mm_entries @ vmxor_mm_entries @ vmorn_mm_entries @ vmnand_mm_entries @ vmnor_mm_entries
  @ vmxnor_mm_entries @ vredsum_vs_entries @ vredand_vs_entries @ vredor_vs_entries
  @ vredxor_vs_entries @ vredminu_vs_entries @ vredmin_vs_entries @ vredmaxu_vs_entries
  @ vredmax_vs_entries @ vwredsumu_vs_entries @ vwredsum_vs_entries @ vmseq_vv_entries
  @ vmseq_vx_entries @ vmseq_vi_entries @ vmsne_vv_entries @ vmsne_vx_entries @ vmsne_vi_entries
  @ vmsltu_vv_entries @ vmsltu_vx_entries @ vmslt_vv_entries @ vmslt_vx_entries @ vmsleu_vv_entries
  @ vmsleu_vx_entries @ vmsleu_vi_entries @ vmsle_vv_entries @ vmsle_vx_entries @ vmsle_vi_entries
  @ vmsgtu_vx_entries @ vmsgtu_vi_entries @ vmsgt_vx_entries @ vmsgt_vi_entries
  @ vslideup_vx_entries @ vslideup_vi_entries @ vslidedown_vx_entries @ vslidedown_vi_entries
  @ vslide1up_vx_entries @ vslide1down_vx_entries @ vmacc_vv_entries @ vmacc_vx_entries
  @ vnmsac_vv_entries @ vnmsac_vx_entries @ vmadd_vv_entries @ vmadd_vx_entries @ vnmsub_vv_entries
  @ vnmsub_vx_entries @ vwmaccu_vv_entries @ vwmaccu_vx_entries @ vwmacc_vv_entries
  @ vwmacc_vx_entries @ vwmaccsu_vv_entries @ vwmaccsu_vx_entries @ vwmaccus_vx_entries
  @ vid_v_entries @ viota_m_entries @ vcompress_vm_entries @ vmsbf_m_entries @ vmsif_m_entries
  @ vmsof_m_entries @ vcpop_m_entries @ vfirst_m_entries @ vadc_vvm_entries @ vadc_vxm_entries
  @ vadc_vim_entries @ vmadc_vvm_entries @ vmadc_vxm_entries @ vmadc_vim_entries @ vmadc_vv_entries
  @ vmadc_vx_entries @ vmadc_vi_entries @ vsbc_vvm_entries @ vsbc_vxm_entries @ vmsbc_vvm_entries
  @ vmsbc_vxm_entries @ vmsbc_vv_entries @ vmsbc_vx_entries @ vmerge_vvm_entries
  @ vmerge_vxm_entries @ vmerge_vim_entries @ vmv_x_s_entries @ vmv_s_x_entries @ vmv_v_v_entries
  @ vmv_v_x_entries @ vmv_v_i_entries @ vmv1r_v_entries @ vmv2r_v_entries @ vmv4r_v_entries
  @ vmv8r_v_entries @ vsmul_vv_entries @ vsmul_vx_entries

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
    | ("fadd.s" | "fsub.s" | "fmul.s" | "fdiv.s" | "fadd.d" | "fsub.d" | "fmul.d" | "fdiv.d") as
      mnemonic ->
        Printf.sprintf
          "riscv_family_encode.ml's f_arith_desc Opcode.%s / Lowered.R OP-FP alternative (opcode \
           0x53, bare rm=dyn funct3 7)"
          (match mnemonic with
          | "fadd.s" -> "Fadd_s"
          | "fsub.s" -> "Fsub_s"
          | "fmul.s" -> "Fmul_s"
          | "fdiv.s" -> "Fdiv_s"
          | "fadd.d" -> "Fadd_d"
          | "fsub.d" -> "Fsub_d"
          | "fmul.d" -> "Fmul_d"
          | "fdiv.d" -> "Fdiv_d"
          | _ -> assert false)
    | ("sh1add" | "sh2add" | "sh3add") as mnemonic ->
        Printf.sprintf
          "riscv_family_encode.ml's r_desc Opcode.%s / Lowered.R alternative (opcode 0x33, funct3 \
           %d, funct7 0x10)"
          (match mnemonic with
          | "sh1add" -> "Sh1add"
          | "sh2add" -> "Sh2add"
          | "sh3add" -> "Sh3add"
          | _ -> assert false)
          (match mnemonic with "sh1add" -> 2 | "sh2add" -> 4 | "sh3add" -> 6 | _ -> assert false)
    | ("sh1add.uw" | "sh2add.uw" | "sh3add.uw") as mnemonic ->
        Printf.sprintf
          "riscv_family_encode.ml's r_desc Opcode.%s / Lowered.R alternative (opcode 0x3b, funct3 \
           %d, funct7 0x10, RV64-only)"
          (match mnemonic with
          | "sh1add.uw" -> "Sh1adduw"
          | "sh2add.uw" -> "Sh2adduw"
          | "sh3add.uw" -> "Sh3adduw"
          | _ -> assert false)
          (match mnemonic with
          | "sh1add.uw" -> 2
          | "sh2add.uw" -> 4
          | "sh3add.uw" -> 6
          | _ -> assert false)
    | ("min" | "minu" | "max" | "maxu") as mnemonic ->
        Printf.sprintf
          "riscv_family_encode.ml's r_desc Opcode.%s / Lowered.R alternative (opcode 0x33, funct3 \
           %d, funct7 0x05)"
          (match mnemonic with
          | "min" -> "Min"
          | "minu" -> "Minu"
          | "max" -> "Max"
          | "maxu" -> "Maxu"
          | _ -> assert false)
          (match mnemonic with
          | "min" -> 4
          | "minu" -> 5
          | "max" -> 6
          | "maxu" -> 7
          | _ -> assert false)
    | ("clz" | "ctz" | "cpop" | "sext.b" | "sext.h" | "orc.b" | "clzw" | "ctzw" | "cpopw") as
      mnemonic ->
        Printf.sprintf
          "riscv_family_encode.ml's unary_imm_desc Opcode.%s / Lowered.I alternative (opcode 0x%x, \
           funct3 %d, funct12 0x%x)"
          (match mnemonic with
          | "clz" -> "Clz"
          | "ctz" -> "Ctz"
          | "cpop" -> "Cpop"
          | "sext.b" -> "Sextb"
          | "sext.h" -> "Sexth"
          | "orc.b" -> "Orcb"
          | "clzw" -> "Clzw"
          | "ctzw" -> "Ctzw"
          | "cpopw" -> "Cpopw"
          | _ -> assert false)
          (match mnemonic with
          | "clz" | "ctz" | "cpop" | "sext.b" | "sext.h" | "orc.b" -> 0x13
          | _ -> 0x1b)
          (match mnemonic with "orc.b" -> 5 | _ -> 1)
          (match mnemonic with
          | "clz" | "clzw" -> 0x600
          | "ctz" | "ctzw" -> 0x601
          | "cpop" | "cpopw" -> 0x602
          | "sext.b" -> 0x604
          | "sext.h" -> 0x605
          | "orc.b" -> 0x287
          | _ -> assert false)
    | "brev8" ->
        "riscv_family_encode.ml's unary_imm_desc Opcode.Brev8 / Lowered.I alternative (opcode \
         0x13, funct3 5, funct12 0x687)"
    | "rev8" ->
        "riscv_family_encode.ml's unary_imm_desc Opcode.Rev8 / Lowered.I alternative (opcode 0x13, \
         funct3 5, funct12 0x6b8 on RV64)"
    | "rev8.rv32" ->
        "riscv_family_encode.ml's unary_imm_desc Opcode.Rev8 / Lowered.I alternative (opcode 0x13, \
         funct3 5, funct12 0x698 on RV32 - same Opcode.t and rendered mnemonic \"rev8\" as the \
         RV64 record above, XLEN-dependent funct12)"
    | ("pack" | "packh") as mnemonic ->
        Printf.sprintf
          "riscv_family_encode.ml's r_desc Opcode.%s / Lowered.R alternative (opcode 0x33, funct3 \
           %d, funct7 0x04)"
          (match mnemonic with "pack" -> "Pack" | "packh" -> "Packh" | _ -> assert false)
          (match mnemonic with "pack" -> 4 | "packh" -> 7 | _ -> assert false)
    | "packw" ->
        "riscv_family_encode.ml's r_desc Opcode.Packw / Lowered.R alternative (opcode 0x3b, funct3 \
         4, funct7 0x04, RV64-only)"
    | ("zip" | "unzip") as mnemonic ->
        Printf.sprintf
          "riscv_family_encode.ml's unary_imm_desc Opcode.%s / Lowered.I alternative (opcode 0x13, \
           funct3 %d, funct12 0x08f, RV32-only)"
          (match mnemonic with "zip" -> "Zip" | "unzip" -> "Unzip" | _ -> assert false)
          (match mnemonic with "zip" -> 1 | "unzip" -> 5 | _ -> assert false)
    | ("rolw" | "rorw") as mnemonic ->
        Printf.sprintf
          "riscv_family_encode.ml's r_desc Opcode.%s / Lowered.R alternative (opcode 0x3b, funct3 \
           %d, funct7 0x30, RV64-only)"
          (match mnemonic with "rolw" -> "Rolw" | "rorw" -> "Rorw" | _ -> assert false)
          (match mnemonic with "rolw" -> 1 | "rorw" -> 5 | _ -> assert false)
    | "rori" ->
        "riscv_family_encode.ml's i_desc Opcode.Rori / Lowered.I alternative (opcode 0x13, funct3 \
         5, funct_hi 0x18 on RV64 with a 6-bit shamt, 0x30 on RV32 with a 5-bit shamt)"
    | "rori.rv32" ->
        "riscv_family_encode.ml's i_desc Opcode.Rori / Lowered.I alternative (opcode 0x13, funct3 \
         5, funct_hi 0x30 on RV32 with a 5-bit shamt - same Opcode.t and rendered mnemonic \
         \"rori\" as the RV64 record above, XLEN-dependent funct_hi/shamt width)"
    | "roriw" ->
        "riscv_family_encode.ml's i_desc Opcode.Roriw / Lowered.I alternative (opcode 0x1b, funct3 \
         5, funct_hi 0x30, 5-bit shamt, RV64-only)"
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
