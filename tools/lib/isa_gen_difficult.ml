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
  @ fcvt_s_l_entries @ fcvt_s_lu_entries @ fcvt_d_l_entries @ fcvt_d_lu_entries

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
