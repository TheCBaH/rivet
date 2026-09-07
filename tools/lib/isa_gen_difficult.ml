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
  @ fmul_d_entries @ fdiv_d_entries @ sh1add_entries @ sh2add_entries @ sh3add_entries
  @ sh1adduw_entries @ sh2adduw_entries @ sh3adduw_entries @ min_entries @ minu_entries
  @ max_entries @ maxu_entries @ andn_entries @ orn_entries @ xnor_entries @ rol_entries
  @ ror_entries @ clz_entries @ ctz_entries @ cpop_entries @ sextb_entries @ sexth_entries
  @ orcb_entries @ clzw_entries @ ctzw_entries @ cpopw_entries @ brev8_entries @ rev8_entries
  @ pack_entries @ packh_entries @ packw_entries @ zip_entries @ unzip_entries @ rolw_entries
  @ rorw_entries @ rori_entries @ roriw_entries @ zext_h_entries @ clmul_entries @ clmulh_entries
  @ xperm4_entries @ xperm8_entries

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
