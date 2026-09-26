type entry = {
  name : string;
  target : Target.t;
  category : string;
  mnemonic : string;
  lines : string list;
  gas_configuration : string list;
  ours_features : string option;
  expected_code : string;
}

let riscv_profiles = [ Target.Riscv32; Target.Riscv64 ]
let x86_profiles = [ Target.X86_32; Target.X86_64 ]
let t = Target.to_string

(* GAS's baseline configuration for a target, as for every other case; a feature negative swaps
   the -march for one that leaves the component out. *)
let base_config = Isa_gen_case_build.configuration_for

let riscv_config target arch32 arch64 =
  match target with
  | Target.Riscv32 -> [ "-march=" ^ arch32; "-mabi=ilp32"; "-mno-relax" ]
  | _ -> [ "-march=" ^ arch64; "-mabi=lp64"; "-mno-relax" ]

let riscv_entry ~name ~category ~mnemonic ?(gas_configuration = None) ?ours_features ~code line
    target =
  {
    name;
    target;
    category;
    mnemonic;
    lines = [ line ];
    gas_configuration = Option.value gas_configuration ~default:(base_config target);
    ours_features;
    expected_code = Printf.sprintf "%s.%s" (t target) code;
  }

let riscv_common =
  List.concat_map
    (fun target ->
      let shamt = match target with Target.Riscv32 -> "32" | _ -> "64" in
      [
        riscv_entry ~name:"imm12-out-of-range" ~category:"immediate-range" ~mnemonic:"addi"
          ~code:"fixup" "addi a0, a1, 2048" target;
        riscv_entry ~name:"shamt-out-of-range" ~category:"immediate-range" ~mnemonic:"slli"
          ~code:"fixup" ("slli a0, a1, " ^ shamt) target;
        riscv_entry ~name:"upper-imm-out-of-range" ~category:"immediate-range" ~mnemonic:"lui"
          ~code:"fixup" "lui a0, 1048576" target;
        riscv_entry ~name:"missing-operand" ~category:"operand-shape" ~mnemonic:"addi" ~code:"lower"
          "addi a0, a1" target;
        riscv_entry ~name:"zmmul-disabled" ~category:"feature-disabled" ~mnemonic:"mul"
          ~gas_configuration:(Some (riscv_config target "rv32i" "rv64i"))
          ~ours_features:"none" ~code:"feature" "mul a0, a1, a2" target;
        riscv_entry ~name:"m-disabled" ~category:"feature-disabled" ~mnemonic:"remu"
          ~gas_configuration:(Some (riscv_config target "rv32i_zmmul" "rv64i_zmmul"))
          ~ours_features:"none,+zmmul" ~code:"feature" "remu a0, a1, a2" target;
      ])
    riscv_profiles

let riscv_xlen =
  [
    riscv_entry ~name:"rv64-only-mnemonic" ~category:"xlen-restricted" ~mnemonic:"addw"
      ~code:"lower" "addw a0, a1, a2" Target.Riscv32;
  ]

let x86_entry ~name ~category ~mnemonic ?gas_configuration ?ours_features ~code line target =
  {
    name;
    target;
    category;
    mnemonic;
    lines = [ line ];
    gas_configuration = Option.value gas_configuration ~default:(base_config target);
    ours_features;
    expected_code = "x86." ^ code;
  }

let x86_common =
  List.concat_map
    (fun target ->
      let no87 =
        match target with Target.X86_32 -> "-march=i686+no87" | _ -> "-march=generic64+no87"
      in
      [
        x86_entry ~name:"immediate-destination" ~category:"operand-shape" ~mnemonic:"movl"
          ~code:"lower" "movl $1, $2" target;
        x86_entry ~name:"missing-operand" ~category:"operand-shape" ~mnemonic:"addl" ~code:"lower"
          "addl %eax" target;
        x86_entry ~name:"register-width-mismatch" ~category:"operand-width" ~mnemonic:"movl"
          ~code:"lower" "movl %ax, %ecx" target;
        x86_entry ~name:"bad-index-scale" ~category:"address-form" ~mnemonic:"addl" ~code:"operand"
          "addl $1, (%eax,%eax,3)" target;
        x86_entry ~name:"x87-disabled" ~category:"feature-disabled" ~mnemonic:"fldl"
          ~gas_configuration:[ no87 ] ~ours_features:"-x87" ~code:"feature" "fldl 8(%esp)" target;
      ])
    x86_profiles

let x86_mode =
  [
    x86_entry ~name:"64-bit-register-in-32-bit-mode" ~category:"xlen-restricted" ~mnemonic:"movq"
      ~code:"operand" "movq %rax, %rbx" Target.X86_32;
  ]

let all = riscv_common @ riscv_xlen @ x86_common @ x86_mode
let case_id e = Printf.sprintf "negative:%s:%s:%s" e.mnemonic e.name (t e.target)

let case_of e : Isa_generated_case.case =
  {
    case_id = case_id e;
    target = e.target;
    form_id = "negative:" ^ e.mnemonic;
    source_record_ids = [];
    rule_ids =
      ([
         "negative";
         "category:" ^ e.category;
         Isa_generated_corpus.expect_code_prefix ^ e.expected_code;
       ]
      @
      match e.ours_features with
      | Some spec -> [ Isa_gen_ours.features_rule_prefix ^ spec ]
      | None -> []);
    operands = [];
    rendered_source = Isa_gen_render.render_source_lines e.lines;
    configuration = e.gas_configuration;
    negative = true;
  }

let dummy_encoding = Isa_norm_model.Riscv_encoding { width_bits = 32; mask = "0x0"; value = "0x0" }
