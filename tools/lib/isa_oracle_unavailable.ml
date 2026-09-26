type t = {
  source : string;
  target : Target.t;
  extension : string;
  native_name : string option;
  reason : string;
  probe : string;
}

let template ~target ~native_name ~spellings =
  {
    source = "riscv_opcodes";
    target;
    extension = "rv_zimop";
    native_name = Some native_name;
    reason = "template-not-a-mnemonic";
    probe =
      Printf.sprintf
        "riscv64-linux-gnu-as 2.44, -march=rv64im_zimop: unrecognized opcode `%s'; the template's \
         concrete spellings %s are each their own promoted record"
        (String.lowercase_ascii native_name)
        spellings;
  }

let gas_lacks ~target ~extension ~reason ~probe =
  { source = "riscv_opcodes"; target; extension; native_name = None; reason; probe }

let both f = List.map f [ Target.Riscv32; Target.Riscv64 ]

let all =
  both (fun target ->
      gas_lacks ~target ~extension:"rv_zalasr" ~reason:"gas-lacks-zalasr"
        ~probe:
          "riscv32-linux-gnu-as 2.43.1 and riscv64-linux-gnu-as 2.44: unknown extension `zalasr'")
  @ both (fun target ->
      gas_lacks ~target ~extension:"rv_smrnmi" ~reason:"gas-lacks-smrnmi"
        ~probe:
          "riscv64-linux-gnu-as 2.44, -march=rv64im_smrnmi: unrecognized opcode `mnret'; \
           riscv32-linux-gnu-as 2.43.1: unknown prefixed ISA extension `smrnmi'")
  @ [
      gas_lacks ~target:Target.Riscv32 ~extension:"rv_ssctr" ~reason:"gas-lacks-ssctr"
        ~probe:
          "riscv32-linux-gnu-as 2.43.1, -march=rv32im_ssctr: unknown prefixed ISA extension \
           `ssctr' (riscv64-linux-gnu-as 2.44 accepts sctrclr)";
      gas_lacks ~target:Target.Riscv32 ~extension:"rv_zcmt" ~reason:"gas-lacks-zcmt"
        ~probe:
          "riscv32-linux-gnu-as 2.43.1, -march=rv32imc_zcmt: unknown prefixed ISA extension `zcmt' \
           (riscv64-linux-gnu-as 2.44 accepts cm.jalt)";
      {
        (gas_lacks ~target:Target.Riscv32 ~extension:"rv_zcmp" ~reason:"gas-lacks-cm-mv"
           ~probe:
             "riscv32-linux-gnu-as 2.43.1, -march=rv32imc_zcmp: unrecognized opcode `cm.mva01s \
              s0,s7' (cm.push/cm.pop are accepted; riscv64-linux-gnu-as 2.44 accepts both)")
        with
        native_name = Some "cm.mva01s";
      };
      {
        (gas_lacks ~target:Target.Riscv32 ~extension:"rv_zcmp" ~reason:"gas-lacks-cm-mv"
           ~probe:
             "riscv32-linux-gnu-as 2.43.1, -march=rv32imc_zcmp: unrecognized opcode `cm.mvsa01 \
              s2,s1' (riscv64-linux-gnu-as 2.44 accepts it)")
        with
        native_name = Some "cm.mvsa01";
      };
      gas_lacks ~target:Target.Riscv32 ~extension:"rv32_zclsd" ~reason:"gas-lacks-zclsd"
        ~probe:
          "riscv32-linux-gnu-as 2.43.1, -march=rv32imc_zclsd: unknown prefixed ISA extension \
           `zclsd' (riscv64-linux-gnu-as 2.44 does not know it either)";
      gas_lacks ~target:Target.Riscv32 ~extension:"rv_c_zicfiss" ~reason:"gas-lacks-zicfiss"
        ~probe:
          "riscv32-linux-gnu-as 2.43.1, -march=rv32imc_zicfiss_zcmop: unknown prefixed ISA \
           extension `zicfiss'";
      gas_lacks ~target:Target.Riscv32 ~extension:"rv32_zilsd" ~reason:"gas-lacks-zilsd"
        ~probe:
          "riscv32-linux-gnu-as 2.43.1, -march=rv32im_zilsd: unknown prefixed ISA extension `zilsd'";
      gas_lacks ~target:Target.Riscv32 ~extension:"rv_zicfilp" ~reason:"gas-lacks-zicfilp"
        ~probe:
          "riscv32-linux-gnu-as 2.43.1, -march=rv32im_zicfilp: unknown prefixed ISA extension \
           `zicfilp' (riscv64-linux-gnu-as 2.44 accepts lpad)";
      {
        source = "riscv_opcodes";
        target = Target.Riscv64;
        extension = "rv_zicfiss";
        native_name = Some "ssamoswap.w";
        reason = "gas-lacks-ssamoswap";
        probe =
          "riscv64-linux-gnu-as 2.44, -march=rv64ima_zicfiss: unrecognized opcode `ssamoswap.w \
           a0,a2,(a1)'";
      };
      {
        source = "riscv_opcodes";
        target = Target.Riscv64;
        extension = "rv_zicfiss";
        native_name = Some "ssamoswap.d";
        reason = "gas-lacks-ssamoswap";
        probe =
          "riscv64-linux-gnu-as 2.44, -march=rv64ima_zicfiss: unrecognized opcode `ssamoswap.d \
           a0,a2,(a1)'";
      };
    ]
  @ [
      {
        source = "riscv_opcodes";
        target = Target.Riscv32;
        extension = "rv_zicfiss";
        native_name = None;
        reason = "gas-lacks-zicfiss";
        probe =
          "riscv32-linux-gnu-as 2.43.1 (crosstool-NG 1.27.0), -march=rv32im_zimop_zicfiss: unknown \
           prefixed ISA extension `zicfiss' (riscv64-linux-gnu-as 2.44 accepts it)";
      };
    ]
  (* XED's ACE_1 tile/zmm operations: GNU as 2.44 has none of them (tilemovcol, the top ops) and
     only the zmm-destination tilemovrow *)
  @ List.map
      (fun target ->
        {
          source = "xed_resolved";
          target;
          extension = "ACE_1";
          native_name = None;
          reason = "gas-lacks-ace";
          probe =
            "x86_64-linux-gnu-as 2.44: no such instruction `tilemovcol'/`top2bf16ps'; `tilemovrow \
             %ebx,%zmm2,%tmm1': operand size mismatch";
        })
      [ Target.X86_32; Target.X86_64 ]
  @ List.concat_map
      (fun target ->
        [
          template ~target ~native_name:"mop.r.N" ~spellings:"mop.r.0..mop.r.31";
          template ~target ~native_name:"mop.rr.N" ~spellings:"mop.rr.0..mop.rr.7";
          {
            (template ~target ~native_name:"c.mop.N" ~spellings:"c.mop.1..c.mop.15") with
            extension = "rv_zcmop";
            probe =
              "riscv64-linux-gnu-as 2.44, -march=rv64imc_zcmop: unrecognized opcode `c.mop.n'; the \
               template's concrete spellings c.mop.1..c.mop.15 are each their own promoted record";
          };
        ])
      [ Target.Riscv32; Target.Riscv64 ]

(* XED iclasses (lower case: the AT&T spelling) that x86_64-linux-gnu-as and
   i686-linux-gnu-as 2.44 do not know: "no such instruction" for every spelling the
   isa-difficult generator tried. *)
let x86_gas_lacks =
  [
    (* undocumented one-byte opcodes GNU as has no mnemonic for *)
    "udb";
    "salc";
    "fstpnce";
    "vcvtbf42hf8";
    "vcvtbf62hf8";
    "vcvtbf82bf4s";
    "vcvtbf82bf6s";
    "vcvtbf82ps";
    "vcvtbiasps2bf8";
    "vcvtbiasps2bf8s";
    "vcvtbiasps2hf8";
    "vcvtbiasps2hf8s";
    "vcvthf62hf8";
    "vcvthf82bf4s";
    "vcvthf82hf6s";
    "vcvthf82ps";
    "vcvtps2bf8";
    "vcvtps2bf8s";
    "vcvtps2hf8";
    "vcvtps2hf8s";
    "vcvtrops2hf8";
    "vcvtrops2hf8s";
    "vpmovssdb";
    "vunpackb";
  ]

let x86_lacking ~source target ~extension ~native_name =
  if source = "xed_resolved" && List.mem (String.lowercase_ascii native_name) x86_gas_lacks then
    Some
      {
        source;
        target;
        extension;
        native_name = Some native_name;
        reason = "gas-lacks-instruction";
        probe =
          Printf.sprintf "x86_64-linux-gnu-as / i686-linux-gnu-as 2.44: no such instruction `%s'"
            (String.lowercase_ascii native_name);
      }
  else None

let find ~source target ~extension =
  List.find_opt
    (fun u ->
      u.source = source && u.target = target && u.extension = extension && u.native_name = None)
    all

let find_record ~source target ~extension ~native_name =
  match x86_lacking ~source target ~extension ~native_name with
  | Some _ as u -> u
  | None ->
      List.find_opt
        (fun u ->
          u.source = source && u.target = target && u.extension = extension
          && u.native_name = Some native_name)
        all
