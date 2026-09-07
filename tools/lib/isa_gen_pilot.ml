type enumeration_method =
  | Dump_codec of { alternative_label : string; evidence : string }
  | Hand_read_table of { function_name : string; evidence : string }

type pilot_entry = {
  form_id : string;
  target : Target.t;
  lookup_key : string;
  implementation : enumeration_method;
}

let riscv_opcode_table_fn =
  "riscv_family_encode.ml's Opcode -> (opcode, funct3, funct7) / (opcode, funct3, funct_hi, shamt) \
   tables"

let riscv_entry ~mnemonic ~target ~evidence =
  {
    form_id = "riscv:" ^ mnemonic;
    target;
    lookup_key = mnemonic;
    implementation = Hand_read_table { function_name = riscv_opcode_table_fn; evidence };
  }

let riscv_pilot =
  [
    riscv_entry ~mnemonic:"add" ~target:Target.Riscv32 ~evidence:"Add -> Some (0x33, 0, 0x00)";
    riscv_entry ~mnemonic:"add" ~target:Target.Riscv64 ~evidence:"Add -> Some (0x33, 0, 0x00)";
    riscv_entry ~mnemonic:"sub" ~target:Target.Riscv32 ~evidence:"Sub -> Some (0x33, 0, 0x20)";
    riscv_entry ~mnemonic:"sub" ~target:Target.Riscv64 ~evidence:"Sub -> Some (0x33, 0, 0x20)";
    riscv_entry ~mnemonic:"mul" ~target:Target.Riscv32 ~evidence:"Mul -> Some (0x33, 0, 0x01)";
    riscv_entry ~mnemonic:"mul" ~target:Target.Riscv64 ~evidence:"Mul -> Some (0x33, 0, 0x01)";
    riscv_entry ~mnemonic:"addi" ~target:Target.Riscv32 ~evidence:"Addi -> Some (0x13, 0, 0, None)";
    riscv_entry ~mnemonic:"addi" ~target:Target.Riscv64 ~evidence:"Addi -> Some (0x13, 0, 0, None)";
    riscv_entry ~mnemonic:"addw" ~target:Target.Riscv64
      ~evidence:
        "RV64I-only mnemonic: no RV32 counterpart exists at all (absent from the riscv32 export); \
         its single-target presence in this manifest IS the XLEN restriction";
  ]

let x86_entry ~form_id ~iform ~target ~alternative_label ~evidence =
  {
    form_id = "x86:" ^ form_id;
    target;
    lookup_key = iform;
    implementation = Dump_codec { alternative_label; evidence };
  }

let x86_pilot =
  List.concat_map
    (fun target ->
      [
        x86_entry ~form_id:"ADD_GPRv_GPRv_01" ~iform:"ADD_GPRv_GPRv_01" ~target
          ~alternative_label:"alu-rm-r"
          ~evidence:
            "alu_rm_r_codec's entries include Add -> Opcode.to_rm_r Add = Some 0x01L \
             (x86_family_encode.ml)";
        x86_entry ~form_id:"ADD_GPRv_GPRv_03" ~iform:"ADD_GPRv_GPRv_03" ~target
          ~alternative_label:"alu-r-rm"
          ~evidence:
            "alu_r_rm_codec's entries include Add -> Opcode.to_r_rm Add = Some 0x03L \
             (x86_family_encode.ml)";
        x86_entry ~form_id:"ADD_GPRv_IMMz" ~iform:"ADD_GPRv_IMMz" ~target
          ~alternative_label:"alu-rm-imm8 / alu-rm-imm32"
          ~evidence:
            "Add -> 0 selects the /0 ModR/M-reg extension shared by alu-rm-imm8 (opcode 0x83) and \
             alu-rm-imm32 (opcode 0x81) (x86_family_encode.ml)";
        x86_entry ~form_id:"MOV_GPRv_GPRv_89" ~iform:"MOV_GPRv_GPRv_89" ~target
          ~alternative_label:"mov-rm-r"
          ~evidence:"mov-rm-r(){prefixes 10001001 modrm} - opcode 0x89 (--dump-codec)";
        x86_entry ~form_id:"MOV_GPRv_GPRv_8B" ~iform:"MOV_GPRv_GPRv_8B" ~target
          ~alternative_label:"mov-r-rm"
          ~evidence:"mov-r-rm(){prefixes 10001011 modrm} - opcode 0x8B (--dump-codec)";
        x86_entry ~form_id:"MOV_GPRv_IMMz" ~iform:"MOV_GPRv_IMMz" ~target
          ~alternative_label:"mov-r-imm"
          ~evidence:"mov-r-imm(){prefixes 10111 reg:3u imm-sym32} - opcode 0xB8+reg (--dump-codec)";
      ])
    [ Target.X86_32; Target.X86_64 ]

let all = riscv_pilot @ x86_pilot

type mandatory_obligation =
  | Negative_immediate_boundary
  | Riscv_xlen_restriction
  | X86_short_vs_full_immediate

let obligation_form_ids = function
  | Negative_immediate_boundary -> [ "riscv:addi"; "x86:ADD_GPRv_IMMz" ]
  | Riscv_xlen_restriction -> [ "riscv:addw" ]
  | X86_short_vs_full_immediate -> [ "x86:ADD_GPRv_IMMz" ]

let source_of_target = function
  | Target.Riscv32 | Target.Riscv64 -> "riscv_opcodes"
  | _ -> "xed_resolved"

let find_and_normalize_riscv records ~native_name =
  List.find_map
    (fun (r : Isa_source_record.t) ->
      if String.equal r.native_name native_name then
        match Isa_norm_riscv.normalize r with Ok form -> Some form | Error _ -> None
      else None)
    records

let find_and_normalize_xed records ~iform =
  List.find_map
    (fun (r : Isa_source_record.t) ->
      match r.provenance with
      | Isa_source_record.Xed_provenance { iform = Some i; _ } when String.equal i iform -> (
          match Isa_norm_xed.normalize r with Ok form -> Some form | Error _ -> None)
      | _ -> None)
    records

let normalize_entry repo (entry : pilot_entry) =
  let ( let* ) = Result.bind in
  let source = source_of_target entry.target in
  let path = Repo.isa_db_export repo ~source entry.target in
  let* records = Isa_source_record.read_file path in
  let found =
    match source with
    | "riscv_opcodes" -> find_and_normalize_riscv records ~native_name:entry.lookup_key
    | _ -> find_and_normalize_xed records ~iform:entry.lookup_key
  in
  match found with
  | Some form -> Ok form
  | None ->
      Err.fail ~pos:__POS__ ~pp_error:Tool_error.pp
        (Tool_error.v Tool_error.Validate
           (Printf.sprintf "no record in %s normalizes lookup_key %S to a form (pilot form_id %s)"
              (Fpath.to_string path) entry.lookup_key entry.form_id))
