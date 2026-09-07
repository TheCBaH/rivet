open Isa_norm_model
module R = Isa_source_record

let err rule message : (_, diagnostic) result = Error { rule; message }

(* rv64_i/rv64_m name the RV64-only (XLEN=64) siblings of the base/M
   extensions - riscv32.jsonl and riscv64.jsonl are already separate
   per-profile exports, but a form's own requirement should still say so
   (an XLEN predicate) rather than assert unconditional availability,
   since a later multi-profile consumer cannot otherwise tell an
   *w mnemonic apart from a same-shaped RV32-and-RV64 one. *)
let feature_of_extension = function
  | "rv_i" -> Req_all []
  | "rv_m" -> Req_feature "riscv:m"
  | "rv_c" -> Req_feature "riscv:c"
  | "rv_zba" -> Req_feature "riscv:zba"
  | "rv_f" -> Req_feature "riscv:f"
  | "rv64_i" -> Req_xlen 64
  | "rv64_m" -> Req_all [ Req_xlen 64; Req_feature "riscv:m" ]
  | ext -> Req_unknown (Printf.sprintf "unmapped riscv-opcodes extension: %s" ext)

let riscv_encoding_of (rec_ : R.t) =
  match rec_.encoding with
  | R.Fixed_bits { width_bits; mask; value; _ } -> Ok (Riscv_encoding { width_bits; mask; value })
  | _ -> Error "record's encoding is not riscv-opcodes fixed_bits"

let riscv_provenance_of (rec_ : R.t) =
  match rec_.provenance with
  | R.Riscv_provenance p -> Ok p
  | _ -> Error "record's provenance is not a riscv-opcodes provenance"

let requirement_of (rec_ : R.t) =
  match riscv_provenance_of rec_ with
  | Ok { extension = Some ext; _ } -> feature_of_extension ext
  | Ok { extension = None; _ } -> Req_unknown "riscv-opcodes record has no provenance.extension"
  | Error msg -> Req_unknown msg

let gpr ?(excluded = []) () = Register { class_ = Riscv_gpr; excluded }
let fpr () = Register { class_ = Riscv_fpr; excluded = [] }

(* imm[hi:lo] <- one raw field's bits verbatim, high-to-low: sw's imm12hi
   (source bits 11:5) then imm12lo (bits 4:0). This is plain concatenation,
   the simplest case where "bit range names alone do not describe
   the permutation of immediate bits." *)
let sw_form (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err "sw-not-fixed-bits" msg
  | Ok encoding ->
      let value = { op_name = "value"; op_kind = gpr (); role = In; explicit = true } in
      let base = { op_name = "base"; op_kind = gpr (); role = In; explicit = true } in
      let offset =
        {
          op_name = "offset";
          op_kind =
            Immediate
              {
                width_bits = 12;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [
                    {
                      field_name = "imm12hi";
                      field_hi = 6;
                      field_lo = 0;
                      dest_hi = 11;
                      dest_lo = 5;
                    };
                    { field_name = "imm12lo"; field_hi = 4; field_lo = 0; dest_hi = 4; dest_lo = 0 };
                  ];
              };
          role = In;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "riscv:sw";
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ value; base; offset ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic = "sw";
              operands =
                [
                  Syn_operand "value";
                  Syn_group
                    [ Syn_operand "offset"; Syn_literal "("; Syn_operand "base"; Syn_literal ")" ];
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields imm12hi, rs1, rs2, imm12lo taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "offset is a signed 12-bit S-type immediate, imm12hi:imm12lo (bits 11:5 then \
                   4:0); this permutation is the RISC-V ISA manual's S-type layout, not stated by \
                   the source record itself";
              };
            ];
          diagnostics = [];
        }

(* beq's B-type immediate is a genuine bit permutation, not concatenation:
   bimm12hi (source bits 31:25) holds imm[12] at its own top bit then
   imm[10:5]; bimm12lo (source bits 11:7) holds imm[4:1] then imm[11] at its
   own bottom bit. Bit 0 of the 13-bit offset is never stored (branch targets
   are 2-byte aligned) - see implicit_low_zero_bits. *)
let beq_form (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err "beq-not-fixed-bits" msg
  | Ok encoding ->
      let lhs = { op_name = "lhs"; op_kind = gpr (); role = In; explicit = true } in
      let rhs = { op_name = "rhs"; op_kind = gpr (); role = In; explicit = true } in
      let offset =
        {
          op_name = "offset";
          op_kind =
            Immediate
              {
                width_bits = 13;
                signed = true;
                implicit_low_zero_bits = 1;
                nonzero = false;
                runs =
                  [
                    {
                      field_name = "bimm12hi";
                      field_hi = 6;
                      field_lo = 6;
                      dest_hi = 12;
                      dest_lo = 12;
                    };
                    {
                      field_name = "bimm12hi";
                      field_hi = 5;
                      field_lo = 0;
                      dest_hi = 10;
                      dest_lo = 5;
                    };
                    {
                      field_name = "bimm12lo";
                      field_hi = 4;
                      field_lo = 1;
                      dest_hi = 4;
                      dest_lo = 1;
                    };
                    {
                      field_name = "bimm12lo";
                      field_hi = 0;
                      field_lo = 0;
                      dest_hi = 11;
                      dest_lo = 11;
                    };
                  ];
              };
          role = In;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "riscv:beq";
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ lhs; rhs; offset ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic = "beq";
              operands = [ Syn_operand "lhs"; Syn_operand "rhs"; Syn_operand "offset" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields bimm12hi, rs1, rs2, bimm12lo taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "offset is a signed 13-bit B-type immediate assembled as imm[12|10:5|4:1|11] \
                   with an implicit zero low bit; this permutation is the RISC-V ISA manual's \
                   B-type layout, not stated by the source record itself";
              };
              {
                label = Inferred;
                note =
                  "GAS resolves offset from a label, applying the PC-relative/alignment policy - \
                   not modeled here";
              };
            ];
          diagnostics = [];
        }

(* c.addi's rd_rs1_n0 is tied (same register read and written) and excludes
   x0 (an encoding with rd_rs1=x0 is HINT/reserved, per the RISC-V ISA
   manual); its 6-bit immediate is split the same way as beq's high/low
   fields, and is architecturally required to be nonzero (a nzimm=0 encoding
   is a different, reserved instruction, per the same manual). Neither
   exclusion is stated in the raw fixed_bits record. *)
let c_addi_form (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err "c.addi-not-fixed-bits" msg
  | Ok encoding ->
      let acc =
        { op_name = "acc"; op_kind = gpr ~excluded:[ "x0" ] (); role = In_out; explicit = true }
      in
      let nzimm =
        {
          op_name = "nzimm";
          op_kind =
            Immediate
              {
                width_bits = 6;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = true;
                runs =
                  [
                    {
                      field_name = "c_nzimm6hi";
                      field_hi = 0;
                      field_lo = 0;
                      dest_hi = 5;
                      dest_lo = 5;
                    };
                    {
                      field_name = "c_nzimm6lo";
                      field_hi = 4;
                      field_lo = 0;
                      dest_hi = 4;
                      dest_lo = 0;
                    };
                  ];
              };
          role = In;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "riscv:c.addi";
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ acc; nzimm ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic = "c.addi";
              operands = [ Syn_operand "acc"; Syn_operand "nzimm" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields rd_rs1_n0, c_nzimm6lo, c_nzimm6hi taken verbatim from \
                   encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "acc excludes x0 and nzimm excludes 0: both are reserved/HINT encodings per the \
                   RISC-V ISA manual, not stated by the source record - see the '_n0' and 'nz' \
                   field-name conventions the record does carry";
              };
            ];
          diagnostics = [];
        }

(* Generic R-type integer register-register form (the RISC-V
   add/sub/mul pilot): riscv-opcodes' [rd, rs1, rs2] variable_fields shape is
   shared by every base-integer and M-extension register-register op, with a
   uniform GAS syntax "mnemonic rd, rs1, rs2" - unlike I-type mnemonics
   (below), where the same [rd, rs1, imm12] shape hides real syntax
   differences (e.g. loads and [jalr] are not "mnemonic rd, rs1, imm"), so
   this dispatch stays an explicit mnemonic allowlist, not a shape-driven
   catch-all: do not model all ISAs as a fixed opcode plus a
   list of numeric fields. *)
let r_type_gpr_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = gpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ rd; rs1; rs2 ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "rs1"; Syn_operand "rs2" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1, rs2 taken verbatim from encoding.fields";
              };
            ];
          diagnostics =
            (match requirement with
            | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
            | _ -> []);
        }

(* Generic I-type integer register-immediate form: riscv-opcodes' [rd, rs1,
   imm12] shape with GAS syntax "mnemonic rd, rs1, imm" - the arithmetic
   mnemonics below, not loads/jalr/shifts, which share the field name but not
   this syntax (see the comment on {!r_type_gpr_form}). *)
let i_type_imm_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let imm =
        {
          op_name = "imm";
          op_kind =
            Immediate
              {
                width_bits = 12;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [
                    { field_name = "imm12"; field_hi = 11; field_lo = 0; dest_hi = 11; dest_lo = 0 };
                  ];
              };
          role = In;
          explicit = true;
        }
      in
      let xlen_diagnostics =
        match requirement with
        | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
        | _ -> []
      in
      let sltiu_diagnostics =
        if mnemonic = "sltiu" then
          [
            {
              rule = "sltiu-imm-sign-vs-compare";
              message =
                "the imm12 field is decoded as a sign-extended 12-bit value like every other \
                 I-type immediate; sltiu's unsigned *comparison* semantics are not a separate \
                 operand encoding and are not modeled here";
            };
          ]
        else []
      in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ rd; rs1; imm ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "rs1"; Syn_operand "imm" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1, imm12 taken verbatim from encoding.fields";
              };
            ];
          diagnostics = xlen_diagnostics @ sltiu_diagnostics;
        }

let r_type_mnemonics =
  [
    "add";
    "sub";
    "and";
    "or";
    "xor";
    "sll";
    "srl";
    "sra";
    "slt";
    "sltu";
    "addw";
    "subw";
    "sllw";
    "srlw";
    "sraw";
    "mul";
    "mulh";
    "mulhsu";
    "mulhu";
    "div";
    "divu";
    "rem";
    "remu";
    "mulw";
    "divw";
    "divuw";
    "remw";
    "remuw";
    "sh1add";
  ]

let i_type_mnemonics = [ "addi"; "slti"; "sltiu"; "andi"; "ori"; "xori"; "addiw" ]

(* fadd.s's rd/rs1/rs2 are floating registers, not the rd/rs1/rs2 GPR class
   sw/beq/sh1add use - the vocabulary does not state that distinction (plan
   §3.3); this rule supplies it by mnemonic/extension. rm's GAS default
   (dynamic rounding when omitted) is not asserted as fact - see diagnostics. *)
let fadd_s_form (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err "fadd.s-not-fixed-bits" msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = fpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = fpr (); role = In; explicit = true } in
      let rm = { op_name = "rm"; op_kind = Rounding_mode; role = In; explicit = false } in
      Ok
        {
          form_id = "riscv:fadd.s";
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs1; rs2; rm ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic = "fadd.s";
              operands = [ Syn_operand "rd"; Syn_operand "rs1"; Syn_operand "rs2" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1, rs2, rm taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "rd/rs1/rs2 are floating-point registers (rv_f), not the GPR class rd/rs1/rs2 \
                   denote in integer forms";
              };
            ];
          diagnostics =
            [
              {
                rule = "fadd.s-rm-default-unverified";
                message =
                  "rm is left implicit/omitted in the syntax recipe on the assumption GAS defaults \
                   it to dynamic rounding; not confirmed against an installed assembler";
              };
            ];
        }

let normalize (rec_ : R.t) =
  match rec_.native_name with
  | "sw" -> sw_form rec_
  | "beq" -> beq_form rec_
  | "c.addi" -> c_addi_form rec_
  | "fadd.s" -> fadd_s_form rec_
  | mnemonic when List.mem mnemonic r_type_mnemonics -> r_type_gpr_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic i_type_mnemonics -> i_type_imm_form ~mnemonic rec_
  | other ->
      err "unhandled-native-name"
        (Printf.sprintf
           "Isa_norm_riscv only normalizes the frozen pilot mnemonics (sw, beq, c.addi, fadd.s) \
            plus the R-type/I-type integer allowlists; %s is not one of them"
           other)
