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
  | "rv_zbb" -> Req_feature "riscv:zbb"
  | "rv_zbkb" -> Req_feature "riscv:zbkb"
  | "rv_zk" -> Req_feature "riscv:zk"
  | "rv_zkn" -> Req_feature "riscv:zkn"
  | "rv_zks" -> Req_feature "riscv:zks"
  | "rv_zbc" -> Req_feature "riscv:zbc"
  | "rv_zbkc" -> Req_feature "riscv:zbkc"
  | "rv_zbkx" -> Req_feature "riscv:zbkx"
  | "rv_f" -> Req_feature "riscv:f"
  | "rv64_i" -> Req_xlen 64
  | "rv64_m" -> Req_all [ Req_xlen 64; Req_feature "riscv:m" ]
  | "rv64_zba" -> Req_all [ Req_xlen 64; Req_feature "riscv:zba" ]
  | "rv64_zbb" -> Req_all [ Req_xlen 64; Req_feature "riscv:zbb" ]
  | "rv64_zbkb" -> Req_all [ Req_xlen 64; Req_feature "riscv:zbkb" ]
  | "rv64_zk" -> Req_all [ Req_xlen 64; Req_feature "riscv:zk" ]
  | "rv64_zkn" -> Req_all [ Req_xlen 64; Req_feature "riscv:zkn" ]
  | "rv64_zks" -> Req_all [ Req_xlen 64; Req_feature "riscv:zks" ]
  | "rv32_zbb" -> Req_all [ Req_xlen 32; Req_feature "riscv:zbb" ]
  | "rv32_zbkb" -> Req_all [ Req_xlen 32; Req_feature "riscv:zbkb" ]
  | "rv32_zk" -> Req_all [ Req_xlen 32; Req_feature "riscv:zk" ]
  | "rv32_zkn" -> Req_all [ Req_xlen 32; Req_feature "riscv:zkn" ]
  | "rv32_zks" -> Req_all [ Req_xlen 32; Req_feature "riscv:zks" ]
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

(* The Zbb "alternative extension" blocker: riscv-opcodes exports andn/
   orn/xnor/rol/ror once per importing extension file (rv_zbb, the primary
   instruction-form record, plus rv_zbkb/rv_zk/rv_zkn/rv_zks, each a
   $import-kind record whose own relationships/relationship-resolution point
   back at the rv_zbb record - hand-verified identical mask/value across all
   five in both checked-in profiles). [requirement_of] alone cannot express
   "any of these" from one record, since a record only ever names its own
   extension (and, for an import record, the one extension it imports from -
   not its other sibling importers). This table is therefore a hand-verified
   fact about the pinned snapshot, established the same "grep every candidate
   family's extension membership before choosing" way every other
   sub-slice was, not a general cross-record join: adding a sixth import site
   upstream would need this list extended, not inferred. *)
let alternative_extensions_by_mnemonic =
  let zbb_import_group = [ "rv_zbb"; "rv_zbkb"; "rv_zk"; "rv_zkn"; "rv_zks" ] in
  (* brev8 (within-byte bit-reverse) is a separate, four-way import group:
     riscv-opcodes' primary record is rv_zbkb (not rv_zbb), imported by
     rv_zk/rv_zkn/rv_zks - the same mnemonic and encoding on BOTH profiles
     (unlike rev8/rev8.rv32, brev8's own mnemonic does not change with XLEN),
     hand-verified identical mask/value across all four in both checked-in
     profiles. *)
  let zbkb_import_group = [ "rv_zbkb"; "rv_zk"; "rv_zkn"; "rv_zks" ] in
  (* rev8 (byte-reverse) needs a fifth, XLEN-specific pair of five-way import
     groups: riscv-opcodes exports it as native_name "rev8" in riscv64.jsonl
     (rv64_zbb primary, imported by rv64_zbkb/rv64_zk/rv64_zkn/rv64_zks) and
     as native_name "rev8.rv32" in riscv32.jsonl (rv32_zbkb primary, imported
     by rv32_zbb/rv32_zk/rv32_zkn/rv32_zks) - two disjoint, profile-specific
     extension-name sets, hand-verified identical mask/value across all five
     records in each profile. These two keys are looked up by native_name
     (see {!unary_gpr_form}'s [extension_lookup_key]), not by the shared
     rendered mnemonic "rev8" both profiles normalize to - real GNU as
     rejects riscv-opcodes' own "rev8.rv32" disambiguation label as an
     unrecognized opcode (confirmed), so the two profiles' records must
     resolve to two different alternative-extension lists despite rendering
     identically. *)
  let rev8_import_group_rv64 = [ "rv64_zbb"; "rv64_zbkb"; "rv64_zk"; "rv64_zkn"; "rv64_zks" ] in
  let rev8_import_group_rv32 = [ "rv32_zbkb"; "rv32_zbb"; "rv32_zk"; "rv32_zkn"; "rv32_zks" ] in
  (* packw (RV64-only pack.w sibling) and zip/unzip (RV32-only bit
     interleave/de-interleave) are each a four-way Req_any over their own
     XLEN-prefixed extension names - packw's rv64_zbkb primary imported by
     rv64_zk/zkn/zks, zip/unzip's rv32_zbkb primary imported by
     rv32_zk/zkn/zks - hand-verified identical mask/value across all four
     records in their one profile before writing any code. *)
  let packw_import_group_rv64 = [ "rv64_zbkb"; "rv64_zk"; "rv64_zkn"; "rv64_zks" ] in
  let zip_import_group_rv32 = [ "rv32_zbkb"; "rv32_zk"; "rv32_zkn"; "rv32_zks" ] in
  (* rolw/rorw (RV64-only word-operand siblings of rol/ror, no RV32 record at
     all) are a fifth entry that happens to share rev8's RV64 five-way group
     verbatim: primary rv64_zbb, imported by rv64_zbkb/rv64_zk/rv64_zkn/
     rv64_zks, hand-verified identical mask/value across all five records in
     riscv64.jsonl before writing any code. *)
  (* clmul/clmulh (carry-less multiply) are a fifth, XLEN-independent import
     group like andn/orn/xnor/rol/ror's own zbb_import_group above, but
     rooted in Zbc rather than Zbb: riscv-opcodes' primary record is rv_zbc,
     imported by rv_zbkc/rv_zk/rv_zkn/rv_zks - identical mnemonic and
     encoding on both profiles (no rev8-style native_name split), hand-
     verified identical mask/value across all five records in both checked-in
     profiles. *)
  let zbc_import_group = [ "rv_zbc"; "rv_zbkc"; "rv_zk"; "rv_zkn"; "rv_zks" ] in
  (* xperm4/xperm8 (crossbar permute) are a four-way import group like
     pack/packh's own zbkb_import_group above, but rooted in Zbkx: primary
     rv_zbkx, imported by rv_zk/rv_zkn/rv_zks (no separate non-K sibling
     extension the way clmul/clmulh have rv_zbc alongside rv_zbkc) -
     identical mnemonic and encoding on both profiles, hand-verified
     identical mask/value across all four records in both checked-in
     profiles. *)
  let zbkx_import_group = [ "rv_zbkx"; "rv_zk"; "rv_zkn"; "rv_zks" ] in
  [
    ("andn", zbb_import_group);
    ("orn", zbb_import_group);
    ("xnor", zbb_import_group);
    ("rol", zbb_import_group);
    ("ror", zbb_import_group);
    ("brev8", zbkb_import_group);
    ("rev8", rev8_import_group_rv64);
    ("rev8.rv32", rev8_import_group_rv32);
    ("pack", zbkb_import_group);
    ("packh", zbkb_import_group);
    ("packw", packw_import_group_rv64);
    ("zip", zip_import_group_rv32);
    ("unzip", zip_import_group_rv32);
    ("rolw", rev8_import_group_rv64);
    ("rorw", rev8_import_group_rv64);
    ("rori", rev8_import_group_rv64);
    ("rori.rv32", rev8_import_group_rv32);
    ("roriw", rev8_import_group_rv64);
    ("clmul", zbc_import_group);
    ("clmulh", zbc_import_group);
    ("xperm4", zbkx_import_group);
    ("xperm8", zbkx_import_group);
  ]

(* Builds [Req_any] over every extension in [extensions] once [rec_]'s own
   extension is confirmed to actually be a member - guarding against silently
   misclassifying an unrelated record that happens to share the mnemonic. *)
let requirement_of_any ~extensions (rec_ : R.t) =
  match riscv_provenance_of rec_ with
  | Ok { extension = Some ext; _ } when List.mem ext extensions ->
      Req_any (List.map feature_of_extension extensions)
  | Ok { extension = Some ext; _ } ->
      Req_unknown
        (Printf.sprintf
           "record's own extension %s is not one of the hand-verified alternatives [%s]" ext
           (String.concat ", " extensions))
  | Ok { extension = None; _ } -> Req_unknown "riscv-opcodes record has no provenance.extension"
  | Error msg -> Req_unknown msg

let requirement_of_mnemonic ~mnemonic (rec_ : R.t) =
  match List.assoc_opt mnemonic alternative_extensions_by_mnemonic with
  | Some extensions -> requirement_of_any ~extensions rec_
  | None -> requirement_of rec_

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
      let requirement = requirement_of_mnemonic ~mnemonic rec_ in
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

(* Generic OP-IMM/OP-IMM-32 "pseudo-unary" form: riscv-opcodes' [rd, rs1]
   variable_fields shape with a fully fixed encoding.fields immediate
   (funct12) - Zbb's population-count/sign-extend/byte family (clz, ctz,
   cpop, sext.b, sext.h, orc.b and their RV64-only *w siblings), distinct
   from {!i_type_imm_form}'s genuine, syntax-visible immediate operand: here
   the fixed bits select the mnemonic, and GAS syntax is "mnemonic rd, rs1"
   with no immediate written.

   [extension_lookup_key] defaults to [mnemonic] (every other unary form),
   but rev8/rev8.rv32 need it decoupled: riscv-opcodes' own native_name
   "rev8.rv32" (RV32) must still RENDER as ["rev8"] (the only spelling real
   GNU as accepts on either profile - it rejects "rev8.rv32" outright), while
   its {!alternative_extensions_by_mnemonic} lookup must use the RV32-specific
   five-way group, not the RV64 one keyed by the rendered mnemonic "rev8". *)
let unary_gpr_form ?extension_lookup_key ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement =
        requirement_of_mnemonic ~mnemonic:(Option.value extension_lookup_key ~default:mnemonic) rec_
      in
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ rd; rs1 ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "rs1" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "the remaining encoding bits are a fully fixed funct12 selecting this mnemonic, \
                   not a genuine immediate operand - GAS syntax is \"mnemonic rd, rs1\" with no \
                   immediate written";
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

(* Generic OP-IMM/OP-IMM-32 rotate-immediate form: riscv-opcodes' [rd, rs1,
   shamtd/shamtw] shape with GAS syntax "mnemonic rd, rs1, shamt" - a genuine
   syntax-visible immediate like {!i_type_imm_form}'s, but UNSIGNED and only
   [width] bits wide (5 on RV32/the *w siblings, 6 on RV64's own [rori]),
   unlike that form's fixed signed 12-bit imm12. This is Zbb's first
   shift-amount shape: [rori] (RV64's own "shamtd" field) and [rori.rv32]
   (riscv-opcodes' pseudo-op alias for RV32, "shamtw") share one canonical
   rendered mnemonic ["rori"] and [form_id] the same way {!unary_gpr_form}'s
   rev8/rev8.rv32 dispatch does - see {!extension_lookup_key} there for why
   a separate lookup key is threaded through instead of [mnemonic] itself.
   [roriw] is the plain RV64-only *w sibling, needing no such split. *)
let shamt_gpr_form ?extension_lookup_key ~mnemonic ~width (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement =
        requirement_of_mnemonic ~mnemonic:(Option.value extension_lookup_key ~default:mnemonic) rec_
      in
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let field_name = if width = 6 then "shamtd" else "shamtw" in
      let shamt =
        {
          op_name = "shamt";
          op_kind =
            Immediate
              {
                width_bits = width;
                signed = false;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [
                    {
                      field_name;
                      field_hi = width - 1;
                      field_lo = 0;
                      dest_hi = width - 1;
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
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ rd; rs1; shamt ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "rs1"; Syn_operand "shamt" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf "operand fields rd, rs1, %s taken verbatim from encoding.fields"
                    field_name;
              };
            ];
          diagnostics =
            (match requirement with
            | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
            | _ -> []);
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
    "sh2add";
    "sh3add";
    "sh1add.uw";
    "sh2add.uw";
    "sh3add.uw";
    "min";
    "minu";
    "max";
    "maxu";
    "andn";
    "orn";
    "xnor";
    "rol";
    "ror";
    "pack";
    "packh";
    "packw";
    "rolw";
    "rorw";
    "clmul";
    "clmulh";
    "xperm4";
    "xperm8";
  ]

let i_type_mnemonics = [ "addi"; "slti"; "sltiu"; "andi"; "ori"; "xori"; "addiw" ]

(* Zbb's population-count, sign-extend and byte-processing family: a fixed
   funct12 selects the mnemonic (see {!unary_gpr_form}); clz/ctz/cpop/
   sext.b/sext.h/orc.b are XLEN-independent (single rv_zbb record exported
   on both profiles), clzw/ctzw/cpopw are RV64-only word-operand siblings
   (rv64_zbb, no RV32 counterpart at all). brev8 is the same unary shape with
   a four-way Req_any (rv_zbkb/rv_zk/rv_zkn/rv_zks, {!alternative_extensions_by_mnemonic}),
   the same mnemonic and encoding on both profiles. rev8 (below, dispatched
   separately since its native_name differs by profile) is the last member of
   this family. *)
let unary_gpr_mnemonics =
  [
    "clz";
    "ctz";
    "cpop";
    "sext.b";
    "sext.h";
    "orc.b";
    "clzw";
    "ctzw";
    "cpopw";
    "brev8";
    "zip";
    "unzip";
  ]

(* The scalar floating-point arithmetic forms' rd/rs1/rs2 are floating
   registers, not the GPR class those field names denote in integer forms.
   GNU as was measured on both supported profiles to encode omitted rm as dyn
   (funct3=7), so this bounded bare-mnemonic recipe does not imply support for
   explicit rounding-mode syntax. *)
let f_arith_mnemonics =
  [ "fadd.s"; "fsub.s"; "fmul.s"; "fdiv.s"; "fadd.d"; "fsub.d"; "fmul.d"; "fdiv.d" ]

let f_arith_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = fpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = fpr (); role = In; explicit = true } in
      let rm = { op_name = "rm"; op_kind = Rounding_mode; role = In; explicit = false } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs1; rs2; rm ];
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
                note = "operand fields rd, rs1, rs2, rm taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "rd/rs1/rs2 are floating-point registers (rv_f), not the GPR class rd/rs1/rs2 \
                   denote in integer forms";
              };
              {
                label = Inferred;
                note =
                  "the omitted rm operand is GNU as's dynamic-rounding default (funct3=7), \
                   measured for this bare scalar arithmetic spelling on rv32imf and rv64imf";
              };
            ];
          diagnostics = [];
        }

let normalize (rec_ : R.t) =
  match rec_.native_name with
  | "sw" -> sw_form rec_
  | "beq" -> beq_form rec_
  | "c.addi" -> c_addi_form rec_
  | mnemonic when List.mem mnemonic f_arith_mnemonics -> f_arith_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic r_type_mnemonics -> r_type_gpr_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic i_type_mnemonics -> i_type_imm_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic unary_gpr_mnemonics -> unary_gpr_form ~mnemonic rec_
  (* rev8 (byte-reverse): riscv64.jsonl's own native_name is already "rev8",
     dispatched like any other {!unary_gpr_mnemonics} entry above would be,
     but riscv32.jsonl's native_name is riscv-opcodes' internal
     disambiguation label "rev8.rv32" - real GNU as accepts only the bare
     ["rev8"] spelling on that profile too (confirmed: it rejects
     "rev8.rv32" as an unrecognized opcode), so this dispatches to the same
     rendered mnemonic while keying {!alternative_extensions_by_mnemonic}'s
     lookup on the RV32-specific "rev8.rv32" entry, not "rev8"'s RV64 one. *)
  | "rev8" -> unary_gpr_form ~mnemonic:"rev8" rec_
  | "rev8.rv32" -> unary_gpr_form ~mnemonic:"rev8" ~extension_lookup_key:"rev8.rv32" rec_
  (* rori/rori.rv32: the same profile-specific native_name split as
     rev8/rev8.rv32 above, applied to {!shamt_gpr_form} instead of
     {!unary_gpr_form} - riscv64.jsonl's "rori" (6-bit shamtd) and
     riscv32.jsonl's pseudo-op "rori.rv32" (5-bit shamtw) both render as the
     bare ["rori"] spelling real GNU as accepts on either profile (confirmed:
     `ror a0, a1, 5` assembles as "rori" on both RV32 and RV64 2.44/2.43.1). *)
  | "rori" -> shamt_gpr_form ~mnemonic:"rori" ~width:6 rec_
  | "rori.rv32" -> shamt_gpr_form ~mnemonic:"rori" ~width:5 ~extension_lookup_key:"rori.rv32" rec_
  | "roriw" -> shamt_gpr_form ~mnemonic:"roriw" ~width:5 rec_
  (* zext.h/zext.h.rv32: the same profile-specific native_name split as
     rev8/rev8.rv32 above, but with no import duplication to disambiguate -
     each profile's record names only its own single extension
     (rv64_zbb/rv32_zbb), so {!requirement_of}'s plain per-record lookup
     already gives the right answer with no {!alternative_extensions_by_mnemonic}
     entry needed. Both render as the bare ["zext.h"] spelling real GNU as
     accepts on either profile (confirmed: `zext.h a0, a1` assembles on both
     RV32 2.43.1 and RV64 2.44; "zext.h.rv32" is rejected as an unrecognized
     opcode on both). The record's own [rd, rs1] variable_fields shape is
     identical in category to rev8's - {!unary_gpr_form} applies unchanged,
     even though the fixed bits it treats as "a funct12" are really an R-type
     opcode/funct3/funct7 with [rs2] hardwired to x0, not an OP-IMM funct12;
     the normalized model does not distinguish the two, only the encoder
     does (riscv_family_encode.ml's own dedicated two-operand match arm). *)
  | "zext.h" -> unary_gpr_form ~mnemonic:"zext.h" rec_
  | "zext.h.rv32" -> unary_gpr_form ~mnemonic:"zext.h" rec_
  | other ->
      err "unhandled-native-name"
        (Printf.sprintf
           "Isa_norm_riscv only normalizes the frozen pilot mnemonics (sw, beq, c.addi, \
            fadd.s/fsub.s/fmul.s/fdiv.s/fadd.d/fsub.d/fmul.d/fdiv.d) plus the R-type/I-type \
            integer allowlists; %s is not one of them"
           other)
