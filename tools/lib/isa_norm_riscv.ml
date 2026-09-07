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
  | "rv_zknh" -> Req_feature "riscv:zknh"
  | "rv_zicsr" -> Req_feature "riscv:zicsr"
  | "rv_f" -> Req_feature "riscv:f"
  | "rv_d" -> Req_feature "riscv:d"
  | "rv_a" -> Req_feature "riscv:a"
  | "rv64_a" -> Req_all [ Req_xlen 64; Req_feature "riscv:a" ]
  | "rv64_f" -> Req_all [ Req_xlen 64; Req_feature "riscv:f" ]
  | "rv64_d" -> Req_all [ Req_xlen 64; Req_feature "riscv:d" ]
  | "rv64_i" -> Req_xlen 64
  | "rv64_m" -> Req_all [ Req_xlen 64; Req_feature "riscv:m" ]
  | "rv64_zba" -> Req_all [ Req_xlen 64; Req_feature "riscv:zba" ]
  | "rv64_zbb" -> Req_all [ Req_xlen 64; Req_feature "riscv:zbb" ]
  | "rv64_zbkb" -> Req_all [ Req_xlen 64; Req_feature "riscv:zbkb" ]
  | "rv64_zk" -> Req_all [ Req_xlen 64; Req_feature "riscv:zk" ]
  | "rv64_zkn" -> Req_all [ Req_xlen 64; Req_feature "riscv:zkn" ]
  | "rv64_zks" -> Req_all [ Req_xlen 64; Req_feature "riscv:zks" ]
  | "rv64_zknh" -> Req_all [ Req_xlen 64; Req_feature "riscv:zknh" ]
  | "rv64_zknd" -> Req_all [ Req_xlen 64; Req_feature "riscv:zknd" ]
  | "rv64_zkne" -> Req_all [ Req_xlen 64; Req_feature "riscv:zkne" ]
  | "rv32_zknd" -> Req_all [ Req_xlen 32; Req_feature "riscv:zknd" ]
  | "rv32_zkne" -> Req_all [ Req_xlen 32; Req_feature "riscv:zkne" ]
  | "rv32_zbb" -> Req_all [ Req_xlen 32; Req_feature "riscv:zbb" ]
  | "rv32_zbkb" -> Req_all [ Req_xlen 32; Req_feature "riscv:zbkb" ]
  | "rv32_zk" -> Req_all [ Req_xlen 32; Req_feature "riscv:zk" ]
  | "rv32_zkn" -> Req_all [ Req_xlen 32; Req_feature "riscv:zkn" ]
  | "rv32_zks" -> Req_all [ Req_xlen 32; Req_feature "riscv:zks" ]
  | "rv32_zknh" -> Req_all [ Req_xlen 32; Req_feature "riscv:zknh" ]
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
  (* sha256sum0/sha256sum1/sha256sig0/sha256sig1 (Zknh's SHA-256
     message-schedule helpers) are a three-way import group - primary
     rv_zknh, imported by rv_zk/rv_zkn (no rv_zks: SHA-256 belongs to the
     "NIST" crypto profile Zkn groups, not the "ShangMi" Zks one) - the same
     two-GPR unary shape {!unary_gpr_mnemonics} already dispatches through
     (see {!Isa_norm_riscv.normalize}'s dispatch list below), identical
     mnemonic/encoding on both profiles, hand-verified identical mask/value
     across all three records in both checked-in profiles. *)
  let zknh_import_group = [ "rv_zknh"; "rv_zk"; "rv_zkn" ] in
  (* sha512sum0/sha512sum1/sha512sig0/sha512sig1 are the RV64-only siblings
     of the sha256 group above - same three-way import structure, but
     XLEN-prefixed (primary rv64_zknh, imported by rv64_zk/rv64_zkn; no
     RV32 record at all - riscv-opcodes' own RV32 answer is a genuinely
     different 32-bit-word-pair-split family, sha512sig0h/l etc., out of
     this slice's scope). *)
  let zknh_import_group_rv64 = [ "rv64_zknh"; "rv64_zk"; "rv64_zkn" ] in
  (* SHA-512's own RV32-only 32-bit-word-pair-split helpers - the mirror
     image of the RV64 group above (primary rv32_zknh, imported by
     rv32_zk/rv32_zkn), a plain three-GPR R-type shape (not the two-GPR
     unary one sha256/sha512's non-split forms use), reusing
     {!r_type_gpr_form}/{!r_type_mnemonics} unchanged. *)
  let zknh_import_group_rv32 = [ "rv32_zknh"; "rv32_zk"; "rv32_zkn" ] in
  (* AES-64's plain three-GPR round functions and aes64im's two-GPR unary
     sibling: aes64ds/aes64dsm are primary rv64_zknd, imported by
     rv64_zk/rv64_zkn (three-way); aes64es/aes64esm are primary rv64_zkne,
     imported by rv64_zk/rv64_zkn (three-way, disjoint primary from
     ds/dsm); aes64ks2 is primary rv64_zknd, imported by
     rv64_zk/rv64_zkn/rv64_zkne (four-way - the one AES-64 mnemonic both
     the decrypt and encrypt key-schedule extensions import); aes64im is
     primary rv64_zknd, imported by rv64_zk/rv64_zkn (three-way, same shape
     as ds/dsm's group). All hand-verified identical mask/value across
     every member in the checked-in riscv64.jsonl before writing any code. *)
  let zknd_import_group_rv64 = [ "rv64_zknd"; "rv64_zk"; "rv64_zkn" ] in
  let zkne_import_group_rv64 = [ "rv64_zkne"; "rv64_zk"; "rv64_zkn" ] in
  let aes64ks2_import_group_rv64 = [ "rv64_zknd"; "rv64_zk"; "rv64_zkn"; "rv64_zkne" ] in
  (* AES-32's own round functions - the RV32 mirror of aes64ds/aes64dsm's
     and aes64es/aes64esm's own groups above (aes32dsi/aes32dsmi rooted in
     rv32_zknd, aes32esi/aes32esmi in a disjoint rv32_zkne), each imported
     by rv32_zk/rv32_zkn (three-way, no rv32_zkne membership for the
     zknd-rooted pair or vice versa - hand-verified, not assumed). *)
  let aes32d_import_group_rv32 = [ "rv32_zknd"; "rv32_zk"; "rv32_zkn" ] in
  let aes32e_import_group_rv32 = [ "rv32_zkne"; "rv32_zk"; "rv32_zkn" ] in
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
    ("sha256sum0", zknh_import_group);
    ("sha256sum1", zknh_import_group);
    ("sha256sig0", zknh_import_group);
    ("sha256sig1", zknh_import_group);
    ("sha512sum0", zknh_import_group_rv64);
    ("sha512sum1", zknh_import_group_rv64);
    ("sha512sig0", zknh_import_group_rv64);
    ("sha512sig1", zknh_import_group_rv64);
    ("sha512sum0r", zknh_import_group_rv32);
    ("sha512sum1r", zknh_import_group_rv32);
    ("sha512sig0l", zknh_import_group_rv32);
    ("sha512sig1l", zknh_import_group_rv32);
    ("sha512sig0h", zknh_import_group_rv32);
    ("sha512sig1h", zknh_import_group_rv32);
    ("aes64ds", zknd_import_group_rv64);
    ("aes64dsm", zknd_import_group_rv64);
    ("aes64es", zkne_import_group_rv64);
    ("aes64esm", zkne_import_group_rv64);
    ("aes64ks2", aes64ks2_import_group_rv64);
    ("aes64im", zknd_import_group_rv64);
    ("aes64ks1i", aes64ks2_import_group_rv64);
    ("aes32dsi", aes32d_import_group_rv32);
    ("aes32dsmi", aes32d_import_group_rv32);
    ("aes32esi", aes32e_import_group_rv32);
    ("aes32esmi", aes32e_import_group_rv32);
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
   [roriw] is the plain RV64-only *w sibling, needing no such split.

   [operand_name]/[field_name] default to rori's own "shamt"/"shamtd"-or-
   "shamtw" naming, but are overridable - {!aes64ks1i}'s [rnum] is the same
   two-GPR-plus-narrow-unsigned-immediate shape at the normalized-model
   level (this function only models the field's raw width, not any
   semantic subrange a real assembler further restricts), just with the
   record's own field name "rnum", not a shift amount. *)
let shamt_gpr_form ?extension_lookup_key ?operand_name ?field_name ~mnemonic ~width (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement =
        requirement_of_mnemonic ~mnemonic:(Option.value extension_lookup_key ~default:mnemonic) rec_
      in
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let operand_name = Option.value operand_name ~default:"shamt" in
      let field_name =
        Option.value field_name ~default:(if width = 6 then "shamtd" else "shamtw")
      in
      let shamt =
        {
          op_name = operand_name;
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
              operands = [ Syn_operand "rd"; Syn_operand "rs1"; Syn_operand operand_name ];
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

(* Generic three-GPR-plus-narrow-unsigned-immediate form: AES-32's own
   round functions (aes32dsi/aes32dsmi/aes32esi/aes32esmi), riscv-opcodes'
   [rd, rs1, rs2, bs] variable_fields shape - the three-GPR analogue of
   {!shamt_gpr_form}'s two-GPR-plus-immediate one. Like that function, this
   only models the field's raw width; a real assembler's own narrower
   semantic subrange (if any - AES-32's own "bs" happens to use its full
   2-bit range, unlike aes64ks1i's "rnum") is an encoder concern, not a
   normalization one. *)
let r_type_imm_gpr_form ~mnemonic ~width ~operand_name ~field_name (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of_mnemonic ~mnemonic rec_ in
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = gpr (); role = In; explicit = true } in
      let imm =
        {
          op_name = operand_name;
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
          operands = [ rd; rs1; rs2; imm ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [ Syn_operand "rd"; Syn_operand "rs1"; Syn_operand "rs2"; Syn_operand operand_name ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  Printf.sprintf
                    "operand fields rd, rs1, rs2, %s taken verbatim from encoding.fields" field_name;
              };
            ];
          diagnostics =
            (match requirement with
            | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
            | _ -> []);
        }

(* Zicsr's register-source CSR forms (csrrw/csrrs/csrrc): [rd, rs1, csr] in
   riscv-opcodes' own encoding.fields/operands order, but GAS's own text
   order is [rd, csr, rs1] - the CSR address comes second, confirmed
   against real GNU as (`csrrw a0, mstatus, a1` -> the same bit pattern as
   riscv-opcodes' [rd, rs1, csr] field order, just written with [csr]
   before [rs1] in the source text). [csr] is a genuine, syntax-visible
   12-bit UNSIGNED immediate (0-4095), unlike every prior I-type immediate
   this project's normalized model has recorded (all signed so far) - the
   riscv_family_encode.ml encoder side handles the unsigned range by
   writing the address's own signed 12-bit two's-complement equivalent
   through the existing bit-masking machinery unchanged; the normalized
   model here just records the field's own raw width and its explicit
   [signed = false]. Neither record is import-duplicated (a single
   rv_zicsr record per mnemonic), so this is a plain [requirement_of], not
   a Req_any. *)
let csr_reg_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let csr =
        {
          op_name = "csr";
          op_kind =
            Immediate
              {
                width_bits = 12;
                signed = false;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [ { field_name = "csr"; field_hi = 11; field_lo = 0; dest_hi = 11; dest_lo = 0 } ];
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
          operands = [ rd; csr; rs1 ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "csr"; Syn_operand "rs1" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1, csr taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "GAS syntax reorders these as \"mnemonic rd, csr, rs1\" - csr before rs1 - not \
                   the encoding.fields declaration order";
              };
            ];
          diagnostics =
            (match requirement with
            | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
            | _ -> []);
        }

(* Zicsr's immediate-source CSR forms (csrrwi/csrrsi/csrrci): [rd, csr,
   zimm5] - riscv-opcodes' own operands order already matches GAS syntax
   here (unlike the register-source forms above), since there is no [rs1]
   to reorder around. [zimm5] is a genuine, syntax-visible 5-bit unsigned
   immediate (0-31 - real GNU as rejects 32+ as "improper CSRxI
   immediate"); the encoder reuses the bit position a GPR number would
   occupy in [rs1] to carry it, but that is an encoder-side reuse, not a
   normalized-model one - this form has no register operand besides [rd]
   at all. *)
let csr_imm_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let csr =
        {
          op_name = "csr";
          op_kind =
            Immediate
              {
                width_bits = 12;
                signed = false;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [ { field_name = "csr"; field_hi = 11; field_lo = 0; dest_hi = 11; dest_lo = 0 } ];
              };
          role = In;
          explicit = true;
        }
      in
      let zimm5 =
        {
          op_name = "zimm5";
          op_kind =
            Immediate
              {
                width_bits = 5;
                signed = false;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [ { field_name = "zimm5"; field_hi = 4; field_lo = 0; dest_hi = 4; dest_lo = 0 } ];
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
          operands = [ rd; csr; zimm5 ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "csr"; Syn_operand "zimm5" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, csr, zimm5 taken verbatim from encoding.fields";
              };
            ];
          diagnostics =
            (match requirement with
            | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
            | _ -> []);
        }

let csr_immediate_operand () =
  {
    op_name = "csr";
    op_kind =
      Immediate
        {
          width_bits = 12;
          signed = false;
          implicit_low_zero_bits = 0;
          nonzero = false;
          runs = [ { field_name = "csr"; field_hi = 11; field_lo = 0; dest_hi = 11; dest_lo = 0 } ];
        };
    role = In;
    explicit = true;
  }

(* [csrr rd, csr] - GAS's read-only alias for [csrrs rd, csr, x0] (rd = the
   only register operand, rs1 is implicitly x0). riscv-opcodes' own
   encoding.fields/operands order here, [rd, csr], already matches GAS
   syntax (there is no [rs1] to reorder around, unlike {!csr_write_form}
   below), confirmed against real GNU as (`csrr a0, mstatus` -> the same
   bit pattern as `csrrs a0, mstatus, zero`). *)
let csr_read_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let csr = csr_immediate_operand () in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ rd; csr ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "csr" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, csr taken verbatim from encoding.fields";
              };
            ];
          diagnostics =
            (match requirement with
            | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
            | _ -> []);
        }

(* [csrw/csrs/csrc csr, rs1] - GAS's write/set/clear-only aliases for
   [csrrw/csrrs/csrrc x0, csr, rs1] (rd = x0, discarding the old value; not
   a normalized-model operand since it is never syntax-visible). Like
   {!csr_reg_form} above, riscv-opcodes' own encoding.fields/operands order,
   [rs1, csr], does NOT match GAS's own text order [csr, rs1] - confirmed
   against real GNU as (`csrw mstatus, a1` -> the same bit pattern as
   `csrrw zero, mstatus, a1`). *)
let csr_write_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let csr = csr_immediate_operand () in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ csr; rs1 ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "csr"; Syn_operand "rs1" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rs1, csr taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "GAS syntax reorders these as \"mnemonic csr, rs1\" - not the encoding.fields \
                   declaration order";
              };
            ];
          diagnostics =
            (match requirement with
            | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
            | _ -> []);
        }

(* [csrwi/csrsi/csrci csr, zimm5] - GAS's write/set/clear-only aliases for
   [csrrwi/csrrsi/csrrci x0, csr, zimm5] (rd = x0). riscv-opcodes' own
   encoding.fields/operands order here, [csr, zimm5], already matches GAS
   syntax, confirmed against real GNU as (`csrwi mstatus, 5` -> the same
   bit pattern as `csrrwi zero, mstatus, 5`). *)
let csr_write_imm_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let csr = csr_immediate_operand () in
      let zimm5 =
        {
          op_name = "zimm5";
          op_kind =
            Immediate
              {
                width_bits = 5;
                signed = false;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [ { field_name = "zimm5"; field_hi = 4; field_lo = 0; dest_hi = 4; dest_lo = 0 } ];
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
          operands = [ csr; zimm5 ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "csr"; Syn_operand "zimm5" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields csr, zimm5 taken verbatim from encoding.fields";
              };
            ];
          diagnostics =
            (match requirement with
            | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
            | _ -> []);
        }

(* [amoOP rd, rs2, (rs1)] / [scOP rd, rs2, (rs1)] - Zaamo's three-GPR-
   plus-memory shape. Unlike {!sw_form}'s [offset(base)], the memory group
   here has no offset field at all - riscv-opcodes' own encoding.fields
   never lists one for this record, since GAS's own "(rs1)" is pure
   parenthesization, not an encoded value (confirmed against real GNU as:
   `amoadd.w a0, a1, 4(a2)` is rejected, "illegal operands"). The record's
   own `aq`/`rl` fields are real, independently encodable bits (GAS's
   `.aq`/`.rl`/`.aqrl` mnemonic-suffix decorators read/write them), but
   this normalizes only GAS's bare canonical (aq=0,rl=0) spelling per the
   plan's "canonical spelling first ... decorators ... as separately
   identified cases" policy (section 5.2) - not modeled as operands here,
   flagged as an explicit diagnostic instead of silently dropped. *)
let amo_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = gpr (); role = In; explicit = true } in
      let base = { op_name = "base"; op_kind = gpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ rd; rs2; base ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_operand "rd";
                  Syn_operand "rs2";
                  Syn_group [ Syn_literal "("; Syn_operand "base"; Syn_literal ")" ];
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1, rs2 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note = "rs1 renders as GAS's parenthesized memory base, never a bare register";
              };
            ];
          diagnostics =
            (match requirement with
              | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
              | _ -> [])
            @ [
                {
                  rule = mnemonic ^ "-aq-rl-not-modeled";
                  message =
                    "aq/rl bits are real encodable fields (GAS .aq/.rl/.aqrl mnemonic-suffix \
                     decorators); only the bare aq=0,rl=0 canonical spelling is normalized here";
                };
              ];
        }

(* [lr.w/lr.d rd, (rs1)] - the same family's only two-operand member; rs2's
   field is architecturally fixed to 0 and never syntax-visible (unlike
   {!amo_form}'s real rs2 operand), so it is not modeled as an operand at
   all, the same omission convention {!csr_write_form} uses for its
   implicit-x0 register. *)
let lr_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let base = { op_name = "base"; op_kind = gpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ rd; base ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_operand "rd";
                  Syn_group [ Syn_literal "("; Syn_operand "base"; Syn_literal ")" ];
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand field rd, rs1 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "rs1 renders as GAS's parenthesized memory base; rs2's field is fixed to 0 and \
                   is not a real operand";
              };
            ];
          diagnostics =
            (match requirement with
              | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
              | _ -> [])
            @ [
                {
                  rule = mnemonic ^ "-aq-rl-not-modeled";
                  message =
                    "aq/rl bits are real encodable fields (GAS .aq/.rl/.aqrl mnemonic-suffix \
                     decorators); only the bare aq=0,rl=0 canonical spelling is normalized here";
                };
              ];
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
    "sha512sum0r";
    "sha512sum1r";
    "sha512sig0l";
    "sha512sig1l";
    "sha512sig0h";
    "sha512sig1h";
    "aes64ds";
    "aes64dsm";
    "aes64es";
    "aes64esm";
    "aes64ks2";
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
    "sha256sum0";
    "sha256sum1";
    "sha256sig0";
    "sha256sig1";
    "sha512sum0";
    "sha512sum1";
    "sha512sig0";
    "sha512sig1";
    "aes64im";
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

(* fsgnj.s/fsgnjn.s/fsgnjx.s/fsgnj.d/fsgnjn.d/fsgnjx.d: unlike {!f_arith_mnemonics},
   funct3 here is a real per-mnemonic selector baked into the source record's own
   fixed-bits mask (rd_d/rs1/rs2 fixed_bits width 27 does not cover it, but bits
   [14:12] are included in the mask - each of the three sign-injection mnemonics
   per precision is a separate record with a distinct mask/value), not a
   rounding-mode field - so no [rm] operand is modeled. *)
let f_sgnj_mnemonics = [ "fsgnj.s"; "fsgnjn.s"; "fsgnjx.s"; "fsgnj.d"; "fsgnjn.d"; "fsgnjx.d" ]

let f_sgnj_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = fpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = fpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
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
              {
                label = Inferred;
                note =
                  "rd/rs1/rs2 are floating-point registers (rv_f/rv_d), not the GPR class \
                   rd/rs1/rs2 denote in integer forms";
              };
            ];
          diagnostics = [];
        }

(* fmin.s/fmax.s/fmin.d/fmax.d: the same three-FP-register, no-[rm] shape as
   {!f_sgnj_form} (funct3 here selects min vs max, again a fixed
   per-mnemonic selector baked into the record's own fixed-bits mask, not a
   rounding mode). *)
let f_minmax_mnemonics = [ "fmin.s"; "fmax.s"; "fmin.d"; "fmax.d" ]

let f_minmax_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = fpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = fpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
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
              {
                label = Inferred;
                note =
                  "rd/rs1/rs2 are floating-point registers (rv_f/rv_d), not the GPR class \
                   rd/rs1/rs2 denote in integer forms";
              };
            ];
          diagnostics = [];
        }

(* fsqrt.s/fsqrt.d: {!f_arith_form}'s own shape minus [rs2] (a fixed
   selector, not a real second operand - the source record's own mask
   fixes bits[24:20] to 0) - [rm] is a genuine, implicit dynamic-rounding
   operand exactly like the arithmetic family. *)
let f_sqrt_mnemonics = [ "fsqrt.s"; "fsqrt.d" ]

let f_sqrt_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = fpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      let rm = { op_name = "rm"; op_kind = Rounding_mode; role = In; explicit = false } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs1; rm ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "rs1" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1, rm taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "rd/rs1 are floating-point registers (rv_f/rv_d), not the GPR class rd/rs1 \
                   denote in integer forms";
              };
              {
                label = Inferred;
                note =
                  "the omitted rm operand is GNU as's dynamic-rounding default (funct3=7), \
                   measured for this bare scalar spelling on rv32imf and rv64imf";
              };
            ];
          diagnostics = [];
        }

(* fclass.s/fclass.d: [rd] is a GPR (the classification bitmask), [rs1] is
   FP, no [rm] - [rs2] is again a fixed selector the source record's own
   mask fixes, not a real operand. *)
let f_class_mnemonics = [ "fclass.s"; "fclass.d" ]

let f_class_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
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
                  "rd is a GPR (the classification bitmask), rs1 a floating-point register \
                   (rv_f/rv_d)";
              };
            ];
          diagnostics = [];
        }

(* fmadd.s/fmsub.s/fnmsub.s/fnmadd.s/fmadd.d/fmsub.d/fnmsub.d/fnmadd.d:
   {!f_arith_form}'s own shape plus a fourth floating-point source ([rs3]) -
   riscv-opcodes' own record already lists rd/rs1/rs2/rs3/rm verbatim, so
   [riscv_encoding_of] needs no special handling; only the encoder's
   R4-type word layout (rd/rs1/rs2/rs3 fields split differently than R-type's
   rd/rs1/rs2/funct7 - see {!Riscv_family_encode.f_fma_desc}) is genuinely
   new. Like {!f_arith_mnemonics}, this claims only the bare
   dynamic-rounding spelling. *)
let f_fma_mnemonics =
  [ "fmadd.s"; "fmsub.s"; "fnmsub.s"; "fnmadd.s"; "fmadd.d"; "fmsub.d"; "fnmsub.d"; "fnmadd.d" ]

let f_fma_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = fpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = fpr (); role = In; explicit = true } in
      let rs3 = { op_name = "rs3"; op_kind = fpr (); role = In; explicit = true } in
      let rm = { op_name = "rm"; op_kind = Rounding_mode; role = In; explicit = false } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs1; rs2; rs3; rm ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [ Syn_operand "rd"; Syn_operand "rs1"; Syn_operand "rs2"; Syn_operand "rs3" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1, rs2, rs3, rm taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "rd/rs1/rs2/rs3 are floating-point registers (rv_f/rv_d), not the GPR class \
                   rd/rs1/rs2 denote in integer forms";
              };
              {
                label = Inferred;
                note =
                  "the omitted rm operand is GNU as's dynamic-rounding default (funct3=7), \
                   measured for this bare fused-multiply-add spelling on rv32imf(d)/rv64imf(d)";
              };
            ];
          diagnostics = [];
        }

(* flw/fld: I-type loads whose [rd] is a floating-point register, not the
   GPR class {!i_type_imm_form}'s own [rd] denotes - otherwise byte-for-byte
   {!sw_form}'s single-run [imm12] I-type sibling (both already fully
   implemented by the encoder's shared [f_load_desc] path; this closes only
   the normalization/corpus/admission side). *)
let f_load_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let value = { op_name = "value"; op_kind = fpr (); role = Out; explicit = true } in
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
                    { field_name = "imm12"; field_hi = 11; field_lo = 0; dest_hi = 11; dest_lo = 0 };
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
          operands = [ value; base; offset ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
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
                note = "operand fields rd, rs1, imm12 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "value (rd) is a floating-point register, not the GPR class rd denotes in \
                   integer loads";
              };
            ];
          diagnostics =
            (match requirement with
            | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
            | _ -> []);
        }

(* fsw/fsd: S-type stores whose stored value is a floating-point register -
   otherwise byte-for-byte {!sw_form}'s own imm12hi/imm12lo split-immediate
   shape (also already fully implemented by the encoder's shared
   [f_store_desc] path). *)
let f_store_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let value = { op_name = "value"; op_kind = fpr (); role = In; explicit = true } in
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
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ value; base; offset ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
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
                  "value (rs2) is a floating-point register, not the GPR class rs2 denotes in \
                   integer stores";
              };
            ];
          diagnostics =
            (match requirement with
            | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
            | _ -> []);
        }

(* feq.s/fle.s/flt.s/feq.d/fle.d/flt.d: [rd] is a GPR (the boolean result),
   [rs1]/[rs2] are FP - the mirror image of {!f_fma_form}'s all-FPR shape,
   here with no [rm] at all (funct3 is a real comparison-kind selector, per
   {!Riscv_family_encode.f_cmp_desc}). *)
let f_cmp_mnemonics = [ "feq.s"; "fle.s"; "flt.s"; "feq.d"; "fle.d"; "flt.d" ]

let f_cmp_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = fpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
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
              {
                label = Inferred;
                note =
                  "rd is a GPR (the boolean comparison result), rs1/rs2 are floating-point \
                   registers (rv_f/rv_d)";
              };
            ];
          diagnostics = [];
        }

(* fmv.x.w: bit-for-bit move (not a conversion), [fmv.x.d]'s single-precision
   sibling - [rd] a GPR, [rs1] FP, no [rm]. *)
let f_mv_x_w_mnemonics = [ "fmv.x.w" ]

let f_mv_x_w_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
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
                  "rd is a GPR (the raw bit pattern, sign-extended), rs1 a floating-point register \
                   (rv_f); this is a bit-for-bit move, not a numeric conversion";
              };
            ];
          diagnostics = [];
        }

(* fmv.w.x: [fmv.x.w]'s reverse-direction sibling - [rd] FP, [rs1] a GPR, no
   [rm]. *)
let f_mv_w_x_mnemonics = [ "fmv.w.x" ]

let f_mv_w_x_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = fpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
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
                  "rd is a floating-point register (rv_f), rs1 a GPR (the raw bit pattern); this \
                   is a bit-for-bit move, not a numeric conversion";
              };
            ];
          diagnostics = [];
        }

(* fcvt.w.s/fcvt.wu.s/fcvt.w.d/fcvt.wu.d/fcvt.l.d/fcvt.lu.d/fcvt.l.s/fcvt.lu.s:
   float-to-integer word/long converts, {!f_sqrt_form}'s own shape (a real,
   genuine dynamic-rounding [rm], unlike [fmv.x.w]'s fixed funct3) with
   [rd] a GPR instead of FP - [rs2] is again a fixed selector the source
   record's own mask fixes, not a real operand. Every sibling here shares
   this exact shape and dynamic-rounding default verbatim (`f_to_i_desc`'s
   own funct3 = 7 for all eight); only {!requirement_of} distinguishes the
   D-extension pairs (riscv:d) from the F-extension ones (riscv:f) and the
   RV64-only long pairs (an added `Req_xlen 64`) from the XLEN-independent
   word ones, automatically, from each record's own extension. *)
let f_cvt_w_s_mnemonics =
  [
    "fcvt.w.s";
    "fcvt.wu.s";
    "fcvt.w.d";
    "fcvt.wu.d";
    "fcvt.l.d";
    "fcvt.lu.d";
    "fcvt.l.s";
    "fcvt.lu.s";
  ]

let f_cvt_w_s_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      let rm = { op_name = "rm"; op_kind = Rounding_mode; role = In; explicit = false } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs1; rm ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "rs1" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1, rm taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note = "rd is a GPR (the truncated/rounded integer), rs1 a floating-point register";
              };
              {
                label = Inferred;
                note =
                  "the omitted rm operand is GNU as's dynamic-rounding default (funct3=7), \
                   measured for this bare scalar spelling on rv32imf and rv64imf";
              };
            ];
          diagnostics = [];
        }

(* fcvt.s.w/fcvt.s.wu/fcvt.s.l/fcvt.s.lu/fcvt.d.l/fcvt.d.lu: {!f_cvt_w_s_form}'s
   reverse direction - [rd] FP, [rs1] a GPR, same genuine dynamic-rounding
   [rm] (unlike {!f_cvt_d_w_form}'s [fcvt.d.w]/[fcvt.d.wu] pair, [fcvt.d.l]/
   [fcvt.d.lu] do NOT default to always-exact rne - a 64-bit long does not
   always fit exactly in a double's 52-bit mantissa, confirmed against real
   GNU as: `fcvt.d.l fa0, a1` -> `d225f553`, funct3 = 7 (dyn), not 0). *)
let f_cvt_s_w_mnemonics =
  [ "fcvt.s.w"; "fcvt.s.wu"; "fcvt.s.l"; "fcvt.s.lu"; "fcvt.d.l"; "fcvt.d.lu" ]

let f_cvt_s_w_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = fpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let rm = { op_name = "rm"; op_kind = Rounding_mode; role = In; explicit = false } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs1; rm ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "rs1" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1, rm taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note = "rd is a floating-point register, rs1 a GPR (the source integer)";
              };
              {
                label = Inferred;
                note =
                  "the omitted rm operand is GNU as's dynamic-rounding default (funct3=7), \
                   measured for this bare scalar spelling on rv32imf and rv64imf";
              };
            ];
          diagnostics = [];
        }

(* fcvt.d.w/fcvt.d.wu: {!f_cvt_s_w_form}'s own shape ([rd] FP, [rs1] a GPR)
   but with real hardware's "always exact" rne=0 default instead of dyn=7
   ([i_to_f_desc]'s own funct3 = 0 for both) - a 32-bit integer always fits
   exactly in a double, so GAS picks the exact rounding mode rather than
   reading fcsr, unlike every dynamic-rounding mnemonic above. *)
let f_cvt_d_w_mnemonics = [ "fcvt.d.w"; "fcvt.d.wu" ]

let f_cvt_d_w_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = fpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let rm = { op_name = "rm"; op_kind = Rounding_mode; role = In; explicit = false } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs1; rm ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "rs1" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1, rm taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note = "rd is a floating-point register, rs1 a GPR (the source integer)";
              };
              {
                label = Inferred;
                note =
                  "the omitted rm operand is GNU as's always-exact default (funct3=0, rne), not \
                   the dynamic default every arithmetic mnemonic uses - a 32-bit integer always \
                   converts to double exactly, measured for this bare scalar spelling on rv32imfd \
                   and rv64imfd";
              };
            ];
          diagnostics = [];
        }

(* fcvt.s.d/fcvt.d.s: float-to-float precision converts, [rd]/[rs1] both FP
   with no GPR operand at all - [rs2] is again a fixed selector (the source
   format) the source record's own mask fixes. [fcvt.s.d] (narrowing,
   double to single, can lose precision) defaults to dynamic rounding like
   every arithmetic mnemonic; [fcvt.d.s] (widening, single to double,
   always exact) defaults to rne=0 like {!f_cvt_d_w_form}'s pair. *)
let f_cvt_f_f_mnemonics = [ "fcvt.s.d"; "fcvt.d.s" ]

let f_cvt_f_f_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = fpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      let rm = { op_name = "rm"; op_kind = Rounding_mode; role = In; explicit = false } in
      let default_note =
        if String.equal mnemonic "fcvt.d.s" then
          "the omitted rm operand is GNU as's always-exact default (funct3=0, rne) - \
           single-to-double widening never loses precision"
        else
          "the omitted rm operand is GNU as's dynamic-rounding default (funct3=7) - \
           double-to-single narrowing can lose precision"
      in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs1; rm ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "rs1" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1, rm taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note = "rd/rs1 are both floating-point registers, of different precisions";
              };
              { label = Inferred; note = default_note };
            ];
          diagnostics = [];
        }

let normalize (rec_ : R.t) =
  match rec_.native_name with
  | "sw" -> sw_form rec_
  | "beq" -> beq_form rec_
  | "c.addi" -> c_addi_form rec_
  | "flw" -> f_load_form ~mnemonic:"flw" rec_
  | "fld" -> f_load_form ~mnemonic:"fld" rec_
  | "fsw" -> f_store_form ~mnemonic:"fsw" rec_
  | "fsd" -> f_store_form ~mnemonic:"fsd" rec_
  | mnemonic when List.mem mnemonic f_arith_mnemonics -> f_arith_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic f_sgnj_mnemonics -> f_sgnj_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic f_minmax_mnemonics -> f_minmax_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic f_sqrt_mnemonics -> f_sqrt_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic f_class_mnemonics -> f_class_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic f_fma_mnemonics -> f_fma_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic f_cmp_mnemonics -> f_cmp_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic f_mv_x_w_mnemonics -> f_mv_x_w_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic f_mv_w_x_mnemonics -> f_mv_w_x_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic f_cvt_w_s_mnemonics -> f_cvt_w_s_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic f_cvt_s_w_mnemonics -> f_cvt_s_w_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic f_cvt_d_w_mnemonics -> f_cvt_d_w_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic f_cvt_f_f_mnemonics -> f_cvt_f_f_form ~mnemonic rec_
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
  (* aes64ks1i: the same two-GPR-plus-narrow-unsigned-immediate shape as
     rori/roriw above, reusing shamt_gpr_form's generalized [operand_name]/
     [field_name] overrides for riscv-opcodes' own "rnum" field (a 4-bit
     round-select, not a shift amount) - the normalized model only records
     the field's raw width; aes64ks1i's real semantic subrange (0-10 of the
     16 representable values) is an encoder concern, not a normalization
     one. *)
  | "aes64ks1i" ->
      shamt_gpr_form ~mnemonic:"aes64ks1i" ~width:4 ~operand_name:"rnum" ~field_name:"rnum" rec_
  (* aes32dsi/aes32dsmi/aes32esi/aes32esmi: AES-32's own round functions -
     the three-GPR-plus-narrow-unsigned-immediate shape r_type_imm_gpr_form
     above models, riscv-opcodes' own "bs" (byte-select) field. *)
  | "aes32dsi" ->
      r_type_imm_gpr_form ~mnemonic:"aes32dsi" ~width:2 ~operand_name:"bs" ~field_name:"bs" rec_
  | "aes32dsmi" ->
      r_type_imm_gpr_form ~mnemonic:"aes32dsmi" ~width:2 ~operand_name:"bs" ~field_name:"bs" rec_
  | "aes32esi" ->
      r_type_imm_gpr_form ~mnemonic:"aes32esi" ~width:2 ~operand_name:"bs" ~field_name:"bs" rec_
  | "aes32esmi" ->
      r_type_imm_gpr_form ~mnemonic:"aes32esmi" ~width:2 ~operand_name:"bs" ~field_name:"bs" rec_
  (* Zicsr's register-source and immediate-source CSR forms. *)
  | "csrrw" -> csr_reg_form ~mnemonic:"csrrw" rec_
  | "csrrs" -> csr_reg_form ~mnemonic:"csrrs" rec_
  | "csrrc" -> csr_reg_form ~mnemonic:"csrrc" rec_
  | "csrrwi" -> csr_imm_form ~mnemonic:"csrrwi" rec_
  | "csrrsi" -> csr_imm_form ~mnemonic:"csrrsi" rec_
  | "csrrci" -> csr_imm_form ~mnemonic:"csrrci" rec_
  | "csrr" -> csr_read_form ~mnemonic:"csrr" rec_
  | "csrw" -> csr_write_form ~mnemonic:"csrw" rec_
  | "csrs" -> csr_write_form ~mnemonic:"csrs" rec_
  | "csrc" -> csr_write_form ~mnemonic:"csrc" rec_
  | "csrwi" -> csr_write_imm_form ~mnemonic:"csrwi" rec_
  | "csrsi" -> csr_write_imm_form ~mnemonic:"csrsi" rec_
  | "csrci" -> csr_write_imm_form ~mnemonic:"csrci" rec_
  (* Zaamo's 22 atomic-memory-operation/load-reserved/store-conditional
     records - 9 amoOP mnemonics plus sc on each of .w/.d, plus lr.w/lr.d
     (see {!amo_form}/{!lr_form} above). *)
  | "amoswap.w" -> amo_form ~mnemonic:"amoswap.w" rec_
  | "amoadd.w" -> amo_form ~mnemonic:"amoadd.w" rec_
  | "amoxor.w" -> amo_form ~mnemonic:"amoxor.w" rec_
  | "amoand.w" -> amo_form ~mnemonic:"amoand.w" rec_
  | "amoor.w" -> amo_form ~mnemonic:"amoor.w" rec_
  | "amomin.w" -> amo_form ~mnemonic:"amomin.w" rec_
  | "amomax.w" -> amo_form ~mnemonic:"amomax.w" rec_
  | "amominu.w" -> amo_form ~mnemonic:"amominu.w" rec_
  | "amomaxu.w" -> amo_form ~mnemonic:"amomaxu.w" rec_
  | "sc.w" -> amo_form ~mnemonic:"sc.w" rec_
  | "lr.w" -> lr_form ~mnemonic:"lr.w" rec_
  | "amoswap.d" -> amo_form ~mnemonic:"amoswap.d" rec_
  | "amoadd.d" -> amo_form ~mnemonic:"amoadd.d" rec_
  | "amoxor.d" -> amo_form ~mnemonic:"amoxor.d" rec_
  | "amoand.d" -> amo_form ~mnemonic:"amoand.d" rec_
  | "amoor.d" -> amo_form ~mnemonic:"amoor.d" rec_
  | "amomin.d" -> amo_form ~mnemonic:"amomin.d" rec_
  | "amomax.d" -> amo_form ~mnemonic:"amomax.d" rec_
  | "amominu.d" -> amo_form ~mnemonic:"amominu.d" rec_
  | "amomaxu.d" -> amo_form ~mnemonic:"amomaxu.d" rec_
  | "sc.d" -> amo_form ~mnemonic:"sc.d" rec_
  | "lr.d" -> lr_form ~mnemonic:"lr.d" rec_
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
