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
  | "rv_v" -> Req_feature "riscv:v"
  (* Zvbc (vector carry-less multiply): despite being a V sub-extension,
     real GNU as accepts Zvbc mnemonics under [-march=rv64i_zvbc] alone (no
     explicit [v]; confirmed byte-identical to [-march=rv64iv_zvbc]) but
     rejects [-march=rv64iv] (no [zvbc]) with "extension `zvbc' required" -
     so [riscv:zvbc] alone is the accurate requirement, not a [Req_all]
     with [riscv:v]. *)
  | "rv_zvbc" -> Req_feature "riscv:zvbc"
  (* Zvkg (vector GCM/GHASH), the same real-GNU-as finding as Zvbc above:
     confirmed accepted under [-march=...zvkg] alone, rejected under plain
     [-march=...v] ("extension `zvkg' required"). *)
  | "rv_zvkg" -> Req_feature "riscv:zvkg"
  | "rv_zvknha" -> Req_feature "riscv:zvknha"
  | "rv_zvknhb" -> Req_feature "riscv:zvknhb"
  | "rv_zvkn" -> Req_feature "riscv:zvkn"
  | "rv_zvksed" -> Req_feature "riscv:zvksed"
  | "rv_zvks" -> Req_feature "riscv:zvks"
  | "rv_zvksh" -> Req_feature "riscv:zvksh"
  | "rv_zvbb" -> Req_feature "riscv:zvbb"
  | "rv_zvkned" -> Req_feature "riscv:zvkned"
  | "rv_zvfbfmin" -> Req_feature "riscv:zvfbfmin"
  | "rv_zvfbfwma" -> Req_feature "riscv:zvfbfwma"
  (* Zvkb never appears as a real riscv-opcodes record's own extension in
     this snapshot - it exists only as a real-GNU-as-recognized
     alternative for Zvbb's own bit-manipulation subset (see
     {!alternative_extensions_by_mnemonic}'s own comment). Included here
     purely so {!requirement_of_any} can list it as a Req_any feature. *)
  | "rv_zvkb" -> Req_feature "riscv:zvkb"
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
  (* vsha2ms.vv/vsha2ch.vv/vsha2cl.vv (Zvknha's SHA-256 vector helpers) are a
     three-way import group, XLEN-independent: riscv-opcodes' primary record
     is rv_zvknha, imported by rv_zvknhb (SHA-256-and-512's superset
     extension) and by rv_zvkn (the NIST vector-crypto bundle, which also
     imports Zvbb's and Zvkned's own mnemonics - not modeled as an
     alternative for those yet, only for this group, since this repository
     hasn't promoted Zvbb/Zvkned themselves). Confirmed directly against
     real GNU as (not just riscv-opcodes' relationships field): both
     `-march=...i_zvknhb` and `-march=...i_zvkn` alone (no explicit
     `zvknha`) assemble all three mnemonics byte-identical to
     `-march=...i_zvknha`, and the "unrecognized opcode" error under plain V
     literally names two of the three alternatives ("extension `zvknha' or
     `zvknhb' required" - GNU as's own message happens not to mention
     `zvkn`, but real assembly under `-march=...i_zvkn` alone still
     succeeds, confirmed directly). Identical mnemonic/encoding on both
     profiles, hand-verified identical mask/value across all three members
     in the checked-in riscv64.jsonl before writing any code. *)
  let zvknha_import_group = [ "rv_zvknha"; "rv_zvknhb"; "rv_zvkn" ] in
  (* vsm4k.vi/vsm4r.vs/vsm4r.vv (Zvksed's SM4 block-cipher helpers) are a
     two-way import group, XLEN-independent: riscv-opcodes' primary record
     is rv_zvksed, imported by rv_zvks (the ShangMi vector-crypto bundle) -
     confirmed directly against real GNU as: `-march=...i_zvks` alone (no
     explicit `zvksed`) assembles all three mnemonics byte-identical to
     `-march=...i_zvksed`. Unlike the Zvknha group above, only one importer
     exists here - hand-verified by grepping every mnemonic's own
     provenance.extension across the whole checked-in export, not assumed
     from riscv-opcodes' relationships field alone. *)
  let zvksed_import_group = [ "rv_zvksed"; "rv_zvks" ] in
  (* vsm3c.vi/vsm3me.vv (Zvksh's SM3 hash helpers) are a two-way import
     group like Zvksed's own above: primary rv_zvksh, imported by rv_zvks
     alone - confirmed directly against real GNU as (`-march=...i_zvks`
     alone assembles both mnemonics byte-identical to
     `-march=...i_zvksh`), hand-verified no third alternative exists by
     grepping every candidate's own provenance.extension across the whole
     checked-in export before writing any code. *)
  let zvksh_import_group = [ "rv_zvksh"; "rv_zvks" ] in
  (* Zvbb's own bit-manipulation subset (vandn.vv/.vx, vbrev8.v, vrev8.v,
     vrol.vv/.vx, vror.vv/.vx/.vi - 9 mnemonics, NOT vbrev.v/vclz.v/
     vcpop.v/vctz.v/vwsll.* which need full Zvbb) is a genuinely different
     kind of alternative-extensions group from every other entry in this
     table: riscv-opcodes' own records only ever tag these as rv_zvbb (or,
     for rv_zvkn/rv_zvks, an import of it) - there is no "rv_zvkb" record
     anywhere in this snapshot at all. Real GNU as nonetheless accepts a
     bare `-march=...zvkb` for exactly these 9 mnemonics (confirmed
     directly, not inferred from riscv-opcodes), rejecting the
     Zvbb-only four (`vbrev.v`/`vclz.v`/`vcpop.v`/`vctz.v`) and
     `vwsll.*` the same way plain V rejects them - so Zvkb is included as
     a fourth Req_any alternative purely on GNU-as-observed grounds, with
     {!feature_of_extension}'s own "rv_zvkb" case existing only to name it,
     never to classify a real record. rv_zvkn/rv_zvks both import exactly
     this same 9-mnemonic subset (hand-verified against the checked-in
     export - their own 23/14-record totals include Zvkned's/other
     families' mnemonics too, out of scope here). *)
  let zvkb_subset_group = [ "rv_zvbb"; "rv_zvkb"; "rv_zvkn"; "rv_zvks" ] in
  (* Zvkned's AES round/key-schedule family (11 mnemonics) is a two-way
     import group like Zvksed's own above: primary rv_zvkned, imported by
     rv_zvkn alone (confirmed directly against real GNU as:
     `-march=...i_zvkn` alone assembles all 11 mnemonics byte-identical
     to `-march=...i_zvkned`) - hand-verified no third alternative
     exists (unlike Zvbb's own Zvkb-subset) by grepping every mnemonic's
     own provenance.extension across the whole checked-in export before
     writing any code. *)
  let zvkned_import_group = [ "rv_zvkned"; "rv_zvkn" ] in
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
    ("vsha2ms.vv", zvknha_import_group);
    ("vsha2ch.vv", zvknha_import_group);
    ("vsha2cl.vv", zvknha_import_group);
    ("vsm4k.vi", zvksed_import_group);
    ("vsm4r.vs", zvksed_import_group);
    ("vsm4r.vv", zvksed_import_group);
    ("vsm3c.vi", zvksh_import_group);
    ("vsm3me.vv", zvksh_import_group);
    ("vandn.vv", zvkb_subset_group);
    ("vandn.vx", zvkb_subset_group);
    ("vbrev8.v", zvkb_subset_group);
    ("vrev8.v", zvkb_subset_group);
    ("vrol.vv", zvkb_subset_group);
    ("vrol.vx", zvkb_subset_group);
    ("vror.vv", zvkb_subset_group);
    ("vror.vx", zvkb_subset_group);
    ("vror.vi", zvkb_subset_group);
    ("vaesdf.vv", zvkned_import_group);
    ("vaesdf.vs", zvkned_import_group);
    ("vaesdm.vv", zvkned_import_group);
    ("vaesdm.vs", zvkned_import_group);
    ("vaesef.vv", zvkned_import_group);
    ("vaesef.vs", zvkned_import_group);
    ("vaesem.vv", zvkned_import_group);
    ("vaesem.vs", zvkned_import_group);
    ("vaesz.vs", zvkned_import_group);
    ("vaeskf1.vi", zvkned_import_group);
    ("vaeskf2.vi", zvkned_import_group);
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
let vreg () = Register { class_ = Riscv_vec; excluded = [] }

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

(* V's unit-stride loads/stores: [vle8/16/32/64.v vd, (rs1)] /
   [vse8/16/32/64.v vs3, (rs1)] - the same "vd/vs3, (rs1)" no-offset shape as
   {!lr_form}, with a vector register (not a GPR) on the data side and the
   element width baked into each mnemonic's own fixed [encoding.mask]/
   [encoding.value] rather than a separate operand (see {!opivv_form}'s
   analogous [vm]-not-modeled note for the trailing mask suffix). The source
   record's own [nf] field (bits 31:29) is left free by riscv-opcodes'
   [mask] - architecturally shared with the segmented [vlseg<nf>e<eew>.v]
   family - but this snapshot's checked-in export has no segmented-mnemonic
   records at all, and GAS's own bare [vle32.v] spelling always emits nf=0,
   so only that canonical spelling is normalized here. *)
let v_load_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let rd = { op_name = "vd"; op_kind = vreg (); role = Out; explicit = true } in
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
                  Syn_operand "vd";
                  Syn_group [ Syn_literal "("; Syn_operand "base"; Syn_literal ")" ];
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vd, rs1 taken verbatim from encoding.fields, renamed base";
              };
              {
                label = Inferred;
                note =
                  "rs1 renders as GAS's parenthesized memory base with no encodable offset; GAS \
                   accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics =
            (match requirement with
              | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
              | _ -> [])
            @ [
                {
                  rule = mnemonic ^ "-nf-not-modeled";
                  message =
                    "nf is a real encodable field shared with the segmented vlseg<nf>e<eew>.v \
                     family (absent from this snapshot's export); only the bare nf=0 canonical \
                     spelling is normalized here";
                };
              ];
        }

let v_store_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let vs3 = { op_name = "vs3"; op_kind = vreg (); role = In; explicit = true } in
      let base = { op_name = "base"; op_kind = gpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ vs3; base ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_operand "vs3";
                  Syn_group [ Syn_literal "("; Syn_operand "base"; Syn_literal ")" ];
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vs3, rs1 taken verbatim from encoding.fields, renamed base";
              };
              {
                label = Inferred;
                note =
                  "rs1 renders as GAS's parenthesized memory base with no encodable offset; GAS \
                   accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics =
            (match requirement with
              | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
              | _ -> [])
            @ [
                {
                  rule = mnemonic ^ "-nf-not-modeled";
                  message =
                    "nf is a real encodable field shared with the segmented vsseg<nf>e<eew>.v \
                     family (absent from this snapshot's export); only the bare nf=0 canonical \
                     spelling is normalized here";
                };
              ];
        }

(* [vlm.v vd, (base)] / [vsm.v vs3, (base)]: V's mask-register load/store -
   {!v_load_form}/[v_store_form]'s identical "vd/vs3, (base)" shape, but
   the source record's own [lumop]/[sumop] field is fixed to 0b01011
   (rather than 0 for the unit-stride family) and, unlike that family,
   [vm] is fixed to 1 as well - real GNU as rejects a trailing mask
   operand outright (`vlm.v v1,(a0),v0.t` -> "illegal operands"), so
   unlike {!v_load_form} this shape has no unmodeled mask suffix to note,
   and (confirmed against the checked-in riscv32.jsonl/riscv64.jsonl)
   [nf] is not even a free field here - the record's own [encoding.mask]
   covers it, so there is no nf-not-modeled diagnostic either. *)
let vlm_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let rd = { op_name = "vd"; op_kind = vreg (); role = Out; explicit = true } in
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
                  Syn_operand "vd";
                  Syn_group [ Syn_literal "("; Syn_operand "base"; Syn_literal ")" ];
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vd, rs1 taken verbatim from encoding.fields, renamed base";
              };
              {
                label = Inferred;
                note =
                  "rs1 renders as GAS's parenthesized memory base with no encodable offset; unlike \
                   vle*.v/vse*.v, GAS accepts no trailing mask operand here (vm is fixed to 1)";
              };
            ];
          diagnostics =
            (match requirement with
            | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
            | _ -> []);
        }

let vsm_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let vs3 = { op_name = "vs3"; op_kind = vreg (); role = In; explicit = true } in
      let base = { op_name = "base"; op_kind = gpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ vs3; base ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_operand "vs3";
                  Syn_group [ Syn_literal "("; Syn_operand "base"; Syn_literal ")" ];
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vs3, rs1 taken verbatim from encoding.fields, renamed base";
              };
              {
                label = Inferred;
                note =
                  "rs1 renders as GAS's parenthesized memory base with no encodable offset; unlike \
                   vle*.v/vse*.v, GAS accepts no trailing mask operand here (vm is fixed to 1)";
              };
            ];
          diagnostics =
            (match requirement with
            | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
            | _ -> []);
        }

(* [vlse8/16/32/64.v vd, (base), stride] / [vsse8/16/32/64.v vs3, (base),
   stride]: V's strided loads/stores - {!v_load_form}/[v_store_form]'s
   identical memory-operand pair, plus a third plain GPR operand (the
   byte stride) the unit-stride family has no room for. Like
   {!v_load_form}, [nf] is a free field the record's own encoding leaves
   unconstrained (shared with the segmented `vlsseg<nf>e<eew>.v` family,
   absent from this snapshot), so only the bare nf=0 spelling is
   normalized here. *)
let v_strided_load_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let rd = { op_name = "vd"; op_kind = vreg (); role = Out; explicit = true } in
      let base = { op_name = "base"; op_kind = gpr (); role = In; explicit = true } in
      let stride = { op_name = "rs2"; op_kind = gpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ rd; base; stride ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_operand "vd";
                  Syn_group [ Syn_literal "("; Syn_operand "base"; Syn_literal ")" ];
                  Syn_operand "rs2";
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, rs1, rs2 taken verbatim from encoding.fields, rs1 renamed \
                   base";
              };
              {
                label = Inferred;
                note =
                  "rs1 renders as GAS's parenthesized memory base with no encodable offset, \
                   followed by rs2 as the plain byte-stride operand; GAS accepts an optional \
                   trailing mask operand (\", v0.t\") selecting vm=0 - not modeled here, owned by \
                   the encoder";
              };
            ];
          diagnostics =
            (match requirement with
              | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
              | _ -> [])
            @ [
                {
                  rule = mnemonic ^ "-nf-not-modeled";
                  message =
                    "nf is a real encodable field shared with the segmented vlsseg<nf>e<eew>.v \
                     family (absent from this snapshot's export); only the bare nf=0 canonical \
                     spelling is normalized here";
                };
              ];
        }

let v_strided_store_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let vs3 = { op_name = "vs3"; op_kind = vreg (); role = In; explicit = true } in
      let base = { op_name = "base"; op_kind = gpr (); role = In; explicit = true } in
      let stride = { op_name = "rs2"; op_kind = gpr (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ vs3; base; stride ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_operand "vs3";
                  Syn_group [ Syn_literal "("; Syn_operand "base"; Syn_literal ")" ];
                  Syn_operand "rs2";
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vs3, rs1, rs2 taken verbatim from encoding.fields, rs1 renamed \
                   base";
              };
              {
                label = Inferred;
                note =
                  "rs1 renders as GAS's parenthesized memory base with no encodable offset, \
                   followed by rs2 as the plain byte-stride operand; GAS accepts an optional \
                   trailing mask operand (\", v0.t\") selecting vm=0 - not modeled here, owned by \
                   the encoder";
              };
            ];
          diagnostics =
            (match requirement with
              | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
              | _ -> [])
            @ [
                {
                  rule = mnemonic ^ "-nf-not-modeled";
                  message =
                    "nf is a real encodable field shared with the segmented vssseg<nf>e<eew>.v \
                     family (absent from this snapshot's export); only the bare nf=0 canonical \
                     spelling is normalized here";
                };
              ];
        }

(* [vl{u,o}xei8/16/32/64.v vd, (base), vs2] / [vs{u,o}xei8/16/32/64.v
   vs3, (base), vs2]: V's indexed loads/stores - {!v_strided_load_form}/
   [v_strided_store_form]'s identical three-operand shape, but the third
   operand is a vector register (the source record's own "vs2" field,
   the element index) rather than a GPR stride - ordered/unordered
   ("o"/"u") differ only in the encoder's fixed mop value, invisible at
   this normalization layer, so one pair of functions covers all 8
   mnemonics. *)
let v_indexed_load_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let rd = { op_name = "vd"; op_kind = vreg (); role = Out; explicit = true } in
      let base = { op_name = "base"; op_kind = gpr (); role = In; explicit = true } in
      let index = { op_name = "vs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ rd; base; index ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_operand "vd";
                  Syn_group [ Syn_literal "("; Syn_operand "base"; Syn_literal ")" ];
                  Syn_operand "vs2";
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, rs1, vs2 taken verbatim from encoding.fields, rs1 renamed \
                   base";
              };
              {
                label = Inferred;
                note =
                  "rs1 renders as GAS's parenthesized memory base with no encodable offset, \
                   followed by vs2 as the plain vector-register index operand; GAS accepts an \
                   optional trailing mask operand (\", v0.t\") selecting vm=0 - not modeled here, \
                   owned by the encoder";
              };
            ];
          diagnostics =
            (match requirement with
              | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
              | _ -> [])
            @ [
                {
                  rule = mnemonic ^ "-nf-not-modeled";
                  message =
                    "nf is a real encodable field shared with the segmented vlsseg<nf>e<eew>.v \
                     family (absent from this snapshot's export); only the bare nf=0 canonical \
                     spelling is normalized here";
                };
              ];
        }

let v_indexed_store_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let requirement = requirement_of rec_ in
      let vs3 = { op_name = "vs3"; op_kind = vreg (); role = In; explicit = true } in
      let base = { op_name = "base"; op_kind = gpr (); role = In; explicit = true } in
      let index = { op_name = "vs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement;
          encoding;
          operands = [ vs3; base; index ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [
                  Syn_operand "vs3";
                  Syn_group [ Syn_literal "("; Syn_operand "base"; Syn_literal ")" ];
                  Syn_operand "vs2";
                ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vs3, rs1, vs2 taken verbatim from encoding.fields, rs1 renamed \
                   base";
              };
              {
                label = Inferred;
                note =
                  "rs1 renders as GAS's parenthesized memory base with no encodable offset, \
                   followed by vs2 as the plain vector-register index operand; GAS accepts an \
                   optional trailing mask operand (\", v0.t\") selecting vm=0 - not modeled here, \
                   owned by the encoder";
              };
            ];
          diagnostics =
            (match requirement with
              | Req_unknown message -> [ { rule = mnemonic ^ "-xlen-unmodeled"; message } ]
              | _ -> [])
            @ [
                {
                  rule = mnemonic ^ "-nf-not-modeled";
                  message =
                    "nf is a real encodable field shared with the segmented vssseg<nf>e<eew>.v \
                     family (absent from this snapshot's export); only the bare nf=0 canonical \
                     spelling is normalized here";
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
    (* vsetvl (V's register-register configuration-setting instruction):
       riscv-opcodes exports a single rv_v record on both profiles, with no
       import duplication (relationships is empty), so the plain
       {!requirement_of} fallback (no {!alternative_extensions_by_mnemonic}
       entry needed) already gives the right Req_feature "riscv:v" answer.
       vsetvli/vsetivli (V's own immediate-vtype siblings) are a separate,
       larger normalization problem - GAS's "e<SEW>,m<LMUL>,ta|tu,ma|mu"
       operand syntax is not a plain register or a numeric immediate - and
       are left as a named follow-up, not covered by this entry. *)
    "vsetvl";
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

(* vsetvli's own [rd, rs1, zimm11] shape: riscv-opcodes exports a single,
   flat 11-bit [zimm11] field - GAS's "e<SEW>,m<LMUL>,ta|tu,ma|mu" spelling
   is a keyword decomposition of that one field's bits, not four separate
   riscv-opcodes fields, so this models one 11-bit unsigned [vtype]
   operand rather than four narrower ones; the encoder (not this
   normalized model) owns interpreting the keyword syntax, the same
   division of labor {!csr_reg_form} already uses for [csr]'s own numeric-
   vs-symbolic spelling. *)
let vsetvli_form (rec_ : R.t) =
  let mnemonic = "vsetvli" in
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let vtype =
        {
          op_name = "vtype";
          op_kind =
            Immediate
              {
                width_bits = 11;
                signed = false;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [
                    {
                      field_name = "zimm11";
                      field_hi = 10;
                      field_lo = 0;
                      dest_hi = 10;
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
          form_id = "riscv:vsetvli";
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs1; vtype ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "rs1"; Syn_operand "vtype" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields rd, rs1, zimm11 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "GAS spells the vtype operand as a keyword decomposition \
                   (\"e<SEW>,m<LMUL>,ta|tu,ma|mu\"), not a numeral - not modeled here, owned by \
                   the encoder";
              };
            ];
          diagnostics = [];
        }

(* vsetivli's own [rd, zimm5, zimm10] shape: the immediate-AVL sibling of
   {!vsetvli_form} above, [zimm5] (the AVL, GAS's own [uimm]) taking the
   bit position a GPR number would in [rs1] - the same reuse
   {!csr_imm_form}'s own [zimm5] already documents - and [zimm10] one bit
   narrower than [vsetvli]'s [zimm11] (bits[31:30] are fixed to 0b11
   instead of carrying one more content bit). GAS's own text order, [rd,
   uimm, vtype...], matches riscv-opcodes' field order reversed (rd last
   there), the same kind of reordering {!csr_reg_form} documents. *)
let vsetivli_form (rec_ : R.t) =
  let mnemonic = "vsetivli" in
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let uimm =
        {
          op_name = "uimm";
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
      let vtype =
        {
          op_name = "vtype";
          op_kind =
            Immediate
              {
                width_bits = 10;
                signed = false;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [
                    { field_name = "zimm10"; field_hi = 9; field_lo = 0; dest_hi = 9; dest_lo = 0 };
                  ];
              };
          role = In;
          explicit = true;
        }
      in
      Ok
        {
          form_id = "riscv:vsetivli";
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; uimm; vtype ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "uimm"; Syn_operand "vtype" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields zimm10, zimm5, rd taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note = "GAS syntax reorders these as \"mnemonic rd, uimm, vtype...\"";
              };
              {
                label = Inferred;
                note =
                  "GAS spells the vtype operand as a keyword decomposition \
                   (\"e<SEW>,m<LMUL>,ta|tu,ma|mu\"), not a numeral - not modeled here, owned by \
                   the encoder";
              };
            ];
          diagnostics = [];
        }

(* OP-V's OPIVV/OPIVX/OPIVI shapes, generalized across every mnemonic that
   uses them: {!opivv_form} for the all-vector-register [vadd.vv]/[vsub.vv]/
   [vand.vv]/[vor.vv]/[vxor.vv] shape, {!opivx_form} for the
   scalar-broadcast [.vx] siblings (also [vrsub.vx]), and {!opivi_form} for
   the signed-5-bit-immediate [.vi] siblings (also [vrsub.vi]; note there is
   no [vsub.vi] or [vrsub.vv] - riscv-opcodes exports no such records, and
   real GNU as rejects both as "unrecognized opcode" rather than "illegal
   operands"). All three real operands in every shape are renamed
   [rd]/[rs2]/[rs1], the same convention {!r_type_gpr_form} uses regardless
   of a field's own riscv-opcodes name; [rs1] is a vector register in the
   [.vv] shape, a GPR in [.vx], and a signed 5-bit immediate in [.vi]. The
   fourth riscv-opcodes variable field, [vm], is a mask-select bit, not a
   value-carrying operand - GAS's own optional trailing [, v0.t] is left to
   the encoder to model, the same way {!vsetvli_form} leaves its
   keyword-list [vtype] syntax to the encoder. *)
let opivv_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = vreg (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of_mnemonic ~mnemonic rec_;
          encoding;
          operands = [ rd; rs1; rs2 ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "rs1" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, vs2, vs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1";
              };
              {
                label = Inferred;
                note =
                  "GAS accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics = [];
        }

let opivx_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of_mnemonic ~mnemonic rec_;
          encoding;
          operands = [ rd; rs1; rs2 ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "rs1" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, vs2, rs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1";
              };
              {
                label = Inferred;
                note =
                  "GAS accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics = [];
        }

(* [vsll.vi]/[vsrl.vi]/[vsra.vi] carry riscv-opcodes' own "zimm5" field - an
   UNSIGNED 5-bit shift amount (0..31) - rather than every other [.vi]
   mnemonic's "simm5" (-16..15); real GNU as's own rejection message
   confirms the range split ("bad value for vector immediate field, value
   must be 0...31" versus "...-16...15"). {!opivi_form} takes the field
   name/signedness as parameters instead of hardcoding "simm5" so both
   variants share one function. *)
let opivi_zimm5_mnemonics =
  [
    "vsll.vi";
    "vsrl.vi";
    "vsra.vi";
    "vnsrl.wi";
    "vnsra.wi";
    "vnclipu.wi";
    "vnclip.wi";
    "vssrl.vi";
    "vssra.vi";
    "vrgather.vi";
    "vslideup.vi";
    "vslidedown.vi";
    "vwsll.vi";
  ]

let opivi_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let unsigned = List.mem mnemonic opivi_zimm5_mnemonics in
      let imm_name = if unsigned then "zimm5" else "simm5" in
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let imm =
        {
          op_name = imm_name;
          op_kind =
            Immediate
              {
                width_bits = 5;
                signed = not unsigned;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [
                    { field_name = imm_name; field_hi = 4; field_lo = 0; dest_hi = 4; dest_lo = 0 };
                  ];
              };
          role = In;
          explicit = true;
        }
      in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; imm; rs2 ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand imm_name ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, vs2, " ^ imm_name ^ " taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "GAS accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics = [];
        }

(* Zvbb's [vror.vi]: OPIVI's shape ([rd, rs2, imm], real selectable [vm])
   but with a genuinely new immediate encoding - riscv-opcodes splits the
   6-bit unsigned rotate amount across TWO non-adjacent fields,
   [zimm6hi] (1 bit, occupying the bit position every other [.vi]
   mnemonic's [vm] would sit at one position higher) and [zimm6lo] (5
   bits, the usual immediate field position) - real GNU as's own range
   message confirms 6 bits ("bad value for vector immediate field, value
   must be 0...63", not "...0...31"). This is the same "multiple raw
   fields concatenate into one logical operand" shape {!sw_form}'s own
   [imm12hi]/[imm12lo] split already established, just unsigned and
   high-part-first is reversed here (hi is the single top bit, lo the
   low five) - modeled as one logical [zimm6] operand via two [runs]
   entries rather than as two separate immediates. *)
let vror_vi_form (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err "vror.vi-not-fixed-bits" msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let imm =
        {
          op_name = "zimm6";
          op_kind =
            Immediate
              {
                width_bits = 6;
                signed = false;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [
                    { field_name = "zimm6hi"; field_hi = 0; field_lo = 0; dest_hi = 5; dest_lo = 5 };
                    { field_name = "zimm6lo"; field_hi = 4; field_lo = 0; dest_hi = 4; dest_lo = 0 };
                  ];
              };
          role = In;
          explicit = true;
        }
      in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:vror.vi";
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of_mnemonic ~mnemonic:"vror.vi" rec_;
          encoding;
          operands = [ rd; imm; rs2 ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic = "vror.vi";
              operands = [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "zimm6" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, vs2, zimm6hi, zimm6lo taken verbatim from encoding.fields; \
                   zimm6hi/zimm6lo concatenate (hi first) into one logical 6-bit zimm6 operand";
              };
              {
                label = Inferred;
                note =
                  "GAS accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics = [];
        }

(* The add-with-carry/subtract-with-borrow family - [vadc]/[vmadc]/[vsbc]/
   [vmsbc] - is structurally the same OPIVV/OPIVX/OPIVI shape
   {!opivv_form}/{!opivx_form}/{!opivi_form} already model, but [vm] is
   never a toggleable mask here (see {!Riscv_family_encode}'s own
   observation that riscv-opcodes' mask covers bit 25 for every one of
   these 15 records, rather than leaving it free). The "m"-suffixed
   variants ([vadc.vvm]/[vmadc.vvm]/[vsbc.vvm]/[vmsbc.vvm] and their
   [.vxm]/[.vim] siblings) have a mandatory, real 4th operand - GAS's
   literal [v0] supplying the carry/borrow input, modeled here as a real
   vector-register operand (not left to the encoder as a fixed constant,
   since it genuinely appears in GAS's own text syntax) - so these need
   their own form functions rather than reusing {!opivv_form}/etc., which
   would otherwise both omit this operand and falsely claim an optional
   [, v0.t] mask exists. *)
let carry_m_vv_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = vreg (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      let vcarry = { op_name = "vcarry"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs1; rs2; vcarry ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "rs1"; Syn_operand "vcarry" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, vs2, vs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1";
              };
              {
                label = Inferred;
                note =
                  "vm is architecturally fixed at 0 for this add-with-carry/subtract-with-borrow \
                   family; GAS requires a mandatory, literal fourth operand (\"v0\", not the \
                   \"v0.t\" mask-toggle sigil) supplying the carry/borrow input - modeled here as \
                   a real operand, unlike every other family's optional trailing mask";
              };
            ];
          diagnostics = [];
        }

let carry_m_vx_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      let vcarry = { op_name = "vcarry"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs1; rs2; vcarry ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "rs1"; Syn_operand "vcarry" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, vs2, rs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1";
              };
              {
                label = Inferred;
                note =
                  "vm is architecturally fixed at 0 for this add-with-carry/subtract-with-borrow \
                   family; GAS requires a mandatory, literal fourth operand (\"v0\", not the \
                   \"v0.t\" mask-toggle sigil) supplying the carry/borrow input - modeled here as \
                   a real operand, unlike every other family's optional trailing mask";
              };
            ];
          diagnostics = [];
        }

(* [vfmerge.vfm]: the FPR-typed mirror of {!carry_m_vx_form} - the
   identical mandatory-literal-`v0` shape, but [rs1] is a floating-point
   register rather than a GPR. *)
let carry_m_vf_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      let vcarry = { op_name = "vcarry"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs1; rs2; vcarry ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "rs1"; Syn_operand "vcarry" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, vs2, rs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1; rs1 is a floating-point register, not a GPR";
              };
              {
                label = Inferred;
                note =
                  "vm is architecturally fixed at 0 for this family; GAS requires a mandatory, \
                   literal fourth operand (\"v0\", not the \"v0.t\" mask-toggle sigil) supplying \
                   the merge-select input - modeled here as a real operand, unlike every other \
                   family's optional trailing mask";
              };
            ];
          diagnostics = [];
        }

let carry_m_vi_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let imm =
        {
          op_name = "simm5";
          op_kind =
            Immediate
              {
                width_bits = 5;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [ { field_name = "simm5"; field_hi = 4; field_lo = 0; dest_hi = 4; dest_lo = 0 } ];
              };
          role = In;
          explicit = true;
        }
      in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      let vcarry = { op_name = "vcarry"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; imm; rs2; vcarry ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands =
                [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "simm5"; Syn_operand "vcarry" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vd, vs2, simm5 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "vm is architecturally fixed at 0 for this add-with-carry/subtract-with-borrow \
                   family; GAS requires a mandatory, literal fourth operand (\"v0\", not the \
                   \"v0.t\" mask-toggle sigil) supplying the carry/borrow input - modeled here as \
                   a real operand, unlike every other family's optional trailing mask";
              };
            ];
          diagnostics = [];
        }

(* [vmadc.vv]/[.vx]/[.vi], [vmsbc.vv]/[.vx]: the bare (non-"m") compare-
   with-carry/borrow siblings - [vadc]/[vsbc] have no bare form since
   add/subtract always need a real carry/borrow input, but compare-with-
   carry/borrow can run with none, producing a carry/borrow-out mask with
   [vm] fixed at 1. {!opivv_form}/{!opivx_form}/{!opivi_form} don't fit
   here either: their own "optional trailing mask" fact would be false -
   no masked sibling exists for these mnemonics, matching {!mm_form}'s
   own precedent for a different OPMVV family. *)
let carry_vv_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = vreg (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
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
              operands = [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "rs1" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, vs2, vs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1";
              };
              {
                label = Inferred;
                note =
                  "vm is architecturally fixed at 1 for this bare (non-\"m\") \
                   compare-with-carry/borrow form - unlike every other OPIVV/OPIVX/OPIVI mnemonic \
                   there is no masked (, v0.t) sibling and no carry-in operand; confirmed real GNU \
                   as rejects both as illegal operands";
              };
            ];
          diagnostics = [];
        }

let carry_vx_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
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
              operands = [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "rs1" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, vs2, rs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1";
              };
              {
                label = Inferred;
                note =
                  "vm is architecturally fixed at 1 for this bare (non-\"m\") \
                   compare-with-carry/borrow form - unlike every other OPIVV/OPIVX/OPIVI mnemonic \
                   there is no masked (, v0.t) sibling and no carry-in operand; confirmed real GNU \
                   as rejects both as illegal operands";
              };
            ];
          diagnostics = [];
        }

let carry_vi_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let imm =
        {
          op_name = "simm5";
          op_kind =
            Immediate
              {
                width_bits = 5;
                signed = true;
                implicit_low_zero_bits = 0;
                nonzero = false;
                runs =
                  [ { field_name = "simm5"; field_hi = 4; field_lo = 0; dest_hi = 4; dest_lo = 0 } ];
              };
          role = In;
          explicit = true;
        }
      in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; imm; rs2 ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "simm5" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vd, vs2, simm5 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "vm is architecturally fixed at 1 for this bare (non-\"m\") \
                   compare-with-carry/borrow form - unlike every other OPIVV/OPIVX/OPIVI mnemonic \
                   there is no masked (, v0.t) sibling and no carry-in operand; confirmed real GNU \
                   as rejects both as illegal operands";
              };
            ];
          diagnostics = [];
        }

(* Zvkg's [vghsh.vv]/[vgmul.vv]: vector crypto's own major opcode 0x77 (not
   OP-V's 0x57), a genuinely new instruction space this repository has not
   modeled before. Unlike every OPIVV/OPMVV/OPFVV shape elsewhere in this
   file, this space has no mask bit at all - bit 25 (where [vm] would sit)
   is a fixed constant 1, not a toggle, and real GNU as rejects a trailing
   [, v0.t] outright ("illegal operands") rather than defaulting it, the
   same way [vlm.v]/[vsm.v]/the whole-register load/store family reject a
   mask suffix. [zvk_ternary_form] is [vghsh.vv]'s plain three-vector-
   register shape ([vd, vs2, vs1]); [zvk_unary_form] is [vgmul.vv]'s
   two-register shape with [vs1]'s field position a fixed per-mnemonic
   constant, the same "fixed field, not a real operand" precedent
   {!vext_form} below already established for OP-V proper. *)
let zvk_ternary_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = vreg (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of_mnemonic ~mnemonic rec_;
          encoding;
          operands = [ rd; rs1; rs2 ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "rs1" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, vs2, vs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1";
              };
              {
                label = Inferred;
                note =
                  "this instruction space (major opcode 0x77) has no mask bit at all - GAS rejects \
                   a trailing \", v0.t\" outright, unlike OP-V proper";
              };
            ];
          diagnostics = [];
        }

let zvk_unary_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of_mnemonic ~mnemonic rec_;
          encoding;
          operands = [ rd; rs2 ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "rs2" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vd, vs2 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "the vs1 field position is a fixed per-mnemonic constant, not a genuine operand \
                   - GAS syntax is \"mnemonic rd, rs2\" with nothing written there; the encoder \
                   supplies the constant";
              };
              {
                label = Inferred;
                note =
                  "this instruction space (major opcode 0x77) has no mask bit at all - GAS rejects \
                   a trailing \", v0.t\" outright, unlike OP-V proper";
              };
            ];
          diagnostics = [];
        }

(* Zvksed's [vsm4k.vi]: the same opcode-0x77, no-mask shape as
   [vghsh.vv]/[vsha2ms.vv] above, but with an UNSIGNED 5-bit immediate
   (riscv-opcodes' own "zimm5" field, 0..31 - confirmed by real GNU as's
   own rejection message, "bad value for vector immediate field, value
   must be 0...31") in [vs1]'s field position instead of a third vector
   register - the same field {!opivi_form}'s own [opivi_zimm5_mnemonics]
   branch already models for OP-V proper, just under this opcode/no-mask
   space instead. *)
let zvk_zimm5_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let imm =
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
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of_mnemonic ~mnemonic rec_;
          encoding;
          operands = [ rd; rs2; imm ];
          syntax =
            {
              dialect = "gas-att";
              mnemonic;
              operands = [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "zimm5" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vd, vs2, zimm5 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "this instruction space (major opcode 0x77) has no mask bit at all - GAS rejects \
                   a trailing \", v0.t\" outright, unlike OP-V proper";
              };
            ];
          diagnostics = [];
        }

(* [vsext.vf2]/[vf4]/[vf8], [vzext.vf2]/[vf4]/[vf8]: OP-V's integer
   sign-/zero-extend family - a genuinely new two-vector-register shape
   ([rd, rs2], no third operand at all). Under the same OPMVV major
   opcode/funct3 as [vmul]/etc., riscv-opcodes' own encoding.fields lists
   only [vm]/[vs2]/[vd] as variable - the field every other OPMVV/OPIVV
   mnemonic uses for [vs1]/[rs1] is entirely fixed per mnemonic (a
   source-width-divisor selector), not a real operand, so the encoder
   supplies it as a constant rather than this normalization layer
   modeling a fourth pseudo-operand. *)
let vext_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of_mnemonic ~mnemonic rec_;
          encoding;
          operands = [ rd; rs2 ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "rs2" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vd, vs2 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "the vs1 field position is a fixed per-mnemonic constant, not a genuine operand \
                   - GAS syntax is \"mnemonic rd, rs2\" with nothing written there; the encoder \
                   supplies the constant";
              };
              {
                label = Inferred;
                note =
                  "GAS accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics = [];
        }

(* [vcpop.m]/[vfirst.m]: the mask-population-count/first-set-bit-index
   pair - {!vext_form}'s exact "rd, vs2" shape, but with a GPR destination
   (riscv-opcodes' own field is literally named "rd", not "vd") rather
   than a vector-register one. *)
let v_to_x_unary_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs2 ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "rs2" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vd, vs2 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "the vs1 field position is a fixed per-mnemonic constant, not a genuine operand \
                   - GAS syntax is \"mnemonic rd, rs2\" with nothing written there; the encoder \
                   supplies the constant";
              };
              {
                label = Inferred;
                note =
                  "GAS accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics = [];
        }

(* [vmv.x.s]: the same OPMVV GPR-destination two-operand shape
   {!v_to_x_unary_form} models, but with no masked sibling at all - real
   GNU as rejects `vmv.x.s a0,v2,v0.t`, so reusing {!v_to_x_unary_form}
   would falsely claim an optional trailing mask exists. *)
let mv_x_s_form (rec_ : R.t) =
  let mnemonic = "vmv.x.s" in
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = gpr (); role = Out; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs2 ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "rs2" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vd, vs2 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "the vs1 field position is a fixed constant and vm is architecturally fixed at 1 \
                   - unlike vcpop.m/vfirst.m's own shape, no masked (, v0.t) sibling exists; \
                   confirmed real GNU as rejects one as illegal operands";
              };
            ];
          diagnostics = [];
        }

(* [vmv.s.x]: the mirror-image shape of {!mv_x_s_form} - a vector
   destination and a GPR source, with the same fixed-[vs2]-constant,
   no-masked-sibling discipline. *)
let mv_s_x_form (rec_ : R.t) =
  let mnemonic = "vmv.s.x" in
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
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
                note = "operand fields vd, rs1 taken verbatim from encoding.fields, renamed rd/rs1";
              };
              {
                label = Inferred;
                note =
                  "the vs2 field position is a fixed constant and vm is architecturally fixed at 1 \
                   - no masked (, v0.t) sibling exists; confirmed real GNU as rejects one as \
                   illegal operands";
              };
            ];
          diagnostics = [];
        }

(* [vfmv.f.s]: the FPR-typed mirror of {!mv_x_s_form} - the identical
   shape, but the destination is a floating-point register rather than a
   GPR (riscv-opcodes' own field is still named "rd"). *)
let vfmv_f_s_form (rec_ : R.t) =
  let mnemonic = "vfmv.f.s" in
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = fpr (); role = Out; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs2 ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "rs2" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vd, vs2 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "the vs1 field position is a fixed constant and vm is architecturally fixed at 1 \
                   - no masked (, v0.t) sibling exists; confirmed real GNU as rejects one as \
                   illegal operands";
              };
            ];
          diagnostics = [];
        }

(* [vfmv.s.f]: the mirror-image shape of {!vfmv_f_s_form} - a vector
   destination and a floating-point-register source, with the same
   fixed-[vs2]-constant, no-masked-sibling discipline as {!mv_s_x_form}. *)
let vfmv_s_f_form (rec_ : R.t) =
  let mnemonic = "vfmv.s.f" in
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
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
                note = "operand fields vd, rs1 taken verbatim from encoding.fields, renamed rd/rs1";
              };
              {
                label = Inferred;
                note =
                  "the vs2 field position is a fixed constant and vm is architecturally fixed at 1 \
                   - no masked (, v0.t) sibling exists; confirmed real GNU as rejects one as \
                   illegal operands";
              };
            ];
          diagnostics = [];
        }

(* [vmv.v.v]/[.v.x]/[.v.i]/[vfmv.v.f]: OP-V's unconditional-move family - a
   genuinely new two-operand shape: unlike every other OPIVV/OPIVX/OPIVI
   mnemonic there is no [vs2] operand at all (its field position is a
   fixed constant), and [vm] is fixed at 1 with no masked sibling
   (confirmed real GNU as rejects `vmv.v.v v1,v2,v0.t`). [vfmv.v.f] shares
   this exact shape under OPFVF instead of OPIVX, with an FPR [rs1]. *)
let vmv_v_form ~mnemonic ~(rs1_kind : [ `Vreg | `Gpr | `Imm | `Fpr ]) (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1_name, rs1 =
        match rs1_kind with
        | `Vreg -> ("rs1", { op_name = "rs1"; op_kind = vreg (); role = In; explicit = true })
        | `Gpr -> ("rs1", { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true })
        | `Fpr -> ("rs1", { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true })
        | `Imm ->
            ( "simm5",
              {
                op_name = "simm5";
                op_kind =
                  Immediate
                    {
                      width_bits = 5;
                      signed = true;
                      implicit_low_zero_bits = 0;
                      nonzero = false;
                      runs =
                        [
                          {
                            field_name = "simm5";
                            field_hi = 4;
                            field_lo = 0;
                            dest_hi = 4;
                            dest_lo = 0;
                          };
                        ];
                    };
                role = In;
                explicit = true;
              } )
      in
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
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand rs1_name ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vd, " ^ rs1_name ^ " taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "the vs2 field position is a fixed constant (unlike every other \
                   OPIVV/OPIVX/OPIVI mnemonic) and vm is architecturally fixed at 1 - no masked (, \
                   v0.t) sibling exists; confirmed real GNU as rejects one as illegal operands";
              };
            ];
          diagnostics = [];
        }

(* [vmv1r.v]/[vmv2r.v]/[vmv4r.v]/[vmv8r.v]: OP-V's whole-register-group
   move family - shares {!vext_form}'s exact "rd, rs2" shape, but with no
   masked sibling (confirmed real GNU as rejects `vmv1r.v v1,v2,v0.t`),
   so reusing {!vext_form} would falsely claim an optional trailing mask
   exists. *)
let whole_reg_move_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd; rs2 ];
          syntax =
            { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd"; Syn_operand "rs2" ] };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note = "operand fields vd, vs2 taken verbatim from encoding.fields";
              };
              {
                label = Inferred;
                note =
                  "the field position every other OPIVI mnemonic uses for its immediate instead \
                   holds a fixed per-mnemonic constant (register-group count minus one), and vm is \
                   architecturally fixed at 1 - no masked (, v0.t) sibling exists; confirmed real \
                   GNU as rejects one as illegal operands, and rejects no register-alignment \
                   requirement either (e.g. `vmv2r.v v1,v2` assembles despite v1 not being \
                   2-aligned)";
              };
            ];
          diagnostics = [];
        }

(* [vid.v]: OP-V's element-index instruction - the first family surveyed
   with no [vs2]/[vs1]/[rs1] operand at all, just a destination and an
   optional mask; {!vext_form}'s own "rd, vs2" shape doesn't fit since
   there is no second operand either. *)
let vid_form (rec_ : R.t) =
  let mnemonic = "vid.v" in
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      Ok
        {
          form_id = "riscv:" ^ mnemonic;
          arch = Riscv;
          native_name = rec_.native_name;
          source_record_ids = [ rec_.record_id ];
          requirement = requirement_of rec_;
          encoding;
          operands = [ rd ];
          syntax = { dialect = "gas-att"; mnemonic; operands = [ Syn_operand "rd" ] };
          concreteness = Concrete;
          facts =
            [
              { label = Upstream; note = "operand field vd taken verbatim from encoding.fields" };
              {
                label = Inferred;
                note =
                  "the vs1 and vs2 field positions are both fixed per-mnemonic constants, not \
                   genuine operands - GAS syntax is \"vid.v rd\" with nothing written there; the \
                   encoder supplies the constants";
              };
              {
                label = Inferred;
                note =
                  "GAS accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics = [];
        }

(* [vmand]/[vmandn]/[vmor]/[vmxor]/[vmorn]/[vmnand]/[vmnor]/[vmxnor]: OP-V's
   mask-register logical family ([.mm]) - structurally identical to
   {!opivv_form}'s three-vector-register shape ([rd, rs2, rs1] from
   [vd, vs2, vs1]), but unlike every OPIVV/OPIVX/OPIVI/OPMVV/OPMVX mnemonic,
   [vm] is architecturally fixed at 1 here: real GNU as rejects a trailing
   [, v0.t] as "illegal operands" (confirmed), so this needs its own form
   rather than reusing {!opivv_form}, which would otherwise state a false
   "optional trailing mask operand" fact. *)
let mm_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = vreg (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
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
              operands = [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "rs1" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, vs2, vs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1";
              };
              {
                label = Inferred;
                note =
                  "vm is architecturally fixed at 1 for this mask-register-logical family - unlike \
                   every other OPIVV/OPIVX/OPIVI/OPMVV/OPMVX mnemonic there is no masked (, v0.t) \
                   sibling; confirmed real GNU as rejects one as illegal operands";
              };
            ];
          diagnostics = [];
        }

let mm_mnemonics =
  [
    "vmand.mm"; "vmandn.mm"; "vmor.mm"; "vmxor.mm"; "vmorn.mm"; "vmnand.mm"; "vmnor.mm"; "vmxnor.mm";
  ]

let opivv_mnemonics =
  [
    "vadd.vv";
    "vsub.vv";
    "vand.vv";
    "vor.vv";
    "vxor.vv";
    "vsll.vv";
    "vsrl.vv";
    "vsra.vv";
    "vminu.vv";
    "vmin.vv";
    "vmaxu.vv";
    "vmax.vv";
    "vsaddu.vv";
    "vsadd.vv";
    "vssubu.vv";
    "vssub.vv";
    "vnsrl.wv";
    "vnsra.wv";
    "vnclipu.wv";
    "vnclip.wv";
    "vssrl.vv";
    "vssra.vv";
    "vrgather.vv";
    "vrgatherei16.vv";
    "vwredsumu.vs";
    "vwredsum.vs";
    "vmseq.vv";
    "vmsne.vv";
    "vmsltu.vv";
    "vmslt.vv";
    "vmsleu.vv";
    "vmsle.vv";
    "vsmul.vv";
    "vandn.vv";
    "vrol.vv";
    "vror.vv";
    "vwsll.vv";
  ]

let opivx_mnemonics =
  [
    "vadd.vx";
    "vsub.vx";
    "vrsub.vx";
    "vand.vx";
    "vor.vx";
    "vxor.vx";
    "vsll.vx";
    "vsrl.vx";
    "vsra.vx";
    "vminu.vx";
    "vmin.vx";
    "vmaxu.vx";
    "vmax.vx";
    "vsaddu.vx";
    "vsadd.vx";
    "vssubu.vx";
    "vssub.vx";
    "vnsrl.wx";
    "vnsra.wx";
    "vnclipu.wx";
    "vnclip.wx";
    "vssrl.vx";
    "vssra.vx";
    "vrgather.vx";
    "vmseq.vx";
    "vmsne.vx";
    "vmsltu.vx";
    "vmslt.vx";
    "vmsleu.vx";
    "vmsle.vx";
    "vmsgtu.vx";
    "vmsgt.vx";
    "vslideup.vx";
    "vslidedown.vx";
    "vsmul.vx";
    "vandn.vx";
    "vrol.vx";
    "vror.vx";
    "vwsll.vx";
  ]

let opivi_mnemonics =
  [
    "vadd.vi";
    "vrsub.vi";
    "vand.vi";
    "vor.vi";
    "vxor.vi";
    "vsaddu.vi";
    "vsadd.vi";
    "vmseq.vi";
    "vmsne.vi";
    "vmsleu.vi";
    "vmsle.vi";
    "vmsgtu.vi";
    "vmsgt.vi";
  ]
  @ opivi_zimm5_mnemonics

(* OP-V's OPMVV (funct3 = 2)/OPMVX (funct3 = 6) shapes: the same
   all-vector-register/scalar-broadcast operand layout as OPIVV/OPIVX, so
   {!opivv_form}/{!opivx_form} are reused unchanged - only funct3 (an
   encoder-side concern the normalized model does not represent) differs. *)
let opmvv_mnemonics =
  [
    "vmul.vv";
    "vmulh.vv";
    "vmulhu.vv";
    "vmulhsu.vv";
    "vdivu.vv";
    "vdiv.vv";
    "vremu.vv";
    "vrem.vv";
    "vaaddu.vv";
    "vaadd.vv";
    "vasubu.vv";
    "vasub.vv";
    "vwaddu.vv";
    "vwadd.vv";
    "vwsubu.vv";
    "vwsub.vv";
    "vwaddu.wv";
    "vwadd.wv";
    "vwsubu.wv";
    "vwsub.wv";
    "vwmulu.vv";
    "vwmulsu.vv";
    "vwmul.vv";
    "vredsum.vs";
    "vredand.vs";
    "vredor.vs";
    "vredxor.vs";
    "vredminu.vs";
    "vredmin.vs";
    "vredmaxu.vs";
    "vredmax.vs";
    "vclmul.vv";
    "vclmulh.vv";
  ]

let opmvx_mnemonics =
  [
    "vmul.vx";
    "vmulh.vx";
    "vmulhu.vx";
    "vmulhsu.vx";
    "vdivu.vx";
    "vdiv.vx";
    "vremu.vx";
    "vrem.vx";
    "vaaddu.vx";
    "vaadd.vx";
    "vasubu.vx";
    "vasub.vx";
    "vwaddu.vx";
    "vwadd.vx";
    "vwsubu.vx";
    "vwsub.vx";
    "vwaddu.wx";
    "vwadd.wx";
    "vwsubu.wx";
    "vwsub.wx";
    "vwmulu.vx";
    "vwmulsu.vx";
    "vwmul.vx";
    "vslide1up.vx";
    "vslide1down.vx";
    "vclmul.vx";
    "vclmulh.vx";
  ]

(* OP-V's OPFVV (funct3 = 1) shape - the entry point into the floating-point
   arithmetic space - is the identical all-vector-register [rd, rs2, rs1]
   shape {!opivv_form} already models (only funct3, an encoder-side concern,
   differs), so it is reused unchanged, the same way {!opmvv_mnemonics}
   reuses it for OPMVV. *)
let opfvv_mnemonics =
  [
    "vfadd.vv";
    "vfsub.vv";
    "vfmul.vv";
    "vfdiv.vv";
    "vfmin.vv";
    "vfmax.vv";
    "vfsgnj.vv";
    "vfsgnjn.vv";
    "vfsgnjx.vv";
    "vfredosum.vs";
    "vfredusum.vs";
    "vfredmin.vs";
    "vfredmax.vs";
    "vmfeq.vv";
    "vmfle.vv";
    "vmflt.vv";
    "vmfne.vv";
    "vfwadd.vv";
    "vfwadd.wv";
    "vfwsub.vv";
    "vfwsub.wv";
    "vfwmul.vv";
    "vfwredosum.vs";
    "vfwredusum.vs";
  ]

(* OPFVF (funct3 = 5)'s scalar-broadcast shape differs from {!opivx_form}'s
   OPIVX/OPMVX shape in exactly one respect: the scalar operand ([rs1]) is a
   floating-point register ({!fpr}), not a GPR. *)
let opfvf_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
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
              operands = [ Syn_operand "rd"; Syn_operand "rs2"; Syn_operand "rs1" ];
            };
          concreteness = Concrete;
          facts =
            [
              {
                label = Upstream;
                note =
                  "operand fields vd, vs2, rs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1; rs1 is a floating-point register, not a GPR";
              };
              {
                label = Inferred;
                note =
                  "GAS accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics = [];
        }

let opfvf_mnemonics =
  [
    "vfadd.vf";
    "vfsub.vf";
    "vfrsub.vf";
    "vfmul.vf";
    "vfdiv.vf";
    "vfrdiv.vf";
    "vfmin.vf";
    "vfmax.vf";
    "vfsgnj.vf";
    "vfsgnjn.vf";
    "vfsgnjx.vf";
    "vmfeq.vf";
    "vmfle.vf";
    "vmflt.vf";
    "vmfne.vf";
    "vmfgt.vf";
    "vmfge.vf";
    "vfslide1up.vf";
    "vfslide1down.vf";
    "vfwadd.vf";
    "vfwadd.wf";
    "vfwsub.vf";
    "vfwsub.wf";
    "vfwmul.vf";
  ]

(* The multiply-accumulate family - [vmacc]/[vnmsac]/[vmadd]/[vnmsub] and
   their widening siblings [vwmaccu]/[vwmacc]/[vwmaccsu]/[vwmaccus] - is
   structurally the same OPMVV/OPMVX shape {!opivv_form}/{!opivx_form}
   already model, but real GNU as swaps the last two text operands: [vd,
   rs1, rs2] rather than {!opivv_form}/{!opivx_form}'s [vd, rs2, rs1] -
   confirmed by decoding the assembled word's own vs1/vs2 field bits (not
   just accepted/rejected status): `vmacc.vv v1,v2,v3` places `v2` in the
   vs1 field position and `v3` in the vs2 one. Needs its own form functions
   rather than reusing {!opivv_form}/{!opivx_form} to get that operand
   order right. *)
let opmacc_vv_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = vreg (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
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
                note =
                  "operand fields vd, vs2, vs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1";
              };
              {
                label = Inferred;
                note =
                  "GAS's text operand order is \"mnemonic rd, rs1, rs2\", swapping the last two \
                   operands relative to every other OPMVV/OPMVX mnemonic's \"rd, rs2, rs1\"";
              };
              {
                label = Inferred;
                note =
                  "GAS accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics = [];
        }

let opmacc_vx_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = gpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
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
                note =
                  "operand fields vd, vs2, rs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1";
              };
              {
                label = Inferred;
                note =
                  "GAS's text operand order is \"mnemonic rd, rs1, rs2\", swapping the last two \
                   operands relative to every other OPMVV/OPMVX mnemonic's \"rd, rs2, rs1\"";
              };
              {
                label = Inferred;
                note =
                  "GAS accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics = [];
        }

(* [vfmadd]/[vfnmadd]/[vfmsub]/[vfnmsub]/[vfmacc]/[vfnmacc]/[vfmsac]/
   [vfnmsac] `.vf`: the floating FMA family's scalar-broadcast form -
   {!opmacc_vx_form}'s exact reordered-operand shape, but [rs1] is a
   floating-point register rather than a GPR. *)
let opfmacc_vf_form ~mnemonic (rec_ : R.t) =
  match riscv_encoding_of rec_ with
  | Error msg -> err (mnemonic ^ "-not-fixed-bits") msg
  | Ok encoding ->
      let rd = { op_name = "rd"; op_kind = vreg (); role = Out; explicit = true } in
      let rs1 = { op_name = "rs1"; op_kind = fpr (); role = In; explicit = true } in
      let rs2 = { op_name = "rs2"; op_kind = vreg (); role = In; explicit = true } in
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
                note =
                  "operand fields vd, vs2, rs1 taken verbatim from encoding.fields, renamed \
                   rd/rs2/rs1; rs1 is a floating-point register, not a GPR";
              };
              {
                label = Inferred;
                note =
                  "GAS's text operand order is \"mnemonic rd, rs1, rs2\", swapping the last two \
                   operands relative to every other OPFVV/OPFVF mnemonic's \"rd, rs2, rs1\"";
              };
              {
                label = Inferred;
                note =
                  "GAS accepts an optional trailing mask operand (\", v0.t\") selecting vm=0 - not \
                   modeled here, owned by the encoder";
              };
            ];
          diagnostics = [];
        }

let opfmacc_vv_mnemonics =
  [
    "vfmadd.vv";
    "vfnmadd.vv";
    "vfmsub.vv";
    "vfnmsub.vv";
    "vfmacc.vv";
    "vfnmacc.vv";
    "vfmsac.vv";
    "vfnmsac.vv";
    "vfwmacc.vv";
    "vfwnmacc.vv";
    "vfwmsac.vv";
    "vfwnmsac.vv";
    "vfwmaccbf16.vv";
  ]

let opfmacc_vf_mnemonics =
  [
    "vfmadd.vf";
    "vfnmadd.vf";
    "vfmsub.vf";
    "vfnmsub.vf";
    "vfmacc.vf";
    "vfnmacc.vf";
    "vfmsac.vf";
    "vfnmsac.vf";
    "vfwmacc.vf";
    "vfwnmacc.vf";
    "vfwmsac.vf";
    "vfwnmsac.vf";
    "vfwmaccbf16.vf";
  ]

let opmacc_vv_mnemonics =
  [ "vmacc.vv"; "vnmsac.vv"; "vmadd.vv"; "vnmsub.vv"; "vwmaccu.vv"; "vwmacc.vv"; "vwmaccsu.vv" ]

let opmacc_vx_mnemonics =
  [
    "vmacc.vx";
    "vnmsac.vx";
    "vmadd.vx";
    "vnmsub.vx";
    "vwmaccu.vx";
    "vwmacc.vx";
    "vwmaccsu.vx";
    "vwmaccus.vx";
  ]

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
  | "vsetvli" -> vsetvli_form rec_
  | "vsetivli" -> vsetivli_form rec_
  | "vadc.vvm" -> carry_m_vv_form ~mnemonic:"vadc.vvm" rec_
  | "vadc.vxm" -> carry_m_vx_form ~mnemonic:"vadc.vxm" rec_
  | "vadc.vim" -> carry_m_vi_form ~mnemonic:"vadc.vim" rec_
  | "vmadc.vvm" -> carry_m_vv_form ~mnemonic:"vmadc.vvm" rec_
  | "vmadc.vxm" -> carry_m_vx_form ~mnemonic:"vmadc.vxm" rec_
  | "vmadc.vim" -> carry_m_vi_form ~mnemonic:"vmadc.vim" rec_
  | "vmadc.vv" -> carry_vv_form ~mnemonic:"vmadc.vv" rec_
  | "vmadc.vx" -> carry_vx_form ~mnemonic:"vmadc.vx" rec_
  | "vmadc.vi" -> carry_vi_form ~mnemonic:"vmadc.vi" rec_
  | "vsbc.vvm" -> carry_m_vv_form ~mnemonic:"vsbc.vvm" rec_
  | "vsbc.vxm" -> carry_m_vx_form ~mnemonic:"vsbc.vxm" rec_
  | "vmsbc.vvm" -> carry_m_vv_form ~mnemonic:"vmsbc.vvm" rec_
  | "vmsbc.vxm" -> carry_m_vx_form ~mnemonic:"vmsbc.vxm" rec_
  | "vmsbc.vv" -> carry_vv_form ~mnemonic:"vmsbc.vv" rec_
  | "vmsbc.vx" -> carry_vx_form ~mnemonic:"vmsbc.vx" rec_
  | "vmerge.vvm" -> carry_m_vv_form ~mnemonic:"vmerge.vvm" rec_
  | "vmerge.vxm" -> carry_m_vx_form ~mnemonic:"vmerge.vxm" rec_
  | "vmerge.vim" -> carry_m_vi_form ~mnemonic:"vmerge.vim" rec_
  | "vmv.x.s" -> mv_x_s_form rec_
  | "vmv.s.x" -> mv_s_x_form rec_
  | "vmv.v.v" -> vmv_v_form ~mnemonic:"vmv.v.v" ~rs1_kind:`Vreg rec_
  | "vmv.v.x" -> vmv_v_form ~mnemonic:"vmv.v.x" ~rs1_kind:`Gpr rec_
  | "vmv.v.i" -> vmv_v_form ~mnemonic:"vmv.v.i" ~rs1_kind:`Imm rec_
  | "vfmv.f.s" -> vfmv_f_s_form rec_
  | "vfmv.s.f" -> vfmv_s_f_form rec_
  | "vfmv.v.f" -> vmv_v_form ~mnemonic:"vfmv.v.f" ~rs1_kind:`Fpr rec_
  | "vfmerge.vfm" -> carry_m_vf_form ~mnemonic:"vfmerge.vfm" rec_
  | "vmv1r.v" -> whole_reg_move_form ~mnemonic:"vmv1r.v" rec_
  | "vmv2r.v" -> whole_reg_move_form ~mnemonic:"vmv2r.v" rec_
  | "vmv4r.v" -> whole_reg_move_form ~mnemonic:"vmv4r.v" rec_
  | "vmv8r.v" -> whole_reg_move_form ~mnemonic:"vmv8r.v" rec_
  | mnemonic when List.mem mnemonic opivv_mnemonics -> opivv_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic opivx_mnemonics -> opivx_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic opivi_mnemonics -> opivi_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic opmvv_mnemonics -> opivv_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic opmvx_mnemonics -> opivx_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic opfvv_mnemonics -> opivv_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic opfvf_mnemonics -> opfvf_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic opmacc_vv_mnemonics -> opmacc_vv_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic opmacc_vx_mnemonics -> opmacc_vx_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic opfmacc_vv_mnemonics -> opmacc_vv_form ~mnemonic rec_
  | mnemonic when List.mem mnemonic opfmacc_vf_mnemonics -> opfmacc_vf_form ~mnemonic rec_
  | "vsext.vf2" -> vext_form ~mnemonic:"vsext.vf2" rec_
  | "vsext.vf4" -> vext_form ~mnemonic:"vsext.vf4" rec_
  | "vsext.vf8" -> vext_form ~mnemonic:"vsext.vf8" rec_
  | "vzext.vf2" -> vext_form ~mnemonic:"vzext.vf2" rec_
  | "vzext.vf4" -> vext_form ~mnemonic:"vzext.vf4" rec_
  | "vzext.vf8" -> vext_form ~mnemonic:"vzext.vf8" rec_
  | "viota.m" -> vext_form ~mnemonic:"viota.m" rec_
  | "vid.v" -> vid_form rec_
  | mnemonic when List.mem mnemonic mm_mnemonics -> mm_form ~mnemonic rec_
  | "vcompress.vm" -> mm_form ~mnemonic:"vcompress.vm" rec_
  | "vfsqrt.v" -> vext_form ~mnemonic:"vfsqrt.v" rec_
  | "vfrsqrt7.v" -> vext_form ~mnemonic:"vfrsqrt7.v" rec_
  | "vfrec7.v" -> vext_form ~mnemonic:"vfrec7.v" rec_
  | "vfclass.v" -> vext_form ~mnemonic:"vfclass.v" rec_
  | "vfcvt.xu.f.v" -> vext_form ~mnemonic:"vfcvt.xu.f.v" rec_
  | "vfcvt.x.f.v" -> vext_form ~mnemonic:"vfcvt.x.f.v" rec_
  | "vfcvt.f.xu.v" -> vext_form ~mnemonic:"vfcvt.f.xu.v" rec_
  | "vfcvt.f.x.v" -> vext_form ~mnemonic:"vfcvt.f.x.v" rec_
  | "vfcvt.rtz.xu.f.v" -> vext_form ~mnemonic:"vfcvt.rtz.xu.f.v" rec_
  | "vfcvt.rtz.x.f.v" -> vext_form ~mnemonic:"vfcvt.rtz.x.f.v" rec_
  | "vfwcvt.xu.f.v" -> vext_form ~mnemonic:"vfwcvt.xu.f.v" rec_
  | "vfwcvt.x.f.v" -> vext_form ~mnemonic:"vfwcvt.x.f.v" rec_
  | "vfwcvt.f.xu.v" -> vext_form ~mnemonic:"vfwcvt.f.xu.v" rec_
  | "vfwcvt.f.x.v" -> vext_form ~mnemonic:"vfwcvt.f.x.v" rec_
  | "vfwcvt.f.f.v" -> vext_form ~mnemonic:"vfwcvt.f.f.v" rec_
  | "vfwcvt.rtz.xu.f.v" -> vext_form ~mnemonic:"vfwcvt.rtz.xu.f.v" rec_
  | "vfwcvt.rtz.x.f.v" -> vext_form ~mnemonic:"vfwcvt.rtz.x.f.v" rec_
  | "vfncvt.xu.f.w" -> vext_form ~mnemonic:"vfncvt.xu.f.w" rec_
  | "vfncvt.x.f.w" -> vext_form ~mnemonic:"vfncvt.x.f.w" rec_
  | "vfncvt.f.xu.w" -> vext_form ~mnemonic:"vfncvt.f.xu.w" rec_
  | "vfncvt.f.x.w" -> vext_form ~mnemonic:"vfncvt.f.x.w" rec_
  | "vfncvt.f.f.w" -> vext_form ~mnemonic:"vfncvt.f.f.w" rec_
  | "vfncvt.rod.f.f.w" -> vext_form ~mnemonic:"vfncvt.rod.f.f.w" rec_
  | "vfncvt.rtz.xu.f.w" -> vext_form ~mnemonic:"vfncvt.rtz.xu.f.w" rec_
  | "vfncvt.rtz.x.f.w" -> vext_form ~mnemonic:"vfncvt.rtz.x.f.w" rec_
  | "vfwcvtbf16.f.f.v" -> vext_form ~mnemonic:"vfwcvtbf16.f.f.v" rec_
  | "vfncvtbf16.f.f.w" -> vext_form ~mnemonic:"vfncvtbf16.f.f.w" rec_
  | "vmsbf.m" -> vext_form ~mnemonic:"vmsbf.m" rec_
  | "vmsif.m" -> vext_form ~mnemonic:"vmsif.m" rec_
  | "vmsof.m" -> vext_form ~mnemonic:"vmsof.m" rec_
  | "vcpop.m" -> v_to_x_unary_form ~mnemonic:"vcpop.m" rec_
  | "vfirst.m" -> v_to_x_unary_form ~mnemonic:"vfirst.m" rec_
  | "vle8.v" -> v_load_form ~mnemonic:"vle8.v" rec_
  | "vle16.v" -> v_load_form ~mnemonic:"vle16.v" rec_
  | "vle32.v" -> v_load_form ~mnemonic:"vle32.v" rec_
  | "vle64.v" -> v_load_form ~mnemonic:"vle64.v" rec_
  | "vse8.v" -> v_store_form ~mnemonic:"vse8.v" rec_
  | "vse16.v" -> v_store_form ~mnemonic:"vse16.v" rec_
  | "vse32.v" -> v_store_form ~mnemonic:"vse32.v" rec_
  | "vse64.v" -> v_store_form ~mnemonic:"vse64.v" rec_
  | "vlm.v" -> vlm_form ~mnemonic:"vlm.v" rec_
  | "vsm.v" -> vsm_form ~mnemonic:"vsm.v" rec_
  | "vle8ff.v" -> v_load_form ~mnemonic:"vle8ff.v" rec_
  | "vle16ff.v" -> v_load_form ~mnemonic:"vle16ff.v" rec_
  | "vle32ff.v" -> v_load_form ~mnemonic:"vle32ff.v" rec_
  | "vle64ff.v" -> v_load_form ~mnemonic:"vle64ff.v" rec_
  | "vlse8.v" -> v_strided_load_form ~mnemonic:"vlse8.v" rec_
  | "vlse16.v" -> v_strided_load_form ~mnemonic:"vlse16.v" rec_
  | "vlse32.v" -> v_strided_load_form ~mnemonic:"vlse32.v" rec_
  | "vlse64.v" -> v_strided_load_form ~mnemonic:"vlse64.v" rec_
  | "vsse8.v" -> v_strided_store_form ~mnemonic:"vsse8.v" rec_
  | "vsse16.v" -> v_strided_store_form ~mnemonic:"vsse16.v" rec_
  | "vsse32.v" -> v_strided_store_form ~mnemonic:"vsse32.v" rec_
  | "vsse64.v" -> v_strided_store_form ~mnemonic:"vsse64.v" rec_
  | "vluxei8.v" -> v_indexed_load_form ~mnemonic:"vluxei8.v" rec_
  | "vluxei16.v" -> v_indexed_load_form ~mnemonic:"vluxei16.v" rec_
  | "vluxei32.v" -> v_indexed_load_form ~mnemonic:"vluxei32.v" rec_
  | "vluxei64.v" -> v_indexed_load_form ~mnemonic:"vluxei64.v" rec_
  | "vloxei8.v" -> v_indexed_load_form ~mnemonic:"vloxei8.v" rec_
  | "vloxei16.v" -> v_indexed_load_form ~mnemonic:"vloxei16.v" rec_
  | "vloxei32.v" -> v_indexed_load_form ~mnemonic:"vloxei32.v" rec_
  | "vloxei64.v" -> v_indexed_load_form ~mnemonic:"vloxei64.v" rec_
  | "vsuxei8.v" -> v_indexed_store_form ~mnemonic:"vsuxei8.v" rec_
  | "vsuxei16.v" -> v_indexed_store_form ~mnemonic:"vsuxei16.v" rec_
  | "vsuxei32.v" -> v_indexed_store_form ~mnemonic:"vsuxei32.v" rec_
  | "vsuxei64.v" -> v_indexed_store_form ~mnemonic:"vsuxei64.v" rec_
  | "vsoxei8.v" -> v_indexed_store_form ~mnemonic:"vsoxei8.v" rec_
  | "vsoxei16.v" -> v_indexed_store_form ~mnemonic:"vsoxei16.v" rec_
  | "vsoxei32.v" -> v_indexed_store_form ~mnemonic:"vsoxei32.v" rec_
  | "vsoxei64.v" -> v_indexed_store_form ~mnemonic:"vsoxei64.v" rec_
  | "vl1re8.v" -> vlm_form ~mnemonic:"vl1re8.v" rec_
  | "vl1re16.v" -> vlm_form ~mnemonic:"vl1re16.v" rec_
  | "vl1re32.v" -> vlm_form ~mnemonic:"vl1re32.v" rec_
  | "vl1re64.v" -> vlm_form ~mnemonic:"vl1re64.v" rec_
  | "vl2re8.v" -> vlm_form ~mnemonic:"vl2re8.v" rec_
  | "vl2re16.v" -> vlm_form ~mnemonic:"vl2re16.v" rec_
  | "vl2re32.v" -> vlm_form ~mnemonic:"vl2re32.v" rec_
  | "vl2re64.v" -> vlm_form ~mnemonic:"vl2re64.v" rec_
  | "vl4re8.v" -> vlm_form ~mnemonic:"vl4re8.v" rec_
  | "vl4re16.v" -> vlm_form ~mnemonic:"vl4re16.v" rec_
  | "vl4re32.v" -> vlm_form ~mnemonic:"vl4re32.v" rec_
  | "vl4re64.v" -> vlm_form ~mnemonic:"vl4re64.v" rec_
  | "vl8re8.v" -> vlm_form ~mnemonic:"vl8re8.v" rec_
  | "vl8re16.v" -> vlm_form ~mnemonic:"vl8re16.v" rec_
  | "vl8re32.v" -> vlm_form ~mnemonic:"vl8re32.v" rec_
  | "vl8re64.v" -> vlm_form ~mnemonic:"vl8re64.v" rec_
  | "vs1r.v" -> vsm_form ~mnemonic:"vs1r.v" rec_
  | "vs2r.v" -> vsm_form ~mnemonic:"vs2r.v" rec_
  | "vs4r.v" -> vsm_form ~mnemonic:"vs4r.v" rec_
  | "vs8r.v" -> vsm_form ~mnemonic:"vs8r.v" rec_
  | "vghsh.vv" -> zvk_ternary_form ~mnemonic:"vghsh.vv" rec_
  | "vgmul.vv" -> zvk_unary_form ~mnemonic:"vgmul.vv" rec_
  | "vsha2ms.vv" -> zvk_ternary_form ~mnemonic:"vsha2ms.vv" rec_
  | "vsha2ch.vv" -> zvk_ternary_form ~mnemonic:"vsha2ch.vv" rec_
  | "vsha2cl.vv" -> zvk_ternary_form ~mnemonic:"vsha2cl.vv" rec_
  | "vsm4k.vi" -> zvk_zimm5_form ~mnemonic:"vsm4k.vi" rec_
  | "vsm4r.vv" -> zvk_unary_form ~mnemonic:"vsm4r.vv" rec_
  | "vsm4r.vs" -> zvk_unary_form ~mnemonic:"vsm4r.vs" rec_
  | "vsm3c.vi" -> zvk_zimm5_form ~mnemonic:"vsm3c.vi" rec_
  | "vsm3me.vv" -> zvk_ternary_form ~mnemonic:"vsm3me.vv" rec_
  | "vbrev.v" -> vext_form ~mnemonic:"vbrev.v" rec_
  | "vbrev8.v" -> vext_form ~mnemonic:"vbrev8.v" rec_
  | "vclz.v" -> vext_form ~mnemonic:"vclz.v" rec_
  | "vcpop.v" -> vext_form ~mnemonic:"vcpop.v" rec_
  | "vctz.v" -> vext_form ~mnemonic:"vctz.v" rec_
  | "vrev8.v" -> vext_form ~mnemonic:"vrev8.v" rec_
  | "vror.vi" -> vror_vi_form rec_
  | "vaesdf.vv" -> zvk_unary_form ~mnemonic:"vaesdf.vv" rec_
  | "vaesdf.vs" -> zvk_unary_form ~mnemonic:"vaesdf.vs" rec_
  | "vaesdm.vv" -> zvk_unary_form ~mnemonic:"vaesdm.vv" rec_
  | "vaesdm.vs" -> zvk_unary_form ~mnemonic:"vaesdm.vs" rec_
  | "vaesef.vv" -> zvk_unary_form ~mnemonic:"vaesef.vv" rec_
  | "vaesef.vs" -> zvk_unary_form ~mnemonic:"vaesef.vs" rec_
  | "vaesem.vv" -> zvk_unary_form ~mnemonic:"vaesem.vv" rec_
  | "vaesem.vs" -> zvk_unary_form ~mnemonic:"vaesem.vs" rec_
  | "vaesz.vs" -> zvk_unary_form ~mnemonic:"vaesz.vs" rec_
  | "vaeskf1.vi" -> zvk_zimm5_form ~mnemonic:"vaeskf1.vi" rec_
  | "vaeskf2.vi" -> zvk_zimm5_form ~mnemonic:"vaeskf2.vi" rec_
  | other ->
      err "unhandled-native-name"
        (Printf.sprintf
           "Isa_norm_riscv only normalizes the frozen pilot mnemonics (sw, beq, c.addi, \
            fadd.s/fsub.s/fmul.s/fdiv.s/fadd.d/fsub.d/fmul.d/fdiv.d) plus the R-type/I-type \
            integer allowlists; %s is not one of them"
           other)
