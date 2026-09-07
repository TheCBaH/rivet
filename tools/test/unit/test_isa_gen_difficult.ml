(* Isa_gen_difficult: the manifest's
   own shape - entry counts, legal offset/label domains, distinct case_ids,
   register choices, and render_source_lines wiring. Grounding every entry
   against real normalization output needs the checked-in exports and lives
   in repo_tests.ml instead, matching Isa_gen_pilot's own split
   (test_isa_gen_pilot.ml). *)

open Compcert_tools

let failures = ref 0
let checks = ref 0

let check name cond =
  incr checks;
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let test_names () =
  check "fixture_dir_name is isa-difficult, distinct from isa-generated"
    (String.equal Isa_gen_difficult.fixture_dir_name "isa-difficult"
    && not (String.equal Isa_gen_difficult.fixture_dir_name Isa_generated_case.fixture_dir_name));
  check "cli_group_name is isa-difficult"
    (String.equal Isa_gen_difficult.cli_group_name "isa-difficult");
  check "check_make_target follows the asm-<thing>-check convention"
    (String.equal Isa_gen_difficult.check_make_target "asm-isa-difficult-check");
  check "regen_make_target follows the asm-<thing>-regen convention"
    (String.equal Isa_gen_difficult.regen_make_target "asm-isa-difficult-regen")

let test_counts () =
  check "sw_entries has 8 entries (4 offsets x 2 profiles)"
    (List.length Isa_gen_difficult.sw_entries = 8);
  check "beq_entries has 4 entries (2 directions x 2 profiles)"
    (List.length Isa_gen_difficult.beq_entries = 4);
  check "c_addi_entries has 8 entries (4 nzimm boundaries x 2 profiles)"
    (List.length Isa_gen_difficult.c_addi_entries = 8);
  check "x86_mov_entries has 4 entries (load/store x 2 modes)"
    (List.length Isa_gen_difficult.x86_mov_entries = 4);
  check "x86_fadd_entries has 2 entries (one x87 stack form x 2 modes)"
    (List.length Isa_gen_difficult.x86_fadd_entries = 2);
  check "fadd_s_entries has 2 entries (bare dynamic-rounding form x 2 profiles)"
    (List.length Isa_gen_difficult.fadd_s_entries = 2);
  check "fsub/fmul/fdiv scalar entries have 2 entries each (bare dynamic rounding x 2 profiles)"
    (List.length Isa_gen_difficult.fsub_s_entries = 2
    && List.length Isa_gen_difficult.fmul_s_entries = 2
    && List.length Isa_gen_difficult.fdiv_s_entries = 2);
  check "double scalar arithmetic entries have 2 entries each (bare dynamic rounding x 2 profiles)"
    (List.length Isa_gen_difficult.fadd_d_entries = 2
    && List.length Isa_gen_difficult.fsub_d_entries = 2
    && List.length Isa_gen_difficult.fmul_d_entries = 2
    && List.length Isa_gen_difficult.fdiv_d_entries = 2);
  check "flw/fld/fsw/fsd entries have 2 entries each (interior-offset x 2 profiles)"
    (List.length Isa_gen_difficult.flw_entries = 2
    && List.length Isa_gen_difficult.fld_entries = 2
    && List.length Isa_gen_difficult.fsw_entries = 2
    && List.length Isa_gen_difficult.fsd_entries = 2);
  check "sh1add_entries has 2 entries (Zba scale-one form x 2 profiles)"
    (List.length Isa_gen_difficult.sh1add_entries = 2);
  check "sh2add/sh3add entries have 2 entries each (Zba scale-two/scale-three x 2 profiles)"
    (List.length Isa_gen_difficult.sh2add_entries = 2
    && List.length Isa_gen_difficult.sh3add_entries = 2);
  check "min/minu/max/maxu entries have 2 entries each (Zbb comparisons x 2 profiles)"
    (List.length Isa_gen_difficult.min_entries = 2
    && List.length Isa_gen_difficult.minu_entries = 2
    && List.length Isa_gen_difficult.max_entries = 2
    && List.length Isa_gen_difficult.maxu_entries = 2);
  check "sh1add.uw/sh2add.uw/sh3add.uw entries have 1 entry each (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.sh1adduw_entries = 1
    && List.length Isa_gen_difficult.sh2adduw_entries = 1
    && List.length Isa_gen_difficult.sh3adduw_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         (Isa_gen_difficult.sh1adduw_entries @ Isa_gen_difficult.sh2adduw_entries
        @ Isa_gen_difficult.sh3adduw_entries));
  check "andn/orn/xnor/rol/ror entries have 2 entries each (Zbb Req_any comparisons x 2 profiles)"
    (List.length Isa_gen_difficult.andn_entries = 2
    && List.length Isa_gen_difficult.orn_entries = 2
    && List.length Isa_gen_difficult.xnor_entries = 2
    && List.length Isa_gen_difficult.rol_entries = 2
    && List.length Isa_gen_difficult.ror_entries = 2);
  check "clmul/clmulh entries have 2 entries each (Zbc Req_any x 2 profiles)"
    (List.length Isa_gen_difficult.clmul_entries = 2
    && List.length Isa_gen_difficult.clmulh_entries = 2);
  check "xperm4/xperm8 entries have 2 entries each (Zbkx Req_any x 2 profiles)"
    (List.length Isa_gen_difficult.xperm4_entries = 2
    && List.length Isa_gen_difficult.xperm8_entries = 2);
  check "sha256sum0/sum1/sig0/sig1 entries have 2 entries each (Zknh Req_any x 2 profiles)"
    (List.length Isa_gen_difficult.sha256sum0_entries = 2
    && List.length Isa_gen_difficult.sha256sum1_entries = 2
    && List.length Isa_gen_difficult.sha256sig0_entries = 2
    && List.length Isa_gen_difficult.sha256sig1_entries = 2);
  check "sha512sum0/sum1/sig0/sig1 entries have 1 entry each (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.sha512sum0_entries = 1
    && List.length Isa_gen_difficult.sha512sum1_entries = 1
    && List.length Isa_gen_difficult.sha512sig0_entries = 1
    && List.length Isa_gen_difficult.sha512sig1_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         (Isa_gen_difficult.sha512sum0_entries @ Isa_gen_difficult.sha512sum1_entries
        @ Isa_gen_difficult.sha512sig0_entries @ Isa_gen_difficult.sha512sig1_entries));
  check
    "sha512sum0r/sum1r/sig0l/sig1l/sig0h/sig1h entries have 1 entry each (RV32-only, no RV64 \
     counterpart)"
    (List.length Isa_gen_difficult.sha512sum0r_entries = 1
    && List.length Isa_gen_difficult.sha512sum1r_entries = 1
    && List.length Isa_gen_difficult.sha512sig0l_entries = 1
    && List.length Isa_gen_difficult.sha512sig1l_entries = 1
    && List.length Isa_gen_difficult.sha512sig0h_entries = 1
    && List.length Isa_gen_difficult.sha512sig1h_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv32)
         (Isa_gen_difficult.sha512sum0r_entries @ Isa_gen_difficult.sha512sum1r_entries
        @ Isa_gen_difficult.sha512sig0l_entries @ Isa_gen_difficult.sha512sig1l_entries
        @ Isa_gen_difficult.sha512sig0h_entries @ Isa_gen_difficult.sha512sig1h_entries));
  check "aes64ds/dsm/es/esm/ks2/im entries have 1 entry each (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.aes64ds_entries = 1
    && List.length Isa_gen_difficult.aes64dsm_entries = 1
    && List.length Isa_gen_difficult.aes64es_entries = 1
    && List.length Isa_gen_difficult.aes64esm_entries = 1
    && List.length Isa_gen_difficult.aes64ks2_entries = 1
    && List.length Isa_gen_difficult.aes64im_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         (Isa_gen_difficult.aes64ds_entries @ Isa_gen_difficult.aes64dsm_entries
        @ Isa_gen_difficult.aes64es_entries @ Isa_gen_difficult.aes64esm_entries
        @ Isa_gen_difficult.aes64ks2_entries @ Isa_gen_difficult.aes64im_entries));
  check "aes64ks1i_entries has 1 entry (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.aes64ks1i_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         Isa_gen_difficult.aes64ks1i_entries);
  check "aes32dsi/dsmi/esi/esmi entries have 1 entry each (RV32-only, no RV64 counterpart)"
    (List.length Isa_gen_difficult.aes32dsi_entries = 1
    && List.length Isa_gen_difficult.aes32dsmi_entries = 1
    && List.length Isa_gen_difficult.aes32esi_entries = 1
    && List.length Isa_gen_difficult.aes32esmi_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv32)
         (Isa_gen_difficult.aes32dsi_entries @ Isa_gen_difficult.aes32dsmi_entries
        @ Isa_gen_difficult.aes32esi_entries @ Isa_gen_difficult.aes32esmi_entries));
  check
    "csrrw/csrrs/csrrc/csrrwi/csrrsi/csrrci entries have 2 entries each (XLEN-independent x 2 \
     profiles)"
    (List.length Isa_gen_difficult.csrrw_entries = 2
    && List.length Isa_gen_difficult.csrrs_entries = 2
    && List.length Isa_gen_difficult.csrrc_entries = 2
    && List.length Isa_gen_difficult.csrrwi_entries = 2
    && List.length Isa_gen_difficult.csrrsi_entries = 2
    && List.length Isa_gen_difficult.csrrci_entries = 2);
  check
    "csrr/csrw/csrs/csrc/csrwi/csrsi/csrci entries have 2 entries each (XLEN-independent x 2 \
     profiles)"
    (List.length Isa_gen_difficult.csrr_entries = 2
    && List.length Isa_gen_difficult.csrw_entries = 2
    && List.length Isa_gen_difficult.csrs_entries = 2
    && List.length Isa_gen_difficult.csrc_entries = 2
    && List.length Isa_gen_difficult.csrwi_entries = 2
    && List.length Isa_gen_difficult.csrsi_entries = 2
    && List.length Isa_gen_difficult.csrci_entries = 2);
  check
    "clz/ctz/cpop/sext.b/sext.h/orc.b entries have 2 entries each (XLEN-independent x 2 profiles)"
    (List.length Isa_gen_difficult.clz_entries = 2
    && List.length Isa_gen_difficult.ctz_entries = 2
    && List.length Isa_gen_difficult.cpop_entries = 2
    && List.length Isa_gen_difficult.sextb_entries = 2
    && List.length Isa_gen_difficult.sexth_entries = 2
    && List.length Isa_gen_difficult.orcb_entries = 2);
  check "clzw/ctzw/cpopw entries have 1 entry each (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.clzw_entries = 1
    && List.length Isa_gen_difficult.ctzw_entries = 1
    && List.length Isa_gen_difficult.cpopw_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         (Isa_gen_difficult.clzw_entries @ Isa_gen_difficult.ctzw_entries
        @ Isa_gen_difficult.cpopw_entries));
  check "brev8_entries has 2 entries (Zbkb four-way Req_any x 2 profiles)"
    (List.length Isa_gen_difficult.brev8_entries = 2);
  check "rev8_entries has 2 entries (byte-reverse, distinct lookup_key per profile)"
    (List.length Isa_gen_difficult.rev8_entries = 2
    && List.exists
         (fun (e : Isa_gen_difficult.entry) ->
           e.target = Target.Riscv32 && String.equal e.lookup_key "rev8.rv32")
         Isa_gen_difficult.rev8_entries
    && List.exists
         (fun (e : Isa_gen_difficult.entry) ->
           e.target = Target.Riscv64 && String.equal e.lookup_key "rev8")
         Isa_gen_difficult.rev8_entries);
  check "pack/packh_entries have 2 entries each (Zbkb four-way Req_any x 2 profiles)"
    (List.length Isa_gen_difficult.pack_entries = 2
    && List.length Isa_gen_difficult.packh_entries = 2);
  check "packw_entries has 1 entry (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.packw_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         Isa_gen_difficult.packw_entries);
  check "zip/unzip_entries have 1 entry each (RV32-only, no RV64 counterpart)"
    (List.length Isa_gen_difficult.zip_entries = 1
    && List.length Isa_gen_difficult.unzip_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv32)
         (Isa_gen_difficult.zip_entries @ Isa_gen_difficult.unzip_entries));
  check "rolw/rorw_entries have 1 entry each (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.rolw_entries = 1
    && List.length Isa_gen_difficult.rorw_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         (Isa_gen_difficult.rolw_entries @ Isa_gen_difficult.rorw_entries));
  check "rori_entries has 2 entries (profile-specific native_name split like rev8)"
    (List.length Isa_gen_difficult.rori_entries = 2
    && List.exists
         (fun (e : Isa_gen_difficult.entry) ->
           e.target = Target.Riscv32 && String.equal e.lookup_key "rori.rv32")
         Isa_gen_difficult.rori_entries
    && List.exists
         (fun (e : Isa_gen_difficult.entry) ->
           e.target = Target.Riscv64 && String.equal e.lookup_key "rori")
         Isa_gen_difficult.rori_entries);
  check "roriw_entries has 1 entry (RV64-only, no RV32 counterpart)"
    (List.length Isa_gen_difficult.roriw_entries = 1
    && List.for_all
         (fun (e : Isa_gen_difficult.entry) -> e.target = Target.Riscv64)
         Isa_gen_difficult.roriw_entries);
  check "zext_h_entries has 2 entries (profile-specific native_name split like rev8)"
    (List.length Isa_gen_difficult.zext_h_entries = 2
    && List.exists
         (fun (e : Isa_gen_difficult.entry) ->
           e.target = Target.Riscv32 && String.equal e.lookup_key "zext.h.rv32")
         Isa_gen_difficult.zext_h_entries
    && List.exists
         (fun (e : Isa_gen_difficult.entry) ->
           e.target = Target.Riscv64 && String.equal e.lookup_key "zext.h")
         Isa_gen_difficult.zext_h_entries);
  check "vsetvl_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsetvl_entries = 2);
  check "vsetvli_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsetvli_entries = 2);
  check "vsetivli_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsetivli_entries = 2);
  check "vadd_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vadd_vv_entries = 2);
  check "vadd_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vadd_vx_entries = 2);
  check "vadd_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vadd_vi_entries = 2);
  check "all includes every difficult-form family"
    (List.length Isa_gen_difficult.all
    = List.length Isa_gen_difficult.sw_entries
      + List.length Isa_gen_difficult.beq_entries
      + List.length Isa_gen_difficult.c_addi_entries
      + List.length Isa_gen_difficult.x86_mov_entries
      + List.length Isa_gen_difficult.x86_fadd_entries
      + List.length Isa_gen_difficult.fadd_s_entries
      + List.length Isa_gen_difficult.fsub_s_entries
      + List.length Isa_gen_difficult.fmul_s_entries
      + List.length Isa_gen_difficult.fdiv_s_entries
      + List.length Isa_gen_difficult.fadd_d_entries
      + List.length Isa_gen_difficult.fsub_d_entries
      + List.length Isa_gen_difficult.fmul_d_entries
      + List.length Isa_gen_difficult.fdiv_d_entries
      + List.length Isa_gen_difficult.flw_entries
      + List.length Isa_gen_difficult.fld_entries
      + List.length Isa_gen_difficult.fsw_entries
      + List.length Isa_gen_difficult.fsd_entries
      + List.length Isa_gen_difficult.sh1add_entries
      + List.length Isa_gen_difficult.sh2add_entries
      + List.length Isa_gen_difficult.sh3add_entries
      + List.length Isa_gen_difficult.sh1adduw_entries
      + List.length Isa_gen_difficult.sh2adduw_entries
      + List.length Isa_gen_difficult.sh3adduw_entries
      + List.length Isa_gen_difficult.min_entries
      + List.length Isa_gen_difficult.minu_entries
      + List.length Isa_gen_difficult.max_entries
      + List.length Isa_gen_difficult.maxu_entries
      + List.length Isa_gen_difficult.andn_entries
      + List.length Isa_gen_difficult.orn_entries
      + List.length Isa_gen_difficult.xnor_entries
      + List.length Isa_gen_difficult.rol_entries
      + List.length Isa_gen_difficult.ror_entries
      + List.length Isa_gen_difficult.clz_entries
      + List.length Isa_gen_difficult.ctz_entries
      + List.length Isa_gen_difficult.cpop_entries
      + List.length Isa_gen_difficult.sextb_entries
      + List.length Isa_gen_difficult.sexth_entries
      + List.length Isa_gen_difficult.orcb_entries
      + List.length Isa_gen_difficult.clzw_entries
      + List.length Isa_gen_difficult.ctzw_entries
      + List.length Isa_gen_difficult.cpopw_entries
      + List.length Isa_gen_difficult.brev8_entries
      + List.length Isa_gen_difficult.rev8_entries
      + List.length Isa_gen_difficult.pack_entries
      + List.length Isa_gen_difficult.packh_entries
      + List.length Isa_gen_difficult.packw_entries
      + List.length Isa_gen_difficult.zip_entries
      + List.length Isa_gen_difficult.unzip_entries
      + List.length Isa_gen_difficult.rolw_entries
      + List.length Isa_gen_difficult.rorw_entries
      + List.length Isa_gen_difficult.rori_entries
      + List.length Isa_gen_difficult.roriw_entries
      + List.length Isa_gen_difficult.zext_h_entries
      + List.length Isa_gen_difficult.clmul_entries
      + List.length Isa_gen_difficult.clmulh_entries
      + List.length Isa_gen_difficult.xperm4_entries
      + List.length Isa_gen_difficult.xperm8_entries
      + List.length Isa_gen_difficult.sha256sum0_entries
      + List.length Isa_gen_difficult.sha256sum1_entries
      + List.length Isa_gen_difficult.sha256sig0_entries
      + List.length Isa_gen_difficult.sha256sig1_entries
      + List.length Isa_gen_difficult.sha512sum0_entries
      + List.length Isa_gen_difficult.sha512sum1_entries
      + List.length Isa_gen_difficult.sha512sig0_entries
      + List.length Isa_gen_difficult.sha512sig1_entries
      + List.length Isa_gen_difficult.sha512sum0r_entries
      + List.length Isa_gen_difficult.sha512sum1r_entries
      + List.length Isa_gen_difficult.sha512sig0l_entries
      + List.length Isa_gen_difficult.sha512sig1l_entries
      + List.length Isa_gen_difficult.sha512sig0h_entries
      + List.length Isa_gen_difficult.sha512sig1h_entries
      + List.length Isa_gen_difficult.aes64ds_entries
      + List.length Isa_gen_difficult.aes64dsm_entries
      + List.length Isa_gen_difficult.aes64es_entries
      + List.length Isa_gen_difficult.aes64esm_entries
      + List.length Isa_gen_difficult.aes64ks2_entries
      + List.length Isa_gen_difficult.aes64im_entries
      + List.length Isa_gen_difficult.aes64ks1i_entries
      + List.length Isa_gen_difficult.aes32dsi_entries
      + List.length Isa_gen_difficult.aes32dsmi_entries
      + List.length Isa_gen_difficult.aes32esi_entries
      + List.length Isa_gen_difficult.aes32esmi_entries
      + List.length Isa_gen_difficult.csrrw_entries
      + List.length Isa_gen_difficult.csrrs_entries
      + List.length Isa_gen_difficult.csrrc_entries
      + List.length Isa_gen_difficult.csrrwi_entries
      + List.length Isa_gen_difficult.csrrsi_entries
      + List.length Isa_gen_difficult.csrrci_entries
      + List.length Isa_gen_difficult.csrr_entries
      + List.length Isa_gen_difficult.csrw_entries
      + List.length Isa_gen_difficult.csrs_entries
      + List.length Isa_gen_difficult.csrc_entries
      + List.length Isa_gen_difficult.csrwi_entries
      + List.length Isa_gen_difficult.csrsi_entries
      + List.length Isa_gen_difficult.csrci_entries
      + List.length Isa_gen_difficult.amoswap_w_entries
      + List.length Isa_gen_difficult.amoadd_w_entries
      + List.length Isa_gen_difficult.amoxor_w_entries
      + List.length Isa_gen_difficult.amoand_w_entries
      + List.length Isa_gen_difficult.amoor_w_entries
      + List.length Isa_gen_difficult.amomin_w_entries
      + List.length Isa_gen_difficult.amomax_w_entries
      + List.length Isa_gen_difficult.amominu_w_entries
      + List.length Isa_gen_difficult.amomaxu_w_entries
      + List.length Isa_gen_difficult.sc_w_entries
      + List.length Isa_gen_difficult.lr_w_entries
      + List.length Isa_gen_difficult.amoswap_d_entries
      + List.length Isa_gen_difficult.amoadd_d_entries
      + List.length Isa_gen_difficult.amoxor_d_entries
      + List.length Isa_gen_difficult.amoand_d_entries
      + List.length Isa_gen_difficult.amoor_d_entries
      + List.length Isa_gen_difficult.amomin_d_entries
      + List.length Isa_gen_difficult.amomax_d_entries
      + List.length Isa_gen_difficult.amominu_d_entries
      + List.length Isa_gen_difficult.amomaxu_d_entries
      + List.length Isa_gen_difficult.sc_d_entries
      + List.length Isa_gen_difficult.lr_d_entries
      + List.length Isa_gen_difficult.fsgnj_s_entries
      + List.length Isa_gen_difficult.fsgnjn_s_entries
      + List.length Isa_gen_difficult.fsgnjx_s_entries
      + List.length Isa_gen_difficult.fsgnj_d_entries
      + List.length Isa_gen_difficult.fsgnjn_d_entries
      + List.length Isa_gen_difficult.fsgnjx_d_entries
      + List.length Isa_gen_difficult.fmin_s_entries
      + List.length Isa_gen_difficult.fmax_s_entries
      + List.length Isa_gen_difficult.fmin_d_entries
      + List.length Isa_gen_difficult.fmax_d_entries
      + List.length Isa_gen_difficult.fsqrt_s_entries
      + List.length Isa_gen_difficult.fsqrt_d_entries
      + List.length Isa_gen_difficult.fclass_s_entries
      + List.length Isa_gen_difficult.fclass_d_entries
      + List.length Isa_gen_difficult.fmadd_s_entries
      + List.length Isa_gen_difficult.fmsub_s_entries
      + List.length Isa_gen_difficult.fnmsub_s_entries
      + List.length Isa_gen_difficult.fnmadd_s_entries
      + List.length Isa_gen_difficult.fmadd_d_entries
      + List.length Isa_gen_difficult.fmsub_d_entries
      + List.length Isa_gen_difficult.fnmsub_d_entries
      + List.length Isa_gen_difficult.fnmadd_d_entries
      + List.length Isa_gen_difficult.feq_s_entries
      + List.length Isa_gen_difficult.fle_s_entries
      + List.length Isa_gen_difficult.flt_s_entries
      + List.length Isa_gen_difficult.feq_d_entries
      + List.length Isa_gen_difficult.fle_d_entries
      + List.length Isa_gen_difficult.flt_d_entries
      + List.length Isa_gen_difficult.fmv_x_w_entries
      + List.length Isa_gen_difficult.fmv_w_x_entries
      + List.length Isa_gen_difficult.fcvt_w_s_entries
      + List.length Isa_gen_difficult.fcvt_wu_s_entries
      + List.length Isa_gen_difficult.fcvt_s_w_entries
      + List.length Isa_gen_difficult.fcvt_s_wu_entries
      + List.length Isa_gen_difficult.fcvt_w_d_entries
      + List.length Isa_gen_difficult.fcvt_wu_d_entries
      + List.length Isa_gen_difficult.fcvt_d_w_entries
      + List.length Isa_gen_difficult.fcvt_d_wu_entries
      + List.length Isa_gen_difficult.fcvt_s_d_entries
      + List.length Isa_gen_difficult.fcvt_d_s_entries
      + List.length Isa_gen_difficult.fcvt_l_d_entries
      + List.length Isa_gen_difficult.fcvt_lu_d_entries
      + List.length Isa_gen_difficult.fcvt_l_s_entries
      + List.length Isa_gen_difficult.fcvt_lu_s_entries
      + List.length Isa_gen_difficult.fcvt_s_l_entries
      + List.length Isa_gen_difficult.fcvt_s_lu_entries
      + List.length Isa_gen_difficult.fcvt_d_l_entries
      + List.length Isa_gen_difficult.fcvt_d_lu_entries
      + List.length Isa_gen_difficult.vsetvl_entries
      + List.length Isa_gen_difficult.vsetvli_entries
      + List.length Isa_gen_difficult.vsetivli_entries
      + List.length Isa_gen_difficult.vadd_vv_entries
      + List.length Isa_gen_difficult.vadd_vx_entries
      + List.length Isa_gen_difficult.vadd_vi_entries)

let test_case_ids_distinct () =
  let ids = List.map (fun (e : Isa_gen_difficult.entry) -> e.case_id) Isa_gen_difficult.all in
  check "every case_id is unique" (List.length (List.sort_uniq String.compare ids) = List.length ids)

let test_sw_offsets_are_legal_s_type_domain () =
  let offset_of (e : Isa_gen_difficult.entry) = List.assoc "offset" e.operands in
  let offsets_for target =
    List.filter_map
      (fun (e : Isa_gen_difficult.entry) -> if e.target = target then Some (offset_of e) else None)
      Isa_gen_difficult.sw_entries
    |> List.sort_uniq String.compare
  in
  List.iter
    (fun target ->
      let offsets = offsets_for target in
      check
        (Printf.sprintf "%s sw offsets are exactly {-2048, -2048's boundary sibling, 0, 16, 2047}"
           (Target.to_string target))
        (List.equal String.equal offsets [ "-2048"; "0"; "16"; "2047" ]);
      List.iter
        (fun offset ->
          let n = int_of_string offset in
          check
            (Printf.sprintf "%s offset %s is within the signed 12-bit S-type domain"
               (Target.to_string target) offset)
            (n >= -2048 && n <= 2047))
        offsets)
    [ Target.Riscv32; Target.Riscv64 ]

let test_c_addi_nzimm_is_legal_nonzero_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      let imm = int_of_string (List.assoc "nzimm" e.operands) in
      check
        (Printf.sprintf "%s: nzimm %d is within the signed 6-bit domain" e.case_id imm)
        (imm >= -32 && imm <= 31);
      check (Printf.sprintf "%s: nzimm %d is nonzero" e.case_id imm) (imm <> 0);
      check
        (Printf.sprintf "%s: brackets the instruction in .option rvc/norvc" e.case_id)
        (e.lines_before = [ ".option rvc" ] && e.lines_after = [ ".option norvc" ]);
      check
        (Printf.sprintf "%s: configuration includes the c extension" e.case_id)
        (List.exists
           (fun arg ->
             String.length arg > 7
             && String.sub arg 0 7 = "-march="
             && String.ends_with ~suffix:"c" arg)
           e.configuration))
    Isa_gen_difficult.c_addi_entries

let test_beq_uses_local_labels_both_directions () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      let offset = List.assoc "offset" e.operands in
      check
        (Printf.sprintf "%s: offset is a local numeric label reference, not a raw immediate"
           e.case_id)
        (String.equal offset "1f" || String.equal offset "1b"))
    Isa_gen_difficult.beq_entries;
  check "beq_entries covers both the forward (1f) and backward (1b) direction"
    (List.exists
       (fun (e : Isa_gen_difficult.entry) -> List.assoc "offset" e.operands = "1f")
       Isa_gen_difficult.beq_entries
    && List.exists
         (fun (e : Isa_gen_difficult.entry) -> List.assoc "offset" e.operands = "1b")
         Isa_gen_difficult.beq_entries)

(* Mirrors Isa_gen_case_build's own "never x0/accumulator" check: a0/a1/a2 are
   plain GPRs with no special-cased RISC-V encoding, so a byte match actually
   exercises the general register field. *)
let test_no_entry_uses_x0 () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      List.iter
        (fun (_, v) ->
          check (Printf.sprintf "%s: operand %s is not x0" e.case_id v) (not (String.equal v "x0")))
        e.operands)
    Isa_gen_difficult.all

let test_x86_address_and_x87_domains () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      let mem = List.assoc "mem" e.operands in
      check
        (Printf.sprintf "%s: uses an explicit 32-bit width rule" e.case_id)
        (List.mem "explicit-32-bit-width" e.rule_ids);
      check
        (Printf.sprintf "%s: has AT&T memory syntax" e.case_id)
        (String.contains mem '(' && String.contains mem '%'))
    Isa_gen_difficult.x86_mov_entries;
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: binds source stack slot one" e.case_id)
        (List.assoc "src" e.operands = "1");
      check
        (Printf.sprintf "%s: identifies the implicit ST0 destination" e.case_id)
        (List.mem "implicit-st0-destination" e.rule_ids))
    Isa_gen_difficult.x86_fadd_entries

let test_f_arith_s_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses floating-point register operands" e.case_id)
        (List.map snd e.operands = [ "ft0"; "ft1"; "ft2" ]);
      check
        (Printf.sprintf "%s: selects measured implicit dynamic rounding" e.case_id)
        (List.mem "implicit-dynamic-rounding" e.rule_ids);
      check
        (Printf.sprintf "%s: configuration enables scalar FP" e.case_id)
        (List.exists
           (fun arg -> String.contains arg 'f' || String.contains arg 'd')
           e.configuration))
    (Isa_gen_difficult.fadd_s_entries @ Isa_gen_difficult.fsub_s_entries
   @ Isa_gen_difficult.fmul_s_entries @ Isa_gen_difficult.fdiv_s_entries
   @ Isa_gen_difficult.fadd_d_entries @ Isa_gen_difficult.fsub_d_entries
   @ Isa_gen_difficult.fmul_d_entries @ Isa_gen_difficult.fdiv_d_entries)

(* flw/fld/fsw/fsd: value/base/offset operands, value a floating-point
   register rather than sw's own GPR. *)
let test_f_ldst_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses value, base, offset operands in that order" e.case_id)
        (List.map fst e.operands = [ "value"; "base"; "offset" ]
        && List.map snd e.operands = [ "fa0"; "a1"; "8" ]);
      check
        (Printf.sprintf "%s: configuration enables scalar FP" e.case_id)
        (List.exists
           (fun arg -> String.contains arg 'f' || String.contains arg 'd')
           e.configuration))
    (Isa_gen_difficult.flw_entries @ Isa_gen_difficult.fld_entries @ Isa_gen_difficult.fsw_entries
   @ Isa_gen_difficult.fsd_entries)

(* fsgnj.s/fsgnjn.s/fsgnjx.s/fsgnj.d/fsgnjn.d/fsgnjx.d: the general
   three-distinct-FP-register sign-injection form; distinct rs1/rs2 registers
   distinguish this from the fneg.s/fneg.d/fmv.d rs1=rs2 pseudo-alias forms
   the encoder implemented before this pass. *)
let test_f_sgnj_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses distinct floating-point register operands" e.case_id)
        (List.map snd e.operands = [ "ft0"; "ft1"; "ft2" ]);
      check
        (Printf.sprintf "%s: flags distinct source registers" e.case_id)
        (List.mem "distinct-source-registers" e.rule_ids);
      check
        (Printf.sprintf "%s: configuration enables scalar FP" e.case_id)
        (List.exists
           (fun arg -> String.contains arg 'f' || String.contains arg 'd')
           e.configuration))
    (Isa_gen_difficult.fsgnj_s_entries @ Isa_gen_difficult.fsgnjn_s_entries
   @ Isa_gen_difficult.fsgnjx_s_entries @ Isa_gen_difficult.fsgnj_d_entries
   @ Isa_gen_difficult.fsgnjn_d_entries @ Isa_gen_difficult.fsgnjx_d_entries)

(* fmin.s/fmax.s/fmin.d/fmax.d: the same three-distinct-FP-register shape as
   {!test_f_sgnj_domain}, but no pseudo-alias shares this word. *)
let test_f_minmax_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses distinct floating-point register operands" e.case_id)
        (List.map snd e.operands = [ "ft0"; "ft1"; "ft2" ]);
      check
        (Printf.sprintf "%s: flags min-vs-max selection" e.case_id)
        (List.mem "min-max-select" e.rule_ids);
      check
        (Printf.sprintf "%s: configuration enables scalar FP" e.case_id)
        (List.exists
           (fun arg -> String.contains arg 'f' || String.contains arg 'd')
           e.configuration))
    (Isa_gen_difficult.fmin_s_entries @ Isa_gen_difficult.fmax_s_entries
   @ Isa_gen_difficult.fmin_d_entries @ Isa_gen_difficult.fmax_d_entries)

(* fsqrt.s/fsqrt.d: {!f_arith_entry}'s own shape minus the third operand. *)
let test_f_sqrt_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, rs1 floating-point register operands" e.case_id)
        (List.map snd e.operands = [ "ft0"; "ft1" ]);
      check
        (Printf.sprintf "%s: selects measured implicit dynamic rounding" e.case_id)
        (List.mem "implicit-dynamic-rounding" e.rule_ids);
      check
        (Printf.sprintf "%s: configuration enables scalar FP" e.case_id)
        (List.exists
           (fun arg -> String.contains arg 'f' || String.contains arg 'd')
           e.configuration))
    (Isa_gen_difficult.fsqrt_s_entries @ Isa_gen_difficult.fsqrt_d_entries)

(* fclass.s/fclass.d: [rd] is a GPR, [rs1] is FP. *)
let test_f_class_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses a GPR result and an FP source" e.case_id)
        (List.map snd e.operands = [ "a0"; "ft1" ]);
      check (Printf.sprintf "%s: flags the GPR result" e.case_id) (List.mem "gpr-result" e.rule_ids);
      check
        (Printf.sprintf "%s: configuration enables scalar FP" e.case_id)
        (List.exists
           (fun arg -> String.contains arg 'f' || String.contains arg 'd')
           e.configuration))
    (Isa_gen_difficult.fclass_s_entries @ Isa_gen_difficult.fclass_d_entries)

(* fmadd.s/fmsub.s/fnmsub.s/fnmadd.s/fmadd.d/fmsub.d/fnmsub.d/fnmadd.d: RISC-V's
   only R4-type mnemonics, {!test_f_sqrt_domain}'s implicit-dynamic-rounding
   shape with two more distinct FP register operands. *)
let test_f_fma_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, rs1, rs2, rs3 floating-point register operands" e.case_id)
        (List.map snd e.operands = [ "ft0"; "ft1"; "ft2"; "ft3" ]);
      check
        (Printf.sprintf "%s: selects measured implicit dynamic rounding" e.case_id)
        (List.mem "implicit-dynamic-rounding" e.rule_ids);
      check
        (Printf.sprintf "%s: configuration enables scalar FP" e.case_id)
        (List.exists
           (fun arg -> String.contains arg 'f' || String.contains arg 'd')
           e.configuration))
    (Isa_gen_difficult.fmadd_s_entries @ Isa_gen_difficult.fmsub_s_entries
   @ Isa_gen_difficult.fnmsub_s_entries @ Isa_gen_difficult.fnmadd_s_entries
   @ Isa_gen_difficult.fmadd_d_entries @ Isa_gen_difficult.fmsub_d_entries
   @ Isa_gen_difficult.fnmsub_d_entries @ Isa_gen_difficult.fnmadd_d_entries)

(* feq.s/fle.s/flt.s/feq.d/fle.d/flt.d: [rd] is a GPR (the boolean result),
   [rs1]/[rs2] are FP - the mirror image of {!test_f_fma_domain}'s all-FPR
   shape. *)
let test_f_cmp_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses a GPR result and two FP sources" e.case_id)
        (List.map snd e.operands = [ "a0"; "ft1"; "ft2" ]);
      check (Printf.sprintf "%s: flags the GPR result" e.case_id) (List.mem "gpr-result" e.rule_ids);
      check
        (Printf.sprintf "%s: configuration enables scalar FP" e.case_id)
        (List.exists
           (fun arg -> String.contains arg 'f' || String.contains arg 'd')
           e.configuration))
    (Isa_gen_difficult.feq_s_entries @ Isa_gen_difficult.fle_s_entries
   @ Isa_gen_difficult.flt_s_entries @ Isa_gen_difficult.feq_d_entries
   @ Isa_gen_difficult.fle_d_entries @ Isa_gen_difficult.flt_d_entries)

(* fmv.x.w/fmv.w.x: bit-for-bit moves, not conversions, each with a plain GPR
   operand on one side and a plain FP operand on the other. *)
let test_fmv_w_domain () =
  check "fmv.x.w: uses a GPR result and an FP source"
    (List.for_all
       (fun (e : Isa_gen_difficult.entry) -> List.map snd e.operands = [ "a0"; "ft1" ])
       Isa_gen_difficult.fmv_x_w_entries);
  check "fmv.w.x: uses an FP result and a GPR source"
    (List.for_all
       (fun (e : Isa_gen_difficult.entry) -> List.map snd e.operands = [ "ft0"; "a1" ])
       Isa_gen_difficult.fmv_w_x_entries)

(* fcvt.w.s/fcvt.wu.s/fcvt.s.w/fcvt.s.wu: real conversions (unlike
   {!test_fmv_w_domain}'s bit-for-bit moves), so - like {!test_f_fma_domain}
   and {!test_f_cmp_domain} - each carries a genuine implicit-dynamic-rounding
   [rm]. *)
let test_f_cvt_w_s_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses a GPR result and an FP source" e.case_id)
        (List.map snd e.operands = [ "a0"; "ft1" ]);
      check (Printf.sprintf "%s: flags the GPR result" e.case_id) (List.mem "gpr-result" e.rule_ids);
      check
        (Printf.sprintf "%s: selects measured implicit dynamic rounding" e.case_id)
        (List.mem "implicit-dynamic-rounding" e.rule_ids))
    (Isa_gen_difficult.fcvt_w_s_entries @ Isa_gen_difficult.fcvt_wu_s_entries);
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses an FP result and a GPR source" e.case_id)
        (List.map snd e.operands = [ "ft0"; "a1" ]);
      check (Printf.sprintf "%s: flags the FP result" e.case_id) (List.mem "fpr-result" e.rule_ids);
      check
        (Printf.sprintf "%s: selects measured implicit dynamic rounding" e.case_id)
        (List.mem "implicit-dynamic-rounding" e.rule_ids))
    (Isa_gen_difficult.fcvt_s_w_entries @ Isa_gen_difficult.fcvt_s_wu_entries)

(* fcvt.w.d/fcvt.wu.d/fcvt.d.w/fcvt.d.wu/fcvt.s.d/fcvt.d.s: the D-extension
   conversions {!test_f_cvt_w_s_domain} left open. fcvt.w.d/fcvt.wu.d keep
   the dynamic-rounding default; fcvt.d.w/fcvt.d.wu and the widening
   fcvt.d.s get the always-exact default instead - real hardware's own
   "never loses precision" pair. *)
let test_f_cvt_w_d_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses a GPR result and an FP source" e.case_id)
        (List.map snd e.operands = [ "a0"; "ft1" ]);
      check (Printf.sprintf "%s: flags the GPR result" e.case_id) (List.mem "gpr-result" e.rule_ids);
      check
        (Printf.sprintf "%s: selects measured implicit dynamic rounding" e.case_id)
        (List.mem "implicit-dynamic-rounding" e.rule_ids))
    (Isa_gen_difficult.fcvt_w_d_entries @ Isa_gen_difficult.fcvt_wu_d_entries);
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses an FP result and a GPR source" e.case_id)
        (List.map snd e.operands = [ "ft0"; "a1" ]);
      check (Printf.sprintf "%s: flags the FP result" e.case_id) (List.mem "fpr-result" e.rule_ids);
      check
        (Printf.sprintf "%s: selects measured implicit exact rounding" e.case_id)
        (List.mem "implicit-exact-rounding" e.rule_ids))
    (Isa_gen_difficult.fcvt_d_w_entries @ Isa_gen_difficult.fcvt_d_wu_entries);
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses two floating-point operands, no GPR" e.case_id)
        (List.map snd e.operands = [ "ft0"; "ft1" ]))
    (Isa_gen_difficult.fcvt_s_d_entries @ Isa_gen_difficult.fcvt_d_s_entries);
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: selects measured implicit dynamic rounding" e.case_id)
        (List.mem "implicit-dynamic-rounding" e.rule_ids))
    Isa_gen_difficult.fcvt_s_d_entries;
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: selects measured implicit exact rounding" e.case_id)
        (List.mem "implicit-exact-rounding" e.rule_ids))
    Isa_gen_difficult.fcvt_d_s_entries

(* fcvt.l.d/fcvt.lu.d/fcvt.d.l/fcvt.d.lu/fcvt.l.s/fcvt.lu.s/fcvt.s.l/fcvt.s.lu:
   the RV64-only long conversions {!test_f_cvt_w_d_domain} left open - every
   one keeps the family's usual dynamic-rounding default (unlike
   fcvt.d.w/fcvt.d.wu's always-exact one, since a 64-bit long is not always
   exact in a double), and every entries list targets RV64 only. *)
let test_f_cvt_l_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check (Printf.sprintf "%s: targets riscv64 only" e.case_id) (e.target = Target.Riscv64);
      check
        (Printf.sprintf "%s: selects measured implicit dynamic rounding" e.case_id)
        (List.mem "implicit-dynamic-rounding" e.rule_ids))
    (Isa_gen_difficult.fcvt_l_d_entries @ Isa_gen_difficult.fcvt_lu_d_entries
   @ Isa_gen_difficult.fcvt_l_s_entries @ Isa_gen_difficult.fcvt_lu_s_entries
   @ Isa_gen_difficult.fcvt_s_l_entries @ Isa_gen_difficult.fcvt_s_lu_entries
   @ Isa_gen_difficult.fcvt_d_l_entries @ Isa_gen_difficult.fcvt_d_lu_entries);
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses a GPR result and an FP source" e.case_id)
        (List.map snd e.operands = [ "a0"; "ft1" ]))
    (Isa_gen_difficult.fcvt_l_d_entries @ Isa_gen_difficult.fcvt_lu_d_entries
   @ Isa_gen_difficult.fcvt_l_s_entries @ Isa_gen_difficult.fcvt_lu_s_entries);
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses an FP result and a GPR source" e.case_id)
        (List.map snd e.operands = [ "ft0"; "a1" ]))
    (Isa_gen_difficult.fcvt_s_l_entries @ Isa_gen_difficult.fcvt_s_lu_entries
   @ Isa_gen_difficult.fcvt_d_l_entries @ Isa_gen_difficult.fcvt_d_lu_entries)

(* vsetvl: three ordinary GPR operands, both profiles, `v` enabled and no
   other extension needed (unlike every Zb*/Zk* family above). *)
let test_vsetvl_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses three ordinary GPR operands" e.case_id)
        (List.map snd e.operands = [ "a0"; "a1"; "a2" ]);
      check
        (Printf.sprintf "%s: flags the v-enabled rule" e.case_id)
        (List.mem "v-enabled" e.rule_ids);
      check
        (Printf.sprintf "%s: configuration enables the v extension" e.case_id)
        (List.exists
           (fun arg ->
             String.length arg > 7
             && String.sub arg 0 7 = "-march="
             && String.ends_with ~suffix:"v" arg)
           e.configuration))
    Isa_gen_difficult.vsetvl_entries

(* vsetvli/vsetivli: the [vtype] operand's own value is the full literal
   "e<SEW>,m<LMUL>,ta|tu,ma|mu" spelling, embedded commas and all -
   {!Isa_gen_render.render_line} joins operand values with ", " regardless
   of whether a value itself contains one, so this still renders the
   canonical six-operand spelling from a three-element operands list. *)
let test_vsetvli_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, rs1, vtype operands in that order" e.case_id)
        (List.map fst e.operands = [ "rd"; "rs1"; "vtype" ]
        && List.map snd e.operands = [ "a0"; "a1"; "e32, m1, ta, ma" ]);
      check
        (Printf.sprintf "%s: flags the vtype-keyword-list rule" e.case_id)
        (List.mem "vtype-keyword-list" e.rule_ids))
    Isa_gen_difficult.vsetvli_entries;
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, uimm, vtype operands in that order" e.case_id)
        (List.map fst e.operands = [ "rd"; "uimm"; "vtype" ]
        && List.map snd e.operands = [ "a0"; "5"; "e32, m1, ta, ma" ]))
    Isa_gen_difficult.vsetivli_entries

(* vadd.vv/vadd.vx/vadd.vi: the entry point into OP-V's real vector-register
   arithmetic space, distinct from the configuration-setting group above -
   flags "vector-register-operands" rather than "vtype-keyword-list". *)
let test_vadd_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, rs2, rs1 vector-register operands" e.case_id)
        (List.map fst e.operands = [ "rd"; "rs2"; "rs1" ]
        && List.map snd e.operands = [ "v1"; "v2"; "v3" ]);
      check
        (Printf.sprintf "%s: flags the vector-register-operands rule" e.case_id)
        (List.mem "vector-register-operands" e.rule_ids))
    Isa_gen_difficult.vadd_vv_entries;
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, rs2 vector registers and a GPR rs1" e.case_id)
        (List.map fst e.operands = [ "rd"; "rs2"; "rs1" ]
        && List.map snd e.operands = [ "v1"; "v2"; "a0" ]))
    Isa_gen_difficult.vadd_vx_entries;
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, rs2 vector registers and a signed immediate" e.case_id)
        (List.map fst e.operands = [ "rd"; "rs2"; "simm5" ]
        && List.map snd e.operands = [ "v1"; "v2"; "-5" ]))
    Isa_gen_difficult.vadd_vi_entries

let test_sh1add_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses three ordinary GPR operands" e.case_id)
        (List.map snd e.operands = [ "a0"; "a1"; "a2" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zba extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.sh1add_entries @ Isa_gen_difficult.sh2add_entries
   @ Isa_gen_difficult.sh3add_entries)

let test_minmax_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses three ordinary GPR operands" e.case_id)
        (List.map snd e.operands = [ "a0"; "a1"; "a2" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zbb extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.min_entries @ Isa_gen_difficult.minu_entries @ Isa_gen_difficult.max_entries
   @ Isa_gen_difficult.maxu_entries @ Isa_gen_difficult.andn_entries @ Isa_gen_difficult.orn_entries
   @ Isa_gen_difficult.xnor_entries @ Isa_gen_difficult.rol_entries @ Isa_gen_difficult.ror_entries
   @ Isa_gen_difficult.pack_entries @ Isa_gen_difficult.packh_entries
   @ Isa_gen_difficult.packw_entries @ Isa_gen_difficult.rolw_entries
   @ Isa_gen_difficult.rorw_entries @ Isa_gen_difficult.clmul_entries
   @ Isa_gen_difficult.clmulh_entries @ Isa_gen_difficult.xperm4_entries
   @ Isa_gen_difficult.xperm8_entries @ Isa_gen_difficult.sha512sum0r_entries
   @ Isa_gen_difficult.sha512sum1r_entries @ Isa_gen_difficult.sha512sig0l_entries
   @ Isa_gen_difficult.sha512sig1l_entries @ Isa_gen_difficult.sha512sig0h_entries
   @ Isa_gen_difficult.sha512sig1h_entries @ Isa_gen_difficult.aes64ds_entries
   @ Isa_gen_difficult.aes64dsm_entries @ Isa_gen_difficult.aes64es_entries
   @ Isa_gen_difficult.aes64esm_entries @ Isa_gen_difficult.aes64ks2_entries)

let test_unary_gpr_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses two ordinary GPR operands, no immediate" e.case_id)
        (List.map snd e.operands = [ "a0"; "a1" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zbb/Zbkb extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.clz_entries @ Isa_gen_difficult.ctz_entries @ Isa_gen_difficult.cpop_entries
   @ Isa_gen_difficult.sextb_entries @ Isa_gen_difficult.sexth_entries
   @ Isa_gen_difficult.orcb_entries @ Isa_gen_difficult.clzw_entries
   @ Isa_gen_difficult.ctzw_entries @ Isa_gen_difficult.cpopw_entries
   @ Isa_gen_difficult.brev8_entries @ Isa_gen_difficult.rev8_entries
   @ Isa_gen_difficult.zip_entries @ Isa_gen_difficult.unzip_entries
   @ Isa_gen_difficult.zext_h_entries @ Isa_gen_difficult.sha256sum0_entries
   @ Isa_gen_difficult.sha256sum1_entries @ Isa_gen_difficult.sha256sig0_entries
   @ Isa_gen_difficult.sha256sig1_entries @ Isa_gen_difficult.sha512sum0_entries
   @ Isa_gen_difficult.sha512sum1_entries @ Isa_gen_difficult.sha512sig0_entries
   @ Isa_gen_difficult.sha512sig1_entries @ Isa_gen_difficult.aes64im_entries)

let test_shamt_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses two ordinary GPR operands plus a shamt immediate" e.case_id)
        (List.map fst e.operands = [ "rd"; "rs1"; "shamt" ]
        && List.map snd e.operands = [ "a0"; "a1"; "5" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zbb extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.rori_entries @ Isa_gen_difficult.roriw_entries)

(* aes64ks1i: the same two-GPR-plus-narrow-immediate shape as
   {!test_shamt_domain} above, but keyed "rnum" (riscv-opcodes' own field
   name), not "shamt", so it gets its own check rather than folding into
   that shared one. *)
let test_aes64ks1i_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses two ordinary GPR operands plus an rnum immediate" e.case_id)
        (List.map fst e.operands = [ "rd"; "rs1"; "rnum" ]
        && List.map snd e.operands = [ "a0"; "a1"; "5" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zknd extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    Isa_gen_difficult.aes64ks1i_entries

(* aes32dsi/dsmi/esi/esmi: the three-GPR-plus-immediate analogue of
   {!test_aes64ks1i_domain} above, keyed "bs" (riscv-opcodes' own field
   name), not "rnum". *)
let test_aes32_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses three ordinary GPR operands plus a bs immediate" e.case_id)
        (List.map fst e.operands = [ "rd"; "rs1"; "rs2"; "bs" ]
        && List.map snd e.operands = [ "a0"; "a1"; "a2"; "3" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zknd/Zkne extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.aes32dsi_entries @ Isa_gen_difficult.aes32dsmi_entries
   @ Isa_gen_difficult.aes32esi_entries @ Isa_gen_difficult.aes32esmi_entries)

(* csrrw/csrrs/csrrc: rd/csr/rs1 operands, GAS's own text order (not
   riscv-opcodes' rd/rs1/csr field order). *)
let test_csr_reg_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, csr, rs1 operands in that order" e.case_id)
        (List.map fst e.operands = [ "rd"; "csr"; "rs1" ]
        && List.map snd e.operands = [ "a0"; "0x300"; "a1" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zicsr extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.csrrw_entries @ Isa_gen_difficult.csrrs_entries
   @ Isa_gen_difficult.csrrc_entries)

(* csrrwi/csrrsi/csrrci: rd/csr/zimm5 operands - no register operand
   besides rd. *)
let test_csr_imm_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, csr, zimm5 operands in that order" e.case_id)
        (List.map fst e.operands = [ "rd"; "csr"; "zimm5" ]
        && List.map snd e.operands = [ "a0"; "0x300"; "5" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zicsr extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.csrrwi_entries @ Isa_gen_difficult.csrrsi_entries
   @ Isa_gen_difficult.csrrci_entries)

(* csrr: rd/csr operands, no rs1. *)
let test_csrr_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, csr operands in that order" e.case_id)
        (List.map fst e.operands = [ "rd"; "csr" ] && List.map snd e.operands = [ "a0"; "0x300" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zicsr extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    Isa_gen_difficult.csrr_entries

(* csrw/csrs/csrc: csr/rs1 operands, no rd - csr comes first, unlike
   {!test_csr_reg_domain}'s rd/csr/rs1. *)
let test_csr_write_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses csr, rs1 operands in that order" e.case_id)
        (List.map fst e.operands = [ "csr"; "rs1" ] && List.map snd e.operands = [ "0x300"; "a1" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zicsr extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.csrw_entries @ Isa_gen_difficult.csrs_entries
   @ Isa_gen_difficult.csrc_entries)

(* csrwi/csrsi/csrci: csr/zimm5 operands, no rd. *)
let test_csr_write_imm_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses csr, zimm5 operands in that order" e.case_id)
        (List.map fst e.operands = [ "csr"; "zimm5" ] && List.map snd e.operands = [ "0x300"; "5" ]);
      check
        (Printf.sprintf "%s: enables only the measured Zicsr extension" e.case_id)
        (List.exists (fun arg -> String.contains arg 'z') e.configuration))
    (Isa_gen_difficult.csrwi_entries @ Isa_gen_difficult.csrsi_entries
   @ Isa_gen_difficult.csrci_entries)

(* amoOP/scOP: rd/rs2/base operands, base rendering as GAS's parenthesized
   memory group rather than a bare register. *)
let test_amo_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, rs2, base operands in that order" e.case_id)
        (List.map fst e.operands = [ "rd"; "rs2"; "base" ]
        && List.map snd e.operands = [ "a0"; "a1"; "a2" ]);
      check
        (Printf.sprintf "%s: enables only the measured A extension" e.case_id)
        (List.exists
           (fun arg -> String.equal arg "-march=rv32ia" || String.equal arg "-march=rv64ia")
           e.configuration))
    (Isa_gen_difficult.amoswap_w_entries @ Isa_gen_difficult.amoadd_w_entries
   @ Isa_gen_difficult.amoxor_w_entries @ Isa_gen_difficult.amoand_w_entries
   @ Isa_gen_difficult.amoor_w_entries @ Isa_gen_difficult.amomin_w_entries
   @ Isa_gen_difficult.amomax_w_entries @ Isa_gen_difficult.amominu_w_entries
   @ Isa_gen_difficult.amomaxu_w_entries @ Isa_gen_difficult.sc_w_entries
   @ Isa_gen_difficult.amoswap_d_entries @ Isa_gen_difficult.amoadd_d_entries
   @ Isa_gen_difficult.amoxor_d_entries @ Isa_gen_difficult.amoand_d_entries
   @ Isa_gen_difficult.amoor_d_entries @ Isa_gen_difficult.amomin_d_entries
   @ Isa_gen_difficult.amomax_d_entries @ Isa_gen_difficult.amominu_d_entries
   @ Isa_gen_difficult.amomaxu_d_entries @ Isa_gen_difficult.sc_d_entries)

(* lr.w/lr.d: rd/base operands only, no rs2 (its field is fixed to 0). *)
let test_lr_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, base operands in that order" e.case_id)
        (List.map fst e.operands = [ "rd"; "base" ] && List.map snd e.operands = [ "a0"; "a2" ]);
      check
        (Printf.sprintf "%s: enables only the measured A extension" e.case_id)
        (List.exists
           (fun arg -> String.equal arg "-march=rv32ia" || String.equal arg "-march=rv64ia")
           e.configuration))
    (Isa_gen_difficult.lr_w_entries @ Isa_gen_difficult.lr_d_entries)

let test_render_source_lines () =
  check "render_source_lines matches render_source for a single line"
    (String.equal
       (Isa_gen_render.render_source_lines [ "add a0, a1, a2" ])
       (Isa_gen_render.render_source "add a0, a1, a2"));
  check "render_source_lines joins multiple lines with newlines under one .text"
    (String.equal
       (Isa_gen_render.render_source_lines [ "beq a0, a1, 1f"; "nop"; "1:" ])
       ".text\nbeq a0, a1, 1f\nnop\n1:\n")

(* Isa_gen_difficult.build itself - and grounding every entry's lookup_key
   against a real normalized form - needs the checked-in exports via a real
   Repo.t and lives in repo_tests.ml, matching Isa_gen_pilot's own split
   (test_isa_gen_pilot.ml's header comment above). This module only checks
   what render_source_lines produces from literal syntax_recipe/operand
   values, with no file access. *)
let test_build_wraps_lines_before_and_after () =
  let syntax =
    Isa_norm_model.
      {
        dialect = "gas-att";
        mnemonic = "beq";
        operands = [ Syn_operand "lhs"; Syn_operand "rhs"; Syn_operand "offset" ];
      }
  in
  match
    Isa_gen_render.render_line syntax ~operands:[ ("lhs", "a0"); ("rhs", "a1"); ("offset", "1f") ]
  with
  | Error msg -> check (Printf.sprintf "beq's syntax recipe renders (%s)" msg) false
  | Ok line ->
      check "a forward-branch entry's lines_before/lines_after wrap the instruction line"
        (String.equal
           (Isa_gen_render.render_source_lines ([] @ [ line ] @ [ "nop"; "1:" ]))
           ".text\nbeq a0, a1, 1f\nnop\n1:\n")

let () =
  print_endline "isa-gen-difficult:";
  test_names ();
  test_counts ();
  test_case_ids_distinct ();
  test_sw_offsets_are_legal_s_type_domain ();
  test_c_addi_nzimm_is_legal_nonzero_domain ();
  test_beq_uses_local_labels_both_directions ();
  test_no_entry_uses_x0 ();
  test_x86_address_and_x87_domains ();
  test_f_arith_s_domain ();
  test_f_ldst_domain ();
  test_f_sgnj_domain ();
  test_f_minmax_domain ();
  test_f_sqrt_domain ();
  test_f_class_domain ();
  test_f_fma_domain ();
  test_f_cmp_domain ();
  test_fmv_w_domain ();
  test_f_cvt_w_s_domain ();
  test_f_cvt_w_d_domain ();
  test_f_cvt_l_domain ();
  test_vsetvl_domain ();
  test_vsetvli_domain ();
  test_vadd_domain ();
  test_sh1add_domain ();
  test_minmax_domain ();
  test_unary_gpr_domain ();
  test_shamt_domain ();
  test_aes64ks1i_domain ();
  test_aes32_domain ();
  test_csr_reg_domain ();
  test_csr_imm_domain ();
  test_csrr_domain ();
  test_csr_write_domain ();
  test_csr_write_imm_domain ();
  test_amo_domain ();
  test_lr_domain ();
  test_render_source_lines ();
  test_build_wraps_lines_before_and_after ();
  if !failures > 0 then (
    Printf.printf "isa-gen-difficult: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-gen-difficult: all %d checks passed\n" !checks
