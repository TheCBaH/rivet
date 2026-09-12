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
  check "vsub_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsub_vv_entries = 2);
  check "vsub_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsub_vx_entries = 2);
  check "vrsub_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vrsub_vx_entries = 2);
  check "vrsub_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vrsub_vi_entries = 2);
  check "vand_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vand_vv_entries = 2);
  check "vand_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vand_vx_entries = 2);
  check "vand_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vand_vi_entries = 2);
  check "vor_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vor_vv_entries = 2);
  check "vor_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vor_vx_entries = 2);
  check "vor_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vor_vi_entries = 2);
  check "vxor_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vxor_vv_entries = 2);
  check "vxor_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vxor_vx_entries = 2);
  check "vxor_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vxor_vi_entries = 2);
  check "vsll_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsll_vv_entries = 2);
  check "vsll_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsll_vx_entries = 2);
  check "vsll_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsll_vi_entries = 2);
  check "vsrl_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsrl_vv_entries = 2);
  check "vsrl_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsrl_vx_entries = 2);
  check "vsrl_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsrl_vi_entries = 2);
  check "vsra_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsra_vv_entries = 2);
  check "vsra_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsra_vx_entries = 2);
  check "vsra_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsra_vi_entries = 2);
  check "vminu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vminu_vv_entries = 2);
  check "vminu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vminu_vx_entries = 2);
  check "vmin_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmin_vv_entries = 2);
  check "vmin_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmin_vx_entries = 2);
  check "vmaxu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmaxu_vv_entries = 2);
  check "vmaxu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmaxu_vx_entries = 2);
  check "vmax_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmax_vv_entries = 2);
  check "vmax_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmax_vx_entries = 2);
  check "vmul_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmul_vv_entries = 2);
  check "vmul_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmul_vx_entries = 2);
  check "vmulh_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmulh_vv_entries = 2);
  check "vmulh_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmulh_vx_entries = 2);
  check "vmulhu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmulhu_vv_entries = 2);
  check "vmulhu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmulhu_vx_entries = 2);
  check "vmulhsu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmulhsu_vv_entries = 2);
  check "vmulhsu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmulhsu_vx_entries = 2);
  check "vdivu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vdivu_vv_entries = 2);
  check "vdivu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vdivu_vx_entries = 2);
  check "vdiv_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vdiv_vv_entries = 2);
  check "vdiv_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vdiv_vx_entries = 2);
  check "vremu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vremu_vv_entries = 2);
  check "vremu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vremu_vx_entries = 2);
  check "vrem_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vrem_vv_entries = 2);
  check "vrem_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vrem_vx_entries = 2);
  check "vsaddu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsaddu_vv_entries = 2);
  check "vsaddu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsaddu_vx_entries = 2);
  check "vsaddu_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsaddu_vi_entries = 2);
  check "vsadd_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsadd_vv_entries = 2);
  check "vsadd_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsadd_vx_entries = 2);
  check "vsadd_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsadd_vi_entries = 2);
  check "vssubu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vssubu_vv_entries = 2);
  check "vssubu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vssubu_vx_entries = 2);
  check "vssub_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vssub_vv_entries = 2);
  check "vssub_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vssub_vx_entries = 2);
  check "vaaddu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vaaddu_vv_entries = 2);
  check "vaaddu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vaaddu_vx_entries = 2);
  check "vaadd_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vaadd_vv_entries = 2);
  check "vaadd_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vaadd_vx_entries = 2);
  check "vasubu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vasubu_vv_entries = 2);
  check "vasubu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vasubu_vx_entries = 2);
  check "vasub_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vasub_vv_entries = 2);
  check "vasub_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vasub_vx_entries = 2);
  check "vnsrl_wv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnsrl_wv_entries = 2);
  check "vnsrl_wx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnsrl_wx_entries = 2);
  check "vnsrl_wi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnsrl_wi_entries = 2);
  check "vnsra_wv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnsra_wv_entries = 2);
  check "vnsra_wx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnsra_wx_entries = 2);
  check "vnsra_wi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnsra_wi_entries = 2);
  check "vnclipu_wv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnclipu_wv_entries = 2);
  check "vnclipu_wx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnclipu_wx_entries = 2);
  check "vnclipu_wi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnclipu_wi_entries = 2);
  check "vnclip_wv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnclip_wv_entries = 2);
  check "vnclip_wx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnclip_wx_entries = 2);
  check "vnclip_wi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnclip_wi_entries = 2);
  check "vssrl_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vssrl_vv_entries = 2);
  check "vssrl_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vssrl_vx_entries = 2);
  check "vssrl_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vssrl_vi_entries = 2);
  check "vssra_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vssra_vv_entries = 2);
  check "vssra_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vssra_vx_entries = 2);
  check "vssra_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vssra_vi_entries = 2);
  check "vrgather_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vrgather_vv_entries = 2);
  check "vrgather_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vrgather_vx_entries = 2);
  check "vrgather_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vrgather_vi_entries = 2);
  check "vrgatherei16_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vrgatherei16_vv_entries = 2);
  check "vwaddu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwaddu_vv_entries = 2);
  check "vwaddu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwaddu_vx_entries = 2);
  check "vwadd_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwadd_vv_entries = 2);
  check "vwadd_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwadd_vx_entries = 2);
  check "vwsubu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwsubu_vv_entries = 2);
  check "vwsubu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwsubu_vx_entries = 2);
  check "vwsub_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwsub_vv_entries = 2);
  check "vwsub_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwsub_vx_entries = 2);
  check "vwaddu_wv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwaddu_wv_entries = 2);
  check "vwaddu_wx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwaddu_wx_entries = 2);
  check "vwadd_wv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwadd_wv_entries = 2);
  check "vwadd_wx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwadd_wx_entries = 2);
  check "vwsubu_wv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwsubu_wv_entries = 2);
  check "vwsubu_wx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwsubu_wx_entries = 2);
  check "vwsub_wv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwsub_wv_entries = 2);
  check "vwsub_wx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwsub_wx_entries = 2);
  check "vwmulu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmulu_vv_entries = 2);
  check "vwmulu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmulu_vx_entries = 2);
  check "vwmulsu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmulsu_vv_entries = 2);
  check "vwmulsu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmulsu_vx_entries = 2);
  check "vwmul_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmul_vv_entries = 2);
  check "vwmul_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmul_vx_entries = 2);
  check "vsext_vf2_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsext_vf2_entries = 2);
  check "vsext_vf4_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsext_vf4_entries = 2);
  check "vsext_vf8_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsext_vf8_entries = 2);
  check "vzext_vf2_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vzext_vf2_entries = 2);
  check "vzext_vf4_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vzext_vf4_entries = 2);
  check "vzext_vf8_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vzext_vf8_entries = 2);
  check "vmand_mm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmand_mm_entries = 2);
  check "vmandn_mm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmandn_mm_entries = 2);
  check "vmor_mm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmor_mm_entries = 2);
  check "vmxor_mm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmxor_mm_entries = 2);
  check "vmorn_mm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmorn_mm_entries = 2);
  check "vmnand_mm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmnand_mm_entries = 2);
  check "vmnor_mm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmnor_mm_entries = 2);
  check "vmxnor_mm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmxnor_mm_entries = 2);
  check "vredsum_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vredsum_vs_entries = 2);
  check "vredand_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vredand_vs_entries = 2);
  check "vredor_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vredor_vs_entries = 2);
  check "vredxor_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vredxor_vs_entries = 2);
  check "vredminu_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vredminu_vs_entries = 2);
  check "vredmin_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vredmin_vs_entries = 2);
  check "vredmaxu_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vredmaxu_vs_entries = 2);
  check "vredmax_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vredmax_vs_entries = 2);
  check "vwredsumu_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwredsumu_vs_entries = 2);
  check "vwredsum_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwredsum_vs_entries = 2);
  check "vmseq_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmseq_vv_entries = 2);
  check "vmseq_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmseq_vx_entries = 2);
  check "vmseq_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmseq_vi_entries = 2);
  check "vmsne_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsne_vv_entries = 2);
  check "vmsne_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsne_vx_entries = 2);
  check "vmsne_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsne_vi_entries = 2);
  check "vmsltu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsltu_vv_entries = 2);
  check "vmsltu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsltu_vx_entries = 2);
  check "vmslt_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmslt_vv_entries = 2);
  check "vmslt_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmslt_vx_entries = 2);
  check "vmsleu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsleu_vv_entries = 2);
  check "vmsleu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsleu_vx_entries = 2);
  check "vmsleu_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsleu_vi_entries = 2);
  check "vmsle_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsle_vv_entries = 2);
  check "vmsle_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsle_vx_entries = 2);
  check "vmsle_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsle_vi_entries = 2);
  check "vmsgtu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsgtu_vx_entries = 2);
  check "vmsgtu_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsgtu_vi_entries = 2);
  check "vmsgt_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsgt_vx_entries = 2);
  check "vmsgt_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsgt_vi_entries = 2);
  check "vslideup_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vslideup_vx_entries = 2);
  check "vslideup_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vslideup_vi_entries = 2);
  check "vslidedown_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vslidedown_vx_entries = 2);
  check "vslidedown_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vslidedown_vi_entries = 2);
  check "vslide1up_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vslide1up_vx_entries = 2);
  check "vslide1down_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vslide1down_vx_entries = 2);
  check "vmacc_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmacc_vv_entries = 2);
  check "vmacc_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmacc_vx_entries = 2);
  check "vnmsac_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnmsac_vv_entries = 2);
  check "vnmsac_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnmsac_vx_entries = 2);
  check "vmadd_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmadd_vv_entries = 2);
  check "vmadd_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmadd_vx_entries = 2);
  check "vnmsub_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnmsub_vv_entries = 2);
  check "vnmsub_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vnmsub_vx_entries = 2);
  check "vwmaccu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmaccu_vv_entries = 2);
  check "vwmaccu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmaccu_vx_entries = 2);
  check "vwmacc_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmacc_vv_entries = 2);
  check "vwmacc_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmacc_vx_entries = 2);
  check "vwmaccsu_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmaccsu_vv_entries = 2);
  check "vwmaccsu_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmaccsu_vx_entries = 2);
  check "vwmaccus_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwmaccus_vx_entries = 2);
  check "vid_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vid_v_entries = 2);
  check "viota_m_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.viota_m_entries = 2);
  check "vcompress_vm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vcompress_vm_entries = 2);
  check "vmsbf_m_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsbf_m_entries = 2);
  check "vmsif_m_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsif_m_entries = 2);
  check "vmsof_m_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsof_m_entries = 2);
  check "vcpop_m_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vcpop_m_entries = 2);
  check "vfirst_m_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfirst_m_entries = 2);
  check "vadc_vvm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vadc_vvm_entries = 2);
  check "vadc_vxm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vadc_vxm_entries = 2);
  check "vadc_vim_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vadc_vim_entries = 2);
  check "vmadc_vvm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmadc_vvm_entries = 2);
  check "vmadc_vxm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmadc_vxm_entries = 2);
  check "vmadc_vim_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmadc_vim_entries = 2);
  check "vmadc_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmadc_vv_entries = 2);
  check "vmadc_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmadc_vx_entries = 2);
  check "vmadc_vi_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmadc_vi_entries = 2);
  check "vsbc_vvm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsbc_vvm_entries = 2);
  check "vsbc_vxm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsbc_vxm_entries = 2);
  check "vmsbc_vvm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsbc_vvm_entries = 2);
  check "vmsbc_vxm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsbc_vxm_entries = 2);
  check "vmsbc_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsbc_vv_entries = 2);
  check "vmsbc_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmsbc_vx_entries = 2);
  check "vmerge_vvm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmerge_vvm_entries = 2);
  check "vmerge_vxm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmerge_vxm_entries = 2);
  check "vmerge_vim_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmerge_vim_entries = 2);
  check "vmv_x_s_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmv_x_s_entries = 2);
  check "vmv_s_x_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmv_s_x_entries = 2);
  check "vmv_v_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmv_v_v_entries = 2);
  check "vmv_v_x_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmv_v_x_entries = 2);
  check "vmv_v_i_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmv_v_i_entries = 2);
  check "vmv1r_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmv1r_v_entries = 2);
  check "vmv2r_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmv2r_v_entries = 2);
  check "vmv4r_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmv4r_v_entries = 2);
  check "vmv8r_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmv8r_v_entries = 2);
  check "vsmul_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsmul_vv_entries = 2);
  check "vsmul_vx_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsmul_vx_entries = 2);
  check "vfadd_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfadd_vv_entries = 2);
  check "vfadd_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfadd_vf_entries = 2);
  check "vfsub_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfsub_vv_entries = 2);
  check "vfsub_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfsub_vf_entries = 2);
  check "vfrsub_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfrsub_vf_entries = 2);
  check "vfmul_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmul_vv_entries = 2);
  check "vfmul_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmul_vf_entries = 2);
  check "vfdiv_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfdiv_vv_entries = 2);
  check "vfdiv_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfdiv_vf_entries = 2);
  check "vfrdiv_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfrdiv_vf_entries = 2);
  check "vfmin_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmin_vv_entries = 2);
  check "vfmin_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmin_vf_entries = 2);
  check "vfmax_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmax_vv_entries = 2);
  check "vfmax_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmax_vf_entries = 2);
  check "vfsgnj_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfsgnj_vv_entries = 2);
  check "vfsgnj_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfsgnj_vf_entries = 2);
  check "vfsgnjn_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfsgnjn_vv_entries = 2);
  check "vfsgnjn_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfsgnjn_vf_entries = 2);
  check "vfsgnjx_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfsgnjx_vv_entries = 2);
  check "vfsgnjx_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfsgnjx_vf_entries = 2);
  check "vfsqrt_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfsqrt_v_entries = 2);
  check "vfrsqrt7_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfrsqrt7_v_entries = 2);
  check "vfrec7_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfrec7_v_entries = 2);
  check "vfclass_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfclass_v_entries = 2);
  check "vfredosum_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfredosum_vs_entries = 2);
  check "vfredusum_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfredusum_vs_entries = 2);
  check "vfredmin_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfredmin_vs_entries = 2);
  check "vfredmax_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfredmax_vs_entries = 2);
  check "vmfeq_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmfeq_vv_entries = 2);
  check "vmfeq_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmfeq_vf_entries = 2);
  check "vmfle_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmfle_vv_entries = 2);
  check "vmfle_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmfle_vf_entries = 2);
  check "vmflt_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmflt_vv_entries = 2);
  check "vmflt_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmflt_vf_entries = 2);
  check "vmfne_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmfne_vv_entries = 2);
  check "vmfne_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmfne_vf_entries = 2);
  check "vmfgt_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmfgt_vf_entries = 2);
  check "vmfge_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vmfge_vf_entries = 2);
  check "vfmv_f_s_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmv_f_s_entries = 2);
  check "vfmv_s_f_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmv_s_f_entries = 2);
  check "vfmv_v_f_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmv_v_f_entries = 2);
  check "vfmerge_vfm_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmerge_vfm_entries = 2);
  check "vfcvt_xu_f_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfcvt_xu_f_v_entries = 2);
  check "vfcvt_x_f_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfcvt_x_f_v_entries = 2);
  check "vfcvt_f_xu_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfcvt_f_xu_v_entries = 2);
  check "vfcvt_f_x_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfcvt_f_x_v_entries = 2);
  check "vfcvt_rtz_xu_f_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfcvt_rtz_xu_f_v_entries = 2);
  check "vfcvt_rtz_x_f_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfcvt_rtz_x_f_v_entries = 2);
  check "vfmadd_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmadd_vv_entries = 2);
  check "vfmadd_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmadd_vf_entries = 2);
  check "vfnmadd_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfnmadd_vv_entries = 2);
  check "vfnmadd_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfnmadd_vf_entries = 2);
  check "vfmsub_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmsub_vv_entries = 2);
  check "vfmsub_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmsub_vf_entries = 2);
  check "vfnmsub_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfnmsub_vv_entries = 2);
  check "vfnmsub_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfnmsub_vf_entries = 2);
  check "vfmacc_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmacc_vv_entries = 2);
  check "vfmacc_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmacc_vf_entries = 2);
  check "vfnmacc_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfnmacc_vv_entries = 2);
  check "vfnmacc_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfnmacc_vf_entries = 2);
  check "vfmsac_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmsac_vv_entries = 2);
  check "vfmsac_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfmsac_vf_entries = 2);
  check "vfnmsac_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfnmsac_vv_entries = 2);
  check "vfnmsac_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfnmsac_vf_entries = 2);
  check "vfslide1up_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfslide1up_vf_entries = 2);
  check "vfslide1down_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfslide1down_vf_entries = 2);
  check "vfwadd_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwadd_vv_entries = 2);
  check "vfwadd_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwadd_vf_entries = 2);
  check "vfwadd_wv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwadd_wv_entries = 2);
  check "vfwadd_wf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwadd_wf_entries = 2);
  check "vfwsub_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwsub_vv_entries = 2);
  check "vfwsub_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwsub_vf_entries = 2);
  check "vfwsub_wv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwsub_wv_entries = 2);
  check "vfwsub_wf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwsub_wf_entries = 2);
  check "vfwmul_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwmul_vv_entries = 2);
  check "vfwmul_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwmul_vf_entries = 2);
  check "vfwredosum_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwredosum_vs_entries = 2);
  check "vfwredusum_vs_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwredusum_vs_entries = 2);
  check "vfwcvt_xu_f_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwcvt_xu_f_v_entries = 2);
  check "vfwcvt_x_f_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwcvt_x_f_v_entries = 2);
  check "vfwcvt_f_xu_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwcvt_f_xu_v_entries = 2);
  check "vfwcvt_f_x_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwcvt_f_x_v_entries = 2);
  check "vfwcvt_f_f_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwcvt_f_f_v_entries = 2);
  check "vfwcvt_rtz_xu_f_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwcvt_rtz_xu_f_v_entries = 2);
  check "vfwcvt_rtz_x_f_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwcvt_rtz_x_f_v_entries = 2);
  check "vfncvt_xu_f_w_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfncvt_xu_f_w_entries = 2);
  check "vfncvt_x_f_w_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfncvt_x_f_w_entries = 2);
  check "vfncvt_f_xu_w_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfncvt_f_xu_w_entries = 2);
  check "vfncvt_f_x_w_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfncvt_f_x_w_entries = 2);
  check "vfncvt_f_f_w_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfncvt_f_f_w_entries = 2);
  check "vfncvt_rod_f_f_w_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfncvt_rod_f_f_w_entries = 2);
  check "vfncvt_rtz_xu_f_w_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfncvt_rtz_xu_f_w_entries = 2);
  check "vfncvt_rtz_x_f_w_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfncvt_rtz_x_f_w_entries = 2);
  check "vfwmacc_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwmacc_vv_entries = 2);
  check "vfwmacc_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwmacc_vf_entries = 2);
  check "vfwnmacc_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwnmacc_vv_entries = 2);
  check "vfwnmacc_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwnmacc_vf_entries = 2);
  check "vfwmsac_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwmsac_vv_entries = 2);
  check "vfwmsac_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwmsac_vf_entries = 2);
  check "vfwnmsac_vv_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwnmsac_vv_entries = 2);
  check "vfwnmsac_vf_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwnmsac_vf_entries = 2);
  check "vle8_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vle8_v_entries = 2);
  check "vle16_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vle16_v_entries = 2);
  check "vle32_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vle32_v_entries = 2);
  check "vle64_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vle64_v_entries = 2);
  check "vse8_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vse8_v_entries = 2);
  check "vse16_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vse16_v_entries = 2);
  check "vse32_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vse32_v_entries = 2);
  check "vse64_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vse64_v_entries = 2);
  check "vlm_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vlm_v_entries = 2);
  check "vsm_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsm_v_entries = 2);
  check "vle8ff_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vle8ff_v_entries = 2);
  check "vle16ff_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vle16ff_v_entries = 2);
  check "vle32ff_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vle32ff_v_entries = 2);
  check "vle64ff_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vle64ff_v_entries = 2);
  check "vlse8_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vlse8_v_entries = 2);
  check "vlse16_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vlse16_v_entries = 2);
  check "vlse32_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vlse32_v_entries = 2);
  check "vlse64_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vlse64_v_entries = 2);
  check "vsse8_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsse8_v_entries = 2);
  check "vsse16_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsse16_v_entries = 2);
  check "vsse32_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsse32_v_entries = 2);
  check "vsse64_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsse64_v_entries = 2);
  check "vluxei8_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vluxei8_v_entries = 2);
  check "vluxei16_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vluxei16_v_entries = 2);
  check "vluxei32_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vluxei32_v_entries = 2);
  check "vluxei64_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vluxei64_v_entries = 2);
  check "vloxei8_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vloxei8_v_entries = 2);
  check "vloxei16_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vloxei16_v_entries = 2);
  check "vloxei32_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vloxei32_v_entries = 2);
  check "vloxei64_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vloxei64_v_entries = 2);
  check "vsuxei8_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsuxei8_v_entries = 2);
  check "vsuxei16_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsuxei16_v_entries = 2);
  check "vsuxei32_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsuxei32_v_entries = 2);
  check "vsuxei64_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsuxei64_v_entries = 2);
  check "vsoxei8_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsoxei8_v_entries = 2);
  check "vsoxei16_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsoxei16_v_entries = 2);
  check "vsoxei32_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsoxei32_v_entries = 2);
  check "vsoxei64_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vsoxei64_v_entries = 2);
  check "vl1re8_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl1re8_v_entries = 2);
  check "vl1re16_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl1re16_v_entries = 2);
  check "vl1re32_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl1re32_v_entries = 2);
  check "vl1re64_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl1re64_v_entries = 2);
  check "vl2re8_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl2re8_v_entries = 2);
  check "vl2re16_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl2re16_v_entries = 2);
  check "vl2re32_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl2re32_v_entries = 2);
  check "vl2re64_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl2re64_v_entries = 2);
  check "vl4re8_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl4re8_v_entries = 2);
  check "vl4re16_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl4re16_v_entries = 2);
  check "vl4re32_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl4re32_v_entries = 2);
  check "vl4re64_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl4re64_v_entries = 2);
  check "vl8re8_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl8re8_v_entries = 2);
  check "vl8re16_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl8re16_v_entries = 2);
  check "vl8re32_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl8re32_v_entries = 2);
  check "vl8re64_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vl8re64_v_entries = 2);
  check "vs1r_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vs1r_v_entries = 2);
  check "vs2r_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vs2r_v_entries = 2);
  check "vs4r_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vs4r_v_entries = 2);
  check "vs8r_v_entries has 2 entries (rv_v, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vs8r_v_entries = 2);
  check "vclmul_vv_entries has 2 entries (rv_zvbc, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vclmul_vv_entries = 2);
  check "vclmul_vx_entries has 2 entries (rv_zvbc, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vclmul_vx_entries = 2);
  check "vclmulh_vv_entries has 2 entries (rv_zvbc, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vclmulh_vv_entries = 2);
  check "vclmulh_vx_entries has 2 entries (rv_zvbc, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vclmulh_vx_entries = 2);
  check "vghsh_vv_entries has 2 entries (rv_zvkg, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vghsh_vv_entries = 2);
  check "vgmul_vv_entries has 2 entries (rv_zvkg, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vgmul_vv_entries = 2);
  check
    "vsha2ms_vv_entries has 2 entries (rv_zvknha, one per profile, real Req_any with zvknhb/zvkn)"
    (List.length Isa_gen_difficult.vsha2ms_vv_entries = 2);
  check
    "vsha2ch_vv_entries has 2 entries (rv_zvknha, one per profile, real Req_any with zvknhb/zvkn)"
    (List.length Isa_gen_difficult.vsha2ch_vv_entries = 2);
  check
    "vsha2cl_vv_entries has 2 entries (rv_zvknha, one per profile, real Req_any with zvknhb/zvkn)"
    (List.length Isa_gen_difficult.vsha2cl_vv_entries = 2);
  check "vsm4k_vi_entries has 2 entries (rv_zvksed, one per profile, real Req_any with zvks)"
    (List.length Isa_gen_difficult.vsm4k_vi_entries = 2);
  check "vsm4r_vv_entries has 2 entries (rv_zvksed, one per profile, real Req_any with zvks)"
    (List.length Isa_gen_difficult.vsm4r_vv_entries = 2);
  check "vsm4r_vs_entries has 2 entries (rv_zvksed, one per profile, real Req_any with zvks)"
    (List.length Isa_gen_difficult.vsm4r_vs_entries = 2);
  check "vsm3c_vi_entries has 2 entries (rv_zvksh, one per profile, real Req_any with zvks)"
    (List.length Isa_gen_difficult.vsm3c_vi_entries = 2);
  check "vsm3me_vv_entries has 2 entries (rv_zvksh, one per profile, real Req_any with zvks)"
    (List.length Isa_gen_difficult.vsm3me_vv_entries = 2);
  check
    "vandn_vv_entries has 2 entries (rv_zvbb, one per profile, real Req_any with zvkb/zvkn/zvks)"
    (List.length Isa_gen_difficult.vandn_vv_entries = 2);
  check
    "vandn_vx_entries has 2 entries (rv_zvbb, one per profile, real Req_any with zvkb/zvkn/zvks)"
    (List.length Isa_gen_difficult.vandn_vx_entries = 2);
  check "vbrev_v_entries has 2 entries (rv_zvbb, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vbrev_v_entries = 2);
  check
    "vbrev8_v_entries has 2 entries (rv_zvbb, one per profile, real Req_any with zvkb/zvkn/zvks)"
    (List.length Isa_gen_difficult.vbrev8_v_entries = 2);
  check "vclz_v_entries has 2 entries (rv_zvbb, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vclz_v_entries = 2);
  check "vcpop_v_entries has 2 entries (rv_zvbb, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vcpop_v_entries = 2);
  check "vctz_v_entries has 2 entries (rv_zvbb, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vctz_v_entries = 2);
  check "vrev8_v_entries has 2 entries (rv_zvbb, one per profile, real Req_any with zvkb/zvkn/zvks)"
    (List.length Isa_gen_difficult.vrev8_v_entries = 2);
  check "vrol_vv_entries has 2 entries (rv_zvbb, one per profile, real Req_any with zvkb/zvkn/zvks)"
    (List.length Isa_gen_difficult.vrol_vv_entries = 2);
  check "vrol_vx_entries has 2 entries (rv_zvbb, one per profile, real Req_any with zvkb/zvkn/zvks)"
    (List.length Isa_gen_difficult.vrol_vx_entries = 2);
  check "vror_vv_entries has 2 entries (rv_zvbb, one per profile, real Req_any with zvkb/zvkn/zvks)"
    (List.length Isa_gen_difficult.vror_vv_entries = 2);
  check "vror_vx_entries has 2 entries (rv_zvbb, one per profile, real Req_any with zvkb/zvkn/zvks)"
    (List.length Isa_gen_difficult.vror_vx_entries = 2);
  check "vror_vi_entries has 2 entries (rv_zvbb, one per profile, real Req_any with zvkb/zvkn/zvks)"
    (List.length Isa_gen_difficult.vror_vi_entries = 2);
  check "vwsll_vv_entries has 2 entries (rv_zvbb, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwsll_vv_entries = 2);
  check "vwsll_vx_entries has 2 entries (rv_zvbb, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwsll_vx_entries = 2);
  check "vwsll_vi_entries has 2 entries (rv_zvbb, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vwsll_vi_entries = 2);
  check "vaesdf_vv_entries has 2 entries (rv_zvkned, one per profile, real Req_any with zvkn)"
    (List.length Isa_gen_difficult.vaesdf_vv_entries = 2);
  check "vaesdf_vs_entries has 2 entries (rv_zvkned, one per profile, real Req_any with zvkn)"
    (List.length Isa_gen_difficult.vaesdf_vs_entries = 2);
  check "vaesdm_vv_entries has 2 entries (rv_zvkned, one per profile, real Req_any with zvkn)"
    (List.length Isa_gen_difficult.vaesdm_vv_entries = 2);
  check "vaesdm_vs_entries has 2 entries (rv_zvkned, one per profile, real Req_any with zvkn)"
    (List.length Isa_gen_difficult.vaesdm_vs_entries = 2);
  check "vaesef_vv_entries has 2 entries (rv_zvkned, one per profile, real Req_any with zvkn)"
    (List.length Isa_gen_difficult.vaesef_vv_entries = 2);
  check "vaesef_vs_entries has 2 entries (rv_zvkned, one per profile, real Req_any with zvkn)"
    (List.length Isa_gen_difficult.vaesef_vs_entries = 2);
  check "vaesem_vv_entries has 2 entries (rv_zvkned, one per profile, real Req_any with zvkn)"
    (List.length Isa_gen_difficult.vaesem_vv_entries = 2);
  check "vaesem_vs_entries has 2 entries (rv_zvkned, one per profile, real Req_any with zvkn)"
    (List.length Isa_gen_difficult.vaesem_vs_entries = 2);
  check "vaesz_vs_entries has 2 entries (rv_zvkned, one per profile, real Req_any with zvkn)"
    (List.length Isa_gen_difficult.vaesz_vs_entries = 2);
  check "vaeskf1_vi_entries has 2 entries (rv_zvkned, one per profile, real Req_any with zvkn)"
    (List.length Isa_gen_difficult.vaeskf1_vi_entries = 2);
  check "vaeskf2_vi_entries has 2 entries (rv_zvkned, one per profile, real Req_any with zvkn)"
    (List.length Isa_gen_difficult.vaeskf2_vi_entries = 2);
  check "vfwcvtbf16_f_f_v_entries has 2 entries (rv_zvfbfmin, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwcvtbf16_f_f_v_entries = 2);
  check "vfncvtbf16_f_f_w_entries has 2 entries (rv_zvfbfmin, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfncvtbf16_f_f_w_entries = 2);
  check "vfwmaccbf16_vv_entries has 2 entries (rv_zvfbfwma, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwmaccbf16_vv_entries = 2);
  check "vfwmaccbf16_vf_entries has 2 entries (rv_zvfbfwma, one per profile, no Req_any)"
    (List.length Isa_gen_difficult.vfwmaccbf16_vf_entries = 2);
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
      + List.length Isa_gen_difficult.vadd_vi_entries
      + List.length Isa_gen_difficult.vsub_vv_entries
      + List.length Isa_gen_difficult.vsub_vx_entries
      + List.length Isa_gen_difficult.vrsub_vx_entries
      + List.length Isa_gen_difficult.vrsub_vi_entries
      + List.length Isa_gen_difficult.vand_vv_entries
      + List.length Isa_gen_difficult.vand_vx_entries
      + List.length Isa_gen_difficult.vand_vi_entries
      + List.length Isa_gen_difficult.vor_vv_entries
      + List.length Isa_gen_difficult.vor_vx_entries
      + List.length Isa_gen_difficult.vor_vi_entries
      + List.length Isa_gen_difficult.vxor_vv_entries
      + List.length Isa_gen_difficult.vxor_vx_entries
      + List.length Isa_gen_difficult.vxor_vi_entries
      + List.length Isa_gen_difficult.vsll_vv_entries
      + List.length Isa_gen_difficult.vsll_vx_entries
      + List.length Isa_gen_difficult.vsll_vi_entries
      + List.length Isa_gen_difficult.vsrl_vv_entries
      + List.length Isa_gen_difficult.vsrl_vx_entries
      + List.length Isa_gen_difficult.vsrl_vi_entries
      + List.length Isa_gen_difficult.vsra_vv_entries
      + List.length Isa_gen_difficult.vsra_vx_entries
      + List.length Isa_gen_difficult.vsra_vi_entries
      + List.length Isa_gen_difficult.vminu_vv_entries
      + List.length Isa_gen_difficult.vminu_vx_entries
      + List.length Isa_gen_difficult.vmin_vv_entries
      + List.length Isa_gen_difficult.vmin_vx_entries
      + List.length Isa_gen_difficult.vmaxu_vv_entries
      + List.length Isa_gen_difficult.vmaxu_vx_entries
      + List.length Isa_gen_difficult.vmax_vv_entries
      + List.length Isa_gen_difficult.vmax_vx_entries
      + List.length Isa_gen_difficult.vmul_vv_entries
      + List.length Isa_gen_difficult.vmul_vx_entries
      + List.length Isa_gen_difficult.vmulh_vv_entries
      + List.length Isa_gen_difficult.vmulh_vx_entries
      + List.length Isa_gen_difficult.vmulhu_vv_entries
      + List.length Isa_gen_difficult.vmulhu_vx_entries
      + List.length Isa_gen_difficult.vmulhsu_vv_entries
      + List.length Isa_gen_difficult.vmulhsu_vx_entries
      + List.length Isa_gen_difficult.vdivu_vv_entries
      + List.length Isa_gen_difficult.vdivu_vx_entries
      + List.length Isa_gen_difficult.vdiv_vv_entries
      + List.length Isa_gen_difficult.vdiv_vx_entries
      + List.length Isa_gen_difficult.vremu_vv_entries
      + List.length Isa_gen_difficult.vremu_vx_entries
      + List.length Isa_gen_difficult.vrem_vv_entries
      + List.length Isa_gen_difficult.vrem_vx_entries
      + List.length Isa_gen_difficult.vsaddu_vv_entries
      + List.length Isa_gen_difficult.vsaddu_vx_entries
      + List.length Isa_gen_difficult.vsaddu_vi_entries
      + List.length Isa_gen_difficult.vsadd_vv_entries
      + List.length Isa_gen_difficult.vsadd_vx_entries
      + List.length Isa_gen_difficult.vsadd_vi_entries
      + List.length Isa_gen_difficult.vssubu_vv_entries
      + List.length Isa_gen_difficult.vssubu_vx_entries
      + List.length Isa_gen_difficult.vssub_vv_entries
      + List.length Isa_gen_difficult.vssub_vx_entries
      + List.length Isa_gen_difficult.vaaddu_vv_entries
      + List.length Isa_gen_difficult.vaaddu_vx_entries
      + List.length Isa_gen_difficult.vaadd_vv_entries
      + List.length Isa_gen_difficult.vaadd_vx_entries
      + List.length Isa_gen_difficult.vasubu_vv_entries
      + List.length Isa_gen_difficult.vasubu_vx_entries
      + List.length Isa_gen_difficult.vasub_vv_entries
      + List.length Isa_gen_difficult.vasub_vx_entries
      + List.length Isa_gen_difficult.vnsrl_wv_entries
      + List.length Isa_gen_difficult.vnsrl_wx_entries
      + List.length Isa_gen_difficult.vnsrl_wi_entries
      + List.length Isa_gen_difficult.vnsra_wv_entries
      + List.length Isa_gen_difficult.vnsra_wx_entries
      + List.length Isa_gen_difficult.vnsra_wi_entries
      + List.length Isa_gen_difficult.vnclipu_wv_entries
      + List.length Isa_gen_difficult.vnclipu_wx_entries
      + List.length Isa_gen_difficult.vnclipu_wi_entries
      + List.length Isa_gen_difficult.vnclip_wv_entries
      + List.length Isa_gen_difficult.vnclip_wx_entries
      + List.length Isa_gen_difficult.vnclip_wi_entries
      + List.length Isa_gen_difficult.vssrl_vv_entries
      + List.length Isa_gen_difficult.vssrl_vx_entries
      + List.length Isa_gen_difficult.vssrl_vi_entries
      + List.length Isa_gen_difficult.vssra_vv_entries
      + List.length Isa_gen_difficult.vssra_vx_entries
      + List.length Isa_gen_difficult.vssra_vi_entries
      + List.length Isa_gen_difficult.vrgather_vv_entries
      + List.length Isa_gen_difficult.vrgather_vx_entries
      + List.length Isa_gen_difficult.vrgather_vi_entries
      + List.length Isa_gen_difficult.vrgatherei16_vv_entries
      + List.length Isa_gen_difficult.vwaddu_vv_entries
      + List.length Isa_gen_difficult.vwaddu_vx_entries
      + List.length Isa_gen_difficult.vwadd_vv_entries
      + List.length Isa_gen_difficult.vwadd_vx_entries
      + List.length Isa_gen_difficult.vwsubu_vv_entries
      + List.length Isa_gen_difficult.vwsubu_vx_entries
      + List.length Isa_gen_difficult.vwsub_vv_entries
      + List.length Isa_gen_difficult.vwsub_vx_entries
      + List.length Isa_gen_difficult.vwaddu_wv_entries
      + List.length Isa_gen_difficult.vwaddu_wx_entries
      + List.length Isa_gen_difficult.vwadd_wv_entries
      + List.length Isa_gen_difficult.vwadd_wx_entries
      + List.length Isa_gen_difficult.vwsubu_wv_entries
      + List.length Isa_gen_difficult.vwsubu_wx_entries
      + List.length Isa_gen_difficult.vwsub_wv_entries
      + List.length Isa_gen_difficult.vwsub_wx_entries
      + List.length Isa_gen_difficult.vwmulu_vv_entries
      + List.length Isa_gen_difficult.vwmulu_vx_entries
      + List.length Isa_gen_difficult.vwmulsu_vv_entries
      + List.length Isa_gen_difficult.vwmulsu_vx_entries
      + List.length Isa_gen_difficult.vwmul_vv_entries
      + List.length Isa_gen_difficult.vwmul_vx_entries
      + List.length Isa_gen_difficult.vsext_vf2_entries
      + List.length Isa_gen_difficult.vsext_vf4_entries
      + List.length Isa_gen_difficult.vsext_vf8_entries
      + List.length Isa_gen_difficult.vzext_vf2_entries
      + List.length Isa_gen_difficult.vzext_vf4_entries
      + List.length Isa_gen_difficult.vzext_vf8_entries
      + List.length Isa_gen_difficult.vmand_mm_entries
      + List.length Isa_gen_difficult.vmandn_mm_entries
      + List.length Isa_gen_difficult.vmor_mm_entries
      + List.length Isa_gen_difficult.vmxor_mm_entries
      + List.length Isa_gen_difficult.vmorn_mm_entries
      + List.length Isa_gen_difficult.vmnand_mm_entries
      + List.length Isa_gen_difficult.vmnor_mm_entries
      + List.length Isa_gen_difficult.vmxnor_mm_entries
      + List.length Isa_gen_difficult.vredsum_vs_entries
      + List.length Isa_gen_difficult.vredand_vs_entries
      + List.length Isa_gen_difficult.vredor_vs_entries
      + List.length Isa_gen_difficult.vredxor_vs_entries
      + List.length Isa_gen_difficult.vredminu_vs_entries
      + List.length Isa_gen_difficult.vredmin_vs_entries
      + List.length Isa_gen_difficult.vredmaxu_vs_entries
      + List.length Isa_gen_difficult.vredmax_vs_entries
      + List.length Isa_gen_difficult.vwredsumu_vs_entries
      + List.length Isa_gen_difficult.vwredsum_vs_entries
      + List.length Isa_gen_difficult.vmseq_vv_entries
      + List.length Isa_gen_difficult.vmseq_vx_entries
      + List.length Isa_gen_difficult.vmseq_vi_entries
      + List.length Isa_gen_difficult.vmsne_vv_entries
      + List.length Isa_gen_difficult.vmsne_vx_entries
      + List.length Isa_gen_difficult.vmsne_vi_entries
      + List.length Isa_gen_difficult.vmsltu_vv_entries
      + List.length Isa_gen_difficult.vmsltu_vx_entries
      + List.length Isa_gen_difficult.vmslt_vv_entries
      + List.length Isa_gen_difficult.vmslt_vx_entries
      + List.length Isa_gen_difficult.vmsleu_vv_entries
      + List.length Isa_gen_difficult.vmsleu_vx_entries
      + List.length Isa_gen_difficult.vmsleu_vi_entries
      + List.length Isa_gen_difficult.vmsle_vv_entries
      + List.length Isa_gen_difficult.vmsle_vx_entries
      + List.length Isa_gen_difficult.vmsle_vi_entries
      + List.length Isa_gen_difficult.vmsgtu_vx_entries
      + List.length Isa_gen_difficult.vmsgtu_vi_entries
      + List.length Isa_gen_difficult.vmsgt_vx_entries
      + List.length Isa_gen_difficult.vmsgt_vi_entries
      + List.length Isa_gen_difficult.vslideup_vx_entries
      + List.length Isa_gen_difficult.vslideup_vi_entries
      + List.length Isa_gen_difficult.vslidedown_vx_entries
      + List.length Isa_gen_difficult.vslidedown_vi_entries
      + List.length Isa_gen_difficult.vslide1up_vx_entries
      + List.length Isa_gen_difficult.vslide1down_vx_entries
      + List.length Isa_gen_difficult.vmacc_vv_entries
      + List.length Isa_gen_difficult.vmacc_vx_entries
      + List.length Isa_gen_difficult.vnmsac_vv_entries
      + List.length Isa_gen_difficult.vnmsac_vx_entries
      + List.length Isa_gen_difficult.vmadd_vv_entries
      + List.length Isa_gen_difficult.vmadd_vx_entries
      + List.length Isa_gen_difficult.vnmsub_vv_entries
      + List.length Isa_gen_difficult.vnmsub_vx_entries
      + List.length Isa_gen_difficult.vwmaccu_vv_entries
      + List.length Isa_gen_difficult.vwmaccu_vx_entries
      + List.length Isa_gen_difficult.vwmacc_vv_entries
      + List.length Isa_gen_difficult.vwmacc_vx_entries
      + List.length Isa_gen_difficult.vwmaccsu_vv_entries
      + List.length Isa_gen_difficult.vwmaccsu_vx_entries
      + List.length Isa_gen_difficult.vwmaccus_vx_entries
      + List.length Isa_gen_difficult.vid_v_entries
      + List.length Isa_gen_difficult.viota_m_entries
      + List.length Isa_gen_difficult.vcompress_vm_entries
      + List.length Isa_gen_difficult.vmsbf_m_entries
      + List.length Isa_gen_difficult.vmsif_m_entries
      + List.length Isa_gen_difficult.vmsof_m_entries
      + List.length Isa_gen_difficult.vcpop_m_entries
      + List.length Isa_gen_difficult.vfirst_m_entries
      + List.length Isa_gen_difficult.vadc_vvm_entries
      + List.length Isa_gen_difficult.vadc_vxm_entries
      + List.length Isa_gen_difficult.vadc_vim_entries
      + List.length Isa_gen_difficult.vmadc_vvm_entries
      + List.length Isa_gen_difficult.vmadc_vxm_entries
      + List.length Isa_gen_difficult.vmadc_vim_entries
      + List.length Isa_gen_difficult.vmadc_vv_entries
      + List.length Isa_gen_difficult.vmadc_vx_entries
      + List.length Isa_gen_difficult.vmadc_vi_entries
      + List.length Isa_gen_difficult.vsbc_vvm_entries
      + List.length Isa_gen_difficult.vsbc_vxm_entries
      + List.length Isa_gen_difficult.vmsbc_vvm_entries
      + List.length Isa_gen_difficult.vmsbc_vxm_entries
      + List.length Isa_gen_difficult.vmsbc_vv_entries
      + List.length Isa_gen_difficult.vmsbc_vx_entries
      + List.length Isa_gen_difficult.vmerge_vvm_entries
      + List.length Isa_gen_difficult.vmerge_vxm_entries
      + List.length Isa_gen_difficult.vmerge_vim_entries
      + List.length Isa_gen_difficult.vmv_x_s_entries
      + List.length Isa_gen_difficult.vmv_s_x_entries
      + List.length Isa_gen_difficult.vmv_v_v_entries
      + List.length Isa_gen_difficult.vmv_v_x_entries
      + List.length Isa_gen_difficult.vmv_v_i_entries
      + List.length Isa_gen_difficult.vmv1r_v_entries
      + List.length Isa_gen_difficult.vmv2r_v_entries
      + List.length Isa_gen_difficult.vmv4r_v_entries
      + List.length Isa_gen_difficult.vmv8r_v_entries
      + List.length Isa_gen_difficult.vsmul_vv_entries
      + List.length Isa_gen_difficult.vsmul_vx_entries
      + List.length Isa_gen_difficult.vfadd_vv_entries
      + List.length Isa_gen_difficult.vfadd_vf_entries
      + List.length Isa_gen_difficult.vfsub_vv_entries
      + List.length Isa_gen_difficult.vfsub_vf_entries
      + List.length Isa_gen_difficult.vfrsub_vf_entries
      + List.length Isa_gen_difficult.vfmul_vv_entries
      + List.length Isa_gen_difficult.vfmul_vf_entries
      + List.length Isa_gen_difficult.vfdiv_vv_entries
      + List.length Isa_gen_difficult.vfdiv_vf_entries
      + List.length Isa_gen_difficult.vfrdiv_vf_entries
      + List.length Isa_gen_difficult.vfmin_vv_entries
      + List.length Isa_gen_difficult.vfmin_vf_entries
      + List.length Isa_gen_difficult.vfmax_vv_entries
      + List.length Isa_gen_difficult.vfmax_vf_entries
      + List.length Isa_gen_difficult.vfsgnj_vv_entries
      + List.length Isa_gen_difficult.vfsgnj_vf_entries
      + List.length Isa_gen_difficult.vfsgnjn_vv_entries
      + List.length Isa_gen_difficult.vfsgnjn_vf_entries
      + List.length Isa_gen_difficult.vfsgnjx_vv_entries
      + List.length Isa_gen_difficult.vfsgnjx_vf_entries
      + List.length Isa_gen_difficult.vfsqrt_v_entries
      + List.length Isa_gen_difficult.vfrsqrt7_v_entries
      + List.length Isa_gen_difficult.vfrec7_v_entries
      + List.length Isa_gen_difficult.vfclass_v_entries
      + List.length Isa_gen_difficult.vfredosum_vs_entries
      + List.length Isa_gen_difficult.vfredusum_vs_entries
      + List.length Isa_gen_difficult.vfredmin_vs_entries
      + List.length Isa_gen_difficult.vfredmax_vs_entries
      + List.length Isa_gen_difficult.vmfeq_vv_entries
      + List.length Isa_gen_difficult.vmfeq_vf_entries
      + List.length Isa_gen_difficult.vmfle_vv_entries
      + List.length Isa_gen_difficult.vmfle_vf_entries
      + List.length Isa_gen_difficult.vmflt_vv_entries
      + List.length Isa_gen_difficult.vmflt_vf_entries
      + List.length Isa_gen_difficult.vmfne_vv_entries
      + List.length Isa_gen_difficult.vmfne_vf_entries
      + List.length Isa_gen_difficult.vmfgt_vf_entries
      + List.length Isa_gen_difficult.vmfge_vf_entries
      + List.length Isa_gen_difficult.vfmv_f_s_entries
      + List.length Isa_gen_difficult.vfmv_s_f_entries
      + List.length Isa_gen_difficult.vfmv_v_f_entries
      + List.length Isa_gen_difficult.vfmerge_vfm_entries
      + List.length Isa_gen_difficult.vfcvt_xu_f_v_entries
      + List.length Isa_gen_difficult.vfcvt_x_f_v_entries
      + List.length Isa_gen_difficult.vfcvt_f_xu_v_entries
      + List.length Isa_gen_difficult.vfcvt_f_x_v_entries
      + List.length Isa_gen_difficult.vfcvt_rtz_xu_f_v_entries
      + List.length Isa_gen_difficult.vfcvt_rtz_x_f_v_entries
      + List.length Isa_gen_difficult.vfmadd_vv_entries
      + List.length Isa_gen_difficult.vfmadd_vf_entries
      + List.length Isa_gen_difficult.vfnmadd_vv_entries
      + List.length Isa_gen_difficult.vfnmadd_vf_entries
      + List.length Isa_gen_difficult.vfmsub_vv_entries
      + List.length Isa_gen_difficult.vfmsub_vf_entries
      + List.length Isa_gen_difficult.vfnmsub_vv_entries
      + List.length Isa_gen_difficult.vfnmsub_vf_entries
      + List.length Isa_gen_difficult.vfmacc_vv_entries
      + List.length Isa_gen_difficult.vfmacc_vf_entries
      + List.length Isa_gen_difficult.vfnmacc_vv_entries
      + List.length Isa_gen_difficult.vfnmacc_vf_entries
      + List.length Isa_gen_difficult.vfmsac_vv_entries
      + List.length Isa_gen_difficult.vfmsac_vf_entries
      + List.length Isa_gen_difficult.vfnmsac_vv_entries
      + List.length Isa_gen_difficult.vfnmsac_vf_entries
      + List.length Isa_gen_difficult.vfslide1up_vf_entries
      + List.length Isa_gen_difficult.vfslide1down_vf_entries
      + List.length Isa_gen_difficult.vfwadd_vv_entries
      + List.length Isa_gen_difficult.vfwadd_vf_entries
      + List.length Isa_gen_difficult.vfwadd_wv_entries
      + List.length Isa_gen_difficult.vfwadd_wf_entries
      + List.length Isa_gen_difficult.vfwsub_vv_entries
      + List.length Isa_gen_difficult.vfwsub_vf_entries
      + List.length Isa_gen_difficult.vfwsub_wv_entries
      + List.length Isa_gen_difficult.vfwsub_wf_entries
      + List.length Isa_gen_difficult.vfwmul_vv_entries
      + List.length Isa_gen_difficult.vfwmul_vf_entries
      + List.length Isa_gen_difficult.vfwredosum_vs_entries
      + List.length Isa_gen_difficult.vfwredusum_vs_entries
      + List.length Isa_gen_difficult.vfwcvt_xu_f_v_entries
      + List.length Isa_gen_difficult.vfwcvt_x_f_v_entries
      + List.length Isa_gen_difficult.vfwcvt_f_xu_v_entries
      + List.length Isa_gen_difficult.vfwcvt_f_x_v_entries
      + List.length Isa_gen_difficult.vfwcvt_f_f_v_entries
      + List.length Isa_gen_difficult.vfwcvt_rtz_xu_f_v_entries
      + List.length Isa_gen_difficult.vfwcvt_rtz_x_f_v_entries
      + List.length Isa_gen_difficult.vfncvt_xu_f_w_entries
      + List.length Isa_gen_difficult.vfncvt_x_f_w_entries
      + List.length Isa_gen_difficult.vfncvt_f_xu_w_entries
      + List.length Isa_gen_difficult.vfncvt_f_x_w_entries
      + List.length Isa_gen_difficult.vfncvt_f_f_w_entries
      + List.length Isa_gen_difficult.vfncvt_rod_f_f_w_entries
      + List.length Isa_gen_difficult.vfncvt_rtz_xu_f_w_entries
      + List.length Isa_gen_difficult.vfncvt_rtz_x_f_w_entries
      + List.length Isa_gen_difficult.vfwmacc_vv_entries
      + List.length Isa_gen_difficult.vfwmacc_vf_entries
      + List.length Isa_gen_difficult.vfwnmacc_vv_entries
      + List.length Isa_gen_difficult.vfwnmacc_vf_entries
      + List.length Isa_gen_difficult.vfwmsac_vv_entries
      + List.length Isa_gen_difficult.vfwmsac_vf_entries
      + List.length Isa_gen_difficult.vfwnmsac_vv_entries
      + List.length Isa_gen_difficult.vfwnmsac_vf_entries
      + List.length Isa_gen_difficult.vle8_v_entries
      + List.length Isa_gen_difficult.vle16_v_entries
      + List.length Isa_gen_difficult.vle32_v_entries
      + List.length Isa_gen_difficult.vle64_v_entries
      + List.length Isa_gen_difficult.vse8_v_entries
      + List.length Isa_gen_difficult.vse16_v_entries
      + List.length Isa_gen_difficult.vse32_v_entries
      + List.length Isa_gen_difficult.vse64_v_entries
      + List.length Isa_gen_difficult.vlm_v_entries
      + List.length Isa_gen_difficult.vsm_v_entries
      + List.length Isa_gen_difficult.vle8ff_v_entries
      + List.length Isa_gen_difficult.vle16ff_v_entries
      + List.length Isa_gen_difficult.vle32ff_v_entries
      + List.length Isa_gen_difficult.vle64ff_v_entries
      + List.length Isa_gen_difficult.vlse8_v_entries
      + List.length Isa_gen_difficult.vlse16_v_entries
      + List.length Isa_gen_difficult.vlse32_v_entries
      + List.length Isa_gen_difficult.vlse64_v_entries
      + List.length Isa_gen_difficult.vsse8_v_entries
      + List.length Isa_gen_difficult.vsse16_v_entries
      + List.length Isa_gen_difficult.vsse32_v_entries
      + List.length Isa_gen_difficult.vluxei8_v_entries
      + List.length Isa_gen_difficult.vluxei16_v_entries
      + List.length Isa_gen_difficult.vluxei32_v_entries
      + List.length Isa_gen_difficult.vluxei64_v_entries
      + List.length Isa_gen_difficult.vloxei8_v_entries
      + List.length Isa_gen_difficult.vloxei16_v_entries
      + List.length Isa_gen_difficult.vloxei32_v_entries
      + List.length Isa_gen_difficult.vloxei64_v_entries
      + List.length Isa_gen_difficult.vsuxei8_v_entries
      + List.length Isa_gen_difficult.vsuxei16_v_entries
      + List.length Isa_gen_difficult.vsuxei32_v_entries
      + List.length Isa_gen_difficult.vsuxei64_v_entries
      + List.length Isa_gen_difficult.vsoxei8_v_entries
      + List.length Isa_gen_difficult.vsoxei16_v_entries
      + List.length Isa_gen_difficult.vsoxei32_v_entries
      + List.length Isa_gen_difficult.vsoxei64_v_entries
      + List.length Isa_gen_difficult.vsse64_v_entries
      + List.length Isa_gen_difficult.vl1re8_v_entries
      + List.length Isa_gen_difficult.vl1re16_v_entries
      + List.length Isa_gen_difficult.vl1re32_v_entries
      + List.length Isa_gen_difficult.vl1re64_v_entries
      + List.length Isa_gen_difficult.vl2re8_v_entries
      + List.length Isa_gen_difficult.vl2re16_v_entries
      + List.length Isa_gen_difficult.vl2re32_v_entries
      + List.length Isa_gen_difficult.vl2re64_v_entries
      + List.length Isa_gen_difficult.vl4re8_v_entries
      + List.length Isa_gen_difficult.vl4re16_v_entries
      + List.length Isa_gen_difficult.vl4re32_v_entries
      + List.length Isa_gen_difficult.vl4re64_v_entries
      + List.length Isa_gen_difficult.vl8re8_v_entries
      + List.length Isa_gen_difficult.vl8re16_v_entries
      + List.length Isa_gen_difficult.vl8re32_v_entries
      + List.length Isa_gen_difficult.vl8re64_v_entries
      + List.length Isa_gen_difficult.vs1r_v_entries
      + List.length Isa_gen_difficult.vs2r_v_entries
      + List.length Isa_gen_difficult.vs4r_v_entries
      + List.length Isa_gen_difficult.vs8r_v_entries
      + List.length Isa_gen_difficult.vclmul_vv_entries
      + List.length Isa_gen_difficult.vclmul_vx_entries
      + List.length Isa_gen_difficult.vclmulh_vv_entries
      + List.length Isa_gen_difficult.vclmulh_vx_entries
      + List.length Isa_gen_difficult.vghsh_vv_entries
      + List.length Isa_gen_difficult.vgmul_vv_entries
      + List.length Isa_gen_difficult.vsha2ms_vv_entries
      + List.length Isa_gen_difficult.vsha2ch_vv_entries
      + List.length Isa_gen_difficult.vsha2cl_vv_entries
      + List.length Isa_gen_difficult.vsm4k_vi_entries
      + List.length Isa_gen_difficult.vsm4r_vv_entries
      + List.length Isa_gen_difficult.vsm4r_vs_entries
      + List.length Isa_gen_difficult.vsm3c_vi_entries
      + List.length Isa_gen_difficult.vsm3me_vv_entries
      + List.length Isa_gen_difficult.vandn_vv_entries
      + List.length Isa_gen_difficult.vandn_vx_entries
      + List.length Isa_gen_difficult.vbrev_v_entries
      + List.length Isa_gen_difficult.vbrev8_v_entries
      + List.length Isa_gen_difficult.vclz_v_entries
      + List.length Isa_gen_difficult.vcpop_v_entries
      + List.length Isa_gen_difficult.vctz_v_entries
      + List.length Isa_gen_difficult.vrev8_v_entries
      + List.length Isa_gen_difficult.vrol_vv_entries
      + List.length Isa_gen_difficult.vrol_vx_entries
      + List.length Isa_gen_difficult.vror_vv_entries
      + List.length Isa_gen_difficult.vror_vx_entries
      + List.length Isa_gen_difficult.vror_vi_entries
      + List.length Isa_gen_difficult.vwsll_vv_entries
      + List.length Isa_gen_difficult.vwsll_vx_entries
      + List.length Isa_gen_difficult.vwsll_vi_entries
      + List.length Isa_gen_difficult.vaesdf_vv_entries
      + List.length Isa_gen_difficult.vaesdf_vs_entries
      + List.length Isa_gen_difficult.vaesdm_vv_entries
      + List.length Isa_gen_difficult.vaesdm_vs_entries
      + List.length Isa_gen_difficult.vaesef_vv_entries
      + List.length Isa_gen_difficult.vaesef_vs_entries
      + List.length Isa_gen_difficult.vaesem_vv_entries
      + List.length Isa_gen_difficult.vaesem_vs_entries
      + List.length Isa_gen_difficult.vaesz_vs_entries
      + List.length Isa_gen_difficult.vaeskf1_vi_entries
      + List.length Isa_gen_difficult.vaeskf2_vi_entries
      + List.length Isa_gen_difficult.vfwcvtbf16_f_f_v_entries
      + List.length Isa_gen_difficult.vfncvtbf16_f_f_w_entries
      + List.length Isa_gen_difficult.vfwmaccbf16_vv_entries
      + List.length Isa_gen_difficult.vfwmaccbf16_vf_entries)

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

(* vsub/vrsub/vand/vor/vxor: the same three OPIVV/OPIVX/OPIVI shapes
   {!test_vadd_domain} already covers, generalized across every mnemonic
   that reuses {!Isa_gen_difficult.opivv_entries}/[opivx_entries]/
   [opivi_entries]. *)
let test_v_opiv_domain () =
  let check_opivv entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs2, rs1 vector-register operands" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2"; "rs1" ]
          && List.map snd e.operands = [ "v1"; "v2"; "v3" ]);
        check
          (Printf.sprintf "%s: flags the vector-register-operands rule" e.case_id)
          (List.mem "vector-register-operands" e.rule_ids))
      entries
  in
  let check_opivx entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs2 vector registers and a GPR rs1" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2"; "rs1" ]
          && List.map snd e.operands = [ "v1"; "v2"; "a0" ]))
      entries
  in
  let check_opfvf entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs2 vector registers and an FPR rs1" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2"; "rs1" ]
          && List.map snd e.operands = [ "v1"; "v2"; "fa0" ]))
      entries
  in
  let check_opivi entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs2 vector registers and a signed immediate" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2"; "simm5" ]
          && List.map snd e.operands = [ "v1"; "v2"; "-5" ]))
      entries
  in
  (* [vsll.vi]/[vsrl.vi]/[vsra.vi]: the UNSIGNED [zimm5] shape - a different
     operand name/value pair from every other admitted [.vi] mnemonic,
     confirming {!Isa_gen_difficult.opivi_entry}'s optional
     [imm_name]/[imm_value] override actually reaches the built entry. *)
  let check_opivi_uimm ~imm_value entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs2 vector registers and an unsigned immediate" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2"; "zimm5" ]
          && List.map snd e.operands = [ "v1"; "v2"; imm_value ]))
      entries
  in
  (* [vmacc]/[vnmsac]/[vmadd]/[vnmsub]/[vwmaccu]/[vwmacc]/[vwmaccsu]/
     [vwmaccus]: the multiply-accumulate family's own reordered text
     operand order - [rd, rs1, rs2] rather than [check_opivv]/[check_opivx]'s
     [rd, rs2, rs1]. *)
  let check_opmacc_vv entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs1, rs2 vector-register operands" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs1"; "rs2" ]
          && List.map snd e.operands = [ "v1"; "v2"; "v3" ]);
        check
          (Printf.sprintf "%s: flags the vector-register-operands rule" e.case_id)
          (List.mem "vector-register-operands" e.rule_ids))
      entries
  in
  let check_opmacc_vx entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs2 vector registers and a GPR rs1, reordered" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs1"; "rs2" ]
          && List.map snd e.operands = [ "v1"; "a0"; "v3" ]))
      entries
  in
  let check_opfmacc_vf entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs2 vector registers and an FPR rs1, reordered" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs1"; "rs2" ]
          && List.map snd e.operands = [ "v1"; "fa0"; "v3" ]))
      entries
  in
  let check_vext entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs2 vector-register operands only" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2" ] && List.map snd e.operands = [ "v1"; "v2" ]);
        check
          (Printf.sprintf "%s: flags the vector-register-operands rule" e.case_id)
          (List.mem "vector-register-operands" e.rule_ids))
      entries
  in
  (* [vid.v]: the first family with no [vs2]/[vs1]/[rs1] operand at all,
     just a destination. *)
  let check_vid entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd only" e.case_id)
          (List.map fst e.operands = [ "rd" ] && List.map snd e.operands = [ "v1" ]))
      entries
  in
  (* [vcpop.m]/[vfirst.m]: {!check_vext}'s exact [rd, rs2] shape but with a
     GPR destination. *)
  let check_v_to_x_unary entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses a GPR rd and a vector-register rs2" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2" ] && List.map snd e.operands = [ "a0"; "v2" ]))
      entries
  in
  (* [vadc]/[vmadc]/[vsbc]/[vmsbc]'s "m"-suffixed variants: the same
     operand shape as {!check_opivv}/{!check_opivx}/{!check_opivi} plus a
     mandatory, literal [v0] 4th operand. *)
  let check_carry_m_vv entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf
             "%s: uses rd, rs2, rs1 vector-register operands plus a literal v0 carry-in" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2"; "rs1"; "vcarry" ]
          && List.map snd e.operands = [ "v1"; "v2"; "v3"; "v0" ]))
      entries
  in
  let check_carry_m_vx entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs2 vector registers, a GPR rs1, and a literal v0 carry-in"
             e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2"; "rs1"; "vcarry" ]
          && List.map snd e.operands = [ "v1"; "v2"; "a0"; "v0" ]))
      entries
  in
  let check_carry_m_vf entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs2 vector registers, an FPR rs1, and a literal v0 carry-in"
             e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2"; "rs1"; "vcarry" ]
          && List.map snd e.operands = [ "v1"; "v2"; "fa0"; "v0" ]))
      entries
  in
  let check_carry_m_vi entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf
             "%s: uses rd, rs2 vector registers, a signed immediate, and a literal v0 carry-in"
             e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2"; "simm5"; "vcarry" ]
          && List.map snd e.operands = [ "v1"; "v2"; "-5"; "v0" ]))
      entries
  in
  (* [vmv.x.s]/[vmv.s.x]/[vmv.v.v]/[.v.x]/[.v.i]/[vmv1r.v]/etc.: OP-V's
     scalar-move, unconditional-move, and whole-register-group-move
     families, each a plain two-operand shape with no masked sibling. *)
  let check_vmv_x_s entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses a GPR rd and a vector-register rs2" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2" ] && List.map snd e.operands = [ "a0"; "v2" ]))
      entries
  in
  let check_vmv_s_x entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses a vector-register rd and a GPR rs1" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs1" ] && List.map snd e.operands = [ "v1"; "a0" ]))
      entries
  in
  let check_vmv_v_vx ~rs1_value entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs1 operands with no vs2 operand at all" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs1" ]
          && List.map snd e.operands = [ "v1"; rs1_value ]))
      entries
  in
  let check_vfmv_f_s entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses an FPR rd and a vector-register rs2" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2" ] && List.map snd e.operands = [ "fa0"; "v2" ]))
      entries
  in
  let check_vfmv_s_f entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses a vector-register rd and an FPR rs1" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs1" ] && List.map snd e.operands = [ "v1"; "fa0" ]))
      entries
  in
  let check_vmv_v_i entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, simm5 operands with no vs2 operand at all" e.case_id)
          (List.map fst e.operands = [ "rd"; "simm5" ] && List.map snd e.operands = [ "v1"; "5" ]))
      entries
  in
  let check_whole_reg_move ~rd ~rs2 entries =
    List.iter
      (fun (e : Isa_gen_difficult.entry) ->
        check
          (Printf.sprintf "%s: uses rd, rs2 vector-register operands" e.case_id)
          (List.map fst e.operands = [ "rd"; "rs2" ] && List.map snd e.operands = [ rd; rs2 ]))
      entries
  in
  check_opivv Isa_gen_difficult.vsub_vv_entries;
  check_opivx Isa_gen_difficult.vsub_vx_entries;
  check_opivx Isa_gen_difficult.vrsub_vx_entries;
  check_opivi Isa_gen_difficult.vrsub_vi_entries;
  check_opivv Isa_gen_difficult.vand_vv_entries;
  check_opivx Isa_gen_difficult.vand_vx_entries;
  check_opivi Isa_gen_difficult.vand_vi_entries;
  check_opivv Isa_gen_difficult.vor_vv_entries;
  check_opivx Isa_gen_difficult.vor_vx_entries;
  check_opivi Isa_gen_difficult.vor_vi_entries;
  check_opivv Isa_gen_difficult.vxor_vv_entries;
  check_opivx Isa_gen_difficult.vxor_vx_entries;
  check_opivi Isa_gen_difficult.vxor_vi_entries;
  check_opivv Isa_gen_difficult.vsll_vv_entries;
  check_opivx Isa_gen_difficult.vsll_vx_entries;
  check_opivi_uimm ~imm_value:"31" Isa_gen_difficult.vsll_vi_entries;
  check_opivv Isa_gen_difficult.vsrl_vv_entries;
  check_opivx Isa_gen_difficult.vsrl_vx_entries;
  check_opivi_uimm ~imm_value:"5" Isa_gen_difficult.vsrl_vi_entries;
  check_opivv Isa_gen_difficult.vsra_vv_entries;
  check_opivx Isa_gen_difficult.vsra_vx_entries;
  check_opivi_uimm ~imm_value:"5" Isa_gen_difficult.vsra_vi_entries;
  check_opivv Isa_gen_difficult.vminu_vv_entries;
  check_opivx Isa_gen_difficult.vminu_vx_entries;
  check_opivv Isa_gen_difficult.vmin_vv_entries;
  check_opivx Isa_gen_difficult.vmin_vx_entries;
  check_opivv Isa_gen_difficult.vmaxu_vv_entries;
  check_opivx Isa_gen_difficult.vmaxu_vx_entries;
  check_opivv Isa_gen_difficult.vmax_vv_entries;
  check_opivx Isa_gen_difficult.vmax_vx_entries;
  check_opivv Isa_gen_difficult.vmul_vv_entries;
  check_opivx Isa_gen_difficult.vmul_vx_entries;
  check_opivv Isa_gen_difficult.vmulh_vv_entries;
  check_opivx Isa_gen_difficult.vmulh_vx_entries;
  check_opivv Isa_gen_difficult.vmulhu_vv_entries;
  check_opivx Isa_gen_difficult.vmulhu_vx_entries;
  check_opivv Isa_gen_difficult.vmulhsu_vv_entries;
  check_opivx Isa_gen_difficult.vmulhsu_vx_entries;
  check_opivv Isa_gen_difficult.vdivu_vv_entries;
  check_opivx Isa_gen_difficult.vdivu_vx_entries;
  check_opivv Isa_gen_difficult.vdiv_vv_entries;
  check_opivx Isa_gen_difficult.vdiv_vx_entries;
  check_opivv Isa_gen_difficult.vremu_vv_entries;
  check_opivx Isa_gen_difficult.vremu_vx_entries;
  check_opivv Isa_gen_difficult.vrem_vv_entries;
  check_opivx Isa_gen_difficult.vrem_vx_entries;
  check_opivv Isa_gen_difficult.vsaddu_vv_entries;
  check_opivx Isa_gen_difficult.vsaddu_vx_entries;
  check_opivi Isa_gen_difficult.vsaddu_vi_entries;
  check_opivv Isa_gen_difficult.vsadd_vv_entries;
  check_opivx Isa_gen_difficult.vsadd_vx_entries;
  check_opivi Isa_gen_difficult.vsadd_vi_entries;
  check_opivv Isa_gen_difficult.vssubu_vv_entries;
  check_opivx Isa_gen_difficult.vssubu_vx_entries;
  check_opivv Isa_gen_difficult.vssub_vv_entries;
  check_opivx Isa_gen_difficult.vssub_vx_entries;
  check_opivv Isa_gen_difficult.vaaddu_vv_entries;
  check_opivx Isa_gen_difficult.vaaddu_vx_entries;
  check_opivv Isa_gen_difficult.vaadd_vv_entries;
  check_opivx Isa_gen_difficult.vaadd_vx_entries;
  check_opivv Isa_gen_difficult.vasubu_vv_entries;
  check_opivx Isa_gen_difficult.vasubu_vx_entries;
  check_opivv Isa_gen_difficult.vasub_vv_entries;
  check_opivx Isa_gen_difficult.vasub_vx_entries;
  check_opivv Isa_gen_difficult.vnsrl_wv_entries;
  check_opivx Isa_gen_difficult.vnsrl_wx_entries;
  check_opivi_uimm ~imm_value:"31" Isa_gen_difficult.vnsrl_wi_entries;
  check_opivv Isa_gen_difficult.vnsra_wv_entries;
  check_opivx Isa_gen_difficult.vnsra_wx_entries;
  check_opivi_uimm ~imm_value:"31" Isa_gen_difficult.vnsra_wi_entries;
  check_opivv Isa_gen_difficult.vnclipu_wv_entries;
  check_opivx Isa_gen_difficult.vnclipu_wx_entries;
  check_opivi_uimm ~imm_value:"31" Isa_gen_difficult.vnclipu_wi_entries;
  check_opivv Isa_gen_difficult.vnclip_wv_entries;
  check_opivx Isa_gen_difficult.vnclip_wx_entries;
  check_opivi_uimm ~imm_value:"31" Isa_gen_difficult.vnclip_wi_entries;
  check_opivv Isa_gen_difficult.vssrl_vv_entries;
  check_opivx Isa_gen_difficult.vssrl_vx_entries;
  check_opivi_uimm ~imm_value:"31" Isa_gen_difficult.vssrl_vi_entries;
  check_opivv Isa_gen_difficult.vssra_vv_entries;
  check_opivx Isa_gen_difficult.vssra_vx_entries;
  check_opivi_uimm ~imm_value:"31" Isa_gen_difficult.vssra_vi_entries;
  check_opivv Isa_gen_difficult.vrgather_vv_entries;
  check_opivx Isa_gen_difficult.vrgather_vx_entries;
  check_opivi_uimm ~imm_value:"31" Isa_gen_difficult.vrgather_vi_entries;
  check_opivv Isa_gen_difficult.vrgatherei16_vv_entries;
  check_opivv Isa_gen_difficult.vwaddu_vv_entries;
  check_opivx Isa_gen_difficult.vwaddu_vx_entries;
  check_opivv Isa_gen_difficult.vwadd_vv_entries;
  check_opivx Isa_gen_difficult.vwadd_vx_entries;
  check_opivv Isa_gen_difficult.vwsubu_vv_entries;
  check_opivx Isa_gen_difficult.vwsubu_vx_entries;
  check_opivv Isa_gen_difficult.vwsub_vv_entries;
  check_opivx Isa_gen_difficult.vwsub_vx_entries;
  check_opivv Isa_gen_difficult.vwaddu_wv_entries;
  check_opivx Isa_gen_difficult.vwaddu_wx_entries;
  check_opivv Isa_gen_difficult.vwadd_wv_entries;
  check_opivx Isa_gen_difficult.vwadd_wx_entries;
  check_opivv Isa_gen_difficult.vwsubu_wv_entries;
  check_opivx Isa_gen_difficult.vwsubu_wx_entries;
  check_opivv Isa_gen_difficult.vwsub_wv_entries;
  check_opivx Isa_gen_difficult.vwsub_wx_entries;
  check_opivv Isa_gen_difficult.vwmulu_vv_entries;
  check_opivx Isa_gen_difficult.vwmulu_vx_entries;
  check_opivv Isa_gen_difficult.vwmulsu_vv_entries;
  check_opivx Isa_gen_difficult.vwmulsu_vx_entries;
  check_opivv Isa_gen_difficult.vwmul_vv_entries;
  check_opivx Isa_gen_difficult.vwmul_vx_entries;
  check_vext Isa_gen_difficult.vsext_vf2_entries;
  check_vext Isa_gen_difficult.vsext_vf4_entries;
  check_vext Isa_gen_difficult.vsext_vf8_entries;
  check_vext Isa_gen_difficult.vzext_vf2_entries;
  check_vext Isa_gen_difficult.vzext_vf4_entries;
  check_vext Isa_gen_difficult.vzext_vf8_entries;
  check_opivv Isa_gen_difficult.vmand_mm_entries;
  check_opivv Isa_gen_difficult.vmandn_mm_entries;
  check_opivv Isa_gen_difficult.vmor_mm_entries;
  check_opivv Isa_gen_difficult.vmxor_mm_entries;
  check_opivv Isa_gen_difficult.vmorn_mm_entries;
  check_opivv Isa_gen_difficult.vmnand_mm_entries;
  check_opivv Isa_gen_difficult.vmnor_mm_entries;
  check_opivv Isa_gen_difficult.vmxnor_mm_entries;
  check_opivv Isa_gen_difficult.vredsum_vs_entries;
  check_opivv Isa_gen_difficult.vredand_vs_entries;
  check_opivv Isa_gen_difficult.vredor_vs_entries;
  check_opivv Isa_gen_difficult.vredxor_vs_entries;
  check_opivv Isa_gen_difficult.vredminu_vs_entries;
  check_opivv Isa_gen_difficult.vredmin_vs_entries;
  check_opivv Isa_gen_difficult.vredmaxu_vs_entries;
  check_opivv Isa_gen_difficult.vredmax_vs_entries;
  check_opivv Isa_gen_difficult.vwredsumu_vs_entries;
  check_opivv Isa_gen_difficult.vwredsum_vs_entries;
  check_opivv Isa_gen_difficult.vmseq_vv_entries;
  check_opivx Isa_gen_difficult.vmseq_vx_entries;
  check_opivi Isa_gen_difficult.vmseq_vi_entries;
  check_opivv Isa_gen_difficult.vmsne_vv_entries;
  check_opivx Isa_gen_difficult.vmsne_vx_entries;
  check_opivi Isa_gen_difficult.vmsne_vi_entries;
  check_opivv Isa_gen_difficult.vmsltu_vv_entries;
  check_opivx Isa_gen_difficult.vmsltu_vx_entries;
  check_opivv Isa_gen_difficult.vmslt_vv_entries;
  check_opivx Isa_gen_difficult.vmslt_vx_entries;
  check_opivv Isa_gen_difficult.vmsleu_vv_entries;
  check_opivx Isa_gen_difficult.vmsleu_vx_entries;
  check_opivi Isa_gen_difficult.vmsleu_vi_entries;
  check_opivv Isa_gen_difficult.vmsle_vv_entries;
  check_opivx Isa_gen_difficult.vmsle_vx_entries;
  check_opivi Isa_gen_difficult.vmsle_vi_entries;
  check_opivx Isa_gen_difficult.vmsgtu_vx_entries;
  check_opivi Isa_gen_difficult.vmsgtu_vi_entries;
  check_opivx Isa_gen_difficult.vmsgt_vx_entries;
  check_opivi Isa_gen_difficult.vmsgt_vi_entries;
  check_opivx Isa_gen_difficult.vslideup_vx_entries;
  check_opivi_uimm ~imm_value:"31" Isa_gen_difficult.vslideup_vi_entries;
  check_opivx Isa_gen_difficult.vslidedown_vx_entries;
  check_opivi_uimm ~imm_value:"31" Isa_gen_difficult.vslidedown_vi_entries;
  check_opivx Isa_gen_difficult.vslide1up_vx_entries;
  check_opivx Isa_gen_difficult.vslide1down_vx_entries;
  check_opmacc_vv Isa_gen_difficult.vmacc_vv_entries;
  check_opmacc_vx Isa_gen_difficult.vmacc_vx_entries;
  check_opmacc_vv Isa_gen_difficult.vnmsac_vv_entries;
  check_opmacc_vx Isa_gen_difficult.vnmsac_vx_entries;
  check_opmacc_vv Isa_gen_difficult.vmadd_vv_entries;
  check_opmacc_vx Isa_gen_difficult.vmadd_vx_entries;
  check_opmacc_vv Isa_gen_difficult.vnmsub_vv_entries;
  check_opmacc_vx Isa_gen_difficult.vnmsub_vx_entries;
  check_opmacc_vv Isa_gen_difficult.vwmaccu_vv_entries;
  check_opmacc_vx Isa_gen_difficult.vwmaccu_vx_entries;
  check_opmacc_vv Isa_gen_difficult.vwmacc_vv_entries;
  check_opmacc_vx Isa_gen_difficult.vwmacc_vx_entries;
  check_opmacc_vv Isa_gen_difficult.vwmaccsu_vv_entries;
  check_opmacc_vx Isa_gen_difficult.vwmaccsu_vx_entries;
  check_opmacc_vx Isa_gen_difficult.vwmaccus_vx_entries;
  check_vid Isa_gen_difficult.vid_v_entries;
  check_vext Isa_gen_difficult.viota_m_entries;
  check_opivv Isa_gen_difficult.vcompress_vm_entries;
  check_vext Isa_gen_difficult.vmsbf_m_entries;
  check_vext Isa_gen_difficult.vmsif_m_entries;
  check_vext Isa_gen_difficult.vmsof_m_entries;
  check_v_to_x_unary Isa_gen_difficult.vcpop_m_entries;
  check_v_to_x_unary Isa_gen_difficult.vfirst_m_entries;
  check_carry_m_vv Isa_gen_difficult.vadc_vvm_entries;
  check_carry_m_vx Isa_gen_difficult.vadc_vxm_entries;
  check_carry_m_vi Isa_gen_difficult.vadc_vim_entries;
  check_carry_m_vv Isa_gen_difficult.vmadc_vvm_entries;
  check_carry_m_vx Isa_gen_difficult.vmadc_vxm_entries;
  check_carry_m_vi Isa_gen_difficult.vmadc_vim_entries;
  check_opivv Isa_gen_difficult.vmadc_vv_entries;
  check_opivx Isa_gen_difficult.vmadc_vx_entries;
  check_opivi Isa_gen_difficult.vmadc_vi_entries;
  check_carry_m_vv Isa_gen_difficult.vsbc_vvm_entries;
  check_carry_m_vx Isa_gen_difficult.vsbc_vxm_entries;
  check_carry_m_vv Isa_gen_difficult.vmsbc_vvm_entries;
  check_carry_m_vx Isa_gen_difficult.vmsbc_vxm_entries;
  check_opivv Isa_gen_difficult.vmsbc_vv_entries;
  check_opivx Isa_gen_difficult.vmsbc_vx_entries;
  check_carry_m_vv Isa_gen_difficult.vmerge_vvm_entries;
  check_carry_m_vx Isa_gen_difficult.vmerge_vxm_entries;
  check_carry_m_vi Isa_gen_difficult.vmerge_vim_entries;
  check_vmv_x_s Isa_gen_difficult.vmv_x_s_entries;
  check_vmv_s_x Isa_gen_difficult.vmv_s_x_entries;
  check_vmv_v_vx ~rs1_value:"v2" Isa_gen_difficult.vmv_v_v_entries;
  check_vmv_v_vx ~rs1_value:"a0" Isa_gen_difficult.vmv_v_x_entries;
  check_vmv_v_i Isa_gen_difficult.vmv_v_i_entries;
  check_whole_reg_move ~rd:"v1" ~rs2:"v2" Isa_gen_difficult.vmv1r_v_entries;
  check_whole_reg_move ~rd:"v2" ~rs2:"v4" Isa_gen_difficult.vmv2r_v_entries;
  check_whole_reg_move ~rd:"v4" ~rs2:"v8" Isa_gen_difficult.vmv4r_v_entries;
  check_whole_reg_move ~rd:"v8" ~rs2:"v16" Isa_gen_difficult.vmv8r_v_entries;
  check_opivv Isa_gen_difficult.vsmul_vv_entries;
  check_opivx Isa_gen_difficult.vsmul_vx_entries;
  check_opivv Isa_gen_difficult.vfadd_vv_entries;
  check_opfvf Isa_gen_difficult.vfadd_vf_entries;
  check_opivv Isa_gen_difficult.vfsub_vv_entries;
  check_opfvf Isa_gen_difficult.vfsub_vf_entries;
  check_opfvf Isa_gen_difficult.vfrsub_vf_entries;
  check_opivv Isa_gen_difficult.vfmul_vv_entries;
  check_opfvf Isa_gen_difficult.vfmul_vf_entries;
  check_opivv Isa_gen_difficult.vfdiv_vv_entries;
  check_opfvf Isa_gen_difficult.vfdiv_vf_entries;
  check_opfvf Isa_gen_difficult.vfrdiv_vf_entries;
  check_opivv Isa_gen_difficult.vfmin_vv_entries;
  check_opfvf Isa_gen_difficult.vfmin_vf_entries;
  check_opivv Isa_gen_difficult.vfmax_vv_entries;
  check_opfvf Isa_gen_difficult.vfmax_vf_entries;
  check_opivv Isa_gen_difficult.vfsgnj_vv_entries;
  check_opfvf Isa_gen_difficult.vfsgnj_vf_entries;
  check_opivv Isa_gen_difficult.vfsgnjn_vv_entries;
  check_opfvf Isa_gen_difficult.vfsgnjn_vf_entries;
  check_opivv Isa_gen_difficult.vfsgnjx_vv_entries;
  check_opfvf Isa_gen_difficult.vfsgnjx_vf_entries;
  check_vext Isa_gen_difficult.vfsqrt_v_entries;
  check_vext Isa_gen_difficult.vfrsqrt7_v_entries;
  check_vext Isa_gen_difficult.vfrec7_v_entries;
  check_vext Isa_gen_difficult.vfclass_v_entries;
  check_opivv Isa_gen_difficult.vfredosum_vs_entries;
  check_opivv Isa_gen_difficult.vfredusum_vs_entries;
  check_opivv Isa_gen_difficult.vfredmin_vs_entries;
  check_opivv Isa_gen_difficult.vfredmax_vs_entries;
  check_opivv Isa_gen_difficult.vmfeq_vv_entries;
  check_opfvf Isa_gen_difficult.vmfeq_vf_entries;
  check_opivv Isa_gen_difficult.vmfle_vv_entries;
  check_opfvf Isa_gen_difficult.vmfle_vf_entries;
  check_opivv Isa_gen_difficult.vmflt_vv_entries;
  check_opfvf Isa_gen_difficult.vmflt_vf_entries;
  check_opivv Isa_gen_difficult.vmfne_vv_entries;
  check_opfvf Isa_gen_difficult.vmfne_vf_entries;
  check_opfvf Isa_gen_difficult.vmfgt_vf_entries;
  check_opfvf Isa_gen_difficult.vmfge_vf_entries;
  check_vfmv_f_s Isa_gen_difficult.vfmv_f_s_entries;
  check_vfmv_s_f Isa_gen_difficult.vfmv_s_f_entries;
  check_vmv_v_vx ~rs1_value:"fa0" Isa_gen_difficult.vfmv_v_f_entries;
  check_carry_m_vf Isa_gen_difficult.vfmerge_vfm_entries;
  check_vext Isa_gen_difficult.vfcvt_xu_f_v_entries;
  check_vext Isa_gen_difficult.vfcvt_x_f_v_entries;
  check_vext Isa_gen_difficult.vfcvt_f_xu_v_entries;
  check_vext Isa_gen_difficult.vfcvt_f_x_v_entries;
  check_vext Isa_gen_difficult.vfcvt_rtz_xu_f_v_entries;
  check_vext Isa_gen_difficult.vfcvt_rtz_x_f_v_entries;
  check_opmacc_vv Isa_gen_difficult.vfmadd_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfmadd_vf_entries;
  check_opmacc_vv Isa_gen_difficult.vfnmadd_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfnmadd_vf_entries;
  check_opmacc_vv Isa_gen_difficult.vfmsub_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfmsub_vf_entries;
  check_opmacc_vv Isa_gen_difficult.vfnmsub_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfnmsub_vf_entries;
  check_opmacc_vv Isa_gen_difficult.vfmacc_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfmacc_vf_entries;
  check_opmacc_vv Isa_gen_difficult.vfnmacc_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfnmacc_vf_entries;
  check_opmacc_vv Isa_gen_difficult.vfmsac_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfmsac_vf_entries;
  check_opmacc_vv Isa_gen_difficult.vfnmsac_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfnmsac_vf_entries;
  check_opfvf Isa_gen_difficult.vfslide1up_vf_entries;
  check_opfvf Isa_gen_difficult.vfslide1down_vf_entries;
  check_opivv Isa_gen_difficult.vfwadd_vv_entries;
  check_opfvf Isa_gen_difficult.vfwadd_vf_entries;
  check_opivv Isa_gen_difficult.vfwadd_wv_entries;
  check_opfvf Isa_gen_difficult.vfwadd_wf_entries;
  check_opivv Isa_gen_difficult.vfwsub_vv_entries;
  check_opfvf Isa_gen_difficult.vfwsub_vf_entries;
  check_opivv Isa_gen_difficult.vfwsub_wv_entries;
  check_opfvf Isa_gen_difficult.vfwsub_wf_entries;
  check_opivv Isa_gen_difficult.vfwmul_vv_entries;
  check_opfvf Isa_gen_difficult.vfwmul_vf_entries;
  check_opivv Isa_gen_difficult.vfwredosum_vs_entries;
  check_opivv Isa_gen_difficult.vfwredusum_vs_entries;
  check_vext Isa_gen_difficult.vfwcvt_xu_f_v_entries;
  check_vext Isa_gen_difficult.vfwcvt_x_f_v_entries;
  check_vext Isa_gen_difficult.vfwcvt_f_xu_v_entries;
  check_vext Isa_gen_difficult.vfwcvt_f_x_v_entries;
  check_vext Isa_gen_difficult.vfwcvt_f_f_v_entries;
  check_vext Isa_gen_difficult.vfwcvt_rtz_xu_f_v_entries;
  check_vext Isa_gen_difficult.vfwcvt_rtz_x_f_v_entries;
  check_vext Isa_gen_difficult.vfncvt_xu_f_w_entries;
  check_vext Isa_gen_difficult.vfncvt_x_f_w_entries;
  check_vext Isa_gen_difficult.vfncvt_f_xu_w_entries;
  check_vext Isa_gen_difficult.vfncvt_f_x_w_entries;
  check_vext Isa_gen_difficult.vfncvt_f_f_w_entries;
  check_vext Isa_gen_difficult.vfncvt_rod_f_f_w_entries;
  check_vext Isa_gen_difficult.vfncvt_rtz_xu_f_w_entries;
  check_vext Isa_gen_difficult.vfncvt_rtz_x_f_w_entries;
  check_opmacc_vv Isa_gen_difficult.vfwmacc_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfwmacc_vf_entries;
  check_opmacc_vv Isa_gen_difficult.vfwnmacc_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfwnmacc_vf_entries;
  check_opmacc_vv Isa_gen_difficult.vfwmsac_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfwmsac_vf_entries;
  check_opmacc_vv Isa_gen_difficult.vfwnmsac_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfwnmsac_vf_entries;
  check_opivv Isa_gen_difficult.vclmul_vv_entries;
  check_opivx Isa_gen_difficult.vclmul_vx_entries;
  check_opivv Isa_gen_difficult.vclmulh_vv_entries;
  check_opivx Isa_gen_difficult.vclmulh_vx_entries;
  check_opivv Isa_gen_difficult.vghsh_vv_entries;
  check_vext Isa_gen_difficult.vgmul_vv_entries;
  check_opivv Isa_gen_difficult.vsha2ms_vv_entries;
  check_opivv Isa_gen_difficult.vsha2ch_vv_entries;
  check_opivv Isa_gen_difficult.vsha2cl_vv_entries;
  check_opivi_uimm ~imm_value:"5" Isa_gen_difficult.vsm4k_vi_entries;
  check_vext Isa_gen_difficult.vsm4r_vv_entries;
  check_vext Isa_gen_difficult.vsm4r_vs_entries;
  check_opivi_uimm ~imm_value:"5" Isa_gen_difficult.vsm3c_vi_entries;
  check_opivv Isa_gen_difficult.vsm3me_vv_entries;
  check_opivv Isa_gen_difficult.vandn_vv_entries;
  check_opivx Isa_gen_difficult.vandn_vx_entries;
  check_vext Isa_gen_difficult.vbrev_v_entries;
  check_vext Isa_gen_difficult.vbrev8_v_entries;
  check_vext Isa_gen_difficult.vclz_v_entries;
  check_vext Isa_gen_difficult.vcpop_v_entries;
  check_vext Isa_gen_difficult.vctz_v_entries;
  check_vext Isa_gen_difficult.vrev8_v_entries;
  check_opivv Isa_gen_difficult.vrol_vv_entries;
  check_opivx Isa_gen_difficult.vrol_vx_entries;
  check_opivv Isa_gen_difficult.vror_vv_entries;
  check_opivx Isa_gen_difficult.vror_vx_entries;
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses rd, rs2 vector registers and an unsigned zimm6 immediate"
           e.case_id)
        (List.map fst e.operands = [ "rd"; "rs2"; "zimm6" ]
        && List.map snd e.operands = [ "v1"; "v2"; "40" ]))
    Isa_gen_difficult.vror_vi_entries;
  check_opivv Isa_gen_difficult.vwsll_vv_entries;
  check_opivx Isa_gen_difficult.vwsll_vx_entries;
  check_opivi_uimm ~imm_value:"5" Isa_gen_difficult.vwsll_vi_entries;
  check_vext Isa_gen_difficult.vaesdf_vv_entries;
  check_vext Isa_gen_difficult.vaesdf_vs_entries;
  check_vext Isa_gen_difficult.vaesdm_vv_entries;
  check_vext Isa_gen_difficult.vaesdm_vs_entries;
  check_vext Isa_gen_difficult.vaesef_vv_entries;
  check_vext Isa_gen_difficult.vaesef_vs_entries;
  check_vext Isa_gen_difficult.vaesem_vv_entries;
  check_vext Isa_gen_difficult.vaesem_vs_entries;
  check_vext Isa_gen_difficult.vaesz_vs_entries;
  check_opivi_uimm ~imm_value:"5" Isa_gen_difficult.vaeskf1_vi_entries;
  check_opivi_uimm ~imm_value:"5" Isa_gen_difficult.vaeskf2_vi_entries;
  check_vext Isa_gen_difficult.vfwcvtbf16_f_f_v_entries;
  check_vext Isa_gen_difficult.vfncvtbf16_f_f_w_entries;
  check_opmacc_vv Isa_gen_difficult.vfwmaccbf16_vv_entries;
  check_opfmacc_vf Isa_gen_difficult.vfwmaccbf16_vf_entries

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

let test_v_ldst_domain () =
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses vd, base operands in that order" e.case_id)
        (List.map fst e.operands = [ "vd"; "base" ] && List.map snd e.operands = [ "v1"; "a0" ]);
      check
        (Printf.sprintf "%s: flags the vector-register-memory-operand rule" e.case_id)
        (List.mem "vector-register-memory-operand" e.rule_ids))
    (Isa_gen_difficult.vle8_v_entries @ Isa_gen_difficult.vle16_v_entries
   @ Isa_gen_difficult.vle32_v_entries @ Isa_gen_difficult.vle64_v_entries
   @ Isa_gen_difficult.vlm_v_entries @ Isa_gen_difficult.vle8ff_v_entries
   @ Isa_gen_difficult.vle16ff_v_entries @ Isa_gen_difficult.vle32ff_v_entries
   @ Isa_gen_difficult.vle64ff_v_entries @ Isa_gen_difficult.vl1re8_v_entries
   @ Isa_gen_difficult.vl1re16_v_entries @ Isa_gen_difficult.vl1re32_v_entries
   @ Isa_gen_difficult.vl1re64_v_entries @ Isa_gen_difficult.vl2re8_v_entries
   @ Isa_gen_difficult.vl2re16_v_entries @ Isa_gen_difficult.vl2re32_v_entries
   @ Isa_gen_difficult.vl2re64_v_entries @ Isa_gen_difficult.vl4re8_v_entries
   @ Isa_gen_difficult.vl4re16_v_entries @ Isa_gen_difficult.vl4re32_v_entries
   @ Isa_gen_difficult.vl4re64_v_entries @ Isa_gen_difficult.vl8re8_v_entries
   @ Isa_gen_difficult.vl8re16_v_entries @ Isa_gen_difficult.vl8re32_v_entries
   @ Isa_gen_difficult.vl8re64_v_entries);
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses vs3, base operands in that order" e.case_id)
        (List.map fst e.operands = [ "vs3"; "base" ] && List.map snd e.operands = [ "v1"; "a0" ]);
      check
        (Printf.sprintf "%s: flags the vector-register-memory-operand rule" e.case_id)
        (List.mem "vector-register-memory-operand" e.rule_ids))
    (Isa_gen_difficult.vse8_v_entries @ Isa_gen_difficult.vse16_v_entries
   @ Isa_gen_difficult.vse32_v_entries @ Isa_gen_difficult.vse64_v_entries
   @ Isa_gen_difficult.vsm_v_entries @ Isa_gen_difficult.vs1r_v_entries
   @ Isa_gen_difficult.vs2r_v_entries @ Isa_gen_difficult.vs4r_v_entries
   @ Isa_gen_difficult.vs8r_v_entries);
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses vd, base, rs2 operands in that order" e.case_id)
        (List.map fst e.operands = [ "vd"; "base"; "rs2" ]
        && List.map snd e.operands = [ "v1"; "a0"; "a1" ]);
      check
        (Printf.sprintf "%s: flags the stride-gpr-operand rule" e.case_id)
        (List.mem "stride-gpr-operand" e.rule_ids))
    (Isa_gen_difficult.vlse8_v_entries @ Isa_gen_difficult.vlse16_v_entries
   @ Isa_gen_difficult.vlse32_v_entries @ Isa_gen_difficult.vlse64_v_entries);
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses vs3, base, rs2 operands in that order" e.case_id)
        (List.map fst e.operands = [ "vs3"; "base"; "rs2" ]
        && List.map snd e.operands = [ "v1"; "a0"; "a1" ]);
      check
        (Printf.sprintf "%s: flags the stride-gpr-operand rule" e.case_id)
        (List.mem "stride-gpr-operand" e.rule_ids))
    (Isa_gen_difficult.vsse8_v_entries @ Isa_gen_difficult.vsse16_v_entries
   @ Isa_gen_difficult.vsse32_v_entries @ Isa_gen_difficult.vsse64_v_entries);
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses vd, base, vs2 operands in that order" e.case_id)
        (List.map fst e.operands = [ "vd"; "base"; "vs2" ]
        && List.map snd e.operands = [ "v1"; "a0"; "v2" ]);
      check
        (Printf.sprintf "%s: flags the index-vreg-operand rule" e.case_id)
        (List.mem "index-vreg-operand" e.rule_ids))
    (Isa_gen_difficult.vluxei8_v_entries @ Isa_gen_difficult.vluxei16_v_entries
   @ Isa_gen_difficult.vluxei32_v_entries @ Isa_gen_difficult.vluxei64_v_entries
   @ Isa_gen_difficult.vloxei8_v_entries @ Isa_gen_difficult.vloxei16_v_entries
   @ Isa_gen_difficult.vloxei32_v_entries @ Isa_gen_difficult.vloxei64_v_entries);
  List.iter
    (fun (e : Isa_gen_difficult.entry) ->
      check
        (Printf.sprintf "%s: uses vs3, base, vs2 operands in that order" e.case_id)
        (List.map fst e.operands = [ "vs3"; "base"; "vs2" ]
        && List.map snd e.operands = [ "v1"; "a0"; "v2" ]);
      check
        (Printf.sprintf "%s: flags the index-vreg-operand rule" e.case_id)
        (List.mem "index-vreg-operand" e.rule_ids))
    (Isa_gen_difficult.vsuxei8_v_entries @ Isa_gen_difficult.vsuxei16_v_entries
   @ Isa_gen_difficult.vsuxei32_v_entries @ Isa_gen_difficult.vsuxei64_v_entries
   @ Isa_gen_difficult.vsoxei8_v_entries @ Isa_gen_difficult.vsoxei16_v_entries
   @ Isa_gen_difficult.vsoxei32_v_entries @ Isa_gen_difficult.vsoxei64_v_entries)

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
  test_v_opiv_domain ();
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
  test_v_ldst_domain ();
  test_render_source_lines ();
  test_build_wraps_lines_before_and_after ();
  if !failures > 0 then (
    Printf.printf "isa-gen-difficult: %d of %d checks failed\n" !failures !checks;
    exit 1)
  else Printf.printf "isa-gen-difficult: all %d checks passed\n" !checks
