type row = {
  id : string;
  source : string;
  families : string list;
  capability : string;
  evidence : string;
  task : string;
  reopening_gate : string;
}

(* Ownership is by source and family name. The families are the source-native groupings
   Isa_family_admission reports: riscv-opcodes extension files, and XED ISA_SET labels. Grouping
   many families under one row is deliberate - most share the machinery that is missing - but a
   row still lists each family by name, so nothing hides behind a pattern. *)
let rows =
  [
    {
      id = "RES-RV-BASE-INT";
      source = "riscv_opcodes";
      families = [ "rv_i"; "rv32_i"; "rv64_i" ];
      capability =
        "Source-derived recipes for the base integer ISA that the assembler already encodes: \
         memory operands (imm(rs1)) for loads/stores, PC-relative branch/jump operands, \
         upper-immediate and shift-amount shapes, and the remaining pseudo-ops (j, jr, \
         branch-on-zero). mv, snez, neg, seqz, sltz, sgtz, zext.b, sext.w, nop and ret are now \
         promoted as the alias class - neg/seqz/sltz/sgtz/zext.b needed a new encoder alias each \
         (GNU as 2.44 accepts them, this assembler previously did not); the rest reused an \
         existing one.";
      evidence =
        "family-admission reports these families as lacking a normalizer rule; the assembler \
         already emits them (asm/test/targets transcripts, CompCert runtime fixtures)";
      task = "GEN-05-RV-BASE";
      reopening_gate =
        "each remaining pseudo-op has an encoder alias, a normalizer rule and a differential case \
         per profile; loads/stores/branches have an operand-shape rule and a relocation-free or \
         controlled-address case";
    };
    {
      id = "RES-RV-COMPRESSED";
      source = "riscv_opcodes";
      families =
        [
          "rv_c";
          "rv32_c";
          "rv64_c";
          "rv_c_d";
          "rv32_c_f";
          "rv_c_zicfiss";
          "rv_c_zihintntl";
          "rv_zcb";
          "rv64_zcb";
          "rv_zcmp";
          "rv_zcmt";
          "rv_zcmop";
          "rv32_zclsd";
        ];
      capability =
        "Compressed (16-bit) form recipes: the register subsets, nonzero and split-immediate \
         constraints, and an explicit .option rvc policy so baseline tests never compress \
         opportunistically.";
      evidence =
        "family-admission credits c.addi, the CA-format register-register class (c.and/c.or/ \
         c.xor/c.sub on both profiles, c.addw/c.subw RV64-only), and the CR-format \
         register-register cluster (c.jr/c.jalr/c.mv/c.add/c.ebreak, full 0..31 GPR space, no \
         compressed-register-subset restriction) in this group; a compressed-register-subset \
         operand domain (Riscv_gpr_c, x8..x15) now exists in the normalizer for the CA class, and \
         the CR cluster's own x0 rules (reserved for c.jr/c.jalr's rs1, a documented HINT for \
         c.mv/c.add's rd) are modeled directly on the plain gpr domain - but every other \
         compressed form (loads/stores, branches, li/lui, addi16sp/4spn, andi/srli/srai, the \
         *w/RV64-only siblings, and every rv_c_d/rv32_c_f/rv_zcb/rv_zcmp/... sub-extension) \
         remains blocked; width derivation is already per record (CAP-01)";
      task = "GEN-05-RV-C";
      reopening_gate =
        "a compressed-register-subset operand domain exists in the normalizer (done - see \
         Riscv_gpr_c) and c.* cases assemble under -march=..._c/_zca with --features/rvc scope \
         recorded (done for the CA-format and CR-format register-register classes; still open for \
         every other compressed form)";
    };
    {
      id = "RES-RV-FP";
      source = "riscv_opcodes";
      families =
        [
          "rv_f";
          "rv_q";
          "rv64_d";
          "rv64_q";
          "rv_zfh";
          "rv64_zfh";
          "rv_zfhmin";
          "rv_zfbfmin";
          "rv_f_zfa";
          "rv_d_zfa";
          "rv32_d_zfa";
          "rv_q_zfa";
          "rv64_q_zfa";
          "rv_zfh_zfa";
          "rv_d_zfhmin";
          "rv_q_zfhmin";
        ];
      capability =
        "Floating-point leftovers: fcsr access pseudo-ops (frcsr, frflags, frrm, fscsr, fsflags, \
         fsflagsi, fsrm, fsrmi), quad and half precision, bfloat16 conversion, and Zfa. Scalar \
         single/double arithmetic is already promoted; so is the sign-injection/move alias class \
         (fabs.s, fabs.d, fneg.s, fneg.d, fmv.s, fmv.d, fmv.x.s, fmv.s.x) - rv_d's own pseudo-ops \
         were entirely that class, so it no longer names a blocked family here.";
      evidence =
        "family-admission: rv_f's fcsr pseudo-ops, and all of rv_q and rv_zfh, are unhandled";
      task = "GEN-05-RV-FP";
      reopening_gate =
        "each precision has an FP-register operand class and rounding-mode recipe verified against \
         a GAS that accepts the extension";
    };
    {
      id = "RES-RV-ATOMIC-SYNC";
      source = "riscv_opcodes";
      families = [ "rv_zabha"; "rv_zabha_zacas"; "rv_zacas"; "rv64_zacas"; "rv_zalasr"; "rv_zawrs" ];
      capability =
        "Sub-word atomics (Zabha), compare-and-swap (Zacas), load-acquire/store-release (Zalasr) \
         and wait-on-reservation (Zawrs): the ordering-suffix and address-only operand shapes.";
      evidence =
        "family-admission: rv_zabha and rv_zalasr are entirely unhandled; base A (rv_a) is promoted";
      task = "GEN-05-RV-ATOMIC";
      reopening_gate =
        "the .aq/.rl suffix shape used by base A is reused and each extension has a GAS-version \
         probe";
    };
    {
      id = "RES-RV-PRIV";
      source = "riscv_opcodes";
      families =
        [
          "rv_h";
          "rv64_h";
          "rv_s";
          "rv_system";
          "rv_svinval";
          "rv_svinval_h";
          "rv_smrnmi";
          "rv_sdext";
          "rv_ssctr";
        ];
      capability =
        "Hypervisor, supervisor, debug and privileged-system instructions: forms whose \
         availability depends on privilege mode and platform extensions that -march alone does not \
         select.";
      evidence =
        "family-admission: rv_h is entirely unhandled; the normalized requirement type has no \
         privilege-mode predicate";
      task = "GEN-05-RV-PRIV";
      reopening_gate =
        "a privilege-mode requirement predicate exists in Isa_norm_model and GAS -march spellings \
         are probed per extension";
    };
    {
      id = "RES-RV-HINT-MISC";
      source = "riscv_opcodes";
      families =
        [
          "rv_zimop";
          "rv_zicbo";
          "rv_zicfilp";
          "rv_zicfiss";
          "rv_zihintntl";
          "rv_zifencei";
          "rv_zicntr";
          "rv32_zicntr";
          "rv32_zilsd";
        ];
      capability =
        "May-be-operation, cache-block, control-flow-integrity, non-temporal-hint, fence.i, \
         counter-read and paired-load/store forms: mostly single-mnemonic families that each need \
         a feature-name mapping and a GAS probe.";
      evidence = "family-admission: rv_zimop, rv_zicbo and rv_zicfiss are entirely unhandled";
      task = "GEN-05-RV-MISC";
      reopening_gate =
        "each family has a feature mapping in Isa_norm_riscv and an installed-GAS probe recorded";
    };
    {
      id = "RES-RV-ZBA-UW";
      source = "riscv_opcodes";
      families = [ "rv64_zba" ];
      capability =
        "Zba word-operand leftovers (add.uw, slli.uw, zext.w): the shift-amount and unsigned-word \
         operand shapes on RV64.";
      evidence =
        "family-admission: add.uw, slli.uw and zext.w are unhandled; sh1add.uw/sh2add.uw/sh3add.uw \
         are promoted";
      task = "GEN-05-RV-ZBA";
      reopening_gate =
        "add.uw uses the R-type shape, slli.uw the 6-bit shamt shape, zext.w the two-GPR pseudo \
         shape";
    };
    {
      id = "RES-X86-EVEX";
      source = "xed_resolved";
      families =
        [
          "AVX10_2_BF16_128";
          "AVX10_2_BF16_256";
          "AVX10_2_BF16_512";
          "AVX10_2_BF16_SCALAR";
          "AVX10_MOVRS_128";
          "AVX10_MOVRS_256";
          "AVX10_MOVRS_512";
          "AVX10_V2_AUX_128";
          "AVX10_V2_AUX_256";
          "AVX10_V2_AUX_512";
          "AVX512BW_128";
          "AVX512BW_128N";
          "AVX512BW_256";
          "AVX512BW_512";
          "AVX512BW_KOPD";
          "AVX512BW_KOPQ";
          "AVX512CD_128";
          "AVX512CD_256";
          "AVX512CD_512";
          "AVX512DQ_128";
          "AVX512DQ_128N";
          "AVX512DQ_256";
          "AVX512DQ_512";
          "AVX512DQ_KOPB";
          "AVX512DQ_KOPW";
          "AVX512DQ_SCALAR";
          "AVX512ER_512";
          "AVX512ER_SCALAR";
          "AVX512F_128";
          "AVX512F_128N";
          "AVX512F_256";
          "AVX512F_512";
          "AVX512F_KOPW";
          "AVX512F_SCALAR";
          "AVX512PF_512";
          "AVX512_4FMAPS_512";
          "AVX512_4FMAPS_SCALAR";
          "AVX512_4VNNIW_512";
          "AVX512_BF16_128";
          "AVX512_BF16_256";
          "AVX512_BF16_512";
          "AVX512_BITALG_128";
          "AVX512_BITALG_256";
          "AVX512_BITALG_512";
          "AVX512_COM_EF_SCALAR";
          "AVX512_FP16_128";
          "AVX512_FP16_128N";
          "AVX512_FP16_256";
          "AVX512_FP16_512";
          "AVX512_FP16_CONVERT_128";
          "AVX512_FP16_CONVERT_256";
          "AVX512_FP16_CONVERT_512";
          "AVX512_FP16_SCALAR";
          "AVX512_FP8_CONVERT_128";
          "AVX512_FP8_CONVERT_256";
          "AVX512_FP8_CONVERT_512";
          "AVX512_GFNI_128";
          "AVX512_GFNI_256";
          "AVX512_GFNI_512";
          "AVX512_IFMA_128";
          "AVX512_IFMA_256";
          "AVX512_IFMA_512";
          "AVX512_MEDIAX_128";
          "AVX512_MEDIAX_256";
          "AVX512_MEDIAX_512";
          "AVX512_MINMAX_128";
          "AVX512_MINMAX_256";
          "AVX512_MINMAX_512";
          "AVX512_MINMAX_SCALAR";
          "AVX512_MOVZXC_128";
          "AVX512_SAT_CVT_128";
          "AVX512_SAT_CVT_256";
          "AVX512_SAT_CVT_512";
          "AVX512_SAT_CVT_DS_128";
          "AVX512_SAT_CVT_DS_256";
          "AVX512_SAT_CVT_DS_512";
          "AVX512_SAT_CVT_DS_SCALAR";
          "AVX512_VAES_128";
          "AVX512_VAES_256";
          "AVX512_VAES_512";
          "AVX512_VBMI2_128";
          "AVX512_VBMI2_256";
          "AVX512_VBMI2_512";
          "AVX512_VBMI_128";
          "AVX512_VBMI_256";
          "AVX512_VBMI_512";
          "AVX512_VNNI_128";
          "AVX512_VNNI_256";
          "AVX512_VNNI_512";
          "AVX512_VNNI_FP16_128";
          "AVX512_VNNI_FP16_256";
          "AVX512_VNNI_FP16_512";
          "AVX512_VNNI_INT16_128";
          "AVX512_VNNI_INT16_256";
          "AVX512_VNNI_INT16_512";
          "AVX512_VNNI_INT8_128";
          "AVX512_VNNI_INT8_256";
          "AVX512_VNNI_INT8_512";
          "AVX512_VP2INTERSECT_128";
          "AVX512_VP2INTERSECT_256";
          "AVX512_VP2INTERSECT_512";
          "AVX512_VPCLMULQDQ_128";
          "AVX512_VPCLMULQDQ_256";
          "AVX512_VPCLMULQDQ_512";
          "AVX512_VPOPCNTDQ_128";
          "AVX512_VPOPCNTDQ_256";
          "AVX512_VPOPCNTDQ_512";
        ];
      capability =
        "EVEX (0x62) encoding beyond unmasked 512-bit register-register packed-float: opmask \
         registers with {%k}/{z}, embedded broadcast and rounding/SAE (the BCRC records), disp8*N \
         compressed displacements, xmm/ymm/zmm16-31 (R'/V'/X), 128/256-bit EVEX, and the \
         integer/BW/DQ/FP16/VNNI/IFMA/VBMI/BF16/AVX10 mnemonics.";
      evidence =
        "family-admission: only the unmasked 512-bit register-register packed-float subset of \
         AVX512F_512 is promoted; the BCRC=1 embedded-rounding records are reported as diagnosed";
      task = "GEN-05-X86-EVEX";
      reopening_gate =
        "memory operands and opmask decoration are implemented in the encoder and a per-family GAS \
         -march probe exists";
    };
    {
      id = "RES-X86-VEX";
      source = "xed_resolved";
      families =
        [
          "AVX";
          "AVX2";
          "AVX2GATHER";
          "AVXAES";
          "AVX_GFNI";
          "AVX_IFMA";
          "AVX_NE_CONVERT";
          "AVX_VNNI";
          "AVX_VNNI_INT16";
          "AVX_VNNI_INT8";
          "F16C";
          "FMA";
          "FMA4";
          "SHA512";
          "SM3";
          "SM4";
          "SM4_128";
          "SM4_256";
          "SM4_512";
          "VAES";
          "VPCLMULQDQ";
          "XOP";
        ];
      capability =
        "VEX/XOP forms not yet admitted: four-operand and is4 encodings (FMA4, XOP), gathers (VSIB \
         addressing), mixed xmm/ymm widths (shifts by xmm count, VPMOVSX/ZX, lane extract/insert), \
         imm8-selector forms, VZEROUPPER/ALL, src2 registers 8-15 in the two-byte prefix, and the \
         FMA/F16C/VAES/GFNI/VNNI families.";
      evidence =
        "family-admission: AVX and AVX2 are partly promoted; FMA and XOP are entirely unhandled";
      task = "GEN-05-X86-VEX";
      reopening_gate = "VSIB addressing and the three-byte prefix for src2>=8 exist in the encoder";
    };
    {
      id = "RES-X86-LEGACY-SIMD";
      source = "xed_resolved";
      families =
        [
          "3DNOW";
          "ACE_1";
          "AES";
          "GFNI";
          "PCLMULQDQ";
          "PENTIUMMMX";
          "SHA";
          "SSE";
          "SSE2";
          "SSE2MMX";
          "SSE3";
          "SSE4";
          "SSE42";
          "SSE4a";
          "SSEMXCSR";
          "SSE_PREFETCH";
          "SSSE3MMX";
        ];
      capability =
        "Legacy MMX/SSE/3DNow remainders: the mm0-mm7 register class, imm8-selector forms, \
         mandatory-prefix combinations, and AES/PCLMUL/SHA/GFNI legacy encodings. SSE2 packed and \
         scalar forms are largely promoted.";
      evidence =
        "family-admission: SSE2 and SSE are mostly promoted; PENTIUMMMX and 3DNOW are entirely \
         unhandled";
      task = "GEN-05-X86-SIMD";
      reopening_gate =
        "an MMX register class exists in the normalized model and each family has a GAS -march \
         spelling";
    };
    {
      id = "RES-X86-X87";
      source = "xed_resolved";
      families = [ "FCMOV"; "FCOMI"; "SSE3X87"; "X87" ];
      capability =
        "x87 beyond the thirteen implemented forms: real32/64/80 and int16/32/64 memory widths \
         (fadds/fsubs also lack a register-indirect operand), stack-register forms in both \
         directions, popping and reversed arithmetic, comparison and control forms, FCMOV/FCOMI.";
      evidence =
        "family-admission: all but one X87 record is unhandled; fadds 4(%esp) is rejected \
         (test_components transcript) while GNU as accepts it";
      task = "GEN-05-X86-X87";
      reopening_gate =
        "the x87 component grows its memory-width table and both stack directions are recipes with \
         GAS spelling probes";
    };
    {
      id = "RES-X86-BASE-INT";
      source = "xed_resolved";
      families =
        [
          "ADOX_ADCX";
          "BMI1";
          "BMI2";
          "CLDEMOTE";
          "CLFLUSHOPT";
          "CLFSH";
          "CLWB";
          "CLZERO";
          "CMOV";
          "CMPXCHG16B";
          "FAT_NOP";
          "I186";
          "I286PROTECTED";
          "I286REAL";
          "I386";
          "I486";
          "I486REAL";
          "I86";
          "LAHF";
          "LONGMODE";
          "LZCNT";
          "MOVBE";
          "PAUSE";
          "PENTIUMREAL";
          "POPCNT";
          "PPRO";
          "PPRO_UD0_LONG";
          "PPRO_UD0_SHORT";
          "PREFETCHWT1";
          "PREFETCH_NOP";
          "RDRAND";
          "RDSEED";
          "RDTSCP";
          "RDWRFSGS";
          "SEP";
          "TBM";
        ];
      capability =
        "Integer, string, stack, flag, bit-manipulation and data-movement instructions outside the \
         admitted ALU space: mode-dependent operand sizes, implicit-operand and string forms, \
         segment registers, BMI/LZCNT/POPCNT, atomics (CMPXCHG16B), and cache/prefetch hints.";
      evidence = "family-admission: most of I86 and all of I386, CMOV and BMI2 are unhandled";
      task = "GEN-05-X86-INT";
      reopening_gate =
        "implicit-operand syntax and address-size/segment recipes exist and each family has a GAS \
         spelling probe";
    };
    {
      id = "RES-X86-APX";
      source = "xed_resolved";
      families =
        [
          "APX_F";
          "APX_F_ADX";
          "APX_F_ADX_N3";
          "APX_F_AMX";
          "APX_F_AMX_BASE";
          "APX_F_AMX_MOVRS";
          "APX_F_BMI1";
          "APX_F_BMI1_N3";
          "APX_F_BMI2";
          "APX_F_BMI2_N3";
          "APX_F_CET";
          "APX_F_CMPCCXADD";
          "APX_F_ENQCMD";
          "APX_F_INVPCID";
          "APX_F_KOPB";
          "APX_F_KOPD";
          "APX_F_KOPQ";
          "APX_F_KOPW";
          "APX_F_LZCNT";
          "APX_F_LZCNT_N3";
          "APX_F_MOVBE";
          "APX_F_MOVDIR64B";
          "APX_F_MOVDIRI";
          "APX_F_MOVRS";
          "APX_F_MSR_IMM";
          "APX_F_N3";
          "APX_F_POPCNT";
          "APX_F_POPCNT_N3";
          "APX_F_RAO_INT";
          "APX_F_USER_MSR";
          "APX_F_VMX";
        ];
      capability =
        "Intel APX: REX2 and EVEX map-4 prefixes, r16-r31, new data destination (NDD), no-flags \
         (NF) and conditional forms.";
      evidence = "family-admission: every APX_* family is unhandled";
      task = "GEN-05-X86-APX";
      reopening_gate =
        "the encoder models REX2/extended GPRs and a GAS accepting APX is probed; the capture \
         already preserves the pattern";
    };
    {
      id = "RES-X86-AMX";
      source = "xed_resolved";
      families =
        [
          "AMX_AVX512";
          "AMX_BF16";
          "AMX_COMPLEX";
          "AMX_FP16";
          "AMX_FP8";
          "AMX_INT8";
          "AMX_MOVRS";
          "AMX_TILE";
          "AMX_TILE_BASE";
        ];
      capability =
        "Advanced Matrix Extensions: tile registers and the tile-configuration state, VEX-encoded \
         tile operations.";
      evidence = "family-admission: every AMX_* family unhandled";
      task = "GEN-05-X86-AMX";
      reopening_gate =
        "a tile register class exists in the normalized model and GAS -march=+amx-* is probed";
    };
    {
      id = "RES-X86-SYSTEM";
      source = "xed_resolved";
      families =
        [
          "AMD";
          "AMD_INVLPGB";
          "CET";
          "CMPCCXADD";
          "ENQCMD";
          "FRED";
          "FXSAVE";
          "FXSAVE64";
          "HRESET";
          "IBHF";
          "ICACHE_PREFETCH";
          "INVPCID";
          "KEYLOCKER";
          "KEYLOCKER_WIDE";
          "LKGS";
          "LWP";
          "MCOMMIT";
          "MONITOR";
          "MONITORX";
          "MOVDIR64B";
          "MOVDIRI";
          "MOVRS";
          "MPX";
          "MSRLIST";
          "MSR_IMM";
          "PBNDKB";
          "PCONFIG";
          "PKU";
          "PTWRITE";
          "RAO_INT";
          "RDPID";
          "RDPMC";
          "RDPRU";
          "RTM";
          "SERIALIZE";
          "SGX";
          "SGX_ENCLV";
          "SMAP";
          "SMX";
          "SNP";
          "SVM";
          "TDX";
          "TSX_LDTRK";
          "UINTR";
          "USER_MSR";
          "VIA_PADLOCK_AES";
          "VIA_PADLOCK_MONTMUL";
          "VIA_PADLOCK_RNG";
          "VIA_PADLOCK_SHA";
          "VMFUNC";
          "VTX";
          "WAITPKG";
          "WBNOINVD";
          "WRMSRNS";
          "XSAVE";
          "XSAVEC";
          "XSAVEOPT";
          "XSAVES";
        ];
      capability =
        "System, virtualization, security and vendor extensions: privileged and MSR forms, state \
         save/restore (XSAVE/FXSAVE), transactional memory, enclave/TDX/SGX, CET, virtualization \
         (VMX/SVM), key locker and VIA PadLock. Each needs a feature-name mapping, a GAS -march \
         spelling and an operand recipe, and most are not selectable by an ISA feature alone.";
      evidence = "family-admission: 58 families, none with a normalizer rule";
      task = "GEN-05-X86-SYSTEM";
      reopening_gate =
        "a per-extension feature mapping and installed-GAS probe is recorded, and privileged forms \
         are marked oracle-unavailable where GAS cannot assemble them";
    };
  ]

type cell = {
  source : string;
  family : string;
  target : Target.t;
  total : int;
  normalized_only : int;
  gas_generatable : int;
  promoted : int;
  oracle_unavailable : int;
  blocked : int;
}

type audit = {
  unowned : (string * string) list;
  ambiguous : (string * string * string list) list;
  empty_rows : string list;
  stale_names : (string * string) list;
}

let inputs =
  [
    ("riscv_opcodes", Target.Riscv32);
    ("riscv_opcodes", Target.Riscv64);
    ("xed_resolved", Target.X86_32);
    ("xed_resolved", Target.X86_64);
  ]

let blocked_count (t : Isa_family_admission.tally) =
  List.fold_left (fun acc (_, n) -> acc + n) 0 t.blocked

let cells repo =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | (source, target) :: rest -> (
        match Isa_family_admission.summarize repo ~source target with
        | Error e -> Error e
        | Ok summary ->
            let here =
              List.map
                (fun (f : Isa_family_admission.family) ->
                  {
                    source;
                    family = f.name;
                    target;
                    total = f.total;
                    normalized_only = f.tally.normalized_only;
                    gas_generatable = f.tally.gas_generatable;
                    promoted = f.tally.promoted_support;
                    oracle_unavailable = f.tally.oracle_unavailable;
                    blocked = blocked_count f.tally;
                  })
                summary.families
            in
            go (List.rev_append here acc) rest)
  in
  go [] inputs

module Key = struct
  type t = string * string

  let compare = compare
end

module Key_set = Set.Make (Key)

let blocked_families cells =
  List.fold_left
    (fun acc (c : cell) -> if c.blocked > 0 then Key_set.add (c.source, c.family) acc else acc)
    Key_set.empty cells

let owners rows (source, family) =
  List.filter
    (fun (r : row) -> String.equal r.source source && List.exists (String.equal family) r.families)
    rows

let audit rows cells =
  let blocked = blocked_families cells in
  let unowned, ambiguous =
    Key_set.fold
      (fun key (un, am) ->
        match owners rows key with
        | [] -> (key :: un, am)
        | [ _ ] -> (un, am)
        | many -> (un, (fst key, snd key, List.map (fun (r : row) -> r.id) many) :: am))
      blocked ([], [])
  in
  let empty_rows =
    List.filter_map
      (fun (r : row) ->
        if List.exists (fun f -> Key_set.mem (r.source, f) blocked) r.families then None
        else Some r.id)
      rows
  in
  let stale_names =
    List.concat_map
      (fun (r : row) ->
        List.filter_map
          (fun f -> if Key_set.mem (r.source, f) blocked then None else Some (r.id, f))
          r.families)
      rows
  in
  { unowned = List.rev unowned; ambiguous = List.rev ambiguous; empty_rows; stale_names }

let is_clean a = a.unowned = [] && a.ambiguous = [] && a.empty_rows = [] && a.stale_names = []

let problems a =
  List.map
    (fun (s, f) -> Printf.sprintf "%s family %s has blocked records and no ledger row owns it" s f)
    a.unowned
  @ List.map
      (fun (s, f, ids) ->
        Printf.sprintf "%s family %s is owned by more than one row: %s" s f (String.concat ", " ids))
      a.ambiguous
  @ List.map (Printf.sprintf "row %s owns no family with blocked records") a.empty_rows
  @ List.map
      (fun (id, f) -> Printf.sprintf "row %s names %s, which has no blocked records" id f)
      a.stale_names

let sum (f : cell -> int) l = List.fold_left (fun acc c -> acc + f c) 0 l

let report_lines rows cells =
  let row_lines (r : row) =
    let mine =
      List.filter
        (fun (c : cell) ->
          String.equal c.source r.source && List.exists (String.equal c.family) r.families)
        cells
    in
    let per_target =
      List.filter_map
        (fun (source, target) ->
          if not (String.equal source r.source) then None
          else
            let of_target = List.filter (fun c -> c.target = target) mine in
            Some
              (Printf.sprintf "%s=%d/%d" (Target.to_string target)
                 (sum (fun c -> c.blocked) of_target)
                 (sum (fun c -> c.total) of_target)))
        inputs
    in
    [
      Printf.sprintf "isa-residual: %s task=%s families=%d blocked/total: %s" r.id r.task
        (List.length r.families) (String.concat " " per_target);
      "  missing: " ^ r.capability;
      "  evidence: " ^ r.evidence;
      "  reopen when: " ^ r.reopening_gate;
    ]
  in
  let totals =
    List.map
      (fun (source, target) ->
        let of_target =
          List.filter (fun c -> String.equal c.source source && c.target = target) cells
        in
        let owned =
          sum
            (fun c ->
              if c.blocked > 0 && owners rows (c.source, c.family) <> [] then c.blocked else 0)
            of_target
        in
        Printf.sprintf
          "isa-residual-totals: %s/%s: records=%d promoted=%d gas-generatable=%d \
           normalized-only=%d oracle-unavailable=%d blocked=%d ledger-owned=%d"
          source (Target.to_string target)
          (sum (fun c -> c.total) of_target)
          (sum (fun c -> c.promoted) of_target)
          (sum (fun c -> c.gas_generatable) of_target)
          (sum (fun c -> c.normalized_only) of_target)
          (sum (fun c -> c.oracle_unavailable) of_target)
          (sum (fun c -> c.blocked) of_target)
          owned)
      inputs
  in
  List.concat_map row_lines rows @ totals

let run repo =
  match cells repo with
  | Error e -> Command.of_error e
  | Ok cells ->
      let a = audit rows cells in
      let report = List.map Diagnostic.stdout (report_lines rows cells) in
      if is_clean a then Command.ok report
      else
        Command.accumulate
          [
            Command.ok report;
            Command.of_error
              (Err.Error.make ~pos:__POS__ ~pp_error:Tool_error.pp
                 (Tool_error.v Tool_error.Validate
                    ("isa-residual-ledger: " ^ String.concat "; " (problems a))));
          ]
          ~f:Fun.id
