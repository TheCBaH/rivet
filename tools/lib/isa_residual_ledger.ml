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
      id = "RES-X86-EVEX";
      source = "xed_resolved";
      families =
        [
          "AVX512BW_128";
          "AVX512BW_128N";
          "AVX512BW_256";
          "AVX512BW_512";
          "AVX512DQ_128";
          "AVX512DQ_128N";
          "AVX512DQ_256";
          "AVX512DQ_512";
          "AVX512DQ_SCALAR";
          "AVX512ER_512";
          "AVX512ER_SCALAR";
          "AVX512F_128";
          "AVX512F_128N";
          "AVX512F_256";
          "AVX512F_512";
          "AVX512F_SCALAR";
          "AVX512PF_512";
          "AVX512_COM_EF_SCALAR";
          "AVX512_FP16_512";
          "AVX512_FP16_CONVERT_512";
          "AVX512_FP16_SCALAR";
          "AVX512_GFNI_128";
          "AVX512_GFNI_256";
          "AVX512_MEDIAX_128";
          "AVX512_MEDIAX_256";
          "AVX512_MINMAX_512";
          "AVX512_MINMAX_SCALAR";
          "AVX512_MOVZXC_128";
          "AVX512_SAT_CVT_512";
          "AVX512_SAT_CVT_DS_512";
          "AVX512_SAT_CVT_DS_SCALAR";
          "AVX512_VAES_128";
          "AVX512_VAES_256";
          "AVX512_VNNI_INT16_128";
          "AVX512_VNNI_INT16_256";
          "AVX512_VNNI_INT8_128";
          "AVX512_VNNI_INT8_256";
          "AVX512_VPCLMULQDQ_128";
          "AVX512_VPCLMULQDQ_256";
        ];
      capability =
        "EVEX forms beyond the generated table's base obligation (unmasked, no zeroing, no \
         broadcast, registers 0-15, disp8*N): rounding/SAE (BCRC) records, VSIB gathers and \
         scatters, forms that require a mask (MASKNOT0), GPR-with-memory spellings, and the EVEX \
         xmm/ymm twins of VEX forms (reachable only with {evex}). Masking, zeroing, broadcast and \
         registers 16-31 are obligations on already-promoted records.";
      evidence =
        "family-admission: most AVX-512F/BW/DQ/CD/FP16/BF16/VBMI/VNNI/IFMA/BITALG/VPOPCNTDQ and \
         AVX10.2 records are promoted through DEC-X86-TABLE EVEX rows; the remainder is rounding, \
         VSIB, mask-register and pseudo-prefix-only forms";
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
          "AVX2GATHER";
          "AVX_IFMA";
          "AVX_NE_CONVERT";
          "AVX_VNNI";
          "FMA4";
          "SM4_128";
          "SM4_256";
          "XOP";
        ];
      capability =
        "VEX/XOP forms the generated x86 table does not yet cover: VSIB gathers, the XOP encoding \
         space, VZEROUPPER/ALL (no ModR/M), GPR-with-memory spellings that need a width suffix, \
         and VEX forms GNU as reaches only through a {vex} pseudo-prefix (AVX-VNNI, AVX-IFMA, \
         AVX-NE-CONVERT, whose plain spelling is EVEX) or a {load}/{store} one (same-spelled twins \
         such as FMA4's W0 register form).";
      evidence =
        "family-admission: most AVX/AVX2/FMA/F16C/VAES/GFNI/VNNI-INT forms are promoted through \
         DEC-X86-TABLE rows; the remainder is gathers, XOP, pseudo-prefix-only encodings and a few \
         unparsed pattern shapes";
      task = "GEN-05-X86-VEX";
      reopening_gate =
        "VSIB addressing and XOP rows exist, and the text parser accepts {vex}/{load}/{store} \
         pseudo-prefixes";
    };
    {
      id = "RES-X86-LEGACY-SIMD";
      source = "xed_resolved";
      families =
        [ "3DNOW"; "ACE_1"; "PENTIUMMMX"; "SSE"; "SSE2"; "SSE4"; "SSE42"; "SSE4a"; "SSE_PREFETCH" ];
      capability =
        "Legacy MMX/SSE/3DNow remainders: 3DNow's suffix-opcode encoding (0F 0F ... op), \
         GPR-with-memory spellings that need a width suffix, implicit operands, and store-form \
         twins reachable only with {store}. xmm and mm SSE/SSE2/SSSE3/SSE4/AES/PCLMUL/SHA/GFNI \
         forms are promoted through DEC-X86-TABLE rows.";
      evidence =
        "family-admission: SSE, SSE2 and PENTIUMMMX are promoted but for a handful of records; \
         3DNOW is unhandled";
      task = "GEN-05-X86-SIMD";
      reopening_gate =
        "the table encodes 3DNow's trailing opcode byte, and the text parser accepts \
         {load}/{store} pseudo-prefixes";
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
          "CLDEMOTE";
          "CLFLUSHOPT";
          "CLFSH";
          "CLWB";
          "CLZERO";
          "CMPXCHG16B";
          "FAT_NOP";
          "I186";
          "I286PROTECTED";
          "I286REAL";
          "I386";
          "I486REAL";
          "I86";
          "LONGMODE";
          "PAUSE";
          "PENTIUMREAL";
          "PPRO";
          "PPRO_UD0_LONG";
          "PPRO_UD0_SHORT";
          "PREFETCHWT1";
          "PREFETCH_NOP";
          "RDTSCP";
          "RDWRFSGS";
          "TBM";
        ];
      capability =
        "Integer, string, stack, flag, bit-manipulation and data-movement instructions outside the \
         generated rows: DF64/FORCE64 stack forms, implicit-operand and string forms, LOCK/REP, \
         segment registers, mixed-width moves, atomics (CMPXCHG16B), and cache/prefetch hints.";
      evidence =
        "family-admission: CMOVcc, SETcc, BMI1/BMI2/LZCNT and single-width I86/I386 forms are \
         promoted through DEC-X86-TABLE rows; the rest of I86/I386/I186 is stack, string and \
         implicit-operand forms";
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
          "AMD_INVLPGB";
          "CET";
          "ENQCMD";
          "FRED";
          "FXSAVE";
          "FXSAVE64";
          "HRESET";
          "IBHF";
          "ICACHE_PREFETCH";
          "INVPCID";
          "KEYLOCKER_WIDE";
          "LKGS";
          "LWP";
          "MCOMMIT";
          "MONITOR";
          "MONITORX";
          "MOVDIR64B";
          "MOVRS";
          "MPX";
          "MSRLIST";
          "MSR_IMM";
          "PBNDKB";
          "PCONFIG";
          "PKU";
          "PTWRITE";
          "RDPID";
          "RDPRU";
          "RTM";
          "SERIALIZE";
          "SGX";
          "SGX_ENCLV";
          "SMAP";
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
