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
          "AVX512BW_128N";
          "AVX512F_128";
          "AVX512F_128N";
          "AVX512F_256";
          "AVX512F_512";
          "AVX512F_SCALAR";
          "AVX512_MOVZXC_128";
          "AVX512_SAT_CVT_512";
        ];
      capability =
        "EVEX forms GNU as never emits for their spelling (same-spelled twins no pseudo-prefix \
         separates) and XED's BCRC=1 register forms of conversions that take no rounding operand. \
         Broadcast ({1toN}) and registers 16-31 remain obligations on promoted records.";
      evidence =
        "family-admission: AVX-512/AVX10.2 records are promoted through DEC-X86-TABLE EVEX rows \
         with opmask ({%kN}, {z}), embedded rounding/{sae} and VSIB gathers/scatters; the \
         remainder is twins and four BCRC=1 conversions";
      task = "GEN-05-X86-EVEX";
      reopening_gate = "a pseudo-prefix or spelling that reaches the twin exists in GNU as";
    };
    {
      id = "RES-X86-VEX";
      source = "xed_resolved";
      families = [ "AVX"; "FMA4"; "XOP" ];
      capability =
        "VEX/XOP forms GNU as never emits for their spelling: same-spelled twins no pseudo-prefix \
         separates (the FMA4/XOP is4 register forms of the other VEX.W, vmovq's 6E/7E memory \
         forms, vpcmpistri's W1 form), which share an iform or an encoding choice with GNU's.";
      evidence =
        "family-admission: most AVX/AVX2/FMA/F16C/VAES/GFNI/VNNI-INT forms are promoted through \
         DEC-X86-TABLE rows, AVX-VNNI/IFMA/NE-CONVERT through {vex}, XOP/TBM/LWP through XOP rows; \
         AVX2 gathers through VSIB rows; the remainder is twins";
      task = "GEN-05-X86-VEX";
      reopening_gate = "a pseudo-prefix or spelling that reaches the twin exists in GNU as";
    };
    {
      id = "RES-X86-LEGACY-SIMD";
      source = "xed_resolved";
      families = [ "PENTIUMMMX"; "SSE"; "SSE2"; "SSE42"; "SSE4a" ];
      capability =
        "Legacy MMX/SSE remainders: GPR-with-memory spellings that need a width suffix, implicit \
         operands, and same-iform twins GNU never emits. xmm, mm and 3DNow! forms are promoted \
         through DEC-X86-TABLE rows.";
      evidence =
        "family-admission: SSE, SSE2, PENTIUMMMX and 3DNOW are promoted but for a handful of \
         records";
      task = "GEN-05-X86-SIMD";
      reopening_gate = "a GAS spelling for the remaining records is probed";
    };
    {
      id = "RES-X86-X87";
      source = "xed_resolved";
      families = [ "X87" ];
      capability =
        "x87 forms the generated table does not take: the 16-bit (14- and 94-byte) environment \
         images and same-iform stack-register twins GNU never emits.";
      evidence =
        "family-admission: X87/FCMOV/FCOMI are promoted through DEC-X86-TABLE rows (st(i) in \
         ModR/M.rm, real/int memory suffixes, GNU's fsub/fdiv AT&T swap); a handful remain";
      task = "GEN-05-X86-X87";
      reopening_gate = "a GAS spelling for the remaining forms is probed";
    };
    {
      id = "RES-X86-BASE-INT";
      source = "xed_resolved";
      families =
        [
          "FAT_NOP";
          "I186";
          "I286PROTECTED";
          "I386";
          "I486REAL";
          "I86";
          "LONGMODE";
          "PPRO";
          "PPRO_UD0_LONG";
          "PPRO_UD0_SHORT";
          "PREFETCH_NOP";
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
          "APX_F_AMX";
          "APX_F_AMX_MOVRS";
          "APX_F_BMI1";
          "APX_F_BMI1_N3";
          "APX_F_ENQCMD";
          "APX_F_LZCNT";
          "APX_F_LZCNT_N3";
          "APX_F_MOVBE";
          "APX_F_MOVDIR64B";
          "APX_F_MOVRS";
          "APX_F_N3";
          "APX_F_POPCNT";
          "APX_F_POPCNT_N3";
        ];
      capability =
        "Intel APX beyond the EVEX map-4 promotions, CCMP/CTEST and r16-r31: the REX2-only forms \
         (push/pop with PPX, jmpabs), and promotions GNU spells differently from XED \
         (setzu/imulzu, CFCMOV's store form); same-iform direction twins GNU never emits.";
      evidence =
        "family-admission: the APX_F map-4 promotions of legacy instructions (plain {evex}, NDD, \
         {nf}, $1 shifts) and CCMP/CTEST with {dfv=} are promoted; REX2 and the rest remain";
      task = "GEN-05-X86-APX";
      reopening_gate = "the table models REX2-only encodings (PPX hints, jmpabs)";
    };
    {
      id = "RES-X86-AMX";
      source = "xed_resolved";
      families = [ "AMX_MOVRS"; "AMX_TILE" ];
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
          "ENQCMD";
          "IBHF";
          "ICACHE_PREFETCH";
          "MONITOR";
          "MONITORX";
          "MOVDIR64B";
          "MOVRS";
          "RTM";
          "SNP";
          "SVM";
          "VIA_PADLOCK_MONTMUL";
          "WAITPKG";
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
