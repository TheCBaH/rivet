(* Tests that need files outside the dune workspace. Run by Make, with the
   repository root as argv.

   The one documented exception to "production code never invokes a shell": this
   test runs a fixed repository script in order to compare Target against the
   shell matrix. Production code does not do this.

   Phase 7 moved ownership, so what this proves changed and the test stayed.
   tools/target-matrix.sh is now GENERATED from Target by Target_emit, which
   makes the comparison a round trip - OCaml to shell text, sourced by bash,
   back to a comparison. That is not a tautology and it is not weaker in the way
   that matters: it is exactly what catches a quoting bug, a lost array element,
   or a default the renderer forgot to emit, none of which any other check would
   see. What it no longer proves is that OCaml agrees with an INDEPENDENTLY
   written shell - and it cannot, because nobody writes that shell any more.
   `make tools-matrix-diff` is what holds the committed file to the source. *)

open Compcert_tools

let failures = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n" name;
    incr failures)

let check_eq name ~expected ~actual =
  if String.equal expected actual then Printf.printf "  ok   %s\n" name
  else (
    Printf.printf "  FAIL %s\n    shell:  %S\n    ocaml:  %S\n" name expected actual;
    incr failures)

let run_capture root args =
  match
    Tool_process.exec
      (Tool_process.spec ~stdout:Tool_process.Out_capture ~stderr:Tool_process.Err_inherit
         ~parsed_output:true ~accepted:Process_status.Zero_only ~label:"shell"
         (Fpath.to_string root) args)
  with
  | Ok { Tool_process.stdout = Some s; _ } -> Some s
  | _ -> None

let lines s = List.filter (fun l -> l <> "") (String.split_on_char '\n' s)

let tsv s =
  List.filter_map
    (fun line ->
      match String.index_opt line '\t' with
      | Some i -> Some (String.sub line 0 i, String.sub line (i + 1) (String.length line - i - 1))
      | None -> Some (line, ""))
    (lines s)

(* {1 All seventeen shell values, verbatim}

   Including HAS_SYSROOT, which the OCaml side DERIVES. Comparing only the
   derived form would let a stale HAS_SYSROOT assignment in the shell hide
   behind an equivalent computed value - and the shell is what the other seven
   scripts still read. *)
let ocaml_view t =
  let c = Target.config t in
  let opt = Option.value ~default:"" in
  [
    ("CONFIGURE_TARGET", c.Target.configure_target);
    ("TOOLPREFIX", c.Target.toolprefix);
    ("QEMU_BIN", c.Target.qemu_bin);
    ("QEMU_SYSROOT", opt c.Target.qemu_sysroot);
    ("CCOMP_EXTRA_ARGS", String.concat " " c.Target.ccomp_args);
    ("COMPCERT_CONFIGURE_ARGS", String.concat " " c.Target.compcert_configure_args);
    ("AS_FLAGS", String.concat " " c.Target.as_args);
    ("LD_FLAGS", String.concat " " c.Target.ld_args);
    ("LINKER_EMULATION", opt c.Target.linker_emulation);
    ("ELF_CLASS", c.Target.elf_class);
    ("WORD_SIZE", string_of_int c.Target.word_size);
    ("HAS_SYSROOT", if Target.has_sysroot t then "true" else "false");
    ("READELF_MACHINE", c.Target.readelf_machine);
    ("LINK_TEXT_ADDR", Target.hex c.Target.link.Target.text);
    ("LINK_RODATA_ADDR", Target.hex c.Target.link.Target.rodata);
    ("LINK_DATA_ADDR", Target.hex c.Target.link.Target.data);
    ("LINK_BSS_ADDR", Target.hex c.Target.link.Target.bss);
  ]

let test_target_db_agrees root =
  let dump = Fpath.(root / "tools" / "dev" / "dump-target-config.sh") in
  List.iter
    (fun t ->
      let name = Target.to_string t in
      match run_capture dump [ name ] with
      | None -> check (Printf.sprintf "target_db: %s: dump script ran" name) false
      | Some out ->
          let shell = tsv out in
          let ours = ocaml_view t in
          check
            (Printf.sprintf "target_db: %s: all seventeen values present" name)
            (List.length shell = 17 && List.length ours = 17);
          List.iter
            (fun (k, v) ->
              match List.assoc_opt k shell with
              | Some sv ->
                  check_eq (Printf.sprintf "target_db: %s: %s" name k) ~expected:sv ~actual:v
              | None -> check (Printf.sprintf "target_db: %s: %s present in shell" name k) false)
            ours)
    Target.all

(* {2 The three target SETS, against the enumerator} *)

let test_target_sets root =
  let matrix = Fpath.(root / "tools" / "target-matrix.sh") in
  List.iter
    (fun (arg, cap) ->
      match run_capture matrix [ arg ] with
      | None -> check (Printf.sprintf "target sets: %s ran" arg) false
      | Some out ->
          check_eq
            (Printf.sprintf "target sets: %s" arg)
            ~expected:(String.concat " " (lines out))
            ~actual:(String.concat " " (List.map Target.to_string (Target.set cap))))
    [ ("fixture", Target.Fixture); ("assembler", Target.Assembler); ("libc", Target.Libc_smoke) ]

(* {3 The derived invariants, stated against the shell rather than assumed} *)

let test_derived_invariants root =
  let dump = Fpath.(root / "tools" / "dev" / "dump-target-config.sh") in
  List.iter
    (fun t ->
      let name = Target.to_string t in
      match run_capture dump [ name ] with
      | None -> check (Printf.sprintf "derived: %s" name) false
      | Some out ->
          let shell = tsv out in
          let get k = Option.value ~default:"" (List.assoc_opt k shell) in
          (* qemu_sysroot = None IFF not HAS_SYSROOT - the equivalence that
             lets the OCaml side omit the flag entirely. *)
          check
            (Printf.sprintf "derived: %s: HAS_SYSROOT <-> QEMU_SYSROOT non-empty" name)
            (get "HAS_SYSROOT" = "true" = (get "QEMU_SYSROOT" <> ""));
          (* And the link family really is base, +0x10000, +0x20000, +0x80000
             (M3: bss, the first byte after the largest stack abi_v2.ml's
             stack_size_max permits). *)
          let addr k = int_of_string (get k) in
          check
            (Printf.sprintf "derived: %s: link addresses are one arithmetic family" name)
            (addr "LINK_RODATA_ADDR" = addr "LINK_TEXT_ADDR" + 0x10000
            && addr "LINK_DATA_ADDR" = addr "LINK_TEXT_ADDR" + 0x20000
            && addr "LINK_BSS_ADDR" = addr "LINK_TEXT_ADDR" + 0x80000))
    Target.all

(* Isa_db_cross_validate reads isa-db/export/*.jsonl, which lives outside
   asm/ (the standalone isa-db/ Python project) - so like the rest of this
   executable, it needs the real repository root rather than
   %{workspace_root}, and belongs here rather than in a runtest rule. *)
let test_isa_db_cross_validate repo =
  match Isa_db_cross_validate.check repo with
  | { Command.exit = `Success; events } ->
      List.iter
        (function
          | Command.Output d -> Format.printf "  ok   %a@." Diagnostic.pp d
          | Command.Fatal e -> check (Format.asprintf "%a" (Err.Error.pp Tool_error.pp) e) false)
        events
  | { Command.events; _ } ->
      List.iter
        (function
          | Command.Output d -> Format.eprintf "%a@." Diagnostic.pp d
          | Command.Fatal e -> Format.eprintf "%a@." (Err.Error.pp Tool_error.pp) e)
        events;
      check "isa-db cross-validate" false

(* Complete record accounting: pin the exact total/normalized
   counts against the real checked-in exports, so a decode/dispatch
   regression that silently stops matching a mnemonic/iform allowlist entry
   (dropping the normalized count) or a corpus update (changing the total)
   is caught here, not just "the command still exits Success" (every record
   is Ok or Error by construction, so that alone proves nothing). *)
let test_isa_norm_accounting repo =
  let expect ~source target ~total ~normalized =
    let label = Printf.sprintf "%s/%s" source (Target.to_string target) in
    match Isa_norm_accounting.summarize repo ~source target with
    | Ok (s : Isa_norm_accounting.summary) ->
        check
          (Printf.sprintf "isa-norm-accounting: %s: %d records (expected %d)" label s.total total)
          (s.total = total);
        check
          (Printf.sprintf "isa-norm-accounting: %s: %d normalized (expected %d)" label s.normalized
             normalized)
          (s.normalized = normalized)
    | Error e -> check (Format.asprintf "%a" (Err.Error.pp Tool_error.pp) e) false
  in
  expect ~source:"riscv_opcodes" Target.Riscv32 ~total:1089 ~normalized:726;
  expect ~source:"riscv_opcodes" Target.Riscv64 ~total:1154 ~normalized:779;
  expect ~source:"xed_resolved" Target.X86_32 ~total:7887 ~normalized:1033;
  expect ~source:"xed_resolved" Target.X86_64 ~total:10571 ~normalized:1035

(* The family matrix is a second view over the same complete population,
   not a hand-maintained support claim. Pinning its aggregate states makes a
   source update or an accidental widening of support credit a reviewed
   change, while the per-family invariant makes a dropped native family fail
   even if an aggregate happens to stay plausible. *)
(* The ledger against the real matrix: every blocked family is owned exactly once, no row is
   stale, and the records the ledger owns are exactly the blocked records - so the promoted,
   normalized-only and blocked counts partition each profile's full source denominator. *)
let test_isa_residual_ledger repo =
  match Isa_residual_ledger.cells repo with
  | Error e -> check (Format.asprintf "%a" (Err.Error.pp Tool_error.pp) e) false
  | Ok cells ->
      let audit = Isa_residual_ledger.audit Isa_residual_ledger.rows cells in
      List.iter
        (fun p -> check ("isa-residual-ledger: " ^ p) false)
        (Isa_residual_ledger.problems audit);
      check "isa-residual-ledger: ledger is clean" (Isa_residual_ledger.is_clean audit);
      List.iter
        (fun (source, target) ->
          let mine =
            List.filter
              (fun (c : Isa_residual_ledger.cell) ->
                String.equal c.source source && c.target = target)
              cells
          in
          let sum f = List.fold_left (fun acc c -> acc + f c) 0 mine in
          let total = sum (fun (c : Isa_residual_ledger.cell) -> c.total) in
          check
            (Printf.sprintf "isa-residual-ledger: %s/%s every record is in exactly one state" source
               (Target.to_string target))
            (sum (fun (c : Isa_residual_ledger.cell) -> c.promoted)
             + sum (fun c -> c.gas_generatable)
             + sum (fun c -> c.normalized_only)
             + sum (fun c -> c.oracle_unavailable)
             + sum (fun c -> c.blocked)
            = total))
        Isa_residual_ledger.inputs

let test_isa_family_admission repo =
  let expect ~source target ~total ~normalized_only ~gas_generatable ~promoted_support ~blocked =
    let label = Printf.sprintf "%s/%s" source (Target.to_string target) in
    match Isa_family_admission.summarize repo ~source target with
    | Error e -> check (Format.asprintf "%a" (Err.Error.pp Tool_error.pp) e) false
    | Ok (summary : Isa_family_admission.summary) ->
        let sums =
          List.fold_left
            (fun (n, g, p, u, b) (family : Isa_family_admission.family) ->
              let t = family.tally in
              check
                (Printf.sprintf "isa-family-admission: %s/%s is total" label family.name)
                (family.total = Isa_family_admission.tally_total t);
              ( n + t.normalized_only,
                g + t.gas_generatable,
                p + t.promoted_support,
                u + t.oracle_unavailable,
                b + List.fold_left (fun count (_, n) -> count + n) 0 t.blocked ))
            (0, 0, 0, 0, 0) summary.families
        in
        let n, g, p, u, b = sums in
        check (Printf.sprintf "isa-family-admission: %s total" label) (summary.total = total);
        check (Printf.sprintf "isa-family-admission: %s has families" label) (summary.families <> []);
        check (Printf.sprintf "isa-family-admission: %s normalized-only" label) (n = normalized_only);
        check (Printf.sprintf "isa-family-admission: %s gas-generatable" label) (g = gas_generatable);
        check
          (Printf.sprintf "isa-family-admission: %s promoted-support" label)
          (p = promoted_support);
        check (Printf.sprintf "isa-family-admission: %s oracle-unavailable" label) (u = 0);
        check (Printf.sprintf "isa-family-admission: %s blockers" label) (b = blocked)
  in
  (* Promotes the four bare scalar single-precision arithmetic forms
     after persisted RV{32,64}IMF(D) cases pin their implicit dynamic rounding,
     sh2add/sh3add after persisted RV{32,64}IM_Zba cases pin them alongside
     sh1add (Zba's non-word scale family), min/minu/max/maxu after
     persisted RV{32,64}IM_Zbb cases pin Zbb's single-extension comparison
     family, sh1add.uw/sh2add.uw/sh3add.uw (RV64-only) after a persisted
     RV64IM_Zba case pins each, closing Zba's *.uw word-operand family, and
     andn/orn/xnor/rol/ror after a persisted RV{32,64}IM_Zbb case pins each
     under its primary rv_zbb record - each mnemonic's Req_any also promotes
     the four import-duplicate records (rv_zbkb/rv_zk/rv_zkn/rv_zks) that
     independently normalize to the same form_id, so this slice moves 25
     records per profile (5 mnemonics x 5 extension-membership records), not 5.
     The Zbkb pack/packh/packw/zip/unzip slice, its rolw/rorw
     continuation, and rori/rori.rv32/roriw (rori's own profile-specific
     native_name split reusing rev8's Req_any groups, roriw the plain
     RV64-only *w sibling) add further promoted records on top of that.
     zext.h/zext.h.rv32 add one more promoted record per profile - unlike
     rev8/rori, neither is import-duplicated, so this adds only 1 record
     per profile, not 5. clmul/clmulh are import-duplicated five ways again
     (rv_zbc/rv_zbkc/rv_zk/rv_zkn/rv_zks), identical on both profiles, so
     this slice moves 10 records per profile (2 mnemonics x 5 records).
     xperm4/xperm8 are import-duplicated four ways (rv_zbkx/rv_zk/rv_zkn/
     rv_zks - no separate non-K sibling extension), identical on both
     profiles, so this slice moves 8 records per profile (2 mnemonics x
     4 records). sha256sum0/sha256sum1/sha256sig0/sha256sig1 are
     import-duplicated three ways (rv_zknh/rv_zk/rv_zkn - no rv_zks),
     identical on both profiles, so this slice moves 12 records per profile
     (4 mnemonics x 3 records). sha512sum0/sha512sum1/sha512sig0/sha512sig1
     are sha256's RV64-only siblings, also import-duplicated three ways
     (rv64_zknh/rv64_zk/rv64_zkn), so this slice moves 12 records on RV64
     only (4 mnemonics x 3 records), 0 on RV32 (no RV32 record at all).
     sha512sum0r/sha512sum1r/sha512sig0l/sha512sig1l/sha512sig0h/sha512sig1h
     are SHA-512's own RV32-only 32-bit-split siblings, also
     import-duplicated three ways (rv32_zknh/rv32_zk/rv32_zkn), so this
     slice moves 18 records on RV32 only (6 mnemonics x 3 records), 0 on
     RV64 (no RV64 record at all). aes64ds/aes64dsm/aes64im are
     import-duplicated three ways (rv64_zknd/rv64_zk/rv64_zkn),
     aes64es/aes64esm three ways (rv64_zkne/rv64_zk/rv64_zkn, a disjoint
     primary), and aes64ks2 four ways (rv64_zknd/rv64_zk/rv64_zkn/rv64_zkne
     - the one mnemonic both key-schedule extensions import), so this
     slice moves 19 records on RV64 only (5 mnemonics x 3 + 1 mnemonic x 4),
     0 on RV32 (no RV32 record at all). aes64ks1i shares aes64ks2's own
     four-way group (rv64_zknd/rv64_zk/rv64_zkn/rv64_zkne), so this slice
     moves 4 more records on RV64 only. aes32dsi/aes32dsmi/aes32esi/
     aes32esmi are each import-duplicated three ways (rv32_zknd or
     rv32_zkne, rv32_zk, rv32_zkn), so this slice moves 12 records on RV32
     only (4 mnemonics x 3 records). csrrw/csrrs/csrrc/csrrwi/csrrsi/csrrci
     are the first family here outside Zb/Zk - each a single, non-import-
     duplicated rv_zicsr record, XLEN-independent, so this slice moves 6
     records on EACH profile (6 mnemonics x 1 record, not x2/x3 the way
     every import-duplicated slice above did). fsgnj.s/fsgnjn.s/fsgnjx.s/
     fsgnj.d/fsgnjn.d/fsgnjx.d are likewise each a single, non-import-
     duplicated rv_f/rv_d record (rv_zfh's .h and rv_q's .q siblings are
     separate native_names, not import duplicates of these), so this slice
     also moves 6 records on EACH profile. fmin.s/fmax.s/fmin.d/fmax.d are
     likewise each a single, non-import-duplicated rv_f/rv_d record, so
     this slice moves 4 records on EACH profile. fsqrt.s/fsqrt.d/fclass.s/
     fclass.d are likewise each a single, non-import-duplicated rv_f/rv_d
     record, so this slice moves 4 records on EACH profile. fmadd.s/fmsub.s/
     fnmsub.s/fnmadd.s/fmadd.d/fmsub.d/fnmsub.d/fnmadd.d are likewise each a
     single, non-import-duplicated rv_f/rv_d record, so this slice moves 8
     records on EACH profile. feq.s/fle.s/flt.s/feq.d/fle.d/flt.d are
     likewise each a single, non-import-duplicated rv_f/rv_d record, so this
     slice moves 6 records on EACH profile. fmv.x.w/fmv.w.x are likewise
     each a single, non-import-duplicated rv_f record (XLEN-independent, no
     rv_d sibling - the "w"/"s" name refers to single precision, not RV32),
     so this slice moves 2 records on EACH profile. fcvt.w.s/fcvt.wu.s/
     fcvt.s.w/fcvt.s.wu are likewise each a single, non-import-duplicated
     rv_f record (XLEN-independent, no rv_d sibling), so this slice moves 4
     records on EACH profile - straight from blocked (they never normalized
     at all before this commit) to promoted-support, leaving
     normalized_only unchanged. fcvt.w.d/fcvt.wu.d/fcvt.d.w/fcvt.d.wu/
     fcvt.s.d/fcvt.d.s are likewise each a single, non-import-duplicated
     rv_d record (XLEN-independent - the D extension has no RV32/RV64
     split), so this slice moves 6 records on EACH profile, the same way
     straight from blocked to promoted-support (these six already had
     encoder support from an earlier pass, but had never been wired into
     normalization/admission at all before this commit). fcvt.l.d/fcvt.lu.d/
     fcvt.d.l/fcvt.d.lu/fcvt.l.s/fcvt.lu.s/fcvt.s.l/fcvt.s.lu are RV64-only
     (rv64_d/rv64_f, no RV32 counterpart at all - riscv32.jsonl does not
     even contain these 8 records), so this slice moves 8 records on RV64
     ONLY, straight from blocked to promoted-support; RV32's own counts are
     unaffected. vsetvl (V's register-register configuration-setting
     instruction, the entry point into the 375-record rv_v family) is a
     single, non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 1 record on EACH profile, straight from
     blocked to promoted-support. vsetvli/vsetivli (V's immediate-vtype
     siblings) are likewise each a single, non-import-duplicated rv_v record
     present identically on both profiles, so that slice moves 2 records on
     EACH profile. vadd.vv/vadd.vx/vadd.vi (the entry point into OP-V's real
     vector-register arithmetic space) are likewise each a single,
     non-import-duplicated rv_v record present identically on both profiles,
     so this slice moves 3 records on EACH profile, straight from blocked to
     promoted-support. vsub.vv/vsub.vx, vrsub.vx/vrsub.vi, and
     vand/vor/vxor's full .vv/.vx/.vi triples (13 mnemonics total - vsub has
     no .vi sibling and vrsub has no .vv sibling, matching riscv-opcodes'
     own export and real GNU as's "unrecognized opcode" rejection of both)
     are likewise each a single, non-import-duplicated rv_v record present
     identically on both profiles, so this slice moves 13 records on EACH
     profile, straight from blocked to promoted-support. vsll/vsrl/vsra's
     full .vv/.vx/.vi triples, vminu/vmin/vmaxu/vmax's .vv/.vx pairs (no .vi
     sibling), and vmul/vmulh/vmulhu/vmulhsu's OPMVV/OPMVX .vv/.vx pairs (25
     mnemonics total) are likewise each a single, non-import-duplicated rv_v
     record present identically on both profiles, so this slice moves 25
     records on EACH profile, straight from blocked to promoted-support.
     vdivu/vdiv/vremu/vrem's OPMVV/OPMVX .vv/.vx pairs (8 mnemonics total, no
     .vi sibling) are likewise each a single, non-import-duplicated rv_v
     record present identically on both profiles, so this slice moves 8
     records on EACH profile, straight from blocked to promoted-support.
     vsaddu/vsadd/vssubu/vssub's OPIVV/OPIVX/OPIVI .vv/.vx(/.vi) forms (10
     mnemonics total - vssubu/vssub have no .vi sibling) are likewise each a
     single, non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 10 records on EACH profile, straight from
     blocked to promoted-support. vaadd/vaaddu/vasub/vasubu's OPMVV/OPMVX
     .vv/.vx pairs (8 mnemonics total, no .vi sibling) are likewise each a
     single, non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 8 records on EACH profile, straight from
     blocked to promoted-support. vnsrl/vnsra/vnclipu/vnclip's OPIVV/OPIVX/
     OPIVI .wv/.wx/.wi forms (12 mnemonics total, the narrowing shift/clip
     family - the [.w*] suffix denotes a semantically wide vs2, but the
     assembler only encodes register/immediate field positions, identical
     to the plain OPIVV/OPIVX/OPIVI shape) are likewise each a single,
     non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 12 records on EACH profile, straight
     from blocked to promoted-support. vssrl/vssra's full OPIVV/OPIVX/OPIVI
     .vv/.vx/.vi forms (6 mnemonics total, the scaling shift-right family -
     the same full triple shape as vsll/vsrl/vsra including the UNSIGNED
     [.vi]) are likewise each a single, non-import-duplicated rv_v record
     present identically on both profiles, so this slice moves 6 records on
     EACH profile, straight from blocked to promoted-support. vrgather's
     full OPIVV/OPIVX/OPIVI .vv/.vx/.vi forms plus vrgatherei16.vv (4
     mnemonics total, the gather/permute family, the same shape as
     vadd/etc.) are likewise each a single, non-import-duplicated rv_v
     record present identically on both profiles, so this slice moves 4
     records on EACH profile, straight from blocked to promoted-support.
     vwaddu/vwadd/vwsubu/vwsub's OPMVV/OPMVX .vv/.vx/.wv/.wx forms (16
     mnemonics total, the widening add/subtract family, no .vi sibling)
     are likewise each a single, non-import-duplicated rv_v record present
     identically on both profiles, so this slice moves 16 records on EACH
     profile, straight from blocked to promoted-support. vwmulu/vwmulsu/
     vwmul's OPMVV/OPMVX .vv/.vx forms (6 mnemonics total, the widening
     multiply family, no .vi sibling) are likewise each a single,
     non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 6 records on EACH profile, straight
     from blocked to promoted-support. vsext/vzext's `.vf2`/`.vf4`/`.vf8`
     forms (6 mnemonics total, a genuinely new two-vector-register shape
     with no third operand) are likewise each a single, non-import-
     duplicated rv_v record present identically on both profiles, so this
     slice moves 6 records on EACH profile, straight from blocked to
     promoted-support. vmand/vmandn/vmor/vmxor/vmorn/vmnand/vmnor/vmxnor
     (8 mnemonics total, the mask-register logical family - the same
     [rd, rs2, rs1] shape as vadd/etc. but with [vm] architecturally fixed
     at 1, so no masked sibling exists) are likewise each a single,
     non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 8 records on EACH profile, straight
     from blocked to promoted-support. vredsum/vredand/vredor/vredxor/
     vredminu/vredmin/vredmaxu/vredmax.vs (8 mnemonics, the plain
     vector-reduction family, the same [rd, rs2, rs1] shape as vadd/etc.
     but with a real, selectable mask) plus vwredsumu/vwredsum.vs (2
     mnemonics, the widening-sum reduction pair, sharing OPIVV's funct3
     space rather than OPMVV's) are likewise each a single,
     non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 10 records on EACH profile, straight
     from blocked to promoted-support. vmseq/vmsne/vmsltu/vmslt/vmsleu/
     vmsle/vmsgtu/vmsgt (20 mnemonics, the mask-writing comparison family,
     full OPIVV/OPIVX/OPIVI shape minus [.vv] for vmsgtu/vmsgt and [.vi]
     for vmsltu/vmslt) are likewise each a single, non-import-duplicated
     rv_v record present identically on both profiles, so this slice moves
     20 records on EACH profile, straight from blocked to promoted-support.
     vslideup/vslidedown (OPIVX/OPIVI, no [.vv] sibling) plus
     vslide1up/vslide1down (OPMVX, no [.vi] sibling) - 6 mnemonics total -
     are likewise each a single, non-import-duplicated rv_v record present
     identically on both profiles, so this slice moves 6 records on EACH
     profile, straight from blocked to promoted-support. vfadd.vv/vfadd.vf
     (the entry point into OP-V's floating-point arithmetic space, OPFVV/
     OPFVF) are likewise each a single, non-import-duplicated rv_v record
     present identically on both profiles, so this slice moves 2 records on
     EACH profile, straight from blocked to promoted-support. vfsub.vv/
     vfsub.vf/vfrsub.vf (3 mnemonics, the subtract/reverse-subtract pair -
     no [vfrsub.vv] sibling exists) are likewise each a single,
     non-import-duplicated rv_v record present identically on both profiles,
     so this slice moves 3 records on EACH profile, straight from blocked to
     promoted-support. vfmul.vv/vfmul.vf/vfdiv.vv/vfdiv.vf/vfrdiv.vf (5
     mnemonics, the multiply/divide/reverse-divide triple - still the
     identical OPFVV/OPFVF shape, unlike integer multiply/divide's own
     OPMVV/OPMVX space; vfrdiv has no [.vv] sibling) are likewise each a
     single, non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 5 records on EACH profile, straight from
     blocked to promoted-support. vfmin.vv/vfmin.vf/vfmax.vv/vfmax.vf (4
     mnemonics, the min/max pair, full [.vv]/[.vf] shapes with no [.vi]
     sibling for either) are likewise each a single, non-import-duplicated
     rv_v record present identically on both profiles, so this slice moves
     4 records on EACH profile, straight from blocked to promoted-support.
     vfsgnj.vv/vfsgnj.vf/vfsgnjn.vv/vfsgnjn.vf/vfsgnjx.vv/vfsgnjx.vf (6
     mnemonics, the sign-injection triple, full [.vv]/[.vf] shapes with no
     [.vi] sibling for any) are likewise each a single, non-import-duplicated
     rv_v record present identically on both profiles, so this slice moves
     6 records on EACH profile, straight from blocked to promoted-support.
     vfsqrt.v/vfrsqrt7.v/vfrec7.v/vfclass.v (4 mnemonics, the floating unary
     family, the same [vd, vs2] shape vsext.vf2/etc. already use, funct6
     0x13 disambiguated by a fixed rs1-position constant) are likewise each
     a single, non-import-duplicated rv_v record present identically on
     both profiles, so this slice moves 4 records on EACH profile, straight
     from blocked to promoted-support. vfredosum.vs/vfredusum.vs/
     vfredmin.vs/vfredmax.vs (4 mnemonics, the floating vector-reduction
     family, the same [rd, rs2, rs1] all-vector shape as vfadd.vv/etc. but
     under OPFVV rather than OPMVV) are likewise each a single,
     non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 4 records on EACH profile, straight
     from blocked to promoted-support. vmfeq.vv/vmfeq.vf/vmfle.vv/vmfle.vf/
     vmflt.vv/vmflt.vf/vmfne.vv/vmfne.vf/vmfgt.vf/vmfge.vf (10 mnemonics,
     the mask-writing floating comparison family, the same [rd, rs2, rs1]
     shape as vfadd.vv/.vf - vmfgt/vmfge have no [.vv] sibling, matching
     the integer vmsgt/vmsgtu precedent) are likewise each a single,
     non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 10 records on EACH profile, straight
     from blocked to promoted-support. vfmv.f.s/vfmv.s.f/vfmv.v.f (3
     mnemonics, the FPR-typed mirror of vmv.x.s/vmv.s.x/vmv.v.x) are
     likewise each a single, non-import-duplicated rv_v record present
     identically on both profiles, so this slice moves 3 records on EACH
     profile, straight from blocked to promoted-support. vfmerge.vfm (1
     mnemonic, the FPR-typed mirror of vmerge.vxm) is likewise a single,
     non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 1 record on EACH profile, straight from
     blocked to promoted-support. vfcvt.xu.f.v/vfcvt.x.f.v/vfcvt.f.xu.v/
     vfcvt.f.x.v/vfcvt.rtz.xu.f.v/vfcvt.rtz.x.f.v (6 mnemonics, the
     scalar-width float<->integer conversion family, vext_form's exact
     "vd, vs2" shape) are likewise each a single, non-import-duplicated
     rv_v record present identically on both profiles, so this slice moves
     6 records on EACH profile, straight from blocked to promoted-support.
     vfmadd.vv/.vf/vfnmadd.vv/.vf/vfmsub.vv/.vf/vfnmsub.vv/.vf/vfmacc.vv/.vf/
     vfnmacc.vv/.vf/vfmsac.vv/.vf/vfnmsac.vv/.vf (16 mnemonics, the floating
     fused-multiply-add family, the same reordered [rd, rs1, rs2] shape as
     vmacc/etc.) are likewise each a single, non-import-duplicated rv_v
     record present identically on both profiles, so this slice moves 16
     records on EACH profile, straight from blocked to promoted-support.
     vfslide1up.vf/vfslide1down.vf (2 mnemonics, the slide family's floating
     single-element siblings, OPFVF reusing opfvf_form's own shape verbatim)
     are likewise each a single, non-import-duplicated rv_v record present
     identically on both profiles, so this slice moves 2 records on EACH
     profile, straight from blocked to promoted-support. vfwadd.vv/.vf/.wv/
     .wf and vfwsub.vv/.vf/.wv/.wf (8 mnemonics, the widening floating add/
     subtract pair - operand width is invisible to the assembler, so this
     reuses opfvv_form/opfvf_form's shape verbatim, the same "width doesn't
     change the encoding" precedent vwadd/vwsub's own promotion established)
     are likewise each a single, non-import-duplicated rv_v record present
     identically on both profiles, so this slice moves 8 records on EACH
     profile, straight from blocked to promoted-support. vfwmul.vv/.vf (2
     mnemonics, the widening floating multiply - no [.wv]/[.wf] sibling,
     unlike vfwadd/vfwsub's symmetric pairs) are likewise each a single,
     non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 2 records on EACH profile, straight
     from blocked to promoted-support. vfwredosum.vs/vfwredusum.vs (2
     mnemonics, the widening floating reduction pair, reusing
     opfvv_funct6's own [rd, rs2, rs1] shape verbatim - vfwredsum.vs is a
     real-GNU-as pseudo-op alias for vfwredusum.vs, riscv-opcodes' own
     kind: pseudo-op record rather than a distinct kind: instruction-form
     one, so it is not separately counted here) are likewise each a
     single, non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 2 records on EACH profile, straight
     from blocked to promoted-support. vfwcvt.xu.f.v/vfwcvt.x.f.v/
     vfwcvt.f.xu.v/vfwcvt.f.x.v/vfwcvt.f.f.v/vfwcvt.rtz.xu.f.v/
     vfwcvt.rtz.x.f.v/vfncvt.xu.f.w/vfncvt.x.f.w/vfncvt.f.xu.w/
     vfncvt.f.x.w/vfncvt.f.f.w/vfncvt.rod.f.f.w/vfncvt.rtz.xu.f.w/
     vfncvt.rtz.x.f.w (15 mnemonics, the widening/narrowing
     float<->integer conversion families, reusing vfcvt.*.v's own "vd,
     vs2" opfvv_unary_const shape verbatim under the same funct6 - the
     two bf16 sibling mnemonics, rv_zvfbfmin, are deliberately not
     admitted, having no existing requirement/admission plumbing) are
     likewise each a single, non-import-duplicated rv_v record present
     identically on both profiles, so this slice moves 15 records on EACH
     profile, straight from blocked to promoted-support. vfwmacc.vv/.vf,
     vfwnmacc.vv/.vf, vfwmsac.vv/.vf, vfwnmsac.vv/.vf (8 mnemonics, the
     widening floating fused-multiply-add family, reusing
     vfmacc/vfnmacc/vfmsac/vfnmsac's own reordered-operand shape verbatim)
     are likewise each a single, non-import-duplicated rv_v record present
     identically on both profiles, so this slice moves 8 records on EACH
     profile, straight from blocked to promoted-support. vle8.v/vle16.v/
     vle32.v/vle64.v/vse8.v/vse16.v/vse32.v/vse64.v (8 mnemonics, V's
     unit-stride vector-register loads/stores, a genuinely new "vd/vs3,
     (base)" memory-operand shape reusing lr_form's own zero-offset
     encoding pattern with a vector register in place of a GPR) are
     likewise each a single, non-import-duplicated rv_v record present
     identically on both profiles, so this slice moves 8 records on EACH
     profile, straight from blocked to promoted-support. vlm.v/vsm.v (2
     mnemonics, V's mask-register load/store, the identical "vd/vs3,
     (base)" shape but with a fixed lumop/sumop and vm permanently 1 - no
     masked variant, and unlike the unit-stride family nf is not even a
     free field in the source record) are likewise each a single,
     non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 2 records on EACH profile, straight
     from blocked to promoted-support. vle8ff.v/vle16ff.v/vle32ff.v/
     vle64ff.v (4 mnemonics, V's fault-only-first unit-stride loads, the
     identical "vd, (base)" load shape as vle8.v/etc. but with lumop
     fixed to 0x10 instead of 0 - no store counterpart) are likewise each
     a single, non-import-duplicated rv_v record present identically on
     both profiles, so this slice moves 4 records on EACH profile,
     straight from blocked to promoted-support. vlse8.v/vlse16.v/
     vlse32.v/vlse64.v/vsse8.v/vsse16.v/vsse32.v/vsse64.v (8 mnemonics,
     V's strided loads/stores, a genuinely new three-operand "vd/vs3,
     (base), rs2" shape - the plain-GPR byte stride carried in the
     word's rs2 field, mop=0b10) are likewise each a single,
     non-import-duplicated rv_v record present identically on both
     profiles, so this slice moves 8 records on EACH profile, straight
     from blocked to promoted-support. vluxei8.v/vluxei16.v/vluxei32.v/
     vluxei64.v/vloxei8.v/vloxei16.v/vloxei32.v/vloxei64.v/vsuxei8.v/
     vsuxei16.v/vsuxei32.v/vsuxei64.v/vsoxei8.v/vsoxei16.v/vsoxei32.v/
     vsoxei64.v (16 mnemonics, V's indexed loads/stores, the strided
     family's own three-operand shape with a vector-register index in
     place of the GPR stride, mop=0b01/0b11 for unordered/ordered) are
     likewise each a single, non-import-duplicated rv_v record present
     identically on both profiles, so this slice moves 16 records on
     EACH profile, straight from blocked to promoted-support - closing
     out every remaining blocked rv_v load/store record except the
     whole-register family. vl{1,2,4,8}re{8,16,32,64}.v/
     vs{1,2,4,8}r.v (20 mnemonics, V's whole-register loads/stores - the
     register count is baked into the mnemonic itself, so nf is fixed
     rather than a free field, exactly like vlm.v/vsm.v, and both
     vlm_form/vsm_form are reused verbatim with zero new normalization
     code) are likewise each a single, non-import-duplicated rv_v record
     present identically on both profiles, so this slice moves 20
     records on EACH profile, straight from blocked to promoted-support
     - closing every remaining blocked rv_v record: the entire 375-record
     family is now promoted-support. vclmul.vv/vclmul.vx/vclmulh.vv/
     vclmulh.vx (4 mnemonics, Zvbc's carry-less multiply, the same OPMVV/
     OPMVX shape as vmul/vdivu/etc.) are likewise each a single,
     non-import-duplicated rv_zvbc record present identically on both
     profiles, so this slice moves 4 records on EACH profile, straight
     from blocked to promoted-support. vghsh.vv/vgmul.vv (2 mnemonics,
     Zvkg's GCM/GHASH pair, a genuinely new major opcode - 0x77 rather
     than OP-V's 0x57 - with no mask bit at all) are likewise each a
     single, non-import-duplicated rv_zvkg record present identically on
     both profiles, so this slice moves 2 records on EACH profile,
     straight from blocked to promoted-support. vsha2ms.vv/vsha2ch.vv/
     vsha2cl.vv (3 mnemonics, Zvknha's SHA-256 vector helpers, the same
     opcode-0x77 no-mask ternary shape as vghsh.vv) are imported verbatim
     by both rv_zvknhb (whose own 3 records are ONLY these imports, so
     the entire family promotes) and rv_zvkn (a 23-record bundle
     extension that also imports Zvbb's/Zvkned's own mnemonics, neither
     promoted yet - only these 3 of its 23 records promote here), so
     this slice moves 3 (rv_zvknha, non-import) + 3 (rv_zvknhb, import)
     + 3 (rv_zvkn, import) = 9 records on EACH profile, straight from
     blocked to promoted-support. vsm4k.vi/vsm4r.vv/vsm4r.vs (3
     mnemonics, Zvksed's SM4 block-cipher helpers - vsm4k.vi the first
     opcode-0x77 zimm5-ternary shape, vsm4r.vv/vs the fixed-vs1 unary
     shape) are imported verbatim by rv_zvks alone (a 14-record bundle,
     not fully promoted - only these 3 of its 14 records promote here),
     so this slice moves 3 (rv_zvksed, non-import) + 3 (rv_zvks, import)
     = 6 records on EACH profile, straight from blocked to
     promoted-support. vsm3c.vi/vsm3me.vv (2 mnemonics, Zvksh's SM3 hash
     helpers, reusing vsm4k.vi's own zimm5-ternary shape and vghsh.vv's
     own ternary shape verbatim) are imported by rv_zvks alone too (5 of
     its 14 records now promote), so this slice moves 2 (rv_zvksh,
     non-import) + 2 (rv_zvks, import) = 4 records on EACH profile,
     straight from blocked to promoted-support. Zvbb's bit-manipulation
     family (16 mnemonics: vandn.vv/.vx, vbrev.v, vbrev8.v, vclz.v,
     vcpop.v, vctz.v, vrev8.v, vrol.vv/.vx, vror.vv/.vx/.vi, vwsll.vv/
     .vx/.vi - opcode 0x57/OP-V proper, unlike every other rv_zv* family
     above) closes rv_zvbb entirely, and its own 9-mnemonic Zvkb subset
     (vandn/vbrev8/vrev8/vrol/vror) is imported verbatim by BOTH rv_zvkn
     and rv_zvks, closing rv_zvks entirely (its last 9 blocked records)
     and rv_zvkn partway (12 of 23; the other 11 belong to not-yet-
     promoted Zvkned). So this slice moves 16 (rv_zvbb, non-import) + 9
     (rv_zvkn, import) + 9 (rv_zvks, import) = 34 records on EACH
     profile, straight from blocked to promoted-support. Zvkned's AES
     round/key-schedule family (11 mnemonics) is imported by rv_zvkn
     alone, closing rv_zvkn entirely (its last 11 blocked records), so
     this slice moves 11 (rv_zvkned, non-import) + 11 (rv_zvkn, import)
     = 22 records on EACH profile, straight from blocked to
     promoted-support. Zvfbfmin's bf16<->f32 conversion pair
     (vfwcvtbf16.f.f.v/vfncvtbf16.f.f.w) and Zvfbfwma's bf16 widening
     FMA pair (vfwmaccbf16.vv/.vf) are each single, non-import-
     duplicated records with no alternative-extension group, so this
     slice moves 4 records on EACH profile, straight from blocked to
     promoted-support - closing the entire twelve-family rv_zv*
     vector-crypto/bf16 scope: every rv_zv* family (and both bundle
     extensions, rv_zvkn/rv_zvks) is now fully promoted.

     x86's SUB_GPRv_GPRv_29/AND_GPRv_GPRv_21/OR_GPRv_GPRv_09/
     XOR_GPRv_GPRv_31/ADC_GPRv_GPRv_11/SBB_GPRv_GPRv_19/CMP_GPRv_GPRv_39/
     TEST_GPRv_GPRv (the x86 continuation, ADD_GPRv_GPRv_01's own
     [to_rm_r] opcode-selection precedent generalized to the rest of that
     table) are each a single, non-import-duplicated I86 record, straight
     from blocked to promoted-support, so this slice moves 8 records on
     EACH x86 profile.

     x86's ADD_GPRv_MEMv/ADC_GPRv_MEMv/XOR_GPRv_MEMv (the register<-memory
     ALU direction, ADD/ADC/XOR being the only three [to_r_rm] opcodes
     this project's encoder currently lowers with a memory source) are
     each a single, non-import-duplicated I86 record, straight from
     blocked to promoted-support, so this slice moves 3 records on EACH
     x86 profile.

     x86's SUB_GPRv_MEMv/AND_GPRv_MEMv/OR_GPRv_MEMv/SBB_GPRv_MEMv/
     CMP_GPRv_MEMv (the register<-memory ALU direction's remaining five
     [to_r_rm] opcodes, this session's first x86 slice needing real
     encoder changes rather than only normalization/corpus wiring - new
     [Opcode.to_r_rm] table entries, a generalized [Alu_r_rm] lowering
     match arm, and a generalized [alu_r_rm_codec] entries list in
     x86_family_encode.ml) are each a single, non-import-duplicated I86
     record, straight from blocked to promoted-support, so this slice
     moves 5 records on EACH x86 profile.

     x86's OR_GPRv_IMMz/ADC_GPRv_IMMz/SBB_GPRv_IMMz/AND_GPRv_IMMz/
     SUB_GPRv_IMMz/XOR_GPRv_IMMz/CMP_GPRv_IMMz (the rest of the
     register/immediate ALU family, generalizing ADD_GPRv_IMMz's own
     shape to an explicit-32-bit mnemonic the same way the
     register-register slice generalized ADD_GPRv_GPRv_01's own bare
     mnemonic) are each a single, non-import-duplicated I86 record,
     straight from blocked to promoted-support; SBB needed its own small
     encoder change too (Opcode.to_ext/of_ext gained the one gap at ext
     3, and the Alu_rm_imm-producing lowering arm's opcode list gained
     Sbb), so this slice moves 7 records on EACH x86 profile.

     x86's ADD/OR/ADC/SBB/AND/SUB/XOR/CMP_GPRv_IMMb (the imm8 rung of the
     same register/immediate ALU family, opcode 0x83 - includes ADD this
     time, since the immz rung's own ADD exclusion is specific to
     ADD_GPRv_IMMz's bare-mnemonic design test) plus the MEMv<-IMMb and
     MEMv<-IMMz directions of all eight mnemonics (opcodes 0x83/0x81 with
     a memory r/m) are each a single, non-import-duplicated I86 record,
     straight from blocked to promoted-support, needing no encoder change
     at all - this project's own [lower_instruction] already lowers an
     [Operand.Mem] ALU-immediate destination through the same
     [Lowered.Alu_rm_imm] codec the register destination uses - so this
     slice moves 24 records on EACH x86 profile (8 + 8 + 8).

     rv_v_aliases' eleven riscv-opcodes-native deprecated pseudo-op
     spellings for already-admitted rv_v mnemonics - vpopc.m (vcpop.m),
     vmandnot.mm/vmornot.mm (vmandn.mm/vmorn.mm), vfredsum.vs/vfwredsum.vs
     (vfredusum.vs/vfwredusum.vs), vl1r.v/vl2r.v/vl4r.v/vl8r.v
     (vl1re8.v/vl2re8.v/vl4re8.v/vl8re8.v), and vle1.v/vse1.v (vlm.v/
     vsm.v) - are each a single, non-import-duplicated record whose own
     mask/value is identical to the canonical mnemonic it specializes
     (hand-verified against real GNU as for all eleven), so each reuses
     the canonical mnemonic's own normalization shape function unchanged;
     the assembler's own frontend needed one small addition -
     [Opcode.of_mnemonic] resolves each deprecated spelling to the
     canonical opcode before table lookup, rather than a new opcode
     variant - confirmed byte-identical against real GNU as via this
     project's own encoder too. This slice moves 11 records on EACH RISC-V
     profile.

     GRP1's own byte-operand rung of the register/immediate ALU family
     (opcode 0x80, register or memory destination -
     ADD/OR/ADC/SBB/AND/SUB/XOR/CMP_GPR8_IMMb_80r<N> and
     _MEMb_IMMb_80r<N>) and the accumulator-immediate byte rung (opcode
     ext<<3|4 - ADD/OR/ADC/SBB/AND/SUB/XOR/CMP_AL_IMMb) close the same eight
     mnemonics' byte-width immediate space {!Isa_norm_xed.alu_gpr8_immb_form}/
     {!alu_memb_immb_form}/{!alu_al_immb_form} normalize - a genuinely
     different opcode from the GPRv rungs above, not a narrower reading of
     one of them (x86_family_encode.ml's {!alu_form_byte}/
     {!alu_acc_form_byte} doc comments explain why). This slice moves 24
     records per x86 profile (8 mnemonics x 3 shapes).

     The reverse, MEMv<-GPRv, ALU direction (ADD/OR/ADC/SBB/AND/SUB/XOR/
     CMP/TEST_MEMv_GPRv - `addl %eax, 0x10(%esp)`) closes the last
     unbuilt direction of the base legacy ALU/MOV opcode space: unlike
     every family above, `Opcode.to_rm_r`'s own table already covered
     this opcode (a memory r/m and a register r/m share one opcode per
     operation, only ModR/M's mod field differs), so only the new
     `Lowered.Alu_rm_r`-producing lowering arm and
     {!Isa_norm_xed.alu_memv_gprv_form} were needed, no encoder table
     change. TEST is included, unlike its own GPRv_MEMv load-direction
     exclusion above - XED does export a `TEST_MEMv_GPRv` record. This
     slice moves 9 records per x86 profile (9 mnemonics x 1 shape), closing
     the base legacy x86 ALU/MOV opcode space in full.

     `ADDSD`/`SUBSD`/`MULSD`/`DIVSD_XMMsd_XMMsd` (SSE2 scalar-float
     register-register binops, `addsd %xmm1, %xmm0`) are the first
     xmm-register admission: `x86_family_encode.ml`'s own
     `Lowered.Sse_binop_r_rm` and `sse_binop_f2_codec` table already fully
     implement and fixture-verify these four ops (M5, asm/docs/corpus.md),
     so this slice is pure normalization/admission wiring, needing only
     the model's own new `X86_xmm` register class (`Isa_norm_model`'s own
     docstring already names "vector masks, EVEX broadcast, VEX operands"
     as exactly the kind of gap later normalization work is for) and
     `Isa_norm_xed.xmm_binop_rr_form`. This slice moves 4 records per x86
     profile and opens the entirely-unadmitted x86 vector/SIMD space the
     prior milestone flagged as needing new infrastructure - register-memory
     SSE forms, the remaining SSE/SSE2 op families, and VEX/EVEX all remain
     unadmitted.

     `ADDSD`/`SUBSD`/`MULSD`/`DIVSD_XMMsd_MEMsd` (the register<-memory
     sibling, `addsd 16(%esp), %xmm0`) close that immediate follow-up: the
     encoder's own `Lowered.Sse_binop_r_rm` already builds both directions
     off one `rm : Rm.t` field, so this needed only the new
     `Isa_norm_xed.xmm_binop_rm_form` normalizer, no encoder change. This
     slice moves 4 more records per x86 profile.

     `MULSS`/`DIVSS` (SSE, not SSE2 - `requirement_of` gained its own
     `Req_feature "x86:sse"` case) and `COMISD`/`UCOMISD`/`COMISS`/`XORPD`/
     `PXOR`/`MOVAPD_XMMpd_XMMpd_0F28`/`CVTSD2SS`/`CVTSS2SD` (SSE2/SSE) close
     the rest of the plain xmm-xmm/xmm-memory binop shape in both directions:
     `x86_family_encode.ml`'s existing `sse_binop_f3_codec`/
     `sse_binop_66_codec`/`sse_binop_none_alt` machinery already fully
     implements every one of these ten mnemonics, so `Isa_norm_xed.
     xmm_binop_rr_form`/`xmm_binop_rm_form` needed only more dispatch cases,
     no new shape or encoder change. `MOVAPD`'s own reverse `MEMpd<-XMMpd`
     store direction and its redundant `_0F29` register-register iform stay
     unadmitted, matching this file's to_rm_r "low-numbered iform" precedent
     (confirmed against real GNU as: `movapd %xmm1, %xmm0` selects opcode
     0x28). This slice moves 20 more records per x86 profile (10 mnemonics x
     2 directions).

     `MOVSD`/`MOVSS` load/store (`MOVSD_XMM_XMMdq_MEMsd`/
     `MOVSD_XMM_MEMsd_XMMsd` and their MOVSS siblings) are a plain move, not
     another binop: `x86_family_encode.ml`'s own comment on
     `Lowered.Sse_mov_r_rm`/`Sse_mov_rm_r` already notes register-register
     `movsd`/`movss` is unbuilt (unevidenced by the corpus), so only the
     load/store direction is admitted here, via a new `Isa_norm_xed.
     xmm_mov_form` generalizing `mov_gprv_memv_form`'s own `~load` direction
     flag to the `X86_xmm` register class instead of `X86_gpr`. No encoder
     change. This slice moves 4 more records per x86 profile.

     `CVTSI2SD`/`CVTSI2SS` (GPR/memory source, XMM dest) and `CVTTSD2SI`
     (XMM/memory source, GPR dest) close the named GPR-mixed conversion
     follow-up: the model's first mixed-register-class shape, via four new
     `Isa_norm_xed` forms (`cvtsi2f_rr_form`/`cvtsi2f_rm_form`/
     `cvtf2i_rr_form`/`cvtf2i_rm_form`), no encoder change -
     `x86_family_encode.ml`'s `Lowered.Cvtsi2f_r_rm`/`Cvtf2i_r_rm` already
     implement both directions. XED reports the 32-bit and 64-bit GPR
     widths (`GPR32d`/`GPR64q`, `MEMd`/`MEMq`) as separate records with
     `provenance.mode_restriction` "unspecified"; a GPR64 operand implies
     64-bit mode by register-class fact alone, so the 64-bit records carry
     a derived `Req_mode {mode="mode64"; equals=true}` and are promoted on
     `X86_64` only (confirmed against real GNU as: `cvtsi2sdq %rax, %xmm0`/
     `cvttsd2si %xmm0, %rax` assemble only in 64-bit mode), while the
     32-bit records promote on both targets like every prior x86 form.
     This slice moves 12 more records per x86 profile (6 promoted on both
     targets, 6 additionally promoted on `X86_64` only), closing the named
     GPR-mixed conversion follow-up.

     `ANDPS`/`ANDNPS`/`ORPS`/`XORPS` (SSE, no mandatory prefix) and
     `ANDPD`/`ANDNPD`/`ORPD` (SSE2, 66 mandatory prefix) - `XORPD`'s packed
     bitwise-logical siblings - reuse the plain xmm-xmm/xmm-memory binop
     shape's normalization (`xmm_binop_rr_form`/`xmm_binop_rm_form`)
     unchanged, but unlike every prior slice in this shape, none of the
     seven existed in the encoder yet: this needed seven new `Opcode.t`
     variants and opcode-table entries (generalizing the previously
     hardcoded-single-member `sse_binop_none_alt` into a table-driven
     `sse_binop_none_codec` the same way the 66-prefixed group already was),
     still emitting the unchanged `Lowered.Sse_binop_r_rm` representation.
     This slice moves 14 records per x86 profile (7 mnemonics x 2
     directions).

     `MOVAPS`/`MOVUPS` (SSE, no mandatory prefix) and `MOVUPD` (SSE2, 66
     mandatory prefix) are `MOVAPD`'s data-movement siblings - aligned and
     unaligned packed move - reusing the identical plain xmm-xmm/xmm-memory
     binop shape and normalization unchanged; like `ANDPS`/etc., none
     existed in the encoder, so this added three more `Opcode.t` variants
     and opcode-table entries (`MOVAPS`/`MOVUPS` into `sse_binop_none_codec`,
     `MOVUPD` into `sse_binop_66_codec`). Only the reg-dest load direction is
     admitted, matching `MOVAPD`'s own precedent: each mnemonic's reverse
     store direction (`MEMxx<-XMMxx`) is a distinct, real form not yet
     built. Confirmed against real GNU as (i686-linux-gnu-as 2.44):
     `movaps %xmm1,%xmm2` -> `0f 28 d1`, `movaps (%eax),%xmm2` -> `0f 28 10`,
     `movups %xmm1,%xmm2` -> `0f 10 d1`, `movupd %xmm1,%xmm2` ->
     `66 0f 10 d1`. This slice moves 6 more records per x86 profile (3
     mnemonics x 2 directions).

     `ADDPS`/`SUBPS`/`MULPS`/`DIVPS` (SSE, no mandatory prefix) and
     `ADDPD`/`SUBPD`/`MULPD`/`DIVPD` (SSE2, 66 mandatory prefix) complete the
     prefix square for opcodes `0x58`/`0x59`/`0x5C`/`0x5E` - the same four
     bytes `ADDSD`/`MULSD`/`SUBSD`/`DIVSD` (F2-prefixed) and `ADDSS`/`MULSS`/
     `SUBSS`/`DIVSS` (F3-prefixed) already use - the way `ANDPS`/etc. and
     `MOVAPS`/etc. each completed the square for their own opcode groups.
     Same plain xmm-xmm/xmm-memory binop shape and normalization unchanged;
     none of the eight existed in the encoder, so this added eight more
     `Opcode.t` variants and opcode-table entries (`ADDPS`/`SUBPS`/`MULPS`/
     `DIVPS` into `sse_binop_none_codec`, `ADDPD`/`SUBPD`/`MULPD`/`DIVPD` into
     `sse_binop_66_codec`). Confirmed against real GNU as (i686-linux-gnu-as/
     x86_64-linux-gnu-as 2.44): `addps %xmm1,%xmm0` -> `0f 58 c1`, `addpd
     %xmm1,%xmm0` -> `66 0f 58 c1`, and likewise for `sub`/`mul`/`div` at
     `0x5C`/`0x59`/`0x5E`. This slice moves 16 more records per x86 profile (8
     mnemonics x 2 directions), closing every already-encoder-supported
     scalar/plain-binop-shaped SSE/SSE2 mnemonic; only VEX/EVEX remain
     unadmitted in x86 vector/SIMD.

     `VADDSD`/`VSUBSD`/`VMULSD`/`VDIVSD` (AVX, `VEX.LIG.F2.0F.WIG` register-
     register form only) are this project's first x86 vector-extension
     (VEX/AVX) admission, distinct in kind from every SSE/SSE2 slice above:
     a genuinely new `Lowered.Vex_binop_rrr` shape (three real registers,
     `dst := src1 op src2`, non-destructive - unlike every legacy binop's
     destructive `dst := dst op src`) and a new two-byte-VEX (`0xC5`) codec
     path (`vex_scalar_f2_rrr_alt`/`vex_scalar_f2_codec`), reusing the
     existing `rm_codec` for the trailing ModR/M byte. `src2` (the ModR/M
     r/m operand) is restricted to xmm0-7 - encoding xmm8-15 there needs
     REX.B's VEX counterpart, which only the not-yet-built three-byte VEX
     prefix (`0xC4`) carries - while `dst`/`src1` reach all of xmm0-15
     already via the two-byte prefix's own R and vvvv bits, confirmed
     against real GNU as: `vaddsd %xmm2,%xmm1,%xmm0` -> `c5 f3 58 c2`,
     `vaddsd %xmm7,%xmm3,%xmm5` -> `c5 e3 58 ef`, `vaddsd %xmm2,%xmm9,%xmm10`
     -> `c5 33 58 d2` (dst/src1 both xmm8+), and `vaddsd %xmm8,...` correctly
     rejected with a named diagnostic (`Vex_rm_extended_register`) rather
     than silently misencoding. New `vex_binop_rrr_form` normalizer and a
     new `x86:avx` feature mapping. This slice moved 4 records per x86
     profile (4 mnemonics, register-register only); the register<-memory
     sibling was admitted in a later slice and is reflected in the totals
     below already.

     `VADDSS`/`VSUBSS`/`VMULSS`/`VDIVSS` (AVX, `VEX.LIG.F3.0F.WIG`) are
     `VADDSD`/etc.'s scalar-single siblings, differing only in the VEX
     codec's `pp` field (2, not 3) - `vex_scalar_f2_rrr_alt` generalized
     into `vex_scalar_rrr_alt ~pp ~opcode_codec` (mirroring how
     `sse_binop_alt` is generalized over `~mandatory`/`~opcode_codec`), plus
     a new `vex_scalar_f3_codec` opcode table; `Lowered.Vex_binop_rr_rm`,
     `vex_rm_ok`, and the `vex_binop_rrr_form`/`vex_binop_rr_mem_form`
     normalizers are reused unchanged. Confirmed against real GNU as:
     `vaddss %xmm2,%xmm1,%xmm0` -> `c5 f2 58 c2`, `vsubss
     %xmm7,%xmm3,%xmm5` -> `c5 e2 5c ef`. Both register-register and
     register<-memory forms admitted together (8 records per x86 profile,
     4 mnemonics x 2 directions).

     `VADDPS`/`VSUBPS`/`VMULPS`/`VDIVPS` (`pp = 0`, no mandatory prefix) and
     `VADDPD`/`VSUBPD`/`VMULPD`/`VDIVPD` (`pp = 1`, mandatory `66`) complete
     the VEX `pp` square (F2/F3/none/66) for opcodes `0x58`/`0x59`/`0x5C`/
     `0x5E`, mirroring how the legacy `ADDPS`/`ADDPD` slice completed the
     same square for the non-VEX encoding. Two new opcode tables
     (`vex_scalar_none_codec`/`vex_scalar_66_codec`) plug into the existing
     `vex_scalar_rrr_alt`/`Vex_binop_rr_rm`/`vex_rm_ok` machinery unchanged -
     pure mechanical extension, no new representation. Confirmed against
     real GNU as (i686-linux-gnu-as/x86_64-linux-gnu-as 2.44), identical on
     both targets: `vaddps %xmm2,%xmm1,%xmm0` -> `c5 f0 58 c2`, `vsubps
     %xmm7,%xmm3,%xmm5` -> `c5 e0 5c ef`, `vaddpd %xmm2,%xmm1,%xmm0` ->
     `c5 f1 58 c2`, and likewise for `mul`/`div` at `0x59`/`0x5E` and for the
     register<-memory direction (e.g. `vaddps 16(%esp),%xmm1,%xmm0` ->
     `c5 f0 58 44 24 10`). Both register-register and register<-memory forms
     admitted together (16 records per x86 profile, 8 mnemonics x 2
     directions). AVX (the XED family) now stands at promoted-support
     32/676 (x86-32) and 32/702 (x86-64). YMM (VEX.L), three-byte VEX, and
     EVEX remain deferred.

     PSHUFD/PSHUFLW/PSHUFHW (opcode 0x70, 66/F2/F3 mandatory-prefix groups) reuse `Sse_binop_imm_r_rm`/`xmm_binop_imm_rr_form`/
     `xmm_binop_imm_rm_form` unchanged - genuinely unary (XED marks `dest`
     `w`, not `rw`, unlike `SHUFPS`'s destructive read-write `reg`), but the
     byte-level ModR/M-reg/ModR/M-rm/trailing-imm8 shape is identical.
     Their VEX siblings VPSHUFD/VPSHUFLW/VPSHUFHW are genuinely
     two-operand-plus-immediate (no real `vvvv` operand, confirmed against
     real GNU as rejecting a third register operand), needing a new
     `Lowered.Vex_unop_imm_r_rm`/`vex_unop_imm_alt` shape mirroring
     `Vex_unop_r_rm`'s own no-`vvvv` convention plus `Vex_binop_imm_rr_rm`'s
     trailing imm8. Confirmed against real GNU as (i686-linux-gnu-as/
     x86_64-linux-gnu-as 2.44), identical on both targets:
     `pshufd $0x1b,%xmm2,%xmm1` -> `66 0f 70 ca 1b`, `pshuflw`/`pshufhw` at
     the same opcode with `F2`/`F3`, `vpshufd $0x1b,%xmm2,%xmm1` ->
     `c5 f9 70 ca 1b`, and the register<-memory direction for all six. 12
     new records per x86 profile (6 legacy + 6 VEX). SSE2 and AVX both move
     up by 12 promoted-support records on each profile with this slice.

     PADDB/PADDW/PADDD/PADDQ/PSUBB/PSUBW/PSUBD/PSUBQ (opcodes
     0xFC/0xFD/0xFE/0xD4 add, 0xF8/0xF9/0xFA/0xFB subtract) reuse
     `Sse_binop_r_rm`/`xmm_binop_rr_form`/`xmm_binop_rm_form` (legacy) and
     `Vex_binop_rr_rm`/`vex_binop_rrr_form`/`vex_binop_rr_mem_form` (VEX)
     completely unchanged - 66-mandatory-prefix-only integer SIMD, no
     non-66 sibling, the same shape as `PUNPCKLQDQ`/`VPUNPCKLQDQ`. Confirmed
     against real GNU as (i686-linux-gnu-as/x86_64-linux-gnu-as 2.44):
     `paddb %xmm2,%xmm1` -> `66 0f fc ca`, `vpaddb %xmm3,%xmm2,%xmm1` ->
     `c5 e9 fc cb`. 32 new records per x86 profile (16 legacy + 16 VEX, 8
     mnemonics x 2 directions each). SSE2 and AVX both move up by 16
     promoted-support records on each profile with this slice.

     PCMPEQB/PCMPEQW/PCMPEQD/PCMPGTB/PCMPGTW/PCMPGTD (opcodes 0x74/0x75/0x76
     equal, 0x64/0x65/0x66 greater-than) reuse the same shapes as
     `PADDB`/`VPADDB` unchanged - `PADDB`'s own 66-mandatory-prefix-only
     integer-SIMD group at different opcode bytes. Confirmed against real
     GNU as: `pcmpeqb %xmm2,%xmm1` -> `66 0f 74 ca`, `vpcmpeqb
     %xmm3,%xmm2,%xmm1` -> `c5 e9 74 cb`. 24 new records per x86 profile (12
     legacy + 12 VEX, 6 mnemonics x 2 directions each). SSE2 and AVX both
     move up by 12 promoted-support records on each profile with this
     slice.

     PAND/PANDN/POR (opcodes 0xDB/0xDF/0xEB) reuse the same shapes as
     `PADDB`/`VPADDB` unchanged - `PXOR`'s own 66-mandatory-prefix-only
     integer-SIMD group at different opcode bytes, and PMINUB/PMAXUB/PMINSW/
     PMAXSW (opcodes 0xDA/0xDE/0xEA/0xEE) - `PADDB`'s own group at yet more
     opcode bytes. Confirmed against real GNU as: `pand %xmm2,%xmm1` ->
     `66 0f db ca`, `vpand %xmm3,%xmm2,%xmm1` -> `c5 e9 db cb`, `pminub
     %xmm2,%xmm1` -> `66 0f da ca`, `vpminub %xmm3,%xmm2,%xmm1` ->
     `c5 e9 da cb`. 28 new records per x86 profile (14 legacy + 14 VEX, 7
     mnemonics x 2 directions each). SSE2 and AVX both move up by 14
     promoted-support records on each profile with this slice. *)
  (* The alias class: [mv], [snez], [nop] and [ret] on both profiles and RV64-only [sext.w] -
     riscv-opcodes $pseudo_op records with a fully fixed encoding, each an alias of the
     instruction it specializes - move 4 (RV32) and 5 (RV64) records from blocked to
     promoted-support after a persisted case per profile pins GNU as and this assembler to the
     same bytes for the alias spelling. *)
  expect ~source:"riscv_opcodes" Target.Riscv32 ~total:1089 ~normalized_only:20 ~gas_generatable:0
    ~promoted_support:706 ~blocked:363;
  expect ~source:"riscv_opcodes" Target.Riscv64 ~total:1154 ~normalized_only:30 ~gas_generatable:0
    ~promoted_support:749 ~blocked:375;
  expect ~source:"xed_resolved" Target.X86_32 ~total:7887 ~normalized_only:6 ~gas_generatable:5
    ~promoted_support:1022 ~blocked:6854;
  expect ~source:"xed_resolved" Target.X86_64 ~total:10571 ~normalized_only:0 ~gas_generatable:5
    ~promoted_support:1030 ~blocked:9536

(* Export and round-trip deterministic normalized JSONL: every
   form Isa_norm_riscv/Isa_norm_xed produce from the real checked-in exports
   - not just synthetic values, which Test_isa_norm_jsonl already covers for
   every constructor - must survive Isa_norm_jsonl.encode_line followed by
   decode_line unchanged. The pinned total is the sum of the accounting
   tests' own pinned normalized counts (726+779+354+356); a drop here without
   a matching drop there would mean the codec silently lost a form the
   accounting still credits as normalized. *)
let normalize_one source (rec_ : Isa_source_record.t) =
  match source with
  | "riscv_opcodes" -> Isa_norm_riscv.normalize rec_
  | "xed_resolved" -> Isa_norm_xed.normalize rec_
  | other -> Error { Isa_norm_model.rule = "unhandled-source"; message = other }

let test_isa_norm_jsonl_roundtrip repo =
  let roundtrip_count = ref 0 in
  let check_source ~source target =
    let label = Printf.sprintf "%s/%s" source (Target.to_string target) in
    let path = Repo.isa_db_export repo ~source target in
    match Isa_source_record.read_file path with
    | Error e -> check (Format.asprintf "%a" (Err.Error.pp Tool_error.pp) e) false
    | Ok records ->
        List.iter
          (fun rec_ ->
            match normalize_one source rec_ with
            | Error _ -> ()
            | Ok (form : Isa_norm_model.form) -> (
                incr roundtrip_count;
                match Isa_norm_jsonl.encode_line form with
                | Error e ->
                    check
                      (Format.asprintf "%s: %s: encode_line (%a)" label form.form_id
                         (Err.Error.pp Tool_error.pp) e)
                      false
                | Ok line -> (
                    match Isa_norm_jsonl.decode_line line with
                    | Error e ->
                        check
                          (Format.asprintf "%s: %s: decode_line (%a)" label form.form_id
                             (Err.Error.pp Tool_error.pp) e)
                          false
                    | Ok decoded ->
                        check
                          (Printf.sprintf "%s: %s: round-trips" label form.form_id)
                          (decoded = form))))
          records
  in
  check_source ~source:"riscv_opcodes" Target.Riscv32;
  check_source ~source:"riscv_opcodes" Target.Riscv64;
  check_source ~source:"xed_resolved" Target.X86_32;
  check_source ~source:"xed_resolved" Target.X86_64;
  check
    (Printf.sprintf "isa-norm-jsonl: %d real normalized forms round-tripped (expected 3573)"
       !roundtrip_count)
    (!roundtrip_count = 3573)

(* Exercise the snapshot-update mapping report, Isa_source_snapshot_diff,
   against the real checked-in exports, not just Test_isa_source_snapshot_diff's
   synthetic pairs. Only one snapshot is pinned today, so diffing each file
   against itself is the whole real workflow currently available - but it is
   the actual load/diff/report path a real future snapshot bump would use,
   proving it against real data (correct decoding of every real record_id,
   real fingerprinting, no spurious drift) rather than only against
   hand-built pairs. *)
let test_isa_source_snapshot_diff repo =
  let expect ~source target ~total =
    let label = Printf.sprintf "%s/%s" source (Target.to_string target) in
    let path = Repo.isa_db_export repo ~source target in
    match Isa_source_snapshot_diff.diff_files path path with
    | Error e -> check (Format.asprintf "%a" (Err.Error.pp Tool_error.pp) e) false
    | Ok r ->
        check
          (Printf.sprintf "isa-snapshot-diff: %s: no drift against itself" label)
          (r.added = 0 && r.removed = 0 && r.changed = 0);
        check
          (Printf.sprintf "isa-snapshot-diff: %s: %d ids unchanged (expected %d)" label r.unchanged
             total)
          (r.unchanged = total)
  in
  expect ~source:"riscv_opcodes" Target.Riscv32 ~total:1089;
  expect ~source:"riscv_opcodes" Target.Riscv64 ~total:1154;
  expect ~source:"xed_resolved" Target.X86_32 ~total:7887;
  expect ~source:"xed_resolved" Target.X86_64 ~total:10571

(* The pilot manifest freezes form_ids against what normalization produces
   TODAY (Isa_gen_pilot.mli: "must equal what Isa_norm_riscv/Isa_norm_xed
   actually produce today"); this is what makes that a checked claim rather
   than an assertion. Isa_gen_pilot.normalize_entry is the shared lookup
   the case builder also uses - this is deliberately not a duplicate. *)
let test_gen_pilot_manifest repo =
  List.iter
    (fun (entry : Isa_gen_pilot.pilot_entry) ->
      let label = Printf.sprintf "%s: %s" (Target.to_string entry.target) entry.lookup_key in
      match Isa_gen_pilot.normalize_entry repo entry with
      | Error e ->
          check (Format.asprintf "gen-pilot: %s: %a" label (Err.Error.pp Tool_error.pp) e) false
      | Ok (form : Isa_norm_model.form) ->
          check
            (Printf.sprintf "gen-pilot: %s normalizes to %s (got %s)" label entry.form_id
               form.form_id)
            (String.equal form.form_id entry.form_id))
    Isa_gen_pilot.all

(* The non-frozen difficult-form manifest freezes form_ids the same way
   the pilot does; Isa_gen_difficult.normalize_entry delegates to
   Isa_gen_pilot.normalize_entry, so this is the same real-export grounding
   check as test_gen_pilot_manifest, over Isa_gen_difficult.all instead. *)
let test_gen_difficult_manifest repo =
  List.iter
    (fun (entry : Isa_gen_difficult.entry) ->
      let label = Printf.sprintf "%s: %s" (Target.to_string entry.target) entry.case_id in
      match Isa_gen_difficult.normalize_entry repo entry with
      | Error e ->
          check (Format.asprintf "gen-difficult: %s: %a" label (Err.Error.pp Tool_error.pp) e) false
      | Ok (form : Isa_norm_model.form) ->
          check
            (Printf.sprintf "gen-difficult: %s normalizes to %s (got %s)" label entry.form_id
               form.form_id)
            (String.equal form.form_id entry.form_id))
    Isa_gen_difficult.all

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "usage: repo_tests.exe <repository-root>";
    exit 2);
  let root = Fpath.v Sys.argv.(1) in
  (* The root is validated by the same sentinels every command uses, so a
     mis-passed argument fails here rather than producing confusing findings. *)
  let repo =
    match Repo.resolve ~cli:(Some root) ~env:(fun _ -> None) ~cwd:root with
    | Ok repo -> repo
    | Error e ->
        prerr_endline (Tool_error.to_fatal_line (Err.Error.kind e));
        exit 1
  in
  print_endline "repo_tests:";
  test_target_db_agrees root;
  test_target_sets root;
  test_derived_invariants root;
  test_isa_db_cross_validate repo;
  test_isa_norm_accounting repo;
  test_isa_family_admission repo;
  test_isa_residual_ledger repo;
  test_isa_norm_jsonl_roundtrip repo;
  test_isa_source_snapshot_diff repo;
  test_gen_pilot_manifest repo;
  test_gen_difficult_manifest repo;
  if !failures > 0 then (
    Printf.printf "repo_tests: %d failures\n" !failures;
    exit 1)
  else print_endline "repo_tests: all checks passed"
