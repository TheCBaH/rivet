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
  expect ~source:"riscv_opcodes" Target.Riscv32 ~total:1089 ~normalized:607;
  expect ~source:"riscv_opcodes" Target.Riscv64 ~total:1154 ~normalized:659;
  expect ~source:"xed_resolved" Target.X86_32 ~total:7887 ~normalized:9;
  expect ~source:"xed_resolved" Target.X86_64 ~total:10571 ~normalized:9

(* The family matrix is a second view over the same complete population,
   not a hand-maintained support claim. Pinning its aggregate states makes a
   source update or an accidental widening of support credit a reviewed
   change, while the per-family invariant makes a dropped native family fail
   even if an aggregate happens to stay plausible. *)
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
     family is now promoted-support. *)
  expect ~source:"riscv_opcodes" Target.Riscv32 ~total:1089 ~normalized_only:20 ~gas_generatable:0
    ~promoted_support:587 ~blocked:482;
  expect ~source:"riscv_opcodes" Target.Riscv64 ~total:1154 ~normalized_only:30 ~gas_generatable:0
    ~promoted_support:629 ~blocked:495;
  expect ~source:"xed_resolved" Target.X86_32 ~total:7887 ~normalized_only:0 ~gas_generatable:5
    ~promoted_support:4 ~blocked:7878;
  expect ~source:"xed_resolved" Target.X86_64 ~total:10571 ~normalized_only:0 ~gas_generatable:5
    ~promoted_support:4 ~blocked:10562

(* Export and round-trip deterministic normalized JSONL: every
   form Isa_norm_riscv/Isa_norm_xed produce from the real checked-in exports
   - not just synthetic values, which Test_isa_norm_jsonl already covers for
   every constructor - must survive Isa_norm_jsonl.encode_line followed by
   decode_line unchanged. The pinned total is the sum of the accounting
   tests' own pinned normalized counts (36+47+9+9); a drop here without a matching drop
   there would mean the codec silently lost a form the accounting still
   credits as normalized (67+81+9+9). *)
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
    (Printf.sprintf "isa-norm-jsonl: %d real normalized forms round-tripped (expected 1284)"
       !roundtrip_count)
    (!roundtrip_count = 1284)

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
  test_isa_norm_jsonl_roundtrip repo;
  test_isa_source_snapshot_diff repo;
  test_gen_pilot_manifest repo;
  test_gen_difficult_manifest repo;
  if !failures > 0 then (
    Printf.printf "repo_tests: %d failures\n" !failures;
    exit 1)
  else print_endline "repo_tests: all checks passed"
