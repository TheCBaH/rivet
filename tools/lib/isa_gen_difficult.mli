(** Non-frozen, growable corpus of difficult-form cases beyond the frozen
    S3 pilot manifest ({!Isa_gen_pilot}, closed): RISC-V
    split-immediate/branch/compressed cases, plus bounded x86 addressing and
    x87 cases.

    [sw] and [beq] are both real, long-implemented base RV32I/RV64I forms
    (unlike the pilot, this is not a claim about NEWLY implemented support);
    what this corpus adds is GENERATED differential evidence for their
    split/permuted immediates and label-resolved branch offsets.
    Unlike {!Isa_gen_pilot}, this manifest is expected to GROW as more
    difficult families are admitted - it deliberately reuses
    {!Isa_generated_case}/{!Isa_generated_corpus}/{!Isa_gen_oracle}/
    {!Isa_gen_ours}/{!Isa_gen_verdict}'s generic machinery rather than
    inventing a second schema, and is persisted to its OWN corpus file
    ([asm/fixtures/isa-difficult/cases.jsonl]) so growing it can never
    silently touch the frozen 21-entry pilot corpus. *)

type entry = {
  form_id : string;  (** must equal what {!Isa_norm_riscv} actually produces today *)
  target : Target.t;
  lookup_key : string;  (** the checked-in riscv-opcodes export's [native_name], e.g. ["sw"] *)
  case_id : string;
  rule_ids : string list;  (** which coverage obligation(s) this case satisfies *)
  operands : (string * string) list;
  lines_before : string list;
      (** GAS source lines emitted before the rendered instruction line, e.g. a
          backward branch's target label *)
  lines_after : string list;
      (** GAS source lines emitted after the rendered instruction line, e.g. a
          forward branch's filler instruction and target label *)
  configuration : string list;
      (** the resolved argv fragment for THIS entry - not always
          {!Isa_gen_case_build.configuration_for}'s per-target default:
          [c_addi_entries] needs [-march=rv32imc]/[-march=rv64imc] (the [c]
          extension), which [sw]/[beq]'s base-ISA [-march=rv32im]/
          [-march=rv64im] does not include and [c.addi] cannot assemble
          without. *)
}

val fixture_dir_name : string
(** ["isa-difficult"] under [asm/fixtures/] - this corpus's OWN tree, never
    {!Isa_generated_case.fixture_dir_name}'s ["isa-generated"] (the
    frozen pilot) nor gas-xref's. *)

val cli_group_name : string
(** ["isa-difficult"], the [compcert_tools.exe isa-difficult check|regen]
    subcommand group, mirroring {!Isa_generated_case.cli_group_name}'s
    ["isa-generated"]. *)

val check_make_target : string
(** ["asm-isa-difficult-check"], following the repository's existing
    [asm-<thing>-check] convention (compare
    {!Isa_generated_case.make_target}'s ["asm-isa-generated-check"]).
    Toolchain-free; joins [asm-test]'s prerequisites. *)

val regen_make_target : string
(** ["asm-isa-difficult-regen"] - needs the real cross toolchains and this
    project's own built assembler; deliberately kept off the [asm-test]/
    [asm-ci] critical path, like {!Isa_generated_case.make_target}'s
    ["asm-isa-generated-regen"]. *)

val sw_entries : entry list
(** [sw]'s signed 12-bit S-type offset domain (zero when legal,
    positive/negative interior values, endpoints): offset 0, a small
    positive interior value, the positive endpoint 2047, and the negative
    endpoint -2048 - each on both RV32 and RV64. Every case is a single
    relocation-free instruction line, exactly like the pilot cases -
    [sw]'s three explicit register/immediate operands need no label. *)

val c_addi_entries : entry list
(** [c.addi]'s signed 6-bit NONZERO immediate domain ("zero when
    legal" does not apply here - zero is architecturally reserved, not a
    boundary case; see {!Isa_norm_riscv.c_addi_form}'s own comment): the
    smallest positive (1) and smallest negative (-1) legal magnitudes, plus
    the positive endpoint 31 and the negative endpoint -32 - each on both
    RV32 and RV64. Every entry brackets the instruction in [.option
    rvc]/[.option norvc] ({!lines_before}/{!lines_after}) exactly like the
    mixed-stream regression in [test/targets/test_targets.ml], and
    overrides {!entry.configuration} to include the [c] extension. This is
    what promotes [c.addi] out of {!Isa_family_admission}'s
    [normalized-only] state into a real committed differential case - a
    persisted differential corpus, not a direct probe, is what grants
    support credit. *)

val beq_entries : entry list
(** [beq]'s label-resolved branch offset ("GAS resolves offset from a
    label" - {!Isa_norm_riscv.beq_form}'s own diagnostic names this exact
    gap). One forward branch (["1f"], skipping one [nop]) and one backward
    branch (["1b"], looping back over one [nop]), each on both RV32 and
    RV64 - the positive/negative legal label/offset domain. Both resolve
    locally within one assembled unit, so neither needs a linker or an
    external symbol (begin with relocation-free instructions).

    Each beq case's rendered source is TWO instructions (the branch plus its
    [nop] filler), not one - unlike every [sw] case and the whole pilot.
    {!Isa_gen_oracle.observed_form_check}'s single-instruction contract
    ([width_bits / 8] bytes expected) is therefore never satisfiable here: the
    real, measured {!Isa_gen_oracle.outcome} is always
    [Assembled_mismatched {detail = "expected 4 assembled bytes ... got 8"}],
    reported as [DIFFERENT-FORM] and persisted as
    {!Isa_generated_corpus.Different_observed_form} - a TRUE statement (the
    whole [.text] section genuinely is not one instruction's width), not a
    defect. The credit-bearing comparison is unaffected: {!Isa_gen_verdict}
    compares GAS's and "ours"' raw bytes directly across the WHOLE rendered
    source, and all four beq cases measure byte-for-byte identical, i.e.
    {!Isa_generated_case.Pass}. *)

val x86_mov_entries : entry list
(** Two explicit-32-bit [movl] address forms in each x86 execution mode: a
    base+disp8 SIB load and an indexed scale-4 disp32 store.  These are a
    bounded slice of XED's variable-width GPRv/MEMv forms. *)

val x86_fadd_entries : entry list
(** [fadd %st(1), %st] on x86-32 and x86-64, selecting XED's
    [FADD_ST0_X87] rather than its reverse-direction sibling. *)

val fadd_s_entries : entry list
(** Bare [fadd.s]/[fsub.s]/[fmul.s]/[fdiv.s] and [fadd.d]/[fsub.d]/
    [fmul.d]/[fdiv.d] with [ft0], [ft1], [ft2] on RV32IMF(D) and RV64IMF(D).
    Their omitted rounding mode is pinned by persisted
    GNU evidence to dynamic (funct3 7); explicit rounding-mode spellings are
    outside this bounded slice. *)

val fsub_s_entries : entry list
val fmul_s_entries : entry list
val fdiv_s_entries : entry list
val fadd_d_entries : entry list
val fsub_d_entries : entry list
val fmul_d_entries : entry list
val fdiv_d_entries : entry list

val flw_entries : entry list
(** [flw fa0, 8(a1)] on both profiles - F's floating-point load, already
    fully implemented by the encoder's shared [f_load_desc] path; this
    closes only the normalization/corpus/admission side. *)

val fld_entries : entry list
(** [fld fa0, 8(a1)] on both profiles, {!flw_entries}'s D (double-precision)
    sibling. *)

val fsw_entries : entry list
(** [fsw fa0, 8(a1)] on both profiles - F's floating-point store, already
    fully implemented by the encoder's shared [f_store_desc] path. *)

val fsd_entries : entry list
(** [fsd fa0, 8(a1)] on both profiles, {!fsw_entries}'s D (double-precision)
    sibling. *)

val fsgnj_s_entries : entry list
(** [fsgnj.s ft0, ft1, ft2] on both profiles - the general three-distinct-FP-
    register sign-injection form the encoder's [f_sgnj3_desc] closes; its
    [rs1 = rs2] alias siblings [fneg.s]/[fneg.d]/[fmv.d] predate this
    pass. *)

val fsgnjn_s_entries : entry list
(** {!fsgnj_s_entries}'s negated-sign sibling. *)

val fsgnjx_s_entries : entry list
(** {!fsgnj_s_entries}'s XOR-sign sibling. *)

val fsgnj_d_entries : entry list
(** {!fsgnj_s_entries}'s D (double-precision) sibling. *)

val fsgnjn_d_entries : entry list
(** {!fsgnjn_s_entries}'s D (double-precision) sibling. *)

val fsgnjx_d_entries : entry list
(** {!fsgnjx_s_entries}'s D (double-precision) sibling. *)

val fmin_s_entries : entry list
(** [fmin.s ft0, ft1, ft2] on both profiles - the same three-distinct-FP-
    register shape as {!fsgnj_s_entries}, but no pseudo-alias shares this
    word to distinguish from. *)

val fmax_s_entries : entry list
(** {!fmin_s_entries}'s max sibling. *)

val fmin_d_entries : entry list
(** {!fmin_s_entries}'s D (double-precision) sibling. *)

val fmax_d_entries : entry list
(** {!fmax_s_entries}'s D (double-precision) sibling. *)

val fsqrt_s_entries : entry list
(** [fsqrt.s ft0, ft1] on both profiles - {!fadd_s_entries}'s own shape
    minus the third operand ([rs2] is a fixed selector, not a real
    register). *)

val fsqrt_d_entries : entry list
(** {!fsqrt_s_entries}'s D (double-precision) sibling. *)

val fclass_s_entries : entry list
(** [fclass.s a0, ft1] on both profiles - [rd] is a GPR (the
    classification bitmask), [rs1] FP. *)

val fclass_d_entries : entry list
(** {!fclass_s_entries}'s D (double-precision) sibling. *)

val fmadd_s_entries : entry list
(** [fmadd.s ft0, ft1, ft2, ft3] on both profiles - RISC-V's only R4-type
    mnemonics, {!fsqrt_s_entries}'s implicit-dynamic-rounding shape with two
    more distinct FP register operands. *)

val fmsub_s_entries : entry list
(** {!fmadd_s_entries}'s subtract-the-addend sibling. *)

val fnmsub_s_entries : entry list
(** {!fmadd_s_entries}'s negate-the-product sibling. *)

val fnmadd_s_entries : entry list
(** {!fmadd_s_entries}'s negate-the-sum sibling. *)

val fmadd_d_entries : entry list
(** {!fmadd_s_entries}'s D (double-precision) sibling. *)

val fmsub_d_entries : entry list
(** {!fmsub_s_entries}'s D (double-precision) sibling. *)

val fnmsub_d_entries : entry list
(** {!fnmsub_s_entries}'s D (double-precision) sibling. *)

val fnmadd_d_entries : entry list
(** {!fnmadd_s_entries}'s D (double-precision) sibling. *)

val feq_s_entries : entry list
(** [feq.s a0, ft1, ft2] on both profiles - [rd] is a GPR (the boolean
    comparison result), [rs1]/[rs2] FP. *)

val fle_s_entries : entry list
(** {!feq_s_entries}'s less-or-equal sibling. *)

val flt_s_entries : entry list
(** {!feq_s_entries}'s less-than sibling. *)

val feq_d_entries : entry list
(** {!feq_s_entries}'s D (double-precision) sibling. *)

val fle_d_entries : entry list
(** {!fle_s_entries}'s D (double-precision) sibling. *)

val flt_d_entries : entry list
(** {!flt_s_entries}'s D (double-precision) sibling. *)

val fmv_x_w_entries : entry list
(** [fmv.x.w a0, ft1] on both profiles - bit-for-bit move, not a conversion,
    [rd] a GPR, [rs1] FP. *)

val fmv_w_x_entries : entry list
(** [fmv.w.x ft0, a1] on both profiles - {!fmv_x_w_entries}'s
    reverse-direction sibling, [rd] FP, [rs1] a GPR. *)

val fcvt_w_s_entries : entry list
(** [fcvt.w.s a0, ft1] on both profiles - {!fsqrt_s_entries}'s own
    implicit-dynamic-rounding shape with [rd] a GPR instead of FP. *)

val fcvt_wu_s_entries : entry list
(** {!fcvt_w_s_entries}'s unsigned sibling. *)

val fcvt_s_w_entries : entry list
(** [fcvt.s.w ft0, a1] on both profiles - {!fcvt_w_s_entries}'s
    reverse-direction sibling, [rd] FP, [rs1] a GPR. *)

val fcvt_s_wu_entries : entry list
(** {!fcvt_s_w_entries}'s unsigned sibling. *)

val fcvt_w_d_entries : entry list
(** [fcvt.w.d a0, ft1] on both profiles - {!fcvt_w_s_entries}'s own shape
    and dynamic-rounding default, D-extension configuration. *)

val fcvt_wu_d_entries : entry list
(** {!fcvt_w_d_entries}'s unsigned sibling. *)

val fcvt_d_w_entries : entry list
(** [fcvt.d.w ft0, a1] on both profiles - {!fcvt_s_w_entries}'s own shape,
    but real hardware's always-exact rne default rather than dynamic
    rounding. *)

val fcvt_d_wu_entries : entry list
(** {!fcvt_d_w_entries}'s unsigned sibling. *)

val fcvt_s_d_entries : entry list
(** [fcvt.s.d ft0, ft1] on both profiles - float-to-float precision
    convert, no GPR involved, dynamic-rounding default (narrowing). *)

val fcvt_d_s_entries : entry list
(** {!fcvt_s_d_entries}'s reverse direction (widening) - always-exact rne
    default instead. *)

val fcvt_l_d_entries : entry list
(** [fcvt.l.d a0, ft1] on RV64 only (no RV32 counterpart) -
    {!fcvt_w_d_entries}'s own shape and configuration. *)

val fcvt_lu_d_entries : entry list
(** {!fcvt_l_d_entries}'s unsigned sibling. *)

val fcvt_l_s_entries : entry list
(** [fcvt.l.s a0, ft1] on RV64 only - {!fcvt_w_s_entries}'s own shape and F
    configuration. *)

val fcvt_lu_s_entries : entry list
(** {!fcvt_l_s_entries}'s unsigned sibling. *)

val fcvt_s_l_entries : entry list
(** [fcvt.s.l ft0, a1] on RV64 only - {!fcvt_s_w_entries}'s own shape and F
    configuration. *)

val fcvt_s_lu_entries : entry list
(** {!fcvt_s_l_entries}'s unsigned sibling. *)

val fcvt_d_l_entries : entry list
(** [fcvt.d.l ft0, a1] on RV64 only - {!fcvt_d_w_entries}'s own shape and D
    configuration, but the family's usual dynamic-rounding default rather
    than the always-exact one (a 64-bit long is not always exact in a
    double). *)

val fcvt_d_lu_entries : entry list
(** {!fcvt_d_l_entries}'s unsigned sibling. *)

val vsetvl_entries : entry list
(** [vsetvl a0, a1, a2] on RV32IV and RV64IV - V's register-register
    configuration-setting instruction, the cheapest entry point into the
    rv_v family: a plain three-GPR R-type shape with no vector register
    class and no import duplication. *)

val vsetvli_entries : entry list
(** [vsetvli a0, a1, e32, m1, ta, ma] on RV32IV and RV64IV - V's
    register-AVL immediate-vtype sibling of {!vsetvl_entries}; the
    "e<SEW>,m<LMUL>,ta|tu,ma|mu" vtype spelling is GAS's own keyword
    decomposition of a single 11-bit field. *)

val vsetivli_entries : entry list
(** [vsetivli a0, 5, e32, m1, ta, ma] on RV32IV and RV64IV -
    {!vsetvli_entries}'s immediate-AVL sibling. *)

val vadd_vv_entries : entry list
(** [vadd.vv v1, v2, v3] on RV32IV and RV64IV - the entry point into OP-V's
    real vector-register arithmetic space, all three operands vector
    registers. *)

val vadd_vx_entries : entry list
(** [vadd.vx v1, v2, a0] - {!vadd_vv_entries}'s scalar-broadcast sibling,
    [rs1] a real GPR. *)

val vadd_vi_entries : entry list
(** [vadd.vi v1, v2, -5] - {!vadd_vv_entries}'s immediate sibling, a real
    5-bit signed immediate. *)

val vsub_vv_entries : entry list
(** [vsub.vv v1, v2, v3] on RV32IV and RV64IV - {!vadd_vv_entries}'s
    subtract sibling; has no [.vi] form (neither riscv-opcodes nor real GNU
    as define one - subtract-by-immediate is covered by [vrsub.vi]). *)

val vsub_vx_entries : entry list
(** [vsub.vx v1, v2, a0] - {!vsub_vv_entries}'s scalar-broadcast sibling. *)

val vrsub_vx_entries : entry list
(** [vrsub.vx v1, v2, a0] on RV32IV and RV64IV - the reverse-subtract
    ([vd = rs1 - vs2]) sibling; has no [.vv] form (riscv-opcodes exports
    none, and real GNU as rejects [vrsub.vv] as an unrecognized opcode). *)

val vrsub_vi_entries : entry list
(** [vrsub.vi v1, v2, -5] - {!vrsub_vx_entries}'s immediate sibling, a real
    5-bit signed immediate. *)

val vand_vv_entries : entry list
(** [vand.vv v1, v2, v3] on RV32IV and RV64IV - vector bitwise AND, the
    first of three full [.vv]/[.vx]/[.vi] logical siblings. *)

val vand_vx_entries : entry list
(** [vand.vx v1, v2, a0] - {!vand_vv_entries}'s scalar-broadcast sibling. *)

val vand_vi_entries : entry list
(** [vand.vi v1, v2, -5] - {!vand_vv_entries}'s immediate sibling. *)

val vor_vv_entries : entry list
(** [vor.vv v1, v2, v3] on RV32IV and RV64IV - vector bitwise OR. *)

val vor_vx_entries : entry list
(** [vor.vx v1, v2, a0] - {!vor_vv_entries}'s scalar-broadcast sibling. *)

val vor_vi_entries : entry list
(** [vor.vi v1, v2, -5] - {!vor_vv_entries}'s immediate sibling. *)

val vxor_vv_entries : entry list
(** [vxor.vv v1, v2, v3] on RV32IV and RV64IV - vector bitwise XOR. *)

val vxor_vx_entries : entry list
(** [vxor.vx v1, v2, a0] - {!vxor_vv_entries}'s scalar-broadcast sibling. *)

val vxor_vi_entries : entry list
(** [vxor.vi v1, v2, -5] - {!vxor_vv_entries}'s immediate sibling. *)

val vsll_vv_entries : entry list
(** [vsll.vv v1, v2, v3] on RV32IV and RV64IV - OP-V's shift-left family,
    the first to admit its full [.vi] sibling with an UNSIGNED immediate
    (see {!vsll_vi_entries}). *)

val vsll_vx_entries : entry list
(** [vsll.vx v1, v2, a0] - {!vsll_vv_entries}'s scalar-broadcast sibling. *)

val vsll_vi_entries : entry list
(** [vsll.vi v1, v2, 31] - {!vsll_vv_entries}'s immediate sibling: a real
    UNSIGNED 5-bit shift amount (0..31, riscv-opcodes' own "zimm5" field),
    unlike every other admitted [.vi] mnemonic's SIGNED [simm5]. *)

val vsrl_vv_entries : entry list
(** [vsrl.vv v1, v2, v3] on RV32IV and RV64IV - {!vsll_vv_entries}'s
    logical-shift-right sibling. *)

val vsrl_vx_entries : entry list
(** [vsrl.vx v1, v2, a0] - {!vsrl_vv_entries}'s scalar-broadcast sibling. *)

val vsrl_vi_entries : entry list
(** [vsrl.vi v1, v2, 5] - {!vsrl_vv_entries}'s UNSIGNED-immediate sibling. *)

val vsra_vv_entries : entry list
(** [vsra.vv v1, v2, v3] on RV32IV and RV64IV - {!vsll_vv_entries}'s
    arithmetic-shift-right sibling. *)

val vsra_vx_entries : entry list
(** [vsra.vx v1, v2, a0] - {!vsra_vv_entries}'s scalar-broadcast sibling. *)

val vsra_vi_entries : entry list
(** [vsra.vi v1, v2, 5] - {!vsra_vv_entries}'s UNSIGNED-immediate sibling. *)

val vminu_vv_entries : entry list
(** [vminu.vv v1, v2, v3] on RV32IV and RV64IV - OP-V's min/max family;
    [.vv]/[.vx] only, no [.vi] sibling (neither riscv-opcodes nor real GNU
    as define one). *)

val vminu_vx_entries : entry list
(** [vminu.vx v1, v2, a0] - {!vminu_vv_entries}'s scalar-broadcast sibling. *)

val vmin_vv_entries : entry list
(** [vmin.vv v1, v2, v3] on RV32IV and RV64IV - {!vminu_vv_entries}'s
    signed sibling. *)

val vmin_vx_entries : entry list
(** [vmin.vx v1, v2, a0] - {!vmin_vv_entries}'s scalar-broadcast sibling. *)

val vmaxu_vv_entries : entry list
(** [vmaxu.vv v1, v2, v3] on RV32IV and RV64IV - {!vminu_vv_entries}'s
    max sibling. *)

val vmaxu_vx_entries : entry list
(** [vmaxu.vx v1, v2, a0] - {!vmaxu_vv_entries}'s scalar-broadcast sibling. *)

val vmax_vv_entries : entry list
(** [vmax.vv v1, v2, v3] on RV32IV and RV64IV - {!vmin_vv_entries}'s
    max sibling. *)

val vmax_vx_entries : entry list
(** [vmax.vx v1, v2, a0] - {!vmax_vv_entries}'s scalar-broadcast sibling. *)

val vmul_vv_entries : entry list
(** [vmul.vv v1, v2, v3] on RV32IV and RV64IV - OP-V's second major
    functional-unit group, OPMVV (funct3 = 2); the identical
    all-vector-register operand layout as OPIVV, only funct3 differs. No
    [.vi] sibling. *)

val vmul_vx_entries : entry list
(** [vmul.vx v1, v2, a0] - {!vmul_vv_entries}'s OPMVX (funct3 = 6)
    scalar-broadcast sibling. *)

val vmulh_vv_entries : entry list
(** [vmulh.vv v1, v2, v3] on RV32IV and RV64IV - {!vmul_vv_entries}'s
    high-half signed*signed sibling. *)

val vmulh_vx_entries : entry list
(** [vmulh.vx v1, v2, a0] - {!vmulh_vv_entries}'s scalar-broadcast sibling. *)

val vmulhu_vv_entries : entry list
(** [vmulhu.vv v1, v2, v3] on RV32IV and RV64IV - {!vmul_vv_entries}'s
    high-half unsigned*unsigned sibling. *)

val vmulhu_vx_entries : entry list
(** [vmulhu.vx v1, v2, a0] - {!vmulhu_vv_entries}'s scalar-broadcast
    sibling. *)

val vmulhsu_vv_entries : entry list
(** [vmulhsu.vv v1, v2, v3] on RV32IV and RV64IV - {!vmul_vv_entries}'s
    high-half signed*unsigned sibling. *)

val vmulhsu_vx_entries : entry list
(** [vmulhsu.vx v1, v2, a0] - {!vmulhsu_vv_entries}'s scalar-broadcast
    sibling. *)

val vdivu_vv_entries : entry list
(** [vdivu.vv v1, v2, v3] on RV32IV and RV64IV - OP-V's divide/remainder
    family, the same OPMVV shape as {!vmul_vv_entries}. No [.vi] sibling. *)

val vdivu_vx_entries : entry list
(** [vdivu.vx v1, v2, a0] - {!vdivu_vv_entries}'s scalar-broadcast sibling. *)

val vdiv_vv_entries : entry list
(** [vdiv.vv v1, v2, v3] on RV32IV and RV64IV - {!vdivu_vv_entries}'s signed
    sibling. *)

val vdiv_vx_entries : entry list
(** [vdiv.vx v1, v2, a0] - {!vdiv_vv_entries}'s scalar-broadcast sibling. *)

val vremu_vv_entries : entry list
(** [vremu.vv v1, v2, v3] on RV32IV and RV64IV - {!vdivu_vv_entries}'s
    remainder sibling. *)

val vremu_vx_entries : entry list
(** [vremu.vx v1, v2, a0] - {!vremu_vv_entries}'s scalar-broadcast sibling. *)

val vrem_vv_entries : entry list
(** [vrem.vv v1, v2, v3] on RV32IV and RV64IV - {!vremu_vv_entries}'s signed
    sibling. *)

val vrem_vx_entries : entry list
(** [vrem.vx v1, v2, a0] - {!vrem_vv_entries}'s scalar-broadcast sibling. *)

val vsaddu_vv_entries : entry list
(** [vsaddu.vv v1, v2, v3] on RV32IV and RV64IV - OP-V's saturating
    add/subtract family, the same OPIVV shape as {!vadd_vv_entries}. *)

val vsaddu_vx_entries : entry list
(** [vsaddu.vx v1, v2, a0] - {!vsaddu_vv_entries}'s scalar-broadcast
    sibling. *)

val vsaddu_vi_entries : entry list
(** [vsaddu.vi v1, v2, -5] - {!vsaddu_vv_entries}'s SIGNED-immediate
    sibling. *)

val vsadd_vv_entries : entry list
(** [vsadd.vv v1, v2, v3] on RV32IV and RV64IV - {!vsaddu_vv_entries}'s
    signed sibling. *)

val vsadd_vx_entries : entry list
(** [vsadd.vx v1, v2, a0] - {!vsadd_vv_entries}'s scalar-broadcast sibling. *)

val vsadd_vi_entries : entry list
(** [vsadd.vi v1, v2, -5] - {!vsadd_vv_entries}'s SIGNED-immediate sibling. *)

val vssubu_vv_entries : entry list
(** [vssubu.vv v1, v2, v3] on RV32IV and RV64IV - {!vsaddu_vv_entries}'s
    subtract sibling. No [.vi] sibling. *)

val vssubu_vx_entries : entry list
(** [vssubu.vx v1, v2, a0] - {!vssubu_vv_entries}'s scalar-broadcast
    sibling. *)

val vssub_vv_entries : entry list
(** [vssub.vv v1, v2, v3] on RV32IV and RV64IV - {!vssubu_vv_entries}'s
    signed sibling. No [.vi] sibling. *)

val vssub_vx_entries : entry list
(** [vssub.vx v1, v2, a0] - {!vssub_vv_entries}'s scalar-broadcast sibling. *)

val vaaddu_vv_entries : entry list
(** [vaaddu.vv v1, v2, v3] on RV32IV and RV64IV - OP-V's averaging
    add/subtract family, the same OPMVV shape as {!vmul_vv_entries}. No
    [.vi] sibling. *)

val vaaddu_vx_entries : entry list
(** [vaaddu.vx v1, v2, a0] - {!vaaddu_vv_entries}'s scalar-broadcast
    sibling. *)

val vaadd_vv_entries : entry list
(** [vaadd.vv v1, v2, v3] on RV32IV and RV64IV - {!vaaddu_vv_entries}'s
    signed sibling. *)

val vaadd_vx_entries : entry list
(** [vaadd.vx v1, v2, a0] - {!vaadd_vv_entries}'s scalar-broadcast sibling. *)

val vasubu_vv_entries : entry list
(** [vasubu.vv v1, v2, v3] on RV32IV and RV64IV - {!vaaddu_vv_entries}'s
    subtract sibling. *)

val vasubu_vx_entries : entry list
(** [vasubu.vx v1, v2, a0] - {!vasubu_vv_entries}'s scalar-broadcast
    sibling. *)

val vasub_vv_entries : entry list
(** [vasub.vv v1, v2, v3] on RV32IV and RV64IV - {!vasubu_vv_entries}'s
    signed sibling. *)

val vasub_vx_entries : entry list
(** [vasub.vx v1, v2, a0] - {!vasub_vv_entries}'s scalar-broadcast sibling. *)

val vnsrl_wv_entries : entry list
(** [vnsrl.wv v1, v2, v3] on RV32IV and RV64IV - OP-V's narrowing
    shift/clip family ([.wv]/[.wx]/[.wi], vs2 semantically wide), the same
    OPIVV shape as {!vadd_vv_entries}. *)

val vnsrl_wx_entries : entry list
(** [vnsrl.wx v1, v2, a0] - {!vnsrl_wv_entries}'s scalar-broadcast sibling. *)

val vnsrl_wi_entries : entry list
(** [vnsrl.wi v1, v2, 31] - {!vnsrl_wv_entries}'s UNSIGNED-immediate
    sibling. *)

val vnsra_wv_entries : entry list
(** [vnsra.wv v1, v2, v3] on RV32IV and RV64IV - {!vnsrl_wv_entries}'s
    arithmetic-shift sibling. *)

val vnsra_wx_entries : entry list
(** [vnsra.wx v1, v2, a0] - {!vnsra_wv_entries}'s scalar-broadcast sibling. *)

val vnsra_wi_entries : entry list
(** [vnsra.wi v1, v2, 31] - {!vnsra_wv_entries}'s UNSIGNED-immediate
    sibling. *)

val vnclipu_wv_entries : entry list
(** [vnclipu.wv v1, v2, v3] on RV32IV and RV64IV - {!vnsrl_wv_entries}'s
    clip-to-unsigned sibling. *)

val vnclipu_wx_entries : entry list
(** [vnclipu.wx v1, v2, a0] - {!vnclipu_wv_entries}'s scalar-broadcast
    sibling. *)

val vnclipu_wi_entries : entry list
(** [vnclipu.wi v1, v2, 31] - {!vnclipu_wv_entries}'s UNSIGNED-immediate
    sibling. *)

val vnclip_wv_entries : entry list
(** [vnclip.wv v1, v2, v3] on RV32IV and RV64IV - {!vnclipu_wv_entries}'s
    signed sibling. *)

val vnclip_wx_entries : entry list
(** [vnclip.wx v1, v2, a0] - {!vnclip_wv_entries}'s scalar-broadcast
    sibling. *)

val vnclip_wi_entries : entry list
(** [vnclip.wi v1, v2, 31] - {!vnclip_wv_entries}'s UNSIGNED-immediate
    sibling. *)

val vssrl_vv_entries : entry list
(** [vssrl.vv v1, v2, v3] on RV32IV and RV64IV - OP-V's scaling
    shift-right family (logical/arithmetic, rounding), the same full
    OPIVV/OPIVX/OPIVI shape as {!vsll_vv_entries} including the UNSIGNED
    [zimm5] [.vi] immediate. *)

val vssrl_vx_entries : entry list
(** [vssrl.vx v1, v2, a0] - {!vssrl_vv_entries}'s scalar-broadcast sibling. *)

val vssrl_vi_entries : entry list
(** [vssrl.vi v1, v2, 31] - {!vssrl_vv_entries}'s UNSIGNED-immediate
    sibling. *)

val vssra_vv_entries : entry list
(** [vssra.vv v1, v2, v3] on RV32IV and RV64IV - {!vssrl_vv_entries}'s
    arithmetic-shift sibling. *)

val vssra_vx_entries : entry list
(** [vssra.vx v1, v2, a0] - {!vssra_vv_entries}'s scalar-broadcast sibling. *)

val vssra_vi_entries : entry list
(** [vssra.vi v1, v2, 31] - {!vssra_vv_entries}'s UNSIGNED-immediate
    sibling. *)

val vrgather_vv_entries : entry list
(** [vrgather.vv v1, v2, v3] on RV32IV and RV64IV - OP-V's full-vector-
    register gather/permute family, the same OPIVV shape as
    {!vadd_vv_entries}. *)

val vrgather_vx_entries : entry list
(** [vrgather.vx v1, v2, a0] - {!vrgather_vv_entries}'s scalar-broadcast
    sibling. *)

val vrgather_vi_entries : entry list
(** [vrgather.vi v1, v2, 31] - {!vrgather_vv_entries}'s UNSIGNED-immediate
    sibling. *)

val vrgatherei16_vv_entries : entry list
(** [vrgatherei16.vv v1, v2, v3] - {!vrgather_vv_entries}'s fixed-EEW16-
    index sibling; no [.vx]/[.vi] siblings exist for it. *)

val vwaddu_vv_entries : entry list
(** [vwaddu.vv v1, v2, v3] on RV32IV and RV64IV - OP-V's widening
    add/subtract family (both operands narrow), the same OPMVV shape as
    {!vmul_vv_entries}. No [.vi] sibling. *)

val vwaddu_vx_entries : entry list
(** [vwaddu.vx v1, v2, a0] - {!vwaddu_vv_entries}'s scalar-broadcast
    sibling. *)

val vwadd_vv_entries : entry list
(** [vwadd.vv v1, v2, v3] on RV32IV and RV64IV - {!vwaddu_vv_entries}'s
    signed sibling. *)

val vwadd_vx_entries : entry list
(** [vwadd.vx v1, v2, a0] - {!vwadd_vv_entries}'s scalar-broadcast sibling. *)

val vwsubu_vv_entries : entry list
(** [vwsubu.vv v1, v2, v3] on RV32IV and RV64IV - {!vwaddu_vv_entries}'s
    subtract sibling. *)

val vwsubu_vx_entries : entry list
(** [vwsubu.vx v1, v2, a0] - {!vwsubu_vv_entries}'s scalar-broadcast
    sibling. *)

val vwsub_vv_entries : entry list
(** [vwsub.vv v1, v2, v3] on RV32IV and RV64IV - {!vwsubu_vv_entries}'s
    signed sibling. *)

val vwsub_vx_entries : entry list
(** [vwsub.vx v1, v2, a0] - {!vwsub_vv_entries}'s scalar-broadcast sibling. *)

val vwaddu_wv_entries : entry list
(** [vwaddu.wv v1, v2, v3] on RV32IV and RV64IV - {!vwaddu_vv_entries}'s
    wide-[vs2]-operand sibling. *)

val vwaddu_wx_entries : entry list
(** [vwaddu.wx v1, v2, a0] - {!vwaddu_wv_entries}'s scalar-broadcast
    sibling. *)

val vwadd_wv_entries : entry list
(** [vwadd.wv v1, v2, v3] on RV32IV and RV64IV - {!vwaddu_wv_entries}'s
    signed sibling. *)

val vwadd_wx_entries : entry list
(** [vwadd.wx v1, v2, a0] - {!vwadd_wv_entries}'s scalar-broadcast sibling. *)

val vwsubu_wv_entries : entry list
(** [vwsubu.wv v1, v2, v3] on RV32IV and RV64IV - {!vwaddu_wv_entries}'s
    subtract sibling. *)

val vwsubu_wx_entries : entry list
(** [vwsubu.wx v1, v2, a0] - {!vwsubu_wv_entries}'s scalar-broadcast
    sibling. *)

val vwsub_wv_entries : entry list
(** [vwsub.wv v1, v2, v3] on RV32IV and RV64IV - {!vwsubu_wv_entries}'s
    signed sibling. *)

val vwsub_wx_entries : entry list
(** [vwsub.wx v1, v2, a0] - {!vwsub_wv_entries}'s scalar-broadcast sibling. *)

val vwmulu_vv_entries : entry list
(** [vwmulu.vv v1, v2, v3] on RV32IV and RV64IV - OP-V's widening multiply
    family, the same OPMVV shape as {!vwaddu_vv_entries}. No [.vi]
    sibling. *)

val vwmulu_vx_entries : entry list
(** [vwmulu.vx v1, v2, a0] - {!vwmulu_vv_entries}'s scalar-broadcast
    sibling. *)

val vwmulsu_vv_entries : entry list
(** [vwmulsu.vv v1, v2, v3] on RV32IV and RV64IV - {!vwmulu_vv_entries}'s
    signed*unsigned sibling. *)

val vwmulsu_vx_entries : entry list
(** [vwmulsu.vx v1, v2, a0] - {!vwmulsu_vv_entries}'s scalar-broadcast
    sibling. *)

val vwmul_vv_entries : entry list
(** [vwmul.vv v1, v2, v3] on RV32IV and RV64IV - {!vwmulu_vv_entries}'s
    signed sibling. *)

val vwmul_vx_entries : entry list
(** [vwmul.vx v1, v2, a0] - {!vwmul_vv_entries}'s scalar-broadcast sibling. *)

val vsext_vf2_entries : entry list
(** [vsext.vf2 v1, v2] on RV32IV and RV64IV - OP-V's integer sign-extend
    family, a genuinely new two-vector-register shape ([rd, rs2], no
    third operand). *)

val vsext_vf4_entries : entry list
(** [vsext.vf4 v1, v2] - {!vsext_vf2_entries}'s divide-by-4 sibling. *)

val vsext_vf8_entries : entry list
(** [vsext.vf8 v1, v2] - {!vsext_vf2_entries}'s divide-by-8 sibling. *)

val vzext_vf2_entries : entry list
(** [vzext.vf2 v1, v2] on RV32IV and RV64IV - {!vsext_vf2_entries}'s
    zero-extend sibling. *)

val vzext_vf4_entries : entry list
(** [vzext.vf4 v1, v2] - {!vzext_vf2_entries}'s divide-by-4 sibling. *)

val vzext_vf8_entries : entry list
(** [vzext.vf8 v1, v2] - {!vzext_vf2_entries}'s divide-by-8 sibling. *)

val vmand_mm_entries : entry list
(** [vmand.mm v1, v2, v3] on RV32IV and RV64IV - OP-V's mask-register
    logical family, the same [rd, rs2, rs1] shape as {!vsub_vv_entries}
    with no masked sibling (architecturally [vm] is fixed at 1). *)

val vmandn_mm_entries : entry list
(** [vmandn.mm v1, v2, v3] - {!vmand_mm_entries}'s complement-first
    sibling. *)

val vmor_mm_entries : entry list
(** [vmor.mm v1, v2, v3] - {!vmand_mm_entries}'s logical-or sibling. *)

val vmxor_mm_entries : entry list
(** [vmxor.mm v1, v2, v3] - {!vmand_mm_entries}'s logical-xor sibling. *)

val vmorn_mm_entries : entry list
(** [vmorn.mm v1, v2, v3] - {!vmand_mm_entries}'s complement-first
    logical-or sibling. *)

val vmnand_mm_entries : entry list
(** [vmnand.mm v1, v2, v3] - {!vmand_mm_entries}'s negated sibling. *)

val vmnor_mm_entries : entry list
(** [vmnor.mm v1, v2, v3] - {!vmor_mm_entries}'s negated sibling. *)

val vmxnor_mm_entries : entry list
(** [vmxnor.mm v1, v2, v3] - {!vmxor_mm_entries}'s negated sibling. *)

val vredsum_vs_entries : entry list
(** [vredsum.vs v1, v2, v3] on RV32IV and RV64IV - OP-V's vector-reduction
    family, the same [rd, rs2, rs1] shape as {!vsub_vv_entries} but with a
    real, selectable mask (unlike {!vmand_mm_entries}). *)

val vredand_vs_entries : entry list
(** [vredand.vs v1, v2, v3] - {!vredsum_vs_entries}'s bitwise-and sibling. *)

val vredor_vs_entries : entry list
(** [vredor.vs v1, v2, v3] - {!vredsum_vs_entries}'s bitwise-or sibling. *)

val vredxor_vs_entries : entry list
(** [vredxor.vs v1, v2, v3] - {!vredsum_vs_entries}'s bitwise-xor sibling. *)

val vredminu_vs_entries : entry list
(** [vredminu.vs v1, v2, v3] - {!vredsum_vs_entries}'s unsigned-minimum
    sibling. *)

val vredmin_vs_entries : entry list
(** [vredmin.vs v1, v2, v3] - {!vredsum_vs_entries}'s signed-minimum
    sibling. *)

val vredmaxu_vs_entries : entry list
(** [vredmaxu.vs v1, v2, v3] - {!vredsum_vs_entries}'s unsigned-maximum
    sibling. *)

val vredmax_vs_entries : entry list
(** [vredmax.vs v1, v2, v3] - {!vredsum_vs_entries}'s signed-maximum
    sibling. *)

val vwredsumu_vs_entries : entry list
(** [vwredsumu.vs v1, v2, v3] - {!vredsum_vs_entries}'s widening unsigned-sum
    sibling. *)

val vwredsum_vs_entries : entry list
(** [vwredsum.vs v1, v2, v3] - {!vredsum_vs_entries}'s widening signed-sum
    sibling. *)

val vmseq_vv_entries : entry list
(** [vmseq.vv v1, v2, v3] on RV32IV and RV64IV - OP-V's mask-writing
    equal-comparison family, the full OPIVV/OPIVX/OPIVI shape. *)

val vmseq_vx_entries : entry list
(** [vmseq.vx v1, v2, a0] - {!vmseq_vv_entries}'s scalar-broadcast
    sibling. *)

val vmseq_vi_entries : entry list
(** [vmseq.vi v1, v2, -5] - {!vmseq_vv_entries}'s signed-immediate
    sibling. *)

val vmsne_vv_entries : entry list
(** [vmsne.vv v1, v2, v3] - {!vmseq_vv_entries}'s not-equal sibling. *)

val vmsne_vx_entries : entry list
(** [vmsne.vx v1, v2, a0] - {!vmsne_vv_entries}'s scalar-broadcast
    sibling. *)

val vmsne_vi_entries : entry list
(** [vmsne.vi v1, v2, -5] - {!vmsne_vv_entries}'s signed-immediate
    sibling. *)

val vmsltu_vv_entries : entry list
(** [vmsltu.vv v1, v2, v3] - {!vmseq_vv_entries}'s unsigned
    strictly-less-than sibling; has no [.vi] sibling. *)

val vmsltu_vx_entries : entry list
(** [vmsltu.vx v1, v2, a0] - {!vmsltu_vv_entries}'s scalar-broadcast
    sibling. *)

val vmslt_vv_entries : entry list
(** [vmslt.vv v1, v2, v3] - {!vmsltu_vv_entries}'s signed sibling; has no
    [.vi] sibling. *)

val vmslt_vx_entries : entry list
(** [vmslt.vx v1, v2, a0] - {!vmslt_vv_entries}'s scalar-broadcast
    sibling. *)

val vmsleu_vv_entries : entry list
(** [vmsleu.vv v1, v2, v3] - {!vmseq_vv_entries}'s unsigned
    less-than-or-equal sibling. *)

val vmsleu_vx_entries : entry list
(** [vmsleu.vx v1, v2, a0] - {!vmsleu_vv_entries}'s scalar-broadcast
    sibling. *)

val vmsleu_vi_entries : entry list
(** [vmsleu.vi v1, v2, -5] - {!vmsleu_vv_entries}'s signed-immediate
    sibling. *)

val vmsle_vv_entries : entry list
(** [vmsle.vv v1, v2, v3] - {!vmsleu_vv_entries}'s signed sibling. *)

val vmsle_vx_entries : entry list
(** [vmsle.vx v1, v2, a0] - {!vmsle_vv_entries}'s scalar-broadcast
    sibling. *)

val vmsle_vi_entries : entry list
(** [vmsle.vi v1, v2, -5] - {!vmsle_vv_entries}'s signed-immediate
    sibling. *)

val vmsgtu_vx_entries : entry list
(** [vmsgtu.vx v1, v2, a0] - {!vmsleu_vv_entries}'s unsigned
    strictly-greater-than sibling; has no [.vv] sibling (real GNU as
    accepts [vmsgtu.vv] only as a pseudo-instruction reversing
    {!vmsltu_vv_entries}'s own operands). *)

val vmsgtu_vi_entries : entry list
(** [vmsgtu.vi v1, v2, -5] - {!vmsgtu_vx_entries}'s signed-immediate
    sibling. *)

val vmsgt_vx_entries : entry list
(** [vmsgt.vx v1, v2, a0] - {!vmsgtu_vx_entries}'s signed sibling; has no
    [.vv] sibling (real GNU as accepts [vmsgt.vv] only as a
    pseudo-instruction reversing {!vmslt_vv_entries}'s own operands). *)

val vmsgt_vi_entries : entry list
(** [vmsgt.vi v1, v2, -5] - {!vmsgt_vx_entries}'s signed-immediate
    sibling. *)

val vslideup_vx_entries : entry list
(** [vslideup.vx v1, v2, a0] on RV32IV and RV64IV - OP-V's slide family,
    the same OPIVX shape as {!vsub_vx_entries}; has no [.vv] sibling. *)

val vslideup_vi_entries : entry list
(** [vslideup.vi v1, v2, 31] - {!vslideup_vx_entries}'s
    UNSIGNED-immediate sibling. *)

val vslidedown_vx_entries : entry list
(** [vslidedown.vx v1, v2, a0] - {!vslideup_vx_entries}'s downward
    sibling. *)

val vslidedown_vi_entries : entry list
(** [vslidedown.vi v1, v2, 31] - {!vslidedown_vx_entries}'s
    UNSIGNED-immediate sibling. *)

val vslide1up_vx_entries : entry list
(** [vslide1up.vx v1, v2, a0] - {!vslideup_vx_entries}'s single-element
    OPMVX sibling; has no [.vi] sibling. *)

val vslide1down_vx_entries : entry list
(** [vslide1down.vx v1, v2, a0] - {!vslide1up_vx_entries}'s downward
    sibling. *)

val vmacc_vv_entries : entry list
(** [vmacc.vv v1, v2, v3] on RV32IV and RV64IV - the multiply-accumulate
    family's own reordered text operand order ([vd, vs1, vs2] rather than
    {!vsub_vv_entries}'s [vd, vs2, vs1]). *)

val vmacc_vx_entries : entry list
(** [vmacc.vx v1, a0, v3] - {!vmacc_vv_entries}'s scalar-broadcast sibling,
    also reordered ([vd, rs1, vs2]). *)

val vnmsac_vv_entries : entry list
(** [vnmsac.vv v1, v2, v3] - {!vmacc_vv_entries}'s negated-multiply
    sibling. *)

val vnmsac_vx_entries : entry list
(** [vnmsac.vx v1, a0, v3] - {!vnmsac_vv_entries}'s scalar-broadcast
    sibling. *)

val vmadd_vv_entries : entry list
(** [vmadd.vv v1, v2, v3] - the same reordered shape as {!vmacc_vv_entries},
    with the addend (rather than the product) held in [vd]. *)

val vmadd_vx_entries : entry list
(** [vmadd.vx v1, a0, v3] - {!vmadd_vv_entries}'s scalar-broadcast
    sibling. *)

val vnmsub_vv_entries : entry list
(** [vnmsub.vv v1, v2, v3] - {!vmadd_vv_entries}'s negated-multiply
    sibling. *)

val vnmsub_vx_entries : entry list
(** [vnmsub.vx v1, a0, v3] - {!vnmsub_vv_entries}'s scalar-broadcast
    sibling. *)

val vwmaccu_vv_entries : entry list
(** [vwmaccu.vv v1, v2, v3] - the widening (unsigned x unsigned)
    multiply-accumulate sibling of {!vmacc_vv_entries}, same reordered
    shape. *)

val vwmaccu_vx_entries : entry list
(** [vwmaccu.vx v1, a0, v3] - {!vwmaccu_vv_entries}'s scalar-broadcast
    sibling. *)

val vwmacc_vv_entries : entry list
(** [vwmacc.vv v1, v2, v3] - the widening (signed x signed)
    multiply-accumulate sibling of {!vmacc_vv_entries}. *)

val vwmacc_vx_entries : entry list
(** [vwmacc.vx v1, a0, v3] - {!vwmacc_vv_entries}'s scalar-broadcast
    sibling. *)

val vwmaccsu_vv_entries : entry list
(** [vwmaccsu.vv v1, v2, v3] - the widening (signed x unsigned)
    multiply-accumulate sibling of {!vmacc_vv_entries}. *)

val vwmaccsu_vx_entries : entry list
(** [vwmaccsu.vx v1, a0, v3] - {!vwmaccsu_vv_entries}'s scalar-broadcast
    sibling. *)

val vwmaccus_vx_entries : entry list
(** [vwmaccus.vx v1, a0, v3] - the widening (unsigned x signed)
    multiply-accumulate mnemonic; has no [.vv] sibling (real GNU as rejects
    [vwmaccus.vv] as "unrecognized opcode"). *)

val vid_v_entries : entry list
(** [vid.v v1] on RV32IV and RV64IV - OP-V's element-index instruction, the
    first family with no [vs2]/[vs1]/[rs1] operand at all, just a
    destination. *)

val viota_m_entries : entry list
(** [viota.m v1, v2] - shares {!vsext_vf2_entries}'s exact [rd, rs2]
    shape. *)

val vcompress_vm_entries : entry list
(** [vcompress.vm v1, v2, v3] - shares {!vsub_vv_entries}'s exact
    [rd, rs2, rs1] shape; has no masked [, v0.t] sibling (real GNU as
    rejects one as "illegal operands"). *)

val vmsbf_m_entries : entry list
(** [vmsbf.m v1, v2] - shares {!vsext_vf2_entries}'s exact [rd, rs2]
    shape. *)

val vmsif_m_entries : entry list
(** [vmsif.m v1, v2] - {!vmsbf_m_entries}'s "including-first" sibling. *)

val vmsof_m_entries : entry list
(** [vmsof.m v1, v2] - {!vmsbf_m_entries}'s "only-first" sibling. *)

val vcpop_m_entries : entry list
(** [vcpop.m a0, v2] - the same [rd, rs2] shape as {!vmsbf_m_entries} but
    with a GPR destination. *)

val vfirst_m_entries : entry list
(** [vfirst.m a0, v2] - {!vcpop_m_entries}'s first-set-bit-index
    sibling. *)

val vadc_vvm_entries : entry list
(** [vadc.vvm v1, v2, v3, v0] on RV32IV and RV64IV - add-with-carry, with
    a mandatory literal [v0] 4th operand supplying the carry-in. *)

val vadc_vxm_entries : entry list
(** [vadc.vxm v1, v2, a0, v0] - {!vadc_vvm_entries}'s scalar-broadcast
    sibling. *)

val vadc_vim_entries : entry list
(** [vadc.vim v1, v2, -5, v0] - {!vadc_vvm_entries}'s signed-immediate
    sibling. *)

val vmadc_vvm_entries : entry list
(** [vmadc.vvm v1, v2, v3, v0] - compare-with-carry, the same
    mandatory-[v0] shape as {!vadc_vvm_entries}. *)

val vmadc_vxm_entries : entry list
(** [vmadc.vxm v1, v2, a0, v0] - {!vmadc_vvm_entries}'s scalar-broadcast
    sibling. *)

val vmadc_vim_entries : entry list
(** [vmadc.vim v1, v2, -5, v0] - {!vmadc_vvm_entries}'s signed-immediate
    sibling. *)

val vmadc_vv_entries : entry list
(** [vmadc.vv v1, v2, v3] - {!vmadc_vvm_entries}'s bare (no carry-in)
    sibling; shares {!vsub_vv_entries}'s exact shape, with no masked
    [, v0.t] sibling of its own. *)

val vmadc_vx_entries : entry list
(** [vmadc.vx v1, v2, a0] - {!vmadc_vv_entries}'s scalar-broadcast
    sibling. *)

val vmadc_vi_entries : entry list
(** [vmadc.vi v1, v2, -5] - {!vmadc_vv_entries}'s signed-immediate
    sibling. *)

val vsbc_vvm_entries : entry list
(** [vsbc.vvm v1, v2, v3, v0] - subtract-with-borrow, the same
    mandatory-[v0] shape as {!vadc_vvm_entries}; has no bare (non-"m")
    sibling. *)

val vsbc_vxm_entries : entry list
(** [vsbc.vxm v1, v2, a0, v0] - {!vsbc_vvm_entries}'s scalar-broadcast
    sibling. *)

val vmsbc_vvm_entries : entry list
(** [vmsbc.vvm v1, v2, v3, v0] - compare-with-borrow, the same
    mandatory-[v0] shape as {!vadc_vvm_entries}. *)

val vmsbc_vxm_entries : entry list
(** [vmsbc.vxm v1, v2, a0, v0] - {!vmsbc_vvm_entries}'s scalar-broadcast
    sibling. *)

val vmsbc_vv_entries : entry list
(** [vmsbc.vv v1, v2, v3] - {!vmsbc_vvm_entries}'s bare (no borrow-in)
    sibling; has no [.vi] sibling. *)

val vmsbc_vx_entries : entry list
(** [vmsbc.vx v1, v2, a0] - {!vmsbc_vv_entries}'s scalar-broadcast
    sibling. *)

val vmerge_vvm_entries : entry list
(** [vmerge.vvm v1, v2, v3, v0] - the same mandatory-[v0] shape as
    {!vadc_vvm_entries}; has no bare (non-"m") sibling. *)

val vmerge_vxm_entries : entry list
(** [vmerge.vxm v1, v2, a0, v0] - {!vmerge_vvm_entries}'s scalar-broadcast
    sibling. *)

val vmerge_vim_entries : entry list
(** [vmerge.vim v1, v2, -5, v0] - {!vmerge_vvm_entries}'s signed-immediate
    sibling. *)

val vmv_x_s_entries : entry list
(** [vmv.x.s a0, v2] on RV32IV and RV64IV - OP-V's element-0-to-GPR
    extract, a GPR-destination two-operand shape. *)

val vmv_s_x_entries : entry list
(** [vmv.s.x v1, a0] - {!vmv_x_s_entries}'s mirror-image
    GPR-to-element-0 insert. *)

val vmv_v_v_entries : entry list
(** [vmv.v.v v1, v2] - OP-V's unconditional-move family, a two-operand
    shape with no [vs2] operand at all. *)

val vmv_v_x_entries : entry list
(** [vmv.v.x v1, a0] - {!vmv_v_v_entries}'s scalar-broadcast sibling. *)

val vmv_v_i_entries : entry list
(** [vmv.v.i v1, 5] - {!vmv_v_v_entries}'s signed-immediate sibling. *)

val vmv1r_v_entries : entry list
(** [vmv1r.v v1, v2] on RV32IV and RV64IV - OP-V's whole-register-group
    move; real GNU as does not enforce the register-group alignment
    requirement at assembly time. *)

val vmv2r_v_entries : entry list
(** [vmv2r.v v2, v4] - {!vmv1r_v_entries}'s 2-register-group sibling. *)

val vmv4r_v_entries : entry list
(** [vmv4r.v v4, v8] - {!vmv1r_v_entries}'s 4-register-group sibling. *)

val vmv8r_v_entries : entry list
(** [vmv8r.v v8, v16] - {!vmv1r_v_entries}'s 8-register-group sibling. *)

val vsmul_vv_entries : entry list
(** [vsmul.vv v1, v2, v3] on RV32IV and RV64IV - the saturating
    fixed-point multiply pair, sharing {!vsub_vv_entries}'s exact OPIVV
    shape; has no [.vi] sibling. *)

val vsmul_vx_entries : entry list
(** [vsmul.vx v1, v2, a0] - {!vsmul_vv_entries}'s scalar-broadcast
    sibling. *)

val vfadd_vv_entries : entry list
(** [vfadd.vv v1, v2, v3] on RV32IV and RV64IV - the entry point into OP-V's
    floating-point arithmetic space (OPFVV), sharing {!vsub_vv_entries}'s
    exact all-vector-register OPIVV shape (only funct3 differs, an
    encoder-side concern). Real GNU as accepts it under plain [-march=rv32iv]/
    [rv64iv] with no explicit F/D dependency enforced. *)

val vfadd_vf_entries : entry list
(** [vfadd.vf v1, v2, fa0] - {!vfadd_vv_entries}'s scalar-broadcast (OPFVF)
    sibling; [rs1] is a floating-point register rather than a GPR. Has no
    [.vi] sibling. *)

val vfsub_vv_entries : entry list
(** [vfsub.vv v1, v2, v3] - {!vfadd_vv_entries}'s subtract sibling, the
    identical OPFVV shape. *)

val vfsub_vf_entries : entry list
(** [vfsub.vf v1, v2, fa0] - {!vfsub_vv_entries}'s scalar-broadcast (OPFVF)
    sibling. *)

val vfrsub_vf_entries : entry list
(** [vfrsub.vf v1, v2, fa0] - the reverse-subtract OPFVF sibling; has no
    [.vv] sibling (real GNU as rejects [vfrsub.vv] as unrecognized). *)

val vfmul_vv_entries : entry list
(** [vfmul.vv v1, v2, v3] - the same OPFVV shape as {!vfadd_vv_entries},
    despite integer multiply's own OPMVV space. *)

val vfmul_vf_entries : entry list
(** [vfmul.vf v1, v2, fa0] - {!vfmul_vv_entries}'s scalar-broadcast (OPFVF)
    sibling. *)

val vfdiv_vv_entries : entry list
(** [vfdiv.vv v1, v2, v3] - the same OPFVV shape as {!vfadd_vv_entries}. *)

val vfdiv_vf_entries : entry list
(** [vfdiv.vf v1, v2, fa0] - {!vfdiv_vv_entries}'s scalar-broadcast (OPFVF)
    sibling. *)

val vfrdiv_vf_entries : entry list
(** [vfrdiv.vf v1, v2, fa0] - the reverse-divide OPFVF sibling; has no
    [.vv] sibling (real GNU as rejects [vfrdiv.vv] as unrecognized). *)

val vfmin_vv_entries : entry list
(** [vfmin.vv v1, v2, v3] - the same OPFVV shape as {!vfadd_vv_entries}. *)

val vfmin_vf_entries : entry list
(** [vfmin.vf v1, v2, fa0] - {!vfmin_vv_entries}'s scalar-broadcast (OPFVF)
    sibling. *)

val vfmax_vv_entries : entry list
(** [vfmax.vv v1, v2, v3] - the same OPFVV shape as {!vfadd_vv_entries}. *)

val vfmax_vf_entries : entry list
(** [vfmax.vf v1, v2, fa0] - {!vfmax_vv_entries}'s scalar-broadcast (OPFVF)
    sibling. *)

val vfsgnj_vv_entries : entry list
(** [vfsgnj.vv v1, v2, v3] - the same OPFVV shape as {!vfadd_vv_entries}. *)

val vfsgnj_vf_entries : entry list
(** [vfsgnj.vf v1, v2, fa0] - {!vfsgnj_vv_entries}'s scalar-broadcast
    (OPFVF) sibling. *)

val vfsgnjn_vv_entries : entry list
(** [vfsgnjn.vv v1, v2, v3] - the same OPFVV shape as
    {!vfsgnj_vv_entries}. *)

val vfsgnjn_vf_entries : entry list
(** [vfsgnjn.vf v1, v2, fa0] - {!vfsgnjn_vv_entries}'s scalar-broadcast
    (OPFVF) sibling. *)

val vfsgnjx_vv_entries : entry list
(** [vfsgnjx.vv v1, v2, v3] - the same OPFVV shape as
    {!vfsgnj_vv_entries}. *)

val vfsgnjx_vf_entries : entry list
(** [vfsgnjx.vf v1, v2, fa0] - {!vfsgnjx_vv_entries}'s scalar-broadcast
    (OPFVF) sibling. *)

val vfsqrt_v_entries : entry list
(** [vfsqrt.v v1, v2] - the floating unary family, the same "vd, vs2" shape
    {!viota_m_entries} uses. *)

val vfrsqrt7_v_entries : entry list
(** [vfrsqrt7.v v1, v2] - the same unary shape as {!vfsqrt_v_entries}. *)

val vfrec7_v_entries : entry list
(** [vfrec7.v v1, v2] - the same unary shape as {!vfsqrt_v_entries}. *)

val vfclass_v_entries : entry list
(** [vfclass.v v1, v2] - the same unary shape as {!vfsqrt_v_entries}. *)

val vfredosum_vs_entries : entry list
(** [vfredosum.vs v1, v2, v3] - the floating vector-reduction family, the
    same "rd, rs2, rs1" all-vector shape as {!vfadd_vv_entries} (OPFVV
    rather than OPMVV). No [.vf]/[.vx] sibling exists. *)

val vfredusum_vs_entries : entry list
(** [vfredusum.vs v1, v2, v3] - the same shape as
    {!vfredosum_vs_entries}. *)

val vfredmin_vs_entries : entry list
(** [vfredmin.vs v1, v2, v3] - the same shape as {!vfredosum_vs_entries}. *)

val vfredmax_vs_entries : entry list
(** [vfredmax.vs v1, v2, v3] - the same shape as {!vfredosum_vs_entries}. *)

val vmfeq_vv_entries : entry list
(** [vmfeq.vv v1, v2, v3] - the mask-writing floating comparison family,
    the same "rd, rs2, rs1" shape as {!vfadd_vv_entries}. *)

val vmfeq_vf_entries : entry list
(** [vmfeq.vf v1, v2, fa0] - {!vmfeq_vv_entries}'s scalar-broadcast (OPFVF)
    sibling. *)

val vmfle_vv_entries : entry list
(** [vmfle.vv v1, v2, v3] - the same shape as {!vmfeq_vv_entries}. *)

val vmfle_vf_entries : entry list
(** [vmfle.vf v1, v2, fa0] - the same shape as {!vmfeq_vf_entries}. *)

val vmflt_vv_entries : entry list
(** [vmflt.vv v1, v2, v3] - the same shape as {!vmfeq_vv_entries}. *)

val vmflt_vf_entries : entry list
(** [vmflt.vf v1, v2, fa0] - the same shape as {!vmfeq_vf_entries}. *)

val vmfne_vv_entries : entry list
(** [vmfne.vv v1, v2, v3] - the same shape as {!vmfeq_vv_entries}. *)

val vmfne_vf_entries : entry list
(** [vmfne.vf v1, v2, fa0] - the same shape as {!vmfeq_vf_entries}. *)

val vmfgt_vf_entries : entry list
(** [vmfgt.vf v1, v2, fa0] - the reverse-greater-than OPFVF sibling; has no
    [.vv] sibling (real GNU as accepts [vmfgt.vv] only as a pseudo
    reversing [vmflt.vv]'s operands, deliberately not admitted here). *)

val vmfge_vf_entries : entry list
(** [vmfge.vf v1, v2, fa0] - the reverse-greater-or-equal OPFVF sibling;
    same [.vv]-exclusion reasoning as {!vmfgt_vf_entries}. *)

val vfmv_f_s_entries : entry list
(** [vfmv.f.s fa0, v2] - the FPR-typed mirror of {!vmv_x_s_entries}. *)

val vfmv_s_f_entries : entry list
(** [vfmv.s.f v1, fa0] - the FPR-typed mirror of {!vmv_s_x_entries}. *)

val vfmv_v_f_entries : entry list
(** [vfmv.v.f v1, fa0] - the FPR-typed sibling of {!vmv_v_x_entries}. *)

val vfmerge_vfm_entries : entry list
(** [vfmerge.vfm v1, v2, fa0, v0] - the FPR-typed mirror of
    {!vmerge_vxm_entries}: mandatory literal [v0] fourth operand, no bare
    (non-"m") sibling and no [, v0.t] masked form. *)

val vfcvt_xu_f_v_entries : entry list
(** [vfcvt.xu.f.v v1, v2] - the scalar-width float<->integer conversion
    family, the same "vd, vs2" shape {!vfsqrt_v_entries} uses. *)

val vfcvt_x_f_v_entries : entry list
(** [vfcvt.x.f.v v1, v2] - the same shape as {!vfcvt_xu_f_v_entries}. *)

val vfcvt_f_xu_v_entries : entry list
(** [vfcvt.f.xu.v v1, v2] - the same shape as {!vfcvt_xu_f_v_entries}. *)

val vfcvt_f_x_v_entries : entry list
(** [vfcvt.f.x.v v1, v2] - the same shape as {!vfcvt_xu_f_v_entries}. *)

val vfcvt_rtz_xu_f_v_entries : entry list
(** [vfcvt.rtz.xu.f.v v1, v2] - the same shape as
    {!vfcvt_xu_f_v_entries}. *)

val vfcvt_rtz_x_f_v_entries : entry list
(** [vfcvt.rtz.x.f.v v1, v2] - the same shape as {!vfcvt_xu_f_v_entries}. *)

val vfmadd_vv_entries : entry list
(** [vfmadd.vv v1, v2, v3] - the floating FMA family, {!vmacc_vv_entries}'s
    exact reordered-operand shape. *)

val vfmadd_vf_entries : entry list
(** [vfmadd.vf v1, fa0, v3] - {!vfmadd_vv_entries}'s scalar-broadcast
    (OPFVF) sibling; [rs1] is an FPR rather than a GPR. *)

val vfnmadd_vv_entries : entry list
(** [vfnmadd.vv v1, v2, v3] - the same shape as {!vfmadd_vv_entries}. *)

val vfnmadd_vf_entries : entry list
(** [vfnmadd.vf v1, fa0, v3] - the same shape as {!vfmadd_vf_entries}. *)

val vfmsub_vv_entries : entry list
(** [vfmsub.vv v1, v2, v3] - the same shape as {!vfmadd_vv_entries}. *)

val vfmsub_vf_entries : entry list
(** [vfmsub.vf v1, fa0, v3] - the same shape as {!vfmadd_vf_entries}. *)

val vfnmsub_vv_entries : entry list
(** [vfnmsub.vv v1, v2, v3] - the same shape as {!vfmadd_vv_entries}. *)

val vfnmsub_vf_entries : entry list
(** [vfnmsub.vf v1, fa0, v3] - the same shape as {!vfmadd_vf_entries}. *)

val vfmacc_vv_entries : entry list
(** [vfmacc.vv v1, v2, v3] - the same shape as {!vfmadd_vv_entries}. *)

val vfmacc_vf_entries : entry list
(** [vfmacc.vf v1, fa0, v3] - the same shape as {!vfmadd_vf_entries}. *)

val vfnmacc_vv_entries : entry list
(** [vfnmacc.vv v1, v2, v3] - the same shape as {!vfmadd_vv_entries}. *)

val vfnmacc_vf_entries : entry list
(** [vfnmacc.vf v1, fa0, v3] - the same shape as {!vfmadd_vf_entries}. *)

val vfmsac_vv_entries : entry list
(** [vfmsac.vv v1, v2, v3] - the same shape as {!vfmadd_vv_entries}. *)

val vfmsac_vf_entries : entry list
(** [vfmsac.vf v1, fa0, v3] - the same shape as {!vfmadd_vf_entries}. *)

val vfnmsac_vv_entries : entry list
(** [vfnmsac.vv v1, v2, v3] - the same shape as {!vfmadd_vv_entries}. *)

val vfnmsac_vf_entries : entry list
(** [vfnmsac.vf v1, fa0, v3] - the same shape as {!vfmadd_vf_entries}. *)

val vfslide1up_vf_entries : entry list
(** [vfslide1up.vf v1, v2, fa0] - {!vslide1up_vx_entries}'s floating OPFVF
    sibling; has no [.vi] sibling. *)

val vfslide1down_vf_entries : entry list
(** [vfslide1down.vf v1, v2, fa0] - {!vfslide1up_vf_entries}'s downward
    sibling. *)

val vfwadd_vv_entries : entry list
(** [vfwadd.vv v1, v2, v3] - the widening floating add, {!vfadd_vv_entries}'s
    exact shape reused (operand width is invisible to the assembler). *)

val vfwadd_vf_entries : entry list
(** [vfwadd.vf v1, v2, fa0] - {!vfwadd_vv_entries}'s scalar-broadcast
    sibling. *)

val vfwadd_wv_entries : entry list
(** [vfwadd.wv v1, v2, v3] - {!vfwadd_vv_entries}'s wide-[vs2] sibling; same
    shape. *)

val vfwadd_wf_entries : entry list
(** [vfwadd.wf v1, v2, fa0] - {!vfwadd_wv_entries}'s scalar-broadcast
    sibling. *)

val vfwsub_vv_entries : entry list
(** [vfwsub.vv v1, v2, v3] - the widening floating subtract,
    {!vfwadd_vv_entries}'s exact shape reused. *)

val vfwsub_vf_entries : entry list
(** [vfwsub.vf v1, v2, fa0] - {!vfwsub_vv_entries}'s scalar-broadcast
    sibling. *)

val vfwsub_wv_entries : entry list
(** [vfwsub.wv v1, v2, v3] - {!vfwsub_vv_entries}'s wide-[vs2] sibling; same
    shape. *)

val vfwsub_wf_entries : entry list
(** [vfwsub.wf v1, v2, fa0] - {!vfwsub_wv_entries}'s scalar-broadcast
    sibling. *)

val vfwmul_vv_entries : entry list
(** [vfwmul.vv v1, v2, v3] - the widening floating multiply,
    {!vfwadd_vv_entries}'s exact shape reused; no [.wv]/[.wf] sibling. *)

val vfwmul_vf_entries : entry list
(** [vfwmul.vf v1, v2, fa0] - {!vfwmul_vv_entries}'s scalar-broadcast
    sibling. *)

val vfwredosum_vs_entries : entry list
(** [vfwredosum.vs v1, v2, v3] - the widening floating ordered-sum
    reduction, {!vfredosum_vs_entries}'s exact shape reused; no [.vf]/[.vx]
    sibling. *)

val vfwredusum_vs_entries : entry list
(** [vfwredusum.vs v1, v2, v3] - {!vfwredosum_vs_entries}'s unordered-sum
    sibling. *)

val vfwcvt_xu_f_v_entries : entry list
(** [vfwcvt.xu.f.v v1, v2] - the widening float->unsigned-integer
    conversion, {!vfcvt_xu_f_v_entries}'s exact "vd, vs2" shape reused. *)

val vfwcvt_x_f_v_entries : entry list
(** [vfwcvt.x.f.v v1, v2] - {!vfwcvt_xu_f_v_entries}'s signed sibling. *)

val vfwcvt_f_xu_v_entries : entry list
(** [vfwcvt.f.xu.v v1, v2] - the widening unsigned-integer->float
    conversion, {!vfwcvt_xu_f_v_entries}'s exact shape reused. *)

val vfwcvt_f_x_v_entries : entry list
(** [vfwcvt.f.x.v v1, v2] - {!vfwcvt_f_xu_v_entries}'s signed sibling. *)

val vfwcvt_f_f_v_entries : entry list
(** [vfwcvt.f.f.v v1, v2] - the widening float->float conversion,
    {!vfwcvt_xu_f_v_entries}'s exact shape reused. *)

val vfwcvt_rtz_xu_f_v_entries : entry list
(** [vfwcvt.rtz.xu.f.v v1, v2] - {!vfwcvt_xu_f_v_entries}'s
    round-toward-zero sibling. *)

val vfwcvt_rtz_x_f_v_entries : entry list
(** [vfwcvt.rtz.x.f.v v1, v2] - {!vfwcvt_x_f_v_entries}'s round-toward-zero
    sibling. *)

val vfncvt_xu_f_w_entries : entry list
(** [vfncvt.xu.f.w v1, v2] - the narrowing float->unsigned-integer
    conversion, {!vfwcvt_xu_f_v_entries}'s exact shape reused. *)

val vfncvt_x_f_w_entries : entry list
(** [vfncvt.x.f.w v1, v2] - {!vfncvt_xu_f_w_entries}'s signed sibling. *)

val vfncvt_f_xu_w_entries : entry list
(** [vfncvt.f.xu.w v1, v2] - the narrowing unsigned-integer->float
    conversion, {!vfncvt_xu_f_w_entries}'s exact shape reused. *)

val vfncvt_f_x_w_entries : entry list
(** [vfncvt.f.x.w v1, v2] - {!vfncvt_f_xu_w_entries}'s signed sibling. *)

val vfncvt_f_f_w_entries : entry list
(** [vfncvt.f.f.w v1, v2] - the narrowing float->float conversion,
    {!vfncvt_xu_f_w_entries}'s exact shape reused. *)

val vfncvt_rod_f_f_w_entries : entry list
(** [vfncvt.rod.f.f.w v1, v2] - {!vfncvt_f_f_w_entries}'s
    round-to-odd sibling. *)

val vfncvt_rtz_xu_f_w_entries : entry list
(** [vfncvt.rtz.xu.f.w v1, v2] - {!vfncvt_xu_f_w_entries}'s
    round-toward-zero sibling. *)

val vfncvt_rtz_x_f_w_entries : entry list
(** [vfncvt.rtz.x.f.w v1, v2] - {!vfncvt_x_f_w_entries}'s round-toward-zero
    sibling. *)

val vfwmacc_vv_entries : entry list
(** [vfwmacc.vv v1, v2, v3] - the widening floating fused-multiply-add,
    {!vfmacc_vv_entries}'s exact reordered-operand shape reused. *)

val vfwmacc_vf_entries : entry list
(** [vfwmacc.vf v1, fa0, v3] - {!vfwmacc_vv_entries}'s scalar-broadcast
    sibling. *)

val vfwnmacc_vv_entries : entry list
(** [vfwnmacc.vv v1, v2, v3] - {!vfwmacc_vv_entries}'s negated sibling. *)

val vfwnmacc_vf_entries : entry list
(** [vfwnmacc.vf v1, fa0, v3] - {!vfwnmacc_vv_entries}'s scalar-broadcast
    sibling. *)

val vfwmsac_vv_entries : entry list
(** [vfwmsac.vv v1, v2, v3] - {!vfwmacc_vv_entries}'s subtract-form
    sibling. *)

val vfwmsac_vf_entries : entry list
(** [vfwmsac.vf v1, fa0, v3] - {!vfwmsac_vv_entries}'s scalar-broadcast
    sibling. *)

val vfwnmsac_vv_entries : entry list
(** [vfwnmsac.vv v1, v2, v3] - {!vfwmsac_vv_entries}'s negated sibling. *)

val vfwnmsac_vf_entries : entry list
(** [vfwnmsac.vf v1, fa0, v3] - {!vfwnmsac_vv_entries}'s scalar-broadcast
    sibling. *)

val vle8_v_entries : entry list
(** [vle8.v v1, (a0)] - V's unit-stride vector-register load, the entry point
    into the load/store slice of [rv_v]. *)

val vle16_v_entries : entry list
(** [vle16.v v1, (a0)] - {!vle8_v_entries}'s wider-element sibling. *)

val vle32_v_entries : entry list
(** [vle32.v v1, (a0)] - {!vle8_v_entries}'s wider-element sibling. *)

val vle64_v_entries : entry list
(** [vle64.v v1, (a0)] - {!vle8_v_entries}'s wider-element sibling. *)

val vse8_v_entries : entry list
(** [vse8.v v1, (a0)] - {!vle8_v_entries}'s store-shape sibling. *)

val vse16_v_entries : entry list
(** [vse16.v v1, (a0)] - {!vse8_v_entries}'s wider-element sibling. *)

val vse32_v_entries : entry list
(** [vse32.v v1, (a0)] - {!vse8_v_entries}'s wider-element sibling. *)

val vse64_v_entries : entry list
(** [vse64.v v1, (a0)] - {!vse8_v_entries}'s wider-element sibling. *)

val vlm_v_entries : entry list
(** [vlm.v v1, (a0)] - V's mask-register load, {!vle8_v_entries}'s exact
    shape reused (no masked variant exists to note as out of scope). *)

val vsm_v_entries : entry list
(** [vsm.v v1, (a0)] - {!vlm_v_entries}'s store-shape sibling. *)

val vle8ff_v_entries : entry list
(** [vle8ff.v v1, (a0)] - V's fault-only-first unit-stride load,
    {!vle8_v_entries}'s exact shape reused (no store counterpart). *)

val vle16ff_v_entries : entry list
(** [vle16ff.v v1, (a0)] - {!vle8ff_v_entries}'s wider-element sibling. *)

val vle32ff_v_entries : entry list
(** [vle32ff.v v1, (a0)] - {!vle8ff_v_entries}'s wider-element sibling. *)

val vle64ff_v_entries : entry list
(** [vle64ff.v v1, (a0)] - {!vle8ff_v_entries}'s wider-element sibling. *)

val vlse8_v_entries : entry list
(** [vlse8.v v1, (a0), a1] - V's strided vector-register load, a
    three-operand "vd, (base), rs2" shape ({!vle8_v_entries}'s memory
    operand plus a plain-GPR stride). *)

val vlse16_v_entries : entry list
(** [vlse16.v v1, (a0), a1] - {!vlse8_v_entries}'s wider-element sibling. *)

val vlse32_v_entries : entry list
(** [vlse32.v v1, (a0), a1] - {!vlse8_v_entries}'s wider-element sibling. *)

val vlse64_v_entries : entry list
(** [vlse64.v v1, (a0), a1] - {!vlse8_v_entries}'s wider-element sibling. *)

val vsse8_v_entries : entry list
(** [vsse8.v v1, (a0), a1] - {!vlse8_v_entries}'s store-shape sibling. *)

val vsse16_v_entries : entry list
(** [vsse16.v v1, (a0), a1] - {!vsse8_v_entries}'s wider-element sibling. *)

val vsse32_v_entries : entry list
(** [vsse32.v v1, (a0), a1] - {!vsse8_v_entries}'s wider-element sibling. *)

val vsse64_v_entries : entry list
(** [vsse64.v v1, (a0), a1] - {!vsse8_v_entries}'s wider-element sibling. *)

val vluxei8_v_entries : entry list
(** [vluxei8.v v1, (a0), v2] - V's unordered indexed vector-register
    load, {!vlse8_v_entries}'s shape with a vector-register index in
    place of the GPR stride. *)

val vluxei16_v_entries : entry list
(** [vluxei16.v v1, (a0), v2] - {!vluxei8_v_entries}'s wider-index
    sibling. *)

val vluxei32_v_entries : entry list
(** [vluxei32.v v1, (a0), v2] - {!vluxei8_v_entries}'s wider-index
    sibling. *)

val vluxei64_v_entries : entry list
(** [vluxei64.v v1, (a0), v2] - {!vluxei8_v_entries}'s wider-index
    sibling. *)

val vloxei8_v_entries : entry list
(** [vloxei8.v v1, (a0), v2] - {!vluxei8_v_entries}'s ordered sibling
    (mop=0b11 instead of 0b01). *)

val vloxei16_v_entries : entry list
(** [vloxei16.v v1, (a0), v2] - {!vloxei8_v_entries}'s wider-index
    sibling. *)

val vloxei32_v_entries : entry list
(** [vloxei32.v v1, (a0), v2] - {!vloxei8_v_entries}'s wider-index
    sibling. *)

val vloxei64_v_entries : entry list
(** [vloxei64.v v1, (a0), v2] - {!vloxei8_v_entries}'s wider-index
    sibling. *)

val vsuxei8_v_entries : entry list
(** [vsuxei8.v v1, (a0), v2] - {!vluxei8_v_entries}'s store-shape
    sibling. *)

val vsuxei16_v_entries : entry list
(** [vsuxei16.v v1, (a0), v2] - {!vsuxei8_v_entries}'s wider-index
    sibling. *)

val vsuxei32_v_entries : entry list
(** [vsuxei32.v v1, (a0), v2] - {!vsuxei8_v_entries}'s wider-index
    sibling. *)

val vsuxei64_v_entries : entry list
(** [vsuxei64.v v1, (a0), v2] - {!vsuxei8_v_entries}'s wider-index
    sibling. *)

val vsoxei8_v_entries : entry list
(** [vsoxei8.v v1, (a0), v2] - {!vloxei8_v_entries}'s store-shape
    sibling. *)

val vsoxei16_v_entries : entry list
(** [vsoxei16.v v1, (a0), v2] - {!vsoxei8_v_entries}'s wider-index
    sibling. *)

val vsoxei32_v_entries : entry list
(** [vsoxei32.v v1, (a0), v2] - {!vsoxei8_v_entries}'s wider-index
    sibling. *)

val vsoxei64_v_entries : entry list
(** [vsoxei64.v v1, (a0), v2] - {!vsoxei8_v_entries}'s wider-index
    sibling. *)

val vl1re8_v_entries : entry list
(** [vl1re8.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl1re16_v_entries : entry list
(** [vl1re16.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl1re32_v_entries : entry list
(** [vl1re32.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl1re64_v_entries : entry list
(** [vl1re64.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl2re8_v_entries : entry list
(** [vl2re8.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl2re16_v_entries : entry list
(** [vl2re16.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl2re32_v_entries : entry list
(** [vl2re32.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl2re64_v_entries : entry list
(** [vl2re64.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl4re8_v_entries : entry list
(** [vl4re8.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl4re16_v_entries : entry list
(** [vl4re16.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl4re32_v_entries : entry list
(** [vl4re32.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl4re64_v_entries : entry list
(** [vl4re64.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl8re8_v_entries : entry list
(** [vl8re8.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl8re16_v_entries : entry list
(** [vl8re16.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl8re32_v_entries : entry list
(** [vl8re32.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vl8re64_v_entries : entry list
(** [vl8re64.v v1, (a0)] - V's whole-register load,
    {!vle8_v_entries}'s shape reused (fixed lumop/nf, no masked
    variant). *)

val vs1r_v_entries : entry list
(** [vs1r.v v1, (a0)] - {!vl1re8_v_entries}'s store-shape sibling
    (no width variant). *)

val vs2r_v_entries : entry list
(** [vs2r.v v1, (a0)] - {!vl2re8_v_entries}'s store-shape sibling
    (no width variant). *)

val vs4r_v_entries : entry list
(** [vs4r.v v1, (a0)] - {!vl4re8_v_entries}'s store-shape sibling
    (no width variant). *)

val vs8r_v_entries : entry list
(** [vs8r.v v1, (a0)] - {!vl8re8_v_entries}'s store-shape sibling
    (no width variant). *)

val sh1add_entries : entry list
(** [sh1add a0, a1, a2] on RV32IM_Zba and RV64IM_Zba, Zba's scale-one R-type
    form. *)

val sh2add_entries : entry list
(** [sh2add a0, a1, a2] on RV32IM_Zba and RV64IM_Zba, Zba's scale-two R-type
    form. *)

val sh3add_entries : entry list
(** [sh3add a0, a1, a2] on RV32IM_Zba and RV64IM_Zba, Zba's scale-three
    R-type form. This closes Zba's non-word scale family. *)

val sh1adduw_entries : entry list
(** [sh1add.uw a0, a1, a2] on RV64IM_Zba only - riscv-opcodes has no RV32
    counterpart, Zba's scale-one word-operand R-type form (opcode 0x3b). *)

val sh2adduw_entries : entry list
(** [sh2add.uw a0, a1, a2] on RV64IM_Zba only, Zba's scale-two word-operand
    R-type form. *)

val sh3adduw_entries : entry list
(** [sh3add.uw a0, a1, a2] on RV64IM_Zba only, Zba's scale-three
    word-operand R-type form. This closes Zba's [*.uw] family, completing
    Zba's full scale-and-add coverage. *)

val min_entries : entry list
(** [min a0, a1, a2] on RV32IM_Zbb and RV64IM_Zbb. *)

val minu_entries : entry list
(** [minu a0, a1, a2] on RV32IM_Zbb and RV64IM_Zbb. *)

val max_entries : entry list
(** [max a0, a1, a2] on RV32IM_Zbb and RV64IM_Zbb. *)

val maxu_entries : entry list
(** [maxu a0, a1, a2] on RV32IM_Zbb and RV64IM_Zbb. This closes Zbb's
    single-extension min/max comparison family; [andn]/[orn]/[xnor]/[rol]/
    [ror] (below) are the OR-requirement-across-extensions family instead. *)

val andn_entries : entry list
(** [andn a0, a1, a2] on RV32IM_Zbb and RV64IM_Zbb - the checked-in
    riscv-opcodes snapshot lists [andn] identically under five different
    extension files (rv_zbb, rv_zbkb, rv_zk, rv_zkn, rv_zks);
    {!Isa_norm_riscv}'s [Req_any] now expresses that, and this generator
    exercises the rv_zbb (primary, non-import) configuration. *)

val orn_entries : entry list
(** [orn a0, a1, a2], the same five-extension Req_any shape as {!andn_entries}. *)

val xnor_entries : entry list
(** [xnor a0, a1, a2], the same five-extension Req_any shape as {!andn_entries}. *)

val rol_entries : entry list
(** [rol a0, a1, a2], the same five-extension Req_any shape as {!andn_entries}. *)

val ror_entries : entry list
(** [ror a0, a1, a2], the same five-extension Req_any shape as {!andn_entries}.
    This closes the five-way import-duplicated slice of Zbb; the rest of Zbb
    (population count, sign/zero-extend, byte-reverse) remains outside this
    bounded slice. *)

val clz_entries : entry list
(** [clz a0, a1] on RV32IM_Zbb and RV64IM_Zbb - two plain GPR operands, no
    immediate; the remaining encoding bits are a fully fixed funct12. *)

val ctz_entries : entry list
(** [ctz a0, a1], the same two-GPR-operand shape as {!clz_entries}. *)

val cpop_entries : entry list
(** [cpop a0, a1], the same two-GPR-operand shape as {!clz_entries}. *)

val sextb_entries : entry list
(** [sext.b a0, a1], the same two-GPR-operand shape as {!clz_entries}. *)

val sexth_entries : entry list
(** [sext.h a0, a1], the same two-GPR-operand shape as {!clz_entries}. *)

val orcb_entries : entry list
(** [orc.b a0, a1], the same two-GPR-operand shape as {!clz_entries}. This
    closes Zbb's XLEN-independent unary family. *)

val clzw_entries : entry list
(** [clzw a0, a1] on RV64IM_Zbb only - riscv-opcodes has no RV32
    counterpart, Zbb's word-operand population-count sibling. *)

val ctzw_entries : entry list
(** [ctzw a0, a1] on RV64IM_Zbb only, the same RV64-only shape as
    {!clzw_entries}. *)

val cpopw_entries : entry list
(** [cpopw a0, a1] on RV64IM_Zbb only, the same RV64-only shape as
    {!clzw_entries}. This closes Zbb's [*w] word-operand unary family. *)

val brev8_entries : entry list
(** [brev8 a0, a1] on RV32I_Zbkb and RV64I_Zbkb - the same two-GPR-operand
    unary shape as {!clz_entries}, but with a four-way Req_any
    (rv_zbkb/rv_zk/rv_zkn/rv_zks) and the identical mnemonic/encoding on
    both profiles. *)

val rev8_entries : entry list
(** [rev8 a0, a1] on both profiles - the same two-GPR-operand unary shape as
    {!brev8_entries}, but with an XLEN-dependent funct12 (0x6b8 on RV64,
    0x698 on RV32) and a five-way Req_any keyed by riscv-opcodes' own
    per-profile native_name ("rev8" on RV64, "rev8.rv32" on RV32 - the
    latter a source-internal disambiguation label real GNU as does not
    accept; both render as the bare ["rev8"] spelling GNU as DOES accept on
    both profiles). This closes the population-count/sign-extend/
    byte-reverse family. *)

val pack_entries : entry list
(** [pack a0, a1, a2] on RV32I_Zbkb and RV64I_Zbkb - Zbkb's plain
    three-GPR-operand R-type pack-low form, with a four-way Req_any and the
    identical mnemonic/encoding on both profiles. *)

val packh_entries : entry list
(** [packh a0, a1, a2], the same shape as {!pack_entries}. *)

val packw_entries : entry list
(** [packw a0, a1, a2] on RV64I_Zbkb only - riscv-opcodes has no RV32
    counterpart, {!pack_entries}'s RV64-only word-operand sibling. *)

val zip_entries : entry list
(** [zip a0, a1] on RV32I_Zbkb only - the same two-GPR-operand unary shape
    as {!brev8_entries}, but riscv-opcodes has no RV64 counterpart at all
    (confirmed: real GNU as rejects it as an unrecognized opcode). *)

val unzip_entries : entry list
(** [unzip a0, a1] on RV32I_Zbkb only, the same RV32-only shape as
    {!zip_entries}. *)

val rolw_entries : entry list
(** [rolw a0, a1, a2] on RV64IM_Zbb only - riscv-opcodes has no RV32
    counterpart (confirmed: real riscv32-linux-gnu-as rejects it as an
    unrecognized opcode), {!rol_entries}'s RV64-only word-operand sibling
    (opcode 0x3b). *)

val rorw_entries : entry list
(** [rorw a0, a1, a2] on RV64IM_Zbb only, the same RV64-only shape as
    {!rolw_entries}. *)

val rori_entries : entry list
(** [rori a0, a1, 5] on both profiles - Zbb's rotate-*immediate* shape (rd,
    rs1, shamt), the first entry family with a genuine shift-amount
    immediate operand rather than a third GPR. Uses riscv32.jsonl's own
    native_name ["rori.rv32"] as {!entry.lookup_key} on RV32 (5-bit shamtw)
    and ["rori"] on RV64 (6-bit shamtd), the same profile-specific split
    {!rev8_entries} uses, since both render as the bare ["rori"] spelling
    real GNU as accepts on either profile. *)

val roriw_entries : entry list
(** [roriw a0, a1, 5] on RV64IM_Zbb only - riscv-opcodes has no RV32
    counterpart (confirmed: real riscv32-linux-gnu-as rejects it as an
    unrecognized opcode), {!rori_entries}'s plain RV64-only *w sibling
    (opcode 0x1b, no profile-specific native_name split needed). *)

val zext_h_entries : entry list
(** [zext.h a0, a1] on both profiles - Zbb's zero-extend-halfword pseudo (rd,
    rs1), the same two-GPR-operand unary shape as {!clz_entries} but needing
    only Zbb (not Zbkb). Uses riscv32.jsonl's own native_name
    ["zext.h.rv32"] as {!entry.lookup_key} on RV32 (specializing [pack]) and
    ["zext.h"] on RV64 (specializing [packw]), the same profile-specific
    split {!rev8_entries} uses, since both render as the bare ["zext.h"]
    spelling real GNU as accepts on either profile - unlike rev8/rori,
    neither record is imported by a second extension file, so there is no
    Req_any here, just a plain per-profile Zbb requirement. *)

val clmul_entries : entry list
(** [clmul a0, a1, a2] on both profiles - Zbc's carry-less multiply, the same
    five-way Req_any three-GPR R-type shape {!andn_entries} uses but rooted
    in Zbc (rv_zbc primary, imported by rv_zbkc/rv_zk/rv_zkn/rv_zks) rather
    than Zbb, with an identical mnemonic/encoding on both profiles (no
    rev8-style native_name split needed). *)

val clmulh_entries : entry list
(** [clmulh a0, a1, a2] on both profiles, {!clmul_entries}'s high-half
    sibling (same Req_any group, opcode, funct7; only funct3 differs). *)

val xperm4_entries : entry list
(** [xperm4 a0, a1, a2] on both profiles - Zbkx's crossbar-permute-nibble,
    the same three-GPR R-type shape {!clmul_entries} uses but a four-way
    Req_any group (rv_zbkx primary, imported by rv_zk/rv_zkn/rv_zks - no
    separate non-K sibling extension the way clmul/clmulh have rv_zbc
    alongside rv_zbkc). *)

val xperm8_entries : entry list
(** [xperm8 a0, a1, a2] on both profiles, {!xperm4_entries}'s byte-granular
    sibling (same Req_any group, opcode, funct7; only funct3 differs). *)

val sha256sum0_entries : entry list
(** [sha256sum0 a0, a1] on both profiles - Zknh's SHA-256 message-schedule
    helper, the same two-GPR-operand unary shape {!clz_entries} uses but a
    three-way Req_any group (rv_zknh primary, imported by rv_zk/rv_zkn - no
    rv_zks, since SHA-256 belongs to the NIST crypto profile, not
    ShangMi). *)

val sha256sum1_entries : entry list
(** [sha256sum1 a0, a1] on both profiles, {!sha256sum0_entries}'s sibling
    (same Req_any group, opcode, funct3; only funct12 differs). *)

val sha256sig0_entries : entry list
(** [sha256sig0 a0, a1] on both profiles, {!sha256sum0_entries}'s sibling
    (same Req_any group, opcode, funct3; only funct12 differs). *)

val sha256sig1_entries : entry list
(** [sha256sig1 a0, a1] on both profiles, {!sha256sum0_entries}'s sibling
    (same Req_any group, opcode, funct3; only funct12 differs). *)

val sha512sum0_entries : entry list
(** [sha512sum0 a0, a1] on RV64 only - {!sha256sum0_entries}'s RV64-only
    sibling; riscv-opcodes has no RV32 record at all (confirmed: real
    riscv32-linux-gnu-as rejects it as an unrecognized opcode). *)

val sha512sum1_entries : entry list
(** [sha512sum1 a0, a1] on RV64 only, {!sha512sum0_entries}'s sibling
    (same Req_any group, opcode, funct3; only funct12 differs). *)

val sha512sig0_entries : entry list
(** [sha512sig0 a0, a1] on RV64 only, {!sha512sum0_entries}'s sibling
    (same Req_any group, opcode, funct3; only funct12 differs). *)

val sha512sig1_entries : entry list
(** [sha512sig1 a0, a1] on RV64 only, {!sha512sum0_entries}'s sibling
    (same Req_any group, opcode, funct3; only funct12 differs). *)

val sha512sum0r_entries : entry list
(** [sha512sum0r a0, a1, a2] on RV32 only - SHA-512's own 32-bit-word-pair-
    split helper, a plain three-GPR R-type shape (not the two-GPR unary
    one {!sha512sum0_entries} etc. use), a three-way Req_any group
    (rv32_zknh primary, imported by rv32_zk/rv32_zkn); riscv-opcodes has no
    RV64 record at all (confirmed: real riscv64-linux-gnu-as rejects it as
    an unrecognized opcode). *)

val sha512sum1r_entries : entry list
(** [sha512sum1r a0, a1, a2] on RV32 only, {!sha512sum0r_entries}'s sibling
    (same Req_any group, opcode, funct3; only funct7 differs). *)

val sha512sig0l_entries : entry list
(** [sha512sig0l a0, a1, a2] on RV32 only, {!sha512sum0r_entries}'s sibling
    (same Req_any group, opcode, funct3; only funct7 differs). *)

val sha512sig1l_entries : entry list
(** [sha512sig1l a0, a1, a2] on RV32 only, {!sha512sum0r_entries}'s sibling
    (same Req_any group, opcode, funct3; only funct7 differs). *)

val sha512sig0h_entries : entry list
(** [sha512sig0h a0, a1, a2] on RV32 only, {!sha512sum0r_entries}'s sibling
    (same Req_any group, opcode, funct3; only funct7 differs). *)

val sha512sig1h_entries : entry list
(** [sha512sig1h a0, a1, a2] on RV32 only, {!sha512sum0r_entries}'s sibling
    (same Req_any group, opcode, funct3; only funct7 differs). *)

val aes64ds_entries : entry list
(** [aes64ds a0, a1, a2] on RV64 only - AES-64's decrypt round, a plain
    three-GPR R-type shape reusing {!r_type_gpr_form}/{!r_desc}, a
    three-way Req_any group (rv64_zknd primary, imported by
    rv64_zk/rv64_zkn). riscv-opcodes has no RV32 record at all. *)

val aes64dsm_entries : entry list
(** [aes64dsm a0, a1, a2] on RV64 only, {!aes64ds_entries}'s
    mixed-columns sibling (same Req_any group, opcode, funct3; only
    funct7 differs). *)

val aes64es_entries : entry list
(** [aes64es a0, a1, a2] on RV64 only - AES-64's encrypt round, the same
    shape as {!aes64ds_entries} but rooted in a disjoint primary
    extension (rv64_zkne, imported by rv64_zk/rv64_zkn). *)

val aes64esm_entries : entry list
(** [aes64esm a0, a1, a2] on RV64 only, {!aes64es_entries}'s
    mixed-columns sibling (same Req_any group, opcode, funct3; only
    funct7 differs). *)

val aes64ks2_entries : entry list
(** [aes64ks2 a0, a1, a2] on RV64 only - AES-64's key-schedule helper,
    the one mnemonic in this family imported by BOTH zknd and zkne (a
    four-way Req_any group: rv64_zknd primary, imported by
    rv64_zk/rv64_zkn/rv64_zkne). *)

val aes64im_entries : entry list
(** [aes64im a0, a1] on RV64 only - AES-64's inverse-mix-columns helper,
    the one two-GPR unary form in this family (same shape as
    {!sha512sum0_entries}), sharing {!aes64ds_entries}'s own three-way
    Zknd-rooted Req_any group. *)

val aes64ks1i_entries : entry list
(** [aes64ks1i a0, a1, 5] on RV64 only - AES-64's first key-schedule helper,
    the same two-GPR-plus-narrow-unsigned-immediate shape {!rori_entries}
    uses, sharing {!aes64ks2_entries}'s own four-way Req_any group. [rnum]'s
    real valid range is 0-10 (of the 16 the 4-bit field can represent) -
    real GNU as rejects 11-15 outright. *)

val aes32dsi_entries : entry list
(** [aes32dsi a0, a1, a2, 3] on RV32 only - AES-32's own decrypt round, a
    three-GPR-plus-narrow-unsigned-immediate shape (riscv-opcodes' own "bs"
    byte-select field), a three-way Req_any group (rv32_zknd primary,
    imported by rv32_zk/rv32_zkn). riscv-opcodes has no RV64 record at
    all. *)

val aes32dsmi_entries : entry list
(** [aes32dsmi a0, a1, a2, 3] on RV32 only, {!aes32dsi_entries}'s
    mixed-columns sibling (same Req_any group, opcode, funct3; only the
    fixed 5-bit selector portion of funct7 differs). *)

val aes32esi_entries : entry list
(** [aes32esi a0, a1, a2, 3] on RV32 only - AES-32's own encrypt round, the
    same shape as {!aes32dsi_entries} but rooted in a disjoint primary
    extension (rv32_zkne, imported by rv32_zk/rv32_zkn). *)

val aes32esmi_entries : entry list
(** [aes32esmi a0, a1, a2, 3] on RV32 only, {!aes32esi_entries}'s
    mixed-columns sibling (same Req_any group, opcode, funct3; only the
    fixed 5-bit selector portion of funct7 differs). *)

val csrrw_entries : entry list
(** [csrrw a0, 0x300, a1] on both profiles - Zicsr's register-source
    read/write CSR form, [rd, csr, rs1] operands (GAS's own text order,
    not riscv-opcodes' [rd, rs1, csr] field order). A plain Req_feature
    (no import duplication); XLEN-independent. *)

val csrrs_entries : entry list
(** [csrrs a0, 0x300, a1] on both profiles, {!csrrw_entries}'s read-set
    sibling (same shape; only funct3 differs). *)

val csrrc_entries : entry list
(** [csrrc a0, 0x300, a1] on both profiles, {!csrrw_entries}'s read-clear
    sibling (same shape; only funct3 differs). *)

val csrrwi_entries : entry list
(** [csrrwi a0, 0x300, 5] on both profiles - Zicsr's immediate-source
    read/write CSR form, [rd, csr, zimm5] operands (no register operand
    besides [rd]). *)

val csrrsi_entries : entry list
(** [csrrsi a0, 0x300, 5] on both profiles, {!csrrwi_entries}'s read-set
    sibling (same shape; only funct3 differs). *)

val csrrci_entries : entry list
(** [csrrci a0, 0x300, 5] on both profiles, {!csrrwi_entries}'s read-clear
    sibling (same shape; only funct3 differs). *)

val csrr_entries : entry list
(** [csrr a0, 0x300] on both profiles - GAS's read-only alias for
    {!csrrs_entries} with an implicit x0 source, [rd, csr] operands. *)

val csrw_entries : entry list
(** [csrw 0x300, a1] on both profiles - GAS's write-only alias for
    {!csrrw_entries} with an implicit x0 destination, [csr, rs1] operands
    (csr first, unlike {!csrrw_entries}'s [rd, csr, rs1]). *)

val csrs_entries : entry list
(** [csrs 0x300, a1] on both profiles, {!csrw_entries}'s set-only sibling
    (same shape; only funct3 differs). *)

val csrc_entries : entry list
(** [csrc 0x300, a1] on both profiles, {!csrw_entries}'s clear-only sibling
    (same shape; only funct3 differs). *)

val csrwi_entries : entry list
(** [csrwi 0x300, 5] on both profiles - GAS's write-only alias for
    {!csrrwi_entries} with an implicit x0 destination, [csr, zimm5]
    operands. *)

val csrsi_entries : entry list
(** [csrsi 0x300, 5] on both profiles, {!csrwi_entries}'s set-only sibling
    (same shape; only funct3 differs). *)

val csrci_entries : entry list
(** [csrci 0x300, 5] on both profiles, {!csrwi_entries}'s clear-only
    sibling (same shape; only funct3 differs). *)

val amoswap_w_entries : entry list
(** [amoswap.w a0, a1, (a2)] on both profiles - Zaamo's atomic-swap, the
    first entry using a real GAS memory group [(base)] third operand
    rather than a plain register or immediate. *)

val amoadd_w_entries : entry list
(** [amoadd.w a0, a1, (a2)] on both profiles, {!amoswap_w_entries}'s
    atomic-add sibling (same shape; only funct5 differs). *)

val amoxor_w_entries : entry list
(** [amoxor.w a0, a1, (a2)] on both profiles, {!amoswap_w_entries}'s
    atomic-xor sibling (same shape; only funct5 differs). *)

val amoand_w_entries : entry list
(** [amoand.w a0, a1, (a2)] on both profiles, {!amoswap_w_entries}'s
    atomic-and sibling (same shape; only funct5 differs). *)

val amoor_w_entries : entry list
(** [amoor.w a0, a1, (a2)] on both profiles, {!amoswap_w_entries}'s
    atomic-or sibling (same shape; only funct5 differs). *)

val amomin_w_entries : entry list
(** [amomin.w a0, a1, (a2)] on both profiles, {!amoswap_w_entries}'s
    signed-atomic-min sibling (same shape; only funct5 differs). *)

val amomax_w_entries : entry list
(** [amomax.w a0, a1, (a2)] on both profiles, {!amoswap_w_entries}'s
    signed-atomic-max sibling (same shape; only funct5 differs). *)

val amominu_w_entries : entry list
(** [amominu.w a0, a1, (a2)] on both profiles, {!amoswap_w_entries}'s
    unsigned-atomic-min sibling (same shape; only funct5 differs). *)

val amomaxu_w_entries : entry list
(** [amomaxu.w a0, a1, (a2)] on both profiles, {!amoswap_w_entries}'s
    unsigned-atomic-max sibling (same shape; only funct5 differs). *)

val sc_w_entries : entry list
(** [sc.w a0, a1, (a2)] on both profiles, {!amoswap_w_entries}'s
    store-conditional sibling (same operand shape; only funct5 differs). *)

val lr_w_entries : entry list
(** [lr.w a0, (a2)] on both profiles - Zaamo's own two-operand member
    ([rd, (base)], no [rs2]: its field is fixed to 0). *)

val amoswap_d_entries : entry list
(** [amoswap.d a0, a1, (a2)] on RV64 only (rv64_a - no RV32 record),
    {!amoswap_w_entries}'s 64-bit sibling (same shape; only funct3
    differs). *)

val amoadd_d_entries : entry list
(** [amoadd.d a0, a1, (a2)] on RV64 only, {!amoswap_d_entries}'s
    atomic-add sibling. *)

val amoxor_d_entries : entry list
(** [amoxor.d a0, a1, (a2)] on RV64 only, {!amoswap_d_entries}'s
    atomic-xor sibling. *)

val amoand_d_entries : entry list
(** [amoand.d a0, a1, (a2)] on RV64 only, {!amoswap_d_entries}'s
    atomic-and sibling. *)

val amoor_d_entries : entry list
(** [amoor.d a0, a1, (a2)] on RV64 only, {!amoswap_d_entries}'s atomic-or
    sibling. *)

val amomin_d_entries : entry list
(** [amomin.d a0, a1, (a2)] on RV64 only, {!amoswap_d_entries}'s
    signed-atomic-min sibling. *)

val amomax_d_entries : entry list
(** [amomax.d a0, a1, (a2)] on RV64 only, {!amoswap_d_entries}'s
    signed-atomic-max sibling. *)

val amominu_d_entries : entry list
(** [amominu.d a0, a1, (a2)] on RV64 only, {!amoswap_d_entries}'s
    unsigned-atomic-min sibling. *)

val amomaxu_d_entries : entry list
(** [amomaxu.d a0, a1, (a2)] on RV64 only, {!amoswap_d_entries}'s
    unsigned-atomic-max sibling. *)

val sc_d_entries : entry list
(** [sc.d a0, a1, (a2)] on RV64 only, {!amoswap_d_entries}'s
    store-conditional sibling. *)

val lr_d_entries : entry list
(** [lr.d a0, (a2)] on RV64 only, {!lr_w_entries}'s 64-bit sibling. *)

val all : entry list
(** [sw_entries @ beq_entries @ c_addi_entries @ x86_mov_entries @
    x86_fadd_entries @ fadd_s_entries @ fsub_s_entries @ fmul_s_entries @
    fdiv_s_entries @ fadd_d_entries @ fsub_d_entries @ fmul_d_entries @
    fdiv_d_entries @ flw_entries @ fld_entries @ fsw_entries @ fsd_entries @
    sh1add_entries @ sh2add_entries @ sh3add_entries @
    sh1adduw_entries @ sh2adduw_entries @ sh3adduw_entries @ min_entries @
    minu_entries @ max_entries @ maxu_entries @ andn_entries @ orn_entries @
    xnor_entries @ rol_entries @ ror_entries @ clz_entries @ ctz_entries @
    cpop_entries @ sextb_entries @ sexth_entries @ orcb_entries @
    clzw_entries @ ctzw_entries @ cpopw_entries @ brev8_entries @
    rev8_entries @ pack_entries @ packh_entries @ packw_entries @
    zip_entries @ unzip_entries @ rolw_entries @ rorw_entries @
    rori_entries @ roriw_entries @ zext_h_entries @ clmul_entries @
    clmulh_entries @ xperm4_entries @ xperm8_entries @ sha256sum0_entries @
    sha256sum1_entries @ sha256sig0_entries @ sha256sig1_entries @
    sha512sum0_entries @ sha512sum1_entries @ sha512sig0_entries @
    sha512sig1_entries @ sha512sum0r_entries @ sha512sum1r_entries @
    sha512sig0l_entries @ sha512sig1l_entries @ sha512sig0h_entries @
    sha512sig1h_entries @ aes64ds_entries @ aes64dsm_entries @
    aes64es_entries @ aes64esm_entries @ aes64ks2_entries @
    aes64im_entries @ aes64ks1i_entries @ aes32dsi_entries @
    aes32dsmi_entries @ aes32esi_entries @ aes32esmi_entries @
    csrrw_entries @ csrrs_entries @ csrrc_entries @ csrrwi_entries @
    csrrsi_entries @ csrrci_entries @ csrr_entries @ csrw_entries @
    csrs_entries @ csrc_entries @ csrwi_entries @ csrsi_entries @
    csrci_entries @ amoswap_w_entries @ amoadd_w_entries @
    amoxor_w_entries @ amoand_w_entries @ amoor_w_entries @
    amomin_w_entries @ amomax_w_entries @ amominu_w_entries @
    amomaxu_w_entries @ sc_w_entries @ lr_w_entries @
    amoswap_d_entries @ amoadd_d_entries @ amoxor_d_entries @
    amoand_d_entries @ amoor_d_entries @ amomin_d_entries @
    amomax_d_entries @ amominu_d_entries @ amomaxu_d_entries @
    sc_d_entries @ lr_d_entries @ fsgnj_s_entries @ fsgnjn_s_entries @
    fsgnjx_s_entries @ fsgnj_d_entries @ fsgnjn_d_entries @
    fsgnjx_d_entries @ fmin_s_entries @ fmax_s_entries @ fmin_d_entries @
    fmax_d_entries @ fsqrt_s_entries @ fsqrt_d_entries @ fclass_s_entries @
    fclass_d_entries @ fmadd_s_entries @ fmsub_s_entries @
    fnmsub_s_entries @ fnmadd_s_entries @ fmadd_d_entries @
    fmsub_d_entries @ fnmsub_d_entries @ fnmadd_d_entries @ feq_s_entries @
    fle_s_entries @ flt_s_entries @ feq_d_entries @ fle_d_entries @
    flt_d_entries @ fmv_x_w_entries @ fmv_w_x_entries @ fcvt_w_s_entries @
    fcvt_wu_s_entries @ fcvt_s_w_entries @ fcvt_s_wu_entries @ fcvt_w_d_entries @
    fcvt_wu_d_entries @ fcvt_d_w_entries @ fcvt_d_wu_entries @ fcvt_s_d_entries @
    fcvt_d_s_entries @ fcvt_l_d_entries @ fcvt_lu_d_entries @ fcvt_l_s_entries @
    fcvt_lu_s_entries @ fcvt_s_l_entries @ fcvt_s_lu_entries @ fcvt_d_l_entries @
    fcvt_d_lu_entries]. *)

val normalize_entry : Repo.t -> entry -> (Isa_norm_model.form, Tool_error.t) Err.t
(** Finds the real record in the checked-in riscv-opcodes export matching
    [entry.lookup_key] and normalizes it, exactly like
    {!Isa_gen_pilot.normalize_entry} (implemented by delegating to it with a
    freshly built {!Isa_gen_pilot.pilot_entry}) - both [sw] and [beq] are
    long-implemented base forms, so [entry]'s own {!Isa_gen_pilot.pilot_entry}
    citing {!Isa_gen_pilot.Hand_read_table} evidence from
    [riscv_family_encode.ml]'s [store_desc]/[branch_desc] tables is
    documentation, not an enumeration-method review gate. *)

val build : entry -> Isa_norm_model.form -> (Isa_generated_case.case, string) result
(** Renders [form.syntax] with [entry.operands], wraps it as
    [entry.lines_before @ [instruction] @ entry.lines_after] via
    {!Isa_gen_render.render_source_lines}, and builds the
    {!Isa_generated_case.case} through the same oracle/ours/verdict/corpus
    machinery (reused unmodified) that runs the pilot cases. *)
