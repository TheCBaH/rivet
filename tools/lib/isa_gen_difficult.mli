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
