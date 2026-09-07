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

val all : entry list
(** [sw_entries @ beq_entries @ c_addi_entries @ x86_mov_entries @
    x86_fadd_entries @ fadd_s_entries @ fsub_s_entries @ fmul_s_entries @
    fdiv_s_entries @ fadd_d_entries @ fsub_d_entries @ fmul_d_entries @
    fdiv_d_entries @ sh1add_entries @ sh2add_entries @ sh3add_entries @
    sh1adduw_entries @ sh2adduw_entries @ sh3adduw_entries @ min_entries @
    minu_entries @ max_entries @ maxu_entries @ andn_entries @ orn_entries @
    xnor_entries @ rol_entries @ ror_entries @ clz_entries @ ctz_entries @
    cpop_entries @ sextb_entries @ sexth_entries @ orcb_entries @
    clzw_entries @ ctzw_entries @ cpopw_entries @ brev8_entries @
    rev8_entries @ pack_entries @ packh_entries @ packw_entries @
    zip_entries @ unzip_entries @ rolw_entries @ rorw_entries @
    rori_entries @ roriw_entries @ zext_h_entries @ clmul_entries @
    clmulh_entries @ xperm4_entries @ xperm8_entries]. *)

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
