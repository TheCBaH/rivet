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

val all : entry list
(** [sw_entries @ beq_entries @ c_addi_entries @ x86_mov_entries @
    x86_fadd_entries]. *)

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
