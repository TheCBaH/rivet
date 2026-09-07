(** Frozen case/artifact/verdict schema for the pilot GAS differential
    generator: a third data layer ("test cases and observations") with a
    per-case retention list and an outcome table.

    This is a schema freeze, not an implementation: later work generates
    real {!case} values, invokes GNU tools (reusing {!Gnu_tools}) to produce
    {!artifact} values, and classifies the result into a {!verdict}. A case
    references a normalized form only by its [form_id]/[source_record_ids]
    strings, never by importing {!Isa_norm_model} directly - the three data
    layers stay independently inspectable/serializable.

    This also freezes the concrete tier names decided
    here rather than left to whichever later task first needs one - see
    {!fixture_dir_name}, {!cli_group_name}, {!make_target}, and
    {!joins_prerequisite_of}. *)

type case = {
  case_id : string;  (** stable within one target, e.g. ["riscv32:sw:imm-zero"] *)
  target : Target.t;
  form_id : string;  (** the {!Isa_norm_model.form}[.form_id] this case exercises *)
  source_record_ids : string list;  (** provenance, mirrors the owning form's own field *)
  rule_ids : string list;
      (** which coverage obligation(s) this case satisfies ("obligations
          reached"), e.g. ["boundary-zero-immediate"] *)
  operands : (string * string) list;  (** concrete operand name -> its rendered text *)
  rendered_source : string;  (** the exact GAS input text *)
  configuration : string list;
      (** the resolved argv fragment selecting mode/features, e.g.
          [ [ "-march=rv32i_zca_zcb"; "-mabi=ilp32"; "-mno-relax" ] ] *)
  negative : bool;  (** a must-reject case, per the negative row below *)
}

type artifact = {
  tool_label : string;
      (** identifies BOTH the tool and its version, e.g. ["riscv32-gas-2.43.1"]
          or ["ours-<git-rev>"] - required to be resolved via
          {!Gnu_tools.version_line} at generation time, never assumed from
          an earlier measured snapshot (RV32 2.43.1 / RV64 2.44
          skew), so the coverage matrix can
          key on (target, tool_label) and attribute a gap to tool version
          rather than to an architectural difference between the two RISC-V
          profiles. *)
  argv : string list;
  exit_status : Process_status.t;
  stdout : string;
  stderr : string;
  bytes : string option;  (** hex-encoded assembled section bytes; [None] if rejected *)
  relocations : string list;  (** relocation evidence *)
}

(** The outcome table, one constructor per row, in the table's own
    order. *)
type verdict =
  | Pass  (** row 1: both accept, bytes agree - credit only the observed form/obligations *)
  | Byte_mismatch  (** row 2: both accept, bytes differ - hard failure, including frontier cases *)
  | Regression  (** row 3: our assembler rejects a promoted case *)
  | Frontier_gap
      (** row 4: our assembler rejects an explicitly unimplemented form; no positive credit *)
  | Gas_rejected_valid_case
      (** row 5: GAS rejects a supposedly valid case - investigate the recipe/constraints/flags/tool
          capability; never blanket-skip *)
  | Oracle_unavailable of { probe : string }
      (** row 6: a known unavailable GAS capability, with probe/version evidence *)
  | Blocked_unknown_requirement of { rule : string }
      (** row 7: an unknown normalization constraint blocked positive generation; [rule] names the
          missing rule *)
  | Negative_case_accepted  (** row 8: a negative case was accepted unexpectedly - a failure *)

val verdict_description : verdict -> string
(** One sentence transcribing the outcome table's own wording for that row,
    so a report can render a verdict without a reader needing to consult the
    table. *)

type observation = { case : case; gas : artifact option; ours : artifact option; verdict : verdict }
(** [gas]/[ours] are [None] exactly when generation was blocked before that
    tool ran: {!Blocked_unknown_requirement} always carries [gas = None;
    ours = None] (generation itself was blocked); {!Oracle_unavailable}
    always carries [gas = None] (GAS was never invoked, only probed). *)

(** {1 Frozen execution tiers} *)

type tier =
  | Offline_consumer  (** checked-in cases/artifacts only; no Python, toolchain, or QEMU *)
  | Gnu_regeneration  (** invokes the real GNU cross toolchains to regenerate artifacts *)
  | Producer_update  (** the existing Python producer lane; untouched by this work *)

val fixture_dir_name : string
(** ["isa-generated"] under [asm/fixtures/] - this generator's OWN corpus
    (the existing [gas-xref regen] recreates its own tree and
    should not own this corpus), never shared with gas-xref's. *)

val cli_group_name : string
(** ["isa-generated"], the planned [compcert_tools.exe isa-generated
    check|regen] subcommand group, mirroring the existing [gas-xref]/
    [fixture] groups. *)

val make_target : tier -> string option
(** The planned Make target name for a tier, following this repository's
    existing [asm-<thing>-check]/[asm-<thing>-regen] convention (compare
    [asm-gas-xref-check]/[asm-gas-xref-regen]):
    - {!Offline_consumer}: [Some "asm-isa-generated-check"]
    - {!Gnu_regeneration}: [Some "asm-isa-generated-regen"]
    - {!Producer_update}: [None] - that tier is the existing
      [python3 -m unittest discover -s isa-db/tests -t isa-db] lane, which
      this work does not rename. *)

val joins_prerequisite_of : tier -> string option
(** The exact existing Makefile target {!make_target}'s check should be
    ADDED as a prerequisite of, once this generator lands real content:
    - {!Offline_consumer}: [Some "asm-test"] - joining
      [asm-fixtures-check]/[asm-gas-xref-check] there (root Makefile's
      [asm-test] rule), the toolchain-free discipline that also lets it
      gate [asm-ci] transitively.
    - {!Gnu_regeneration}: [None] - deliberately kept off that critical
      path, like the [asm-fixture-oracle-*] targets.
    - {!Producer_update}: [None] - the separate Python lane,
      outside [asm-ci] entirely. *)
