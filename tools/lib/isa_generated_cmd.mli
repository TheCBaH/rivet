(** The CLI-facing command for {!Isa_gen_pilot}/{!Isa_gen_case_build}/
    {!Isa_gen_oracle}: the
    {!Isa_generated_case.Gnu_regeneration} tier named
    [asm-isa-generated-regen]. Needs the real cross GNU binutils
    for every pilot target; never part of the toolchain-free
    [tools-test]/[tools-integration] suites. *)

val regen : Repo.t -> Command.t
(** Build and run one canonical case for every {!Isa_gen_pilot.all} entry,
    reporting each outcome as one line: assembled bytes matching the
    normalized encoding, assembled but with a DIFFERENT observed form (a
    recorded finding, not necessarily a bug), or rejected by GAS. Also
    writes every successfully-built case's {!Isa_generated_corpus.record} to
    the checked-in [asm/fixtures/isa-generated/cases.jsonl] corpus,
    which {!Check_cmd.isa_generated_check} replays offline without a
    toolchain. *)
