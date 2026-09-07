(** The CLI-facing command for {!Isa_gen_difficult}: the
    [Gnu_regeneration]-tier command named
    [Isa_gen_difficult.regen_make_target] ([asm-isa-difficult-regen]). Needs
    the real cross GNU binutils for every difficult-corpus target; never part
    of the toolchain-free [tools-test]/[tools-integration] suites. *)

val regen : Repo.t -> Command.t
(** Build and run one case for every {!Isa_gen_difficult.all} entry, via the
    same {!Isa_gen_drive.run_case} driving logic
    {!Isa_generated_cmd.regen} uses for the frozen pilot. Also writes
    every successfully-built case's {!Isa_generated_corpus.record} to the
    checked-in [asm/fixtures/isa-difficult/cases.jsonl] corpus, which
    {!Check_cmd.isa_difficult_check} replays offline without a toolchain. *)
