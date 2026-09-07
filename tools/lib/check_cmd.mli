(** The two toolchain-free check commands.

    Their output is byte-identical to the shell they replaced - the ten
    characterization snapshots recorded from that shell before the port existed
    still reproduce exactly, and asm-characterize-verify is what says so. *)

val fixture_check : Repo.t -> cases:string list -> Command.t
(** [cases = []] means every discovered case. Cases are resolved and checked ONE
    AT A TIME, immediately before running each, which is what preserves the
    streaming order (P4): a valid case's success line is printed before a later
    unknown case's failure. *)

val gas_xref_check : Repo.t -> Command.t
(** [present = 0] is an error, not "no unrecorded files". The shell counts for
    the same reason: its scan runs in a process substitution where a failure
    would not reach `set -e`, so the loop would simply not execute and --check
    would pass having compared nothing in that direction. *)

val isa_generated_check : Repo.t -> Command.t
(** {!Isa_generated_case.Offline_consumer} tier: for every
    {!Isa_gen_pilot.all} entry, rebuild its case offline (no toolchain),
    require a matching committed {!Isa_generated_corpus.record} in the
    checked-in corpus, and {!Isa_generated_corpus.replay} it - never invoking
    a real assembler. Also rejects a committed record that no pilot manifest
    entry produces (a stale corpus after the manifest shrinks). *)

val isa_difficult_check : Repo.t -> Command.t
(** The same {!Isa_generated_case.Offline_consumer}-tier replay as
    {!isa_generated_check}, generalized over {!Isa_gen_difficult.all}/
    [normalize_entry]/[build] and the separate
    [asm/fixtures/isa-difficult/cases.jsonl] corpus, so growing this
    non-frozen manifest can never touch the frozen pilot corpus. *)

val targets : Target.capability -> Command.t
