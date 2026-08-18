(** [compcert-tools fixture oracle] - the GNU reference-assembler oracle.

    Runs the target's GNU [as] over a committed fixture and records what it
    produced: section headers, symbols, relocations, disassembly, the .text
    bytes, and a controlled reference link. These artifacts are what the
    encoder's differential gate compares against, so they are committed and
    reviewed rather than recomputed on the fly. *)

val run : Repo.t -> targets:Target.t list -> cases:string list -> Command.t
(** Every (target, case) pair, target-major, matching the shell's loop order so
    the transcripts line up. Unlike [verify], a failure STOPS the run: the
    shell calls run_one directly rather than through an accumulator, so
    ordinary errexit is the per-command failure mechanism here. *)
