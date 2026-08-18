(** The tool gate's per-target reference snippet.

    A freestanding [_start] that issues [exit_group] directly - no libc, no crt,
    no dynamic loader - so what it exercises is the emulator and the cross
    binutils, and nothing this repository builds can make it pass. *)

val source : Target.t -> string
(** The assembly text, byte-identical to the shell's [snippet_for]. *)

val expected_status : Target.t -> int
(** The status the snippet exits with, DISTINCT per target (11-16). A shared
    value would let the wrong emulator, or a stale artifact from another target,
    pass the run check. *)
