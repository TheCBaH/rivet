(** The freestanding [_start] stub, per target.

    Same shape and the same verified syscall numbers as asm-tool-gate.sh's
    snippet_for, except that here the fixture's return value becomes the
    exit_group argument rather than a constant. On arm, aarch64 and both RISC-V
    profiles the return register already IS the first argument register, so
    only x86 needs a move.

    Two things are per-target rather than templated, and both are correctness
    rather than taste: ARM's comment character is [@], so its section flags must
    be spelled [%progbits] where the others use [@progbits]; and x86 needs the
    stack aligned before the call, while the other four inherit a 16-byte
    aligned sp from qemu. *)

val source : Target.t -> string
