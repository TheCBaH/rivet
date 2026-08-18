(** The two controlled linker scripts.

    Both place .text/.rodata/.data at the target's oracle addresses, which are
    the same addresses the QEMU manifest uses - so the post-link artifact, the
    bound image and the executed program are all the same program.

    .eh_frame is DISCARDED rather than placed. It is the only thing the .cfi_*
    directives produce, they are dropped at parse, and keeping it would add
    relocations to an output that is compared byte for byte. *)

val oracle : Target.t -> string
(** Wildcard input selection and [ENTRY(asm_test_entry)]: the oracle links the
    fixture object alone, so there is nothing to disambiguate. *)

val exec : Target.t -> string
(** [ENTRY(_start)], the startup stub placed in its own [.start] output section
    below .text, and PER-OBJECT input selection - [start.o(.text.startup)] and
    [fixture.o(.text)] rather than wildcards.

    That distinction is load-bearing: with a wildcard the startup could land in
    the very .text that is about to be compared against the oracle. The discard
    list is the union across targets; every entry is either non-alloc or
    unwind-only, so discarding one a given target never emits is a no-op and
    none of them can move an allocated byte. *)
