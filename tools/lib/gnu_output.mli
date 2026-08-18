(** Parsers for GNU binutils output.

    The binutils themselves stay external, exactly as {!Tool_process} keeps
    [diff] external (P1): their output IS the committed artifact for
    readelf.txt and objdump.txt, and no reimplementation could be byte-exact
    against a moving target. What moves here is the AWK that reads that output
    and renders reloc.txt, symbols.txt and the allocated-section checks.

    Every function takes the tool's captured stdout as a string and is
    therefore pure - which is what lets the unit tests run against recorded
    output with no cross toolchain, while the differential gate re-renders all
    36 committed trees with the real tools. *)

val alloc_sections : string -> (string list, Tool_error.t) Err.t
(** Every SHF_ALLOC section named by [objdump -h] output, in file order.

    [objdump -h] rather than [readelf -SW] because readelf's flag column is
    EMPTY for an unflagged section, which shifts every later field and turns
    "does this line say ALLOC" into a question about column positions. objdump
    prints the flags on their own line after each header, so ALLOC is either
    present or it is not. *)

val section_size : string -> name:string -> string option
(** The size field, as objdump's own hex string, or [None] when the section is
    absent. Used for the .start provenance check, where the comparison is
    between two objdump spellings and so never needs the numeric value. It is
    also a NOBITS section's only source of size (M3 §11,
    .ai/asm_plan.md §12): objdump reports its full logical extent here even
    though there is nothing to copy out of it. *)

val section_is_nobits : string -> name:string -> bool
(** Whether an allocated section carries no [CONTENTS] flag in [objdump -h] -
    a NOBITS/[.bss]-shaped section, which [objcopy] always extracts as ZERO
    bytes regardless of {!section_size}, and which therefore has no byte
    artifact of its own. [false] for a section [objdump] does not list at all
    - a caller checks presence via {!section_size} or {!alloc_sections}
    first, exactly as it already must. Measured directly (real
    [objdump -h]): a NOBITS section's flag line reads plain [ALLOC], where a
    PROGBITS one reads [CONTENTS, ALLOC, ...]. *)

val has_rela : string -> bool
(** Whether [readelf -SW] output lists any [.rela.*] section.

    This cannot be read off [objdump -r]: its banner names the section the
    relocations APPLY TO (.text), not the one they live in (.rela.text). The
    distinction is not decoration - it is the difference between "the addend is
    0" and "the addend is in the instruction field", which is what recovering
    an ARM split immediate depends on. *)

val relocations : objdump_r:string -> rela:bool -> (string, Tool_error.t) Err.t
(** The rendered reloc.txt body, sorted, one tab-separated row per record:
    [section, 0x<offset padded to 8>, kind, type, symbol, addend].

    Records are kept only for .text, .rodata and .data. That allowlist is what
    drops the .eh_frame records the .cfi_* directives produce - they are
    discarded at parse and never reproduced, so recording them would compare
    bytes nothing will ever generate.

    Offsets are zero-padded to 8 so a plain bytewise sort agrees with address
    order and a 32- and a 64-bit target spell the same offset identically.

    The addend is [-] on REL targets, where it lives in the instruction field
    rather than the record; recovering it is per-relocation-kind work and
    belongs to the reader. Where RELA carries one it is kept in signed hex
    exactly as objdump spelled it, which [Int64.of_string] reads directly. *)

val symbols : readelf_s:string -> (string, Tool_error.t) Err.t
(** The rendered symbols.txt body: [name, binding, visibility, ndx, defined],
    lower-cased, sorted and deduplicated.

    File symbols (a name ending [.c] or [.s]) and the unnamed index-0 entry are
    dropped. Mapping symbols ([$d], [$x]) and section symbols are KEPT - they
    are facts about what the assembler emitted, and two entries differing only
    in [ndx] are two distinct rows that dedup must not merge. *)
