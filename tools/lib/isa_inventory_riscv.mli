(** Phase A of the whole-ISA RISC-V inventory (asm/docs/isa-inventory.md):
    mnemonic + extension only, mechanically read from the vendored
    [riscv-opcodes] submodule ({!Repo.isa_data_riscv_opcodes}). Phase B fields
    (compcert-emitted, gcc-emitted, implementation state) are not computed
    here - see that document's "Follow-up: sourcing Phase B" - every entry
    carries them as ["unknown"] and a deferral/promotion field of ["-"]. *)

type entry = { mnemonic : string; extension : string }

val list_extension_files : Repo.t -> (string list, Tool_error.t) Err.t
(** Every ratified extension file directly under [extensions/], sorted by
    name. [extensions/unratified/] is deliberately excluded: this project's
    parser targets what real toolchains accept, and an unratified extension
    is not yet part of that by definition - a candidate for its own,
    separately-decided track, not silently folded into this one. *)

val parse_extension_file : name:string -> string -> (entry list, Tool_error.t) Err.t
(** One extension file's own text (already read, [name] is the file's own
    name for error messages and as the {!entry.extension} of every line
    it defines) into its entries.

    - Blank and [#]-comment lines are skipped.
    - [$import <extension>::<mnemonic>] lines are skipped: they declare a
      composite extension's dependency on a mnemonic that another file
      already defines, not a new one - counting it here would double-count
      the mnemonic under two extensions.
    - [$pseudo_op <extension>::<mnemonic> <pseudo-mnemonic> ...] lines
      contribute one entry for the pseudo-mnemonic (third field), keyed by
      THIS file's own name - the file the pseudo-instruction is spelled in -
      not the extension of the base instruction it expands to.
    - Every other non-blank line contributes one entry for its first
      whitespace-separated field, keyed by THIS file's own name. *)

val applies_to : Target.t -> extension:string -> bool
(** [riscv32]/[riscv64] only; any other target is [false]. An [rv_*] file
    applies to both; [rv32_*]/[rv64_*] apply to the matching one only - the
    convention every ratified extension file name already follows. *)

val entries_for_target : Repo.t -> Target.t -> (entry list, Tool_error.t) Err.t
(** Every entry from every applicable extension file, sorted and deduplicated
    by [(mnemonic, extension)] for a stable, reviewable diff. A source file
    can legitimately spell the same mnemonic more than once for distinct
    operand-count forms (e.g. [rv_i]'s two-operand [jal rd, offset] and its
    own one-operand pseudo-op alias for [rd=x1]) - Phase A's granularity does
    not distinguish operand forms, so these collapse to one record rather
    than being treated as a data error. *)

val render_manifest : source_commit:string -> Target.t -> entry list -> string
(** [asm/docs/isa-inventory.md]'s manifest.txt format, source [riscv-opcodes]. *)

val render_summary : Target.t -> entry list -> string
(** [asm/docs/isa-inventory.md]'s summary.txt format. *)
