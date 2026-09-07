(** Phase A of the whole-ISA x86 inventory (asm/docs/isa-inventory.md):
    mnemonic + extension only, mechanically read from the vendored Intel XED
    submodule ({!Repo.isa_data_xed}). Phase B fields are not computed here,
    exactly as {!Isa_inventory_riscv}. See asm/docs/isa-inventory.md's "XED's
    data shape, verified against the vendored checkout" for the format this
    module implements against - it is messier than riscv-opcodes' and this
    is the ONE place that should encode that knowledge. *)

type entry = { mnemonic : string; extension : string; applies_32 : bool; applies_64 : bool }
(** One [ICLASS] block, aggregated over every [PATTERN] line it carries.
    [applies_32]/[applies_64] are true iff at least one of the block's own
    [PATTERN] lines lacks the token that would exclude that mode ([mode64]
    excludes [x86_32], [not64] excludes [x86_64]) - never computed from a
    single [PATTERN] line in isolation, since one block can carry several
    with different restrictions. *)

val list_extension_dirs : Repo.t -> (string list, Tool_error.t) Err.t
(** Every subdirectory of [datafiles/], sorted by name. Unlike
    {!Isa_inventory_riscv.list_extension_files}, nothing is excluded by name
    here: roughly a quarter of these hold no instruction data at all (chip
    codenames, shared type/register definitions) and contribute no entries,
    which {!parse_datafile} discovers by content. [datafiles/]'s own loose
    files (the base ISA, e.g. [xed-isa.txt]) are not a subdirectory and so
    are not listed here - see {!entries_for_target}. *)

val parse_datafile : extension:string -> string -> (entry list, Tool_error.t) Err.t
(** One data file's own text (already read) into its entries, keyed by
    [extension] (the caller's directory name, exactly as
    {!Isa_inventory_riscv.parse_extension_file} keys by its file name).

    Recognizes both XED dialects sharing one directory: a one-[PATTERN]
    "generated" block and a hand-authored block with several [PATTERN]/
    [OPERANDS] pairs under one [ICLASS] - blank lines inside a block are
    ordinary formatting, not block separators. A [{ ... }] block with no
    [ICLASS] field (a handful of raw pattern/macro blocks) contributes
    nothing rather than being rejected. Field lines are recognized by their
    leading keyword regardless of the gutter width before their [:]
    (["ICLASS    : X"] and ["ICLASS: X"] both parse).

    [UDELETE] (a handful of cross-file directives suppressing another file's
    block by its [UNAME] at XED's own build time) is NOT honored: this may
    include a small number of (mnemonic, extension) pairs a strict XED build
    would suppress. Deliberate and documented, not a silent gap - at Phase
    A's coarse granularity and this directive's rarity (~10 occurrences
    project-wide against ~7800 ICLASS blocks) the mnemonic in question
    almost always survives under the same or another block regardless. *)

val entries_for_target : Repo.t -> Target.t -> (entry list, Tool_error.t) Err.t
(** Every entry across every extension directory - each read recursively, so
    a nested subdirectory like [amd/amdxop/] is reached too - plus
    [datafiles/]'s own loose files (extension ["base"]; see the "XED's data
    shape" section of asm/docs/isa-inventory.md), whose {!entry.applies_32}
    (for [x86_32]) or {!entry.applies_64} (for [x86_64]) holds, sorted and
    deduplicated by [(mnemonic, extension)] - the same operand-form collapse
    {!Isa_inventory_riscv.entries_for_target} performs, for the same reason:
    Phase A does not distinguish encoding forms. Any target other than
    [x86_32]/[x86_64] returns [[]]. *)

val render_manifest : source_commit:string -> Target.t -> entry list -> string
(** asm/docs/isa-inventory.md's manifest.txt format, source [xed]. Takes the
    already-target-filtered {!entries_for_target} result, not raw entries -
    it renders [mnemonic]/[extension] only, the same shape
    {!Isa_inventory_riscv.render_manifest} produces. *)

val render_summary : Target.t -> entry list -> string
(** asm/docs/isa-inventory.md's summary.txt format. *)
