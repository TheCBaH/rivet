(** Cross-validation only, deliberately not a replacement for
    {!Isa_inventory_riscv}/{!Isa_inventory_xed}. For each of the four
    (source, profile) pairs, reads the checked-in
    [asm/fixtures/isa-inventory/<target>/manifest.txt] and the checked-in
    [isa-db/export/<source>/<target>.jsonl] (the standalone Python isa-db/
    project's export), and asserts every [(mnemonic, extension)] manifest
    row has a matching isa-db source record for that profile.

    This is strictly stronger evidence than [tools-isa-inventory-diff], which
    only proves the OCaml generator reproduces itself, not that its output
    agrees with an independently-sourced dataset (isa-db reads the same two
    vendored submodules through its own, separately-written adapters). It is
    one-directional on purpose: isa-db's richer records ([$import] relations,
    per-block XED provenance) are not required to have a manifest
    counterpart, only the reverse. Read-only: never regenerates or writes
    either side, and reads no submodule directly - only the two checked-in
    artifacts. *)

val check : Repo.t -> Command.t
