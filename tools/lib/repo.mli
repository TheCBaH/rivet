(** The repository root, validated by sentinels rather than assumed.

    {b tools/ is a nested dune project, so its %\{workspace_root\} is not the
    repository root.} Feeding it to {!resolve} would fail sentinel validation,
    since all three sentinels are repo-root relative. The repository root
    reaches an integration test only as an explicit argv argument supplied by
    Make. *)

type t

val resolve :
  cli:Fpath.t option -> env:(string -> string option) -> cwd:Fpath.t -> (t, Tool_error.t) Err.t
(** Precedence: [cli] > [RIVET_ROOT] > upward search from [cwd].
    Canonicalizes, then requires the sentinels Makefile,
    scripts/target-matrix.sh and dune-project - all three, because any one alone appears in plenty of
    unrelated trees.

    Tests and launchers never use the cwd fallback: they pass [~cli] or set the
    environment variable, so the root a command operated on is visible in the
    command line rather than inferred. *)

val path : t -> Fpath.t

val with_fixtures : t -> sources:string -> outputs:string -> t
(** The same repository with its fixture corpus elsewhere: [sources] and
    [outputs] are relative to the root. For a consumer whose compiler's output
    lives in a different directory, or whose C sources are a submodule's. *)

val fixture_sources : t -> Fpath.t
(** [fixtures/c/], one directory per case: its C sources and
    [expected-status.txt]. *)

val fixture_compiler_dir : string
(** ["gcc-14"]: the directory under [fixtures/] holding what
    {!Gcc}[.compiler] generated from {!fixture_sources}. Bump it with the
    toolchain's major version. *)

val fixture_corpus : t -> Corpus.corpus
(** The sources and the generated outputs of the fixture cases. *)

val gas_xref_corpus : t -> Fpath.t

val isa_generated_corpus : t -> Fpath.t
(** [fixtures/isa-generated/] ({!Isa_generated_case.fixture_dir_name}),
    the pilot differential generator's own corpus - never
    {!gas_xref_corpus}'s tree. *)

val isa_difficult_corpus : t -> Fpath.t
(** [fixtures/isa-difficult/] ({!Isa_gen_difficult.fixture_dir_name}), the
    non-frozen difficult-form corpus - never {!isa_generated_corpus}'s
    tree, which stays the frozen 21-entry pilot. *)

val isa_data_riscv_opcodes : t -> Fpath.t
(** [vendor/isa-data/riscv-opcodes/upstream], the vendored submodule
    {!Isa_inventory_riscv} reads (docs/isa-inventory.md). *)

val isa_data_xed_upstream : t -> Fpath.t
(** [vendor/isa-data/xed/upstream], the submodule root - use this to
    resolve its pinned commit; {!isa_data_xed} below is the [datafiles/]
    subdirectory {!Isa_inventory_xed} actually reads. *)

val isa_data_xed : t -> Fpath.t
(** [vendor/isa-data/xed/upstream/datafiles], the vendored submodule
    {!Isa_inventory_xed} reads (docs/isa-inventory.md). *)

val isa_inventory : t -> Target.t -> Fpath.t
(** [fixtures/isa-inventory/<target>/], the whole-ISA inventory
    manifest/summary destination for that target. *)

val isa_db_export : t -> source:string -> Target.t -> Fpath.t
(** [isa-db/export/<source>/<target>.jsonl], the checked-in JSON Lines export
    the standalone [isa-db/] Python project writes.
    [source] is ["riscv_opcodes"] or ["xed"], matching isa-db's own naming -
    not a {!Target.t}, since it is not one of the six targets. Consumed by
    {!Isa_db_cross_validate}. *)
