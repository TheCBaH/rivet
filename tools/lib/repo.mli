(** The repository root, validated by sentinels rather than assumed.

    {b %\{workspace_root\} is NOT the repository root.} The only dune-project on
    the path is asm/dune-project, so a build run from asm/ roots the workspace
    there - which is why asm/test/differential/dune spells its corpus as
    %\{workspace_root\}/fixtures/... Feeding %\{workspace_root\} to {!resolve}
    would fail sentinel validation, since all three sentinels are repo-root
    relative. The repository root reaches an integration test only as an
    explicit argv argument supplied by Make. *)

type t

val resolve :
  cli:Fpath.t option -> env:(string -> string option) -> cwd:Fpath.t -> (t, Tool_error.t) Err.t
(** Precedence: [cli] > [COMPCERT_REPO_ROOT] > upward search from [cwd].
    Canonicalizes, then requires the sentinels Makefile, tools/target-matrix.sh
    and asm/dune-project - all three, because any one alone appears in plenty of
    unrelated trees.

    Tests and launchers never use the cwd fallback: they pass [~cli] or set the
    environment variable, so the root a command operated on is visible in the
    command line rather than inferred. *)

val path : t -> Fpath.t
val fixture_corpus : t -> Fpath.t
val gas_xref_corpus : t -> Fpath.t
val corpus_work : t -> Fpath.t

val corpus_c : t -> Target.t -> Fpath.t
(** [asm/fixtures/corpus/c/<target>/], the classify-c manifest/summary
    destination for that target. *)

val corpus_c_assemble : t -> Target.t -> Fpath.t
(** [asm/fixtures/corpus/c-assemble/<target>/], the assemble-c manifest/summary
    destination for that target - separate from {!corpus_c} so a classify-c
    regression and an assemble-c regression are never conflated. *)

val corpus_regression : t -> Target.t -> Fpath.t
(** [asm/fixtures/corpus/regression/<target>/], the classify-regression
    manifest/summary destination for that target - CompCert's
    [test/regression/] suite, classified the same way {!corpus_c} classifies
    [test/c/]. *)

val corpus_compression : t -> Target.t -> Fpath.t
(** [asm/fixtures/corpus/compression/<target>/], the classify-compression
    manifest/summary destination for that target - CompCert's
    [test/compression/] suite. *)
