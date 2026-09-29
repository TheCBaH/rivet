(** Writing a fixture manifest.

    The most important behavior here is that a scope change is INSTALLED and
    THEN reported as a failure. That is how a change to the tested scope is
    forced through review: the new manifest is on disk so the diff can be
    committed deliberately, and the run fails so it cannot pass unnoticed. *)

type outcome = Up_to_date | Changed of string  (** the `diff -u` body, for the caller to report *)

val records :
  case:Corpus.case ->
  targets:Target.t list ->
  compiler:Compiler.t ->
  previous:Manifest.t option ->
  sources:Corpus.unit_source list ->
  (Manifest.record list, Tool_error.t) Err.t
(** Every fallible step is a CHECKED command. The shell's producer wrote `printf
    '...' "$(hash_of ...)"`, where printf's success masked the substitution's
    failure and the record was written with an empty hash.

    [sources] is a case's units from {!Corpus.sources}: its [Compiled] entries
    alone decide [source]/[source-unit:<name>] exactly as before ([Preexisting]
    entries never do - their identity is their own carried- forward
    [origin:<stem>] record, below); exactly one [Compiled] unit emits the legacy
    [source] record, byte-identical to every already-committed single-source
    manifest, more than one emits a [source-unit:<name>] record per unit
    instead.

    [<compiler>-version:<t>] comes from the live compiler when one is installed
    and is otherwise carried forward from [previous] verbatim - `--rehash` runs
    with no compiler at all. The other per-target records are the compiler's own
    {!Compiler.t.provenance}; a bare key (no tab) is a [None] value, as the
    shell's `printf '<compiler>-args:%s\n' "$t"` emitted for an empty argument
    list. The [generator] record is the compiler's own.

    M4 (docs/design.md §12): [abi-version], [supported-targets],
    [expected-value:*], [observation:*] and [origin:*] are author-declared,
    never derived from [sources]/a toolchain/the case's file tree - carried
    forward from [previous] UNCONDITIONALLY (not gated on "no compiler
    installed" the way the compiler version is), since nothing in this function could
    regenerate them if dropped. *)

val write :
  final:Fpath.t ->
  previous:Manifest.t option ->
  Manifest.record list ->
  (outcome, Tool_error.t) Err.t
(** [previous = None] commits and reports [Up_to_date] (P6): the shell's
    `[ -f "$MANIFEST" ] && ! diff ...` short-circuits, so [Changed] is
    unreachable on a first generation.

    A `diff` exit of 2 or more is an OPERATIONAL error and NOTHING is installed
    (D9). The shell's `! diff` is true for both 1 and 2, so a broken diff there
    installs the new manifest and reports a changed scope. *)
