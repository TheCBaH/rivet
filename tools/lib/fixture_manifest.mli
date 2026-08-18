(** Writing a fixture manifest.

    The most important behavior here is that a scope change is INSTALLED and
    THEN reported as a failure. That is how a change to the tested scope is
    forced through review: the new manifest is on disk so the diff can be
    committed deliberately, and the run fails so it cannot pass unnoticed. *)

type outcome = Up_to_date | Changed of string  (** the `diff -u` body, for the caller to report *)

val records :
  case:Corpus.case ->
  targets:Target.t list ->
  work_root:Fpath.t ->
  previous:Manifest.t option ->
  sources:(string * string) list ->
  (Manifest.record list, Tool_error.t) Err.t
(** Every fallible step is a CHECKED command. The shell's producer wrote
    `printf '...' "$(hash_of ...)"`, where printf's success masked the
    substitution's failure and the record was written with an empty hash.

    [sources] is a case's units from {!Corpus.sources}: exactly one emits the
    legacy [source] record, byte-identical to every already-committed
    single-source manifest; more than one emits a [source-unit:<name>] record
    per unit instead.

    [ccomp-version:<t>] comes from the live compiler when one is installed and
    is otherwise carried forward from [previous] verbatim - `--rehash` runs with
    no compiler at all.

    [ccomp-args:<t>] and [ccomp-configure-args:<t>] emit a BARE KEY with no tab
    when the argument list is empty, because `printf 'ccomp-args:%s\n' "$t"`
    does. *)

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
