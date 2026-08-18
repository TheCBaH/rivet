(** Case discovery and manifest checking over a fixture or gas-xref corpus. *)

type case = { name : string; root : Fpath.t }

val discover : Fpath.t -> (case list, Tool_error.t) Err.t
(** Every directory under the corpus root that has a [source/] subdirectory, in
    bytewise order. A case is anything with that shape, so adding a fixture is
    adding a directory - there is no list to forget to update.

    An EMPTY discovery is an error, never an empty success: a run that found no
    cases and reported success is the failure mode the fixture scripts already
    guard against. *)

val resolve : Fpath.t -> string -> (case, Tool_error.t) Err.t
(** ONE case, by name. Callers resolve immediately before running each, which is
    what preserves the streaming behavior (P4): `--check return42 nope` prints
    return42's success line FIRST and then fails. *)

val files : Fpath.t -> (string list, Tool_error.t) Err.t
(** Every regular file under a case root except [manifest.txt*]. The glob is
    deliberately wider than [manifest.txt]: --regen writes manifest.txt.new and
    manifest.txt.diff beside the fixtures, and a plain exclusion would record
    the scratch file in the very manifest it is about to become. *)

type finding = Missing of string | Changed of string | Unrecorded of string

val pp_finding : Format.formatter -> finding -> unit

val check : Fpath.t -> Manifest.t -> (int * finding list, Tool_error.t) Err.t
(** Returns the number of records SEEN and every finding, in a deterministic
    order: recorded paths in manifest order, then unrecorded ones in traversal
    order. A file present in the tree but absent from the manifest is as much a
    divergence as a changed one - it would otherwise ride along unrecorded
    forever.

    Membership is exact (D1). The shell asks [grep -q "^sha256:$f\t"], which
    interpolates the path as a REGEX, so a name containing a metacharacter is
    matched loosely. *)

val stem_of_source : string -> (string, Tool_error.t) Err.t
(** The basename without its .c suffix. This is why return42 keeps the
    asm_test_entry.c spelling: CompCert writes the source path into its
    "# Command line:" banner, so renaming the file would change every hash. *)

val previous_and_source : case -> (Manifest.t option * string, Tool_error.t) Err.t
(** The case's committed manifest, if it has one, and the source path to build
    from.

    SOURCE_REL comes from that manifest and only falls back to
    [source/<case>.c] for a case being generated for the first time. Deriving
    it rather than fixing it is what lets return42 keep the asm_test_entry.c
    spelling its committed bytes were generated with: CompCert writes the
    source path into its "# Command line:" banner, so renaming the file would
    change every one of that case's hashes. *)

val source_units : case -> ((string * string) list, Tool_error.t) Err.t
(** M3 (.ai/asm_plan.md §12): the case's [source-unit] records, or [[]] for a
    case with no manifest yet, or one whose manifest predates M3 and carries
    only the single legacy [source] record - a caller reads that case's own
    ONE source through {!previous_and_source} exactly as before, and decides
    its own single-source unit-naming convention itself, unchanged. This
    function answers only "is this case multi-source", not "what is this
    case's source" - the two legacy and multi-source paths stay genuinely
    separate rather than being forced through one fabricated default. *)

val sources : case -> ((string * string) list, Tool_error.t) Err.t
(** Every compilation unit a case has, as [(unit_name, source_rel)], in build
    order. This is the one function fixture tooling should loop over - it
    never returns [[]].

    - A manifest with [source-unit] records: exactly those (via
      {!source_units}).
    - A manifest with only the legacy [source] record: that one source,
      under the case's own name - an already-committed single-source case's
      scope is what its manifest says, regardless of what else later lands
      in [source/].
    - No manifest yet (a case being authored for the first time): every
      [.c] file directly under [source/], sorted. Exactly one file keeps the
      historical single-source convention (unit named after the case, like
      {!previous_and_source}'s fallback); more than one makes this a
      multi-source case from its very first [--regen], with each unit named
      after its own file's stem (via {!stem_of_source}). *)

val unit_stems : case -> ((string * string) list, Tool_error.t) Err.t
(** {!sources}, with each unit's source path already reduced to its stem via
    {!stem_of_source} - what the oracle and exec tooling key a case's
    per-target generated files by (`<target>/<stem>.s`), one call away from
    {!sources} rather than every caller reimplementing the same
    [List.map stem_of_source]. *)
