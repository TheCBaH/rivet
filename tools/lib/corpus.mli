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
