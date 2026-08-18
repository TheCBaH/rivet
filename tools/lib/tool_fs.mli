(** Traversal, reading, hashing, and the staged-write discipline. *)

val files : root:Fpath.t -> exclude:(string -> bool) -> (string list, Tool_error.t) Err.t
(** Regular files, decided by [lstat] - a symlink is not a file, matching
    `find -type f`, and a symlinked directory is never descended. Results are
    root-relative, "/"-joined, and sorted by [String.compare], which is
    byte-wise and therefore identical to `LC_ALL=C sort`. *)

val read : Fpath.t -> (string, Tool_error.t) Err.t
val write : Fpath.t -> string -> (unit, Tool_error.t) Err.t

val mkdir_p : Fpath.t -> (unit, Tool_error.t) Err.t
(** For scratch paths, which are plain [Fpath.t] rather than a workspace
    capability - the capability types exist to guard the ROOTS the tools own,
    and a private temporary directory is not one of them. *)

val copy : src:Fpath.t -> dst:Fpath.t -> (unit, Tool_error.t) Err.t
val same_bytes : Fpath.t -> Fpath.t -> (bool, Tool_error.t) Err.t
val sha256 : Fpath.t -> (string, Tool_error.t) Err.t

(** {1 Staged writes}

    write_manifest creates TWO siblings - manifest.txt.new and
    manifest.txt.diff - and both are covered by the manifest.txt* traversal
    glob. Only the first may ever be installed. Distinct stage types make that a
    compile-time property instead of a runtime check: [commit] takes no stage
    argument, and {!auxiliary_stage} has no path into it, so committing a .diff
    is not expressible. *)

type install_stage
type auxiliary_stage

val install_path : install_stage -> Fpath.t
val auxiliary_path : auxiliary_stage -> Fpath.t

val with_staging :
  final:Fpath.t ->
  install_suffix:string ->
  auxiliary_suffixes:string list ->
  (install:install_stage ->
  auxiliary:(string -> (auxiliary_stage, Tool_error.t) Err.t) ->
  write_install:(string -> (unit, Tool_error.t) Err.t) ->
  write_auxiliary:(auxiliary_stage -> string -> (unit, Tool_error.t) Err.t) ->
  commit:(unit -> (unit, Tool_error.t) Err.t) ->
  ('a, Tool_error.t) Err.t) ->
  ('a, Tool_error.t) Err.t
(** Names are deterministic ([final ^ suffix]), not mkstemp: the ".new"
    spelling is part of the diagnostic contract, since the shell's own diff
    output names it.

    Suffix declarations are validated - each non-empty, mutually distinct, free
    of path separators and NUL, and none equal to "" (which would alias [final]
    itself). Without that check a caller could declare two aliases for one
    sibling, or escape the final's directory.

    [commit] preserves an existing file's mode, and uses 0644 for a new one
    (divergence D13: the shell's `mv` installs whatever the umask produced,
    effectively resetting mode on every write).

    Cleanup runs on every path, including exceptions, and tolerates the
    installed stage already being absent. *)

val remove_matching : Fpath.t -> suffix:string -> (unit, Tool_error.t) Err.t
(** Deletes the immediate children of [dir] whose names end in [suffix].

    Non-recursive and non-creating: a missing directory is not an error, since
    the caller has just ensured it. This exists for the oracle's stale-dump
    problem - a section that stops being emitted must not leave its previous
    .hex behind, where the manifest would no longer name it but the
    traversal-based corpus check still would. *)

val remove_tree : Fpath.t -> (unit, Tool_error.t) Err.t
(** Recursive delete, tolerating an absent path. Never follows a symlinked
    directory - it unlinks the link itself, so a symlink planted inside a work
    root cannot be used to delete outside it. *)
