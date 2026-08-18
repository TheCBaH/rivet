(** Capabilities for the roots the tools work in.

    {b Construction never creates anything.} Resolving configuration must not
    mutate the tree: asking whether an installed ccomp exists does not create
    .fixture-work today, a missing-compiler failure must not leave a new work
    root behind, and the Phase 2 check commands are read-only. So constructors
    validate lexical form, inspect the nearest existing ancestor, and return a
    capability for a possibly-absent path. Only {!ensure}, the [recreate_*]
    functions and staged writes create anything.

    The three types are distinct so that "delete and recreate this" cannot be
    applied to a corpus. There is no function that deletes a {!read_root}. *)

type read_root
type child_owned
type recreatable_root

val read_path : read_root -> Fpath.t
val recreatable_path : recreatable_root -> Fpath.t
val child_path : child_owned -> Fpath.t

(** {1 Repository-owned roots}

    These must EQUAL the canonical location - no override exists, and accepting
    one would let a corpus check silently run against a different corpus. *)

val fixture_corpus : Repo.t -> read_root
val gas_xref_corpus : Repo.t -> recreatable_root

(** {2 Environment-selected roots}

    External roots are supported (P5): FIXTURE_WORK and friends may point
    outside the repository, and the scripts allow that today. But they must be
    absolute, must not be "/", $HOME, the repository root or a corpus root, must
    contain no ".." and no symlinked component (divergence D11 - the shell uses
    the value with zero validation, so relative and symlinked roots work today).

    A symlinked component is rejected rather than resolved because these roots
    are recreated: [rm -rf] through a symlink deletes something the caller did
    not name. *)

val fixture_work : Repo.t -> env:(string -> string option) -> (read_root, Tool_error.t) Err.t
val exec_artifacts : Repo.t -> env:(string -> string option) -> (read_root, Tool_error.t) Err.t

val tool_gate_work :
  Repo.t -> env:(string -> string option) -> (recreatable_root, Tool_error.t) Err.t

(** {3 Children and creation} *)

val child : read_root -> string list -> (child_owned, Tool_error.t) Err.t
(** Every component must parse as an {!Identifier.t}, and the result must stay
    beneath its root after resolution. *)

val ensure : child_owned -> (unit, Tool_error.t) Err.t
val recreate_child : child_owned -> (unit, Tool_error.t) Err.t
val recreate_root : recreatable_root -> (unit, Tool_error.t) Err.t

val with_scratch : label:string -> (Fpath.t -> ('a, Tool_error.t) Err.t) -> ('a, Tool_error.t) Err.t
(** A private temporary directory, removed on every exit path including an
    exception. *)
