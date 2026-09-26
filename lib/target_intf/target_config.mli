(** The feature configuration a target assembles under.

    A configuration is the set of optional components (see {!Target_component}) that are enabled.
    It is validated once, at the boundary where external feature names arrive, and is a plain
    value from then on: everything below that boundary asks {!enabled} and never re-parses a name.

    {2 Policy}

    - The default enables every component the target implements, so an unconfigured assembler
      behaves as it always has. It does not claim that any extension is fully implemented: a
      component lists only the forms that exist.
    - A {!spec} is processed left to right. [none] empties the set. [+f] (or a bare [f]) enables
      [f] together with everything it requires, transitively. [-f] disables exactly [f].
    - The result must be closed under requirements and free of conflicts. A contradictory request
      - disabling a feature another enabled feature requires, or enabling two conflicting ones - is
      rejected with an error naming both, rather than silently repaired.
    - Unknown feature names are errors, and list the names the target knows. *)

type t

type item =
  | Enable of string
  | Disable of string
  | None_  (** One step of a {!spec}. [None_] is the [none] keyword. *)

type spec = item list

val default_spec : spec
(** The empty spec: keep the default configuration. *)

val parse_spec : string -> (spec, string) result
(** Parse a comma-separated spec such as ["none,+zmmul"] or ["-x87"]. Whitespace around items is
    ignored; an empty string is {!default_spec}. Feature names are not validated here, since only a
    target knows its own. *)

val spec_to_string : spec -> string

val default : Target_component.t list -> t
(** Every component enabled. *)

val resolve : Target_component.t list -> spec -> (t, string) result
(** Apply a spec to the default configuration of these components. *)

val enabled : t -> string -> bool
(** Whether a feature is on. A name no component declares is never enabled. *)

val features : t -> string list
(** The enabled feature names, sorted: the observable effective configuration. *)

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
