(** Names and relative paths that reach the filesystem or the manifest.

    The manifest has no escape layer on the KEY side, so a TAB or newline in a
    recorded path would not round-trip: it would silently become a different
    record, or two. These are therefore rejected rather than escaped.

    The `source` value matters as much as a case name and is currently
    unvalidated: it is used directly as a copy and compile path, so
    [source=../../x] escapes the case root today (divergence D6). *)

type t

val to_string : t -> string
val pp : Format.formatter -> t -> unit

val parse : string -> (t, Tool_error.t) Err.t
(** One path COMPONENT. Rejects "", ".", "..", and anything containing '/',
    '\t', '\n', '\000' or a leading '-'.

    The leading-dash rule is not cosmetic: a component beginning with '-' is
    reinterpreted as an option by essentially every program the tools invoke. *)

val parse_case_name : string -> (t, Tool_error.t) Err.t
(** As {!parse}, and additionally [[A-Za-z0-9_-]+] - the rule
    asm-fixture-exec.sh already applies to case names. *)

val relative_path : string -> (string, Tool_error.t) Err.t
(** A multi-component path that must stay under its root. Rejects the empty
    string, an absolute path, any "." or ".." component, a component with a
    leading '-', and TAB/newline/NUL anywhere. Returns the path unchanged on
    success, so callers keep the exact spelling the manifest recorded. *)
