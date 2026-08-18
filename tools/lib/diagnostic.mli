(** One line (or block) of ordinary program output, tagged with its stream.

    Findings - MISSING, CHANGED, UNRECORDED, the manifest-changed notice and its
    diff - are output, not errors. They go to stderr and they must NEVER acquire
    a "FATAL:" prefix, because the shell does not put one there. *)

type stream = Stdout | Stderr
type t = private { stream : stream; text : string }

val stdout : string -> t
val stderr : string -> t

(** [text] may contain internal newlines. Exactly one trailing newline is
    stripped at construction and re-added by the renderer, so a `diff -u` body
    (which ends in one) and a bare finding line behave identically and neither
    can produce a doubled blank line. *)

val pp : Format.formatter -> t -> unit
