(** Diagnostics: the user-facing presentation of a failure.

    A diagnostic is a rendering, not an error. The error itself is a typed value in some module's
    error domain (see [asm/docs/errors.md]); {!of_error} is where one becomes the other, and it is
    called once, at the reporting boundary, rather than at every layer that passes a failure along.

    Rendering is deterministic and free of host paths, locale, and process state
    ([.ai/asm_plan.md] §3.7), because these strings go straight into expect output that must match
    byte-for-byte across native OCaml, js_of_ocaml, and Melange. *)

type severity = Error | Warning | Note

type t
(** A rendered diagnostic. Opaque: the only way to build one is {!of_error}, plus the transitional
    constructors below. *)

(** {1 Construction} *)

val of_error :
  ?notes:string list ->
  ?severity:severity ->
  origin:Origin.t ->
  code:('e -> string) ->
  pp:(Format.formatter -> 'e -> unit) ->
  'e ->
  t
(** [of_error ~origin ~code ~pp e] renders the typed error [e] as a diagnostic. The domain owns both
    strings: [code] names the diagnostic class and [pp] writes the message.

    [code] is a function rather than a string because the diagnostic code is a property of the error
    value. The ~40 string literals this replaces were a taxonomy nothing enforced.

    [severity] defaults to [Error]. *)

(* The string constructors that used to sit here - [make], [error], [warning],
   each taking [~code:string ~message:string] - are gone. Every error-producing
   module owns a typed domain now, so a diagnostic is always the rendering of a
   value, and a hand-written message is not expressible: there is no function
   left that accepts one. That is what the migration was for, and removing them
   is what makes it stick. *)

(** {1 Accessors} *)

val severity : t -> severity
val code : t -> string
val message : t -> string
val origin : t -> Origin.t
val notes : t -> string list

(** {1 Rendering}

    None of these emit a breakable space, so the enclosing box's right margin can never reflow them:
    a diagnostic renders identically wherever it is printed. *)

val pp_severity : Format.formatter -> severity -> unit
val severity_to_string : severity -> string

val pp : Format.formatter -> t -> unit
(** One diagnostic as [origin: severity[code]: message], followed by a source snippet with a caret
    line when the origin has a span, and one line per note. *)

val pp_all : Format.formatter -> t list -> unit
(** {!pp} for each, one per line. *)

val render : t -> string
val render_all : t list -> string

val pp_repr : Format.formatter -> t -> unit
(** The structural dump, for expect tests. {!pp} deliberately drops detail — an origin collapses to
    a location — so a test that only pinned {!render} could not tell a misattributed origin from a
    correctly attributed one that happens to print the same. *)
