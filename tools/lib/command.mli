(** The result of running one command: an ORDERED list of events plus how to
    exit.

    The obvious shape - [(outcome, error list) result] - is wrong here, and
    wrong in a way that is easy to miss until the byte comparison fails. It
    discards every successful diagnostic as soon as anything fails, which breaks
    four behaviors the shell has today:

    - `--check return42 does_not_exist` prints return42's success line FIRST and
      then fails (decision P4);
    - `--verify` writes a diff to stdout, a FATAL to stderr, AND continues to the
      next case;
    - a two-case regeneration mixes one case's fatal with another's output;
    - `--verify badtarget` prints usage, then a fatal, and exits 1.

    So events are kept in order and success is never dropped. *)

type event =
  | Output of Diagnostic.t
  | Fatal of Tool_error.t Err.Error.t
      (** the wrapper, not the bare payload: the trail survives to the boundary
          even when the default renderer does not print it *)

type exit_class = [ `Success | `Failure | `Usage ]
type t = { events : event list; exit : exit_class }

val ok : Diagnostic.t list -> t
val fail : Diagnostic.t list -> t
val of_error : Tool_error.t Err.Error.t -> t
val of_result : ('a, Tool_error.t) Err.t -> ok:('a -> t) -> t

val of_errors : Tool_error.t Err.Accum.errors -> t
(** The accumulated-error boundary.

    [Err.Accum.errors] is ['e Error.t list], so an accumulating computation has
    error type [Tool_error.t Err.Accum.errors Err.Error.t] and CANNOT reach
    {!of_result} or inhabit a single [Fatal]. This is the one adapter, and it
    emits one [Fatal] per failure, in input order, each keeping its own
    detection origin and event trail.

    [Err.Accum.fold_errors] is rejected for this: it retains only the first
    failure's origin and trail, which is exactly the information a
    multi-diagnostic report exists to carry. *)

val of_accum_result : ('a, Tool_error.t Err.Accum.errors) Err.t -> ok:('a -> t) -> t

val accumulate : 'a list -> f:('a -> t) -> t
(** Concatenate every command's events in order - successful ones are never
    dropped - and take the worst exit_class (Usage > Failure > Success). *)

val render : err_trace:bool -> t -> int
(** Write each event exactly once to its declared stream and return the process
    exit code. [~err_trace:true] switches [Fatal] rendering to the full
    [Err.Error.pp] trail; the default prints only [Tool_error.to_fatal_line],
    which is what keeps compatibility bytes free of source locations (P7). *)

val exit_code : exit_class -> int

val fail_after : Diagnostic.t list -> Tool_error.t Err.Error.t -> t
(** The output produced before a failure, then the fatal, then exit 1.

    For the stop-at-first-failure commands (oracle, exec), where the shell runs
    a loop under plain errexit rather than through an `|| rc=1` aggregator: the
    lines already printed must survive, but nothing after the failure runs. *)
