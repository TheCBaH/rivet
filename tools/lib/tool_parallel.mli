(** Sharding independent, tool-invoking jobs over forked worker processes.

    Each job still runs its own child processes exactly as it would
    sequentially, so a failing job is reported under its own identity and its
    artifacts are indistinguishable from a sequential run's. Results come
    back in input order. Workers return their results to the parent by
    [Marshal] (closures included, which is sound only because a forked worker
    runs the parent's own binary). *)

val default_jobs : unit -> int
(** [COMPCERT_TOOLS_JOBS] when set to a positive integer, else the online
    processor count reported by [getconf], else 1. *)

val map : jobs:int -> ('a -> 'b) -> 'a list -> ('b list, Tool_error.t) Err.t
(** [map ~jobs f xs] is [List.map f xs], computed by up to [jobs] forked
    workers (sequentially in-process when [jobs <= 1] or [xs] has at most one
    element). A worker that raises or dies is an error naming it; [f] itself
    should report job failures in its result rather than by raising. *)
