(** Supervised background processes.

    Everything else in this tooling runs a child to completion and reads its
    result. This is for the one shape that does not: a server that must be
    STARTED, waited on until it is ready, talked to, and then guaranteed to be
    gone - the paused qemu-system the GDB handshake connects to.

    {1 Why a process group}

    The child is put in its own process group ([setpgid] in the child, before
    exec), and termination signals the GROUP rather than the pid. qemu-system
    does not fork here, but the guarantee must not depend on that: a child that
    spawns its own children would otherwise leave them holding the port, and the
    next run would fail to bind with no indication why. Signalling the group is
    what makes cleanup a property of the API rather than of the callee.

    Doing it in the CHILD rather than the parent avoids the race where the
    parent's setpgid loses to a fast exec.

    {2 Why a monotonic clock}

    Deadlines use [Mtime_clock], never [Unix.gettimeofday]. A wall clock can be
    stepped backwards by NTP, and a deadline computed from it would then wait
    far longer than asked - on CI, indistinguishably from a hang. *)

type t

val spawn :
  ?cwd:Fpath.t ->
  ?env:(string * string option) list ->
  stdout:Fpath.t ->
  label:string ->
  string ->
  string list ->
  (t, Tool_error.t) Err.t
(** Starts the process in its own group. stdout and stderr are merged into the
    named file - a background server's diagnostics are evidence, and splitting
    them would reorder interleaved output. *)

val pid : t -> int

val await : t -> ready:(unit -> bool) -> timeout:float -> poll:float -> (bool, Tool_error.t) Err.t
(** Polls [ready] until it holds or [timeout] elapses on the MONOTONIC clock.

    [ready] is a caller-supplied probe, not a fixed sleep: a fixed sleep is
    either flaky or slow, and on a loaded CI runner it is both. It also detects
    the child EXITING - a server that died during startup can never become
    ready, so waiting the full timeout for it would turn a crash into a
    timeout. Returns [false] on timeout or early exit; the caller decides which
    diagnostic that deserves. *)

val exit_status : t -> (Process_status.t option, Tool_error.t) Err.t
(** Non-blocking: [Some] iff the child has already ended, [None] while it is
    still running.

    It exists so a caller can tell WHY a wait ended without inspecting the
    child's output. "The server never became ready" and "the server died" are
    the same observation from the outside and get very different diagnoses. *)

val terminate : t -> (Process_status.t option, Tool_error.t) Err.t
(** TERM the group, wait up to [term_grace] seconds, then KILL the group and
    reap. Idempotent, and safe to call on an already-exited process.

    Returns the status if this call reaped it, [None] if it had already been
    reaped. Escalation intervals are module constants rather than parameters so
    that every supervised process in this tooling behaves identically. *)

val term_grace : float
(** 2.0 seconds between TERM and KILL. Long enough for a signal handler to run
    and short enough that a wedged process does not stall CI. *)

val with_process :
  ?cwd:Fpath.t ->
  ?env:(string * string option) list ->
  stdout:Fpath.t ->
  label:string ->
  string ->
  string list ->
  (t -> ('a, Tool_error.t) Err.t) ->
  ('a, Tool_error.t) Err.t
(** The only shape callers should use. [terminate] runs on EVERY exit path -
    success, error, and exception - through [Fun.protect], so a raised
    exception cannot leave a qemu holding a TCP port.

    A bare [spawn] plus a manual [terminate] is exactly the discipline this
    module exists to remove: the shell's `kill "$qpid" || true` at the end of
    gdb_gate is never reached if anything above it exits non-zero under
    errexit. *)
