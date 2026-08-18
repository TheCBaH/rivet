(** How a child process ended.

    A failure to *start* a program is deliberately not representable here: it is
    not a child outcome at all, and collapsing the two is how exec failure comes
    to be reported as exit 127. It travels as a {!Tool_error.t} with [op =
    Spawn] instead. *)

type t =
  | Exited of int
  | Signaled of int
      (** The POSIX signal number, never 128 + n - and never OCaml's internal
          numbering either, in which Sys.sigterm is -11. See {!signaled}. *)
  | Timed_out

val signaled : int -> t
(** Build a [Signaled] from an OCaml-internal signal number, as [WSIGNALED]
    and [Unix.kill] use. This is the only correct way to construct one from a
    wait status. *)

val pp : Format.formatter -> t -> unit

(** Which outcomes a caller is willing to accept. *)
type accepted =
  | Zero_only
  | Statuses of int list  (** e.g. [0; 1] for `diff`, where 1 means "differs" *)
  | Any_exit
      (** every [Exited _] and NOTHING else. A signal death or a timeout is
          never an accepted outcome, however permissive the caller meant to be:
          those say the program did not get to decide its own status. *)

val accepts : accepted -> t -> bool
