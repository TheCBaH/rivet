type t = Exited of int | Signaled of int | Timed_out

(* OCaml does NOT use POSIX signal numbers: Sys.sigterm is -11 and Sys.sigkill
   is -7, and both Unix.kill and WSIGNALED speak that internal numbering. Every
   other party - the shell scripts, the diagnostics they print, `kill -l`, a
   reader of a log - speaks POSIX. So the conversion happens once, here, where a
   child outcome becomes a value this program reasons about.

   Storing OCaml's number instead would render an ordinary SIGTERM as "killed by
   signal -11". That is not a printer bug that could be fixed later at the
   renderer: a comparison against an expected signal number would be wrong too. *)
let posix_of_ocaml_signal n =
  if n = Sys.sighup then 1
  else if n = Sys.sigint then 2
  else if n = Sys.sigquit then 3
  else if n = Sys.sigill then 4
  else if n = Sys.sigabrt then 6
  else if n = Sys.sigbus then 7
  else if n = Sys.sigfpe then 8
  else if n = Sys.sigkill then 9
  else if n = Sys.sigusr1 then 10
  else if n = Sys.sigsegv then 11
  else if n = Sys.sigusr2 then 12
  else if n = Sys.sigpipe then 13
  else if n = Sys.sigalrm then 14
  else if n = Sys.sigterm then 15
  else if n = Sys.sigchld then 17
  else if n = Sys.sigcont then 18
  else if n = Sys.sigstop then 19
  else if n = Sys.sigtstp then 20
  else if n = Sys.sigttin then 21
  else if n = Sys.sigttou then 22
  else if n = Sys.sigvtalrm then 26
  else if n = Sys.sigprof then 27
  else if n = Sys.sigsys then 31
  else if n >= 0 then n (* already a platform number *)
  else 0 (* an OCaml-internal signal with no POSIX name here; 0 is never real *)

let signaled n = Signaled (posix_of_ocaml_signal n)

let pp ppf = function
  | Exited n -> Format.fprintf ppf "exited %d" n
  | Signaled n -> Format.fprintf ppf "killed by signal %d" n
  | Timed_out -> Format.pp_print_string ppf "timed out"

type accepted = Zero_only | Statuses of int list | Any_exit

let accepts accepted status =
  match (accepted, status) with
  | Zero_only, Exited 0 -> true
  | Statuses codes, Exited n -> List.mem n codes
  | Any_exit, Exited _ -> true
  | _, (Exited _ | Signaled _ | Timed_out) -> false
