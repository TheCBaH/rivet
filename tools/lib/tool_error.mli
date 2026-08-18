(** The application-owned error {e payload}.

    This carries what went wrong; [Err] carries where it was detected and which
    boundaries it crossed. Keeping them apart is what lets the compatibility
    diagnostics stay byte-identical: the default renderer prints only
    {!to_fatal_line} of the payload, so a trail can exist without ever reaching
    the bytes a gate compares (decision P7). *)

type op =
  | Read_file
  | Write_file
  | Traverse
  | Parse
  | Hash
  | Spawn  (** failed to START a program; never a child's exit status *)
  | Exec  (** a program ran and ended in a way the caller does not accept *)
  | Validate
  | Usage

type t = {
  op : op;
  context : string list;  (** outermost first, e.g. ["return42"; "x86_64"] *)
  path : Fpath.t option;
  detail : string;  (** the historical diagnostic wording, verbatim *)
  status : Process_status.t option;
  stderr : string option;
  cause : string option;  (** a foreign library's own message, unmodified *)
}

val v :
  ?context:string list ->
  ?path:Fpath.t ->
  ?status:Process_status.t ->
  ?stderr:string ->
  ?cause:string ->
  op ->
  string ->
  t

val pp : Format.formatter -> t -> unit
(** Full rendering, including whichever optional fields are present. Used for
    [~pp_error] and by the opt-in trace output - never for compatibility bytes. *)

val to_fatal_line : t -> string
(** ["FATAL: " ^ detail], the shell's exact wording. No path, no status, no
    context: those are carried for the trace, and adding them here would change
    bytes that the equivalence gates compare. *)

val exit_code : t -> int
(** [Usage] is 2, everything else 1 - matching the scripts. *)
