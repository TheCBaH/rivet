(** Running one foreground child, on an owned Unix runner.

    Phase 1 used a Bos backend. This replaces it, because three properties the
    fixture work needs cannot be expressed through Bos: a working directory
    (CompCert embeds its command line in a banner, so the compile must run from
    inside the fixture directory with relative paths), independent capture of
    the two streams, and stderr merged into stdout as `2>&1` does.

    Still absent, and arriving in Phase 6: deadlines, background children,
    process groups. External `timeout` remains until then, which preserves its
    124-127 status contract. *)

type stdout_sink = Out_capture | Out_file of Fpath.t | Out_null | Out_inherit

type stderr_sink =
  | Err_capture
  | Err_file of Fpath.t
  | Err_null
  | Err_inherit
  | Err_to_stdout
      (** dup2 onto the stdout descriptor, exactly as `2>&1`. There is no stdout
          counterpart, because merging in the other direction is not a thing the
          shell can express either - and `ccomp -version 2>&1 | head -1` is why
          this constructor exists: capturing separately and concatenating is NOT
          equivalent, since it loses the interleaving. *)

type input = From_string of string | From_file of Fpath.t | From_dev_null | In_inherit

type result = {
  status : Process_status.t;
  stdout : string option;  (** [Some] iff [Out_capture] *)
  stderr : string option;  (** [Some] iff [Err_capture] *)
}

type spec = {
  prog : string;
  args : string list;  (** NOT including argv[0] *)
  cwd : Fpath.t option;
  env : (string * string option) list;
      (** Ordered overrides applied to the PARENT environment: [(k, Some v)]
          sets, [(k, None)] removes. [LC_ALL=C] from [parsed_output] is applied
          AFTER these, so it wins. *)
  stdin : input;
  stdout : stdout_sink;
  stderr : stderr_sink;
  accepted : Process_status.accepted;
  parsed_output : bool;
  label : string;
}

val spec :
  ?cwd:Fpath.t ->
  ?env:(string * string option) list ->
  ?stdin:input ->
  ?stdout:stdout_sink ->
  ?stderr:stderr_sink ->
  ?parsed_output:bool ->
  accepted:Process_status.accepted ->
  label:string ->
  string ->
  string list ->
  spec

val exec : spec -> (result, Tool_error.t) Err.t
(** Never through a shell: the program and its arguments are passed to [execve]
    as a vector, so no argument can be reinterpreted as syntax.

    A program that could not be STARTED yields [op = Spawn] and never
    [Exited 127]. The distinction survives because the child reports [execve]'s
    errno to the parent through a close-on-exec pipe - on a successful exec the
    pipe closes and the parent reads EOF, so there is no ambiguity and no
    timing assumption.

    A signal death stays [Signaled n] with the POSIX number, never [128 + n].

    Every parent-side failure path terminates AND REAPS the child before
    returning. Closing descriptors is not enough: a child still running would
    outlive the call. *)

val display : spec -> string
(** For logging only. Never re-parsed and never executed - [exec] uses the
    original vector, so no quoting decision here can change what runs. *)

val resolve : spec -> (string * string array, Tool_error.t) Err.t
(** The resolved program path and the child environment, without running
    anything.

    Exposed for {!Supervisor}, which forks itself and so cannot go through
    [exec], but must not re-derive either of these: a supervised process and an
    ordinary one have to agree about PATH resolution and about the LC_ALL /
    override precedence, and two copies of that logic is how they would stop
    agreeing. *)
