(** The repository tools' command line.

    A library so that a consumer generating the same corpora with another
    compiler, or adding commands of its own, runs this command tree rather than
    forking it. *)

val common : (bool * string option) Cmdliner.Term.t
(** [--err-trace] and [--repo-root], the flags every command takes. *)

val with_repo :
  string option -> (Rivet_tools.Repo.t -> Rivet_tools.Command.t) -> Rivet_tools.Command.t
(** Resolves the repository root from [--repo-root], [RIVET_ROOT] or the
    working directory, and runs the command against it. *)

val main :
  ?preexisting:Rivet_tools.Fixture_cmd.preexisting ->
  ?name:string ->
  ?doc:string ->
  ?extra:(bool * Rivet_tools.Command.t) Cmdliner.Cmd.t list ->
  compiler:Rivet_tools.Compiler.t ->
  unit ->
  int
(** Evaluates the command line and returns the exit code. [compiler] generates
    and verifies the fixture corpus. [extra] subcommands join the built-in
    ones; each one's term yields [(err_trace, command)], as {!common} does. *)
