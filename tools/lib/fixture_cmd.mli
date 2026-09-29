(** The three fixture-generator modes that need a compiler, plus rehash.

    The compiler is a parameter: this library ships the gcc one ({!Gcc}), and a
    consumer that generates the same corpus with another compiler passes its
    own. *)

type preexisting =
  Repo.t -> target:Target.t -> origin:string -> out:Fpath.t -> (unit, Tool_error.t) Err.t
(** How a [Preexisting] unit's [<target>/<stem>.s] is produced from its declared
    [origin], an upstream assembly source. Without one, a case declaring such a
    unit cannot be generated or verified. *)

val verify :
  ?preexisting:preexisting ->
  Repo.t ->
  compiler:Compiler.t ->
  target:Target.t ->
  cases:string list ->
  Command.t
(** Regenerate each case in scratch and byte-compare. A mismatch emits the `diff
    -u` body on STDOUT, then a fatal on stderr, and then CONTINUES to the next
    case - the mixed ordering Command exists to represent. *)

val regen :
  ?preexisting:preexisting -> Repo.t -> compiler:Compiler.t -> cases:string list -> Command.t
(** Per case: require only that case's own declared [supported-targets]
    compilers (M4, docs/design.md §12 - all six when the manifest declares no
    restriction, exactly today's behavior), compile every target CHECKING each
    invocation, run the gate, and STOP BEFORE WRITING if the gate fails. That
    last clause is the Phase 0A contract - a rejected fixture must not have its
    manifest installed. *)

val rehash : Repo.t -> compiler:Compiler.t -> cases:string list -> Command.t
(** Recompute the manifest without recompiling, for after the oracle adds its
    artifacts. Differences are reported exactly as regen reports them: if a
    fixture .s changed here, something regenerated it behind our back, and that
    is a finding rather than a refresh. *)

val usage_error : string -> Command.t
(** A fatal, exiting 1 - the `verify badtarget` shape. Exit 1 rather than 2
    because a rejected target is a bad VALUE, not a malformed command line;
    Cmdliner's own help covers the latter. *)
