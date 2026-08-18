(** The behavioral tool gate (.ai/asm_plan.md §3.2, an M0 exit criterion).

    APT tooling is range-checked rather than digest-pinned - devcontainer-lock
    pins feature references, not the mutable debian:13 base - and version numbers
    alone do not establish compatibility where behaviour matters. So this asks
    the tools to DO the things the project depends on, in a freshly built
    container, and records what answered.

    {1 What it must not depend on}

    Check 1 executes a self-contained reference ELF that exits through a direct
    exit_group syscall: no ABI v1, no transport helpers, no OCaml runner - none
    of which existed when this gate was specified, and none of which should be
    able to make it pass. This establishes E2 tool compatibility ONLY; the
    return/fault/timeout suite is the distinct E4 proof and lives in
    [make asm-abi-conform].

    {2 Failures are recorded, not fatal}

    Every check that fails is counted and the run CONTINUES - a gate whose job is
    to report which tools moved must not stop at the first one. Only an
    operational failure (an unusable work root, an unwritable artifact) ends the
    run through [Command.of_error]. *)

type mode = All | User | System | Gdb

val mode_of_string : string -> mode option

val run : Repo.t -> mode:mode -> env:(string -> string option) -> Command.t
(** Recreates the work root, runs the selected checks, writes provenance and
    dumps it. Provenance is written whatever the outcome, so a base-image update
    that moved a tool under us stays visible in the artifact even when the gate
    failed for an unrelated reason.

    [env] supplies ASM_TOOL_GATE_WORK and ASM_TOOL_GATE_PORT. *)

val usage_error : string -> Command.t
(** An unknown mode: one line on stderr naming the four, exit 2. The mode is
    validated inside the term rather than by a Cmdliner converter because it is
    OPTIONAL - `${1:-all}` on the shell it replaced - so "no argument" has to
    reach the term rather than becoming a parse error. *)

(** {3 Transcriptions}

    Four one-line reproductions of the shell's sed, awk and tr. Exposed because
    a passing run exercises only two of them, and the two it does not are on the
    diagnostic paths - where being quietly wrong costs exactly the information
    the gate exists to produce. test_tool_gate holds each to the sed, awk or tr
    it reproduces. *)

val squeeze_spaces : string -> string
(** [sed 's/  */ /g']: runs of SPACES collapse to one. Tabs untouched. *)

val first_field : string -> string
(** [awk]'s [$1]: leading blanks ignored, split on spaces and tabs. *)

val flatten : string -> string
(** [tr '\n' ' ']: every newline including the final one. *)

val last_bytes : int -> string -> string
(** [tail -c n]. *)
