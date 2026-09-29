(** Renders scripts/target-matrix.sh from {!Target}.

    {1 Why generate rather than query}

    The shell consumers - asm-helpers.sh and the Makefile - drive cross tools
    and make, so their bodies are genuinely shell. The alternative was for each
    to ask the built executable for its target configuration, which moves
    ownership the same way but gives them a run-time dependency on a dune
    build: asm-helpers.sh runs in abi-conform, which builds none of the OCaml
    tools first.

    Generating a committed file moves ownership with no such coupling. Nobody
    edits the shell any more; the gate is [make tools-matrix-diff], which
    regenerates and fails if the working tree changed - the same byte-exact
    pattern tools-oracle-diff and tools-gasxref-diff already use.

    {2 What still checks the values}

    target_db_agrees keeps comparing all sixteen shell values against
    {!Target.config}, per target, through scripts/dev/dump-target-config.sh. Once
    this file is generated that comparison becomes a ROUND TRIP - OCaml to
    shell text, sourced by bash, back to a comparison - which is not a tautology:
    it is what catches a quoting bug, a lost array element, or a default this
    renderer forgot to emit. *)

val emit : unit -> Command.t
(** The complete file on stdout. The caller redirects; nothing is written into
    the source tree from here, so no work-root capability has to be invented for
    a path that is neither a corpus nor a work root. *)
