(** Renders tools/target-matrix.sh from {!Target}.

    {1 Why generate rather than query}

    The three remaining shell consumers - compcert-cross-smoke.sh,
    compcert-fixture-setup.sh, asm-helpers.sh - drive ./configure, make and
    linkhier, so their bodies are genuinely shell. The alternative was for each
    to ask the built executable for its target configuration, which moves
    ownership the same way but gives three scripts a run-time dependency on a
    dune build: compcert-fixture-setup.sh is the FIRST thing the fixture-oracle
    CI job runs, and asm-helpers.sh runs in abi-conform, and neither job builds
    the OCaml tools today.

    Generating a committed file moves ownership with no such coupling. Nobody
    edits the shell any more; the gate is [make tools-matrix-diff], which
    regenerates and fails if the working tree changed - the same byte-exact
    pattern tools-oracle-diff and tools-gasxref-diff already use.

    {2 What still checks the values}

    target_db_agrees keeps comparing all sixteen shell values against
    {!Target.config}, per target, through tools/dev/dump-target-config.sh. Once
    this file is generated that comparison becomes a ROUND TRIP - OCaml to
    shell text, sourced by bash, back to a comparison - which is not a tautology:
    it is what catches a quoting bug, a lost array element, or a default this
    renderer forgot to emit. *)

val emit : unit -> Command.t
(** The complete file on stdout. The caller redirects; nothing is written into
    the source tree from here, so no work-root capability has to be invented for
    a path that is neither a corpus nor a work root. *)
