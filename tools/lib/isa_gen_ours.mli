(** Runs one {!Isa_generated_case.case} through THIS project's own assembler -
    a second independent assertion ("our assembler produced exactly
    the corresponding bytes under the same mode, feature and layout
    configuration"), which {!Isa_gen_oracle} deliberately does not attempt:
    wrong-byte, wrong-form and unexpected-relocation controls, plus
    actionable minimal failure reproduction.

    Never linked in-process: [compcert_tools] must never require building the
    whole assembler (tools-boundary; tools/asm-check-purity.sh), so this shells
    out to a separately built [tool/asm.exe], exactly the pattern
    [gas_xref_cmd.ml]'s [emit_generated] already uses for the same reason. *)

type outcome =
  | Assembled of { bytes_hex : string }
      (** [tool/asm.exe]'s own committed hex-dump format (matching {!Hex_dump.of_bytes}
          byte-for-byte, reproduced independently in [asm/tool/asm.ml] since that
          executable cannot depend on this library either - see its own comment). *)
  | Rejected of string  (** The assembler's rendered diagnostic (stderr), trimmed to one line. *)

val normalized_argv : Isa_generated_case.case -> string list
(** [["--target"; <target>; "--fixed-base"; "0x0"; "--dump-bytes"; "case.s"]] -
    the argv {!run} actually passes to [tool/asm.exe], but with the real
    (machine-local, scratch-directory) source path replaced by the fixed
    basename it always uses internally, mirroring
    {!Isa_gen_oracle.normalized_argv}'s "removing temporary paths...
    from committed identities". Exposed so an offline replay can
    recompute the same value without a toolchain. *)

val run :
  Repo.t -> Isa_generated_case.case -> (outcome * Isa_generated_case.artifact, Tool_error.t) Err.t
(** Writes [case.rendered_source] to a private scratch directory - the SAME
    text handed to GAS, never a re-rendered or suffix-adjusted spelling
    (distinguishing canonical-spelling tests from forced-encoding tests -
    silently rewriting the line to make our own parser accept it would stop
    testing the canonical spelling at all, and would hide exactly the kind of
    syntax gap this task exists to surface) - then invokes [tool/asm.exe
    --target <case.target> --fixed-base 0x0 --dump-bytes] on it through [dune
    exec] (never in-process, see above). [case.configuration] is deliberately
    NOT passed: this project's CLI has no feature-configuration flags yet
    (not started) - every current pilot form is
    feature-free, so running at the default configuration is correct today,
    and a future feature-gated pilot form would need this reconsidered, not
    silently passed through.

    Fails ([Err]) only on a harness/tool-invocation problem (e.g. [tool/asm.exe]
    does not build); the assembler accepting or rejecting the case is DATA,
    returned as an [outcome], exactly as {!Isa_gen_oracle.run} treats GAS. The
    returned {!Isa_generated_case.artifact} carries the real, measured
    [exit_status] and stdout/stderr capture, and [tool_label] resolved from
    this checkout's own git revision (["ours-<rev>"], suffixed [-dirty] if
    [asm/] has uncommitted changes) - never a hardcoded version, since "ours"
    changes with every commit unlike a fixed cross-toolchain release. *)
