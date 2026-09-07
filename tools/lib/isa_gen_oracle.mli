(** Runs one {!Isa_generated_case.case} through the real cross GNU assembler
    for its target and checks the assembled bytes against the case's own
    normalized {!Isa_norm_model.encoding}: explicit profile/tool
    configuration and capability probes, GNU artifacts, exact bytes and
    observed-form checks.

    This is the {!Isa_generated_case.Gnu_regeneration} tier: every call
    needs the real cross binutils for [case.target] and is therefore never
    part of the toolchain-free [tools-test]/[tools-integration] suites. *)

type outcome =
  | Assembled_matching of { bytes_hex : string }
      (** GAS accepted the case and the assembled bytes satisfy the observed-form
          check against [case]'s own normalized encoding. *)
  | Assembled_mismatched of { bytes_hex : string; detail : string }
      (** GAS accepted the case, but the assembled bytes do NOT match the expected
          encoding - a genuine "GAS selected a different form" finding, not
          necessarily a bug: three pilot forms are expected to report this. *)
  | Unexpected_relocation of { bytes_hex : string; relocations : string list }
      (** GAS accepted the case, but its object's [.text] section carries at
          least one relocation record: inspect relocation tables and fail if
          a supposedly relocation-free case contains one, never comparing
          unresolved object placeholders with our bound image. Every current
          pilot case is a
          plain register/immediate instruction with no symbol reference, so
          none is expected to reach this in practice - it exists so an
          unexpectedly relocatable case is a loud, distinct outcome instead of
          a silent, misleading comparison against placeholder bytes.
          [relocations] is the raw [objdump -r] lines for [.text] (banner,
          header, and entries), kept as evidence for whoever investigates. *)
  | Rejected of string
      (** GAS rejected the case; carries {!Gnu_tools.gas_outcome}'s trimmed diagnostic. *)

val observed_form_check : Isa_norm_model.encoding -> string -> (unit, string) result
(** Checks assembled section bytes against a normalized encoding: for
    {!Isa_norm_model.Riscv_encoding}, [(bytes interpreted little-endian) land
    mask = value] and the byte length matches [width_bits / 8]; for
    {!Isa_norm_model.X86_encoding}, the leading byte equals [opcode]. Exposed
    for unit testing without a toolchain. *)

val has_text_relocations : string -> bool
(** Whether raw [objdump -r] output names a [.text] relocation ("RELOCATION
    RECORDS FOR [.text]"). Exposed for unit testing without a toolchain. *)

val text_relocation_lines : string -> string list
(** Raw [objdump -r] output, split into non-blank lines - what {!run} records
    as [Unexpected_relocation]'s evidence when {!has_text_relocations} is
    [true]. Exposed for unit testing without a toolchain. *)

val normalized_argv : Isa_generated_case.case -> string list
(** [case.configuration @ ["-o"; "case.o"; "case.s"]] - the argv {!run}
    actually passes to the child process, but with the real (machine-local,
    scratch-directory) source/object paths replaced by the fixed basenames it
    always uses internally, so the result is reproducible and safe to persist
    in a committed artifact. Exposed so
    an offline replay can recompute the same value without a toolchain. *)

val run :
  Isa_generated_case.case ->
  Isa_norm_model.encoding ->
  (outcome * Isa_generated_case.artifact, Tool_error.t) Err.t
(** Probes the target's cross toolchain ({!Gnu_tools.require}), writes
    [case.rendered_source] to a private scratch directory, assembles it with
    [case.configuration] (via {!Gnu_tools.try_assemble_with_args}, never the
    frozen [Target.config] flags), extracts the [.text] section, and applies
    {!observed_form_check}. Fails only on a probe/tool-invocation error, not
    on GAS rejecting the case, a form mismatch, or an unexpected relocation -
    all are data, returned as
    an [outcome]. The returned {!Isa_generated_case.artifact} carries the
    real, measured [exit_status] and merged stdout/stderr capture (the
    frozen shape, populated here for the first time) plus [tool_label]
    resolved via {!Gnu_tools.version_line}, never assumed from a prior
    measurement (RV32/RV64 GAS version skew) - it is what gets persisted as
    this pilot's checked-in corpus. *)
