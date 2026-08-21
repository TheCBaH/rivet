(** [compcert-tools gas-xref regen] - rebuild the GNU as cross-reference.

    Two case families, and the asymmetry between them is the point:

    - GENERATED cases come from this project's own AST corpus through the same
      canonical printer that produces our bytes, so GNU as rejecting one is a
      bug in that printer and is FATAL;
    - FRONTIER cases are real assembly this project did not write, so GNU as
      rejecting one is RECORDED EVIDENCE and the run continues. A file neither
      assembler accepts says nothing about the difference between them.

    Destructive: it recreates the whole corpus root, which is why that root is a
    [recreatable_root] capability rather than a path. *)

val regen : Repo.t -> Command.t

val runtime_defines : Target.t -> string list
(** The [-DMODEL_*/-DABI_*/-DENDIANNESS_*/-DSYS_*] set CompCert's own
    [runtime/Makefile] uses per target - without them [FUNCTION] is left
    undefined and preprocessing a runtime [.S] file produces text no assembler
    can read. Empty for riscv32/riscv64, which have no upstream
    [runtime/<target>] directory. Exposed for M4's [Preexisting]-unit
    preprocessing ({!Fixture_cmd}), which reuses this exact invocation rather
    than a second one. *)
