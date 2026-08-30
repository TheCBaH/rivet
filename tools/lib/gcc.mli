(** The installed system cross-[gcc] toolchains, used to compile CompCert's own
    [test/c/] corpus with a second, independent code generator
    ([classify-c-gcc], asm/docs/corpus.md).

    Unlike {!Ccomp}, these are never built by this project: five are Debian
    cross packages and the sixth (riscv32) is the same published toolchain
    archive [asm-cross-setup]'s riscv32 leg already installs (M5's own
    [riscv-inventory.md]) - all six are already on the container's PATH before
    any [make] target runs, so [classify-c-gcc] needs no
    [asm-cross-setup]-style build step, only the checkout to compile. Resolved
    by bare name through the child's PATH, exactly the convention
    {!Gnu_tools}'s [as]/[ld]/[objdump] already use - never an absolute path
    rooted under a work directory. *)

val tool_name : Target.t -> string
(** [<target's toolprefix>gcc], e.g. ["x86_64-linux-gnu-gcc"]. *)

val args : Target.t -> string list
(** The flags [classify-c-gcc] passes after [-S]. Every target gets its
    {{!Target.config}[ccomp_args]} verbatim ([-fno-pie], plus [-marm] on arm) -
    the same calling-convention/PIE decision {!Ccomp.compile_s} already applies
    for the identical reason (asm/docs/corpus.md: this project has no dynamic
    linker or GOT). RISC-V additionally needs an explicit [-march]/[-mabi]
    (mirroring {{!Target.config}[as_args]}'s own values) plus [-mno-relax]:
    unlike [ccomp], whose ISA/ABI choice is baked into the configured binary,
    a generic cross [gcc] defaults to its multilib's widest ISA - the
    riscv64-linux-gnu-gcc package defaults to [rv64imafdc_zicsr_zifencei], a
    strictly bigger extension set (compressed instructions included) than the
    [rv64imafd]/[lp64d] profile every other target-aware tool in this project
    assumes, so left unpinned it would silently test the wrong ISA rather than
    this project's own scope. Arm is deliberately NOT given a matching
    [-march]/[-mfpu] override: the [arm-linux-gnueabihf] triplet's own default
    ([armv7-a+fp], [vfpv3-d16], hard-float) already agrees with
    {{!Target.config}[as_args]}'s [-march=armv7-a], and adding an explicit
    [-march=armv7-a] here (without also restating the FPU) regresses the
    default FPU selection to none, which then conflicts with the triplet's
    hard-float default and fails to compile - checked by hand against the real
    installed cross gcc before being written down. *)

val installed : Target.t -> bool
(** Whether the target's [gcc] can be STARTED, resolved the same PATH search a
    child would use - {!Gnu_tools.installed}'s own convention, applied to
    [gcc] instead of [as]/[ld]. Performs no [ensure] and creates nothing. *)

val require_all : Target.t list -> (unit, Tool_error.t) Err.t
(** Names every missing target's [gcc] in one diagnostic. *)

val version : Target.t -> (string, Tool_error.t) Err.t
(** The first line of [<gcc> --version]. *)
