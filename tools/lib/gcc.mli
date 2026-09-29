(** The installed system cross-[gcc] toolchains, which compile the fixture
    corpus.

    Nothing here builds a compiler: five are Debian cross packages and the sixth
    (riscv32) is a published toolchain archive, all on the container's PATH
    before any [make] target runs. Resolved by bare name through the child's
    PATH, exactly the convention {!Gnu_tools}'s [as]/[ld]/[objdump] use - never
    an absolute path rooted under a work directory. *)

val tool_name : Target.t -> string
(** [<target's toolprefix>gcc], e.g. ["x86_64-linux-gnu-gcc"]. *)

val installed : Target.t -> bool
(** Whether the target's [gcc] can be STARTED, by the same PATH search a child
    would use. Performs no [ensure] and creates nothing. *)

val require_all : Target.t list -> (unit, Tool_error.t) Err.t
(** Names every missing target's [gcc] in one diagnostic. *)

val version : Target.t -> (string, Tool_error.t) Err.t
(** The first line of [<gcc> --version]. *)

val compiler : Compiler.t
(** Compiles with [<gcc> -S <Target.gcc_fixture_args> -o out_rel source_rel]. *)
