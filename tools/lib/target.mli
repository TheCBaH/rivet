(** The six target profiles.

    A closed variant, not a string: every consumer of the matrix - eight shell
    scripts today - re-derives the same six names, and a typo currently surfaces
    as an unknown-target Fatal at whatever point the typo is reached.

    This is deliberately NOT a literal transcription of the sixteen values
    target_config assigns. Two of them are derived rather than stored, because
    storing them lets the two copies disagree:

    - [HAS_SYSROOT] is exactly [qemu_sysroot <> None], so it is a function;
    - the three [LINK_*_ADDR] are one arithmetic family, so they are one record.

    tools/dev/dump-target-config.sh + the target_db_agrees integration test
    compare all sixteen shell values verbatim ANYWAY, including HAS_SYSROOT, so
    a stale shell assignment cannot hide behind an equivalent derived value. *)

type t = X86_32 | X86_64 | Arm | Aarch64 | Riscv32 | Riscv64

val all : t list
val to_string : t -> string

val of_string : string -> (t, Tool_error.t) Err.t
(** A plain function, not only a [Cmdliner.Arg.conv]: a converter failure is a
    Cmdliner parse error and exits 2, where `--verify badtarget` must exit 1. *)

val pp : Format.formatter -> t -> unit

type capability = Fixture | Assembler | Libc_smoke

val set : capability -> t list
(** [Libc_smoke] omits riscv32/riscv64: fixture work is freestanding and must
    not acquire the cross-smoke suite's libc requirement. *)

type link = { text : int; rodata : int; data : int }

type config = {
  configure_target : string;
  toolprefix : string;
  qemu_bin : string;
  qemu_sysroot : string option;
  ccomp_args : string list;
  compcert_configure_args : string list;
  as_args : string list;
  ld_args : string list;
  linker_emulation : string option;
  elf_class : string;
  word_size : int;
  readelf_machine : string;
  link : link;
}

val config : t -> config

val has_sysroot : t -> bool
(** Derived: [qemu_sysroot <> None]. *)

val hex : int -> string
(** Lowercase [0x...], the spelling the shell's `printf '0x%x'` produces. *)
