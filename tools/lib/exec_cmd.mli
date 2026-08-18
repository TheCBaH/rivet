(** [compcert-tools fixture exec] - execute every fixture under qemu-user.

    The fixture .text/.rodata/.data stay at the controlled oracle addresses and
    a tiny freestanding _start in its own section turns the fixture's return
    value into an exit_group status.

    The bytes that execute are PROVED to be the bytes the GNU differential
    oracle accepted: every present section of the linked image is compared with
    the committed oracle/linked/<section>.hex before qemu is invoked at all. *)

val run : Repo.t -> targets:Target.t list -> cases:string list -> Command.t
(** Target-major, provisioning provenance once per target, and stopping at the
    first failure - the shell runs this loop under plain errexit. *)
