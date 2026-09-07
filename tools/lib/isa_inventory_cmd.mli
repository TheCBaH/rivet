(** [compcert-tools isa-inventory regen] - rebuild the whole-ISA instruction
    inventory (asm/docs/isa-inventory.md).

    Toolchain-free: it reads only the vendored submodules under
    asm/vendor/isa-data/, never invokes a compiler or cross assembler.
    Currently RISC-V only ({!Isa_inventory_riscv}) - the "RISC-V-first pilot";
    an x86 generator against the already-vendored XED submodule is follow-up
    work. *)

val regen : Repo.t -> Command.t
