(* requires: rivet.riscv_asm *)
(* error: ld *)
let _ = Riscv_asm.Rv32.(ld a0 (off sp 8))
