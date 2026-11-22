(* requires: rivet.riscv_asm *)
(* error: Freg *)
let _ = Rivet_riscv_asm.Rv32.(fadd_s a0 a1 a2)
