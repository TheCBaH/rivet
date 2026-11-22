(* requires: rivet.riscv32_asm *)
(* error: Freg *)
let _ = Rivet_riscv32_asm.(fadd_s a0 a1 a2)
