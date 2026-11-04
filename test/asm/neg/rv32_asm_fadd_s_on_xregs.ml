(* requires: rivet.riscv32_asm *)
(* error: Freg *)
let _ = Riscv32_asm.(fadd_s a0 a1 a2)
