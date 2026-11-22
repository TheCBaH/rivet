(* requires: rivet.riscv32_asm *)
(* error: Vreg *)
let _ = Rivet_riscv32_asm.(vadd_vv a0 v1 v2 false)
