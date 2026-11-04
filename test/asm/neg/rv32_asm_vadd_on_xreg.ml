(* requires: rivet.riscv32_asm *)
(* error: Vreg *)
let _ = Riscv32_asm.(vadd_vv a0 v1 v2 false)
