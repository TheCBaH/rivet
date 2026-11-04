(* requires: rivet.riscv_asm *)
(* error: Creg *)
let _ = Riscv_asm.Rv32.(c_lw a0 (cmem Creg.a0 4))
