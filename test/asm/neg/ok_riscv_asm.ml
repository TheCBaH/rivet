(* requires: rivet.riscv_asm *)
let _ = Riscv_asm.Rv32.[ add x1 x2 x3; lw a0 (off sp 8); c_lw Creg.s0 (cmem Creg.a0 4) ]
