(* requires: rivet.riscv32_asm *)
let _ = Riscv32_asm.[ add x1 x2 x3; lw a0 (off sp 8); vadd_vv v1 v2 v3 false ]
