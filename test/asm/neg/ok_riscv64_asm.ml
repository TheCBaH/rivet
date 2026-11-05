(* requires: rivet.riscv64_asm *)
let _ = Riscv64_asm.[ add x1 x2 x3; ld a0 (off sp 8) ]
