(* requires: rivet.x86_32_asm *)
(* error: addq_im *)
let _ = X86_32_asm.(addq_im 1L (mem eax))
