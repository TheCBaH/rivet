(* requires: rivet.x86_64_asm *)
(* error: evec *)
let _ = X86_64_asm.(vaddps xmm1 xmm2 xmm16)
