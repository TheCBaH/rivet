(* requires: rivet.x86_64_asm *)
(* error: L256 *)
let _ = X86_64_asm.(vaddps xmm1 ymm2 ymm3)
