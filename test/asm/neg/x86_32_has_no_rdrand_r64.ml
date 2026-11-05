(* requires: rivet.x86_32_asm *)
(* error: rdrand_r64 *)
let _ = X86_32_asm.(rdrand_r64 eax)
