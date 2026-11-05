(* requires: rivet.aarch64_asm *)
(* error: `D2 *)
let _ = Aarch64_asm.(fadd_v (v4s v0) (v2d v1) (v4s v2))
