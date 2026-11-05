(* requires: rivet.aarch64_asm *)
(* error: `Sp *)
let _ = Aarch64_asm.(add x0 sp x2)
