(* requires: rivet.aarch64_asm *)
(* error: `Sp *)
let _ = Rivet_aarch64_asm.(add x0 sp x2)
