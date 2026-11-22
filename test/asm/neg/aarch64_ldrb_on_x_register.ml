(* requires: rivet.aarch64_asm *)
(* error: View.x *)
let _ = Rivet_aarch64_asm.(ldrb x0 (off x1 0))
