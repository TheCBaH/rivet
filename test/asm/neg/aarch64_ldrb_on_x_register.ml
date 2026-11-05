(* requires: rivet.aarch64_asm *)
(* error: View.x *)
let _ = Aarch64_asm.(ldrb x0 (off x1 0))
