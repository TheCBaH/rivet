(* requires: rivet.aarch64_asm *)
(* error: `Zr *)
let _ = Rivet_aarch64_asm.(add_i xzr x0 1)
