(* requires: rivet.arm_asm *)
(* error: Dreg *)
let _ = Rivet_arm_asm.(vadd_f32 d0 d1 d2)
