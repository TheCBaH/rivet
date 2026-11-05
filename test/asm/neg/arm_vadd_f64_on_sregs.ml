(* requires: rivet.arm_asm *)
(* error: Sreg *)
let _ = Arm_asm.(vadd_f64 s0 s1 s2)
