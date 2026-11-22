(* requires: rivet.arm_asm *)
(* error: Sreg *)
let _ = Rivet_arm_asm.(vldr_d s0 (mem r1))
