(* requires: rivet.arm_asm *)
(* error: Sreg *)
let _ = Arm_asm.(vldr_d s0 (mem r1))
