(* requires: rivet.arm_asm *)
(* error: Sreg *)
let _ = Arm_asm.(vcvt_f64_s32 s0 s1)
