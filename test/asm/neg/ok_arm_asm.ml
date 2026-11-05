(* requires: rivet.arm_asm *)
let _ =
  Arm_asm.
    [
      add r0 r1 r2;
      adds ~cond:Cond.Ne r0 r1 r2;
      vadd_f64 d0 d1 d2;
      vadd_f32 s0 s1 s2;
      vcvt_f64_s32 d0 s1;
      ldr r0 (mem r1);
      nop ();
    ]
