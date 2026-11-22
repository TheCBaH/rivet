(* requires: rivet.x86_64_asm *)
(* error: `Hi *)
let _ = Rivet_x86_64_asm.(vgatherdps_xmx xmm1 (mem rax) (Xmm_evex.of_int_exn 16))
