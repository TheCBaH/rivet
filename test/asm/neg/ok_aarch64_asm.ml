(* requires: rivet.aarch64_asm *)
let _ =
  Aarch64_asm.
    [
      add x1 x2 x3;
      ldr x0 (off sp 16);
      add_i sp sp 16;
      add_sh w0 w1 w2 (by_lsl 3);
      add_ext x0 x1 w2 (ext_uxtw 2);
      mov x0 sp;
      csel x0 x1 x2 Cond.Ne;
      b_eq (label "l");
      fadd d0 d1 d2;
      fadd_v (v4s v0) (v4s v1) (v4s v2);
      scvtf d0 x1;
      ret ();
    ]
