(* requires: rivet.i8080_asm *)
let _ =
  I8080_asm.
    [
      lxi sp 0xf000;
      mvi a 42;
      mov m a;
      push psw;
      pop hl;
      ldax de;
      dad sp;
      jnz_e (symbol "loop");
      hlt ();
    ]
