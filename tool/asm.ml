(* The `asm` CLI (.ai/asm_plan.md's single-slice inspection command).

   Filled in at M0.5/M1.5, at which point it grows cmdliner and the
   --target/--fixed-base/--dump-image/--dump-disasm flags. Kept minimal until
   then so that the purity audit has something to classify and the layer check
   has an edge to verify: a production library reaching this executable is
   impossible, but a production library reaching a *tool library* would not be,
   which is why tool/ is audited rather than merely conventionally separate. *)

let () = print_endline "asm: not implemented before M0.5 (see tool/asm.ml)"
