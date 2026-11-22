(* requires: rivet.riscv32_encode rivet.asm_core rivet.foundation *)
let _ =
  Rivet_riscv32_encode.Lowered.U
    {
      op = Rivet_riscv32_encode.Opcode.Lui;
      rd = Rivet_riscv32_encode.Xreg.of_int_exn 1;
      imm = Rivet_asm_core.Expr.Const Rivet_foundation.Bigint.zero;
    }
