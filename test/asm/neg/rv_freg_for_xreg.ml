(* requires: rivet.riscv32_encode rivet.asm_core rivet.foundation *)
(* error: Xreg *)
let _ =
  Riscv32_encode.Lowered.U
    {
      op = Riscv32_encode.Opcode.Lui;
      rd = Riscv32_encode.Freg.of_int_exn 1;
      imm = Rivet_asm_core.Expr.Const Rivet_foundation.Bigint.zero;
    }
