(* requires: rivet.riscv32_encode rivet.asm_core rivet.foundation *)
let _ =
  Riscv32_encode.Lowered.U
    {
      op = Riscv32_encode.Opcode.Lui;
      rd = Riscv32_encode.Xreg.of_int_exn 1;
      imm = Asm_core.Expr.Const Foundation.Bigint.zero;
    }
