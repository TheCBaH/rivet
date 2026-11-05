(* Every RV64 instruction as a typed builder: the hand-written opcodes' (Riscv_asm.Rv64) and the
   standard rows' (Riscv_asm_rows64), with the registers and operand helpers.
   [let open Riscv64_asm in [ add x1 x2 x3; lw a0 (off sp 8) ]]. *)

include module type of struct
  include Riscv_asm.Rv64
end

include module type of struct
  include Riscv_asm_rows64
end
