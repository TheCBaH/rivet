(* The operands of the generated row builders, one function per kind of row operand: registers
   and immediates as values, a vector mask as a [bool], a vtype as its keywords, an [fli]
   constant and a Zcmp register list as their spellings. Fixed registers and keywords are added
   by the builder. *)

open Riscv_family_encode
open Riscv_types

let mk row ops = Instruction.mk (Opcode.Table row) (List.concat ops)
let x r = [ Operand.Reg (Reg.X r) ]
let f r = [ Operand.Reg (Reg.F r) ]
let v r = [ Operand.Reg (Reg.V r) ]
let creg r = x (Creg.to_xreg r)
let cfreg r = f (Cfreg.to_freg r)
let imm n = [ Operand.Imm (Foundation.Bigint.of_int n) ]
let mem m = [ Operand.Mem m ]
let cmem m = [ Riscv_ops.Shape.cmem m ]
let spmem n = [ Riscv_ops.Shape.mem ~base:Xreg.sp n ]
let word s = [ Operand.Sym (Asm_core.Expr.Symbol s) ]
let rm r = word (Rm.name r)
let vm masked = if masked then word "v0.t" else []
let vtype keywords = List.concat_map word keywords
let fixed_x n = x (Xreg.of_int_exn n)
let fixed_v n = v (Vreg.of_int_exn n)
