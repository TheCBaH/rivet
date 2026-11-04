(* The compressed extensions. The C bit of the ISA string is the sum Zca + Zcf + Zcd, with Zcd and
   Zcf present only when D and F are (Zcf only on RV32); the table rows tagged [c] are split by the
   format they move. Zcmp and Zcmt reuse the encodings of the double-precision compressed
   load/store, so they conflict with Zcd and are off by default. *)

module Req = Target_component.Req
module Opcode = Riscv_types.Opcode

let in_c names (r : Riscv_table_row.row) = String.equal r.feature "c" && List.mem r.mnemonic names
let double = [ "c.fld"; "c.fldsp"; "c.fsd"; "c.fsdsp" ]
let single = [ "c.flw"; "c.flwsp"; "c.fsw"; "c.fswsp" ]

let zca_forms =
  [
    (Opcode.C_addi, "rv_c/c.addi");
    (Opcode.C_and, "rv_c/c.and");
    (Opcode.C_or, "rv_c/c.or");
    (Opcode.C_xor, "rv_c/c.xor");
    (Opcode.C_sub, "rv_c/c.sub");
    (Opcode.C_addw, "rv64_c/c.addw");
    (Opcode.C_subw, "rv64_c/c.subw");
    (Opcode.C_jr, "rv_c/c.jr");
    (Opcode.C_jalr, "rv_c/c.jalr");
    (Opcode.C_mv, "rv_c/c.mv");
    (Opcode.C_add, "rv_c/c.add");
    (Opcode.C_ebreak, "rv_c/c.ebreak");
    (Opcode.C_lw, "rv_c/c.lw");
    (Opcode.C_sw, "rv_c/c.sw");
    (Opcode.C_lwsp, "rv_c/c.lwsp");
    (Opcode.C_swsp, "rv_c/c.swsp");
    (Opcode.C_ld, "rv64_c/c.ld");
    (Opcode.C_sd, "rv64_c/c.sd");
    (Opcode.C_ldsp, "rv64_c/c.ldsp");
    (Opcode.C_sdsp, "rv64_c/c.sdsp");
    (Opcode.C_beqz, "rv_c/c.beqz");
    (Opcode.C_j, "rv_c/c.j");
    (Opcode.C_jal, "rv32_c/c.jal");
    (Opcode.C_bnez, "rv_c/c.bnez");
  ]

let zca =
  Riscv_ext.component ~id:"riscv.zca" ~feature:"zca"
    ~rows:(fun r -> String.equal r.feature "c" && not (List.mem r.mnemonic (double @ single)))
    ~summary:"compressed integer instructions" zca_forms

let zcd =
  Riscv_ext.component ~id:"riscv.zcd" ~feature:"zcd" ~requires:[ "d"; "zca" ] ~default_on:false
    ~rows:(in_c double) ~summary:"compressed double-precision load/store" []

let zcf =
  Riscv_ext.component ~id:"riscv.zcf" ~feature:"zcf" ~requires:[ "f"; "zca" ] ~default_on:false
    ~xlens:[ 32 ] ~rows:(in_c single) ~summary:"compressed single-precision load/store" []

let zcb =
  let legal = function
    | "c.mul" -> Some (Req.All [ Feature "zcb"; Feature "zmmul" ])
    | "c.sext.b" | "c.sext.h" | "c.zext.h" -> Some (Req.All [ Feature "zcb"; Feature "zbb" ])
    | "c.zext.w" -> Some (Req.All [ Feature "zcb"; Feature "zba" ])
    | _ -> None
  in
  Riscv_ext.component ~id:"riscv.zcb" ~feature:"zcb" ~requires:[ "zca" ] ~legal
    ~summary:"compressed byte and halfword operations" []

let zcmp =
  Riscv_ext.component ~id:"riscv.zcmp" ~feature:"zcmp" ~requires:[ "zca" ] ~conflicts:[ "zcd" ]
    ~default_on:false ~summary:"push, pop and double-move" []

let zcmt =
  Riscv_ext.component ~id:"riscv.zcmt" ~feature:"zcmt" ~requires:[ "zca" ] ~conflicts:[ "zcd" ]
    ~default_on:false ~summary:"table jump" []

let zcmop =
  Riscv_ext.component ~id:"riscv.zcmop" ~feature:"zcmop" ~requires:[ "zca" ]
    ~summary:"compressed may-be-operations" []

let c =
  Riscv_ext.component ~id:"riscv.c" ~feature:"c" ~implies:[ "zca" ]
    ~conditional:[ (Has "d", "zcd"); (And [ Has "f"; Xlen 32 ], "zcf") ]
    ~rows:(fun _ -> false)
    ~summary:"compressed instructions: Zca, with Zcd and Zcf when D and F are present" []
