(* Atomics. Zacas and Zabha have only generated rows; amocas.b and amocas.h are Zacas encodings of
   Zabha-width operations, so they need both. *)

module Opcode = Riscv_types.Opcode

let a_forms =
  [
    (Opcode.Amoswap_w, "rv_a/amoswap.w");
    (Opcode.Amoadd_w, "rv_a/amoadd.w");
    (Opcode.Amoxor_w, "rv_a/amoxor.w");
    (Opcode.Amoand_w, "rv_a/amoand.w");
    (Opcode.Amoor_w, "rv_a/amoor.w");
    (Opcode.Amomin_w, "rv_a/amomin.w");
    (Opcode.Amomax_w, "rv_a/amomax.w");
    (Opcode.Amominu_w, "rv_a/amominu.w");
    (Opcode.Amomaxu_w, "rv_a/amomaxu.w");
    (Opcode.Lr_w, "rv_a/lr.w");
    (Opcode.Sc_w, "rv_a/sc.w");
    (Opcode.Amoswap_d, "rv64_a/amoswap.d");
    (Opcode.Amoadd_d, "rv64_a/amoadd.d");
    (Opcode.Amoxor_d, "rv64_a/amoxor.d");
    (Opcode.Amoand_d, "rv64_a/amoand.d");
    (Opcode.Amoor_d, "rv64_a/amoor.d");
    (Opcode.Amomin_d, "rv64_a/amomin.d");
    (Opcode.Amomax_d, "rv64_a/amomax.d");
    (Opcode.Amominu_d, "rv64_a/amominu.d");
    (Opcode.Amomaxu_d, "rv64_a/amomaxu.d");
    (Opcode.Lr_d, "rv64_a/lr.d");
    (Opcode.Sc_d, "rv64_a/sc.d");
  ]

let a = Riscv_ext.component ~id:"riscv.a" ~feature:"a" ~summary:"atomic memory operations" a_forms

let zacas =
  let legal m =
    if String.starts_with ~prefix:"amocas.b" m || String.starts_with ~prefix:"amocas.h" m then
      Some (Target_component.Req.All [ Feature "zacas"; Feature "zabha" ])
    else None
  in
  Riscv_ext.component ~id:"riscv.zacas" ~feature:"zacas" ~requires:[ "a" ] ~legal
    ~summary:"atomic compare-and-swap" []

let zabha =
  Riscv_ext.component ~id:"riscv.zabha" ~feature:"zabha" ~requires:[ "a" ]
    ~summary:"byte and halfword atomic operations" []
