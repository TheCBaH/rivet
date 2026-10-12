(* Atomics. Zacas and Zabha have only generated rows; amocas.b and amocas.h are Zacas encodings of
   Zabha-width operations, so they need both. *)

let a_forms =
  [
    ("amoswap.w", "rv_a/amoswap.w");
    ("amoadd.w", "rv_a/amoadd.w");
    ("amoxor.w", "rv_a/amoxor.w");
    ("amoand.w", "rv_a/amoand.w");
    ("amoor.w", "rv_a/amoor.w");
    ("amomin.w", "rv_a/amomin.w");
    ("amomax.w", "rv_a/amomax.w");
    ("amominu.w", "rv_a/amominu.w");
    ("amomaxu.w", "rv_a/amomaxu.w");
    ("lr.w", "rv_a/lr.w");
    ("sc.w", "rv_a/sc.w");
    ("amoswap.d", "rv64_a/amoswap.d");
    ("amoadd.d", "rv64_a/amoadd.d");
    ("amoxor.d", "rv64_a/amoxor.d");
    ("amoand.d", "rv64_a/amoand.d");
    ("amoor.d", "rv64_a/amoor.d");
    ("amomin.d", "rv64_a/amomin.d");
    ("amomax.d", "rv64_a/amomax.d");
    ("amominu.d", "rv64_a/amominu.d");
    ("amomaxu.d", "rv64_a/amomaxu.d");
    ("lr.d", "rv64_a/lr.d");
    ("sc.d", "rv64_a/sc.d");
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
