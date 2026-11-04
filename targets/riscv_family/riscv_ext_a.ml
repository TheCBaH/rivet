(* Atomics. Zacas and Zabha have only generated rows; amocas.b and amocas.h are Zacas encodings of
   Zabha-width operations, so they need both. *)

let a_forms = Riscv_ops.forms "a"
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
