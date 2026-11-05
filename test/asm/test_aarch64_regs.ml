(* The AArch64 register classes keep the REGS laws, their names are the parser's spellings, the
   named values are the registers their names read as, and every register of a class assembles in
   an instruction that takes the class. *)

open Aarch64_encode

module type REGS = Target_encode.REGS

let laws (type t) (module R : REGS with type t = t) =
  let probe = List.init 66 (fun n -> n - 1) in
  R.numbers = List.filter R.mem probe
  && List.for_all (fun n -> Option.is_some (R.of_int n) = List.mem n R.numbers) probe
  && List.for_all (fun n -> R.to_int (R.of_int_exn n) = n) R.numbers
  && R.count = List.length R.numbers
  && List.map R.to_int R.all = R.numbers
  && R.range = Some (List.hd R.numbers, List.nth R.numbers (R.count - 1))

(* [text n] assembles with register [n] in the class's position *)
let assembles text numbers =
  List.for_all (fun n -> Result.is_ok (Harness.assemble "aarch64" (text n))) numbers

let%expect_test "register classes" =
  let report name ok = Printf.printf "%-7s %b\n" name ok in
  report "x" (laws (module Xreg));
  report "w" (laws (module Wreg));
  report "d" (laws (module Dreg));
  report "s" (laws (module Sreg));
  report "q" (laws (module Qreg));
  report "v" (laws (module Vreg));
  (* names read back as the registers, both ways *)
  let reg_of_x r = (r : Xreg.t :> Reg.t) and reg_of_w r = (r : Wreg.t :> Reg.t) in
  report "x names" (List.for_all (fun r -> Reg.find (Xreg.name r) = Some (reg_of_x r)) Xreg.all);
  report "w names" (List.for_all (fun r -> Reg.find (Wreg.name r) = Some (reg_of_w r)) Wreg.all);
  report "d names" (List.for_all (fun r -> Freg.find (Dreg.name r) = Some (r :> Freg.t)) Dreg.all);
  report "s names" (List.for_all (fun r -> Freg.find (Sreg.name r) = Some (r :> Freg.t)) Sreg.all);
  report "x find" (List.for_all (fun r -> Xreg.find (Xreg.name r) = Some r) Xreg.all);
  report "v find" (List.for_all (fun r -> Vreg.find (Vreg.name r) = Some r) Vreg.all);
  (* and every register assembles in its class *)
  report "x asm" (assembles (Printf.sprintf "add x%d, x0, x0") Xreg.numbers);
  report "w asm" (assembles (Printf.sprintf "add w%d, w0, w0") Wreg.numbers);
  report "d asm" (assembles (Printf.sprintf "fmov d%d, d0") Dreg.numbers);
  report "s asm" (assembles (Printf.sprintf "fmov s%d, s0") Sreg.numbers);
  report "q asm" (assembles (Printf.sprintf "ldr q%d, [x0]") Qreg.numbers);
  report "v asm" (assembles (Printf.sprintf "mov v%d.16b, v0.16b") Vreg.numbers);
  [%expect
    {|
    x       true
    w       true
    d       true
    s       true
    q       true
    v       true
    x names true
    w names true
    d names true
    s names true
    x find  true
    v find  true
    x asm   true
    w asm   true
    d asm   true
    s asm   true
    q asm   true
    v asm   true |}]

let%expect_test "stack pointer and zero registers" =
  let same name r = Reg.find name = Some (r : (_, _) View.greg :> Reg.t) in
  Printf.printf "%b %b %b %b\n" (same "sp" sp) (same "wsp" wsp) (same "xzr" xzr) (same "wzr" wzr);
  List.iter
    (fun text -> Printf.printf "%s %b\n" text (Result.is_ok (Harness.assemble "aarch64" text)))
    [ "add sp, sp, #16"; "add wsp, wsp, #16"; "add x0, xzr, x1"; "add w0, wzr, w1" ];
  [%expect
    {|
    true true true true
    add sp, sp, #16 true
    add wsp, wsp, #16 true
    add x0, xzr, x1 true
    add w0, wzr, w1 true |}]
