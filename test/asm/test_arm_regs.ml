(* The ARM register classes keep the REGS laws, their names are the parser's spellings, the named
   values are the registers their names read as, and every register of a class assembles in an
   instruction that takes the class. *)

open Arm_encode

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
  List.for_all (fun n -> Result.is_ok (Harness.assemble "arm" (text n))) numbers

let%expect_test "register classes" =
  let report name ok = Printf.printf "%-5s %b\n" name ok in
  report "core" (laws (module Reg));
  report "d" (laws (module Dreg));
  report "s" (laws (module Sreg));
  report "lo" (laws (module Lo));
  (* names read back as the registers, both ways *)
  report "core names" (List.for_all (fun r -> Reg.find (Reg.name r) = Some r) Reg.all);
  report "d names" (List.for_all (fun r -> Dreg.find (Dreg.name r) = Some r) Dreg.all);
  report "s names" (List.for_all (fun r -> Sreg.find (Sreg.name r) = Some r) Sreg.all);
  report "lo names" (List.for_all (fun r -> Reg.find (Lo.name r) = Some (Lo.to_reg r)) Lo.all);
  (* and every register assembles in its class *)
  report "core asm" (assembles (Printf.sprintf "mov r%d, r0") Reg.numbers);
  report "d asm" (assembles (Printf.sprintf "vmov.f64 d%d, d0") Dreg.numbers);
  report "s asm" (assembles (Printf.sprintf "vmov.f32 s%d, s0") Sreg.numbers);
  report "lo asm" (assembles (Printf.sprintf "mov r%d, r0") Lo.numbers);
  [%expect
    {|
    core  true
    d     true
    s     true
    lo    true
    core names true
    d names true
    s names true
    lo names true
    core asm true
    d asm true
    s asm true
    lo asm true |}]

let%expect_test "named values" =
  let open Reg in
  let same a b = Reg.equal a b in
  Printf.printf "%b\n"
    (same r0 (of_int_exn 0)
    && same r15 (of_int_exn 15)
    && same sp (of_int_exn 13)
    && same lr (of_int_exn 14)
    && same pc (of_int_exn 15)
    && same ip (of_int_exn 12)
    && same fp (of_int_exn 11)
    && same sl (of_int_exn 10)
    && List.for_all
         (fun (s, r) -> Reg.find s = Some r)
         [ ("sp", sp); ("lr", lr); ("pc", pc); ("ip", ip); ("fp", fp); ("sl", sl) ]);
  Printf.printf "%b\n"
    (Dreg.equal Dreg.d0 (Dreg.of_int_exn 0)
    && Dreg.equal Dreg.d31 (Dreg.of_int_exn 31)
    && Sreg.equal Sreg.s0 (Sreg.of_int_exn 0)
    && Sreg.equal Sreg.s31 (Sreg.of_int_exn 31));
  [%expect {|
    true
    true |}]
