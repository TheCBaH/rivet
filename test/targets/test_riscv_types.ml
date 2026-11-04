(* The typed RISC-V operand kinds: each value reaches the encoding and comes back from it. *)

module Types = Riscv_family_encode.Riscv_types
module Rm = Types.Rm

let%expect_test "every rounding mode round-trips through fcvt.w.s" =
  List.iter (fun rm -> Test_zicsr.round_trip "riscv64" ("fcvt.w.s a0, fa0, " ^ Rm.name rm)) Rm.all;
  [%expect
    {|
    fcvt.w.s a0, fa0, rne        c0050553 fcvt.w.s x10, f10, rne       same
    fcvt.w.s a0, fa0, rtz        c0051553 fcvt.w.s x10, f10, rtz       same
    fcvt.w.s a0, fa0, rdn        c0052553 fcvt.w.s x10, f10, rdn       same
    fcvt.w.s a0, fa0, rup        c0053553 fcvt.w.s x10, f10, rup       same
    fcvt.w.s a0, fa0, rmm        c0054553 fcvt.w.s x10, f10, rmm       same
    fcvt.w.s a0, fa0, dyn        c0057553 fcvt.w.s x10, f10            same |}]

let%expect_test "reserved rounding-mode encodings decode as before" =
  List.iter
    (fun f3 ->
      let w = Int64.logor 0xc0050553L (Int64.of_int (f3 lsl 12)) in
      Printf.printf "funct3 %d: of_funct3 %s, %08Lx -> %s\n" f3
        (match Rm.of_funct3 f3 with Some r -> Rm.name r | None -> "none")
        w
        (match Test_zicsr.decoded "riscv64" (Test_zicsr.word_bytes w) with
        | Ok t -> t
        | Error m -> "REFUSED " ^ m))
    [ 5; 6; 7 ];
  [%expect
    {|
    funct3 5: of_funct3 none, c0055553 -> fcvt.w.s x10, f10
    funct3 6: of_funct3 none, c0056553 -> fcvt.w.s x10, f10
    funct3 7: of_funct3 dyn, c0057553 -> fcvt.w.s x10, f10 |}]
