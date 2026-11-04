(* Typed state and operands of the targets other than RISC-V (which has test_riscv_types). *)

let%expect_test "ARM .syntax takes unified or divided and refuses anything else" =
  List.iter
    (fun arg ->
      let text = Printf.sprintf ".syntax %s\n\tmov r0, r1" arg in
      Printf.printf "%-8s %s\n" arg
        (match Test_zicsr.assembled "arm" text with
        | Ok bytes ->
            String.concat " "
              (List.init (String.length bytes) (fun i ->
                   Printf.sprintf "%02x" (Char.code bytes.[i])))
        | Error m -> m))
    [ "unified"; "divided"; "bogus" ];
  [%expect
    {|
    unified  01 00 a0 e1
    divided  01 00 a0 e1
    bogus    arm.directive: .syntax bogus: expected unified or divided |}]
