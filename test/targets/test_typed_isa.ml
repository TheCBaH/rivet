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

(* x86 legality is looked up by opcode: a hand-written opcode needs what the component publishing
   its spelling provides, a row what its ISA set needs, in both modes. *)
module X86_req (E : module type of X86_64_encode) = struct
  let check label =
    let need op = E.required_feature (E.Instruction.mk op 32 []) in
    let by_spelling op = Target_component.feature_of_mnemonic E.components (E.Opcode.name op) in
    let hand = Array.to_list E.Opcode.all in
    let rows = List.init (Array.length E.table_rows) X86_family_encode.Row.of_index in
    let differ =
      List.filter_map
        (fun op -> if need op = by_spelling op then None else Some (E.Opcode.name op))
        hand
      @ List.filter_map
          (fun r ->
            let op = E.Opcode.Table r in
            if need op = E.row_feature (X86_family_encode.Row.index r) then None
            else Some (E.Opcode.name op))
          rows
    in
    let gated = List.filter (fun op -> need op <> None) hand in
    Printf.printf "%s: %d opcodes (%d gated), %d rows, differ [%s]\n" label (List.length hand)
      (List.length gated) (List.length rows) (String.concat " " differ)
end

let%expect_test "x86 requirements by opcode agree with the components' spellings" =
  let module A = X86_req (X86_32_encode) in
  let module B = X86_req (X86_64_encode) in
  A.check "x86_32";
  B.check "x86_64";
  [%expect
    {|
    x86_32: 463 opcodes (13 gated), 12055 rows, differ []
    x86_64: 463 opcodes (13 gated), 12055 rows, differ [] |}]
