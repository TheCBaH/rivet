(* The preconditions of the concrete primitives. *)

let%expect_test "Bytes_memory refuses a length that would alias or index out of range" =
  List.iter
    (fun n ->
      match Compute.Bytes_memory.create n with
      | m -> Printf.printf "%d: ok, %d bytes\n" n (Bytes.length m)
      | exception Invalid_argument msg -> Printf.printf "%d: %s\n" n msg)
    [ 0; 1; 3; 100; 256; 65536 ];
  [%expect
    {|
    0: Compute.Bytes_memory.create: size must be a power of two
    1: ok, 1 bytes
    3: Compute.Bytes_memory.create: size must be a power of two
    100: Compute.Bytes_memory.create: size must be a power of two
    256: ok, 256 bytes
    65536: ok, 65536 bytes |}]

let%expect_test "Bytes_memory access never leaves the buffer" =
  let m = Compute.Bytes_memory.create 16 in
  ignore (Compute.Bytes_memory.store8 m 17 0xab);
  Printf.printf "%02x %02x\n" (Compute.Bytes_memory.load8 m 1) (Compute.Bytes_memory.load8 m 33);
  (match Compute.Bytes_memory.load8 (Bytes.create 0) 5 with
  | _ -> print_endline "read an empty memory"
  | exception Invalid_argument _ -> print_endline "empty memory: Invalid_argument");
  [%expect {|
    ab ab
    empty memory: Invalid_argument |}]

let%expect_test "Concrete.concat takes only the lo_w bits of lo" =
  Printf.printf "%x %x\n"
    (Compute.Concrete.concat ~hi:1 ~lo:0x1ff ~lo_w:8)
    (Compute.Concrete.concat ~hi:0 ~lo:0x1ff ~lo_w:8);
  [%expect {| 1ff ff |}]

let%expect_test "Width bounds and masks" =
  let module W = Compute.Width in
  Printf.printf "%d %d %x %Lx %Lx\n" W.max_concrete W.max_term (W.mask 30) (W.mask64 40)
    (W.mask64 64);
  List.iter
    (fun w ->
      match W.check ~who:"X" ~max:W.max_term w with
      | () -> Printf.printf "%d: ok\n" w
      | exception Invalid_argument m -> Printf.printf "%d: %s\n" w m)
    [ 0; 1; 64; 65 ];
  [%expect
    {|
    30 64 3fffffff ffffffffff ffffffffffffffff
    0: X: width must be 1..64
    1: ok
    64: ok
    65: X: width must be 1..64 |}]
