(* [Compute.Concrete64]: the edges of 64 bits, which the property tests in test/compute_props
   cover in bulk. *)

module C = Compute.Concrete64

let hex = Printf.printf "%Lx\n"

let%expect_test "64 bits with the sign bit set" =
  let top = C.const64 ~w:64 Int64.min_int in
  hex top;
  hex (C.const ~w:64 (-1));
  hex (C.add ~w:64 top top);
  hex (C.add ~w:64 (C.const ~w:64 (-1)) (C.const ~w:64 1));
  hex (C.sub ~w:64 (C.const ~w:64 0) (C.const ~w:64 1));
  hex (C.lognot ~w:64 top);
  hex (C.ult top (C.const ~w:64 1));
  hex (C.ult (C.const ~w:64 1) top);
  hex (C.eq top top);
  [%expect
    {|
    8000000000000000
    ffffffffffffffff
    0
    0
    ffffffffffffffff
    7fffffffffffffff
    0
    1
    1 |}]

let%expect_test "shifts at and beyond the width" =
  let x = C.const64 ~w:64 (-1L) in
  List.iter
    (fun k -> Printf.printf "%d: %Lx %Lx\n" k (C.shl ~w:64 x k) (C.shr x k))
    [ 0; 1; 63; 64; 65; 200 ];
  let y = C.const ~w:8 0xff in
  List.iter
    (fun k -> Printf.printf "w8 %d: %Lx %Lx\n" k (C.shl ~w:8 y k) (C.shr y k))
    [ 7; 8; 9; 64 ];
  [%expect
    {|
    0: ffffffffffffffff ffffffffffffffff
    1: fffffffffffffffe 7fffffffffffffff
    63: 8000000000000000 1
    64: 0 0
    65: 0 0
    200: 0 0
    w8 7: 80 1
    w8 8: 0 0
    w8 9: 0 0
    w8 64: 0 0 |}]

let%expect_test "extract and concat at the top" =
  let x = C.const64 ~w:64 0xfedcba9876543210L in
  hex (C.extract x ~hi:63 ~lo:56);
  hex (C.extract x ~hi:63 ~lo:63);
  hex (C.extract x ~hi:63 ~lo:0);
  hex (C.extract x ~hi:0 ~lo:0);
  let hi = C.const ~w:32 (-1) and lo = C.const ~w:32 0x12345678 in
  hex (C.concat ~hi ~lo ~lo_w:32);
  hex (C.concat ~hi:(C.const ~w:1 1) ~lo:(C.const64 ~w:63 (-1L)) ~lo_w:63);
  hex (C.concat ~hi:(C.const ~w:8 0xab) ~lo:0x1ffL ~lo_w:8);
  [%expect
    {|
    fe
    1
    fedcba9876543210
    0
    ffffffff12345678
    ffffffffffffffff
    abff |}]

let%expect_test "const, ite, to_int and the width limits" =
  List.iter
    (fun w ->
      match C.const ~w 5 with
      | v -> Printf.printf "%d: ok %Lx\n" w v
      | exception Invalid_argument m -> Printf.printf "%d: %s\n" w m)
    [ 0; 1; 64; 65 ];
  hex (C.const ~w:12 (-1));
  hex (C.ite (C.const ~w:1 1) 7L 9L);
  hex (C.ite (C.const ~w:1 0) 7L 9L);
  let show = function Some n -> string_of_int n | None -> "none" in
  Printf.printf "%s %s %s\n"
    (show (C.to_int 0x3fffffffL))
    (show (C.to_int 0x40000000L))
    (show (C.to_int (-1L)));
  [%expect
    {|
    0: Compute.Concrete64: width must be 1..64
    1: ok 1
    64: ok 5
    65: Compute.Concrete64: width must be 1..64
    fff
    7
    9
    1073741823 none none |}]

module M = Compute.Sparse_memory

let%expect_test "Sparse_memory allocates on write, reads zero elsewhere" =
  let m = M.create () in
  Printf.printf "%d pages, %Lx %Lx\n" (M.pages m) (M.load8 m 0L) (M.load8 m (-1L));
  Printf.printf "%d pages after reads\n" (M.pages m);
  ignore (M.store8 m 0x1234L 0xabL);
  ignore (M.store8 m 0x1235L 0x1cdL);
  Printf.printf "%Lx %Lx %Lx, %d page\n" (M.load8 m 0x1234L) (M.load8 m 0x1235L) (M.load8 m 0x1236L)
    (M.pages m);
  [%expect {|
    0 pages, 0 0
    0 pages after reads
    ab cd 0, 1 page |}]

let%expect_test "Sparse_memory across pages and at the top of the address space" =
  let m = M.create () in
  let ps = Int64.of_int M.page_size in
  let last_of_page = Int64.pred ps in
  ignore (M.store8 m last_of_page 1L);
  ignore (M.store8 m ps 2L);
  Printf.printf "%Lx %Lx %d pages\n" (M.load8 m last_of_page) (M.load8 m ps) (M.pages m);
  ignore (M.store8 m (-1L) 3L);
  ignore (M.store8 m (C.add ~w:64 (-1L) 1L) 4L);
  ignore (M.store8 m Int64.min_int 5L);
  ignore (M.store8 m Int64.max_int 6L);
  Printf.printf "%Lx %Lx %Lx %Lx %d pages\n" (M.load8 m (-1L)) (M.load8 m 0L)
    (M.load8 m Int64.min_int) (M.load8 m Int64.max_int) (M.pages m);
  Printf.printf "%Lx %Lx\n"
    (M.load8 m (Int64.add Int64.min_int 1L))
    (M.load8 m (Int64.sub Int64.max_int 1L));
  [%expect {|
    1 2 2 pages
    3 4 5 6 5 pages
    0 0 |}]
