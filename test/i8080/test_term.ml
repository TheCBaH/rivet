(* [Rivet_compute.Term]: sharing, local rewrites, widths beyond 30 bits, evaluation and printing over
   the DAG. *)

module T = Rivet_compute.Term

let show t = print_endline (T.to_string t)
let x8 = T.var ~w:8 "x"
let y8 = T.var ~w:8 "y"

let%expect_test "equal terms are one term" =
  let a = T.add ~w:8 x8 y8 and b = T.add ~w:8 (T.var ~w:8 "x") (T.var ~w:8 "y") in
  Printf.printf "%b %b %b %b\n" (a == b)
    (T.const ~w:8 5 == T.const ~w:8 0x105)
    (T.var ~w:8 "x" == T.var ~w:9 "x")
    (T.equal a (T.add ~w:8 y8 x8));
  [%expect {| true true false false |}]

let%expect_test "local rewrites" =
  let c = T.var ~w:1 "c" in
  let zero = T.const ~w:8 0 and ones = T.const ~w:8 0xff in
  List.iter
    (fun (name, t) -> Printf.printf "%s: %s\n" name (T.to_string t))
    [
      ("x ^ x", T.logxor x8 x8);
      ("x ^ 0", T.logxor x8 zero);
      ("x & 0", T.logand x8 zero);
      ("x & x", T.logand x8 x8);
      ("x & ff", T.logand ones x8);
      ("x | 0", T.logor zero x8);
      ("x | ff", T.logor x8 ones);
      ("x + 0", T.add ~w:8 x8 zero);
      ("x - x", T.sub ~w:8 x8 x8);
      ("~~x", T.lognot ~w:8 (T.lognot ~w:8 x8));
      ("x == x", T.eq x8 x8);
      ("x <u x", T.ult x8 x8);
      ("c ? x : x", T.ite c x8 x8);
      ("1 ? x : y", T.ite (T.const ~w:1 1) x8 y8);
      ("x[7:0]", T.extract x8 ~hi:7 ~lo:0);
      ("zext8 x", T.zext ~w:8 x8);
      ("x >> 8", T.shr x8 8);
      ("x & y", T.logand x8 y8);
    ];
  [%expect
    {|
    x ^ x: 8'0
    x ^ 0: x
    x & 0: 8'0
    x & x: x
    x & ff: x
    x | 0: x
    x | ff: 8'ff
    x + 0: x
    x - x: 8'0
    ~~x: x
    x == x: 1'1
    x <u x: 1'0
    c ? x : x: x
    1 ? x : y: x
    x[7:0]: x
    zext8 x: x
    x >> 8: 8'0
    x & y: (x & y) |}]

let%expect_test "widths beyond 30 bits" =
  let w = 64 in
  let a = T.const64 ~w (-1L) and b = T.const ~w 1 in
  show (T.add ~w a b);
  show (T.sub ~w (T.const ~w 0) b);
  show (T.const ~w:40 (-1));
  show (T.ult b a);
  show (T.ult a b);
  show (T.concat ~hi:(T.const ~w:32 0xdead) ~lo:(T.const64 ~w:32 0xbeefL) ~lo_w:32);
  show (T.shl ~w (T.const ~w 3) 62);
  show (T.shr a 60);
  show (T.extract a ~hi:63 ~lo:32);
  Printf.printf "%b %b\n"
    (T.to_int (T.const ~w:40 0x3fff_ffff) = Some 0x3fff_ffff)
    (T.to_int (T.const ~w:40 0x4000_0000) = None);
  let v = T.var ~w:40 "v" in
  let e = T.add ~w:40 (T.shl ~w:40 v 35) v in
  Printf.printf "%Lx\n" (T.eval64 (fun _ -> 0x1_0000_0003L) e);
  (match T.eval (fun _ -> 0) e with
  | _ -> print_endline "eval of a wide term"
  | exception Invalid_argument m -> print_endline m);
  [%expect
    {|
    64'0
    64'ffffffffffffffff
    40'ffffffffff
    1'1
    1'0
    64'dead0000beef
    64'c000000000000000
    64'f
    32'ffffffff
    true true
    1900000003
    Compute.Term.eval: a 40 bits term does not fit an int; use eval64 |}]

let%expect_test "width errors" =
  let err f =
    match f () with
    | _ -> print_endline "no error"
    | exception Invalid_argument m -> print_endline m
  in
  err (fun () -> T.const ~w:65 0);
  err (fun () -> T.const ~w:0 0);
  err (fun () -> T.var ~w:100 "v");
  err (fun () -> T.logand x8 (T.var ~w:9 "z"));
  err (fun () -> T.add ~w:9 x8 y8);
  err (fun () -> T.concat ~hi:(T.var ~w:40 "h") ~lo:(T.var ~w:30 "l") ~lo_w:30);
  err (fun () -> T.extract x8 ~hi:8 ~lo:0);
  err (fun () -> Rivet_compute.Concrete.const ~w:31 0);
  [%expect
    {|
    Compute.Term: width must be 1..64
    Compute.Term: width must be 1..64
    Compute.Term: width must be 1..64
    Compute.Term.logand: operand widths 8 and 9 differ
    Compute.Term.add: operands must be 9 bits
    Compute.Term: width must be 1..64
    Compute.Term.extract: [8:0] of 8 bits
    Compute.Concrete: width must be 1..30 |}]

let%expect_test "sharing is printed once" =
  let s = T.add ~w:8 x8 y8 in
  show (T.logxor (T.add ~w:8 s s) (T.shl ~w:8 s 1));
  show (T.logxor (T.add ~w:8 x8 x8) y8);
  [%expect {|
    let t1 = (x + y) in
    ((t1 + t1) ^ (t1 << 1))
    ((x + x) ^ y) |}]

(* [x := (x + x) ^ y], [n] times. Sharing makes the DAG linear in [n] where the tree is
   exponential, so [size], [eval] and the printer must work on the DAG. *)
let doubling n =
  let y = T.var ~w:16 "y" in
  let rec go i x = if i = 0 then x else go (i - 1) (T.logxor (T.add ~w:16 x x) y) in
  go n (T.var ~w:16 "x")

let reference n ~x ~y =
  let rec go i x =
    if i = 0 then x
    else go (i - 1) (Rivet_compute.Concrete.logxor (Rivet_compute.Concrete.add ~w:16 x x) y)
  in
  go n x

let%expect_test "a doubling loop stays linear" =
  let n = 20_000 in
  let t = doubling n in
  let env = function "x" -> 0x1235 | _ -> 0x00f1 in
  let t0 = Sys.time () in
  let v = T.eval env t in
  let dt = Sys.time () -. t0 in
  Printf.printf "size=%d value=%b fast=%b\n" (T.size t)
    (v = reference n ~x:0x1235 ~y:0x00f1)
    (dt < 2.0);
  let s = T.to_string (doubling 1_000) in
  Printf.printf "printed in under 100 kB: %b\n" (String.length s < 100_000);
  [%expect {|
    size=40002 value=true fast=true
    printed in under 100 kB: true |}]

let%expect_test "one evaluator shares work between terms" =
  let a = doubling 50 in
  let b = T.logand a (T.var ~w:16 "x") in
  let ev = T.evaluator (function "x" -> 0x35L | _ -> 0xf1L) in
  let r = reference 50 ~x:0x35 ~y:0xf1 in
  Printf.printf "%b %b\n" (ev a = Int64.of_int r) (ev b = Int64.of_int (r land 0x35));
  [%expect {| true true |}]

let%expect_test "undecided and symbolic addresses as results" =
  let c = T.var ~w:1 "c" in
  let show_stop = function
    | Ok () -> print_endline "ok"
    | Error (Rivet_compute.Undecided t) -> Printf.printf "undecided %s\n" (T.to_string t)
    | Error (Rivet_compute.Symbolic_address t) ->
        Printf.printf "symbolic address %s\n" (T.to_string t)
  in
  Printf.printf "%s %s\n"
    (match T.try_decide (T.const ~w:1 1) with Ok b -> string_of_bool b | Error _ -> "stuck")
    (match T.try_decide c with Ok _ -> "decided" | Error t -> "stuck " ^ T.to_string t);
  show_stop (Rivet_compute.try_step (fun () -> ignore (T.decide c)));
  show_stop (Rivet_compute.try_step (fun () -> ignore (T.decide (T.const ~w:1 0))));
  let m = Rivet_compute.Term_memory.empty in
  let a = T.var ~w:16 "a" in
  show_stop (Rivet_compute.try_step (fun () -> ignore (Rivet_compute.Term_memory.load8 m a)));
  (match Rivet_compute.Term_memory.try_store8 m (T.const ~w:16 0x10) x8 with
  | Error _ -> print_endline "store failed"
  | Ok m ->
      Printf.printf "%s %s\n"
        (match Rivet_compute.Term_memory.try_load8 m (T.const ~w:16 0x10) with
        | Ok v -> T.to_string v
        | Error _ -> "load failed")
        (match Rivet_compute.Term_memory.try_load8 m a with
        | Ok _ -> "loaded"
        | Error t -> T.to_string t));
  (match Rivet_compute.Term_memory.try_store8 m a x8 with
  | Ok _ -> print_endline "stored"
  | Error t -> Printf.printf "store at %s refused\n" (T.to_string t));
  [%expect
    {|
    true stuck c
    undecided c
    ok
    symbolic address a
    x a
    store at a refused |}]
