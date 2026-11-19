(* Concrete64 against Concrete (widths up to 30), against Bigint (widths up to 64), and against
   Term.eval64 on random terms; Sparse_memory against a table. Values are unsigned and masked
   to their width throughout. *)

let seed = 20261119

module Q = QCheck2
module G = Q.Gen
module B = Foundation.Bigint
module C = Compute.Concrete
module C64 = Compute.Concrete64
module T = Compute.Term
module Mem = Compute.Sparse_memory

let ( let* ) = G.bind
let mask = Compute.Width.mask64
let big x = B.of_string_exn (Printf.sprintf "%Lu" x)
let pow2 n = B.shift_left_exn B.one n
let modw w x = B.rem_exn x (pow2 w)

let unbig x =
  match B.to_uint64_opt x with
  | Some v -> v
  | None -> Q.Test.fail_reportf "out of range: %s" (B.to_string x)

(* Edge values weigh in beside uniform ones: the sign bit, its predecessor, all ones. *)
let gen_val w =
  let edges =
    [ 0L; 1L; mask w; Int64.shift_left 1L (w - 1); Int64.pred (Int64.shift_left 1L (w - 1)) ]
  in
  G.oneof_weighted
    [
      (3, G.map (fun v -> Int64.logand v (mask w)) G.int64);
      (2, G.oneof_list (List.map (fun v -> Int64.logand v (mask w)) edges));
    ]

let check_all props =
  List.iter (fun (name, ok) -> if not ok then Q.Test.fail_reportf "%s" name) props;
  true

(* {1 Concrete64 against Concrete} *)

type small = {
  w : int;
  a : int64;
  b : int64;
  k : int;
  c : bool;
  lo : int;
  hi : int;
  w2 : int;
  raw : int;
}

let gen_small =
  let* w = G.int_range 1 30 in
  let* a = gen_val w in
  let* b = gen_val w in
  let* k = G.int_range 0 30 in
  let* c = G.bool in
  let* lo = G.int_range 0 (w - 1) in
  let* hi = G.int_range lo (w - 1) in
  let* w2 = G.int_range w 30 in
  let* raw = G.int_range (-0x40000000) 0x3fffffff in
  G.return { w; a; b; k; c; lo; hi; w2; raw }

let show_small s =
  Printf.sprintf "w=%d a=%Lx b=%Lx k=%d c=%b [%d:%d] w2=%d raw=%d" s.w s.a s.b s.k s.c s.hi s.lo
    s.w2 s.raw

let prop_vs_concrete =
  Q.Test.make ~count:5000 ~name:"Concrete64 agrees with Concrete up to 30 bits" ~print:show_small
    gen_small (fun (s : small) ->
      let w = s.w in
      let a = Int64.to_int s.a and b = Int64.to_int s.b in
      let same x y = Int64.equal x (Int64.of_int y) in
      let bit v = if v then 1 else 0 in
      let w1 = (w + 1) / 2 in
      let w_lo = w - w1 in
      let hi_part = C.extract a ~hi:(w - 1) ~lo:w_lo
      and lo_part = C.extract a ~hi:(w_lo - 1) ~lo:0 in
      check_all
        [
          ("const", same (C64.const ~w s.raw) (C.const ~w s.raw));
          ("add", same (C64.add ~w s.a s.b) (C.add ~w a b));
          ("sub", same (C64.sub ~w s.a s.b) (C.sub ~w a b));
          ("logand", same (C64.logand s.a s.b) (C.logand a b));
          ("logor", same (C64.logor s.a s.b) (C.logor a b));
          ("logxor", same (C64.logxor s.a s.b) (C.logxor a b));
          ("lognot", same (C64.lognot ~w s.a) (C.lognot ~w a));
          ("shl", same (C64.shl ~w s.a s.k) (C.shl ~w a s.k));
          ("shr", same (C64.shr s.a s.k) (C.shr a s.k));
          ("zext", same (C64.zext ~w:s.w2 s.a) (C.zext ~w:s.w2 a));
          ("extract", same (C64.extract s.a ~hi:s.hi ~lo:s.lo) (C.extract a ~hi:s.hi ~lo:s.lo));
          ( "concat",
            w_lo = 0
            || same
                 (C64.concat
                    ~hi:(C64.extract s.a ~hi:(w - 1) ~lo:w_lo)
                    ~lo:(C64.extract s.a ~hi:(w_lo - 1) ~lo:0)
                    ~lo_w:w_lo)
                 (C.concat ~hi:hi_part ~lo:lo_part ~lo_w:w_lo) );
          ( "concat masks lo",
            same (C64.concat ~hi:s.a ~lo:s.b ~lo_w:w) (C.concat ~hi:a ~lo:b ~lo_w:w) || w * 2 > 30
          );
          ("eq", same (C64.eq s.a s.b) (C.eq a b));
          ("ult", same (C64.ult s.a s.b) (C.ult a b));
          ( "ite",
            same (C64.ite (C64.const ~w:1 (bit s.c)) s.a s.b) (C.ite (C.const ~w:1 (bit s.c)) a b)
          );
          ("decide", C64.decide (C64.const ~w:1 (bit s.c)) = s.c);
          ("to_int", C64.to_int s.a = C.to_int a);
        ])

(* {1 Concrete64 against Bigint} *)

type wide = {
  w : int;
  a : int64;
  b : int64;
  k : int;
  c : bool;
  lo : int;
  hi : int;
  w1 : int;
  w2 : int;
}

let gen_wide =
  let* w = G.int_range 1 64 in
  let* a = gen_val w in
  let* b = gen_val w in
  let* k = G.int_range 0 130 in
  let* c = G.bool in
  let* lo = G.int_range 0 (w - 1) in
  let* hi = G.int_range lo (w - 1) in
  let* w1 = G.int_range 1 (max 1 (w - 1)) in
  let w2 = if w = 1 then 1 else w - w1 in
  G.return { w; a; b; k; c; lo; hi; w1; w2 }

let show_wide s =
  Printf.sprintf "w=%d a=%Lx b=%Lx k=%d c=%b [%d:%d] w1=%d w2=%d" s.w s.a s.b s.k s.c s.hi s.lo s.w1
    s.w2

let prop_vs_bigint =
  Q.Test.make ~count:5000 ~name:"Concrete64 agrees with Bigint up to 64 bits" ~print:show_wide
    gen_wide (fun (s : wide) ->
      let w = s.w in
      let ua = big s.a and ub = big s.b in
      let is x y = Int64.equal x (unbig y) in
      let flag v = if v then B.one else B.zero in
      (* a [w1]-bit high part over a [w2]-bit low part; only for w >= 2 *)
      let hi_v = Int64.logand s.a (mask s.w1) and lo_v = Int64.logand s.b (mask s.w2) in
      check_all
        ([
           ("add", is (C64.add ~w s.a s.b) (modw w (B.add ua ub)));
           ("sub", is (C64.sub ~w s.a s.b) (modw w (B.add (B.sub ua ub) (pow2 w))));
           ("logand", is (C64.logand s.a s.b) (B.logand ua ub));
           ("logor", is (C64.logor s.a s.b) (B.logor ua ub));
           ("logxor", is (C64.logxor s.a s.b) (B.logxor ua ub));
           ("lognot", is (C64.lognot ~w s.a) (B.sub (B.sub (pow2 w) B.one) ua));
           ("shl", is (C64.shl ~w s.a s.k) (modw w (B.shift_left_exn ua s.k)));
           ("shr", is (C64.shr s.a s.k) (B.shift_right_exn ua s.k));
           ( "extract",
             is
               (C64.extract s.a ~hi:s.hi ~lo:s.lo)
               (modw (s.hi - s.lo + 1) (B.shift_right_exn ua s.lo)) );
           ("eq", is (C64.eq s.a s.b) (flag (B.equal ua ub)));
           ("ult", is (C64.ult s.a s.b) (flag (B.compare ua ub < 0)));
           ( "ite",
             Int64.equal
               (C64.ite (C64.const ~w:1 (if s.c then 1 else 0)) s.a s.b)
               (if s.c then s.a else s.b) );
           ("zext", Int64.equal (C64.zext ~w:64 s.a) s.a);
           ("const64", is (C64.const64 ~w (Int64.mul s.a s.b)) (modw w (big (Int64.mul s.a s.b))));
         ]
        @
        if s.w >= 2 then
          [
            ( "concat",
              is
                (C64.concat ~hi:hi_v ~lo:lo_v ~lo_w:s.w2)
                (B.add (B.mul (big hi_v) (pow2 s.w2)) (big lo_v)) );
            ( "concat masks lo",
              is
                (C64.concat ~hi:hi_v ~lo:s.b ~lo_w:s.w2)
                (B.add (B.mul (big hi_v) (pow2 s.w2)) (modw s.w2 ub)) );
          ]
        else []))

(* {1 Term.eval64 against Concrete64} *)

type expr =
  | Var of int * string
  | Const of int * int64
  | Add of int * expr * expr
  | Sub of int * expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Xor of expr * expr
  | Not of int * expr
  | Shl of int * expr * int
  | Shr of expr * int
  | Zext of int * expr
  | Extract of expr * int * int
  | Concat of expr * expr
  | Eq of expr * expr
  | Ult of expr * expr
  | Ite of expr * expr * expr

let rec width = function
  | Var (w, _)
  | Const (w, _)
  | Add (w, _, _)
  | Sub (w, _, _)
  | Not (w, _)
  | Shl (w, _, _)
  | Zext (w, _) ->
      w
  | And (a, _) | Or (a, _) | Xor (a, _) | Shr (a, _) -> width a
  | Extract (_, hi, lo) -> hi - lo + 1
  | Concat (a, b) -> width a + width b
  | Eq _ | Ult _ -> 1
  | Ite (_, a, _) -> width a

let rec gen_expr ~wmax ~kmax w fuel =
  let leaf =
    G.oneof
      [
        G.map (fun i -> Var (w, Printf.sprintf "x%d_%d" w i)) (G.int_range 0 1);
        G.map (fun v -> Const (w, v)) (gen_val w);
      ]
  in
  if fuel <= 0 then leaf
  else
    let sub w = gen_expr ~wmax ~kmax w (fuel - 1) in
    let* choice = G.int_range 0 13 in
    match choice with
    | 1 -> G.map2 (fun a b -> Add (w, a, b)) (sub w) (sub w)
    | 2 -> G.map2 (fun a b -> Sub (w, a, b)) (sub w) (sub w)
    | 3 -> G.map2 (fun a b -> And (a, b)) (sub w) (sub w)
    | 4 -> G.map2 (fun a b -> Or (a, b)) (sub w) (sub w)
    | 5 -> G.map2 (fun a b -> Xor (a, b)) (sub w) (sub w)
    | 6 -> G.map (fun a -> Not (w, a)) (sub w)
    | 7 ->
        let* k = G.int_range 0 kmax in
        G.map (fun a -> Shl (w, a, k)) (sub w)
    | 8 ->
        let* k = G.int_range 0 kmax in
        G.map (fun a -> Shr (a, k)) (sub w)
    | 9 when w > 1 ->
        let* w' = G.int_range 1 w in
        G.map (fun a -> Zext (w, a)) (sub w')
    | 10 ->
        let* w' = G.int_range w wmax in
        let* lo = G.int_range 0 (w' - w) in
        G.map (fun a -> Extract (a, lo + w - 1, lo)) (sub w')
    | 11 when w > 1 ->
        let* lo_w = G.int_range 1 (w - 1) in
        G.map2 (fun a b -> Concat (a, b)) (sub (w - lo_w)) (sub lo_w)
    | 12 -> G.map3 (fun c a b -> Ite (c, a, b)) (sub 1) (sub w) (sub w)
    | 13 when w = 1 ->
        let* w' = G.int_range 1 wmax in
        let* is_eq = G.bool in
        G.map2 (fun a b -> if is_eq then Eq (a, b) else Ult (a, b)) (sub w') (sub w')
    | _ -> leaf

let gen_top ~wmax ~kmax =
  let* w = G.int_range 1 wmax in
  let* e = gen_expr ~wmax ~kmax w 5 in
  let* env_seed = G.int_range 0 0xfffff in
  G.return (e, env_seed)

let env_value env_seed name =
  let h = Hashtbl.hash (env_seed, name) in
  match h mod 3 with
  | 0 -> List.nth [ 0L; -1L; 1L; Int64.min_int; Int64.max_int; 0x80000000L ] (h / 3 mod 6)
  | _ -> Int64.(mul (of_int (h + 1)) 0x9e3779b97f4a7c15L)

module Interp (V : sig
  include Compute.VALUE

  val var : w:int -> string -> t
  val const64 : w:int -> int64 -> t
end) =
struct
  let rec go = function
    | Var (w, n) -> V.var ~w n
    | Const (w, v) -> V.const64 ~w v
    | Add (w, a, b) -> V.add ~w (go a) (go b)
    | Sub (w, a, b) -> V.sub ~w (go a) (go b)
    | And (a, b) -> V.logand (go a) (go b)
    | Or (a, b) -> V.logor (go a) (go b)
    | Xor (a, b) -> V.logxor (go a) (go b)
    | Not (w, a) -> V.lognot ~w (go a)
    | Shl (w, a, k) -> V.shl ~w (go a) k
    | Shr (a, k) -> V.shr (go a) k
    | Zext (w, a) -> V.zext ~w (go a)
    | Extract (a, hi, lo) -> V.extract (go a) ~hi ~lo
    | Concat (a, b) -> V.concat ~hi:(go a) ~lo:(go b) ~lo_w:(width b)
    | Eq (a, b) -> V.eq (go a) (go b)
    | Ult (a, b) -> V.ult (go a) (go b)
    | Ite (c, a, b) -> V.ite (go c) (go a) (go b)
end

module On_term = Interp (struct
  include T

  let var = T.var
  let const64 = T.const64
end)

let on_c64 env_seed =
  let module I = Interp (struct
    include C64

    let var ~w n = C64.const64 ~w (env_value env_seed n)
  end) in
  I.go

let on_c env_seed =
  let module I = Interp (struct
    include C

    let var ~w n = C.const ~w (Int64.to_int (env_value env_seed n))
    let const64 ~w v = C.const ~w (Int64.to_int v)
  end) in
  I.go

let rec show_expr = function
  | Var (_, n) -> n
  | Const (w, v) -> Printf.sprintf "%d'%Lx" w v
  | Add (_, a, b) -> Printf.sprintf "(%s + %s)" (show_expr a) (show_expr b)
  | Sub (_, a, b) -> Printf.sprintf "(%s - %s)" (show_expr a) (show_expr b)
  | And (a, b) -> Printf.sprintf "(%s & %s)" (show_expr a) (show_expr b)
  | Or (a, b) -> Printf.sprintf "(%s | %s)" (show_expr a) (show_expr b)
  | Xor (a, b) -> Printf.sprintf "(%s ^ %s)" (show_expr a) (show_expr b)
  | Not (_, a) -> Printf.sprintf "~%s" (show_expr a)
  | Shl (_, a, k) -> Printf.sprintf "(%s << %d)" (show_expr a) k
  | Shr (a, k) -> Printf.sprintf "(%s >> %d)" (show_expr a) k
  | Zext (w, a) -> Printf.sprintf "zext%d(%s)" w (show_expr a)
  | Extract (a, hi, lo) -> Printf.sprintf "%s[%d:%d]" (show_expr a) hi lo
  | Concat (a, b) -> Printf.sprintf "{%s,%s}" (show_expr a) (show_expr b)
  | Eq (a, b) -> Printf.sprintf "(%s == %s)" (show_expr a) (show_expr b)
  | Ult (a, b) -> Printf.sprintf "(%s <u %s)" (show_expr a) (show_expr b)
  | Ite (c, a, b) -> Printf.sprintf "(%s ? %s : %s)" (show_expr c) (show_expr a) (show_expr b)

let show_case (e, env_seed) = Printf.sprintf "%s  (env seed %d)" (show_expr e) env_seed

let prop_eval64 =
  Q.Test.make ~count:3000 ~name:"Term.eval64 agrees with Concrete64 on random terms"
    ~print:show_case (gen_top ~wmax:64 ~kmax:70) (fun (e, env_seed) ->
      Int64.equal (T.eval64 (env_value env_seed) (On_term.go e)) (on_c64 env_seed e))

let prop_expr_vs_concrete =
  Q.Test.make ~count:3000 ~name:"Concrete64 agrees with Concrete on random expressions"
    ~print:show_case (gen_top ~wmax:30 ~kmax:30) (fun (e, env_seed) ->
      Int64.equal (on_c64 env_seed e) (Int64.of_int (on_c env_seed e)))

(* {1 Sparse_memory} *)

let gen_addr =
  G.oneof_weighted
    [
      (3, G.int64);
      (2, G.map Int64.of_int (G.int_range 0 0x3000));
      (2, G.map (fun d -> Int64.sub 0L (Int64.of_int d)) (G.int_range 1 0x3000));
      ( 1,
        G.map (fun d -> Int64.add Int64.min_int (Int64.of_int (d - 0x1800))) (G.int_range 0 0x3000)
      );
    ]

let prop_memory =
  Q.Test.make ~count:2000 ~name:"Sparse_memory reads back the last store, zero elsewhere"
    ~print:(fun (ops, _) ->
      String.concat "; " (List.map (fun (a, v) -> Printf.sprintf "%Lx<-%Lx" a v) ops))
    (G.pair
       (G.list_size (G.int_range 0 60) (G.pair gen_addr G.int64))
       (G.list_size (G.return 20) gen_addr))
    (fun (ops, probes) ->
      let m = Mem.create () in
      let model = Hashtbl.create 16 in
      List.iter
        (fun (a, v) ->
          ignore (Mem.store8 m a v);
          Hashtbl.replace model a (Int64.logand v 0xffL))
        ops;
      let expect a = Option.value (Hashtbl.find_opt model a) ~default:0L in
      let pages_before = Mem.pages m in
      List.for_all (fun (a, _) -> Int64.equal (Mem.load8 m a) (expect a)) ops
      && List.for_all (fun a -> Int64.equal (Mem.load8 m a) (expect a)) probes
      && Mem.pages m = pages_before
      && Mem.pages m <= List.length ops)

let () =
  Printf.printf "compute property tests, seed %d\n%!" seed;
  let rand = Random.State.make [| seed |] in
  exit
    (QCheck_base_runner.run_tests ~colors:false ~verbose:false ~rand
       [ prop_vs_concrete; prop_vs_bigint; prop_eval64; prop_expr_vs_concrete; prop_memory ])
