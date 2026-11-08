(* The model's table entries run over [Compute.Term] build expressions; those
   expressions, evaluated, equal what the concrete interpretation computes.
   Each of the 256 opcode bytes is stepped once from the same state in both. *)

module C = I8080_sem.Model
module T = Compute.Term
module TM = Compute.Term_memory
module Sym = I8080_sem.Make (T) (TM)

let pattern a = ((a * 7) + 13) land 0xff
let origin = 0x100
let stack = 0x8000

let rec pp ppf (t : T.t) =
  match t with
  | Const { n; w } -> Format.fprintf ppf "%d'%x" w n
  | Var { name; _ } -> Format.pp_print_string ppf name
  | Add (_, a, b) -> Format.fprintf ppf "(%a + %a)" pp a pp b
  | Sub (_, a, b) -> Format.fprintf ppf "(%a - %a)" pp a pp b
  | And (a, b) -> Format.fprintf ppf "(%a & %a)" pp a pp b
  | Or (a, b) -> Format.fprintf ppf "(%a | %a)" pp a pp b
  | Xor (a, b) -> Format.fprintf ppf "(%a ^ %a)" pp a pp b
  | Not (_, a) -> Format.fprintf ppf "~%a" pp a
  | Shl (_, a, k) -> Format.fprintf ppf "(%a << %d)" pp a k
  | Shr (a, k) -> Format.fprintf ppf "(%a >> %d)" pp a k
  | Zext (w, a) -> Format.fprintf ppf "zext%d(%a)" w pp a
  | Extract (a, hi, lo) -> Format.fprintf ppf "%a[%d:%d]" pp a hi lo
  | Concat (hi, lo, _) -> Format.fprintf ppf "{%a,%a}" pp hi pp lo
  | Eq (a, b) -> Format.fprintf ppf "(%a == %a)" pp a pp b
  | Ult (a, b) -> Format.fprintf ppf "(%a <u %a)" pp a pp b
  | Ite (c, a, b) -> Format.fprintf ppf "(%a ? %a : %a)" pp c pp a pp b

let program bytes =
  List.fold_left
    (fun (m, a) b -> (TM.store8 m (T.const ~w:16 a) (T.const ~w:8 b), a + 1))
    (TM.empty, origin) bytes
  |> fst

let symbolic_state ~vars ~flags bytes =
  let env = { Sym.input = (fun p -> T.logxor p (T.const ~w:8 0x5a)); output = (fun _ _ -> ()) } in
  let st = Sym.create ~env (program bytes) in
  let v n = T.var ~w:8 n in
  List.iter
    (fun n ->
      match n with
      | "a" -> st.Sym.a <- v n
      | "b" -> st.b <- v n
      | "c" -> st.c <- v n
      | "d" -> st.d <- v n
      | "e" -> st.e <- v n
      | _ -> ())
    vars;
  (match flags with
  | `Var ->
      let f n = T.var ~w:1 n in
      st.s <- f "s";
      st.z <- f "z";
      st.ac <- f "ac";
      st.p <- f "p";
      st.cy <- f "cy"
  | `Const _ -> ());
  st.pc <- T.const ~w:16 origin;
  st.sp <- T.const ~w:16 stack;
  st

(* [add b], symbolically: the accumulator after the instruction. *)
let%expect_test "ADD B builds an expression" =
  let st = symbolic_state ~vars:[ "a"; "b" ] ~flags:`Var [ 0x80 ] in
  Sym.step st;
  Format.printf "a = %a@.cy = %a@." pp st.Sym.a pp st.cy;
  [%expect
    {|
    a = ((zext9(a) + zext9(b)) + 9'0)[7:0]
    cy = ((zext9(a) + zext9(b)) + 9'0)[8:8] |}]

(* Step one opcode byte in both interpretations from the same state and compare
   registers, flags, timing and every memory and port access. *)

type outcome = Same | Different of string | Skipped

let trial ~vars ~flags op =
  let rng =
    Random.State.make [| op; List.length vars; (match flags with `Var -> 1 | `Const _ -> 0) |]
  in
  let byte () = Random.State.int rng 256 in
  let values = Hashtbl.create 16 in
  let assign n w =
    let x = Random.State.int rng (1 lsl w) in
    Hashtbl.replace values n x;
    x
  in
  let bytes = [ op; 0x34; 0x12 ] in
  ignore byte;
  (* concrete side *)
  let cenv = { C.input = (fun p -> p lxor 0x5a); output = (fun _ _ -> ()) } in
  let mem = C.create_memory () in
  Bytes.iteri (fun a _ -> Bytes.set mem a (Char.chr (pattern a))) mem;
  let cst = C.create ~env:cenv mem in
  C.poke cst origin (String.init 3 (fun i -> Char.chr (List.nth bytes i)));
  cst.C.pc <- origin;
  cst.C.sp <- stack;
  let sym_state = symbolic_state ~vars ~flags bytes in
  let reg n set sset =
    let x = if List.mem n vars then assign n 8 else Random.State.int rng 256 in
    set x;
    ignore sset
  in
  reg "a" (fun x -> cst.C.a <- x) ();
  reg "b" (fun x -> cst.C.b <- x) ();
  reg "c" (fun x -> cst.C.c <- x) ();
  reg "d" (fun x -> cst.C.d <- x) ();
  reg "e" (fun x -> cst.C.e <- x) ();
  let h = Random.State.int rng 256 and l = Random.State.int rng 256 in
  cst.C.h <- h;
  cst.C.l <- l;
  sym_state.Sym.h <- T.const ~w:8 h;
  sym_state.l <- T.const ~w:8 l;
  let flag n set sset =
    match flags with
    | `Var -> set (assign n 1)
    | `Const _ ->
        let x = Random.State.int rng 2 in
        set x;
        sset (T.const ~w:1 x)
  in
  flag "s" (fun x -> cst.C.s <- x) (fun t -> sym_state.Sym.s <- t);
  flag "z" (fun x -> cst.C.z <- x) (fun t -> sym_state.z <- t);
  flag "ac" (fun x -> cst.C.ac <- x) (fun t -> sym_state.ac <- t);
  flag "p" (fun x -> cst.C.p <- x) (fun t -> sym_state.p <- t);
  flag "cy" (fun x -> cst.C.cy <- x) (fun t -> sym_state.cy <- t);
  (* the symbolic registers other than the free ones take the same constants *)
  let const_reg n get =
    if not (List.mem n vars) then
      match n with
      | "a" -> sym_state.Sym.a <- T.const ~w:8 (get ())
      | "b" -> sym_state.b <- T.const ~w:8 (get ())
      | "c" -> sym_state.c <- T.const ~w:8 (get ())
      | "d" -> sym_state.d <- T.const ~w:8 (get ())
      | _ -> sym_state.e <- T.const ~w:8 (get ())
  in
  const_reg "a" (fun () -> cst.C.a);
  const_reg "b" (fun () -> cst.C.b);
  const_reg "c" (fun () -> cst.C.c);
  const_reg "d" (fun () -> cst.C.d);
  const_reg "e" (fun () -> cst.C.e);
  let env name =
    match Hashtbl.find_opt values name with
    | Some x -> x
    | None -> pattern (int_of_string ("0x" ^ String.sub name 4 4))
  in
  let events_c = ref [] and events_s = ref [] in
  cst.C.log <-
    Some
      (function
      | C.Fetch (a, v) -> events_c := ("fetch", a, v) :: !events_c
      | Read (a, v) -> events_c := ("read", a, v) :: !events_c
      | Write (a, v) -> events_c := ("write", a, v) :: !events_c
      | In (a, v) -> events_c := ("in", a, v) :: !events_c
      | Out (a, v) -> events_c := ("out", a, v) :: !events_c);
  let ev t = T.eval env t in
  sym_state.Sym.log <-
    Some
      (function
      | Sym.Fetch (a, v) -> events_s := ("fetch", ev a, ev v) :: !events_s
      | Read (a, v) -> events_s := ("read", ev a, ev v) :: !events_s
      | Write (a, v) -> events_s := ("write", ev a, ev v) :: !events_s
      | In (a, v) -> events_s := ("in", ev a, ev v) :: !events_s
      | Out (a, v) -> events_s := ("out", ev a, ev v) :: !events_s);
  match Sym.step sym_state with
  | exception (T.Undecided _ | TM.Symbolic_address _) -> Skipped
  | () ->
      C.step cst;
      let s = sym_state in
      let pairs =
        [
          ("a", cst.C.a, ev s.Sym.a);
          ("b", cst.b, ev s.b);
          ("c", cst.c, ev s.c);
          ("d", cst.d, ev s.d);
          ("e", cst.e, ev s.e);
          ("h", cst.h, ev s.h);
          ("l", cst.l, ev s.l);
          ("s", cst.s, ev s.s);
          ("z", cst.z, ev s.z);
          ("ac", cst.ac, ev s.ac);
          ("p", cst.p, ev s.p);
          ("cy", cst.cy, ev s.cy);
          ("sp", cst.sp, ev s.sp);
          ("pc", cst.pc, ev s.pc);
          ("tstates", cst.tstates, s.tstates);
          ("halted", Bool.to_int cst.halted, Bool.to_int s.halted);
          ("inte", Bool.to_int cst.inte, Bool.to_int s.inte);
          ("ei_pending", Bool.to_int cst.ei_pending, Bool.to_int s.ei_pending);
        ]
      in
      let bad = List.filter (fun (_, x, y) -> x <> y) pairs in
      if bad <> [] then
        Different
          (String.concat " " (List.map (fun (n, x, y) -> Printf.sprintf "%s:%x/%x" n x y) bad))
      else if !events_c <> !events_s then Different "accesses"
      else Same

let sweep ~title ~vars ~flags =
  let same = ref 0 and skipped = ref 0 and diffs = ref [] in
  for op = 0 to 255 do
    match trial ~vars ~flags op with
    | Same -> incr same
    | Skipped -> incr skipped
    | Different d -> diffs := Printf.sprintf "%02x %s" op d :: !diffs
  done;
  Printf.printf "%s: same=%d skipped=%d different=%d\n" title !same !skipped (List.length !diffs);
  List.iter print_endline (List.rev !diffs)

let%expect_test "every opcode byte: the expression evaluates to the concrete result" =
  sweep ~title:"a b c d e and flags free" ~vars:[ "a"; "b"; "c"; "d"; "e" ] ~flags:`Var;
  sweep ~title:"a and flags free" ~vars:[ "a" ] ~flags:`Var;
  sweep ~title:"a free" ~vars:[ "a" ] ~flags:(`Const ());
  [%expect
    {|
    a b c d e and flags free: same=228 skipped=28 different=0
    a and flags free: same=232 skipped=24 different=0
    a free: same=256 skipped=0 different=0 |}]
