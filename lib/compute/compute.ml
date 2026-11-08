(** Fixed-width values and byte memory, as signatures and two interpretations:
    {!Concrete} evaluates, {!Term} builds expressions.

    A computation is a functor over {!VALUE} (and {!MEMORY}); instantiating it
    with either runs the same definition. Widths are explicit arguments rather than part of the
    value, so that the concrete interpretation is a plain [int]. Operands of a
    binary operation have the same width; the result of an arithmetic operation
    is truncated to [w]. Predicates are 1-bit values.

    The concrete interpretation keeps values in OCaml [int]s and so supports
    widths up to 30, which fits the 31-bit [int] of the 32-bit CI legs. *)

module type VALUE = sig
  type t

  val const : w:int -> int -> t
  (** [const ~w n]: the low [w] bits of [n]. *)

  val add : w:int -> t -> t -> t
  val sub : w:int -> t -> t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : w:int -> t -> t
  val shl : w:int -> t -> int -> t

  val shr : t -> int -> t
  (** Logical right shift. *)

  val zext : w:int -> t -> t
  (** Widen to [w] bits with zeros (no change to the number). *)

  val extract : t -> hi:int -> lo:int -> t
  (** Bits [hi..lo], as a [hi - lo + 1] bit value. *)

  val concat : hi:t -> lo:t -> lo_w:int -> t
  (** [hi] above the [lo_w] bits of [lo]. *)

  val eq : t -> t -> t
  val ult : t -> t -> t

  val ite : t -> t -> t -> t
  (** [ite c a b]: [a] if the 1-bit [c] is set, else [b]. *)

  val decide : t -> bool
  (** The branch oracle: whether a 1-bit value is set. A concrete interpretation
      reads it; a symbolic one forks or refuses. *)

  val to_int : t -> int option
  (** The number, if the value is concrete. *)
end

module type MEMORY = sig
  type value
  type t

  val load8 : t -> value -> value

  val store8 : t -> value -> value -> t
  (** The memory after the store; a mutable memory returns itself. *)
end

(** Derived operations every computation wants, over any {!VALUE}. *)
module Ops (V : VALUE) = struct
  let bit v i = V.extract v ~hi:i ~lo:i
  let one = V.const ~w:1 1
  let zero1 = V.const ~w:1 0
  let not1 b = V.logxor b one

  (** Even parity of the bits of [v] (width [w]), as a 1-bit value. *)
  let parity_even ~w v =
    let rec fold acc i = if i >= w then acc else fold (V.logxor acc (bit v i)) (i + 1) in
    not1 (fold zero1 0)

  let is_zero ~w v = V.eq v (V.const ~w 0)
end

module Concrete : VALUE with type t = int = struct
  type t = int

  let mask w = (1 lsl w) - 1
  let check w = if w < 1 || w > 30 then invalid_arg "Compute.Concrete: width must be 1..30"

  let const ~w n =
    check w;
    n land mask w

  let add ~w a b = (a + b) land mask w
  let sub ~w a b = (a - b) land mask w
  let logand = ( land )
  let logor = ( lor )
  let logxor = ( lxor )
  let lognot ~w a = lnot a land mask w
  let shl ~w a n = (a lsl n) land mask w
  let shr a n = a lsr n
  let zext ~w:_ a = a
  let extract a ~hi ~lo = (a lsr lo) land mask (hi - lo + 1)
  let concat ~hi ~lo ~lo_w = (hi lsl lo_w) lor lo
  let eq a b = if a = b then 1 else 0
  let ult a b = if a < b then 1 else 0
  let ite c a b = if c <> 0 then a else b
  let decide c = c <> 0
  let to_int a = Some a
end

module Bytes_memory : MEMORY with type value = int and type t = Bytes.t = struct
  type value = int
  type t = Bytes.t

  let load8 m a = Char.code (Bytes.unsafe_get m (a land (Bytes.length m - 1)))

  let store8 m a v =
    Bytes.unsafe_set m (a land (Bytes.length m - 1)) (Char.unsafe_chr (v land 0xff));
    m
end

(** The term-building interpretation: the same computation, run symbolically,
    constructs an expression. Constants fold as they are built. [decide] on a
    value that is not a constant raises {!Undecided} rather than choosing a
    path; a driver that explores paths catches it. *)
module Term = struct
  type t =
    | Const of { w : int; n : int }
    | Var of { w : int; name : string }
    | Add of int * t * t
    | Sub of int * t * t
    | And of t * t
    | Or of t * t
    | Xor of t * t
    | Not of int * t
    | Shl of int * t * int
    | Shr of t * int
    | Zext of int * t
    | Extract of t * int * int
    | Concat of t * t * int
    | Eq of t * t
    | Ult of t * t
    | Ite of t * t * t

  exception Undecided of t

  let rec width = function
    | Const { w; _ } | Var { w; _ } | Add (w, _, _) | Sub (w, _, _) | Not (w, _) | Shl (w, _, _) ->
        w
    | Zext (w, _) -> w
    | And (a, _) | Or (a, _) | Xor (a, _) | Shr (a, _) | Ite (_, a, _) -> width a
    | Extract (_, hi, lo) -> hi - lo + 1
    | Concat (hi, _, lo_w) -> width hi + lo_w
    | Eq _ | Ult _ -> 1

  let var ~w name = Var { w; name }
  let const ~w n = Const { w; n = Concrete.const ~w n }
  let fold1 f make a = match a with Const { n; _ } -> f n | _ -> make a

  let fold2 f make a b =
    match (a, b) with Const { n = x; _ }, Const { n = y; _ } -> f x y | _ -> make a b

  let add ~w = fold2 (fun x y -> const ~w (Concrete.add ~w x y)) (fun a b -> Add (w, a, b))
  let sub ~w = fold2 (fun x y -> const ~w (Concrete.sub ~w x y)) (fun a b -> Sub (w, a, b))
  let logand a b = fold2 (fun x y -> const ~w:(width a) (x land y)) (fun a b -> And (a, b)) a b
  let logor a b = fold2 (fun x y -> const ~w:(width a) (x lor y)) (fun a b -> Or (a, b)) a b
  let logxor a b = fold2 (fun x y -> const ~w:(width a) (x lxor y)) (fun a b -> Xor (a, b)) a b
  let lognot ~w = fold1 (fun x -> const ~w (Concrete.lognot ~w x)) (fun a -> Not (w, a))
  let shl ~w a k = fold1 (fun x -> const ~w (Concrete.shl ~w x k)) (fun a -> Shl (w, a, k)) a
  let shr a k = fold1 (fun x -> const ~w:(width a) (Concrete.shr x k)) (fun a -> Shr (a, k)) a
  let zext ~w a = fold1 (fun x -> const ~w x) (fun a -> Zext (w, a)) a

  let extract a ~hi ~lo =
    fold1
      (fun x -> const ~w:(hi - lo + 1) (Concrete.extract x ~hi ~lo))
      (fun a -> Extract (a, hi, lo))
      a

  let concat ~hi ~lo ~lo_w =
    fold2
      (fun h l -> const ~w:(width hi + lo_w) (Concrete.concat ~hi:h ~lo:l ~lo_w))
      (fun hi lo -> Concat (hi, lo, lo_w))
      hi lo

  let eq a b = fold2 (fun x y -> const ~w:1 (Concrete.eq x y)) (fun a b -> Eq (a, b)) a b
  let ult a b = fold2 (fun x y -> const ~w:1 (Concrete.ult x y)) (fun a b -> Ult (a, b)) a b
  let ite c a b = match c with Const { n; _ } -> if n <> 0 then a else b | _ -> Ite (c, a, b)
  let decide = function Const { n; _ } -> n <> 0 | c -> raise (Undecided c)
  let to_int = function Const { n; _ } -> Some n | _ -> None

  (** The value of a term under an assignment of its variables. *)
  let rec eval env = function
    | Const { n; _ } -> n
    | Var { name; _ } -> env name
    | Add (w, a, b) -> Concrete.add ~w (eval env a) (eval env b)
    | Sub (w, a, b) -> Concrete.sub ~w (eval env a) (eval env b)
    | And (a, b) -> eval env a land eval env b
    | Or (a, b) -> eval env a lor eval env b
    | Xor (a, b) -> eval env a lxor eval env b
    | Not (w, a) -> Concrete.lognot ~w (eval env a)
    | Shl (w, a, k) -> Concrete.shl ~w (eval env a) k
    | Shr (a, k) -> Concrete.shr (eval env a) k
    | Zext (_, a) -> eval env a
    | Extract (a, hi, lo) -> Concrete.extract (eval env a) ~hi ~lo
    | Concat (hi, lo, lo_w) -> Concrete.concat ~hi:(eval env hi) ~lo:(eval env lo) ~lo_w
    | Eq (a, b) -> Concrete.eq (eval env a) (eval env b)
    | Ult (a, b) -> Concrete.ult (eval env a) (eval env b)
    | Ite (c, a, b) -> Concrete.ite (eval env c) (eval env a) (eval env b)
end

(** Byte memory over terms, persistent. An address must fold to a constant, else
    {!Symbolic_address} is raised; a byte never stored reads as a variable named
    [mem[aaaa]] after its address. *)
module Term_memory : sig
  include MEMORY with type value = Term.t

  exception Symbolic_address of Term.t

  val empty : t
end = struct
  module Int_map = Map.Make (Int)

  type value = Term.t
  type t = Term.t Int_map.t

  exception Symbolic_address of Term.t

  let empty = Int_map.empty
  let address a = match Term.to_int a with Some n -> n | None -> raise (Symbolic_address a)

  let load8 m a =
    let n = address a in
    match Int_map.find_opt n m with
    | Some v -> v
    | None -> Term.var ~w:8 (Printf.sprintf "mem[%04x]" n)

  let store8 m a v = Int_map.add (address a) v m
end
