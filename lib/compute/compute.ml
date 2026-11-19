(** Fixed-width values and byte memory, as signatures and three interpretations:
    {!Concrete} and {!Concrete64} evaluate, {!Term} builds expressions.

    A computation is a functor over {!VALUE} (and {!MEMORY}); instantiating it
    with either runs the same definition. Widths are explicit arguments rather than part of the
    value, so that the concrete interpretation is a plain [int]. Operands of a
    binary operation have the same width; the result of an arithmetic operation
    is truncated to [w]. Predicates are 1-bit values.

    The concrete interpretation keeps values in OCaml [int]s and so supports
    widths up to 30, which fits the 31-bit [int] of the 32-bit CI legs. {!Concrete64} and the
    term interpretation hold constants as [int64] and support widths up to 64. *)

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

(** The width limits of the two interpretations. *)
module Width : sig
  val max_concrete : int
  (** 30: what the 31-bit [int] of the 32-bit CI legs holds. *)

  val max_term : int
  (** 64: what an [int64] constant holds. *)

  val check : who:string -> max:int -> int -> unit
  (** Raises [Invalid_argument "<who>: width must be 1..<max>"] unless [1 <= w <= max]. *)

  val mask : int -> int
  (** The low [w] bits as an [int], for [w <= max_concrete]. *)

  val mask64 : int -> int64
  (** The low [w] bits as an [int64], for [w <= max_term]. *)
end = struct
  let max_concrete = 30
  let max_term = 64

  let check ~who ~max w =
    if w < 1 || w > max then invalid_arg (Printf.sprintf "%s: width must be 1..%d" who max)

  let mask w = (1 lsl w) - 1
  let mask64 w = if w >= 64 then -1L else Int64.pred (Int64.shift_left 1L w)
end

module Concrete : VALUE with type t = int = struct
  type t = int

  let mask = Width.mask
  let check = Width.check ~who:"Compute.Concrete" ~max:Width.max_concrete

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
  let concat ~hi ~lo ~lo_w = (hi lsl lo_w) lor (lo land mask lo_w)
  let eq a b = if a = b then 1 else 0
  let ult a b = if a < b then 1 else 0
  let ite c a b = if c <> 0 then a else b
  let decide c = c <> 0
  let to_int a = Some a
end

(** The concrete interpretation for widths up to {!Width.max_term}, in [int64]. A value of width
    [w] keeps its bits above [w] zero and is read as unsigned, so [ult] is an unsigned compare and
    a 64-bit value with the sign bit set is just a large number. As in {!Concrete}, only [const]
    checks its width; the other operations trust their operands.

    {!Concrete} stays the interpretation for widths up to {!Width.max_concrete}: it is a plain
    [int], and nothing here assumes the [int] is wider than 31 bits. *)
module Concrete64 : sig
  include VALUE with type t = int64

  val const64 : w:int -> int64 -> t
  (** [const64 ~w n]: the low [w] bits of [n]. *)

  val to_int64 : t -> int64
  (** The number; the identity. [to_int] gives it only below 2^30, as {!Term.to_int} does. *)
end = struct
  type t = int64

  let mask = Width.mask64
  let check = Width.check ~who:"Compute.Concrete64" ~max:Width.max_term

  let const64 ~w n =
    check w;
    Int64.logand n (mask w)

  let const ~w n = const64 ~w (Int64.of_int n)
  let shl64 x k = if k >= 64 then 0L else Int64.shift_left x k
  let shr64 x k = if k >= 64 then 0L else Int64.shift_right_logical x k
  let add ~w a b = Int64.logand (Int64.add a b) (mask w)
  let sub ~w a b = Int64.logand (Int64.sub a b) (mask w)
  let logand = Int64.logand
  let logor = Int64.logor
  let logxor = Int64.logxor
  let lognot ~w a = Int64.logand (Int64.lognot a) (mask w)
  let shl ~w a n = Int64.logand (shl64 a n) (mask w)
  let shr = shr64
  let zext ~w:_ a = a
  let extract a ~hi ~lo = Int64.logand (shr64 a lo) (mask (hi - lo + 1))
  let concat ~hi ~lo ~lo_w = Int64.logor (shl64 hi lo_w) (Int64.logand lo (mask lo_w))
  let eq a b = if Int64.equal a b then 1L else 0L
  let ult a b = if Int64.unsigned_compare a b < 0 then 1L else 0L
  let ite c a b = if Int64.equal c 0L then b else a
  let decide c = not (Int64.equal c 0L)
  let to_int64 a = a

  let to_int a =
    if Int64.unsigned_compare a (Int64.of_int (Width.mask Width.max_concrete)) <= 0 then
      Some (Int64.to_int a)
    else None
end

module Bytes_memory : sig
  include MEMORY with type value = int and type t = Bytes.t

  val create : int -> t
  (** Zeroed memory of [n] bytes; addresses wrap modulo [n], so [n] must be a power of two. *)
end = struct
  type value = int
  type t = Bytes.t

  let create n =
    if n < 1 || n land (n - 1) <> 0 then
      invalid_arg "Compute.Bytes_memory.create: size must be a power of two";
    Bytes.make n '\000'

  let load8 m a = Char.code (Bytes.get m (a land (Bytes.length m - 1)))

  let store8 m a v =
    Bytes.set m (a land (Bytes.length m - 1)) (Char.unsafe_chr (v land 0xff));
    m
end

(** Byte memory over the 64-bit address space, mutable: the pages written so far are in a hash
    table, so creating one allocates nothing and an access costs one lookup. An address is an
    unsigned 64-bit number, an access at [2^64 - 1] stays in range (the caller's address
    arithmetic wraps at its own width), and a byte never stored reads as zero, as in
    {!Bytes_memory}. A read does not allocate. *)
module Sparse_memory : sig
  include MEMORY with type value = int64

  val create : unit -> t

  val pages : t -> int
  (** The number of pages allocated, each {!page_size} bytes. *)

  val page_size : int
end = struct
  type value = int64
  type t = (int64, Bytes.t) Hashtbl.t

  let page_bits = 12
  let page_size = 1 lsl page_bits
  let create () = Hashtbl.create 16
  let pages = Hashtbl.length
  let page_of a = Int64.shift_right_logical a page_bits
  let offset_of a = Int64.to_int (Int64.logand a (Int64.of_int (page_size - 1)))

  let load8 m a =
    match Hashtbl.find_opt m (page_of a) with
    | None -> 0L
    | Some p -> Int64.of_int (Char.code (Bytes.get p (offset_of a)))

  let store8 m a v =
    let key = page_of a in
    let p =
      match Hashtbl.find_opt m key with
      | Some p -> p
      | None ->
          let p = Bytes.make page_size '\000' in
          Hashtbl.add m key p;
          p
    in
    Bytes.set p (offset_of a) (Char.unsafe_chr (Int64.to_int v land 0xff));
    m
end

(** The term-building interpretation: the same computation, run symbolically,
    constructs an expression. Terms are hash-consed: structurally equal terms
    are physically equal, so a value combined with itself builds a DAG whose size
    is the number of distinct subterms, and {!eval}, {!pp} and {!size} visit each
    node once. Constants fold as they are built, and a few local identities
    apply ([x xor x], [x and 0], [ite c a a], ...). Operands of a binary operation
    must have the same width, else [Invalid_argument]. Widths go up to
    {!Width.max_term}; constants are [int64].

    [decide] on a value that is not a constant raises {!Undecided} rather than
    choosing a path; {!try_decide} and {!try_step} report it as a result instead.
    The weak table that shares the terms is not safe to use from several domains
    at once. *)
module Term : sig
  type t = private { id : int; w : int; node : node }
  (** [id] is unique among live terms and increases with construction, so a term's
      operands have smaller ids than the term. [w] is the width. *)

  and node =
    | Const of int64  (** the value, in the low [w] bits *)
    | Var of string
    | Add of t * t
    | Sub of t * t
    | And of t * t
    | Or of t * t
    | Xor of t * t
    | Not of t
    | Shl of t * int
    | Shr of t * int
    | Zext of t
    | Extract of t * int * int  (** [hi], [lo] *)
    | Concat of t * t * int  (** [hi], [lo], [lo_w] *)
    | Eq of t * t
    | Ult of t * t
    | Ite of t * t * t

  include VALUE with type t := t

  exception Undecided of t

  val var : w:int -> string -> t
  val const64 : w:int -> int64 -> t
  val width : t -> int

  val to_int64 : t -> int64 option
  (** The number, if the term is a constant. [to_int] gives it only when it fits in 30 bits. *)

  val try_decide : t -> (bool, t) result
  (** {!decide} with the undecided condition as the [Error]. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val size : t -> int
  (** The number of distinct nodes reachable from the term. *)

  val evaluator : (string -> int64) -> t -> int64
  (** [evaluator env] evaluates terms under the assignment [env] of the variables (masked to
      their widths), keeping the value of every node it has visited: applying it to several
      terms evaluates their shared parts once. *)

  val eval64 : (string -> int64) -> t -> int64

  val eval : (string -> int) -> t -> int
  (** [eval env t] is the value of [t] under [env]; [Invalid_argument] if [t] is wider than
      {!Width.max_concrete}. Time is linear in {!size}. *)

  val pp : Format.formatter -> t -> unit
  (** Prints the term; a subterm used more than once is bound once by a [let]. *)

  val to_string : t -> string
end = struct
  type t = { id : int; w : int; node : node }

  and node =
    | Const of int64
    | Var of string
    | Add of t * t
    | Sub of t * t
    | And of t * t
    | Or of t * t
    | Xor of t * t
    | Not of t
    | Shl of t * int
    | Shr of t * int
    | Zext of t
    | Extract of t * int * int
    | Concat of t * t * int
    | Eq of t * t
    | Ult of t * t
    | Ite of t * t * t

  exception Undecided of t

  let who = "Compute.Term"
  let bad fmt = Printf.ksprintf invalid_arg fmt

  module Hashed = struct
    type nonrec t = t

    let equal a b =
      a.w = b.w
      &&
      match (a.node, b.node) with
      | Const x, Const y -> Int64.equal x y
      | Var x, Var y -> String.equal x y
      | Add (p, q), Add (r, s)
      | Sub (p, q), Sub (r, s)
      | And (p, q), And (r, s)
      | Or (p, q), Or (r, s)
      | Xor (p, q), Xor (r, s)
      | Eq (p, q), Eq (r, s)
      | Ult (p, q), Ult (r, s) ->
          p == r && q == s
      | Not p, Not r | Zext p, Zext r -> p == r
      | Shl (p, k), Shl (r, l) | Shr (p, k), Shr (r, l) -> p == r && k = l
      | Extract (p, h, l), Extract (r, h', l') -> p == r && h = h' && l = l'
      | Concat (p, q, k), Concat (r, s, l) -> p == r && q == s && k = l
      | Ite (p, q, u), Ite (r, s, v) -> p == r && q == s && u == v
      | _ -> false

    let hash x =
      match x.node with
      | Const n -> Hashtbl.hash (0, x.w, n)
      | Var s -> Hashtbl.hash (1, x.w, s)
      | Add (p, q) -> Hashtbl.hash (2, x.w, p.id, q.id)
      | Sub (p, q) -> Hashtbl.hash (3, x.w, p.id, q.id)
      | And (p, q) -> Hashtbl.hash (4, x.w, p.id, q.id)
      | Or (p, q) -> Hashtbl.hash (5, x.w, p.id, q.id)
      | Xor (p, q) -> Hashtbl.hash (6, x.w, p.id, q.id)
      | Not p -> Hashtbl.hash (7, x.w, p.id)
      | Shl (p, k) -> Hashtbl.hash (8, x.w, p.id, k)
      | Shr (p, k) -> Hashtbl.hash (9, x.w, p.id, k)
      | Zext p -> Hashtbl.hash (10, x.w, p.id)
      | Extract (p, h, l) -> Hashtbl.hash (11, p.id, h, l)
      | Concat (p, q, k) -> Hashtbl.hash (12, p.id, q.id, k)
      | Eq (p, q) -> Hashtbl.hash (13, p.id, q.id)
      | Ult (p, q) -> Hashtbl.hash (14, p.id, q.id)
      | Ite (p, q, u) -> Hashtbl.hash (15, p.id, q.id, u.id)
  end

  module Table = Weak.Make (Hashed)

  let table = Table.create 4096
  let counter = ref 0

  let intern w node =
    let probe = { id = -1; w; node } in
    match Table.find_opt table probe with
    | Some t -> t
    | None ->
        incr counter;
        let t = { id = !counter; w; node } in
        Table.add table t;
        t

  let check_w w = Width.check ~who ~max:Width.max_term w
  let width x = x.w
  let equal (a : t) b = a == b
  let compare a b = Int.compare a.id b.id
  let hash a = a.id
  let same_w op a b = if a.w <> b.w then bad "%s.%s: operand widths %d and %d differ" who op a.w b.w

  let const64 ~w n =
    check_w w;
    intern w (Const (Int64.logand n (Width.mask64 w)))

  let const ~w n = const64 ~w (Int64.of_int n)

  let var ~w name =
    check_w w;
    intern w (Var name)

  let zero w = const64 ~w 0L
  let one1 = const64 ~w:1 1L
  let zero1 = const64 ~w:1 0L
  let is_zero x = match x.node with Const 0L -> true | _ -> false
  let is_ones x = match x.node with Const n -> Int64.equal n (Width.mask64 x.w) | _ -> false

  (* The operations on constants, shared by folding and [eval]. *)
  let shl64 x k = if k >= 64 then 0L else Int64.shift_left x k
  let shr64 x k = if k >= 64 then 0L else Int64.shift_right_logical x k

  let add ~w a b =
    check_w w;
    if a.w <> w || b.w <> w then bad "%s.add: operands must be %d bits" who w;
    match (a.node, b.node) with
    | Const x, Const y -> const64 ~w (Int64.add x y)
    | _ -> if is_zero b then a else if is_zero a then b else intern w (Add (a, b))

  let sub ~w a b =
    check_w w;
    if a.w <> w || b.w <> w then bad "%s.sub: operands must be %d bits" who w;
    match (a.node, b.node) with
    | Const x, Const y -> const64 ~w (Int64.sub x y)
    | _ -> if is_zero b then a else if a == b then zero w else intern w (Sub (a, b))

  let logand a b =
    same_w "logand" a b;
    match (a.node, b.node) with
    | Const x, Const y -> const64 ~w:a.w (Int64.logand x y)
    | _ ->
        if a == b then a
        else if is_zero a || is_zero b then zero a.w
        else if is_ones a then b
        else if is_ones b then a
        else intern a.w (And (a, b))

  let logor a b =
    same_w "logor" a b;
    match (a.node, b.node) with
    | Const x, Const y -> const64 ~w:a.w (Int64.logor x y)
    | _ ->
        if a == b then a
        else if is_zero a then b
        else if is_zero b then a
        else if is_ones a then a
        else if is_ones b then b
        else intern a.w (Or (a, b))

  let logxor a b =
    same_w "logxor" a b;
    match (a.node, b.node) with
    | Const x, Const y -> const64 ~w:a.w (Int64.logxor x y)
    | _ ->
        if a == b then zero a.w
        else if is_zero a then b
        else if is_zero b then a
        else intern a.w (Xor (a, b))

  let lognot ~w a =
    check_w w;
    if a.w <> w then bad "%s.lognot: operand must be %d bits" who w;
    match a.node with Const x -> const64 ~w (Int64.lognot x) | Not x -> x | _ -> intern w (Not a)

  let shl ~w a k =
    check_w w;
    if a.w <> w then bad "%s.shl: operand must be %d bits" who w;
    if k < 0 then bad "%s.shl: negative shift" who;
    match a.node with
    | Const x -> const64 ~w (shl64 x k)
    | _ -> if k = 0 then a else if k >= w then zero w else intern w (Shl (a, k))

  let shr a k =
    if k < 0 then bad "%s.shr: negative shift" who;
    match a.node with
    | Const x -> const64 ~w:a.w (shr64 x k)
    | _ -> if k = 0 then a else if k >= a.w then zero a.w else intern a.w (Shr (a, k))

  let zext ~w a =
    check_w w;
    if w < a.w then bad "%s.zext: %d bits is narrower than the operand's %d" who w a.w;
    match a.node with Const x -> const64 ~w x | _ -> if w = a.w then a else intern w (Zext a)

  let extract a ~hi ~lo =
    if lo < 0 || hi < lo || hi >= a.w then bad "%s.extract: [%d:%d] of %d bits" who hi lo a.w;
    let w = hi - lo + 1 in
    match a.node with
    | Const x -> const64 ~w (shr64 x lo)
    | _ -> if w = a.w then a else intern w (Extract (a, hi, lo))

  let concat ~hi ~lo ~lo_w =
    if lo.w <> lo_w then bad "%s.concat: low part is %d bits, not %d" who lo.w lo_w;
    let w = hi.w + lo_w in
    check_w w;
    match (hi.node, lo.node) with
    | Const x, Const y -> const64 ~w (Int64.logor (shl64 x lo_w) y)
    | _ -> intern w (Concat (hi, lo, lo_w))

  let eq a b =
    same_w "eq" a b;
    match (a.node, b.node) with
    | Const x, Const y -> if Int64.equal x y then one1 else zero1
    | _ -> if a == b then one1 else intern 1 (Eq (a, b))

  let ult a b =
    same_w "ult" a b;
    match (a.node, b.node) with
    | Const x, Const y -> if Int64.unsigned_compare x y < 0 then one1 else zero1
    | _ -> if a == b then zero1 else intern 1 (Ult (a, b))

  let ite c a b =
    if c.w <> 1 then bad "%s.ite: condition is %d bits" who c.w;
    same_w "ite" a b;
    match c.node with
    | Const n -> if Int64.equal n 0L then b else a
    | _ -> if a == b then a else intern a.w (Ite (c, a, b))

  let decide c = match c.node with Const n -> not (Int64.equal n 0L) | _ -> raise (Undecided c)
  let try_decide c = match c.node with Const n -> Ok (not (Int64.equal n 0L)) | _ -> Error c
  let to_int64 x = match x.node with Const n -> Some n | _ -> None

  let to_int x =
    match x.node with
    | Const n when Int64.unsigned_compare n (Int64.of_int Width.(mask max_concrete)) <= 0 ->
        Some (Int64.to_int n)
    | _ -> None

  let children x =
    match x.node with
    | Const _ | Var _ -> []
    | Not a | Shl (a, _) | Shr (a, _) | Zext a | Extract (a, _, _) -> [ a ]
    | Add (a, b)
    | Sub (a, b)
    | And (a, b)
    | Or (a, b)
    | Xor (a, b)
    | Concat (a, b, _)
    | Eq (a, b)
    | Ult (a, b) ->
        [ a; b ]
    | Ite (c, a, b) -> [ c; a; b ]

  (* Every node reachable from [root] once, with the number of edges that lead to it. No
     recursion, so the depth of the DAG is not limited by the stack. *)
  let reachable root =
    let uses = Hashtbl.create 64 in
    let order = ref [] in
    let stack = ref [ root ] in
    Hashtbl.replace uses root.id 1;
    let rec loop () =
      match !stack with
      | [] -> ()
      | x :: rest ->
          stack := rest;
          List.iter
            (fun c ->
              match Hashtbl.find_opt uses c.id with
              | Some n -> Hashtbl.replace uses c.id (n + 1)
              | None ->
                  Hashtbl.replace uses c.id 1;
                  stack := c :: !stack)
            (children x);
          order := x :: !order;
          loop ()
    in
    loop ();
    (!order, uses)

  let size root = List.length (fst (reachable root))

  let apply env value x =
    let m = Width.mask64 x.w in
    let v = value in
    match x.node with
    | Const n -> n
    | Var name -> Int64.logand (env name) m
    | Add (a, b) -> Int64.logand (Int64.add (v a) (v b)) m
    | Sub (a, b) -> Int64.logand (Int64.sub (v a) (v b)) m
    | And (a, b) -> Int64.logand (v a) (v b)
    | Or (a, b) -> Int64.logor (v a) (v b)
    | Xor (a, b) -> Int64.logxor (v a) (v b)
    | Not a -> Int64.logand (Int64.lognot (v a)) m
    | Shl (a, k) -> Int64.logand (shl64 (v a) k) m
    | Shr (a, k) -> shr64 (v a) k
    | Zext a -> v a
    | Extract (a, hi, lo) -> Int64.logand (shr64 (v a) lo) (Width.mask64 (hi - lo + 1))
    | Concat (hi, lo, lo_w) ->
        Int64.logor (shl64 (v hi) lo_w) (Int64.logand (v lo) (Width.mask64 lo_w))
    | Eq (a, b) -> if Int64.equal (v a) (v b) then 1L else 0L
    | Ult (a, b) -> if Int64.unsigned_compare (v a) (v b) < 0 then 1L else 0L
    | Ite (c, a, b) -> if Int64.equal (v c) 0L then v b else v a

  let evaluator env =
    let memo : (int, int64) Hashtbl.t = Hashtbl.create 64 in
    let value x = Hashtbl.find memo x.id in
    fun root ->
      let stack = ref [ root ] in
      let rec loop () =
        match !stack with
        | [] -> ()
        | x :: rest ->
            if Hashtbl.mem memo x.id then stack := rest
            else begin
              match List.filter (fun c -> not (Hashtbl.mem memo c.id)) (children x) with
              | [] ->
                  Hashtbl.replace memo x.id (apply env value x);
                  stack := rest
              | pending -> stack := pending @ !stack
            end;
            loop ()
      in
      loop ();
      value root

  let eval64 env t = evaluator env t

  let eval env t =
    if t.w > Width.max_concrete then
      bad "%s.eval: a %d bits term does not fit an int; use eval64" who t.w;
    Int64.to_int (evaluator (fun n -> Int64.of_int (env n)) t)

  let pp ppf root =
    let order, uses = reachable root in
    let leaf x = match x.node with Const _ | Var _ -> true | _ -> false in
    let shared =
      List.filter (fun x -> Hashtbl.find uses x.id > 1 && not (leaf x)) order
      |> List.sort (fun a b -> Int.compare a.id b.id)
    in
    let names = Hashtbl.create 16 in
    List.iteri (fun i x -> Hashtbl.replace names x.id (Printf.sprintf "t%d" (i + 1))) shared;
    let rec expr ppf x =
      match Hashtbl.find_opt names x.id with
      | Some n -> Format.pp_print_string ppf n
      | None -> body ppf x
    and body ppf x =
      let f fmt = Format.fprintf ppf fmt in
      match x.node with
      | Const n -> f "%d'%Lx" x.w n
      | Var name -> Format.pp_print_string ppf name
      | Add (a, b) -> f "(%a + %a)" expr a expr b
      | Sub (a, b) -> f "(%a - %a)" expr a expr b
      | And (a, b) -> f "(%a & %a)" expr a expr b
      | Or (a, b) -> f "(%a | %a)" expr a expr b
      | Xor (a, b) -> f "(%a ^ %a)" expr a expr b
      | Not a -> f "~%a" expr a
      | Shl (a, k) -> f "(%a << %d)" expr a k
      | Shr (a, k) -> f "(%a >> %d)" expr a k
      | Zext a -> f "zext%d(%a)" x.w expr a
      | Extract (a, hi, lo) -> f "%a[%d:%d]" expr a hi lo
      | Concat (hi, lo, _) -> f "{%a,%a}" expr hi expr lo
      | Eq (a, b) -> f "(%a == %a)" expr a expr b
      | Ult (a, b) -> f "(%a <u %a)" expr a expr b
      | Ite (c, a, b) -> f "(%a ? %a : %a)" expr c expr a expr b
    in
    List.iter
      (fun x -> Format.fprintf ppf "let %s = %a in@\n" (Hashtbl.find names x.id) body x)
      shared;
    body ppf root

  let to_string t = Format.asprintf "%a" pp t
end

(** Byte memory over terms, persistent. An address must fold to a constant that fits in 30
    bits, else {!Symbolic_address} is raised ({!try_load8} and {!try_store8} return it as an
    [Error]); a byte never stored reads as a variable named [mem[aaaa]] after its address. *)
module Term_memory : sig
  include MEMORY with type value = Term.t

  exception Symbolic_address of Term.t

  val empty : t
  val try_load8 : t -> value -> (value, Term.t) result
  val try_store8 : t -> value -> value -> (t, Term.t) result
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
  let try_load8 m a = match load8 m a with v -> Ok v | exception Symbolic_address t -> Error t

  let try_store8 m a v =
    match store8 m a v with m -> Ok m | exception Symbolic_address t -> Error t
end

(** Why a symbolic step stopped. *)
type stop =
  | Undecided of Term.t  (** a branch on a condition that is not a constant *)
  | Symbolic_address of Term.t  (** a memory access at an address that is not a constant *)

(** [try_step f] runs [f], reporting {!Term.Undecided} and {!Term_memory.Symbolic_address}
    as an [Error]. A step that stops has already made some of its updates: a driver that
    explores both sides of the condition snapshots the state before the step. *)
let try_step f =
  match f () with
  | v -> Ok v
  | exception Term.Undecided c -> Error (Undecided c)
  | exception Term_memory.Symbolic_address a -> Error (Symbolic_address a)
