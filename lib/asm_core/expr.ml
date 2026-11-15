(* The common expression language (docs/design.md §4.8).

   Shared by data directives, symbol assignments, alignment and instruction
   operands, and deliberately not extended per target: a target influences it
   only through [Modifier], which lets RISC-V's [%pcrel_hi], PowerPC's [@ha] and
   AArch64's page-address forms exist without their semantics being embedded in
   a grammar every other target also uses. *)

open Foundation

type binop = Add | Sub | Mul | Div | Mod | And | Or | Xor | Shl | Shr
type unop = Neg | Lognot

type t =
  | Const of Bigint.t
  | Symbol of string
  | Current_location  (** [.] - the address of the statement being assembled *)
  | Local_ref of int * [ `Back | `Forward ]
  | Binary of binop * t * t
  | Unary of unop * t
  | Modifier of string * t  (** interpreted by the target, opaque here *)

let binop_name = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | And -> "&"
  | Or -> "|"
  | Xor -> "^"
  | Shl -> "<<"
  | Shr -> ">>"

let unop_name = function Neg -> "-" | Lognot -> "~"

let rec pp ppf = function
  | Const v -> Bigint.pp ppf v
  | Symbol s -> Fmt.string ppf s
  | Current_location -> Fmt.string ppf "."
  | Local_ref (n, d) -> Fmt.pf ppf "%d%s" n (match d with `Back -> "b" | `Forward -> "f")
  | Binary (op, a, b) -> Fmt.pf ppf "(%a %s %a)" pp a (binop_name op) pp b
  | Unary (op, a) -> Fmt.pf ppf "%s%a" (unop_name op) pp a
  | Modifier (m, a) -> Fmt.pf ppf "%s(%a)" m pp a

let rec equal a b =
  match (a, b) with
  | Const x, Const y -> Bigint.equal x y
  | Symbol x, Symbol y -> String.equal x y
  | Current_location, Current_location -> true
  | Local_ref (n, d), Local_ref (m, e) -> n = m && d = e
  | Binary (o, a1, a2), Binary (p, b1, b2) -> o = p && equal a1 b1 && equal a2 b2
  | Unary (o, a), Unary (p, b) -> o = p && equal a b
  | Modifier (n, a), Modifier (m, b) -> String.equal n m && equal a b
  | _ -> false

let to_string = Fmt.to_to_string pp

(* {1 Evaluation}

   §4.3's rule, and the reason this is not simply "evaluate": only *fully
   absolute* subexpressions may fold. An expression mentioning a symbol, the
   current location, or a section-relative base stays symbolic until the
   environment that could evaluate it exists - which for [.size s, . - s] is
   layout, not parsing. Folding it early with a placeholder address is how an
   assembler produces a confidently wrong number.

   [env] is how a caller supplies the absolute values it does know; returning
   [None] for a symbol means "not absolute yet", not "undefined". *)

type env = { lookup : string -> Bigint.t option; here : Bigint.t option }

let no_env = { lookup = (fun _ -> None); here = None }

(* This module's error domain (docs/errors.md).

   [`Arithmetic] wraps rather than flattens: a division by zero is [Bigint]'s
   observation, and the seam that used to render it into a string here is gone.
   [`Not_absolute] carries the residual expression itself, which is the whole
   point of the constructor - the old message spent [to_string] on it and threw
   the value away, so a caller could not ask *which* symbol was still
   unresolved without re-parsing prose.

   Rendering is unchanged, so the diagnostics these become are the ones the
   fixtures already pin. *)
type resource = { limit : int; needed : int }

type error =
  [ `Arithmetic of Bigint.error
  | `Shift_out_of_range of Bigint.t
  | `Not_absolute of t
  | `Resource of resource ]

(* A resource budget, not an arithmetic cap: values keep their arbitrary precision (design.md
   §4.3), but an operation whose operands or result would need more than this many bits is
   refused, so a short expression cannot demand memory and time out of proportion to its length. *)
let default_resource_limit = 65_536
let resource_limit = ref default_resource_limit

let set_resource_limit ~bits =
  if bits < 1 then invalid_arg "Expr.set_resource_limit";
  resource_limit := bits

let resource_limit_bits () = !resource_limit

let pp_error ppf : error -> unit = function
  | `Arithmetic e -> Bigint.pp_error ppf e
  | `Shift_out_of_range _ -> Fmt.string ppf "shift count out of range"
  | `Not_absolute residual -> Fmt.pf ppf "%s is not an absolute value here" (to_string residual)
  | `Resource { limit; needed } ->
      Fmt.pf ppf "expression value needs %d bits, over the limit of %d" needed limit

let error_code : error -> string = function
  | `Arithmetic _ -> "expr.arithmetic"
  | `Shift_out_of_range _ -> "expr.shift"
  | `Not_absolute _ -> "expr.not-absolute"
  | `Resource _ -> "expr.resource"

(* [Bigint]'s failures are ours once they happen inside an expression, so this
   is a wrap with no added context - the tag is what records the crossing, and
   [Err.map_error] the position of it (§2). *)
let of_bigint r = Err.map_error ~pos:__POS__ ~pp_error (fun e -> `Arithmetic e) r
let fail ?pos (e : error) = Err.fail ?pos ~pp_error e

let within_budget needed =
  if needed <= !resource_limit then Ok ()
  else fail ~pos:__POS__ (`Resource { limit = !resource_limit; needed })

let ( let* ) = Result.bind

let apply_binop op a b =
  let na = Bigint.numbits a and nb = Bigint.numbits b in
  let* () =
    match op with
    | Add | Sub -> within_budget (max na nb + 1)
    | Mul -> within_budget (na + nb)
    | Shl -> (
        match Bigint.to_int_opt b with
        | Some n when n >= 0 && n <= 4096 -> within_budget (na + n)
        | _ -> Ok ())
    | Div | Mod | And | Or | Xor | Shr -> within_budget (max na nb)
  in
  match op with
  | Add -> Ok (Bigint.add a b)
  | Sub -> Ok (Bigint.sub a b)
  | Mul -> Ok (Bigint.mul a b)
  (* No zero guard of our own: [Bigint.div] checks, and duplicating the check
     here is how the two would eventually disagree. Its message is the one this
     function used to write, so nothing downstream sees a difference. *)
  | Div -> of_bigint (Bigint.div a b)
  | Mod -> of_bigint (Bigint.rem a b)
  | And -> Ok (Bigint.logand a b)
  | Or -> Ok (Bigint.logor a b)
  | Xor -> Ok (Bigint.logxor a b)
  | Shl -> (
      match Bigint.to_int_opt b with
      | Some n when n >= 0 && n <= 4096 -> Ok (Bigint.shift_left_exn a n)
      | _ -> fail ~pos:__POS__ (`Shift_out_of_range b))
  | Shr -> (
      match Bigint.to_int_opt b with
      | Some n when n >= 0 && n <= 4096 -> Ok (Bigint.shift_right_exn a n)
      | _ -> fail ~pos:__POS__ (`Shift_out_of_range b))

(* [fold] rewrites the expression, collapsing every absolute subterm and leaving
   the rest alone. It is not [eval]: the result is an expression, so a caller
   that needs a number asks for one separately and gets a diagnostic if the
   expression is not yet absolute. *)
let rec fold env e =
  match e with
  | Const _ -> Ok e
  | Symbol s -> Ok (match env.lookup s with Some v -> Const v | None -> e)
  | Current_location -> Ok (match env.here with Some v -> Const v | None -> e)
  | Local_ref _ -> Ok e
  | Unary (op, a) -> (
      match fold env a with
      | Error _ as err -> err
      | Ok (Const v) -> (
          match within_budget (Bigint.numbits v + 1) with
          | Error _ as err -> err
          | Ok () -> Ok (Const (match op with Neg -> Bigint.neg v | Lognot -> Bigint.lognot v)))
      | Ok a' -> Ok (Unary (op, a')))
  | Binary (op, a, b) -> (
      (* Left operand first, and each folded once: re-folding a failed operand doubled the work
         at every level of nesting. *)
      match fold env a with
      | Error _ as err -> err
      | Ok a' -> (
          match fold env b with
          | Error _ as err -> err
          | Ok b' -> (
              match (a', b') with
              | Const x, Const y -> (
                  match apply_binop op x y with Ok v -> Ok (Const v) | Error m -> Error m)
              | _ -> Ok (Binary (op, a', b')))))
  | Modifier (m, a) -> (
      match fold env a with Error _ as err -> err | Ok a' -> Ok (Modifier (m, a')))

(* [value] used to sit here. It had no callers, and its error branch smuggled a
   message into a [Symbol] node - a string wearing an expression's type, which
   is the failure this domain exists to remove. What it did that anyone wants is
   [absolute]. *)

(* [absolute] is the common case: an expression the caller requires to be a
   number now. The failure carries the residual expression, so the diagnostic
   can say *which* symbol is still unresolved rather than only that something
   was. *)
let absolute env e =
  match fold env e with
  | Error m -> Error m
  | Ok (Const v) -> Ok v
  | Ok residual -> fail ~pos:__POS__ (`Not_absolute residual)

let rec symbols acc = function
  | Const _ | Current_location | Local_ref _ -> acc
  | Symbol s -> if List.mem s acc then acc else s :: acc
  | Unary (_, a) | Modifier (_, a) -> symbols acc a
  | Binary (_, a, b) -> symbols (symbols acc a) b

let symbols e = List.rev (symbols [] e)
