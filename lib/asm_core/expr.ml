(* The common expression language (.ai/asm_plan.md §4.8).

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

let apply_binop op a b =
  match op with
  | Add -> Ok (Bigint.add a b)
  | Sub -> Ok (Bigint.sub a b)
  | Mul -> Ok (Bigint.mul a b)
  | Div -> if Bigint.is_zero b then Error "division by zero" else Ok (Bigint.div a b)
  | Mod -> if Bigint.is_zero b then Error "division by zero" else Ok (Bigint.rem a b)
  | And -> Ok (Bigint.logand a b)
  | Or -> Ok (Bigint.logor a b)
  | Xor -> Ok (Bigint.logxor a b)
  | Shl -> (
      match Bigint.to_int_opt b with
      | Some n when n >= 0 && n <= 4096 -> Ok (Bigint.shift_left a n)
      | _ -> Error "shift count out of range")
  | Shr -> (
      match Bigint.to_int_opt b with
      | Some n when n >= 0 && n <= 4096 -> Ok (Bigint.shift_right a n)
      | _ -> Error "shift count out of range")

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
      | Ok (Const v) -> Ok (Const (match op with Neg -> Bigint.neg v | Lognot -> Bigint.lognot v))
      | Ok a' -> Ok (Unary (op, a')))
  | Binary (op, a, b) -> (
      match (fold env a, fold env b) with
      | Error _, _ -> fold env a
      | _, Error _ -> fold env b
      | Ok (Const x), Ok (Const y) -> (
          match apply_binop op x y with Ok v -> Ok (Const v) | Error m -> Error m)
      | Ok a', Ok b' -> Ok (Binary (op, a', b')))
  | Modifier (m, a) -> (
      match fold env a with Error _ as err -> err | Ok a' -> Ok (Modifier (m, a')))

let value env e =
  match fold env e with Ok (Const v) -> Ok v | Ok e' -> Error e' | Error m -> Error (Symbol m)

(* [absolute] is the common case: an expression the caller requires to be a
   number now. The failure carries the residual expression, so the diagnostic
   can say *which* symbol is still unresolved rather than only that something
   was. *)
let absolute env e =
  match fold env e with
  | Error m -> Error m
  | Ok (Const v) -> Ok v
  | Ok residual -> Error (Printf.sprintf "%s is not an absolute value here" (to_string residual))

let rec symbols acc = function
  | Const _ | Current_location | Local_ref _ -> acc
  | Symbol s -> if List.mem s acc then acc else s :: acc
  | Unary (_, a) | Modifier (_, a) -> symbols acc a
  | Binary (_, a, b) -> symbols (symbols acc a) b

let symbols e = List.rev (symbols [] e)
