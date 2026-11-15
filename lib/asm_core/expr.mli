type binop = Add | Sub | Mul | Div | Mod | And | Or | Xor | Shl | Shr
type unop = Neg | Lognot

type t =
  | Const of Foundation.Bigint.t
  | Symbol of string
  | Current_location
  | Local_ref of int * [ `Back | `Forward ]
  | Binary of binop * t * t
  | Unary of unop * t
  | Modifier of string * t

val binop_name : binop -> string
val unop_name : unop -> string
val pp : Format.formatter -> t -> unit
val to_string : t -> string

type env = { lookup : string -> Foundation.Bigint.t option; here : Foundation.Bigint.t option }

val no_env : env

type error =
  [ `Arithmetic of Foundation.Bigint.error
  | `Not_absolute of t
  | `Shift_out_of_range of Foundation.Bigint.t ]

val pp_error : Format.formatter -> error -> unit
val error_code : error -> string
val fold : env -> t -> (t, error Err.Error.t) result
val absolute : env -> t -> (Foundation.Bigint.t, error) Err.t
val symbols : t -> string list
