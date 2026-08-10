(** Arbitrary-precision signed integers, pure OCaml.

    [.ai/asm_plan.md] §5.3 bans Zarith/GMP from the production closure (it is a C-backed package),
    so the minimal signed big-integer operations required by assembly expressions live here.

    Representation is sign-magnitude with an immutable little-endian list of base-2^14 limbs.

    The plan called for base 2^15; 2^14 is used instead for the same reason the plan gave for 2^15 —
    the [linux/i386] and [linux/arm/v7] CI legs have a 31-bit native [int] ([max_int = 2^30 - 1]).
    With base 2^15 the schoolbook inner step [limb + limb * limb + carry] reaches exactly [max_int],
    leaving zero headroom. Base 2^14 caps it below 2^29. *)

type t

(** {1 Constructors} *)

val zero : t
val one : t
val minus_one : t
val of_int : int -> t

val of_string : string -> (t, string) result
(** [of_string s] accepts an optional sign followed by GAS integer spellings: [0x]/[0X] hex,
    [0b]/[0B] binary, a leading [0] for octal, otherwise decimal. Underscores are not accepted. *)

val of_string_exn : string -> t

(** {1 Predicates and comparison} *)

val sign : t -> int
val is_zero : t -> bool
val compare : t -> t -> int
val equal : t -> t -> bool

val numbits : t -> int
(** [numbits x] is the number of bits in the magnitude of [x]; [numbits zero] is 0. It describes
    |x|, not a two's-complement width. *)

(** {1 Arithmetic} *)

val neg : t -> t
val abs : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t

val divrem : t -> t -> t * t
(** [divrem a b] truncates toward zero, so the remainder takes the sign of [a] and
    [fst (divrem a b) * b + snd (divrem a b) = a]. Raises [Division_by_zero] when [b] is zero. *)

val div : t -> t -> t
val rem : t -> t -> t

(** {1 Bitwise}

    Bitwise operations treat operands as two's-complement values of unbounded width, so negative
    operands behave as though preceded by infinitely many set bits. *)

val logand : t -> t -> t
val logor : t -> t -> t
val logxor : t -> t -> t
val lognot : t -> t
val shift_left : t -> int -> t

val shift_right : t -> int -> t
(** Arithmetic shift right: rounds toward negative infinity, matching the usual assembler semantics
    for [>>] on signed values. *)

(** {1 Field conversion}

    This is where [.ai/asm_plan.md] §4.3's rule is enforced: range checks happen when an
    arbitrary-precision value is narrowed to a specific field, never by letting a host integer
    overflow during folding. *)

val fits_signed : width:int -> t -> bool
val fits_unsigned : width:int -> t -> bool

val to_bits : width:int -> t -> (t, string) result
(** [to_bits ~width x] is the [width]-bit two's-complement pattern of [x] as a non-negative value.
    It fails unless [x] fits the width either as a signed or as an unsigned value, so a value that
    is out of range for both is an error rather than a silent truncation. *)

(** {1 Rendering} *)

val to_string : t -> string

val to_string_hex : t -> string
(** [to_string_hex x] renders as [-0x..] / [0x..], lower case, no leading zeros (other than [0x0]).
*)

val to_int_opt : t -> int option

(** {1 Pretty-printing}

    None of these emit a breakable space, so the enclosing box's right margin can never reflow them
    — expect output stays stable regardless of where a value is printed. *)

val pp : Format.formatter -> t -> unit
(** Decimal, as {!to_string}. *)

val pp_hex : Format.formatter -> t -> unit
(** Hexadecimal, as {!to_string_hex}. *)

val pp_repr : Format.formatter -> t -> unit
(** [pp_repr] prints the internal representation rather than the value:

    {[
      { sign = +1;
        base = 2^14;
        mag = [0x0002; 0x0001] }
    ]}

    [mag] holds the limbs least-significant first, and [sign] is one of [-1], [0], [+1]. [base] is
    not a field of the type — it is the compile-time limb base, printed because it is what makes
    the limb list readable and because changing it reinterprets every committed expect output.

    This exists for expect tests, where it makes the representation invariants observable:
    canonical zero is the only value with an empty [mag], and a magnitude never keeps a
    most-significant zero limb. Comparing values with {!equal} cannot see either property, because
    both normalize away.

    The layout is {!Fmt.Dump.record}'s, so it is a vertical box: long magnitudes wrap at the
    enclosing formatter's margin. *)

(** {1 64-bit interoperation}

    Addresses and encoded field values are [int64] throughout the image and codec layers, while
    expressions are arbitrary-precision. These are the two conversions at that boundary, and they
    are deliberately not [to_int]: a 64-bit address does not fit a native [int] on the 31-bit CI
    legs, so routing addresses through [int] would work everywhere the tests usually run and fail
    on exactly the platforms the project promises to support. *)

val of_int64 : int64 -> t

val to_int64_opt : t -> int64 option
(** [None] when the value does not fit a signed 64-bit integer. *)

val to_uint64_opt : t -> int64 option
(** The unsigned reading: accepts [0 .. 2^64 - 1] and returns the two's-complement bit pattern, so an
    address at or above [2^63] round-trips instead of being rejected. Negative values are [None]. *)
