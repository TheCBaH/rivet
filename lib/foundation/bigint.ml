(* Sign-magnitude arbitrary-precision integers.

   The magnitude is an immutable little-endian list of base-2^14 limbs with no
   trailing (most-significant) zeros, so structural equality of [t] coincides
   with numeric equality. See bigint.mli for why base 2^14 rather than 2^15. *)

let limb_bits = 14
let limb_base = 1 lsl limb_bits
let limb_mask = limb_base - 1

type t = { sign : int; mag : int list }

let zero = { sign = 0; mag = [] }

(* Drop most-significant zero limbs, collapsing an all-zero magnitude to the
   canonical zero. *)
let rec strip = function
  | [] -> []
  | x :: tl -> ( match strip tl with [] -> if x = 0 then [] else [ x ] | tl' -> x :: tl')

let normalize sign mag = match strip mag with [] -> zero | m -> { sign; mag = m }
let sign x = x.sign
let is_zero x = x.sign = 0
let neg x = if x.sign = 0 then zero else { x with sign = -x.sign }
let abs x = if x.sign >= 0 then x else { x with sign = 1 }

(* {1 Magnitude primitives} *)

let rec limbs_of_neg v = if v = 0 then [] else -(v mod limb_base) :: limbs_of_neg (v / limb_base)

let of_int n =
  if n = 0 then zero
  else
    (* min_int has no positive counterpart, so accumulate from the negative side
       and apply the sign at the end. For v < 0, [v mod limb_base] lies in
       (-limb_base, 0], so its negation is the limb value. *)
    normalize (if n < 0 then -1 else 1) (limbs_of_neg (if n < 0 then n else -n))

let one = of_int 1
let minus_one = of_int (-1)

let cmp_mag a b =
  let la = List.length a and lb = List.length b in
  if la <> lb then compare la lb
  else
    (* Equal lengths: walk from least to most significant, letting the most
       significant difference win. *)
    let rec go a b acc =
      match (a, b) with
      | [], [] -> acc
      | x :: xs, y :: ys -> go xs ys (if x <> y then compare x y else acc)
      | _ -> acc
    in
    go a b 0

let rec add_mag a b carry =
  match (a, b) with
  | [], [] -> if carry = 0 then [] else [ carry ]
  | [], x :: xs | x :: xs, [] ->
      let s = x + carry in
      (s land limb_mask) :: add_mag xs [] (s lsr limb_bits)
  | x :: xs, y :: ys ->
      let s = x + y + carry in
      (s land limb_mask) :: add_mag xs ys (s lsr limb_bits)

(* Requires |a| >= |b|, so the final borrow is always zero. *)
let rec sub_mag a b borrow =
  match (a, b) with
  | [], _ -> []
  | x :: xs, [] ->
      let d = x - borrow in
      if d < 0 then (d + limb_base) :: sub_mag xs [] 1 else d :: sub_mag xs [] 0
  | x :: xs, y :: ys ->
      let d = x - y - borrow in
      if d < 0 then (d + limb_base) :: sub_mag xs ys 1 else d :: sub_mag xs ys 0

(* a * k + carry. The product of two limbs plus a carry stays below 2^29, so
   this is safe on a 31-bit native int. *)
let rec mul_limb a k carry =
  match a with
  | [] -> if carry = 0 then [] else [ carry ]
  | x :: xs ->
      let acc = (x * k) + carry in
      (acc land limb_mask) :: mul_limb xs k (acc lsr limb_bits)

let rec mul_mag a b =
  match b with
  | [] -> []
  | k :: rest ->
      let shifted = match mul_mag a rest with [] -> [] | m -> 0 :: m in
      add_mag (mul_limb a k 0) shifted 0

let rec bits_of_limb v = if v = 0 then 0 else 1 + bits_of_limb (v lsr 1)

let numbits_mag mag =
  let rec go mag idx =
    match mag with
    | [] -> 0
    | [ top ] -> (idx * limb_bits) + bits_of_limb top
    | _ :: tl -> go tl (idx + 1)
  in
  go mag 0

let numbits x = numbits_mag x.mag

let rec test_bit_mag mag i =
  match mag with
  | [] -> 0
  | x :: tl -> if i < limb_bits then (x lsr i) land 1 else test_bit_mag tl (i - limb_bits)

(* 2 * mag + bit *)
let rec shl1_mag mag bit =
  match mag with
  | [] -> if bit = 0 then [] else [ bit ]
  | x :: tl ->
      let v = (x lsl 1) lor bit in
      (v land limb_mask) :: shl1_mag tl (v lsr limb_bits)

(* {1 Comparison} *)

let compare a b =
  if a.sign <> b.sign then compare a.sign b.sign
  else if a.sign = 0 then 0
  else a.sign * cmp_mag a.mag b.mag

let equal a b = compare a b = 0

(* {1 Additive arithmetic} *)

let rec add a b =
  if a.sign = 0 then b
  else if b.sign = 0 then a
  else if a.sign = b.sign then normalize a.sign (add_mag a.mag b.mag 0)
  else
    let c = cmp_mag a.mag b.mag in
    if c = 0 then zero
    else if c > 0 then normalize a.sign (sub_mag a.mag b.mag 0)
    else normalize b.sign (sub_mag b.mag a.mag 0)

and sub a b = add a (neg b)

let mul a b =
  if a.sign = 0 || b.sign = 0 then zero else normalize (a.sign * b.sign) (mul_mag a.mag b.mag)

(* {1 Division}

   Binary long division, most significant bit first. Assembler constants are
   small and the vector suite tops out at a few hundred bits, so a
   shift-and-subtract recursion is worth more here than Knuth D's constant
   factor. Both quotient and remainder are grown by [shl1_mag], which keeps the
   whole loop allocation-only. *)

let divrem_mag a b =
  let rec go i q r =
    if i < 0 then (q, r)
    else
      let r = shl1_mag r (test_bit_mag a i) in
      if cmp_mag r b >= 0 then go (i - 1) (shl1_mag q 1) (strip (sub_mag r b 0))
      else go (i - 1) (shl1_mag q 0) r
  in
  go (numbits_mag a - 1) [] []

let divrem a b =
  if b.sign = 0 then raise Division_by_zero
  else if a.sign = 0 then (zero, zero)
  else
    let q, r = divrem_mag a.mag b.mag in
    (* Truncated toward zero, so the remainder carries the dividend's sign. *)
    (normalize (a.sign * b.sign) q, normalize a.sign r)

let div a b = fst (divrem a b)
let rem a b = snd (divrem a b)

(* {1 Shifts} *)

let rec prepend_zeros n l = if n <= 0 then l else 0 :: prepend_zeros (n - 1) l

let rec shl_bits mag k carry =
  match mag with
  | [] -> if carry = 0 then [] else [ carry ]
  | x :: tl ->
      let v = (x lsl k) lor carry in
      (v land limb_mask) :: shl_bits tl k (v lsr limb_bits)

let shift_left x n =
  if n < 0 then invalid_arg "Bigint.shift_left: negative shift"
  else if x.sign = 0 || n = 0 then x
  else normalize x.sign (prepend_zeros (n / limb_bits) (shl_bits x.mag (n mod limb_bits) 0))

let rec drop n l = if n <= 0 then l else match l with [] -> [] | _ :: tl -> drop (n - 1) tl

(* [k = 0] needs no special case: [y lsl limb_bits] has no bits inside the mask. *)
let rec shr_bits mag k =
  match mag with
  | [] -> []
  | [ x ] -> [ x lsr k ]
  | x :: (y :: _ as tl) -> ((x lsr k) lor ((y lsl (limb_bits - k)) land limb_mask)) :: shr_bits tl k

let shift_right_mag mag n = shr_bits (drop (n / limb_bits) mag) (n mod limb_bits)
let rec any_bit_below mag n i = i < n && (test_bit_mag mag i = 1 || any_bit_below mag n (i + 1))

let shift_right x n =
  if n < 0 then invalid_arg "Bigint.shift_right: negative shift"
  else if x.sign = 0 || n = 0 then x
  else if x.sign > 0 then normalize 1 (shift_right_mag x.mag n)
  else
    (* Arithmetic shift rounds toward negative infinity, so a negative value
       loses one more when any shifted-out bit was set. *)
    let q = normalize 1 (shift_right_mag x.mag n) in
    neg (if any_bit_below x.mag n 0 then add q one else q)

(* {1 Bitwise}

   Operands are widened to a common two's-complement width, combined limb-wise,
   and converted back. The extra limb guarantees the sign bit is representable
   for both inputs and for the result. *)

let rec take_pad n l =
  if n <= 0 then []
  else match l with [] -> 0 :: take_pad (n - 1) [] | x :: tl -> x :: take_pad (n - 1) tl

(* Adds [carry] while preserving the limb count: a carry out of the top is
   discarded, which is exactly the truncation two's complement wants. *)
let rec incr_fixed l carry =
  match l with
  | [] -> []
  | x :: tl ->
      let v = x + carry in
      (v land limb_mask) :: incr_fixed tl (v lsr limb_bits)

let complement l = List.map (fun x -> lnot x land limb_mask) l

let twos_complement ~len x =
  let padded = take_pad len x.mag in
  if x.sign >= 0 then padded else incr_fixed (complement padded) 1

let from_twos_complement ~negative limbs =
  if not negative then normalize 1 limbs else normalize (-1) (incr_fixed (complement limbs) 1)

let bitwise op negative a b =
  let len = max (List.length a.mag) (List.length b.mag) + 1 in
  let combined =
    List.map2 (fun x y -> op x y land limb_mask) (twos_complement ~len a) (twos_complement ~len b)
  in
  from_twos_complement ~negative:(negative (a.sign < 0) (b.sign < 0)) combined

let logand a b = bitwise ( land ) (fun x y -> x && y) a b
let logor a b = bitwise ( lor ) (fun x y -> x || y) a b
let logxor a b = bitwise ( lxor ) (fun x y -> x <> y) a b
let lognot x = neg (add x one)

(* {1 Field conversion}

   This is where §4.3's rule lives: narrowing to a field is the only place a
   range check happens, and it is explicit rather than a host-integer overflow. *)

let fits_unsigned ~width x =
  if width < 0 then invalid_arg "Bigint.fits_unsigned: negative width"
  else x.sign >= 0 && numbits x <= width

let fits_signed ~width x =
  if width < 0 then invalid_arg "Bigint.fits_signed: negative width"
  else if width = 0 then is_zero x
  else if x.sign >= 0 then numbits x <= width - 1
  else
    (* The negative range reaches one further: |x| may equal 2^(width-1) exactly,
       whose numbits is width. *)
    numbits x <= width - 1 || equal (abs x) (shift_left one (width - 1))

(* {1 Rendering and parsing} *)

let hex_digits = "0123456789abcdef"

let to_string_hex_mag mag =
  let n = numbits_mag mag in
  if n = 0 then "0"
  else
    let nibble d =
      test_bit_mag mag (d * 4)
      lor (test_bit_mag mag ((d * 4) + 1) lsl 1)
      lor (test_bit_mag mag ((d * 4) + 2) lsl 2)
      lor (test_bit_mag mag ((d * 4) + 3) lsl 3)
    in
    let top = ((n + 3) / 4) - 1 in
    (* Ascend from the least significant nibble, prepending, so the result comes
       out most significant first. *)
    let rec go d acc = if d > top then acc else go (d + 1) (hex_digits.[nibble d] :: acc) in
    String.of_seq (List.to_seq (go 0 []))

let to_string_hex x =
  match x.sign with 0 -> "0x0" | s -> (if s < 0 then "-0x" else "0x") ^ to_string_hex_mag x.mag

let to_bits ~width x =
  if width <= 0 then Error "to_bits: width must be positive"
  else if fits_unsigned ~width x then Ok x
  else if fits_signed ~width x then if x.sign >= 0 then Ok x else Ok (add (shift_left one width) x)
  else
    Error
      (Printf.sprintf "value %s does not fit in %d bits (signed or unsigned)" (to_string_hex x)
         width)

let ten = of_int 10

let to_string x =
  if is_zero x then "0"
  else
    let rec digits v acc =
      if is_zero v then acc
      else
        let q, r = divrem v ten in
        let d = match r.mag with [] -> 0 | lo :: _ -> lo in
        digits q (Char.chr (Char.code '0' + d) :: acc)
    in
    let s = String.of_seq (List.to_seq (digits (abs x) [])) in
    if x.sign < 0 then "-" ^ s else s

let digit_value c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

let of_string s =
  let n = String.length s in
  let sgn, after_sign =
    if n = 0 then (1, 0) else match s.[0] with '-' -> (-1, 1) | '+' -> (1, 1) | _ -> (1, 0)
  in
  let base, start =
    if
      after_sign + 1 < n
      && s.[after_sign] = '0'
      && (s.[after_sign + 1] = 'x' || s.[after_sign + 1] = 'X')
    then (16, after_sign + 2)
    else if
      after_sign + 1 < n
      && s.[after_sign] = '0'
      && (s.[after_sign + 1] = 'b' || s.[after_sign + 1] = 'B')
    then (2, after_sign + 2)
    else if after_sign + 1 < n && s.[after_sign] = '0' then (8, after_sign + 1)
    else (10, after_sign)
  in
  let bbase = of_int base in
  let rec go i acc =
    if i >= n then Ok (if sgn < 0 then neg acc else acc)
    else
      match digit_value s.[i] with
      | Some d when d < base -> go (i + 1) (add (mul acc bbase) (of_int d))
      | _ -> Error (Printf.sprintf "invalid digit %C in base-%d literal %S" s.[i] base s)
  in
  if n = 0 then Error "empty integer literal"
  else if start >= n then Error (Printf.sprintf "integer literal %S has no digits" s)
  else go start zero

let of_string_exn s = match of_string s with Ok v -> v | Error e -> invalid_arg e
let to_int_opt x = int_of_string_opt (to_string x)

(* {1 Pretty-printing}

   [pp] and [pp_hex] wrap the string renderers rather than the other way round:
   a decimal rendering is a repeated division, not a composition of boxes, so
   there is nothing for Format to lay out. No breakable space appears in any of
   these, so the right margin cannot reflow them. *)

let pp ppf x = Fmt.string ppf (to_string x)
let pp_hex ppf x = Fmt.string ppf (to_string_hex x)

(* The internal representation, for expect tests.

   Everything the invariants talk about is visible here: the sign is separate
   from the magnitude, limbs are listed least-significant first, and a canonical
   value has no trailing zero limb — so [{sign = 0; mag = []}] is the *only*
   legal spelling of zero, and a regression that leaves a stripped limb behind
   or a sign on an empty magnitude shows up as a diff rather than passing an
   equality check that normalizes it away. *)

let pp_limb ppf limb = Fmt.pf ppf "0x%04x" limb
let pp_sign ppf s = Fmt.string ppf (if s < 0 then "-1" else if s = 0 then "0" else "+1")
let pp_base ppf bits = Fmt.pf ppf "2^%d" bits

(* [Fmt.Dump.field]'s default label printer quotes the name and styles it, which
   would put [ "sign" = ...] and ANSI escapes into expect output; [Fmt.string]
   gives the plain OCaml record spelling instead. *)
let field name prj pp_v = Fmt.Dump.field ~label:Fmt.string name prj pp_v

let pp_repr =
  Fmt.Dump.record
    [
      field "sign" (fun x -> x.sign) pp_sign;
      (* Not a field of [t]: the limb base is a compile-time constant. It is
         printed anyway because it is what makes the limb list readable, and
         changing it silently reinterprets every committed expect output — so it
         should surface as a diff in all of them at once. *)
      field "base" (fun _ -> limb_bits) pp_base;
      field "mag" (fun x -> x.mag) (Fmt.Dump.list pp_limb);
    ]

(* {1 64-bit interoperation}

   The image and codec layers speak int64 - an address does not fit a native int
   on the 31-bit legs - so these are the conversions at that boundary. Both go
   through the unsigned bit pattern rather than through [Int64.neg], because
   [Int64.min_int] has no positive counterpart and negating it returns itself. *)

let limbs_of_uint64 v =
  let rec go acc v =
    if Int64.equal v 0L then List.rev acc
    else
      go
        (Int64.to_int (Int64.logand v (Int64.of_int limb_mask)) :: acc)
        (Int64.shift_right_logical v limb_bits)
  in
  List.rev (go [] v)

let of_int64 v =
  if Int64.equal v 0L then zero
  else if Int64.compare v 0L > 0 then normalize 1 (limbs_of_uint64 v)
  else
    (* Two's complement: the magnitude of a negative v is 2^64 - (unsigned v),
       computed as (~v) + 1 on the bit pattern, which is exact for min_int. *)
    let magnitude = normalize 1 (limbs_of_uint64 (Int64.neg v)) in
    if Int64.equal v Int64.min_int then
      neg
        ( normalize 1 (limbs_of_uint64 (Int64.logand v Int64.max_int)) |> fun m ->
          add m (shift_left one 63) )
    else neg magnitude

let uint64_of_mag m =
  List.fold_right
    (fun limb acc -> Int64.logor (Int64.shift_left acc limb_bits) (Int64.of_int limb))
    m 0L

let to_uint64_opt x =
  if is_zero x then Some 0L
  else if sign x < 0 then None
  else if numbits x > 64 then None
  else Some (uint64_of_mag x.mag)

let to_int64_opt x =
  if is_zero x then Some 0L
  else if numbits x > 64 then None
  else
    let bits = uint64_of_mag x.mag in
    if sign x > 0 then if Int64.compare bits 0L >= 0 then Some bits else None
    else if Int64.compare bits 0L > 0 then Some (Int64.neg bits)
    else if Int64.equal bits Int64.min_int then Some Int64.min_int
    else None
