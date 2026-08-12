(* Bigint self-checks.

   These are the fast in-repo checks; the authoritative vectors come from Node
   BigInt (see test/js) so that a bug cannot be preserved symmetrically by
   testing an implementation against itself. Everything here is deliberately
   value-based so the same executable runs unchanged under native, js_of_ocaml,
   and Melange. *)

open Foundation

let failures = ref []
let check name cond = if not cond then failures := name :: !failures

let check_eq name got want =
  check (Printf.sprintf "%s: got %s want %s" name got want) (String.equal got want)

let b = Bigint.of_string_exn

(* {1 Parsing and rendering} *)

let () =
  check_eq "dec" (Bigint.to_string (b "12345678901234567890")) "12345678901234567890";
  check_eq "neg-dec" (Bigint.to_string (b "-42")) "-42";
  check_eq "hex" (Bigint.to_string (b "0xdeadbeef")) "3735928559";
  check_eq "bin" (Bigint.to_string (b "0b1011")) "11";
  check_eq "oct" (Bigint.to_string (b "0755")) "493";
  check_eq "zero" (Bigint.to_string (b "0")) "0";
  check_eq "hex-render" (Bigint.to_string_hex (b "3735928559")) "0xdeadbeef";
  check_eq "hex-render-neg" (Bigint.to_string_hex (b "-255")) "-0xff";
  check_eq "hex-render-zero" (Bigint.to_string_hex Bigint.zero) "0x0";
  check "bad-digit" (Result.is_error (Bigint.of_string "0b12"));
  check "empty" (Result.is_error (Bigint.of_string ""))

(* {1 Arithmetic, including values well beyond 64 bits} *)

let big = b "0x123456789abcdef0123456789abcdef0123456789abcdef0"
let big2 = b "0xfedcba9876543210fedcba9876543210"

let () =
  check_eq "add-comm"
    (Bigint.to_string (Bigint.add big big2))
    (Bigint.to_string (Bigint.add big2 big));
  check "sub-inverse" (Bigint.equal (Bigint.sub (Bigint.add big big2) big2) big);
  check "mul-comm" (Bigint.equal (Bigint.mul big big2) (Bigint.mul big2 big));
  check "distrib"
    (Bigint.equal
       (Bigint.mul big (Bigint.add big2 Bigint.one))
       (Bigint.add (Bigint.mul big big2) big));
  check "assoc"
    (Bigint.equal (Bigint.mul (Bigint.mul big big2) big) (Bigint.mul big (Bigint.mul big2 big)));
  (* min_int must survive of_int, which has no positive counterpart. *)
  check_eq "min-int" (Bigint.to_string (Bigint.of_int min_int)) (string_of_int min_int)

(* {1 Division: truncation toward zero, remainder takes the dividend's sign} *)

let divcheck a bb q r =
  let a = b a and bb = b bb in
  let gq, gr = Bigint.divrem_exn a bb in
  check_eq
    (Printf.sprintf "div %s/%s q" (Bigint.to_string a) (Bigint.to_string bb))
    (Bigint.to_string gq) q;
  check_eq
    (Printf.sprintf "div %s/%s r" (Bigint.to_string a) (Bigint.to_string bb))
    (Bigint.to_string gr) r;
  (* q*b + r = a must hold for every sign combination. *)
  check "div-identity" (Bigint.equal (Bigint.add (Bigint.mul gq bb) gr) a)

let () =
  divcheck "17" "5" "3" "2";
  divcheck "-17" "5" "-3" "-2";
  divcheck "17" "-5" "-3" "2";
  divcheck "-17" "-5" "3" "-2";
  divcheck "0" "7" "0" "0";
  divcheck "5" "17" "0" "5";
  divcheck "0x123456789abcdef0123456789abcdef0" "0x10000000000000000" "1311768467463790320"
    "1311768467463790320";
  (* Both halves of the checked/total pair, because they are the pair: the
     checked form must report, and the companion must still refuse. The
     companion now raises Err's structured exception rather than
     [Division_by_zero] - it is [Err.or_raise] over the checked form - so this
     catches the change instead of leaving it to be discovered. *)
  check "div-by-zero-checked" (Result.is_error (Bigint.divrem Bigint.one Bigint.zero));
  check "div-by-zero-exn"
    (try
       ignore (Bigint.divrem_exn Bigint.one Bigint.zero);
       false
     with Err.Exn.E _ -> true)

(* {1 Shifts} *)

let () =
  check "shl" (Bigint.equal (Bigint.shift_left_exn (b "1") 100) (b "0x10000000000000000000000000"));
  check "shl-shr" (Bigint.equal (Bigint.shift_right_exn (Bigint.shift_left_exn big 37) 37) big);
  check "shl-is-mul"
    (Bigint.equal (Bigint.shift_left_exn big 13)
       (Bigint.mul big (Bigint.shift_left_exn Bigint.one 13)));
  (* Arithmetic shift right rounds toward negative infinity. *)
  check_eq "shr-neg" (Bigint.to_string (Bigint.shift_right_exn (b "-5") 1)) "-3";
  check_eq "shr-neg-exact" (Bigint.to_string (Bigint.shift_right_exn (b "-4") 1)) "-2";
  check_eq "shr-neg-big" (Bigint.to_string (Bigint.shift_right_exn (b "-1") 64)) "-1"

(* {1 Bitwise over infinite-width two's complement} *)

let () =
  check_eq "and" (Bigint.to_string (Bigint.logand (b "0xff00") (b "0x0ff0"))) "3840";
  check_eq "or" (Bigint.to_string (Bigint.logor (b "0xf0") (b "0x0f"))) "255";
  check_eq "xor" (Bigint.to_string (Bigint.logxor (b "0xff") (b "0x0f"))) "240";
  check_eq "not" (Bigint.to_string (Bigint.lognot (b "0"))) "-1";
  check_eq "not-neg" (Bigint.to_string (Bigint.lognot (b "-1"))) "0";
  (* lognot x = -x-1 must agree with the two's-complement view. *)
  check "not-identity" (Bigint.equal (Bigint.lognot big) (Bigint.sub (Bigint.neg big) Bigint.one));
  (* -1 is all ones, so it is the identity for logand. *)
  check "and-minus-one" (Bigint.equal (Bigint.logand big (b "-1")) big);
  check "or-minus-one" (Bigint.equal (Bigint.logor big (b "-1")) (b "-1"));
  check_eq "and-neg" (Bigint.to_string (Bigint.logand (b "-2") (b "-3"))) "-4";
  check_eq "or-neg" (Bigint.to_string (Bigint.logor (b "-2") (b "-3"))) "-1";
  check_eq "xor-neg" (Bigint.to_string (Bigint.logxor (b "-2") (b "-3"))) "3"

(* {1 Field conversion — the §4.3 range-check boundary}

   Every width the four M1 targets actually use, at min-1 / min / max / max+1. *)

let widths = [ 5; 7; 8; 12; 16; 19; 21; 26; 32; 64 ]

let () =
  List.iter
    (fun w ->
      let two_pow k = Bigint.shift_left_exn Bigint.one k in
      let smax = Bigint.sub (two_pow (w - 1)) Bigint.one in
      let smin = Bigint.neg (two_pow (w - 1)) in
      let umax = Bigint.sub (two_pow w) Bigint.one in
      let tag s = Printf.sprintf "w%d-%s" w s in
      check (tag "smax") (Bigint.fits_signed_exn ~width:w smax);
      check (tag "smax+1") (not (Bigint.fits_signed_exn ~width:w (Bigint.add smax Bigint.one)));
      check (tag "smin") (Bigint.fits_signed_exn ~width:w smin);
      check (tag "smin-1") (not (Bigint.fits_signed_exn ~width:w (Bigint.sub smin Bigint.one)));
      check (tag "umax") (Bigint.fits_unsigned_exn ~width:w umax);
      check (tag "umax+1") (not (Bigint.fits_unsigned_exn ~width:w (Bigint.add umax Bigint.one)));
      check (tag "u-neg") (not (Bigint.fits_unsigned_exn ~width:w Bigint.minus_one));
      (* A negative value narrows to its two's-complement pattern. *)
      match Bigint.to_bits ~width:w Bigint.minus_one with
      | Ok v -> check (tag "to_bits-1") (Bigint.equal v umax)
      | Error e ->
          check (tag ("to_bits-1: " ^ Fmt.to_to_string Bigint.pp_error (Err.Error.kind e))) false)
    widths

let () =
  check "to_bits-out-of-range" (Result.is_error (Bigint.to_bits ~width:8 (b "256")));
  check "to_bits-neg-out-of-range" (Result.is_error (Bigint.to_bits ~width:8 (b "-129")));
  (match Bigint.to_bits ~width:8 (b "-128") with
  | Ok v -> check_eq "to_bits-smin" (Bigint.to_string v) "128"
  | Error _ -> check "to_bits-smin" false);
  (* 42 in the ARM/AArch64 immediate fields the M1 fixtures use. *)
  match Bigint.to_bits ~width:12 (b "42") with
  | Ok v -> check_eq "to_bits-42" (Bigint.to_string_hex v) "0x2a"
  | Error _ -> check "to_bits-42" false

let () =
  match List.rev !failures with
  | [] -> print_endline "bigint: all checks passed"
  | fs ->
      List.iter (fun f -> print_endline ("FAIL " ^ f)) fs;
      exit 1
