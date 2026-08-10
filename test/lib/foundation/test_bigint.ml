(* Expect tests for Foundation.Bigint.

   Sections that the three-build equality gate also needs come from
   Corpus.Bigint_corpus, so the values checked here and the values compared
   across native/js_of_ocaml/Melange are one definition rather than two.
   [to_bits] is written inline instead: its cases are a table of width/value
   pairs whose whole interest is which ones fail and with what message, and that
   reads better next to the output it produces than behind a function call. *)

open Foundation
open Foundation_corpus.Bigint_corpus

let%expect_test "representation" =
  representation ();
  [%expect
    {|
    # representation
    zero =
      { sign = 0;
        base = 2^14;
        mag = [] }
    one =
      { sign = +1;
        base = 2^14;
        mag = [0x0001] }
    minus_one =
      { sign = -1;
        base = 2^14;
        mag = [0x0001] }
    2^14 - 1 =
      { sign = +1;
        base = 2^14;
        mag = [0x3fff] }
    2^14 =
      { sign = +1;
        base = 2^14;
        mag = [0x0000; 0x0001] }
    2^14 - (2^14 - 1) =
      { sign = +1;
        base = 2^14;
        mag = [0x0001] }
    big - big =
      { sign = 0;
        base = 2^14;
        mag = [] }
    of_int (-2^30) =
      { sign = -1;
        base = 2^14;
        mag = [0x0000; 0x0000; 0x0004] }
    -2^62 =
      { sign = -1;
        base = 2^14;
        mag = [0x0000; 0x0000; 0x0000; 0x0000; 0x0040] } |}]

let%expect_test "parsing" =
  parsing ();
  [%expect
    {|
    # parsing
    "0"                      -> 0
    "-0"                     -> 0
    "42"                     -> 42
    "-42"                    -> -42
    "+42"                    -> 42
    "0x2a"                   -> 42
    "0X2A"                   -> 42
    "0b101010"               -> 42
    "052"                    -> 42
    "12345678901234567890123456789012345678901234567890" -> 12345678901234567890123456789012345678901234567890
    "-12345678901234567890123456789012345678901234567890" -> -12345678901234567890123456789012345678901234567890
    ""                       -> error: empty integer literal
    "-"                      -> error: integer literal "-" has no digits
    "0x"                     -> error: integer literal "0x" has no digits
    "0b12"                   -> error: invalid digit '2' in base-2 literal "0b12"
    "099"                    -> error: invalid digit '9' in base-8 literal "099"
    "12a"                    -> error: invalid digit 'a' in base-10 literal "12a"
    "1_000"                  -> error: invalid digit '_' in base-10 literal "1_000" |}]

let%expect_test "rendering" =
  rendering ();
  [%expect
    {|
    # rendering
    0 = 0 (0x0)
    42 = 42 (0x2a)
    -42 = -42 (-0x2a)
    255 = 255 (0xff)
    -255 = -255 (-0xff)
    65536 = 65536 (0x10000)
    -1 = -1 (-0x1)
    0x123456789abcdef0123456789abcdef0 = 24197857203266734864793317670504947440 (0x123456789abcdef0123456789abcdef0) |}]

let%expect_test "beyond 2^53" =
  beyond_2_53 ();
  [%expect
    {|
    # beyond 2^53
    2^53 = 9007199254740992 (0x20000000000000)
    2^53 + 1 = 9007199254740993 (0x20000000000001)
    2^64 - 1 = 18446744073709551615 (0xffffffffffffffff)
    2^127 = 170141183460469231731687303715884105728 (0x80000000000000000000000000000000)
    (2^64 - 1) * (2^64 - 1) = 340282366920938463426481119284349108225 (0xfffffffffffffffe0000000000000001)
    2^53 + 1 =
      { sign = +1;
        base = 2^14;
        mag = [0x0001; 0x0000; 0x0000; 0x0800] } |}]

(* The §4.3 boundary: narrowing to a field either yields the two's-complement
   pattern or the exact diagnostic, and both are reviewed. [%a] takes no padding
   modifier, so the operand is rendered first and padded as a string. *)
let bits width s =
  let x = Bigint.of_string_exn s in
  Fmt.pr "to_bits ~width:%-3d %-22s -> %a@." width (Fmt.str "%a" Bigint.pp x)
    (pp_result Bigint.pp_hex) (Bigint.to_bits ~width x)

let%expect_test "to_bits: 8-bit boundaries" =
  List.iter (fun s -> bits 8 s) [ "127"; "128"; "255"; "256"; "-128"; "-129" ];
  [%expect
    {|
    to_bits ~width:8   127                    -> 0x7f
    to_bits ~width:8   128                    -> 0x80
    to_bits ~width:8   255                    -> 0xff
    to_bits ~width:8   256                    -> error: value 0x100 does not fit in 8 bits (signed or unsigned)
    to_bits ~width:8   -128                   -> 0x80
    to_bits ~width:8   -129                   -> error: value -0x81 does not fit in 8 bits (signed or unsigned) |}]

let%expect_test "to_bits: negative values narrow to their pattern" =
  bits 12 "-42";
  bits 32 "-1";
  bits 64 "-1";
  [%expect
    {|
    to_bits ~width:12  -42                    -> 0xfd6
    to_bits ~width:32  -1                     -> 0xffffffff
    to_bits ~width:64  -1                     -> 0xffffffffffffffff |}]

let%expect_test "to_bits: 64-bit boundaries" =
  List.iter
    (fun s -> bits 64 s)
    [
      "9223372036854775807"; "-9223372036854775808"; "18446744073709551615"; "18446744073709551616";
    ];
  [%expect
    {|
    to_bits ~width:64  9223372036854775807    -> 0x7fffffffffffffff
    to_bits ~width:64  -9223372036854775808   -> 0x8000000000000000
    to_bits ~width:64  18446744073709551615   -> 0xffffffffffffffff
    to_bits ~width:64  18446744073709551616   -> error: value 0x10000000000000000 does not fit in 64 bits (signed or unsigned) |}]
