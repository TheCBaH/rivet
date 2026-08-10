  $ sh run.sh
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
      mag = [0x0000; 0x0000; 0x0000; 0x0000; 0x0040] }
  
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
  "1_000"                  -> error: invalid digit '_' in base-10 literal "1_000"
  
  # rendering
  0 = 0 (0x0)
  42 = 42 (0x2a)
  -42 = -42 (-0x2a)
  255 = 255 (0xff)
  -255 = -255 (-0xff)
  65536 = 65536 (0x10000)
  -1 = -1 (-0x1)
  0x123456789abcdef0123456789abcdef0 = 24197857203266734864793317670504947440 (0x123456789abcdef0123456789abcdef0)
  
  # beyond 2^53
  2^53 = 9007199254740992 (0x20000000000000)
  2^53 + 1 = 9007199254740993 (0x20000000000001)
  2^64 - 1 = 18446744073709551615 (0xffffffffffffffff)
  2^127 = 170141183460469231731687303715884105728 (0x80000000000000000000000000000000)
  (2^64 - 1) * (2^64 - 1) = 340282366920938463426481119284349108225 (0xfffffffffffffffe0000000000000001)
  2^53 + 1 =
    { sign = +1;
      base = 2^14;
      mag = [0x0001; 0x0000; 0x0000; 0x0800] }
