let hex s =
  String.concat " " (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let show text =
  match I8080_as.assemble ~file:"t.asm" text with
  | Error ds -> print_endline (I8080_as.render ds)
  | Ok img ->
      List.iter
        (fun (s : I8080_as.segment) -> Printf.printf "%04x: %s\n" s.origin (hex s.bytes))
        img.I8080_as.segments

let%expect_test "literals: NNh, NNb, 0x, $NN, decimal, characters" =
  show {|	org 0
	db 0ffh, 0x2a, $7f, 1011b, 10, 'A', "z", 0ah, 12h
	dw 1234h, 0x1234, $1234|};
  [%expect {| 0000: ff 2a 7f 0b 0a 41 7a 0a 12 34 12 34 12 34 12 |}]

let%expect_test "operators and AS priorities" =
  show "\torg 0\n\tdw 2+3*4, (2+3)*4, 6!3, 1<<4+1, 5&3!1, 2^3, -2^2, 7#4, 3<4, 3>=3, 1+2!4";
  [%expect {| 0000: 0e 00 14 00 05 00 11 00 00 00 08 00 fc ff 03 00 01 00 01 00 07 00 |}]

let%expect_test "low and high, prefix and function" =
  show "\torg 0\nx\tequ 1234h\n\tdb low x, high x, low(x)+1, high(x+0100h)";
  [%expect {| 0000: 34 12 35 13 |}]

let%expect_test "equ, set, forward references, several org, ds, end" =
  show
    {|	org 100h
v	set 1
	db v
v	set v+1
	db v
	jmp later
	org 200h
later:	db size
	ds 2
	db 9
size	equ later-1f0h
	end
	db 1|};
  [%expect {|
    0100: 01 02 c3 00 02
    0200: 10
    0203: 09 |}]

let%expect_test "macros, local labels and parameters" =
  show {|m	macro a,b
	mvi a,b
	jmp $$l
$$l:	inr a
	endm
	org 0
	m b,1
	m c,2|};
  [%expect {| 0000: 06 01 c3 05 00 04 0e 02 c3 0b 00 0c |}]

let%expect_test "rept, if/else/endif, dup" =
  show
    {|	org 0
n	set 0
	rept 3
n	set n+1
	db n
	endm
	if n = 3
	db 0aah
	else
	db 0bbh
	endif
	db 2 dup (1,2), 3 dup (0)|};
  [%expect {| 0000: 01 02 03 aa 01 02 01 02 00 00 00 |}]

let%expect_test "errors carry positions; unsupported constructs are listed" =
  show {|	org 0
	mvi a,300
	.8080
	foo bar
	db undefined
|};
  [%expect
    {|
    t.asm:2: mvi operand 300 does not fit -128..255
    t.asm:3: unsupported: directive or instruction .8080
    t.asm:4: unsupported: directive or instruction foo
    t.asm:5: undefined symbol undefined |}]

let%expect_test "macro errors name the expansion" =
  show {|m	macro
	mvi a,300
	endm
	org 0
	m
|};
  [%expect {| t.asm:2 (in m from line 5): mvi operand 300 does not fit -128..255 |}]

let%expect_test "gaps and the listing map" =
  (match I8080_as.assemble ~file:"t.asm" {|	org 0
	nop
	org 4
lab:	mvi a,lab
	db 1
|} with
  | Ok img ->
      let origin, bytes = I8080_as.flatten img in
      Printf.printf "%04x: %s\n%s\n" origin (hex bytes) (I8080_as.listing_to_string img)
  | Error ds -> print_endline (I8080_as.render ds));
  [%expect
    {|
    0000: 00 00 00 00 3e 04 01
    0000               t.asm:1  org 0
    0000 00            t.asm:2  nop
    0001               t.asm:3  org 4
    0004 3e04          t.asm:4  lab:	mvi a,lab
    0006 01            t.asm:5  db 1 |}]
