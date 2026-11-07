let hex s =
  String.concat " " (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let unhex s =
  String.concat ""
    (List.map
       (fun h -> String.make 1 (Char.chr (int_of_string ("0x" ^ h))))
       (List.filter (( <> ) "") (String.split_on_char ' ' s)))

let render e = Foundation.Diagnostic.render_all (Foundation.Diag.diagnostics e)

let assemble text =
  match I8080_text.assemble text with
  | Ok { I8080_text.origin; bytes } -> Printf.sprintf "%04x: %s" origin (hex bytes)
  | Error e -> "REFUSED " ^ render e

(* One line per instruction: bytes, canonical text, length, class. The 256
   lines were derived from the reviewed opcode table of the Radio-86RK
   contracts, not from this implementation. *)
let fixture () =
  let ic = open_in "opcodes.txt" in
  let rec lines acc =
    match input_line ic with
    | l ->
        lines
          (match String.split_on_char '\t' l with
          | [ b; t; n; c ] -> (b, t, int_of_string n, c) :: acc
          | _ -> acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  lines []

let%expect_test "all 256 opcode bytes decode, print and re-assemble as the fixtures say" =
  let rows = fixture () in
  let failures = ref [] in
  let fail fmt = Printf.ksprintf (fun m -> failures := m :: !failures) fmt in
  List.iter
    (fun (bytes, text, len, _) ->
      let raw = unhex bytes in
      if String.length raw <> len then fail "%s: fixture length %d" bytes len;
      (match I8080_text.canonical ~origin:0 raw with
      | Error e -> fail "%s: decode: %s" bytes (render e)
      | Ok c -> (
          let expect = Printf.sprintf "\torg 0x0000\n\t%s\n" text in
          if c <> expect then fail "%s: printed %S, fixture %S" bytes c expect;
          match I8080_text.assemble c with
          | Ok i ->
              if i.I8080_text.bytes <> raw then
                fail "%s: canonical re-assembles to %s" bytes (hex i.I8080_text.bytes)
          | Error e -> fail "%s: canonical text refused: %s" bytes (render e)));
      match I8080_text.assemble text with
      | Ok i ->
          if i.I8080_text.bytes <> raw then
            fail "%s: %S assembles to %s" bytes text (hex i.I8080_text.bytes)
      | Error e -> fail "%s: %S refused: %s" bytes text (render e))
    rows;
  let firsts = List.sort_uniq compare (List.map (fun (b, _, _, _) -> String.sub b 0 2) rows) in
  Printf.printf "rows=%d distinct-first-bytes=%d documented=%d alias=%d\n" (List.length rows)
    (List.length firsts)
    (List.length (List.filter (fun (_, _, _, c) -> c = "doc") rows))
    (List.length (List.filter (fun (_, _, _, c) -> c = "alias") rows));
  List.iter print_endline (List.rev !failures);
  [%expect {| rows=256 distinct-first-bytes=256 documented=244 alias=12 |}]

let%expect_test "the MVI/STA/HLT fixture assembles to 3e 2a 32 00 20 76" =
  print_endline (assemble "mvi a, 0x2a\nsta 0x2000\nhlt\n");
  print_endline
    (assemble "\torg 0x0000\n\tMVI A,0x2A ; Intel case and comments\n\tSTA 0x2000\n\tHLT\n");
  [%expect {|
    0000: 3e 2a 32 00 20 76
    0000: 3e 2a 32 00 20 76 |}]

let%expect_test "labels, data, origin" =
  print_endline
    (assemble
       "org 0x0100\n\
        start:\n\
        \tlxi h, table\n\
        \tmvi b, 3\n\
        loop:\n\
        \tmov a, m\n\
        \tinx h\n\
        \tdcr b\n\
        \tjnz loop\n\
        \tjmp start\n\
        table:\n\
        \tdb 1, 2, 0xff, \"AB\"\n\
        \tdw 0x1234, start, table\n");
  print_endline (assemble "org 0x0100\nlxi h, 0x1234\nmvi a, -1\nmvi a, 255\n");
  [%expect
    {|
    0100: 21 0e 01 06 03 7e 23 05 c2 05 01 c3 00 01 01 02 ff 41 42 34 12 00 01 0e 01
    0100: 21 34 12 3e ff 3e ff |}]

let%expect_test "canonical text round-trips a program" =
  (match
     I8080_text.assemble
       "org 0x0100\n\
        again:\tlxi sp, 0xf000\n\
        call again\n\
        rst 7\n\
        in 0x10\n\
        out 0x11\n\
        push psw\n\
        db 0xcb, 1, 2\n\
        ret\n"
   with
  | Error e -> print_endline (render e)
  | Ok { I8080_text.origin; bytes } -> (
      match I8080_text.canonical ~origin bytes with
      | Error e -> print_endline (render e)
      | Ok text ->
          print_string text;
          print_endline (assemble text)));
  [%expect
    {|
    	org 0x0100
    	lxi sp, 0xf000
    	call 0x0100
    	rst 7
    	in 0x10
    	out 0x11
    	push psw
    	db 0xcb, 0x01, 0x02
    	ret
    0100: 31 00 f0 cd 00 01 ff db 10 d3 11 f5 cb 01 02 c9 |}]

let%expect_test "diagnostics" =
  List.iter
    (fun src -> Printf.printf "%-22s %s\n" (String.escaped src) (assemble src))
    [
      "foo a";
      "mov m, m";
      "mov a";
      "mvi a, 256";
      "mvi a, -129";
      "lxi sp, 65536";
      "lxi a, 1";
      "push sp";
      "ldax h";
      "rst 8";
      "org 0x10000";
      "nop\norg 0x10";
      "org 1\norg 2";
      "l: org 1";
      "org 0xffff\nnop\nnop";
      "nop\n.align 4\nnop";
      "jmp undefined";
      "mvi a, ";
      "db \"x\", a";
      "dw \"x\"";
    ];
  [%expect
    {|
    foo a                  REFUSED i8080:1:1: error[i8080.simplify]: unknown instruction foo
      foo a
      ^^^
    mov m, m               REFUSED i8080:1:1: error[i8080.simplify]: no mov form takes these operands
      mov m, m
      ^^^
    mov a                  REFUSED i8080:1:1: error[i8080.simplify]: no mov form takes these operands
      mov a
      ^^^
    mvi a, 256             REFUSED <synthesized by i8080.lower from i8080:1:1>: error[i8080.lower]: mvi operand 256 does not fit -128..255
      mvi a, 256
      ^^^
    mvi a, -129            REFUSED <synthesized by i8080.lower from i8080:1:1>: error[i8080.lower]: mvi operand -129 does not fit -128..255
      mvi a, -129
      ^^^
    lxi sp, 65536          REFUSED <synthesized by i8080.lower from i8080:1:1>: error[i8080.lower]: lxi operand 65536 does not fit -32768..65535
      lxi sp, 65536
      ^^^
    lxi a, 1               REFUSED i8080:1:1: error[i8080.simplify]: lxi: operand 1 must be a register pair (b d h sp)
      lxi a, 1
      ^^^
    push sp                REFUSED i8080:1:1: error[i8080.simplify]: push: operand 1 must be a register pair (b d h psw)
      push sp
      ^^^^
    ldax h                 REFUSED i8080:1:1: error[i8080.simplify]: ldax: operand 1 must be b or d
      ldax h
      ^^^^
    rst 8                  REFUSED i8080:1:1: error[i8080.simplify]: rst: operand 1 must be 0..7
      rst 8
      ^^^
    org 0x10000            REFUSED i8080:1:1: error[i8080.lower]: origin 65536 does not fit 0..65535
      org 0x10000
      ^^^
    nop\norg 0x10          REFUSED i8080:2:1: error[i8080.lower]: org must precede every instruction and datum, and appear once
      org 0x10
      ^^^
    org 1\norg 2           REFUSED i8080:2:1: error[i8080.lower]: org must precede every instruction and datum, and appear once
      org 2
      ^^^
    l: org 1               REFUSED i8080:1:4: error[i8080.lower]: org must precede every instruction and datum, and appear once
      l: org 1
         ^^^
    org 0xffff\nnop\nnop   REFUSED <synthesized by i8080>: error[i8080.image]: image of 2 bytes at 0xffff leaves the 64 KiB address space
    nop\n.align 4\nnop     REFUSED i8080:2:1: error[image.align-fill]: the target supplies no 3-byte padding for a 4-byte alignment
      .align 4
      ^^^^^^
    jmp undefined          REFUSED <synthesized by i8080.encode>: error[image.undefined]: fixup imm references undefined symbol undefined
    mvi a,                 REFUSED i8080:1:1: error[i8080.simplify]: no mvi form takes these operands
      mvi a,
      ^^^
    db \"x\", a            REFUSED i8080:1:1: error[i8080.simplify]: db: operand 2 must be a value or string
      db "x", a
      ^^
    dw \"x\"               REFUSED i8080:1:1: error[i8080.simplify]: dw: operand 1 must be a value
      dw "x"
      ^^ |}]

let%expect_test "the codec is consistent" =
  print_endline (String.concat "; " (I8080_text.check_codec ()));
  [%expect {| |}]
