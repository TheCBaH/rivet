(* Direct lexer tests (.ai/asm_plan.md §4.6, §11.1) for Lexer.scan_string's
   escape decoding.

   Nothing downstream renders a decoded string's *content*: Source_ast.pp
   prints a directive's arguments via Token.slice_text, which reconstructs the
   source *spelling* from spans, not the token payload (see
   Token.slice_text's own comment) - so a wrong decoded byte would pass
   dump-source-ast, dump-tokens, and every corpus/expect test downstream
   unnoticed. This is the one place that actually looks at the payload.

   The octal-escape cases (M5 corpus evidence: asm/fixtures/corpus/c/x86_64,
   aes.c's string-literal table) were checked against the real, installed
   x86_64-linux-gnu-as/objdump (binutils 2.44) before being written down here -
   see lexer.ml's comment on scan_string. *)

let strings text =
  let source = Foundation.Span.source ~name:"<test>" ~contents:text in
  match Asm_syntax.Lexer.tokenize ~profile:X86_64.lexical_profile ~source with
  | Error e -> Format.printf "error: %a@." Asm_syntax.Lexer.pp_error e
  | Ok toks ->
      List.iter
        (fun t ->
          match Asm_syntax.Token.kind t with
          | Asm_syntax.Token.String _ as k -> Format.printf "%a@." Asm_syntax.Token.pp_kind k
          | _ -> ())
        toks

let%expect_test "the real aes.c octal escape run decodes byte-for-byte" =
  strings {|.ascii "i\304\340\330j{\0040\330\315\267\200p\264\305Z\000"|};
  [%expect {| string("i\196\224\216j{\0040\216\205\183\128p\180\197Z\000") |}]

let%expect_test "\\377 is the maximum 3-digit octal escape, no truncation" =
  strings {|.ascii "\377"|};
  [%expect {| string("\255") |}]

let%expect_test "\\400 exceeds a byte and is truncated to its low 8 bits" =
  (* 0o400 = 256; real GNU as assembles this as a single zero byte. *)
  strings {|.ascii "\400"|};
  [%expect {| string("\000") |}]

let%expect_test "an octal escape stops at the first non-octal character" =
  strings {|.ascii "\04x"|};
  [%expect {| string("\004x") |}]

let%expect_test "a single-digit octal escape at end of string" =
  strings {|.ascii "\4"|};
  [%expect {| string("\004") |}]

let%expect_test "\\0 still works as the one-digit zero case" =
  strings {|.ascii "a\0b"|};
  [%expect {| string("a\000b") |}]

let%expect_test "the other named escapes are unaffected" =
  strings {|.ascii "\n\t\r\\\""|};
  [%expect {| string("\n\t\r\\\"") |}]

(* M5 classify-c-gcc corpus evidence (aes.c/sha3.c/siphash24.c): checked
   against real x86_64-linux-gnu-as - [.ascii "a\bc\fd"] assembles to the five
   bytes [61 08 63 0c 64]. *)
let%expect_test "\\b and \\f decode to backspace and form feed" =
  strings {|.ascii "a\bc\fd"|};
  [%expect {| string("a\bc\012d") |}]

let%expect_test "a genuinely unknown escape is still rejected" =
  strings {|.ascii "\8"|};
  [%expect {| error: unknown escape \8 |}]

(* GNU as itself passes an otherwise-unrecognized \X through as the bare
   character X (confirmed by hand: \e and \a assemble as 'e'/'a', not
   ESC/BEL) - \b/\f are corpus-evidenced and added above, but this project
   deliberately does not adopt that general passthrough rule, so an escape
   letter neither named nor octal stays a rejection. *)
let%expect_test "\\e (not corpus-evidenced) is still rejected, not passed through" =
  strings {|.ascii "\e"|};
  [%expect {| error: unknown escape \e |}]
