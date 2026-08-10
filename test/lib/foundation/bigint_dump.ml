(* Prints the whole Bigint corpus. This is the portable driver: no ppx and no
   assertions, so the same program compiles under native OCaml, js_of_ocaml, and
   Melange, and the three runs are required to emit identical bytes
   (.ai/asm_plan.md §11.6). The expect assertions over this same corpus live in
   test_bigint.ml, which is native-only because ppx_expect is. *)

let () = Foundation_corpus.Bigint_corpus.all ()
