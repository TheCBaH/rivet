(* The js_of_ocaml adapter (.ai/asm_plan.md §11.6).

   A thin alias of Driver.Portable, and deliberately free of any js_of_ocaml
   *binding*. The API it exposes is plain OCaml over strings and int64s: nothing
   here mentions a JavaScript value, a typed array, or a runtime stub.

   That is not an omission. §3.7 bans JS runtime files from every production
   package, and tools/asm-check-planted.sh carries a planted js-runtime-stub
   violation specifically to prove the ban is enforced rather than merely
   stated. Whatever glue turns these functions into a JavaScript object -
   [Js.export], an --export list, a bundler entry point - belongs outside the
   production closure, where it is free to depend on whatever it likes.

   The package exists separately from browser/melange because the two backends
   need different library stanzas, not because they need different code; both
   re-export one implementation, so there is no second surface to keep in step
   and the three-build gate is comparing one program. *)

include Driver.Portable
