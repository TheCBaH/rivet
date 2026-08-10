(* The js_of_ocaml adapter (.ai/asm_plan.md §11.6).

   Filled in at M0.5 alongside browser/melange. Both expose the same in-memory
   API as the native driver - M4.5's real-browser gate runs over that same API
   rather than a parallel one, which is what makes it evidence. *)

type nothing = |
