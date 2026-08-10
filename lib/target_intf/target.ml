(* The TARGET signature (.ai/asm_plan.md §5.1). Signatures only - this package
   deliberately contains no implementation, so that every concrete target
   depends on a description it cannot influence.

   Filled in at M0.4, with one refinement the fixtures already forced:

     val parse_operands :
       mnemonic:string -> token_slice -> (surface_operand list, Diagnostic.t) result

   replacing §5.1's parse_surface_operand, which cannot parse
   [stp x15, x30, [sp, #-16]!] (the [!] binds to the whole memory operand) or
   decide whether [lsl #0] is an operand or a modifier of the preceding one. *)

type nothing = |
