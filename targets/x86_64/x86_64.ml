(* The x86_64 target (.ai/asm_plan.md §5.1).

   The mode itself is in {!X86_64_encode}, beside the encoder it parameterizes,
   so that a consumer wanting x86-64 encoding without a parser has something to
   name. This module is that mode carried through the family's *front end* as
   well - the TARGET the registry and the pipeline use. *)

include X86_family.Make (X86_64_encode.Mode)
