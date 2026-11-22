(* The x86_32 target (docs/design.md §5.1).

   The mode itself is in {!Rivet_x86_32_encode}, beside the encoder it parameterizes,
   so that a consumer wanting x86-32 encoding without a parser has something to
   name. This module is that mode carried through the family's *front end* as
   well - the TARGET the registry and the pipeline use. *)

include Rivet_x86_family.Make (Rivet_x86_32_encode.Mode)
