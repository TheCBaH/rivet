(* The Melange adapter (.ai/asm_plan.md §11.6).

   The same alias of Driver.Portable that browser/jsoo exposes, for the same
   reasons; see that file for why neither carries a backend binding.

   Note the two trees. This is the adapter *package*; it is mirrored again under
   melange/browser/melange like every other production package, because dune
   cannot make a library's modes conditional (melange/dune explains that at
   length). So the same source is compiled by three stanzas and there is still
   exactly one copy of it. *)

include Driver.Portable
