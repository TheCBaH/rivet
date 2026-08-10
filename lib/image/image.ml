(* Symbols, layout policy, the internal linker, and the §9 image types.

   Filled in at M1.4 (restricted single-module linker) and M4 (portable
   binding). The §9 boundary is load-bearing: plan_image / bind_image
   manipulate pure OCaml values and record permissions as *metadata*. Nothing
   in this package - now or later - allocates pages, changes protection,
   flushes caches, or calls generated code. *)

type nothing = |
