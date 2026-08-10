(* Portable functor application and the target registry (.ai/asm_plan.md §3.7).

   Filled in at M0.5. The registry is a pure value, not a mutable table
   populated by module-initialisation side effects: a registry that depended on
   link order would not survive being compiled three ways.

   This is the only package that may name all four targets at once. *)

type nothing = |
