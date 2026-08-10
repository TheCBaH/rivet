(* The typed bidirectional codec EDSL (.ai/asm_plan.md §6).

   Filled in at M0.4 and frozen at M0.5: an inspectable node type parameterized
   over the target's fixup-kind type, [Iso_table] (a finite relation whose
   injectivity, totality and invertibility are statically checkable), [Iso_fun]
   (an opaque escape hatch with no checkable laws), and [Alt] carrying priority,
   an encode-side guard, and a cost. Four interpreters over one tree: encode,
   decode, inspect, check.

   The hard constraint on this package is negative and enforced by
   tools/asm-check-layers.sh: nothing here may name or branch on a target. x86
   vocabulary belongs in targets/x86_family, not in a generic combinator. *)

type nothing = |
