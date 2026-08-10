(* The x86_32 target description (.ai/asm_plan.md §5.1).

   Filled in at M1.3. Dependencies run one way: this package may name codec and
   target_intf, and they may never name it (enforced by
   tools/asm-check-layers.sh). *)

type nothing = |
