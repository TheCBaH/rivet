(* Shared x86 encoding machinery: prefixes, REX, opcode maps, ModR/M, SIB,
   displacements and immediates (.ai/asm_plan.md §5.1).

   The only family package. It is built from generic Seq/Iso/Alt combinators,
   and the x86 vocabulary lives here rather than in lib/codec - which is what
   keeps the codec EDSL target-agnostic.

   Filled in at M1.3. The fixtures already pin one selection rule it must
   reproduce: GAS encodes [movl %eax, 0(%esp)] as [89 04 24], picking mod=00
   with no displacement byte for a zero displacement with base esp. *)

type nothing = |
