(* The x86_64 target (.ai/asm_plan.md §5.1).

   The same family functor as x86_32 with a different mode. The extended
   registers exist here and the REX byte does; the default operand size does
   *not* change, which is why [movl $42, %eax] is byte-identical in both modes
   and why the fixtures use it to pin that fact. *)

module Mode = struct
  let name = "x86_64"
  let triple = "x86_64-linux-gnu"
  let address_width = 64
  let rex_allowed = true

  let registers =
    X86_family.base_regs 64 X86_family.names_64
    @ X86_family.extended_regs 64 ""
    @ X86_family.base_regs 32 X86_family.names_32
    @ X86_family.extended_regs 32 "d"
    @ X86_family.base_regs 16 X86_family.names_16
    @ X86_family.base_regs 8 X86_family.names_8l
end

include X86_family.Make (Mode)
