(* The x86_32 target (.ai/asm_plan.md §5.1).

   A mode parameter, not a copy: everything about how x86 encodes lives in
   x86_family, and what this file states is the three things that genuinely
   differ from x86_64 - the register file, the width of an address register, and
   the fact that there is no REX byte.

   That last one is not a simplification. In 32-bit mode the encodings 0x40-0x4f
   are inc/dec, so a REX-shaped alternative in the codec would not merely go
   unused: it would decode [inc %eax] as a prefix on whatever followed. *)

module Mode = struct
  let name = "x86_32"
  let triple = "i686-linux-gnu"
  let address_width = 32
  let rex_allowed = false

  let registers =
    X86_family.base_regs 32 X86_family.names_32
    @ X86_family.base_regs 16 X86_family.names_16
    @ X86_family.base_regs 8 X86_family.names_8l
end

include X86_family.Make (Mode)
