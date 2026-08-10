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
    X86_family.Reg.base_regs 32 X86_family.Reg.names_32
    @ X86_family.Reg.base_regs 16 X86_family.Reg.names_16
    @ X86_family.Reg.base_regs 8 X86_family.Reg.names_8l

  (* Measured against i686-linux-gnu-as, and *not* the 64-bit table: 32-bit GAS
     pads with [lea] forms because the long NOP is P6+ and the default 32-bit
     target does not assume it. Sharing one table would have been silently wrong
     here in every padded section. *)
  let nop_table =
    [|
      "\x90";
      "\x66\x90";
      "\x8d\x76\x00";
      "\x8d\x74\x26\x00";
      "\x2e\x8d\x74\x26\x00";
      "\x8d\xb6\x00\x00\x00\x00";
      "\x8d\xb4\x26\x00\x00\x00\x00";
      "\x2e\x8d\xb4\x26\x00\x00\x00\x00";
    |]
end

include X86_family.Make (Mode)
