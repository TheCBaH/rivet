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

  (* xmm0-xmm7 only: 32-bit mode has no REX byte, so the REX.R/REX.B
     extension bits that reach xmm8-xmm15 in 64-bit mode do not exist here -
     verified against i686-linux-gnu-as, which rejects %xmm8 outright. The
     SSE2 codec itself (x86_family_encode.ml) is unconditional and already
     produces the identical opcode bytes in both modes (no REX prefix either
     way for xmm0-7); this register-table addition is the only x86_32 change
     the M5 corpus (asm/docs/corpus.md) needs for SSE2 float support.

     8-bit registers are 32-bit mode's OWN spelling ([al]-[bl] then
     [ah]/[ch]/[dh]/[bh]), not {!X86_family_encode.Reg.names_8l} (that array is
     [al]-[bl] then [spl]/[bpl]/[sil]/[dil], the REX-required low-byte forms
     that exist only where a REX prefix does - x86_64's own table, [rex_allowed
     = true]). Without a REX byte, register numbers 4-7 in an 8-bit operand
     ALWAYS mean the legacy high-byte halves, never spl/bpl/sil/dil - checked
     against real i686-linux-gnu-as/objdump: [movb %ah,%al]/[%ch,%al]/
     [%dh,%al]/[%bh,%al] encode ModRM.reg 4/5/6/7 respectively (M5
     classify-c-gcc corpus evidence: [binarytrees.c]'s [%ah]). *)
  let registers =
    X86_family_encode.Reg.base_regs 32 X86_family_encode.Reg.names_32
    @ X86_family_encode.Reg.base_regs 16 X86_family_encode.Reg.names_16
    @ X86_family_encode.Reg.base_regs 8 [| "al"; "cl"; "dl"; "bl"; "ah"; "ch"; "dh"; "bh" |]
    @ X86_family_encode.Reg.base_regs 128 (Array.sub X86_family_encode.Reg.names_xmm 0 8)
    (* [st] (x87 top-of-stack, [%st] alone - [%st(1)]-[%st(7)] are parsed
       directly from the [Register "st"; Lparen; Int n; Rparen] token shape,
       x86_family.ml, and never reach this table). Width 80 is not a real
       operand width (x87 is 80-bit extended precision, but nothing here
       lowers or encodes an x87 instruction yet - M5 classify-c-gcc corpus
       evidence, parse-level only, asm/docs/corpus.md): it exists purely as a
       class marker distinct from 8/16/32/64/128, the same convention
       [names_xmm]'s width 128 already established. *)
    @ [ { X86_family_encode.Reg.name = "st"; num = 0; width = 80 } ]

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

  (* Measured against a real controlled link (i686-linux-gnu-ld 2.44, M3 §3):
     the linker's own merge-gap fill is NOT this mode's [nop_table] - it never
     reaches for the LEA forms above, only the always-safe 2-byte [66 90],
     repeated, with one trailing single-byte [90] for an odd-length gap. A
     13-byte gap measured as six [66 90] pairs plus one [90], matching the
     greedy fill over this exact two-entry table. *)
  let merge_nop_table = [| "\x90"; "\x66\x90" |]
end

include X86_family_encode.Make (Mode)
