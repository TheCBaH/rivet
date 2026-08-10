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
    X86_family_encode.Reg.base_regs 64 X86_family_encode.Reg.names_64
    @ X86_family_encode.Reg.extended_regs 64 ""
    @ X86_family_encode.Reg.base_regs 32 X86_family_encode.Reg.names_32
    @ X86_family_encode.Reg.extended_regs 32 "d"
    @ X86_family_encode.Reg.base_regs 16 X86_family_encode.Reg.names_16
    @ X86_family_encode.Reg.base_regs 8 X86_family_encode.Reg.names_8l

  (* Measured against x86_64-linux-gnu-as by padding a .align 16 at each
     distance, not copied from a manual: the long-NOP family plus the CS-prefix
     runs GAS uses to reach ten and eleven bytes. Longer gaps are this table's
     last entry followed by the remainder, which is what GAS does too. *)
  let nop_table =
    [|
      "\x90";
      "\x66\x90";
      "\x0f\x1f\x00";
      "\x0f\x1f\x40\x00";
      "\x0f\x1f\x44\x00\x00";
      "\x66\x0f\x1f\x44\x00\x00";
      "\x0f\x1f\x80\x00\x00\x00\x00";
      "\x0f\x1f\x84\x00\x00\x00\x00\x00";
      "\x66\x0f\x1f\x84\x00\x00\x00\x00\x00";
      "\x66\x2e\x0f\x1f\x84\x00\x00\x00\x00\x00";
      "\x66\x66\x2e\x0f\x1f\x84\x00\x00\x00\x00\x00";
    |]
end

include X86_family_encode.Make (Mode)
