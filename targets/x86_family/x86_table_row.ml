(* One table-driven x86 form (DEC-X86-TABLE). The rows are generated from the
   captured XED export into X86_table_rows by [compcert_tools isa-table
   x86-emit]; nothing here reads the capture at run time. *)

type rclass = Gpr8 | Gpr16 | Gpr32 | Gpr64 | Xmm | Ymm | Zmm | Mmx | Kmask | St | Tmm

(* Where a register operand is encoded. *)
type field =
  | Modrm_reg  (** ModR/M.reg (with REX.R / VEX.R) *)
  | Modrm_rm  (** ModR/M.rm with mod = 3 (with REX.B / VEX.B) *)
  | Vvvv  (** VEX.vvvv *)
  | Is4  (** bits 7:4 of a trailing immediate byte *)
  | Opcode_low  (** the low three bits of the opcode byte (with REX.B): [bswap], [push] *)

type operand =
  | Reg of { cls : rclass; field : field }
  | Mem of { bits : int }  (** a ModR/M memory operand; [bits] is informational *)
  | Vsib of { cls : rclass }
      (** a VSIB memory operand: SIB always, its index a vector register of class [cls] *)
  | Imm of { bytes : int }  (** an immediate of 1, 2 or 4 bytes *)
  | Fixed_reg of string  (** a register the spelling names but the encoding implies: [%cl] *)
  | One  (** the implied shift count of the D0/D1 forms, spelled [$1] *)
  | Dfv  (** APX CCMP/CTEST's default flags [{dfv=of,sf,zf,cf}], in vvvv *)
  | Rounding of { sae_only : bool }
      (** EVEX embedded rounding ([{rn-sae}]: EVEX.b with the mode in L'L) or, [sae_only],
          [{sae}] (EVEX.b, L'L = 0) *)

type space = Legacy | Vex | Evex | Xop  (** XOP: 8F, the VEX three-byte layout, maps 8-10 *)

type row = {
  mnemonic : string;  (** the AT&T spelling *)
  space : space;
  map : int;  (** 0: one-byte, 1: 0F, 2: 0F 38, 3: 0F 3A *)
  opcode : int;
  prefix : int;  (** 0, or the mandatory 0x66 / 0xF2 / 0xF3 (VEX.pp) *)
  osz : bool;  (** a 0x66 operand-size prefix (16-bit GPR forms), besides any mandatory one *)
  w : int;  (** REX.W / VEX.W: 0 or 1, or -1 when ignored (encoded as 0) *)
  l : int;  (** VEX.L (0, 1) or EVEX.L'L (0, 1, 2), or -1 when ignored (encoded as 0) *)
  disp8n : int;
      (** EVEX's compressed displacement scale N: a displacement that is a multiple of N and
          fits a byte after dividing is stored as disp8 (1 outside EVEX) *)
  digit : int;  (** a fixed ModR/M.reg value, or -1 *)
  rm : int;  (** a fixed ModR/M.rm (register form, no rm operand), or -1 *)
  operands : operand list;  (** in AT&T order *)
  mode : int;  (** 0 in both modes; 64 or 32 when only that mode has the form *)
  evex_p2 : int;
      (** fixed EVEX P2 bits of an APX map-4 row: ND (0x10, a new destination in vvvv) and NF
          (0x04, flags untouched: the [{nf}] pseudo-prefix) *)
  mask : int;
      (** EVEX opmask on the destination: 0 none, 1 [{%kN}] or [{%kN}{z}], 2 [{%kN}] only
          (merging), 3 a [{%kN}] other than k0 required (gathers, scatters) *)
  pseudo : string;
      (** the pseudo-prefix that alone reaches this row: [nf], or [evex] for an APX promotion of
          a legacy instruction (GNU encodes the plain spelling as the legacy one); or empty *)
  no_acc : int list;
      (** operand positions that must not be the accumulator: GNU as encodes that spelling with
          an accumulator-specific form ([xchg %ebx, %eax] is 0x93) *)
  feature : string;
  source : string;  (** the XED iform *)
}

let class_width = function
  | Gpr8 -> 8
  | Gpr16 -> 16
  | Gpr32 -> 32
  | Gpr64 -> 64
  | Xmm -> 128
  | Ymm -> 256
  | Zmm -> 512
  (* not operand widths: class markers apart from the GPRs' (an mm register is 64 bits wide) *)
  | Mmx -> 1064
  | Kmask -> 1001
  (* x87 stack registers, the width marker the register table already gives them *)
  | St -> 80
  (* AMX tile registers *)
  | Tmm -> 1002
