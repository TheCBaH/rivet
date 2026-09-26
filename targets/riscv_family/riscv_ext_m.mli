(** The RISC-V multiply/divide components.

    The architecture splits integer multiplication out of M: Zmmul is the multiply-only subset and
    M includes it and adds division and remainder. Only the forms the assembler implements are
    listed: [mul] and the RV64-only [mulw] (Zmmul), and [remu] (M). The rest of M is captured by
    the ISA database but not admitted, and stays visible as unimplemented rather than being
    described here. *)

type form = {
  mnemonic : string;
  feature : string;  (** ["zmmul"] or ["m"] *)
  opcode : int;  (** major opcode, bits 6..0 *)
  funct3 : int;
  funct7 : int;
  rv64_only : bool;
  source : string;  (** the riscv-opcodes record this form corresponds to *)
}

val forms : form list

val find : string -> form option
(** The form spelled [mnemonic], if either component owns it. *)

val zmmul : Target_component.t
(** Multiply only. Gated by the [zmmul] feature. *)

val m : Target_component.t
(** Adds [remu]. Gated by the [m] feature, which requires [zmmul]. *)
