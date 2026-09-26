(** The x86 x87 floating-point component.

    The assembler implements a small, corpus-driven subset: ten memory-operand loads, stores and
    arithmetic ops, the register form [fadd %st(i), %st], and the bare [fucomp] and [fnstsw %ax].
    Everything else in x87 is captured by the ISA database but not admitted. Each form's opcode
    fields and codec priority are listed here once; the family builds its codec alternatives from
    these tables, so the descriptor and the encoder cannot disagree. *)

type operand_shape =
  | Memory  (** a memory operand only *)
  | Symbol  (** a bare symbol, read as an absolute address, only *)
  | Memory_or_symbol  (** either *)

type memory_form = {
  mnemonic : string;
  label : string;  (** the codec alternative label *)
  priority : int;  (** the codec alternative priority *)
  opcode_byte : int;  (** [0xD8]/[0xD9]/[0xDD]/[0xDF] *)
  ext : int;  (** the ModR/M-reg opcode extension *)
  shape : operand_shape;
  source : string;  (** the XED iform this form corresponds to *)
}

val memory_forms : memory_form list

type fixed_form = {
  mnemonic : string;
  label : string;
  priority : int;
  word : int64;  (** the whole instruction, big-endian *)
  bits : int;
  source : string;
}

val fixed_forms : fixed_form list
(** Bare instructions with no ModR/M: [fucomp] and [fnstsw %ax]. *)

val fadd_st0 : memory_form
(** [fadd %st(i), %st] ([0xD8 0xC0+i]). It has no memory operand: only [mnemonic], [label],
    [priority] and [source] are meaningful, and [opcode_byte]/[ext] give the fixed [0xD8]/[0]. *)

val find_memory : string -> memory_form option
val owns : string -> bool
val component : Target_component.t
