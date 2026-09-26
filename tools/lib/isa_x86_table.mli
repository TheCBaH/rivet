(** The table-driven x86 forms (DEC-X86-TABLE): which captured XED records
    become rows of the assembler's generated
    [asm/targets/x86_family/x86_table_rows.ml], read from each record's
    encoding pattern and operand list.

    One rule serves the emitter ({!Isa_x86_table_emit}), the normalizer
    ({!Isa_norm_xed}, for records its hand-written rules do not cover) and the
    case generator ({!Isa_gen_difficult.x86_table_entries}). A record is a row
    only when its ISA set is on the allowlist and every piece of its pattern
    and every visible operand is understood; anything else stays with the
    hand-written forms or blocked. *)

type rclass = Gpr8 | Gpr16 | Gpr32 | Gpr64 | Gprv | Xmm | Ymm | Zmm | Mmx | Kmask
type field = Modrm_reg | Modrm_rm | Vvvv | Is4 | Opcode_low

type operand =
  | Reg of { cls : rclass; field : field }
  | Mem of { bits : int }
  | Imm of { bytes : int }  (** [bytes = 0]: a [z] immediate, 2 or 4 by operand size *)
  | Fixed_reg of string  (** spelled, implied by the encoding: [%cl] *)

type spec = {
  record_id : string;
  iform : string;
  isa_set : string;
  mnemonic : string;  (** AT&T *)
  space : [ `Legacy | `Vex | `Evex ];
  map : int;
  opcode : int;
  prefix : int;
  osz : bool;
  w : int;
  l : int;
  digit : int;
  operands : operand list;  (** AT&T order *)
  mode : int;  (** 0, or 64 for a 64-bit-only form *)
  disp8n : int;  (** EVEX's disp8*N scale; 1 elsewhere *)
  sized : bool;  (** spelled with an operand-size suffix, added by {!expand} *)
  no_acc : int list;  (** AT&T positions that must not be the accumulator *)
  widths : int list;  (** operand sizes of a width-variable (GPRv) form *)
}

val expand : spec -> spec list
(** The concrete rows: a width-variable integer form's 16/32/64-bit rows, each with its suffix. *)

val canonical : spec -> spec
(** The row the normalized form and first case describe (the 32-bit one when width-variable). *)

val spec_of_record : Isa_source_record.t -> spec option

val form :
  requirement:Isa_norm_model.requirement -> Isa_source_record.t -> spec -> Isa_norm_model.form
(** Operands are named [op0], [op1], ... in AT&T order; registers are spelled with [%],
    immediates with [$], memory as given. *)

val operand_name : int -> string

val twins : spec list -> (string, string) Hashtbl.t
(** Records whose spelling and operand shape repeat an earlier spec's: record id to the earlier
    (reachable) iform. Their rows stay for decoding, but they get no case and are reported as
    needing a pseudo-prefix. *)

val not_in_32bit_mode : Isa_source_record.t -> bool
(** A form the x86-32 export lists but 32-bit mode cannot encode (legacy REX.W, a 64-bit GPR, a
    MODE=2 pattern). *)

val accumulator_positions : Isa_source_record.t list -> spec -> int list
(** The AT&T positions of an (expanded) integer spec where GNU as would use an
    accumulator-specific sibling form instead, so the row must refuse the accumulator there. *)
