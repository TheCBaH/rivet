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

type rclass =
  | Gpr8
  | Gpr16
  | Gpr32
  | Gpr64
  | Gprv
  | Xmm
  | Ymm
  | Zmm
  | Mmx
  | Kmask
  | St
  | Tmm
  | Cr
  | Dr

type field = Modrm_reg | Modrm_rm | Vvvv | Is4 | Opcode_low

type operand =
  | Reg of { cls : rclass; field : field }
  | Mem of { bits : int }
  | Imm of { bytes : int }  (** [bytes = 0]: a [z] immediate, 2 or 4 by operand size *)
  | Fixed_reg of string  (** spelled, implied by the encoding: [%cl] *)
  | Dfv  (** APX CCMP/CTEST's default flags [{dfv=...}], in vvvv *)
  | One  (** the implied count 1 of a shift/rotate D0/D1 form, spelled [$1] *)
  | Rounding of { sae_only : bool }  (** EVEX embedded rounding [{rn-sae}], or [{sae}] *)
  | Vsib of { cls : rclass }  (** a VSIB address: its index a vector register of [cls] *)

type spec = {
  record_id : string;
  iform : string;
  isa_set : string;
  mnemonic : string;  (** AT&T *)
  space : [ `Legacy | `Vex | `Evex | `Xop ];
  map : int;
  opcode : int;
  prefix : int;
  osz : bool;
  w : int;
  l : int;
  digit : int;
  operands : operand list;  (** AT&T order *)
  mode : int;  (** 0, or 64 for a 64-bit-only form *)
  evex_p2 : int;  (** APX map 4: ND (0x10) and NF (0x04) *)
  bcst : int;  (** EVEX broadcast element count N ({1toN}), or 0 *)
  bcst_elem : int;  (** its element bytes *)
  mask : int;  (** EVEX opmask: 0 none, 1 merge or zero, 2 merge only, 3 required *)
  rm : int;  (** a fixed ModR/M.rm of a register-form encoding with no rm operand, or -1 *)
  disp8n : int;  (** EVEX's disp8*N scale; 1 elsewhere *)
  sized : bool;  (** spelled with an operand-size suffix, added by {!expand} *)
  suffix_isa : string;
      (** the ISA set whose suffix convention the spelling follows: the record's own, or for an
          APX promotion its legacy instruction's ({!inherit_suffix_rule}) *)
  pseudo : string;
      (** the pseudo-prefix the row is reached only through: [nf], or [evex] for an APX
          promotion of a legacy instruction; empty otherwise *)
  df64 : bool;  (** DF64(): 64-bit operand size by default in 64-bit mode, no REX.W *)
  direction : string;  (** ["#0x03"] when the iform is XED's in both directions; else empty *)
  no_rex2 : bool;  (** NOREX2=1: no REX2 prefix, so no r16-r31 *)
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

val branch : Isa_source_record.t -> (string * int) option
(** A relative near jcc/jmp/call record: its AT&T mnemonic and displacement width. *)

val branch_form :
  requirement:Isa_norm_model.requirement -> Isa_source_record.t -> Isa_norm_model.form option

val directional_iforms : Isa_source_record.t list -> (string, unit) Hashtbl.t
(** The iforms XED lists with more than one opcode (both directions of a two-register form). *)

val mark_directional : Isa_source_record.t list -> spec list -> spec list
(** Sets [direction] on the specs of {!directional_iforms}. *)

val lookup_key : ?directional:(string, unit) Hashtbl.t -> Isa_source_record.t -> string
(** The iform, told apart for an EVEX embedded-rounding register variant (which XED lists under
    the plain form's iform) by a [#er] suffix, an APX [{nf}] variant by [#nf]. *)

val spec_lookup_key : spec -> string

val twins : spec list -> (string, string) Hashtbl.t
(** Records whose spelling and operand shape repeat an earlier spec's: record id to the earlier
    (reachable) iform. Their rows stay for decoding, but they get no case and are reported as
    needing a pseudo-prefix. *)

val twin_rank : spec list -> (string, int) Hashtbl.t
(** Each twin's position in GNU as's preference order among its same-spelled forms. *)

val reachable_twins : spec list -> (string, string) Hashtbl.t
(** The twins GNU as reaches with a pseudo-prefix: record id to [evex], [vex], [load] or
    [store]. A twin sharing its primary's iform is not listed. *)

val inherit_suffix_rule : Isa_source_record.t list -> spec list -> spec list
(** APX map-4 promotions take their legacy instruction's suffix convention (the same iclass
    outside APX) and need [{evex}]; one with no legacy sibling keeps neither. *)

val not_in_32bit_mode : Isa_source_record.t -> bool
(** A form the x86-32 export lists but 32-bit mode cannot encode (legacy REX.W, a 64-bit GPR, a
    MODE=2 pattern). *)

val accumulator_positions : Isa_source_record.t list -> spec -> int list
(** The AT&T positions of an (expanded) integer spec where GNU as would use an
    accumulator-specific sibling form instead, so the row must refuse the accumulator there. *)
