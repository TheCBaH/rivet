(** Minimum normalized ISA model.

    Deliberately small: this expresses exactly what the frozen
    pilot worked examples need ([sw], [beq], [c.addi], [sh1add], an x87 form,
    and [ADD_GPRv_IMMz]) and nothing else. It is a design test of the
    contract, not a general per-family model - a family this shape cannot
    yet express (vector masks, EVEX broadcast, VEX operands, RISC-V CSR
    operands, ...) is exactly the kind of gap later normalization work is
    for, and must stay visible as such rather than being forced into these
    types. See {!Isa_norm_riscv} and {!Isa_norm_xed} for the source-specific
    normalization rules that produce values of this model's {!form}. *)

type arch = Riscv | X86

type register_class =
  | Riscv_gpr
  | Riscv_fpr
  | X86_gpr
  | X87_st  (** an x87 stack register, ST(0)..ST(7) *)

type bit_run = { field_name : string; field_hi : int; field_lo : int; dest_hi : int; dest_lo : int }
(** A destination bit range [dest_hi..dest_lo] of a reconstructed operand
    value, sourced from bits [field_hi..field_lo] local to one raw
    {!Isa_source_record.field} (i.e. [field_lo]/[field_hi] = 0 is that
    field's own lsb, not the instruction's). Plain concatenation ([sw]'s
    [imm12hi]/[imm12lo]) and a genuine bit permutation ([beq]'s
    [bimm12hi]/[bimm12lo]) are both expressible as one
    [bit_run] per contiguous source/destination run. *)

type immediate = {
  width_bits : int;  (** width of the reconstructed value, not of any one raw field *)
  signed : bool;
  implicit_low_zero_bits : int;
      (** low bits not stored because they are architecturally always zero,
          e.g. 1 for [beq]'s 2-byte-aligned branch offset *)
  nonzero : bool;  (** e.g. [c.addi]'s split immediate excludes zero *)
  runs : bit_run list;  (** highest destination bits first *)
}

type operand_kind =
  | Register of { class_ : register_class; excluded : string list }
      (** [excluded] holds excluded concrete register spellings, e.g. [c.addi]'s [["x0"]] *)
  | Immediate of immediate
  | Memory of { width_bits : int option }
      (** A memory operand.  [None] means the native form's data width is
          variable and this normalized slice does not claim a universal width. *)
  | Rounding_mode
  | Implicit_register of { class_ : register_class; native_name : string }
      (** a fixed, not-independently-selectable operand, e.g. x87's ST0 *)

type role = In | Out | In_out
type operand = { op_name : string; op_kind : operand_kind; role : role; explicit : bool }

type encoding =
  | Riscv_encoding of { width_bits : int; mask : string; value : string }
  | X86_encoding of { space : string; opcode_map : int; opcode : string; pattern : string }

type requirement =
  | Req_all of requirement list
  | Req_any of requirement list
  | Req_not of requirement
  | Req_feature of string
  | Req_mode of { mode : string; equals : bool }
      (** an x86 execution-mode predicate, e.g. [{mode = "mode64"; equals = true}]
          for a form XED's [applicability] restricts to 64-bit mode
          (a mode/XLEN/size predicate) *)
  | Req_xlen of int  (** a RISC-V XLEN predicate, e.g. [Req_xlen 64] for an RV64-only form *)
  | Req_unknown of string  (** an unmapped native feature/extension name; never a positive match *)

type concreteness = Concrete | Alias_of of string | Expansion_of of string

type syntax_token =
  | Syn_operand of string  (** references an {!operand}'s [op_name] *)
  | Syn_literal of string
  | Syn_group of syntax_token list  (** concatenated with no separator, e.g. ["offset(base)"] *)
  | Syn_decorated of string * syntax_token  (** a prefix decorator, e.g. ["$"] or ["%"] *)

type syntax_recipe = { dialect : string; mnemonic : string; operands : syntax_token list }

(** Whether a recorded fact was read verbatim from the source record, or
    supplied by this normalization step's own interpretation (e.g. the RISC-V
    ISA manual's immediate-field layout, which no captured record states). *)
type provenance_label = Upstream | Inferred

type fact = { label : provenance_label; note : string }

type diagnostic = { rule : string; message : string }
(** An unresolved or deliberately-deferred point about one form, per plan
    §3.3's "unknown constructs must be reported, never silently treated as
    unconstrained or dropped." *)

type form = {
  form_id : string;
  arch : arch;
  native_name : string;
  source_record_ids : string list;
  requirement : requirement;
  encoding : encoding;
  operands : operand list;
  syntax : syntax_recipe;
  concreteness : concreteness;
  facts : fact list;
  diagnostics : diagnostic list;
}

val render_syntax : syntax_recipe -> string
(** Render a syntax recipe to one text line, e.g. ["sw value, offset(base)"]
    or ["add $imm, %dest"]. Exists so a worked example's recipe can be
    checked against its own literal text (the [sw] example) rather
    than only inspected structurally; it is not a claim that every rendered
    line is confirmed GAS-accepted syntax - {!diagnostic}s on the owning
    {!form} say which ones are not. *)
