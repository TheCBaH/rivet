type arch = Riscv | X86
type register_class = Riscv_gpr | Riscv_fpr | X86_gpr | X87_st
type bit_run = { field_name : string; field_hi : int; field_lo : int; dest_hi : int; dest_lo : int }

type immediate = {
  width_bits : int;
  signed : bool;
  implicit_low_zero_bits : int;
  nonzero : bool;
  runs : bit_run list;
}

type operand_kind =
  | Register of { class_ : register_class; excluded : string list }
  | Immediate of immediate
  | Memory of { width_bits : int option }
      (** A memory operand whose data width is either fixed by the form or
          deliberately left source-variable.  Addressing syntax is a
          target-specific recipe concern; it is not fabricated from XED's
          [MEM0] placeholder. *)
  | Rounding_mode
  | Implicit_register of { class_ : register_class; native_name : string }

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
  | Req_xlen of int
  | Req_unknown of string

type concreteness = Concrete | Alias_of of string | Expansion_of of string

type syntax_token =
  | Syn_operand of string
  | Syn_literal of string
  | Syn_group of syntax_token list
  | Syn_decorated of string * syntax_token

type syntax_recipe = { dialect : string; mnemonic : string; operands : syntax_token list }
type provenance_label = Upstream | Inferred
type fact = { label : provenance_label; note : string }
type diagnostic = { rule : string; message : string }

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

let rec render_token = function
  | Syn_operand name -> name
  | Syn_literal s -> s
  | Syn_group toks -> String.concat "" (List.map render_token toks)
  | Syn_decorated (prefix, t) -> prefix ^ render_token t

let render_syntax (s : syntax_recipe) =
  match s.operands with
  | [] -> s.mnemonic
  | operands -> s.mnemonic ^ " " ^ String.concat ", " (List.map render_token operands)
