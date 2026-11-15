open Asm_core
module Symtab = Symtab

type layout_policy = {
  entry_symbol : string option;
  exported : string list option;
  max_size : int option;
  externs : string list;
  allow_rwx : bool;
}

val default_policy : layout_policy

type segment_plan = {
  seg_name : string;
  perms : Perms.t;
  alignment : int;
  init_size : int;
  zero_fill : int;
}

type plan = {
  segments : segment_plan list;
  entry : string option;
  exports : string list;
  imports : string list;
  allow_rwx : bool;
  unresolved : string list;
}

type laid_out
(** A planned image: opaque; read it through the accessors below. *)

val definitions : laid_out -> Symtab.definition list

type segment = {
  name : string;
  perms : Perms.t;
  alignment : int;
  address : int64;
  load_address : int64 option;
  bytes : string;
  zero_fill : int;
}

type t = {
  segments : segment list;
  entry : int64 option;
  exports : (string * int64) list;
  symbol_sizes : (string * int64) list;
}

type link_error =
  [ `Align_fill of align_fill
  | `Ambiguous_symbol of string
  | `Common_conflicts_with_local of string
  | `Declared_never_defined of string
  | `Duplicate_definition of string
  | `Entry_undefined of string
  | `Export_undefined of string
  | `Fixup_undefined_symbol of fixup_undefined_symbol
  | `Invalid_module of invalid_module
  | `Duplicate_unit of string
  | `Rwx_segment of string
  | `No_modules
  | `No_section
  | `Nobits_content of string
  | `Pair_ambiguous_head of string
  | `Pair_cross_section of string
  | `Pair_duplicate_head of string
  | `Pair_incompatible_family of string
  | `Pair_missing_head of string
  | `Pair_undefined_anchor of string
  | `Relax_ladder of Lowered_ast.relax_error
  | `Relax_unreachable of relax_unreachable
  | `Section_kind_mismatch of string
  | `Too_large of too_large ]

and invalid_module = { unit_name : string; reason : Lowered_ast.invalid }
and fixup_undefined_symbol = { fixup : string; symbol : string }
and too_large = { size : int; limit : int }
and relax_unreachable = { widest : string; at : int }
and align_fill = { pad : int; boundary : int }

type bind_error =
  [ `Alignment of alignment
  | `Cross_overlay of cross_overlay
  | `Fixup_out_of_range of fixup_out_of_range
  | `Fixup_target_too_wide of string
  | `Fixup_value of fixup_value
  | `Fixup_wraps of string
  | `Load_overlap of overlap
  | `No_address of string
  | `No_import of string
  | `Overflow of overflow
  | `Overlap of overlap
  | `Overlay_twice of string
  | `Overlay_unknown of string
  | `Symbol_size of symbol_size
  | `Symbol_size_too_wide of string
  | `Target_fixup of Foundation.Diagnostic.t ]

and overflow = { segment : string; address : int64; size : int }

and overlap = {
  a_name : string;
  a_at : int64;
  a_size : int;
  b_name : string;
  b_at : int64;
  b_size : int;
}

and alignment = { segment : string; needs : int; address : int64 }
and cross_overlay = { fixup : string; symbol : string; from_segment : string; to_segment : string }
and symbol_size = { symbol : string; reason : Expr.error }
and fixup_value = { fixup : string; reason : Expr.error }
and fixup_out_of_range = { fixup : string; value : int64; range : Lowered_ast.range; at : int }

type error =
  [ `Align_fill of align_fill
  | `Alignment of alignment
  | `Ambiguous_symbol of string
  | `Common_conflicts_with_local of string
  | `Cross_overlay of cross_overlay
  | `Declared_never_defined of string
  | `Duplicate_definition of string
  | `Entry_undefined of string
  | `Export_undefined of string
  | `Fixup_out_of_range of fixup_out_of_range
  | `Fixup_target_too_wide of string
  | `Fixup_undefined_symbol of fixup_undefined_symbol
  | `Invalid_module of invalid_module
  | `Duplicate_unit of string
  | `Rwx_segment of string
  | `Fixup_value of fixup_value
  | `Fixup_wraps of string
  | `Load_overlap of overlap
  | `No_address of string
  | `No_import of string
  | `No_modules
  | `No_section
  | `Nobits_content of string
  | `Overflow of overflow
  | `Overlap of overlap
  | `Overlay_twice of string
  | `Overlay_unknown of string
  | `Pair_ambiguous_head of string
  | `Pair_cross_section of string
  | `Pair_duplicate_head of string
  | `Pair_incompatible_family of string
  | `Pair_missing_head of string
  | `Pair_undefined_anchor of string
  | `Relax_ladder of Lowered_ast.relax_error
  | `Relax_unreachable of relax_unreachable
  | `Section_kind_mismatch of string
  | `Symbol_size of symbol_size
  | `Symbol_size_too_wide of string
  | `Target_fixup of Foundation.Diagnostic.t
  | `Too_large of too_large ]

val pp_link_error : Format.formatter -> link_error -> unit
val pp_bind_error : Format.formatter -> bind_error -> unit
val pp_error : Format.formatter -> error -> unit
val link_error_code : link_error -> string
val bind_error_code : bind_error -> string
val error_code : error -> string
val diag : ?origin:Foundation.Origin.t -> error -> Foundation.Diagnostic.t

val plan_image :
  evaluate:('k -> place:int64 -> target:int64 -> (Int64.t, Foundation.Diagnostic.t) result) ->
  ?fill:(executable:bool -> int -> string) ->
  ?align_fill:(executable:bool -> int -> string option) ->
  ?section_pad:(length:int -> string) ->
  layout_policy ->
  'k Lowered_ast.module_ list ->
  (laid_out, Foundation.Diagnostic.t list) Err.t

val bind_image :
  ?imports:(string * int64) list ->
  ?overlays:string list list ->
  ?load_addresses:(string * int64) list ->
  laid_out ->
  addresses:(string * int64) list ->
  (t, Foundation.Diagnostic.t list) Err.t

val pp_plan : Format.formatter -> plan -> unit
val pp : Format.formatter -> t -> unit

type site = {
  o_section : string;
  o_offset : int;
  o_place : int;
  o_kind_name : string;
  o_family : string;
  o_role : Lowered_ast.fixup_role;
}

type symbolic_ref = {
  site : site;
  symbol : string;
  addend : int64;
  binding : [ `Global | `Local ];
  visibility : Lowered_ast.visibility;
  defined : bool;
  same_section : bool;
}

type non_normalizable = { nn_site : site; nn_expr : string }
type fixup_observation = Symbolic_ref of symbolic_ref | Non_normalizable of non_normalizable

val fixup_observations : laid_out -> fixup_observation list
val pp_observation : Format.formatter -> fixup_observation -> unit
val plan_of : laid_out -> plan
