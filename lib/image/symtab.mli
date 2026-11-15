(** Input-qualified symbol identity: a definition belongs to the unit that defined it. *)

open Asm_core

type definition = {
  d_unit : string;
  d_name : string;
  d_binding : Lowered_ast.binding;
  d_section : string;
  d_offset : int;
}

type link_global_table = (string * definition list) list
(** Keyed by the foreign-visible name; never holds a [Local] definition. Each group is strong-first. *)

val build_link_global_table : definition list -> link_global_table
