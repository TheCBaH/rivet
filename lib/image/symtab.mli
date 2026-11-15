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

type index
(** Every definition keyed by [(unit, name)], by name and by link-visible name, built once. *)

val index : definition list -> index

val find_in_unit : index -> unit_name:string -> string -> definition option
(** The first definition of the name in that input, whatever its binding. *)

val find_global : index -> string -> definition option
(** The winning link-visible definition. *)

val is_global : index -> string -> bool
val find_any : index -> string -> definition option
val defined : index -> string -> bool

val named : index -> string -> definition list
(** Every definition of the name, in definition order. *)
