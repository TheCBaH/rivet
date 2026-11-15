(* Input-qualified symbol identity: a name spelled the same in two inputs is
   not automatically the same symbol, so every definition carries the unit
   that produced it. *)

open Asm_core

(* Comes only from a [Label_def] fragment: qualified identity, binding,
   section, offset. *)
type definition = {
  d_unit : string;
  d_name : string;
  d_binding : Lowered_ast.binding;
  d_section : string;
  d_offset : int;
}

type link_global_table = (string * definition list) list
(** Keyed by the bare, foreign-visible name; only [Global]/[Weak] definitions appear, so a [Local]
    one stays invisible outside its own input. *)

(* Every group is kept strong-first (§7: strong beats common beats weak - a
   resolved common allocation is represented as a [Global]-bound definition
   by the caller that builds it, so "strong-first" alone expresses both
   rules). A successful plan never has more than one [Global] per name - two
   would be [Duplicate_definition], and a resolved common is only ever built
   for a name with no [Global] definition already - so prepending a [Global]
   and appending a [Weak] is enough to guarantee it, without needing to
   re-sort after the fact. *)
let build_link_global_table (defs : definition list) : link_global_table =
  List.fold_left
    (fun tbl d ->
      match d.d_binding with
      | Lowered_ast.Local -> tbl
      | Lowered_ast.Global -> (
          match List.assoc_opt d.d_name tbl with
          | Some ds -> (d.d_name, d :: ds) :: List.remove_assoc d.d_name tbl
          | None -> (d.d_name, [ d ]) :: tbl)
      | Lowered_ast.Weak -> (
          match List.assoc_opt d.d_name tbl with
          | Some ds -> (d.d_name, ds @ [ d ]) :: List.remove_assoc d.d_name tbl
          | None -> (d.d_name, [ d ]) :: tbl))
    [] defs
  |> List.rev
