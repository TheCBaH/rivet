(* Input-qualified symbol identity: a name spelled the same in two inputs is
   not automatically the same symbol, so every definition carries the unit
   that produced it. *)

open Rivet_asm_core

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
  let groups : (string, int * definition list) Hashtbl.t = Hashtbl.create 64 in
  List.iteri
    (fun stamp d ->
      let update f =
        let ds = match Hashtbl.find_opt groups d.d_name with Some (_, ds) -> ds | None -> [] in
        Hashtbl.replace groups d.d_name (stamp, f ds)
      in
      match d.d_binding with
      | Lowered_ast.Local -> ()
      | Lowered_ast.Global -> update (fun ds -> d :: ds)
      | Lowered_ast.Weak -> update (fun ds -> ds @ [ d ]))
    defs;
  Hashtbl.fold (fun name (stamp, ds) acc -> (stamp, (name, ds)) :: acc) groups []
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.map snd

type index = {
  in_unit : (string * string, definition) Hashtbl.t;
  by_name : (string, definition list) Hashtbl.t;
  global : (string, definition list) Hashtbl.t;
}

let index (defs : definition list) =
  let in_unit = Hashtbl.create 256 and by_name = Hashtbl.create 256 in
  List.iter
    (fun d ->
      if not (Hashtbl.mem in_unit (d.d_unit, d.d_name)) then
        Hashtbl.add in_unit (d.d_unit, d.d_name) d;
      Hashtbl.replace by_name d.d_name
        (d :: (match Hashtbl.find_opt by_name d.d_name with Some ds -> ds | None -> [])))
    defs;
  Hashtbl.filter_map_inplace (fun _ ds -> Some (List.rev ds)) by_name;
  let global = Hashtbl.create 256 in
  List.iter (fun (n, ds) -> Hashtbl.replace global n ds) (build_link_global_table defs);
  { in_unit; by_name; global }

let find_in_unit t ~unit_name name = Hashtbl.find_opt t.in_unit (unit_name, name)

let find_global t name =
  match Hashtbl.find_opt t.global name with Some (d :: _) -> Some d | Some [] | None -> None

let is_global t name = Hashtbl.mem t.global name

let find_any t name =
  match Hashtbl.find_opt t.by_name name with Some (d :: _) -> Some d | Some [] | None -> None

let defined t name = Hashtbl.mem t.by_name name
let named t name = match Hashtbl.find_opt t.by_name name with Some ds -> ds | None -> []
