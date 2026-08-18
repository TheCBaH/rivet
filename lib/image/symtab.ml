(* The M3 linker data model (.ai/asm_plan.md §12, "Milestone 3"; the M3
   implementation plan's §1-§2). One input's symbols are not one input's
   business once more than one module links together: a name spelled the
   same in two inputs is not automatically the same symbol, and "declared"
   is not "defined". This module names the four facts symbol collection
   actually produces and keeps them apart, so nothing downstream has to
   reconstruct the distinction from a flatter shape.

   Four record kinds, not one - conflating them would let a directive-only
   declaration ([.globl name] with no label ever defining [name]) falsely
   satisfy an undefined-reference check, since only a [Label_def] fragment
   contributes an address and a fixup reference does not necessarily create a
   symbol record at all.

   This step builds the data model and the reference-resolution rule alone,
   against synthetic inputs in [test_image.ml] - it does not yet touch
   [plan_image] (still single-module, still rejecting [`Multi_module]) or
   [directives.ml]. Wiring this into a real multi-module [plan_image] is the
   M3 plan's step 4; [.weak]/[.local]/[.comm] parsing and the strong/common/
   weak precedence that fills in [link_global_table]'s grouped duplicates are
   step 6. *)

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

(* Comes from [.globl]/[.weak]/[.local]/[.type]/[.size] alone, with no
   address backing it yet - kept apart from [definition] so a declaration
   with no matching label is a distinct, reportable fact
   ([Declared_never_defined]) rather than silently satisfying a reference. *)
type declaration = {
  c_unit : string;
  c_name : string;
  c_binding : Lowered_ast.binding;
  c_visibility : Lowered_ast.visibility;
  c_kind : Directive.sym_kind;
}

(* One occurrence of a name inside a fixup's expression ([Expr.symbols]),
   owned by the input the fixup lives in. *)
type reference = { r_unit : string; r_name : string }

(* A [.comm] claim: size and alignment, no input-section offset. The §7
   resolver folds these into the canonical [.bss] contribution; this module
   only names the fact. *)
type common_decl = { k_unit : string; k_name : string; k_size : int; k_align : int }

(* {1 Identity: input-qualified, not a bare string (§2)}

   The per-module local table answers "what does this input itself see under
   this name" - every definition an input contributes, regardless of
   binding, because a name is locally visible inside its own input whether
   or not it is also exported. The link-global table answers "what may the
   rest of the link see" - only [Global]/[Weak] definitions ever appear
   there, which is what keeps a [Local] definition invisible outside its own
   input by construction rather than by a filter a caller has to remember to
   apply. *)

type local_table = ((string * string) * definition) list
(** Keyed by the qualified [(unit_name, name)] pair. A duplicate local
    definition of the same name in the same input is a [Duplicate_definition]
    the caller checks before building this table (mirroring [image.ml]'s
    existing single-module check, generalized link-wide by the plan's step
    4) - this module does not silently pick one. *)

type link_global_table = (string * definition list) list
(** Keyed by the bare, foreign-visible name. Grouped rather than resolved:
    which of several [Global]/[Weak] definitions of one name wins is the §7
    strong/common/weak precedence, a later step. This step's contract is
    only that the table contains exactly the definitions a foreign input is
    allowed to see - never a [Local] one. *)

let build_local_table (defs : definition list) : local_table =
  List.map (fun d -> ((d.d_unit, d.d_name), d)) defs

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

(* {1 Reference resolution (§2)}

   In this fixed order, and never any other:

   1. an input's own local table - a reference always sees what its own
      input defines, whatever the binding;
   2. otherwise the link-global table;
   3. a [Local] definition in a foreign input is never visible, regardless
      of name - it is absent from the link-global table by construction, so
      this is not a rule [resolve_reference] enforces so much as a fact its
      inputs already guarantee;
   4. duplicate local names across different inputs are legal and do not
      collide, because the local table is keyed by the qualified pair. *)
type resolution =
  | Resolved_local of definition
  | Resolved_global of definition list
      (** more than one element is a live possibility here - resolving that
          down to one winner is §7's job, not this function's. *)
  | Unresolved

let resolve_reference ~(local : local_table) ~(link_global : link_global_table) (r : reference) =
  match List.assoc_opt (r.r_unit, r.r_name) local with
  | Some d -> Resolved_local d
  | None -> (
      match List.assoc_opt r.r_name link_global with
      | Some (_ :: _ as ds) -> Resolved_global ds
      | Some [] | None -> Unresolved)
