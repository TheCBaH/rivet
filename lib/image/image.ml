(* The internal linker and the portable image contract (docs/design.md §8, §9).

   The production API stops here, and the stopping point is the design rather
   than an omission. Everything below manipulates ordinary pure-OCaml values and
   records intended R/W/X permissions as *metadata*; nothing allocates a page,
   changes protection, flushes a cache, or calls generated code. That is what
   makes the same API work in a browser, where a target virtual address is a
   layout value and not a JavaScript pointer.

   Two steps, not one (§9):

   - [plan_image] lays fragments out, resolves symbols to section-relative
     offsets, and reports sizes, alignments, permissions, the entry identity and
     the exports - everything a host needs to decide *where* to put things.
   - [bind_image] takes the addresses the host chose, validates them against the
     constraints the plan stated, evaluates the expressions that could not be
     evaluated before addresses existed, and returns byte-exact segments.

   Collapsing them would force the assembler to choose addresses, which is
   precisely the decision an embedding host has to make.

   Imports exist only for names the policy lists as [externs], bound to
   host-supplied addresses, and every unsupported condition is an explicit
   rejection rather than a silent partial result. Several inputs are linked in
   one plan: same-named sections merge by name (a PROGBITS/NOBITS mismatch is a
   dedicated rejection, as GNU ld would silently coerce it), and symbols resolve
   across inputs through [Symtab], with strong over common over weak. *)

open Foundation
open Asm_core
module List = Safe_list

let ( @ ) = List.append

(* Re-exported so a caller writes [Image.Symtab], matching every other public
   name here - [symtab.ml] is a separate file only because [image.ml] is
   already long, not because it is a separate public surface. *)
module Symtab = Symtab

(* {1 Layout policy (§8)} *)

type layout_policy = {
  entry_symbol : string option;
  exported : string list option;  (** [None] exports every global symbol *)
  max_size : int option;
  externs : string list;
      (** names no input has to define: a reference to one is an import, bound by [bind_image]'s
          [~imports] *)
  allow_rwx : bool;  (** accept a merged segment that is both writable and executable *)
  group_sections : bool;
      (** merge [.text.*], [.rodata.*], [.data.*], [.bss.*] into their parent section first *)
}

let default_policy =
  {
    entry_symbol = None;
    exported = None;
    max_size = None;
    externs = [];
    allow_rwx = false;
    group_sections = true;
  }

(* {1 What a plan says}

   Sizes and constraints, not addresses: this is the half of the contract the
   host reads before it has decided anything. *)

type segment_plan = {
  seg_name : string;
  perms : Perms.t;
  alignment : int;
  init_size : int;
  zero_fill : int;
}

type plan = {
  target : string;  (** the target every input module was lowered for *)
  segments : segment_plan list;
  entry : string option;
  exports : string list;
  imports : string list;
      (** the policy's externs that some fixup references and no input defines: the names
          [bind_image]'s [~imports] has to supply *)
  allow_rwx : bool;  (** the layout policy allowed writable executable segments *)
  unresolved : string list;
      (** placement-dependent fixups still outstanding: their values await an address. Not
          "symbols we could not find" - a fixup naming a symbol no input defines is rejected
          during planning unless the name is an extern, in which case it is in [imports]. *)
}

(* The laid-out content. Opaque to a caller: it is [plan_image]'s working state,
   handed back to [bind_image] so the two halves cannot be applied to different
   inputs. *)
(* A fixup after layout, with the target's kind erased.

   Erasing is what keeps [laid_out] unparameterized, and therefore keeps
   [Portable]'s architecture-erased types the shape they already are. The
   evaluator is partially applied at plan time, so the one thing the kind was
   needed for travels with the record instead of the kind itself. The
   descriptive strings travel too, because the differential oracle enters
   through the erased boundary and has to classify what it finds there. *)
type resolved_fixup = {
  rf_unit : string;
      (** the owning input's [unit_name] - which local table a reference sees first (§2) *)
  rf_section : string;
  rf_frag_offset : int;  (** the fragment start, section-relative: the PC base is measured here *)
  rf_byte_offset : int;  (** the patch container, section-relative *)
  rf_container : int;
  rf_pc_bias : int;
  rf_slices : Lowered_ast.slice list;
  rf_range : Lowered_ast.range;
  rf_value : Expr.t;
  rf_original_value : Expr.t;
  rf_pairing : Lowered_ast.pairing;
  rf_name : string;
  rf_kind_name : string;
  rf_family : string;
  rf_role : Lowered_ast.fixup_role;
  rf_origin : Origin.t;
  rf_evaluate : place:int64 -> target:int64 -> (int64, Diagnostic.t) result;
}

(* M3 §2: identity is input-qualified, not a bare string. [definitions] is
   every definition symbol collection produced, from every input, still
   distinguishable by [Symtab.definition.d_unit] - an input's own local table
   is exactly "the subset with that [d_unit]". [link_global] is [Symtab]'s
   grouped table, kept strong-first per §7's precedence (a resolved common
   allocation is represented as a [Global] definition, so "strong-first" is
   both "strong beats weak" and "common beats weak" at once) - a successful
   plan's groups still have at most one [Global] apiece, since two would be
   [Duplicate_definition] and a resolved common is only ever built for a name
   with no [Global] definition already. *)
type laid_out = {
  plan : plan;
  contents : (string * string) list;  (** merged section name -> initialized bytes *)
  definitions : Symtab.definition list;
  index : Symtab.index;
  sizes : (string * string * Expr.t * int) list;
      (** owning unit, symbol, size expression, offset where it was written *)
  fixups : resolved_fixup list;
  symbols : (string * Lowered_ast.symbol) list;  (** owning unit, symbol *)
  weak_names : string list;
      (** every name declared [Weak] by some input, defined or not (§7) - what lets
          [bind_image]'s [address_of] tell "undefined weak, substitute 0" from "a real import,
          reject", the same distinction [plan_image]'s [Fixup_undefined_symbol] check makes. *)
}

(* The lookup order §2 states: a reference always sees its own input's
   definitions first (whatever their binding), and otherwise the definitions
   every other input is allowed to see. Shared by [plan_image] (before
   [laid_out] exists), [bind_image] and [fixup_observations] (after), so the
   rule is stated once. *)
let resolve_symbol ~(index : Symtab.index) ~from_unit name =
  match Symtab.find_in_unit index ~unit_name:from_unit name with
  | Some d -> Some d
  | None -> Symtab.find_global index name

(* {1 The bound image (§9)} *)

type segment = {
  name : string;
  perms : Perms.t;
  alignment : int;
  address : int64;
  load_address : int64 option;
      (** where the initialized bytes are stored, when not at [address]: an ELF [p_paddr] *)
  bytes : string;
  zero_fill : int;
}

type t = {
  segments : segment list;
  entry : int64 option;
  exports : (string * int64) list;
  symbol_sizes : (string * int64) list;
}

(* The linker's error domain (docs/errors.md), in two halves because the two
   phases fail for unrelated reasons: [plan_image] is about a module that cannot
   be laid out, [bind_image] about addresses that do not satisfy the plan it
   produced. Splitting them is what lets a host tell "this program is wrong"
   from "these addresses are wrong", which is why §9 has two steps at all.

   [`Relax_ladder], [`Symbol_size] and [`Fixup_value] wrap their causes, so a
   caller reaches the expression or ladder failure underneath rather than the
   sentence about it.

   One constructor stays rendered, and it is a real erasure rather than a
   shortcut: the fixup evaluator a [laid_out] carries is target-supplied, and
   this module is generic while the architecture-erased [DRIVER] hands a
   [laid_out] around - so neither can be parameterized by a target's error type.
   [`Target_fixup] therefore holds a [Diagnostic.t], is the only constructor
   here that does, and keeps the code the target chose. *)
type link_error =
  [ `No_modules
  | `No_section
  | `Section_kind_mismatch of string
    (** M3 §4: a PROGBITS contribution and a NOBITS contribution share this section name.
          Measured GNU behavior (docs/m3-evidence) under this project's own controlled-link
          shape is to merge and coerce them to one type instead of rejecting - unsafe for the
          NOBITS model M3 builds, so this diverges from GNU on purpose. *)
  | `Invalid_module of invalid_module
  | `Duplicate_unit of string
  | `Mixed_targets of string list
  | `Rwx_segment of string
  | `Duplicate_definition of string
  | `Common_conflicts_with_local of string
    (** M3 §7: a [.comm] declaration and an ordinary local definition of the same name in the
          same input - detectable within one input alone, distinct from the cross-input
          strong/common/weak precedence [Duplicate_definition] and the resolver otherwise
          arbitrate. *)
  | `Declared_never_defined of string
  | `Fixup_undefined_symbol of fixup_undefined_symbol
  | `Export_undefined of string
  | `Entry_undefined of string
  | `Ambiguous_symbol of string
  | `Too_large of too_large
  | `Relax_ladder of Lowered_ast.relax_error
  | `Relax_unreachable of relax_unreachable
  | `Align_fill of align_fill
  | `Nobits_content of string
    (** M3 §6: a NOBITS section can hold labels and [.zero]/[.space] reservations, never real
          bytes - initialized data or a fixup-bearing instruction inside one is a validation
          error, not a silent drop, naming the section it was found in. *)
  | `Pair_undefined_anchor of string
  | `Pair_cross_section of string
  | `Pair_missing_head of string
  | `Pair_ambiguous_head of string
  | `Pair_incompatible_family of string
  | `Pair_duplicate_head of string ]

and invalid_module = { unit_name : string; reason : Lowered_ast.invalid }
and fixup_undefined_symbol = { fixup : string; symbol : string }
and too_large = { size : int; limit : int }
and relax_unreachable = { widest : string; at : int }
and align_fill = { pad : int; boundary : int }

type bind_error =
  [ `Overflow of overflow
  | `Overlap of overlap
  | `Load_overlap of overlap
  | `No_address of string
  | `No_import of string
  | `Overlay_unknown of string
  | `Overlay_twice of string
  | `Cross_overlay of cross_overlay
  | `Alignment of alignment
  | `Symbol_size of symbol_size
  | `Symbol_size_too_wide of string
  | `Fixup_value of fixup_value
  | `Fixup_target_too_wide of string
  | `Fixup_wraps of string
  | `Fixup_out_of_range of fixup_out_of_range
  | `Target_fixup of Diagnostic.t ]

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

type error = [ link_error | bind_error ]

let pp_link_error ppf : link_error -> unit = function
  | `No_modules -> Fmt.string ppf "no lowered modules"
  | `No_section -> Fmt.string ppf "no module defines an allocatable section"
  | `Section_kind_mismatch n ->
      Fmt.pf ppf "section %s is PROGBITS in one input and NOBITS in another" n
  | `Invalid_module { unit_name; reason } ->
      Fmt.pf ppf "invalid module %s: %a" unit_name Lowered_ast.pp_invalid reason
  | `Duplicate_unit n -> Fmt.pf ppf "unit name %s is used by more than one module" n
  | `Mixed_targets ts ->
      Fmt.pf ppf "modules were lowered for different targets: %s" (String.concat ", " ts)
  | `Rwx_segment n ->
      Fmt.pf ppf
        "section %s is both writable and executable and the layout policy does not allow it" n
  | `Duplicate_definition n -> Fmt.pf ppf "duplicate definition of %s" n
  | `Common_conflicts_with_local n ->
      Fmt.pf ppf "common declaration of %s conflicts with a local definition in the same input" n
  | `Declared_never_defined n -> Fmt.pf ppf "symbol %s is declared but never defined" n
  | `Fixup_undefined_symbol { fixup; symbol } ->
      Fmt.pf ppf "fixup %s references undefined symbol %s" fixup symbol
  | `Export_undefined n -> Fmt.pf ppf "exported symbol %s is not defined" n
  | `Entry_undefined n -> Fmt.pf ppf "entry symbol %s is not defined" n
  | `Ambiguous_symbol n ->
      Fmt.pf ppf "%s names a non-global symbol defined by more than one input" n
  | `Too_large { size; limit } -> Fmt.pf ppf "image is %d bytes, limit is %d" size limit
  | `Relax_ladder e -> Lowered_ast.pp_relax_error ppf e
  | `Relax_unreachable { widest; at } ->
      Fmt.pf ppf
        "no encoding of this instruction reaches its target: widest form %s still out of range at \
         section offset %d"
        widest at
  | `Align_fill { pad; boundary } ->
      Fmt.pf ppf "the target supplies no %d-byte padding for a %d-byte alignment" pad boundary
  | `Nobits_content s -> Fmt.pf ppf "section %s is NOBITS and cannot hold initialized content" s
  | `Pair_undefined_anchor s -> Fmt.pf ppf "paired fixup anchor %s is undefined" s
  | `Pair_cross_section s -> Fmt.pf ppf "paired fixup anchor %s is in another section" s
  | `Pair_missing_head s -> Fmt.pf ppf "paired fixup %s has no matching head" s
  | `Pair_ambiguous_head s -> Fmt.pf ppf "paired fixup %s has multiple matching heads" s
  | `Pair_incompatible_family s -> Fmt.pf ppf "paired fixup %s has an incompatible head family" s
  | `Pair_duplicate_head s -> Fmt.pf ppf "duplicate paired-fixup head %s" s

let pp_bind_error ppf : bind_error -> unit = function
  | `Overflow { segment; address; size } ->
      Fmt.pf ppf "segment %s at 0x%Lx overflows with size %d" segment address size
  | `Overlap { a_name; a_at; a_size; b_name; b_at; b_size } ->
      Fmt.pf ppf "segments %s at 0x%Lx (%d bytes) and %s at 0x%Lx (%d bytes) overlap" a_name a_at
        a_size b_name b_at b_size
  | `Load_overlap { a_name; a_at; a_size; b_name; b_at; b_size } ->
      Fmt.pf ppf "segment %s stored at 0x%Lx (%d bytes) overlaps %s at 0x%Lx (%d bytes)" a_name a_at
        a_size b_name b_at b_size
  | `No_address s -> Fmt.pf ppf "no address for segment %s" s
  | `No_import n -> Fmt.pf ppf "no address for imported symbol %s" n
  | `Overlay_unknown s -> Fmt.pf ppf "overlays name no segment %s" s
  | `Overlay_twice s -> Fmt.pf ppf "segment %s is in more than one overlay" s
  | `Cross_overlay { fixup; symbol; from_segment; to_segment } ->
      Fmt.pf ppf "fixup %s in %s references %s in %s, another overlay" fixup from_segment symbol
        to_segment
  | `Alignment { segment; needs; address } ->
      Fmt.pf ppf "segment %s needs %d-byte alignment, address is 0x%Lx" segment needs address
  | `Symbol_size { symbol; reason } -> Fmt.pf ppf "size of %s: %a" symbol Expr.pp_error reason
  | `Symbol_size_too_wide symbol -> Fmt.pf ppf "size of %s does not fit 64 bits" symbol
  | `Fixup_value { fixup; reason } -> Fmt.pf ppf "fixup %s: %a" fixup Expr.pp_error reason
  | `Fixup_target_too_wide f -> Fmt.pf ppf "fixup %s: target does not fit 64 bits" f
  | `Fixup_wraps f ->
      Fmt.pf ppf "fixup %s: the displacement wraps around the end of the address space" f
  | `Fixup_out_of_range { fixup; value; range; at } ->
      Fmt.pf ppf "fixup %s: value %Ld does not fit %a at section offset %d" fixup value
        Lowered_ast.pp_range range at
  (* Already a diagnostic when it arrived: the target rendered it at the erasure
     boundary, so this arm reproduces the message rather than composing one. *)
  | `Target_fixup d -> Fmt.string ppf (Diagnostic.message d)

let pp_error ppf : error -> unit = function
  | #link_error as e -> pp_link_error ppf e
  | #bind_error as e -> pp_bind_error ppf e

let link_error_code : link_error -> string = function
  | `No_modules -> "image.no-modules"
  | `No_section -> "image.no-section"
  | `Section_kind_mismatch _ -> "image.kind-mismatch"
  | `Invalid_module _ -> "image.invalid-module"
  | `Duplicate_unit _ -> "image.duplicate-unit"
  | `Mixed_targets _ -> "image.mixed-targets"
  | `Rwx_segment _ -> "image.rwx-segment"
  | `Duplicate_definition _ -> "image.duplicate"
  | `Common_conflicts_with_local _ -> "image.common-conflicts-local"
  | `Declared_never_defined _ | `Fixup_undefined_symbol _ -> "image.undefined"
  | `Export_undefined _ -> "image.export"
  | `Entry_undefined _ -> "image.entry"
  | `Ambiguous_symbol _ -> "image.ambiguous"
  | `Too_large _ -> "image.too-large"
  | `Relax_ladder _ -> "image.relax-ladder"
  | `Relax_unreachable _ -> "image.relax-unreachable"
  | `Align_fill _ -> "image.align-fill"
  | `Nobits_content _ -> "image.nobits-content"
  | `Pair_undefined_anchor _ -> "image.pair-undefined-anchor"
  | `Pair_cross_section _ -> "image.pair-cross-section"
  | `Pair_missing_head _ -> "image.pair-missing-head"
  | `Pair_ambiguous_head _ -> "image.pair-ambiguous-head"
  | `Pair_incompatible_family _ -> "image.pair-family"
  | `Pair_duplicate_head _ -> "image.pair-duplicate-head"

let bind_error_code : bind_error -> string = function
  | `Overflow _ -> "bind.overflow"
  | `Overlap _ -> "bind.overlap"
  | `Load_overlap _ -> "bind.load-overlap"
  | `No_address _ -> "bind.address"
  | `No_import _ -> "bind.import"
  | `Overlay_unknown _ | `Overlay_twice _ -> "bind.overlay"
  | `Cross_overlay _ -> "bind.cross-overlay"
  | `Alignment _ -> "bind.alignment"
  | `Symbol_size _ | `Symbol_size_too_wide _ -> "bind.size"
  | `Fixup_value _ | `Fixup_target_too_wide _ | `Fixup_wraps _ -> "bind.fixup"
  | `Fixup_out_of_range _ -> "bind.fixup-range"
  (* The target named it; re-coding here would overwrite what only the target
     could know (docs/errors.md §2, delegation). *)
  | `Target_fixup d -> Diagnostic.code d

let error_code : error -> string = function
  | #link_error as e -> link_error_code e
  | #bind_error as e -> bind_error_code e

let diag ?origin (e : error) =
  Diagnostic.of_error ~code:error_code ~pp:pp_error
    ~origin:(match origin with Some o -> o | None -> Origin.synthesized ~pass:"image" ())
    e

(* {1 Laying fragments out}

   Two phases now, because relaxation exists. Sizes are not all known up front:
   a branch encoded short or near depends on where its target lands, which
   depends on the sizes. Phase one reaches a fixpoint on that; phase two emits
   bytes for the selection it settled on.

   Alignment still only moves a fragment forward, but it is recomputed on every
   iteration rather than assumed, because a promotion upstream changes padding
   downstream. *)

let align_up n boundary = if boundary <= 1 then n else (n + boundary - 1) / boundary * boundary

(* The chosen alternative of each [Relax] fragment, by position in the section.
   Selections only ever move up their ladder, which is what makes the fixpoint
   terminate. *)
type selection = int array

let alt_bytes (a : 'k Lowered_ast.encoded_form) = String.length a.Lowered_ast.bytes

let fragment_size (sel : selection) i frag at =
  match frag with
  | Lowered_ast.Bytes { bytes; _ } -> String.length bytes
  | Lowered_ast.Relax { alts; _ } -> alt_bytes (List.nth alts sel.(i))
  | Lowered_ast.Zero { length; _ } -> length
  | Lowered_ast.Align { boundary; _ } -> align_up at boundary - at
  | Lowered_ast.Label_def _ | Lowered_ast.Set_size _ -> 0

(* Offsets of every fragment and every label under one selection. *)
let scan_offsets (sel : selection) frags =
  let at = ref 0 in
  let starts = Array.make (List.length frags) 0 in
  let labels = Hashtbl.create 64 in
  List.iteri
    (fun i frag ->
      let size = fragment_size sel i frag !at in
      (match frag with
      | Lowered_ast.Align _ -> at := !at + size
      | Lowered_ast.Label_def { name; _ } ->
          starts.(i) <- !at;
          if not (Hashtbl.mem labels name) then Hashtbl.add labels name !at
      | _ ->
          starts.(i) <- !at;
          at := !at + size);
      if match frag with Lowered_ast.Align _ -> true | _ -> false then starts.(i) <- !at)
    frags;
  (starts, labels, !at)

(* Can this alternative's fixups all reach, with the section laid out as it
   currently is?

   [None] means undecidable here, not "no": a reference to another section has
   no section-relative answer, and its displacement depends on a segment base
   nobody has chosen yet. Undecidable keeps the longest rung, which is always
   safe. Decidability is exactly the intra-section rule - PC-relative distances
   inside one section are independent of where the section is placed, which is
   why relaxation can run at plan time at all. *)
let alt_reaches ~evaluate ~labels ~frag_off (a : 'k Lowered_ast.encoded_form) =
  (* Being *evaluable* against this section's labels is not the same as being
     *in* this section, and the difference is the whole of the intra-section
     rule. [Expr.absolute] happily reduces [Const 0x400078] to a number, and
     comparing that number against a section-relative [place] silently assumes
     the section base is zero: the rung would be chosen for a displacement that
     no longer holds once [bind_image] picks a real base, and by then promotion
     is over.

     So decide it by measurement rather than by inspecting the expression's
     shape. Evaluate the target twice - once with the section at 0, once with it
     at a probe base, moving every label and [here] together - and keep the
     answer only if the target moved with the section. That is exactly "the
     target is a location in this section": a constant does not move (coefficient
     0), nor does a label difference, and a reference to a section we did not lay
     out here does not evaluate at all. Only a coefficient of 1 cancels against
     [place] to leave a base-independent displacement.

     The probe is arbitrary and only its difference is read, so no expression can
     collude with it; it is offset well past any plausible section size so that a
     coincidental match cannot come from small-integer arithmetic. *)
  let probe = 0x1000_0000 in
  (* [here] is the fragment address, matching [bind_image]: [.] is the
     instruction, not the PC base. Getting it wrong here would also make the
     probe misjudge which rung reaches. *)
  let env_for ~base (_ : 'k Lowered_ast.fixup) =
    {
      Expr.lookup =
        (fun n -> Option.map (fun o -> Bigint.of_int (o + base)) (Hashtbl.find_opt labels n));
      here = Some (Bigint.of_int (frag_off + base));
    }
  in
  let at ~base f =
    match Expr.absolute (env_for ~base f) f.Lowered_ast.value with
    | Error _ -> None
    | Ok target -> Bigint.to_int64_opt target
  in
  List.fold_left
    (fun acc (f : 'k Lowered_ast.fixup) ->
      match acc with
      | Some false | None -> acc
      | Some true -> (
          match (at ~base:0 f, at ~base:probe f) with
          | Some t0, Some t1 when Int64.equal (Int64.sub t1 t0) (Int64.of_int probe) -> (
              let place = Int64.of_int (frag_off + f.Lowered_ast.pc_bias) in
              match evaluate f.Lowered_ast.kind ~place ~target:t0 with
              | Error _ -> Some false
              | Ok v -> Some (Lowered_ast.range_admits f.Lowered_ast.range v))
          | _ -> None))
    (Some true) a.Lowered_ast.fixups

(* Promotion-only fixpoint.

   Start every ladder at its shortest rung, lay the section out, and promote any
   rung that does not reach to the shortest one that does - or to the longest if
   none does. Repeat until nothing moves.

   Selections only travel one way along a finite ladder, so the state is
   monotone in a finite product order and this terminates in at most the sum of
   the ladder lengths; exceeding that bound is a bug and raises. On exit every selection reaches, or [verify] reports the
   one that does not.

   It deliberately does not claim minimality. Alignment padding makes layout
   non-monotone in fragment size - promoting one fragment can shift others
   either way - so "smallest feasible selection" is not something this computes
   or that the plan asserts. What establishes that our choices match GNU is the
   byte-for-byte differential gate on each fixture. *)
let relax_fixpoint ~evaluate frags =
  let n = List.length frags in
  let sel = Array.make n 0 in
  let ladders =
    Array.of_list
      (List.map (function Lowered_ast.Relax { alts; _ } -> Array.of_list alts | _ -> [||]) frags)
  in
  let bound = Array.fold_left (fun acc l -> acc + max 0 (Array.length l - 1)) 0 ladders + 1 in
  let rec iterate steps =
    if steps > bound then invalid_arg "Image.relax_fixpoint: promotions exceed the ladder bound"
    else
      let starts, labels, _ = scan_offsets sel frags in
      let moved = ref false in
      Array.iteri
        (fun i ladder ->
          if Array.length ladder > 0 then
            let frag_off = starts.(i) in
            let reaches j = alt_reaches ~evaluate ~labels ~frag_off ladder.(j) in
            if reaches sel.(i) <> Some true then begin
              let target =
                let rec first j =
                  if j >= Array.length ladder then Array.length ladder - 1
                  else if reaches j = Some true then j
                  else first (j + 1)
                in
                first sel.(i)
              in
              if target > sel.(i) then begin
                sel.(i) <- target;
                moved := true
              end
            end)
        ladders;
      if !moved then iterate (steps + 1)
  in
  iterate 0;
  sel

(* NOBITS-ness is a section-level property: a section's [kind] is fixed before any of its
   fragments is laid out. A PROGBITS section's fragments all produce bytes, so [Buffer.length buf]
   equals the logical offset throughout and equals [frag_off], the position [scan_offsets]
   computed. A NOBITS section writes no byte: [buf] stays empty and every reported offset comes
   from [frag_off]. [logical_size] is the address-space extent ([init_size + zero_fill]): the
   byte length for PROGBITS, the whole reservation for NOBITS. *)
let layout_section ~evaluate ~align_fill (sc : 'k Lowered_ast.section_content) =
  let frags = sc.Lowered_ast.fragments in
  let sel = relax_fixpoint ~evaluate frags in
  let starts, labels, logical_size = scan_offsets sel frags in
  let nobits = sc.Lowered_ast.sec.Lowered_ast.kind = Lowered_ast.Nobits in
  let executable = Perms.executable sc.Lowered_ast.sec.Lowered_ast.perms in
  let sec_name = sc.Lowered_ast.sec.Lowered_ast.sec_name in
  let buf = Buffer.create 256 in
  let offsets = ref [] in
  let sizes = ref [] in
  let fixups = ref [] in
  let errors = ref [] in
  let emit_bytes ~frag_off ~form:_ bytes (fs : 'k Lowered_ast.fixup list) origin =
    Buffer.add_string buf bytes;
    List.iter
      (fun (f : 'k Lowered_ast.fixup) ->
        ignore origin;
        fixups := (frag_off, f) :: !fixups)
      fs
  in
  List.iteri
    (fun i frag ->
      let frag_off = starts.(i) in
      match frag with
      | Lowered_ast.Bytes { bytes; fixups = fs; origin; form } ->
          if nobits && (String.length bytes > 0 || fs <> []) then
            errors := diag ~origin (`Nobits_content sec_name) :: !errors
          else if not nobits then emit_bytes ~frag_off ~form bytes fs origin
      | Lowered_ast.Relax { alts; origin } -> (
          if nobits then errors := diag ~origin (`Nobits_content sec_name) :: !errors
          else
            match Lowered_ast.validate_relax alts with
            | Error m -> errors := diag ~origin (`Relax_ladder (Err.Error.kind m)) :: !errors
            | Ok () ->
                let a = List.nth alts sel.(i) in
                (* The fixpoint promotes; if even the longest rung cannot reach,
                   that is a program that cannot be laid out, and saying so is
                   the whole point of verifying after convergence. *)
                (match alt_reaches ~evaluate ~labels ~frag_off a with
                | Some false ->
                    errors :=
                      diag ~origin
                        (`Relax_unreachable { widest = a.Lowered_ast.form; at = frag_off })
                      :: !errors
                | Some true | None -> ());
                emit_bytes ~frag_off ~form:(Some a.Lowered_ast.form) a.Lowered_ast.bytes
                  a.Lowered_ast.fixups origin)
      | Lowered_ast.Align { boundary; origin } ->
          (* NOBITS: [scan_offsets] already folded this padding into every
             later fragment's [frag_off] - nothing to write. *)
          if not nobits then begin
            let target = align_up (Buffer.length buf) boundary in
            let pad = target - Buffer.length buf in
            (* The padding is asked for at the one gap size that occurs. A
               shorter pattern repeated would be the same number of bytes and
               a different program: x86 pads with a single no-op sized for the
               gap, so ten bytes is one ten-byte instruction and not two nops
               that add up. *)
            if pad > 0 then
              match align_fill ~executable pad with
              | Some b when String.length b = pad -> Buffer.add_string buf b
              | _ -> errors := diag ~origin (`Align_fill { pad; boundary }) :: !errors
          end
      | Lowered_ast.Zero { length; _ } ->
          if not nobits then Buffer.add_string buf (String.make length '\000')
      | Lowered_ast.Label_def { name; _ } -> offsets := (name, frag_off) :: !offsets
      | Lowered_ast.Set_size { name; size; _ } -> sizes := (name, size, frag_off) :: !sizes)
    frags;
  ( Buffer.contents buf,
    List.rev !offsets,
    List.rev !sizes,
    List.rev !fixups,
    List.rev !errors,
    logical_size )

(* {1 plan_image}

   Every unsupported condition is an explicit rejection rather than a silent
   partial result. M3 (docs/design.md §12) removes the one-module limit:
   sections merge across every input by name alone (§4), and symbols resolve
   across every input through [Symtab] (§2) instead of per-module. *)

(* M3 §5: only x86_32/x86_64 need a real callback here - a merge-boundary gap
   fills with real NOP instructions on those two profiles alone, and only for
   an executable section (docs/m3-evidence). Zero is the measured,
   correct answer everywhere else, including every non-executable gap on
   every target, which is why it is the default rather than one target's
   special case. *)
let default_fill ~executable:_ n = String.make n '\000'

(* The padding of an [Align] gap of [n] bytes, or [None] when the target has none of that
   length. Zero everywhere unless the caller (a target's pipeline) says otherwise. *)
let default_align_fill ~executable:_ n = Some (String.make n '\000')

let invalid_modules modules =
  let rec dups seen = function
    | [] -> []
    | (m : 'k Lowered_ast.module_) :: rest ->
        let n = m.Lowered_ast.unit_name in
        if List.mem n seen then `Duplicate_unit n :: dups seen rest else dups (n :: seen) rest
  in
  let mixed =
    match List.sort_uniq compare (List.map (fun m -> m.Lowered_ast.target) modules) with
    | [] | [ _ ] -> []
    | ts -> [ `Mixed_targets ts ]
  in
  dups [] modules @ mixed
  @ List.concat_map
      (fun (m : 'k Lowered_ast.module_) ->
        match Lowered_ast.validate m with
        | Ok () -> []
        | Error rs ->
            List.map
              (fun reason -> `Invalid_module { unit_name = m.Lowered_ast.unit_name; reason })
              rs)
      modules

let first_symbols (m : 'k Lowered_ast.module_) =
  let t = Hashtbl.create 64 in
  List.iter
    (fun (s : Lowered_ast.symbol) ->
      if not (Hashtbl.mem t s.Lowered_ast.name) then Hashtbl.add t s.Lowered_ast.name s)
    m.Lowered_ast.symbols;
  t

let string_set names =
  let t = Hashtbl.create 64 in
  List.iter (fun n -> Hashtbl.replace t n ()) names;
  t

(* {2 Common-symbol resolution (§7), before any section is laid out}

   Whether a [.comm] declaration ever contributes storage is a link-wide
   fact ("does some input define this name with a real, strong
   definition") that does not depend on offsets or addresses, so it can
   be decided from the raw modules alone, before merging exists. Every
   winning declaration becomes one extra, synthetic contribution to the
   canonical [.bss] group - built from the same fragment vocabulary
   (`Align`/`Label_def`/[Zero]) everything else uses, so it flows
   through the ordinary merge/layout machinery below rather than
   needing its own. *)
let resolve_commons ~(fail : ?origin:Origin.t -> error -> unit)
    (modules : 'k Lowered_ast.module_ list) =
  let module_symbols = List.map (fun m -> (m, first_symbols m)) modules in
  let strong_defined_names = Hashtbl.create 64 in
  List.iter
    (fun ((m : 'k Lowered_ast.module_), symbols) ->
      List.iter
        (fun (sc : 'k Lowered_ast.section_content) ->
          List.iter
            (function
              | Lowered_ast.Label_def { name; _ } -> (
                  match Hashtbl.find_opt symbols name with
                  | Some { Lowered_ast.binding = Lowered_ast.Global; _ } ->
                      Hashtbl.replace strong_defined_names name ()
                  | _ -> ())
              | _ -> ())
            sc.Lowered_ast.fragments)
        m.Lowered_ast.sections)
    module_symbols;
  let all_commons =
    List.concat_map (fun (m : 'k Lowered_ast.module_) -> m.Lowered_ast.commons) modules
  in
  let common_names =
    let seen = Hashtbl.create 16 in
    List.filter_map
      (fun (c : Lowered_ast.common) ->
        let n = c.Lowered_ast.comm_name in
        if Hashtbl.mem seen n then None
        else begin
          Hashtbl.add seen n ();
          Some n
        end)
      all_commons
  in
  (* A common declaration conflicting with an ordinary *local* definition
         of the same name in the *same* input is its own diagnostic,
         detectable within one input alone - distinct from the cross-input
         strong/common/weak precedence below, and checked first because it is
         about one input being self-contradictory rather than about how
         inputs combine. *)
  List.iter
    (fun ((m : 'k Lowered_ast.module_), symbols) ->
      if m.Lowered_ast.commons <> [] then begin
        let local_labels = Hashtbl.create 64 in
        List.iter
          (fun (sc : 'k Lowered_ast.section_content) ->
            List.iter
              (function
                | Lowered_ast.Label_def { name; _ } -> (
                    match Hashtbl.find_opt symbols name with
                    | Some { Lowered_ast.binding = Lowered_ast.Local; _ } ->
                        Hashtbl.replace local_labels name ()
                    | _ -> ())
                | _ -> ())
              sc.Lowered_ast.fragments)
          m.Lowered_ast.sections;
        List.iter
          (fun (c : Lowered_ast.common) ->
            if Hashtbl.mem local_labels c.Lowered_ast.comm_name then
              fail (`Common_conflicts_with_local c.Lowered_ast.comm_name))
          m.Lowered_ast.commons
      end)
    module_symbols;
  (* Strong beats common outright (no error - the common declarations are
         simply discarded); several declarations of a name that wins fold
         into one reservation sized to the maximum requested size and aligned
         to the maximum requested alignment. *)
  let decls_by_name = Hashtbl.create 16 in
  List.iter
    (fun (c : Lowered_ast.common) ->
      let n = c.Lowered_ast.comm_name in
      Hashtbl.replace decls_by_name n
        (c :: Option.value (Hashtbl.find_opt decls_by_name n) ~default:[]))
    (List.rev all_commons);
  let winning_commons =
    List.filter_map
      (fun name ->
        if Hashtbl.mem strong_defined_names name then None
        else
          let decls = Option.value (Hashtbl.find_opt decls_by_name name) ~default:[] in
          let size =
            List.fold_left
              (fun acc (c : Lowered_ast.common) -> max acc c.Lowered_ast.comm_size)
              0 decls
          in
          let align =
            List.fold_left
              (fun acc (c : Lowered_ast.common) -> max acc c.Lowered_ast.comm_align)
              1 decls
          in
          Some (name, size, align))
      common_names
  in
  (* Not a real input - there is no file this reservation came from - so a
         name outside anything a real [unit_name] could ever be (no real
         input is named with angle brackets) is what marks a resolved
         definition as common-derived rather than something a fixup could
         accidentally address as its "own input". *)
  let common_unit = "<common>" in
  let common_origin = Origin.synthesized ~pass:"image" () in
  let common_contribution =
    match winning_commons with
    | [] -> None
    | _ ->
        let fragments =
          List.concat_map
            (fun (name, size, align) ->
              [
                Lowered_ast.Align { boundary = align; origin = common_origin };
                Lowered_ast.Label_def { name; origin = common_origin };
                Lowered_ast.Zero { length = size; origin = common_origin };
              ])
            winning_commons
        in
        Some
          ( common_unit,
            {
              Lowered_ast.sec =
                {
                  Lowered_ast.sec_name = ".bss";
                  perms = Perms.rw;
                  alignment =
                    List.fold_left (fun acc (_, _, align) -> max acc align) 1 winning_commons;
                  kind = Lowered_ast.Nobits;
                };
              fragments;
            } )
  in
  (module_symbols, common_unit, common_contribution)

(* {2 Merge sections by name alone, first-occurrence order (§4)}

   Matches [contents], [portable.ml]'s [section_bytes] and every other
   name-keyed consumer exactly as it works today - no new compound key.
   A same-name PROGBITS/NOBITS mismatch is a dedicated rejection instead
   of GNU's measured silent coercion (§4); a flag mismatch is a silent
   union, matching measured GNU behavior. *)
let merge_sections ~(fail : ?origin:Origin.t -> error -> unit) (policy : layout_policy)
    (modules : 'k Lowered_ast.module_ list) common_contribution =
  let real_contributions = Hashtbl.create 64 in
  let section_names =
    let real =
      List.fold_left
        (fun acc (m : 'k Lowered_ast.module_) ->
          List.fold_left
            (fun acc (sc : 'k Lowered_ast.section_content) ->
              let n = sc.Lowered_ast.sec.Lowered_ast.sec_name in
              let prior = Hashtbl.find_opt real_contributions n in
              Hashtbl.replace real_contributions n
                ((m.Lowered_ast.unit_name, sc) :: Option.value prior ~default:[]);
              if Option.is_some prior then acc else n :: acc)
            acc m.Lowered_ast.sections)
        [] modules
      |> List.rev
    in
    match common_contribution with
    | Some _ when not (Hashtbl.mem real_contributions ".bss") -> real @ [ ".bss" ]
    | _ -> real
  in
  (match section_names with [] -> fail `No_section | _ -> ());
  let contributions name =
    let real = List.rev (Option.value (Hashtbl.find_opt real_contributions name) ~default:[]) in
    match common_contribution with
    | Some (u, sc) when String.equal name ".bss" -> real @ [ (u, sc) ]
    | _ -> real
  in
  let union_perms =
    List.fold_left
      (fun acc (p : Perms.t) ->
        {
          Perms.read = acc.Perms.read || p.Perms.read;
          write = acc.Perms.write || p.Perms.write;
          execute = acc.Perms.execute || p.Perms.execute;
        })
      Perms.none
  in
  let merged =
    List.map
      (fun name ->
        let cs = contributions name in
        let perms =
          union_perms (List.map (fun (_, sc) -> sc.Lowered_ast.sec.Lowered_ast.perms) cs)
        in
        let alignment =
          List.fold_left (fun acc (_, sc) -> max acc sc.Lowered_ast.sec.Lowered_ast.alignment) 1 cs
        in
        if Perms.writable perms && Perms.executable perms && not policy.allow_rwx then
          fail (`Rwx_segment name);
        (match
           List.sort_uniq compare (List.map (fun (_, sc) -> sc.Lowered_ast.sec.Lowered_ast.kind) cs)
         with
        | [] | [ _ ] -> ()
        | _ -> fail (`Section_kind_mismatch name));
        let kind =
          match cs with
          | (_, sc) :: _ -> sc.Lowered_ast.sec.Lowered_ast.kind
          | [] -> Lowered_ast.Progbits
        in
        (name, cs, perms, alignment, kind))
      section_names
  in
  merged

(* {2 Lay each contribution out on its own - unchanged from M1/M2 - then
   concatenate with inter-contribution padding (§3/§5). A single-module
   link has exactly one contribution per section, so this fold inserts
   no padding and reproduces M1/M2's byte-for-byte output. *)
let layout_contributions ~evaluate ~fill ~align_fill ~section_pad ~add_errors merged =
  let laid =
    List.map
      (fun (name, cs, perms, alignment, kind) ->
        let contribs =
          List.map
            (fun (unit_name, sc) ->
              let bytes, offs, szs, fx, errs, logical_size =
                layout_section ~evaluate ~align_fill sc
              in
              ( unit_name,
                sc.Lowered_ast.sec.Lowered_ast.alignment,
                bytes,
                offs,
                szs,
                fx,
                errs,
                logical_size ))
            cs
        in
        List.iter (fun (_, _, _, _, _, _, errs, _) -> add_errors errs) contribs;
        let buf = Buffer.create 256 in
        let total_logical_size, bases =
          List.fold_left
            (fun (len, bases) (unit_name, calign, bytes, _, _, _, _, logical_size) ->
              let target = align_up len calign in
              let pad = target - len in
              (* NOBITS (§6): pure logical extent, no real bytes anywhere
                     in this section - not even for the merge-boundary gap,
                     since there is nothing for a "fill" to mean here. *)
              (match kind with
              | Lowered_ast.Progbits ->
                  if pad > 0 then
                    Buffer.add_string buf (fill ~executable:(Perms.executable perms) pad);
                  Buffer.add_string buf bytes
              | Lowered_ast.Nobits -> ());
              (target + logical_size, (unit_name, target) :: bases))
            (0, []) contribs
        in
        (* A target that sets [section_pad] (RISC-V alone, measured) rounds this
               executable PROGBITS section's own final size up to its recorded alignment (data
               sections stay as laid out, as in GAS), the same
               way GAS's own assembler does at end-of-section - distinct from the
               inter-contribution merge-gap [fill] above, which every target either lacks
               or answers differently. NOBITS has no bytes to pad (§6). *)
        let total_logical_size =
          match (kind, section_pad) with
          | Lowered_ast.Progbits, Some pad_fn when Perms.executable perms ->
              let target = align_up total_logical_size alignment in
              let pad = target - total_logical_size in
              if pad > 0 then Buffer.add_string buf (pad_fn ~length:pad);
              target
          | _ -> total_logical_size
        in
        let bases = List.rev_map snd bases in
        let shifted =
          List.map2
            (fun base (unit_name, _, _, offs, szs, fx, _, _) ->
              ( unit_name,
                List.map (fun (n, o) -> (n, o + base)) offs,
                List.map (fun (n, e, o) -> (n, e, o + base)) szs,
                List.map (fun (fo, f) -> (fo + base, f)) fx ))
            bases contribs
        in
        (name, perms, alignment, kind, Buffer.contents buf, shifted, total_logical_size))
      merged
  in
  laid

(* {2 Symbol collection and identity (§1/§2): every [Label_def], in its
   merged section's coordinates, qualified by owning input. *)
let collect_definitions ~common_unit module_symbols laid =
  let owners = Hashtbl.create 16 in
  List.iter
    (fun ((m : 'k Lowered_ast.module_), symbols) ->
      if not (Hashtbl.mem owners m.Lowered_ast.unit_name) then
        Hashtbl.add owners m.Lowered_ast.unit_name symbols)
    module_symbols;
  let definitions =
    List.concat_map
      (fun (name, _, _, _, _, shifted, _) ->
        List.concat_map
          (fun (unit_name, offs, _, _) ->
            let owner = Hashtbl.find_opt owners unit_name in
            List.map
              (fun (sym_name, off) ->
                let binding =
                  (* Not a real input (§7 above) - its symbols are
                         resolver-decided [Global], never looked up in any
                         module's own declarations. *)
                  if String.equal unit_name common_unit then Lowered_ast.Global
                  else
                    match owner with
                    | None -> Lowered_ast.Local
                    | Some symbols -> (
                        match Hashtbl.find_opt symbols sym_name with
                        | Some s -> s.Lowered_ast.binding
                        | None -> Lowered_ast.Local)
                in
                {
                  Symtab.d_unit = unit_name;
                  d_name = sym_name;
                  d_binding = binding;
                  d_section = name;
                  d_offset = off;
                })
              offs)
          shifted)
      laid
  in
  definitions

let resolve_fixups ~evaluate laid =
  let fixups =
    List.concat_map
      (fun (name, _, _, _, _, shifted, _) ->
        List.concat_map
          (fun (unit_name, _, _, fx) ->
            List.map
              (fun (frag_off, (f : 'k Lowered_ast.fixup)) ->
                {
                  rf_unit = unit_name;
                  rf_section = name;
                  rf_frag_offset = frag_off;
                  rf_byte_offset = frag_off + f.Lowered_ast.byte_offset;
                  rf_container = f.Lowered_ast.container;
                  rf_pc_bias = f.Lowered_ast.pc_bias;
                  rf_slices = f.Lowered_ast.slices;
                  rf_range = f.Lowered_ast.range;
                  rf_value = f.Lowered_ast.value;
                  rf_original_value = f.Lowered_ast.value;
                  rf_pairing = f.Lowered_ast.pairing;
                  rf_name = f.Lowered_ast.name;
                  rf_kind_name = f.Lowered_ast.kind_name;
                  rf_family = f.Lowered_ast.family;
                  rf_role = f.Lowered_ast.role;
                  rf_origin = f.Lowered_ast.origin;
                  rf_evaluate = evaluate f.Lowered_ast.kind;
                })
              fx)
          shifted)
      laid
  in
  let label_locations =
    List.concat_map
      (fun (name, _, _, _, _, shifted, _) ->
        List.concat_map
          (fun (unit_name, offs, _, _) ->
            List.map (fun (symbol, offset) -> (symbol, name, offset, unit_name)) offs)
          shifted)
      laid
  in
  (fixups, label_locations)

let pair_fixups ~(fail : ?origin:Origin.t -> error -> unit) fixups label_locations =
  let heads =
    List.filter
      (fun f -> match f.rf_pairing with Lowered_ast.Pair_head _ -> true | _ -> false)
      fixups
  in
  (* Head keys are fragment-local.  A duplicate is rejected even if no
         tail happens to reference it, since accepting it would make a later
         low fixup depend on list order. *)
  let seen_head_keys = Hashtbl.create 64 in
  List.iter
    (fun h ->
      match h.rf_pairing with
      | Lowered_ast.Pair_head key ->
          let qualified = (h.rf_section, h.rf_frag_offset, key) in
          if Hashtbl.mem seen_head_keys qualified then
            fail ~origin:h.rf_origin (`Pair_duplicate_head key)
          else Hashtbl.add seen_head_keys qualified ()
      | _ -> ())
    heads;
  let heads_by_sibling = Hashtbl.create 64 and heads_by_offset = Hashtbl.create 64 in
  List.iter
    (fun h ->
      (match h.rf_pairing with
      | Lowered_ast.Pair_head k ->
          let key = (h.rf_section, h.rf_frag_offset, k) in
          Hashtbl.replace heads_by_sibling key
            (h :: Option.value (Hashtbl.find_opt heads_by_sibling key) ~default:[])
      | _ -> ());
      let key = (h.rf_section, h.rf_byte_offset) in
      Hashtbl.replace heads_by_offset key
        (h :: Option.value (Hashtbl.find_opt heads_by_offset key) ~default:[]))
    (List.rev heads);
  let labels_by_name = Hashtbl.create 256 in
  List.iter
    (fun ((n, _, _, _) as l) ->
      Hashtbl.replace labels_by_name n
        (l :: Option.value (Hashtbl.find_opt labels_by_name n) ~default:[]))
    (List.rev label_locations);
  let pair_tail tail candidates description =
    match candidates with
    | [] ->
        fail ~origin:tail.rf_origin (`Pair_missing_head description);
        tail
    | [ head ] ->
        if not (String.equal tail.rf_family head.rf_family) then begin
          fail ~origin:tail.rf_origin (`Pair_incompatible_family description);
          tail
        end
        else
          {
            tail with
            rf_value = head.rf_value;
            rf_frag_offset = head.rf_frag_offset;
            rf_pc_bias = head.rf_pc_bias;
          }
    | _ ->
        fail ~origin:tail.rf_origin (`Pair_ambiguous_head description);
        tail
  in
  let fixups =
    List.map
      (fun tail ->
        match tail.rf_pairing with
        | Lowered_ast.Unpaired | Lowered_ast.Pair_head _ -> tail
        | Lowered_ast.Pair_tail (Lowered_ast.Sibling_key key) ->
            pair_tail tail
              (Option.value
                 (Hashtbl.find_opt heads_by_sibling (tail.rf_section, tail.rf_frag_offset, key))
                 ~default:[])
              key
        | Lowered_ast.Pair_tail (Lowered_ast.Anchor_symbol anchor) -> (
            (* Anchors are synthetic, per-instruction-sequence labels an
                   encoder emits alongside the fixups it paired - always
                   within the input that emitted them, never across a link
                   boundary. §9 requires pairing to resolve in merged
                   coordinates, which this does (candidates carry merged
                   offsets); restricting the *name* search to the tail's own
                   input is what keeps a same-spelled anchor in another input
                   (legal - anchors are ordinary local labels, and §2 says two
                   inputs' locals never collide) from producing a spurious
                   [Pair_ambiguous_head] or a wrong match. *)
            match
              List.filter
                (fun (_, _, _, u) -> String.equal u tail.rf_unit)
                (Option.value (Hashtbl.find_opt labels_by_name anchor) ~default:[])
            with
            | [] ->
                fail ~origin:tail.rf_origin (`Pair_undefined_anchor anchor);
                (* The pairing diagnostic is the root cause.  Replace the
                       unresolved effective expression so the generic import
                       check below does not add a misleading second error.  The
                       original anchor remains in [rf_original_value] for
                       relocation observations. *)
                { tail with rf_value = Expr.Const Bigint.zero }
            | (_, section, _, _) :: _ when not (String.equal section tail.rf_section) ->
                fail ~origin:tail.rf_origin (`Pair_cross_section anchor);
                tail
            | [ (_, section, offset, _) ] ->
                pair_tail tail
                  (Option.value (Hashtbl.find_opt heads_by_offset (section, offset)) ~default:[])
                  anchor
            | _ ->
                fail ~origin:tail.rf_origin (`Pair_ambiguous_head anchor);
                tail))
      fixups
  in
  fixups

let check_definitions ~(fail : ?origin:Origin.t -> error -> unit) ~externs ~index
    (modules : 'k Lowered_ast.module_ list) definitions =
  let global_addresses = Hashtbl.create 64 in
  let conflicting = Hashtbl.create 16 in
  List.iter
    (fun (d : Symtab.definition) ->
      if d.Symtab.d_binding = Lowered_ast.Global then
        let at = (d.Symtab.d_section, d.Symtab.d_offset) in
        match Hashtbl.find_opt global_addresses d.Symtab.d_name with
        | None -> Hashtbl.add global_addresses d.Symtab.d_name at
        | Some first -> if first <> at then Hashtbl.replace conflicting d.Symtab.d_name ())
    definitions;
  List.iter
    (fun n -> fail (`Duplicate_definition n))
    (List.sort compare (Hashtbl.fold (fun n () acc -> n :: acc) conflicting []));
  (* A declaration with no definition anywhere it is allowed to see one
         is [Declared_never_defined] - distinct from an undefined
         *reference* (§1): this is about a declaration nothing backs, not a
         fixup naming something nothing provides. A [Local] declaration must
         be backed by its own input; a [Global] one by any input. [Weak] is
         exempt entirely - an undefined weak symbol is not an error, it is
         exactly the case §7's undefined-weak substitution exists for. *)
  List.iter
    (fun (m : 'k Lowered_ast.module_) ->
      List.iter
        (fun (s : Lowered_ast.symbol) ->
          let ok =
            match s.Lowered_ast.binding with
            | Lowered_ast.Local ->
                Option.is_some
                  (Symtab.find_in_unit index ~unit_name:m.Lowered_ast.unit_name s.Lowered_ast.name)
            | Lowered_ast.Global ->
                Hashtbl.mem externs s.Lowered_ast.name || Symtab.is_global index s.Lowered_ast.name
            | Lowered_ast.Weak -> true
          in
          if not ok then fail (`Declared_never_defined s.Lowered_ast.name))
        m.Lowered_ast.symbols)
    modules;
  ()

let check_references ~(fail : ?origin:Origin.t -> error -> unit) ~externs ~index
    (policy : layout_policy) (modules : 'k Lowered_ast.module_ list) fixups =
  let resolve = resolve_symbol ~index in
  let globals =
    (* First-occurrence order across modules, deduplicated stably: a name
           [.globl]'d in more than one input (legitimate re-declaration of the
           one, single winning definition) must not be exported twice. *)
    let seen = Hashtbl.create 64 in
    List.concat_map
      (fun (m : 'k Lowered_ast.module_) ->
        List.filter_map
          (fun (s : Lowered_ast.symbol) ->
            match s.Lowered_ast.binding with
            | Lowered_ast.Global when not (Hashtbl.mem seen s.Lowered_ast.name) ->
                Hashtbl.add seen s.Lowered_ast.name ();
                Some s.Lowered_ast.name
            | _ -> None)
          m.Lowered_ast.symbols)
      modules
  in
  (* Two or more strong (Global) definitions of the same name at *different*
         addresses - in one input or across several - is a rejection rather
         than a last-wins, unchanged from M1/M2 and now checked link-wide.
         [Weak] is never "strong" here: a weak definition never conflicts with
         anything, per §7's strong > common > weak precedence. Two labels of
         the identical name at the identical (section, offset) - GAS's own
         ordinary "label: label: instruction" idiom, gas_frontier.t's own
         runtime-i64_stod.S/i64_stof.S/i64_utod.S/i64_utof.S (each opens with
         [.global name; name:] immediately followed by a bare [name:] at the
         same address) - is not a conflict on real hardware's own assembler
         (confirmed against real arm-linux-gnueabihf-as: redefining a symbol
         to its own existing value is accepted, only a differing value is
         rejected), so only a genuine second *address* counts here. *)
  (* Preferably a link-global definition, since that is the one an entry
         or export name is supposed to mean - but an explicit entry/export
         name is not required to be [.globl]'d (M1/M2 never required it, and
         a single-module link has no other input a plain local could be
         mistaken for), so a name any input defines at all is accepted too. *)
  let resolvable n = Symtab.defined index n in
  let exports =
    match policy.exported with
    | None -> List.filter (fun n -> resolvable n || not (Hashtbl.mem externs n)) globals
    | Some names -> names
  in
  (* Without a link-global definition the name falls back to the input's own, which
         only means one thing when a single input defines it. *)
  let ambiguous n =
    (not (Symtab.is_global index n))
    && List.length
         (List.sort_uniq String.compare
            (List.map (fun (d : Symtab.definition) -> d.Symtab.d_unit) (Symtab.named index n)))
       > 1
  in
  List.iter
    (fun n ->
      if not (resolvable n) then fail (`Export_undefined n)
      else if ambiguous n then fail (`Ambiguous_symbol n))
    exports;
  (match policy.entry_symbol with
  | Some e when not (resolvable e) -> fail (`Entry_undefined e)
  | Some e when ambiguous e -> fail (`Ambiguous_symbol e)
  | _ -> ());
  (* A fixup naming a symbol nothing in this link defines cannot become a
         bound image unless the host promised to supply it: a name in
         [policy.externs] is an import, and [bind_image] requires its address.
         The other exception is a name declared [Weak] anywhere: §7 requires an
         undefined weak reference to resolve to 0, not to fail - [bind_image]'s
         [address_of] substitutes the value itself, using this same
         [weak_names] set, unless an import supplies one. *)
  let weak_names =
    List.sort_uniq compare
      (List.concat_map
         (fun (m : 'k Lowered_ast.module_) ->
           List.filter_map
             (fun (s : Lowered_ast.symbol) ->
               if s.Lowered_ast.binding = Lowered_ast.Weak then Some s.Lowered_ast.name else None)
             m.Lowered_ast.symbols)
         modules)
  in
  let weak_set = string_set weak_names in
  let imports = ref [] in
  List.iter
    (fun f ->
      List.iter
        (fun s ->
          if Option.is_some (resolve ~from_unit:f.rf_unit s) then ()
          else if Hashtbl.mem externs s then imports := s :: !imports
          else if not (Hashtbl.mem weak_set s) then
            fail ~origin:f.rf_origin (`Fixup_undefined_symbol { fixup = f.rf_name; symbol = s }))
        (Expr.symbols f.rf_value))
    fixups;
  (exports, weak_names, List.sort_uniq compare !imports)

let plan_image ~evaluate ?(fill = default_fill) ?(align_fill = default_align_fill) ?section_pad
    (policy : layout_policy) (modules : 'k Lowered_ast.module_ list) =
  let errors = ref [] in
  let fail ?origin e = errors := diag ?origin e :: !errors in
  match modules with
  | [] -> Diag.fail ~pos:__POS__ [ diag `No_modules ]
  | modules when invalid_modules modules <> [] ->
      Diag.fail ~pos:__POS__ (List.map (fun e -> diag e) (invalid_modules modules))
  | modules ->
      let modules =
        if policy.group_sections then List.map Lowered_ast.group_sections modules else modules
      in
      let externs = string_set policy.externs in
      let module_symbols, common_unit, common_contribution = resolve_commons ~fail modules in
      let merged = merge_sections ~fail policy modules common_contribution in
      let laid =
        layout_contributions ~evaluate ~fill ~align_fill ~section_pad
          ~add_errors:(fun errs -> errors := List.rev_append errs !errors)
          merged
      in
      let contents = List.map (fun (n, _, _, _, bytes, _, _) -> (n, bytes)) laid in
      let definitions = collect_definitions ~common_unit module_symbols laid in
      let index = Symtab.index definitions in
      let fixups, label_locations = resolve_fixups ~evaluate laid in
      let fixups = pair_fixups ~fail fixups label_locations in
      check_definitions ~fail ~externs ~index modules definitions;
      let exports, weak_names, imports =
        check_references ~fail ~externs ~index policy modules fixups
      in
      let segments =
        List.map
          (fun (name, perms, alignment, _kind, bytes, _, total_logical_size) ->
            let init_size = String.length bytes in
            {
              seg_name = name;
              perms;
              alignment;
              init_size;
              zero_fill = total_logical_size - init_size;
            })
          laid
      in
      (match policy.max_size with
      | Some limit ->
          let total = List.fold_left (fun a s -> a + s.init_size + s.zero_fill) 0 segments in
          if total > limit then fail (`Too_large { size = total; limit })
      | None -> ());
      let symbols =
        List.concat_map
          (fun (m : 'k Lowered_ast.module_) ->
            List.map (fun s -> (m.Lowered_ast.unit_name, s)) m.Lowered_ast.symbols)
          modules
      in
      if !errors <> [] then Diag.fail ~pos:__POS__ (List.rev !errors)
      else
        Ok
          {
            plan =
              {
                segments;
                entry = policy.entry_symbol;
                exports;
                imports;
                (* Not "symbols we could not find" - those were just rejected -
                   but placement-dependent fixups still outstanding, which is
                   what §9 says a plan reports and what binding satisfies. *)
                target = (List.hd modules).Lowered_ast.target;
                allow_rwx = policy.allow_rwx;
                unresolved = List.sort_uniq compare (List.map (fun f -> f.rf_name) fixups);
              };
            contents;
            definitions;
            index;
            sizes =
              List.concat_map
                (fun (_, _, _, _, _, shifted, _) ->
                  List.concat_map
                    (fun (unit_name, _, szs, _) ->
                      List.map (fun (n, e, o) -> (unit_name, n, e, o)) szs)
                    shifted)
                laid;
            fixups;
            symbols;
            weak_names;
          }

(* {1 bind_image}

   The addresses arrive from outside. Everything that could not be decided
   without them is decided here: absolute symbol values, the entry, the exports,
   and the [.size] expressions - which is the whole reason [.size s, . - s] is
   not metadata. It exercises the current-location operator and symbol-relative
   subtraction, and its value is a number only once the section has an address. *)

(* Insert a logical value into its slices inside one little-endian container.
   Every target here is little-endian (exec-abi-v1.md §1), so one routine
   covers x86's byte-aligned disp32, ARM's 24-bit field inside a word, and
   AArch64's split adrp immediate alike.

   It overwrites the slice bits rather than adding to them. Addends live in the
   expression, so applying a patch twice cannot double a displacement - which
   is what makes it safe for [bind_image] to be handed a fragment whose bytes
   already contain a resolved encoding. *)
let patch_container bytes ~at ~size ~slices ~value =
  let container = ref 0L in
  for i = size - 1 downto 0 do
    container :=
      Int64.logor (Int64.shift_left !container 8)
        (Int64.of_int (Char.code (Bytes.get bytes (at + i))))
  done;
  List.iter
    (fun (s : Lowered_ast.slice) ->
      let mask =
        if s.Lowered_ast.bit_width >= 64 then -1L
        else Int64.sub (Int64.shift_left 1L s.Lowered_ast.bit_width) 1L
      in
      let piece = Int64.logand (Int64.shift_right_logical value s.Lowered_ast.value_lsb) mask in
      let cleared =
        Int64.logand !container (Int64.lognot (Int64.shift_left mask s.Lowered_ast.bit_offset))
      in
      container := Int64.logor cleared (Int64.shift_left piece s.Lowered_ast.bit_offset))
    slices;
  for i = 0 to size - 1 do
    Bytes.set bytes (at + i)
      (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical !container (8 * i)) 0xFFL)))
  done

(* Addresses are 64-bit patterns: the full unsigned range is valid, so every comparison below is
   unsigned. A segment may end exactly at 2^64, which as a sum wraps to 0. *)
let ult a b = Int64.unsigned_compare a b < 0

let fits_address_space a size =
  let e = Int64.add a (Int64.of_int size) in
  (not (ult e a)) || Int64.equal e 0L

(* [x < b + size] without forming a sum that may wrap. *)
let before_end x b size = ult x b || ult (Int64.sub x b) (Int64.of_int size)

(* An address as the non-negative number it denotes, so that expressions over addresses are exact. *)
let big_of_address a =
  if Int64.compare a 0L >= 0 then Bigint.of_int64 a
  else Bigint.add (Bigint.of_int64 a) (Bigint.shift_left_exn Bigint.one 64)

(* The target kinds that measure from the place of the fixup: the displacement is a distance, and a
   distance between two unsigned addresses must not wrap. *)
let pc_relative f =
  let family = f.rf_family in
  let starts p =
    String.length family >= String.length p && String.sub family 0 (String.length p) = p
  in
  starts "pcrel" || starts "adrp"

let bind_image ?(imports : (string * int64) list = []) ?(overlays : string list list = [])
    ?(load_addresses : (string * int64) list = []) (l : laid_out)
    ~(addresses : (string * int64) list) =
  (* Two segments at one address was M1's silent behaviour, because there was
     only ever one segment. With .text and .data both present it would map one
     over the other and the byte comparison would be against a program that
     cannot run. Overlays are the one deliberate exception: each is a set of
     segments, and overlays take turns in the same memory, so a segment of one
     may overlap a segment of another - and nothing else. *)
  let overlay_of n =
    let rec find k = function
      | [] -> None
      | o :: rest -> if List.mem n o then Some k else find (k + 1) rest
    in
    find 0 overlays
  in
  let other_overlays a b =
    match (overlay_of a, overlay_of b) with Some i, Some j -> i <> j | _ -> false
  in
  let placed =
    List.filter_map
      (fun s ->
        Option.map
          (fun a -> (s.seg_name, a, s.init_size + s.zero_fill))
          (List.assoc_opt s.seg_name addresses))
      l.plan.segments
  in
  let reject_if ~pos condition error = if condition then Diag.fail1 ~pos (diag error) else Ok () in
  let overflow_checks =
    List.map
      (fun (n, a, sz) ->
        reject_if ~pos:__POS__
          (not (fits_address_space a sz))
          (`Overflow { segment = n; address = a; size = sz }))
      placed
  in
  let rec overlap_checks = function
    | [] -> []
    | (n, a, sz) :: rest ->
        List.map
          (fun (n', a', sz') ->
            reject_if ~pos:__POS__
              (before_end a a' sz' && before_end a' a sz && not (other_overlays n n'))
              (`Overlap { a_name = n; a_at = a; a_size = sz; b_name = n'; b_at = a'; b_size = sz' }))
          rest
        @ overlap_checks rest
  in
  let address_checks =
    List.map
      (fun s ->
        reject_if ~pos:__POS__ (not (List.mem_assoc s.seg_name addresses)) (`No_address s.seg_name))
      l.plan.segments
  in
  let alignment_checks =
    List.map
      (fun s ->
        match List.assoc_opt s.seg_name addresses with
        | Some a ->
            reject_if ~pos:__POS__
              (s.alignment > 1 && Int64.unsigned_rem a (Int64.of_int s.alignment) <> 0L)
              (`Alignment { segment = s.seg_name; needs = s.alignment; address = a })
        | None -> Ok ())
      l.plan.segments
  in
  let import_checks =
    List.map
      (fun n ->
        reject_if ~pos:__POS__
          (not (List.mem_assoc n imports || List.mem n l.weak_names))
          (`No_import n))
      l.plan.imports
  in
  (* A segment stored apart from where it runs must not be stored over another
     segment's run extent or over another stored segment. *)
  let stored =
    List.filter_map
      (fun s ->
        match List.assoc_opt s.seg_name load_addresses with
        | Some la when s.init_size > 0 -> Some (s.seg_name, la, s.init_size)
        | _ -> None)
      l.plan.segments
  in
  let intersects (a, sz) (a', sz') = before_end a a' sz' && before_end a' a sz in
  let load_overlap (n, a, sz) (n', a', sz') =
    reject_if ~pos:__POS__
      ((not (String.equal n n')) && intersects (a, sz) (a', sz'))
      (`Load_overlap { a_name = n; a_at = a; a_size = sz; b_name = n'; b_at = a'; b_size = sz' })
  in
  let rec stored_pairs = function
    | [] -> []
    | x :: rest -> List.map (load_overlap x) rest @ stored_pairs rest
  in
  let load_checks =
    List.map
      (fun (n, a, sz) ->
        reject_if ~pos:__POS__
          (not (fits_address_space a sz))
          (`Overflow { segment = n; address = a; size = sz }))
      stored
    @ List.concat_map (fun x -> List.map (load_overlap x) placed) stored
    @ stored_pairs stored
  in
  let overlay_checks =
    List.concat_map
      (fun n ->
        [
          reject_if ~pos:__POS__
            (not (List.exists (fun s -> String.equal s.seg_name n) l.plan.segments))
            (`Overlay_unknown n);
          reject_if ~pos:__POS__
            (List.length (List.filter (List.mem n) overlays) > 1)
            (`Overlay_twice n);
        ])
      (List.sort_uniq compare (List.concat overlays))
  in
  (* At most one overlay is resident at a time, so a reference from one into
     another reaches whatever happens to be loaded. *)
  let cross_overlay_checks =
    List.concat_map
      (fun f ->
        List.filter_map
          (fun s ->
            match resolve_symbol ~index:l.index ~from_unit:f.rf_unit s with
            | Some d when other_overlays f.rf_section d.Symtab.d_section ->
                Some
                  (reject_if ~pos:__POS__ true
                     (`Cross_overlay
                        {
                          fixup = f.rf_name;
                          symbol = s;
                          from_segment = f.rf_section;
                          to_segment = d.Symtab.d_section;
                        }))
            | _ -> None)
          (Expr.symbols f.rf_value))
      l.fixups
  in
  let finish () =
    let errors = ref [] in
    let fail e = errors := diag e :: !errors in
    let address_table = Hashtbl.create 16 and import_table = Hashtbl.create 16 in
    List.iter (fun (n, a) -> Hashtbl.replace address_table n a) (List.rev addresses);
    List.iter (fun (n, a) -> Hashtbl.replace import_table n a) (List.rev imports);
    let plan_imports = string_set l.plan.imports and weak_set = string_set l.weak_names in
    (* §2's lookup order, restated at bind time against [l.definitions]/
       [l.index] rather than re-derived: a reference always resolves
       against its own input first, and only then against what the rest of
       the link may see. This is why [resolved_fixup] carries [rf_unit] and
       [laid_out.sizes] carries its owning unit alongside each entry - a bare
       name alone is not enough to find the right definition once more than
       one input can define same-spelled locals (§2). *)
    let address_of_def (d : Symtab.definition) =
      match Hashtbl.find_opt address_table d.Symtab.d_section with
      | None -> None
      | Some base -> Some (Int64.add base (Int64.of_int d.Symtab.d_offset))
    in
    (* §7: an undefined weak reference resolves to the literal value 0 - not
       a rejection, which is why [plan_image] let it through in the first
       place. A PC-relative fixup needs no separate case: [target = 0] flows
       into the same [rf_evaluate ~place ~target] every other fixup uses, so
       "relative to address 0" falls out of the ordinary displacement
       arithmetic rather than needing to be special-cased here too. *)
    let address_of ~from_unit name =
      match resolve_symbol ~index:l.index ~from_unit name with
      | Some d -> address_of_def d
      | None -> (
          match
            if Hashtbl.mem plan_imports name then Hashtbl.find_opt import_table name else None
          with
          | Some a -> Some a
          | None -> if Hashtbl.mem weak_set name then Some 0L else None)
    in
    (* Entry and exports are always link-global names by construction
       ([plan_image] checked them against [link_global] before this ever
       runs), so there is no owning input to prefer a local over - going
       straight to [link_global] is not a shortcut, it is the correct rule
       for a name that can only mean one thing link-wide. *)
    let address_of_global name =
      match Symtab.find_global l.index name with
      | Some d -> address_of_def d
      | None -> (
          match Symtab.find_any l.index name with Some d -> address_of_def d | None -> None)
    in
    let env_at ~from_unit here_offset section =
      {
        Expr.lookup = (fun n -> Option.map big_of_address (address_of ~from_unit n));
        here =
          (match Hashtbl.find_opt address_table section with
          | None -> None
          | Some base -> Some (big_of_address (Int64.add base (Int64.of_int here_offset))));
      }
    in
    let symbol_sizes =
      List.filter_map
        (fun (unit_name, name, expr, at) ->
          let section =
            match resolve_symbol ~index:l.index ~from_unit:unit_name name with
            | Some d -> d.Symtab.d_section
            | None -> ""
          in
          match Expr.absolute (env_at ~from_unit:unit_name at section) expr with
          | Ok v -> (
              match Bigint.to_int64_opt v with
              | Some n -> Some (name, n)
              | None ->
                  fail (`Symbol_size_too_wide name);
                  None)
          | Error m ->
              fail (`Symbol_size { symbol = name; reason = Err.Error.kind m });
              None)
        l.sizes
    in
    (* {2 Applying fixups}

       Everything a fixup needed and could not have before now: the expression
       becomes an address, [place] becomes the PC the architecture measures
       from, the target turns that into a field value, and the bits go in. *)
    let patched : (string, Bytes.t) Hashtbl.t = Hashtbl.create 16 in
    let () =
      List.iter
        (fun f ->
          let base = Hashtbl.find_opt address_table f.rf_section in
          match base with
          | None -> ()
          | Some base -> (
              let place = Int64.add base (Int64.of_int (f.rf_frag_offset + f.rf_pc_bias)) in
              (* [.] is the address of the instruction, not the PC the
                 architecture measures from. The two differ by [pc_bias] - two
                 bytes for a short x86 branch, eight on ARM - and GAS resolves
                 [jmp .] to a self-loop, so taking [place] here would encode a
                 displacement of zero and fall through instead. *)
              let env =
                {
                  Expr.lookup =
                    (fun n -> Option.map big_of_address (address_of ~from_unit:f.rf_unit n));
                  here = Some (big_of_address (Int64.add base (Int64.of_int f.rf_frag_offset)));
                }
              in
              match Expr.absolute env f.rf_value with
              | Error m ->
                  fail (`Fixup_value { fixup = f.rf_name; reason = Err.Error.kind m });
                  ()
              | Ok target -> (
                  match
                    match Bigint.to_int64_opt target with
                    | Some _ as r -> r
                    | None -> Bigint.to_uint64_opt target
                  with
                  | None ->
                      fail (`Fixup_target_too_wide f.rf_name);
                      ()
                  | Some _
                    when pc_relative f
                         && not
                              (Bigint.fits_signed_exn ~width:64
                                 (Bigint.sub target (big_of_address place))) ->
                      fail (`Fixup_wraps f.rf_name);
                      ()
                  | Some t -> (
                      match f.rf_evaluate ~place ~target:t with
                      (* The one arm that arrives already rendered: the
                         evaluator is the target's, and this module cannot hold
                         its domain. Routing it through the same [fail] as every
                         other bind failure is what keeps the code it chose. *)
                      | Error d ->
                          fail (`Target_fixup d);
                          ()
                      | Ok v ->
                          if not (Lowered_ast.range_admits f.rf_range v) then begin
                            fail
                              (`Fixup_out_of_range
                                 {
                                   fixup = f.rf_name;
                                   value = v;
                                   range = f.rf_range;
                                   at = f.rf_byte_offset;
                                 });
                            ()
                          end
                          else
                            let cur =
                              match Hashtbl.find_opt patched f.rf_section with
                              | Some b -> b
                              | None ->
                                  let b =
                                    Bytes.of_string
                                      (try List.assoc f.rf_section l.contents with Not_found -> "")
                                  in
                                  Hashtbl.replace patched f.rf_section b;
                                  b
                            in
                            patch_container cur ~at:f.rf_byte_offset ~size:f.rf_container
                              ~slices:f.rf_slices ~value:v))))
        l.fixups
    in
    let content_of name =
      match Hashtbl.find_opt patched name with
      | Some b -> Bytes.to_string b
      | None -> ( try List.assoc name l.contents with Not_found -> "")
    in
    let segments =
      List.map
        (fun s ->
          {
            name = s.seg_name;
            perms = s.perms;
            alignment = s.alignment;
            address = (match List.assoc_opt s.seg_name addresses with Some a -> a | None -> 0L);
            load_address = List.assoc_opt s.seg_name load_addresses;
            bytes = content_of s.seg_name;
            zero_fill = s.zero_fill;
          })
        l.plan.segments
    in
    let entry = match l.plan.entry with None -> None | Some e -> address_of_global e in
    let exports =
      List.filter_map (fun n -> Option.map (fun a -> (n, a)) (address_of_global n)) l.plan.exports
    in
    if !errors <> [] then Diag.fail ~pos:__POS__ (List.rev !errors)
    else Ok { segments; entry; exports; symbol_sizes }
  in
  match
    Diag.validate_all ~pos:__POS__
      (overflow_checks @ overlap_checks placed @ address_checks @ alignment_checks @ import_checks
     @ overlay_checks @ cross_overlay_checks @ load_checks)
  with
  | Error errors -> Error errors
  | Ok () -> finish ()

(* {1 Rendering}

   The canonical dump for the [--dump-image] boundary. Addresses are the
   caller's, so this is only meaningful after binding. *)

(* The annotation is load-bearing: [t] declares a [segments] field too, and it
   is declared later, so without it OCaml resolves [p.segments] to [t]'s. *)
let pp_plan ppf (p : plan) =
  Fmt.pf ppf "@[<v>%a%a%a%a%a@]"
    Fmt.(
      list ~sep:cut (fun ppf (s : segment_plan) ->
          Fmt.pf ppf "segment %s size=%d zero=%d align=%d permissions=%a" s.seg_name s.init_size
            s.zero_fill s.alignment Perms.pp s.perms))
    p.segments
    Fmt.(option (any "@,entry " ++ string))
    p.entry
    Fmt.(list ~sep:nop (fun ppf e -> Fmt.pf ppf "@,export %s" e))
    p.exports
    Fmt.(list ~sep:nop (fun ppf i -> Fmt.pf ppf "@,import %s" i))
    p.imports
    (* What binding still has to do, which is the other half of what a plan is
       for: §9 splits layout from address assignment, and a plan that showed only
       what it had already decided would not say why the second step exists. *)
    Fmt.(list ~sep:nop (fun ppf u -> Fmt.pf ppf "@,unresolved %s" u))
    p.unresolved

let pp ppf (t : t) =
  Fmt.pf ppf "@[<v>%a%a%a@]"
    Fmt.(
      list ~sep:cut (fun ppf (s : segment) ->
          (* [size] is the address-space extent ([init_size + zero_fill], §6's
             standing invariant), not [String.length s.bytes] alone - a NOBITS
             segment has real bytes only for its own [init_size] (always 0
             until common-symbol storage, §7), and reporting less than its
             true extent here would under-report exactly the segment kind
             this refactor exists to get right. *)
          Fmt.pf ppf "section %s address=0x%Lx%a size=%d permissions=%a" s.name s.address
            Fmt.(option (fun ppf a -> Fmt.pf ppf " load=0x%Lx" a))
            s.load_address
            (String.length s.bytes + s.zero_fill)
            Perms.pp s.perms))
    t.segments
    Fmt.(option (fun ppf a -> Fmt.pf ppf "@,entry 0x%Lx" a))
    t.entry
    Fmt.(
      list ~sep:nop (fun ppf (n, a) ->
          (* The size is printed only when a [.size] directive gave the symbol
             one. Defaulting to zero would make "no .size directive" and
             ".size s, 0" print identically, and the first is the common case. *)
          match List.assoc_opt n t.symbol_sizes with
          | Some sz -> Fmt.pf ppf "@,export %s = 0x%Lx size=%Ld" n a sz
          | None -> Fmt.pf ppf "@,export %s = 0x%Lx" n a))
    t.exports

(* {1 Fixup observations}

   What the differential oracle sees. It enters through the architecture-erased
   driver, so without this it would have no way to ask what references the
   assembler produced, let alone classify them - and classification needs
   binding, visibility and section relation, none of which survive as anything
   the kind could carry.

   Two variants rather than optional fields: an expression naming two symbols
   has no single symbol, and therefore no binding or visibility either. Saying
   so is more useful than four [None]s that a caller has to interpret. *)

type site = {
  o_section : string;
  o_offset : int;  (** the patch container, section-relative: where an ELF record would point *)
  o_place : int;
      (** the PC base this fixup measures from, section-relative. Carried because the ELF addend of
          a PC-relative record is [expr_addend + o_offset - o_place] and neither term is recoverable
          from the other; on x86 [o_place] is the realized instruction length past the fragment
          start, so it is not a constant anyone could substitute. *)
  o_kind_name : string;
  o_family : string;
  o_role : Lowered_ast.fixup_role;
}

(* Named rather than inline, so a classifier can take the payload on its own.
   That is not a convenience: an observation with no single symbol has no
   binding, visibility or section relation either, and a [classify] handed the
   whole sum would have a case it cannot decide - which is an invitation to
   invent an answer for it. The caller matches first and routes the other
   variant to the post-link byte comparison. *)
type symbolic_ref = {
  site : site;
  symbol : string;
  addend : int64;
  binding : [ `Local | `Global ];
  visibility : Lowered_ast.visibility;
  defined : bool;
  same_section : bool;
}

type non_normalizable = { nn_site : site; nn_expr : string }
type fixup_observation = Symbolic_ref of symbolic_ref | Non_normalizable of non_normalizable

(* An ELF record names one symbol and one addend. Only these two shapes reduce
   to that; anything else is reported as itself and gated by the post-link byte
   comparison instead of pretending to a record-for-record match.

   Folding first is what makes that "shapes", plural, rather than "spellings":
   [g + (2 + 2)] and [g + 4] are the same reference and must produce the same
   record, and matching on the unfolded tree would call the first
   non-normalizable and quietly drop it out of the comparison. [Expr.fold]
   leaves symbols and modifiers alone, so a genuine two-symbol expression still
   arrives here unreduced and is still reported as itself. *)
let normalize_ref (e : Expr.t) =
  (* A folding failure - a division by zero in a subterm, say - is not a shape
     this can name, so it takes the same route as any other unreducible one. *)
  match match Expr.fold Expr.no_env e with Ok folded -> folded | Error _ -> e with
  | Expr.Symbol s -> Some (s, 0L)
  | Expr.Binary (Expr.Add, Expr.Symbol s, Expr.Const k)
  (* Addition commutes and GAS accepts both spellings, so [4+g] is the same
     reference as [g+4]. Subtraction does not, and [4-g] is deliberately absent
     below: it is not a symbol plus an addend at all. *)
  | Expr.Binary (Expr.Add, Expr.Const k, Expr.Symbol s) ->
      Option.map (fun v -> (s, v)) (Bigint.to_int64_opt k)
  | Expr.Binary (Expr.Sub, Expr.Symbol s, Expr.Const k) ->
      Option.map (fun v -> (s, Int64.neg v)) (Bigint.to_int64_opt k)
  | _ -> None

let fixup_observations (l : laid_out) =
  let in_unit = Hashtbl.create 256 and link_visible = Hashtbl.create 256 in
  List.iter
    (fun (u, (s : Lowered_ast.symbol)) ->
      let n = s.Lowered_ast.name in
      if not (Hashtbl.mem in_unit (u, n)) then Hashtbl.add in_unit (u, n) s;
      if s.Lowered_ast.binding <> Lowered_ast.Local && not (Hashtbl.mem link_visible n) then
        Hashtbl.add link_visible n s)
    l.symbols;
  List.map
    (fun f ->
      let site =
        {
          o_section = f.rf_section;
          o_offset = f.rf_byte_offset;
          o_place = f.rf_frag_offset + f.rf_pc_bias;
          o_kind_name = f.rf_kind_name;
          o_family = f.rf_family;
          o_role = f.rf_role;
        }
      in
      match normalize_ref f.rf_original_value with
      | None -> Non_normalizable { nn_site = site; nn_expr = Expr.to_string f.rf_original_value }
      | Some (symbol, addend) ->
          (* §2's lookup order again: the fixup's own input's declaration of
             [symbol] first (a local declaration is real metadata only there),
             and only then any input's link-visible one. *)
          let sym =
            match Hashtbl.find_opt in_unit (f.rf_unit, symbol) with
            | Some _ as s -> s
            | None -> Hashtbl.find_opt link_visible symbol
          in
          let resolved = resolve_symbol ~index:l.index ~from_unit:f.rf_unit symbol in
          Symbolic_ref
            {
              site;
              symbol;
              addend;
              (* The oracle-facing pair has no weak variant: a weak definition is
                 reported as [`Global]. *)
              binding =
                (match sym with
                | Some { Lowered_ast.binding = Lowered_ast.Global | Lowered_ast.Weak; _ } -> `Global
                | Some { Lowered_ast.binding = Lowered_ast.Local; _ } | None -> `Local);
              visibility =
                (match sym with Some s -> s.Lowered_ast.visibility | None -> Lowered_ast.Default);
              defined = Option.is_some resolved;
              same_section =
                (match resolved with
                | Some d -> String.equal d.Symtab.d_section f.rf_section
                | None -> false);
            })
    l.fixups

let pp_observation ppf = function
  | Symbolic_ref o ->
      Fmt.pf ppf "%s\t0x%08x\t%s\t%s\t%s\t%s\t%s\t%s\t%s" o.site.o_section o.site.o_offset
        o.site.o_kind_name
        (Lowered_ast.fixup_role_name o.site.o_role)
        o.symbol
        (Printf.sprintf "%s0x%Lx"
           (if Int64.compare o.addend 0L < 0 then "-" else "+")
           (Int64.abs o.addend))
        (match o.binding with `Local -> "local" | `Global -> "global")
        (Lowered_ast.visibility_name o.visibility)
        (if o.defined then if o.same_section then "same-section" else "other-section"
         else "undefined")
  | Non_normalizable o ->
      Fmt.pf ppf "%s\t0x%08x\t%s\t%s\tnon-normalizable\t%s" o.nn_site.o_section o.nn_site.o_offset
        o.nn_site.o_kind_name
        (Lowered_ast.fixup_role_name o.nn_site.o_role)
        o.nn_expr

(* The plan a laid-out module carries. [laid_out] is otherwise opaque to a
   caller by convention: it is [plan_image]'s working state, handed straight
   back to [bind_image] so that the two halves of §9's contract cannot be
   applied to different inputs. The plan is the half a host is meant to read. *)
let plan_of (l : laid_out) = l.plan
let definitions (l : laid_out) = l.definitions
