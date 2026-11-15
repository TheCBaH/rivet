type fixup_role = Call | Branch | Data_address

val fixup_role_name : fixup_role -> string

type range = Signed of int | Unsigned of int | Bitpattern of int

val range_width : range -> int
val range_admits : range -> Int64.t -> bool
val pp_range : Format.formatter -> range -> unit

type slice = { bit_offset : int; bit_width : int; value_lsb : int }
type pair_ref = Sibling_key of string | Anchor_symbol of string
type pairing = Unpaired | Pair_head of string | Pair_tail of pair_ref

type 'k fixup = {
  kind : 'k;
  kind_name : string;
  family : string;
  role : fixup_role;
  name : string;
  slices : slice list;
  byte_offset : int;
  container : int;
  pc_bias : int;
  range : range;
  value : Expr.t;
  pairing : pairing;
  origin : Foundation.Origin.t;
}

type branch =
  | Symbolic of { value : Expr.t; rung : string option }
  | Resolved of { value : int64; rung : string }

val pp_branch : Format.formatter -> branch -> unit
val equal_branch : branch -> branch -> bool

type 'k encoded_form = { bytes : string; form : string; fixups : 'k fixup list }

type 'k fragment =
  | Bytes of {
      bytes : string;
      form : string option;
      fixups : 'k fixup list;
      origin : Foundation.Origin.t;
    }
  | Align of { boundary : int; origin : Foundation.Origin.t }
  | Relax of { alts : 'k encoded_form list; origin : Foundation.Origin.t }
  | Zero of { length : int; origin : Foundation.Origin.t }
  | Label_def of { name : string; origin : Foundation.Origin.t }
  | Set_size of { name : string; size : Expr.t; origin : Foundation.Origin.t }

type section_kind = Progbits | Nobits

val section_kind_name : section_kind -> string

type section = { sec_name : string; perms : Perms.t; alignment : int; kind : section_kind }
type 'k section_content = { sec : section; fragments : 'k fragment list }
type visibility = Default | Hidden | Protected | Internal

val visibility_name : visibility -> string

type binding = Local | Global | Weak

val binding_name : binding -> string

type symbol = {
  name : string;
  binding : binding;
  visibility : visibility;
  kind : Directive.sym_kind;
  size : Expr.t option;
  section : string;
}

type common = { comm_name : string; comm_size : int; comm_align : int }

type 'k module_ = {
  unit_name : string;
  target : string;
      (** the target the module was lowered for ([ENCODE.name]); a direct producer states it, and
          an image of one target refuses to run on a host of another *)
  sections : 'k section_content list;
  symbols : symbol list;
  commons : common list;
  declared_sections : string list;
}

val pp_slice : Format.formatter -> slice -> unit
val pp_pairing : Format.formatter -> pairing -> unit
val pp_fixup : Format.formatter -> 'a fixup -> unit
val pp_fixups : Format.formatter -> 'a fixup list -> unit
val pp_fragment : Format.formatter -> 'a fragment -> unit

type relax_error =
  [ `Empty_ladder
  | `Fixup_family_changes of fixup_family_changes
  | `Fixup_target_differs of string
  | `Rung_fixups_differ of rung_pair
  | `Rung_not_shorter of rung_pair ]

and rung_pair = { rung : string; rung_bytes : int; other : string; other_bytes : int }
and fixup_family_changes = { fixup : string; from_family : string; to_family : string }

val pp_relax_error : Format.formatter -> relax_error -> unit
val relax_error_code : relax_error -> string
val validate_relax : 'k encoded_form list -> (unit, relax_error) Err.t
val pp_symbol : Format.formatter -> symbol -> unit
val pp_common : Format.formatter -> common -> unit
val pp : Format.formatter -> 'a module_ -> unit

type fixup_defect =
  | Offset_outside_fragment
  | Container_size of int
  | Slice_outside_container
  | Slice_outside_range
  | Range_width of int

type invalid =
  | Empty_name of [ `Unit | `Target | `Section ]
  | Duplicate_section of string
  | Bad_alignment of { where : string; value : int }
  | Negative_length of { section : string; length : int }
  | Bad_fixup of { section : string; fixup : string; defect : fixup_defect }
  | Duplicate_label of { section : string; name : string }

val pp_invalid : Format.formatter -> invalid -> unit

val validate : 'k module_ -> (unit, invalid list) result
(** Checks one module on its own: names, alignments, zero lengths, fixup geometry and label
    uniqueness. Two labels of one name are accepted only at the same address. *)

val group_parent : string -> string option
(** [Some ".text"] for [".text.foo"], and likewise for [.rodata], [.data] and [.bss]; [None]
    otherwise *)

val group_sections : 'k module_ -> 'k module_
(** Merges [.text.*], [.rodata.*], [.data.*] and [.bss.*] sections into their parent section, in
    input order (the parent's own content first if it comes first), each child starting at its
    own alignment, symbols moved with them. A child whose permissions or kind differ from the
    parent's (or, with no parent, from the first child's) keeps its own name. *)
