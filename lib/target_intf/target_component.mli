(** Descriptors for a target family's separable instruction components.

    A component is an implementation boundary - a group of forms one module owns, such as RISC-V M
    or x86 x87. A feature is the architectural predicate that gates it. The two are related many to
    many in general; a descriptor records the one feature name a component is gated by today and
    leaves the feature vocabulary to the configuration layer.

    Descriptors are pure data. They carry no encoder: the family builds its codec alternatives from
    the same tables the descriptor is derived from, so the descriptor and the codec cannot drift
    apart, and {!check} catches the remaining ways two components can collide. *)

type source = { upstream : string; name : string }
(** One upstream record a form is known to correspond to: [upstream] names the ISA source (for
    example ["xed"] or ["riscv-opcodes"]) and [name] that source's own record identifier. The
    mapping is one to many by design: a single encoding can be described by several records, and
    one record can describe several spellings. *)

(** Legality of a form: which enabled features admit it. This is separate from ownership, which is
    only catalogue provenance; [Any] expresses an instruction several extensions provide
    independently, [All] one that needs several at once. *)
module Req : sig
  type t = Feature of string | All of t list | Any of t list

  val eval : (string -> bool) -> t -> bool
  (** Whether the predicate holds when [enabled] says which features are on. *)

  val unmet : (string -> bool) -> t -> t option
  (** The part of the predicate that fails, [None] when it holds. An [Any] that fails is reported
      whole, so its alternatives are not presented as mandatory. *)

  val features : t -> string list
  val pp : Format.formatter -> t -> unit
end

(** A condition on an implication. [Xlen] is resolved by {!specialize}. *)
type cond = Always | Has of string | Xlen of int | And of cond list

type form = {
  label : string;  (** the codec alternative's label, which is the stable [form_id] prefix *)
  mnemonics : string list;  (** the source spellings the form accepts *)
  sources : source list;
  legal : Req.t option;  (** [None]: legal exactly when the owning component's feature is on *)
}

type t = {
  id : string;  (** stable, dot-separated component identity, for example ["riscv.m"] *)
  feature : string;  (** the feature name that gates this component *)
  requires : string list;  (** features that must also be enabled whenever this one is *)
  conflicts : string list;  (** features that cannot be enabled together with this one *)
  implies : (cond * string) list;
      (** features enabled whenever this one is on and the condition holds, such as RISC-V C
          implying Zcd when D is present. A component with no forms and only implications is
          valid. *)
  default_on : bool;  (** whether the default configuration enables it *)
  xlens : int list;  (** the XLENs it applies to; empty means all *)
  not_applicable : string option;
      (** set by {!specialize} when the target's XLEN is outside [xlens] *)
  summary : string;
  forms : form list;
}

val specialize : xlen:int -> t -> t
(** Resolve [Xlen] conditions and applicability for a target of this XLEN. *)

val legal : t list -> string -> Req.t option
(** The legality predicate of the form spelled [mnemonic], or [None] when no component owns it - the
    mnemonic is then part of the always-available base. *)

val labels : t -> string list
val mnemonics : t -> string list

val owns_mnemonic : t -> string -> bool
(** Whether the component accepts [mnemonic] as a spelling of one of its forms. *)

val feature_of_mnemonic : t list -> string -> string option
(** The feature gating the component that owns [mnemonic], or [None] when no component does - the
    mnemonic is then part of the always-available base. *)

val check : t list -> string list
(** Structural problems across a set of components composed into one family: duplicate component
    ids or feature names, duplicate labels, a mnemonic claimed by two components, a requirement or
    conflict naming an unknown feature, a component or form with nothing in it, and a form with no
    source mapping. Empty means clean. It says nothing about whether the
    forms encode correctly - that is the codec and differential tests' job. *)
