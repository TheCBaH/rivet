(** Builds one canonical, legal {!Isa_generated_case.case} per
    {!Isa_gen_pilot.pilot_entry}.

    "Canonical" means the plainest legal spelling of the form - representative
    non-special registers, a small legal immediate - before any
    alias/boundary/negative obligations. Only one case per pilot entry: full
    obligation coverage (every register class, every immediate boundary, both
    x86 immediate widths) is later work, not this task's. *)

val configuration_for : Target.t -> string list
(** The resolved argv fragment for a target's own per-suite features -
    RISC-V's `-march`/`-mabi`/`-mno-relax`, x86's empty list (the two cross
    assemblers are already ISA-specific binaries) - NEVER CompCert's own
    frozen [Target.config] flags. *)

val operands_for : string -> (string * string) list option
(** The hardcoded canonical operand assignment for a pilot [form_id], or
    [None] if this module has none recorded. Exposed so tests can check the
    table's own properties (e.g. "never assigns a special-cased register")
    without needing a full {!Isa_norm_model.form} to call {!build} with. *)

val build :
  Isa_gen_pilot.pilot_entry -> Isa_norm_model.form -> (Isa_generated_case.case, string) result
(** [build entry form] renders [form]'s own syntax recipe with this module's
    hardcoded canonical operand assignment for [entry.form_id]. [Error] names
    [entry.form_id] if this module has no recorded assignment for it yet -
    every current pilot entry has one; this is a completeness net for a
    future manifest addition, not an expected outcome today. *)
