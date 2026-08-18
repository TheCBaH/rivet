(** Acceptance gates on generated fixture assembly.

    A generated fixture that silently loses the construct it exists to exercise
    is worse than no fixture: it passes every byte gate and proves nothing. A
    failure here means the C source must change, not the gate. *)

type violation =
  | Comm_or_local
      (** uninitialized globals and statics - out of scope for every case
          except [cross_bss], the one fixture that exists to exercise this *)
  | Bss_or_nobits
      (** an all-zero initializer routed to .bss - same carve-out as
          {!Comm_or_local}, for the same one case *)
  | Tail_converted_call
      (** `return callee(x);` becomes a tail branch on x86-64, ARM and AArch64 -
          which is exactly the relocation the direct_call fixture exists to
          produce, so requiring the call by name is the only way to notice *)
  | Missing_entry_label

val check : case:string -> asm:string -> violation list
(** Every per-FILE violation found, in a fixed order - a case can trip more
    than one and reporting only the first would hide the rest. [asm] is one
    compilation unit's generated text; for a multi-source case this is called
    once per unit per target, since each unit's own content (a `.comm`, a
    tail-converted call) is independent of every other unit's.

    [Missing_entry_label] is NOT among these: it is a whole-CASE property, not
    a per-file one - see {!has_entry_label}. *)

val has_entry_label : string -> bool
(** Whether this one compilation unit's generated text defines the frozen
    [asm_test_entry] label. A single-source case has exactly one unit, so
    checking it alone is equivalent to the old per-file check; a multi-source
    case's caller/entry unit is the only one of its N units expected to
    satisfy this, so the [Missing_entry_label] gate must be evaluated once per
    (case, target) over the UNION of all of that case's units, not once per
    unit - a callee-only or data-only unit legitimately has no entry label of
    its own. *)

val message : case:string -> target:Target.t -> violation -> string
(** The shell's exact wording, because these lines are byte-compared. *)
