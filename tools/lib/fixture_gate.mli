(** Acceptance gates on generated fixture assembly.

    A generated fixture that silently loses the construct it exists to exercise
    is worse than no fixture: it passes every byte gate and proves nothing. A
    failure here means the C source must change, not the gate. *)

type violation =
  | Comm_or_local  (** uninitialized globals and statics: M3 scope *)
  | Bss_or_nobits  (** an all-zero initializer routed to .bss: also M3 *)
  | Tail_converted_call
      (** `return callee(x);` becomes a tail branch on x86-64, ARM and AArch64 -
          which is exactly the relocation the direct_call fixture exists to
          produce, so requiring the call by name is the only way to notice *)
  | Missing_entry_label

val check : case:string -> asm:string -> violation list
(** Every violation found, in a fixed order - a case can trip more than one and
    reporting only the first would hide the rest. *)

val message : case:string -> target:Target.t -> violation -> string
(** The shell's exact wording, because these lines are byte-compared. *)
