(* Normalized directives (.ai/asm_plan.md §4.3).

   The source AST keeps directive arguments as token slices, because GAS
   directive syntax is not one grammar - [.arch armv7-a] and [.size s, . - s]
   have nothing in common. Normalization is where each becomes a value with
   meaning, and where the three categories the M1 fixtures exercise separate:

   - semantic: it changes what is assembled ([.text], [.align], [.globl],
     [.type], [.size]);
   - declared-but-not-imaged: [.note.GNU-stack] is recorded and never becomes a
     section object, which is why M1.4's restriction is "reject a second
     *allocatable* section" rather than "a second section";
   - target state: [.syntax], [.arch], [.fpu], [.arm] go to the target, which
     owns modes and features;
   - metadata-only: the [.cfi_*] forms, whose arguments are consumed lexically
     and then discarded.

   Anything else is a diagnostic, per §2.2. An assembler that ignores directives
   it does not know produces a plausible image from a file it did not
   understand. *)

type sym_kind = Function | Object | Notype

let sym_kind_name = function Function -> "function" | Object -> "object" | Notype -> "notype"

type t =
  | Section of { name : string; perms : Perms.t; nobits : bool }
      (** [nobits] (M3 §6): a plain [bool] rather than [Lowered_ast.section_kind], because
          [Lowered_ast] depends on this module for [sym_kind] and referencing it back here would
          be circular. The driver converts at the point it builds a [Lowered_ast.section]. *)
  | Declared_section of { name : string }
      (** recorded in the module and never allocated; [.note.GNU-stack] is the M1 case *)
  | Align of { boundary : int }
      (** always a byte count. [.align 16] on x86 and [.balign 4] on ARM normalize to the same
          constructor with *different values*: the dialect difference is which unit the surface
          syntax denotes, not the value. *)
  | Global of { name : string }
  | Weak of { name : string }
      (** M3 §7. Binding-directive transitions are a measured table, not a uniform
          last-directive-wins rule: [.weak] is sticky against a later [.local]/[.globl], but
          [.weak] itself always applies. The driver implements the transition; this constructor
          only names the directive. *)
  | Local of { name : string }  (** M3 §7: last-wins against [.globl] unless [.weak] has applied. *)
  | Common of { name : string; size : int; align : int }
      (** M3 §7: [.comm sym,size[,align]]. A size/alignment claim, no input-section offset - the
          image layer folds every declaration of one name into a single reservation sized to the
          maximum requested size and aligned to the maximum requested alignment, then places it as
          an internal contribution to the canonical [.bss] group. *)
  | Sym_type of { name : string; kind : sym_kind }
  | Sym_size of { name : string; size : Expr.t }
      (** not metadata: it exercises the current-location operator and symbol-relative
          subtraction, and lands in the symbol record and the link map *)
  | Data of { width : int; values : Expr.t list }
      (** [.byte], [.short], [.long], [.quad] and the dialect spellings that mean the same widths.
          The width in *bytes* rather than the spelling, because [.word] is two bytes in GNU x86
          syntax and four on ARM and AArch64 - so the surface name is a dialect fact and the width
          is the value. An initializer naming a symbol stays an expression and becomes a fixup. *)
  | Zero of { length : int }
      (** [.zero]/[.space] (M3 §6), single-argument form only - a fill byte ([.space size,fill])
          is not supported, since a NOBITS section cannot hold one anyway and a PROGBITS
          [.space size,fill] is not a case any fixture needs yet. Reservation-only: real,
          initialized NUL bytes in a PROGBITS section, pure logical extent in a NOBITS one - the
          image layer (not this one) decides which, from the section's own kind. *)
  | Target_state of { name : string; argument : string }

let pp ppf = function
  | Section { name; perms; nobits } ->
      Fmt.pf ppf "section %s %a%s" name Perms.pp perms (if nobits then " nobits" else "")
  | Declared_section { name } -> Fmt.pf ppf "declared-section %s" name
  | Align { boundary } -> Fmt.pf ppf "align %d" boundary
  | Global { name } -> Fmt.pf ppf "globl %s" name
  | Weak { name } -> Fmt.pf ppf "weak %s" name
  | Local { name } -> Fmt.pf ppf "local %s" name
  | Common { name; size; align } -> Fmt.pf ppf "comm %s, %d, %d" name size align
  | Sym_type { name; kind } -> Fmt.pf ppf "type %s %s" name (sym_kind_name kind)
  | Sym_size { name; size } -> Fmt.pf ppf "size %s %a" name Expr.pp size
  | Data { width; values } ->
      Fmt.pf ppf "data %d %a" width Fmt.(list ~sep:(any ", ") Expr.pp) values
  | Zero { length } -> Fmt.pf ppf "zero %d" length
  | Target_state { name; argument } -> Fmt.pf ppf "target-state %s %s" name argument
