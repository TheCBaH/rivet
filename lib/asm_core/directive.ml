(* Normalized directives (docs/design.md §4.3).

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

(** What the argument of [.align] denotes. GNU [as] reads it as a byte count on x86 and as an
    exponent on ARM, AArch64 and RISC-V; [zero_exponent] is the exponent an argument of 0 stands
    for (ARM treats [.align 0] as [.align 2]). *)
type align_unit = Bytes | Exponent of { zero_exponent : int }

type visibility = Default | Hidden | Protected | Internal

type t =
  | Section of { name : string; perms : Perms.t; nobits : bool }
      (** [nobits] (M3 §6): a plain [bool] rather than [Lowered_ast.section_kind], because
          [Lowered_ast] depends on this module for [sym_kind] and referencing it back here would
          be circular. The driver converts at the point it builds a [Lowered_ast.section]. *)
  | Declared_section of { name : string }
      (** recorded in the module and never allocated; [.note.GNU-stack] is the M1 case *)
  | Align of { boundary : int; fill : int option; max_skip : int option }
      (** [fill] is the byte value of the padding (GNU's second operand; the padding is the
          target's no-op in an executable section and zero elsewhere when absent); [max_skip]
          skips the padding altogether when more than that many bytes would be needed. The
          section's alignment is raised to [boundary] either way. Always a byte count. [.align 16] on x86 and [.balign 4] on ARM normalize to the same
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
  | Ascii of { bytes : string }
      (** [.ascii], [.asciz] and [.string]: the decoded bytes of every string argument, with the
          terminating NUL of the last two already appended. One string rather than one [Data]
          value per character. *)
  | Zero of { length : int }
      (** [.zero]/[.space] (M3 §6), single-argument form only - a fill byte ([.space size,fill])
          is not supported, since a NOBITS section cannot hold one anyway and a PROGBITS
          [.space size,fill] is not a case any fixture needs yet. Reservation-only: real,
          initialized NUL bytes in a PROGBITS section, pure logical extent in a NOBITS one - the
          image layer (not this one) decides which, from the section's own kind. *)
  | Visibility of { name : string; visibility : visibility }
      (** [.hidden], [.protected], [.internal] *)
  | Push_section of t
      (** [.pushsection]: the current section goes on a stack and the [Section] or
          [Declared_section] it carries becomes current *)
  | Pop_section  (** [.popsection] *)
  | Previous_section  (** [.previous]: swap with the section that was current before *)
  | Target_state of { name : string; arguments : string list }
      (** a state directive the target interpreted, with its comma-separated arguments as text *)
  | Cfi of { name : string; argument : string }
      (** a [.cfi_*] directive ([name] with its dot), carried so a producer's call-frame
          information survives into a GNU export. It changes no byte of the image: lowering
          drops it, and the text route reads and discards it as before. *)

let ascii_values bytes =
  List.init (String.length bytes) (fun i ->
      Expr.Const (Rivet_foundation.Bigint.of_int (Char.code bytes.[i])))

let visibility_name = function
  | Default -> "default"
  | Hidden -> "hidden"
  | Protected -> "protected"
  | Internal -> "internal"

let rec pp ppf = function
  | Section { name; perms; nobits } ->
      Fmt.pf ppf "section %s %a%s" name Perms.pp perms (if nobits then " nobits" else "")
  | Declared_section { name } -> Fmt.pf ppf "declared-section %s" name
  | Align { boundary; fill; max_skip } ->
      Fmt.pf ppf "align %d" boundary;
      Option.iter (Fmt.pf ppf " fill %d") fill;
      Option.iter (Fmt.pf ppf " max %d") max_skip
  | Global { name } -> Fmt.pf ppf "globl %s" name
  | Weak { name } -> Fmt.pf ppf "weak %s" name
  | Local { name } -> Fmt.pf ppf "local %s" name
  | Common { name; size; align } -> Fmt.pf ppf "comm %s, %d, %d" name size align
  | Sym_type { name; kind } -> Fmt.pf ppf "type %s %s" name (sym_kind_name kind)
  | Sym_size { name; size } -> Fmt.pf ppf "size %s %a" name Expr.pp size
  | Data { width; values } ->
      Fmt.pf ppf "data %d %a" width Fmt.(list ~sep:(any ", ") Expr.pp) values
  | Ascii { bytes } ->
      Fmt.pf ppf "data 1 %a" Fmt.(list ~sep:(any ", ") Expr.pp) (ascii_values bytes)
  | Zero { length } -> Fmt.pf ppf "zero %d" length
  | Visibility { name; visibility } -> Fmt.pf ppf "%s %s" (visibility_name visibility) name
  | Push_section d -> Fmt.pf ppf "push-%a" pp d
  | Pop_section -> Fmt.string ppf "pop-section"
  | Previous_section -> Fmt.string ppf "previous-section"
  | Target_state { name; arguments } ->
      Fmt.pf ppf "target-state %s %s" name (String.concat ", " arguments)
  | Cfi { name; argument } -> Fmt.pf ppf "cfi %s %s" name argument
