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
  | Section of { name : string; perms : Perms.t }
  | Declared_section of { name : string }
      (** recorded in the module and never allocated; [.note.GNU-stack] is the M1 case *)
  | Align of { boundary : int }
      (** always a byte count. [.align 16] on x86 and [.balign 4] on ARM normalize to the same
          constructor with *different values*: the dialect difference is which unit the surface
          syntax denotes, not the value. *)
  | Global of { name : string }
  | Sym_type of { name : string; kind : sym_kind }
  | Sym_size of { name : string; size : Expr.t }
      (** not metadata: it exercises the current-location operator and symbol-relative
          subtraction, and lands in the symbol record and the link map *)
  | Target_state of { name : string; argument : string }

let pp ppf = function
  | Section { name; perms } -> Fmt.pf ppf "section %s %a" name Perms.pp perms
  | Declared_section { name } -> Fmt.pf ppf "declared-section %s" name
  | Align { boundary } -> Fmt.pf ppf "align %d" boundary
  | Global { name } -> Fmt.pf ppf "globl %s" name
  | Sym_type { name; kind } -> Fmt.pf ppf "type %s %s" name (sym_kind_name kind)
  | Sym_size { name; size } -> Fmt.pf ppf "size %s %a" name Expr.pp size
  | Target_state { name; argument } -> Fmt.pf ppf "target-state %s %s" name argument
