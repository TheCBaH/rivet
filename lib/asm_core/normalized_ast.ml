(* The normalized semantic AST (.ai/asm_plan.md §4.3).

   The preferred entry point for a compiler that wants the assembler to keep
   responsibility for form selection, pseudo lowering, fixups and layout - which
   is exactly what the CompCert adapter will be. It has no dependence on textual
   tokenization: directives are values, expressions are folded as far as they
   can honestly be folded, and instructions are canonical semantic operations
   rather than surface spellings. *)

open Foundation

type 'insn item =
  | Label of { name : string; origin : Origin.t }
  | Directive of { directive : Directive.t; origin : Origin.t }
  | Instruction of { insn : 'insn; origin : Origin.t }

type 'insn module_ = { unit_name : string; items : 'insn item list }

let pp_item pp_insn ppf = function
  | Label { name; _ } -> Fmt.pf ppf "label %s" name
  | Directive { directive; _ } -> Fmt.pf ppf "%a" Directive.pp directive
  | Instruction { insn; _ } -> Fmt.pf ppf "%a" pp_insn insn

let pp pp_insn ppf m =
  Fmt.pf ppf "@[<v>normalized %s@,%a@]" m.unit_name Fmt.(list ~sep:cut (pp_item pp_insn)) m.items
