(* The source AST (.ai/asm_plan.md §4.2): the public result of parsing.

   Not tokens, and not the shallow statement representation - those are private
   frontend intermediates. This is the first boundary a direct producer may
   construct, so it is a complete representation of the file: labels,
   assignments, directives and target surface instructions, with the origin each
   came from.

   Parameterized over the target's surface instruction type rather than
   functorized over TARGET. A container holding an ['sinsn] does not need an
   interface to hold it, and keeping it parameter-shaped is what lets this
   package sit *below* target_intf in the dependency DAG (§5.1's one-way rule)
   instead of beside it. *)

open Foundation

type 'sinsn item =
  | Label of { name : string; origin : Origin.t }
  | Directive of { name : string; arguments : Token.slice list; origin : Origin.t }
      (** arguments stay as token slices here: what they mean is decided by simplify, which knows
          the directive, and by the target, which knows its own state directives *)
  | Assignment of { name : string; value : Expr.t; origin : Origin.t }
  | Instruction of { insn : 'sinsn; origin : Origin.t }

type 'sinsn module_ = { unit_name : string; items : 'sinsn item list }

let origin_of = function
  | Label { origin; _ }
  | Directive { origin; _ }
  | Assignment { origin; _ }
  | Instruction { origin; _ } ->
      origin

let pp_item pp_insn ppf = function
  | Label { name; _ } -> Fmt.pf ppf "label %s" name
  | Directive { name; arguments; _ } ->
      Fmt.pf ppf "directive %s%a" name
        Fmt.(list ~sep:nop (fun ppf s -> Fmt.pf ppf " [%s]" (Token.slice_text s)))
        arguments
  | Assignment { name; value; _ } -> Fmt.pf ppf "assign %s = %a" name Expr.pp value
  | Instruction { insn; _ } -> Fmt.pf ppf "insn %a" pp_insn insn

let pp pp_insn ppf m =
  Fmt.pf ppf "@[<v>source %s@,%a@]" m.unit_name Fmt.(list ~sep:cut (pp_item pp_insn)) m.items
