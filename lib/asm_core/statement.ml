(* The common statement representation (.ai/asm_plan.md §4.7).

   A private frontend intermediate, not the public source AST: operands are
   still token slices here, and only a target parser can turn them into surface
   operands. Keeping this type separate from the source AST is what stops "the
   parser understands x86 addressing" from becoming true by accident. *)

open Foundation

type label = Named of Span.t * string | Numeric of Span.t * int

type statement =
  | Empty
  | Instruction of { mnemonic : string; operands : Token.slice list; span : Span.t }
  | Directive of { name : string; arguments : Token.slice list; span : Span.t }
  | Assignment of { name : string; value : Expr.t; span : Span.t }

type line = { labels : label list; statement : statement }

let label_span = function Named (s, _) -> s | Numeric (s, _) -> s

let pp_label ppf = function
  | Named (_, s) -> Fmt.pf ppf "%s:" s
  | Numeric (_, n) -> Fmt.pf ppf "%d:" n

let pp_statement ppf = function
  | Empty -> Fmt.string ppf "(empty)"
  | Instruction { mnemonic; operands; _ } ->
      Fmt.pf ppf "insn %s%a" mnemonic
        Fmt.(list ~sep:nop (fun ppf s -> Fmt.pf ppf " [%a]" Token.pp_slice s))
        operands
  | Directive { name; arguments; _ } ->
      Fmt.pf ppf "dir %s%a" name
        Fmt.(list ~sep:nop (fun ppf s -> Fmt.pf ppf " [%a]" Token.pp_slice s))
        arguments
  | Assignment { name; value; _ } -> Fmt.pf ppf "assign %s = %a" name Expr.pp value

let pp_line ppf { labels; statement } =
  Fmt.pf ppf "@[<h>%a%a@]"
    Fmt.(list ~sep:(any " ") pp_label)
    labels
    (fun ppf s ->
      match (labels, s) with
      | [], _ -> pp_statement ppf s
      | _, Empty -> ()
      | _ -> Fmt.pf ppf " %a" pp_statement s)
    statement

let span_of_line { labels; statement } =
  match (labels, statement) with
  | l :: _, _ -> Some (label_span l)
  | [], Instruction { span; _ } | [], Directive { span; _ } | [], Assignment { span; _ } ->
      Some span
  | [], Empty -> None
