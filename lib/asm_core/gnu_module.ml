(* A normalized module as GNU assembler source (docs/design.md §4.3).

   The same typed module the direct pipeline lowers is printed here for GNU as,
   so an independent assembler can be asked what the project's own encoder was
   asked, with no second producer of directives: this is the only text exporter,
   and it prints exactly what {!Normalized_ast} holds - sections with their
   permissions, byte alignment, bindings, symbol types and sizes, exact data
   expressions, and the labels and instructions in order.

   It is not [Normalized_ast.pp], whose output is a diagnostic dump (the
   directive names there are this project's, not GNU's). What differs between
   GNU dialects is a parameter: the character that introduces a section or
   symbol type ([%] on AArch64, where [@] starts a comment on some targets, and
   [@] on x86), and how one instruction is spelled, because a symbolic operand
   with a relocation modifier is written differently by every target.

   CFI is not represented in {!Directive.t}, so none is printed: the module
   carries no frame description to lose, and a producer that wants one needs the
   directive constructor first. *)

open Foundation

type syntax = { type_char : char  (** introduces [function], [object], [progbits], [nobits] *) }

(* A numeric local label reaches the module as the generated symbol [#L<n>#<k>] (the [k]th
   definition of [n:]), named so that no dialect can read it and no written name can collide
   with it. GNU source spells it as a [.L] local symbol instead. *)
let symbol s =
  match String.split_on_char '#' s with
  | [ ""; l; k ] when String.length l > 1 && l.[0] = 'L' ->
      Printf.sprintf ".Lrivet.%s.%s" (String.sub l 1 (String.length l - 1)) k
  | _ -> s

(* GNU spelling of an expression. A relocation modifier is the target's: [modifier] prints one,
   and {!expr} is AArch64's [:m:a]. *)
let rec expr_with ~modifier ppf (e : Expr.t) =
  let expr = expr_with ~modifier in
  match e with
  | Expr.Const v -> Bigint.pp ppf v
  | Expr.Symbol s -> Fmt.string ppf (symbol s)
  | Expr.Current_location -> Fmt.string ppf "."
  | Expr.Local_ref (n, d) -> Fmt.pf ppf "%d%s" n (match d with `Back -> "b" | `Forward -> "f")
  | Expr.Binary (op, a, b) -> Fmt.pf ppf "(%a %s %a)" expr a (Expr.binop_name op) expr b
  | Expr.Unary (op, a) -> Fmt.pf ppf "%s%a" (Expr.unop_name op) expr a
  | Expr.Modifier (m, a) -> modifier expr ppf m a

let expr = expr_with ~modifier:(fun expr ppf m a -> Fmt.pf ppf ":%s:%a" m expr a)

(* [e] with every symbol as {!symbol} spells it, for a target whose operand printer is not
   built on {!expr_with}. *)
let rec rename (e : Expr.t) : Expr.t =
  match e with
  | Expr.Symbol s -> Expr.Symbol (symbol s)
  | Expr.Binary (op, a, b) -> Expr.Binary (op, rename a, rename b)
  | Expr.Unary (op, a) -> Expr.Unary (op, rename a)
  | Expr.Modifier (m, a) -> Expr.Modifier (m, rename a)
  | Expr.Const _ | Expr.Current_location | Expr.Local_ref _ -> e

let flags (p : Perms.t) =
  (if p.Perms.read then "a" else "")
  ^ (if p.Perms.write then "w" else "")
  ^ if p.Perms.execute then "x" else ""

let rec directive syntax ppf (d : Directive.t) =
  let t = syntax.type_char in
  match d with
  | Directive.Section { name; perms; nobits } ->
      Fmt.pf ppf "\t.section\t%s,\"%s\",%c%s" name (flags perms) t
        (if nobits then "nobits" else "progbits")
  | Directive.Declared_section { name } -> Fmt.pf ppf "\t.section\t%s,\"\",%cprogbits" name t
  | Directive.Align { boundary; fill; max_skip } -> (
      Fmt.pf ppf "\t.balign\t%d" boundary;
      match (fill, max_skip) with
      | None, None -> ()
      | _ ->
          Fmt.pf ppf ",%a" Fmt.(option int) fill;
          Option.iter (Fmt.pf ppf ",%d") max_skip)
  | Directive.Visibility { name; visibility } ->
      Fmt.pf ppf "\t.%s\t%s" (Directive.visibility_name visibility) name
  | Directive.Push_section (Directive.Section { name; perms; nobits }) ->
      Fmt.pf ppf "\t.pushsection\t%s,\"%s\",%c%s" name (flags perms) t
        (if nobits then "nobits" else "progbits")
  | Directive.Push_section (Directive.Declared_section { name }) ->
      Fmt.pf ppf "\t.pushsection\t%s,\"\",%cprogbits" name t
  | Directive.Push_section other -> directive syntax ppf other
  | Directive.Pop_section -> Fmt.string ppf "\t.popsection"
  | Directive.Previous_section -> Fmt.string ppf "\t.previous"
  | Directive.Global { name } -> Fmt.pf ppf "\t.globl\t%s" name
  | Directive.Weak { name } -> Fmt.pf ppf "\t.weak\t%s" name
  | Directive.Local { name } -> Fmt.pf ppf "\t.local\t%s" name
  | Directive.Common { name; size; align } -> Fmt.pf ppf "\t.comm\t%s,%d,%d" name size align
  | Directive.Sym_type { name; kind } ->
      Fmt.pf ppf "\t.type\t%s,%c%s" name t
        (match kind with
        | Directive.Function -> "function"
        | Directive.Object -> "object"
        | Directive.Notype -> "notype")
  | Directive.Sym_size { name; size } -> Fmt.pf ppf "\t.size\t%s,%a" name expr size
  | Directive.Data { width; values } ->
      Fmt.pf ppf "\t%s\t%a"
        (if width = 1 then ".byte" else Printf.sprintf ".%dbyte" width)
        Fmt.(list ~sep:(any ",") expr)
        values
  | Directive.Ascii { bytes } ->
      Fmt.pf ppf "\t.byte\t%a" Fmt.(list ~sep:(any ",") expr) (Directive.ascii_values bytes)
  | Directive.Zero { length } -> Fmt.pf ppf "\t.zero\t%d" length
  | Directive.Cfi { name; argument } ->
      if argument = "" then Fmt.pf ppf "\t%s" name else Fmt.pf ppf "\t%s\t%s" name argument
  | Directive.Target_state { name; arguments } ->
      Fmt.pf ppf "\t%s\t%s" name (String.concat ", " arguments)

let item syntax ~instruction ppf (i : _ Normalized_ast.item) =
  match i with
  | Normalized_ast.Label { name; _ } -> Fmt.pf ppf "%s:" (symbol name)
  | Normalized_ast.Directive { directive = d; _ } -> directive syntax ppf d
  | Normalized_ast.Instruction { insn; _ } -> Fmt.pf ppf "\t%a" instruction insn

(* A module whose every item is a label, a directive or an instruction the
   target can spell. *)
let pp syntax ~instruction ppf (m : _ Normalized_ast.module_) =
  Fmt.pf ppf "@[<v>%a@]@." Fmt.(list ~sep:cut (item syntax ~instruction)) m.Normalized_ast.items

let to_string syntax ~instruction m = Fmt.str "%a" (pp syntax ~instruction) m
