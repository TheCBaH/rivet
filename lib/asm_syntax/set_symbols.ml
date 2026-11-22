(* [.set]/[.equ] and [name = expr] constant assignments.

   Like numeric local labels this runs on statement lines before a target sees an
   operand, so every later use is an ordinary constant and instruction selection
   sees the value (a short immediate form is chosen exactly as for a literal).
   The assignment itself is consumed. Only an absolute right-hand side is
   supported: the value must fold to an integer using the symbols assigned so
   far. A right-hand side that names a label or [.] is refused rather than
   guessed at, and so is applying [.globl], [.weak], [.type] and the like to an
   assigned name, since an absolute symbol is not emitted. *)

open Rivet_foundation
open Rivet_asm_core

type error = { message : string; span : Span.t }

let error_code (_ : error) = "simplify.assignment"
let pp_error ppf (e : error) = Fmt.string ppf e.message

let symbol_directives =
  [ ".globl"; ".global"; ".weak"; ".local"; ".type"; ".hidden"; ".protected"; ".internal" ]

let substitute table (s : Token.slice) =
  List.map
    (fun t ->
      match Token.kind t with
      | Token.Ident n -> (
          match Hashtbl.find_opt table n with
          | Some v -> Token.make (Token.Int v) (Token.span t)
          | None -> t)
      | _ -> t)
    s

let env table = { Expr.no_env with Expr.lookup = (fun n -> Hashtbl.find_opt table n) }

let run lines =
  let table : (string, Bigint.t) Hashtbl.t = Hashtbl.create 8 in
  let errors = ref [] in
  let err span fmt = Fmt.kstr (fun message -> errors := { message; span } :: !errors) fmt in
  let assign name span value =
    match Expr.absolute (env table) value with
    | Ok v ->
        Hashtbl.replace table name v;
        true
    | Error _ ->
        err span "%s: only a constant expression is supported (no labels or '.')" name;
        false
  in
  let line (l : Statement.line) =
    List.iter
      (function
        | Statement.Named (span, n) when Hashtbl.mem table n ->
            err span "%s is an absolute symbol set by .set and cannot be a label" n
        | _ -> ())
      l.Statement.labels;
    let statement =
      match l.Statement.statement with
      | Statement.Directive { name = (".set" | ".equ") as d; arguments = [ [ n ]; value ]; span }
        -> (
          match Token.kind n with
          | Token.Ident name -> (
              match Parse_lines.parse_expression (substitute table value) with
              | Ok e -> if assign name span e then Statement.Empty else l.Statement.statement
              | Error _ ->
                  err span "%s: not an expression" d;
                  l.Statement.statement)
          | _ ->
              err span "%s needs a symbol name" d;
              l.Statement.statement)
      | Statement.Directive { name = (".set" | ".equ") as d; span; _ } ->
          err span "%s needs a symbol name and a value" d;
          l.Statement.statement
      | Statement.Assignment { name; value; span } ->
          if assign name span value then Statement.Empty else l.Statement.statement
      | Statement.Directive { name; arguments; span } when Hashtbl.length table > 0 ->
          (if List.mem name symbol_directives then
             match arguments with
             | [ [ t ] ] | [ [ t ]; _ ] -> (
                 match Token.kind t with
                 | Token.Ident n when Hashtbl.mem table n ->
                     err span "%s %s: an absolute symbol set by .set is not emitted" name n
                 | _ -> ())
             | _ -> ());
          Statement.Directive { name; arguments = List.map (substitute table) arguments; span }
      | Statement.Instruction { mnemonic; operands; span } when Hashtbl.length table > 0 ->
          Statement.Instruction { mnemonic; operands = List.map (substitute table) operands; span }
      | s -> s
    in
    { l with Statement.statement }
  in
  let out = List.map line lines in
  (out, List.rev !errors)

let diagnostic_of_error (e : error) =
  Diagnostic.of_error ~origin:(Origin.text e.span) ~code:error_code ~pp:pp_error e
