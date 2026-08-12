(* Numeric local labels and their [1b]/[1f] references (.ai/asm_plan.md §4.6).

   This runs on [Statement.line list] - before the target sees an operand - and
   that placement is the whole design. By the time an operand has been through
   [T.parse_operands] it lives inside the target's abstract [Surface.t], and
   [TARGET] exposes no expression traversal, so shared code could not find a
   [Local_ref] to rewrite even though the feature has nothing to do with any
   architecture. Here the pipeline still owns the tokens.

   Resolution turns each definition into an ordinary generated symbol and each
   reference into an ordinary [Ident] naming it. Nothing downstream learns that
   local labels exist: layout, fixups and the linker see a symbol like any
   other.

   Scope is source order across the whole file, as GAS does - a numeric label is
   not section-scoped, and a reference may legitimately cross a section change.
   What is restricted is *relaxation*, which stays intra-section because a
   cross-section displacement is not knowable before binding. *)

open Foundation

(* The generated name uses ['#'], which no dialect admits in an identifier:
   [Lexical_profile.identifier_extra] is ['_'], ['.'] and ['$'] everywhere. So a
   generated name cannot collide with a written one by construction, and the
   check below is a backstop against that invariant changing rather than the
   thing that makes it true. *)
let generated n k = Printf.sprintf "#L%d#%d" n k
let is_generated s = String.length s > 0 && s.[0] = '#'

type def = { num : int; line : int; ordinal : int }

(* Every definition, in source order, numbered per label so that repeated
   definitions of [1:] are distinguishable. A repetition is legal and common -
   that is what makes them *local* - so this must not be an error. *)
let collect_defs lines =
  let defs = ref [] in
  let counts = Hashtbl.create 8 in
  List.iteri
    (fun i (l : Statement.line) ->
      List.iter
        (fun lab ->
          match lab with
          | Statement.Numeric (_, n) ->
              let k = try Hashtbl.find counts n with Not_found -> 0 in
              Hashtbl.replace counts n (k + 1);
              defs := { num = n; line = i; ordinal = k } :: !defs
          | Statement.Named _ -> ())
        l.Statement.labels)
    lines;
  List.rev !defs

(* Labels precede the statement on their line, so a definition on line [i] is
   *behind* a reference in that line's statement and a forward reference must
   look strictly past it. Getting this backwards makes [1: jmp 1f] a loop. *)
let resolve defs ~num ~dir ~line =
  let candidates = List.filter (fun d -> d.num = num) defs in
  match dir with
  | `Back -> List.fold_left (fun acc d -> if d.line <= line then Some d else acc) None candidates
  | `Forward -> List.find_opt (fun d -> d.line > line) candidates

(* Accumulated like the lexer's, and for the same reason carrying its own span:
   [run] collects every unresolved reference before returning, so the site that
   builds the error is not the site that reports it (asm/docs/errors.md §1). *)
type error_kind = [ `No_such_local of no_such_local | `Reserved_namespace of string ]

(* [direction] mirrors [Token.Local_label]'s own payload rather than inventing a
   second spelling of the same distinction. *)
and no_such_local = { number : int; direction : [ `Back | `Forward ] }

type error = { kind : error_kind; span : Span.t }

let pp_kind ppf : error_kind -> unit = function
  | `No_such_local { number; direction } ->
      Fmt.pf ppf "no %d%s: %s this reference" number
        (match direction with `Back -> "b" | `Forward -> "f")
        (match direction with `Back -> "before" | `Forward -> "after")
  | `Reserved_namespace n -> Fmt.pf ppf "%s uses the reserved local-label namespace" n

let kind_code : error_kind -> string = function
  | `No_such_local _ | `Reserved_namespace _ -> "parse.local-label"

let pp_error ppf (e : error) = pp_kind ppf e.kind
let error_code (e : error) = kind_code e.kind
let err kind span = { kind; span }

(* {1 Rewriting} *)

let rewrite_slice defs ~line ~errors (s : Token.slice) =
  List.map
    (fun t ->
      match Token.kind t with
      | Token.Local_label (n, dir) -> (
          match resolve defs ~num:n ~dir ~line with
          | Some d -> Token.make (Token.Ident (generated d.num d.ordinal)) (Token.span t)
          | None ->
              errors :=
                err (`No_such_local { number = n; direction = dir }) (Token.span t) :: !errors;
              t)
      | _ -> t)
    s

let rewrite_line defs ~line ~errors (l : Statement.line) =
  let slices = List.map (rewrite_slice defs ~line ~errors) in
  let labels =
    List.mapi
      (fun _ lab ->
        match lab with
        | Statement.Numeric (span, n) ->
            let k =
              (* The definition recorded for this line and number. There may be
                 several on one line only if the same number appears twice,
                 which GAS allows and which the ordinal already distinguishes. *)
              match List.find_opt (fun d -> d.num = n && d.line = line) defs with
              | Some d -> d.ordinal
              | None -> 0
            in
            Statement.Named (span, generated n k)
        | Statement.Named _ -> lab)
      l.Statement.labels
  in
  let statement =
    match l.Statement.statement with
    | Statement.Instruction { mnemonic; operands; span } ->
        Statement.Instruction { mnemonic; operands = slices operands; span }
    (* Directive arguments are token slices too, and a numeric reference is as
       legal in [.long 1f - 1b] as in a branch operand. *)
    | Statement.Directive { name; arguments; span } ->
        Statement.Directive { name; arguments = slices arguments; span }
    | (Statement.Empty | Statement.Assignment _) as s -> s
  in
  { Statement.labels; statement }

let collides lines =
  let written = ref [] in
  List.iter
    (fun (l : Statement.line) ->
      List.iter
        (fun lab ->
          match lab with
          | Statement.Named (span, n) when is_generated n -> written := (n, span) :: !written
          | _ -> ())
        l.Statement.labels)
    lines;
  !written

let run lines =
  let errors = ref [] in
  (* A written label that looks generated would make the two namespaces
     overlap. It cannot happen through the lexer, so this fires only if a
     dialect ever admits ['#'] in an identifier. *)
  List.iter
    (fun (n, span) -> errors := err (`Reserved_namespace n) span :: !errors)
    (collides lines);
  let defs = collect_defs lines in
  let out = List.mapi (fun i l -> rewrite_line defs ~line:i ~errors l) lines in
  (out, List.rev !errors)

(* The presentation of one accumulated error. Here rather than in the pipeline
   because the code and the message are this module's to name: a caller that
   spelled the code itself would be a second place for the taxonomy to drift
   (asm/docs/errors.md §2). *)
let diagnostic_of_error (e : error) =
  Diagnostic.of_error ~origin:(Origin.text e.span) ~code:error_code ~pp:pp_error e
