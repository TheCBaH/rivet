(* Diagnostic provenance, per .ai/asm_plan.md §4.1.

   This is deliberately not debugger or source-map information: it never enters
   a lowered module or a linked image, and exists only so a diagnostic can say
   where a construct came from. Parsing is one entry point among several, so an
   origin must be able to describe a programmatic producer or an internally
   synthesized value just as well as a span of text. *)

type t =
  | Text of Span.t
  | Producer of { tool : string; item : string option }
  | Synthesized of { pass : string; parent : t option }

let text span = Text span
let producer ?item ~tool () = Producer { tool; item }
let synthesized ?parent ~pass () = Synthesized { pass; parent }

let rec pp ppf = function
  | Text span -> Span.pp ppf span
  | Producer { tool; item } -> Fmt.pf ppf "<%s%a>" tool Fmt.(option (any ":" ++ string)) item
  | Synthesized { pass; parent } ->
      Fmt.pf ppf "<synthesized by %s%a>" pass Fmt.(option (any " from " ++ pp)) parent

let to_string = Fmt.to_to_string pp

(* The structural dump, for expect tests. [pp] renders a [Synthesized] chain as
   one prose line and drops a [Text] span down to its location, so it cannot
   show that a pass recorded the wrong parent or lost one.

   The constructors carry inline records, which cannot be projected as values,
   so each is dumped through a tuple of its fields — the printed field names
   still match the declared ones. *)
let field name prj pp_v = Fmt.Dump.field ~label:Fmt.string name prj pp_v

let pp_producer_repr =
  Fmt.Dump.record
    [ field "tool" fst Fmt.Dump.string; field "item" snd (Fmt.Dump.option Fmt.Dump.string) ]

let rec pp_repr ppf = function
  | Text span -> Fmt.pf ppf "@[<2>Text@ %a@]" Span.pp_repr span
  | Producer { tool; item } -> Fmt.pf ppf "@[<2>Producer@ %a@]" pp_producer_repr (tool, item)
  | Synthesized { pass; parent } ->
      Fmt.pf ppf "@[<2>Synthesized@ %a@]" pp_synthesized_repr (pass, parent)

and pp_synthesized_repr ppf fields =
  Fmt.Dump.record
    [
      field "pass" fst Fmt.Dump.string;
      (* [Fmt.Dump.option] adds no parentheses, so a nested constructor would
         print as [Some Text {...}]. *)
      field "parent" snd (Fmt.Dump.option (Fmt.parens pp_repr));
    ]
    ppf fields

(* The nearest enclosing text span, if any: a synthesized value inherits the
   location of whatever it was derived from. *)
let rec span = function
  | Text s -> Some s
  | Producer _ -> None
  | Synthesized { parent = Some p; _ } -> span p
  | Synthesized { parent = None; _ } -> None
