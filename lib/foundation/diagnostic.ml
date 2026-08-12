(* Diagnostics.

   Rendering is deterministic and free of host paths, locale, and process state
   (.ai/asm_plan.md §3.7), because these strings go straight into expect output
   that must match byte-for-byte across native, js_of_ocaml, and Melange. *)

type severity = Error | Warning | Note

type t = {
  severity : severity;
  code : string;
  message : string;
  origin : Origin.t;
  notes : string list;
}

let make ?(notes = []) ~severity ~code ~message ~origin () =
  { severity; code; message; origin; notes }

(* The only constructor (see asm/docs/errors.md). A diagnostic is not something
   a failure site *writes*; it is the rendering of a typed error value, and the
   domain supplies both strings. [make] above is private to this module - the
   interface does not export it - so there is no way in that takes a
   hand-written message.

   [code] is a function rather than a string because the whole point is that the
   diagnostic code is a property of the error - the ~40 literals it replaces
   were an unenforced taxonomy that nothing stopped drifting.

   Rendering goes through [Fmt.to_to_string], i.e. the same path every other
   printer here uses, so a domain printer that emits a breakable space is
   subject to the same margin rules as the rest of this module. *)
let of_error ?notes ?(severity = Error) ~origin ~code ~pp e =
  make ?notes ~severity ~code:(code e) ~message:(Fmt.to_to_string pp e) ~origin ()

let severity t = t.severity
let code t = t.code
let message t = t.message
let origin t = t.origin
let notes t = t.notes

let pp_severity ppf s =
  Fmt.string ppf (match s with Error -> "error" | Warning -> "warning" | Note -> "note")

let severity_to_string = Fmt.to_to_string pp_severity

(* A caret line under the offending span. Tabs in the source are echoed as tabs
   in the marker so the caret stays aligned under any tab width. *)
let caret_line span =
  let text = Span.line_text span in
  let col = Span.column span in
  let width = max 1 (Span.length span) in
  let pad =
    String.init (col - 1) (fun i -> if i < String.length text && text.[i] = '\t' then '\t' else ' ')
  in
  pad ^ String.make width '^'

let pp_header ppf t =
  Fmt.pf ppf "%a: %a[%s]: %s" Origin.pp t.origin pp_severity t.severity t.code t.message

let pp_snippet ppf span = Fmt.pf ppf "  %s@,  %s" (Span.line_text span) (caret_line span)
let pp_note ppf note = Fmt.pf ppf "  note: %s" note

(* A vertical box whose only break hints are explicit cuts: nothing here uses a
   breakable space, so Format's right margin can never reflow the rendering.
   That matters because these strings are pinned byte-for-byte by expect tests
   and must agree under native OCaml, js_of_ocaml, and Melange. Message and
   snippet text goes through [%s], which passes [@] characters through
   untouched. *)
let pp ppf t =
  Fmt.pf ppf "@[<v>%a%a%a@]" pp_header t
    Fmt.(option (any "@," ++ pp_snippet))
    (Origin.span t.origin)
    Fmt.(list ~sep:nop (any "@," ++ pp_note))
    t.notes

let pp_all ppf ds = Fmt.pf ppf "@[<v>%a@]" Fmt.(list ~sep:cut pp) ds
let render = Fmt.to_to_string pp
let render_all = Fmt.to_to_string pp_all

(* The structural dump. [pp] is the user-facing rendering and deliberately drops
   detail — an origin collapses to a location, and the severity/code/message
   line reads as prose — so a test that only pinned [render] could not tell a
   misattributed origin from a correctly attributed one that happens to print
   the same. *)
let field name prj pp_v = Fmt.Dump.field ~label:Fmt.string name prj pp_v

let pp_repr =
  Fmt.Dump.record
    [
      field "severity" (fun t -> t.severity) pp_severity;
      field "code" (fun t -> t.code) Fmt.Dump.string;
      field "message" (fun t -> t.message) Fmt.Dump.string;
      field "origin" (fun t -> t.origin) Origin.pp_repr;
      field "notes" (fun t -> t.notes) (Fmt.Dump.list Fmt.Dump.string);
    ]
