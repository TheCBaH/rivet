(* Source positions. Kept free of host paths: a [source] carries a caller-chosen
   name, so diagnostics never leak a filesystem layout into expect output
   (.ai/asm_plan.md §3.7). *)

type source = { name : string; contents : string }
type t = { source : source; offset : int; length : int; line : int; column : int }

let source ~name ~contents = { name; contents }
let name s = s.name
let contents s = s.contents
let make ~source ~offset ~length ~line ~column = { source; offset; length; line; column }

let of_offset ~source ~offset ~length =
  (* Recount from the start rather than threading a mutable position: inputs are
     small, and this keeps the API total for any offset a caller hands us. *)
  let rec go i line column =
    if i >= offset || i >= String.length source.contents then (line, column)
    else if source.contents.[i] = '\n' then go (i + 1) (line + 1) 1
    else go (i + 1) line (column + 1)
  in
  let line, column = go 0 1 1 in
  { source; offset; length; line; column }

let source_of t = t.source
let offset t = t.offset
let length t = t.length
let line t = t.line
let column t = t.column

(* [pp] is the primary renderer and [to_string] derives from it, so there is one
   definition of what a span looks like in diagnostics and in expect output.
   Only literal text is used — never a breakable space — so Format's margin can
   never reflow a rendering that a test has pinned. *)
let pp ppf t = Fmt.pf ppf "%s:%d:%d" t.source.name t.line t.column
let pp_source ppf s = Fmt.pf ppf "%s (%d bytes)" s.name (String.length s.contents)
let to_string = Fmt.to_to_string pp

(* Structural dumps for expect tests. [pp] answers "where is this", which is all
   a diagnostic needs; [pp_repr] answers "what is stored", which is what a test
   of the position arithmetic has to see — [line]/[column] are derived from
   [offset] by a recount, so a test that only compared renderings could not tell
   a stale [line] from a wrong [offset]. Source contents are deliberately
   summarized rather than dumped: a span carries the whole file. *)
let field name prj pp_v = Fmt.Dump.field ~label:Fmt.string name prj pp_v

let pp_source_repr =
  Fmt.Dump.record
    [
      field "name" (fun s -> s.name) Fmt.Dump.string;
      field "contents" (fun s -> String.length s.contents) (fun ppf n -> Fmt.pf ppf "<%d bytes>" n);
    ]

let pp_repr =
  Fmt.Dump.record
    [
      field "source" (fun t -> t.source) pp_source_repr;
      field "offset" (fun t -> t.offset) Fmt.int;
      field "length" (fun t -> t.length) Fmt.int;
      field "line" (fun t -> t.line) Fmt.int;
      field "column" (fun t -> t.column) Fmt.int;
    ]

(* The text of the line containing the span, without its terminator. *)
let line_text t =
  let s = t.source.contents in
  let n = String.length s in
  let rec back i = if i <= 0 then 0 else if s.[i - 1] = '\n' then i else back (i - 1) in
  let rec fwd i = if i >= n then n else if s.[i] = '\n' then i else fwd (i + 1) in
  let start = back (min t.offset n) in
  String.sub s start (fwd start - start)
