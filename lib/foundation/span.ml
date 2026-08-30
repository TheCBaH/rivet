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
     small, and this keeps the API total for any offset a caller hands us. Not
     for a caller that builds one span per token over a whole file - see
     [line_index]/[of_offset_indexed], which a lexer's per-token hot loop uses
     instead, since a rescan-from-zero on every token is quadratic in file
     size. *)
  let rec go i line column =
    if i >= offset || i >= String.length source.contents then (line, column)
    else if source.contents.[i] = '\n' then go (i + 1) (line + 1) 1
    else go (i + 1) line (column + 1)
  in
  let line, column = go 0 1 1 in
  { source; offset; length; line; column }

(* {1 Indexed line/column lookup}

   [line_index] is the offset of each line's first character - line 1 starts
   at 0, and a line following a ['\n'] at position [i] starts at [i + 1] - the
   same convention [of_offset]'s [go] counts by, so [of_offset_indexed] finds
   exactly the same (line, column) it would, in O(log n) instead of O(n). *)
type line_index = int array

let build_line_index (source : source) : line_index =
  let text = source.contents in
  let n = String.length text in
  let starts = ref [ 0 ] in
  for i = 0 to n - 1 do
    if text.[i] = '\n' then starts := (i + 1) :: !starts
  done;
  Array.of_list (List.rev !starts)

(* The greatest line start [<= offset], found by binary search: that line is
   the one containing [offset]. [line_index] is never empty (it always holds
   at least line 1's start, 0), so the search range is never empty either. *)
let of_offset_indexed ~source ~(line_index : line_index) ~offset ~length =
  let offset' = min offset (String.length source.contents) in
  let lo = ref 0 and hi = ref (Array.length line_index - 1) in
  while !lo < !hi do
    let mid = (!lo + !hi + 1) / 2 in
    if line_index.(mid) <= offset' then lo := mid else hi := mid - 1
  done;
  let line = !lo + 1 in
  let column = offset' - line_index.(!lo) + 1 in
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

(* The exact source text a span covers. Directive arguments are raw token
   slices - GAS directive syntax is not one grammar - so a handler that needs
   the spelling back, like [.arch armv7-a], reads it from here rather than from
   a reconstruction that could differ from what was written. *)
let text t =
  let n = String.length t.source.contents in
  let start = min t.offset n in
  String.sub t.source.contents start (min t.length (n - start))

(* The text of the line containing the span, without its terminator. *)
let line_text t =
  let s = t.source.contents in
  let n = String.length s in
  let rec back i = if i <= 0 then 0 else if s.[i - 1] = '\n' then i else back (i - 1) in
  let rec fwd i = if i >= n then n else if s.[i] = '\n' then i else fwd (i + 1) in
  let start = back (min t.offset n) in
  String.sub s start (fwd start - start)
