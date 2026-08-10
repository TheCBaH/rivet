(* Section permissions, as metadata (.ai/asm_plan.md §9).

   Recorded, never applied: no production package changes memory protection.
   These travel from a section directive through the lowered module into the
   image's segments, where an embedding host may act on them. *)

type t = { read : bool; write : bool; execute : bool }

let rx = { read = true; write = false; execute = true }
let ro = { read = true; write = false; execute = false }
let rw = { read = true; write = true; execute = false }
let none = { read = false; write = false; execute = false }
let executable t = t.execute
let writable t = t.write

(* Three characters, with [-] for absent, per asm/docs/contracts.md §1.4. Fixed
   width matters: the lowered and image dumps put permissions in a column, and a
   variable-width field would make every downstream diff show alignment noise
   rather than the change. *)
let to_string t =
  (if t.read then "r" else "-") ^ (if t.write then "w" else "-") ^ if t.execute then "x" else "-"

let pp ppf t = Fmt.string ppf (to_string t)
