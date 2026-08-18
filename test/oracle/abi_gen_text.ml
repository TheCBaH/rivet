(* A small combinator for emitting GNU-assembler text (.ai/asm_plan.md M4
   Phase 3.2), deterministic and used instead of ad hoc string concatenation.

   [line] is intentionally thin - GAS itself does not care whether a given
   physical line is a label, a directive, an instruction or a comment; the
   distinction below is purely for the *generator's own* readability and for
   giving genuinely new code (the v3-specific insertions) real structure
   under [label]/[directive]/[insn]/[comment], rather than building it with
   [^].

   [verbatim_block] is the deliberate escape hatch for the *unchanged* v1
   logic: large sections of a v3 helper are byte-identical to the existing
   frozen [helpers/<profile>.s] (assembled with a different [ABI_VERSION]
   value, or wrapped in a [.if ABI_VERSION >= 3] block around it), and
   splitting an already-correct, already-reviewed block into one [Insn] per
   line by hand would be a second, needless transcription risk on top of the
   first. [verbatim_block] threads that text through the same [line list]
   representation (one [Raw] per physical line, so it is still inspectable
   and reorderable) rather than concatenating it separately. *)

type line = Raw of string | Blank

let label name = Raw (name ^ ":")
let directive d = Raw ("\t" ^ d)
let insn i = Raw ("\t" ^ i)
let comment c = Raw ("/* " ^ c ^ " */")

(* One [Raw] per physical line of [text], preserving its own indentation
   exactly - the source it is copied from already formats correctly. A
   trailing newline in [text] does not produce a spurious empty final line. *)
let verbatim_block text =
  String.split_on_char '\n' text
  |> (fun lines ->
       match List.rev lines with "" :: rest -> List.rev rest | _ -> lines)
  |> List.map (fun l -> Raw l)

let render lines =
  let buf = Buffer.create 8192 in
  List.iter
    (fun l ->
      (match l with Raw s -> Buffer.add_string buf s | Blank -> ());
      Buffer.add_char buf '\n')
    lines;
  Buffer.contents buf
