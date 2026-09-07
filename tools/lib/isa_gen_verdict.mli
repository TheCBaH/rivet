(** Classifies {!Isa_generated_case.verdict} for one POSITIVE
    (non-negative) pilot case whose GAS side has already run: wrong-byte,
    wrong-form and unexpected-relocation controls, plus actionable minimal
    failure reproduction.

    Every current pilot case is positive -
    {!Isa_generated_case.case.negative} is [false] throughout - so a real
    negative-case classification (row 8, [Negative_case_accepted])
    has no driving example yet and is left to whichever future task first adds
    one, rather than guessed at here without real data. {!classify} therefore
    only ever produces rows 1 through 5 of the frozen eight-row table; a
    caller must not call it for a [negative = true] case.

    This module compares RAW ASSEMBLED BYTES directly - the same
    [case.rendered_source] handed to both tools, bytes compared byte-for-byte
    - rather than re-deriving "which form did each tool pick" the way
    {!Isa_gen_oracle.observed_form_check} does against the case's own
    normalized encoding. GAS may select a documented
    canonical alternative for the SAME textual line (three of the twelve
    x86 pilot entries measurably do), so "did GAS pick
    the intended form" and "do the two tools agree on the bytes for this
    exact input" are different, complementary questions - {!Isa_gen_oracle}
    already answers the first; this module answers the second, which is the
    actual regression gate ("Byte equality alone
    does not prove form coverage" - and conversely, form mismatch alone is not
    a byte-comparison failure either). *)

type gas_result =
  | Gas_assembled of string  (** hex bytes, {!Hex_dump.of_bytes} format *)
  | Gas_rejected of string  (** trimmed diagnostic *)

type ours_result = Ours_assembled of string | Ours_rejected of string

type outcome =
  | Gas_only_rejected of string
      (** GAS itself rejected the case; "ours" is never invoked - nothing to
          compare against. *)
  | Gas_unexpected_relocation of string list
      (** GAS accepted, but {!Isa_gen_oracle}'s own [.text] relocation check
          fired ({!Isa_gen_oracle.Unexpected_relocation}) - "ours" is never
          invoked, since the committed bytes are an unresolved placeholder,
          not a real encoding to compare against - never comparing
          unresolved object placeholders with our bound image. Carries the
          raw relocation-table lines as evidence. *)
  | Both_ran of { gas_hex : string; ours : ours_result }
      (** GAS accepted with no relocation; {!Isa_gen_ours.run} ran against the
          exact same [case.rendered_source]. *)

val known_syntax_gap : case_id:string -> diagnostic:string -> bool
(** The explicit, reviewed allowlist of pilot [case_id]s whose "ours"
    rejection is a documented syntax gap already measured and explained (see
    [Isa_gen_verdict.ml]'s own comment for the evidence behind each entry),
    not a genuine regression - checked against BOTH the case id and a
    substring of the actual rejection diagnostic, so a same-case rejection for
    a DIFFERENT, unreviewed reason is never silently absorbed into the same
    excuse. [false] for every case not on this list (or whose diagnostic no
    longer matches), so an unexpected rejection defaults to
    {!Isa_generated_case.Regression} in {!classify} rather than a silent
    pass. *)

val classify : known_syntax_gap:bool -> outcome -> Isa_generated_case.verdict
(** - {!Gas_only_rejected}: {!Isa_generated_case.Gas_rejected_valid_case} (row 5).
    - {!Gas_unexpected_relocation}: {!Isa_generated_case.Byte_mismatch} - the
      frozen eight-row table has no dedicated row for a should-be-relocation-
      free canonical case that turns out not to be one; row 2's own wording
      ("hard failure, including for a frontier case") is the closest fit, and
      a real per-relocation comparison discipline belongs in a future task,
      not a ninth row invented here. [known_syntax_gap] is ignored.
    - {!Both_ran} with [ours = Ours_assembled o]: byte-for-byte [o] against
      [gas_hex] (trimmed, case-folded - both are already lowercase
      {!Hex_dump.of_bytes} text, so this is a robustness margin, not a real
      normalization) - {!Isa_generated_case.Pass} if equal (row 1), else
      {!Isa_generated_case.Byte_mismatch} (row 2) UNCONDITIONALLY, even for a
      case [known_syntax_gap] would otherwise excuse - "hard
      failure, including frontier cases" is checked before [known_syntax_gap]
      is even consulted.
    - {!Both_ran} with [ours = Ours_rejected _]: {!Isa_generated_case.Frontier_gap}
      (row 4) if [known_syntax_gap] is [true], else
      {!Isa_generated_case.Regression} (row 3) - the conservative default
      ("do not assume a difference is intentional because it is
      small"): an UNEXPLAINED rejection of a case selected because it
      believed the form was already supported is a hard failure until someone
      reviews and records why, never a silent pass. *)
