(* The dialect configuration a target hands the lexer (.ai/asm_plan.md §4.6).

   This exists because the same character means opposite things in the four
   dialects this project must read at once: [#] introduces an immediate on
   ARM and AArch64 and a comment on x86, and [%] prefixes a register on x86 and
   is the modulo operator elsewhere. A single hard-coded assembly lexer would
   not fail loudly on that - it would parse [mov r0, #42] as [mov r0] followed
   by a comment, and produce a plausible wrong answer. *)

type t = {
  name : string;
  comment_introducers : string list;
      (** [#] on x86, [@] on ARM, [//] on AArch64. Matched longest-first, so a dialect can have
          both [//] and [/]. *)
  statement_separator : char option;
      (** [;] in GAS; unused by the M1 fixtures but part of the dialect *)
  immediate_sigil : char option;  (** [$] on x86, [#] on ARM and AArch64 *)
  register_sigil : char option;
      (** [%] on x86; [None] on ARM and AArch64, where registers are ordinary identifiers and only
          the target operand parser can tell [r12] from a symbol *)
  identifier_extra : char list;
      (** characters beyond letters and digits that may appear in a name *)
  modifier_syntax : string list;
      (** The relocation modifiers this dialect spells between colons: [lower16] and [upper16] on
          ARM, [lo12] on AArch64, none on x86. The *names* are declared rather than a bare "colons
          may wrap an identifier" switch, because [:] also terminates a label and GAS accepts two
          labels on one line - so [foo: bar: mov r0, r1] would otherwise lex [bar] as a modifier.
          Listing the names means the ambiguity is decided by the dialect rather than by whichever
          reading the scanner reaches first. *)
  percent_call_modifiers : string list;
      (** Relocation modifiers written as a percent-prefixed call, for example
          [%pcrel_hi(symbol)].  As with the colon-wrapped style, spellings are
          declared explicitly: an undeclared [%name] must remain a percent
          token followed by an identifier and fail in the ordinary grammar. *)
}

(* [-] is deliberately not an identifier character in any dialect, even though
   [.arch armv7-a] and [.section .note.GNU-stack] both contain one. GAS scans
   those directive arguments to end of line rather than as expressions, and
   admitting [-] into identifiers would silently turn [a-b] in an *expression*
   into a single symbol. Directive arguments are raw token slices for exactly
   this reason: the handler reassembles the spelling from the spans. *)
let gas_identifier_extra = [ '_'; '.'; '$' ]

(* The concrete profiles are *not* here. They live in the target packages, one
   per target, reached through [TARGET.lexical_profile].

   The first draft did define [x86], [arm] and [aarch64] in this file and it was
   wrong for a reason worth recording: a table of target names inside generic
   machinery is §3.7's violation in its mildest form, and the mild form is the
   one that spreads - the next function that needs "just one" target-specific
   decision has somewhere obvious to put it. The layer audit caught it, which is
   what the audit is for.

   So the only thing shared here is the record type, [gas_identifier_extra] -
   which is a property of the GAS dialect family rather than of any target - and
   the constructor below, whose labelled arguments make each target state its
   whole dialect explicitly rather than inherit a default it did not read. *)
let make ~name ~comment_introducers ?statement_separator ?immediate_sigil ?register_sigil
    ?(identifier_extra = gas_identifier_extra) ?(modifier_syntax = [])
    ?(percent_call_modifiers = []) () =
  {
    name;
    comment_introducers;
    statement_separator;
    immediate_sigil;
    register_sigil;
    identifier_extra;
    modifier_syntax;
    percent_call_modifiers;
  }

let pp ppf t =
  Fmt.pf ppf "%s: comments=%s immediate=%s register=%s modifiers=%s percent-call=%s" t.name
    (String.concat "," t.comment_introducers)
    (match t.immediate_sigil with None -> "none" | Some c -> String.make 1 c)
    (match t.register_sigil with None -> "none" | Some c -> String.make 1 c)
    (match t.modifier_syntax with [] -> "none" | ms -> String.concat "," ms)
    (match t.percent_call_modifiers with [] -> "none" | ms -> String.concat "," ms)
