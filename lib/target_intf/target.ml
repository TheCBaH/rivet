(* The TARGET signature (.ai/asm_plan.md §5.1).

   Signatures only. This package deliberately contains no implementation, so
   that every concrete target depends on a description it cannot influence, and
   so that the architecture-independent pipeline can be written against
   something that no target can widen locally.

   Every architecture-specific type stays abstract. What the shared code is
   allowed to know about a target is exactly what appears below: a lexical
   profile, a way to get from tokens to a surface instruction, three staged
   transformations, a codec, a fixup evaluator, padding, and directive
   handling. Everything else - source-AST construction, layout iteration,
   symbol processing, image building, diagnostics rendering - is written once.

   Four refinements to §5.1's sketch, each forced by something concrete:

   1. [parse_operands] replaces [parse_surface_operand]. Operands cannot be
      parsed one at a time: [stp x15, x30, [sp, #-16]!] has a [!] that binds to
      the whole preceding memory operand, and in [add r0, r1, lsl #2] the
      [lsl #2] is a modifier of the previous operand rather than an operand.
      Both decisions need the neighbours, so the whole comma-separated list is
      the unit. It takes the mnemonic too, because [ldm sp!, {r0, r1}] and
      [.word 1, 2] tokenize identically and only the mnemonic says which
      shape is expected.

   2. [encoding_alternatives] and [encode_form] collapse into [codec]. §5.1
      wrote alternative selection as a function returning a list, which is a
      shape no interpreter can inspect: the reason a form was chosen lives
      inside OCaml code. §4.9.1 requires the opposite, so alternatives are the
      codec's own [Alt] node, with priority, guard and cost as data. The shared
      encoder calls [Codec.encode]; [Codec.check] then reports unreachable
      alternatives and overlapping patterns statically, and [Codec.decode] is
      the same tree read backwards, so a target cannot encode and decode
      differently by accident.

   3. [decode] survives separately, because a decoder needs a *context* the
      codec node does not carry - the current mode, and the address, since
      whether some bytes are ARM or Thumb is not in the bytes.

   4. [handle_directive] returns a result rather than mutating: target state is
      a value threaded through lowering, which is what makes lowering a pure
      function of (state, instruction) and therefore replayable. §5.1's own
      constraint - "must not contain global linker state" - is easier to keep
      when the state cannot be reached except by threading. *)

open Foundation

(* What a target does with a directive it was handed. [Unhandled] is not an
   error and not silence: the caller (simplify) tries the target first for
   [.syntax]/[.arch]/[.fpu]/[.arm]-style state directives, and on [Unhandled]
   falls back to the common table. A target that answered "yes, ignored" to
   everything would make every misspelled directive assemble. *)
type ('state, 'insn) directive_result =
  | Handled of { state : 'state; emit : 'insn list }
      (** the directive changed target state, and possibly emitted instructions - [.arm] after
          [.thumb] does not, but a target macro directive would *)
  | Unhandled
  | Rejected of Diagnostic.t

module type TARGET = sig
  include Target_encode.ENCODE

  (* {1 Frontend}

     The three members that read source text, and the whole of the difference
     between this signature and {!Target_encode.ENCODE}. A target satisfies
     TARGET by adding them to the encoding half it already has. *)

  val lexical_profile : Asm_syntax.Lexical_profile.t
  (** comment introducers, statement separator, sigils. The lexer is shared; only its table is
      per-target, which is why [#] can start a comment on ARM and an immediate on x86 without two
      lexers existing. *)

  val parse_operands :
    mnemonic:string -> Asm_syntax.Token.slice list -> (Operand.t list, Diagnostic.t) result
  (** the hand-written per-target operand parser (§4.7). Menhir has already split the line into
      comma-separated slices; what those slices mean is decided here. *)

  (* {1 Directives} *)

  val handle_directive :
    name:string -> argument:string -> target_state -> (target_state, Instruction.t) directive_result
end

(* The architecture-erased view (§5.1's DRIVER). Its whole purpose is that the
   CLI and the internal linker never mention a target-specific type, so no
   existential escapes into them. Defined here rather than in [driver] because
   it is part of the same contract: it says which requests survive erasure. *)
module type DRIVER = sig
  val name : string
  val triple : string

  val assemble :
    ?entry:string ->
    unit_name:string ->
    source:Span.source ->
    unit ->
    (Image.laid_out, Diagnostic.t list) result
  (** Text in, laid-out image out - laid out but *not bound*: choosing addresses is the host's
      step and stays outside every target (§9). [Image.bind_image] is what turns the result into
      bytes at an address, and it is deliberately not part of this interface, because it is not
      target-specific at all.

      [?entry] names the entry symbol. It has to cross this boundary rather than live in the
      concrete pipeline, because every architecture-erased caller - the CLI, the differential
      gate, both browser adapters - would otherwise be stuck with the single-global inference,
      which names nothing as soon as a module declares a callee or a data object beside its entry.
      Omitting it keeps that inference as the fallback, which is what lets the four M1 fixtures
      call this unchanged. *)

  val fixup_observations : Image.laid_out -> Image.fixup_observation list
  (** The relocation evidence a laid-out module carries, kind erased. Named here even though
      [Image.fixup_observations] is reachable from the result above, because this interface is the
      statement of which requests survive erasure - and "classify this module's fixups" is one the
      differential oracle makes through nothing else. *)

  (* The six dumps of asm/docs/contracts.md §1. Each is a separate entry point
     rather than a [~stage] parameter, because they have genuinely different
     inputs: three take source text, two take bytes and an address, and one
     takes nothing. *)

  val dump_tokens : source:Span.source -> (string, Diagnostic.t list) result
  val dump_source_ast : unit_name:string -> source:Span.source -> (string, Diagnostic.t list) result

  val dump_normalized_ast :
    unit_name:string -> source:Span.source -> (string, Diagnostic.t list) result

  val dump_lowered_ast :
    unit_name:string -> source:Span.source -> (string, Diagnostic.t list) result

  val dump_disasm_canonical : address:int64 -> string -> (string, Diagnostic.t list) result
  (** exactly re-parseable: feeding this back through the assembler must produce the identical
      image, and a round-trip test does exactly that *)

  val dump_disasm_diagnostic : address:int64 -> string -> (string, Diagnostic.t list) result
  (** address, bytes, spelling and form id in columns. Read by humans and by the differential gate;
      **not** re-parseable, and no test may re-parse it - a format that had to satisfy both
      audiences would satisfy neither. *)

  val dump_codec : unit -> string

  val check_codec : unit -> string list
  (** [Codec.check] rendered. A driver that cannot be asked whether its own encoding tables are
      consistent is one whose tables are only tested where a fixture happens to look. *)
end
